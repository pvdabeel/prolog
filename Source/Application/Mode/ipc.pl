/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> IPC
Client side of the local ipc transport. Connects to a persistent daemon
(see daemon.pl) over a Unix domain socket, forwards CLI arguments, terminal
dimensions, and portage-relevant environment variables, streams the daemon's
output back to stdout, and returns the daemon's exit code.

Also provides daemon lifecycle control invoked from the client/launcher side:
background forking (`ipc:fork_background/1`), status queries, autostart, and
stop/relaunch commands. The daemon server loop itself lives in daemon.pl.

Authentication relies on Unix file permissions (socket mode 0600 inside a
0700 owner-verified runtime directory; see daemon:ensure_runtime_dir/0).
Only processes running as the same OS user can connect.

@see daemon.pl
@see config:daemon_socket_path/1
@see config:daemon_pid_path/1
*/

:- module(ipc, []).

% =============================================================================
%  IPC declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Client connection
% -----------------------------------------------------------------------------

%! ipc:connect(-ExitCode) is semidet.
%
% Connects to the daemon, sends the current CLI arguments and terminal
% dimensions, streams the output to current_output, and unifies ExitCode
% with the daemon's exit code. Fails if no daemon is running.

ipc:connect(ExitCode) :-
  config:daemon_socket_path(SocketPath),
  ( \+ access_file(SocketPath, exist)
  -> no_daemon_error,
     ExitCode = 1
  ;  catch(
       do_connect(SocketPath, ExitCode),
       _Error,
       ( no_daemon_error,
         ExitCode = 1 )
     )
  ).


%! ipc:do_connect(+SocketPath, -ExitCode) is det.
%
% Performs the actual connection and I/O with the daemon.

ipc:do_connect(SocketPath, ExitCode) :-
  unix_domain_socket(Socket),
  tcp_connect(Socket, SocketPath),
  tcp_open_socket(Socket, StreamPair),
  stream_pair(StreamPair, In, Out),
  set_stream(Out, encoding(utf8)),
  set_stream(In, encoding(utf8)),
  send_request(Out),
  flush_output(Out),
  stream_response(In, ExitCode),
  catch(close(In), _, true),
  catch(close(Out), _, true).


%! ipc:send_request(+Out) is det.
%
% Sends the current CLI arguments, terminal dimensions, and
% portage-relevant environment variables to the daemon.

ipc:send_request(Out) :-
  current_prolog_flag(argv, RawArgs),
  config:local_tty_size(Rows, Cols),
  collect_env(Env0),
  ( stream_property(user_output, tty(true))
  -> Env = ['_CLIENT_IS_TTY'-true | Env0]
  ;  Env = Env0
  ),
  format(Out, 'request(~q, ~w, ~w, ~q).~n', [RawArgs, Cols, Rows, Env]).


%! ipc:collect_env(-Env:list) is det.
%
% Collects portage-relevant environment variables that are set in the
% client process, as Name-Value pairs.

ipc:collect_env(Env) :-
  findall(Name-Value,
    ( forwarded_env_var(Name),
      system:getenv(Name, Value)
    ),
    Env).


%! ipc:forwarded_env_var(?Name) is nondet.
%
% Environment variables forwarded from IPC client to daemon.

ipc:forwarded_env_var('USE').
ipc:forwarded_env_var('ACCEPT_KEYWORDS').
ipc:forwarded_env_var('PYTHON_TARGETS').
ipc:forwarded_env_var('PYTHON_SINGLE_TARGET').
ipc:forwarded_env_var('RUBY_TARGETS').
ipc:forwarded_env_var('RUBY_SINGLE_TARGET').
ipc:forwarded_env_var('LUA_SINGLE_TARGET').
ipc:forwarded_env_var('PERL_FEATURES').
ipc:forwarded_env_var('LLVM_SLOT').
ipc:forwarded_env_var('VIDEO_CARDS').
ipc:forwarded_env_var('INPUT_DEVICES').
ipc:forwarded_env_var('CPU_FLAGS_X86').
ipc:forwarded_env_var('APACHE2_MODULES').
ipc:forwarded_env_var('APACHE2_MPMS').


%! ipc:stream_response(+In, -ExitCode) is det.
%
% Reads the daemon's output byte by byte, writing to current_output,
% until the EXIT terminator is encountered.

ipc:stream_response(In, ExitCode) :-
  read_string(In, _, FullOutput),
  parse_output(FullOutput, ExitCode).


%! ipc:parse_output(+Output, -ExitCode) is det.
%
% Splits the output at the EXIT terminator, prints the main output,
% and extracts the exit code.

ipc:parse_output(Output, ExitCode) :-
  atom_codes(Sentinel, [0, 0'E, 0'X, 0'I, 0'T, 0':]),
  ( sub_string(Output, Before, _, _, Sentinel)
  -> sub_string(Output, 0, Before, _, MainOutput),
     write(MainOutput),
     flush_output,
     SentLen = 6,
     TermStart is Before + SentLen,
     sub_string(Output, TermStart, _, 0, Tail),
     ( sub_string(Tail, NL, _, _, "\n")
     -> sub_string(Tail, 0, NL, _, CodeStr)
     ;  CodeStr = Tail
     ),
     ( number_string(ExitCode, CodeStr) -> true ; ExitCode = 1 )
  ;  write(Output),
     flush_output,
     ExitCode = 0
  ).


%! ipc:no_daemon_error is det.
%
% Prints an error message when no daemon is running.

ipc:no_daemon_error :-
  config:daemon_socket_path(SocketPath),
  format(user_error,
    'Error: No daemon running (socket ~w not found).~n\c
     Start one with: portage-ng --mode daemon~n',
    [SocketPath]).


% -----------------------------------------------------------------------------
%  Lifecycle management
% -----------------------------------------------------------------------------

%! ipc:fork_background(+Mode) is det.
%
% Forks a new detached swipl process running the given Mode (daemon or
% server) without --background. The parent polls for readiness then exits.

ipc:fork_background(Mode) :-
  config:installation_dir(Dir),
  atomic_list_concat([Dir, '/portage-ng.pl'], MainFile),
  atom_concat('portage=', Dir, PortagePath),
  process_create(
    path(swipl),
    [ '-O',
      '--stack-limit=256G', '--table-space=256G', '--shared-table-space=256G',
      '-f', MainFile,
      '-p', PortagePath,
      '-Dverbose_autoload=false',
      '-g', 'main',
      '--',
      '--mode', Mode
    ],
    [ process(Pid),
      detached(true),
      stdout(null),
      stderr(null)
    ]
  ),
  atom_string(Mode, ModeStr),
  format('Starting ~w in background (PID ~w)...~n', [ModeStr, Pid]),
  ( Mode == daemon
  -> config:daemon_pid_path(PidPath),
     wait_for_file(PidPath, 100),
     ( exists_file(PidPath)
     -> format('Daemon ready (PID ~w).~n', [Pid])
     ;  format(user_error, 'Warning: daemon may not have started.~n', [])
     )
  ;  sleep(2),
     format('Server started in background (PID ~w).~n', [Pid])
  ).


%! ipc:wait_for_file(+Path, +Retries) is det.
%
% Polls for a file to appear, sleeping 100ms between retries.

ipc:wait_for_file(_, 0) :- !.
ipc:wait_for_file(Path, N) :-
  ( access_file(Path, exist)
  -> true
  ;  sleep(0.1),
     N1 is N - 1,
     wait_for_file(Path, N1)
  ).


%! ipc:status is semidet.
%
% Checks if a daemon is running. Prints PID if running, error if not.
% Succeeds if daemon is running, fails otherwise.

ipc:status :-
  config:daemon_socket_path(SocketPath),
  config:daemon_pid_path(PidPath),
  ( access_file(SocketPath, exist)
  -> ( exists_file(PidPath)
     -> setup_call_cleanup(
          open(PidPath, read, S),
          read_string(S, _, PidStr),
          close(S)),
        normalize_space(atom(Pid), PidStr),
        format('Daemon running (PID ~w, socket ~w)~n', [Pid, SocketPath])
     ;  format('Daemon socket exists (~w) but no PID file.~n', [SocketPath])
     )
  ;  format('No daemon running.~n', []),
     fail
  ).


%! ipc:send_command(+Cmd) is det.
%
% Sends a command term to the daemon over the Unix socket.
% Cmd is one of: halt, relaunch.

ipc:send_command(halt) :-
  !,
  config:daemon_socket_path(SocketPath),
  ( \+ access_file(SocketPath, exist)
  -> format(user_error, 'No daemon running.~n', [])
  ;  send_term(SocketPath, shutdown),
     format('Daemon stopped.~n', [])
  ).

ipc:send_command(relaunch) :-
  !,
  ipc:send_command(halt),
  sleep(1),
  config:daemon_socket_path(SocketPath),
  wait_for_socket_gone(SocketPath, 50),
  ipc:fork_background(daemon).

ipc:send_command(Cmd) :-
  format(user_error, 'Unknown command: ~w. Use halt or relaunch.~n', [Cmd]).


%! ipc:autostart is det.
%
% If no daemon socket exists and autostart is configured, fork a
% background daemon. Otherwise succeed silently.

ipc:autostart :-
  config:daemon_socket_path(SocketPath),
  \+ access_file(SocketPath, exist),
  config:daemon_autostart(true),
  !,
  ipc:fork_background(daemon).
ipc:autostart.


%! ipc:relaunch is det.
%
% Convenience: stops the current daemon and starts a new one in background.

ipc:relaunch :-
  ipc:send_command(relaunch).


%! ipc:send_term(+SocketPath, +Term) is det.
%
% Connects to the daemon socket and sends a Prolog term.

ipc:send_term(SocketPath, Term) :-
  catch(
    ( unix_domain_socket(Socket),
      tcp_connect(Socket, SocketPath),
      tcp_open_socket(Socket, StreamPair),
      stream_pair(StreamPair, In, Out),
      set_stream(Out, encoding(utf8)),
      set_stream(In, encoding(utf8)),
      format(Out, '~q.~n', [Term]),
      flush_output(Out),
      catch(read_string(In, _, _Response), _, true),
      catch(close(In), _, true),
      catch(close(Out), _, true)
    ),
    Error,
    format(user_error, 'Connection error: ~w~n', [Error])
  ).


%! ipc:wait_for_socket_gone(+Path, +Retries) is det.
%
% Polls until the socket file disappears, sleeping 100ms between retries.

ipc:wait_for_socket_gone(_, 0) :- !.
ipc:wait_for_socket_gone(Path, N) :-
  ( \+ access_file(Path, exist)
  -> true
  ;  sleep(0.1),
     N1 is N - 1,
     wait_for_socket_gone(Path, N1)
  ).
