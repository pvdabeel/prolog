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

%! ipc:connect(-ExitCode) is det.
%
% Connects to the daemon, sends the current CLI arguments and terminal
% dimensions, streams the output to current_output, and unifies ExitCode
% with the daemon's exit code. Only connection-phase errors are reported
% as "no daemon running"; errors during request I/O are reported verbatim.

ipc:connect(ExitCode) :-
  config:daemon_socket_path(SocketPath),
  ( \+ access_file(SocketPath, exist)
  -> no_daemon_error,
     ExitCode = 1
  ;  connect_socket(SocketPath, In, Out)
  -> exchange(In, Out, ExitCode)
  ;  ExitCode = 1
  ).


%! ipc:connect_socket(+SocketPath, -In, -Out) is semidet.
%
% Connection phase: creates a Unix domain socket and connects it to the
% daemon. Only errors raised here mean "no daemon running"; on such an
% error the message is printed and the predicate fails.

ipc:connect_socket(SocketPath, In, Out) :-
  catch(
    ( unix_domain_socket(Socket),
      tcp_connect(Socket, SocketPath),
      tcp_open_socket(Socket, StreamPair),
      stream_pair(StreamPair, In, Out),
      set_stream(Out, encoding(utf8)),
      set_stream(In, encoding(utf8))
    ),
    _ConnectError,
    ( no_daemon_error,
      fail )
  ).


%! ipc:exchange(+In, +Out, -ExitCode) is det.
%
% I/O phase: sends the request and streams the response. Errors raised
% after the connection was established (encoding errors, daemon crash
% mid-stream, permission problems, ...) are reported verbatim instead of
% being masked as "no daemon running". Output streamed before the error
% has already been written to current_output and is preserved.

ipc:exchange(In, Out, ExitCode) :-
  catch(
    ( send_request(Out),
      flush_output(Out),
      stream_response(In, ExitCode)
    ),
    Error,
    ( flush_output,
      print_message(error, Error),
      ExitCode = 1 )
  ),
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
% Streams the daemon's output to current_output incrementally, chunk by
% chunk as it arrives, until the `\0EXIT:<code>` terminator or end of
% stream is encountered. The terminator is matched across chunk
% boundaries, and a NUL that does not start the terminator is passed
% through as ordinary output.

ipc:stream_response(In, ExitCode) :-
  stream_chunks(In, [], ExitCode).


%! ipc:sentinel_marker(-Codes) is det.
%
% Character codes of the EXIT marker that follows the NUL terminator
% byte sent by the daemon.

ipc:sentinel_marker([0'E, 0'X, 0'I, 0'T, 0':]).


%! ipc:stream_chunks(+In, +Carry, -ExitCode) is det.
%
% Reads the next available chunk without waiting for end of stream.
% Carry holds codes held back from the previous chunk because they may
% start the EXIT sentinel (Carry is empty or begins with NUL). End of
% stream without a sentinel means the daemon went away mid-request:
% any held-back codes are flushed, the problem is reported, and the
% exit code is 1.

ipc:stream_chunks(In, Carry, ExitCode) :-
  fill_buffer(In),
  read_pending_codes(In, Chunk, []),
  ( Chunk == [],
    at_end_of_stream(In)
  -> emit_codes(Carry),
     connection_lost_error,
     ExitCode = 1
  ;  Chunk == []
  -> stream_chunks(In, Carry, ExitCode)
  ;  append(Carry, Chunk, Codes),
     scan_output(In, Codes, ExitCode)
  ).


%! ipc:scan_output(+In, +Codes, -ExitCode) is det.
%
% Prints everything before the first NUL and dispatches on whether the
% NUL starts the EXIT sentinel. A buffer without a NUL is plain output.

ipc:scan_output(In, Codes, ExitCode) :-
  ( append(Before, [0|Tail], Codes)
  -> emit_codes(Before),
     match_sentinel(In, Tail, ExitCode)
  ;  emit_codes(Codes),
     stream_chunks(In, [], ExitCode)
  ).


%! ipc:match_sentinel(+In, +Tail, -ExitCode) is det.
%
% Tail holds the codes following a NUL. Three cases: the full `EXIT:`
% marker follows (parse the exit code), Tail is a prefix of the marker
% so more input is needed to decide (hold the codes back), or the NUL
% was ordinary output and scanning continues after it.

ipc:match_sentinel(In, Tail, ExitCode) :-
  sentinel_marker(Marker),
  ( append(Marker, AfterMarker, Tail)
  -> collect_exit_code(In, AfterMarker, ExitCode)
  ;  append(Tail, _, Marker)
  -> stream_chunks(In, [0|Tail], ExitCode)
  ;  emit_codes([0]),
     scan_output(In, Tail, ExitCode)
  ).


%! ipc:collect_exit_code(+In, +Acc, -ExitCode) is det.
%
% Accumulates the codes following the EXIT marker until a newline (or
% end of stream) is reached, then parses them as the exit code.

ipc:collect_exit_code(In, Acc, ExitCode) :-
  ( append(CodeCodes, [0'\n|_], Acc)
  -> parse_exit_code(CodeCodes, ExitCode)
  ;  fill_buffer(In),
     read_pending_codes(In, More, []),
     ( More == []
     -> parse_exit_code(Acc, ExitCode)
     ;  append(Acc, More, Acc1),
        collect_exit_code(In, Acc1, ExitCode)
     )
  ).


%! ipc:parse_exit_code(+Codes, -ExitCode) is det.
%
% Parses the exit code digits; defaults to 1 when malformed.

ipc:parse_exit_code(Codes, ExitCode) :-
  ( catch(number_codes(Number, Codes), _, fail),
    integer(Number)
  -> ExitCode = Number
  ;  ExitCode = 1
  ).


%! ipc:emit_codes(+Codes) is det.
%
% Writes codes to current_output and flushes, keeping output live.

ipc:emit_codes([]) :- !.
ipc:emit_codes(Codes) :-
  format('~s', [Codes]),
  flush_output.


%! ipc:no_daemon_error is det.
%
% Prints an error message when no daemon is running.

ipc:no_daemon_error :-
  config:daemon_socket_path(SocketPath),
  format(user_error,
    'Error: No daemon running (socket ~w not found).~n\c
     Start one with: portage-ng --mode daemon~n',
    [SocketPath]).


%! ipc:connection_lost_error is det.
%
% Prints an error message when the daemon closed the connection without
% sending the EXIT terminator (e.g. it crashed or halted mid-request).

ipc:connection_lost_error :-
  format(user_error,
    'Error: daemon closed the connection without an exit status.~n', []).


% -----------------------------------------------------------------------------
%  Lifecycle management
% -----------------------------------------------------------------------------

%! ipc:fork_background(+Mode) is det.
%
% Forks a new detached swipl process running the given Mode (daemon or
% server) without --background. The swipl executable and flags are taken
% from the current process, so they cannot drift from the launcher's.
% The parent polls for readiness then exits.

ipc:fork_background(Mode) :-
  background_command(Mode, Exe, Args),
  process_create(Exe, Args,
    [ process(Pid),
      detached(true),
      stdout(null),
      stderr(null)
    ]),
  format('Starting ~w in background (PID ~w)...~n', [Mode, Pid]),
  wait_for_ready(Mode, Pid).


%! ipc:background_command(+Mode, -Exe, -Args) is det.
%
% Rebuilds the command line of the current process with the application
% arguments (after `--`) replaced by `--mode Mode`. This keeps the swipl
% flags (-O, stack limits, -f, -p, -g main) in a single place — the
% launcher that started the current process — instead of duplicating
% them here.

ipc:background_command(Mode, Exe, Args) :-
  current_prolog_flag(executable, Exe),
  current_prolog_flag(os_argv, [_Argv0|Rest]),
  ( append(SwiplFlags, ['--'|_AppArgs], Rest)
  -> true
  ;  SwiplFlags = Rest
  ),
  append(SwiplFlags, ['--', '--mode', Mode], Args).


%! ipc:wait_for_ready(+Mode, +Pid) is det.
%
% Polls for mode-specific readiness instead of sleeping a fixed amount:
% the daemon is ready once its PID file appears, the server once its
% port accepts TCP connections.

ipc:wait_for_ready(daemon, Pid) :-
  !,
  config:daemon_pid_path(PidPath),
  wait_for_file(PidPath, 600),
  ( exists_file(PidPath)
  -> format('Daemon ready (PID ~w).~n', [Pid])
  ;  format(user_error, 'Warning: daemon may not have started.~n', [])
  ).
ipc:wait_for_ready(server, Pid) :-
  !,
  interface:get_port(Port),
  ( wait_for_port(localhost, Port, 600)
  -> format('Server ready (PID ~w, port ~w).~n', [Pid, Port])
  ;  format(user_error, 'Warning: server may not have started.~n', [])
  ).
ipc:wait_for_ready(_, _).


%! ipc:wait_for_port(+Host, +Port, +Retries) is semidet.
%
% Polls until a TCP connection to Host:Port succeeds, sleeping 100ms
% between retries. Fails when the retries are exhausted.

ipc:wait_for_port(_, _, 0) :- !, fail.
ipc:wait_for_port(Host, Port, N) :-
  ( interface:server_reachable(Host, Port)
  -> true
  ;  sleep(0.1),
     N1 is N - 1,
     wait_for_port(Host, Port, N1)
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
