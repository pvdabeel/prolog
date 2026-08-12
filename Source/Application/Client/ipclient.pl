/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> IPCLIENT
Ultralight local IPC client for a portage-ng daemon.

Canonical implementation of the client-side Unix-domain protocol (also used
by `Source/Application/Mode/ipc.pl` for connect/status/halt). This file is a
standalone entry point: the hot path loads only `library(socket)` — no
portage-ng.pl, common modules, or world set.

Heavy work is deferred:

- `library(process)` is loaded only for autostart / relaunch
- Starting the daemon runs `portage-ng-dev --mode daemon --background` in a
  child process (full KB load stays out of this address space)

Launch (portage-ng-dev --mode ipc fast-path):

```
swipl -q -f Source/Application/Client/ipclient.pl -g ipclient:main -t halt -- ARGS
```

@see Source/Application/Mode/ipc.pl
@see Source/Application/Mode/daemon.pl
*/


% main/0 is intentionally not exported: exporting it weakly-imports into
% `user` when this file is loaded via the full portage-ng stack and warns
% about overriding user:main/0. Callers use ipclient:main/0 explicitly
% (`-g ipclient:main`).
:- module(ipclient,
          [ connect/1,
            status/0,
            send_command/1,
            send_term/2,
            maybe_autostart/0,
            relaunch/0,
            socket_path/1,
            pid_path/1,
            wait_for_file/2,
            wait_for_socket_gone/2
          ]).

:- use_module(library(socket)).

% =============================================================================
%  IPCLIENT declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Entry
% -----------------------------------------------------------------------------

%! ipclient:main is det.
%
% Dispatches --status / --cmd / --shell, optionally autostarts the daemon,
% then connects and streams one request. Always halts.

ipclient:main :-
  current_prolog_flag(argv, Args),
  ( memberchk('--shell', Args)
  -> format(user_error,
       'Error: --shell is not supported in ipc mode. Use --mode standalone --shell instead.~n',
       []),
     halt(1)
  ;  memberchk('--status', Args)
  -> ( status -> halt(0) ; halt(1) )
  ;  cmd_arg(Args, Cmd)
  -> send_command(Cmd),
     halt(0)
  ;  maybe_autostart,
     connect(ExitCode),
     halt(ExitCode)
  ).


%! ipclient:cmd_arg(+Args, -Cmd) is semidet.
%
% True when Args contains `--cmd Cmd` or `--cmd=Cmd`.

ipclient:cmd_arg(['--cmd', Cmd|_], Cmd) :- !.
ipclient:cmd_arg([Arg|Rest], Cmd) :-
  ( atom_concat('--cmd=', Cmd, Arg)
  -> true
  ;  cmd_arg(Rest, Cmd)
  ).


% -----------------------------------------------------------------------------
%  Paths (mirrors config:daemon_* without loading config.pl)
% -----------------------------------------------------------------------------

%! ipclient:runtime_dir(-Dir) is det.

ipclient:runtime_dir(Dir) :-
  ( getenv('XDG_RUNTIME_DIR', Dir)
  -> true
  ;  ( getenv('USER', User) -> true ; User = unknown ),
     atom_concat('/tmp/portage-ng-', User, Dir)
  ).


%! ipclient:socket_path(-Path) is det.

ipclient:socket_path(Path) :-
  runtime_dir(Dir),
  atom_concat(Dir, '/portage-ng.sock', Path).


%! ipclient:pid_path(-Path) is det.

ipclient:pid_path(Path) :-
  runtime_dir(Dir),
  atom_concat(Dir, '/portage-ng.pid', Path).


%! ipclient:autostart_enabled is semidet.
%
% Same default as config:daemon_autostart(true). Override with
% PORTAGE_NG_IPC_AUTOSTART=0|false (checked only when the socket is missing).

ipclient:autostart_enabled :-
  ( getenv('PORTAGE_NG_IPC_AUTOSTART', V)
  -> V \== '0', V \== 'false'
  ;  true
  ).


%! ipclient:daemon_wrapper(-Wrapper) is det.
%
% Absolute path to portage-ng-dev, used to start/relaunch the daemon in a
% child process (lazy full-stack load).

ipclient:daemon_wrapper(Wrapper) :-
  getenv('PORTAGE_NG_DEV', W),
  !,
  exists_file(W),
  Wrapper = W.
ipclient:daemon_wrapper(Wrapper) :-
  source_file(ipclient:main, File),
  file_directory_name(File, Dir),
  atomic_list_concat([Dir, '/../Wrapper/portage-ng-dev'], Rel),
  absolute_file_name(Rel, Wrapper, [access(exist)]).


% -----------------------------------------------------------------------------
%  Connect + request
% -----------------------------------------------------------------------------

%! ipclient:connect(-ExitCode) is det.

ipclient:connect(ExitCode) :-
  socket_path(SocketPath),
  ( \+ access_file(SocketPath, exist)
  -> no_daemon_error,
     ExitCode = 1
  ;  connect_socket(SocketPath, In, Out)
  -> exchange(In, Out, ExitCode)
  ;  ExitCode = 1
  ).


%! ipclient:connect_socket(+SocketPath, -In, -Out) is semidet.

ipclient:connect_socket(SocketPath, In, Out) :-
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


%! ipclient:exchange(+In, +Out, -ExitCode) is det.

ipclient:exchange(In, Out, ExitCode) :-
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


%! ipclient:send_request(+Out) is det.

ipclient:send_request(Out) :-
  current_prolog_flag(argv, RawArgs),
  local_tty_size(Rows, Cols),
  collect_env(Env0),
  ( stream_property(user_output, tty(true))
  -> Env = ['_CLIENT_IS_TTY'-true | Env0]
  ;  Env = Env0
  ),
  format(Out, 'request(~q, ~w, ~w, ~q).~n', [RawArgs, Cols, Rows, Env]).


%! ipclient:local_tty_size(-Rows, -Cols) is det.
%
% Same defaults as config:local_tty_size/2 (80 rows, 160 cols when not a tty).

ipclient:local_tty_size(Rows, Cols) :-
  ( catch(tty_size(Rows, Cols), _, fail)
  -> true
  ;  Rows = 80,
     Cols = 160
  ).


%! ipclient:collect_env(-Env) is det.

ipclient:collect_env(Env) :-
  findall(Name-Value,
    ( forwarded_env_var(Name),
      getenv(Name, Value)
    ),
    Env).


%! ipclient:forwarded_env_var(?Name) is nondet.

ipclient:forwarded_env_var('USE').
ipclient:forwarded_env_var('ACCEPT_KEYWORDS').
ipclient:forwarded_env_var('ACCEPT_LICENSE').
ipclient:forwarded_env_var('PYTHON_TARGETS').
ipclient:forwarded_env_var('PYTHON_SINGLE_TARGET').
ipclient:forwarded_env_var('RUBY_TARGETS').
ipclient:forwarded_env_var('RUBY_SINGLE_TARGET').
ipclient:forwarded_env_var('LUA_SINGLE_TARGET').
ipclient:forwarded_env_var('PERL_FEATURES').
ipclient:forwarded_env_var('LLVM_SLOT').
ipclient:forwarded_env_var('VIDEO_CARDS').
ipclient:forwarded_env_var('INPUT_DEVICES').
ipclient:forwarded_env_var('CPU_FLAGS_X86').
ipclient:forwarded_env_var('APACHE2_MODULES').
ipclient:forwarded_env_var('APACHE2_MPMS').


% -----------------------------------------------------------------------------
%  Stream response (NUL EXIT:<code> sentinel)
% -----------------------------------------------------------------------------

%! ipclient:stream_response(+In, -ExitCode) is det.

ipclient:stream_response(In, ExitCode) :-
  stream_chunks(In, [], ExitCode).


%! ipclient:sentinel_marker(-Codes) is det.

ipclient:sentinel_marker([0'E, 0'X, 0'I, 0'T, 0':]).


%! ipclient:stream_chunks(+In, +Carry, -ExitCode) is det.

ipclient:stream_chunks(In, Carry, ExitCode) :-
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


%! ipclient:scan_output(+In, +Codes, -ExitCode) is det.

ipclient:scan_output(In, Codes, ExitCode) :-
  ( append(Before, [0|Tail], Codes)
  -> emit_codes(Before),
     match_sentinel(In, Tail, ExitCode)
  ;  emit_codes(Codes),
     stream_chunks(In, [], ExitCode)
  ).


%! ipclient:match_sentinel(+In, +Tail, -ExitCode) is det.

ipclient:match_sentinel(In, Tail, ExitCode) :-
  sentinel_marker(Marker),
  ( append(Marker, AfterMarker, Tail)
  -> collect_exit_code(In, AfterMarker, ExitCode)
  ;  append(Tail, _, Marker)
  -> stream_chunks(In, [0|Tail], ExitCode)
  ;  emit_codes([0]),
     scan_output(In, Tail, ExitCode)
  ).


%! ipclient:collect_exit_code(+In, +Acc, -ExitCode) is det.

ipclient:collect_exit_code(In, Acc, ExitCode) :-
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


%! ipclient:parse_exit_code(+Codes, -ExitCode) is det.

ipclient:parse_exit_code(Codes, ExitCode) :-
  ( catch(number_codes(Number, Codes), _, fail),
    integer(Number)
  -> ExitCode = Number
  ;  ExitCode = 1
  ).


%! ipclient:emit_codes(+Codes) is det.

ipclient:emit_codes([]) :- !.
ipclient:emit_codes(Codes) :-
  format('~s', [Codes]),
  flush_output.


%! ipclient:no_daemon_error is det.

ipclient:no_daemon_error :-
  socket_path(SocketPath),
  format(user_error,
    'Error: No daemon running (socket ~w not found).~n\c
     Start one with: portage-ng --mode daemon~n',
    [SocketPath]).


%! ipclient:connection_lost_error is det.

ipclient:connection_lost_error :-
  format(user_error,
    'Error: daemon closed the connection without an exit status.~n', []).


% -----------------------------------------------------------------------------
%  Status / commands / autostart
% -----------------------------------------------------------------------------

%! ipclient:status is semidet.

ipclient:status :-
  socket_path(SocketPath),
  pid_path(PidPath),
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


%! ipclient:send_command(+Cmd) is det.
%
% Cmd is halt or relaunch (atoms or strings).

ipclient:send_command(Cmd0) :-
  to_cmd_atom(Cmd0, Cmd),
  send_command_(Cmd).


%! ipclient:to_cmd_atom(+Cmd0, -Cmd) is det.

ipclient:to_cmd_atom(Cmd, Cmd) :-
  atom(Cmd), !.
ipclient:to_cmd_atom(Cmd0, Cmd) :-
  atom_string(Cmd, Cmd0).


%! ipclient:send_command_(+Cmd) is det.

ipclient:send_command_(halt) :-
  !,
  socket_path(SocketPath),
  ( \+ access_file(SocketPath, exist)
  -> format(user_error, 'No daemon running.~n', [])
  ;  send_term(SocketPath, shutdown),
     format('Daemon stopped.~n', [])
  ).
ipclient:send_command_(relaunch) :-
  !,
  relaunch.
ipclient:send_command_(Cmd) :-
  format(user_error, 'Unknown command: ~w. Use halt or relaunch.~n', [Cmd]).


%! ipclient:relaunch is det.
%
% Stops the daemon and starts a new one via the full-stack wrapper child.

ipclient:relaunch :-
  send_command_(halt),
  sleep(1),
  socket_path(SocketPath),
  wait_for_socket_gone(SocketPath, 50),
  start_daemon_background.


%! ipclient:send_term(+SocketPath, +Term) is det.

ipclient:send_term(SocketPath, Term) :-
  catch(
    ( unix_domain_socket(Socket),
      tcp_connect(Socket, SocketPath),
      tcp_open_socket(Socket, StreamPair),
      stream_pair(StreamPair, In, Out),
      set_stream(Out, encoding(utf8)),
      set_stream(In, encoding(utf8)),
      format(Out, '~q.~n', [Term]),
      flush_output(Out),
      catch(read_string(In, _, _), _, true),
      catch(close(In), _, true),
      catch(close(Out), _, true)
    ),
    Error,
    format(user_error, 'Connection error: ~w~n', [Error])
  ).


%! ipclient:maybe_autostart is det.
%
% If no daemon socket exists and autostart is enabled, start one in a child
% process. Hot path (socket already present) does not load library(process).

ipclient:maybe_autostart :-
  socket_path(SocketPath),
  \+ access_file(SocketPath, exist),
  autostart_enabled,
  !,
  start_daemon_background.
ipclient:maybe_autostart.


%! ipclient:ensure_process_lib is det.
%
% Lazily loads library(process) the first time daemon lifecycle needs it.
% use_module/1 is a no-op when the library is already imported.

ipclient:ensure_process_lib :-
  use_module(library(process)).


%! ipclient:start_daemon_background is det.
%
% Starts the full daemon via portage-ng-dev --mode daemon --background.
% The child loads the knowledge base; this client stays ultralight.

ipclient:start_daemon_background :-
  ensure_process_lib,
  ( daemon_wrapper(Wrapper)
  -> true
  ;  format(user_error,
       'Error: cannot locate portage-ng-dev to start the daemon.~n\c
        Set PORTAGE_NG_DEV or start one with: portage-ng --mode daemon~n',
       []),
     halt(1)
  ),
  % stdout/stderr must be null (not pipes): the --background child prints
  % status while we wait on process_wait/2; piping both invites deadlock
  % when the pipe buffer fills before we drain.
  format('Starting daemon in background...~n', []),
  process_create(Wrapper, ['--mode', 'daemon', '--background'],
    [ process(Pid),
      stdout(null),
      stderr(null)
    ]),
  process_wait(Pid, Status),
  ( Status == exit(0)
  -> socket_path(SocketPath),
     wait_for_file(SocketPath, 600),
     ( access_file(SocketPath, exist)
     -> format('Daemon ready.~n', [])
     ;  format(user_error, 'Warning: daemon may not have started.~n', [])
     )
  ;  format(user_error, 'Error: failed to start daemon (~w).~n', [Status]),
     halt(1)
  ).


%! ipclient:wait_for_file(+Path, +Retries) is det.

ipclient:wait_for_file(_, 0) :- !.
ipclient:wait_for_file(Path, N) :-
  ( access_file(Path, exist)
  -> true
  ;  sleep(0.1),
     N1 is N - 1,
     wait_for_file(Path, N1)
  ).


%! ipclient:wait_for_socket_gone(+Path, +Retries) is det.

ipclient:wait_for_socket_gone(_, 0) :- !.
ipclient:wait_for_socket_gone(Path, N) :-
  ( \+ access_file(Path, exist)
  -> true
  ;  sleep(0.1),
     N1 is N - 1,
     wait_for_socket_gone(Path, N1)
  ).
