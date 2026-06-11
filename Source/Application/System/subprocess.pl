/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> SUBPROCESS
Predicates for spawning and interacting with external processes.
*/

:- module(subprocess, []).

% =============================================================================
%  SUBPROCESS declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Process spawning
% -----------------------------------------------------------------------------

%! subprocess:spawn_background(+Program, +Args, -Pid)
%
% Spawn a long-running external process in the background and return its
% Pid immediately. Stdout and stderr are discarded. The caller owns the
% Pid and is responsible for eventually stopping the process via
% stop_process/1 (or reaping it with process_wait/2).

subprocess:spawn_background(Program, Args, Pid) :-
  process_create(path(Program), Args, [stdout(null), stderr(null), process(Pid)]).


%! subprocess:spawn(+Program, +Args, -Lines)
%
% Run an external process, wait for it to complete, and return its
% stdout as a list of strings (one per line). Stderr is discarded so it
% cannot pollute the caller's output stream (e.g. the daemon socket in
% ipc mode). Fails with a warning when the process exits with a non-zero
% status, so failure is distinguishable from legitimate empty output.

subprocess:spawn(Program, Args, Lines) :-
  subprocess:spawn(Program, Args, Lines, []).


%! subprocess:spawn(+Program, +Args, -Lines, +Options)
%
% As spawn/3, with Options controlling timeouts:
%   * timeout(Seconds): max time to wait for output / exit (default 300)
%   * grace(Seconds):   SIGTERM-to-SIGKILL grace period (default 5)
%
% The timeout also bounds the stdout read (via a stream timeout): EOF on
% the pipe only arrives when the child exits (observed on macOS), so a
% hung child would otherwise block the read forever and the process-wait
% timeout would never be reached. When the read times out, the child is
% terminated immediately (SIGTERM, then SIGKILL after the grace period).

subprocess:spawn(Program, Args, Lines, Options) :-
  option(timeout(Timeout), Options, 300),
  option(grace(Grace), Options, 5),
  process_create(path(Program), Args,
                 [stdout(pipe(Out)), stderr(null), process(Pid)]),
  set_stream(Out, timeout(Timeout)),
  catch(reader:read_lines_to_strings(Out, Lines0), Error, true),
  close(Out, [force(true)]),
  ( var(Error)
  -> subprocess:wait_with_timeout(Pid, Status, Options)
  ;  subprocess:wait_with_timeout(Pid, Status, [timeout(0), grace(Grace)])
  ),
  ( var(Error), Status == exit(0)
  -> Lines = Lines0
  ;  subprocess:spawn_failure_reason(Error, Status, Timeout, Reason),
     message:warning([Program, ' failed: ', Reason]),
     fail
  ).


%! subprocess:spawn_failure_reason(?Error, +Status, +Timeout, -Reason)
%
% Render a human-readable failure reason for spawn/4: a read timeout
% becomes a 'no output' message, otherwise the exit status or error term
% is shown.

subprocess:spawn_failure_reason(Error, _Status, Timeout, Reason) :-
  nonvar(Error),
  Error = error(timeout_error(read, _), _),
  !,
  format(atom(Reason), 'no output for ~w seconds (process terminated)', [Timeout]).
subprocess:spawn_failure_reason(Error, Status, _Timeout, Reason) :-
  ( var(Error) -> Culprit = Status ; Culprit = Error ),
  format(atom(Reason), '~w', [Culprit]).


%! subprocess:spawn_parsed(+Program, +Args, :Parser, -Results)
%
% Run an external process, parse each output line with Parser, and
% return the collected Results. Parser is called as call(Parser, Line, Result);
% lines for which Parser fails are silently skipped.

:- meta_predicate subprocess:spawn_parsed(+, +, 3, -).

subprocess:spawn_parsed(Program, Args, Parser, Results) :-
  subprocess:spawn(Program, Args, Lines),
  findall(R, (member(L, Lines), call(Parser, L, R)), Results).


% -----------------------------------------------------------------------------
%  Process termination
% -----------------------------------------------------------------------------

%! subprocess:wait_with_timeout(+Pid, -Status) is det.
%
% Wait for a child process with the default options (300s timeout).

subprocess:wait_with_timeout(Pid, Status) :-
  subprocess:wait_with_timeout(Pid, Status, []).


%! subprocess:wait_with_timeout(+Pid, -Status, +Options) is det.
%
% Wait for a child process with a bounded timeout. If the process does
% not exit within timeout(Seconds) (default 300), escalate: send SIGTERM,
% wait grace(Seconds) (default 5), then SIGKILL. Always reaps the process
% and returns its exit Status.

subprocess:wait_with_timeout(Pid, Status, Options) :-
  option(timeout(Timeout), Options, 300),
  option(grace(Grace), Options, 5),
  subprocess:wait_bounded(Pid, Timeout, Status0),
  ( Status0 == timeout
  -> process_kill(Pid, term),
     subprocess:wait_bounded(Pid, Grace, Status1),
     ( Status1 == timeout
     -> process_kill(Pid, kill),
        process_wait(Pid, Status)
     ;  Status = Status1
     )
  ;  Status = Status0
  ).


%! subprocess:wait_bounded(+Pid, +Timeout, -Status) is det.
%
% Wait up to Timeout seconds for Pid to exit; Status is the exit status,
% or the atom 'timeout' when the deadline passes. Implemented as a
% non-blocking poll loop because process_wait/3 does not honour nonzero
% timeout options on all platforms (it blocks indefinitely on macOS).

subprocess:wait_bounded(Pid, Timeout, Status) :-
  get_time(Now),
  Deadline is Now + Timeout,
  subprocess:wait_bounded_(Pid, Deadline, Status).

subprocess:wait_bounded_(Pid, Deadline, Status) :-
  process_wait(Pid, Status0, [timeout(0)]),
  ( Status0 \== timeout
  -> Status = Status0
  ;  get_time(Now),
     ( Now >= Deadline
     -> Status = timeout
     ;  sleep(0.2),
        subprocess:wait_bounded_(Pid, Deadline, Status)
     )
  ).


%! subprocess:stop_process(+Pid) is det.
%
% Terminate a background process: SIGTERM, short grace period, then
% SIGKILL. Always reaps the child. Succeeds even when the process is
% already gone.

subprocess:stop_process(Pid) :-
  catch(
    ( process_kill(Pid, term),
      subprocess:wait_with_timeout(Pid, _, [timeout(5), grace(2)])
    ),
    _, true).


% -----------------------------------------------------------------------------
%  dns-sd interface
% -----------------------------------------------------------------------------

% Registry of running `dns-sd -R` advertise processes, keyed on Host and
% Service. Re-advertising the same Host/Service stops the previous
% registration first, and all registrations are stopped at halt, so
% advertise processes can never pile up or outlive the application.

:- dynamic subprocess:advertised/3.

:- at_halt(subprocess:stop_advertised).


%! subprocess:dns_sd(+browse, +Service, -Hostnames)
%
% Browse for hosts advertising Service via mDNS.

subprocess:dns_sd(browse, Service, Hostnames) :-
  subprocess:spawn_parsed('dns-sd', ['-t','1','-B', Service],
                          subprocess:dns_sd_parse_host(Service), Raw),
  list_to_set(Raw, Hostnames).


%! subprocess:dns_sd(+advertise, +Host, +Service, +Port)
%
% Advertise a Service on Host and Port via mDNS. The `dns-sd -R` process
% never exits by itself; its Pid is recorded so a re-advertise replaces
% the previous registration and halt cleans it up.

subprocess:dns_sd(advertise, Host, Service, Port) :-
  subprocess:unadvertise(Host, Service),
  subprocess:spawn_background('dns-sd', ['-R', Host, Service, 'local', Port], Pid),
  assertz(subprocess:advertised(Host, Service, Pid)).


%! subprocess:dns_sd(+resolve, +Service, +Hostname, -Port)
%
% Resolve a Hostname advertising Service to its Port via mDNS.

subprocess:dns_sd(resolve, Service, Hostname, Port) :-
  subprocess:spawn_parsed('dns-sd', ['-t','1','-L', Hostname, Service],
                          subprocess:dns_sd_parse_port, Ports),
  Ports = [Port|_],
  !.


%! subprocess:unadvertise(+Host, +Service) is det.
%
% Stop any running advertise process registered for Host and Service.

subprocess:unadvertise(Host, Service) :-
  forall(retract(subprocess:advertised(Host, Service, Pid)),
         subprocess:stop_process(Pid)).


%! subprocess:stop_advertised is det.
%
% Stop all running advertise processes. Called at halt.

subprocess:stop_advertised :-
  forall(retract(subprocess:advertised(_, _, Pid)),
         subprocess:stop_process(Pid)).


%! subprocess:dns_sd_parse_host(+Service, +Line, -Host)
%
% Parse one `dns-sd -B` output line into an instance name. Anchored on
% the browse column layout (Timestamp, A/R, Flags, if, Domain, Service
% Type, Instance Name) and restricted to 'Add' events, so headers,
% warnings and removal events never produce entries. The instance name
% may contain spaces (e.g. 'Mac Pro').

subprocess:dns_sd_parse_host(_Service, Line, Host) :-
  re_matchsub('^[0-9]{1,2}:[0-9]{2}:[0-9]{2}[.][0-9]+ +Add +[0-9]+ +[0-9]+ +[^ ]+ +[^ ]+ +(.+)$',
              Line, M, [capture_type(string)]),
  split_string(M.1, "", " \t", [Host]).


%! subprocess:dns_sd_parse_port(+Line, -Port)
%
% Parse one `dns-sd -L` output line into a port number.

subprocess:dns_sd_parse_port(Line, Port) :-
  re_matchsub('reached at ([0-9A-Za-z-.]+):([0-9]+)', Line, M, [capture_type(string)]),
  number_string(Port, M.2).
