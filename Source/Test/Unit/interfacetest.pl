/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> INTERFACETEST
Unit tests for CLI request dispatch (Source/Application/Interface/requests.pl).
*/

:- module(interfacetest, []).

:- use_module(library(plunit)).
:- use_module(library(lists)).

% =============================================================================
%  INTERFACETEST declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Interface request dispatch tests
% -----------------------------------------------------------------------------

:- begin_tests(interface_request_dispatch).

% Every handler flag maps to an option declared in interface:spec/1.
% 'shellrun' is the only pseudo-flag (--shell with target arguments).
test(handler_flags_declared_in_spec, [true(Unknown == [shellrun])]) :-
  interface:spec(Spec),
  findall(Opt, (member(Line, Spec), member(opt(Opt), Line)), Opts),
  findall(Flag,
          ( interface:request_handler(Flag, _, _, _, _),
            \+ memberchk(Flag, Opts) ),
          Unknown).

% One handler per flag.
test(handler_flags_unique) :-
  findall(Flag, interface:request_handler(Flag, _, _, _, _), Flags),
  msort(Flags, Sorted),
  sort(Flags, Set),
  Sorted == Set.

% Order is load-bearing: --shell must be dispatched before --merge, because
% merge(true) is the optparse default (spec.pl) and matches any command line.
test(shell_precedes_merge_catchall) :-
  findall(Flag, interface:request_handler(Flag, _, _, _, _), Flags),
  once(nth0(ShellRun, Flags, shellrun)),
  once(nth0(Shell, Flags, shell)),
  once(nth0(Merge, Flags, merge)),
  ShellRun < Shell,
  Shell < Merge,
  last(Flags, merge).

% Per-flag goal lookup: the table binds Mode/Args/Options into the goal.
test(info_handler_goal) :-
  once(interface:request_handler(info, mode, args, opts, Goal)),
  Goal == action:process_action(info, args, opts).

test(sync_handler_goal_uses_mode) :-
  once(interface:request_handler(sync, standalone, args, opts, Goal)),
  Goal == action:process_sync(standalone, args).

% Default guard: boolean flag set to true in Options.
test(matches_boolean_flag) :-
  interface:request_matches(search, [], [search(true)]).

test(matches_boolean_flag_false, [fail]) :-
  interface:request_matches(search, [], [search(false)]).

% Specialised guards.
test(matches_rollback_value) :-
  interface:request_matches(rollback, [], [rollback(snap1)]).

test(matches_rollback_none_fails, [fail]) :-
  interface:request_matches(rollback, [], [rollback(none)]).

test(matches_llm_service) :-
  interface:request_matches(llm, [], [llm(ollama)]).

test(matches_llm_none_fails, [fail]) :-
  interface:request_matches(llm, [], [llm(none)]).

test(matches_shellrun_needs_args) :-
  interface:request_matches(shellrun, ['app-misc/foo'], [shell(true)]).

test(matches_shellrun_no_args_fails, [fail]) :-
  interface:request_matches(shellrun, [], [shell(true)]).

% Selection: first triggered handler in table order wins.
test(select_shellrun_over_merge) :-
  Opts = [shell(true), merge(true)],
  interface:request_select(standalone, ['app-misc/foo'], Opts, Flag, Goal),
  Flag == shellrun,
  Goal == action:process_action(run, ['app-misc/foo'], Opts).

test(select_bare_shell_is_noop) :-
  interface:request_select(standalone, [], [shell(true), merge(true)], Flag, Goal),
  Flag == shell,
  Goal == true.

test(select_merge_catchall) :-
  Opts = [merge(true)],
  interface:request_select(standalone, ['app-misc/foo'], Opts, Flag, Goal),
  Flag == merge,
  Goal == action:process_action(run, ['app-misc/foo'], Opts).

% `--fetchonly` / `-F` prove the same :run plan as --merge.
test(fetchonly_handler_proves_run) :-
  once(interface:request_handler(fetchonly, mode, args, opts, Goal)),
  Goal == action:process_action(run, args, opts).

test(fetchall_handler_proves_run) :-
  once(interface:request_handler(fetchall, mode, args, opts, Goal)),
  Goal == action:process_action(run, args, opts).

% `--build --fetchonly` must execute (filtered) rather than only print.
test(build_precedes_fetchonly) :-
  findall(Flag, interface:request_handler(Flag, _, _, _, _), Flags),
  once(nth0(Build, Flags, build)),
  once(nth0(Fetch, Flags, fetchonly)),
  Build < Fetch.

test(select_build_over_fetchonly) :-
  Opts = [build(true), fetchonly(true), merge(true)],
  interface:request_select(standalone, ['app-misc/foo'], Opts, Flag, Goal),
  Flag == build,
  Goal == action:process_build(['app-misc/foo'], Opts).

% No handler triggered -> selection fails (process_requests then reports
% the unrecognised options and falls through to its catch-all halt(1)).
test(select_fails_on_unrecognised_options, [fail]) :-
  interface:request_select(standalone, [], [merge(false)], _, _).

:- end_tests(interface_request_dispatch).
