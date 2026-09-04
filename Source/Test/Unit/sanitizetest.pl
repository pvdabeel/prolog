/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> SANITIZETEST
Unit tests for input validation (Source/Application/Security/sanitize.pl).

Paths, filenames, categories, names, snapshots, git commits, build
phases and daemon requests.
*/

:- module(sanitizetest, []).

:- use_module(library(plunit)).
:- use_module(library(lists)).

% =============================================================================
%  SANITIZETEST declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Sanitize validation tests
% -----------------------------------------------------------------------------
:- begin_tests(sanitize_path).

test(valid_component) :-
  sanitize:safe_path_component(hello).

test(valid_component_with_dot) :-
  sanitize:safe_path_component('file.txt').

test(reject_empty, [fail]) :-
  sanitize:safe_path_component('').

test(reject_slash, [fail]) :-
  sanitize:safe_path_component('a/b').

test(reject_dotdot, [fail]) :-
  sanitize:safe_path_component('..').

test(reject_embedded_dotdot, [fail]) :-
  sanitize:safe_path_component('foo/../bar').

test(reject_non_atom, [fail]) :-
  sanitize:safe_path_component(123).

:- end_tests(sanitize_path).


:- begin_tests(sanitize_filename).

test(valid_filename) :-
  sanitize:safe_filename('package-1.0.ebuild').

test(reject_directory_traversal, [fail]) :-
  sanitize:safe_filename('../etc/passwd').

:- end_tests(sanitize_filename).


:- begin_tests(sanitize_category).

test(valid_category) :-
  sanitize:safe_portage_category('sys-apps').

test(reject_slash, [fail]) :-
  sanitize:safe_portage_category('sys/apps').

test(reject_empty, [fail]) :-
  sanitize:safe_portage_category('').

:- end_tests(sanitize_category).


:- begin_tests(sanitize_name).

test(valid_name) :-
  sanitize:safe_portage_name(portage).

test(reject_traversal, [fail]) :-
  sanitize:safe_portage_name('../../etc').

:- end_tests(sanitize_name).


:- begin_tests(sanitize_snapshot).

test(valid_id) :-
  sanitize:safe_snapshot_id('snap-2026-01-01').

test(reject_slash, [fail]) :-
  sanitize:safe_snapshot_id('snap/bad').

test(reject_backslash, [fail]) :-
  sanitize:safe_snapshot_id('snap\\bad').

test(reject_dotdot, [fail]) :-
  sanitize:safe_snapshot_id('snap..bad').

:- end_tests(sanitize_snapshot).


:- begin_tests(sanitize_git_commit).

test(valid_sha1) :-
  sanitize:safe_git_commit('0123456789abcdef0123456789abcdef01234567').

test(valid_sha256) :-
  sanitize:safe_git_commit('0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef').

test(reject_short, [fail]) :-
  sanitize:safe_git_commit(abc1234).

test(reject_branch, [fail]) :-
  sanitize:safe_git_commit(master).

test(reject_metachar, [fail]) :-
  sanitize:safe_git_commit('0123456789abcdef0123456789abcdef0123456;').

:- end_tests(sanitize_git_commit).


:- begin_tests(sanitize_phase).

test(known_phase) :-
  sanitize:safe_phase(compile).

test(all_phases) :-
  forall(member(P, [clean,setup,unpack,prepare,configure,compile,
                    test,install,package,merge,unmerge,
                    preinst,postinst,prerm,postrm,config,info,nofetch]),
         sanitize:safe_phase(P)).

test(unknown_phase, [fail]) :-
  sanitize:safe_phase(bogus).

:- end_tests(sanitize_phase).


:- begin_tests(sanitize_daemon_request).

test(shutdown) :-
  sanitize:safe_daemon_request(shutdown).

test(valid_request) :-
  sanitize:safe_daemon_request(request([foo, bar], 80, 24)).

test(valid_request_with_env) :-
  sanitize:safe_daemon_request(request([foo], 80, 24, [a,b])).

test(reject_bad_args, [fail]) :-
  sanitize:safe_daemon_request(request(notalist, 80, 24)).

:- end_tests(sanitize_daemon_request).
