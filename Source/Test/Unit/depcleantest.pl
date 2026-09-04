/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> DEPCLEANTEST
Unit tests for the depclean structural predicates (Source/Domain/Gentoo/depclean.pl).

Model items, dependency-term category/name extraction and PROVIDES
tokens.
*/

:- module(depcleantest, []).

:- use_module(library(plunit)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

% =============================================================================
%  DEPCLEANTEST declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Depclean structural predicate tests
% -----------------------------------------------------------------------------
:- begin_tests(depclean_model_item).

test(extract_depclean_bare, [true(R == myrepo://'cat/pkg-1.0')]) :-
  depclean:model_item_repo_entry(myrepo://'cat/pkg-1.0':depclean, R).

test(extract_depclean_ctx, [true(R == myrepo://'cat/pkg-1.0')]) :-
  depclean:model_item_repo_entry(myrepo://'cat/pkg-1.0':depclean?{[]}, R).

test(reject_non_depclean, [fail]) :-
  depclean:model_item_repo_entry(myrepo://'cat/pkg-1.0':install, _).

test(reject_plain_atom, [fail]) :-
  depclean:model_item_repo_entry(something, _).

:- end_tests(depclean_model_item).


:- begin_tests(depclean_dep_term_cn).

test(with_action_ctx, [true(A-C-N == run-'sys-libs'-glibc)]) :-
  depclean:dep_term_cn_deps(
    grouped_package_dependency(strong,'sys-libs',glibc,[dep1]):run?{[some_ctx]},
    A, C, N, _).

test(with_action_bare, [true(A-C-N == install-'dev-libs'-openssl)]) :-
  depclean:dep_term_cn_deps(
    grouped_package_dependency(weak,'dev-libs',openssl,[]):install,
    A, C, N, _).

test(no_action_defaults_run, [true(A-C-N == run-'app-misc'-foo)]) :-
  depclean:dep_term_cn_deps(
    grouped_package_dependency(strong,'app-misc',foo,[d1,d2]),
    A, C, N, _).

test(extracts_packagedeps, [true(PD == [d1,d2])]) :-
  depclean:dep_term_cn_deps(
    grouped_package_dependency(strong,c,n,[d1,d2]),
    _, _, _, PD).

:- end_tests(depclean_dep_term_cn).


:- begin_tests(depclean_provides_tok).

test(new_token, [true(V == [pkg://e1])]) :-
  empty_assoc(E),
  depclean:provides_tok_put(pkg://e1, 'libfoo.so', E, Out),
  get_assoc('libfoo.so', Out, V).

test(existing_token, [true(V == [pkg://e1, pkg://e2])]) :-
  list_to_assoc(['libfoo.so'-[pkg://e1]], In),
  depclean:provides_tok_put(pkg://e2, 'libfoo.so', In, Out),
  get_assoc('libfoo.so', Out, V).

:- end_tests(depclean_provides_tok).
