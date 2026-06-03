/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> UNITTEST
PLUnit-based unit tests for core modules.

Covers pure-logic predicates in eapi, version_domain, constraint, kahn,
and sanitize that can be tested without a loaded knowledge base.

Run via the project wrapper:

  ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell <<'PL'
  load_files(portage('Source/Test/unittest'), [if(true)]).
  run_tests.
  halt.
  PL

Or via make:

  make test

Profile-mask golden regression (requires `Knowledge/kb.qlf` and
`Knowledge/profile.qlf`; snapshot embedded in this file):

  make test-profile-mask-golden

Regenerate the golden snapshot after an intentional mask-logic change:

  make test-profile-mask-golden-update
*/

:- use_module(library(plunit)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(ordsets)).

:- set_test_options([load(always)]).


% =============================================================================
%  UNITTEST declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  EAPI version parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_version_parsing).

test(simple_version, [true(V == version([1,0], '', 4, 0, '', 0, '1.0')), nondet]) :-
  atom_codes('1.0', Codes),
  phrase(eapi:version(V), Codes, []).

test(three_part_version, [true(V == version([1,2,3], '', 4, 0, '', 0, '1.2.3')), nondet]) :-
  atom_codes('1.2.3', Codes),
  phrase(eapi:version(V), Codes, []).

test(version_with_alpha, [nondet]) :-
  atom_codes('1.0a', Codes),
  phrase(eapi:version(version(_, Alpha, _, _, _, _, _)), Codes, []),
  Alpha \== ''.

test(version_with_revision, [nondet]) :-
  atom_codes('1.0-r1', Codes),
  phrase(eapi:version(version(_, _, _, _, _, Rev, _)), Codes, []),
  Rev == 1.

test(version_with_suffix_alpha, [nondet]) :-
  atom_codes('1.0_alpha1', Codes),
  phrase(eapi:version(version(_, _, Rank, _, _, _, _)), Codes, []),
  Rank == 0.

test(version_with_suffix_beta, [nondet]) :-
  atom_codes('1.0_beta2', Codes),
  phrase(eapi:version(version(_, _, Rank, _, _, _, _)), Codes, []),
  Rank == 1.

test(version_with_suffix_pre, [nondet]) :-
  atom_codes('1.0_pre3', Codes),
  phrase(eapi:version(version(_, _, Rank, _, _, _, _)), Codes, []),
  Rank == 2.

test(version_with_suffix_rc, [nondet]) :-
  atom_codes('1.0_rc1', Codes),
  phrase(eapi:version(version(_, _, Rank, _, _, _, _)), Codes, []),
  Rank == 3.

test(version_with_suffix_p, [nondet]) :-
  atom_codes('1.0_p1', Codes),
  phrase(eapi:version(version(_, _, Rank, _, _, _, _)), Codes, []),
  Rank == 5.

test(version_none_on_empty, [true(V == version_none)]) :-
  phrase(eapi:version0(V), [], []).

:- end_tests(eapi_version_parsing).


% -----------------------------------------------------------------------------
%  EAPI version comparison tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_version_compare).

test(equal_versions, [nondet]) :-
  atom_codes('1.0', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(=, V1, V2).

test(less_than, [nondet]) :-
  atom_codes('1.0', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('2.0', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(<, V1, V2).

test(greater_than, [nondet]) :-
  atom_codes('3.0', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('2.0', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(>, V1, V2).

test(revision_ordering, [nondet]) :-
  atom_codes('1.0-r1', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0-r2', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(<, V1, V2).

test(suffix_ordering_alpha_before_beta, [nondet]) :-
  atom_codes('1.0_alpha1', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0_beta1', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(<, V1, V2).

test(suffix_ordering_rc_before_release, [nondet]) :-
  atom_codes('1.0_rc1', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(<, V1, V2).

test(suffix_ordering_release_before_p, [nondet]) :-
  atom_codes('1.0', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0_p1', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(<, V1, V2).

test(pms_suffix_chain, [true(Order == [VA,VB,VC,VD,VE,VF]), nondet]) :-
  atom_codes('1.0_alpha1', CA), phrase(eapi:version(VA), CA, []),
  atom_codes('1.0_beta1', CB), phrase(eapi:version(VB), CB, []),
  atom_codes('1.0_pre1', CC), phrase(eapi:version(VC), CC, []),
  atom_codes('1.0_rc1', CD), phrase(eapi:version(VD), CD, []),
  atom_codes('1.0', CE), phrase(eapi:version(VE), CE, []),
  atom_codes('1.0_p1', CF), phrase(eapi:version(VF), CF, []),
  msort([VF, VD, VB, VE, VC, VA], Order).

:- end_tests(eapi_version_compare).


% -----------------------------------------------------------------------------
%  EAPI operator parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_operator_parsing).

test(greater_equal, [true(Op == greaterequal)]) :-
  phrase(eapi:operator(Op), [62,61], []).

test(smaller_equal, [true(Op == smallerequal)]) :-
  phrase(eapi:operator(Op), [60,61], []).

test(greater, [true(Op == greater)]) :-
  phrase(eapi:operator(Op), [62], []).

test(smaller, [true(Op == smaller)]) :-
  phrase(eapi:operator(Op), [60], []).

test(equal, [true(Op == equal)]) :-
  phrase(eapi:operator(Op), [61], []).

test(tilde, [true(Op == tilde)]) :-
  phrase(eapi:operator(Op), [126], []).

test(none, [true(Op == none)]) :-
  phrase(eapi:operator(Op), [], []).

:- end_tests(eapi_operator_parsing).


% -----------------------------------------------------------------------------
%  EAPI blocking parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_blocking_parsing).

test(strong_block, [true(B == strong)]) :-
  phrase(eapi:blocking(B), [33,33], []).

test(weak_block, [true(B == weak)]) :-
  phrase(eapi:blocking(B), [33], []).

test(no_block, [true(B == no)]) :-
  phrase(eapi:blocking(B), [], []).

:- end_tests(eapi_blocking_parsing).


% -----------------------------------------------------------------------------
%  EAPI keyword parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_keyword_parsing).

test(stable_keyword, [true(K == stable(amd64))]) :-
  atom_codes(amd64, Codes),
  phrase(eapi:keyword(K), Codes, []).

test(unstable_keyword, [true(K == unstable(amd64))]) :-
  atom_codes('~amd64', Codes),
  phrase(eapi:keyword(K), Codes, []).

test(broken_keyword, [true(K == broken(amd64))]) :-
  atom_codes('-amd64', Codes),
  phrase(eapi:keyword(K), Codes, []).

test(keyword_list, [true(length(Ks, 3))]) :-
  atom_codes('amd64 ~arm64 -x86', Codes),
  phrase(eapi:keywords(Ks), Codes, []).

:- end_tests(eapi_keyword_parsing).


% -----------------------------------------------------------------------------
%  EAPI slot parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_slot_parsing).

test(simple_slot, [true(S == [slot('0')])]) :-
  atom_codes('0', Codes),
  phrase(eapi:slot(S), Codes, []).

test(slot_with_subslot, [true(S == [slot('5'), subslot('3')])]) :-
  atom_codes('5/3', Codes),
  phrase(eapi:slot(S), Codes, []).

test(slot_restriction_star, [true(S == [any_different_slot])]) :-
  phrase(eapi:slot_restriction(S), [58,42], []).

test(slot_restriction_equal, [true(S == [any_same_slot])]) :-
  phrase(eapi:slot_restriction(S), [58,61], []).

test(slot_restriction_empty, [true(S == [])]) :-
  phrase(eapi:slot_restriction(S), [], []).

:- end_tests(eapi_slot_parsing).


% -----------------------------------------------------------------------------
%  EAPI IUSE parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_iuse_parsing).

test(simple_iuse, [true(I == [test])]) :-
  atom_codes('test', Codes),
  phrase(eapi:iuse(repo://entry, I), Codes, []).

test(iuse_with_plus, [true(I == [plus(ssl)])]) :-
  atom_codes('+ssl', Codes),
  phrase(eapi:iuse(repo://entry, I), Codes, []).

test(iuse_with_minus, [true(I == [minus(debug)])]) :-
  atom_codes('-debug', Codes),
  phrase(eapi:iuse(repo://entry, I), Codes, []).

test(iuse_list, [true(length(I, 3))]) :-
  atom_codes('+ssl -debug test', Codes),
  phrase(eapi:iuse(repo://entry, I), Codes, []).

test(iuse_empty, [true(I == [])]) :-
  phrase(eapi:iuse(repo://entry, I), [], []).

:- end_tests(eapi_iuse_parsing).


% -----------------------------------------------------------------------------
%  EAPI package name parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_package_parsing).

test(simple_name, [true(P == gcc)]) :-
  atom_codes(gcc, Codes),
  phrase(eapi:package(P), Codes, []).

test(hyphenated_name, [true(P == 'libxml2-utils')]) :-
  atom_codes('libxml2-utils', Codes),
  phrase(eapi:package(P), Codes, []).

test(name_with_plus, [true(P == 'gtk+')]) :-
  atom_codes('gtk+', Codes),
  phrase(eapi:package(P), Codes, []).

:- end_tests(eapi_package_parsing).


% -----------------------------------------------------------------------------
%  EAPI category parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_category_parsing).

test(simple_category, [true(C == 'sys-apps')]) :-
  atom_codes('sys-apps', Codes),
  phrase(eapi:category(C), Codes, []).

test(virtual_category, [true(C == virtual)]) :-
  atom_codes(virtual, Codes),
  phrase(eapi:category(C), Codes, []).

:- end_tests(eapi_category_parsing).


% -----------------------------------------------------------------------------
%  EAPI dependency parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_dependency_parsing).

test(simple_dep, [true(D == [package_dependency(install, no, 'dev-libs', openssl, none, version_none, [], [])])]) :-
  atom_codes('dev-libs/openssl', Codes),
  phrase(eapi:depend(repo://entry, D), Codes, []).

test(versioned_dep, [nondet]) :-
  atom_codes('>=dev-libs/openssl-1.1.0', Codes),
  phrase(eapi:depend(repo://entry, [package_dependency(install, no, 'dev-libs', openssl, greaterequal, V, [], [])]), Codes, []),
  V \== version_none.

test(blocked_dep, [true(D == [package_dependency(install, weak, 'dev-libs', foo, none, version_none, [], [])])]) :-
  atom_codes('!dev-libs/foo', Codes),
  phrase(eapi:depend(repo://entry, D), Codes, []).

test(strong_blocked_dep, [true(D == [package_dependency(install, strong, 'dev-libs', foo, none, version_none, [], [])])]) :-
  atom_codes('!!dev-libs/foo', Codes),
  phrase(eapi:depend(repo://entry, D), Codes, []).

test(multiple_deps, [true(length(D, 2))]) :-
  atom_codes('dev-libs/openssl dev-libs/glib', Codes),
  phrase(eapi:depend(repo://entry, D), Codes, []).

test(use_conditional_dep, [nondet]) :-
  atom_codes('ssl? ( dev-libs/openssl )', Codes),
  phrase(eapi:depend(repo://entry, [use_conditional_group(positive, ssl, _, _)]), Codes, []).

test(any_of_dep, [nondet]) :-
  atom_codes('|| ( dev-libs/openssl dev-libs/libressl )', Codes),
  phrase(eapi:depend(repo://entry, [any_of_group(_)]), Codes, []).

test(empty_dep, [true(D == [])]) :-
  phrase(eapi:depend(repo://entry, D), [], []).

:- end_tests(eapi_dependency_parsing).


% -----------------------------------------------------------------------------
%  EAPI metadata key=value parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_keyvalue_parsing).

test(parse_description, [true(M == description(["A test package"]))]) :-
  atom_codes('DESCRIPTION=A test package', Codes),
  eapi:parse(metadata, repo://entry, Codes, M).

test(parse_slot, [true(M == slot([slot('0')]))]) :-
  atom_codes('SLOT=0', Codes),
  eapi:parse(metadata, repo://entry, Codes, M).

test(parse_eapi, [nondet]) :-
  atom_codes('EAPI=8', Codes),
  eapi:parse(metadata, repo://entry, Codes, eapi(_)).

test(parse_keywords, [nondet]) :-
  atom_codes('KEYWORDS=amd64 ~arm64', Codes),
  eapi:parse(metadata, repo://entry, Codes, keywords(Ks)),
  length(Ks, 2).

test(parse_defined_phases, [nondet]) :-
  atom_codes('DEFINED_PHASES=compile configure install', Codes),
  eapi:parse(metadata, repo://entry, Codes, defined_phases(P)),
  length(P, 3).

:- end_tests(eapi_keyvalue_parsing).


% -----------------------------------------------------------------------------
%  EAPI helper tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_helpers).

test(strip_use_default_plus, [true(U == ssl)]) :-
  eapi:strip_use_default(plus(ssl), U).

test(strip_use_default_minus, [true(U == debug)]) :-
  eapi:strip_use_default(minus(debug), U).

test(strip_use_default_bare, [true(U == test)]) :-
  eapi:strip_use_default(test, U).

test(check_prefix_atom_match) :-
  eapi:check_prefix_atom(python_targets, python_targets_python3_12).

test(check_prefix_atom_no_match, [fail]) :-
  eapi:check_prefix_atom(python_targets, ruby_targets_ruby31).

test(check_use_expand_atom_match) :-
  eapi:check_use_expand_atom(python_targets_python3_12).

test(check_use_expand_atom_no_match, [fail]) :-
  eapi:check_use_expand_atom(ssl).

test(version2numberlist, [true(N == [1,2,3])]) :-
  eapi:version2numberlist('1.2.3', N).

test(version2numberlist_empty, [true(N == [])]) :-
  eapi:version2numberlist('', N).

test(split_ws_atoms, [true(A == [foo, bar, baz])]) :-
  eapi:split_ws_atoms("foo bar baz", A).

test(split_ws_atoms_empty, [true(A == [])]) :-
  eapi:split_ws_atoms("", A).

test(parse_iuse_search_plus, [true((Sign == plus, Pat == foo))]) :-
  eapi:parse_iuse_search_value('+foo', Sign, Pat).

test(parse_iuse_search_minus, [true((Sign == minus, Pat == bar))]) :-
  eapi:parse_iuse_search_value('-bar', Sign, Pat).

test(parse_iuse_search_bare, [true((Sign == any, Pat == baz))]) :-
  eapi:parse_iuse_search_value(baz, Sign, Pat).

:- end_tests(eapi_helpers).


% -----------------------------------------------------------------------------
%  EAPI manifest parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_manifest_parsing).

test(parse_ebuild_manifest, [nondet]) :-
  atom_codes('EBUILD foo-1.0.ebuild 1234 SHA512:abc', Codes),
  eapi:parse(manifest, repo://entry, Codes, manifest(ebuild, _, _, _)).

test(parse_dist_manifest, [nondet]) :-
  atom_codes('DIST foo-1.0.tar.gz 56789 SHA512:def', Codes),
  eapi:parse(manifest, repo://entry, Codes, manifest(dist, _, Size, _)),
  Size == 56789.

:- end_tests(eapi_manifest_parsing).


% -----------------------------------------------------------------------------
%  EAPI SRC_URI / URI parsing tests
% -----------------------------------------------------------------------------
%
% Covers the eapi:uri/3 DCG, especially the `->` rename form (PMS 9 §7.3.2).
% The historical operator-precedence bug in the inline if-then-else left the
% Proto/Base atoms unbound on renamed distfiles, which then propagated to
% download:resolve_mirror_uri/3 -> atom_string/2 -> instantiation_error in
% the upstream-fallback path. These tests pin both the rename and non-rename
% shapes so the bug cannot regress silently.

:- begin_tests(eapi_uri_parsing).

test(plain_uri, [nondet]) :-
  atom_codes('https://example.com/foo-1.0.tar.gz', Codes),
  phrase(eapi:uri(uri(P, B, L)), Codes, []),
  P == https,
  B == 'example.com/foo-1.0.tar.gz',
  L == 'foo-1.0.tar.gz'.

test(plain_uri_local_is_basename, [true(L == 'bar.tar.xz'), nondet]) :-
  atom_codes('ftp://mirror.example.org/pub/path/bar.tar.xz', Codes),
  phrase(eapi:uri(uri(_, _, L)), Codes, []).

test(mirror_uri, [nondet]) :-
  atom_codes('mirror://gnu/emacs/emacs-29.4.tar.xz', Codes),
  phrase(eapi:uri(uri(P, B, L)), Codes, []),
  P == mirror,
  B == 'gnu/emacs/emacs-29.4.tar.xz',
  L == 'emacs-29.4.tar.xz'.

% Renamed distfile (the actual regression case). Every field must be a
% ground atom -- veracrypt-1.26.20 used to come back as uri(_,_,'veracrypt-
% 1.26.20.tar.gz') with Proto and Base unbound.

test(renamed_uri_binds_all_fields, [nondet]) :-
  atom_codes('https://github.com/veracrypt/VeraCrypt/archive/VeraCrypt_1.26.20.tar.gz -> veracrypt-1.26.20.tar.gz', Codes),
  phrase(eapi:uri(uri(P, B, L)), Codes, []),
  ground(P), ground(B), ground(L),
  P == https,
  B == 'github.com/veracrypt/VeraCrypt/archive/VeraCrypt_1.26.20.tar.gz',
  L == 'veracrypt-1.26.20.tar.gz'.

test(renamed_uri_proto_not_unbound, [nondet]) :-
  atom_codes('https://example.org/upstream-name.tgz -> local-name.tgz', Codes),
  phrase(eapi:uri(uri(P, _, _)), Codes, []),
  nonvar(P).

test(renamed_uri_base_not_unbound, [nondet]) :-
  atom_codes('https://example.org/upstream-name.tgz -> local-name.tgz', Codes),
  phrase(eapi:uri(uri(_, B, _)), Codes, []),
  nonvar(B).

test(renamed_uri_local_matches_arrow_target, [nondet]) :-
  atom_codes('https://example.org/very/long/path/upstream-original.tar.gz -> short.tar.gz', Codes),
  phrase(eapi:uri(uri(_, _, L)), Codes, []),
  L == 'short.tar.gz'.

% Non-prototyped URI (rare, but covered by the second eapi:uri/3 clause).

test(nonprototyped_uri, [nondet]) :-
  atom_codes('plain-distfile.tar.gz', Codes),
  phrase(eapi:uri(uri(P, B, L)), Codes, []),
  P == '',
  B == '',
  L == 'plain-distfile.tar.gz'.

:- end_tests(eapi_uri_parsing).


% -----------------------------------------------------------------------------
%  Version domain normalization tests
% -----------------------------------------------------------------------------

:- begin_tests(version_domain_normalize).

test(normalize_none, [true(D == none)]) :-
  version_domain:domain_normalize(none, D).

test(normalize_unknown, [true(D == none)]) :-
  version_domain:domain_normalize(something_else, D).

test(normalize_domain_identity, [nondet]) :-
  version_domain:domain_normalize(
    version_domain(any, [bound(smallerequal, v1)]),
    version_domain(any, [bound(smallerequal, v1)])
  ).

test(normalize_slot_any, [true(S == any)]) :-
  version_domain:normalize_slot_domain(any, S).

test(normalize_slot_sorts, [true(S == slots([a, b, c]))]) :-
  version_domain:normalize_slot_domain(slots([c, a, b]), S).

:- end_tests(version_domain_normalize).


% -----------------------------------------------------------------------------
%  Version domain meet tests
% -----------------------------------------------------------------------------

:- begin_tests(version_domain_meet).

test(meet_none_left, [true(D == version_domain(any, [bound(equal, v1)]))]) :-
  version_domain:domain_meet(
    none,
    version_domain(any, [bound(equal, v1)]),
    D
  ).

test(meet_none_right, [true(D == version_domain(any, [bound(equal, v1)]))]) :-
  version_domain:domain_meet(
    version_domain(any, [bound(equal, v1)]),
    none,
    D
  ).

test(meet_both_none, [true(D == none)]) :-
  version_domain:domain_meet(none, none, D).

test(meet_bounds_union) :-
  version_domain:domain_meet(
    version_domain(any, [bound(equal, v1)]),
    version_domain(any, [bound(smallerequal, v2)]),
    version_domain(any, Bounds)
  ),
  length(Bounds, 2).

test(meet_slot_intersection) :-
  version_domain:domain_meet(
    version_domain(slots([a, b, c]), []),
    version_domain(slots([b, c, d]), []),
    version_domain(slots(S), _)
  ),
  S == [b, c].

test(meet_empty_slot_intersection_fails, [fail]) :-
  version_domain:domain_meet(
    version_domain(slots([a]), []),
    version_domain(slots([b]), []),
    _
  ).

:- end_tests(version_domain_meet).


% -----------------------------------------------------------------------------
%  Version domain consistency tests
% -----------------------------------------------------------------------------

:- begin_tests(version_domain_consistency).

test(empty_slots_inconsistent) :-
  version_domain:domain_inconsistent(version_domain(slots([]), [])).

test(any_slots_consistent, [fail]) :-
  version_domain:domain_inconsistent(version_domain(any, [])).

test(non_domain_consistent, [fail]) :-
  version_domain:domain_inconsistent(something_else).

test(two_different_exact_bounds_inconsistent) :-
  version_domain:bounds_inconsistent([bound(equal, v1), bound(equal, v2)]).

test(single_exact_bound_consistent, [fail]) :-
  version_domain:bounds_inconsistent([bound(equal, v1)]).

:- end_tests(version_domain_consistency).


% -----------------------------------------------------------------------------
%  Version domain bound normalization tests
% -----------------------------------------------------------------------------

:- begin_tests(version_domain_bound_ops).

test(normalize_smallerorequal, [true(N == smallerequal)]) :-
  version_domain:normalize_bound_op(smallerorequal, N).

test(normalize_smallerequal, [true(N == smallerequal)]) :-
  version_domain:normalize_bound_op(smallerequal, N).

test(normalize_smaller, [true(N == smaller)]) :-
  version_domain:normalize_bound_op(smaller, N).

test(normalize_equal, [true(N == equal)]) :-
  version_domain:normalize_bound_op(equal, N).

test(normalize_greater_drops, [true(N == none)]) :-
  version_domain:normalize_bound_op(greater, N).

test(normalize_greaterequal_drops, [true(N == none)]) :-
  version_domain:normalize_bound_op(greaterequal, N).

test(normalize_unknown_drops, [true(N == none)]) :-
  version_domain:normalize_bound_op(random_op, N).

:- end_tests(version_domain_bound_ops).


% -----------------------------------------------------------------------------
%  Version constraint holds tests
% -----------------------------------------------------------------------------

:- begin_tests(version_constraint_holds).

test(none_bound_always_holds) :-
  version_domain:version_constraint_holds(anything, bound(none, anything)).

test(equal_holds) :-
  version_domain:version_constraint_holds(v1, bound(equal, v1)).

test(equal_fails, [fail]) :-
  version_domain:version_constraint_holds(v1, bound(equal, v2)).

test(notequal_holds) :-
  version_domain:version_constraint_holds(v1, bound(notequal, v2)).

test(notequal_fails, [fail]) :-
  version_domain:version_constraint_holds(v1, bound(notequal, v1)).

test(unknown_op_non_blocking) :-
  version_domain:version_constraint_holds(v1, bound(weird_op, v2)).

:- end_tests(version_constraint_holds).


% -----------------------------------------------------------------------------
%  Canon slot tests
% -----------------------------------------------------------------------------

:- begin_tests(version_domain_canon_slot).

test(atom_passthrough, [true(S == foo)]) :-
  version_domain:canon_slot(foo, S).

test(integer_to_atom, [true(S == '42')]) :-
  version_domain:canon_slot(42, S).

test(float_to_atom) :-
  version_domain:canon_slot(3.2, S),
  atom(S).

:- end_tests(version_domain_canon_slot).


% -----------------------------------------------------------------------------
%  Slot domain meet tests
% -----------------------------------------------------------------------------

:- begin_tests(version_domain_slot_meet).

test(any_any, [true(D == any)]) :-
  version_domain:meet_slot_domains(any, any, D).

test(any_slots, [true(D == slots([a]))]) :-
  version_domain:meet_slot_domains(any, slots([a]), D).

test(slots_any, [true(D == slots([b]))]) :-
  version_domain:meet_slot_domains(slots([b]), any, D).

test(slots_intersection, [true(D == slots([b]))]) :-
  version_domain:meet_slot_domains(slots([a,b,c]), slots([b,d]), D).

:- end_tests(version_domain_slot_meet).


% -----------------------------------------------------------------------------
%  Constraint module tests
% -----------------------------------------------------------------------------

:- begin_tests(constraint_identification).

test(is_constraint_true) :-
  constraint:is_constraint(constraint(foo)).

test(is_constraint_false, [fail]) :-
  constraint:is_constraint(not_a_constraint).

:- end_tests(constraint_identification).


:- begin_tests(constraint_unification).

test(unify_ordset_new) :-
  empty_assoc(C0),
  constraint:unify_constraints(constraint(k:{ordset([b,a,c])}), C0, C1),
  get_assoc(k, C1, ordset(V)),
  V == [a,b,c].

test(unify_ordset_merge) :-
  empty_assoc(C0),
  put_assoc(k, C0, ordset([a,c]), C1),
  constraint:unify_constraints(constraint(k:{ordset([b,d])}), C1, C2),
  get_assoc(k, C2, ordset(V)),
  V == [a,b,c,d].

test(unify_atom_new) :-
  empty_assoc(C0),
  constraint:unify_constraints(constraint(k:{hello}), C0, C1),
  get_assoc(k, C1, V),
  V == hello.

:- end_tests(constraint_unification).


:- begin_tests(constraint_conversion).

test(empty_to_list, [true(L == [])]) :-
  empty_assoc(A),
  constraint:constraints_to_list(A, L).

:- end_tests(constraint_conversion).


% -----------------------------------------------------------------------------
%  EAPI VDB slot parsing tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_vdb_slot).

test(simple_slot_line, [true(T == [slot('0')])]) :-
  eapi:parse_vdb_slot_line('0', T).

test(slot_with_subslot, [true(T == [slot('5'), subslot('3')])]) :-
  eapi:parse_vdb_slot_line('5/3', T).

:- end_tests(eapi_vdb_slot).


% -----------------------------------------------------------------------------
%  EAPI metadata normalization tests
% -----------------------------------------------------------------------------

:- begin_tests(eapi_normalize_metadata).

test(slot_number_normalized) :-
  eapi:normalize_entry_metadata(slot, slot(3), slot('3')).

test(non_slot_passthrough, [true(I == depend(foo))]) :-
  eapi:normalize_entry_metadata(depend, depend(foo), I).

:- end_tests(eapi_normalize_metadata).


% =============================================================================
%  Kahn topological sort tests
% =============================================================================

:- begin_tests(kahn_toposort).

test(empty_graph, [true(Order-Cyclic == []-false)]) :-
  empty_assoc(E),
  kahn:toposort([], E, Order, Cyclic).

test(single_node, [true(Order-Cyclic == [a]-false)]) :-
  list_to_assoc([a-[]], E),
  kahn:toposort([a], E, Order, Cyclic).

test(linear_chain, [true(Order-Cyclic == [a,b,c]-false)]) :-
  list_to_assoc([a-[b], b-[c], c-[]], E),
  kahn:toposort([a,b,c], E, Order, Cyclic).

test(diamond_dag, [true(Cyclic == false)]) :-
  list_to_assoc([a-[b,c], b-[d], c-[d], d-[]], E),
  kahn:toposort([a,b,c,d], E, Order, Cyclic),
  Order = [a|_],
  last(Order, d).

test(two_component, [true(Cyclic == false)]) :-
  list_to_assoc([a-[b], b-[], x-[y], y-[]], E),
  kahn:toposort([a,b,x,y], E, Order, Cyclic),
  length(Order, 4).

test(simple_cycle, [true(Cyclic == true)]) :-
  list_to_assoc([a-[b], b-[a]], E),
  kahn:toposort([a,b], E, Order, Cyclic),
  length(Order, 2).

test(partial_cycle, [true(Cyclic == true)]) :-
  list_to_assoc([a-[b], b-[c], c-[b], d-[]], E),
  kahn:toposort([a,b,c,d], E, Order, Cyclic),
  memberchk(d, Order).

test(self_loop, [true(Cyclic == true)]) :-
  list_to_assoc([a-[a]], E),
  kahn:toposort([a], E, Order, Cyclic),
  length(Order, 1).

:- end_tests(kahn_toposort).


% =============================================================================
%  Sanitize validation tests
% =============================================================================

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


% =============================================================================
%  Depclean structural predicate tests
% =============================================================================

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


% =============================================================================
%  USE helper predicate tests
% =============================================================================

:- begin_tests(use_empty_state).

test(empty_state, [true(S == use_state([],[]))]) :-
  use:empty_use_state(S).

:- end_tests(use_empty_state).


:- begin_tests(use_normalize_bwu).

test(already_state, [true(R == use_state([a,b],[c]))]) :-
  use:normalize_build_with_use(use_state([b,a],[c]), R).

test(list_form, [true(R == use_state([x],[y]))]) :-
  use:normalize_build_with_use([required(x), naf(required(y))], R).

test(unknown_form, [true(R == use_state([],[]))]) :-
  use:normalize_build_with_use(42, R).

:- end_tests(use_normalize_bwu).


:- begin_tests(use_context_bwu).

test(with_bwu, [true(S == use_state([a],[]))]) :-
  use:context_build_with_use_state([build_with_use:use_state([a],[])], S).

test(without_bwu, [true(S == use_state([],[]))]) :-
  use:context_build_with_use_state([other_key:val], S).

test(empty_ctx, [true(S == use_state([],[]))]) :-
  use:context_build_with_use_state([], S).

:- end_tests(use_context_bwu).


:- begin_tests(use_bwu_requirements).

test(state_form, [true(En-Dis == [a,b]-[c])]) :-
  use:build_with_use_requirements(use_state([b,a],[c]), En, Dis).

test(list_form_enable, [true(En == [x])]) :-
  use:build_with_use_requirements([required(x)], En, _).

test(list_form_disable_naf, [true(Dis == [y])]) :-
  use:build_with_use_requirements([naf(required(y))], _, Dis).

test(list_form_disable_assumed, [true(Dis == [z])]) :-
  use:build_with_use_requirements([assumed(minus(z))], _, Dis).

:- end_tests(use_bwu_requirements).


:- begin_tests(use_candidate_bwu_memo).

test(merge_empty_ctx_with_memo, [true(B == use_state([wayland],[]))]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([wayland], []))),
  use:merge_memo_candidate_bwu('dev-qt', qtbase, use_state([], []), B).

test(merge_union_ctx_and_memo, [true(B == use_state([gui,wayland],[]))]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([wayland], []))),
  use:merge_memo_candidate_bwu('dev-qt', qtbase, use_state([gui], []), B).

test(accumulate_two_edges, [true(M == use_state([icu,wayland],[]))]) :-
  use:clear_bwu_cross_dep_memos,
  use:accumulate_candidate_bwu('dev-qt', qtbase, use_state([wayland], [])),
  use:accumulate_candidate_bwu('dev-qt', qtbase, use_state([icu], [])),
  memo:candidate_bwu_('dev-qt', qtbase, M).

test(seed_run_before_install_phase, [true(M == use_state([dbus],[]))]) :-
  use:clear_bwu_cross_dep_memos,
  InstallDeps = [grouped_package_dependency(no, 'dev-libs', glib,
      [package_dependency(install, no, 'dev-libs', glib, none, version_none, [], [])])],
  RunDeps = [grouped_package_dependency(no, 'dev-libs', glib,
      [package_dependency(run, no, 'dev-libs', glib, none, version_none, [],
                          [use(enable(dbus), positive)])])],
  candidate:seed_bwu_memo_from_dep_tree(InstallDeps),
  candidate:seed_bwu_memo_from_dep_tree(RunDeps),
  memo:candidate_bwu_('dev-libs', glib, M).

:- end_tests(use_candidate_bwu_memo).


:- begin_tests(equality_use_pin_propagation).

test(equal_provider_enabled_enables_self, [true(Mode == enable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  candidate:equality_pin_from_usedep('dev-qt', qtbase, use(equal(icu), negative), icu, Mode).

test(equal_provider_disabled_disables_self, [true(Mode == disable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([], [icu]))),
  candidate:equality_pin_from_usedep('dev-qt', qtbase, use(equal(icu), positive), icu, Mode).

test(inverse_provider_enabled_disables_self, [true(Mode == disable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  candidate:equality_pin_from_usedep('dev-qt', qtbase, use(inverse(icu), negative), icu, Mode).

test(inverse_provider_disabled_enables_self, [true(Mode == enable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([], [icu]))),
  candidate:equality_pin_from_usedep('dev-qt', qtbase, use(inverse(icu), positive), icu, Mode).

test(unpinned_provider_yields_no_pin, [fail]) :-
  use:clear_bwu_cross_dep_memos,
  candidate:equality_pin_from_usedep('dev-qt', qtbase, use(equal(icu), negative), icu, _Mode).

test(term_walk_collects_top_level, [true(Pairs == [icu-enable])]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  Term = package_dependency(install, no, 'dev-qt', qtbase, tilde, version_none, [],
                            [use(equal(icu), negative), use(enable(network), positive)]),
  findall(F-M, candidate:equality_pin_from_term(Term, F, M), Pairs).

test(term_walk_descends_all_of_group, [true(Pairs == [icu-enable])]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  Term = all_of_group([package_dependency(install, no, 'dev-qt', qtbase, tilde, version_none, [],
                                          [use(equal(icu), negative)])]),
  findall(F-M, candidate:equality_pin_from_term(Term, F, M), Pairs).

test(conditional_group_not_descended, [true(Pairs == [])]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  Term = use_conditional_group(positive, someflag, none,
           [package_dependency(install, no, 'dev-qt', qtbase, tilde, version_none, [],
                               [use(equal(icu), negative)])]),
  findall(F-M, candidate:equality_pin_from_term(Term, F, M), Pairs).

test(pin_conflict_detected) :-
  candidate:pin_flags_conflict([icu], [icu]).

test(pin_no_conflict, [fail]) :-
  candidate:pin_flags_conflict([icu], [foo]).

test(seed_conditional_minus_use_recurses, [fail]) :-
  candidate:seed_use_conditional_inactive(positive, minus(foo), some://entry).

test(seed_conditional_non_entry_recurses, [fail]) :-
  candidate:seed_use_conditional_inactive(positive, foo, not_an_entry).

:- end_tests(equality_use_pin_propagation).


% =============================================================================
%  Builder base USE state matches planner (portage-ng#22)
% =============================================================================
%
% The builder's base USE string (ebuild_exec:collect_use_string/4) must agree
% with the planner's view of each IUSE flag. Previously the builder folded the
% raw iuse/2 facts with a last-wins dedup, which picked the wrong polarity for
% flags declared with conflicting facts (e.g. x11-libs/wxGTK exposes `X` as
% [positive:ebuild, negative:default], so last-wins gave `-X` while the planner
% resolved `+X`, breaking REQUIRED_USE="spell? ( X )" at setup). The fix routes
% the base polarity through use:effective_use_for_entry/3.
%
% These tests are KB-independent: they pre-seed memo:eff_use_cache_/4 so the
% effective lookup short-circuits without needing cache:ordered_entry/5 (absent
% in CI).

:- begin_tests(builder_base_use_state).

test(prefers_effective_positive, [true(S == positive)]) :-
  retractall(memo:eff_use_cache_(testrepo, 'cat/p-1', _, _)),
  assertz(memo:eff_use_cache_(testrepo, 'cat/p-1', 'X', positive)),
  ( ebuild_exec:base_use_state(testrepo://'cat/p-1', 'X', S)
  -> retractall(memo:eff_use_cache_(testrepo, 'cat/p-1', _, _))
  ;  retractall(memo:eff_use_cache_(testrepo, 'cat/p-1', _, _)), fail
  ).

test(prefers_effective_negative, [true(S == negative)]) :-
  retractall(memo:eff_use_cache_(testrepo, 'cat/p-1', _, _)),
  assertz(memo:eff_use_cache_(testrepo, 'cat/p-1', spell, negative)),
  ( ebuild_exec:base_use_state(testrepo://'cat/p-1', spell, S)
  -> retractall(memo:eff_use_cache_(testrepo, 'cat/p-1', _, _))
  ;  retractall(memo:eff_use_cache_(testrepo, 'cat/p-1', _, _)), fail
  ).

:- end_tests(builder_base_use_state).


:- begin_tests(use_iuse_assoc).

test(single_pair, [true(V == positive)]) :-
  use:iuse_default_pairs_to_assoc([foo-positive], M),
  get_assoc(foo, M, V).

test(positive_wins, [true(V == positive)]) :-
  use:iuse_default_pairs_to_assoc([foo-negative, foo-positive], M),
  get_assoc(foo, M, V).

test(negative_no_override, [true(V == negative)]) :-
  use:iuse_default_pairs_to_assoc([foo-negative, foo-negative], M),
  get_assoc(foo, M, V).

test(empty_list) :-
  use:iuse_default_pairs_to_assoc([], M),
  empty_assoc(M).

:- end_tests(use_iuse_assoc).


:- begin_tests(use_symmetric_diff).

test(different_lists) :-
  use:symmetric_diff_nonempty([a,b], [b,c]).

test(same_lists, [fail]) :-
  use:symmetric_diff_nonempty([a,b], [a,b]).

test(both_empty, [fail]) :-
  use:symmetric_diff_nonempty([], []).

test(one_empty) :-
  use:symmetric_diff_nonempty([a], []).

:- end_tests(use_symmetric_diff).


:- begin_tests(use_abi_x86_flag).

test(valid_abi_flag) :-
  use:is_abi_x86_flag(abi_x86_64).

test(valid_abi_flag_32) :-
  use:is_abi_x86_flag(abi_x86_32).

test(not_abi_flag, [fail]) :-
  use:is_abi_x86_flag(python_targets_python3_12).

test(not_atom, [fail]) :-
  use:is_abi_x86_flag(123).

:- end_tests(use_abi_x86_flag).


% =============================================================================
%  Linkage tests
% =============================================================================

:- begin_tests(linkage_is_linkable).

test(shared_object) :-
  linkage:is_linkable('/usr/lib64/libz.so').

test(versioned_so) :-
  linkage:is_linkable('/usr/lib64/libz.so.1.2.13').

test(usr_bin) :-
  linkage:is_linkable('/usr/bin/bash').

test(usr_sbin) :-
  linkage:is_linkable('/usr/sbin/sshd').

test(usr_lib_prefix) :-
  linkage:is_linkable('/usr/lib64/something').

test(bin) :-
  linkage:is_linkable('/bin/sh').

test(sbin) :-
  linkage:is_linkable('/sbin/init').

test(lib_prefix) :-
  linkage:is_linkable('/lib64/ld-linux-x86-64.so.2').

test(reject_etc, [fail]) :-
  linkage:is_linkable('/etc/portage/make.conf').

test(reject_var, [fail]) :-
  linkage:is_linkable('/var/log/messages').

test(reject_share, [fail]) :-
  linkage:is_linkable('/usr/share/doc/readme.txt').

:- end_tests(linkage_is_linkable).


% =============================================================================
%  Version domain additional tests
% =============================================================================

:- begin_tests(version_normalize_term).

test(var_passthrough) :-
  version_domain:normalize_version_term(X, Y),
  var(Y),
  X == Y.

test(wildcard_atom, [true(Ver == version([0],'',4,0,'',0,'1.0.*'))]) :-
  version_domain:normalize_version_term('1.0.*', Ver).

test(compound_passthrough, [true(Ver == foo(bar))]) :-
  version_domain:normalize_version_term(foo(bar), Ver).

test(version_eq_strip, [true(Ver == myver)]) :-
  version_domain:normalize_version_term(version(a,b,c,d,e,f,g)=myver, Ver).

:- end_tests(version_normalize_term).


:- begin_tests(version_slot_domain_from_reqs).

test(empty_reqs, [true(D == any)]) :-
  version_domain:slot_domain_from_reqs([], D).

test(single_slot_req, [true(D == slots(['3']))]) :-
  version_domain:slot_domain_from_reqs([[slot(3)]], D).

test(any_same_slot, [true(D == any)]) :-
  version_domain:slot_domain_from_reqs([[any_same_slot]], D).

test(any_different_slot, [true(D == any)]) :-
  version_domain:slot_domain_from_reqs([[any_different_slot]], D).

:- end_tests(version_slot_domain_from_reqs).


% =============================================================================
%  Bracketed-USE rebuild for already-installed packages
% =============================================================================

% Regression test for the planner gap that caused podman → iptables[nftables]
% to schedule libnftnl/libmnl AFTER iptables. Root cause: rule(:install/:run
% ?{Ctx}) short-circuited to []/[reinstall] for already-installed packages
% without checking whether the requested build_with_use matched the VDB-
% recorded USE. Fix: when BWU mismatches, re-emit as a transactional :update
% with `replaces(pkg://Ebuild)` so candidate:resolve walks DEPEND/BDEPEND
% under the new BWU and the planner schedules newly-required deps before
% the rebuild.

:- begin_tests(rules_install_run_bwu_rebuild).

% Find an installed package with at least one IUSE flag the VDB build
% does NOT have enabled, so we can construct a real BWU mismatch. We
% prefer net-firewall/iptables (has nftables IUSE) and fall back to any
% installed entry that satisfies the predicate.
test_setup_pick(pkg://Ebuild, Flag) :-
  ( query:search([category('net-firewall'),name(iptables),installed(true)], pkg://Ebuild),
    Flag = nftables,
    cache:entry_metadata(pkg, Ebuild, iuse, Flag),
    \+ cache:entry_metadata(pkg, Ebuild, use, Flag), !
  ; query:search([category(C),name(N),installed(true)], pkg://Ebuild),
    cache:entry_metadata(pkg, Ebuild, iuse, Flag),
    \+ cache:entry_metadata(pkg, Ebuild, use, Flag),
    \+ memberchk(C, ['virtual','acct-group','acct-user']),
    atom(N), atom(Flag), !
  ).

% NOTE: these tests require a populated VDB (installed packages with IUSE
% metadata) and so are gated on `condition(test_setup_pick/2 succeeds)`.
% In CI (no portage tree, no VDB) they are reported as `[blocked]` rather
% than failed. Locally with a real VDB they run end-to-end.

% Pre-fix probe: confirms `installed_entry_satisfies_build_with_use` returns
% false for the chosen mismatched flag (i.e. the test scenario is valid).
test(probe_setup,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, Flag),
  Ctx = [build_with_use:use_state([Flag],[])],
  \+ use:installed_entry_satisfies_build_with_use(pkg://Ebuild, Ctx).

% rule(:install?{Ctx with mismatched bracketed-USE}) on an installed entry
% must NOT short-circuit to []. It must emit a :update?{[...,replaces,...]}
% literal so the dep walker runs.
test(install_rule_emits_update_on_bwu_mismatch,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, Flag),
  Ctx = [build_with_use:use_state([Flag],[])],
  rules:rule(portage://Ebuild:install?{Ctx}, Conds),
  Conds = [portage://Ebuild:update?{UpdCtx}],
  memberchk(replaces(pkg://Ebuild), UpdCtx),
  memberchk(rebuild_reason(build_with_use), UpdCtx).

% rule(:run?{Ctx with mismatched bracketed-USE}) on an installed entry must
% emit the same :update literal (instead of degrading to :reinstall with an
% empty body).
test(run_rule_emits_update_on_bwu_mismatch,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, Flag),
  Ctx = [build_with_use:use_state([Flag],[])],
  rules:rule(portage://Ebuild:run?{Ctx}, Conds),
  Conds = [portage://Ebuild:update?{UpdCtx}],
  memberchk(replaces(pkg://Ebuild), UpdCtx),
  memberchk(rebuild_reason(build_with_use), UpdCtx).

% Empty Ctx (no bracketed-USE annotation) on an installed entry preserves
% the existing fast-path: no rebuild emitted.
test(install_rule_empty_ctx_keeps_short_circuit,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, _),
  rules:rule(portage://Ebuild:install?{[]}, Conds),
  Conds == [].

% End-to-end: prove + plan iptables:run with bracketed-[nftables].
% Verify (a) libnftnl ends up in the proof, (b) iptables:update appears in
% the plan, (c) libnftnl:install is in an EARLIER wave than iptables:update.
test(plan_orders_bwu_dep_before_rebuild,
     [condition(query:search([category('net-firewall'),name(iptables),
                              installed(true)], pkg://_))]) :-
  query:search([category('net-firewall'),name(iptables)], portage://RepoE),
  !,
  Goal = portage://RepoE:run?{[build_with_use:use_state([nftables],[])]},
  pipeline:prove_with_fallback([Goal], Proof, _Model, Triggers),
  planner:plan(Proof, Triggers, t, Plan, _Rem),
  % Find the wave index of any libnftnl literal vs any iptables-VVV:update.
  nth1(WLib, Plan, WaveLib),
    member(RLib, WaveLib),
    ( RLib = rule(HLib, _) ; RLib = assumed(rule(HLib, _)) ; RLib = rule(assumed(HLib), _) ),
    prover:canon_literal(HLib, CHLib, _),
    term_to_atom(CHLib, ALib), sub_atom(ALib, _, _, _, libnftnl), !,
  nth1(WIp, Plan, WaveIp),
    member(RIp, WaveIp),
    ( RIp = rule(HIp, _) ; RIp = assumed(rule(HIp, _)) ; RIp = rule(assumed(HIp), _) ),
    prover:canon_literal(HIp, CHIp, _),
    term_to_atom(CHIp, AIp), sub_atom(AIp, _, _, _, 'iptables'),
    sub_atom(AIp, _, _, _, ':update'), !,
  WLib < WIp.


% =============================================================================
%  Issue #9: same-version :update must not no-op on USE change
% =============================================================================

:- begin_tests(update_use_change_resolve).

test_setup_same_version_installed(portage://RepoE, pkg://PkgE, Flag) :-
  test_setup_pick(pkg://PkgE, Flag),
  query:search([category(C),name(N),version(V)], pkg://PkgE),
  query:search([category(C),name(N),version(V)], portage://RepoE).

test(update_resolve_not_empty_on_use_change,
     [condition(test_setup_same_version_installed(_, _, _))]) :-
  test_setup_same_version_installed(portage://RepoE, _PkgE, Flag),
  Changes = [use_change(Flag, enable)],
  Ctx = [suggestion(use_change, portage://RepoE, Changes)],
  candidate:resolve(portage://RepoE:update?{Ctx}, Conds),
  Conds \== [].

:- end_tests(update_use_change_resolve).

:- end_tests(rules_install_run_bwu_rebuild).


% =============================================================================
%  Phantom grouped-dep assumptions (portage-ng#10, #14, #15)
% =============================================================================

:- begin_tests(phantom_grouped_dep_assumption).

test(unsatisfied_constraints_is_phantom) :-
  explanation:phantom_grouped_dep_assumption(unsatisfied_constraints, 'media-libs', clutter).

test(masked_is_phantom) :-
  explanation:phantom_grouped_dep_assumption(masked, 'sys-apps', systemd).

test(acct_group_keyword_filtered_is_phantom) :-
  explanation:phantom_grouped_dep_assumption(keyword_filtered, 'acct-group', buildbot).

test(other_keyword_filtered_not_phantom, [fail]) :-
  explanation:phantom_grouped_dep_assumption(keyword_filtered, 'dev-qt', qtbase).

% A phantom-reason grouped dep must still produce a domain assumption at the
% prover (so the proof completes at tier 1 instead of cascading through all
% five prove_with_fallback relaxation tiers, portage-ng#20 perf fallout). The
% emitted assumption carries the assumption_reason tag so the scheduler /
% printer classify it as phantom downstream (assumed_inner_phantom/1,
% phantom_grouped_dep_assumption/3) and keep it out of concrete install waves.
test(build_assumption_emits_phantom_with_reason_tag) :-
  assertz(memo:assumption_reason_cache_(install, 'dev-qt', qtbase, unsatisfied_constraints)),
  ( candidate:grouped_dep_build_assumption(install, 'dev-qt', qtbase, [], [], [], Conditions),
    Conditions = [assumed(grouped_package_dependency('dev-qt', qtbase, _):install?{Ctx})],
    memberchk(assumption_reason(unsatisfied_constraints), Ctx)
  -> retractall(memo:assumption_reason_cache_(install, 'dev-qt', qtbase, _))
  ;  retractall(memo:assumption_reason_cache_(install, 'dev-qt', qtbase, _)),
     fail
  ).

test(build_assumption_emits_requse_violation_with_tag) :-
  assertz(memo:assumption_reason_cache_(install, 'dev-qt', qtbase, unsatisfied_constraints)),
  assertz(memo:requse_violation_('dev-qt', qtbase, use_flag_conflict([],[],[]))),
  ( candidate:grouped_dep_build_assumption(install, 'dev-qt', qtbase, [], [], [], Conditions),
    Conditions = [assumed(grouped_package_dependency('dev-qt', qtbase, _):install?{Ctx})],
    memberchk(required_use_violation(_), Ctx)
  -> retractall(memo:assumption_reason_cache_(install, 'dev-qt', qtbase, _)),
     retractall(memo:requse_violation_('dev-qt', qtbase, _))
  ;  retractall(memo:assumption_reason_cache_(install, 'dev-qt', qtbase, _)),
     retractall(memo:requse_violation_('dev-qt', qtbase, _)),
     fail
  ).

:- end_tests(phantom_grouped_dep_assumption).


% =============================================================================
%  Scheduler: install configure closure (portage-ng#21)
% =============================================================================

:- begin_tests(scheduler_install_configure_deps).

% KB-independent: `build_pkg_wave_map/2` needs `cache:ordered_entry/5`, which
% CI lacks. Exercise `sweep_repair/7` with synthetic heads and a hand-built
% PkgWaveMap (grouped RDEPEND aliasing via run_phase-C-N).

test(install_promoted_past_run_rdepend) :-
  BifRun = grouped_package_dependency(no, 'dev-haskell', bifunctors, []):run,
  SgRun = rule(portage://'fake/sg-1':run, [BifRun]),
  SgInstall = rule(portage://'fake/sg-1':install, []),
  BifRunRule = rule(portage://'fake/bif-1':run, []),
  AllRules = [SgInstall, SgRun, BifRunRule],
  list_to_assoc([ (portage://'fake/sg-1':install)-1,
                  (portage://'fake/sg-1':run)-2,
                  (portage://'fake/bif-1':run)-2 ], Map0),
  list_to_assoc([ ('run_phase'-'dev-haskell'-bifunctors)-2 ], PkgMap),
  scheduler:build_install_configure_dep_map([SgRun], CfgMap),
  scheduler:sweep_repair(strict, AllRules, Map0, PkgMap, pd(t, t), CfgMap, 20, Map1),
  get_assoc(portage://'fake/sg-1':install, Map1, WInstall),
  WInstall >= 3.

test(configure_deps_wave_from_run_body) :-
  BifRun = grouped_package_dependency(no, 'dev-haskell', bifunctors, []):run,
  RunRule = rule(portage://'fake/sg-1':run, [BifRun]),
  list_to_assoc([ ('run_phase'-'dev-haskell'-bifunctors)-3 ], PkgMap),
  empty_assoc(Map),
  scheduler:build_install_configure_dep_map([RunRule], CfgMap),
  scheduler:configure_deps_wave(portage://'fake/sg-1':install,
                                Map, PkgMap, pd(t,t), CfgMap, W),
  W =:= 3.

:- end_tests(scheduler_install_configure_deps).


% =============================================================================
%  Scheduler: PDEPEND completion ordering (portage-ng#18)
% =============================================================================
%
% A provider P that declares PDEPEND is only functionally complete once its
% post-install group is merged. A consumer of P (e.g. a ruby extension gem
% whose `configure` phase runs the interpreter) must therefore be ordered
% after P's whole PDEPEND closure, matching emerge. These tests cover the
% pure helpers that implement that ordering in `Source/Pipeline/scheduler.pl`
% (no knowledge base required).

:- begin_tests(scheduler_pdepend_completion).

% Fixtures: a synthetic provider `dev-lang/ruby` with one PDEPEND target
% `fake/rubygems-1` (installed at wave 9, run at wave 10). The closure map is
% per-target: target head -> set of (Category,Name) it transitively depends
% on. Here rubygems' closure is just its own package (no cycle back to the
% leaf consumer). Consumers are grouped dep literals (head_package resolves
% them without a cache lookup).

pdepend_fixture(Map, pd(AnchorMap, ClosureMap)) :-
  list_to_assoc([ (portage://'fake/rubygems-1':install)-9,
                  (portage://'fake/rubygems-1':run)-10 ], Map),
  list_to_assoc([ ('dev-lang'-ruby)-[portage://'fake/rubygems-1':run] ], AnchorMap),
  list_to_assoc([ ('dev-ruby'-rubygems)-true ], TargetCns),
  list_to_assoc([ (portage://'fake/rubygems-1':run)-TargetCns ], ClosureMap).

% A leaf consumer (outside the target's closure) is ordered after the
% target's INSTALL wave (9), not its cyclic :run wave (10). This is the
% ruby-gem case (portage-ng#18).
test(consumer_completes_after_pdepend_install_wave) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_complete_wave(grouped_package_dependency(no,'dev-lang',ruby,[]):install,
                                  grouped_package_dependency(no,'dev-ruby','mecab-ruby',[]):install,
                                  Map, Pd, W),
  W =:= 9.

% A consumer whose package lies in the target's closure must NOT be bumped
% (cycle safety, at (C,N) granularity): the target transitively depends on
% it. This is the LLVM clang/compiler-rt cycle (portage-ng#19).
test(cyclic_consumer_not_bumped, [fail]) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_complete_wave(grouped_package_dependency(no,'dev-lang',ruby,[]):install,
                                  grouped_package_dependency(no,'dev-ruby',rubygems,[]):install,
                                  Map, Pd, _).

% Per-target filtering: a provider with two PDEPEND targets, one acyclic
% (clang-toolchain-symlinks, install wave 10) and one cyclic w.r.t. the
% consumer (clang-runtime, install wave 16, RDEPENDs the consumer). The
% consumer (compiler-rt) is ordered after the acyclic target's install wave
% (10) and never after the cyclic one (portage-ng#19).
test(per_target_cycle_filter_uses_acyclic_max) :-
  list_to_assoc([ (portage://'fake/symlinks-1':install)-10,
                  (portage://'fake/symlinks-1':run)-11,
                  (portage://'fake/runtime-1':install)-16,
                  (portage://'fake/runtime-1':run)-17 ], Map),
  list_to_assoc([ ('llvm-core'-clang)-[portage://'fake/symlinks-1':run,
                                       portage://'fake/runtime-1':run] ], AnchorMap),
  empty_assoc(SymCns),
  list_to_assoc([ ('llvm-runtimes'-'compiler-rt')-true ], RunCns),
  list_to_assoc([ (portage://'fake/symlinks-1':run)-SymCns,
                  (portage://'fake/runtime-1':run)-RunCns ], ClosureMap),
  scheduler:pdepend_complete_wave(grouped_package_dependency(no,'llvm-core',clang,[]):install,
                                  grouped_package_dependency(no,'llvm-runtimes','compiler-rt',[]):install,
                                  Map, pd(AnchorMap, ClosureMap), W),
  W =:= 10.

% A consumer that is ITSELF one of the provider's PDEPEND targets is never
% ordered after the group (e.g. clang-toolchain-symlinks must not wait for
% its sibling clang-runtime; portage-ng#19).
test(pdepend_target_member_not_bumped, [fail]) :-
  list_to_assoc([ (portage://'fake/symlinks-1':install)-5 ], Map),
  GH = grouped_package_dependency(no,'llvm-core','clang-toolchain-symlinks',[]):run,
  list_to_assoc([ ('llvm-core'-clang)-[GH] ], AnchorMap),
  empty_assoc(EmptyCns),
  list_to_assoc([ GH-EmptyCns ], ClosureMap),
  scheduler:pdepend_complete_wave(grouped_package_dependency(no,'llvm-core',clang,[]):install,
                                  grouped_package_dependency(no,'llvm-core','clang-toolchain-symlinks',[]):install,
                                  Map, pd(AnchorMap, ClosureMap), _).

% No PDEPEND provider in plan (empty AnchorMap): fast no-op failure.
test(empty_anchor_map_is_noop, [fail]) :-
  pdepend_fixture(Map, _),
  scheduler:pdepend_complete_wave(grouped_package_dependency(no,'dev-lang',ruby,[]):install,
                                  grouped_package_dependency(no,'dev-ruby','mecab-ruby',[]):install,
                                  Map, pd(t,t), _).

% A non-grouped (concrete) dep literal never triggers completion: consumer
% edges are always grouped deps, and the concrete provider-install node is
% shared with the post-install group.
test(concrete_dep_does_not_complete, [fail]) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_complete_wave(portage://'fake/ruby-1':install,
                                  grouped_package_dependency(no,'dev-ruby','mecab-ruby',[]):install,
                                  Map, Pd, _).

% A dep on a provider without PDEPEND (absent from AnchorMap) fails.
test(provider_without_pdepend_fails, [fail]) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_complete_wave(grouped_package_dependency(no,'dev-libs',glib,[]):install,
                                  grouped_package_dependency(no,'dev-libs',consumer,[]):install,
                                  Map, Pd, _).

% max_pd_install_wave prefers the package's :install wave over a :run head.
test(install_wave_preferred_over_run) :-
  list_to_assoc([ (portage://'fake/x-1':install)-3,
                  (portage://'fake/x-1':run)-5 ], M),
  scheduler:max_pd_install_wave(M, portage://'fake/x-1':run, -1, Out),
  Out =:= 3.

% Forward closure reaches every transitively-depended head (seeds included).
test(forward_closure_reaches_transitive_deps) :-
  list_to_assoc([ a-[b,c], b-[d], c-[], d-[] ], Fwd),
  empty_assoc(V0),
  scheduler:forward_closure([a], Fwd, V0, Closure),
  assoc_to_keys(Closure, Ks),
  sort(Ks, Sorted),
  Sorted == [a,b,c,d].

% Collapsing a head closure to package (C,N) identity: grouped heads map to
% their package, duplicate slots/actions collapse, and non-package heads
% (assumptions/blockers) are dropped (portage-ng#19).
test(closure_heads_to_cns_collapses_to_packages) :-
  list_to_assoc([ (grouped_package_dependency(no,'dev-ruby',rubygems,[]):run)-true,
                  (grouped_package_dependency(no,'dev-ruby',rubygems,[]):install)-true,
                  (assumed(blocker(weak,run,a,b,none,version_none,[])))-true ], Closure),
  scheduler:closure_heads_to_cns(Closure, CnSet),
  assoc_to_keys(CnSet, Ks),
  Ks == ['dev-ruby'-rubygems].

:- end_tests(scheduler_pdepend_completion).


% -----------------------------------------------------------------------------
%  Builder: VDB reconciliation backstop tests
% -----------------------------------------------------------------------------
%
% Defensive backstop for the "silent --ci --build exit 0 despite a
% sub-dep install failure" class of regression. Verifies the predicates
% in `Source/Pipeline/builder.pl` (builder:reconcile_install_actions/3,
% builder:apply_vdb_reconciliation/4, etc.) behave as documented when
% the input plan contains a mix of install rules whose VDB entries do
% or do not exist on disk.

:- begin_tests(builder_vdb_reconciliation).

% A bogus entry that is guaranteed not to exist in any host's VDB.
% Used to simulate the "install action whose target never landed"
% scenario without needing a real failing build.

test(is_install_rule_install_matches, [nondet, condition(eapi_repo_registered)]) :-
  Rule = rule(portage://'app-misc/jq-1.8.1':install?{[]}, []),
  builder:is_install_rule(Rule, R, E, A),
  R == portage, E == 'app-misc/jq-1.8.1', A == install.

test(is_install_rule_reinstall_matches, [nondet, condition(eapi_repo_registered)]) :-
  Rule = rule(portage://'app-misc/jq-1.8.1':reinstall?{[]}, []),
  builder:is_install_rule(Rule, _, _, A),
  A == reinstall.

test(is_install_rule_rejects_non_install_actions, [fail]) :-
  Rule = rule(portage://'app-misc/jq-1.8.1':run?{[]}, []),
  builder:is_install_rule(Rule, _, _, _).

test(is_install_rule_rejects_world_rules, [fail]) :-
  Rule = rule(world('@world'):register?{[]}, []),
  builder:is_install_rule(Rule, _, _, _).

test(is_install_rule_rejects_uninstall, [fail]) :-
  Rule = rule(portage://'app-misc/jq-1.8.1':uninstall?{[]}, []),
  builder:is_install_rule(Rule, _, _, _).

test(is_install_rule_rejects_non_eapi_repo, [fail]) :-
  % `pkg` is the VDB repo (type=vdb). Rules synthesized for VDB-typed
  % targets are not real plan installs and must be skipped by the
  % backstop -- otherwise unrelated entries would falsely trip it.
  Rule = rule(pkg://'app-misc/jq-1.8.1':install?{[]}, []),
  builder:is_install_rule(Rule, _, _, _).

test(vdb_entry_present_false_for_bogus, [fail]) :-
  builder:vdb_entry_present('/var/db/pkg', 'no-such-cat/no-such-pkg-0.0').

test(reconcile_active_false_when_merge_missing, [setup(stash_live_phases(Saved)),
                                                   cleanup(restore_live_phases(Saved))]) :-
  % Drop `merge` from live phases. Reconciliation must short-circuit
  % to Active=false with Missing=[] regardless of plan contents.
  retractall(config:build_live_phases(_)),
  assertz(config:build_live_phases([clean, setup, unpack, prepare, configure, compile, test, install])),
  Plan = [[rule(portage://'no-such-cat/no-such-pkg-0.0':install?{[]}, [])]],
  builder:reconcile_install_actions(Plan, Missing, Active),
  Active == false,
  Missing == [].

test(reconcile_flags_missing_install_when_active, [setup(stash_live_phases(Saved)),
                                                    cleanup(restore_live_phases(Saved)),
                                                    condition((eapi_repo_registered,
                                                               pkg_repo_registered))]) :-
  % Full live phases (including merge) AND a registered pkg repo on
  % the host = reconciliation is active. Only install steps recorded
  % as done (resume_done) are checked; a succeeded merge with no VDB
  % row must be reported missing.
  retractall(config:build_live_phases(_)),
  assertz(config:build_live_phases([clean, setup, unpack, prepare, configure, compile, test, install, merge])),
  retractall(builder:resume_done(_, _)),
  assertz(builder:resume_done('no-such-cat/no-such-pkg-0.0', install)),
  Plan = [[rule(portage://'no-such-cat/no-such-pkg-0.0':install?{[]}, [])]],
  builder:reconcile_install_actions(Plan, Missing, Active),
  Active == true,
  Missing = [_|_],
  member(portage://'no-such-cat/no-such-pkg-0.0':install, Missing).

test(reconcile_ignores_failed_install_without_resume_done,
     [setup(stash_live_phases(Saved)),
      cleanup(restore_live_phases(Saved)),
      condition((eapi_repo_registered, pkg_repo_registered))]) :-
  % portage-ng#11: failed/skipped installs stay out of the plan but must
  % not inflate the reconciliation failure tally.
  retractall(config:build_live_phases(_)),
  assertz(config:build_live_phases([clean, setup, unpack, prepare, configure, compile, test, install, merge])),
  retractall(builder:resume_done(_, _)),
  Plan = [[rule(portage://'no-such-cat/no-such-pkg-0.0':install?{[]}, [])]],
  builder:reconcile_install_actions(Plan, Missing, Active),
  Active == true,
  Missing == [].

test(apply_reconciliation_increments_failed_count, [setup(stash_live_phases(Saved)),
                                                     cleanup(restore_live_phases(Saved)),
                                                     condition((eapi_repo_registered,
                                                                pkg_repo_registered))]) :-
  retractall(config:build_live_phases(_)),
  assertz(config:build_live_phases([clean, setup, unpack, prepare, configure, compile, test, install, merge])),
  retractall(builder:resume_done(_, _)),
  assertz(builder:resume_done('no-such-cat/no-such-pkg-0.0', install)),
  Plan = [[rule(portage://'no-such-cat/no-such-pkg-0.0':install?{[]}, [])]],
  with_output_to(string(_),
    builder:apply_vdb_reconciliation(Plan, 0, Failed, Missing)),
  Missing \= [],
  Failed > 0.

test(apply_reconciliation_leaves_failed_alone_when_inactive, [setup(stash_live_phases(Saved)),
                                                               cleanup(restore_live_phases(Saved))]) :-
  retractall(config:build_live_phases(_)),
  assertz(config:build_live_phases([])),
  Plan = [[rule(portage://'no-such-cat/no-such-pkg-0.0':install?{[]}, [])]],
  builder:apply_vdb_reconciliation(Plan, 7, Failed, Missing),
  Failed == 7,
  Missing == [].

% --- helpers -----------------------------------------------------------------

stash_live_phases(Saved) :-
  findall(P, config:build_live_phases(P), Saved).

restore_live_phases(Saved) :-
  retractall(config:build_live_phases(_)),
  forall(member(P, Saved), assertz(config:build_live_phases(P))).

pkg_repo_registered :-
  current_predicate(pkg:get_location/1),
  catch(pkg:get_location(Root), _, fail),
  exists_directory(Root).

eapi_repo_registered :-
  catch(portage:get_type(eapi), _, fail).

:- end_tests(builder_vdb_reconciliation).


% =============================================================================
%  md5-cache validation harness
% =============================================================================

% Regression harness for the standalone bash extractor at
% Source/Domain/Gentoo/Ebuild/ebuild-depend.sh. Sources every ebuild that
% has an on-disk md5-cache entry through the script's --batch mode and
% diffs the produced KEY=VALUE block against the cached version,
% key by key.
%
% Usage from the project wrapper:
%
%   ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell <<'PL'
%   load_files(portage('Source/Test/unittest'), [if(true)]).
%   md5cache_validate([limit(50), verbose(true)]).
%   halt.
%   PL
%
% Or for a full-tree run with a report file:
%
%   md5cache_validate([out('/tmp/md5cache_report.pl')]).
%
% Compared keys (intersection of both outputs):
%   BDEPEND DEFINED_PHASES DEPEND DESCRIPTION EAPI HOMEPAGE IDEPEND
%   INHERIT IUSE KEYWORDS LICENSE PDEPEND PROPERTIES RDEPEND
%   REQUIRED_USE RESTRICT SLOT SRC_URI
% Skipped keys: _md5_, _eclasses_, INHERITED.

% -----------------------------------------------------------------------------
%  Public entry points
% -----------------------------------------------------------------------------

%! md5cache_metadata_keys(-Keys) is det.
%
% List of metadata keys compared against md5-cache.

md5cache_metadata_keys([
    'BDEPEND', 'DEFINED_PHASES', 'DEPEND', 'DESCRIPTION', 'EAPI',
    'HOMEPAGE', 'IDEPEND', 'INHERIT', 'IUSE', 'KEYWORDS', 'LICENSE',
    'PDEPEND', 'PROPERTIES', 'RDEPEND', 'REQUIRED_USE', 'RESTRICT',
    'SLOT', 'SRC_URI'
]).


%! md5cache_validate is det.
%! md5cache_validate(+Options) is det.
%
% Run ebuild-depend.sh --batch over every md5-cache entry in the
% configured Portage tree and compare its output against the on-disk
% md5-cache file, key by key.
%
% Options:
%   * repo(Atom)      -- repository to validate (default: portage)
%   * limit(N)        -- only process the first N entries (0=all, default 0)
%   * verbose(Bool)   -- print every per-ebuild diff (default false)
%   * out(Path)       -- write a Prolog-term report to Path (default '')

md5cache_validate :-
  md5cache_validate([]).

md5cache_validate(Options) :-
  once(md5cache_validate_(Options)).

md5cache_validate_(Options) :-
  option(repo(Repo), Options, portage),
  option(limit(Limit), Options, 0),
  option(verbose(Verbose), Options, false),
  option(out(OutFile), Options, ''),
  Repo:get_location(RepoRoot),
  Repo:get_cache(CacheDir),
  config:working_dir(WorkingDir),
  atomic_list_concat([WorkingDir, '/Source/Domain/Gentoo/Ebuild/ebuild-depend.sh'],
                     Script),
  ( exists_file(Script)
  -> true
  ;  throw(error(existence_error(file, Script),
                 context(md5cache_validate/1, _)))
  ),
  format('% Discovering md5-cache entries under ~w ...~n', [CacheDir]),
  flush_output,
  md5cache_collect_entries(CacheDir, AllEntries),
  length(AllEntries, Total),
  ( Limit > 0
  -> ( length(AllEntries, ALen), ALen =< Limit
     -> Entries = AllEntries
     ;  length(Entries, Limit), append(Entries, _, AllEntries)
     )
  ;  Entries = AllEntries
  ),
  length(Entries, NEntries),
  format('% Found ~d md5-cache entries, processing ~d.~n', [Total, NEntries]),
  flush_output,
  md5cache_build_batch(Entries, RepoRoot, BatchLines, EntryMap, SkippedNoEbuild),
  length(BatchLines, NBatch),
  format('% ~d ebuilds matched, ~d skipped (no ebuild found).~n',
         [NBatch, SkippedNoEbuild]),
  format('% Running ~w --batch (~d ebuilds) ...~n', [Script, NBatch]),
  flush_output,
  get_time(T0),
  md5cache_run_batch(Script, RepoRoot, BatchLines, Blocks, ExitCode),
  get_time(T1),
  Elapsed is T1 - T0,
  length(Blocks, NBlocks),
  format('% Batch completed in ~3fs (exit=~d, ~d output blocks).~n',
         [Elapsed, ExitCode, NBlocks]),
  flush_output,
  empty_assoc(KAcc0),
  md5cache_compare_fold(EntryMap, Blocks, Verbose,
                        0-0-0-KAcc0-[],
                        Match-Diff-Missing-KAcc-DiffsRev),
  reverse(DiffsRev, DiffDetails),
  assoc_to_list(KAcc, KeyDiffs),
  md5cache_print_summary(Total, NBatch, SkippedNoEbuild, Missing,
                         Match, Diff, KeyDiffs, DiffDetails, Elapsed),
  ( OutFile == ''
  -> true
  ;  md5cache_write_report(OutFile, Total, NBatch, SkippedNoEbuild,
                           Missing, Match, Diff, Elapsed,
                           KeyDiffs, DiffDetails)
  ).


% -----------------------------------------------------------------------------
%  Cache discovery and batch descriptors
% -----------------------------------------------------------------------------

%! md5cache_collect_entries(+CacheDir, -Entries) is det.
%
% Walk CacheDir and collect every md5-cache file as entry(Cat, PF, Path).

md5cache_collect_entries(CacheDir, Entries) :-
  ( exists_directory(CacheDir)
  -> true
  ;  throw(error(existence_error(directory, CacheDir),
                 context(md5cache_collect_entries/2, _)))
  ),
  directory_files(CacheDir, Cats0),
  exclude([X]>>memberchk(X, ['.', '..']), Cats0, Cats),
  findall(entry(Cat, PF, Path),
    ( member(Cat, Cats),
      atomic_list_concat([CacheDir, '/', Cat], CatDir),
      exists_directory(CatDir),
      directory_files(CatDir, PFs0),
      member(PF, PFs0),
      \+ memberchk(PF, ['.', '..']),
      atomic_list_concat([CatDir, '/', PF], Path),
      exists_file(Path)
    ),
    Entries0),
  sort(Entries0, Entries).


%! md5cache_build_batch(+Entries, +RepoRoot, -BatchLines, -EntryMap, -Skipped) is det.
%
% For each cache entry, locate the matching .ebuild file under RepoRoot
% and emit a single-line descriptor for ebuild-depend.sh --batch.
% EntryMap is a list of map(Idx, Cat, PF, Path, EntryId) entries indexed
% by their position in the batch input (= their position in the script's
% output blocks).

md5cache_build_batch(Entries, RepoRoot, BatchLines, EntryMap, Skipped) :-
  md5cache_build_batch_(Entries, RepoRoot, 0,
                        [], BLR, [], EMR, 0, Skipped),
  reverse(BLR, BatchLines),
  reverse(EMR, EntryMap).

md5cache_build_batch_([], _, _, BL, BL, EM, EM, S, S).

md5cache_build_batch_([entry(Cat, PF, Path)|Rest], RepoRoot, Idx,
                       BLAcc, BLOut, EMAcc, EMOut, SAcc, SOut) :-
  ( md5cache_find_ebuild(RepoRoot, Cat, PF, Ebuild, PN)
  -> md5cache_descriptor_line(Cat, PN, PF, Ebuild, Line),
     atomic_list_concat([Cat, '/', PF], EntryId),
     Idx1 is Idx + 1,
     md5cache_build_batch_(Rest, RepoRoot, Idx1,
                           [Line|BLAcc], BLOut,
                           [map(Idx, Cat, PF, Path, EntryId)|EMAcc], EMOut,
                           SAcc, SOut)
  ;  S1 is SAcc + 1,
     md5cache_build_batch_(Rest, RepoRoot, Idx,
                           BLAcc, BLOut, EMAcc, EMOut, S1, SOut)
  ).


%! md5cache_find_ebuild(+RepoRoot, +Cat, +PF, -Ebuild, -PN) is semidet.
%
% Locate <RepoRoot>/<Cat>/<PN>/<PF>.ebuild by scanning the category
% directory for the matching package-name subdirectory.

md5cache_find_ebuild(RepoRoot, Cat, PF, Ebuild, PN) :-
  atomic_list_concat([RepoRoot, '/', Cat], CatDir),
  exists_directory(CatDir),
  directory_files(CatDir, PNs),
  member(PN, PNs),
  \+ memberchk(PN, ['.', '..']),
  atomic_list_concat([CatDir, '/', PN, '/', PF, '.ebuild'], Ebuild),
  exists_file(Ebuild),
  !.


%! md5cache_descriptor_line(+Cat, +PN, +PF, +Ebuild, -Line) is det.

md5cache_descriptor_line(Cat, PN, PF, Ebuild, Line) :-
  md5cache_split_pf(PN, PF, PV, PR, PVR),
  atomic_list_concat([PN, '-', PV], P),
  format(atom(Line),
    'CATEGORY=~w PN=~w PV=~w PR=~w PVR=~w PF=~w P=~w EBUILD=~w',
    [Cat, PN, PV, PR, PVR, PF, P, Ebuild]).


%! md5cache_split_pf(+PN, +PF, -PV, -PR, -PVR) is det.
%
% Strip the PN- prefix and split a trailing -rN suffix off PVR. The
% revision split must pick the rightmost -rN that is followed only by
% digits, mirroring Portage's bash semantics.

md5cache_split_pf(PN, PF, PV, PR, PVR) :-
  atom_concat(PN, '-', PNDash),
  ( atom_concat(PNDash, PVR0, PF)
  -> PVR = PVR0
  ;  PVR = PF
  ),
  md5cache_split_pvr(PVR, PV, PR).

md5cache_split_pvr(PVR, PV, PR) :-
  ( findall(PV0-PR0,
      ( atom_concat(PV0, RevPart, PVR),
        atom_concat('-r', Digits, RevPart),
        atom_codes(Digits, DC), DC = [_|_],
        forall(member(C, DC), (C >= 0'0, C =< 0'9)),
        atom_concat(r, Digits, PR0)
      ), Solutions),
    Solutions \== []
  -> last(Solutions, PV-PR)
  ;  PV = PVR, PR = 'r0'
  ).


% -----------------------------------------------------------------------------
%  Subprocess invocation
% -----------------------------------------------------------------------------

%! md5cache_run_batch(+Script, +RepoRoot, +Lines, -Blocks, -ExitCode) is det.
%
% Spawn ebuild-depend.sh --batch, write the descriptor lines to its
% stdin, and slurp its stdout into ---END--- delimited blocks.

md5cache_run_batch(Script, RepoRoot, Lines, Blocks, ExitCode) :-
  process_create(Script, ['--batch', RepoRoot],
                 [ stdin(pipe(In)),
                   stdout(pipe(Out)),
                   stderr(null),
                   process(Pid)
                 ]),
  catch(
    ( forall(member(L, Lines), format(In, '~w~n', [L])),
      close(In)
    ),
    E,
    ( catch(close(In, [force(true)]), _, true),
      catch(process_wait(Pid, _), _, true),
      throw(E)
    )
  ),
  read_string(Out, _, OutString),
  close(Out),
  process_wait(Pid, exit(ExitCode)),
  md5cache_split_blocks(OutString, Blocks).


%! md5cache_split_blocks(+OutString, -Blocks) is det.

md5cache_split_blocks(OutString, Blocks) :-
  split_string(OutString, "\n", "", Lines),
  md5cache_split_blocks_(Lines, [], [], Rev),
  reverse(Rev, Blocks).

md5cache_split_blocks_([], _, Acc, Acc).
md5cache_split_blocks_([L|Rest], Cur, Acc, Out) :-
  ( L == "---END---"
  -> reverse(Cur, BlockLines),
     md5cache_split_blocks_(Rest, [], [BlockLines|Acc], Out)
  ;  md5cache_split_blocks_(Rest, [L|Cur], Acc, Out)
  ).


% -----------------------------------------------------------------------------
%  KEY=VALUE parsing and comparison
% -----------------------------------------------------------------------------

%! md5cache_parse_kv_lines(+Lines, -KV) is det.
%
% Parse a list of "KEY=VALUE" strings into a list of Atom-Atom pairs.
% Whitespace inside the value is normalised (collapsed runs, trimmed).

md5cache_parse_kv_lines(Lines, KV) :-
  findall(Key-Norm,
    ( member(L, Lines),
      L \== "",
      sub_string(L, Eq, 1, _, "="),
      sub_string(L, 0, Eq, _, KS),
      EqAfter is Eq + 1,
      sub_string(L, EqAfter, _, 0, VS),
      atom_string(Key, KS),
      md5cache_normalize_value(VS, Norm)
    ),
    KV).


%! md5cache_normalize_value(+RawString, -NormAtom) is det.

md5cache_normalize_value(Raw, Norm) :-
  ( atom(Raw) -> atom_string(Raw, S) ; S = Raw ),
  split_string(S, " \t\n\r", " \t\n\r", Tokens0),
  exclude([X]>>(X == ""), Tokens0, Tokens),
  atomic_list_concat(Tokens, ' ', Norm).


%! md5cache_read_md5_cache_file(+Path, -KV) is det.

md5cache_read_md5_cache_file(Path, KV) :-
  read_file_to_string(Path, Content, []),
  split_string(Content, "\n", "", Lines),
  md5cache_parse_kv_lines(Lines, KV).


%! md5cache_pairs_to_assoc(+Pairs, -Assoc) is det.
%
% Last-write-wins fold over Key-Value pairs (avoids the
% domain_error(unique_key_pairs, _) thrown by list_to_assoc/2 on
% duplicate keys).

md5cache_pairs_to_assoc(Pairs, Assoc) :-
  empty_assoc(E),
  foldl([K-V, In, Out]>>put_assoc(K, In, V, Out), Pairs, E, Assoc).


%! md5cache_diff_entry(+CacheKV, +OurKV, -Diffs) is det.
%
% Compare on the intersection of metadata keys. Diffs is a list of
% Key-CacheVal-OurVal triples; missing keys on either side are normalised
% to the empty atom ''.

md5cache_diff_entry(CacheKV, OurKV, Diffs) :-
  md5cache_metadata_keys(Keys),
  md5cache_pairs_to_assoc(CacheKV, CacheAssoc),
  md5cache_pairs_to_assoc(OurKV, OurAssoc),
  findall(Key-CV-OV,
    ( member(Key, Keys),
      ( get_assoc(Key, CacheAssoc, CV) -> true ; CV = '' ),
      ( get_assoc(Key, OurAssoc, OV)   -> true ; OV = '' ),
      CV \== OV
    ),
    Diffs).


%! md5cache_compare_fold(+EntryMap, +Blocks, +Verbose, +State0, -State) is det.
%
% State carries Match-Diff-Missing-KeyAssoc-DiffDetailsRev.

md5cache_compare_fold([], _, _, S, S).

md5cache_compare_fold([map(Idx, _Cat, _PF, Path, EntryId)|Rest],
                       Blocks, Verbose,
                       M-D-X-K-Diffs, OutState) :-
  ( nth0(Idx, Blocks, BlockLines),
    md5cache_parse_kv_lines(BlockLines, OurKV),
    OurKV \== []
  -> md5cache_read_md5_cache_file(Path, CacheKV),
     md5cache_diff_entry(CacheKV, OurKV, EntryDiffs),
     ( EntryDiffs == []
     -> M1 is M + 1,
        md5cache_compare_fold(Rest, Blocks, Verbose,
                              M1-D-X-K-Diffs, OutState)
     ;  D1 is D + 1,
        ( Verbose == true -> md5cache_print_diffs(EntryId, EntryDiffs) ; true ),
        md5cache_accumulate_keydiffs(EntryDiffs, K, K1),
        md5cache_compare_fold(Rest, Blocks, Verbose,
                              M-D1-X-K1-[diff(EntryId, EntryDiffs)|Diffs],
                              OutState)
     )
  ;  X1 is X + 1,
     md5cache_compare_fold(Rest, Blocks, Verbose,
                           M-D-X1-K-Diffs, OutState)
  ).


%! md5cache_accumulate_keydiffs(+Diffs, +AssocIn, -AssocOut) is det.

md5cache_accumulate_keydiffs([], A, A).
md5cache_accumulate_keydiffs([Key-_-_|Rest], AIn, AOut) :-
  ( get_assoc(Key, AIn, N) -> N1 is N + 1 ; N1 = 1 ),
  put_assoc(Key, AIn, N1, A1),
  md5cache_accumulate_keydiffs(Rest, A1, AOut).


% -----------------------------------------------------------------------------
%  Output
% -----------------------------------------------------------------------------

%! md5cache_print_diffs(+EntryId, +Diffs) is det.

md5cache_print_diffs(EntryId, Diffs) :-
  format('~n  DIFF: ~w~n', [EntryId]),
  forall(member(Key-CV-OV, Diffs),
    ( md5cache_truncate(CV, 120, CVT),
      md5cache_truncate(OV, 120, OVT),
      format('    ~w:~n      expected: ~w~n           got: ~w~n',
             [Key, CVT, OVT])
    )).


%! md5cache_truncate(+Value, +Max, -Truncated) is det.

md5cache_truncate(V, Max, T) :-
  ( atom(V)
  -> atom_length(V, L),
     ( L =< Max -> T = V ; sub_atom(V, 0, Max, _, T) )
  ;  string_length(V, L),
     ( L =< Max -> T = V ; sub_string(V, 0, Max, _, T) )
  ).


%! md5cache_print_summary(+Total, +NBatch, +Skipped, +Missing,
%!                       +Match, +Diff, +KeyDiffs, +DiffDetails, +Elapsed) is det.

md5cache_print_summary(Total, NBatch, SkippedNoEbuild, MissingOutput,
                       Match, Diff, KeyDiffs, DiffDetails, Elapsed) :-
  Denom is max(Match + Diff, 1),
  Pct is 100.0 * Match / Denom,
  ( NBatch > 0 -> PerEbuild is 1000.0 * Elapsed / NBatch ; PerEbuild = 0.0 ),
  nl,
  format('~`=t~60|~n', []),
  writeln('VALIDATION SUMMARY'),
  format('~`=t~60|~n', []),
  format('Total md5-cache entries:   ~d~n', [Total]),
  format('Processed:                 ~d~n', [NBatch]),
  format('Skipped (no ebuild):       ~d~n', [SkippedNoEbuild]),
  format('Missing output:            ~d~n', [MissingOutput]),
  format('Exact match:               ~d~n', [Match]),
  format('Mismatched:                ~d~n', [Diff]),
  format('Match rate:                ~2f%~n', [Pct]),
  format('Batch time:                ~3fs~n', [Elapsed]),
  format('Per-ebuild avg:            ~1fms~n', [PerEbuild]),
  ( KeyDiffs == []
  -> true
  ;  nl, writeln('Mismatches by key:'),
     md5cache_sort_keydiffs_desc(KeyDiffs, Sorted),
     forall(member(K-N, Sorted), format('  ~w: ~d~n', [K, N]))
  ),
  ( DiffDetails == []
  -> true
  ;  nl, writeln('First 10 mismatches:'),
     length(DiffDetails, NDiffs),
     ShowN is min(10, NDiffs),
     length(Show, ShowN),
     append(Show, _, DiffDetails),
     forall(member(diff(EID, Ds), Show),
       ( format('  ~w:~n', [EID]),
         forall(member(Key-CV-OV, Ds),
           ( md5cache_truncate(CV, 80, CVT),
             md5cache_truncate(OV, 80, OVT),
             format('    ~w: expected=~q~n', [Key, CVT]),
             format('    ~w:      got=~q~n', [Key, OVT])
           ))
       ))
  ).


%! md5cache_sort_keydiffs_desc(+KVs, -Sorted) is det.

md5cache_sort_keydiffs_desc(KVs, Sorted) :-
  predsort(md5cache_keydiff_cmp, KVs, Sorted).

md5cache_keydiff_cmp(Order, K1-N1, K2-N2) :-
  compare(O1, N2, N1),
  ( O1 == (=) -> compare(Order, K1, K2) ; Order = O1 ).


%! md5cache_write_report(+OutFile, +Total, +NBatch, +Skipped, +Missing,
%!                      +Match, +Diff, +Elapsed, +KeyDiffs, +DiffDetails) is det.
%
% Writes the report as a single Prolog term: md5cache_report([Tag(...), ...]).
% First 100 mismatches are kept, mirroring the original Python output.

md5cache_write_report(OutFile, Total, NBatch, Skipped, Missing,
                      Match, Diff, Elapsed, KeyDiffs, DiffDetails) :-
  length(DiffDetails, NDiff),
  TopN is min(100, NDiff),
  length(Top, TopN),
  append(Top, _, DiffDetails),
  Term = md5cache_report(
    [ total_cache(Total),
      processed(NBatch),
      skipped_no_ebuild(Skipped),
      missing_output(Missing),
      exact_match(Match),
      mismatched(Diff),
      batch_time_s(Elapsed),
      key_diff_counts(KeyDiffs),
      diff_details(Top)
    ]),
  setup_call_cleanup(
    open(OutFile, write, S),
    ( format(S, '% md5-cache validation report~n', []),
      portray_clause(S, Term)
    ),
    close(S)
  ),
  format('~nReport written to ~w~n', [OutFile]).

% =============================================================================
%  Profile package.mask golden regression
% =============================================================================
%
% Applies every `package.mask` atom from `Knowledge/profile.qlf` through the
% same `profile:apply_entry/3` path used at startup, collects the resulting
% `preference:local_masked/1` entry ids, and compares them to the checked-in
% snapshot in `profile_mask_golden_ids/1` below.
%
% Usage:
%
%   make test-profile-mask-golden
%
% Regenerate golden after an intentional change:
%
%   make test-profile-mask-golden-update


% profile-mask-golden-begin
% Golden snapshot (465 entries). Regenerate: make test-profile-mask-golden-update

profile_mask_golden_ids([
  'acct-group/automx2-0-r3',
  'acct-user/automx2-0-r3',
  'app-accessibility/mbrola-3.3-r1',
  'app-admin/amazon-ec2-init-20101127-r2',
  'app-admin/mkosi-24.3',
  'app-admin/mkosi-25.3',
  'app-admin/systemdgenie-0.100.0_pre20241202',
  'app-antivirus/lkrg-0.9.9',
  'app-arch/stuffit-5.2.0.611-r1',
  'app-benchmarks/cpuburn-1.4a-r5',
  'app-crypt/libsecret-0.21.7-r1',
  'app-editors/emacs-18.59-r17',
  'app-emulation/crossover-bin-22.1.1',
  'app-emulation/crossover-bin-23.6.0',
  'app-emulation/crossover-bin-24.0.4',
  'app-emulation/crossover-bin-24.0.6',
  'app-emulation/crossover-bin-25.0.0',
  'app-emulation/q4wine-1.4.2',
  'app-emulation/virtualbox-kvm-7.1.14_pre20251103',
  'app-emulation/virtualbox-kvm-7.1.16_pre20251103-r1',
  'app-emulation/virtualbox-kvm-7.2.4_pre20251103',
  'app-emulation/virtualbox-kvm-7.2.6_pre20260201-r1',
  'app-misc/ca-certificates-20250419.3.112',
  'app-misc/screen-5.0.1',
  'app-office/orage-4.21.0',
  'app-text/calibre-8.15.0',
  'app-text/jabref-bin-4.3.1-r1',
  'dev-build/automake-1.11.6-r4',
  'dev-build/xfce4-dev-tools-4.21.0',
  'dev-cpp/glog-0.7.1',
  'dev-db/mysql-8.4.7',
  'dev-db/mysql-8.4.8',
  'dev-embedded/avr-libc-2.1.0',
  'dev-embedded/libftd2xx-1.4.33',
  'dev-embedded/openocd-0.12.0-r1',
  'dev-embedded/openocd-0.12.0-r2',
  'dev-embedded/openocd-9999',
  'dev-lang/tcl-9.0.3-r2',
  'dev-lang/tk-9.0.3-r2',
  'dev-libs/glib-2.86.4-r1',
  'dev-libs/glib-2.86.5-r1',
  'dev-libs/glib-2.88.0-r1',
  'dev-libs/gobject-introspection-1.86.0',
  'dev-libs/gobject-introspection-common-1.86.0',
  'dev-libs/libassuan-3.0.1-r1',
  'dev-libs/libintl-0.25.1',
  'dev-libs/libintl-0.26',
  'dev-libs/libixion-0.20.0',
  'dev-libs/liborcus-0.20.0',
  'dev-perl/Clone-0.480.0',
  'dev-perl/XML-Parser-2.510.0',
  'dev-php/PHP_Timer-5.0.3',
  'dev-python/amodem-1.15.6',
  'dev-python/autobahn-25.10.2',
  'dev-python/autobahn-25.11.1',
  'dev-python/betterproto-2.0.0_beta6',
  'dev-python/betterproto-2.0.0_beta7',
  'dev-python/calver-2025.10.20',
  'dev-python/click-didyoumean-0.3.1',
  'dev-python/csscompressor-0.9.5-r2',
  'dev-python/dparse-0.6.4',
  'dev-python/dunamai-1.26.0',
  'dev-python/flask-migrate-4.1.0',
  'dev-python/flask-sqlalchemy-3.1.1',
  'dev-python/ghp-import-2.1.0-r1',
  'dev-python/griffe-2.0.0',
  'dev-python/griffe-inherited-docstrings-1.1.3',
  'dev-python/grpclib-0.4.9',
  'dev-python/jsmin-3.0.1',
  'dev-python/markdown-exec-1.12.0',
  'dev-python/mergedeep-1.3.4-r1',
  'dev-python/mkdocs-1.6.0',
  'dev-python/mkdocs-1.6.1',
  'dev-python/mkdocs-autorefs-1.4.4',
  'dev-python/mkdocs-bootstrap-1.1.1-r1',
  'dev-python/mkdocs-bootswatch-1.1-r3',
  'dev-python/mkdocs-gen-files-0.6.0',
  'dev-python/mkdocs-get-deps-0.2.0',
  'dev-python/mkdocs-get-deps-0.2.1',
  'dev-python/mkdocs-get-deps-0.2.2',
  'dev-python/mkdocs-git-authors-plugin-0.10.0',
  'dev-python/mkdocs-git-revision-date-localized-plugin-1.5.0',
  'dev-python/mkdocs-git-revision-date-localized-plugin-1.5.1',
  'dev-python/mkdocs-htmlproofer-plugin-1.5.0',
  'dev-python/mkdocs-i18n-0.4.6',
  'dev-python/mkdocs-material-9.7.1',
  'dev-python/mkdocs-material-9.7.2',
  'dev-python/mkdocs-material-9.7.3',
  'dev-python/mkdocs-material-9.7.4',
  'dev-python/mkdocs-material-9.7.5',
  'dev-python/mkdocs-material-extensions-1.3.1',
  'dev-python/mkdocs-minify-plugin-0.8.0',
  'dev-python/mkdocs-monorepo-plugin-1.1.2',
  'dev-python/mkdocs-pymdownx-material-extras-2.8',
  'dev-python/mkdocs-redirects-1.2.2',
  'dev-python/mkdocs-static-i18n-1.3.0',
  'dev-python/mkdocs-static-i18n-1.3.1',
  'dev-python/mkdocstrings-1.0.3',
  'dev-python/mkdocstrings-python-2.0.3',
  'dev-python/mpi4py-3.1.5',
  'dev-python/paginate-0.5.7',
  'dev-python/pipdeptree-2.23.4',
  'dev-python/pipdeptree-2.29.0',
  'dev-python/pipdeptree-2.30.0',
  'dev-python/pipdeptree-2.31.0',
  'dev-python/pipdeptree-2.32.0',
  'dev-python/pipdeptree-2.33.0',
  'dev-python/pipdeptree-2.34.0',
  'dev-python/pipenv-2024.0.2-r1',
  'dev-python/pipx-1.8.0',
  'dev-python/pipx-1.9.0',
  'dev-python/plette-2.1.0',
  'dev-python/plette-2.1.0-r1',
  'dev-python/pockets-0.9.1-r3',
  'dev-python/pygments-ansi-color-0.3.0',
  'dev-python/pygobject-3.52.3',
  'dev-python/pygobject-3.54.3',
  'dev-python/pygobject-3.54.5',
  'dev-python/pyqt6-6.11.0',
  'dev-python/pyqt6-webengine-6.11.0',
  'dev-python/python-systemd-235',
  'dev-python/pythonfinder-2.1.0',
  'dev-python/pythonfinder-3.0.0',
  'dev-python/pythonfinder-3.0.2',
  'dev-python/pythonfinder-3.0.3',
  'dev-python/pyyaml-env-tag-1.1',
  'dev-python/readtime-3.0.0',
  'dev-python/sigstore-protobuf-specs-0.3.2',
  'dev-python/sigstore-protobuf-specs-0.4.1',
  'dev-python/sigstore-protobuf-specs-0.4.2',
  'dev-python/sigstore-protobuf-specs-0.4.3',
  'dev-python/sigstore-protobuf-specs-0.5.0',
  'dev-python/simsimd-6.5.16',
  'dev-python/testtools-2.8.2',
  'dev-python/txaio-25.12.2',
  'dev-python/uritools-6.0.1',
  'dev-python/uv-dynamic-versioning-0.13.0',
  'dev-qt/qt-docs-6.11.0_p202603180534',
  'dev-qt/qt3d-6.11.0',
  'dev-qt/qt5compat-6.11.0',
  'dev-qt/qtbase-6.11.0',
  'dev-qt/qtcharts-6.11.0',
  'dev-qt/qtconnectivity-6.11.0',
  'dev-qt/qtdeclarative-6.11.0',
  'dev-qt/qtgraphs-6.11.0',
  'dev-qt/qthttpserver-6.11.0',
  'dev-qt/qtimageformats-6.11.0',
  'dev-qt/qtlanguageserver-6.11.0',
  'dev-qt/qtlocation-6.11.0',
  'dev-qt/qtmultimedia-6.11.0',
  'dev-qt/qtnetworkauth-6.11.0',
  'dev-qt/qtpositioning-6.11.0',
  'dev-qt/qtquick3d-6.11.0',
  'dev-qt/qtquicktimeline-6.11.0',
  'dev-qt/qtremoteobjects-6.11.0',
  'dev-qt/qtscxml-6.11.0',
  'dev-qt/qtsensors-6.11.0',
  'dev-qt/qtserialbus-6.11.0',
  'dev-qt/qtserialport-6.11.0',
  'dev-qt/qtshadertools-6.11.0',
  'dev-qt/qtspeech-6.11.0',
  'dev-qt/qtsvg-6.11.0',
  'dev-qt/qttools-6.11.0',
  'dev-qt/qttranslations-6.11.0',
  'dev-qt/qtvirtualkeyboard-6.11.0',
  'dev-qt/qtwayland-6.11.0',
  'dev-qt/qtwebchannel-6.11.0',
  'dev-qt/qtwebengine-6.11.0',
  'dev-qt/qtwebsockets-6.11.0',
  'dev-qt/qtwebview-6.11.0',
  'dev-tcltk/tablelist-6.15.1',
  'dev-util/bpf-linker-0.9.15-r1',
  'dev-util/gdbus-codegen-2.86.4',
  'dev-util/gdbus-codegen-2.86.5',
  'dev-util/gdbus-codegen-2.88.0',
  'dev-util/glib-utils-2.86.4',
  'dev-util/glib-utils-2.86.5',
  'dev-util/glib-utils-2.88.0',
  'dev-util/mdds-3.0.0',
  'dev-util/mig-1.8_p20231217',
  'dev-util/mig-1.8_p20260123',
  'dev-util/mig-9999',
  'dev-util/mingw64-runtime-13.0.0',
  'dev-util/mingw64-runtime-14.0.0',
  'games-action/badland-121-r2',
  'games-action/beathazardultra-20130308-r2',
  'games-action/brutal-legend-gog-2.0.0.3',
  'games-action/brutal-legend-hb-20130615-r3',
  'games-action/crimsonland-1.3.5',
  'games-action/guacamelee-20231012',
  'games-action/heretic2-1.06c-r2',
  'games-action/heretic2-demo-1.06a-r2',
  'games-action/hotline-miami-1.0.9a_p20140221-r3',
  'games-action/intrusion2-1.024-r2',
  'games-action/psychonauts-gog-2.0.0.4',
  'games-action/psychonauts-hb-20130506',
  'games-action/shadowgrounds-bin-0_p1-r1',
  'games-action/shadowgrounds-survivor-bin-0_p1-r1',
  'games-action/solar2-1.10-r1',
  'games-action/swordandsworcery-1.02-r5',
  'games-action/trine-enchanted-edition-2.12.508-r4',
  'games-action/trine2-2.01.425-r2',
  'games-arcade/aquaria-1.1.3-r3',
  'games-arcade/barbarian-bin-1.01-r3',
  'games-arcade/dynamitejack-1.0.23-r3',
  'games-arcade/gish-demo-1.6-r1',
  'games-arcade/jardinains-2.0-r4',
  'games-arcade/thinktanks-demo-1.1-r4',
  'games-emulation/gens-2.15.5-r2',
  'games-emulation/vgba-4.8-r1',
  'games-emulation/zinc-1.1-r1',
  'games-emulation/zsnes-2.1.0',
  'games-fps/etqw-bin-1.5-r4',
  'games-fps/etqw-data-1.0-r1',
  'games-fps/etqw-demo-2.0_p1-r4',
  'games-fps/glxquake-bin-0-r3',
  'games-fps/legends-0.4.1.43-r2',
  'games-fps/sauerbraten-2020.12.29',
  'games-fps/soldieroffortune-1.06a-r2',
  'games-fps/ut2003-2225-r6',
  'games-fps/ut2003-demo-2206-r5',
  'games-fps/ut2004-demo-3334-r3',
  'games-misc/little-inferno-20130509-r1',
  'games-misc/papers-please-1.1.65',
  'games-puzzle/braid-gog-2.0.0.3-r1',
  'games-puzzle/braid-hb-20150611-r1',
  'games-puzzle/triptych-1.16',
  'games-roguelike/adom-3.3.3-r2',
  'games-rpg/baldurs-gate-ee-2.6.6.0.47291-r1',
  'games-rpg/broken-age-2.4.800398',
  'games-rpg/costume-quest-2.0.0.3-r1',
  'games-rpg/dear-esther-20130608-r1',
  'games-rpg/dungeon-defenders-20130305-r1',
  'games-rpg/eschalon-book-1-demo-106-r1',
  'games-rpg/wasteland2-1.9.0.13-r2',
  'games-server/etqw-ded-1.5-r1',
  'games-strategy/darwinia-1.43',
  'games-strategy/dominions2-2.16-r1',
  'games-strategy/knights-demo-1.32-r4',
  'games-strategy/spaz-1.605-r1',
  'gnome-base/librsvg-2.40.21-r1',
  'gnome-extra/gnome-logs-43.0',
  'gnome-extra/gnome-logs-45.0',
  'gnome-extra/gnome-logs-49.0',
  'gnome-extra/office-runner-1.0.3',
  'kde-apps/libkcddb-common-25.12.2',
  'kde-apps/libksane-common-25.12.2',
  'kde-frameworks/purpose-kaccounts-services-6.22.0',
  'kde-frameworks/purpose-kaccounts-services-6.23.0',
  'kde-misc/kio-gdrive-common-25.12.2',
  'kde-plasma/drkonqi-6.5.5',
  'kde-plasma/drkonqi-6.6.3',
  'kde-plasma/drkonqi-6.6.4',
  'kde-plasma/plasma-login-manager-6.6.3',
  'kde-plasma/plasma-login-manager-6.6.4',
  'llvm-runtimes/libatomic-stub-0',
  'llvm-runtimes/libgcc-19.1.7',
  'llvm-runtimes/libgcc-19.1.7-r1',
  'llvm-runtimes/libgcc-20.1.8',
  'llvm-runtimes/libgcc-21.1.8',
  'llvm-runtimes/libgcc-22.1.2',
  'llvm-runtimes/libgcc-22.1.3',
  'llvm-runtimes/libgcc-23.0.0.9999',
  'llvm-runtimes/libgcc-23.0.0_pre20260331',
  'mail-mta/postfix-3.12_pre20260410',
  'media-fonts/culmus-0.133-r1',
  'media-libs/libopenaptx-0.2.1-r1',
  'media-libs/libopenaptx-9999',
  'media-libs/openexr-3.4.4',
  'media-plugins/kodi-game-libretro-dosbox-9999',
  'media-plugins/kodi-game-libretro-nestopia-9999',
  'media-sound/aucdtect-0.8.2-r1',
  'media-video/binkplayer-1.99w',
  'media-video/tsmuxer-2.7.0',
  'media-video/vlc-4.0.0_pre20260320',
  'net-dns/ldns-tools-0.1',
  'net-dns/ldns-tools-0.2',
  'net-im/gajim-2.4.4',
  'net-im/gajim-2.4.5',
  'net-libs/libnsl-0-r2',
  'net-libs/libupnp-1.18.4',
  'net-libs/rpcsvc-proto-0-r1',
  'net-mail/automx2-2025.1',
  'net-mail/automx2-2026.1',
  'net-misc/openntpd-6.8_p1-r2',
  'net-misc/ps3mediaserver-1.90.1-r2',
  'net-print/cndrvcups-common-lb-3.70-r1',
  'net-print/cndrvcups-lb-3.70-r1',
  'net-vpn/microsoft-azurevpnclient-3.0.0',
  'perl-core/Params-Check-0.380.0-r3',
  'sci-biology/foldingathome-7.6.13-r1',
  'sci-biology/foldingathome-7.6.21',
  'sci-chemistry/cara-bin-1.8.4-r2',
  'sci-libs/amd-3.0.3',
  'sci-libs/btf-2.0.3',
  'sci-libs/camd-3.0.3',
  'sci-libs/ccolamd-3.0.3',
  'sci-libs/cholmod-4.0.3',
  'sci-libs/colamd-3.0.3',
  'sci-libs/cxsparse-4.0.3',
  'sci-libs/klu-2.0.3',
  'sci-libs/ldl-3.0.3',
  'sci-libs/spqr-3.0.3',
  'sci-libs/suitesparseconfig-7.0.0',
  'sci-libs/umfpack-6.1.0',
  'sci-physics/bullet-3.22b',
  'sys-apps/gentoo-systemd-integration-9-r2',
  'sys-apps/gentoo-systemd-integration-9999',
  'sys-apps/intune-portal-1.2603.31',
  'sys-apps/musl-locales-0.1.0-r3',
  'sys-apps/systemd-258.3',
  'sys-apps/systemd-259.3-r2',
  'sys-apps/systemd-259.4-r1',
  'sys-apps/systemd-260-r2',
  'sys-apps/systemd-260.1',
  'sys-apps/systemd-9999',
  'sys-apps/systemd-initctl-2',
  'sys-apps/systemd-initctl-4',
  'sys-apps/systemd-readahead-216',
  'sys-block/wait-for-dri-devices-rules-1',
  'sys-boot/plymouth-24.004.60-r1',
  'sys-devel/binutils-2.32-r2',
  'sys-devel/binutils-2.33.1-r1',
  'sys-devel/binutils-2.34-r2',
  'sys-devel/binutils-2.35.2',
  'sys-devel/binutils-2.36.1-r2',
  'sys-devel/binutils-2.37_p1-r2',
  'sys-devel/binutils-2.38-r2',
  'sys-devel/binutils-2.39-r5',
  'sys-devel/binutils-2.40-r9',
  'sys-devel/binutils-2.41-r5',
  'sys-devel/binutils-2.42-r2',
  'sys-devel/binutils-2.43-r2',
  'sys-devel/binutils-hppa64-2.37_p1-r2',
  'sys-devel/binutils-hppa64-2.38-r2',
  'sys-devel/binutils-hppa64-2.39-r5',
  'sys-devel/binutils-hppa64-2.40-r7',
  'sys-devel/binutils-hppa64-2.41-r5',
  'sys-devel/binutils-hppa64-2.42-r2',
  'sys-devel/binutils-hppa64-2.43-r2',
  'sys-devel/clang-crossdev-wrappers-16',
  'sys-devel/clang-crossdev-wrappers-17',
  'sys-devel/clang-crossdev-wrappers-18',
  'sys-devel/clang-crossdev-wrappers-19',
  'sys-devel/clang-crossdev-wrappers-20',
  'sys-devel/clang-crossdev-wrappers-21',
  'sys-devel/clang-crossdev-wrappers-22',
  'sys-devel/clang-crossdev-wrappers-23',
  'sys-devel/gcc-10.5.0',
  'sys-devel/gcc-8.5.0-r2',
  'sys-devel/gcc-9.5.0',
  'sys-devel/gettext-0.25.1',
  'sys-devel/gettext-0.26',
  'sys-devel/gettext-1.0',
  'sys-devel/kgcc64-10.5.0',
  'sys-devel/nvptx-tools-0_pre20240809',
  'sys-devel/nvptx-tools-0_pre20260402',
  'sys-devel/nvptx-tools-9999',
  'sys-fs/atari-fdisk-0.7.1.5.4',
  'sys-fs/atari-fdisk-0.7.1.5.4-r1',
  'sys-kernel/gnumach-1.8_p20260224',
  'sys-kernel/gnumach-1.8_p20260330',
  'sys-kernel/gnumach-9999',
  'sys-kernel/hurd-0.9_p20251029',
  'sys-kernel/hurd-0.9_p20260331',
  'sys-kernel/hurd-9999',
  'sys-kernel/rumpkernel-0_pre20250111_p6',
  'sys-kernel/rumpkernel-9999',
  'sys-libs/argp-standalone-1.5.0',
  'sys-libs/binutils-libs-2.34-r2',
  'sys-libs/binutils-libs-2.35.2',
  'sys-libs/binutils-libs-2.36.1-r2',
  'sys-libs/binutils-libs-2.37_p1-r2',
  'sys-libs/binutils-libs-2.38-r2',
  'sys-libs/binutils-libs-2.39-r5',
  'sys-libs/binutils-libs-2.40-r7',
  'sys-libs/binutils-libs-2.41-r5',
  'sys-libs/binutils-libs-2.42-r2',
  'sys-libs/binutils-libs-2.43-r3',
  'sys-libs/error-standalone-1.0',
  'sys-libs/error-standalone-2.0-r1',
  'sys-libs/fts-standalone-1.2.7',
  'sys-libs/fts-standalone-1.2.7-r1',
  'sys-libs/glibc-2.19-r3',
  'sys-libs/glibc-2.31-r7',
  'sys-libs/glibc-2.32-r8',
  'sys-libs/glibc-2.33-r14',
  'sys-libs/glibc-2.34-r14',
  'sys-libs/glibc-2.35-r11',
  'sys-libs/glibc-2.36-r8',
  'sys-libs/glibc-2.37-r10',
  'sys-libs/glibc-2.38-r13',
  'sys-libs/glibc-2.39-r11',
  'sys-libs/glibc-2.40-r11',
  'sys-libs/libucontext-1.3.1',
  'sys-libs/libucontext-1.3.2',
  'sys-libs/libucontext-1.3.3',
  'sys-libs/musl-1.2.5-r8',
  'sys-libs/musl-1.2.6',
  'sys-libs/musl-9999',
  'sys-libs/newlib-4.5.0.20241231-r1',
  'sys-libs/newlib-4.6.0.20260123',
  'sys-libs/newlib-9999',
  'sys-libs/obstack-standalone-1.2.3',
  'sys-libs/queue-standalone-0.1-r1',
  'sys-libs/rpmatch-standalone-1.0-r1',
  'sys-power/libacpica-0_pre20220331_p6',
  'sys-power/libacpica-9999',
  'sys-power/sandmann-bin-1.3.1',
  'sys-power/sandmann-bin-1.4.1-r1',
  'sys-process/systemd-cron-2.4.0',
  'sys-process/systemd-cron-2.4.1',
  'sys-process/systemd-cron-2.5.1',
  'virtual/libcrypt-1-r2',
  'virtual/perl-HTTP-Tiny-0.88.0',
  'virtual/perl-HTTP-Tiny-0.90.0',
  'virtual/perl-IO-Zlib-1.150.0-r1',
  'virtual/perl-Locale-Maketext-1.330.0-r2',
  'virtual/perl-Math-BigInt-FastCalc-0.501.800',
  'virtual/perl-Math-BigInt-FastCalc-0.502.0',
  'virtual/perl-Math-BigRat-2.3.2',
  'virtual/perl-Math-BigRat-2.5.2',
  'virtual/perl-Math-Complex-1.620.0-r1',
  'virtual/perl-Math-Complex-1.630.0',
  'virtual/perl-Module-Load-Conditional-0.740.0-r4',
  'virtual/perl-Params-Check-0.380.0-r15',
  'virtual/perl-Parse-CPAN-Meta-2.150.10-r9',
  'virtual/perl-Term-ReadLine-1.170.0-r9',
  'virtual/perl-Unicode-Collate-1.310.0-r3',
  'virtual/perl-Unicode-Normalize-1.320.0-r2',
  'virtual/perl-bignum-0.670.0-r1',
  'www-misc/profile-sync-daemon-6.35',
  'www-misc/profile-sync-daemon-6.50',
  'www-misc/profile-sync-daemon-9999',
  'www-plugins/chrome-binary-plugins-149.0.7779.3_alpha',
  'www-servers/nginx-unit-1.34.2',
  'www-servers/nginx-unit-1.35.0-r1',
  'x11-drivers/nvidia-drivers-390.157',
  'x11-drivers/nvidia-drivers-470.256.02-r2',
  'x11-drivers/nvidia-drivers-580.94.18',
  'x11-drivers/nvidia-drivers-595.44.05',
  'x11-misc/emacs-desktop-mail-1.3',
  'x11-themes/fluent-icon-theme-2025.08.21',
  'xfce-base/exo-4.21.0-r1',
  'xfce-base/garcon-4.21.0',
  'xfce-base/libxfce4ui-4.21.2',
  'xfce-base/libxfce4ui-4.21.3',
  'xfce-base/libxfce4ui-4.21.4',
  'xfce-base/libxfce4ui-4.21.7',
  'xfce-base/thunar-4.21.4',
  'xfce-base/thunar-4.21.5',
  'xfce-base/tumbler-4.21.0',
  'xfce-base/tumbler-4.21.1',
  'xfce-base/xfce4-appfinder-4.21.0',
  'xfce-base/xfce4-appfinder-4.21.1',
  'xfce-base/xfce4-panel-4.21.1',
  'xfce-base/xfce4-power-manager-4.21.0',
  'xfce-base/xfce4-power-manager-4.21.1',
  'xfce-base/xfce4-session-4.21.0',
  'xfce-base/xfce4-session-4.21.1',
  'xfce-base/xfce4-settings-4.21.0-r1',
  'xfce-base/xfce4-settings-4.21.1',
  'xfce-base/xfconf-4.21.0',
  'xfce-base/xfconf-4.21.1',
  'xfce-base/xfconf-4.21.2'
]).

% profile-mask-golden-end


% -----------------------------------------------------------------------------
%  Public entry points
% -----------------------------------------------------------------------------

%! profile_mask_golden_validate is semidet.
%! profile_mask_golden_validate(+Options) is semidet.
%
% Compare profile-derived package masks against the golden snapshot.
% Fails when the masked entry set differs (unless update(true)).
%
% Options:
%   * update(Bool)  -- rewrite golden instead of comparing (default false)
%   * verbose(Bool) -- print sample diffs (default true on mismatch)

profile_mask_golden_validate :-
  profile_mask_golden_validate([]).

profile_mask_golden_validate(Options) :-
  ( once(profile_mask_golden_validate_(Options)) ->
      true
  ; halt(1)
  ).

profile_mask_golden_validate_(Options) :-
  option(update(Update), Options, false),
  option(verbose(Verbose), Options, true),
  profile_mask_golden_require_inputs,
  profile_mask_golden_masked_ids(MaskedIds),
  length(MaskedIds, N),
  ( Update == true ->
      profile_mask_golden_unittest_file(Path),
      profile_mask_golden_write_source(MaskedIds),
      format('profile-mask golden updated: ~w (~D entries)~n', [Path, N])
  ; profile_mask_golden_expected(Expected),
    length(Expected, NE),
    ( MaskedIds == Expected ->
        format('profile-mask golden OK (~D entries).~n', [N])
    ;   ord_subtract(Expected, MaskedIds, OnlyExpected),
        ord_subtract(MaskedIds, Expected, OnlyActual),
        length(OnlyExpected, NExp), length(OnlyActual, NAct),
        format('profile-mask golden FAIL: expected ~D, got ~D (~D only-in-golden, ~D only-in-actual).~n',
               [NE, N, NExp, NAct]),
        ( Verbose == true ->
            profile_mask_golden_print_sample('only in golden', OnlyExpected),
            profile_mask_golden_print_sample('only in actual', OnlyActual)
        ; true
        ),
        fail
    )
  ).


%! profile_mask_golden_main is det.
%
% Makefile/CI entry point: load kb, then validate against the golden snapshot.

profile_mask_golden_main :-
  kb:load,
  profile_mask_golden_validate.


%! profile_mask_golden_update is det.
%
% Makefile entry point: load kb, then rewrite the golden snapshot in this file.

profile_mask_golden_update :-
  kb:load,
  profile_mask_golden_validate([update(true)]).


profile_mask_golden_unittest_file('Source/Test/unittest.pl').


%! profile_mask_golden_expected(-Ids) is det.
%
% Sorted entry ids from the checked-in golden snapshot.

profile_mask_golden_expected(Ids) :-
  profile_mask_golden_ids(Golden),
  sort(Golden, Ids).


%! profile_mask_golden_write_source(+Ids) is det.
%
% Rewrite the `profile_mask_golden_ids/1` block in this file.

profile_mask_golden_write_source(Ids) :-
  profile_mask_golden_unittest_file(RelPath),
  config:working_dir(Dir),
  atomic_list_concat([Dir, '/', RelPath], Path),
  read_file_to_string(Path, Content, [encoding(utf8)]),
  profile_mask_golden_replace_block(Content, Ids, NewContent),
  NewContent \== '',
  atomic_list_concat([Path, '.tmp'], TmpPath),
  setup_call_cleanup(
    open(TmpPath, write, Out, [encoding(utf8)]),
    format(Out, '~s', [NewContent]),
    close(Out)
  ),
  rename_file(TmpPath, Path).


profile_mask_golden_replace_block(Content, Ids, NewContent) :-
  profile_mask_golden_marker_begin(Begin),
  profile_mask_golden_marker_end(End),
  sub_string(Content, PrefixLen, BeginLen, _, Begin),
  sub_string(Content, EndBefore, EndLen, SuffixLen, End),
  EndBefore >= PrefixLen + BeginLen,
  sub_string(Content, 0, PrefixLen, _, Prefix),
  SuffixStart is EndBefore + EndLen,
  sub_string(Content, SuffixStart, SuffixLen, _, Suffix),
  profile_mask_golden_format_block(Ids, Block),
  string_concat(Prefix, Block, Temp),
  string_concat(Temp, Suffix, NewContent).


profile_mask_golden_marker_begin(Marker) :-
  atomic_list_concat(['% ', 'profile-mask', '-golden-begin', '\n'], Marker).


profile_mask_golden_marker_end(Marker) :-
  atomic_list_concat(['% ', 'profile-mask', '-golden-end', '\n'], Marker).


profile_mask_golden_format_block(Ids, Block) :-
  profile_mask_golden_marker_begin(Begin),
  profile_mask_golden_marker_end(End),
  length(Ids, N),
  profile_mask_golden_ids_list_lines(Ids, Lines),
  atomic_list_concat(Lines, "\n", Body),
  format(string(Header),
         '% Golden snapshot (~D entries). Regenerate: make test-profile-mask-golden-update~n',
         [N]),
  format(string(List), 'profile_mask_golden_ids([~n~s~n]).~n', [Body]),
  atomic_list_concat([Begin, Header, "\n", List, "\n", End], Block).


profile_mask_golden_ids_list_lines([], []).
profile_mask_golden_ids_list_lines([Last], [Line]) :-
  format(string(Line), "  ~q", [Last]).
profile_mask_golden_ids_list_lines([H|T], [Line|Rest]) :-
  T \== [],
  format(string(Line), "  ~q,", [H]),
  profile_mask_golden_ids_list_lines(T, Rest).


%! profile_mask_golden_require_inputs is det.
%
% Fail fast when kb/profile caches needed for the regression are missing.

profile_mask_golden_require_inputs :-
  ( current_predicate(cache:ordered_entry/5) ->
      true
  ;  throw(error(existence_error(procedure, cache:ordered_entry/5),
                 context(profile_mask_golden_validate/1,
                         'call kb:load before profile_mask_golden_validate/1')))
  ),
  ( profile:cache_load(_, _, _) ->
      true
  ;  throw(error(existence_error(source, 'Knowledge/profile.qlf'),
                 context(profile_mask_golden_validate/1,
                         'run --sync or build profile cache first')))
  ),
  ( current_predicate(profiledata:entry/3) ->
      true
  ;  throw(error(existence_error(procedure, profiledata:entry/3),
                 context(profile_mask_golden_validate/1, _)))
  ).


%! profile_mask_golden_masked_ids(-Ids) is det.
%
% Apply profile `package.mask` entries in cache order and return sorted ids.

profile_mask_golden_masked_ids(Ids) :-
  retractall(preference:local_masked(_)),
  forall(profiledata:entry(package_mask, Atom, true),
         profile:apply_entry(package_mask, Atom, true)),
  findall(Id,
          ( preference:local_masked(Masked),
            profile_mask_golden_entry_id(Masked, Id)
          ),
          Ids0),
  sort(Ids0, Ids).


profile_mask_golden_entry_id(Masked, Id) :-
  compound_name_arguments(Masked, Sep, [portage, Id]),
  Sep = '://'.


%! profile_mask_golden_print_sample(+Label, +Ids) is det.
%
% Print up to five sample ids from a diff list.

profile_mask_golden_print_sample(Label, Ids) :-
  length(Ids, N),
  ( N =:= 0 ->
      true
  ;   TopN is min(5, N),
      length(Sample, TopN),
      append(Sample, _, Ids),
      format('  ~w (~D):', [Label, N]),
      forall(member(Id, Sample), format(' ~w', [Id])),
      ( N > TopN -> format(' ...') ; true ),
      nl
  ).
