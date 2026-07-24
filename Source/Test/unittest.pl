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

:- module(unittest, [md5cache_validate/0,
                     md5cache_validate/1,
                     profile_mask_golden_validate/0,
                     profile_mask_golden_validate/1,
                     profile_mask_golden_main/0,
                     profile_mask_golden_update/0]).

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

test(simple_version, [true(V == version([1,0], '', 4, 0, [], 0, '1.0')), nondet]) :-
  atom_codes('1.0', Codes),
  phrase(eapi:version(V), Codes, []).

test(three_part_version, [true(V == version([1,2,3], '', 4, 0, [], 0, '1.2.3')), nondet]) :-
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

% PMS algorithm 3.5/3.6: multi-suffix versions compare pairwise by suffix
% type then number, not lexicographically on the rest string (issue #30).
test(multi_suffix_p_beats_pre, [nondet]) :-
  atom_codes('1.0_rc1_p2', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0_rc1_pre1', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(>, V1, V2).

test(multi_suffix_numeric_not_lexicographic, [nondet]) :-
  atom_codes('1.0_rc1_p10', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0_rc1_p9', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(>, V1, V2).

test(multi_suffix_shorter_below_p, [nondet]) :-
  atom_codes('1.0_rc1', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0_rc1_p1', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(<, V1, V2).

test(multi_suffix_shorter_above_pre, [nondet]) :-
  atom_codes('1.0_rc1', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0_rc1_pre1', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(>, V1, V2).

test(multi_suffix_pms_chain, [true(Order == [VA,VB,VC,VD,VE]), nondet]) :-
  atom_codes('1.0_rc1_pre1', CA), phrase(eapi:version(VA), CA, []),
  atom_codes('1.0_rc1', CB), phrase(eapi:version(VB), CB, []),
  atom_codes('1.0_rc1_p2', CC), phrase(eapi:version(VC), CC, []),
  atom_codes('1.0_rc1_p10', CD), phrase(eapi:version(VD), CD, []),
  atom_codes('1.0', CE), phrase(eapi:version(VE), CE, []),
  msort([VE, VC, VA, VD, VB], Order).

test(multi_suffix_equal_versions, [nondet]) :-
  atom_codes('1.0_rc1_p2', C1), phrase(eapi:version(V1), C1, []),
  atom_codes('1.0_rc1_p2', C2), phrase(eapi:version(V2), C2, []),
  eapi:version_compare(=, V1, V2).

:- end_tests(eapi_version_compare).


% -----------------------------------------------------------------------------
%  EAPI version comparison: PMS section 3.3 vectors (issue #73)
% -----------------------------------------------------------------------------
%
% Table-driven vectors for the numeric-component comparison rules of PMS
% algorithms 3.2/3.3, focusing on component count and numeric padding.

:- begin_tests(eapi_version_pms_vectors).

pms_version(Atom, V) :-
  atom_codes(Atom, Codes),
  phrase(eapi:version(V), Codes, []),
  !.

% Each vector is A-Op-B, asserting eapi:version_compare(Op, A, B).
pms_order_vector('1'    < '1.0').     % more numeric components wins
pms_order_vector('1.0'  < '1.0.0').
pms_order_vector('1.0'  < '1.1').
pms_order_vector('1.2'  < '1.10').    % numeric, not lexicographic
pms_order_vector('1.01' < '1.1').     % leading zero sorts first (PMS 3.3)
pms_order_vector('9.0'  < '10.0').    % multi-digit first component
pms_order_vector('1.99' < '2.0').

test(pms_numeric_order_vectors) :-
  forall(pms_order_vector(A < B),
         ( pms_version(A, VA),
           pms_version(B, VB),
           eapi:version_compare(<, VA, VB),
           eapi:version_compare(>, VB, VA)
         )).

% Numeric padding (PMS 3.3): trailing zeros in a padded component must not
% change the normalized numeric key ('1.0' and '1.00' share [1,0]). The
% version/7 term keeps the display string as final tie-break, so the full
% terms stay distinguishable for entry identity, but the order-relevant
% numeric prefix is identical.
test(pms_padding_same_numeric_key) :-
  pms_version('1.0',  version(N1, A1, R1, S1, T1, Rev1, _)),
  pms_version('1.00', version(N2, A2, R2, S2, T2, Rev2, _)),
  N1 == N2, A1 == A2, R1 == R2, S1 == S2, T1 == T2, Rev1 == Rev2.

test(pms_padding_not_smaller_than_unpadded, [fail]) :-
  pms_version('1.00', VA),
  pms_version('1.0',  VB),
  eapi:version_compare(<, VA, VB).

:- end_tests(eapi_version_pms_vectors).


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

% Strict PMS-only grammar: non-PMS synonyms and the ':=' slot operator
% must not be consumed as version operators (issue #31).
test(strict_rejects_arrow_ge, [fail]) :-
  phrase(eapi:operator(_), [61,62], []).                 % =>

test(strict_rejects_arrow_le, [fail]) :-
  phrase(eapi:operator(_), [61,60], []).                 % =<

test(strict_rejects_slot_operator, [fail]) :-
  phrase(eapi:operator(_), [58,61], []).                 % :=

% Lenient CLI grammar: accepts => and =< synonyms, still rejects ':='.
test(query_arrow_ge, [true(Op == greaterequal)]) :-
  phrase(eapi:query_operator(Op), [61,62], []).          % =>

test(query_arrow_le, [true(Op == smallerequal)]) :-
  phrase(eapi:query_operator(Op), [61,60], []).          % =<

test(query_greater_equal, [true(Op == greaterequal)]) :-
  phrase(eapi:query_operator(Op), [62,61], []).          % >=

test(query_rejects_slot_operator, [fail]) :-
  phrase(eapi:query_operator(_), [58,61], []).           % :=

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

% Metadata parsing must fail hard on non-PMS operator spellings (issue #31):
% ':=' is the slot operator, never a version operator; => and =< are
% CLI-only synonyms.
test(metadata_rejects_slot_operator_prefix, [fail]) :-
  atom_codes(':=dev-libs/openssl', Codes),
  phrase(eapi:depend(repo://entry, _), Codes, []).

test(metadata_rejects_arrow_ge, [fail]) :-
  atom_codes('=>dev-libs/openssl-1.1.0', Codes),
  phrase(eapi:depend(repo://entry, _), Codes, []).

test(metadata_rejects_arrow_le, [fail]) :-
  atom_codes('=<dev-libs/openssl-1.1.0', Codes),
  phrase(eapi:depend(repo://entry, _), Codes, []).

% Slot operator in its legal position (after the package name) still parses.
test(metadata_slot_operator_legal_position, [true(S == [any_same_slot]), nondet]) :-
  atom_codes('dev-libs/glib:=', Codes),
  phrase(eapi:depend(repo://entry,
    [package_dependency(install, no, 'dev-libs', glib, none, version_none, S, [])]), Codes, []).

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
%  EAPI REQUIRED_USE expression parsing tests (issue #73)
% -----------------------------------------------------------------------------
%
% PMS 8, Section 7.3.4: REQUIRED_USE supports flags, '!'-prefixed flags,
% USE-conditionals, '||' (or), '^^' (exactly-one-of) and '??'
% (at-most-one-of) groups, with arbitrary nesting.

:- begin_tests(eapi_required_use_parsing).

requse(Atom, U) :-
  atom_codes(Atom, Codes),
  phrase(eapi:required_use(test://e, U), Codes, []),
  !.

test(requse_plain_flag, [true(U == [required(foo)])]) :-
  requse('foo', U).

test(requse_blocking_flag, [true(U == [blocking(foo)])]) :-
  requse('!foo', U).

test(requse_flag_sequence, [true(U == [required(a), blocking(b), required(c)])]) :-
  requse('a !b c', U).

test(requse_positive_conditional,
     [true(U == [use_conditional_group(positive, doc, test://e, [required(man)])])]) :-
  requse('doc? ( man )', U).

test(requse_negative_conditional,
     [true(U == [use_conditional_group(negative, doc, test://e, [required(man)])])]) :-
  requse('!doc? ( man )', U).

test(requse_any_of_group,
     [true(U == [any_of_group([required(a), required(b)])])]) :-
  requse('|| ( a b )', U).

test(requse_exactly_one_of_group,
     [true(U == [exactly_one_of_group([required(a), required(b), required(c)])])]) :-
  requse('^^ ( a b c )', U).

test(requse_at_most_one_of_group,
     [true(U == [at_most_one_of_group([required(a), required(b)])])]) :-
  requse('?? ( a b )', U).

test(requse_nested_xor_in_conditional,
     [true(U == [use_conditional_group(positive, x, test://e,
                   [exactly_one_of_group([required(a), required(b)])])])]) :-
  requse('x? ( ^^ ( a b ) )', U).

test(requse_nested_conditional_in_or,
     [true(U == [any_of_group([use_conditional_group(positive, x, test://e,
                                 [required(a)]),
                               required(b)])])]) :-
  requse('|| ( x? ( a ) b )', U).

test(requse_blocking_inside_group,
     [true(U == [exactly_one_of_group([required(a), blocking(b)])])]) :-
  requse('^^ ( a !b )', U).

:- end_tests(eapi_required_use_parsing).


% -----------------------------------------------------------------------------
%  EAPI LICENSE group parsing tests (issue #73)
% -----------------------------------------------------------------------------
%
% PMS 8, Section 7.3: LICENSE is a dependency sequence whose leaves are
% license names; '||' groups, all-of groups and USE-conditionals apply.

:- begin_tests(eapi_license_parsing).

lic(Atom, L) :-
  atom_codes(Atom, Codes),
  phrase(eapi:license(test://e, L), Codes, []),
  !.

test(license_single, [true(L == ['GPL-2'])]) :-
  lic('GPL-2', L).

test(license_sequence, [true(L == ['GPL-2', 'LGPL-2.1'])]) :-
  lic('GPL-2 LGPL-2.1', L).

test(license_any_of_group,
     [true(L == [any_of_group(['GPL-2', 'BSD'])])]) :-
  lic('|| ( GPL-2 BSD )', L).

test(license_use_conditional,
     [true(L == [use_conditional_group(positive, doc, test://e, ['FDL-1.3'])])]) :-
  lic('doc? ( FDL-1.3 )', L).

test(license_all_of_inside_any_of,
     [true(L == [any_of_group([all_of_group(['MIT', 'BSD']), 'GPL-2'])])]) :-
  lic('|| ( ( MIT BSD ) GPL-2 )', L).

test(license_plus_suffix, [true(L == ['GPL-2+'])]) :-
  lic('GPL-2+', L).

:- end_tests(eapi_license_parsing).


% -----------------------------------------------------------------------------
%  EAPI bracketed USE-dependency syntax tests (issue #73)
% -----------------------------------------------------------------------------
%
% PMS 8, Section 8.3.4: 4-style USE dependencies ([flag], [-flag], [flag=],
% [!flag=], [flag?], [!flag?]) with (+)/(-) defaults.

:- begin_tests(eapi_usedep_brackets).

usedeps(Atom, U) :-
  atom_codes(Atom, Codes),
  phrase(eapi:use_dependencies(U), Codes, []),
  !.

test(usedep_enable, [true(U == [use(enable(foo), none)])]) :-
  usedeps('[foo]', U).

test(usedep_disable, [true(U == [use(disable(foo), none)])]) :-
  usedeps('[-foo]', U).

test(usedep_equal, [true(U == [use(equal(foo), none)])]) :-
  usedeps('[foo=]', U).

test(usedep_inverse_equal, [true(U == [use(inverse(foo), none)])]) :-
  usedeps('[!foo=]', U).

test(usedep_optenable, [true(U == [use(optenable(foo), none)])]) :-
  usedeps('[foo?]', U).

test(usedep_optdisable, [true(U == [use(optdisable(foo), none)])]) :-
  usedeps('[!foo?]', U).

test(usedep_default_positive, [true(U == [use(enable(foo), positive)])]) :-
  usedeps('[foo(+)]', U).

test(usedep_default_negative, [true(U == [use(enable(foo), negative)])]) :-
  usedeps('[foo(-)]', U).

test(usedep_equal_with_default, [true(U == [use(equal(foo), positive)])]) :-
  usedeps('[foo(+)=]', U).

test(usedep_optional_with_default, [true(U == [use(optdisable(foo), negative)])]) :-
  usedeps('[!foo(-)?]', U).

test(usedep_comma_list,
     [true(U == [use(enable(a), none),
                 use(equal(b), none),
                 use(optdisable(c), none)])]) :-
  usedeps('[a,b=,!c?]', U).

% Full package dependency carrying bracketed USE deps.
test(usedep_in_package_dependency, [nondet]) :-
  atom_codes('>=dev-libs/foo-1.2[bar=,!baz?]', Codes),
  phrase(eapi:package_dependency(install, test://e, D), Codes, []),
  D = package_dependency(install, no, 'dev-libs', foo, greaterequal, V, [], U),
  eapi:version_full(V, '1.2'),
  U == [use(equal(bar), none), use(optdisable(baz), none)].

:- end_tests(eapi_usedep_brackets).


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
%  Missing-provider feedback tests (portage-ng#102)
% =============================================================================

% -----------------------------------------------------------------------------
%  Detector corpus: each signature variant normalizes to symbol(Kind, Name)
% -----------------------------------------------------------------------------

:- begin_tests(missing_provider_detectors).

test(cmd_not_found_bash, [true(Sym == symbol(command, semodule_package)), nondet]) :-
  missing_provider:detector(["/bin/sh: line 1: semodule_package: command not found"], Sym, _).

test(cmd_not_found_prefix, [true(Sym == symbol(command, semodule_package)), nondet]) :-
  missing_provider:detector(["make[1]: semodule_package: command not found"], Sym, _).

test(cmd_not_found_dash, [true(Sym == symbol(command, checkmodule)), nondet]) :-
  missing_provider:detector(["sh: 1: checkmodule: not found"], Sym, _).

test(cmd_not_found_env, [true(Sym == symbol(command, gperf)), nondet]) :-
  missing_provider:detector(["env: 'gperf': No such file or directory"], Sym, _).

test(missing_header, [true(Sym == symbol(header, 'foo/bar.h')), nondet]) :-
  missing_provider:detector(["main.c:3:10: fatal error: foo/bar.h: No such file or directory"], Sym, _).

test(missing_lib, [true(Sym == symbol(lib, crypto)), nondet]) :-
  missing_provider:detector(["/usr/bin/ld: cannot find -lcrypto"], Sym, _).

test(missing_soname, [true(Sym == symbol(soname, 'libfoo.so.1')), nondet]) :-
  missing_provider:detector(["prog: error while loading shared libraries: libfoo.so.1: cannot open shared object file: No such file"], Sym, _).

test(pkgconfig_notfound, [true(Sym == symbol(pkgconfig, glib_2_0)), nondet]) :-
  missing_provider:detector(["No package 'glib_2_0' found"], Sym, _).

test(pkgconfig_searchpath, [true(Sym == symbol(pkgconfig, 'libssl')), nondet]) :-
  missing_provider:detector(["Package libssl was not found in the pkg-config search path"], Sym, _).

test(python_module, [true(Sym == symbol(python_module, setuptools)), nondet]) :-
  missing_provider:detector(["ModuleNotFoundError: No module named 'setuptools'"], Sym, _).

test(perl_module, [true(Sym == symbol(perl_module, 'Foo/Bar.pm')), nondet]) :-
  missing_provider:detector(["Can't locate Foo/Bar.pm in @INC (you may need to install the Foo::Bar module)"], Sym, _).

test(no_false_positive_on_prose, [fail]) :-
  missing_provider:detector(["checking for a working compiler... yes", "configure: creating ./config.status"], symbol(command, _), _).

:- end_tests(missing_provider_detectors).


% -----------------------------------------------------------------------------
%  Provider dependency term shape (unioned into BDEPEND)
% -----------------------------------------------------------------------------

:- begin_tests(missing_provider_provider_dep).

test(unversioned_shape,
     [true(Dep == package_dependency(install, no, 'sys-apps', 'semodule-utils', none, version_none, [], []))]) :-
  feedback:provider_dep('sys-apps/semodule-utils', Dep).

test(rejects_non_cn, [fail]) :-
  feedback:provider_dep('semodule-utils', _).

test(usedep_shape,
     [true(Dep == package_dependency(install, no, 'kde-frameworks', kwindowsystem, none, version_none, [],
                                     [use(enable('X'), none)]))]) :-
  feedback:provider_dep('kde-frameworks/kwindowsystem',
                        [use(enable('X'), none)], Dep).

:- end_tests(missing_provider_provider_dep).


% USE-enable learning from build failures (portage-ng#110).
:- begin_tests(useenable_detectors).

test(kx11extras_header) :-
  useenable:detector(
    ["fatal error: KX11Extras: No such file or directory"],
    symbol(kf_header, 'KX11Extras'), _).

test(kstartupinfo_header) :-
  useenable:detector(
    ["error: KStartupInfo: No such file or directory"],
    symbol(kf_header, 'KStartupInfo'), _).

test(pimcommon_activities_header) :-
  useenable:detector(
    ["fatal error: KPim6PimCommonActivities: No such file or directory"],
    symbol(kf_header, 'KPim6PimCommonActivities'), _).

% Real kaddressbook configure log: CMake find_package, not a fatal #include.
test(pimcommon_activities_could_not_find) :-
  useenable:detector(
    ["-- Could NOT find KPim6PimCommonActivities (missing: KPim6PimCommonActivities_DIR)"],
    symbol(kf_header, 'KPim6PimCommonActivities'), _).

% Real kget compile log: GCC colors wrap the header token in CSI sequences.
test(kx11extras_ansi_colored) :-
  useenable:detector(
    ["\u001b[01m\u001b[K/tmp/droptarget.cpp:37:10:\u001b[m\u001b[K \u001b[01;31m\u001b[Kfatal error: \u001b[m\u001b[KKX11Extras: No such file or directory"],
    symbol(kf_header, 'KX11Extras'), _).

test(strip_ansi_removes_embedded_csi) :-
  useenable:strip_ansi(
    "fatal error: \u001b[m\u001b[KKX11Extras: No such file",
    Clean),
  Clean == "fatal error: KX11Extras: No such file".

test(cmake_xdamage) :-
  useenable:detector(
    ["-- Looking for X11_Xdamage_LIB - NOTFOUND"],
    symbol(cmake_lib, 'X11_Xdamage'), _).

test(ignores_regular_header, [fail]) :-
  useenable:detector(
    ["fatal error: foo/bar.h: No such file or directory"],
    symbol(kf_header, _), _).

test(ignores_unrelated_could_not_find, [fail]) :-
  useenable:detector(
    ["-- Could NOT find ReuseTool (missing: REUSETOOL_EXECUTABLE)"],
    symbol(kf_header, _), _).

:- end_tests(useenable_detectors).


:- begin_tests(useenable_seed).

test(kx11extras_maps_to_kwindowsystem_x) :-
  useenable:provides_usedep(kf_header, 'KX11Extras',
                            'kde-frameworks/kwindowsystem',
                            [use(enable('X'), none)]).

test(activities_maps_to_pimcommon) :-
  useenable:provides_usedep(kf_header, 'KPim6PimCommonActivities',
                            'kde-apps/pimcommon',
                            [use(enable(activities), none)]).

test(xdamage_bare_cn) :-
  useenable:provides_usedep(cmake_lib, 'X11_Xdamage',
                            'x11-libs/libXdamage', []).

:- end_tests(useenable_seed).


% GHC boot-lib / ghc-pkg readiness (portage-ng#108).
:- begin_tests(ghcabi_boot_dep).

test(boot_dep_detects_cabal_missing,
     [true(Libs == [bytestring, deepseq, 'ghc-prim', 'template-haskell'])]) :-
  tmp_file(ghcboot, Path),
  setup_call_cleanup(
    ( open(Path, write, S),
      format(S,
"Error: setup: Encountered missing or private dependencies:~n\
bytestring >=0.10.4 && <0.12,~n\
deepseq >=1.1 && <1.5,~n\
ghc-prim >=0.2 && <0.9,~n\
template-haskell >=2.5 && <2.19~n", []),
      close(S) ),
    ghcabi:boot_dep_error(Path, 0, Libs),
    catch(delete_file(Path), _, true)).

test(boot_dep_ignores_unrelated_log, [fail]) :-
  tmp_file(ghcboot2, Path),
  setup_call_cleanup(
    ( open(Path, write, S),
      format(S, "configure: error: something else~n", []),
      close(S) ),
    ghcabi:boot_dep_error(Path, 0, _),
    catch(delete_file(Path), _, true)).

test(excluded_version_roundtrip,
     [setup(( feedback:record_excluded_version('dev-haskell', text,
                 version([1,2,5], '', 4, 0, [], 1, '1.2.5.0-r1'),
                 evidence(test)) )),
      cleanup(retractall(feedback:excluded_version('dev-haskell', text, _, _)))]) :-
  feedback:excluded_version('dev-haskell', text, _, evidence(test)),
  feedback:excluded_version_count(N),
  N >= 1.

:- end_tests(ghcabi_boot_dep).


% -----------------------------------------------------------------------------
%  Curated seed resolver
% -----------------------------------------------------------------------------

:- begin_tests(missing_provider_resolver).

test(seed_semodule, [true(P-C == 'sys-apps/semodule-utils'-curated_seed), nondet]) :-
  missing_provider:provider_of(command, semodule_package, P, C).

test(seed_unknown_fails, [fail]) :-
  missing_provider:provider_of(command, this_command_has_no_provider_xyz, _, curated_seed).

:- end_tests(missing_provider_resolver).


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


% REQUIRED_USE choice-group seeding (portage-ng#87/#88): when a package is
% pulled with an empty build_with_use but its own `|| ( ... )` / `^^ ( ... )`
% choice group has no satisfied member under the profile defaults, the
% resolver must seed the highest-priority member so the package (and its
% conditional dep[flag?] edges) build with the chosen backend. We exercise
% the profile-independent logic directly using a non-existent entry (so
% effective USE is empty and entry_iuse_plus_default is empty), forcing the
% deterministic "last member" fallback.

:- begin_tests(rules_required_use_choice_seed).

test(pick_falls_back_to_last_member, [true(F == z3)]) :-
  use:requse_pick_satisfying_flag(portage://'nonexistent-pkg-0',
      [required(z1), required(z2), required(z3)], F).

test(any_of_unsatisfied_needs_seed, [true(F == z3)]) :-
  use:choice_group_needs_seed(portage://'nonexistent-pkg-0', [], [],
      any_of_group([required(z1), required(z2), required(z3)]), F).

test(any_of_already_satisfied_no_seed, [fail]) :-
  use:choice_group_needs_seed(portage://'nonexistent-pkg-0', [z2], [],
      any_of_group([required(z1), required(z2), required(z3)]), _).

test(exactly_one_unsatisfied_needs_seed, [true(F == z3)]) :-
  use:choice_group_needs_seed(portage://'nonexistent-pkg-0', [], [],
      exactly_one_of_group([required(z1), required(z2), required(z3)]), F).

test(at_most_one_never_seeds, [fail]) :-
  use:choice_group_needs_seed(portage://'nonexistent-pkg-0', [], [],
      at_most_one_of_group([required(z1), required(z2), required(z3)]), _).

test(seed_term_enables_chosen_flag, [true(Out == use_state([z3], []))]) :-
  use:seed_choice_group_term(portage://'nonexistent-pkg-0',
      any_of_group([required(z1), required(z2), required(z3)]),
      use_state([], []), Out).

test(seed_term_passthrough_when_satisfied, [true(Out == use_state([z2], []))]) :-
  use:seed_choice_group_term(portage://'nonexistent-pkg-0',
      any_of_group([required(z1), required(z2), required(z3)]),
      use_state([z2], []), Out).

% A plain required()/conditional term must NOT be seeded from empty BWU
% (only choice groups are), preserving the documented samba-safe behavior.
test(plain_required_not_seeded, [fail]) :-
  use:choice_group_needs_seed(portage://'nonexistent-pkg-0', [], [],
      required(z1), _).

% The seed loop must seed the choice group but leave conditional terms
% (e.g. net-fs/samba's `gpg? ( addc )`) untouched, so it never flips a
% flag away from the profile default and poisons the build_with_use.
test(seed_loop_ignores_conditional_terms,
     [true(Out == use_state([z3], []))]) :-
  use:seed_choice_groups_loop(portage://'nonexistent-pkg-0',
      [ use_conditional_group(positive, gpg, portage://'nonexistent-pkg-0',
                              [required(addc)]),
        any_of_group([required(z1), required(z2), required(z3)]) ],
      use_state([], []), Out, 5).

% A profile-masked member must never be seeded: the profile strips it back
% off at build time, so seeding it guarantees a REQUIRED_USE failure (e.g.
% ^^ ( elogind systemd ) on a non-systemd profile where `systemd` is
% use-masked -> the seeder must pick elogind). portage-ng#91.
test(masked_member_excluded_from_seed,
     [setup(assertz(preference:local_profile_masked_use_flag(z3))),
      cleanup(retract(preference:local_profile_masked_use_flag(z3))),
      true(F == z2)]) :-
  use:requse_pick_satisfying_flag(portage://'nonexistent-pkg-0',
      [required(z1), required(z2), required(z3)], F).

test(seedable_false_for_masked_flag,
     [setup(assertz(preference:local_profile_masked_use_flag(z3))),
      cleanup(retract(preference:local_profile_masked_use_flag(z3))),
      fail]) :-
  use:requse_flag_seedable(portage://'nonexistent-pkg-0', z3).

test(seedable_true_for_unmasked_flag, [true]) :-
  use:requse_flag_seedable(portage://'nonexistent-pkg-0', z1).

% When every member is masked the group is genuinely unsatisfiable; the
% seeder falls back to the full list (last member) rather than failing, so
% the normal domain-assumption path still reports it.
test(all_masked_falls_back_to_full_list,
     [setup(( assertz(preference:local_profile_masked_use_flag(z1)),
              assertz(preference:local_profile_masked_use_flag(z2)),
              assertz(preference:local_profile_masked_use_flag(z3)) )),
      cleanup(( retract(preference:local_profile_masked_use_flag(z1)),
                retract(preference:local_profile_masked_use_flag(z2)),
                retract(preference:local_profile_masked_use_flag(z3)) )),
      true(F == z3)]) :-
  use:requse_pick_satisfying_flag(portage://'nonexistent-pkg-0',
      [required(z1), required(z2), required(z3)], F).

:- end_tests(rules_required_use_choice_seed).


% Joint USE-dep / REQUIRED_USE / profile-hard fail-closed checks
% (portage-ng#109/#111 — emerge use_dep_unsat class).
%
% KB-independent: synthetic qtest entry + memo:eff_use_cache_/4 (same pattern
% as builder_base_use_state). CI has no Portage tree, so live acct-user/git
% metadata must not be required.
:- begin_tests(rules_use_dep_unsat).

ude_entry(qtest://'acct-user/git-0').
ude_requse(exactly_one_of_group([required(git), required(gitea),
                                 required(gitolite)])).

ude_setup :-
  ude_entry(Repo://Id),
  retractall(memo:eff_use_cache_(Repo, Id, _, _)),
  retractall(cache:entry_metadata(Repo, Id, required_use, _)),
  ude_requse(RU),
  assertz(cache:entry_metadata(Repo, Id, required_use, RU)),
  % Profile/default-on sibling (not HARD atom): positive via eff-use memo.
  assertz(memo:eff_use_cache_(Repo, Id, git, positive)).

ude_cleanup :-
  ude_entry(Repo://Id),
  retractall(memo:eff_use_cache_(Repo, Id, _, _)),
  retractall(cache:entry_metadata(Repo, Id, required_use, _)).

% Exactly-one-of with two positives: disable the non-HARD sibling.
test(exactly_one_of_n_gt_1_disables_non_hard,
     [setup(ude_setup), cleanup(ude_cleanup),
      true(Fixes == [disable(git)])]) :-
  ude_entry(E), ude_requse(RU),
  use:requse_term_fixes(E, [gitea], [], RU, Fixes).

% Two HARD enables in ^^ cannot be fixed by disable — Fixes fails.
test(exactly_one_of_two_hard_unfixable,
     [setup(ude_setup), cleanup(ude_cleanup), fail]) :-
  ude_entry(E), ude_requse(RU),
  use:requse_term_fixes(E, [git, gitea], [], RU, _).

% HARD enable of a globally masked flag is use_dep_unsat.
test(bwu_rejects_masked_hard_enable,
     [setup(assertz(preference:local_profile_masked_use_flag(gitea))),
      cleanup(retract(preference:local_profile_masked_use_flag(gitea))),
      fail]) :-
  ude_entry(E),
  use:bwu_respects_profile_hard(E, use_state([gitea], [])).

test(bwu_accepts_unmasked_hard_enable, [true]) :-
  ude_entry(E),
  use:bwu_respects_profile_hard(E, use_state([gitea], [])).

% Post-stabilize joint check: REQUIRED_USE ^^ with two HARD enables fails.
test(use_dep_atom_unsat_on_hard_collision,
     [setup(ude_setup), cleanup(ude_cleanup), fail]) :-
  ude_entry(E),
  use:use_dep_atom_satisfiable(E, use_state([git, gitea], [])).

test(use_dep_atom_sat_after_disable_sibling,
     [setup(ude_setup), cleanup(ude_cleanup), true]) :-
  ude_entry(E),
  use:use_dep_atom_satisfiable(E, use_state([gitea], [git, gitolite])).

:- end_tests(rules_use_dep_unsat).


% ||-branch ranking prefers the arm that admits the newest tree version
% (portage-ng#112 — cabal's text 1.2 vs 2.x OR).
:- begin_tests(ranking_any_of_version_branch).

rao_setup :-
  retractall(cache:ordered_entry(qtest, _, 'dev-haskell', text, _)),
  assertz(cache:ordered_entry(qtest, 'dev-haskell/text-1.2.5.0-r1',
                              'dev-haskell', text,
                              version([1,2,5,0],'',4,0,[],1,'1.2.5.0-r1'))),
  assertz(cache:ordered_entry(qtest, 'dev-haskell/text-2.1.1',
                              'dev-haskell', text,
                              version([2,1,1],'',4,0,[],0,'2.1.1'))).

rao_cleanup :-
  retractall(cache:ordered_entry(qtest, _, 'dev-haskell', text, _)).

rao_text12(all_of_group([
  package_dependency(run,no,'dev-haskell',text,greaterequal,
                     version([1,2,3,0],'',4,0,[],0,'1.2.3.0'),[],[]),
  package_dependency(run,no,'dev-haskell',text,smaller,
                     version([1,3],'',4,0,[],0,'1.3'),[],[])])).

rao_text2(all_of_group([
  package_dependency(run,no,'dev-haskell',text,greaterequal,
                     version([2,0],'',4,0,[],0,'2.0'),[],[]),
  package_dependency(run,no,'dev-haskell',text,smaller,
                     version([2,2],'',4,0,[],0,'2.2'),[],[])])).

test(prefers_newer_text_branch_first,
     [setup(rao_setup), cleanup(rao_cleanup)]) :-
  rao_text12(B1), rao_text2(B2),
  ranking:prioritize_deps_keep_all([B1, B2], [], [First|_]),
  First == B2.

test(prefers_newer_even_when_listed_second,
     [setup(rao_setup), cleanup(rao_cleanup)]) :-
  rao_text12(B1), rao_text2(B2),
  ranking:prioritize_deps_keep_all([B2, B1], [], [First|_]),
  First == B2.

:- end_tests(ranking_any_of_version_branch).


% ||-branch ranking: USE-sat, SnapAll, SlotScore, NoDowngrade (emerge bins).
:- begin_tests(ranking_any_of_preference_keys).

rap_foo_entry(qtest://'cat/foo-1.0').
rap_foo_ver(version([1,0],'',4,0,[],0,'1.0')).

rap_use_setup :-
  rap_foo_entry(R://Id),
  rap_foo_ver(V),
  retractall(cache:ordered_entry(qtest, _, cat, foo, _)),
  retractall(memo:iuse_info_cache_(qtest, Id, _)),
  retractall(memo:effective_use_fact(qtest, Id, _)),
  assertz(cache:ordered_entry(R, Id, cat, foo, V)),
  assertz(memo:iuse_info_cache_(R, Id, iuse_info([a, b], []))),
  assertz(memo:effective_use_fact(R, Id, [a])).

rap_use_cleanup :-
  rap_foo_entry(_://Id),
  retractall(cache:ordered_entry(qtest, _, cat, foo, _)),
  retractall(memo:iuse_info_cache_(qtest, Id, _)),
  retractall(memo:effective_use_fact(qtest, Id, _)),
  retractall(preference:local_profile_masked_use_flag(b)).

rap_arm_use(Flag, package_dependency(run, no, cat, foo, none, version_none, [],
                                     [use(enable(Flag), none)])).

test(prefers_use_satisfied_arm,
     [setup(rap_use_setup), cleanup(rap_use_cleanup)]) :-
  rap_arm_use(a, Ba),
  rap_arm_use(b, Bb),
  ranking:prioritize_deps_keep_all([Bb, Ba], [], [First|_]),
  First == Ba.

test(prefers_use_unmasked_among_unsat,
     [setup(( rap_use_setup,
              rap_foo_entry(_://Id),
              retractall(memo:effective_use_fact(qtest, Id, _)),
              assertz(memo:effective_use_fact(qtest, Id, [])),
              assertz(preference:local_profile_masked_use_flag(b)) )),
      cleanup(rap_use_cleanup)]) :-
  rap_arm_use(a, Ba),
  rap_arm_use(b, Bb),
  ranking:prioritize_deps_keep_all([Bb, Ba], [], [First|_]),
  First == Ba.

rap_snap_setup :-
  empty_assoc(Empty),
  nb_setval(memo_selected_cn_snap, Empty),
  retractall(cache:ordered_entry(qtest, _, cat, _, _)),
  assertz(cache:ordered_entry(qtest, 'cat/a-1', cat, a,
                              version([1],'',4,0,[],0,'1'))),
  assertz(cache:ordered_entry(qtest, 'cat/b-1', cat, b,
                              version([1],'',4,0,[],0,'1'))),
  assertz(cache:ordered_entry(qtest, 'cat/c-1', cat, c,
                              version([1],'',4,0,[],0,'1'))),
  cnselect:record_selected_cn_snapshot(cat, a,
    [selected(qtest, 'cat/a-1', run, v, '0')]),
  cnselect:record_selected_cn_snapshot(cat, b,
    [selected(qtest, 'cat/b-1', run, v, '0')]).

rap_snap_cleanup :-
  empty_assoc(Empty),
  nb_setval(memo_selected_cn_snap, Empty),
  retractall(cache:ordered_entry(qtest, _, cat, _, _)).

rap_pkg(N, package_dependency(run, no, cat, N, none, version_none, [], [])).

test(prefers_snap_all_arm,
     [setup(rap_snap_setup), cleanup(rap_snap_cleanup)]) :-
  rap_pkg(a, A), rap_pkg(b, B), rap_pkg(c, C),
  All = all_of_group([A, B]),
  Partial = all_of_group([A, C]),
  ranking:prioritize_deps_keep_all([Partial, All], [], [First|_]),
  First == All.

rap_slot_setup :-
  retractall(cache:ordered_entry(qtest, _, cat, llvm, _)),
  assertz(cache:ordered_entry(qtest, 'cat/llvm-18', cat, llvm,
                              version([18],'',4,0,[],0,'18'))),
  assertz(cache:ordered_entry(qtest, 'cat/llvm-20', cat, llvm,
                              version([20],'',4,0,[],0,'20'))).

rap_slot_cleanup :-
  retractall(cache:ordered_entry(qtest, _, cat, llvm, _)).

rap_slot_arm(Slot, package_dependency(run, no, cat, llvm, none, version_none,
                                      [slot(Slot)], [])).

test(prefers_higher_slot_arm,
     [setup(rap_slot_setup), cleanup(rap_slot_cleanup)]) :-
  rap_slot_arm(18, S18),
  rap_slot_arm(20, S20),
  ranking:prioritize_deps_keep_all([S18, S20], [], [First|_]),
  First == S20.

rap_text_setup :-
  retractall(cache:ordered_entry(qtest, _, 'dev-haskell', text, _)),
  assertz(cache:ordered_entry(qtest, 'dev-haskell/text-1.2.5.0-r1',
                              'dev-haskell', text,
                              version([1,2,5,0],'',4,0,[],1,'1.2.5.0-r1'))),
  assertz(cache:ordered_entry(qtest, 'dev-haskell/text-2.1.1',
                              'dev-haskell', text,
                              version([2,1,1],'',4,0,[],0,'2.1.1'))),
  empty_assoc(Empty),
  nb_setval(memo_selected_cn_snap, Empty),
  cnselect:record_selected_cn_snapshot('dev-haskell', text,
    [selected(qtest, 'dev-haskell/text-2.1.1', run,
              version([2,1,1],'',4,0,[],0,'2.1.1'), '0')]).

rap_text_cleanup :-
  retractall(cache:ordered_entry(qtest, _, 'dev-haskell', text, _)),
  empty_assoc(Empty),
  nb_setval(memo_selected_cn_snap, Empty).

rap_text12(all_of_group([
  package_dependency(run,no,'dev-haskell',text,greaterequal,
                     version([1,2,3,0],'',4,0,[],0,'1.2.3.0'),[],[]),
  package_dependency(run,no,'dev-haskell',text,smaller,
                     version([1,3],'',4,0,[],0,'1.3'),[],[])])).

rap_text2(all_of_group([
  package_dependency(run,no,'dev-haskell',text,greaterequal,
                     version([2,0],'',4,0,[],0,'2.0'),[],[]),
  package_dependency(run,no,'dev-haskell',text,smaller,
                     version([2,2],'',4,0,[],0,'2.2'),[],[])])).

test(no_downgrade_demotes_older_arm,
     [setup(rap_text_setup), cleanup(rap_text_cleanup)]) :-
  rap_text12(B1), rap_text2(B2),
  ranking:dep_choice_scores([], B1,
    scores(_, _, _, _, ND1, _, _)),
  ranking:dep_choice_scores([], B2,
    scores(_, _, _, _, ND2, _, _)),
  ND1 =:= 0,
  ND2 =:= 1.

:- end_tests(ranking_any_of_preference_keys).


% CABAL_CORE_LIB_GHC_PV parse + match (portage-ng#112).
:- begin_tests(ghcabi_cabal_core).

test(parse_quoted_list,
     [true(PVs == ['9.0.2','9.2.8'])]) :-
  ghcabi:parse_cabal_core_line('CABAL_CORE_LIB_GHC_PV="9.0.2 9.2.8"', PVs), !.

test(match_exact) :-
  ghcabi:cabal_core_matches(['9.8.2','9.8.4'], '9.8.4').

test(match_glob) :-
  ghcabi:cabal_core_matches(['9.8.*'], '9.8.4').

test(nomatch_other_series, [fail]) :-
  ghcabi:cabal_core_matches(['9.0.2','9.2.8'], '9.8.4').

:- end_tests(ghcabi_cabal_core).


% Sub-slot (:=) ABI rebuild propagation helpers (portage-ng#89).
:- begin_tests(pipeline_subslot_rebuild).

% A := dependency (any_same_slot) binds to the provider's sub-slot in any slot.
test(any_same_slot_binds_any_slot, [true]) :-
  pipeline:subslot_bound_slotspec([any_same_slot], '0').

test(any_same_slot_binds_other_slot, [true]) :-
  pipeline:subslot_bound_slotspec([any_same_slot], '2').

% A :slot= dependency binds only when the slot matches the changed provider.
test(slot_equal_binds_matching_slot, [true]) :-
  pipeline:subslot_bound_slotspec([slot('0'), equal], '0').

test(slot_equal_rejects_other_slot, [fail]) :-
  pipeline:subslot_bound_slotspec([slot('1'), equal], '0').

% A :slot/subslot= dependency binds on slot match (sub-slot is the trigger).
test(slot_subslot_equal_binds_matching_slot, [true]) :-
  pipeline:subslot_bound_slotspec([slot('0'), subslot('1.2'), equal], '0').

% A plain slot / sub-slot dependency without `=` is NOT a rebuild trigger.
test(plain_slot_not_bound, [fail]) :-
  pipeline:subslot_bound_slotspec([slot('0')], '0').

test(plain_slot_subslot_not_bound, [fail]) :-
  pipeline:subslot_bound_slotspec([slot('0'), subslot('1.2')], '0').

test(any_different_slot_not_bound, [fail]) :-
  pipeline:subslot_bound_slotspec([any_different_slot], '0').

test(empty_slot_not_bound, [fail]) :-
  pipeline:subslot_bound_slotspec([], '0').

% The consumer rebuild goal is a same-version :update that replaces the VDB
% entry and carries the subslot_change reason so the printer renders the note
% and the prover re-walks deps (ordering the rebuild after the provider).
test(consumer_goal_shape,
     [true(Goal == portage://'dev-x/c-1':update?{[replaces(pkg://'dev-x/c-1'),
              rebuild_reason(subslot_change('dev-x'/p, '0', '1'))]})]) :-
  pipeline:subslot_consumer_goal(
      c('dev-x/c-1', portage, 'dev-x'/p, '0', '1'), Goal).

% A goal list with no `pkg`/tree entries (synthetic) targets no real CN, and
% an empty plan changes no provider, so the augmentation finds nothing.
test(no_changed_providers_on_empty_plan, [true(Changed == [])]) :-
  pipeline:subslot_changed_providers([], Changed).

test(extra_goals_fails_without_changes, [fail]) :-
  pipeline:subslot_extra_goals([], [], _).

:- end_tests(pipeline_subslot_rebuild).


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
  ranking:seed_bwu_memo_from_dep_tree(InstallDeps),
  ranking:seed_bwu_memo_from_dep_tree(RunDeps),
  memo:candidate_bwu_('dev-libs', glib, M).

:- end_tests(use_candidate_bwu_memo).


% Deferred shared-dep USE-force flush (portage-ng#94): newly-learned forces
% are recorded in memo:bwu_force_pending_/3 instead of aborting the pass;
% heuristic:reprove_pending/1 reports them after the pass completes and
% prover:deferred_reprove_pending/3 turns them into a single batched reprove.

:- begin_tests(bwu_force_deferred_flush).

test(record_and_report_pending, [true(Pending == [bwu_force('dev-qt', qtbase, [icu, wayland])])]) :-
  use:clear_bwu_cross_dep_memos,
  use:record_bwu_force_pending('dev-qt', qtbase, [icu, wayland]),
  heuristic:reprove_pending(bwu_force_flush(Pending)).

test(record_replaces_previous_pending_for_same_cn, [true(Pending == [bwu_force('dev-qt', qtbase, [icu, wayland])])]) :-
  use:clear_bwu_cross_dep_memos,
  use:record_bwu_force_pending('dev-qt', qtbase, [icu]),
  use:record_bwu_force_pending('dev-qt', qtbase, [icu, wayland]),
  heuristic:reprove_pending(bwu_force_flush(Pending)).

test(no_pending_after_clear, [fail]) :-
  use:clear_bwu_cross_dep_memos,
  use:record_bwu_force_pending('dev-qt', qtbase, [icu]),
  use:clear_bwu_cross_dep_memos,
  heuristic:reprove_pending(_).

test(handle_reprove_confirms_flush_progress, [true(Added == true)]) :-
  heuristic:handle_reprove(bwu_force_flush([bwu_force('dev-qt', qtbase, [icu])]), Added).

test(deferred_reprove_pending_reports_flush_within_budget,
     [true(Info == bwu_force_flush([bwu_force('dev-qt', qtbase, [icu])])),
      cleanup(use:clear_bwu_cross_dep_memos)]) :-
  use:clear_bwu_cross_dep_memos,
  use:record_bwu_force_pending('dev-qt', qtbase, [icu]),
  prover:deferred_reprove_pending(0, 20, Info).

test(deferred_reprove_pending_fails_when_budget_exhausted,
     [fail, cleanup(use:clear_bwu_cross_dep_memos)]) :-
  use:clear_bwu_cross_dep_memos,
  use:record_bwu_force_pending('dev-qt', qtbase, [icu]),
  prover:deferred_reprove_pending(20, 20, _).

test(deferred_reprove_pending_fails_when_nothing_pending, [fail]) :-
  use:clear_bwu_cross_dep_memos,
  prover:deferred_reprove_pending(0, 20, _).

:- end_tests(bwu_force_deferred_flush).


% Conflict-driven partial restart (non-chronological backtracking): after a
% completed pass reports a deferred conflict, the prover prunes only the
% affected literals (domain seeds + dependents-closure over Triggers) from the
% completed artifacts and resumes proving from that state.  Generic machinery
% lives in prover.pl; seeds, obligation anchors and constraint scoping are
% domain hooks in heuristic.pl.

:- begin_tests(partial_restart).

test(triggers_closure_transitive, [true(Keys == [a, b, c])]) :-
  list_to_assoc([a-[b], b-[c], d-[e]], Triggers),
  prover:triggers_closure([a], Triggers, Affected),
  assoc_to_keys(Affected, Keys).

test(triggers_closure_handles_shared_dependents, [true(Keys == [a, b, c])]) :-
  list_to_assoc([a-[b, c], b-[c], c-[]], Triggers),
  prover:triggers_closure([a], Triggers, Affected),
  assoc_to_keys(Affected, Keys).

test(prune_model_removes_affected_plain_assumed_naf_keys, [true(Keys == [c])]) :-
  list_to_assoc([a-ctx1, assumed(b)-ctx2, naf(a)-ctx3, c-ctx4], Model),
  list_to_assoc([a-true, b-true], Affected),
  prover:prune_model(Model, Affected, RModel),
  assoc_to_keys(RModel, Keys).

test(prune_proof_removes_affected_entries,
     [true(Keys == [obligation_done(pdepend_none(c)), rule(c)])]) :-
  list_to_assoc([rule(a)-v1,
                 assumed(rule(b))-v2,
                 cycle_path(a)-v3,
                 obligation_pending(b)-v4,
                 obligation_done(pdepend(a, bwu))-v5,
                 obligation_done(pdepend_none(c))-v6,
                 rule(c)-v7], Proof),
  list_to_assoc([a-true, b-true], Affected),
  prover:prune_proof(Proof, Affected, bwu_force_flush([]), RProof),
  assoc_to_keys(RProof, Keys).

test(prune_triggers_drops_affected_keys_and_dependents, [true(Pairs == [b-[c]])]) :-
  list_to_assoc([a-[b, c], b-[a, c]], Triggers),
  list_to_assoc([a-true], Affected),
  prover:prune_triggers(Triggers, Affected, RTriggers),
  assoc_to_list(RTriggers, Pairs).

test(restart_seed_matches_pending_provider_actions,
     [setup(( retractall(cache:ordered_entry(fakerepo, _, _, _, _)),
              assertz(cache:ordered_entry(fakerepo, 'cat/pkg-1.0', cat, pkg, v)),
              assertz(cache:ordered_entry(fakerepo, 'cat/other-1.0', cat, other, v)) )),
      cleanup(retractall(cache:ordered_entry(fakerepo, _, _, _, _)))]) :-
  Info = bwu_force_flush([bwu_force(cat, pkg, [icu])]),
  heuristic:restart_seed(Info, fakerepo://'cat/pkg-1.0':install),
  heuristic:restart_seed(Info, fakerepo://'cat/pkg-1.0':run),
  \+ heuristic:restart_seed(Info, fakerepo://'cat/other-1.0':install),
  \+ heuristic:restart_seed(Info, grouped_dep(cat, pkg, []):install).

test(restart_obligation_head_maps_pdepend_keys) :-
  heuristic:restart_obligation_head(pdepend(fakerepo://'cat/pkg-1.0':install, bwu), Core1),
  Core1 == (fakerepo://'cat/pkg-1.0':install),
  heuristic:restart_obligation_head(pdepend_none(fakerepo://'cat/pkg-1.0':install), Core2),
  Core2 == (fakerepo://'cat/pkg-1.0':install).

test(strip_ctx_strips_action_attached_context) :-
  Lit = fakerepo://'cat/pkg-1.0':install?{[build_with_use:use_state([],[])]},
  heuristic:strip_ctx(Lit, Core),
  Core == (fakerepo://'cat/pkg-1.0':install),
  heuristic:obligation_candidate(Lit).

test(query_keyword_candidate_excludes_binpkg_repo,
     [setup(( retractall(cache:ordered_entry(binpkg, _, _, _, _)),
              retractall(cache:ordered_entry(portage, 'cat/pkg-1.0', _, _, _)),
              retractall(cache:entry_metadata(binpkg, _, _, _)),
              retractall(cache:entry_metadata(portage, 'cat/pkg-1.0', _, _)),
              assertz(cache:ordered_entry(binpkg, 'cat/pkg-1.0-1', cat, pkg, v)),
              assertz(cache:ordered_entry(portage, 'cat/pkg-1.0', cat, pkg, v)),
              assertz(cache:entry_metadata(binpkg, 'cat/pkg-1.0-1', keywords, amd64)),
              assertz(cache:entry_metadata(portage, 'cat/pkg-1.0', keywords, amd64)) )),
      cleanup(( retractall(cache:ordered_entry(binpkg, _, _, _, _)),
                retractall(cache:ordered_entry(portage, 'cat/pkg-1.0', _, _, _)),
                retractall(cache:entry_metadata(binpkg, _, _, _)),
                retractall(cache:entry_metadata(portage, 'cat/pkg-1.0', _, _)) ))]) :-
  \+ acceptance:query_keyword_candidate(install, cat, pkg, amd64, [], binpkg://_),
  acceptance:query_keyword_candidate(install, cat, pkg, amd64, [], portage://'cat/pkg-1.0').

test(restart_drop_constraint_scopes_use_slot_selected,
     [setup(( retractall(cache:ordered_entry(fakerepo, _, _, _, _)),
              assertz(cache:ordered_entry(fakerepo, 'cat/pkg-1.0', cat, pkg, v)) )),
      cleanup(retractall(cache:ordered_entry(fakerepo, _, _, _, _)))]) :-
  list_to_assoc([(fakerepo://'cat/pkg-1.0':install)-true], Affected),
  heuristic:restart_constraint_scope(bwu_force_flush([]), Affected, Scope),
  heuristic:restart_drop_constraint(Scope, use(fakerepo://'cat/pkg-1.0')),
  heuristic:restart_drop_constraint(Scope, slot(cat, pkg, '0')),
  heuristic:restart_drop_constraint(Scope, selected_cn(cat, pkg)),
  \+ heuristic:restart_drop_constraint(Scope, use(fakerepo://'cat/other-1.0')),
  \+ heuristic:restart_drop_constraint(Scope, slot(cat, other, '0')),
  \+ heuristic:restart_drop_constraint(Scope, cn_domain(cat, pkg, '0')),
  \+ heuristic:restart_drop_constraint(Scope, blocked_cn(cat, pkg)).

test(partial_restart_state_prunes_provider_and_dependents,
     [setup(( retractall(cache:ordered_entry(fakerepo, _, _, _, _)),
              assertz(cache:ordered_entry(fakerepo, 'cat/pkg-1.0', cat, pkg, v)),
              assertz(cache:ordered_entry(fakerepo, 'cat/consumer-1.0', cat, consumer, v)),
              assertz(cache:ordered_entry(fakerepo, 'cat/bystander-1.0', cat, bystander, v)) )),
      cleanup(retractall(cache:ordered_entry(fakerepo, _, _, _, _)))]) :-
  Provider  = (fakerepo://'cat/pkg-1.0':install),
  Consumer  = (fakerepo://'cat/consumer-1.0':run),
  Bystander = (fakerepo://'cat/bystander-1.0':run),
  list_to_assoc([Provider-[], Consumer-[], Bystander-[]], Model),
  list_to_assoc([rule(Provider)-(dep(0, [])?[]),
                 rule(Consumer)-(dep(1, [Provider])?[]),
                 rule(Bystander)-(dep(0, [])?[])], Proof),
  list_to_assoc([Provider-[Consumer]], Triggers),
  list_to_assoc([use(fakerepo://'cat/pkg-1.0')-u1,
                 use(fakerepo://'cat/bystander-1.0')-u2,
                 selected_cn(cat, pkg)-s1,
                 cn_domain(cat, pkg, '0')-d1], Cons),
  Info = bwu_force_flush([bwu_force(cat, pkg, [icu])]),
  prover:partial_restart_state(Info, Proof, Model, Cons, Triggers,
                               RProof, RModel, RCons, RTrig),
  assoc_to_keys(RModel, [Bystander]),
  assoc_to_keys(RProof, [rule(Bystander)]),
  assoc_to_keys(RTrig, []),
  assoc_to_keys(RCons, [use(fakerepo://'cat/bystander-1.0'), cn_domain(cat, pkg, '0')]).

test(begin_pass_clears_per_pass_memos_for_both_kinds,
     [cleanup(use:clear_bwu_cross_dep_memos)]) :-
  forall(member(Kind, [fresh, resume]),
         ( use:clear_bwu_cross_dep_memos,
           assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
           use:record_bwu_force_pending('dev-qt', qtbase, [icu]),
           heuristic:begin_pass(Kind),
           \+ memo:candidate_bwu_(_, _, _),
           \+ memo:bwu_force_pending_(_, _, _)
         )).

test(mark_resume_pass_consumed_by_next_begin_pass) :-
  prover:mark_resume_pass,
  nb_current(prover_resume_pass, true),
  prover:begin_pass,
  nb_current(prover_resume_pass, false).

test(restart_prior_proven_witnesses_cycle_free_pruned_literals,
     [cleanup(nb_delete(prover_restart_prior_proven))]) :-
  % clean: plain key, no assumed marker -> witnessed
  % broken: plain key AND assumed key (cycle-break) -> not witnessed
  % gone:  affected but not in the model -> not witnessed
  list_to_assoc([clean-ctx1, broken-ctx2, assumed(broken)-ctx3], Model),
  list_to_assoc([clean-true, broken-true, gone-true], Affected),
  prover:restart_note_prior_proven(Model, Affected),
  prover:restart_prior_proven(clean),
  \+ prover:restart_prior_proven(broken),
  \+ prover:restart_prior_proven(gone),
  \+ prover:restart_prior_proven(unrelated).

test(begin_pass_fresh_drops_prior_proven_witness) :-
  list_to_assoc([lit-ctx], Model),
  list_to_assoc([lit-true], Affected),
  prover:restart_note_prior_proven(Model, Affected),
  prover:restart_prior_proven(lit),
  % resume pass keeps the witness set
  prover:mark_resume_pass,
  prover:begin_pass,
  prover:restart_prior_proven(lit),
  % fresh pass drops it
  prover:begin_pass,
  \+ prover:restart_prior_proven(lit).

:- end_tests(partial_restart).


:- begin_tests(dep_model_cache).

% Hazard-encoded cache key for model(dependency) queries (see the design
% comment at the top of Source/Knowledge/query.pl).

test(choice_cn_extraction_only_inside_choice_groups) :-
  T1 = any_of_group([package_dependency(install,no,'dev-lang','python',none,version_none,[],[]),
                     all_of_group([package_dependency(install,no,'dev-lang','pypy',none,version_none,[],[])])]),
  findall(CN, query:dep_model_choice_cn(T1, CN), CNs1),
  msort(CNs1, ['dev-lang'-'pypy', 'dev-lang'-'python']),
  % package deps NOT under a choice group are not choice CNs
  T2 = all_of_group([package_dependency(install,no,'sys-libs','zlib',none,version_none,[],[])]),
  findall(CN2, query:dep_model_choice_cn(T2, CN2), []),
  % choice group nested under a conditional is still found
  T3 = use_conditional_group(positive, foo, r://e,
         [exactly_one_of_group([package_dependency(install,no,'app-misc','a',none,version_none,[],[])])]),
  findall(CN3, query:dep_model_choice_cn(T3, CN3), ['app-misc'-'a']).

test(choice_sig_reflects_snapshot_presence,
     [setup(( empty_assoc(Empty), nb_setval(memo_selected_cn_snap, Empty),
              retractall(memo:dep_model_choice_cns_(_, _, _)),
              assertz(memo:dep_model_choice_cns_(testrepo, 'x/y-1', ['dev-lang'-'python'])) )),
      cleanup(retractall(memo:dep_model_choice_cns_(_, _, _)))]) :-
  query:dep_model_choice_sig(testrepo, 'x/y-1', [0]),
  cnselect:record_selected_cn_snapshot('dev-lang', 'python', [selected(portage,'dev-lang/python-3.13',run,v,'3.13')]),
  query:dep_model_choice_sig(testrepo, 'x/y-1', [1]).

test(choice_sig_zero_without_choice_groups,
     [setup(( retractall(memo:dep_model_choice_cns_(_, _, _)),
              assertz(memo:dep_model_choice_cns_(testrepo, 'x/z-1', [])) )),
      cleanup(retractall(memo:dep_model_choice_cns_(_, _, _)))]) :-
  query:dep_model_choice_sig(testrepo, 'x/z-1', 0).

test(assuming_bits_reflect_prover_scopes) :-
  query:dep_model_assuming_bits(bits(0, 0, 0, 0)),
  prover:assuming(conflicts, query:dep_model_assuming_bits(bits(0, 0, 1, 0))),
  prover:assuming(keyword_acceptance,
    prover:assuming(blockers, query:dep_model_assuming_bits(bits(1, 0, 0, 1)))),
  query:dep_model_assuming_bits(bits(0, 0, 0, 0)).

test(key_none_for_nonground_context) :-
  query:dep_model_key(testrepo, 'x/y-1', [build_with_use:use_state([_Var], [])], none).

test(key_none_while_variant_active,
     [setup(assertz(variant:branch_prefer(package_dependency(install,no,'a','b',none,version_none,[],[])))),
      cleanup(retractall(variant:branch_prefer(_)))]) :-
  query:dep_model_key(testrepo, 'x/y-1', [build_with_use:use_state([], [])], none).

test(key_encodes_context_bits_and_sig,
     [setup(( empty_assoc(Empty), nb_setval(memo_selected_cn_snap, Empty),
              retractall(memo:dep_model_choice_cns_(_, _, _)),
              assertz(memo:dep_model_choice_cns_(testrepo, 'x/y-1', ['dev-lang'-'python'])) )),
      cleanup(retractall(memo:dep_model_choice_cns_(_, _, _)))]) :-
  Ctx = [build_with_use:use_state([icu], [])],
  query:dep_model_key(testrepo, 'x/y-1', Ctx, key(Ctx, bits(0,0,0,0), [0])),
  prover:assuming(unmask,
    query:dep_model_key(testrepo, 'x/y-1', Ctx, key(Ctx, bits(0,1,0,0), [0]))).

:- end_tests(dep_model_cache).


:- begin_tests(equality_use_pin_propagation).

test(equal_provider_enabled_enables_self, [true(Mode == enable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  once(ranking:equality_pin_from_usedep('dev-qt', qtbase, use(equal(icu), negative), icu, Mode)).

test(equal_provider_disabled_disables_self, [true(Mode == disable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([], [icu]))),
  once(ranking:equality_pin_from_usedep('dev-qt', qtbase, use(equal(icu), positive), icu, Mode)).

test(inverse_provider_enabled_disables_self, [true(Mode == disable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  once(ranking:equality_pin_from_usedep('dev-qt', qtbase, use(inverse(icu), negative), icu, Mode)).

test(inverse_provider_disabled_enables_self, [true(Mode == enable)]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([], [icu]))),
  once(ranking:equality_pin_from_usedep('dev-qt', qtbase, use(inverse(icu), positive), icu, Mode)).

test(unpinned_provider_yields_no_pin, [fail]) :-
  use:clear_bwu_cross_dep_memos,
  ranking:equality_pin_from_usedep('dev-qt', qtbase, use(equal(icu), negative), icu, _Mode).

test(term_walk_collects_top_level, [true(Pairs == [icu-enable])]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  Term = package_dependency(install, no, 'dev-qt', qtbase, tilde, version_none, [],
                            [use(equal(icu), negative), use(enable(network), positive)]),
  findall(F-M, ranking:equality_pin_from_term(Term, F, M), Pairs).

test(term_walk_descends_all_of_group, [true(Pairs == [icu-enable])]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  Term = all_of_group([package_dependency(install, no, 'dev-qt', qtbase, tilde, version_none, [],
                                          [use(equal(icu), negative)])]),
  findall(F-M, ranking:equality_pin_from_term(Term, F, M), Pairs).

test(conditional_group_not_descended, [true(Pairs == [])]) :-
  use:clear_bwu_cross_dep_memos,
  assertz(memo:candidate_bwu_('dev-qt', qtbase, use_state([icu], []))),
  Term = use_conditional_group(positive, someflag, none,
           [package_dependency(install, no, 'dev-qt', qtbase, tilde, version_none, [],
                               [use(equal(icu), negative)])]),
  findall(F-M, ranking:equality_pin_from_term(Term, F, M), Pairs).

test(pin_conflict_detected) :-
  ranking:pin_flags_conflict([icu], [icu]).

test(pin_no_conflict, [fail]) :-
  ranking:pin_flags_conflict([icu], [foo]).

test(seed_conditional_minus_use_recurses, [fail]) :-
  ranking:seed_use_conditional_inactive(positive, minus(foo), some://entry).

test(seed_conditional_non_entry_recurses, [fail]) :-
  ranking:seed_use_conditional_inactive(positive, foo, not_an_entry).

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


% =============================================================================
%  USE_EXPAND target ranking is family-agnostic
% =============================================================================
%
% The any_of/exactly-one choice ranking prefers the newest USE_EXPAND target
% when the profile has not forced one. This used to be hardcoded for the
% llvm_slot and lua5 families only (ecosystem-specific literals in the domain
% rules). It is now generic over every eapi:use_expand/1 family: the rank is
% derived from the trailing version digits of the flag.

:- begin_tests(use_expand_target_rank).

test(llvm_slot_numeric, [true(R == 20)]) :-
  ranking:use_rank('llvm_slot_20', R).

test(llvm_slot_newer_ranks_higher) :-
  ranking:use_rank('llvm_slot_20', R20),
  ranking:use_rank('llvm_slot_19', R19),
  R20 > R19.

test(python_single_target_newer_ranks_higher) :-
  ranking:use_rank('python_single_target_python3_13', R13),
  ranking:use_rank('python_single_target_python3_12', R12),
  R13 > R12.

test(lua5_newer_ranks_higher) :-
  ranking:use_rank('lua_single_target_lua5-4', R4),
  ranking:use_rank('lua_single_target_lua5-3', R3),
  R4 > R3.

test(lua_non_numeric_is_zero, [true(R == 0)]) :-
  ranking:use_rank('lua_single_target_luajit', R).

test(non_use_expand_is_zero, [true(R == 0)]) :-
  ranking:use_rank(some_random_flag, R).

test(digit_groups_multi, [true(G == [3,13])]) :-
  atom_codes('python3_13', Cs),
  ranking:digit_groups(Cs, G).

test(digit_groups_none, [true(G == [])]) :-
  atom_codes(luajit, Cs),
  ranking:digit_groups(Cs, G).

:- end_tests(use_expand_target_rank).


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

test(wildcard_atom, [true(Ver == version([0],'',4,0,[],0,'1.0.*'))]) :-
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

% Find an installed package with at least one IUSE flag the VDB build
% does NOT have enabled, so we can construct a real BWU mismatch. We
% prefer net-firewall/iptables (has nftables IUSE) and fall back to any
% installed entry that satisfies the predicate. Defined at file level so
% both the rules_install_run_bwu_rebuild and update_use_change_resolve
% units below can share it (PLUnit units inherit from this module).
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


:- begin_tests(rules_install_run_bwu_rebuild).

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
    prover:rule_parts(RLib, HLib, _, _),
    prover:canon_literal(HLib, CHLib, _),
    term_to_atom(CHLib, ALib), sub_atom(ALib, _, _, _, libnftnl), !,
  nth1(WIp, Plan, WaveIp),
    member(RIp, WaveIp),
    prover:rule_parts(RIp, HIp, _, _),
    prover:canon_literal(HIp, CHIp, _),
    term_to_atom(CHIp, AIp), sub_atom(AIp, _, _, _, 'iptables'),
    sub_atom(AIp, _, _, _, ':update'), !,
  WLib < WIp.

:- end_tests(rules_install_run_bwu_rebuild).


% =============================================================================
%  suggestion(use_change) rebuild for already-installed packages (portage-ng#85)
% =============================================================================

% Regression test for the asymmetry that left an already-installed package
% short-circuited to []/reinstall when a self USE flip arrived as a
% suggestion(use_change) with an EMPTY build_with_use term (e.g. a
% REQUIRED_USE pick, or a flip propagated via the per-(C,N) memo). The
% install/run short-circuit consulted installed_entry_satisfies_build_with_use/2
% only, which reported the installed -USE build as satisfactory and never
% reached candidate:update_requires_use_rebuild/2. Fix: consult
% use:installed_entry_satisfies_plan_use/3, which also honours self
% suggestion(use_change) flips, so a transactional :update is emitted and the
% deps gated by the flipped flag enter the plan.

:- begin_tests(rules_install_run_use_change_rebuild).

% installed_entry_satisfies_plan_use/3 must report NOT satisfied when a
% self suggestion(use_change) enables a flag the installed build lacks
% (and the flag is in the entry's IUSE).
test(plan_use_unsatisfied_on_suggestion_flip,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, Flag),
  Ctx = [suggestion(use_change, portage://Ebuild, [use_change(Flag, enable)])],
  \+ use:installed_entry_satisfies_plan_use(pkg://Ebuild, portage://Ebuild, Ctx).

% rule(:install?{Ctx with suggestion(use_change) flip}) on an installed entry
% must NOT short-circuit to []. It must emit a :update?{[...,replaces,...]}
% literal so the dep walker runs under the flipped USE.
test(install_rule_emits_update_on_use_change,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, Flag),
  Ctx = [suggestion(use_change, portage://Ebuild, [use_change(Flag, enable)])],
  rules:rule(portage://Ebuild:install?{Ctx}, Conds),
  Conds = [portage://Ebuild:update?{UpdCtx}],
  memberchk(replaces(pkg://Ebuild), UpdCtx),
  memberchk(rebuild_reason(build_with_use), UpdCtx).

% rule(:run?{Ctx with suggestion(use_change) flip}) on an installed entry must
% emit the same :update literal (instead of degrading to :reinstall with an
% empty body).
test(run_rule_emits_update_on_use_change,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, Flag),
  Ctx = [suggestion(use_change, portage://Ebuild, [use_change(Flag, enable)])],
  rules:rule(portage://Ebuild:run?{Ctx}, Conds),
  Conds = [portage://Ebuild:update?{UpdCtx}],
  memberchk(replaces(pkg://Ebuild), UpdCtx),
  memberchk(rebuild_reason(build_with_use), UpdCtx).

% A suggestion(use_change) for a flag NOT in the entry's IUSE cannot change
% the build, so it must preserve the short-circuit (no spurious rebuild).
test(install_rule_absent_flag_keeps_short_circuit,
     [condition(test_setup_pick(_, _))]) :-
  test_setup_pick(pkg://Ebuild, _),
  Ctx = [suggestion(use_change, portage://Ebuild,
                    [use_change('portage_ng_nonexistent_flag', enable)])],
  rules:rule(portage://Ebuild:install?{Ctx}, Conds),
  Conds == [].

:- end_tests(rules_install_run_use_change_rebuild).


% =============================================================================
%  Issue #9: same-version :update must not no-op on USE change
% =============================================================================

:- begin_tests(update_use_change_resolve).

test_setup_same_version_installed(portage://RepoE, pkg://PkgE, Flag) :-
  test_setup_pick(pkg://PkgE, Flag),
  query:search([category(C),name(N),version(V)], pkg://PkgE),
  query:search([category(C),name(N),version(V)], portage://RepoE).

test(update_resolve_not_empty_on_use_change,
     [condition(test_setup_same_version_installed(_, _, _)), nondet]) :-
  test_setup_same_version_installed(portage://RepoE, _PkgE, Flag),
  Changes = [use_change(Flag, enable)],
  Ctx = [suggestion(use_change, portage://RepoE, Changes)],
  candidate:resolve(portage://RepoE:update?{Ctx}, Conds),
  Conds \== [].

:- end_tests(update_use_change_resolve).


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
% emitted assumption carries the assumption_reason tag so the printer can
% classify it downstream (phantom_grouped_dep_assumption/3). The scheduler
% no longer filters aliasing on these tags: its assumed-dep alias is
% existence-gated on a concrete planned action, which handles phantoms
% naturally while preserving ordering edges to planned providers
% (portage-ng#95).
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

% Visibility-hidden dep concretization (portage-ng#14): reason-to-flags
% mapping only covers reasons that relaxing visibility can fix.
test(hidden_reason_flags_keyword_filtered) :-
  candidate:hidden_reason_flags(keyword_filtered, [keyword_acceptance]).

test(hidden_reason_flags_masked) :-
  candidate:hidden_reason_flags(masked, [unmask]).

test(hidden_reason_flags_rejects_other_reasons, [fail]) :-
  member(R, [unsatisfied_constraints, missing, version_conflict,
             slot_unsatisfied, installed_required]),
  candidate:hidden_reason_flags(R, _).

% record_visibility_override extracts the selected candidate from the
% assembled conditions and asserts a memo:visibility_override_/2 fact.
test(record_visibility_override_asserts_selected,
     [cleanup(retractall(memo:visibility_override_(_, _)))]) :-
  Conditions = [constraint(selected_cn('acct-group', buildbot):{ordset([selected(qtest, 'acct-group/buildbot-0-r3', run, v, [])])})],
  candidate:record_visibility_override('acct-group', buildbot, Conditions),
  memo:visibility_override_(qtest, 'acct-group/buildbot-0-r3').

test(record_visibility_override_noop_without_selection,
     [cleanup(retractall(memo:visibility_override_(_, _)))]) :-
  candidate:record_visibility_override('acct-group', buildbot, []),
  \+ memo:visibility_override_(_, _).

:- end_tests(phantom_grouped_dep_assumption).


% =============================================================================
%  Scheduler: install configure closure (portage-ng#21)
% =============================================================================

:- begin_tests(scheduler_install_configure_deps).

% KB-independent: `build_pkg_head_map/2` needs `cache:ordered_entry/5`, which
% CI lacks. Exercise the repair graph + wave assignment with synthetic heads
% and a hand-built PkgHeadMap (grouped RDEPEND aliasing via run_phase-C-N).

% Repair the wave map for a synthetic rule set: builds the effective repair
% graph, condenses it (Kosaraju) and assigns longest-path waves.
repair_waves(AllRules, Map0, PkgHeadMap, Pd, CfgMap, Map1) :-
  scheduler:build_repair_graph(AllRules, Map0, PkgHeadMap, Pd, CfgMap,
                               Heads, Forward, Reverse),
  scheduler:kosaraju_scc(Heads, Forward, Reverse, SCCs),
  scheduler:repair_comp_map(SCCs, CompMap, CompIds, MembersMap),
  scheduler:comp_edges(Forward, CompMap, CompEdges),
  scheduler:assign_repair_waves(CompIds, CompEdges, MembersMap, Map0, Map1).

test(install_promoted_past_run_rdepend) :-
  BifRun = grouped_package_dependency(no, 'dev-haskell', bifunctors, []):run,
  SgRun = rule(portage://'fake/sg-1':run, [BifRun]),
  SgInstall = rule(portage://'fake/sg-1':install, []),
  BifRunRule = rule(portage://'fake/bif-1':run, []),
  AllRules = [SgInstall, SgRun, BifRunRule],
  list_to_assoc([ (portage://'fake/sg-1':install)-1,
                  (portage://'fake/sg-1':run)-2,
                  (portage://'fake/bif-1':run)-2 ], Map0),
  list_to_assoc([ ('run_phase'-'dev-haskell'-bifunctors)-(portage://'fake/bif-1':run) ],
                 PkgHeadMap),
  scheduler:build_install_configure_dep_map([SgRun], CfgMap),
  repair_waves(AllRules, Map0, PkgHeadMap, pd(t, t), CfgMap, Map1),
  get_assoc(portage://'fake/sg-1':install, Map1, WInstall),
  WInstall >= 3.

test(configure_deps_alias_edge_from_run_body) :-
  BifRun = grouped_package_dependency(no, 'dev-haskell', bifunctors, []):run,
  RunRule = rule(portage://'fake/sg-1':run, [BifRun]),
  list_to_assoc([ ('run_phase'-'dev-haskell'-bifunctors)-(portage://'fake/bif-1':run) ],
                 PkgHeadMap),
  list_to_assoc([ (portage://'fake/sg-1':install)-1,
                  (portage://'fake/bif-1':run)-1 ], Map),
  scheduler:build_install_configure_dep_map([RunRule], CfgMap),
  empty_assoc(F0),
  scheduler:add_repair_edges(Map, PkgHeadMap, pd(t,t), CfgMap,
                             rule(portage://'fake/sg-1':install, []), F0, F1),
  get_assoc(portage://'fake/sg-1':install, F1, Deps),
  memberchk(portage://'fake/bif-1':run, Deps).

% Regression for portage-ng#26: a dependency cycle elsewhere in the plan
% must not collapse an acyclic downstream chain into a single wave. The
% earlier fixpoint-sweep repair diverged on the a<->b cycle, hit its
% iteration cap, and then merged c (BDEPEND consumer) into the same wave
% as its dependency.
test(cycle_does_not_collapse_downstream_chain) :-
  ARun = portage://'fake/a-1':run,
  BRun = portage://'fake/b-1':run,
  CInstall = portage://'fake/c-1':install,
  DInstall = portage://'fake/d-1':install,
  AllRules = [ rule(ARun, [BRun]),
               rule(BRun, [ARun]),
               rule(CInstall, [BRun]),
               rule(DInstall, [CInstall]) ],
  list_to_assoc([ARun-1, BRun-1, CInstall-1, DInstall-1], Map0),
  empty_assoc(PkgHeadMap),
  empty_assoc(CfgMap),
  repair_waves(AllRules, Map0, PkgHeadMap, pd(t, t), CfgMap, Map1),
  get_assoc(ARun, Map1, WA),
  get_assoc(BRun, Map1, WB),
  get_assoc(CInstall, Map1, WC),
  get_assoc(DInstall, Map1, WD),
  WA =:= WB,
  WC > WB,
  WD > WC.

% Regression for portage-ng#83: a provider's soft/hard blocker against the
% very consumers that depend on it must NOT seed a "schedule-after" edge.
% qtbase carries `!<dev-qt/qt*-<ver>` soft blockers against the modules
% that RDEPEND it; treating those as ordering edges closed a cycle with the
% configure-closure edge and co-waved qtbase:run with the modules' :install.
test(soft_blocker_recognized) :-
  scheduler:dep_is_blocker(grouped_package_dependency(weak, 'dev-qt', qtsvg,
      [package_dependency(run, weak, 'dev-qt', qtsvg, smaller,
          version([6,11,1], '', 4, 0, [], 0, '6.11.1'), [slot('6')], [])]):run).

test(hard_blocker_recognized) :-
  scheduler:dep_is_blocker(grouped_package_dependency(strong, 'dev-qt', qtsvg, []):run?{[]}).

test(normal_dep_not_blocker, [fail]) :-
  scheduler:dep_is_blocker(grouped_package_dependency(no, 'dev-qt', qtbase, []):run).

% The blocker on a provider's :run body must not appear as a forward repair
% edge to the consumer it blocks (which would form the spurious SCC cycle).
test(soft_blocker_creates_no_repair_edge) :-
  Blocker = grouped_package_dependency(weak, 'dev-qt', qtsvg, []):run,
  PRun = rule(portage://'fake/qtbase-1':run, [Blocker]),
  list_to_assoc([ (portage://'fake/qtbase-1':run)-1,
                  (portage://'fake/qtsvg-1':run)-1 ], Map),
  list_to_assoc([ ('run_phase'-'dev-qt'-qtsvg)-(portage://'fake/qtsvg-1':run) ],
                 PkgHeadMap),
  empty_assoc(CfgMap),
  empty_assoc(F0),
  scheduler:add_repair_edges(Map, PkgHeadMap, pd(t,t), CfgMap, PRun, F0, F1),
  get_assoc(portage://'fake/qtbase-1':run, F1, Deps),
  \+ memberchk(portage://'fake/qtsvg-1':run, Deps).

% End-to-end wave check: consumer RDEPENDs provider (configure closure) while
% provider soft-blocks consumer. The consumer's :install must land strictly
% after the provider's :run merge, never co-waved with it.
test(blocker_provider_consumer_not_cowaved) :-
  Rdep    = grouped_package_dependency(no, 'dev-qt', qtbase, []):run,
  Blocker = grouped_package_dependency(weak, 'dev-qt', qtsvg, []):run,
  CInstall = rule(portage://'fake/qtsvg-1':install, []),
  CRun     = rule(portage://'fake/qtsvg-1':run, [Rdep]),
  PInstall = rule(portage://'fake/qtbase-1':install, []),
  PRun     = rule(portage://'fake/qtbase-1':run, [Blocker]),
  AllRules = [CInstall, CRun, PInstall, PRun],
  list_to_assoc([ (portage://'fake/qtsvg-1':install)-1,
                  (portage://'fake/qtsvg-1':run)-1,
                  (portage://'fake/qtbase-1':install)-1,
                  (portage://'fake/qtbase-1':run)-1 ], Map0),
  list_to_assoc([ ('run_phase'-'dev-qt'-qtbase)-(portage://'fake/qtbase-1':run),
                  ('run_phase'-'dev-qt'-qtsvg)-(portage://'fake/qtsvg-1':run) ],
                 PkgHeadMap),
  scheduler:build_install_configure_dep_map(AllRules, CfgMap),
  repair_waves(AllRules, Map0, PkgHeadMap, pd(t, t), CfgMap, Map1),
  get_assoc(portage://'fake/qtbase-1':run, Map1, WPRun),
  get_assoc(portage://'fake/qtsvg-1':install, Map1, WCInstall),
  WCInstall > WPRun.

% portage-ng#83 (second build system, meson/pkg-config): media-libs/libglvnd
% carries a *versionless* weak blocker `!media-libs/mesa` in both DEPEND and
% RDEPEND, while mesa DEPEND/RDEPENDs libglvnd. The RDEPEND blocker lands on
% libglvnd:run as a grouped `:run` literal (matched by grouped_run_dep_pkg_key)
% and previously closed the same SCC cycle, co-waving libglvnd's merge with
% mesa's configure. The blocker is strength-based, so the versionless
% `none/version_none` operator must not matter.
test(versionless_run_blocker_recognized) :-
  scheduler:dep_is_blocker(grouped_package_dependency(weak, 'media-libs', mesa,
      [package_dependency(run, weak, 'media-libs', mesa, none, version_none, [],
          [use(disable(libglvnd), positive)])]):run).

test(libglvnd_mesa_provider_consumer_not_cowaved) :-
  Rdep    = grouped_package_dependency(no, 'media-libs', libglvnd, []):run,
  Blocker = grouped_package_dependency(weak, 'media-libs', mesa,
      [package_dependency(run, weak, 'media-libs', mesa, none, version_none, [],
          [use(disable(libglvnd), positive)])]):run,
  CInstall = rule(portage://'fake/mesa-1':install, []),
  CRun     = rule(portage://'fake/mesa-1':run, [Rdep]),
  PInstall = rule(portage://'fake/libglvnd-1':install, []),
  PRun     = rule(portage://'fake/libglvnd-1':run, [Blocker]),
  AllRules = [CInstall, CRun, PInstall, PRun],
  list_to_assoc([ (portage://'fake/mesa-1':install)-1,
                  (portage://'fake/mesa-1':run)-1,
                  (portage://'fake/libglvnd-1':install)-1,
                  (portage://'fake/libglvnd-1':run)-1 ], Map0),
  list_to_assoc([ ('run_phase'-'media-libs'-libglvnd)-(portage://'fake/libglvnd-1':run),
                  ('run_phase'-'media-libs'-mesa)-(portage://'fake/mesa-1':run) ],
                 PkgHeadMap),
  scheduler:build_install_configure_dep_map(AllRules, CfgMap),
  repair_waves(AllRules, Map0, PkgHeadMap, pd(t, t), CfgMap, Map1),
  get_assoc(portage://'fake/libglvnd-1':run, Map1, WPRun),
  get_assoc(portage://'fake/mesa-1':install, Map1, WCInstall),
  WCInstall > WPRun.

% Regression for portage-ng#95: a dep that degraded to a domain assumption
% tagged `required_use_violation` (conflicting REQUIRED_USE on the provider)
% must STILL alias to the concrete planned install of that provider. The
% earlier `assumed_inner_phantom` guard skipped the alias unconditionally,
% severing the qtbase BDEPEND ordering edge for KDE consumers (breeze-icons
% and plasma-wayland-protocols configured before qtbase merged: "No Qt6
% qtpaths executable found").
test(requse_violation_assumed_dep_still_aliases) :-
  Dep = assumed(grouped_package_dependency('dev-qt', qtbase, []):install
                ?{[required_use_violation(dummy)]}),
  scheduler:assumed_dep_alias_key(Dep, Key),
  Key == 'install_phase'-'dev-qt'-qtbase.

test(requse_violation_consumer_promoted_after_planned_provider) :-
  AssumedDep = assumed(grouped_package_dependency('dev-qt', qtbase, []):install
                       ?{[required_use_violation(dummy)]}),
  Consumer = rule(portage://'fake/breeze-icons-1':install, [AssumedDep]),
  Provider = rule(portage://'fake/qtbase-1':install, []),
  AllRules = [Consumer, Provider],
  % Violation: consumer initially waved BEFORE its (assumed) provider.
  list_to_assoc([ (portage://'fake/breeze-icons-1':install)-1,
                  (portage://'fake/qtbase-1':install)-2 ], Map0),
  list_to_assoc([ ('install_phase'-'dev-qt'-qtbase)-(portage://'fake/qtbase-1':install) ],
                 PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map0, PkgHeadMap),
  empty_assoc(CfgMap),
  repair_waves(AllRules, Map0, PkgHeadMap, pd(t, t), CfgMap, Map1),
  get_assoc(portage://'fake/qtbase-1':install, Map1, WProvider),
  get_assoc(portage://'fake/breeze-icons-1':install, Map1, WConsumer),
  WConsumer > WProvider.

:- end_tests(scheduler_install_configure_deps).


% =============================================================================
%  Scheduler: ordering-violation pre-check (portage-ng#54)
% =============================================================================
%
% `repair_ordering_violations/3` only runs the full SCC-condensation repair
% when the cheap single-pass scan (`plan_has_ordering_violation/3`) finds a
% wave-ordering violation. These tests exercise the scan against the same
% synthetic rule sets used by the repair tests above (KB-independent,
% hand-built wave map + PkgHeadMap).

:- begin_tests(scheduler_ordering_precheck).

% Every body dep sits in a strictly earlier wave: no violation, the repair
% fast path returns the plan unchanged.
test(violation_free_plan_passes_scan, [fail]) :-
  AInstall = portage://'fake/a-1':install,
  ARun = portage://'fake/a-1':run,
  BInstall = portage://'fake/b-1':install,
  AllRules = [ rule(AInstall, []),
               rule(ARun, [AInstall]),
               rule(BInstall, [ARun]) ],
  list_to_assoc([AInstall-1, ARun-2, BInstall-3], Map),
  empty_assoc(PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map, PkgHeadMap).

% A rule planned before its direct body dep is a violation.
test(direct_dep_violation_detected) :-
  AInstall = portage://'fake/a-1':install,
  BInstall = portage://'fake/b-1':install,
  AllRules = [ rule(AInstall, []),
               rule(BInstall, [AInstall]) ],
  list_to_assoc([AInstall-2, BInstall-1], Map),
  empty_assoc(PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map, PkgHeadMap).

% Sharing a wave with a dependency is a violation too (the longest-path
% repair places a rule STRICTLY after its cross-SCC dependencies).
test(same_wave_dep_violation_detected) :-
  AInstall = portage://'fake/a-1':install,
  BInstall = portage://'fake/b-1':install,
  AllRules = [ rule(AInstall, []),
               rule(BInstall, [AInstall]) ],
  list_to_assoc([AInstall-1, BInstall-1], Map),
  empty_assoc(PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map, PkgHeadMap).

% Grouped-RDEPEND alias: the grouped head is not a plan head, but the
% concrete provider (via PkgHeadMap) lands in a later wave.
test(grouped_rdepend_alias_violation_detected) :-
  BifRun = grouped_package_dependency(no, 'dev-haskell', bifunctors, []):run,
  AllRules = [ rule(portage://'fake/sg-1':run, [BifRun]),
               rule(portage://'fake/bif-1':run, []) ],
  list_to_assoc([ (portage://'fake/sg-1':run)-1,
                  (portage://'fake/bif-1':run)-2 ], Map),
  list_to_assoc([ ('run_phase'-'dev-haskell'-bifunctors)-(portage://'fake/bif-1':run) ],
                 PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map, PkgHeadMap).

% Assumed-dep alias (Qt6 cmake-find ordering bug): the assumed grouped dep
% aliases to a concrete planned install in a later wave.
test(assumed_dep_alias_violation_detected) :-
  Assumed = assumed(grouped_package_dependency('dev-qt', qtbase, []):install?{[]}),
  AllRules = [ rule(portage://'fake/consumer-1':install, [Assumed]),
               rule(portage://'fake/qtbase-1':install, []) ],
  list_to_assoc([ (portage://'fake/consumer-1':install)-1,
                  (portage://'fake/qtbase-1':install)-2 ], Map),
  list_to_assoc([ ('install_phase'-'dev-qt'-qtbase)-(portage://'fake/qtbase-1':install) ],
                 PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map, PkgHeadMap).

% Configure closure (portage-ng#21): the install sibling of a :run rule is
% planned before the run rule's RDEPEND provider. The run rule's own body
% edges are satisfied (provider wave 2 < run wave 3), so only the configure
% closure check catches this.
test(configure_closure_violation_detected) :-
  BifRun = grouped_package_dependency(no, 'dev-haskell', bifunctors, []):run,
  AllRules = [ rule(portage://'fake/sg-1':install, []),
               rule(portage://'fake/sg-1':run, [BifRun]),
               rule(portage://'fake/bif-1':run, []) ],
  list_to_assoc([ (portage://'fake/sg-1':install)-1,
                  (portage://'fake/sg-1':run)-3,
                  (portage://'fake/bif-1':run)-2 ], Map),
  list_to_assoc([ ('run_phase'-'dev-haskell'-bifunctors)-(portage://'fake/bif-1':run) ],
                 PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map, PkgHeadMap).

% Same shape but with the install sibling correctly placed after the
% provider: the scan stays quiet.
test(configure_closure_satisfied_passes_scan, [fail]) :-
  BifRun = grouped_package_dependency(no, 'dev-haskell', bifunctors, []):run,
  AllRules = [ rule(portage://'fake/bif-1':run, []),
               rule(portage://'fake/sg-1':install, []),
               rule(portage://'fake/sg-1':run, [BifRun]) ],
  list_to_assoc([ (portage://'fake/bif-1':run)-1,
                  (portage://'fake/sg-1':install)-2,
                  (portage://'fake/sg-1':run)-3 ], Map),
  list_to_assoc([ ('run_phase'-'dev-haskell'-bifunctors)-(portage://'fake/bif-1':run) ],
                 PkgHeadMap),
  scheduler:plan_has_ordering_violation(AllRules, Map, PkgHeadMap).

:- end_tests(scheduler_ordering_precheck).


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
% target's INSTALL head (wave 9), not its cyclic :run head (wave 10). This
% is the ruby-gem case (portage-ng#18).
test(consumer_completes_after_pdepend_install_head) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_completion_heads(grouped_package_dependency(no,'dev-lang',ruby,[]):install,
                                     grouped_package_dependency(no,'dev-ruby','mecab-ruby',[]):install,
                                     Map, Pd, Heads),
  Heads == [portage://'fake/rubygems-1':install].

% A consumer whose package lies in the target's closure must NOT be bumped
% (cycle safety, at (C,N) granularity): the target transitively depends on
% it. This is the LLVM clang/compiler-rt cycle (portage-ng#19).
test(cyclic_consumer_not_bumped, [fail]) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_completion_heads(grouped_package_dependency(no,'dev-lang',ruby,[]):install,
                                     grouped_package_dependency(no,'dev-ruby',rubygems,[]):install,
                                     Map, Pd, _).

% Per-target filtering: a provider with two PDEPEND targets, one acyclic
% (clang-toolchain-symlinks) and one cyclic w.r.t. the consumer
% (clang-runtime, RDEPENDs the consumer). The consumer (compiler-rt) is
% ordered after the acyclic target's install head and never after the
% cyclic one (portage-ng#19).
test(per_target_cycle_filter_uses_acyclic_targets) :-
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
  scheduler:pdepend_completion_heads(grouped_package_dependency(no,'llvm-core',clang,[]):install,
                                     grouped_package_dependency(no,'llvm-runtimes','compiler-rt',[]):install,
                                     Map, pd(AnchorMap, ClosureMap), Heads),
  Heads == [portage://'fake/symlinks-1':install].

% A consumer that is ITSELF one of the provider's PDEPEND targets is never
% ordered after the group (e.g. clang-toolchain-symlinks must not wait for
% its sibling clang-runtime; portage-ng#19).
test(pdepend_target_member_not_bumped, [fail]) :-
  list_to_assoc([ (portage://'fake/symlinks-1':install)-5 ], Map),
  GH = grouped_package_dependency(no,'llvm-core','clang-toolchain-symlinks',[]):run,
  list_to_assoc([ ('llvm-core'-clang)-[GH] ], AnchorMap),
  empty_assoc(EmptyCns),
  list_to_assoc([ GH-EmptyCns ], ClosureMap),
  scheduler:pdepend_completion_heads(grouped_package_dependency(no,'llvm-core',clang,[]):install,
                                     grouped_package_dependency(no,'llvm-core','clang-toolchain-symlinks',[]):install,
                                     Map, pd(AnchorMap, ClosureMap), _).

% No PDEPEND provider in plan (empty AnchorMap): fast no-op failure.
test(empty_anchor_map_is_noop, [fail]) :-
  pdepend_fixture(Map, _),
  scheduler:pdepend_completion_heads(grouped_package_dependency(no,'dev-lang',ruby,[]):install,
                                     grouped_package_dependency(no,'dev-ruby','mecab-ruby',[]):install,
                                     Map, pd(t,t), _).

% A non-grouped (concrete) dep literal never triggers completion: consumer
% edges are always grouped deps, and the concrete provider-install node is
% shared with the post-install group.
test(concrete_dep_does_not_complete, [fail]) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_completion_heads(portage://'fake/ruby-1':install,
                                     grouped_package_dependency(no,'dev-ruby','mecab-ruby',[]):install,
                                     Map, Pd, _).

% A dep on a provider without PDEPEND (absent from AnchorMap) fails.
test(provider_without_pdepend_fails, [fail]) :-
  pdepend_fixture(Map, Pd),
  scheduler:pdepend_completion_heads(grouped_package_dependency(no,'dev-libs',glib,[]):install,
                                     grouped_package_dependency(no,'dev-libs',consumer,[]):install,
                                     Map, Pd, _).

% pdepend_effective_head prefers the package's :install head over a :run head.
test(install_head_preferred_over_run) :-
  list_to_assoc([ (portage://'fake/x-1':install)-3,
                  (portage://'fake/x-1':run)-5 ], M),
  scheduler:pdepend_effective_head(M, portage://'fake/x-1':run, EH),
  EH == portage://'fake/x-1':install.

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
  % as done (resume:done/2) are checked; a succeeded merge with no VDB
  % row must be reported missing.
  retractall(config:build_live_phases(_)),
  assertz(config:build_live_phases([clean, setup, unpack, prepare, configure, compile, test, install, merge])),
  retractall(resume:done(_, _)),
  assertz(resume:done('no-such-cat/no-such-pkg-0.0', install)),
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
  retractall(resume:done(_, _)),
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
  retractall(resume:done(_, _)),
  assertz(resume:done('no-such-cat/no-such-pkg-0.0', install)),
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
%  Query macro layer (portage-ng#32)
% =============================================================================

% Regression tests for the compile_query_compound macro table: tilde targets,
% select(keyword/keywords) metadata key, maintainer clause arity, and slot
% filters on operator-less (none) targets. Each query form is exercised
% through both paths:
%   - expanded:  the query is a literal in the test source, so
%                user:goal_expansion/2 inlines the cache goals at compile time
%   - runtime:   the query is constructed at runtime (parsed or =..-built),
%                so query:search/2 compiles it via the same macro table at
%                call time
% Uses synthetic cache facts under a private 'qtest' repository; no KB needed.

query_macros_setup :-
  query_macros_cleanup,
  assertz(cache:ordered_entry(qtest, 'dev-test/foo-2.0', 'dev-test', foo,
                              version([2,0],'',4,0,[],0,'2.0'))),
  assertz(cache:ordered_entry(qtest, 'dev-test/foo-1.0-r1', 'dev-test', foo,
                              version([1,0],'',4,0,[],1,'1.0-r1'))),
  assertz(cache:ordered_entry(qtest, 'dev-test/foo-1.0', 'dev-test', foo,
                              version([1,0],'',4,0,[],0,'1.0'))),
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-1.0',    slot, slot('1'))),
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-1.0-r1', slot, slot('1'))),
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-2.0',    slot, slot('2'))),
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-1.0', keywords, stable(amd64))),
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-2.0', keywords, unstable(amd64))),
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-1.0', maintainer,
                               ['dev@example.org','other@example.org'])).

query_macros_cleanup :-
  retractall(cache:ordered_entry(qtest,_,_,_,_)),
  retractall(cache:entry_metadata(qtest,_,_,_)).

:- begin_tests(query_macros, [setup(query_macros_setup),
                              cleanup(query_macros_cleanup)]).

% The arity-typo class (body goals written as extra head arguments) must not
% silently define compile_query_compound/4 or /5; query.pl also fails loudly
% at load time via a directive when this happens.
test(no_wrong_arity_macro_clauses) :-
  \+ current_predicate(query:compile_query_compound/4),
  \+ current_predicate(query:compile_query_compound/5).

% Tilde targets must match any revision of the given version (runtime path,
% parsed exactly like a CLI target).
test(tilde_target_runtime, [true(Ids == ['dev-test/foo-1.0','dev-test/foo-1.0-r1']), nondet]) :-
  atom_codes('~dev-test/foo-1.0', Codes),
  phrase(eapi:qualified_target(Q), Codes),
  findall(I, query:search(Q, qtest://I), Ids0),
  msort(Ids0, Ids).

% Same query as a source literal (goal-expanded path).
test(tilde_target_expanded, [true(Ids == ['dev-test/foo-1.0','dev-test/foo-1.0-r1'])]) :-
  findall(I,
          query:search(qualified_target(tilde, qtest, 'dev-test', foo,
                                        version([1,0],'',4,0,[],0,'1.0'),
                                        [[],[]]),
                       qtest://I),
          Ids0),
  msort(Ids0, Ids).

% Slot restrictions on operator-less targets must filter candidates
% (previously dropped at query level).
test(cn_target_slot_filter_runtime, [true(Ids == ['dev-test/foo-2.0'])]) :-
  atom_codes('dev-test/foo:2', Codes),
  phrase(eapi:qualified_target(Q), Codes),
  findall(I, query:search(Q, qtest://I), Ids0),
  msort(Ids0, Ids).

% Operator-less target without restrictions still returns all versions
% (goal-expanded path, empty filters).
test(cn_target_expanded_all, [true(N == 3)]) :-
  findall(I,
          query:search(qualified_target(none, qtest, 'dev-test', foo,
                                        version_none, [[],[]]),
                       qtest://I),
          Ids),
  length(Ids, N).

% select(keyword/keywords) must query the 'keywords' metadata key.
test(select_keyword_expanded, [true(Ids == ['dev-test/foo-1.0'])]) :-
  findall(I, query:search(select(keyword,equal,stable(amd64)), qtest://I), Ids0),
  msort(Ids0, Ids).

test(select_keywords_runtime, [true(Ids == ['dev-test/foo-1.0'])]) :-
  Q =.. [select, keywords, equal, stable(amd64)],
  findall(I, query:search(Q, qtest://I), Ids0),
  msort(Ids0, Ids).

% maintainer(M) enumerates list members (previously a wrong-arity clause, so
% the inlining never happened).
test(maintainer_expanded, [true(Ms == ['dev@example.org','other@example.org'])]) :-
  findall(M, query:search(maintainer(M), qtest://'dev-test/foo-1.0'), Ms0),
  msort(Ms0, Ms).

test(select_maintainer_runtime, [true(Ids == ['dev-test/foo-1.0'])]) :-
  Q =.. [select, maintainer, equal, 'dev@example.org'],
  findall(I, query:search(Q, qtest://I), Ids0),
  msort(Ids0, Ids).

% is_cn_target/1 only recognises the version_none form (the stale
% pre-version/7 list form is gone).
test(is_cn_target_version_none) :-
  target:is_cn_target(qualified_target(none, _, 'dev-test', foo, version_none, [[],[]])).

test(is_cn_target_rejects_versioned, [fail]) :-
  target:is_cn_target(qualified_target(none, _, 'dev-test', foo,
                                       version([1,0],'',4,0,[],0,'1.0'), [[],[]])).

:- end_tests(query_macros).


% =============================================================================
%  Query macro vs runtime dedup (portage-ng#59)
% =============================================================================

% compile_query_compound/3 is the single source of truth for every query form
% it covers: the runtime query:search/2 entry clause compiles the query at
% call time and executes the same cache-level goal the compile-time expansion
% would inline (the former duplicate runtime clauses for version comparisons,
% slot constraints, iuse and maintainer were deleted). These tests pin:
%   (a) macro coverage  — forms that must compile to cache-level goals
%                         rather than the runtime fallback,
%   (b) equivalence     — the runtime (=..-built) path returns the same
%                         results as directly executing the compiled goal,
%   (c) hook guards     — the module-local expansion hooks neither fire for
%                         foreign modules nor bind call-site variables.

query_dedup_setup :-
  query_macros_setup,
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-1.0', iuse, plus(minimal))),
  assertz(cache:entry_metadata(qtest, 'dev-test/foo-1.0', iuse, doc)).

query_dedup_cleanup :-
  query_macros_cleanup.

% Runtime path (query:search/2 on a runtime-constructed query) and expanded
% path (executing the compile_query_compound/3 goal directly) must agree.
issue59_runtime_vs_expanded(Q) :-
  copy_term(Q, Qr),
  findall(I1, query:search(Qr, qtest://I1), Rs0),
  msort(Rs0, Rs),
  copy_term(Q, Qe),
  query:compile_query_compound(Qe, qtest://I2, G),
  findall(I2, call(G), Es0),
  msort(Es0, Es),
  Rs == Es.

:- begin_tests(query_dedup, [setup(query_dedup_setup),
                             cleanup(query_dedup_cleanup)]).

% (a) Macro coverage: these forms must compile to cache-level goals, not to
% the search/2 runtime fallback (guards against macro rot reintroducing a
% second, runtime-only implementation).
test(issue59_macro_coverage) :-
  V = version([1,0],'',4,0,[],0,'1.0'),
  forall(member(Q, [ name(_), category(_), version(_), slot(_), keyword(_),
                     iuse(_), masked(true), masked(false),
                     installed(true), installed(false),
                     dependency(_,run), dependency(_,install),
                     dependency(_,fetchonly),
                     select(version,equal,V), select(version,greaterequal,V),
                     select(version,tilde,V), select(name,wildcard,_),
                     select(slot,equal,_), select(slot,constraint([]),_),
                     select(slot,constraint([slot(_)]),_),
                     select(slot,constraint([any_same_slot]),_),
                     select(masked,equal,true), select(installed,equal,true),
                     select(maintainer,equal,_),
                     all(depend(_)), all(dependency(_,run)) ]),
         ( query:compile_query_compound(Q, _R://_I, G),
           G \= search(_,_) )).

% A slot constraint whose list skeleton is still unbound is deferred to the
% runtime fallback (and re-compiled at call time once it is bound).
test(issue59_unbound_slot_constraint_defers) :-
  query:compile_query_compound(select(slot,constraint(C),Sn), R://I, G),
  var(C),
  G == search(select(slot,constraint(C),Sn), R://I).

% (b) Equivalence per query form whose runtime duplicate was deleted.
test(issue59_runtime_matches_expanded) :-
  V10 = version([1,0],'',4,0,[],0,'1.0'),
  forall(member(Q, [ select(version,equal,V10),
                     select(version,greater,V10),
                     select(version,greaterequal,V10),
                     select(version,smaller,V10),
                     select(version,smallerequal,V10),
                     select(version,notequal,V10),
                     select(version,tilde,V10),
                     select(version,wildcard,version(_,_,_,_,_,_,'1.0*')),
                     select(slot,constraint([]),_),
                     select(slot,constraint([slot('1')]),_),
                     select(slot,constraint([slot(_)]),_),
                     select(slot,constraint([any_same_slot]),_),
                     select(slot,constraint([any_different_slot]),_),
                     select(maintainer,equal,'dev@example.org'),
                     iuse(_),
                     installed(false) ]),
         issue59_runtime_vs_expanded(Q)).

% A runtime slot-constraint query with an unbound inner slot argument
% (formerly served by the deleted runtime clauses) binds it from the cache.
test(issue59_slot_constraint_runtime_var_inner,
     [true(S-Sn == '1'-[slot('1')])]) :-
  Q =.. [select, slot, constraint([slot(S)]), Sn],
  once(query:search(Q, qtest://'dev-test/foo-1.0')).

% A runtime slot-constraint query whose entire skeleton is still a variable
% is served by the runtime-only generator clause: it commits to the first
% matching skeleton pattern ([], all slot metadata) exactly like the
% head-unification + cut of the former runtime clauses.
test(issue59_slot_constraint_runtime_var_skeleton,
     [true(C-Sn == []-[slot('1')])]) :-
  Q =.. [select, slot, constraint(C), Sn],
  once(query:search(Q, qtest://'dev-test/foo-1.0')).

test(issue59_slot_constraint_runtime_var_skeleton_enumerates,
     [true(N == 3)]) :-
  Q =.. [select, slot, constraint(_), _],
  aggregate_all(count, query:search(Q, qtest://_), N).

% A variable operator at call time behaves like the 'none' operator
% (mirrors the former runtime clause, which unified it with 'none').
test(issue59_version_var_op_behaves_as_none, [true(N == 3)]) :-
  Q =.. [select, version, _Op, _V],
  aggregate_all(count, query:search(Q, qtest://_), N).

% List queries constructed at runtime go through the same compile-then-call
% path as compile-time literals.
test(issue59_list_query_runtime, [true(Ids == ['dev-test/foo-2.0'])]) :-
  QL = [category('dev-test'), name(foo), select(slot,constraint([slot('2')]),_)],
  findall(I, query:search(QL, qtest://I), Ids0),
  sort(Ids0, Ids).

% iuse/1 returns the RAW metadata value (e.g. plus(flag)); the deleted
% runtime clause silently stripped defaults, diverging from the macro.
test(issue59_iuse_returns_raw_metadata, [true(Vs == [doc, plus(minimal)])]) :-
  Q =.. [iuse, V],
  findall(V, query:search(Q, qtest://'dev-test/foo-1.0'), Vs0),
  msort(Vs0, Vs).

% CLI iuse searches are served by select/4 (sign-aware), the single
% remaining runtime implementation.
test(issue59_select_iuse_equal_runtime, [true(Ids == ['dev-test/foo-1.0'])]) :-
  Q =.. [select, iuse, equal, minimal],
  findall(I, query:search(Q, qtest://I), Ids0),
  sort(Ids0, Ids).

test(issue59_select_iuse_wildcard_runtime, [true(Ids == ['dev-test/foo-1.0'])]) :-
  Q =.. [select, iuse, wildcard, 'mini*'],
  findall(I, query:search(Q, qtest://I), Ids0),
  sort(Ids0, Ids).

% (c) The search/2 expansion hook is module-local to query: qualified
% callers are inlined, while a bare search/2 goal in another module is
% never rewritten (modules may define their own search/2).
test(issue59_search_hook_is_module_local) :-
  query:goal_expansion(search(category(_), qtest://_), G),
  G \= search(_,_),
  \+ user:goal_expansion(search(category(_), qtest://_), _).

% Download-specialized candidate hooks fire only for a bound 'download'
% action; a variable action is left for runtime resolution instead of
% being bound at expansion time.
test(issue59_eligible_download_inlined,
     [true(G =@= cache:ordered_entry(qtest, 'dev-test/foo-1.0', _, _, _))]) :-
  candidate:goal_expansion(eligible(qtest://'dev-test/foo-1.0':download?{[]}), G),
  !.

test(issue59_eligible_var_action_not_expanded) :-
  \+ candidate:goal_expansion(eligible(qtest://'dev-test/foo-1.0':_A?{[]}), _).

test(issue59_resolve_download_expanded,
     [true(G == featureterm:get(after, ctx, Conds))]) :-
  candidate:goal_expansion(resolve(qtest://x:download?{ctx}, Conds), G),
  !.

test(issue59_resolve_var_action_not_expanded) :-
  \+ candidate:goal_expansion(resolve(qtest://x:_A?{ctx}, _C), _).

% candidate:installed/1 inlines through the same compile_query_compound
% table its definition compiles through (single source of truth).
test(issue59_installed_macro_single_source) :-
  candidate:goal_expansion(installed(R://I), G1),
  !,
  query:compile_query_compound(installed(true), R://I, G2),
  G1 =@= G2.

% The version_domain:normalize_version_term/2 hook (formerly a dead
% user:goal_expansion clause in query.pl) is module-local to version_domain
% and rewrites the goal to a cache-free conditional.
test(issue59_version_domain_hook_fires) :-
  version_domain:goal_expansion(normalize_version_term(_, _), G),
  G \= normalize_version_term(_, _).

% The expansion and the predicate agree on every input class: unbound input
% (identity — the dead macro got this wrong), version/7 passthrough,
% wildcard atom, parseable atom, version(...)=Ver strip, arbitrary compound
% passthrough, numeric input. The predicate is meta-called so this compares
% expansion vs predicate, not expansion vs expansion.
test(issue59_version_domain_hook_matches_predicate) :-
  forall(member(V, [_, version([1,0],'',4,0,[],0,'1.0'), '1.0.*', '2.3',
                    version(a,b,c,d,e,f,g)=myver, foo(bar), 42]),
         ( version_domain:goal_expansion(normalize_version_term(V, R1), G),
           call(G),
           P =.. [normalize_version_term, V, R2],
           version_domain:P,
           R1 =@= R2 )).

% The non-download eligible expansion derives its mask check from the
% masked(true) macro.
test(issue59_eligible_install_uses_masked_macro) :-
  candidate:goal_expansion(eligible(qtest://'dev-test/foo-1.0':install?{[]}), G),
  !,
  G = ((Masked -> (prover:assuming(unmask) -> true ; memo:visibility_override_(_, _)) ; true), _),
  query:compile_query_compound(masked(true), qtest://'dev-test/foo-1.0', MaskedExpected),
  Masked == MaskedExpected.

:- end_tests(query_dedup).


% =============================================================================
%  Synthetic-rule resolver core tests (issue #73)
% =============================================================================
%
% KB-independent unit tests for the resolver core, in the same spirit as the
% synthetic scheduler tests above. The rules module exposes a synthetic rule
% store (rules:enable_test_rules/0, rules:test_rule/2): while active,
% rules:rule/2 resolves EXCLUSIVELY against hand-built test_rule/2 clauses,
% so prover:prove/9, planner:plan/5 and the prove_with_fallback tier chain
% can be exercised over tiny rule sets without a knowledge base.
%
% Goals are passed as BARE literals (no ?{[]} proof-context wrapper):
% prover:canon_literal/3 canonicalizes the R://- and :Action-shaped literal
% forms used by the production rules, but a bare atom wrapped in ?{Ctx} is
% itself its canonical form, which would make proof/model keys diverge from
% the body literals. Bare goals keep all keys canonical.

% Replace the synthetic rule store contents with Head-Body pairs.
issue73_rules(Pairs) :-
  rules:enable_test_rules,
  retractall(rules:test_rule(_, _)),
  forall(member(H-B, Pairs), assertz(rules:test_rule(H, B))).

% Wave index of the rule whose canonical head is Lit (fails when unplanned).
issue73_wave(Plan, Lit, W) :-
  nth1(W, Plan, Wave),
  member(R, Wave),
  prover:rule_head(R, Lit),
  !.


% -----------------------------------------------------------------------------
%  Prover core: proof / model / cycle-break shape (issue #73)
% -----------------------------------------------------------------------------

:- begin_tests(prover_core_synthetic, [cleanup(rules:disable_test_rules)]).

% A linear chain proves every literal exactly once: the model holds each
% literal, the proof holds one rule(L) key per literal with the synthetic
% body and dep count, and the triggers AVL is the reverse dependency index.
test(chain_proof_model_triggers_shape) :-
  issue73_rules([a-[b], b-[c], c-[]]),
  prover:prove([a], t, Proof, t, Model, t, _Cons, t, Triggers),
  get_assoc(a, Model, _),
  get_assoc(b, Model, _),
  get_assoc(c, Model, _),
  get_assoc(rule(a), Proof, dep(1, [b])?_),
  get_assoc(rule(b), Proof, dep(1, [c])?_),
  get_assoc(rule(c), Proof, dep(0, [])?_),
  \+ gen_assoc(assumed(_), Model, _),
  forall(gen_assoc(K, Proof, _), K \= assumed(rule(_))),
  get_assoc(b, Triggers, [a]),
  get_assoc(c, Triggers, [b]).

% A shared dependency (diamond) is proven once and triggers both parents.
test(diamond_shared_dep_proved_once) :-
  issue73_rules([a-[b, c], b-[d], c-[d], d-[]]),
  prover:prove([a], t, Proof, t, Model, t, _Cons, t, Triggers),
  get_assoc(rule(d), Proof, dep(0, [])?_),
  get_assoc(d, Model, _),
  get_assoc(d, Triggers, Dependents),
  msort(Dependents, [b, c]).

% A structural cycle yields a prover cycle-break: proof key
% assumed(rule(Lit)) (dep count -1, body preserved for the scheduler), a
% cycle_path witness, and assumed(Lit) in the model — while the regular
% rule(Lit) entry remains. This is the `assumed(rule(X))` axis of the
% assumption taxonomy, distinct from domain assumptions.
test(structural_cycle_break_shape) :-
  issue73_rules([a-[b], b-[a]]),
  prover:prove([a], t, Proof, t, Model, t, _Cons, t, _Triggers),
  get_assoc(assumed(rule(a)), Proof, dep(-1, [b])?_),
  get_assoc(cycle_path(a), Proof, CyclePath),
  CyclePath == [a, b, a],
  get_assoc(assumed(a), Model, _),
  get_assoc(rule(a), Proof, dep(1, [b])?_),
  get_assoc(rule(b), Proof, dep(1, [a])?_),
  get_assoc(b, Model, _).

% An RDEPEND-mediated cycle (a :run step on the cycle path) is classified
% benign by heuristic:cycle_benign/2: no cycle-break assumption of any kind
% is recorded.
test(benign_run_cycle_no_assumption) :-
  issue73_rules([(p:run)-[q:run], (q:run)-[p:run]]),
  prover:prove([p:run], t, Proof, t, Model, t, _Cons, t, _Triggers),
  get_assoc(p:run, Model, _),
  get_assoc(q:run, Model, _),
  \+ gen_assoc(assumed(_), Model, _),
  forall(gen_assoc(K, Proof, _), K \= assumed(rule(_))).

% A domain assumption (assumed/1 emitted by a rule body) is stored under the
% proof key rule(assumed(X)) — the OTHER axis of the assumption taxonomy —
% and never as a prover cycle-break key.
test(domain_assumption_shape) :-
  issue73_rules([p-[assumed(q)], assumed(_)-[]]),
  prover:prove([p], t, Proof, t, Model, t, _Cons, t, _Triggers),
  get_assoc(rule(p), Proof, dep(1, [assumed(q)])?_),
  get_assoc(rule(assumed(q)), Proof, dep(0, [])?_),
  get_assoc(assumed(q), Model, _),
  \+ get_assoc(assumed(rule(q)), Proof, _),
  \+ get_assoc(cycle_path(q), Proof, _).

% naf/1 conflict detection: a body requiring both naf(q) and q has no model.
test(naf_conflict_fails, [fail]) :-
  issue73_rules([p-[naf(q), q], naf(_)-[], q-[]]),
  prover:prove([p], t, _Proof, t, _Model, t, _Cons, t, _Triggers).

% constraint/1 body literals are routed to the constraint store: they never
% appear in the model or the triggers, but the head's dep body retains them
% and the value lands in the constraint AVL.
test(constraint_routed_to_store) :-
  issue73_rules([p-[constraint(k:{hello})]]),
  prover:prove([p], t, Proof, t, Model, t, Cons, t, Triggers),
  get_assoc(rule(p), Proof, dep(1, [constraint(k:{hello})])?_),
  get_assoc(k, Cons, hello),
  \+ gen_assoc(constraint(_), Model, _),
  \+ gen_assoc(constraint(_), Triggers, _).

:- end_tests(prover_core_synthetic).


% -----------------------------------------------------------------------------
%  Planner: wave-ordering invariants over synthetic proofs (issue #73)
% -----------------------------------------------------------------------------

:- begin_tests(planner_waves_synthetic, [cleanup(rules:disable_test_rules)]).

% Helper: prove Goals over the active synthetic rule set and plan the proof.
issue73_plan(Goals, Plan, Remainder) :-
  prover:prove(Goals, t, Proof, t, _Model, t, _Cons, t, Triggers),
  planner:plan(Proof, Triggers, t, Plan, Remainder).

% A linear chain plans leaf-first, one rule per wave, empty remainder.
test(chain_waves_dependency_order) :-
  issue73_rules([a-[b], b-[c], c-[]]),
  issue73_plan([a], Plan, Remainder),
  Remainder == [],
  issue73_wave(Plan, c, W1),
  issue73_wave(Plan, b, W2),
  issue73_wave(Plan, a, W3),
  W1 < W2, W2 < W3.

% Diamond: independent siblings share a wave; the wave invariant holds
% (every non-constraint body dep sits in a strictly earlier wave).
test(diamond_siblings_share_wave_and_invariant_holds) :-
  issue73_rules([a-[b, c], b-[d], c-[d], d-[]]),
  issue73_plan([a], Plan, Remainder),
  Remainder == [],
  issue73_wave(Plan, d, WD),
  issue73_wave(Plan, b, WB),
  issue73_wave(Plan, c, WC),
  issue73_wave(Plan, a, WA),
  WB =:= WC,
  WD < WB, WB < WA,
  forall(( nth1(W, Plan, Wave), member(R, Wave),
           prover:rule_body(R, Body), member(Dep, Body),
           \+ constraint:is_constraint(Dep) ),
         ( prover:canon_literal(Dep, DepLit, _),
           issue73_wave(Plan, DepLit, WDep),
           WDep < W )).

% A cycle keeps its members (and everything depending on them) out of the
% wave plan: they are returned as the remainder for the scheduler, while
% the acyclic portion is still planned.
test(cycle_members_stay_in_remainder) :-
  issue73_rules([top-[a, x], a-[b], b-[a], x-[]]),
  issue73_plan([top], Plan, Remainder),
  issue73_wave(Plan, x, _),
  \+ issue73_wave(Plan, a, _),
  \+ issue73_wave(Plan, b, _),
  \+ issue73_wave(Plan, top, _),
  findall(H, ( member(R, Remainder), prover:rule_head(R, H) ), Heads0),
  msort(Heads0, Heads),
  Heads == [a, b, top].

% Domain assumptions are planned like ordinary heads: the assumed literal
% is a wave-1 leaf and its consumer lands strictly later.
test(domain_assumption_planned_before_consumer) :-
  issue73_rules([p-[assumed(q)], assumed(_)-[]]),
  issue73_plan([p], Plan, Remainder),
  Remainder == [],
  issue73_wave(Plan, assumed(q), W1),
  issue73_wave(Plan, p, W2),
  W1 < W2.

% Constraint body literals are not ordering edges: a head whose body is
% only constraints is immediately ready (wave 1).
test(constraint_deps_do_not_block_readiness) :-
  issue73_rules([p-[constraint(k:{v})]]),
  issue73_plan([p], Plan, Remainder),
  Remainder == [],
  issue73_wave(Plan, p, 1).

:- end_tests(planner_waves_synthetic).


% -----------------------------------------------------------------------------
%  Pipeline: prove_with_fallback tier selection (issue #73)
% -----------------------------------------------------------------------------
%
% The 5-tier committed-choice relaxation chain (strict, keyword_acceptance,
% blockers, unmask, keyword_unmask) is exercised with stubbed failures:
% guarded test_rule/2 clauses succeed only under specific prover:assuming/1
% flags, and a marker literal in the body records which tier produced the
% accepted model.

:- begin_tests(pipeline_fallback_tiers, [cleanup(rules:disable_test_rules)]).

test(strict_tier_succeeds_without_flags) :-
  issue73_rules([s-[]]),
  pipeline:prove_with_fallback([s], _Proof, Model, _Triggers),
  get_assoc(s, Model, _).

% Tier order: keyword_acceptance is tried before blockers, so a goal
% provable under either resolves under keyword_acceptance.
test(keyword_acceptance_preferred_over_blockers) :-
  issue73_rules([marker(_)-[]]),
  assertz((rules:test_rule(k1, [marker(keyword)]) :-
             prover:assuming(keyword_acceptance))),
  assertz((rules:test_rule(k1, [marker(blockers)]) :-
             prover:assuming(blockers))),
  pipeline:prove_with_fallback([k1], _Proof, Model, _Triggers),
  get_assoc(marker(keyword), Model, _),
  \+ get_assoc(marker(blockers), Model, _).

test(blockers_tier_reached_when_keyword_insufficient) :-
  issue73_rules([marker(_)-[]]),
  assertz((rules:test_rule(k2, [marker(blockers)]) :-
             prover:assuming(blockers))),
  pipeline:prove_with_fallback([k2], _Proof, Model, _Triggers),
  get_assoc(marker(blockers), Model, _).

% The unmask tier sets ONLY unmask (no keyword_acceptance); the guard
% rejects the final keyword_unmask tier, so success proves tier 4 ran.
test(unmask_tier_sets_only_unmask) :-
  issue73_rules([marker(_)-[]]),
  assertz((rules:test_rule(k3, [marker(unmask)]) :-
             prover:assuming(unmask),
             \+ prover:assuming(keyword_acceptance))),
  pipeline:prove_with_fallback([k3], _Proof, Model, _Triggers),
  get_assoc(marker(unmask), Model, _).

% The final tier sets keyword_acceptance AND unmask together.
test(keyword_unmask_tier_sets_both_flags) :-
  issue73_rules([marker(_)-[]]),
  assertz((rules:test_rule(k4, [marker(both)]) :-
             prover:assuming(keyword_acceptance),
             prover:assuming(unmask))),
  pipeline:prove_with_fallback([k4], _Proof, Model, _Triggers),
  get_assoc(marker(both), Model, _).

% When no tier can prove the goal, the chain fails deterministically.
test(all_tiers_exhausted_fails, [fail]) :-
  issue73_rules([]),
  pipeline:prove_with_fallback([nope], _Proof, _Model, _Triggers).

% prove_plan_with_fallback/6 reports the tier that succeeded and still
% produces a wave-ordered plan (marker leaf before its consumer).
test(prove_plan_with_fallback_reports_tier, [true(Used == keyword_acceptance)]) :-
  issue73_rules([marker(_)-[]]),
  assertz((rules:test_rule(k5, [marker(keyword)]) :-
             prover:assuming(keyword_acceptance))),
  pipeline:prove_plan_with_fallback([k5], _Proof, _Model, Plan, _Triggers, Used),
  issue73_wave(Plan, marker(keyword), W1),
  issue73_wave(Plan, k5, W2),
  W1 < W2.

test(prove_plan_with_fallback_strict_reports_false, [true(Used == false)]) :-
  issue73_rules([s2-[]]),
  pipeline:prove_plan_with_fallback([s2], _Proof, _Model, _Plan, _Triggers, Used).

% The assuming/1 flags are scoped to the fallback attempt: none survive.
test(assuming_flags_restored_after_fallback) :-
  issue73_rules([marker(_)-[]]),
  assertz((rules:test_rule(k6, [marker(both)]) :-
             prover:assuming(keyword_acceptance),
             prover:assuming(unmask))),
  pipeline:prove_with_fallback([k6], _Proof, _Model, _Triggers),
  \+ prover:assuming(keyword_acceptance),
  \+ prover:assuming(blockers),
  \+ prover:assuming(unmask).

:- end_tests(pipeline_fallback_tiers).


% -----------------------------------------------------------------------------
%  Assumption classification: polarity table (issue #73)
% -----------------------------------------------------------------------------
%
% Table-driven tests for assumption:assumption_type/2 and
% assumption:assumption_reason_type/2, organized by the polarity taxonomy
% from the project rules:
%   - positive / actionable: a config change resolves the plan
%     (unmask, accept ~arch, accept license, resolve blocker)
%   - negative / blocking: structurally unsatisfiable as stated
%   - cycle axis: prover cycle-breaks, a separate benign axis
%   - info: bookkeeping types (assumed installed/running)

:- begin_tests(assumption_polarity).

% assumption_type_vector(Polarity, Term, ExpectedType)

% POSITIVE / actionable
assumption_type_vector(positive,
  portage://'app-misc/x-1.0':unmask,
  masked).
assumption_type_vector(positive,
  grouped_package_dependency(no, 'dev-libs', foo, []):install?{[assumption_reason(masked)]},
  masked_dependency).
assumption_type_vector(positive,
  grouped_package_dependency(no, 'dev-libs', foo, []):install?{[assumption_reason(keyword_filtered)]},
  keyword_filtered_dependency).
assumption_type_vector(positive,
  blocker(weak, run, 'app-misc', x, none, version_none, []),
  blocker_assumption).

% NEGATIVE / blocking
assumption_type_vector(negative,
  grouped_package_dependency(no, 'dev-libs', foo, []):install,
  non_existent_dependency).
assumption_type_vector(negative,
  package_dependency(install, no, 'dev-libs', foo, none, version_none, [], []):install,
  non_existent_dependency).
assumption_type_vector(negative,
  grouped_package_dependency(no, 'dev-libs', foo, []):install?{[assumption_reason(missing)]},
  missing_dependency).
assumption_type_vector(negative,
  grouped_package_dependency(no, 'dev-libs', foo, []):install?{[assumption_reason(version_no_candidate(any, []))]},
  version_no_candidate_dependency).
assumption_type_vector(negative,
  grouped_package_dependency(no, 'dev-libs', foo, []):install?{[assumption_reason(version_conflict(x))]},
  version_conflict_dependency).
assumption_type_vector(negative,
  grouped_package_dependency(no, 'dev-libs', foo, []):install?{[assumption_reason(unsatisfied_constraints)]},
  unsatisfied_constraints_dependency).
assumption_type_vector(negative,
  grouped_package_dependency(no, 'acct-user', git, []):install?{[required_use_violation(use_dep_unsat(x, use_state([gitea], []), profile_hard_conflict))]},
  use_dep_unsat).

% CYCLE axis (benign, separate from domain assumptions)
assumption_type_vector(cycle, cycle_break(foo),                                cycle_break).
assumption_type_vector(cycle, required(flag),                                  use_requirement_cycle).
assumption_type_vector(cycle, blocking(flag),                                  use_requirement_cycle).
assumption_type_vector(cycle, use_conditional_group(positive, f, portage://'a/b-1', []),
                              use_conditional_cycle).
assumption_type_vector(cycle, any_of_group([]),                                dependency_group_cycle).
assumption_type_vector(cycle, all_of_group([]),                                dependency_group_cycle).
assumption_type_vector(cycle, exactly_one_of_group([]),                        dependency_group_cycle).
assumption_type_vector(cycle, at_most_one_of_group([]),                        dependency_group_cycle).
assumption_type_vector(cycle, naf(foo),                                        naf_cycle).

% INFO (bookkeeping). Note: grouped_package_dependency(_,_,_,_):install/run
% classify as non_existent_dependency (the arity-4 catch-all precedes the
% action-specific clauses in assumption.pl), so only the concrete
% R://Entry:Action forms are info-classified.
assumption_type_vector(info, portage://'app-misc/x-1.0':install,               assumed_installed).
assumption_type_vector(info, portage://'app-misc/x-1.0':run,                   assumed_running).

% Catch-all
assumption_type_vector(other, completely_unknown_term(42),                     other).

check_assumption_vectors(Polarity) :-
  forall(assumption_type_vector(Polarity, Term, Expected),
         ( assumption:assumption_type(Term, Got),
           Got == Expected )).

test(positive_actionable_vectors) :- check_assumption_vectors(positive).
test(negative_blocking_vectors)   :- check_assumption_vectors(negative).
test(cycle_axis_vectors)          :- check_assumption_vectors(cycle).
test(info_vectors)                :- check_assumption_vectors(info).
test(other_fallthrough_vector)    :- check_assumption_vectors(other).

% Classification is total and deterministic over all table entries.
test(assumption_type_deterministic) :-
  forall(assumption_type_vector(_, Term, _),
         ( findall(T, assumption:assumption_type(Term, T), [_]) )).

% assumption_reason_type/2: full reason -> bucket table.
test(assumption_reason_type_table) :-
  forall(member(Reason-Type,
                [ missing                      - missing_dependency,
                  masked                       - masked_dependency,
                  keyword_filtered             - keyword_filtered_dependency,
                  installed_required           - installed_required_dependency,
                  slot_unsatisfied             - slot_unsatisfied_dependency,
                  version_no_candidate(any,[]) - version_no_candidate_dependency,
                  version_no_candidate         - version_no_candidate_dependency,
                  version_conflict(x)          - version_conflict_dependency,
                  version_conflict             - version_conflict_dependency,
                  version_unsatisfied          - version_no_candidate_dependency,
                  unsatisfied_constraints      - unsatisfied_constraints_dependency ]),
         ( assumption:assumption_reason_type(Reason, Got),
           Got == Type )).

% Unknown reasons have no bucket (callers fall back explicitly).
test(assumption_reason_type_unknown_fails, [fail]) :-
  assumption:assumption_reason_type(no_such_reason, _).

:- end_tests(assumption_polarity).


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

% No handler triggered -> selection fails (process_requests then reports
% the unrecognised options and falls through to its catch-all halt(1)).
test(select_fails_on_unrecognised_options, [fail]) :-
  interface:request_select(standalone, [], [merge(false)], _, _).

:- end_tests(interface_request_dispatch).


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
% `preference:local_masked/2` entry ids, and compares them to the checked-in
% snapshot in `profile_mask_golden_ids/1` below.
%
% Usage:
%
%   make test-profile-mask-golden
%
% Regenerate golden after an intentional change:
%
%   make test-profile-mask-golden-update
%
% NOTE: the golden list below pins a specific Portage tree snapshot (the
% tree from which `Knowledge/kb.qlf` / `Knowledge/profile.qlf` were last
% generated). After a `--sync` that changes profile package.mask entries,
% a mismatch is expected — review the diff and regenerate the snapshot.


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
% The live `preference:local_masked/2` facts (profile + user config) are
% snapshotted up front and restored afterwards, so running the golden
% regression does not clobber the session's mask state.

profile_mask_golden_masked_ids(Ids) :-
  findall(local_masked(SavedId, SavedRepo),
          preference:local_masked(SavedId, SavedRepo),
          Saved),
  setup_call_cleanup(
    retractall(preference:local_masked(_,_)),
    ( forall(profiledata:entry(package_mask, Atom, true),
             profile:apply_entry(package_mask, Atom, true)),
      findall(Id, preference:local_masked(Id, portage), Ids0),
      sort(Ids0, Ids)
    ),
    ( retractall(preference:local_masked(_,_)),
      forall(member(local_masked(SavedId, SavedRepo), Saved),
             assertz(preference:local_masked(SavedId, SavedRepo)))
    )
  ).


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


% -----------------------------------------------------------------------------
% VDB import round-trip tests (--import-vdb wire format + server registration)
% -----------------------------------------------------------------------------

%! vdb_import_env_ready is semidet.
%
% Load the server-mode module (and the libraries it needs) on demand so the
% /import-vdb handler logic can be exercised in a standalone test session.
% Fails (skipping the dependent tests) when the environment cannot be set up.

vdb_import_env_ready :-
  catch(
    ( load_files([library(pengines),
                  library('http/http_client'),
                  library('http/http_digest'),
                  library(streams)], [if(not_loaded)]),
      load_files(portage('Source/Application/Mode/server'), [if(not_loaded)])
    ),
    _, fail).


%! vdb_emit(+Term) is det.
%
% Emit one canonical wire term (mirrors client:emit_vdb_term/1).

vdb_emit(T) :-
  write_canonical(T),
  write('.\n').


%! vdb_test_payload(+Hostname, +Entries, +Metadata, -Payload) is det.
%
% Build a well-formed /import-vdb payload string from oe/4 and md/3 fact
% lists, mirroring the client-side serializer (client:vdb_payload/3).

vdb_test_payload(Hostname, Entries, Metadata, Payload) :-
  length(Entries, EC),
  length(Metadata, MC),
  variant_sha1(Entries-Metadata, Sha),
  with_output_to(string(Payload),
    ( vdb_emit(vdb_import_v1),
      vdb_emit(hostname(Hostname)),
      vdb_emit(stamp(stamp(EC, Sha))),
      forall(member(E, Entries), vdb_emit(E)),
      forall(member(M, Metadata), vdb_emit(M)),
      vdb_emit(end_of_vdb_import(EC, MC))
    )).


%! vdb_import_cleanup(+Repo) is det.
%
% Retract every fact registered for a test import repository.

vdb_import_cleanup(Repo) :-
  retractall(cache:repository(Repo)),
  retractall(cache:category(Repo, _)),
  retractall(cache:package(Repo, _, _)),
  retractall(cache:ordered_entry(Repo, _, _, _, _)),
  retractall(cache:entry_metadata(Repo, _, _, _)),
  retractall(knowledgebase:client_vdb_stamp(Repo, _)).


:- begin_tests(vdb_import_roundtrip).

test(synthetic_roundtrip,
     [condition(unittest:vdb_import_env_ready),
      cleanup(unittest:vdb_import_cleanup('pkg@unittest.local')),
      nondet]) :-
  V1 = version([1,0], '', 4, 0, [], 0, '1.0'),
  V2 = version([2,1], '', 4, 0, [], 1, '2.1-r1'),
  Entries = [oe('test-cat/foo-2.1-r1', 'test-cat', foo, V2),
             oe('test-cat/foo-1.0',    'test-cat', foo, V1),
             oe('other-cat/bar-1.0',   'other-cat', bar, V1)],
  Metadata = [md('test-cat/foo-2.1-r1', installed, true),
              md('test-cat/foo-1.0',    installed, true),
              md('test-cat/foo-1.0',    slot, slot('0')),
              md('other-cat/bar-1.0',   installed, true)],
  unittest:vdb_test_payload('unittest.local', Entries, Metadata, Payload),
  with_output_to(string(Out), server:import_vdb(Payload)),
  sub_string(Out, _, _, _, "vdb-import: ok"),
  Repo = 'pkg@unittest.local',
  % Repository registered with derived category/package facts.
  cache:repository(Repo),
  findall(C, cache:category(Repo, C), Cats0),
  msort(Cats0, Cats),
  Cats == ['other-cat', 'test-cat'],
  cache:package(Repo, 'test-cat', foo),
  cache:package(Repo, 'other-cat', bar),
  % Ordered entries preserved in wire order (version-descending).
  findall(oe(Id, C2, N2, Vv), cache:ordered_entry(Repo, Id, C2, N2, Vv), Got),
  Got == Entries,
  % Metadata facts round-trip exactly.
  findall(md(Id2, K, Val), cache:entry_metadata(Repo, Id2, K, Val), GotMd),
  msort(GotMd, GotMdS),
  msort(Metadata, MdS),
  GotMdS == MdS,
  % Import stamp registered for staleness checks.
  knowledgebase:client_vdb_stamp(Repo, stamp(3, _)).

test(real_vdb_parity,
     [condition((unittest:vdb_import_env_ready, cache:repository(pkg))),
      cleanup(unittest:vdb_import_cleanup('pkg@unittest-parity.local')),
      nondet]) :-
  % Serialize the real (already loaded) pkg repository and re-import it as
  % a per-client repo; the imported fact set must match the original
  % exactly, including ordered_entry order.
  findall(oe(Id, C, N, V), cache:ordered_entry(pkg, Id, C, N, V), Entries),
  Entries \== [],
  findall(md(Id, K, V), cache:entry_metadata(pkg, Id, K, V), Metadata),
  unittest:vdb_test_payload('unittest-parity.local', Entries, Metadata, Payload),
  with_output_to(string(_), server:import_vdb(Payload)),
  Repo = 'pkg@unittest-parity.local',
  findall(oe(Id, C, N, V), cache:ordered_entry(Repo, Id, C, N, V), Got),
  Got == Entries,
  findall(md(Id, K, V), cache:entry_metadata(Repo, Id, K, V), GotMd),
  msort(GotMd, GotMdS),
  msort(Metadata, MdS),
  GotMdS == MdS,
  % installed(true) parity between the original and imported repo.
  findall(E1, cache:entry_metadata(pkg,  E1, installed, true), I1),
  findall(E2, cache:entry_metadata(Repo, E2, installed, true), I2),
  msort(I1, S1),
  msort(I2, S2),
  S1 == S2.

test(reject_bad_hostname,
     [condition(unittest:vdb_import_env_ready),
      throws(error(permission_error(import, hostname, _), _))]) :-
  V = version([1,0], '', 4, 0, [], 0, '1.0'),
  unittest:vdb_test_payload('../etc', [oe('c/p-1.0', c, p, V)], [], Payload),
  with_output_to(string(_), server:import_vdb(Payload)).

test(reject_bad_fact_shape,
     [condition(unittest:vdb_import_env_ready),
      throws(error(type_error(vdb_import_fact, _), _))]) :-
  with_output_to(string(Payload),
    ( unittest:vdb_emit(vdb_import_v1),
      unittest:vdb_emit(hostname('unittest.local')),
      unittest:vdb_emit(stamp(stamp(1, sha))),
      unittest:vdb_emit(evil_fact(1)),
      unittest:vdb_emit(end_of_vdb_import(1, 0))
    )),
  with_output_to(string(_), server:import_vdb(Payload)).

test(reject_truncated_payload,
     [condition(unittest:vdb_import_env_ready),
      throws(error(type_error(vdb_import_term, end_of_file), _))]) :-
  with_output_to(string(Payload),
    ( unittest:vdb_emit(vdb_import_v1),
      unittest:vdb_emit(hostname('unittest.local')),
      unittest:vdb_emit(stamp(stamp(1, sha)))
    )),
  with_output_to(string(_), server:import_vdb(Payload)).

:- end_tests(vdb_import_roundtrip).
