/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> GLSATEST
Unit tests for GLSA parsing, version ranges and filtering (Source/Domain/Gentoo/glsa.pl).
*/

:- module(glsatest, []).

:- use_module(library(plunit)).
:- use_module(library(lists)).

% =============================================================================
%  GLSATEST declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  GLSA parse / range / filter tests
% -----------------------------------------------------------------------------

:- begin_tests(glsa).

glsa_fixture_xml(Xml) :-
  atomic_list_concat([
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<!DOCTYPE glsa SYSTEM "http://www.gentoo.org/dtd/glsa.dtd">',
    '<glsa id="202501-03">',
    '    <title>pip: arbitrary configuration injection</title>',
    '    <synopsis>test</synopsis>',
    '    <product type="ebuild">pip</product>',
    '    <announced>2025-01-17</announced>',
    '    <revised count="1">2025-01-17</revised>',
    '    <affected>',
    '        <package name="dev-python/pip" auto="yes" arch="*">',
    '            <unaffected range="ge">23.3</unaffected>',
    '            <vulnerable range="lt">23.3</vulnerable>',
    '        </package>',
    '    </affected>',
    '</glsa>'
  ], '\n', Xml).

test(parse_fixture, [true(Title == 'pip: arbitrary configuration injection')]) :-
  tmp_file_stream(text, File, Out),
  glsa_fixture_xml(Xml),
  write(Out, Xml),
  close(Out),
  glsa:parse_file(File, '202501-03', advisory('202501-03', Title), Packages, Ranges),
  Packages = [package('202501-03', 'dev-python', pip, '*')],
  memberchk(range('202501-03', 'dev-python', pip, vulnerable, lt, _, '*'), Ranges),
  memberchk(range('202501-03', 'dev-python', pip, unaffected, ge, _, '*'), Ranges),
  delete_file(File).

test(version_lt_matches) :-
  atom_codes('23.2', C1), once(phrase(eapi:version(V1), C1, [])),
  atom_codes('23.3', C2), once(phrase(eapi:version(V2), C2, [])),
  glsa:version_matches(lt, V2, V1),
  \+ glsa:version_matches(lt, V2, V2),
  glsa:version_matches(ge, V2, V2).

test(revision_range) :-
  atom_codes('1.0-r1', C1), once(phrase(eapi:version(V1), C1, [])),
  atom_codes('1.0-r2', C2), once(phrase(eapi:version(V2), C2, [])),
  glsa:version_matches(rlt, V2, V1),
  glsa:version_matches(rge, V1, V1),
  \+ glsa:version_matches(rgt, V1, V1).

test(filter_new_affected_skips_applied,
     [setup(glsa_filter_setup),
      cleanup(glsa_filter_cleanup)]) :-
  glsa:applied('209901-01'),
  \+ glsa:applied('209901-02'),
  \+ glsa:filter_allows(new_affected, '209901-01'),
  glsa:filter_allows(new_glsa, '209901-02'),
  \+ glsa:filter_allows(new_glsa, '209901-01'),
  glsa:filter_allows(security, '209901-01').

glsa_filter_setup :-
  glsa:clear_facts,
  assertz(glsa:advisory('209901-01', 'applied one')),
  assertz(glsa:advisory('209901-02', 'fresh one')),
  assertz(glsa:loaded),
  assertz(glsa:cache_source(test)),
  tmp_file('glsa_injected', File),
  setup_call_cleanup(
    open(File, write, Out, [encoding(utf8)]),
    format(Out, '209901-01~n', []),
    close(Out)
  ),
  retractall(glsa:injected_file_override(_)),
  assertz(glsa:injected_file_override(File)).

glsa_filter_cleanup :-
  ( retract(glsa:injected_file_override(File)) ->
      ( exists_file(File) -> delete_file(File) ; true )
  ; true
  ),
  glsa:clear_facts.

:- end_tests(glsa).
