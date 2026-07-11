/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> FEEDBACK
Builder-to-prover learned-knowledge channel (portage-ng#102).

When a build phase fails because a required provider (a command or file
some package would supply) is missing, the diagnosis is not repaired in
place: it is recorded here as a durable discovered dependency edge and
fed back into the resolver. On the next proof pass rules unions the
discovered provider into BDEPEND, so the provider is proved and ordered
before the target (plans are derived, never patched).

Two records are persisted (both plain-text term files under Knowledge/,
gitignored like resume.pl / phase_stats.pl):

  - discovered_dep(Target, Provider, Kind, Evidence): a concrete provider
    that a build proved was needed but the metadata never declared. The
    structured Evidence doubles as an upstream ebuild bug report.
  - unresolved_diagnostic(Symbol, Evidence): a missing-provider signature
    that fired but could not be mapped to a concrete package. Kept as a
    maintainer backlog (extend the seed table / index); never guessed.
*/

:- module(feedback, []).

% =============================================================================
%  FEEDBACK declarations
% =============================================================================

:- dynamic feedback:discovered_dep/4.
:- dynamic feedback:unresolved_diagnostic/2.
:- dynamic feedback:session_discovery/4.
:- dynamic feedback:loaded_/0.

% -----------------------------------------------------------------------------
%  State files
% -----------------------------------------------------------------------------

%! feedback:discovered_file(-Path) is det.
%
% Path to the persisted discovered-dependency file (Knowledge/feedback.pl).

feedback:discovered_file(Path) :-
  config:installation_dir(Dir),
  os:compose_path([Dir, 'Knowledge', 'feedback.pl'], Path).


%! feedback:unresolved_file(-Path) is det.
%
% Path to the persisted unresolved-diagnostic backlog
% (Knowledge/feedback.unresolved.pl).

feedback:unresolved_file(Path) :-
  config:installation_dir(Dir),
  os:compose_path([Dir, 'Knowledge', 'feedback.unresolved.pl'], Path).


% -----------------------------------------------------------------------------
%  Startup load
% -----------------------------------------------------------------------------

%! feedback:load is det.
%
% Consults the persisted discovery/backlog files into the dynamic store,
% at most once per session. Called from init_knowledgebase/0 after the
% host config is loaded so config:installation_dir/1 is available. Missing
% files are not an error (a fresh baseline has made no discoveries yet).

feedback:load :-
  ( feedback:loaded_
  -> true
  ;  assertz(feedback:loaded_),
     feedback:discovered_file(DPath),
     feedback:consult_terms(DPath),
     feedback:unresolved_file(UPath),
     feedback:consult_terms(UPath)
  ).


%! feedback:consult_terms(+Path) is det.
%
% Reads every term from Path and asserts it, skipping if the file does not
% exist. Errors are contained so a corrupt file can never abort startup.

feedback:consult_terms(Path) :-
  ( exists_file(Path)
  -> catch(
       setup_call_cleanup(
         open(Path, read, S),
         feedback:read_assert_terms(S),
         close(S)),
       _, true)
  ;  true
  ).


%! feedback:read_assert_terms(+Stream) is det.
%
% Reads terms until end_of_file, asserting each discovered_dep/4 or
% unresolved_diagnostic/2 fact (other terms are ignored defensively).

feedback:read_assert_terms(S) :-
  read_term(S, T, []),
  ( T == end_of_file
  -> true
  ;  ( T = discovered_dep(_, _, _, _)   -> assertz(feedback:T)
     ; T = unresolved_diagnostic(_, _)  -> assertz(feedback:T)
     ; true
     ),
     feedback:read_assert_terms(S)
  ).


% -----------------------------------------------------------------------------
%  Recording discoveries
% -----------------------------------------------------------------------------

%! feedback:record_discovery(+Target, +Provider, +Kind, +Evidence) is det.
%
% Records a concrete discovered dependency: Target (Repo://Entry) needs
% Provider (a Category/Name atom) at Kind (bdepend), justified by the
% structured Evidence term. Deduplicated on (Target, Provider, Kind): a
% one-time runtime discovery becomes durable knowledge, appended to the
% persisted file so future runs plan it proactively.

feedback:record_discovery(Target, Provider, Kind, Evidence) :-
  ( feedback:discovered_dep(Target, Provider, Kind, _)
  -> true
  ;  assertz(feedback:discovered_dep(Target, Provider, Kind, Evidence)),
     feedback:discovered_file(Path),
     feedback:append_term(Path, discovered_dep(Target, Provider, Kind, Evidence))
  ),
  ( feedback:session_discovery(Target, Provider, Kind, _)
  -> true
  ;  assertz(feedback:session_discovery(Target, Provider, Kind, Evidence))
  ).


%! feedback:record_unresolved(+Symbol, +Evidence) is det.
%
% Records that a missing-provider signature (Symbol = symbol(Kind, Name))
% fired but could not be mapped to a concrete package. Deduplicated on
% Symbol. This is a maintainer backlog for extending the resolver seed
% table / index; it never affects proving (no discovery is minted).

feedback:record_unresolved(Symbol, Evidence) :-
  ( feedback:unresolved_diagnostic(Symbol, _)
  -> true
  ;  assertz(feedback:unresolved_diagnostic(Symbol, Evidence)),
     feedback:unresolved_file(Path),
     feedback:append_term(Path, unresolved_diagnostic(Symbol, Evidence))
  ).


%! feedback:append_term(+Path, +Term) is det.
%
% Appends a single quoted, period-terminated term to Path. Errors are
% contained so a persistence failure never breaks the build.

feedback:append_term(Path, Term) :-
  catch(
    setup_call_cleanup(
      open(Path, append, S),
      ( write_term(S, Term, [quoted(true)]),
        format(S, '.~n', []) ),
      close(S)),
    _, true).


% -----------------------------------------------------------------------------
%  Queries
% -----------------------------------------------------------------------------

%! feedback:discovery_count(-Count) is det.
%
% Number of distinct discovered dependencies currently known. Used by the
% builder's replan loop to detect whether a build pass grew the store.

feedback:discovery_count(Count) :-
  aggregate_all(count, feedback:discovered_dep(_, _, _, _), Count).


%! feedback:session_discoveries(-List) is det.
%
% Discoveries recorded in the current session, as
% discovery(Target, Provider, Kind, Evidence) terms. Used by the printer
% to draft bug reports only for dependencies worked around this run (not
% the entire persisted history).

feedback:session_discoveries(List) :-
  findall(discovery(T, P, K, E), feedback:session_discovery(T, P, K, E), List).


%! feedback:clear_session is det.
%
% Drops the current session's discovery marks. Called at the start of a
% build invocation so bug-report drafts are scoped to that build (and do
% not bleed across requests in a long-running daemon/server session). The
% durable discovered_dep/4 store is untouched.

feedback:clear_session :-
  retractall(feedback:session_discovery(_, _, _, _)).


%! feedback:discovered_bdepend_dep(+Repo, +Id, -Dep) is nondet.
%
% Yields, for the tree entry Repo://Id, each discovered build-dependency
% as a parsed package_dependency/8 term shaped exactly like an md5-cache
% BDEPEND atom, so query.pl can union it into the install dependency
% model. Only bdepend-kind discoveries are surfaced (build-time providers).

feedback:discovered_bdepend_dep(Repo, Id, Dep) :-
  feedback:discovered_dep(Repo://Id, Provider, bdepend, _Evidence),
  feedback:provider_dep(Provider, Dep).


%! feedback:provider_dep(+Provider, -Dep) is semidet.
%
% Converts a Category/Name provider atom into an unversioned
% package_dependency/8 term matching the eapi grammar's parse of a bare
% `cat/name` BDEPEND atom (phase install, no blocker/operator/version/
% slot/use).

feedback:provider_dep(Provider, package_dependency(install, no, C, N, none, version_none, [], [])) :-
  atom(Provider),
  atomic_list_concat([C, N], '/', Provider),
  C \== '',
  N \== ''.
