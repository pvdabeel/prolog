/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> REMOVAL
Depclean removal plan rendering.

Renders the depclean output: proposed removals, proved uninstall order
(unmerging rule set, consumers before dependencies), and VDB ELF linkage
risk report. The underlying computation lives in
Source/Domain/Gentoo/depclean.pl; this module is display-only.
*/

:- module(removal, []).

% =============================================================================
%  REMOVAL declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Proposed removals
% -----------------------------------------------------------------------------

%! removal:print_removals(+RequiredInstalled)
%
% Compute the set of removable packages (installed minus required) and
% print the removal list, uninstall order, and linkage risk report.

removal:print_removals(RequiredInstalled) :-
  knowledgebase:vdb_repository(VdbRepo),
  findall(VdbRepo://E,
          query:search([installed(true)], VdbRepo://E),
          Installed0),
  sort(Installed0, Installed),
  subtract(Installed, RequiredInstalled, Removable),
  nl,
  message:header('Depclean (proposed removals)'),
  nl,
  ( Removable == [] ->
      writeln('  (none)')
  ; forall(member(RE, Removable),
           ( removal:pkg_label(RE, L),
             format('  ~w~n', [L])
           ))
  ),
  removal:print_uninstall_order(Removable),
  removal:print_linkage_risks(Installed, Removable),
  nl.


% -----------------------------------------------------------------------------
%  Uninstall order
% -----------------------------------------------------------------------------

%! removal:print_uninstall_order(+Removable)
%
% Compute and print the proved uninstall order for the removable
% packages (consumers before their dependencies). Retained claims —
% cyclic dependencies where a claimant could not be ordered before the
% package it depends on — are reported individually.

removal:print_uninstall_order([]) :- !.
removal:print_uninstall_order(Removable) :-
  depclean:uninstall_order(Removable, Order, Retained),
  nl,
  message:header('Depclean (uninstall order)'),
  nl,
  ( Retained == [] ->
      true
  ; message:warning('dependency cycle in removable set; order is best-effort:'),
    forall(member(retained(C, R), Retained),
           removal:print_retained_claim(C, R))
  ),
  removal:print_pkg_list_numbered(1, Order),
  nl.


%! removal:print_retained_claim(+Claimant, +Dependency)
%
% Print one retained claim: the claimant still depends on the package
% at the moment the package is unmerged.

removal:print_retained_claim(C, R) :-
  removal:pkg_label(C, CL),
  removal:pkg_label(R, RL),
  format('    ~w still depends on ~w at its unmerge point~n', [CL, RL]).


%! removal:pkg_label(+RepoEntry, -Label)
%
% Human-readable category/name-version label for a VDB Repo://Entry,
% falling back to the raw term.

removal:pkg_label(R://E, Label) :-
  query:search([category(C),name(N),version(V)], R://E),
  !,
  removal:version_text(V, VT),
  format(atom(Label), '~w/~w-~w', [C, N, VT]).
removal:pkg_label(Term, Label) :-
  format(atom(Label), '~w', [Term]).


%! removal:version_text(+Version, -Text)
%
% Printable text of a version/7 compound (its Full field); passthrough
% for anything else.

removal:version_text(version(_,_,_,_,_,_,Full), Full) :- !.
removal:version_text(V, V).


%! removal:print_pkg_list_numbered(+Index, +Packages)
%
% Print a numbered list of VDB Repo://Entry terms with category/name-version.

removal:print_pkg_list_numbered(_, []) :- !.
removal:print_pkg_list_numbered(I, [RE|Es]) :-
  removal:pkg_label(RE, L),
  format('  ~d. ~w~n', [I, L]),
  I2 is I + 1,
  removal:print_pkg_list_numbered(I2, Es).


% -----------------------------------------------------------------------------
%  Linkage risk report
% -----------------------------------------------------------------------------

%! removal:print_linkage_risks(+Installed, +Removable)
%
% Best-effort approximation of Portage preserved-libs behavior. Uses VDB
% metadata (NEEDED.ELF.2 / PROVIDES.ELF.2) to identify kept packages
% whose ELF dependencies would lose all providers if the removable set
% is unmerged.

removal:print_linkage_risks(_Installed, Removable) :-
  Removable == [],
  !.
removal:print_linkage_risks(Installed, Removable) :-
  sort(Removable, RemovableSorted),
  list_to_ord_set(RemovableSorted, RemovableSet),
  subtract(Installed, RemovableSorted, Kept),
  list_to_ord_set(Kept, KeptSet),
  depclean:build_provides_map(Installed, ProvidesMap),
  depclean:collect_broken_needed(Kept, KeptSet, RemovableSet, ProvidesMap, BrokenPairs),
  nl,
  message:header('Depclean (linkage risks, VDB ELF metadata)'),
  nl,
  ( BrokenPairs == [] ->
      writeln('  (none detected)')
  ; forall(member(broken(Consumer, NeededTok, RemovedProviders), BrokenPairs),
           removal:print_broken_needed(Consumer, NeededTok, RemovedProviders))
  ),
  nl.


%! removal:print_broken_needed(+Consumer, +Token, +RemovedProviders)
%
% Print a single broken-linkage warning: the consumer package, the ELF
% token it needs, and the removable packages that were its only providers.

removal:print_broken_needed(Consumer, Tok, RemovedProviders) :-
  removal:pkg_label(Consumer, CL),
  format('  ~w needs ~w~n', [CL, Tok]),
  forall(member(RP, RemovedProviders),
         ( removal:pkg_label(RP, PL),
           format('    - would lose provider: ~w~n', [PL])
         )).
