/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> REMOVAL
Depclean removal plan rendering.

Renders the depclean output: proposed removals, topologically sorted
uninstall order, and VDB ELF linkage risk report. The underlying
computation lives in Source/Domain/Gentoo/depclean.pl; this module is
display-only.
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
  findall(pkg://E,
          query:search([installed(true)], pkg://E),
          Installed0),
  sort(Installed0, Installed),
  subtract(Installed, RequiredInstalled, Removable),
  nl,
  message:header('Depclean (proposed removals)'),
  nl,
  ( Removable == [] ->
      writeln('  (none)')
  ; forall(member(pkg://E, Removable),
           ( query:search([category(C),name(N),version(V)], pkg://E),
             format('  ~w/~w-~w~n', [C, N, V])
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
% Compute and print a topologically sorted uninstall order for the
% removable packages. Warns when cycles are detected.

removal:print_uninstall_order([]) :- !.
removal:print_uninstall_order(Removable) :-
  depclean:uninstall_order(Removable, Order, Cyclic),
  nl,
  message:header('Depclean (uninstall order)'),
  nl,
  ( Cyclic == true ->
      message:warning('cycle detected in uninstall graph; order is best-effort')
  ; true
  ),
  removal:print_pkg_list_numbered(1, Order),
  nl.


%! removal:print_pkg_list_numbered(+Index, +Packages)
%
% Print a numbered list of pkg://Entry terms with category/name-version.

removal:print_pkg_list_numbered(_, []) :- !.
removal:print_pkg_list_numbered(I, [pkg://E|Es]) :-
  ( query:search([category(C),name(N),version(V)], pkg://E) ->
      format('  ~d. ~w/~w-~w~n', [I, C, N, V])
  ; format('  ~d. ~w~n', [I, pkg://E])
  ),
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

removal:print_broken_needed(pkg://E, Tok, RemovedProviders) :-
  ( query:search([category(C),name(N),version(V)], pkg://E) ->
      format('  ~w/~w-~w needs ~w~n', [C, N, V, Tok])
  ; format('  ~w needs ~w~n', [pkg://E, Tok])
  ),
  forall(member(pkg://P, RemovedProviders),
         ( ( query:search([category(CP),name(NP),version(VP)], pkg://P) ->
               format('    - would lose provider: ~w/~w-~w~n', [CP, NP, VP])
           ; format('    - would lose provider: ~w~n', [pkg://P])
           )
         )).
