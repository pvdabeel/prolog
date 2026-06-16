/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> SETS
Computed package sets derived from the installed-package database (VDB) and
the portage tree.

These mirror the dynamically computed sets that traditional emerge exposes
through its built-in set classes.  Unlike the file-backed named sets handled
by `preference:set/2`, the members of these sets are computed on demand by
comparing the installed packages against the highest visible ebuild in the
tree:

  - installed        all installed packages, as `cat/name:slot` atoms
                     (mirrors portage.sets.dbapi.EverythingSet)
  - live-rebuild     installed packages whose tree ebuild carries the
                     `live` PROPERTY (mirrors a PROPERTIES=live VariableSet)
  - changed-subslot  installed packages where the highest visible ebuild has
                     a different subslot than the installed version
                     (mirrors portage.sets.dbapi.SubslotChangedSet)
  - downgrade        installed packages where the highest visible ebuild
                     version is lower than the installed version
                     (mirrors portage.sets.dbapi.DowngradeSet)
  - unavailable      installed packages for which no visible ebuild exists
                     for the same category/name:slot
                     (mirrors portage.sets.dbapi.UnavailableSet)
  - rebuilt-binaries binary packages whose BUILD_TIME differs from the
                     installed package of the exact same version, as
                     `=cat/name-version` atoms
                     (mirrors portage.sets.dbapi.RebuiltBinaries)
  - unavailable-binaries
                     installed packages for which no binary package of the
                     exact same version is available, as `cat/name:slot`
                     atoms (mirrors portage.sets.dbapi.UnavailableBinaries)

The expansion entry point is `sets:expand/2`; `eapi:substitute_sets/2`
recognises the names enumerated by `sets:is_computed_set/1` and replaces an
`@<name>` reference with the resolved target atoms.
*/

:- module(sets, []).

% =============================================================================
%  SETS declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Computed set registry
% -----------------------------------------------------------------------------

%! sets:is_computed_set(?Name) is nondet.
%
% True for each computed set name recognised by sets:expand/2.

sets:is_computed_set(installed).
sets:is_computed_set('live-rebuild').
sets:is_computed_set('changed-subslot').
sets:is_computed_set(downgrade).
sets:is_computed_set(unavailable).
sets:is_computed_set('rebuilt-binaries').
sets:is_computed_set('unavailable-binaries').


%! sets:expand(+Name, -Targets) is det.
%
% Resolves a computed set name to a sorted list of `cat/name:slot` target
% atoms.  Fails (or yields []) for unknown names.

sets:expand(installed, Targets) :-
  !,
  sets:installed_set(Targets).

sets:expand('live-rebuild', Targets) :-
  !,
  sets:live_rebuild_set(Targets).

sets:expand('changed-subslot', Targets) :-
  !,
  sets:changed_subslot_set(Targets).

sets:expand(downgrade, Targets) :-
  !,
  sets:downgrade_set(Targets).

sets:expand(unavailable, Targets) :-
  !,
  sets:unavailable_set(Targets).

sets:expand('rebuilt-binaries', Targets) :-
  !,
  sets:rebuilt_binaries_set(Targets).

sets:expand('unavailable-binaries', Targets) :-
  !,
  sets:unavailable_binaries_set(Targets).

sets:expand(_, []).


% -----------------------------------------------------------------------------
%  installed set
% -----------------------------------------------------------------------------

%! sets:installed_set(-Targets) is det.
%
% All installed packages as sorted `cat/name:slot` atoms.

sets:installed_set(Targets) :-
  findall(Atom,
          ( knowledgebase:vdb_repository(VdbRepo),
            query:search([category(C),name(N)], VdbRepo://Entry),
            sets:slot_atom(VdbRepo://Entry, C, N, Atom)
          ),
          Targets0),
  sort(Targets0, Targets).


% -----------------------------------------------------------------------------
%  live-rebuild set
% -----------------------------------------------------------------------------

%! sets:live_rebuild_set(-Targets) is det.
%
% Installed packages whose corresponding tree ebuild carries the `live`
% PROPERTY.  PROPERTIES is not stored for VDB entries, so the installed
% category/name/version is mapped back to its tree ebuild first.

sets:live_rebuild_set(Targets) :-
  findall(Atom,
          ( knowledgebase:vdb_repository(VdbRepo),
            query:search([category(C),name(N),version(V)], VdbRepo://Entry),
            V \== version_none,
            sets:installed_tree_entry(C, N, V, TreeEntry),
            ebuild:is_live(TreeEntry),
            sets:slot_atom(VdbRepo://Entry, C, N, Atom)
          ),
          Targets0),
  sort(Targets0, Targets).


% -----------------------------------------------------------------------------
%  changed-subslot set
% -----------------------------------------------------------------------------

%! sets:changed_subslot_set(-Targets) is det.
%
% Installed packages where the highest visible ebuild in the same slot has a
% different subslot than the installed version.

sets:changed_subslot_set(Targets) :-
  findall(Atom,
          ( knowledgebase:vdb_repository(VdbRepo),
            query:search([category(C),name(N)], VdbRepo://Entry),
            slotmeta:entry_slot_default(VdbRepo, Entry, Slot),
            sets:entry_subslot(VdbRepo://Entry, InstalledSub),
            sets:highest_visible_in_slot(C, N, Slot, BestEntry, _),
            sets:entry_subslot(BestEntry, TreeSub),
            InstalledSub \== TreeSub,
            sets:cn_slot_atom(C, N, Slot, Atom)
          ),
          Targets0),
  sort(Targets0, Targets).


% -----------------------------------------------------------------------------
%  downgrade set
% -----------------------------------------------------------------------------

%! sets:downgrade_set(-Targets) is det.
%
% Installed packages where the highest visible ebuild version in the same slot
% is lower than the installed version.

sets:downgrade_set(Targets) :-
  findall(Atom,
          ( knowledgebase:vdb_repository(VdbRepo),
            query:search([category(C),name(N),version(InstalledVer)], VdbRepo://Entry),
            InstalledVer \== version_none,
            slotmeta:entry_slot_default(VdbRepo, Entry, Slot),
            sets:highest_visible_in_slot(C, N, Slot, _, BestVer),
            eapi:version_compare(<, BestVer, InstalledVer),
            sets:cn_slot_atom(C, N, Slot, Atom)
          ),
          Targets0),
  sort(Targets0, Targets).


% -----------------------------------------------------------------------------
%  unavailable set
% -----------------------------------------------------------------------------

%! sets:unavailable_set(-Targets) is det.
%
% Installed packages for which no visible ebuild exists in the tree for the
% same category/name:slot.

sets:unavailable_set(Targets) :-
  findall(Atom,
          ( knowledgebase:vdb_repository(VdbRepo),
            query:search([category(C),name(N)], VdbRepo://Entry),
            slotmeta:entry_slot_default(VdbRepo, Entry, Slot),
            \+ sets:highest_visible_in_slot(C, N, Slot, _, _),
            sets:cn_slot_atom(C, N, Slot, Atom)
          ),
          Targets0),
  sort(Targets0, Targets).


% -----------------------------------------------------------------------------
%  rebuilt-binaries set
% -----------------------------------------------------------------------------

%! sets:rebuilt_binaries_set(-Targets) is det.
%
% Binary packages whose BUILD_TIME differs from the currently installed
% package of the exact same category/name/version, as `=cat/name-version`
% atoms.  Yields [] when no binpkg repository is registered.  BUILD_TIME is
% not stored in the VDB cache, so it is read from the on-disk VDB metadata
% file.

sets:rebuilt_binaries_set(Targets) :-
  ( sets:binpkg_available ->
      knowledgebase:vdb_repository(VdbRepo),
      findall(Atom,
              ( cache:ordered_entry(binpkg, BinEntry, C, N, V),
                cache:entry_metadata(binpkg, BinEntry, build_time, BinBuildTime),
                cache:ordered_entry(VdbRepo, InstalledEntry, C, N, V),
                vdb:read_metadata_file(InstalledEntry, 'BUILD_TIME', InstalledBuildTime),
                \+ sets:same_build_time(BinBuildTime, InstalledBuildTime),
                atom_concat('=', InstalledEntry, Atom)
              ),
              Targets0),
      sort(Targets0, Targets)
  ; Targets = []
  ).


%! sets:binpkg_available is semidet.
%
% True when binpkg consumption is enabled and a binpkg repository is
% registered.  Refreshes the on-disk index first so externally produced
% binpkgs are visible.

sets:binpkg_available :-
  config:use_binpkg(true),
  ( current_predicate(binpkg_exec:ensure_index_fresh/0)
    -> catch(binpkg_exec:ensure_index_fresh, _, true)
    ;  true
  ),
  cache:repository(binpkg).


%! sets:same_build_time(+A, +B) is semidet.
%
% True when two BUILD_TIME values are equal.  Compares numerically when both
% parse as integers, falling back to atom equality otherwise.

sets:same_build_time(A, B) :-
  ( sets:build_time_number(A, NA),
    sets:build_time_number(B, NB)
    -> NA =:= NB
    ;  A == B
  ).


%! sets:build_time_number(+Value, -Number) is semidet.
%
% Coerces a BUILD_TIME atom/number to an integer.  Fails for non-numeric
% values.

sets:build_time_number(Value, Number) :-
  ( number(Value)
    -> Number = Value
    ;  atom_number(Value, Number)
  ).


% -----------------------------------------------------------------------------
%  unavailable-binaries set
% -----------------------------------------------------------------------------

%! sets:unavailable_binaries_set(-Targets) is det.
%
% Installed packages for which no binary package of the exact same
% category/name/version is available, as sorted `cat/name:slot` atoms.
% When no binpkg repository is registered, every installed package is
% considered to have no available binary.

sets:unavailable_binaries_set(Targets) :-
  ignore(sets:binpkg_available),
  knowledgebase:vdb_repository(VdbRepo),
  findall(Atom,
          ( cache:ordered_entry(VdbRepo, Entry, C, N, V),
            \+ cache:ordered_entry(binpkg, _, C, N, V),
            sets:slot_atom(VdbRepo://Entry, C, N, Atom)
          ),
          Targets0),
  sort(Targets0, Targets).


% -----------------------------------------------------------------------------
%  Shared helpers
% -----------------------------------------------------------------------------

%! sets:slot_atom(+Repo://+Entry, +Category, +Name, -Atom) is det.
%
% Builds a `cat/name:slot` atom from an entry's canonical slot.

sets:slot_atom(Repo://Entry, C, N, Atom) :-
  slotmeta:entry_slot_default(Repo, Entry, Slot),
  sets:cn_slot_atom(C, N, Slot, Atom).


%! sets:cn_slot_atom(+Category, +Name, +Slot, -Atom) is det.
%
% Builds a `cat/name:slot` atom from explicit components.

sets:cn_slot_atom(C, N, Slot, Atom) :-
  atomic_list_concat([C, '/', N, ':', Slot], Atom).


%! sets:installed_tree_entry(+Category, +Name, +Version, -TreeEntry) is semidet.
%
% Maps an installed category/name/version to the matching non-VDB tree entry.

sets:installed_tree_entry(C, N, V, TreeRepo://TreeEntry) :-
  query:search([select(repository,notequal,pkg),category(C),name(N),version(V)],
               TreeRepo://TreeEntry),
  \+ knowledgebase:is_vdb_repository(TreeRepo),
  !.


%! sets:entry_subslot(+Repo://+Entry, -Subslot) is det.
%
% Returns the canonical subslot of an entry, defaulting to the main slot when
% no explicit subslot is recorded (matching PMS subslot defaulting).

sets:entry_subslot(Repo://Entry, Subslot) :-
  ( query:search(subslot(Sub0), Repo://Entry)
    -> slotmeta:canon_slot(Sub0, Subslot)
    ;  slotmeta:entry_slot_default(Repo, Entry, Subslot)
  ).


%! sets:entry_visible(+Repo://+Entry) is semidet.
%
% True when an entry is visible: keyword-accepted, not masked, and not
% license-masked.

sets:entry_visible(Repo://Entry) :-
  acceptance:entry_has_accepted_keyword(Repo://Entry),
  \+ query:search(masked(true), Repo://Entry),
  \+ acceptance:license_masked(Repo://Entry).


%! sets:highest_visible_in_slot(+Category, +Name, +Slot, -BestEntry, -BestVersion) is semidet.
%
% Finds the highest-versioned visible tree ebuild for Category/Name in the
% given canonical Slot.  Fails when no visible candidate exists.

sets:highest_visible_in_slot(C, N, Slot, BestEntry, BestVersion) :-
  findall(Version-(Repo://Entry),
          ( query:search([select(repository,notequal,pkg),category(C),name(N),version(Version)],
                         Repo://Entry),
            \+ knowledgebase:is_vdb_repository(Repo),
            slotmeta:entry_slot_default(Repo, Entry, Slot),
            sets:entry_visible(Repo://Entry)
          ),
          Pairs),
  Pairs \== [],
  sets:max_version_pair(Pairs, BestVersion-BestEntry).


%! sets:max_version_pair(+Pairs, -Max) is det.
%
% Selects the `Version-Entry` pair with the highest version from a non-empty
% list, using the EAPI version comparator.

sets:max_version_pair([First|Rest], Max) :-
  foldl(sets:keep_higher_version, Rest, First, Max).


%! sets:keep_higher_version(+Candidate, +Acc, -Best) is det.
%
% Folding step that retains the higher-versioned of two `Version-Entry` pairs.

sets:keep_higher_version(Version-Entry, AccVersion-AccEntry, Best) :-
  ( eapi:version_compare(>, Version, AccVersion)
    -> Best = Version-Entry
    ;  Best = AccVersion-AccEntry
  ).
