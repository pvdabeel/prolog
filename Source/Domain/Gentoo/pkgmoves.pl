/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> PKGMOVES
Package move (pkgmove / slotmove) handling from profiles/updates.

Gentoo periodically renames packages and slots through quarter files in
`profiles/updates/` (e.g. `1Q-2025`), each containing directives:

  - `move <old-cat>/<old-pkg> <new-cat>/<new-pkg>`
  - `slotmove <spec> <old-slot> <new-slot>`

Traditional Portage applies these to the VDB, world file and binpkgs after
every sync. portage-ng shares its VDB with traditional Portage (which
already rewrites it), so this module instead provides a read-time
translation layer: old category/name pairs encountered in the world file,
named sets, CLI targets or VDB-derived lookups are translated to their
current name before being matched against the tree.

Move chains are resolved transitively in chronological order (a package
moved a->b in 2020 and b->c in 2023 yields a->c and b->c), matching the
behaviour of Portage's _global_updates.
*/

:- module(pkgmoves, []).

% =============================================================================
%  PKGMOVES declarations
% =============================================================================

:- dynamic pkgmoves:pmove_/4.       % pmove_(OldC, OldN, NewC, NewN)
:- dynamic pkgmoves:pslotmove_/5.   % pslotmove_(C, N, Spec, OldSlot, NewSlot)
:- dynamic pkgmoves:loaded_/0.


% -----------------------------------------------------------------------------
%  Loading
% -----------------------------------------------------------------------------

%! pkgmoves:ensure_loaded is det.
%
% Lazily loads the package move facts on first use. Loading failures
% (e.g. no registered repositories in client mode) leave the store empty
% but mark it loaded so callers do not retry on every lookup.

pkgmoves:ensure_loaded :-
  pkgmoves:loaded_, !.
pkgmoves:ensure_loaded :-
  catch(pkgmoves:load, _, true),
  ( pkgmoves:loaded_ -> true ; assertz(pkgmoves:loaded_) ).


%! pkgmoves:load is det.
%
% (Re)populates the move store from `<location>/profiles/updates/` of every
% registered eapi-type repository. Quarter files are applied in
% chronological order so later moves supersede and chain onto earlier ones.

pkgmoves:load :-
  retractall(pkgmoves:pmove_(_, _, _, _)),
  retractall(pkgmoves:pslotmove_(_, _, _, _, _)),
  retractall(pkgmoves:loaded_),
  forall(pkgmoves:eapi_repository_location(Location),
         pkgmoves:load_updates_dir(Location)),
  assertz(pkgmoves:loaded_).


%! pkgmoves:eapi_repository_location(-Location) is nondet.
%
% Yields the on-disk location of every registered eapi-type repository
% (portage tree and overlays), skipping VDB-typed repositories.

pkgmoves:eapi_repository_location(Location) :-
  cache:repository(Repo),
  \+ knowledgebase:is_vdb_repository(Repo),
  catch(Repo:get_type('eapi'), _, fail),
  catch(Repo:get_location(Location), _, fail).


%! pkgmoves:load_updates_dir(+Location) is det.
%
% Parses every quarter file under `<Location>/profiles/updates`, in
% chronological order. Missing directories are silently skipped.

pkgmoves:load_updates_dir(Location) :-
  os:compose_path([Location, 'profiles', 'updates'], Dir),
  ( exists_directory(Dir) ->
      directory_files(Dir, Files0),
      include(pkgmoves:quarter_file_key_, Files0, Files1),
      map_list_to_pairs(pkgmoves:quarter_key_, Files1, Keyed),
      keysort(Keyed, Sorted),
      forall(member(_-File, Sorted),
             ( os:compose_path(Dir, File, Path),
               pkgmoves:load_updates_file(Path)
             ))
  ; true
  ).


%! pkgmoves:quarter_file_key_(+Name) is semidet.
%
% True for file names of the form `<Q>Q-<Year>` (e.g. `1Q-2025`).

pkgmoves:quarter_file_key_(Name) :-
  pkgmoves:quarter_key_(Name, _).


%! pkgmoves:quarter_key_(+Name, -Key) is semidet.
%
% Maps `1Q-2025` to the chronologically sortable key `2025-1`.

pkgmoves:quarter_key_(Name, Year-Quarter) :-
  atom_codes(Name, Codes),
  phrase(pkgmoves:quarter_name_(Quarter, Year), Codes).

pkgmoves:quarter_name_(Quarter, Year) -->
  [QC], { code_type(QC, digit(Quarter)) },
  "Q-",
  [Y1, Y2, Y3, Y4],
  { maplist([C]>>code_type(C, digit), [Y1, Y2, Y3, Y4]),
    number_codes(Year, [Y1, Y2, Y3, Y4]) }.


%! pkgmoves:load_updates_file(+Path) is det.
%
% Parses a single quarter file, applying each move / slotmove line in
% order. Malformed lines are skipped.

pkgmoves:load_updates_file(Path) :-
  read_file_to_string(Path, S, []),
  split_string(S, "\n", "\r\t ", Lines),
  forall(member(Line, Lines),
         ignore(pkgmoves:apply_line(Line))).


%! pkgmoves:apply_line(+Line) is semidet.
%
% Applies one updates directive. `move` lines update the transitive
% mapping; `slotmove` lines are recorded as-is.

pkgmoves:apply_line(Line) :-
  split_string(Line, " \t", " \t", Tokens0),
  exclude(==(""), Tokens0, Tokens),
  pkgmoves:apply_tokens(Tokens).

pkgmoves:apply_tokens(["move", OldS, NewS]) :-
  !,
  pkgmoves:split_cn(OldS, OldC, OldN),
  pkgmoves:split_cn(NewS, NewC, NewN),
  pkgmoves:record_move(OldC, OldN, NewC, NewN).
pkgmoves:apply_tokens(["slotmove", SpecS, OldSlotS, NewSlotS]) :-
  !,
  pkgmoves:spec_cn(SpecS, C, N, Spec),
  atom_string(OldSlot, OldSlotS),
  atom_string(NewSlot, NewSlotS),
  assertz(pkgmoves:pslotmove_(C, N, Spec, OldSlot, NewSlot)).


%! pkgmoves:record_move(+OldC, +OldN, +NewC, +NewN) is det.
%
% Records a move, keeping the store transitively resolved: existing
% mappings that point at the old name are redirected to the new one, and
% a later move of the same old name supersedes an earlier one.

pkgmoves:record_move(OldC, OldN, NewC, NewN) :-
  forall(pkgmoves:pmove_(XC, XN, OldC, OldN),
         ( retract(pkgmoves:pmove_(XC, XN, OldC, OldN)),
           ( XC-XN == NewC-NewN
           -> true
           ;  assertz(pkgmoves:pmove_(XC, XN, NewC, NewN))
           )
         )),
  retractall(pkgmoves:pmove_(OldC, OldN, _, _)),
  ( OldC-OldN == NewC-NewN
  -> true
  ;  assertz(pkgmoves:pmove_(OldC, OldN, NewC, NewN))
  ).


%! pkgmoves:split_cn(+String, -Category, -Name) is semidet.
%
% Splits `cat/name` into two atoms.

pkgmoves:split_cn(String, Category, Name) :-
  split_string(String, "/", "", [CS, NS]),
  CS \== "", NS \== "",
  atom_string(Category, CS),
  atom_string(Name, NS).


%! pkgmoves:spec_cn(+SpecS, -Category, -Name, -Spec) is semidet.
%
% Extracts category/name from a slotmove spec, which may carry a version
% operator prefix and version suffix (e.g. `~app-editors/emacs-27.0.91`).
% Spec preserves the original string as an atom for later matching.

pkgmoves:spec_cn(SpecS, Category, Name, Spec) :-
  atom_string(Spec, SpecS),
  pkgmoves:strip_operator(SpecS, Versioned, Rest),
  pkgmoves:split_cn(Rest, Category, Name0),
  ( Versioned == true
  -> pkgmoves:strip_version(Name0, Name)
  ;  Name = Name0
  ).


%! pkgmoves:strip_operator(+String, -Versioned, -Rest) is det.
%
% Strips a leading version operator (`=`, `<`, `>`, `<=`, `>=`, `~`) from
% a dependency spec. Versioned is true when an operator was present
% (implying the spec carries a version suffix).

pkgmoves:strip_operator(String, Versioned, Rest) :-
  ( member(Op, ["<=", ">=", "=", "<", ">", "~"]),
    string_concat(Op, Rest0, String)
  -> Versioned = true, Rest = Rest0
  ;  Versioned = false, Rest = String
  ).


%! pkgmoves:strip_version(+NameVer, -Name) is det.
%
% Strips the trailing `-<version>` from a versioned package name atom,
% using the PMS convention that a version starts with a digit after `-`.

pkgmoves:strip_version(NameVer, Name) :-
  atomic_list_concat(Parts, '-', NameVer),
  Parts = [_, _|_],
  append(NameParts, [Last|_], Parts),
  NameParts \== [],
  atom_codes(Last, [FC|_]),
  code_type(FC, digit),
  !,
  atomic_list_concat(NameParts, '-', Name).
pkgmoves:strip_version(Name, Name).


% -----------------------------------------------------------------------------
%  Lookup and translation
% -----------------------------------------------------------------------------

%! pkgmoves:moved(+OldC, +OldN, -NewC, -NewN) is semidet.
%
% True when the given category/name has been renamed; returns the final
% (transitively resolved) name.

pkgmoves:moved(OldC, OldN, NewC, NewN) :-
  pkgmoves:ensure_loaded,
  pkgmoves:pmove_(OldC, OldN, NewC, NewN).


%! pkgmoves:slotmoved(?C, ?N, ?Spec, ?OldSlot, ?NewSlot) is nondet.
%
% Recorded slotmove directives (spec preserved verbatim; callers that
% need version-sensitive matching must interpret Spec themselves).

pkgmoves:slotmoved(C, N, Spec, OldSlot, NewSlot) :-
  pkgmoves:ensure_loaded,
  pkgmoves:pslotmove_(C, N, Spec, OldSlot, NewSlot).


%! pkgmoves:translate_atom(+Atom0, -Atom) is semidet.
%
% Translates a target atom referencing a moved package to its current
% name, preserving any version operator prefix and version / slot /
% USE-bracket suffix (e.g. `=old-cat/old-name-1.2:3[flag]` becomes
% `=new-cat/new-name-1.2:3[flag]`). Accepts an atom or a string (world
% file entries are read as strings); the translated result is always an
% atom. Fails when the input does not reference a moved package
% (callers keep the original in that case).

pkgmoves:translate_atom(Atom0, Atom) :-
  pkgmoves:ensure_loaded,
  ( atom(Atom0)   -> Atom1 = Atom0
  ; string(Atom0) -> atom_string(Atom1, Atom0)
  ),
  atomic_list_concat([Prefix, Rest0], '/', Atom1),
  pkgmoves:prefix_operator_category(Prefix, Operator, OldC),
  pkgmoves:name_suffix(OldC, Rest0, OldN, Suffix),
  \+ pkgmoves:cn_in_tree(OldC, OldN),
  pkgmoves:pmove_(OldC, OldN, NewC, NewN),
  atomic_list_concat([Operator, NewC, '/', NewN, Suffix], Atom).


%! pkgmoves:cn_in_tree(+C, +N) is semidet.
%
% True when the category/name still has entries in a non-VDB repository.
% A recorded move is only applied when the old name is really gone from
% the tree, so a package recreated under a previously-moved name is left
% alone.

pkgmoves:cn_in_tree(C, N) :-
  cache:package(Repo, C, N),
  \+ knowledgebase:is_vdb_repository(Repo),
  !.


%! pkgmoves:prefix_operator_category(+Prefix, -Operator, -Category) is det.
%
% Splits the part before `/` into a version operator (possibly empty)
% and the category atom.

pkgmoves:prefix_operator_category(Prefix, Operator, Category) :-
  ( member(Operator, ['<=', '>=', '=', '<', '>', '~']),
    atom_concat(Operator, Category, Prefix)
  -> true
  ;  Operator = '', Category = Prefix
  ).


%! pkgmoves:name_suffix(+Category, +Rest, -Name, -Suffix) is semidet.
%
% Splits `name[-version][:slot][[use]]` into the bare package name and
% the remaining suffix, by matching recorded old names for Category.
% Longest-name match wins so `foo-bar` is preferred over `foo` when both
% moved.

pkgmoves:name_suffix(Category, Rest, Name, Suffix) :-
  findall(L-N,
          ( pkgmoves:pmove_(Category, N, _, _),
            atom_length(N, L)
          ),
          Candidates0),
  sort(0, @>=, Candidates0, Candidates),
  member(_-Name, Candidates),
  atom_concat(Name, Suffix, Rest),
  pkgmoves:valid_suffix(Suffix),
  !.


%! pkgmoves:valid_suffix(+Suffix) is semidet.
%
% True when Suffix is empty or starts a version (`-<digit>`), slot (`:`)
% or USE bracket (`[`) section, guarding against partial name matches
% (`foo` matching `foo-bar`).

pkgmoves:valid_suffix('') :- !.
pkgmoves:valid_suffix(Suffix) :-
  sub_atom(Suffix, 0, 1, _, First),
  ( First == ':' -> true
  ; First == '[' -> true
  ; First == '-'
    -> sub_atom(Suffix, 1, 1, _, Second),
       char_type(Second, digit)
  ).


% -----------------------------------------------------------------------------
%  Sync-time application
% -----------------------------------------------------------------------------

%! pkgmoves:apply_world_moves is det.
%
% Rewrites world-file entries that reference moved packages, mirroring
% the world-file half of Portage's post-sync _global_updates. Reloads
% the move store first (the tree just changed), translates each entry,
% and saves the world file only when something changed. VDB rewriting
% is intentionally left to traditional Portage, which shares the VDB.

pkgmoves:apply_world_moves :-
  pkgmoves:load,
  findall(Old-New,
          ( world::entry(Old),
            pkgmoves:translate_atom(Old, New)
          ),
          Changes),
  ( Changes == []
  -> true
  ;  forall(member(Old-New, Changes),
            ( message:inform(['Package move (world): ', Old, ' -> ', New]),
              world:unregister(Old),
              world:register(New)
            )),
     world:save
  ).
