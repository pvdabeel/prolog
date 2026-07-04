/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> FIXUP
Generic registry and dispatcher for domain exception fixups.

A "fixup" is a build-time workaround for a problem that should really be
fixed at the ebuild / metadata level (an upstream or packaging exception):
e.g. a missing blocker atom surfacing as a merge-time file collision
(portage-ng#90), or a stale GHC ABI hash surfacing as a haskell-updater
die (portage-ng#93). Mechanisms living under Source/Domain/Gentoo/Exceptions/
register themselves here; the builder and printer stay generic:

  - the builder's per-phase retry chain ends in fixup:maybe_phase_retry/9,
    which dispatches to every registered fixup:phase_retry_hook/10;
  - applied fixups are recorded via fixup:record/3 and reported by the
    build printer through fixup:mechanism_note/3, so adding a new
    exception mechanism never touches the builder or printer again.
*/

:- module(fixup, []).

% =============================================================================
%  FIXUP declarations
% =============================================================================

:- dynamic fixup:applied_/3.

%! fixup:mechanism(?Mechanism) is nondet.
%
% Multifile: each exception mechanism declares one fact identifying
% itself (e.g. `fixup:mechanism(ghcabi).`). Declaration (load) order is
% dispatch order for the per-phase retry hooks and display order for the
% build summary.

:- multifile fixup:mechanism/1.


%! fixup:mechanism_note(+Mechanism, +Count, -Lines) is semidet.
%
% Multifile: the summary header lines for a mechanism, given the number
% of applied fixups. Lines is a list of atoms, printed by the build
% printer above the per-item list.

:- multifile fixup:mechanism_note/3.


%! fixup:phase_retry_hook(+Mechanism, +EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCodeIn, -ExitCodeOut) is det.
%
% Multifile: per-phase repair-and-retry hook for a mechanism. Called by
% fixup:maybe_phase_retry/9 on a non-zero phase exit; must be det and
% pass ExitCodeIn through unchanged when the mechanism does not apply.

:- multifile fixup:phase_retry_hook/10.


% -----------------------------------------------------------------------------
%  Applied-fixup record
% -----------------------------------------------------------------------------

%! fixup:record(+Mechanism, +Id, +Detail) is det.
%
% Records that a fixup was applied (deduplicated on Mechanism + Id), so
% it is visible in the build summary in addition to the per-build-log
% markers a mechanism writes. Id is typically the package entry the
% fixup acted on.

fixup:record(Mechanism, Id, Detail) :-
  ( fixup:applied_(Mechanism, Id, _)
  -> true
  ;  assertz(fixup:applied_(Mechanism, Id, Detail))
  ).


%! fixup:applied(?Mechanism, ?Id, ?Detail) is nondet.
%
% Query the applied-fixup record.

fixup:applied(Mechanism, Id, Detail) :-
  fixup:applied_(Mechanism, Id, Detail).


%! fixup:clear is det.
%
% Clears the applied-fixup record.

fixup:clear :-
  retractall(fixup:applied_(_, _, _)).


% -----------------------------------------------------------------------------
%  Same-version repair rebuild
% -----------------------------------------------------------------------------

%! fixup:installed_tree_entry(+Entry, -TreeRepo) is semidet.
%
% True when the installed VDB entry (Category/Name-Version) is still
% carried at the same version by a tree repository, so it can be rebuilt
% as-is. Fails when the installed version is gone from the tree (a
% repair mechanism then skips the entry and the phase failure keeps its
% original semantics).

fixup:installed_tree_entry(Entry, TreeRepo) :-
  cache:ordered_entry(TreeRepo, Entry, _, _, _),
  TreeRepo \== pkg,
  !.


%! fixup:repair_rebuild_use(+TreeRepo, +Entry, -UseString) is det.
%
% USE string for a same-version repair rebuild: the flags recorded in the
% VDB at install time, restricted to the tree ebuild's IUSE (positive for
% recorded flags, negative for the rest), so the rebuild reproduces the
% installed configuration. Falls back to the KB-derived base state when
% the VDB USE file is unavailable.

fixup:repair_rebuild_use(TreeRepo, Entry, UseString) :-
  findall(Flag, kb:query(iuse(Flag, _:_), TreeRepo://Entry), Flags0),
  sort(Flags0, Flags),
  ( Flags \== [],
    catch(vdb:read_metadata_file(Entry, 'USE', UseAtom), _, fail)
  -> atomic_list_concat(Installed0, ' ', UseAtom),
     sort(Installed0, Installed),
     findall(Token,
       ( member(F, Flags),
         ( memberchk(F, Installed) -> Token = F ; atom_concat('-', F, Token) )
       ),
       Tokens),
     atomic_list_concat(Tokens, ' ', UseString)
  ;  ebuild_exec:collect_use_string(TreeRepo, Entry, [], UseString)
  ).


%! fixup:repair_rebuild(+TreeRepo, +Entry, +Marker, -ExitCode) is det.
%
% Rebuilds Entry from source (never the binpkg fast path -- a stale
% binary package is exactly what a repair must not reinstall) and merges
% it. The build portion runs unlocked; only the merge takes the
% portage_pkg_merge mutex, so parallel workers' merges are not stalled
% for the duration of the compile. Output goes to the package's own
% build log, prefixed with the Marker line identifying the repairing
% mechanism.

fixup:repair_rebuild(TreeRepo, Entry, Marker, ExitCode) :-
  ( ebuild_exec:ebuild_path(TreeRepo, Entry, EbuildPath),
    ebuild_exec:ensure_log_dir,
    ebuild_exec:build_log_path(Entry, LogPath),
    fixup:repair_rebuild_use(TreeRepo, Entry, UseString)
  -> catch(
       ( open(LogPath, append, S),
         format(S, '~n=== ~w ===~n', [Marker]),
         close(S)
       ), _, true),
     ebuild_exec:run_phases_unlocked(EbuildPath,
       [clean, setup, unpack, prepare, configure, compile, install], UseString, BuildEC),
     ( BuildEC =:= 0
     -> ebuild_exec:with_portage_pkg_merge_lock(merge,
          ebuild_exec:run_phase_logged_unlocked(EbuildPath, merge, LogPath, UseString, ExitCode))
     ;  ExitCode = BuildEC
     )
  ;  ExitCode = -1
  ).


% -----------------------------------------------------------------------------
%  Per-phase retry dispatch
% -----------------------------------------------------------------------------

%! fixup:maybe_phase_retry(+EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.
%
% Terminal element of the builder's per-phase retry chain: offers a
% failed phase to every registered exception mechanism in declaration
% order, threading the exit code (a mechanism that repairs and re-runs
% the phase returns the re-run's exit code; all others pass through).
% A hook that throws is contained: the pre-hook exit code is kept.

:- meta_predicate fixup:maybe_phase_retry(+, +, +, +, +, 2, +, +, -).

fixup:maybe_phase_retry(_, _, _, _, _, _, _, 0, 0) :- !.

fixup:maybe_phase_retry(EbuildPath, Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode) :-
  findall(M, fixup:mechanism(M), Mechanisms),
  fixup:fold_phase_retry(Mechanisms, EbuildPath, Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode).


%! fixup:fold_phase_retry(+Mechanisms, +EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.

:- meta_predicate fixup:fold_phase_retry(+, +, +, +, +, +, 2, +, +, -).

fixup:fold_phase_retry([], _, _, _, _, _, _, _, ExitCode, ExitCode).

fixup:fold_phase_retry([_|_], _, _, _, _, _, _, _, 0, 0) :- !.

fixup:fold_phase_retry([M|Rest], EbuildPath, Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode) :-
  catch(fixup:phase_retry_hook(M, EbuildPath, Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode1),
        _,
        ExitCode1 = ExitCode0),
  fixup:fold_phase_retry(Rest, EbuildPath, Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode1, ExitCode).
