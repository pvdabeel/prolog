/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> GHCABI
GHC ABI-hash repair exception, the native haskell-updater (portage-ng#93).

Gentoo encodes a Haskell package's identity in ghc-pkg's ABI hash (the
`-<hash>` suffix, e.g. bifunctors-5.6.3-9AmA3NO9963FDwV9BBcxcZ), NOT in the
ebuild sub-slot. When a dev-haskell library is rebuilt (new hash) its
installed reverse-dependencies keep referencing the old hash, and the next
Haskell consumer's pkg_setup/configure aborts with haskell-cabal.eclass's
check:

  installed package semigroupoids-5.3.7 is broken due to missing package
  bifunctors-5.6.3-9AmA3NO9963FDwV9BBcxcZ
  * Detected broken packages: semigroupoids-5.3.7 semialign-1.3
  * //==-- Please, run 'haskell-updater' to fix broken packages --==//

The sub-slot rebuild pass (portage-ng#89) cannot see this: there is no
sub-slot delta to observe (the hash lives only in ghc-pkg's registry). The
real fix is at the ebuild/eclass level (encoding the ABI identity where
the resolver can see it); until then, traditional emerge fails the same
way and defers to a manual haskell-updater run. We do better: gated by
config:ghc_abi_repair/1, the failure is recovered in-transaction by
parsing the broken package list from the log segment of the failed phase,
rebuilding each broken package from source at its installed version
(re-registering it with a hash consistent with the just-rebuilt
dependency), and re-running the failed phase. Cascading breakage (a repair
rebuild bumping another consumer's hash) is handled by one additional
bounded round. Each package is rebuilt at most once per session
(ghcabi:repair_applied_/2), so the mechanism can never loop; repairs are
serialized under a dedicated mutex so parallel workers hitting the same
breakage don't rebuild twice.

Registered with the generic fixup registry (Source/Domain/Gentoo/
Exceptions/fixup.pl); the builder and printer have no knowledge of this
mechanism.
*/

:- module(ghcabi, []).

% =============================================================================
%  GHCABI declarations
% =============================================================================

:- multifile fixup:mechanism/1.
:- multifile fixup:mechanism_note/3.
:- multifile fixup:phase_retry_hook/10.

:- dynamic ghcabi:repair_applied_/2.

:- mutex_create(ghc_abi_repair).

fixup:mechanism(ghcabi).


% -----------------------------------------------------------------------------
%  Gate
% -----------------------------------------------------------------------------

%! ghcabi:repair_enabled is semidet.

ghcabi:repair_enabled :-
  ( catch(config:ghc_abi_repair(V), _, fail), ground(V)
  -> V == true
  ;  true
  ).


%! ghcabi:retry_phase(+Phase) is semidet.
%
% Phases in which haskell-cabal.eclass runs its broken-package check
% (ghc-package.eclass `checks`): pkg_setup and the configure path. A
% compile failure can also surface it via a late `ghc-pkg` invocation.

ghcabi:retry_phase(setup).
ghcabi:retry_phase(configure).
ghcabi:retry_phase(compile).


% -----------------------------------------------------------------------------
%  Failure signature
% -----------------------------------------------------------------------------

%! ghcabi:phase_error(+LogPath, +SizeBefore, -Tokens) is semidet.
%
% True when the log content appended after byte offset SizeBefore (i.e. by
% the phase that just failed) carries the haskell-updater broken-package
% signature. Tokens is the parsed, deduplicated list of broken `name-ver`
% atoms from the "Detected broken packages:" line(s). Only the trailing
% 256KB of the segment is examined (the check output is emitted at the
% point of the die).

ghcabi:phase_error(LogPath, SizeBefore, Tokens) :-
  catch(
    ( exists_file(LogPath),
      size_file(LogPath, Size),
      Size > SizeBefore,
      Start is max(SizeBefore, Size - 262144),
      Len is Size - Start,
      setup_call_cleanup(
        open(LogPath, read, S, [type(binary)]),
        ( seek(S, Start, bof, _),
          read_string(S, Len, Tail)
        ),
        close(S))
    ),
    _, fail),
  sub_string(Tail, _, _, _, "haskell-updater"),
  ghcabi:broken_tokens(Tail, Tokens),
  Tokens \== [],
  !.


%! ghcabi:broken_tokens(+Tail, -Tokens) is det.
%
% Parses every "Detected broken packages:" line in Tail and returns the
% union of the `name-ver` tokens listed after the colon. The eclass
% output is colorized (einfo/eerror ANSI prefixes) but the token area
% itself is plain text; tokens are validated by shape (must contain a
% `-<digit>` version separator) so stray decorations never map to a
% package.

ghcabi:broken_tokens(Tail, Tokens) :-
  split_string(Tail, "\n", "\r", Lines),
  findall(Token,
    ( member(Line, Lines),
      sub_string(Line, B, _, _, "Detected broken packages:"),
      Skip is B + 25,
      sub_string(Line, Skip, _, 0, Rest),
      split_string(Rest, " \t", " \t", Parts),
      member(P0, Parts),
      ghcabi:strip_ansi(P0, P),
      P \== "",
      atom_string(Token, P),
      ghcabi:pv_token(Token)
    ),
    Tokens0),
  sort(Tokens0, Tokens).


%! ghcabi:strip_ansi(+Str, -Clean) is det.
%
% Truncates Str at the first ESC character, dropping any trailing ANSI
% color/reset sequence the eclass output may have attached to a token.

ghcabi:strip_ansi(Str, Clean) :-
  ( sub_string(Str, B, 1, _, "\u001b")
  -> sub_string(Str, 0, B, _, Clean)
  ;  Clean = Str
  ).


%! ghcabi:pv_token(+Token) is semidet.
%
% Token looks like a `name-version` atom: it contains a `-` immediately
% followed by a digit (the Gentoo name/version separator).

ghcabi:pv_token(Token) :-
  sub_atom(Token, B, 1, _, '-'),
  D is B + 1,
  sub_atom(Token, D, 1, _, Ch),
  char_type(Ch, digit),
  !.


% -----------------------------------------------------------------------------
%  Broken-token to entry mapping
% -----------------------------------------------------------------------------

%! ghcabi:token_entry(+Token, -TreeRepo, -Entry) is semidet.
%
% Maps a broken `name-ver` token to the installed VDB entry
% (Category/Name-Version) and a tree repository that still carries the
% same version, so the package can be rebuilt as-is. Fails when the
% installed version is gone from the tree (then the repair skips it and
% the phase failure keeps its original semantics).

ghcabi:token_entry(Token, TreeRepo, Entry) :-
  cache:ordered_entry(pkg, Entry, C, _N, _V),
  atomic_list_concat([C, Token], '/', Entry),
  cache:ordered_entry(TreeRepo, Entry, _, _, _),
  TreeRepo \== pkg,
  !.


% -----------------------------------------------------------------------------
%  Repair rebuild
% -----------------------------------------------------------------------------

%! ghcabi:rebuild_use(+TreeRepo, +Entry, -UseString) is det.
%
% USE string for a same-version repair rebuild: the flags recorded in the
% VDB at install time, restricted to the tree ebuild's IUSE (positive for
% recorded flags, negative for the rest), so the rebuild reproduces the
% installed configuration. Falls back to the KB-derived base state when
% the VDB USE file is unavailable.

ghcabi:rebuild_use(TreeRepo, Entry, UseString) :-
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


%! ghcabi:rebuild(+TreeRepo, +Entry, -ExitCode) is det.
%
% Rebuilds Entry from source (never the binpkg fast path -- a stale
% binpkg ABI is exactly what may be broken) and merges it. The build
% portion runs unlocked; only the merge takes the portage_pkg_merge
% mutex, so parallel workers' merges are not stalled for the duration of
% the compile. Output goes to the package's own build log with a repair
% marker.

ghcabi:rebuild(TreeRepo, Entry, ExitCode) :-
  ( ebuild_exec:ebuild_path(TreeRepo, Entry, EbuildPath),
    ebuild_exec:ensure_log_dir,
    ebuild_exec:build_log_path(Entry, LogPath),
    ghcabi:rebuild_use(TreeRepo, Entry, UseString)
  -> catch(
       ( open(LogPath, append, S),
         format(S, '~n=== ghc-abi repair rebuild (portage-ng#93) ===~n', []),
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


%! ghcabi:repair_tokens(+Tokens, -RepairedCount) is det.
%
% Rebuilds every broken token not already repaired this session, under
% the ghc_abi_repair mutex. Tokens whose rebuild fails on the first pass
% get one more attempt after the others (ghc-pkg lists broken packages in
% registry order, not dependency order, so an inter-broken dependency can
% make the first pass fail). RepairedCount is the number of successful
% rebuilds in this call; already-repaired tokens count as progress
% (another worker fixed them after our phase failed), unmappable tokens
% do not.

ghcabi:repair_tokens(Tokens, RepairedCount) :-
  with_mutex(ghc_abi_repair,
    ghcabi:repair_tokens_locked(Tokens, RepairedCount)).

ghcabi:repair_tokens_locked(Tokens, RepairedCount) :-
  partition([T]>>(ghcabi:repair_applied_(T, _)), Tokens, Done, Todo),
  ghcabi:repair_pass(Todo, Failed1),
  ghcabi:repair_pass(Failed1, Failed2),
  length(Todo, NTodo),
  length(Failed2, NFailed),
  length(Done, NDone),
  RepairedCount is NTodo - NFailed + NDone.

ghcabi:repair_pass([], []).
ghcabi:repair_pass([Token|Rest], Failed) :-
  ( ghcabi:repair_applied_(Token, _)
  -> Failed = MoreFailed
  ;  ghcabi:token_entry(Token, TreeRepo, Entry)
  -> ghcabi:rebuild(TreeRepo, Entry, EC),
     ( EC =:= 0
     -> assertz(ghcabi:repair_applied_(Token, Entry)),
        fixup:record(ghcabi, Entry, broken_abi(Token)),
        Failed = MoreFailed
     ;  Failed = [Token|MoreFailed]
     )
  ;  Failed = [Token|MoreFailed]
  ),
  ghcabi:repair_pass(Rest, MoreFailed).


%! ghcabi:log_retry(+LogPath, +Phase, +ExitCode, +Tokens) is det.
%
% Writes a marker line to the failing consumer's build log so the repair
% is visible when inspecting the build.

ghcabi:log_retry(LogPath, Phase, ExitCode, Tokens) :-
  catch(
    ( open(LogPath, append, S),
      format(S, '~n=== ~w failed (exit ~w) with broken GHC packages ~w; rebuilding and retrying (portage-ng#93 ghc-abi repair) ===~n',
             [Phase, ExitCode, Tokens]),
      close(S)
    ), _, true).


% -----------------------------------------------------------------------------
%  Per-phase retry hook
% -----------------------------------------------------------------------------

%! fixup:phase_retry_hook(+ghcabi, +EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.
%
% On a non-zero exit of a setup/configure/compile phase whose log segment
% matches the haskell-updater broken-package signature, rebuilds the
% broken packages and re-runs the failed phase; a second bounded round
% handles cascading breakage exposed by the repair itself. Otherwise
% passes ExitCode0 through unchanged.

fixup:phase_retry_hook(ghcabi, EbuildPath, _Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode) :-
  ( ghcabi:repair_enabled,
    ghcabi:retry_phase(Phase)
  -> ghcabi:retry_loop(2, EbuildPath, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode)
  ;  ExitCode = ExitCode0
  ).


%! ghcabi:retry_loop(+RoundsLeft, +EbuildPath, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.

:- meta_predicate ghcabi:retry_loop(+, +, +, +, +, 2, +, +, -).

ghcabi:retry_loop(0, _, _, _, _, _, _, ExitCode, ExitCode) :- !.

ghcabi:retry_loop(RoundsLeft, EbuildPath, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode) :-
  ( ghcabi:phase_error(LogPath, SizeBefore, Tokens),
    ghcabi:repair_tokens(Tokens, Repaired),
    Repaired > 0
  -> ghcabi:log_retry(LogPath, Phase, ExitCode0, Tokens),
     ebuild_exec:log_file_size(LogPath, SizeBefore1),
     ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, Pid),
     ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode1),
     ( ExitCode1 =:= 0
     -> ExitCode = 0
     ;  RoundsLeft1 is RoundsLeft - 1,
        ghcabi:retry_loop(RoundsLeft1, EbuildPath, Phase, LogPath, UseString, Callback, SizeBefore1, ExitCode1, ExitCode)
     )
  ;  ExitCode = ExitCode0
  ).


% -----------------------------------------------------------------------------
%  Build summary note
% -----------------------------------------------------------------------------

%! fixup:mechanism_note(+ghcabi, +Count, -Lines) is semidet.

fixup:mechanism_note(ghcabi, N, [Line1, Line2]) :-
  ( N =:= 1 -> Word = 'package' ; Word = 'packages' ),
  format(atom(Line1), 'GHC ABI repair: ~d broken ~w rebuilt in-transaction after a', [N, Word]),
  Line2 = '                dependency ABI-hash change (portage-ng#93, haskell-updater equivalent):'.
