/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> EBUILD_EXEC
Interface to Portage's `ebuild` CLI for executing build phases.

Shells out to the `ebuild` command to run individual phases (setup,
unpack, compile, install, merge, etc.) on a per-package basis. The
builder resolves dependencies and controls ordering; this module handles
the per-package phase execution via Portage's infrastructure.

The `ebuild` command is part of sys-apps/portage and invokes ebuild.sh
with the full Portage helper stack (sandbox, econf, emake, doins, etc.).
This avoids reimplementing ~7,400 lines of bash + ~6,700 lines of Python
merge/VDB code.
*/

:- module(ebuild_exec, []).

:- dynamic ebuild_exec:phase_bytes/3.
:- dynamic ebuild_exec:phase_seconds/3.
:- dynamic ebuild_exec:phase_stats_loaded/0.
:- dynamic ebuild_exec:resuming/0.

:- mutex_create(portage_pkg_merge).


% =============================================================================
%  Action-to-phase mapping
% =============================================================================

%! ebuild_exec:action_phases(+Action, +Ctx, -Phases) is semidet.
%
% Maps a portage-ng plan action to the sequence of ebuild CLI phases.
% The `ebuild` command accepts multiple phase arguments and runs them
% in order. `merge` is a composite phase that internally handles
% pkg_preinst, merging files to the live filesystem, unmerging the
% old version (for update/downgrade/reinstall), and pkg_postinst.
% The phase sequence is therefore identical for all build actions.
% When --buildpkg is active, the `package` phase is inserted after
% `install` to create a binary package before merging.

ebuild_exec:action_phases(install,   _Ctx, Phases) :- ebuild_exec:build_phases(Phases).
ebuild_exec:action_phases(run,       _Ctx, []).
ebuild_exec:action_phases(reinstall, _Ctx, Phases) :- ebuild_exec:build_phases(Phases).
ebuild_exec:action_phases(update,    _Ctx, Phases) :- ebuild_exec:build_phases(Phases).
ebuild_exec:action_phases(downgrade, _Ctx, Phases) :- ebuild_exec:build_phases(Phases).
ebuild_exec:action_phases(uninstall, _Ctx, [unmerge]).


%! ebuild_exec:build_phases(-Phases) is det.
%
% Returns the build phase sequence. When --buildpkgonly is active,
% the merge phase is omitted (binary package only). When --buildpkg
% is active, the `package` phase is inserted after `install` before
% merging. When --resume is active, the `clean` phase is omitted so
% ebuild can pick up from the preserved work directory.
%
% The `test` phase follows Portage semantics: it is included only
% when FEATURES contains positive `test` (see config:features_test_enabled).
% Otherwise it is omitted entirely from the phase list -- matching
% emerge's behaviour, where `src_test` is skipped unless explicitly
% enabled via FEATURES="test".

ebuild_exec:build_phases(Phases) :-
  ( ebuild_exec:resuming -> Clean = [] ; Clean = [clean] ),
  ( config:features_test_enabled -> Test = [test] ; Test = [] ),
  ( (preference:flag(buildpkgonly) ; preference:flag(buildpkg))
  -> Pkg = [package]
  ;  Pkg = []
  ),
  ( preference:flag(buildpkgonly)
  -> Merge = []
  ;  Merge = [merge]
  ),
  append([Clean, [setup, unpack, prepare, configure, compile], Test, [install], Pkg, Merge], Phases).


% =============================================================================
%  Ebuild path resolution
% =============================================================================

%! ebuild_exec:ebuild_path(+Repo, +Entry, -EbuildPath) is semidet.
%
% Resolves a Repo://Entry to the on-disk .ebuild file path by querying
% the repository's location and entry metadata.

ebuild_exec:ebuild_path(Repo, Entry, EbuildPath) :-
  Repo:get_ebuild_file(Entry, EbuildPath).


% =============================================================================
%  DEFINED_PHASES querying
% =============================================================================

%! ebuild_exec:defined_phases(+Repo, +Entry, -Phases) is det.
%
% Retrieves the DEFINED_PHASES for an ebuild from the knowledge base.
% Returns a list of atoms (e.g. [compile, configure, install, setup]).
% Falls back to an empty list if metadata is unavailable.

ebuild_exec:defined_phases(Repo, Entry, Phases) :-
  ( query:search(defined_phases(P), Repo://Entry)
  -> ( is_list(P) -> Phases = P ; Phases = [P] )
  ;  Phases = []
  ).


%! ebuild_exec:display_phases(+Action, +Repo, +Entry, +Ctx, -DisplayPhases) is det.
%
% Returns the full phase lifecycle for the action. Every phase runs
% regardless of DEFINED_PHASES (Portage uses defaults for undefined ones).

ebuild_exec:display_phases(Action, _Repo, _Entry, Ctx, DisplayPhases) :-
  ebuild_exec:action_phases(Action, Ctx, DisplayPhases).


% =============================================================================
%  Build log path
% =============================================================================

%! ebuild_exec:build_log_path(+Entry, -LogPath) is det.
%
% Computes the log file path for a build action. Slashes in the
% entry name are replaced with `--` to produce a safe filename.

ebuild_exec:build_log_path(Entry, LogPath) :-
  config:build_log_dir(LogDir),
  format(atom(RawName), '~w', [Entry]),
  atomic_list_concat(Parts, '/', RawName),
  atomic_list_concat(Parts, '--', SafeName),
  atomic_list_concat([LogDir, '/', SafeName, '.log'], LogPath).


%! ebuild_exec:ensure_log_dir is det.

ebuild_exec:ensure_log_dir :-
  config:build_log_dir(LogDir),
  ( exists_directory(LogDir) -> true ; make_directory_path(LogDir) ).


% =============================================================================
%  Phase log stats (persisted across builds for progress estimates)
% =============================================================================

%! ebuild_exec:phase_stats_file(-Path) is det.

ebuild_exec:phase_stats_file(Path) :-
  working_directory(Cwd, Cwd),
  directory_file_path(Cwd, 'Knowledge/phase_stats.pl', Path).


%! ebuild_exec:load_phase_stats is det.
%
% Loads historical phase log byte counts from disk (once per session).

ebuild_exec:load_phase_stats :-
  ebuild_exec:phase_stats_loaded, !.

ebuild_exec:load_phase_stats :-
  with_mutex(phase_stats_io,
    ( ebuild_exec:phase_stats_loaded -> true
    ;  assertz(ebuild_exec:phase_stats_loaded),
       ebuild_exec:phase_stats_file(Path),
       ( exists_file(Path)
       -> setup_call_cleanup(
            open(Path, read, S),
            ebuild_exec:read_phase_stats(S),
            close(S))
       ;  true
       )
    )).

ebuild_exec:read_phase_stats(S) :-
  read_term(S, Term, []),
  ( Term == end_of_file -> true
  ;  ( Term = phase_bytes(Entry, Phase, Bytes)
     -> assertz(ebuild_exec:phase_bytes(Entry, Phase, Bytes))
     ;  Term = phase_seconds(Entry, Phase, Secs)
     -> assertz(ebuild_exec:phase_seconds(Entry, Phase, Secs))
     ;  true
     ),
     ebuild_exec:read_phase_stats(S)
  ).


%! ebuild_exec:save_phase_stats is det.
%
% Writes all phase_bytes/3 and phase_seconds/3 facts to disk.

ebuild_exec:save_phase_stats :-
  with_mutex(phase_stats_io,
    ( ebuild_exec:phase_stats_file(Path),
      setup_call_cleanup(
        open(Path, write, S),
        ( forall(
            ebuild_exec:phase_bytes(Entry, Phase, Bytes),
            format(S, 'phase_bytes(~q, ~q, ~d).~n', [Entry, Phase, Bytes])
          ),
          forall(
            ebuild_exec:phase_seconds(Entry, Phase, Secs),
            format(S, 'phase_seconds(~q, ~q, ~f).~n', [Entry, Phase, Secs])
          )
        ),
        close(S))
    )).


%! ebuild_exec:record_phase_stats(+Entry, +Phase, +Bytes, +Seconds) is det.
%
% Record (or update) both log byte count and wall-clock seconds
% for a completed phase.

ebuild_exec:record_phase_stats(Entry, Phase, Bytes, Seconds) :-
  retractall(ebuild_exec:phase_bytes(Entry, Phase, _)),
  retractall(ebuild_exec:phase_seconds(Entry, Phase, _)),
  assertz(ebuild_exec:phase_bytes(Entry, Phase, Bytes)),
  assertz(ebuild_exec:phase_seconds(Entry, Phase, Seconds)).


%! ebuild_exec:expected_phase_stats(+Entry, +Phase, -ExpBytes, -ExpSeconds) is semidet.
%
% Look up historical byte count and wall-clock seconds for a phase.
% Succeeds only when at least one signal (bytes or seconds) is available.

ebuild_exec:expected_phase_stats(Entry, Phase, ExpBytes, ExpSeconds) :-
  ( ebuild_exec:phase_bytes(Entry, Phase, ExpBytes) -> true ; ExpBytes = 0 ),
  ( ebuild_exec:phase_seconds(Entry, Phase, ExpSeconds) -> true ; ExpSeconds = 0.0 ),
  ( ExpBytes > 0 ; ExpSeconds > 0.0 ),
  !.


% =============================================================================
%  Log file helpers
% =============================================================================

%! ebuild_exec:log_file_size(+Path, -Size) is det.

ebuild_exec:log_file_size(Path, Size) :-
  ( exists_file(Path) -> size_file(Path, Size) ; Size = 0 ).


% =============================================================================
%  USE flag collection for ebuild environment
% =============================================================================

%! ebuild_exec:collect_use_string(+Repo, +Entry, +Ctx, -UseString) is det.
%
% Builds the USE environment variable value from the knowledge base,
% then applies overrides from the proof context:
%   - build_with_use(Uses) / required_use(Uses): assumed(Flag) or assumed(minus(Flag))
%   - suggestion(use_change, _, Changes): use_change(Flag, enable/disable)
% The context overrides take precedence over KB defaults.

ebuild_exec:collect_use_string(Repo, Entry, Ctx, UseString) :-
  % The base USE state must match the planner's view of the package. Each
  % IUSE flag is resolved through use:effective_use_for_entry/3 -- the same
  % predicate the prover/planner use -- which folds the ebuild default,
  % profile, global and user-config layers in the correct priority order.
  %
  % A prior version collapsed the raw iuse/2 facts with a last-wins dedup.
  % That silently picked the wrong polarity for flags declared with
  % conflicting facts: e.g. x11-libs/wxGTK exposes `X` as
  % [positive:ebuild, negative:default], so last-wins yielded `-X` while
  % the planner resolved `+X`. The builder then emerged wxGTK with `spell`
  % on but `X` off, breaking REQUIRED_USE="spell? ( X )" in the setup phase
  % (issue #22). Resolving via effective_use_for_entry/3 makes the
  % builder's base USE agree with the plan by construction.
  findall(Flag, kb:query(iuse(Flag, _State0:_Reason), Repo://Entry), Flags0),
  sort(Flags0, Flags),
  findall(Flag-State,
    ( member(Flag, Flags),
      ebuild_exec:base_use_state(Repo://Entry, Flag, State)
    ),
    BasePairs),
  % Flags is already deduplicated, but keep the tolerant fold (a flag could
  % in principle resolve more than once) so a duplicate never throws.
  ebuild_exec:pairs_to_assoc_dedup(BasePairs, BaseAssoc),
  ebuild_exec:apply_ctx_use_overrides(Ctx, BaseAssoc, MergedAssoc),
  assoc_to_keys(MergedAssoc, AllFlags),
  findall(Token,
    ( member(F, AllFlags),
      get_assoc(F, MergedAssoc, S),
      ( S == positive -> Token = F
      ; atom_concat('-', F, Token)
      )
    ),
    Tokens0),
  sort(Tokens0, Tokens),
  ( Tokens == []
  -> UseString = ''
  ;  atomic_list_concat(Tokens, ' ', UseString)
  ).


%! ebuild_exec:apply_ctx_use_overrides(+Ctx, +AssocIn, -AssocOut) is det.
%
% Applies USE flag overrides from the proof context on top of the
% KB-derived base flags. Handles three sources, in this order (later
% wins on conflict):
%   1. `build_with_use:use_state(Enable, Disable)` -- the per-package
%      BWU set by the dep walker (e.g. when a parent dep `cairo[X]`
%      forces cairo's X on for this build).
%   2. `required_use:R` -- the prover's REQUIRED_USE proof keys, with
%      their implied flag changes derived via
%      `use:model_required_use_changes/2` (handles both plain
%      `assumed(F)` keys and `assumed(conflict(required_use, ...))`).
%   3. `suggestion(use_change, _, Changes)` -- explicit
%      `use_change(F, enable|disable)` items added by
%      `target:run_tag_suggestions/5` (covers self-flips of the
%      package being installed).
%
% Note: previous versions of this predicate looked for
% `build_with_use(Uses)` and `required_use(Uses)` in *functor* form
% with `Uses` expected to be a list of `assumed(...)` items. The
% codebase actually threads these as KV pairs (`build_with_use:_`
% and `required_use:_`) carrying a `use_state/2` term and a list of
% proof keys, respectively. As a result the BWU/REQUIRED_USE override
% paths were silently dead, and the only USE flips that reached the
% emerge invocation were those tagged via
% `suggestion(use_change, ...)`. That hid bugs like the
% app-admin/hardinfo failure: gtk+ pulled `cairo[X]`, cairo's install
% Context carried `build_with_use:use_state([X],[])`, but cairo
% emerged with `USE="-X"` because this override never applied. Fixed
% by reading the actual KV-pair format and the correct Uses shape.

ebuild_exec:apply_ctx_use_overrides(Ctx, AssocIn, AssocOut) :-
  ( is_list(Ctx) -> CtxList = Ctx ; CtxList = [] ),
  findall(Flag-State,
    ( member(build_with_use:use_state(Enable, Disable), CtxList),
      ( member(Flag, Enable),  State = positive
      ; member(Flag, Disable), State = negative
      )
    ),
    BWUOverrides),
  findall(Flag-State,
    ( member(required_use:R, CtxList),
      is_list(R),
      member(A, R),
      use:model_assumption_to_change(A, use_change(Flag, Dir)),
      ( Dir == enable -> State = positive ; State = negative )
    ),
    RUOverrides),
  findall(Flag-State,
    ( member(suggestion(use_change, _, Changes), CtxList),
      is_list(Changes),
      member(use_change(Flag, Dir), Changes),
      ( Dir == enable -> State = positive ; State = negative )
    ),
    SuggOverrides),
  append([BWUOverrides, RUOverrides, SuggOverrides], AllOverrides),
  foldl(ebuild_exec:apply_use_override, AllOverrides, AssocIn, AssocOut).


%! ebuild_exec:apply_use_override(+FlagState, +AssocIn, -AssocOut) is det.

ebuild_exec:apply_use_override(Flag-State, AssocIn, AssocOut) :-
  put_assoc(Flag, AssocIn, State, AssocOut).


%! ebuild_exec:base_use_state(+RepoEntry, +Flag, -State) is det.
%
% Effective base polarity (positive/negative) for Flag, matching the
% planner via use:effective_use_for_entry/3. Falls back to the raw IUSE
% default polarity (last fact wins) only when the effective lookup cannot
% resolve the flag, so no IUSE flag is ever dropped from the USE string.

ebuild_exec:base_use_state(Repo://Entry, Flag, State) :-
  ( use:effective_use_for_entry(Repo://Entry, Flag, Eff) ->
      State = Eff
  ; findall(S,
      ( kb:query(iuse(Flag, S0:_R), Repo://Entry),
        ( S0 == positive -> S = positive ; S = negative )
      ),
      Ss),
    ( last(Ss, State) -> true ; State = negative )
  ).


%! ebuild_exec:pairs_to_assoc_dedup(+Pairs, -Assoc) is det.
%
% Build an assoc from a list of `Key-Value` pairs, tolerating duplicate
% keys (last occurrence wins). Use this in place of `list_to_assoc/2`
% whenever the input may contain a key more than once: the standard
% `list_to_assoc/2` raises `domain_error(unique_key_pairs, _)` on the
% first duplicate, which previously caused the worker to throw mid-way
% through `install` and the result to be silently dropped by the
% jobserver collector.

ebuild_exec:pairs_to_assoc_dedup(Pairs, Assoc) :-
  empty_assoc(Empty),
  foldl(ebuild_exec:apply_use_override, Pairs, Empty, Assoc).


% =============================================================================
%  Async phase execution (for progress polling)
% =============================================================================

%! ebuild_exec:start_phase_async(+EbuildPath, +Phase, +LogPath, +UseString, -Pid) is det.
%
% Starts a single phase without blocking, appending output to LogPath
% via shell redirection. Positional parameters prevent injection.
% Validates the phase name against a known allowlist before execution.

ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, Pid) :-
  ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, [], Pid).


%! ebuild_exec:start_phase_async(+EbuildPath, +Phase, +LogPath, +UseString, +ExtraEnv, -Pid) is det.
%
% As start_phase_async/5, but extends the child environment with
% ExtraEnv (a list of Name=Value pairs). Used by the serial-make
% retry to inject MAKEOPTS=-j1 (env overrides make.conf in Portage's
% config stack).

ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, ExtraEnv, Pid) :-
  ( sanitize:safe_phase(Phase) -> true
  ; throw(error(permission_error(execute, phase, Phase), context(ebuild_exec:start_phase_async/6, 'Invalid phase name')))
  ),
  config:ebuild_command(EbuildCmd),
  atom_string(Phase, PhaseStr),
  process_create(
    path(sh),
    ['-c', '"$1" --skip-manifest "$2" "$3" >>"$4" 2>&1',
     '_', EbuildCmd, EbuildPath, PhaseStr, LogPath],
    [process(Pid), environment(['USE'=UseString|ExtraEnv])]).


%! ebuild_exec:check_phase_done(+Pid, -ExitCode) is semidet.
%
% Non-blocking check. Succeeds with exit code if the process finished.

ebuild_exec:check_phase_done(Pid, ExitCode) :-
  catch(process_wait(Pid, exit(ExitCode), [timeout(0)]), _, fail).


%! ebuild_exec:poll_phase_progress(+Pid, +Phase, +LogPath, +SizeBefore, +T0, +ExpBytes, +ExpSeconds, :Callback, -ExitCode) is det.
%
% Polls a running phase process, computing progress from the max of
% two signals: log byte growth and elapsed wall-clock time.
% Calls Callback(Phase, progress(Pct)) on each tick until the process
% exits. Percentage is capped at 99 to avoid implying completion.

ebuild_exec:poll_phase_progress(Pid, Phase, LogPath, SizeBefore, T0, ExpBytes, ExpSeconds, Callback, ExitCode) :-
  ( ebuild_exec:check_phase_done(Pid, EC)
  -> ExitCode = EC
  ;  ebuild_exec:log_file_size(LogPath, CurrentSize),
     BytesSoFar is CurrentSize - SizeBefore,
     get_time(Now),
     Elapsed is Now - T0,
     ebuild_exec:dual_progress(BytesSoFar, ExpBytes, Elapsed, ExpSeconds, Pct),
     call(Callback, Phase, progress(Pct)),
     sleep(0.5),
     ebuild_exec:poll_phase_progress(Pid, Phase, LogPath, SizeBefore, T0, ExpBytes, ExpSeconds, Callback, ExitCode)
  ).


%! ebuild_exec:dual_progress(+BytesSoFar, +ExpBytes, +Elapsed, +ExpSeconds, -Pct) is det.
%
% Computes progress percentage from the max of bytes-based and
% time-based estimates. Each is capped at 99 individually before
% taking the max, so the display never implies completion.

ebuild_exec:dual_progress(BytesSoFar, ExpBytes, Elapsed, ExpSeconds, Pct) :-
  ( ExpBytes > 0
  -> BytesPct is min(99, max(0, (BytesSoFar * 100) // ExpBytes))
  ;  BytesPct = 0
  ),
  ( ExpSeconds > 0.0
  -> TimePct is min(99, max(0, round((Elapsed / ExpSeconds) * 100)))
  ;  TimePct = 0
  ),
  Pct is max(BytesPct, TimePct).


% =============================================================================
%  Serial-make retry (parallel-make race recovery, portage-ng#25)
% =============================================================================
%
% Some ebuilds drive Makefiles whose link targets do not declare their
% object dependencies (e.g. net-analyzer/nsat-1.5-r7). With a parallel
% MAKEOPTS (-jN) the link step can race ahead of compilation and fail
% with "cannot find <obj>.o". Traditional emerge hits the same race but
% often masks it through load: its own --jobs saturation makes make's
% -l load guard throttle toward serial execution. portage-ng can reach
% the same package at low system load and run the full -jN, exposing
% the race.
%
% Recovery: when a parallel-make-sensitive phase fails, retry it once
% with MAKEOPTS=-j1 in the environment. Portage gives environment
% variables priority over make.conf, so this forces a serial make for
% the retry only, without touching the user's configuration. Make
% resumes incrementally from the already-built objects, so the retry
% is cheap; deterministic build failures simply fail again and keep
% their original semantics. Gated by config:build_serial_retry/1.

%! ebuild_exec:serial_retry_phase(+Phase) is semidet.
%
% Phases whose failure can plausibly be a parallel-make race. Other
% phases (setup, unpack, configure, merge, ...) never run make in
% parallel, so retrying them serially would only waste time.

ebuild_exec:serial_retry_phase(compile).
ebuild_exec:serial_retry_phase(test).
ebuild_exec:serial_retry_phase(install).


%! ebuild_exec:serial_env(-Env) is det.
%
% Environment overrides forcing a serial build for the retry attempt.
% MAKEOPTS covers emake; eninja (ninja-utils.eclass) falls back to
% MAKEOPTS job parsing when NINJAOPTS is unset, so one variable
% suffices.

ebuild_exec:serial_env(['MAKEOPTS'='-j1']).


%! ebuild_exec:serial_retry_enabled is semidet.

ebuild_exec:serial_retry_enabled :-
  catch(config:build_serial_retry(true), _, fail).


%! ebuild_exec:log_serial_retry(+LogPath, +Phase, +ExitCode) is det.
%
% Writes a marker line to the build log so the retry is visible when
% inspecting failures.

ebuild_exec:log_serial_retry(LogPath, Phase, ExitCode) :-
  catch(
    ( open(LogPath, append, S),
      format(S, '~n=== ~w failed (exit ~w); retrying with MAKEOPTS=-j1 (parallel-make race recovery) ===~n',
             [Phase, ExitCode]),
      close(S)
    ), _, true).


%! ebuild_exec:maybe_serial_retry(+EbuildPath, +Phase, +LogPath, +UseString, :Callback, +ExitCode0, -ExitCode) is det.
%
% Per-phase retry hook for the sequential execution path. On a
% non-zero exit of a serial-retry-eligible phase, re-runs that single
% phase with the serial environment and returns the retry's exit code;
% otherwise passes ExitCode0 through unchanged. The retry is polled
% with the spinner callback (no byte/time estimate is meaningful for
% a resumed build).

:- meta_predicate ebuild_exec:maybe_serial_retry(+, +, +, +, 2, +, -).

ebuild_exec:maybe_serial_retry(_, _, _, _, _, 0, 0) :- !.

ebuild_exec:maybe_serial_retry(EbuildPath, Phase, LogPath, UseString, Callback, ExitCode0, ExitCode) :-
  ( ebuild_exec:serial_retry_enabled,
    ebuild_exec:serial_retry_phase(Phase)
  -> ebuild_exec:log_serial_retry(LogPath, Phase, ExitCode0),
     ebuild_exec:serial_env(Env),
     ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, Env, Pid),
     ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode)
  ;  ExitCode = ExitCode0
  ).


% =============================================================================
%  Transient-failure retry (bash PID-reuse race, portage-ng#76)
% =============================================================================
%
% Portage helpers such as ecompress `wait` on the pid of a process
% substitution to collect find(1)'s exit status. Bash retains only a
% bounded table of reaped background pids, so under a high fork rate
% (e.g. guile's install phase forking thousands of rm/ln invocations,
% tinderbox sessions with bounded kernel.pid_max) the pid gets
% recycled and bash bails with:
%
%   ecompress: line 106: wait: pid 822745 is not a child of this shell
%
% which `die`s the whole phase even though every file operation
% succeeded. This is Gentoo bug #965423, fixed upstream in Nov 2025
% by replacing `wait "$!"` with a PIPESTATUS check -- but any portage
% predating that fix can still hit it. The failure is environmental
% and transient (it depends on system-wide fork rate at the moment
% the helper runs), not a property of the ebuild, so retrying the
% same phase once recovers it. For `install` the retry is cheap:
% dyn_install re-runs src_install from the completed compile.
%
% Detection is signature-based on the log segment written by the
% failed phase (never earlier phases), so deterministic build
% failures never match and keep their original semantics. Gated by
% config:build_transient_retry/1.

%! ebuild_exec:transient_retry_enabled is semidet.

ebuild_exec:transient_retry_enabled :-
  catch(config:build_transient_retry(true), _, fail).


%! ebuild_exec:transient_phase_error(+LogPath, +SizeBefore) is semidet.
%
% True when the log content appended after byte offset SizeBefore
% (i.e. by the phase that just failed) contains the bash PID-reuse
% signature. Only the trailing 64KB of the segment is examined: the
% helpers that emit this signature run at the very end of a phase,
% and this keeps the check cheap even for multi-MB compile logs.

ebuild_exec:transient_phase_error(LogPath, SizeBefore) :-
  catch(
    ( exists_file(LogPath),
      size_file(LogPath, Size),
      Size > SizeBefore,
      Start is max(SizeBefore, Size - 65536),
      Len is Size - Start,
      setup_call_cleanup(
        open(LogPath, read, S, [type(binary)]),
        ( seek(S, Start, bof, _),
          read_string(S, Len, Tail)
        ),
        close(S))
    ),
    _, fail),
  sub_string(Tail, _, _, _, "is not a child of this shell"),
  sub_string(Tail, _, _, _, "wait: pid"),
  !.


%! ebuild_exec:log_transient_retry(+LogPath, +Phase, +ExitCode) is det.
%
% Writes a marker line to the build log so the retry is visible when
% inspecting failures.

ebuild_exec:log_transient_retry(LogPath, Phase, ExitCode) :-
  catch(
    ( open(LogPath, append, S),
      format(S, '~n=== ~w failed (exit ~w) with bash PID-reuse signature; retrying once (transient, portage-ng#76 / Gentoo#965423) ===~n',
             [Phase, ExitCode]),
      close(S)
    ), _, true).


%! ebuild_exec:maybe_transient_retry(+EbuildPath, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.
%
% Per-phase retry hook for the sequential execution path. On a
% non-zero exit whose log segment (bytes after SizeBefore) matches
% the PID-reuse signature, re-runs that single phase once and returns
% the retry's exit code; otherwise passes ExitCode0 through unchanged.
% Runs before maybe_serial_retry/7 in the retry chain: the signature
% match is more specific than the serial heuristic, and the retry
% keeps the original (parallel) environment since the failure has
% nothing to do with make-level parallelism.

:- meta_predicate ebuild_exec:maybe_transient_retry(+, +, +, +, 2, +, +, -).

ebuild_exec:maybe_transient_retry(_, _, _, _, _, _, 0, 0) :- !.

ebuild_exec:maybe_transient_retry(EbuildPath, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode) :-
  ( ebuild_exec:transient_retry_enabled,
    ebuild_exec:transient_phase_error(LogPath, SizeBefore)
  -> ebuild_exec:log_transient_retry(LogPath, Phase, ExitCode0),
     ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, Pid),
     ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode)
  ;  ExitCode = ExitCode0
  ).


% =============================================================================
%  Phase execution
% =============================================================================

%! ebuild_exec:run_phase(+EbuildPath, +Phase, +UseString, -ExitCode) is det.
%
% Invokes the `ebuild` CLI for a single phase. Output is suppressed
% (redirected to null) so it doesn't interfere with the display.

ebuild_exec:run_phase(EbuildPath, Phase, UseString, ExitCode) :-
  config:ebuild_command(EbuildCmd),
  atom_string(Phase, PhaseStr),
  process_create(
    path(EbuildCmd),
    ['--skip-manifest', EbuildPath, PhaseStr],
    [stdout(null), stderr(null), process(Pid), environment(['USE'=UseString])]),
  process_wait(Pid, exit(ExitCode)).


%! ebuild_exec:run_phase_logged(+EbuildPath, +Phase, +LogPath, +UseString, -ExitCode) is det.
%
% Invokes the `ebuild` CLI for a single phase, appending all
% stdout/stderr output to LogPath via shell redirection.
% Validates the phase name before execution.

ebuild_exec:run_phase_logged(EbuildPath, Phase, LogPath, UseString, ExitCode) :-
  ( sanitize:safe_phase(Phase) -> true
  ; throw(error(permission_error(execute, phase, Phase), context(ebuild_exec:run_phase_logged/5, 'Invalid phase name')))
  ),
  ebuild_exec:with_portage_pkg_merge_lock(Phase,
    ebuild_exec:run_phase_logged_unlocked(EbuildPath, Phase, LogPath, UseString, ExitCode)).


%! ebuild_exec:run_phase_logged_unlocked(+EbuildPath, +Phase, +LogPath, +UseString, -ExitCode) is det.

ebuild_exec:run_phase_logged_unlocked(EbuildPath, Phase, LogPath, UseString, ExitCode) :-
  config:ebuild_command(EbuildCmd),
  atom_string(Phase, PhaseStr),
  process_create(
    path(sh),
    ['-c', '"$1" --skip-manifest "$2" "$3" >>"$4" 2>&1',
     '_', EbuildCmd, EbuildPath, PhaseStr, LogPath],
    [process(Pid), environment(['USE'=UseString])]),
  process_wait(Pid, exit(ExitCode)).


%! ebuild_exec:with_portage_pkg_merge_lock(+Phase, :Goal) is det.
%
% Serialize the `merge` phase (and binpkg `qmerge`) across parallel build
% workers so concurrent installs do not race on `.pkg.portage_lockfile`.

:- meta_predicate ebuild_exec:with_portage_pkg_merge_lock(+, 0).

ebuild_exec:with_portage_pkg_merge_lock(merge, Goal) :-
  !,
  with_mutex(portage_pkg_merge, call(Goal)).
ebuild_exec:with_portage_pkg_merge_lock(qmerge, Goal) :-
  !,
  with_mutex(portage_pkg_merge, call(Goal)).
ebuild_exec:with_portage_pkg_merge_lock(_, Goal) :-
  call(Goal).


%! ebuild_exec:log_phase_header(+LogPath, +Phase) is det.
%
% Writes a phase separator line to the log file before each phase runs.

ebuild_exec:log_phase_header(LogPath, Phase) :-
  catch(
    ( open(LogPath, append, S),
      format(S, '~n=== ~w ===~n', [Phase]),
      close(S)
    ), _, true).


%! ebuild_exec:run_phases(+EbuildPath, +Phases, +UseString, -ExitCode) is det.
%
% Invokes the `ebuild` CLI with all phase arguments at once.
% Used for bulk execution without per-phase progress tracking.

ebuild_exec:run_phases(EbuildPath, Phases, UseString, ExitCode) :-
  ( memberchk(merge, Phases)
  -> ebuild_exec:with_portage_pkg_merge_lock(merge,
        ebuild_exec:run_phases_unlocked(EbuildPath, Phases, UseString, ExitCode))
  ;  ebuild_exec:run_phases_unlocked(EbuildPath, Phases, UseString, ExitCode)
  ).


%! ebuild_exec:run_phases_unlocked(+EbuildPath, +Phases, +UseString, -ExitCode) is det.
%
% On failure of a phase list that includes a parallel-make-sensitive
% phase, retries once with MAKEOPTS=-j1 (see "Serial-make retry"
% section above). The retry drops `clean` so ebuild's phase markers
% skip already-completed phases and make resumes incrementally from
% the failure point.

ebuild_exec:run_phases_unlocked(EbuildPath, Phases, UseString, ExitCode) :-
  ebuild_exec:run_phases_once(EbuildPath, Phases, UseString, [], ExitCode0),
  ( ExitCode0 =:= 0
  -> ExitCode = 0
  ;  ( ebuild_exec:serial_retry_enabled,
       member(P, Phases),
       ebuild_exec:serial_retry_phase(P)
     -> subtract(Phases, [clean], RetryPhases),
        ebuild_exec:serial_env(Env),
        ebuild_exec:run_phases_once(EbuildPath, RetryPhases, UseString, Env, ExitCode)
     ;  ExitCode = ExitCode0
     )
  ).


%! ebuild_exec:run_phases_once(+EbuildPath, +Phases, +UseString, +ExtraEnv, -ExitCode) is det.
%
% Single ebuild invocation covering all Phases, with optional extra
% environment bindings (e.g. the serial-retry MAKEOPTS override).

ebuild_exec:run_phases_once(EbuildPath, Phases, UseString, ExtraEnv, ExitCode) :-
  config:ebuild_command(EbuildCmd),
  maplist(atom_string, Phases, PhaseStrs),
  process_create(
    path(EbuildCmd),
    ['--skip-manifest', EbuildPath | PhaseStrs],
    [stdout(null), stderr(null), process(Pid), environment(['USE'=UseString|ExtraEnv])]),
  process_wait(Pid, exit(ExitCode)).


% =============================================================================
%  Live/stub phase splitting
% =============================================================================

%! ebuild_exec:compute_live_prefix(+AllPhases, +LiveConfig, -LivePrefix, -StubTail) is det.
%
% Splits AllPhases into a leading "live prefix" (phases that appear in
% LiveConfig) and a "stub tail" (the rest). Stops at the first phase
% NOT in LiveConfig -- you can't skip a phase in the middle.

ebuild_exec:compute_live_prefix([], _, [], []).

ebuild_exec:compute_live_prefix([Phase|Rest], LiveConfig, LivePrefix, StubTail) :-
  ( memberchk(Phase, LiveConfig)
  -> LivePrefix = [Phase|MoreLive],
     ebuild_exec:compute_live_prefix(Rest, LiveConfig, MoreLive, StubTail)
  ;  LivePrefix = [],
     StubTail = [Phase|Rest]
  ).


% =============================================================================
%  Phase execution with live config
% =============================================================================

%! ebuild_exec:run_phases_with_config(+EbuildPath, +Entry, +AllPhases, +DisplayPhases, +LogPath, +UseString, :PhaseCallback, -Outcome) is det.
%
% Splits AllPhases into a live prefix (phases to actually execute) and
% a stub tail (phases beyond current config). Executes each live phase
% individually, using exit codes for success/failure. Log file size
% is used only for progress estimation, never for phase detection.
% UseString carries the resolved USE flags for the ebuild environment.

:- meta_predicate ebuild_exec:run_phases_with_config(+, +, +, +, +, +, 2, -).

ebuild_exec:run_phases_with_config(EbuildPath, Entry, AllPhases, DisplayPhases, LogPath, UseString, Callback, Outcome) :-
  config:build_live_phases(LiveConfig),
  ebuild_exec:compute_live_prefix(AllPhases, LiveConfig, LivePrefix, StubTail),
  ( LivePrefix \= []
  -> ebuild_exec:run_phases_sequential(EbuildPath, Entry, LivePrefix, DisplayPhases, LogPath, UseString, Callback, LiveOutcome)
  ;  LiveOutcome = done
  ),
  ( LiveOutcome == done
  -> forall(
       (member(P, StubTail), memberchk(P, DisplayPhases)),
       call(Callback, P, stub)
     ),
     Outcome = done
  ;  forall(
       (member(P, StubTail), memberchk(P, DisplayPhases)),
       call(Callback, P, skipped)
     ),
     Outcome = LiveOutcome
  ).


%! ebuild_exec:run_phases_sequential(+EbuildPath, +Entry, +Phases, +DisplayPhases, +LogPath, +UseString, :Callback, -Outcome) is det.
%
% Executes each phase as a separate ebuild invocation. On success,
% moves to the next phase. On failure, marks remaining phases as
% skipped. Uses log file size growth for progress estimation only.
% UseString is passed to each ebuild invocation as the USE env var.

ebuild_exec:run_phases_sequential(_, _, [], _, _, _, _, done).

ebuild_exec:run_phases_sequential(EbuildPath, Entry, [Phase|Rest], DisplayPhases, LogPath, UseString, Callback, Outcome) :-
  ( memberchk(Phase, DisplayPhases)
  -> call(Callback, Phase, active)
  ;  true
  ),
  ebuild_exec:log_phase_header(LogPath, Phase),
  ebuild_exec:log_file_size(LogPath, SizeBefore),
  get_time(T0),
  ebuild_exec:expected_phase_stats(Entry, Phase, ExpBytes, ExpSecs),
  !,
  ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, Pid),
  ebuild_exec:poll_phase_progress(Pid, Phase, LogPath, SizeBefore, T0, ExpBytes, ExpSecs, Callback, ExitCode0),
  ebuild_exec:maybe_transient_retry(EbuildPath, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode1),
  ebuild_exec:maybe_serial_retry(EbuildPath, Phase, LogPath, UseString, Callback, ExitCode1, ExitCode),
  get_time(T1),
  TotalSecs is T1 - T0,
  ebuild_exec:log_file_size(LogPath, SizeAfter),
  TotalBytes is SizeAfter - SizeBefore,
  ebuild_exec:record_phase_stats(Entry, Phase, TotalBytes, TotalSecs),
  ( ExitCode =:= 0
  -> ( memberchk(Phase, DisplayPhases)
     -> call(Callback, Phase, done)
     ;  true
     ),
     ebuild_exec:run_phases_sequential(EbuildPath, Entry, Rest, DisplayPhases, LogPath, UseString, Callback, Outcome)
  ;  ( memberchk(Phase, DisplayPhases)
     -> call(Callback, Phase, failed(ExitCode, LogPath))
     ;  true
     ),
     forall(
       (member(P, Rest), memberchk(P, DisplayPhases)),
       call(Callback, P, skipped)
     ),
     Outcome = failed(ExitCode)
  ).

ebuild_exec:run_phases_sequential(EbuildPath, Entry, [Phase|Rest], DisplayPhases, LogPath, UseString, Callback, Outcome) :-
  ( memberchk(Phase, DisplayPhases)
  -> call(Callback, Phase, active)
  ;  true
  ),
  ebuild_exec:log_phase_header(LogPath, Phase),
  ebuild_exec:log_file_size(LogPath, SizeBefore),
  get_time(T0),
  ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, Pid),
  ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode0),
  ebuild_exec:maybe_transient_retry(EbuildPath, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode1),
  ebuild_exec:maybe_serial_retry(EbuildPath, Phase, LogPath, UseString, Callback, ExitCode1, ExitCode),
  get_time(T1),
  TotalSecs is T1 - T0,
  ebuild_exec:log_file_size(LogPath, SizeAfter),
  TotalBytes is SizeAfter - SizeBefore,
  ebuild_exec:record_phase_stats(Entry, Phase, TotalBytes, TotalSecs),
  ( ExitCode =:= 0
  -> ( memberchk(Phase, DisplayPhases)
     -> call(Callback, Phase, done)
     ;  true
     ),
     ebuild_exec:run_phases_sequential(EbuildPath, Entry, Rest, DisplayPhases, LogPath, UseString, Callback, Outcome)
  ;  ( memberchk(Phase, DisplayPhases)
     -> call(Callback, Phase, failed(ExitCode, LogPath))
     ;  true
     ),
     forall(
       (member(P, Rest), memberchk(P, DisplayPhases)),
       call(Callback, P, skipped)
     ),
     Outcome = failed(ExitCode)
  ).


%! ebuild_exec:poll_phase_spinning(+Pid, +Phase, :Callback, -ExitCode) is det.
%
% Polls a running phase without historical stats. Sends progress(0)
% ticks to keep the spinner alive until the process exits.

ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode) :-
  ( ebuild_exec:check_phase_done(Pid, EC)
  -> ExitCode = EC
  ;  call(Callback, Phase, progress(0)),
     sleep(0.5),
     ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode)
  ).


% =============================================================================
%  Composite action execution
% =============================================================================

%! ebuild_exec:execute(+Action, +Repo, +Entry, +Ctx, -Outcome) is det.
%
% Execute a plan action end-to-end (bulk, no per-phase progress).
% The merge phase handles replacement of old versions internally
% (pkg_preinst, merge files, unmerge old, pkg_postinst), so
% update/downgrade/reinstall use the same phase sequence as install.

% Disabled: explicit unmerge before build is unnecessary and harmful.
% The merge phase already handles old version replacement via the VDB.
% Removing files before building can break builds that depend on the
% old version's files at compile time.
%
% ebuild_exec:execute(Action, Repo, Entry, Ctx, Outcome) :-
%   memberchk(Action, [update, downgrade]),
%   !,
%   ( ebuild_exec:unmerge_old(Repo, Ctx)
%   -> ebuild_exec:execute_phases(Action, Repo, Entry, Ctx, Outcome)
%   ;  Outcome = failed(unmerge_old)
%   ).

ebuild_exec:execute(uninstall, Repo, Entry, Ctx, Outcome) :-
  !,
  ebuild_exec:execute_phases(uninstall, Repo, Entry, Ctx, Outcome).

ebuild_exec:execute(run, _Repo, _Entry, _Ctx, done) :- !.

% Binpkg fast-path: for build-shaped actions, ask binpkg_exec whether a
% USE-compatible binpkg variant exists. If so, short-circuit through
% binpkg_exec:execute/6 (extract gpkg + qmerge) instead of running the
% full source build phase sequence. Falls through to the source path
% silently if no candidate fits (or if config:use_binpkg is false, or
% the binpkg repo is not registered).
ebuild_exec:execute(Action, Repo, Entry, Ctx, Outcome) :-
  memberchk(Action, [install, reinstall, update, downgrade]),
  binpkg_exec:available_for(Repo, Entry, Ctx, BinpkgEntryId),
  ( binpkg_exec:execute(Action, Repo, Entry, BinpkgEntryId, Ctx, BinOutcome),
    BinOutcome == done
  -> Outcome = done
  ;  ebuild_exec:execute_phases(Action, Repo, Entry, Ctx, Outcome)
  ),
  !.

ebuild_exec:execute(Action, Repo, Entry, Ctx, Outcome) :-
  ebuild_exec:execute_phases(Action, Repo, Entry, Ctx, Outcome).


%! ebuild_exec:execute_phases(+Action, +Repo, +Entry, +Ctx, -Outcome) is det.

ebuild_exec:execute_phases(Action, Repo, Entry, Ctx, Outcome) :-
  ( ebuild_exec:action_phases(Action, Ctx, Phases),
    ebuild_exec:ebuild_path(Repo, Entry, EbuildPath)
  -> ebuild_exec:collect_use_string(Repo, Entry, Ctx, UseString),
     ebuild_exec:run_phases(EbuildPath, Phases, UseString, ExitCode),
     ( ExitCode =:= 0
     -> Outcome = done
     ;  Outcome = failed(ExitCode)
     )
  ;  Outcome = failed(no_ebuild)
  ),
  ebuild_exec:maybe_inject_built(Action, Repo, Entry, Ctx, Outcome).


%! ebuild_exec:execute_with_progress(+Action, +Repo, +Entry, +Ctx, :PhaseCallback, -Outcome) is det.
%
% Execute a plan action with per-phase progress callbacks.
% The merge phase handles replacement of old versions internally.

:- meta_predicate ebuild_exec:execute_with_progress(+, +, +, +, 2, -).

% Disabled: see execute/5 comment above.
%
% ebuild_exec:execute_with_progress(Action, Repo, Entry, Ctx, PhaseCallback, Outcome) :-
%   memberchk(Action, [update, downgrade]),
%   !,
%   ( ebuild_exec:unmerge_old(Repo, Ctx)
%   -> ebuild_exec:execute_phases_sequential(Action, Repo, Entry, Ctx, PhaseCallback, Outcome)
%   ;  Outcome = failed(unmerge_old)
%   ).

% Binpkg fast-path (mirrors the execute/5 hook above). Synthesizes a
% single `qmerge` phase event so progress UIs see the binary merge as
% one logical step. The qmerge stdout/stderr stream to the user's
% terminal directly (binpkg_exec doesn't currently log to a file --
% qmerge's output is short and self-explanatory: "Installing app-misc/jq-1.8.1
% to /").
ebuild_exec:execute_with_progress(Action, Repo, Entry, Ctx, PhaseCallback, Outcome) :-
  memberchk(Action, [install, reinstall, update, downgrade]),
  binpkg_exec:available_for(Repo, Entry, Ctx, BinpkgEntryId),
  catch(call(PhaseCallback, qmerge, active), _, true),
  ( binpkg_exec:execute(Action, Repo, Entry, BinpkgEntryId, Ctx, BinOutcome),
    BinOutcome == done
  -> catch(call(PhaseCallback, qmerge, done), _, true),
     Outcome = done
  ;  ebuild_exec:execute_phases_sequential(Action, Repo, Entry, Ctx, PhaseCallback, Outcome)
  ),
  !.

ebuild_exec:execute_with_progress(Action, Repo, Entry, Ctx, PhaseCallback, Outcome) :-
  ebuild_exec:execute_phases_sequential(Action, Repo, Entry, Ctx, PhaseCallback, Outcome).


%! ebuild_exec:execute_phases_sequential(+Action, +Repo, +Entry, +Ctx, :PhaseCallback, -Outcome) is det.

ebuild_exec:execute_phases_sequential(Action, Repo, Entry, Ctx, PhaseCallback, Outcome) :-
  ( ebuild_exec:action_phases(Action, Ctx, AllPhases),
    ebuild_exec:ebuild_path(Repo, Entry, EbuildPath)
  -> ebuild_exec:display_phases(Action, Repo, Entry, Ctx, DisplayPhases),
     ebuild_exec:collect_use_string(Repo, Entry, Ctx, UseString),
     ebuild_exec:ensure_log_dir,
     ebuild_exec:build_log_path(Entry, LogPath),
     ebuild_exec:load_phase_stats,
     ebuild_exec:run_phases_with_config(EbuildPath, Entry, AllPhases, DisplayPhases, LogPath, UseString, PhaseCallback, Outcome),
     ebuild_exec:save_phase_stats
  ;  Outcome = failed(no_ebuild)
  ),
  ebuild_exec:maybe_inject_built(Action, Repo, Entry, Ctx, Outcome).


%! ebuild_exec:maybe_inject_built(+Action, +Repo, +Entry, +Ctx, +Outcome) is det.
%
% After a successful SOURCE build that produced a binary package
% (`--buildpkg` / `--buildpkgonly` -> the `package` phase ran), register
% the produced gpkg in the in-memory binpkg cache so a later
% `binpkg_exec:available_for/4` can reuse it without a full `Packages`
% re-parse (portage-ng#80). Only fires for build-shaped actions that
% completed (`done`); a binpkg fast-path merge never reaches this code
% (it produces nothing). Always succeeds; the inject itself is fully
% guarded so it can never turn a successful build into a failure.

ebuild_exec:maybe_inject_built(Action, Repo, Entry, Ctx, done) :-
  memberchk(Action, [install, reinstall, update, downgrade]),
  ( preference:flag(buildpkg) ; preference:flag(buildpkgonly) ),
  current_predicate(binpkg_exec:inject_built_binpkg/3),
  !,
  catch(binpkg_exec:inject_built_binpkg(Repo, Entry, Ctx), _, true).

ebuild_exec:maybe_inject_built(_Action, _Repo, _Entry, _Ctx, _Outcome).


% =============================================================================
%  Update/downgrade: unmerge old version
% =============================================================================

% Disabled: the merge phase handles old version replacement internally
% via the VDB (pkg_preinst → merge files → unmerge old → pkg_postinst).
% Explicit pre-build unmerge is unnecessary and can break builds that
% depend on the old version's files at compile time.
%
% ebuild_exec:unmerge_old(_Repo, Ctx) :-
%   memberchk(replaces(OldRepo://OldEntry), Ctx),
%   !,
%   ebuild_exec:ebuild_path(OldRepo, OldEntry, OldEbuildPath),
%   ebuild_exec:collect_use_string(OldRepo, OldEntry, [], UseString),
%   ebuild_exec:run_phases(OldEbuildPath, [unmerge], UseString, ExitCode),
%   ExitCode =:= 0.
%
% ebuild_exec:unmerge_old(_, _).
