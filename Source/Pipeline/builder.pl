/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> BUILDER
Build execution orchestrator.

Takes the same pipeline output as printer (Plan, Proof, Model, Triggers)
and executes each step in parallel via the jobserver. Downloads use
parallel curl with per-file progress; other build phases dispatch to
ebuild_exec (Portage's ebuild CLI) when config:build_live_phases is
non-empty, or return stub when fully stubbed.

The full plan is printed first (normal colors, via printer:print/5), then
a progress area below shows slot-based live updates as workers execute
jobs. Within each plan step, all executable rules run in parallel; steps
are sequential (the next step starts only after the previous completes).
*/

:- module(builder, []).

:- dynamic builder:slot_info/6.
:- dynamic builder:slot_outcome/2.
:- dynamic builder:exec_phase_state/3.
:- dynamic builder:resume_done/2.
:- dynamic builder:last_build_status/3.

% =============================================================================
%  BUILDER declarations
% =============================================================================


%! builder:build(+Goals) is det.
%
% Top-level entry point. Proves goals, prints the plan, then executes
% each step in parallel via the jobserver with live progress.

builder:build(Goals) :-
  pipeline:prove_plan_with_fallback(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL),
  printer:print(Goals, ModelAVL, ProofAVL, Plan, TriggersAVL),
  nl,
  ( builder:ask_confirmation
  -> true
  ;  message:inform('Aborted.'),
     !,
     fail
  ),
  builder:maybe_create_snapshot(Plan),
  retractall(builder:resume_done(_, _)),
  builder:save_resume_state(Goals, Plan),
  plan:collect_plan_pre_actions(ProofAVL, PreActions),
  builder:count_actions(Plan, 0, PlanActions),
  length(PreActions, PreCount),
  ( PreCount > 0 -> PreSteps = 1 ; PreSteps = 0 ),
  TotalActions is PlanActions + PreCount,
  builder:count_nonempty_steps(Plan, 0, PlanSteps),
  NumSteps is PlanSteps + PreSteps,
  build:header(NumSteps, TotalActions),
  builder:print_pre_action_step(PreActions, PreSteps),
  StartStep is PreSteps + 1,
  builder:num_workers(NumWorkers),
  jobserver:init(NumWorkers, builder:execute_build_job),
  builder:execute_plan(Plan, StartStep, NumSteps, 0, 0, 0, Completed, Failed0, Stubs),
  jobserver:shutdown(NumWorkers),
  snapshot:finalize,
  builder:apply_vdb_reconciliation(Plan, Failed0, Failed, _Missing),
  ( Failed =:= 0
  -> builder:clear_resume_state
  ;  true
  ),
  build:summary(Completed, Failed, Stubs),
  retractall(builder:last_build_status(_, _, _)),
  assertz(builder:last_build_status(Completed, Failed, Stubs)),
  builder:alert.


%! builder:build_resume is det.
%
% Resumes a previously interrupted build. Loads the saved plan from
% Knowledge/resume.pl, filters out completed packages, and re-executes
% the remainder. Skips the clean phase so ebuild can pick up from the
% preserved work directory.

builder:build_resume :-
  ( builder:load_resume_state(_Goals, Plan, DoneList)
  -> true
  ;  message:failure('No saved build state found. Run --build first.'),
     !,
     fail
  ),
  builder:collect_skip_entries(Plan, SkipDone),
  append(DoneList, SkipDone, AllDone),
  builder:filter_completed_plan(Plan, AllDone, FilteredPlan),
  builder:count_actions(FilteredPlan, 0, RemainingActions),
  ( RemainingActions =:= 0
  -> message:inform('Nothing to resume — all packages completed successfully.'),
     builder:clear_resume_state
  ;  length(DoneList, CompletedCount),
     format(atom(ResumeMsg), '>>> Resuming: ~d completed, ~d remaining', [CompletedCount, RemainingActions]),
     message:color(green),
     message:print(ResumeMsg),
     message:reset,
     nl, nl,
     assertz(ebuild_exec:resuming),
     retractall(builder:resume_done(_, _)),
     builder:count_nonempty_steps(FilteredPlan, 0, NumSteps),
     build:header(NumSteps, RemainingActions),
     builder:num_workers(NumWorkers),
     jobserver:init(NumWorkers, builder:execute_build_job),
     builder:execute_plan(FilteredPlan, 1, NumSteps, 0, 0, 0, Completed, Failed0, Stubs),
     jobserver:shutdown(NumWorkers),
     builder:apply_vdb_reconciliation(FilteredPlan, Failed0, Failed, _Missing),
     ( Failed =:= 0
     -> builder:clear_resume_state
     ;  true
     ),
     build:summary(Completed, Failed, Stubs),
     retractall(builder:last_build_status(_, _, _)),
     assertz(builder:last_build_status(Completed, Failed, Stubs)),
     builder:alert,
     retractall(ebuild_exec:resuming)
  ).


%! builder:alert is det.
%
% When --alert is active, rings the terminal bell to attract attention.

builder:alert :-
  ( preference:flag(alert)
  -> message:bell
  ;  true
  ).


%! builder:ask_confirmation is semidet.
%
% When --ask is active, prompts the user to confirm before proceeding.
% Succeeds immediately if --ask is not set. Fails if the user declines.

builder:ask_confirmation :-
  ( preference:flag(ask)
  -> ( preference:flag(readnews)
     -> catch(news:check, _, true), nl
     ;  true
     ),
     builder:alert,
     nl,
     message:print('Would you like to merge these packages? [Yes/No] '),
     flush_output,
     read_line_to_string(current_input, Answer),
     ( member(Answer, ["Yes", "yes", "Y", "y", ""])
     -> true
     ;  false
     )
  ;  true
  ).


%! builder:num_workers(-N) is det.
%
% Compute the number of worker threads: min(cpu_count, available_display_lines).
% When --jobs N is specified and N > 0, uses that value instead.

builder:num_workers(N) :-
  ( config:cli_jobs(J)
  -> N = J
  ;  config:number_of_cpus(Cpus),
     config:printing_tty_size(H, _W),
     ReservedLines = 6,
     MaxDisplay is max(1, H - ReservedLines),
     N is min(Cpus, MaxDisplay)
  ).


% =============================================================================
%  Pre-action steps (keyword/unmask/use_change)
% =============================================================================

%! builder:print_pre_action_step(+PreActions, +PreSteps) is det.
%
% Renders pre-actions (keyword acceptance, unmask, use flag changes) as
% a completed step in the build display, matching the plan printer's
% format. These are informational — the prover already assumed them.

builder:print_pre_action_step([], _) :- !.

builder:print_pre_action_step(PreActions, _PreSteps) :-
  format(atom(AtomStepNum), '~t~0f~2|', [1]),
  format(atom(StepLabel), 'step ~a', [AtomStepNum]),
  write(' \u2514\u2500'),
  message:bubble(darkgray, StepLabel),
  write('\u2500\u2524 '),
  builder:print_pre_actions(PreActions),
  nl, nl.


%! builder:print_pre_actions(+PreActions) is det.

builder:print_pre_actions([Action]) :-
  !,
  builder:print_pre_action(Action),
  build:right_edge_ok.

builder:print_pre_actions([Action|Rest]) :-
  builder:print_pre_action(Action),
  build:right_edge_ok,
  forall(member(A, Rest),
         ( nl,
           write('             \u2502 '),
           builder:print_pre_action(A),
           build:right_edge_ok
         )).


%! builder:print_pre_action(+PreAction) is det.

builder:print_pre_action(unmask(R, E, _C, _N)) :-
  message:bubble(orange, unmask),
  message:color(green),
  message:column(24, R://E),
  message:color(normal).

builder:print_pre_action(accept_license(R, E, _C, _N)) :-
  message:bubble(orange, license),
  message:color(green),
  message:column(24, R://E),
  message:color(normal).

builder:print_pre_action(accept_keyword(R, E, _C, _N, K)) :-
  warning:keyword_atom(K, KAtom),
  message:bubble(orange, keyword),
  message:color(green),
  message:column(24, R://E),
  message:color(darkgray),
  format(atom(Msg), ' (~w)', [KAtom]),
  message:print(Msg),
  message:color(normal).

builder:print_pre_action(use_change(R, E, _C, _N, _Changes)) :-
  message:bubble(orange, useflag),
  message:color(green),
  message:column(24, R://E),
  message:color(normal).


% =============================================================================
%  Step counting
% =============================================================================

%! builder:count_actions(+Plan, +Acc, -Total) is det.

builder:count_actions([], Total, Total).

builder:count_actions([Step|Rest], Acc, Total) :-
  builder:count_executable_in_step(Step, N),
  Acc1 is Acc + N,
  builder:count_actions(Rest, Acc1, Total).

builder:count_executable_in_step(Rules, N) :-
  include(builder:is_executable_rule, Rules, Executable),
  length(Executable, N).


%! builder:count_nonempty_steps(+Plan, +Acc, -Total) is det.

builder:count_nonempty_steps([], Total, Total).

builder:count_nonempty_steps([Step|Rest], Acc, Total) :-
  builder:count_executable_in_step(Step, N),
  ( N > 0 -> Acc1 is Acc + 1 ; Acc1 = Acc ),
  builder:count_nonempty_steps(Rest, Acc1, Total).


%! builder:is_executable_rule(+Rule) is semidet.

builder:is_executable_rule(rule(_Repository://_Entry:_Action?{_Context}, _Body)) :- !.
builder:is_executable_rule(rule(world(_Atom):_Action?{_Ctx}, _Body)) :- !.
builder:is_executable_rule(_) :- fail.


% =============================================================================
%  VDB reconciliation (defensive backstop)
% =============================================================================
%
% Even with correct per-step failure counting, the engine has historically
% leaked silent-success regressions: a sub-dependency's install fails,
% but the aggregate `Failed` counter ends up at 0 and `--ci --build`
% exits 0. The downstream comparison harness (tinderbox-ng's
% render-compare-matrix.py:real_pn_built/parse_vdb_delta) catches this
% post-hoc by parsing the VDB delta, but by then the engine has already
% lied about its own success.
%
% This block pulls that ground-truth check up into the engine itself.
% After execute_plan returns, we walk the plan and verify that every
% rule whose action would install a package (install / update /
% downgrade / reinstall) has produced an on-disk VDB entry. Any
% missing entries are counted as failures regardless of what the
% step-by-step tally said, so `builder:last_build_status/3` -- and
% therefore `action:maybe_ci_exit_on_build_failure/1` -- reflects
% reality.
%
% The check is intentionally cheap (one directory-stat per install
% action) and is gated so it only runs when a real merge could have
% happened: `merge` must be in `config:build_live_phases/1` and the
% `pkg` repository (VDB) must be registered with a stat'able location.
% Stubbed runs (build_live_phases empty, --pretend-style) are skipped
% silently.

%! builder:apply_vdb_reconciliation(+Plan, +F0, -F, -Missing) is det.
%
% Wrapper that combines `reconcile_install_actions/3` with the
% Failed-counter adjustment + warning print. F = F0 + len(Missing) when
% the check fired; F = F0 otherwise. Missing is the list of plan rules
% (Repo://Entry:Action terms) whose VDB entry is missing on disk.

builder:apply_vdb_reconciliation(Plan, F0, F, Missing) :-
  builder:reconcile_install_actions(Plan, Missing, Active),
  ( Active == true, Missing \= []
  -> length(Missing, N),
     F is F0 + N,
     builder:print_reconciliation_warning(Missing, N)
  ;  F = F0
  ).


%! builder:reconcile_install_actions(+Plan, -Missing, -Active) is det.
%
% Walk Plan and collect install-shaped rules whose target package has
% no corresponding directory under the VDB root. Active is `true`
% when the check actually ran, `false` when it was skipped (no merge
% in live phases, or no pkg repository). When Active is `false`,
% callers MUST ignore Missing -- it is bound to `[]` by convention but
% carries no information about reality.

builder:reconcile_install_actions(Plan, Missing, Active) :-
  ( builder:reconciliation_should_run(VdbRoot)
  -> Active = true,
     findall(Repo://Entry:Action,
             ( member(Step, Plan),
               member(Rule, Step),
               builder:is_install_rule(Rule, Repo, Entry, Action),
               \+ builder:vdb_entry_present(VdbRoot, Entry)
             ),
             Missing)
  ;  Active = false,
     Missing = []
  ).


%! builder:reconciliation_should_run(-VdbRoot) is semidet.
%
% Succeeds (binding VdbRoot to the on-disk VDB location) iff a real
% merge could have happened during this run AND the VDB is stat'able.
% Fails silently for stubbed builds, fully-dry runs, or hosts without
% a registered `pkg` repository.

builder:reconciliation_should_run(VdbRoot) :-
  config:build_live_phases(LP),
  memberchk(merge, LP),
  current_predicate(pkg:get_location/1),
  catch(pkg:get_location(VdbRoot), _, fail),
  exists_directory(VdbRoot).


%! builder:is_install_rule(+Rule, -Repo, -Entry, -Action) is semidet.
%
% True when Rule is an `install` / `update` / `downgrade` / `reinstall`
% action on an eapi-typed source repository -- i.e. an action expected
% to land a directory under `<vdb_root>/<cat>/<pf>/` when it succeeds.
% Other actions (download, fetchonly, register, uninstall, world ops,
% non-eapi script execs) don't write to the VDB and are excluded from
% reconciliation.

builder:is_install_rule(rule(Repo://Entry:Action?{_Ctx}, _Body), Repo, Entry, Action) :-
  memberchk(Action, [install, update, downgrade, reinstall]),
  catch(Repo:get_type(eapi), _, fail).


%! builder:vdb_entry_present(+VdbRoot, +Entry) is semidet.
%
% True iff `<VdbRoot>/<Entry>/` exists as a directory. Entry is the
% canonical `<cat>/<pf>` form used throughout cache:ordered_entry/5,
% which matches the on-disk VDB layout (cf. repository:find_vdb_entry/4).

builder:vdb_entry_present(VdbRoot, Entry) :-
  atomic_list_concat([VdbRoot, '/', Entry], Path),
  exists_directory(Path).


%! builder:print_reconciliation_warning(+Missing, +N) is det.
%
% Print a clearly-flagged warning enumerating the missing installs.
% Format mirrors the existing builder warnings (message:warning) so the
% line shows up in build logs with the standard "!!!" prefix and
% orange/red coloring, making this easy to grep for in CI reports.

builder:print_reconciliation_warning(Missing, N) :-
  ( N =:= 1 -> Suffix = '' ; Suffix = 's' ),
  format(atom(Header),
         'VDB reconciliation: ~w install action~w missing from /var/db/pkg after build',
         [N, Suffix]),
  message:warning([Header]),
  forall(member(Repo://Entry:Action, Missing),
         ( format(atom(Line), '  ~w  ~w://~w', [Action, Repo, Entry]),
           message:warning([Line])
         )),
  message:warning(['Counting these as failures even though the per-step tally missed them.']),
  message:warning(['See builder:apply_vdb_reconciliation/4 for the backstop logic.']).


% =============================================================================
%  Plan execution (step-at-a-time via jobserver)
% =============================================================================

%! builder:execute_plan(+Plan, +PlanStep, +NumSteps, +C0, +F0, +S0, -C, -F, -S) is det.

builder:execute_plan([], _PlanStep, _NumSteps, C, F, S, C, F, S).

builder:execute_plan([Step|Rest], PlanStep, NumSteps, C0, F0, S0, C, F, S) :-
  builder:execute_step(Step, PlanStep, NumSteps, C0, F0, S0, C1, F1, S1, HasJobs),
  ( HasJobs == true -> PlanStep1 is PlanStep + 1 ; PlanStep1 = PlanStep ),
  builder:flush_resume_done_to_disk,
  ( F1 > F0
  -> ( builder:should_continue_on_failure ->
       builder:execute_plan(Rest, PlanStep1, NumSteps, C1, F1, S1, C, F, S)
     ; builder:skip_remaining(Rest, PlanStep1, NumSteps, C1, F1, S1, C, F, S)
     )
  ;  builder:execute_plan(Rest, PlanStep1, NumSteps, C1, F1, S1, C, F, S)
  ).


%! builder:should_continue_on_failure is semidet.
%
% Succeeds when --continue-on-failure is set to a value other
% than 'never'.

builder:should_continue_on_failure :-
  config:continue_on_failure(Mode),
  Mode \== never.


%! builder:skip_remaining(+Plan, +PlanStep, +NumSteps, +C0, +F0, +S0, -C, -F, -S) is det.
%
% When a step has failures, skip all remaining steps. Each remaining
% executable action is counted as failed since its dependencies weren't met.

builder:skip_remaining([], _PlanStep, _NumSteps, C, F, S, C, F, S).

builder:skip_remaining([Step|Rest], PlanStep, NumSteps, C0, F0, S0, C, F, S) :-
  include(builder:is_executable_rule, Step, Executable),
  length(Executable, NumJobs),
  ( NumJobs > 0
  -> builder:assign_slots(Executable, PlanStep, NumSteps, SlottedJobs, TotalLines),
     build:print_skipped_slots(SlottedJobs, NumSteps),
     builder:mark_skipped(SlottedJobs, TotalLines),
     nl,
     F1 is F0 + NumJobs,
     PlanStep1 is PlanStep + 1
  ;  F1 = F0,
     PlanStep1 = PlanStep
  ),
  builder:skip_remaining(Rest, PlanStep1, NumSteps, C0, F1, S0, C, F, S).


%! builder:mark_skipped(+SlottedJobs, +TotalLines) is det.
%
% Mark all slots in a skipped step as failed (dependency not met).

builder:mark_skipped([], _).

builder:mark_skipped([slotted(LineOff, TotalLines, PlanStep, NumSteps, ActionIdx, rule(Repo://Entry:Action?{_Ctx}, _Body), _FileInfo)|Rest], _) :-
  !,
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, skipped, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)),
  builder:mark_skipped(Rest, TotalLines).

builder:mark_skipped([slotted(LineOff, TotalLines, PlanStep, NumSteps, ActionIdx, rule(world(Atom):Action?{_Ctx}, _Body), _FileInfo)|Rest], _) :-
  !,
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, skipped, PlanStep, NumSteps, ActionIdx, Action, Atom)),
  builder:mark_skipped(Rest, TotalLines).

builder:mark_skipped([_|Rest], TotalLines) :-
  builder:mark_skipped(Rest, TotalLines).


%! builder:execute_step(+Step, +PlanStep, +NumSteps, +C0, +F0, +S0, -C, -F, -S, -HasJobs) is det.
%
% Execute all jobs in a step in parallel:
%   1. Extract executable rules and pre-allocate display layout
%   2. Register slot info for result handler lookups
%   3. Print all slots (with file sub-lines for downloads)
%   4. Submit all jobs to the jobserver
%   5. Collect results, updating slots in-place
%   6. Tally outcomes, clean up

builder:execute_step(Step, PlanStep, NumSteps, C0, F0, S0, C, F, S, HasJobs) :-
  plan:stable_sort_by_weight(Step, Sorted),
  include(builder:is_executable_rule, Sorted, Executable),
  length(Executable, NumJobs),
  ( NumJobs > 0
  -> HasJobs = true,
     builder:assign_slots(Executable, PlanStep, NumSteps, SlottedJobs, TotalLines),
     builder:register_slot_info(SlottedJobs),
     build:print_job_slots(SlottedJobs, NumSteps),
     jobserver:submit(SlottedJobs),
     jobserver:collect(NumJobs, builder:handle_result(TotalLines)),
     nl,
     builder:tally_outcomes(C0, F0, S0, C, F, S),
     builder:clear_slot_info
  ;  HasJobs = false,
     C = C0, F = F0, S = S0
  ).


%! builder:assign_slots(+Rules, +PlanStep, +NumSteps, -SlottedJobs, -TotalLines) is det.
%
% Pre-allocate the display layout. For download/fetchonly rules, queries
% distfile specs to determine file sub-line count. Each slotted/7 term
% carries its absolute LineOffset, plan step number, within-step action
% index (0-based), and a shared TotalLines variable (bound when the
% last rule is processed).

builder:assign_slots(Rules, PlanStep, NumSteps, SlottedJobs, TotalLines) :-
  distfiles:get_location(Distdir),
  builder:assign_slots_(Rules, PlanStep, NumSteps, Distdir, 0, 0, TotalLines, SlottedJobs).

builder:assign_slots_([], _PlanStep, _NumSteps, _Distdir, LineOff, _ActionIdx, LineOff, []).

builder:assign_slots_([Rule|Rest], PlanStep, NumSteps, Distdir, LineOff, ActionIdx, TotalLines, [Slotted|More]) :-
  builder:rule_file_info(Rule, Distdir, LineOff, FileInfo, LinesForRule),
  NextLineOff is LineOff + LinesForRule,
  Slotted = slotted(LineOff, TotalLines, PlanStep, NumSteps, ActionIdx, Rule, FileInfo),
  ActionIdx1 is ActionIdx + 1,
  builder:assign_slots_(Rest, PlanStep, NumSteps, Distdir, NextLineOff, ActionIdx1, TotalLines, More).


%! builder:rule_file_info(+Rule, +Distdir, +LineOff, -FileInfo, -Lines) is det.
%
% Determine file metadata for a rule. Downloads with distfiles get
% files(FileStartLine, NumFiles, DistFiles, Distdir); others get no_files.

builder:rule_file_info(rule(Repo://Entry:Action?{_Ctx}, _Body), _Distdir, LineOff, FileInfo, Lines) :-
  memberchk(Action, [download, fetchonly]),
  Repo:get_type(eapi),
  predicate_property(ebuild:is_live(_), defined),
  ebuild:is_live(Repo://Entry),
  !,
  LiveStartLine is LineOff + 1,
  FileInfo = live_source(LiveStartLine),
  Lines = 2.

builder:rule_file_info(rule(Repo://Entry:Action?{_Ctx}, _Body), Distdir, LineOff, FileInfo, Lines) :-
  memberchk(Action, [download, fetchonly]),
  Repo:get_type(eapi),
  !,
  download:collect_distfile_specs(Repo, Entry, DistFiles),
  length(DistFiles, NumFiles),
  ( NumFiles > 0
  -> FileStartLine is LineOff + 1,
     FileInfo = files(FileStartLine, NumFiles, DistFiles, Distdir),
     Lines is 1 + NumFiles
  ;  FileInfo = no_files,
     Lines = 1
  ).

builder:rule_file_info(rule(Repo://Entry:Action?{Ctx}, _Body), _Distdir, LineOff, SubInfo, Lines) :-
  \+ memberchk(Action, [download, fetchonly]),
  Repo:get_type(eapi),
  predicate_property(ebuild_exec:display_phases(_,_,_,_,_), defined),
  catch(ebuild_exec:display_phases(Action, Repo, Entry, Ctx, PhaseList), _, fail),
  PhaseList \= [],
  !,
  ExecLine is LineOff + 1,
  build:exec_phase_line_count(PhaseList, ExecLineCount),
  builder:count_conf_lines(Repo, Entry, Action, Ctx, ConfCount),
  ( predicate_property(ebuild_exec:build_log_path(_,_), defined)
  -> catch(ebuild_exec:build_log_path(Entry, LogPath), _, LogPath = '')
  ;  LogPath = ''
  ),
  ( catch(config:show_build_logs(true), _, fail)
  -> LogsLine is ExecLine + ExecLineCount,
     SubInfo = phases(ExecLine, ExecLineCount, LogsLine, PhaseList, LogPath),
     Lines is 1 + ConfCount + ExecLineCount + 1
  ;  SubInfo = phases(ExecLine, ExecLineCount, -1, PhaseList, LogPath),
     Lines is 1 + ConfCount + ExecLineCount
  ).

builder:rule_file_info(_, _, _, no_files, 1).


%! builder:count_conf_lines(+Repo, +Entry, +Action, +Ctx, -Count) is det.
%
% Count how many display lines plan:print_config would produce for
% this rule (USE flags, USE_EXPAND variables, slot info). Captures
% the output and counts newlines to stay consistent with the plan printer.

builder:count_conf_lines(Repo, Entry, Action, Ctx, Count) :-
  memberchk(Action, [install, update, downgrade, reinstall]),
  !,
  builder:count_conf_lines_as_short(Repo, Entry, Action, Ctx, Count).
builder:count_conf_lines(_, _, _, _, 0).


builder:count_conf_lines_as_short(Repo, Entry, Action, Ctx, Count) :-
  config:printing_style('column'), !,
  setup_call_cleanup(
    ( retract(config:interface_printing_style('column')),
      assertz(config:interface_printing_style('short')) ),
    ( with_output_to(string(S),
        catch(plan:print_config(Repo://Entry:Action?{Ctx}), _, true)),
      split_string(S, "\n", "", Parts),
      length(Parts, N),
      Count is max(0, N - 1) ),
    ( retract(config:interface_printing_style('short')),
      assertz(config:interface_printing_style('column')) )
  ).

builder:count_conf_lines_as_short(Repo, Entry, Action, Ctx, Count) :-
  with_output_to(string(S),
    catch(plan:print_config(Repo://Entry:Action?{Ctx}), _, true)),
  split_string(S, "\n", "", Parts),
  length(Parts, N),
  Count is max(0, N - 1).


% =============================================================================
%  Slot info registry
% =============================================================================

%! builder:register_slot_info(+SlottedJobs) is det.
%
% Store slot metadata so the result handler can look up step/action/entry
% for display without needing the original job term.

builder:register_slot_info([]).

builder:register_slot_info([slotted(LineOff, _TotalLines, PlanStep, NumSteps, ActionIdx, rule(Repo://Entry:Action?{_Ctx}, _Body), _FileInfo)|Rest]) :-
  !,
  assertz(builder:slot_info(LineOff, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)),
  builder:register_slot_info(Rest).

builder:register_slot_info([slotted(LineOff, _TotalLines, PlanStep, NumSteps, ActionIdx, rule(world(Atom):Action?{_Ctx}, _Body), _FileInfo)|Rest]) :-
  !,
  assertz(builder:slot_info(LineOff, PlanStep, NumSteps, ActionIdx, Action, Atom)),
  builder:register_slot_info(Rest).

builder:register_slot_info([_|Rest]) :-
  builder:register_slot_info(Rest).


%! builder:clear_slot_info is det.

builder:clear_slot_info :-
  retractall(builder:slot_info(_, _, _, _, _, _)),
  retractall(builder:slot_outcome(_, _)),
  retractall(builder:dl_prev_snapshot(_, _, _)),
  retractall(builder:exec_phase_state(_, _, _)).


%! builder:get_slot_info(+Slot, -PlanStep, -NumSteps, -ActionIdx, -Action, -Entry) is det.

builder:get_slot_info(Slot, PlanStep, NumSteps, ActionIdx, Action, Entry) :-
  builder:slot_info(Slot, PlanStep, NumSteps, ActionIdx, Action, Entry), !.

builder:get_slot_info(_Slot, 0, 0, 0, unknown, unknown).



% =============================================================================
%  Job execution (called by worker threads)
% =============================================================================

%! builder:execute_build_job(+SlottedJob, +WorkerSlot, -Result) is det.
%
% Execute a single build job. Called by jobserver worker threads.
% Updates the display slot to "active" on entry, then performs the work.
% For download/fetchonly actions, the worker manages file sub-slot display
% itself and returns display_handled(Outcome) to skip redundant updates.

builder:execute_build_job(
    slotted(LineOff, TotalLines, PlanStep, NumSteps, ActionIdx, rule(Repo://Entry:Action?{Ctx}, _Body), FileInfo),
    _WorkerSlot, result(LineOff, ResultOutcome)) :-
  !,
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, active, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)),
  builder:maybe_quickpkg_old(Action, Ctx),
  ( FileInfo = live_source(LiveStartLine)
  -> builder:run_git_download(Repo, Entry, LiveStartLine, TotalLines,
                               LineOff, PlanStep, NumSteps, ActionIdx, Action, Outcome),
     ResultOutcome = display_handled(Outcome)
  ;  FileInfo = files(FileStartLine, _NumFiles, DistFiles, Distdir)
  -> builder:run_download_parallel(Repo, Entry, Ctx, LineOff, TotalLines, PlanStep, NumSteps, ActionIdx, Action,
                                    FileStartLine, DistFiles, Distdir, Outcome),
     ResultOutcome = display_handled(Outcome)
  ;  memberchk(Action, [download, fetchonly])
  -> with_mutex(build_display,
       build:update_slot(LineOff, TotalLines, done, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)),
     ResultOutcome = display_handled(done)
  ;  FileInfo = phases(ExecLine, ExecLineCount, LogsLine, PhaseList, LogPath)
  -> builder:run_action_with_phases(Action, Repo, Entry, Ctx,
                                     TotalLines, ExecLine, ExecLineCount, LogsLine, PhaseList, LogPath,
                                     LineOff, PlanStep, NumSteps, ActionIdx, Outcome),
     ResultOutcome = display_handled(Outcome)
  ;  builder:run_action(Action, Repo, Entry, Ctx, Outcome),
     ResultOutcome = Outcome
  ),
  builder:dispatch_suggestions(Repo, Entry, Ctx).

builder:execute_build_job(
    slotted(LineOff, TotalLines, PlanStep, NumSteps, ActionIdx, rule(world(Atom):Action?{_Ctx}, _Body), _FileInfo),
    _WorkerSlot, result(LineOff, display_handled(done))) :-
  !,
  builder:execute_world(Action, Atom),
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, done, PlanStep, NumSteps, ActionIdx, Action, Atom)).

% Catch-all: a job whose rule shape doesn't match either of the two
% recognized executable patterns above. Historically this silently
% returned `stub`, which tally_outcomes counted as a (harmless) stub
% rather than a failure -- so any future malformed rule would slip
% through `--ci --build` as exit 0. Treat it as a failure with a
% descriptive reason so the bug surfaces immediately.
builder:execute_build_job(Job, _WorkerSlot, result(unknown, failed(unrecognised_job_shape(Job)))) :-
  format(user_error,
         '[builder] unrecognised job shape, treating as failure: ~q~n',
         [Job]).


%! builder:run_action(+Action, +Repo, +Entry, +Ctx, -Outcome) is det.
%
% Execute a non-download action. Downloads are handled by
% run_download_parallel via execute_build_job.
%
% Dispatches to ebuild_exec for real builds when config:build_live_phases
% is non-empty. Falls back to stub when fully stubbed or ebuild_exec
% is unavailable.

builder:run_action(Action, Repo, Entry, Ctx, Outcome) :-
  Repo:get_type(eapi),
  config:build_live_phases(LP), LP \= [],
  predicate_property(ebuild_exec:execute(_,_,_,_,_), defined),
  !,
  ebuild_exec:execute(Action, Repo, Entry, Ctx, Outcome).

builder:run_action(_Action, Repo, _Entry, _Ctx, Outcome) :-
  Repo:get_type(Type),
  Type \= eapi,
  !,
  Repo:get_location(Location),
  ( script:exec(build, [Type, Location])
  -> Outcome = ok
  ;  Outcome = failed
  ).

builder:run_action(_Action, _Repo, _Entry, _Ctx, stub).


%! builder:run_action_with_phases(+Action, +Repo, +Entry, +Ctx, +TotalLines, +ExecLine, +ExecLineCount, +LogsLine, +PhaseList, +LogPath, +LineOff, +PlanStep, +NumSteps, +ActionIdx, -Outcome) is det.
%
% Execute a build action with inline phase progress tracking.
% Uses exec lines with arrow-separated phases and a logs line below.

builder:run_action_with_phases(Action, Repo, Entry, _Ctx,
                                TotalLines, _ExecLine, _ExecLineCount, _LogsLine, _PhaseList, _LogPath,
                                LineOff, PlanStep, NumSteps, ActionIdx, Outcome) :-
  Repo:get_type(Type),
  Type \= eapi,
  !,
  Repo:get_location(Location),
  ( script:exec(build, [Type, Location])
  -> Outcome = ok
  ;  Outcome = failed
  ),
  builder:outcome_to_status(Outcome, FinalStatus),
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, FinalStatus, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)).

builder:run_action_with_phases(Action, Repo, Entry, Ctx,
                                TotalLines, ExecLine, _ExecLineCount, LogsLine, PhaseList, LogPath,
                                LineOff, PlanStep, NumSteps, ActionIdx, Outcome) :-
  Repo:get_type(eapi),
  config:build_live_phases(LP), LP \= [],
  predicate_property(ebuild_exec:execute_with_progress(_,_,_,_,_,_), defined),
  !,
  builder:init_exec_phase_state(ExecLine, PhaseList),
  Callback = builder:phase_callback(TotalLines, ExecLine, LogsLine, Action, PhaseList, LogPath),
  ebuild_exec:execute_with_progress(Action, Repo, Entry, Ctx, Callback, Outcome),
  builder:outcome_to_status(Outcome, FinalStatus),
  builder:clear_exec_phase_state(ExecLine),
  ( LogsLine >= 0
  -> with_mutex(build_display,
       build:update_logs_line(LogsLine, TotalLines, LogPath, FinalStatus))
  ;  true
  ),
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, FinalStatus, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)).

builder:run_action_with_phases(Action, Repo, Entry, _Ctx,
                                TotalLines, ExecLine, _ExecLineCount, LogsLine, PhaseList, LogPath,
                                LineOff, PlanStep, NumSteps, ActionIdx, stub) :-
  builder:stub_all_phases(Action, PhaseList, TotalLines, ExecLine, LogsLine, LogPath),
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, stub, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)).


%! builder:phase_callback(+TotalLines, +ExecLine, +LogsLine, +Action, +PhaseList, +LogPath, +Phase, +Status) is det.
%
% Display callback invoked by ebuild_exec for each phase transition.
% Updates the exec_phase_state facts and re-renders the inline exec line.

builder:phase_callback(TotalLines, ExecLine, LogsLine, Action, PhaseList, LogPath, Phase, Status) :-
  memberchk(Phase, PhaseList),
  !,
  ( retract(builder:exec_phase_state(ExecLine, Phase, _)) -> true ; true ),
  assertz(builder:exec_phase_state(ExecLine, Phase, Status)),
  builder:collect_phase_states(ExecLine, PhaseList, PhaseStates),
  with_mutex(build_display,
    build:update_exec_line(ExecLine, TotalLines, Action, PhaseStates)),
  ( builder:is_failure_status(Status), LogsLine >= 0
  -> with_mutex(build_display,
       build:update_logs_line(LogsLine, TotalLines, LogPath, failed))
  ;  true
  ).

builder:phase_callback(_, _, _, _, _, _, _, _).


%! builder:collect_phase_states(+ExecLine, +PhaseList, -PhaseStates) is det.
%
% Build a list of Phase-Status pairs from the dynamic exec_phase_state facts.

builder:collect_phase_states(_, [], []).

builder:collect_phase_states(ExecLine, [Phase|Rest], [Phase-Status|States]) :-
  ( builder:exec_phase_state(ExecLine, Phase, Status)
  -> true
  ;  Status = pending
  ),
  builder:collect_phase_states(ExecLine, Rest, States).


%! builder:init_exec_phase_state(+ExecLine, +PhaseList) is det.
%
% Initialize all phases as pending for a given exec line.

builder:init_exec_phase_state(_, []).

builder:init_exec_phase_state(ExecLine, [Phase|Rest]) :-
  assertz(builder:exec_phase_state(ExecLine, Phase, pending)),
  builder:init_exec_phase_state(ExecLine, Rest).


%! builder:clear_exec_phase_state(+ExecLine) is det.
%
% Remove all phase state facts for a given exec line.

builder:clear_exec_phase_state(ExecLine) :-
  retractall(builder:exec_phase_state(ExecLine, _, _)).


builder:is_failure_status(failed).
builder:is_failure_status(failed(_)).
builder:is_failure_status(failed(_, _)).


%! builder:stub_all_phases(+Action, +PhaseList, +TotalLines, +ExecLine, +LogsLine, +LogPath) is det.
%
% Mark all phases as stub and render the inline display accordingly.

builder:stub_all_phases(Action, PhaseList, TotalLines, ExecLine, LogsLine, LogPath) :-
  maplist([P, P-stub]>>true, PhaseList, PhaseStates),
  with_mutex(build_display,
    build:update_exec_line(ExecLine, TotalLines, Action, PhaseStates)),
  ( LogsLine >= 0
  -> with_mutex(build_display,
       build:update_logs_line(LogsLine, TotalLines, LogPath, stub))
  ;  true
  ).


%! builder:run_git_download(+Repo, +Entry, +LiveStartLine, +TotalLines, +LineOff, +PlanStep, +NumSteps, +ActionIdx, +Action, -Outcome) is det.
%
% Clones or fetches a live ebuild's git repository with progress tracking.
% Extracts EGIT_REPO_URI from the ebuild, uses the distdir/git3-src cache
% (matching Portage's git-r3.eclass convention), and polls for progress.

builder:run_git_download(Repo, Entry, LiveStartLine, TotalLines,
                          LineOff, PlanStep, NumSteps, ActionIdx, Action, Outcome) :-
  ( download:extract_git_uri(Repo, Entry, URI)
  -> distfiles:get_location(Distdir),
     download:git_cache_dir(Distdir, GitCacheDir),
     ( \+ exists_directory(GitCacheDir) -> make_directory_path(GitCacheDir) ; true ),
     download:git_repo_cache_path(GitCacheDir, URI, RepoPath),
     ebuild_exec:build_log_path(Entry, LogPath),
     ebuild_exec:ensure_log_dir,
     Callback = builder:git_progress_callback(LiveStartLine, TotalLines),
     download:start_git_clone_async(URI, RepoPath, LogPath, Pid),
     download:poll_git_progress(Pid, LogPath, Callback, ExitCode),
     ( ExitCode =:= 0
     -> with_mutex(build_display,
          build:update_live_subslot(0, LiveStartLine, TotalLines, done)),
        with_mutex(build_display,
          build:update_slot(LineOff, TotalLines, done, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)),
        Outcome = done
     ;  with_mutex(build_display,
          build:update_live_subslot(0, LiveStartLine, TotalLines, failed)),
        with_mutex(build_display,
          build:update_slot(LineOff, TotalLines, failed(git), PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)),
        Outcome = failed(git)
     )
  ;  with_mutex(build_display,
       build:update_live_subslot(0, LiveStartLine, TotalLines, done)),
     with_mutex(build_display,
       build:update_slot(LineOff, TotalLines, done, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)),
     Outcome = done
  ).


%! builder:git_progress_callback(+LiveStartLine, +TotalLines, +Phase, +Status) is det.
%
% Updates the live sub-slot display with git clone/fetch progress.

builder:git_progress_callback(LiveStartLine, TotalLines, _Phase, progress(Pct)) :-
  with_mutex(build_display,
    build:update_live_subslot(0, LiveStartLine, TotalLines, progress(Pct))).

builder:git_progress_callback(_, _, _, _).


%! builder:run_download_parallel(+Repo, +Entry, +Ctx, +LineOff, +TotalLines, +PlanStep, +NumSteps, +ActionIdx, +Action, +FileStartLine, +DistFiles, +Distdir, -Outcome) is det.
%
% Parallel download with per-file progress using pre-allocated layout.
% File sub-lines are already printed by print_job_slots; this predicate
% only starts async curls, polls progress in-place, and updates the
% header slot on completion.

builder:run_download_parallel(Repo, Entry, _Ctx, LineOff, TotalLines, PlanStep, NumSteps, ActionIdx, Action,
                               FileStartLine, DistFiles, Distdir, Outcome) :-
  ( \+ exists_directory(Distdir) -> make_directory_path(Distdir) ; true ),
  ( download:is_fetch_restricted(Repo, Entry)
  -> builder:handle_restricted_files(DistFiles, 0, TotalLines, FileStartLine, Distdir, MissingCount),
     ( MissingCount =:= 0
     -> FinalStatus = done, Outcome = done
     ;  FinalStatus = failed('manual fetch required'), Outcome = failed('manual fetch required')
     )
  ;  builder:safe_mirror_layout(Layout),
     builder:prepare_download_jobs(Layout, Distdir, DistFiles, 0, Repo, Entry, DlJobs),
     get_time(T0),
     builder:init_speed_tracking(DlJobs, T0, FileStartLine),
     builder:poll_download_loop(DlJobs, TotalLines, FileStartLine, Distdir, FailCount),
     ( FailCount =:= 0
     -> FinalStatus = done, Outcome = done
     ;  FinalStatus = failed('download errors'), Outcome = failed('download errors')
     )
  ),
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, FinalStatus, PlanStep, NumSteps, ActionIdx, Action, Repo://Entry)).


%! builder:safe_mirror_layout(-Layout) is det.
%
% Defense-in-depth wrapper around download:mirror_layout/1. The
% downstream layout fetch is now thread-safe and tmp_file_stream-free,
% but this catch ensures that any *future* unrelated failure (DNS hiccup,
% mirror down at process start, parse error in layout.conf) degrades
% gracefully to the legacy `flat` layout instead of aborting the whole
% download job (which would propagate to a [jobserver worker error]
% and SKIP every dependent install/run/register in the plan).
%
% The legacy `flat` layout is the safe fallback: it's what Portage used
% before GLEP 75, our local mirror has always served files from the
% flat layout, and the upstream Gentoo mirrors still expose flat-named
% paths. So this fallback strictly preserves correctness for any
% mirror that doesn't actively reject flat lookups.

builder:safe_mirror_layout(Layout) :-
  catch(
    download:mirror_layout(Layout),
    E,
    ( print_message(warning,
        format("mirror_layout failed (~q); falling back to flat layout", [E])),
      Layout = flat,
      ( download:cached_mirror_layout(_)
      -> true
      ;  catch(assertz(download:cached_mirror_layout(flat)), _, true)
      )
    )).


% =============================================================================
%  Parallel download helpers
% =============================================================================

%! builder:prepare_download_jobs(+Layout, +Distdir, +DistFiles, +Idx, +Repo, +Entry, -DlJobs) is det.
%
% Start async curl processes for files not already present. Returns
% dl_job/8 terms for tracking. Already-present files are skipped (they
% already show checkmarks from print_file_subslots). Each curl walks
% every configured mirror_url in declaration order, exiting at the first
% successful download. Upstream SRC_URI fallback is handled later by
% builder:try_upstream_fallback/11 when the file still fails verification.
%
% Race-safety: curl is told to write into a per-process temp path
% (`download:tmp_dest_path/2`), never directly into the shared DestPath.
% builder:finalize_download/12 verifies the temp path and atomic-renames
% to DestPath only after a successful checksum match, then falls
% through to a race-recovery check on DestPath when our own download
% failed (so a concurrent writer that produced a valid file is honoured
% instead of triggering a redundant retry). See `download.pl` --
% "Race-safe distfile staging" for the full rationale.

builder:prepare_download_jobs(_, _, [], _, _, _, []).

builder:prepare_download_jobs(Layout, Distdir, [dist(Filename, Size, Pairs)|Rest], Idx, Repo, Entry, Jobs) :-
  Idx1 is Idx + 1,
  ( mirror:flat_present(Distdir, Filename)
  -> builder:prepare_download_jobs(Layout, Distdir, Rest, Idx1, Repo, Entry, Jobs)
  ;  builder:mirror_urls_for_file(Layout, Filename, URLs),
     atomic_list_concat([Distdir, '/', Filename], DestPath),
     download:tmp_dest_path(DestPath, TmpPath),
     catch(delete_file(TmpPath), _, true),
     download:start_curl_async(URLs, TmpPath, Pid),
     Jobs = [dl_job(Pid, Idx, Filename, Size, Pairs, DestPath, Repo, Entry)|MoreJobs],
     builder:prepare_download_jobs(Layout, Distdir, Rest, Idx1, Repo, Entry, MoreJobs)
  ).


%! builder:mirror_urls_for_file(+Layout, +Filename, -URLs) is det.
%
% Build the ordered list of HTTP mirror URLs to try for Filename. One URL
% per configured config:mirror_url/1 fact, expanded through the same GLEP
% 75 layout. Empty list never returned: caller is only invoked when a
% mirror_url is configured (which the rest of the build pipeline already
% requires).

builder:mirror_urls_for_file(Layout, Filename, URLs) :-
  findall(URL,
          ( config:mirror_url(MirrorUrl),
            download:mirror_download_url(MirrorUrl, Layout, Filename, URL)
          ),
          URLs0),
  list_to_set(URLs0, URLs).


% -----------------------------------------------------------------------------
%  Speed tracking (dynamic state for per-file speed calculation)
% -----------------------------------------------------------------------------

:- dynamic builder:dl_prev_snapshot/3.

%! builder:init_speed_tracking(+DlJobs, +T0, +FileStartLine) is det.

builder:init_speed_tracking([], _, _).

builder:init_speed_tracking([dl_job(_, Idx, _, _, _, _, _, _)|Rest], T0, FileStartLine) :-
  Key is FileStartLine + Idx,
  retractall(builder:dl_prev_snapshot(Key, _, _)),
  assertz(builder:dl_prev_snapshot(Key, 0, T0)),
  builder:init_speed_tracking(Rest, T0, FileStartLine).


%! builder:compute_speed(+Key, +CurrentSize, -Speed) is det.
%
% Compute download speed in bytes/sec using delta from last snapshot.
% Key is FileStartLine + FileIdx (unique across concurrent downloads).

builder:compute_speed(Key, CurrentSize, Speed) :-
  get_time(Now),
  ( builder:dl_prev_snapshot(Key, PrevSize, PrevTime)
  -> Delta is CurrentSize - PrevSize,
     Dt is Now - PrevTime,
     ( Dt > 0.1, Delta > 0
     -> Speed is round(Delta / Dt),
        retractall(builder:dl_prev_snapshot(Key, _, _)),
        assertz(builder:dl_prev_snapshot(Key, CurrentSize, Now))
     ;  Speed = 0
     )
  ;  Speed = 0,
     assertz(builder:dl_prev_snapshot(Key, CurrentSize, Now))
  ).


% -----------------------------------------------------------------------------
%  Poll loop
% -----------------------------------------------------------------------------

%! builder:poll_download_loop(+DlJobs, +TotalLines, +FileStartLine, +Distdir, -FailCount) is det.
%
% Poll all active downloads until none remain. Returns the total
% number of failed downloads. Updates file sub-slot display with
% percentage and speed on each iteration.

builder:poll_download_loop([], _, _, _, 0) :- !.

builder:poll_download_loop(ActiveJobs, TotalLines, FileStartLine, Distdir, TotalFails) :-
  builder:poll_all_jobs(ActiveJobs, TotalLines, FileStartLine, Distdir, StillActive, BatchFails),
  ( StillActive == []
  -> TotalFails = BatchFails
  ;  sleep(0.25),
     builder:poll_download_loop(StillActive, TotalLines, FileStartLine, Distdir, MoreFails),
     TotalFails is BatchFails + MoreFails
  ).


%! builder:poll_all_jobs(+Jobs, +TotalLines, +FileStartLine, +Distdir, -StillActive, -Fails) is det.

builder:poll_all_jobs([], _, _, _, [], 0).

builder:poll_all_jobs([Job|Rest], TotalLines, FileStartLine, Distdir, StillActive, Fails) :-
  Job = dl_job(Pid, FileIdx, Filename, ExpSize, Pairs, DestPath, Repo, Entry),
  ( download:check_process_done(Pid, ExitCode)
  -> builder:finalize_download(ExitCode, FileIdx, Filename, ExpSize, Pairs, DestPath, Repo, Entry,
                                TotalLines, FileStartLine, Distdir, OK),
     builder:poll_all_jobs(Rest, TotalLines, FileStartLine, Distdir, StillActive, RestFails),
     ( OK == true -> Fails = RestFails ; Fails is RestFails + 1 )
  ;  builder:update_download_progress(FileIdx, Filename, ExpSize, DestPath,
                                       TotalLines, FileStartLine, Distdir),
     StillActive = [Job|MoreActive],
     builder:poll_all_jobs(Rest, TotalLines, FileStartLine, Distdir, MoreActive, Fails)
  ).


%! builder:finalize_download(+ExitCode, +FileIdx, +Filename, +ExpSize, +Pairs, +DestPath, +Repo, +Entry, +TotalLines, +FileStartLine, +Distdir, -OK) is det.
%
% Called when a curl process exits. Verifies the per-process temp
% download (see prepare_download_jobs) and atomic-renames it onto
% DestPath when verification succeeds. On any failure the temp path
% is deleted but DestPath is left intact -- a parallel writer (sibling
% external build harness session, host emerge in compare matrices,
% etc.) may have landed a valid copy there in the meantime, in which case
% race_recover/3 short-circuits success without retrying. If neither
% our temp nor a peer-supplied DestPath verifies, falls through to
% the upstream SRC_URI fallback chain.

builder:finalize_download(ExitCode, FileIdx, Filename, ExpSize, Pairs, DestPath, Repo, Entry,
                           TotalLines, FileStartLine, Distdir, OK) :-
  download:tmp_dest_path(DestPath, TmpPath),
  ( ExitCode =:= 0
  -> download:finalize_temp_download(TmpPath, DestPath, ExpSize, Pairs, MirrorOK)
  ;  catch(delete_file(TmpPath), _, true),
     ( download:race_recover(DestPath, ExpSize, Pairs)
     -> MirrorOK = true
     ;  MirrorOK = false
     )
  ),
  ( MirrorOK == true
  -> OK = true,
     with_mutex(build_display,
       build:update_file_subslot(FileIdx, FileStartLine, TotalLines, done, Filename, ExpSize, Distdir))
  ;  builder:try_upstream_fallback(FileIdx, Filename, ExpSize, Pairs, DestPath, Repo, Entry,
                                    TotalLines, FileStartLine, Distdir, OK)
  ).


%! builder:try_upstream_fallback(+FileIdx, +Filename, +ExpSize, +Pairs, +DestPath, +Repo, +Entry, +TotalLines, +FileStartLine, +Distdir, -OK) is det.
%
% Attempts to download a distfile from its upstream SRC_URI peers when
% the Gentoo distfiles mirror has failed. Walks every URL yielded by
% download:upstream_url/4 (canonical mirror:// expansions first, then
% direct URIs) and stops at the first success that also passes size +
% checksum verification. This matches Portage's behaviour: when the
% Gentoo mirror prunes a distfile, the original upstream and its
% thirdpartymirror peers usually still serve it.

builder:try_upstream_fallback(FileIdx, Filename, ExpSize, Pairs, DestPath, Repo, Entry,
                               TotalLines, FileStartLine, Distdir, OK) :-
  findall(U, download:upstream_url(Repo, Entry, Filename, U), URLs0),
  list_to_set(URLs0, URLs),
  ( URLs == []
  -> OK = false,
     with_mutex(build_display,
       build:update_file_subslot(FileIdx, FileStartLine, TotalLines, failed, Filename, ExpSize, Distdir))
  ;  with_mutex(build_display,
       build:update_file_subslot(FileIdx, FileStartLine, TotalLines, progress(0, 0), Filename, ExpSize, Distdir)),
     builder:try_url_chain(URLs, FileIdx, Filename, ExpSize, Pairs, DestPath,
                           TotalLines, FileStartLine, Distdir, OK)
  ).


%! builder:try_url_chain(+URLs, +FileIdx, +Filename, +ExpSize, +Pairs, +DestPath, +TotalLines, +FileStartLine, +Distdir, -OK) is det.
%
% Walk a list of candidate download URLs in order. Each curl writes
% into the same per-process temp path used by the mirror download
% (see download:tmp_dest_path/2); on success the temp is atomic-
% renamed onto DestPath. Stops at the first URL whose curl exits 0
% AND whose temp passes size + checksum verification, OR at the
% first race_recover/3 success (peer writer landed a valid file at
% DestPath while we were trying). If every URL fails, the file is
% marked as failed -- DestPath itself is never deleted.

builder:try_url_chain([], FileIdx, Filename, ExpSize, _Pairs, _DestPath,
                      TotalLines, FileStartLine, Distdir, false) :-
  with_mutex(build_display,
    build:update_file_subslot(FileIdx, FileStartLine, TotalLines, failed, Filename, ExpSize, Distdir)).

builder:try_url_chain([URL|Rest], FileIdx, Filename, ExpSize, Pairs, DestPath,
                      TotalLines, FileStartLine, Distdir, OK) :-
  download:tmp_dest_path(DestPath, TmpPath),
  catch(delete_file(TmpPath), _, true),
  download:curl_download(URL, TmpPath, ExitCode),
  ( ExitCode =:= 0
  -> download:finalize_temp_download(TmpPath, DestPath, ExpSize, Pairs, AttemptOK)
  ;  catch(delete_file(TmpPath), _, true),
     ( download:race_recover(DestPath, ExpSize, Pairs)
     -> AttemptOK = true
     ;  AttemptOK = false
     )
  ),
  ( AttemptOK == true
  -> OK = true,
     with_mutex(build_display,
       build:update_file_subslot(FileIdx, FileStartLine, TotalLines, done, Filename, ExpSize, Distdir))
  ;  builder:try_url_chain(Rest, FileIdx, Filename, ExpSize, Pairs, DestPath,
                           TotalLines, FileStartLine, Distdir, OK)
  ).


% -----------------------------------------------------------------------------
%  RESTRICT=fetch handling
% -----------------------------------------------------------------------------

%! builder:handle_restricted_files(+DistFiles, +Idx, +TotalLines, +FileStartLine, +Distdir, -MissingCount) is det.
%
% For fetch-restricted ebuilds, checks each distfile: present files get
% a green checkmark, missing files get a yellow "manual fetch required" marker.

builder:handle_restricted_files([], _, _, _, _, 0).

builder:handle_restricted_files([dist(Filename, Size, _)|Rest], Idx, TotalLines, FileStartLine, Distdir, MissingCount) :-
  Idx1 is Idx + 1,
  ( mirror:flat_present(Distdir, Filename)
  -> with_mutex(build_display,
       build:update_file_subslot(Idx, FileStartLine, TotalLines, done, Filename, Size, Distdir)),
     builder:handle_restricted_files(Rest, Idx1, TotalLines, FileStartLine, Distdir, MissingCount)
  ;  with_mutex(build_display,
       build:update_file_subslot(Idx, FileStartLine, TotalLines, restricted, Filename, Size, Distdir)),
     builder:handle_restricted_files(Rest, Idx1, TotalLines, FileStartLine, Distdir, RestMissing),
     MissingCount is RestMissing + 1
  ).


%! builder:update_download_progress(+FileIdx, +Filename, +ExpSize, +DestPath, +TotalLines, +FileStartLine, +Distdir) is det.
%
% Update a file sub-slot with current download progress (percentage + speed).
%
% Curl writes to the per-process temp path (see prepare_download_jobs);
% if the temp file exists we use its size for live progress and fall
% back to the final DestPath only after a successful atomic rename.

builder:update_download_progress(FileIdx, Filename, ExpSize, DestPath,
                                  TotalLines, FileStartLine, Distdir) :-
  SpeedKey is FileStartLine + FileIdx,
  download:tmp_dest_path(DestPath, TmpPath),
  ( exists_file(TmpPath) -> SizeSrc = TmpPath
  ; exists_file(DestPath) -> SizeSrc = DestPath
  ; SizeSrc = none
  ),
  ( SizeSrc \== none
  -> size_file(SizeSrc, CurrentSize),
     ( atom(ExpSize) -> atom_number(ExpSize, ES) ; ES = ExpSize ),
     ( ES > 0 -> Pct is min(99, (CurrentSize * 100) // ES) ; Pct = 0 ),
     builder:compute_speed(SpeedKey, CurrentSize, Speed)
  ;  Pct = 0, Speed = 0
  ),
  with_mutex(build_display,
    build:update_file_subslot(FileIdx, FileStartLine, TotalLines, progress(Pct, Speed), Filename, ExpSize, Distdir)).


% =============================================================================
%  Result handling (main thread, display callback)
% =============================================================================

%! builder:handle_result(+TotalLines, +LineOff, +Outcome) is det.
%
% Called by jobserver:collect for each completed job. Updates the
% display slot and records the outcome for tallying.
% display_handled(Outcome) means the worker already updated the display.

builder:handle_result(_TotalLines, LineOff, display_handled(Outcome)) :-
  !,
  assertz(builder:slot_outcome(LineOff, Outcome)),
  builder:maybe_record_resume_done(LineOff, Outcome).

builder:handle_result(TotalLines, LineOff, Outcome) :-
  assertz(builder:slot_outcome(LineOff, Outcome)),
  builder:get_slot_info(LineOff, PlanStep, NumSteps, ActionIdx, Action, Entry),
  builder:outcome_to_status(Outcome, Status),
  with_mutex(build_display,
    build:update_slot(LineOff, TotalLines, Status, PlanStep, NumSteps, ActionIdx, Action, Entry)),
  builder:maybe_record_resume_done(LineOff, Outcome).


%! builder:maybe_record_resume_done(+LineOff, +Outcome) is det.
%
% Records a completed entry for resume tracking. Only records
% repository entries (Repo://Entry pattern) with done outcome.

builder:maybe_record_resume_done(LineOff, Outcome) :-
  ( Outcome == done,
    builder:slot_info(LineOff, _, _, _, Action, Entry),
    Entry = _://_
  -> assertz(builder:resume_done(Entry, Action))
  ;  true
  ).


%! builder:outcome_to_status(+Outcome, -Status) is det.

builder:outcome_to_status(done, done) :- !.
builder:outcome_to_status(stub, stub) :- !.
builder:outcome_to_status(failed(Reason), failed(Reason)) :- !.
builder:outcome_to_status(failed, failed('error')) :- !.
builder:outcome_to_status(error(E), failed(E)) :- !.
builder:outcome_to_status(_, failed('unknown')).


% =============================================================================
%  Tally
% =============================================================================

%! builder:tally_outcomes(+C0, +F0, +S0, -C, -F, -S) is det.
%
% Count recorded outcomes from the last step.

builder:tally_outcomes(C0, F0, S0, C, F, S) :-
  aggregate_all(count, builder:slot_outcome(_, done), DC),
  aggregate_all(count, builder:slot_outcome(_, stub), SC),
  aggregate_all(count, (builder:slot_outcome(_, O),
                         O \= done, O \= stub), FC),
  C is C0 + DC,
  F is F0 + FC,
  S is S0 + SC.


% =============================================================================
%  Resume state management
% =============================================================================

%! builder:resume_state_file(-Path) is det.
%
% Returns the path to the resume state file (Knowledge/resume.pl).

builder:resume_state_file(Path) :-
  config:installation_dir(Dir),
  os:compose_path([Dir, 'Knowledge', 'resume.pl'], Path).


%! builder:save_resume_state(+Goals, +Plan) is det.
%
% Saves the build goals and plan to Knowledge/resume.pl. This is
% called at the start of a --build run so the plan can be loaded
% later by --resume.

builder:save_resume_state(Goals, Plan) :-
  builder:resume_state_file(Path),
  catch(
    setup_call_cleanup(
      open(Path, write, S),
      ( write_term(S, resume_goals(Goals), [quoted(true)]),
        format(S, '.~n', []),
        write_term(S, resume_plan(Plan), [quoted(true)]),
        format(S, '.~n', [])
      ),
      close(S)),
    _, true).


%! builder:flush_resume_done_to_disk is det.
%
% Appends any in-memory resume_done/2 facts to the resume state file,
% then retracts them. Called after each plan step for crash safety.

builder:flush_resume_done_to_disk :-
  builder:resume_state_file(Path),
  ( exists_file(Path)
  -> findall(E-A, builder:resume_done(E, A), Entries),
     ( Entries \= []
     -> catch(
          setup_call_cleanup(
            open(Path, append, S),
            forall(
              member(E-A, Entries),
              ( write_term(S, resume_done(E, A), [quoted(true)]),
                format(S, '.~n', [])
              )
            ),
            close(S)),
          _, true),
        retractall(builder:resume_done(_, _))
     ;  true
     )
  ;  true
  ).


%! builder:load_resume_state(-Goals, -Plan, -DoneList) is semidet.
%
% Loads the resume state from Knowledge/resume.pl. Returns the
% original goals, plan, and a list of done(Entry, Action) terms
% for entries that already completed. Fails if no resume file exists.

builder:load_resume_state(Goals, Plan, DoneList) :-
  builder:resume_state_file(Path),
  exists_file(Path),
  catch(
    setup_call_cleanup(
      open(Path, read, S),
      builder:read_all_resume_terms(S, Terms),
      close(S)),
    _, fail),
  ( memberchk(resume_goals(Goals), Terms) -> true ; Goals = [] ),
  ( memberchk(resume_plan(Plan), Terms) -> true ; Plan = [] ),
  findall(done(E, A), member(resume_done(E, A), Terms), DoneList).


%! builder:read_all_resume_terms(+Stream, -Terms) is det.
%
% Reads all Prolog terms from a stream until end_of_file.

builder:read_all_resume_terms(S, Terms) :-
  read_term(S, T, []),
  ( T == end_of_file
  -> Terms = []
  ;  Terms = [T|Rest],
     builder:read_all_resume_terms(S, Rest)
  ).


%! builder:clear_resume_state is det.
%
% Deletes the resume state file after a successful build.

builder:clear_resume_state :-
  builder:resume_state_file(Path),
  ( exists_file(Path) -> delete_file(Path) ; true ).


%! builder:filter_completed_plan(+Plan, +DoneList, -FilteredPlan) is det.
%
% Removes completed rules from each step in the plan. A rule is
% considered completed if its Entry and Action appear in DoneList.

builder:filter_completed_plan([], _, []).

builder:filter_completed_plan([Step|Rest], DoneList, [Filtered|FilteredRest]) :-
  exclude(builder:rule_is_done(DoneList), Step, Filtered),
  builder:filter_completed_plan(Rest, DoneList, FilteredRest).


%! builder:rule_is_done(+DoneList, +Rule) is semidet.
%
% True if the rule's package and action appear in the done list.

builder:rule_is_done(DoneList, rule(Repo://Entry:Action?{_Ctx}, _Body)) :-
  memberchk(done(Repo://Entry, Action), DoneList).


%! builder:collect_skip_entries(+Plan, -SkipDone) is det.
%
% Collects done/2 entries for rules whose Entry matches any
% config:skip_atom/1 fact. Matches by sub_atom so the user can
% specify a qualified name like dev-lang/python-3.12.0 and it
% will match the full Entry atom in the plan.

builder:collect_skip_entries(Plan, SkipDone) :-
  findall(done(Repo://Entry, Action),
    ( member(Step, Plan),
      member(rule(Repo://Entry:Action?{_Ctx}, _Body), Step),
      config:skip_atom(Skip),
      sub_atom(Entry, _, _, _, Skip)
    ),
    SkipDone).


% =============================================================================
%  Snapshot integration
% =============================================================================

%! builder:maybe_create_snapshot(+Plan) is det.
%
% If --snapshot is active (snapshot:active_id/1 has been asserted by
% interface dispatch), creates a snapshot before the build begins.

builder:maybe_create_snapshot(Plan) :-
  ( snapshot:active_id(Id)
  -> snapshot:create(Id, Plan)
  ;  true
  ).


%! builder:maybe_quickpkg_old(+Action, +Ctx) is det.
%
% When a snapshot is active and the action replaces an installed
% package, quickpkg the old version before the merge phase overwrites it.

builder:maybe_quickpkg_old(Action, Ctx) :-
  snapshot:active_id(_),
  memberchk(Action, [install, update, downgrade, reinstall]),
  memberchk(replaces(OldRepo://OldEntry), Ctx),
  !,
  snapshot:quickpkg_old(OldRepo, OldEntry).

builder:maybe_quickpkg_old(_, _).


% =============================================================================
%  World action execution (stubs)
% =============================================================================

%! builder:execute_world(+Op, +Arg) is det.
%
% Stub for world set modifications. Currently a no-op; future
% implementation will call world:register/1 or world:unregister/1.

builder:execute_world(register, _Arg).
builder:execute_world(unregister, _Arg).


% =============================================================================
%  Suggestion dispatch (auto-config writers)
% =============================================================================
%
% The prover tags fully-resolved literals with three flavors of suggestion:
%
%   suggestion(unmask, Repo://Entry)
%   suggestion(accept_keyword, Kw)
%   suggestion(use_change, Repo://Entry, Changes)
%
% These describe the /etc/portage overrides the prover had to assume in
% order to build a working plan (e.g. picking apache2_mpms_event to
% satisfy a `|| (...)` REQUIRED_USE group, or accepting ~amd64 for a
% candidate ebuild). For the current build the overrides are already
% applied via the proof context (ebuild_exec:apply_ctx_use_overrides), so
% this layer's job is purely persistence: write them into
%
%   $portage_confdir/package.{unmask,accept_keywords,use}/00portage-ng-auto
%
% so that subsequent builds (in this session OR later sessions) start
% from the same configured state. The file format is the standard
% Portage one; lines are line-deduped so retries never accumulate
% duplicates. After every successful write, userconfig:load is invoked
% so the next ebuild in the same build sees the freshly persisted
% override without restarting.
%
% No-op when config:portage_confdir/1 is unset (e.g. dev-Mac setup that
% relies on Source/Domain/Gentoo/Preference/fallback.pl).
%
% Thread-safe: a global mutex (build_auto_config) serializes the
% read-modify-write cycle across parallel build workers.

:- mutex_create(build_auto_config).


%! builder:dispatch_suggestions(+Repo, +Entry, +Ctx) is det.
%
% Persists suggestions tagged on the (Repo, Entry, Ctx) literal. Repo
% and Entry are the outer literal's identifier; the proof-context
% suggestion(unmask, ...) and suggestion(use_change, ..., ...) terms
% may carry their own Repo://Entry, but that's typically a candidate
% the prover is *introducing*, distinct from the outer literal.

builder:dispatch_suggestions(Repo, Entry, Ctx) :-
  is_list(Ctx), !,
  ( memberchk(suggestion(unmask, UR://UE), Ctx)
  -> builder:execute_suggestion(unmask, UR, UE)
  ;  true
  ),
  ( memberchk(suggestion(accept_keyword, Kw), Ctx)
  -> ( memberchk(suggestion(unmask, KR://KE), Ctx)
     -> builder:execute_suggestion(accept_keyword, KR, KE, Kw)
     ;  builder:execute_suggestion(accept_keyword, Repo, Entry, Kw)
     )
  ;  true
  ),
  ( memberchk(suggestion(use_change, UCR://UCE, Changes), Ctx)
  -> builder:execute_suggestion(use_change, UCR, UCE, Changes)
  ;  true
  ).

builder:dispatch_suggestions(_, _, _).


%! builder:execute_suggestion(+Type, +Repo, +Entry) is det.
%! builder:execute_suggestion(+Type, +Repo, +Entry, +Arg) is det.
%
% Persist a single prover suggestion to the corresponding /etc/portage
% package.* file. Each handler is defensive: invalid arguments, missing
% portage_confdir, or unwritable confdir all silently no-op rather than
% break the build.

builder:execute_suggestion(unmask, Repo, Entry) :-
  ( builder:auto_confdir(ConfDir),
    builder:atom_for_entry(Repo, Entry, Atom)
  -> builder:write_auto_config(ConfDir, 'package.unmask', Atom)
  ;  true
  ).


builder:execute_suggestion(accept_keyword, Repo, Entry, Kw) :-
  ( builder:auto_confdir(ConfDir),
    builder:atom_for_entry(Repo, Entry, Atom),
    builder:keyword_token(Kw, KwTok)
  -> atomic_list_concat([Atom, ' ', KwTok], Line),
     builder:write_auto_config(ConfDir, 'package.accept_keywords', Line)
  ;  true
  ).


builder:execute_suggestion(use_change, Repo, Entry, Changes) :-
  ( builder:auto_confdir(ConfDir),
    builder:atom_for_entry(Repo, Entry, Atom),
    builder:format_use_changes(Changes, FlagsAtom),
    FlagsAtom \== ''
  -> atomic_list_concat([Atom, ' ', FlagsAtom], Line),
     builder:write_auto_config(ConfDir, 'package.use', Line)
  ;  true
  ).


% -----------------------------------------------------------------------------
%  Auto-config helpers
% -----------------------------------------------------------------------------

%! builder:auto_confdir(-Dir) is semidet.
%
% Succeed with the configured /etc/portage directory; fail if no
% portage_confdir is configured (e.g. development setups that rely on
% Source/Domain/Gentoo/Preference/fallback.pl).

builder:auto_confdir(Dir) :-
  current_predicate(config:portage_confdir/1),
  config:portage_confdir(Dir).


%! builder:atom_for_entry(+Repo, +Entry, -Atom) is semidet.
%
% Build an exact-version Portage atom (=cat/name-version) from a
% repo entry. Fails when the entry can't be resolved.
%
% Version comes back from cache:ordered_entry/5 as a `version/7`
% compound term whose 7th argument is the canonical version string;
% see Source/Domain/Gentoo/version.pl. We extract that here.

builder:atom_for_entry(Repo, Entry, Atom) :-
  cache:ordered_entry(Repo, Entry, Cat, Name, Version),
  atom(Cat), atom(Name),
  builder:version_atom(Version, VAtom),
  VAtom \== '',
  atomic_list_concat(['=', Cat, '/', Name, '-', VAtom], Atom).


%! builder:version_atom(+Version, -Atom) is det.
%
% Project a version representation (`version/7` compound, atom, or
% `version_none`) onto the canonical version string used in Portage
% atoms. Mirrors plan:version_string/2.

builder:version_atom(version(_,_,_,_,_,_,Full), Atom) :- !,
  ( atom(Full) -> Atom = Full ; format(atom(Atom), '~w', [Full]) ).
builder:version_atom(version_none, '') :- !.
builder:version_atom(V, V) :- atom(V), !.
builder:version_atom(V, A) :- format(atom(A), '~w', [V]).


%! builder:format_use_changes(+Changes, -Atom) is det.
%
% Convert a list of use_change(Flag, enable/disable) terms into a
% portage-style flag list ("flag1 -flag2 flag3"). Empty atom if no
% supported changes are present.

builder:format_use_changes(Changes, Atom) :-
  findall(Tok,
    ( member(use_change(F, Dir), Changes),
      atom(F),
      ( Dir == enable  -> Tok = F
      ; Dir == disable -> atom_concat('-', F, Tok)
      )
    ),
    Toks),
  ( Toks == []
  -> Atom = ''
  ;  atomic_list_concat(Toks, ' ', Atom)
  ).


%! builder:keyword_token(+Kw, -Atom) is semidet.
%
% Normalize a keyword argument from the prover (atom, string, or term
% like keyword(Arch)) to a single atom suitable for the second column
% of a package.accept_keywords line.

builder:keyword_token(Kw, Atom) :- atom(Kw), !, Atom = Kw.
builder:keyword_token(Kw, Atom) :- string(Kw), !, atom_string(Atom, Kw).
builder:keyword_token(keyword(K), Atom) :- !, builder:keyword_token(K, Atom).
builder:keyword_token(Kw, Atom) :- format(atom(Atom), '~w', [Kw]).


%! builder:write_auto_config(+ConfDir, +SubDir, +Line) is det.
%
% Idempotently append `Line` to ConfDir/SubDir/00portage-ng-auto. On
% first use, creates the SubDir (if missing) and writes a clarifying
% header. Skips when the line is already present (whitespace- and
% trailing-comment-tolerant compare). After writing, reload userconfig
% so the next ebuild in this build picks up the override.
%
% Safe to call from parallel workers; serialized via build_auto_config.

builder:write_auto_config(ConfDir, SubDir, Line) :-
  catch(
    with_mutex(build_auto_config,
      builder:write_auto_config_locked(ConfDir, SubDir, Line)),
    Err,
    ( print_message(warning,
        format('portage-ng: failed to persist ~w override "~w": ~w', [SubDir, Line, Err])),
      true
    )).

builder:write_auto_config_locked(ConfDir, SubDir, Line) :-
  atomic_list_concat([ConfDir, '/', SubDir], DirPath),
  ( exists_directory(DirPath) -> true ; make_directory_path(DirPath) ),
  atomic_list_concat([DirPath, '/00portage-ng-auto'], Path),
  ( builder:line_already_present(Path, Line)
  -> true
  ;  builder:append_with_header(Path, SubDir, Line),
     catch(userconfig:load, _, true)
  ).


%! builder:line_already_present(+Path, +Line) is semidet.
%
% True if Path exists and contains Line (after stripping trailing
% comments and surrounding whitespace from each existing line).

builder:line_already_present(Path, Line) :-
  exists_file(Path),
  read_file_to_string(Path, S, []),
  split_string(S, "\n", "", Lines),
  member(L0, Lines),
  builder:strip_comment_and_trim(L0, LStr),
  LStr \== "",
  atom_string(LAtom, LStr),
  LAtom == Line, !.


%! builder:strip_comment_and_trim(+Line0, -Line) is det.
%
% Drop `# ...` trailing comments and surrounding whitespace.

builder:strip_comment_and_trim(L0, L) :-
  ( sub_string(L0, Before, _, _, "#")
  -> sub_string(L0, 0, Before, _, L1)
  ;  L1 = L0
  ),
  split_string(L1, "", " \t\r", [L]).


%! builder:append_with_header(+Path, +SubDir, +Line) is det.
%
% Append Line to Path, creating Path with a clear "auto-managed" header
% on first use. Header explains the file's purpose and gives the user
% an out (delete or edit). SubDir is included so the header is unique
% per file (helps when grepping /etc/portage).

builder:append_with_header(Path, SubDir, Line) :-
  ( exists_file(Path) -> true
  ;  setup_call_cleanup(
       open(Path, write, S),
       format(S,
         '# Auto-managed by portage-ng (~w/00portage-ng-auto).~n~c Lines below are persisted prover suggestions: REQUIRED_USE picks,~n~c keyword acceptance, mask overrides, etc. Safe to edit or delete;~n~c missing entries will be re-derived on the next --build invocation.~n',
         [SubDir, 0'#, 0'#, 0'#]),
       close(S))
  ),
  setup_call_cleanup(
    open(Path, append, S2),
    format(S2, '~w~n', [Line]),
    close(S2)).


% =============================================================================
%  Builder test stats (whole-repo and targeted)
% =============================================================================

%! builder:test_stats(+Repository) is det.
%
% Run a builder test across the entire repository: for each entry,
% prove a plan, download distfiles, and run safe build phases.
% Uses tester:test for parallel iteration with progress.

builder:test_stats(Repository) :-
  config:test_style(Style),
  builder:test_stats(Repository, Style).


%! builder:test_stats(+Repository, +StyleOrTopN) is det.

builder:test_stats(Repository, TopN) :-
  integer(TopN), !,
  config:test_style(Style),
  builder:test_stats(Repository, Style, TopN).

builder:test_stats(Repository, Style) :-
  ( config:test_stats_top_n(TopN) -> true ; TopN = 25 ),
  builder:test_stats(Repository, Style, TopN).


%! builder:test_stats(+Repository, +Style, +TopN) is det.
%
% Core test loop: for each entry, prove plan, download distfiles,
% run safe phases. Failure at any stage is recorded via the sampler.

builder:test_stats(Repository, Style, TopN) :-
  aggregate_all(count, (Repository:entry(_E)), ExpectedTotal),
  sampler:reset('Building', ExpectedTotal),
  aggregate_all(count, (Repository:package(_C,_N)), ExpectedPkgs),
  sampler:set_expected_pkgs(ExpectedPkgs),
  tester:test(Style,
              'Building',
              Repository://Entry,
              Repository:entry(Entry),
              builder:test_single(Repository, Entry)),
  stats:test_stats_print(TopN).


%! builder:test_stats_pkgs(+Repository, +Pkgs) is det.
%
% Run builder test for a specific list of packages (C-N pairs).

builder:test_stats_pkgs(Repository, Pkgs) :-
  config:test_style(Style),
  ( config:test_stats_top_n(TopN) -> true ; TopN = 25 ),
  builder:test_stats_pkgs(Repository, Style, TopN, Pkgs).


%! builder:test_stats_pkgs(+Repository, +Style, +TopN, +Pkgs) is det.

builder:test_stats_pkgs(Repository, Style, TopN, Pkgs) :-
  is_list(Pkgs),
  length(Pkgs, ExpectedTotal),
  sampler:reset('Building', ExpectedTotal),
  sampler:set_expected_pkgs(ExpectedTotal),
  tester:test(Style,
              'Building',
              Repository://Entry,
              ( member(C-N, Pkgs),
                once(Repository:ebuild(Entry, C, N, _))
              ),
              builder:test_single(Repository, Entry)),
  stats:test_stats_print(TopN).


% =============================================================================
%  Per-entry test (prove + download + safe phases)
% =============================================================================

%! builder:test_single(+Repository, +Entry) is det.
%
% Test a single entry end-to-end without display:
%   1. Prove plan via prove_plan_with_fallback (canonical 5-tier fallback)
%   2. Download distfiles (skips already-present, verifies hashes)
%   3. Run safe build phases via ebuild_exec
%
% Succeeds if all steps complete, otherwise records failure via sampler
% and succeeds (tester:test handles outer error classification).

builder:test_single(Repository, Entry) :-
  sampler:reset_counters,
  statistics(inferences, I0),
  statistics(walltime, [T0, _]),
  Goals = [Repository://Entry:run?{[]}],
  ( pipeline:prove_plan_with_fallback(Goals, _Proof, _Model, Plan, _Triggers)
  -> catch(builder:test_plan_downloads(Plan), DlErr,
       with_mutex(mutex,
         (term_to_atom(DlErr, DA), message:warning([Entry, ' download error: ', DA])))),
     catch(builder:test_plan_phases(Plan), PhErr,
       with_mutex(mutex,
         (term_to_atom(PhErr, PA), message:warning([Entry, ' build error: ', PA])))),
     statistics(walltime, [T1, _]),
     statistics(inferences, I1),
     TimeMs is T1 - T0,
     Inferences is I1 - I0,
     sampler:counters(rule_calls(RuleCalls)),
     sampler:record(costs(Repository://Entry, TimeMs, Inferences, RuleCalls))
  ;  ( current_predicate(sampler:record/1)
     -> sampler:record(failed(other))
     ;  true
     )
  ).


%! builder:test_plan_downloads(+Plan) is det.
%
% Walk the plan and download distfiles for all download/fetchonly rules.
% Uses download:fetch_distfiles/4 which handles mirror URLs, fallback,
% hash verification, and skipping already-present files.

builder:test_plan_downloads([]).

builder:test_plan_downloads([Step|Rest]) :-
  builder:test_step_downloads(Step),
  builder:test_plan_downloads(Rest).


%! builder:test_step_downloads(+Step) is det.

builder:test_step_downloads([]).

builder:test_step_downloads([Rule|Rest]) :-
  ( Rule = rule(Head, _Body),
    prover:canon_literal(Head, Core, Ctx),
    Core = Repo://Entry:Action,
    memberchk(Action, [download, fetchonly]),
    \+ download:is_fetch_restricted(Repo, Entry),
    ( predicate_property(ebuild:is_live(_), defined) -> \+ ebuild:is_live(Repo://Entry) ; true )
  -> ( download:fetch_distfiles(Repo, Entry, Ctx, Failures),
       ( Failures == [] -> true
       ; term_to_atom(Failures, FA),
         with_mutex(mutex, message:warning([Entry, ' download failures: ', FA]))
       )
     ; true
     )
  ;  true
  ),
  builder:test_step_downloads(Rest).


%! builder:test_plan_phases(+Plan) is det.
%
% Walk the plan and execute safe build phases for all non-download rules.
% Uses ebuild_exec:execute/5 when config:build_live_phases is non-empty.

builder:test_plan_phases([]).

builder:test_plan_phases([Step|Rest]) :-
  builder:test_step_phases(Step),
  builder:test_plan_phases(Rest).


%! builder:test_step_phases(+Step) is det.

builder:test_step_phases([]).

builder:test_step_phases([Rule|Rest]) :-
  ( Rule = rule(Head, _Body),
    prover:canon_literal(Head, Core, Ctx),
    Core = Repo://Entry:Action,
    \+ memberchk(Action, [download, fetchonly])
  -> ( config:build_live_phases(LP), LP \= [],
       predicate_property(ebuild_exec:execute(_,_,_,_,_), defined)
     -> ( ebuild_exec:execute(Action, Repo, Entry, Ctx, Outcome),
          ( Outcome == done -> true
          ; term_to_atom(Outcome, OA),
            with_mutex(mutex, message:warning([Entry, ' build phase outcome: ', OA]))
          )
        ; true
        )
     ;  true
     )
  ;  true
  ),
  builder:test_step_phases(Rest).
