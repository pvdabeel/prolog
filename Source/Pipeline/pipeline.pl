/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> PIPELINE
The Pipeline orchestrates the two core resolution stages — prover
(pass 1) and orderer (pass 2) — into a single entry point.

Architecture context:

  reader/parser  →  prover  →  orderer  →  printer
                    └───── pipeline ────┘

The pipeline sits between the parsing layer (reader + eapi grammar) and
the output layer (printer + writer).  It takes a list of proof goals and
returns a completed proof, model, ordered plan, and triggers AVL:

  prove_plan(+Goals, -ProofAVL, -ModelAVL, -Plan, -TriggersAVL)

Two canonical entry points with 5-tier progressive relaxation:
- prove_plan_with_fallback/5  — full pipeline (prove + order)
- prove_with_fallback/4       — prover only (for layered tests)

Callers:
- interface.pl  — interactive CLI proving  (--pretend / --merge)
- writer.pl     — batch file generation    (--graph)
- builder.pl    — build testing            (--build)
- bugs.pl       — bug report drafts        (--bugs)
- resolver.pl   — resolve tests            (resolver:test/1, test_stats)
- orderer.pl    — ordering tests           (orderer:test/1, test_stats)

Pipeline stages (both are thin wrappers handing the generic prover
their rule set — `resolving` and `ordering` respectively):
1. resolver:resolve/9 — pass 1: inductive proof search, builds
                        ProofAVL + ModelAVL (what — versions, USE, slots)
2. orderer:order/5    — pass 2: prove over the planning laws, project
                        the availability proofs to the wave plan (when)

Each stage is timed via sampler:phase_walltime and recorded via
sampler:phase_record for performance analysis.

Domain obligations (PDEPEND, sub-slot ABI rebuilds):
The pipeline is domain-agnostic; anything Gentoo-specific that must
extend a proof does so inside pass 1 through the prover's proof
obligation channel (heuristic:proof_obligation/4).  Post-dependencies
and sub-slot `:=` ABI rebuilds (abirebuild, portage-ng#89/#118) are both
resolved single-pass that way — proven and ordered like any other goal,
never patched into the plan afterwards.  The prove_plan_with_pdepend/5
variant provides an alternative multi-pass approach that delegates
PDEPEND goal extraction to dependency:pdepend_goals_from_plan/2 and
re-runs the pipeline with the extended goal set.  It is retained for
experimentation but not currently used in the default path.
*/

:- module(pipeline, []).


% =============================================================================
%  Core pipeline: prove + order
% =============================================================================

%! pipeline:prove_plan(+Goals, -ProofAVL, -ModelAVL, -Plan, -TriggersAVL)
%
% Standard entry point.  Proves Goals, then orders the proof into a
% wave-list Plan.

pipeline:prove_plan(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL) :-
  memo:clear_caches,
  pipeline:prove_plan_basic(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, _SCCs).


%! pipeline:prove_plan(+Goals, -ProofAVL, -ModelAVL, -Plan, -TriggersAVL, -SCCs)
%
% Same as prove_plan/5 but also returns the SCCs argument the printer's
% signature expects (always [] — the ordering engine builds no
% condensation).

pipeline:prove_plan(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, SCCs) :-
  memo:clear_caches,
  pipeline:prove_plan_basic(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, SCCs).


% -----------------------------------------------------------------------------
% Progressive relaxation: fallback tier table + generic driver
% -----------------------------------------------------------------------------

%! pipeline:fallback_tiers(-Tiers) is det
%
% The canonical 5-tier committed-choice relaxation ladder, in the order
% tiers are attempted.  'false' is the strict tier (no assumptions).
% Both prove_with_fallback/4 and prove_plan_with_fallback/6 derive their
% chain from this single table, so tier changes happen in one place.

pipeline:fallback_tiers([false,
                         keyword_acceptance,
                         blockers,
                         unmask,
                         keyword_unmask]).


%! pipeline:tier_goal(+Tier, +Goal, -Wrapped) is det
%
% Maps a tier name to the goal that proves under that tier, by wrapping
% Goal in the prover:assuming/2 flags the tier stands for.

pipeline:tier_goal(false,              Goal, Goal).
pipeline:tier_goal(keyword_acceptance, Goal, prover:assuming(keyword_acceptance, Goal)).
pipeline:tier_goal(blockers,           Goal, prover:assuming(blockers, Goal)).
pipeline:tier_goal(unmask,             Goal, prover:assuming(unmask, Goal)).
pipeline:tier_goal(keyword_unmask,     Goal, prover:assuming(keyword_acceptance,
                                               prover:assuming(unmask, Goal))).


%! pipeline:with_fallback(+Goal, -FallbackUsed) is semidet
%
% Generic committed-choice fallback driver.  Attempts Goal under each
% tier of fallback_tiers/1 in order, committing to the first tier that
% succeeds and unifying FallbackUsed with its name.  Fails when no tier
% succeeds (bindings of failed attempts are undone on backtracking).

pipeline:with_fallback(Goal, FallbackUsed) :-
  pipeline:fallback_tiers(Tiers),
  pipeline:with_fallback_tiers(Tiers, Goal, FallbackUsed).


%! pipeline:with_fallback_tiers(+Tiers, +Goal, -FallbackUsed) is semidet

pipeline:with_fallback_tiers([Tier|Rest], Goal, FallbackUsed) :-
  pipeline:tier_goal(Tier, Goal, Wrapped),
  ( call(Wrapped) ->
      FallbackUsed = Tier
  ; pipeline:with_fallback_tiers(Rest, Goal, FallbackUsed)
  ).


% -----------------------------------------------------------------------------
% Canonical fallback entry points
% -----------------------------------------------------------------------------

%! pipeline:prove_with_fallback(+Goals, -ProofAVL, -ModelAVL, -TriggersAVL) is semidet
%
% Proves Goals with progressive relaxation (prover only, no plan/schedule).
% Same committed-choice fallback chain (fallback_tiers/1) as
% prove_plan_with_fallback.  Clears memo caches and computes multislot
% initial constraints.
%
% Used by layered tests (resolver:test, orderer:test and their
% test_stats/test_latest variants) and by --bugs, so each stage
% exercises the same proving semantics as production.

pipeline:prove_with_fallback(Goals, ProofAVL, ModelAVL, TriggersAVL) :-
  memo:clear_caches,
  pipeline:multislot_initial_constraints(Goals, InitCons),
  pipeline:with_fallback(
    resolver:resolve(Goals, t, ProofAVL, t, ModelAVL, InitCons, _Constraints, t, TriggersAVL),
    _FallbackUsed).


%! pipeline:prove_plan_with_fallback(+Goals, -ProofAVL, -ModelAVL, -Plan, -TriggersAVL)
%
% Proves with progressive relaxation along the fallback_tiers/1 ladder:
% strict first, then keyword_acceptance, blockers, unmask, and finally
% both keyword_acceptance + unmask.  Used by both standalone and client
% paths so the fallback chain is consistent.

pipeline:prove_plan_with_fallback(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL) :-
  pipeline:prove_plan_with_fallback(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, _SCCs, _FallbackUsed).


%! pipeline:prove_plan_with_fallback(+Goals, -Proof, -Model, -Plan, -Triggers, -FallbackUsed)
%
% Same as prove_plan_with_fallback/5 but returns which relaxation tier
% was needed: false (strict), keyword_acceptance, blockers, unmask, or
% keyword_unmask.  Fails deterministically when all tiers fail.

pipeline:prove_plan_with_fallback(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, FallbackUsed) :-
  pipeline:prove_plan_with_fallback(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, _SCCs, FallbackUsed).


%! pipeline:prove_plan_with_fallback(+Goals, -Proof, -Model, -Plan, -Triggers, -SCCs, -FallbackUsed)
%
% Same as prove_plan_with_fallback/6 but additionally returns the SCCs
% argument plan-printing callers pass through to printer:print. Always
% [] — the ordering engine builds no condensation; the argument is
% retained for the printer's signature.

pipeline:prove_plan_with_fallback(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, SCCs, FallbackUsed) :-
  % Clear once for the whole ladder. Dep-model cache keys already encode
  % prover:assuming bits (query.pl), so sharing across tiers is sound and
  % avoids a cold re-resolve per tier (portage-ng#118).
  memo:clear_caches,
  pipeline:with_fallback(
    pipeline:prove_plan_basic(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, SCCs),
    FallbackUsed).


%! pipeline:prove_plan_basic(+Goals, -ProofAVL, -ModelAVL, -Plan, -TriggersAVL, -SCCs)
%
% Single-pass pipeline with per-stage wall-time instrumentation.
% Pre-injects selected_cn_allow_multislot constraints when the goal
% list contains multiple targets for the same Category-Name (different
% versions/slots). SCCs is always [] (retained printer argument).
% Does not clear memo caches; callers that need a fresh cache
% (prove_plan/5,6 and prove_plan_with_fallback/7) clear once themselves.

pipeline:prove_plan_basic(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, SCCs) :-
  sampler:phase_walltime(T0),
  pipeline:multislot_initial_constraints(Goals, InitCons),
  resolver:resolve(Goals, t, ProofAVL0, t, ModelAVL, InitCons, _Constraints, t, TriggersAVL),
  sampler:phase_walltime(T1),
  % Ordering pass: a second prover run over the generic planning laws.
  % ProofAVL gains the pass-2 unreachable/2 assumptions so the printer
  % reports them through the standard domain-assumption machinery.
  orderer:order(ProofAVL0, TriggersAVL, ProofAVL, Plan, SCCs),
  sampler:phase_walltime(T2),
  sampler:phase_record(T0, T1, T2).


% =============================================================================
%  Testing
% =============================================================================

%! pipeline:test(+Repository) is det
%
% Runs the full pipeline (prove + plan + schedule + print) for every entry
% in Repository. Same as printer:test/1.

pipeline:test(Repository) :-
  printer:test(Repository).


%! pipeline:test_stats(+Repository) is det
%
% Runs the full pipeline with statistics recording and Top-N report.
% Uses label 'Pipeline' for the stats output.

pipeline:test_stats(Repository) :-
  config:test_style(Style),
  pipeline:test_stats(Repository, Style).

%! pipeline:test_stats(+Repository, +Style) is det
%
% Same as pipeline:test_stats/1 with explicit Style.
% Uses prove_plan_with_fallback for full-pipeline proving with the
% canonical 5-tier fallback chain, then prints via printer:print/5.
%
% The sub-slot ABI rebuild obligations (portage-ng#89, abirebuild) are
% suspended for the duration so per-entry proving keeps its single-target
% semantics and speed.

pipeline:test_stats(Repository, Style) :-
  setup_call_cleanup(
    assertz(abirebuild:suspended),
    pipeline:test_stats_run(Repository, Style),
    retractall(abirebuild:suspended)).


%! pipeline:test_stats_run(+Repository, +Style) is det

pipeline:test_stats_run(Repository, Style) :-
  config:proving_target(Action),
  aggregate_all(count, (Repository:entry(_E)), ExpectedTotal),
  sampler:reset('Pipeline', ExpectedTotal),
  aggregate_all(count, (Repository:package(_C,_N)), ExpectedPkgs),
  sampler:set_expected_pkgs(ExpectedPkgs),
  tester:test(Style,
              'Pipeline',
              Repository://Entry,
              (Repository:entry(Entry)),
              ( pipeline:prove_plan_with_fallback([Repository://Entry:Action?{[]}],ProofAVL,ModelAVL,Plan,Triggers,SCCs,_FallbackUsed)
              ),
              ( sampler:record(entry(Repository://Entry, ModelAVL, ProofAVL, Triggers, false)),
                sampler:set_current_entry(Repository://Entry),
                printer:print([Repository://Entry:Action?{[]}],ModelAVL,ProofAVL,Plan,Triggers,SCCs),
                sampler:clear_current_entry
              ),
              false),
  stats:test_stats_print.


% =============================================================================
%  Multi-slot initial constraints
% =============================================================================

%! pipeline:multislot_initial_constraints(+Goals, -Constraints) is det.
%
% Scans the goal list for duplicate (Category, Name) pairs (different
% versions of the same package). For each such pair, pre-populates the
% constraint AVL with selected_cn_allow_multislot(C,N) so the prover
% permits per-slot selection instead of enforcing single-selection.

pipeline:multislot_initial_constraints(Goals, Constraints) :-
  pipeline:extract_goal_cns(Goals, CNs),
  msort(CNs, Sorted),
  pipeline:collect_duplicate_cns(Sorted, DupCNs),
  pipeline:build_multislot_avl(DupCNs, t, Constraints).


%! pipeline:extract_goal_cns(+Goals, -CNPairs) is det.

pipeline:extract_goal_cns([], []).

pipeline:extract_goal_cns([target(Q, _):_?{_}|Rest], [C-N|More]) :-
  once(query:search(Q, R://E)),
  query:search([category(C), name(N)], R://E),
  !,
  pipeline:extract_goal_cns(Rest, More).

pipeline:extract_goal_cns([_|Rest], More) :-
  pipeline:extract_goal_cns(Rest, More).


%! pipeline:collect_duplicate_cns(+Sorted, -Duplicates) is det.

pipeline:collect_duplicate_cns([], []).

pipeline:collect_duplicate_cns([CN, CN|Rest], [CN|More]) :-
  !,
  pipeline:skip_same_cn(CN, Rest, Rest1),
  pipeline:collect_duplicate_cns(Rest1, More).

pipeline:collect_duplicate_cns([_|Rest], More) :-
  pipeline:collect_duplicate_cns(Rest, More).


%! pipeline:skip_same_cn(+CN, +List, -Rest) is det.

pipeline:skip_same_cn(CN, [CN|Rest], Rest1) :-
  !,
  pipeline:skip_same_cn(CN, Rest, Rest1).

pipeline:skip_same_cn(_, Rest, Rest).


%! pipeline:build_multislot_avl(+DupCNs, +AVL0, -AVL) is det.

pipeline:build_multislot_avl([], AVL, AVL).

pipeline:build_multislot_avl([C-N|Rest], AVL0, AVL) :-
  put_assoc(selected_cn_allow_multislot(C,N), AVL0, true, AVL1),
  pipeline:build_multislot_avl(Rest, AVL1, AVL).


% =============================================================================
%  Extended pipeline with PDEPEND fixpoint
% =============================================================================

%! pipeline:prove_plan_with_pdepend(+Goals, -ProofAVL, -ModelAVL, -Plan, -TriggersAVL)
%
% Two-pass variant.  Runs the basic pipeline, extracts PDEPEND goals
% from merged entries in the resulting plan, and — if new goals were
% found — re-runs the pipeline with the extended goal set.
%
% Retained for experimentation; the default path uses prove_plan/5.

pipeline:prove_plan_with_pdepend(Goals0, ProofAVL, ModelAVL, Plan, TriggersAVL) :-
  memo:clear_caches,
  statistics(walltime, [T0,_]),
  pipeline:prove_plan_basic(Goals0, Proof0, Model0, Plan0, Trig0, _SCCs0),
  statistics(walltime, [T1,_]),
  Pass1Ms is T1 - T0,
  statistics(walltime, [T2,_]),
  dependency:pdepend_goals_from_plan(Plan0, PdependGoals),
  statistics(walltime, [T3,_]),
  ExtractMs is T3 - T2,
  ( PdependGoals == [] ->
      sampler:pdepend_perf_add(Pass1Ms, ExtractMs, 0, 0, 0),
      ProofAVL = Proof0, ModelAVL = Model0, Plan = Plan0, TriggersAVL = Trig0
  ; sort(Goals0, GoalsU),
    sort(PdependGoals, PdepU),
    subtract(PdepU, GoalsU, NewGoals),
    length(NewGoals, NewGoalsCount),
    ( NewGoals == [] ->
        sampler:pdepend_perf_add(Pass1Ms, ExtractMs, 0, 0, 0),
        ProofAVL = Proof0, ModelAVL = Model0, Plan = Plan0, TriggersAVL = Trig0
    ; append(Goals0, NewGoals, Goals1),
      statistics(walltime, [T4,_]),
      pipeline:prove_plan_basic(Goals1, ProofAVL, ModelAVL, Plan, TriggersAVL, _SCCs1),
      statistics(walltime, [T5,_]),
      Pass2Ms is T5 - T4,
      sampler:pdepend_perf_add(Pass1Ms, ExtractMs, Pass2Ms, 1, NewGoalsCount)
    )
  ).


% =============================================================================
%  Multi-variant pipeline (parallel re-proving)
% =============================================================================

%! pipeline:prove_plan_variants(+Goals, +Targets, +VariantSpecs, -Baseline, -VariantResults) is det.
%
% Proves the baseline plan, then re-proves each variant specification
% in parallel using concurrent threads. Each thread gets its own
% thread-local variant overrides and memo caches.
%
% Baseline = baseline(ProofAVL, ModelAVL, Plan, TriggersAVL)
% VariantResults = list of variant_result(Spec, ProofAVL, ModelAVL, Plan, TriggersAVL)
%                  or variant_result(Spec, failed) on proof failure.

pipeline:prove_plan_variants(Goals, _Targets, VariantSpecs,
                             baseline(ProofAVL, ModelAVL, Plan, TriggersAVL),
                             VariantResults) :-
  pipeline:prove_plan_with_fallback(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL),
  pipeline:prove_variants_parallel(Goals, VariantSpecs, VariantResults).


%! pipeline:prove_variants_parallel(+Goals, +Specs, -Results) is det.
%
% Proves each variant in a separate thread. Thread-local overrides
% ensure variants don't interfere with each other or the main thread.

pipeline:prove_variants_parallel(Goals, Specs, Results) :-
  length(Specs, N),
  ( N =:= 0
  -> Results = []
  ; length(Results, N),
    pipeline:prove_variants_threads(Goals, Specs, Results)
  ).


%! pipeline:prove_variants_threads(+Goals, +Specs, -Results) is det.
%
% Spawns a thread per variant using a shared message queue to
% collect results. Thread bindings do not propagate back via
% thread_join, so each thread posts its result to the queue.

pipeline:prove_variants_threads(Goals, Specs, Results) :-
  message_queue_create(Queue),
  length(Specs, N),
  findall(ThreadId,
    ( nth1(Idx, Specs, Spec),
      thread_create(
        pipeline:prove_single_variant(Goals, Spec, Idx, Queue),
        ThreadId, [])
    ),
    ThreadIds),
  maplist(pipeline:join_variant_thread, ThreadIds),
  pipeline:collect_queue_results(Queue, N, Unsorted),
  message_queue_destroy(Queue),
  msort(Unsorted, Sorted),
  pairs_values(Sorted, Results).


%! pipeline:join_variant_thread(+ThreadId) is det.

pipeline:join_variant_thread(ThreadId) :-
  thread_join(ThreadId, _Status).


%! pipeline:collect_queue_results(+Queue, +N, -Results) is det.

pipeline:collect_queue_results(_, 0, []) :- !.
pipeline:collect_queue_results(Queue, N, [Idx-Result|Rest]) :-
  thread_get_message(Queue, result(Idx, Result)),
  N1 is N - 1,
  pipeline:collect_queue_results(Queue, N1, Rest).


%! pipeline:prove_single_variant(+Goals, +Spec, +Idx, +Queue) is det.
%
% Runs inside a spawned thread. Applies the variant override,
% clears memo caches (thread-local), proves, and posts the result
% to the shared message queue.

pipeline:prove_single_variant(Goals, Spec, Idx, Queue) :-
  setup_call_cleanup(
    ( variant:apply(Spec),
      memo:clear_caches
    ),
    ( catch(
        ( pipeline:prove_plan_with_fallback(Goals, P, M, Pl, T)
        -> thread_send_message(Queue, result(Idx, variant_result(Spec, P, M, Pl, T)))
        ;  thread_send_message(Queue, result(Idx, variant_result(Spec, failed)))
        ),
        _Error,
        thread_send_message(Queue, result(Idx, variant_result(Spec, failed)))
      )
    ),
    ( variant:cleanup,
      memo:clear_caches
    )
  ).