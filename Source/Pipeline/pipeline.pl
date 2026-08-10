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

PDEPEND handling:
Post-dependencies are normally resolved single-pass inside the prover
(see heuristic:proof_obligation/4).  The prove_plan_with_pdepend/5 variant
provides an alternative multi-pass approach that delegates PDEPEND goal
extraction to dependency:pdepend_goals_from_plan/2 and re-runs the
pipeline with the extended goal set.  It is retained for experimentation
but not currently used in the default path.
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
  % Clear once for the whole ladder + sub-slot fixpoint. Dep-model cache
  % keys already encode prover:assuming bits (query.pl), so sharing across
  % tiers is sound and avoids the cold ~10s re-resolve per tier that
  % dominated the perl 5.42→5.44 sub-slot avalanche (portage-ng#118).
  memo:clear_caches,
  pipeline:prove_plan_with_fallback_base(Goals, Proof0, Model0, Plan0, Triggers0, SCCs0, Fallback0),
  pipeline:subslot_rebuild_loop(Goals,
                                Proof0, Model0, Plan0, Triggers0, SCCs0, Fallback0,
                                ProofAVL, ModelAVL, Plan, TriggersAVL, SCCs, FallbackUsed).


%! pipeline:prove_plan_with_fallback_base(+Goals, -Proof, -Model, -Plan, -Triggers, -SCCs, -FallbackUsed)
%
% The bare 5-tier fallback pipeline, without the sub-slot ABI rebuild
% augmentation. Used internally by prove_plan_with_fallback/7 (both for the
% initial proof and for each re-proof of an augmented goal set).
% Does not clear memo caches — the top-level entry point clears once so
% tiers share warm caches (same model as prove_with_fallback/4).

pipeline:prove_plan_with_fallback_base(Goals, ProofAVL, ModelAVL, Plan, TriggersAVL, SCCs, FallbackUsed) :-
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


% -----------------------------------------------------------------------------
%  Sub-slot (:=) ABI rebuild propagation (portage-ng#89)
% -----------------------------------------------------------------------------
%
% Native equivalent of Gentoo's @preserved-rebuild / haskell-updater pass.
% When a transaction changes a provider's sub-slot (e.g. a dev-haskell/*
% library rebuilt with a new GHC ABI hash, or dev-lang/ocaml with a new ABI),
% the already-installed reverse-deps that bound to it through `:=` / `:slot=`
% break ghc-pkg check / findlib's registry and must be rebuilt before the next
% consumer configures. This is transaction-driven: it inspects the freshly
% computed plan, finds providers whose sub-slot differs from the installed
% copy, and appends same-version `:update` rebuild steps (carrying
% `rebuild_reason(subslot_change/3)`) in a final wave after the provider.
%
% Eligible rebuilds are injected into the existing plan/proof/model — they
% are NOT re-proven as an N-goal conjunction. Re-proving ~90 perl `:=`
% consumers after a 5.42→5.44 bump cost minutes per target (portage-ng#118);
% injection is O(consumers) and keeps a real perl upgrade inside a sane
% wall-time budget. Masked / keyword-filtered consumers are skipped and
% recorded as domain assumptions instead of escalating the whole plan to
% the unmask tier. The loop still iterates to a fixpoint (a rebuilt
% consumer keeps its version/sub-slot, so the closure terminates quickly).

%! pipeline:subslot_rebuild_suspended is semidet.
%
% Dynamic flag. When asserted, the sub-slot rebuild augmentation is skipped.
% Set by the bulk per-entry test harnesses (test_stats) so they keep their
% single-entry semantics and performance; real plan paths (merge / build /
% pretend / writer) leave it unset and get the augmentation.

:- dynamic pipeline:subslot_rebuild_suspended/0.


%! pipeline:subslot_rebuild_enabled is semidet.
%
% True when the augmentation should run: config:subslot_rebuild/1 is not
% false (defaults to enabled when unset) and it is not suspended.

pipeline:subslot_rebuild_enabled :-
  \+ pipeline:subslot_rebuild_suspended,
  ( catch(config:subslot_rebuild(Bool), _, fail) -> Bool == true ; true ).


%! pipeline:subslot_rebuild_loop(+Goals, +P0,+M0,+Pl0,+T0,+SCCs0,+FB0, -P,-M,-Pl,-T,-SCCs,-FB)
%
% Augmentation fixpoint. If the plan changed a `:=` provider's sub-slot and
% there are installed consumers not yet targeted, inject same-version
% rebuild steps into the plan (final wave) and record them in proof/model;
% masked/keyword-filtered consumers become domain assumptions. Does not
% re-prove the augmented goal set (portage-ng#118). Any error in detection
% degrades gracefully to passthrough so planning is never broken by this
% pass. FallbackUsed is unchanged from the base prove.

pipeline:subslot_rebuild_loop(Goals, P0, M0, Pl0, T0, SCCs0, FB0, P, M, Pl, T, SCCs, FB) :-
  ( pipeline:subslot_rebuild_enabled,
    catch(pipeline:subslot_extra_goals(Pl0, Goals, Extra, Skipped), _, fail),
    ( Extra \== [] ; Skipped \== [] )
  -> % Mark both injected and skipped consumers as handled in Goals1 so the
     % next fixpoint iteration does not re-detect them (skipped Clone alone
     % previously caused an infinite loop — portage-ng#118).
     findall(SG, member(skipped(_, SG), Skipped), SkippedGoals),
     append(Extra, SkippedGoals, Handled),
     append(Goals, Handled, Goals1),
     pipeline:subslot_inject_rebuilds(Extra, P0, M0, Pl0, P1, M1, Pl1),
     pipeline:subslot_inject_skipped(Skipped, P1, M1, P2, M2),
     pipeline:subslot_rebuild_loop(Goals1, P2, M2, Pl1, T0, SCCs0, FB0, P, M, Pl, T, SCCs, FB)
  ;  P = P0, M = M0, Pl = Pl0, T = T0, SCCs = SCCs0, FB = FB0
  ).


%! pipeline:subslot_inject_rebuilds(+Extra, +Proof0, +Model0, +Plan0, -Proof, -Model, -Plan) is det.
%
% Appends a final plan wave of `rule(Goal, [])` for each eligible consumer
% rebuild and records the same rules in Proof/Model so the printer shows
% `(abi-rebuild: ...)` notes. Consumers already present as merge actions
% in Plan0 are skipped. Empty Extra is a no-op.

pipeline:subslot_inject_rebuilds([], Proof, Model, Plan, Proof, Model, Plan) :- !.
pipeline:subslot_inject_rebuilds(Extra, Proof0, Model0, Plan0, Proof, Model, Plan) :-
  findall(rule(Goal, []),
          ( member(Goal, Extra),
            Goal = Repo://Entry:_Action?{_Ctx},
            \+ pipeline:plan_merge_target(Plan0, Repo, Entry)
          ),
          Wave0),
  ( Wave0 == []
  -> Proof = Proof0, Model = Model0, Plan = Plan0
  ;  sort(Wave0, Wave),
     append(Plan0, [Wave], Plan),
     pipeline:subslot_record_rebuild_rules(Wave, Proof0, Model0, Proof, Model)
  ).


%! pipeline:subslot_record_rebuild_rules(+Rules, +Proof0, +Model0, -Proof, -Model) is det.
%
% Puts each injected rebuild rule into the proof AVL (body []) and its
% head into the model AVL so assumption/plan printers see the action.

pipeline:subslot_record_rebuild_rules([], Proof, Model, Proof, Model) :- !.
pipeline:subslot_record_rebuild_rules([rule(Head, Body)|Rest], Proof0, Model0, Proof, Model) :-
  put_assoc(rule(Head), Proof0, Body, Proof1),
  put_assoc(Head, Model0, true, Model1),
  pipeline:subslot_record_rebuild_rules(Rest, Proof1, Model1, Proof, Model).


%! pipeline:subslot_extra_goals(+Plan, +ExistingGoals, -ExtraGoals, -Skipped) is semidet.
%
% Fails when the plan changes no `:=` provider's sub-slot (the cheap common
% case). Otherwise binds ExtraGoals to the eligible installed `:=`-consumer
% rebuilds not already present in ExistingGoals, and Skipped to
% skipped(Reason, Goal) terms for consumers that are masked or
% keyword-filtered (portage-ng#118). Those must not be appended as prove
% goals — one masked consumer (e.g. a perl-core module after a perl major
% bump) would escalate the whole augmented set through the unmask tier.

pipeline:subslot_extra_goals(Plan, ExistingGoals, ExtraGoals, Skipped) :-
  pipeline:subslot_changed_providers(Plan, Changed),
  Changed \== [],
  pipeline:goals_target_cns(ExistingGoals, TargetedCNs),
  pipeline:subslot_affected_consumers(Changed, TargetedCNs, ExtraGoals, Skipped).


%! pipeline:subslot_changed_providers(+Plan, -Changed) is det.
%
% Collects prov(C, N, Slot, OldSub, NewSub) for every merge action in the
% plan whose new sub-slot differs from the installed copy in the same slot.

pipeline:subslot_changed_providers(Plan, Changed) :-
  findall(prov(C, N, Slot, OldSub, NewSub),
          ( pipeline:plan_merge_target(Plan, Repo, Entry),
            cache:ordered_entry(Repo, Entry, C, N, _),
            slotmeta:entry_slot_default(Repo, Entry, Slot),
            sets:entry_subslot(Repo://Entry, NewSub),
            cache:ordered_entry(pkg, OldEntry, C, N, _),
            slotmeta:entry_slot_default(pkg, OldEntry, Slot),
            sets:entry_subslot(pkg://OldEntry, OldSub),
            OldSub \== NewSub
          ),
          Changed0),
  sort(Changed0, Changed).


%! pipeline:plan_merge_target(+Plan, -Repo, -Entry) is nondet.
%
% Enumerates the merge-shaped rules (install / update / upgrade / downgrade)
% in a scheduled plan (a list of steps, each a list of rule/2 terms).

pipeline:plan_merge_target(Plan, Repo, Entry) :-
  member(Step, Plan),
  member(rule(Repo://Entry:Action?{_Ctx}, _Body), Step),
  memberchk(Action, [install, update, upgrade, downgrade]).


%! pipeline:subslot_affected_consumers(+Changed, +TargetedCNs, -ExtraGoals, -Skipped) is det.
%
% Finds the installed reverse-deps that bound to a changed provider through
% a sub-slot operator and turns each (once, deduplicated by VDB entry) into a
% same-version `:update` rebuild goal carrying rebuild_reason(subslot_change/3).
% Masked / keyword-filtered consumers go into Skipped instead of ExtraGoals
% so they cannot poison the augmented prove (portage-ng#118).

pipeline:subslot_affected_consumers(Changed, TargetedCNs, ExtraGoals, Skipped) :-
  findall(c(Entry, TreeRepo, C/N, OldSub, NewSub),
          ( member(prov(C, N, Slot, OldSub, NewSub), Changed),
            pipeline:subslot_consumer_of(C, N, Slot, TargetedCNs, Entry, TreeRepo)
          ),
          Raw),
  sort(1, @<, Raw, Unique),
  findall(Goal,
          ( member(Cm, Unique),
            pipeline:subslot_consumer_goal(Cm, Goal),
            \+ pipeline:subslot_consumer_skip_reason(Goal, _)
          ),
          ExtraGoals),
  findall(skipped(Reason, Goal),
          ( member(Cm, Unique),
            pipeline:subslot_consumer_goal(Cm, Goal),
            pipeline:subslot_consumer_skip_reason(Goal, Reason)
          ),
          Skipped).


%! pipeline:subslot_consumer_skip_reason(+Goal, -Reason) is semidet.
%
% True when a same-version consumer rebuild cannot be planned without a
% visibility assumption. Reason is `masked` or `keyword_filtered`.

pipeline:subslot_consumer_skip_reason(Repo://Entry:_Action?{_Ctx}, masked) :-
  preference:masked(Repo://Entry),
  !.
pipeline:subslot_consumer_skip_reason(Repo://Entry:_Action?{_Ctx}, keyword_filtered) :-
  \+ acceptance:entry_has_accepted_keyword(Repo://Entry),
  !.


%! pipeline:subslot_inject_skipped(+Skipped, +Proof0, +Model0, -Proof, -Model) is det.
%
% Records each skipped consumer as a domain assumption
% (`rule(assumed(...))` / `assumed(...)`) so the printer still surfaces
% the masked/keyword-filtered rebuild without forcing the unmask tier.

pipeline:subslot_inject_skipped([], Proof, Model, Proof, Model) :- !.
pipeline:subslot_inject_skipped([skipped(Reason, Goal)|Rest], Proof0, Model0, Proof, Model) :-
  pipeline:subslot_skipped_assumption(Reason, Goal, Assumed),
  put_assoc(rule(Assumed), Proof0, [], Proof1),
  put_assoc(Assumed, Model0, true, Model1),
  pipeline:subslot_inject_skipped(Rest, Proof1, Model1, Proof, Model).


%! pipeline:subslot_skipped_assumption(+Reason, +Goal, -Assumed) is det.
%
% Wraps a skipped rebuild goal as `assumed(Goal)` with
% `assumption_reason(Reason)` in the proof context list.

pipeline:subslot_skipped_assumption(Reason, Repo://Entry:Action?{Ctx0},
                                    assumed(Repo://Entry:Action?{Ctx})) :-
  ( is_list(Ctx0) -> Ctx1 = Ctx0 ; Ctx1 = [] ),
  ( memberchk(assumption_reason(Reason), Ctx1)
  -> Ctx = Ctx1
  ;  Ctx = [assumption_reason(Reason)|Ctx1]
  ).


%! pipeline:subslot_consumer_of(+C, +N, +Slot, +TargetedCNs, -ICEntry, -TreeRepo) is nondet.
%
% True for an installed package ICEntry (with a matching tree ebuild in
% TreeRepo) that is not C/N itself, not already targeted, and whose tree
% *DEPEND declares a sub-slot-bound (`:=` / `:slot=`) dependency on C/N in
% slot Slot.

pipeline:subslot_consumer_of(C, N, Slot, TargetedCNs, ICEntry, TreeRepo) :-
  vdb:installed_entry(ICEntry),
  cache:ordered_entry(pkg, ICEntry, ICC, ICN, _),
  \+ ( ICC == C, ICN == N ),
  \+ memberchk(ICC-ICN, TargetedCNs),
  cache:ordered_entry(TreeRepo, ICEntry, ICC, ICN, _),
  TreeRepo \== pkg,
  once(( member(Key, [rdepend, depend, bdepend, pdepend]),
         cache:entry_metadata(TreeRepo, ICEntry, Key, Dep),
         candidate:dep_contains_pkg_dep_on(Dep, C, N, _Op, _V, SlotReq),
         pipeline:subslot_bound_slotspec(SlotReq, Slot)
       )).


%! pipeline:subslot_bound_slotspec(+SlotReq, +Slot) is semidet.
%
% True when a parsed dependency slot restriction binds the consumer to the
% provider's sub-slot (a rebuild trigger) and is compatible with Slot:
%   `:=`       -> [any_same_slot]            (binds, any slot)
%   `:slot=`   -> [slot(S),equal]            (binds, requires S == Slot)
%   `:s/ss=`   -> [slot(S),subslot(_),equal] (binds, requires S == Slot)

pipeline:subslot_bound_slotspec([any_same_slot], _Slot) :- !.
pipeline:subslot_bound_slotspec(SlotReq, Slot) :-
  memberchk(equal, SlotReq),
  ( member(slot(S), SlotReq)
  -> slotmeta:canon_slot(S, Sc), Sc == Slot
  ;  true
  ).


%! pipeline:subslot_consumer_goal(+Consumer, -Goal) is det.
%
% Builds the rebuild goal: a same-version `:update` of the installed consumer
% that re-resolves its dependencies (so the changed provider edge orders the
% rebuild after the provider) and carries the subslot_change reason.

pipeline:subslot_consumer_goal(c(Entry, TreeRepo, Provider, OldSub, NewSub),
                               TreeRepo://Entry:update?{[replaces(pkg://Entry),
                                                        rebuild_reason(subslot_change(Provider, OldSub, NewSub))]}).


%! pipeline:goals_target_cns(+Goals, -CNs) is det.
%
% Collects the Category-Name pairs that the goal list already targets, so the
% augmentation never re-adds a consumer that is already being built.

pipeline:goals_target_cns(Goals, CNs) :-
  findall(C-N,
          ( member(Goal, Goals), pipeline:goal_target_cn(Goal, C-N) ),
          CNs0),
  sort(CNs0, CNs).


%! pipeline:goal_target_cn(+Goal, -CN) is semidet.

pipeline:goal_target_cn(Repo://Entry:_Action?{_Ctx}, C-N) :-
  cache:ordered_entry(Repo, Entry, C, N, _),
  !.


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
% The sub-slot ABI rebuild augmentation (portage-ng#89) is suspended for the
% duration so per-entry proving keeps its single-target semantics and speed.

pipeline:test_stats(Repository, Style) :-
  setup_call_cleanup(
    assertz(pipeline:subslot_rebuild_suspended),
    pipeline:test_stats_run(Repository, Style),
    retractall(pipeline:subslot_rebuild_suspended)).


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