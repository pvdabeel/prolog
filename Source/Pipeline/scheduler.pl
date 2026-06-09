/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> SCHEDULER
The Scheduler is a post-planning step that can deal with cyclic remainders.

Conceptually:
- The prover builds a proof (and triggers graph).
- The planner builds a wave plan for the acyclic portion of the graph.
- If the planner cannot schedule everything (e.g. due to cyclic rule bodies),
  it returns a remainder.

This scheduler computes strongly connected components (SCCs) on the remainder
subgraph (Kosaraju) and can "act" only on SCCs that are safe to merge as a set.

Policy:
- SCCs consisting purely of *mergeable* literals are schedulable as merge sets.
  Mergeable literals are:
  - :run (historical behavior)
  - :install/:update/:downgrade/:reinstall (merge actions)
- Any SCC containing other literal kinds is treated as unschedulable; all rules
  that (transitively) depend on such SCCs remain in the remainder.
- Merge-set SCCs are classified by priority composition (runtime_only vs
  has_build) for diagnostics.
- Multi-member merge-set SCCs are linearized using Portage-like progressive
  relaxation: iteratively extract nodes whose SCC-internal deps are satisfied,
  relaxing :run edges when stuck, then picking the node with the fewest
  unsatisfied hard deps for true hard cycles.

The scheduler does not mutate the prover's TriggersAVL; it derives SCC metadata
and a condensed schedule for the remainder only.
*/

:- module(scheduler, []).

:- thread_local scheduler:scc_info_/3.  % (Id, Kind, Members)

user:goal_expansion(perf_reset, true) :-
  \+ current_prolog_flag(instrumentation, true).

user:goal_expansion(perf_add(_, _, _, _, _, _, _, _), true) :-
  \+ current_prolog_flag(instrumentation, true).

user:goal_expansion(perf_report, true) :-
  \+ current_prolog_flag(instrumentation, true).


%! scheduler:schedule(+ProofAVL,+TriggersAVL,+PlanIn,+RemainderIn,-PlanOut,-RemainderOut)
%
% If RemainderIn is empty, passes PlanIn through unchanged (O(1)).
% Otherwise schedules the schedulable portion of the remainder by collapsing
% :run SCCs (merge sets) and returns a new remainder for the unschedulable part.
%
schedule(ProofAVL, TriggersAVL, PlanIn, RemainderIn, PlanOut, []) :-
  RemainderIn == [],
  !,
  retractall(scheduler:scc_info_(_,_,_)),
  scheduler:perf_add(0, 0, 0, 0, 0, 0, 0, 0),
  scheduler:merge_order_bias(ProofAVL, TriggersAVL, PlanIn, PlanBiased),
  scheduler:enforce_order_after_constraints(PlanBiased, PlanOut).
schedule(ProofAVL, TriggersAVL, PlanIn, RemainderIn, PlanOut, RemainderOut) :-
  % Only schedule from the planner-provided remainder. Do not remove items from
  % the existing plan here: removing + re-adding must be proven correct, and we
  % currently want a safe scheduler that never drops actions.
  scheduler:remainder_heads(RemainderIn, Heads),
  length(Heads, HeadsN),
  % Performance: avoid repeated ProofAVL lookups by building a head->rule map from
  % the planner-provided remainder rules, and extract the remainder subgraph from it.
  scheduler:remainder_head_rule_map(RemainderIn, HeadRuleMap),
  scheduler:build_forward_reverse_from_rules(Heads, HeadRuleMap, Forward, Reverse),
  scheduler:kosaraju_scc(Heads, Forward, Reverse, SCCs),
  length(SCCs, SCCsN),
  scheduler:build_components(SCCs, Forward, CompMap, Comps),
  length(Comps, CompsN),
  scheduler:record_scc_info(Comps),
  scheduler:blocked_components(Comps, Forward, CompMap, BlockedCompIds),
  length(BlockedCompIds, BlockedN),
  scheduler:schedulable_component_waves(Comps, Forward, CompMap, BlockedCompIds, WavesCompIds),
  length(WavesCompIds, WavesN),
  scheduler:count_wave_components(WavesCompIds, WavesCompTotalN),
  scheduler:expand_component_waves_from_map(WavesCompIds, Comps, Forward, HeadRuleMap, WavesRules),
  scheduler:count_rules_in_plan(WavesRules, AddedRulesN),
  append(PlanIn, WavesRules, PlanOut0),
  scheduler:remainder_from_blocked_from_map(BlockedCompIds, Comps, HeadRuleMap, RemainderOut),
  scheduler:perf_add(HeadsN, SCCsN, CompsN, BlockedN, WavesN, WavesCompTotalN, AddedRulesN, 1),
  scheduler:merge_order_bias(ProofAVL, TriggersAVL, PlanOut0, PlanBiased),
  scheduler:enforce_order_after_constraints(PlanBiased, PlanOut).

% -----------------------------------------------------------------------------
%  SCC info recording (for printer visualization)
% -----------------------------------------------------------------------------

scheduler:record_scc_info(Comps) :-
  retractall(scheduler:scc_info_(_,_,_)),
  forall(member(comp(Id, Kind, Members), Comps),
         ( Members = [_] -> true
         ; assertz(scheduler:scc_info_(Id, Kind, Members))
         )).


% -----------------------------------------------------------------------------
%  Merge-order bias: within-wave reordering by reference count
% -----------------------------------------------------------------------------
%
% Portage's _merge_order_bias() sorts nodes by descending reference count
% (number of parent/dependent nodes in the graph).  Packages depended on by
% more things are installed first, since they satisfy the most constraints
% earliest.  This matches Portage's leaf-node selection order.

%! scheduler:merge_order_bias(+ProofAVL, +TriggersAVL, +PlanIn, -PlanOut)
%
% Reorder rules within each wave of PlanIn by descending reference count.
scheduler:merge_order_bias(_ProofAVL, TriggersAVL, PlanIn, PlanOut) :-
  append(PlanIn, AllRules),
  ( AllRules == [] -> PlanOut = PlanIn
  ;
    scheduler:build_refcount_map(AllRules, TriggersAVL, RefCountMap),
    scheduler:reorder_waves_by_refcount(PlanIn, RefCountMap, PlanOut)
  ).

%! scheduler:build_refcount_map(+AllRules, +TriggersAVL, -RefCountMap)
%
% For each rule head in the plan, count how many OTHER planned heads trigger
% on it (= reference count / number of dependents in the full graph).
scheduler:build_refcount_map(AllRules, TriggersAVL, RefCountMap) :-
  empty_assoc(HeadSet0),
  foldl(scheduler:add_rule_to_head_set_, AllRules, HeadSet0, HeadSet),
  assoc_to_keys(HeadSet, AllHeads),
  empty_assoc(M0),
  foldl(scheduler:compute_refcount(TriggersAVL, HeadSet), AllHeads, M0, RefCountMap).

scheduler:add_rule_to_head_set_(Rule, In, Out) :-
  ( scheduler:rule_head(Rule, Head) ->
      put_assoc(Head, In, true, Out)
  ; Out = In
  ).

scheduler:compute_refcount(TriggersAVL, HeadSet, Head, In, Out) :-
  scheduler:effective_trigger_keys(Head, TriggerKeys),
  findall(DepHead,
          ( member(TK, TriggerKeys),
            get_assoc(TK, TriggersAVL, Dependents),
            member(D0, Dependents),
            prover:canon_literal(D0, DepHead, _),
            get_assoc(DepHead, HeadSet, _),
            DepHead \= Head
          ),
          DepHeads0),
  sort(DepHeads0, UniqueDepHeads),
  length(UniqueDepHeads, Count),
  put_assoc(Head, In, Count, Out).

% For merge actions (:install/:update/:downgrade/:reinstall), also count
% triggers on the corresponding :run head, since other packages depend on
% the :run action rather than the :install action directly.
scheduler:effective_trigger_keys(Head, Keys) :-
  ( Head = R://L:Action,
    memberchk(Action, [install, update, downgrade, reinstall])
  -> Keys = [Head, R://L:run]
  ; Keys = [Head]
  ).

scheduler:reorder_waves_by_refcount([], _, []) :- !.
scheduler:reorder_waves_by_refcount([Wave|Ws], RefCountMap, [Sorted|Rs]) :-
  findall(NegCount-Rule,
          ( member(Rule, Wave),
            ( scheduler:rule_head(Rule, Head),
              get_assoc(Head, RefCountMap, Count)
            -> NegCount is -Count
            ; NegCount = 0
            )
          ),
          Pairs),
  keysort(Pairs, SortedPairs),
  findall(Rule, member(_-Rule, SortedPairs), Sorted),
  scheduler:reorder_waves_by_refcount(Ws, RefCountMap, Rs).

% -----------------------------------------------------------------------------
%  Enforce ordering-only constraints (after_only)
% -----------------------------------------------------------------------------
%
% `featureterm:add_after_condition/4` represents after_only/1 as:
%   constraint(order_after(Anchor):{[]})
%
% The wave planner ignores constraints, so we enforce them here by reordering the
% final plan so that any rule carrying such a constraint appears after Anchor.

scheduler:enforce_order_after_constraints(PlanIn, PlanOut) :-
  append(PlanIn, Flat0),
  ( scheduler:flat_has_order_after_constraints(Flat0) ->
      scheduler:plan_step_lengths(PlanIn, Lens),
      scheduler:flat_order_after_constraints(Flat0, Flat),
      scheduler:rechunk_by_lengths(Flat, Lens, PlanMid)
  ; PlanMid = PlanIn
  ),
  % Always run repair_ordering_violations: even without order_after
  % constraints, a rule body may carry an `assumed(grouped_package_dep…)`
  % dep that aliases to a concrete planned install/run further down the
  % wave list (typical case: a soft-blocker cycle forced the prover into
  % a domain assumption while another path resolved the same package
  % concretely with a USE mutation). The repair pass is alias-aware (see
  % `repair_dep_head/6`) and promotes the parent so it lands after the
  % concrete action it actually depends on.
  scheduler:repair_ordering_violations(PlanMid, PlanOut),
  !.

scheduler:flat_has_order_after_constraints(Rules) :-
  member(Rule, Rules),
  scheduler:rule_order_after_anchor(Rule, _Anchor),
  !.

scheduler:plan_step_lengths(Plan, Lens) :-
  findall(N, (member(Step, Plan), length(Step, N)), Lens),
  !.

scheduler:flat_order_after_constraints(Rules0, Rules) :-
  % Build a set of heads present in the plan.
  findall(H, (member(R, Rules0), scheduler:rule_head(R, H)), Heads0),
  sort(Heads0, Heads),
  empty_assoc(Empty),
  foldl(scheduler:assoc_set_put, Heads, Empty, HeadSet),
  % One-pass stable reordering using a waiting bucket per anchor.
  empty_assoc(W0),
  scheduler:order_after_loop(Rules0, HeadSet, W0, W, [], RevOut),
  % If some rules are mutually constrained (cycle), they may remain in Waiting.
  % Do NOT drop them (that would make schedule/6 fail). Instead append them in a
  % deterministic order as a best-effort fallback.
  scheduler:waiting_leftovers(W, Left0),
  reverse(Left0, RevLeft),
  append(RevLeft, RevOut, RevAll),
  reverse(RevAll, Rules).

scheduler:waiting_leftovers(Waiting, Leftovers) :-
  assoc:assoc_to_list(Waiting, Pairs),
  findall(R,
          ( member(_Anchor-Rs, Pairs),
            member(R, Rs)
          ),
          Leftovers).

scheduler:assoc_set_put(K, A0, A) :-
  ( get_assoc(K, A0, _) -> A = A0 ; put_assoc(K, A0, true, A) ).

scheduler:order_after_loop([], _HeadSet, Waiting, Waiting, Acc, Acc) :- !.
scheduler:order_after_loop([Rule|Rest], HeadSet, Waiting0, Waiting, Acc0, Acc) :-
  scheduler:rule_head(Rule, H),
  ( scheduler:rule_order_after_anchor(Rule, Anchor),
    get_assoc(Anchor, HeadSet, true)
  ->
    % Anchor exists in plan: delay until anchor is emitted.
    ( get_assoc(Anchor, Waiting0, L0) -> true ; L0 = [] ),
    append(L0, [Rule], L1),
    put_assoc(Anchor, Waiting0, L1, Waiting1),
    scheduler:order_after_loop(Rest, HeadSet, Waiting1, Waiting, Acc0, Acc)
  ; % Emit now, then flush dependents waiting on this head.
    scheduler:flush_waiting(H, HeadSet, Waiting0, Waiting1, [Rule|Acc0], Acc1),
    scheduler:order_after_loop(Rest, HeadSet, Waiting1, Waiting, Acc1, Acc)
  ).

scheduler:flush_waiting(H, HeadSet, Waiting0, Waiting, Acc0, Acc) :-
  ( get_assoc(H, Waiting0, L), L \== [] ->
      del_assoc(H, Waiting0, _Old, Waiting1),
      scheduler:flush_waiting_list(L, HeadSet, Waiting1, Waiting, Acc0, Acc)
  ; Waiting = Waiting0,
    Acc = Acc0
  ).

scheduler:flush_waiting_list([], _HeadSet, Waiting, Waiting, Acc, Acc) :- !.
scheduler:flush_waiting_list([R|Rs], HeadSet, Waiting0, Waiting, Acc0, Acc) :-
  scheduler:rule_head(R, H),
  scheduler:flush_waiting(H, HeadSet, Waiting0, Waiting1, [R|Acc0], Acc1),
  scheduler:flush_waiting_list(Rs, HeadSet, Waiting1, Waiting, Acc1, Acc).

scheduler:rule_order_after_anchor(Rule, Anchor) :-
  ( Rule = rule(_HeadWithCtx, Body)
  ; Rule = assumed(rule(_HeadWithCtx, Body))
  ; Rule = rule(assumed(_HeadWithCtx), Body)
  ),
  member(constraint(order_after(Anchor):{_}), Body),
  !.

scheduler:rechunk_by_lengths(Flat, Lens, Plan) :-
  scheduler:rechunk_by_lengths_(Flat, Lens, Plan).

scheduler:rechunk_by_lengths_([], [], []) :- !.
scheduler:rechunk_by_lengths_(Rules, [N|Ns], [Step|Rest]) :-
  length(Step, N),
  append(Step, Tail, Rules),
  scheduler:rechunk_by_lengths_(Tail, Ns, Rest).


% -----------------------------------------------------------------------------
%  Repair ordering violations after rechunking
% -----------------------------------------------------------------------------
%
% After order_after reordering + rechunking by original wave lengths, some
% rules may end up in a wave that precedes one of their non-constraint body
% dependencies. This pass moves such rules to the correct later wave.
%
% The repair is computed on the SCC condensation of the effective repair
% dependency graph (body deps + PDEPEND completion edges + configure-closure
% deps + assumed-dep aliases). The condensation is a DAG, so a single
% longest-path computation yields the least wave assignment that
%
%   - never demotes a rule below its incoming wave, and
%   - places every rule strictly after all of its cross-SCC dependencies.
%
% Members of a genuine dependency cycle (one SCC) share a wave -- a strict
% order does not exist for them (Portage merge-set semantics). Crucially,
% rules *downstream* of a cycle still get strictly later waves: an earlier
% fixpoint-sweep implementation diverged on cycles, hit its iteration cap,
% and then collapsed acyclic chains (e.g. a BDEPEND provider and its
% consumer) into a single wave, losing hard build-time ordering
% (portage-ng#26, cvs-fast-export vs dev-ruby/asciidoctor).

%! scheduler:repair_ordering_violations(+PlanIn, -PlanOut)
%
% SCC-condensation longest-path repair for ordering violations.

scheduler:repair_ordering_violations(PlanIn, PlanOut) :-
  scheduler:build_head_wave_map(PlanIn, 1, t, Map0),
  scheduler:build_pkg_head_map(PlanIn, PkgHeadMap),
  append(PlanIn, AllRules),
  scheduler:build_pdepend_anchor_map(AllRules, AnchorMap),
  ( AnchorMap == t
  -> Pd = pd(t, t)                       % no PDEPEND provider in plan: no-op
  ;  scheduler:build_pdepend_closure_map(AllRules, AnchorMap, ClosureMap),
     Pd = pd(AnchorMap, ClosureMap)
  ),
  scheduler:build_install_configure_dep_map(AllRules, CfgMap),
  scheduler:build_repair_graph(AllRules, Map0, PkgHeadMap, Pd, CfgMap,
                               Heads, Forward, Reverse),
  scheduler:kosaraju_scc(Heads, Forward, Reverse, SCCs),
  scheduler:repair_comp_map(SCCs, CompMap, CompIds, MembersMap),
  scheduler:comp_edges(Forward, CompMap, CompEdges),
  scheduler:assign_repair_waves(CompIds, CompEdges, MembersMap, Map0, Map1),
  scheduler:rebuild_plan_from_map(AllRules, Map1, PlanOut).


%! scheduler:build_repair_graph(+AllRules, +Map, +PkgHeadMap, +Pd, +CfgMap,
%!                              -Heads, -Forward, -Reverse)
%
% Builds the effective repair dependency graph over all planned rule heads.
% `Forward` maps each head to the sorted list of in-plan heads it must be
% scheduled after. A rule head depends on another head in four ways:
%
%  1. Direct head match: the canonicalised body dep literal is a key in
%     `Map`. This is the normal "B depends on A and A's rule was scheduled"
%     case.
%
%  2. PDEPEND completion: if a body dep resolves to a provider P that
%     declares PDEPEND, P is only functionally complete once its post-deps
%     are merged. The consumer must therefore land after P's full PDEPEND
%     closure, not merely after P:install (general PDEPEND ordering,
%     portage-ng#18). See `pdepend_completion_heads/5`.
%
%  3. Assumed-dep alias: the dep is an `assumed(grouped_package_dependency
%     (C,N,_):Action?{_})` (or the legacy `assumed(package_dependency(_,_,
%     C,N,_,_,_,_):Action?{_})`) AND `PkgHeadMap` has a concrete planned
%     action in the same `PhaseClass` for (C,N). This recovers the
%     dependency edge that was severed when the prover fell back to a domain
%     assumption for (C,N) on one path while another path resolved the same
%     package concretely. Without this aliasing the parent would be
%     scheduled in wave 1 alongside the empty-body `rule(assumed(...), [])`
%     verify rule and run before the concrete install (Qt6 cmake-find
%     ordering bug).
%
%  4. Configure closure: install-phase heads additionally depend on their
%     sibling `:run` rule's `:run`-phase body deps (RDEPEND providers must
%     be functionally complete before configure starts, portage-ng#21).

scheduler:build_repair_graph(AllRules, Map, PkgHeadMap, Pd, CfgMap,
                             Heads, Forward, Reverse) :-
  assoc_to_keys(Map, Heads),
  empty_assoc(F0),
  foldl(scheduler:init_empty_neighbors, Heads, F0, F1),
  foldl(scheduler:add_repair_edges(Map, PkgHeadMap, Pd, CfgMap),
        AllRules, F1, Forward),
  scheduler:invert_graph(Heads, Forward, Reverse).


%! scheduler:add_repair_edges(+Map, +PkgHeadMap, +Pd, +CfgMap, +Rule, +In, -Out)
%
% Folds the repair dependency heads of one rule into the forward map.

scheduler:add_repair_edges(Map, PkgHeadMap, Pd, CfgMap, Rule, In, Out) :-
  ( scheduler:rule_head(Rule, Head),
    scheduler:rule_body(Rule, Body)
  ->
    scheduler:repair_dep_heads(Body, Head, Map, PkgHeadMap, Pd, BodyDeps),
    ( scheduler:install_phase_key(Head, InstallKey),
      get_assoc(InstallKey, CfgMap, RunDeps)
    -> scheduler:repair_dep_heads(RunDeps, InstallKey, Map, PkgHeadMap, Pd, CfgDeps)
    ;  CfgDeps = []
    ),
    ord_union(BodyDeps, CfgDeps, Deps),
    ( get_assoc(Head, In, Old) -> true ; Old = [] ),
    ord_union(Old, Deps, New),
    put_assoc(Head, In, New, Out)
  ; Out = In
  ).


%! scheduler:repair_dep_heads(+Deps, +RuleHead, +Map, +PkgHeadMap, +Pd, -DepHeads)
%
% Sorted list of in-plan heads that the given dep literals resolve to
% (direct heads, PDEPEND completion heads, and assumed-dep aliases).

scheduler:repair_dep_heads(Deps, RuleHead, Map, PkgHeadMap, Pd, DepHeads) :-
  findall(DH,
          ( member(Dep, Deps),
            \+ constraint:is_constraint(Dep),
            scheduler:repair_dep_head(Dep, RuleHead, Map, PkgHeadMap, Pd, DH)
          ),
          DepHeads0),
  sort(DepHeads0, DepHeads).


%! scheduler:repair_dep_head(+Dep, +RuleHead, +Map, +PkgHeadMap, +Pd, -DepHead)
%
% Enumerates the in-plan heads a single body dep contributes an edge to.

scheduler:repair_dep_head(Dep, RuleHead, Map, _PkgHeadMap, Pd, DH) :-
  prover:canon_literal(Dep, DepHead, _),
  get_assoc(DepHead, Map, _),
  ( DH = DepHead
  ; scheduler:pdepend_completion_heads(DepHead, RuleHead, Map, Pd, EHeads),
    member(DH, EHeads)
  ).
scheduler:repair_dep_head(Dep, _RuleHead, _Map, PkgHeadMap, _Pd, DH) :-
  ( scheduler:assumed_dep_alias_key(Dep, AliasKey)
  ; scheduler:grouped_run_dep_pkg_key(Dep, AliasKey)
  ),
  get_assoc(AliasKey, PkgHeadMap, DH).


%! scheduler:repair_comp_map(+SCCs, -CompMap, -CompIds, -MembersMap)
%
% Numbers the SCCs and builds the head->component and component->members maps.

scheduler:repair_comp_map(SCCs, CompMap, CompIds, MembersMap) :-
  empty_assoc(M0),
  empty_assoc(Mm0),
  scheduler:repair_comp_map_(SCCs, 1, M0, CompMap, Mm0, MembersMap, [], Ids0),
  reverse(Ids0, CompIds).

scheduler:repair_comp_map_([], _I, M, M, Mm, Mm, Ids, Ids).
scheduler:repair_comp_map_([Members|Rest], I, M0, M, Mm0, Mm, Ids0, Ids) :-
  foldl(scheduler:compmap_put(I), Members, M0, M1),
  put_assoc(I, Mm0, Members, Mm1),
  I1 is I + 1,
  scheduler:repair_comp_map_(Rest, I1, M1, M, Mm1, Mm, [I|Ids0], Ids).


%! scheduler:assign_repair_waves(+CompIds, +CompEdges, +MembersMap, +InitMap, -MapOut)
%
% Longest-path wave assignment over the SCC condensation:
%
%   wave(C) = max(max initial wave of C's members,
%                 max over dep components C' of wave(C') + 1)
%
% Computed by memoized DFS (the condensation is acyclic). All members of a
% component share its wave; cross-component edges strictly increase waves.

scheduler:assign_repair_waves(CompIds, CompEdges, MembersMap, InitMap, MapOut) :-
  empty_assoc(A0),
  foldl(scheduler:add_comp_adj, CompEdges, A0, CompAdj),
  empty_assoc(CW0),
  foldl(scheduler:assign_comp_members(CompAdj, MembersMap, InitMap),
        CompIds, s(CW0, InitMap), s(_, MapOut)).

scheduler:add_comp_adj(edge(CU, CV), In, Out) :-
  ( get_assoc(CU, In, L0) -> true ; L0 = [] ),
  put_assoc(CU, In, [CV|L0], Out).

scheduler:assign_comp_members(CompAdj, MembersMap, InitMap, CompId,
                              s(CW0, HM0), s(CW, HM)) :-
  scheduler:comp_wave(CompId, CompAdj, MembersMap, InitMap, CW0, CW, Wave),
  get_assoc(CompId, MembersMap, Members),
  foldl(scheduler:put_head_wave(Wave), Members, HM0, HM).

scheduler:put_head_wave(Wave, Head, In, Out) :-
  put_assoc(Head, In, Wave, Out).


%! scheduler:comp_wave(+CompId, +CompAdj, +MembersMap, +InitMap, +CWIn, -CWOut, -Wave)
%
% Memoized longest-path wave of one component.

scheduler:comp_wave(CompId, CompAdj, MembersMap, InitMap, CW0, CW, Wave) :-
  ( get_assoc(CompId, CW0, Cached)
  -> Wave = Cached,
     CW = CW0
  ;  get_assoc(CompId, MembersMap, Members),
     foldl(scheduler:max_init_wave(InitMap), Members, 1, Base),
     ( get_assoc(CompId, CompAdj, DepComps) -> true ; DepComps = [] ),
     foldl(scheduler:comp_wave_dep(CompAdj, MembersMap, InitMap),
           DepComps, s(CW0, 0), s(CW1, MaxDep)),
     Wave is max(Base, MaxDep + 1),
     put_assoc(CompId, CW1, Wave, CW)
  ).

scheduler:comp_wave_dep(CompAdj, MembersMap, InitMap, DepId, s(CW0, Max0), s(CW, Max)) :-
  scheduler:comp_wave(DepId, CompAdj, MembersMap, InitMap, CW0, CW, W),
  ( W > Max0 -> Max = W ; Max = Max0 ).

scheduler:max_init_wave(InitMap, Head, In, Out) :-
  ( get_assoc(Head, InitMap, W), W > In -> Out = W ; Out = In ).


% -----------------------------------------------------------------------------
%  Install configure closure (RDEPEND at :install time, portage-ng#21)
% -----------------------------------------------------------------------------
%
% Ebuild `:install` proof bodies carry DEPEND/BDEPEND only. RDEPEND literals
% live on the sibling `:run` rule. Gentoo configure (notably Haskell/cabal
% via ghc-pkg) still needs RDEPEND providers functionally complete before
% configure starts. Map each install-phase head to its `:run` rule's
% `:run`-action body deps and fold them into the repair graph
% (`add_repair_edges/7`).

%! scheduler:build_install_configure_dep_map(+AllRules, -CfgMap)
%
% install/update/downgrade/reinstall head -> sorted list of `:run`-phase
% body deps copied from the matching `:run` rule (when present).

scheduler:build_install_configure_dep_map(AllRules, CfgMap) :-
  empty_assoc(M0),
  foldl(scheduler:add_install_configure_deps, AllRules, M0, CfgMap).

scheduler:add_install_configure_deps(Rule, In, Out) :-
  ( scheduler:rule_head(Rule, RunHead),
    RunHead = _Repo://_Entry:run,
    scheduler:rule_body(Rule, Body)
  -> findall(Dep,
             ( member(Dep, Body),
               \+ constraint:is_constraint(Dep),
               scheduler:dep_is_run_phase(Dep)
             ),
             Deps0),
     ( Deps0 == [] ->
         Out = In
     ;  sort(Deps0, Deps),
        scheduler:install_phase_key(RunHead, InstallKey),
        put_assoc(InstallKey, In, Deps, Out)
     )
  ;  Out = In
  ).

scheduler:dep_is_run_phase(_Repo://_Entry:run) :- !.
scheduler:dep_is_run_phase(grouped_package_dependency(_, _, _, _):run) :- !.
scheduler:dep_is_run_phase(grouped_package_dependency(_, _, _, _):run?_) :- !.

scheduler:install_phase_key(Repo://Entry:run, Repo://Entry:install) :- !.
scheduler:install_phase_key(Repo://Entry:Action, Repo://Entry:install) :-
  memberchk(Action, [install, update, downgrade, reinstall]).


% -----------------------------------------------------------------------------
%  PDEPEND completion ordering (portage-ng#18)
% -----------------------------------------------------------------------------
%
% PDEPEND targets are injected by `heuristic:proof_obligation/4` as detached
% top-level goals tagged with `after_only(P:Action)`, which the planner only
% turns into a `constraint(order_after(P:Action))` (ordering D after P, never
% ordering P's *dependents* after D). The planner topo-sort ignores
% constraints, so a consumer C of provider P -- or P's own `:run` -- could be
% scheduled before P's post-deps were merged, even though P is not functional
% until its PDEPEND closure is installed (e.g. an interpreter whose runtime
% stack arrives via PDEPEND). These helpers re-materialize the missing
% "dependent-of-P after PDEPEND-of-P" edge purely from the generic
% `order_after` marker, with no package-specific knowledge.

%! scheduler:build_pdepend_anchor_map(+AllRules, -AnchorMap)
%
% Map each provider base `Repo://Entry` to the list of heads of rules that
% carry a `constraint(order_after(Repo://Entry:_))` -- i.e. that provider's
% PDEPEND targets. Keyed on the provider base (action-agnostic) so both
% build (`:install`) and runtime (`:run`) dependency edges into the provider
% pick up the same completion wave.

scheduler:build_pdepend_anchor_map(AllRules, AnchorMap) :-
  empty_assoc(M0),
  foldl(scheduler:add_pdepend_anchor, AllRules, M0, AnchorMap).

scheduler:add_pdepend_anchor(Rule, In, Out) :-
  ( scheduler:rule_body(Rule, Body),
    scheduler:rule_head(Rule, DHead),
    findall(C-N,
            ( member(constraint(order_after(Anchor):{_}), Body),
              % `Repo://Entry:Action` parses as `://(Repo, Entry:Action)`
              % because `://` (603) is looser than `:` (601); decompose with
              % the operators rather than a bare `Base:_`. Key on the
              % provider's (Category,Name) so a consumer's grouped dep
              % literal -- which never names a concrete ebuild -- can match.
              Anchor = Repo://Entry:_AnchorAction,
              cache:ordered_entry(Repo, Entry, C, N, _)
            ),
            CNs0),
    CNs0 \== []
  -> sort(CNs0, CNs),
     foldl(scheduler:add_anchor_dhead(DHead), CNs, In, Out)
  ;  Out = In
  ).

scheduler:add_anchor_dhead(DHead, CN, In, Out) :-
  ( get_assoc(CN, In, L0) -> true ; L0 = [] ),
  ( memberchk(DHead, L0) -> L1 = L0 ; L1 = [DHead|L0] ),
  put_assoc(CN, In, L1, Out).


%! scheduler:build_pdepend_closure_map(+AllRules, +AnchorMap, -ClosureMap)
%
% For each provider key C-N (a key of `AnchorMap`), compute the forward
% dependency closure of that provider's PDEPEND target heads -- i.e.
% everything those targets (transitively) depend on. A package that lies in
% this closure is part of the provider's post-install group, so it must NOT
% be ordered after the group (that would create a cycle: a target depends on
% it, yet we would push it after the target).
%
% The closure is computed *per PDEPEND target* and collapsed to package
% (Category,Name) identity (rather than kept as a per-provider union of exact
% heads). Per-target granularity is essential: a provider's PDEPEND group can
% mix targets that cycle back to the consumer with targets that do not. For
% LLVM, `clang` PDEPENDs both `clang-toolchain-symlinks` (which a consumer
% like `compiler-rt` must wait for -- it supplies the `${CHOST}-clang` PATH
% wrappers) and `clang-runtime` (which RDEPENDs `compiler-rt` back, forming a
% cycle). The consumer must be ordered after the former but never the latter.
%
% (C,N) identity (rather than exact heads) is also needed because a PDEPEND
% cycle is typically only visible as a *grouped* / cross-slot literal in the
% closure (e.g. `clang-runtime:22` RDEPENDs `grouped(... clang ...)`), never
% as the concrete sibling-slot head being guarded (`clang-20:install`);
% comparing at (C,N) bridges the grouped/concrete and cross-slot gaps
% (portage-ng#19). `ClosureMap` maps each target head -> assoc(C-N -> true).

scheduler:build_pdepend_closure_map(AllRules, AnchorMap, ClosureMap) :-
  scheduler:build_forward_dep_map(AllRules, FwdMap),
  assoc_to_values(AnchorMap, DHeadLists),
  append(DHeadLists, DHeads0),
  sort(DHeads0, DHeads),
  empty_assoc(C0),
  foldl(scheduler:add_target_closure(FwdMap), DHeads, C0, ClosureMap).

scheduler:add_target_closure(FwdMap, DHead, In, Out) :-
  empty_assoc(V0),
  scheduler:forward_closure([DHead], FwdMap, V0, Closure),
  scheduler:closure_heads_to_cns(Closure, CnSet),
  put_assoc(DHead, In, CnSet, Out).

%! scheduler:closure_heads_to_cns(+Closure, -CnSet)
%
% Collapse a closure of exact heads (assoc head->true) to the set of package
% (Category,Name) pairs they belong to (assoc C-N->true). Heads with no
% package identity (e.g. `assumed(blocker(...))`) are dropped.

scheduler:closure_heads_to_cns(Closure, CnSet) :-
  assoc_to_keys(Closure, Heads),
  empty_assoc(C0),
  foldl(scheduler:add_head_cn, Heads, C0, CnSet).

scheduler:add_head_cn(Head, In, Out) :-
  ( scheduler:head_package(Head, C, N)
  -> put_assoc(C-N, In, true, Out)
  ;  Out = In
  ).

%! scheduler:head_package(+Head, -Category, -Name)
%
% Extract the package (Category,Name) of a rule head, for both grouped
% dependency literals and concrete `Repo://Entry:Action` heads. Fails for
% heads that name no package (assumptions, blockers).

scheduler:head_package(grouped_package_dependency(_G, C, N, _Deps):_Action, C, N) :- !.
scheduler:head_package(Repo://Entry:_Action, C, N) :-
  cache:ordered_entry(Repo, Entry, C, N, _).

%! scheduler:build_forward_dep_map(+AllRules, -FwdMap)
%
% head -> sorted list of its canonical non-constraint body-dep heads.

scheduler:build_forward_dep_map(AllRules, FwdMap) :-
  empty_assoc(M0),
  foldl(scheduler:add_forward_dep, AllRules, M0, FwdMap).

scheduler:add_forward_dep(Rule, In, Out) :-
  ( scheduler:rule_head(Rule, Head),
    scheduler:rule_body(Rule, Body)
  -> findall(DepHead,
             ( member(Dep, Body),
               \+ constraint:is_constraint(Dep),
               prover:canon_literal(Dep, DepHead, _)
             ),
             Deps0),
     sort(Deps0, Deps),
     put_assoc(Head, In, Deps, Out)
  ;  Out = In
  ).

%! scheduler:forward_closure(+Seeds, +FwdMap, +Visited0, -Visited)
%
% BFS over FwdMap accumulating every head reachable from Seeds (seeds
% included). `Visited` is an assoc head->true used as a set.

scheduler:forward_closure([], _FwdMap, V, V).
scheduler:forward_closure([H|Hs], FwdMap, V0, V) :-
  ( get_assoc(H, V0, _)
  -> scheduler:forward_closure(Hs, FwdMap, V0, V)
  ;  put_assoc(H, V0, true, V1),
     ( get_assoc(H, FwdMap, Deps) -> true ; Deps = [] ),
     append(Deps, Hs, Q1),
     scheduler:forward_closure(Q1, FwdMap, V1, V)
  ).


%! scheduler:pdepend_completion_heads(+DepHead, +RuleHead, +Map, +Pd, -EHeads)
%
% If a body dep `DepHead` is a `grouped_package_dependency(_,C,N,_):Action`
% on a provider (C,N) that declares PDEPEND, return the *install*-phase heads
% of that provider's PDEPEND targets, so the consumer is ordered after the
% provider's post-install group (matching emerge, which merges the whole
% interpreter stack before a consuming extension builds; portage-ng#18).
% Matching the consumer's grouped dep literal (rather than a concrete
% `Repo://Entry` head) is essential: the concrete provider-install node is
% shared with the post-install group and would be excluded by the cycle
% filter.
%
% Fails (handled by the caller) when no safe targets exist (see
% `pdepend_safe_targets/4`) or none of the safe targets resolves to an
% in-plan head.

scheduler:pdepend_completion_heads(DepHead, RuleHead, Map, Pd, EHeads) :-
  scheduler:pdepend_safe_targets(DepHead, RuleHead, Pd, SafeDHeads),
  findall(EH,
          ( member(DH, SafeDHeads),
            scheduler:pdepend_effective_head(Map, DH, EH)
          ),
          EHeads0),
  sort(EHeads0, EHeads),
  EHeads \== [].


%! scheduler:pdepend_safe_targets(+DepHead, +RuleHead, +Pd, -SafeDHeads)
%
% The PDEPEND targets of the provider named by grouped dep `DepHead` that
% are safe to order consumer `RuleHead` after.
%
% Cycle-safety (portage-ng#19): PDEPEND targets whose forward closure depends
% back on the consumer's package are filtered out *per target*. A consumer is
% thus ordered after the provider's non-cyclic post-deps (e.g. `compiler-rt`
% after `clang-toolchain-symlinks`) while never being pushed after a post-dep
% that requires it (e.g. `clang-runtime`, which RDEPENDs `compiler-rt`).
% Fails when:
%
%  - no PDEPEND provider exists in the plan (`AnchorMap == t`), or
%  - the dep is not a grouped dep on a PDEPEND provider, or
%  - the consumer is itself one of the provider's PDEPEND targets (a member
%    of the post-install group is never ordered after the whole group --
%    e.g. `clang-toolchain-symlinks` must not wait for its sibling
%    `clang-runtime`; portage-ng#19), or
%  - every PDEPEND target cycles back to the consumer's package.

scheduler:pdepend_safe_targets(DepHead, RuleHead, pd(AnchorMap, ClosureMap), SafeDHeads) :-
  AnchorMap \== t,
  DepHead = grouped_package_dependency(_G, C, N, _Deps):_Action,
  get_assoc(C-N, AnchorMap, DHeads),
  ( scheduler:head_package(RuleHead, RC, RN)
  -> \+ ( member(DH, DHeads), scheduler:head_package(DH, RC, RN) ),
     include(scheduler:pdepend_target_acyclic(ClosureMap, RC-RN), DHeads, SafeDHeads)
  ;  SafeDHeads = DHeads
  ),
  SafeDHeads \== [].

%! scheduler:pdepend_target_acyclic(+ClosureMap, +ConsumerCN, +DHead)
%
% True when PDEPEND target `DHead` does NOT transitively depend on the
% consumer package `ConsumerCN` -- i.e. ordering the consumer after `DHead`
% introduces no cycle.

scheduler:pdepend_target_acyclic(ClosureMap, ConsumerCN, DHead) :-
  \+ ( get_assoc(DHead, ClosureMap, CnSet),
       get_assoc(ConsumerCN, CnSet, _)
     ).

%! scheduler:pdepend_effective_head(+Map, +DHead, -EHead)
%
% The in-plan head a PDEPEND target contributes an ordering edge to. The
% target head may be a `:run` literal (PDEPEND deps resolve to `:run`); use
% the package's `:install` head when planned (post-deps are *installed*
% before the consumer builds), falling back to the target head itself.
% Fails when neither is in the plan.

scheduler:pdepend_effective_head(Map, DHead, EHead) :-
  ( DHead = R://E:_Action,
    get_assoc(R://E:install, Map, _)
  -> EHead = R://E:install
  ;  get_assoc(DHead, Map, _)
  -> EHead = DHead
  ).


% -----------------------------------------------------------------------------
%  Assumed-dep → concrete-action aliasing
% -----------------------------------------------------------------------------
%
% When the prover cannot strictly satisfy a `package_dep on C/N`, it emits
% `[assumed(grouped_package_dependency(C,N,Deps):Action?{Ctx})]` as the body
% conditions for the dep (see `candidate:grouped_dep_build_assumption/7`).
% The matching `rules:rule(assumed(_), [])` has an empty body, so the
% planner ranks the verify rule at wave 1 and the parent never sees the
% real concrete action.
%
% These helpers let the repair pass recover that lost ordering edge by
% maintaining a (PhaseClass-C-N) -> Head map of every concrete planned
% install / update / downgrade / reinstall / run rule and looking each
% assumed dep up against it.

%! scheduler:phase_class(+Action, -PhaseClass)
%
% Classifies a concrete action into the broader phase it satisfies for an
% assumed dep alias. `install` / `update` / `downgrade` / `reinstall` all
% put the package on disk (same satisfaction as a BDEPEND/DEPEND `:install`
% sub-goal); `run` matches an RDEPEND/PDEPEND `:run` sub-goal.

scheduler:phase_class(install,   install_phase).
scheduler:phase_class(update,    install_phase).
scheduler:phase_class(downgrade, install_phase).
scheduler:phase_class(reinstall, install_phase).
scheduler:phase_class(run,       run_phase).


%! scheduler:build_pkg_head_map(+Plan, -PkgHeadMap)
%
% Builds a (PhaseClass-C-N) -> Head map from the concrete planned rule
% heads in `Plan`. The first sighting wins when the same package shows up
% multiple times in a phase class (PlanIn is wave-ordered, so the first
% sighting is the canonical install/run head for this package).

scheduler:build_pkg_head_map(Plan, PkgHeadMap) :-
  empty_assoc(M0),
  foldl([Wave, In, Out]>>foldl(scheduler:add_pkg_head, Wave, In, Out),
        Plan, M0, PkgHeadMap).

scheduler:add_pkg_head(Rule, In, Out) :-
  ( scheduler:rule_head(Rule, Head),
    Head = Repo://Entry:Action,
    scheduler:phase_class(Action, PhaseClass),
    cache:ordered_entry(Repo, Entry, C, N, _)
  ->
    Key = PhaseClass-C-N,
    ( get_assoc(Key, In, _Old) ->
        Out = In
    ;   put_assoc(Key, In, Head, Out)
    )
  ; Out = In
  ).


%! scheduler:assumed_dep_alias_key(+Dep, -Key)
%
% If `Dep` is an assumed package dependency literal (grouped or legacy
% per-dep form, with or without a `?{Ctx}` annotation), returns the
% (PhaseClass-C-N) key under which a concrete planned action for the same
% package would appear in `PkgWaveMap`. Fails for any other body element.

scheduler:assumed_dep_alias_key(assumed(Inner), Key) :-
  \+ scheduler:assumed_inner_phantom(Inner),
  !,
  scheduler:assumed_inner_alias_key(Inner, Key).
scheduler:assumed_dep_alias_key(assumed(Inner)?{_}, Key) :-
  \+ scheduler:assumed_inner_phantom(Inner),
  !,
  scheduler:assumed_inner_alias_key(Inner, Key).


%! scheduler:assumed_inner_phantom(+Inner) is semidet.
%
% Assumed deps tagged with a phantom reason (or REQUIRED_USE violation)
% must not inherit a concrete install wave from another path.

scheduler:assumed_inner_phantom(Inner) :-
  explainer:term_ctx(Inner, Ctx),
  memberchk(required_use_violation(_), Ctx),
  !.
scheduler:assumed_inner_phantom(Inner) :-
  explainer:term_ctx(Inner, Ctx),
  memberchk(assumption_reason(Reason), Ctx),
  scheduler:assumed_inner_pkg(Inner, C, N),
  explanation:phantom_grouped_dep_assumption(Reason, C, N).

scheduler:assumed_inner_alias_key((Body:Action)?{_}, PhaseClass-C-N) :-
  !,
  scheduler:assumed_inner_pkg(Body, C, N),
  scheduler:phase_class(Action, PhaseClass).
scheduler:assumed_inner_alias_key(Body:Action, PhaseClass-C-N) :-
  scheduler:assumed_inner_pkg(Body, C, N),
  scheduler:phase_class(Action, PhaseClass).

scheduler:assumed_inner_pkg(grouped_package_dependency(C, N, _Deps),     C, N) :- !.
scheduler:assumed_inner_pkg(package_dependency(_, _, C, N, _, _, _, _), C, N).


%! scheduler:grouped_run_dep_pkg_key(+Dep, -Key)
%
% Maps a concrete grouped RDEPEND literal to the `(run_phase-C-N)` key in
% `PkgWaveMap`, so configure-closure deps resolve to the provider's merged
% `:run` wave even when the plan only records concrete ebuild heads.

scheduler:grouped_run_dep_pkg_key(grouped_package_dependency(_, C, N, _):run, run_phase-C-N) :- !.
scheduler:grouped_run_dep_pkg_key(grouped_package_dependency(_, C, N, _):run?{_}, run_phase-C-N) :- !.


%! scheduler:rebuild_plan_from_map(+AllRules, +Map, -Plan)
%
% Group rules by their wave assignment in the map. Preserves relative
% order within each wave (keysort is stable).

scheduler:rebuild_plan_from_map(AllRules, Map, Plan) :-
  scheduler:assign_waves(AllRules, Map, 1, Pairs),
  keysort(Pairs, Sorted),
  group_pairs_by_key(Sorted, Grouped),
  pairs_values(Grouped, Plan).

scheduler:assign_waves([], _, _, []).
scheduler:assign_waves([Rule|Rules], Map, FallbackWave, [Wave-Rule|Rest]) :-
  ( scheduler:rule_head(Rule, Head),
    get_assoc(Head, Map, Wave0)
  -> Wave = Wave0
  ; Wave = FallbackWave
  ),
  scheduler:assign_waves(Rules, Map, FallbackWave, Rest).


%! scheduler:build_head_wave_map(+Plan, +WaveIdx, +MapIn, -MapOut)
%
% Assigns each rule head in the plan to its wave index.

scheduler:build_head_wave_map([], _Idx, Map, Map).
scheduler:build_head_wave_map([Wave|Waves], Idx, MapIn, MapOut) :-
  foldl(scheduler:add_head_wave(Idx), Wave, MapIn, MapMid),
  Idx1 is Idx + 1,
  scheduler:build_head_wave_map(Waves, Idx1, MapMid, MapOut).

scheduler:add_head_wave(Idx, Rule, MapIn, MapOut) :-
  ( scheduler:rule_head(Rule, Head) ->
      put_assoc(Head, MapIn, Idx, MapOut)
  ; MapOut = MapIn
  ).


% -----------------------------------------------------------------------------
%  Perf counters (whole-repo runs)
% -----------------------------------------------------------------------------
%
% Aggregates SCC/remainder characteristics over many schedule/6 calls.
% Reset/report is triggered by prover:test/*.

scheduler:perf_reset :-
  flag(sch_perf_entries, _OldE, 0),
  flag(sch_perf_nontrivial, _OldNT, 0),
  flag(sch_perf_heads_sum, _OldH, 0),
  flag(sch_perf_scc_sum, _OldS, 0),
  flag(sch_perf_comps_sum, _OldC, 0),
  flag(sch_perf_blocked_sum, _OldB, 0),
  flag(sch_perf_waves_sum, _OldW, 0),
  flag(sch_perf_wave_comps_sum, _OldWC, 0),
  flag(sch_perf_added_rules_sum, _OldAR, 0),
  flag(sch_perf_runtime_sccs, _OldRS, 0),
  flag(sch_perf_build_sccs, _OldBS, 0),
  !.

scheduler:perf_add(HeadsN, SCCsN, CompsN, BlockedN, WavesN, WavesCompN, AddedRulesN, Nontrivial) :-
  flag(sch_perf_entries, E0, E0+1),
  flag(sch_perf_nontrivial, NT0, NT0+Nontrivial),
  flag(sch_perf_heads_sum, H0, H0+HeadsN),
  flag(sch_perf_scc_sum, S0, S0+SCCsN),
  flag(sch_perf_comps_sum, C0, C0+CompsN),
  flag(sch_perf_blocked_sum, B0, B0+BlockedN),
  flag(sch_perf_waves_sum, W0, W0+WavesN),
  flag(sch_perf_wave_comps_sum, WC0, WC0+WavesCompN),
  flag(sch_perf_added_rules_sum, AR0, AR0+AddedRulesN),
  !.

scheduler:perf_report :-
  flag(sch_perf_entries, E, E),
  ( E =:= 0 ->
      true
  ; flag(sch_perf_nontrivial, NT, NT),
    flag(sch_perf_heads_sum, H, H),
    flag(sch_perf_scc_sum, S, S),
    flag(sch_perf_comps_sum, C, C),
    flag(sch_perf_blocked_sum, B, B),
    flag(sch_perf_waves_sum, W, W),
    flag(sch_perf_wave_comps_sum, WC, WC),
    flag(sch_perf_added_rules_sum, AR, AR),
    flag(sch_perf_runtime_sccs, RS, RS),
    flag(sch_perf_build_sccs, BS, BS),
    AvgH is H / E,
    AvgS is S / E,
    AvgC is C / E,
    AvgB is B / E,
    AvgW is W / E,
    AvgWC is WC / E,
    AvgAR is AR / E,
    message:scroll_notice(['scheduler perf: entries=',E,
                           ' nontrivial=',NT,
                           ' heads_sum=',H,' avg=',AvgH,
                           ' scc_sum=',S,' avg=',AvgS,
                           ' comps_sum=',C,' avg=',AvgC,
                           ' blocked_sum=',B,' avg=',AvgB,
                           ' waves_sum=',W,' avg=',AvgW,
                           ' wave_comps_sum=',WC,' avg=',AvgWC,
                           ' added_rules_sum=',AR,' avg=',AvgAR,
                           ' runtime_sccs=',RS,' build_sccs=',BS])
  ),
  nl,
  !.

scheduler:count_wave_components(Waves, Total) :-
  findall(N, (member(W, Waves), length(W, N)), Ns),
  sum_list(Ns, Total),
  !.

scheduler:count_rules_in_plan(Plan, Count) :-
  findall(N, (member(Step, Plan), length(Step, N)), Ns),
  sum_list(Ns, Count),
  !.


% -----------------------------------------------------------------------------
%  Plan / closure helpers
% -----------------------------------------------------------------------------

% Collect all heads present in a plan.
scheduler:plan_heads(Plan, Heads) :-
  findall(H,
          ( member(Step, Plan),
            member(Rule, Step),
            scheduler:rule_head(Rule, H)
          ),
          Hs0),
  sort(Hs0, Heads).

% Compute the dependency closure starting from SeedHeads, but only:
% - include :run heads
% - include heads that are already present in the plan (to keep this bounded)
scheduler:run_closure_in_plan(SeedHeads, PlanHeads, ProofAVL, ClosureHeads) :-
  empty_assoc(V0),
  include(scheduler:is_run_head, SeedHeads, SeedsRun0),
  sort(SeedsRun0, SeedsRun),
  scheduler:closure_queue(SeedsRun, PlanHeads, ProofAVL, V0, _V, [], Closure0),
  sort(Closure0, ClosureHeads).

scheduler:is_run_head(Head) :-
  compound(Head),
  Head =.. [':', _Target, run].

scheduler:closure_queue([], _PlanHeads, _ProofAVL, V, V, Acc, Acc).
scheduler:closure_queue([H|Hs], PlanHeads, ProofAVL, V0, V, Acc0, Acc) :-
  ( get_assoc(H, V0, true) ->
      scheduler:closure_queue(Hs, PlanHeads, ProofAVL, V0, V, Acc0, Acc)
  ; put_assoc(H, V0, true, V1),
    scheduler:deps_in_plan_run(H, PlanHeads, ProofAVL, Deps),
    append(Deps, Hs, Q1),
    scheduler:closure_queue(Q1, PlanHeads, ProofAVL, V1, V, [H|Acc0], Acc)
  ).

scheduler:deps_in_plan_run(Head, PlanHeads, ProofAVL, Deps) :-
  ( scheduler:get_full_rule_from_proof(Head, ProofAVL, Rule) ->
      ( Rule = rule(_HeadWithCtx, Body)
      ; Rule = assumed(rule(_HeadWithCtx, Body))
      ; Rule = rule(assumed(_HeadWithCtx), Body)
      ),
      findall(DepHead0,
              ( member(Dep, Body),
                \+ constraint:is_constraint(Dep),
                prover:canon_literal(Dep, DepHead0, _),
                scheduler:is_run_head(DepHead0),
                memberchk(DepHead0, PlanHeads)
              ),
              Deps0),
      sort(Deps0, Deps)
  ; Deps = []
  ).

% Remove rules whose heads are in RemoveHeads from a plan.
scheduler:remove_heads_from_plan(RemoveHeads, PlanIn, PlanOut) :-
  maplist(scheduler:remove_heads_from_step(RemoveHeads), PlanIn, Plan1),
  exclude(==( [] ), Plan1, PlanOut).

scheduler:remove_heads_from_step(RemoveHeads, StepIn, StepOut) :-
  findall(Rule,
          ( member(Rule, StepIn),
            scheduler:rule_head(Rule, H),
            \+ memberchk(H, RemoveHeads)
          ),
          StepOut).


% -----------------------------------------------------------------------------
%  Test helpers (mirror planner.pl)
% -----------------------------------------------------------------------------

%! scheduler:test(+Repository)
%
% Tests the scheduler (prove -> plan -> schedule).
%
scheduler:test(Repository) :-
  config:test_style(Style),
  scheduler:test(Repository, Style).

%! scheduler:test(+Repository,+Style)
scheduler:test(Repository, Style) :-
  config:proving_target(Action0),
  prover:test_action(Action0, Action),
  tester:test(Style, 'Scheduling', Repository://Entry, (Repository:entry(Entry)),
    ( pipeline:prove_with_fallback([Repository://Entry:Action?{[]}],ProofAVL,_ModelAVL,TriggersAVL),
      planner:plan(ProofAVL,TriggersAVL,t,Plan0,Remainder0),
      scheduler:schedule(ProofAVL,TriggersAVL,Plan0,Remainder0,_Plan,_Remainder)
    )),
  nl.

%! scheduler:test_latest(+Repository)
%
% Same as scheduler:test(+Repository), but only tests highest version of every package.
%
scheduler:test_latest(Repository) :-
  config:test_style(Style),
  scheduler:test_latest(Repository, Style).

%! scheduler:test_latest(+Repository,+Style)
scheduler:test_latest(Repository, Style) :-
  config:proving_target(Action0),
  prover:test_action(Action0, Action),
  tester:test(Style, 'Scheduling latest', Repository://Entry,
              (Repository:package(C,N),once(Repository:ebuild(Entry,C,N,_))),
              ( pipeline:prove_with_fallback([Repository://Entry:Action?{[]}],ProofAVL,_ModelAVL,TriggersAVL),
                planner:plan(ProofAVL,TriggersAVL,t,Plan0,Remainder0),
                scheduler:schedule(ProofAVL,TriggersAVL,Plan0,Remainder0,_Plan,_Remainder)
              )),
  nl.

% -----------------------------------------------------------------------------
%  Testing + statistics (mirror planner.pl)
% -----------------------------------------------------------------------------

%! scheduler:test_stats(+Repository)
scheduler:test_stats(Repository) :-
  config:test_style(Style),
  scheduler:test_stats(Repository, Style).

%! scheduler:test_stats(+Repository,+Style)
scheduler:test_stats(Repository, Style) :-
  config:proving_target(Action0),
  prover:test_action(Action0, Action),
  aggregate_all(count, (Repository:entry(_E)), ExpectedTotal),
  sampler:reset('Scheduling', ExpectedTotal),
  aggregate_all(count, (Repository:package(_C,_N)), ExpectedPkgs),
  sampler:set_expected_pkgs(ExpectedPkgs),
  tester:test(Style,
              'Scheduling',
              Repository://Entry,
              (Repository:entry(Entry)),
              ( pipeline:prove_with_fallback([Repository://Entry:Action?{[]}],ProofAVL,ModelAVL,TriggersAVL),
                planner:plan(ProofAVL,TriggersAVL,t,Plan0,Remainder0),
                scheduler:schedule(ProofAVL,TriggersAVL,Plan0,Remainder0,_Plan,_Remainder),
                sampler:record(entry(Repository://Entry, ModelAVL, ProofAVL, TriggersAVL, true))
              )),
  stats:test_stats_print.


% -----------------------------------------------------------------------------
%  Remainder extraction
% -----------------------------------------------------------------------------

scheduler:remainder_heads(RemainderRules, Heads) :-
  findall(Head,
          ( member(Rule, RemainderRules),
            scheduler:rule_head(Rule, Head)
          ),
          Heads0),
  sort(Heads0, Heads).

scheduler:rule_head(Rule, Head) :-
  ( Rule = rule(HeadWithCtx, _Body)
  ; Rule = assumed(rule(HeadWithCtx, _Body))
  ; Rule = rule(assumed(HeadWithCtx), _Body)
  ),
  prover:canon_literal(HeadWithCtx, Head, _).


% -----------------------------------------------------------------------------
%  Graph extraction (remainder-induced)
% -----------------------------------------------------------------------------

scheduler:build_forward_reverse(Heads, ProofAVL, Forward, Reverse) :-
  empty_assoc(EmptyF),
  foldl(scheduler:forward_put(Heads, ProofAVL), Heads, EmptyF, Forward0),
  scheduler:invert_graph(Heads, Forward0, Reverse0),
  Forward = Forward0,
  Reverse = Reverse0.

scheduler:forward_put(Heads, ProofAVL, Head, In, Out) :-
  scheduler:get_full_rule_from_proof(Head, ProofAVL, Rule),
  ( Rule = rule(_HeadWithCtx, Body)
  ; Rule = assumed(rule(_HeadWithCtx, Body))
  ; Rule = rule(assumed(_HeadWithCtx), Body)
  ),
  findall(DepHead,
          ( member(Dep, Body),
            \+ constraint:is_constraint(Dep),
            prover:canon_literal(Dep, DepHead, _),
            memberchk(DepHead, Heads)
          ),
          Deps0),
  sort(Deps0, Deps),
  put_assoc(Head, In, Deps, Out).

% Build a head->rule assoc from the remainder rules list (planner already fetched
% full rules from the proof).
scheduler:remainder_head_rule_map(RemainderRules, Map) :-
  empty_assoc(M0),
  foldl(scheduler:remainder_head_rule_put, RemainderRules, M0, Map),
  !.

scheduler:remainder_head_rule_put(Rule, In, Out) :-
  scheduler:rule_head(Rule, Head),
  % If duplicates exist, keep the first encountered rule (stable).
  ( get_assoc(Head, In, _) ->
      Out = In
  ; put_assoc(Head, In, Rule, Out)
  ).

% Faster graph extraction using the remainder head->rule map and a heads set.
scheduler:build_forward_reverse_from_rules(Heads, HeadRuleMap, Forward, Reverse) :-
  scheduler:heads_set_assoc(Heads, HeadSet),
  empty_assoc(EmptyF),
  foldl(scheduler:forward_put_from_map(HeadRuleMap, HeadSet), Heads, EmptyF, Forward0),
  scheduler:invert_graph(Heads, Forward0, Reverse0),
  Forward = Forward0,
  Reverse = Reverse0.

scheduler:heads_set_assoc(Heads, Set) :-
  empty_assoc(S0),
  foldl(scheduler:assoc_set_put, Heads, S0, Set),
  !.

scheduler:forward_put_from_map(HeadRuleMap, HeadSet, Head, In, Out) :-
  ( get_assoc(Head, HeadRuleMap, Rule) ->
      scheduler:rule_body(Rule, Body),
      findall(DepHead,
              ( member(Dep, Body),
                \+ constraint:is_constraint(Dep),
                prover:canon_literal(Dep, DepHead, _),
                get_assoc(DepHead, HeadSet, true)
              ),
              Deps0),
      sort(Deps0, Deps),
      put_assoc(Head, In, Deps, Out)
  ; % Should not happen, but keep scheduler total.
    put_assoc(Head, In, [], Out)
  ).

scheduler:rule_body(Rule, Body) :-
  ( Rule = rule(_HeadWithCtx, Body)
  ; Rule = assumed(rule(_HeadWithCtx, Body))
  ; Rule = rule(assumed(_HeadWithCtx), Body)
  ),
  !.

scheduler:invert_graph(Heads, Forward, Reverse) :-
  empty_assoc(Empty),
  foldl(scheduler:init_empty_neighbors, Heads, Empty, R0),
  assoc:assoc_to_list(Forward, Pairs),
  foldl(scheduler:invert_edges, Pairs, R0, Reverse).

scheduler:init_empty_neighbors(Node, In, Out) :-
  ( get_assoc(Node, In, _) -> Out = In ; put_assoc(Node, In, [], Out) ).

scheduler:invert_edges(Node-Neighbors, In, Out) :-
  foldl(scheduler:invert_edge(Node), Neighbors, In, Out).

scheduler:invert_edge(From, To, In, Out) :-
  ( get_assoc(To, In, Ns0) -> true ; Ns0 = [] ),
  ( memberchk(From, Ns0) -> Ns = Ns0 ; Ns = [From|Ns0] ),
  put_assoc(To, In, Ns, Out).


% -----------------------------------------------------------------------------
%  Kosaraju SCC
% -----------------------------------------------------------------------------

scheduler:kosaraju_scc(Nodes, Forward, Reverse, SCCs) :-
  empty_assoc(V0),
  scheduler:finish_order(Nodes, Forward, V0, _V1, [], Order0),
  empty_assoc(V2),
  scheduler:collect_sccs(Order0, Reverse, V2, _V3, [], SCCs0),
  reverse(SCCs0, SCCs).

scheduler:finish_order([], _Forward, V, V, Order, Order).
scheduler:finish_order([N|Ns], Forward, V0, V, Order0, Order) :-
  ( get_assoc(N, V0, true) ->
      scheduler:finish_order(Ns, Forward, V0, V, Order0, Order)
  ; put_assoc(N, V0, true, V1),
    scheduler:dfs_finish(N, Forward, V1, V2, Order0, Order1),
    scheduler:finish_order(Ns, Forward, V2, V, Order1, Order)
  ).

scheduler:dfs_finish(N, Forward, V0, V, Order0, Order) :-
  ( get_assoc(N, Forward, Neigh) -> true ; Neigh = [] ),
  scheduler:finish_order(Neigh, Forward, V0, V1, Order0, Order1),
  Order = [N|Order1],
  V = V1.

scheduler:collect_sccs([], _Reverse, V, V, SCCs, SCCs).
scheduler:collect_sccs([N|Ns], Reverse, V0, V, SCCs0, SCCs) :-
  ( get_assoc(N, V0, true) ->
      scheduler:collect_sccs(Ns, Reverse, V0, V, SCCs0, SCCs)
  ; put_assoc(N, V0, true, V1),
    scheduler:dfs_collect(N, Reverse, V1, V2, [], Members0),
    sort(Members0, Members),
    scheduler:collect_sccs(Ns, Reverse, V2, V, [Members|SCCs0], SCCs)
  ).

scheduler:dfs_collect(N, Reverse, V0, V, Acc0, Acc) :-
  Acc1 = [N|Acc0],
  ( get_assoc(N, Reverse, Neigh) -> true ; Neigh = [] ),
  scheduler:dfs_collect_list(Neigh, Reverse, V0, V, Acc1, Acc).

scheduler:dfs_collect_list([], _Reverse, V, V, Acc, Acc).
scheduler:dfs_collect_list([M|Ms], Reverse, V0, V, Acc0, Acc) :-
  ( get_assoc(M, V0, true) ->
      scheduler:dfs_collect_list(Ms, Reverse, V0, V, Acc0, Acc)
  ; put_assoc(M, V0, true, V1),
    scheduler:dfs_collect(M, Reverse, V1, V2, Acc0, Acc1),
    scheduler:dfs_collect_list(Ms, Reverse, V2, V, Acc1, Acc)
  ).


% -----------------------------------------------------------------------------
%  Components and scheduling
% -----------------------------------------------------------------------------

scheduler:build_components(SCCs, Forward, CompMap, Comps) :-
  empty_assoc(M0),
  scheduler:build_components_(SCCs, Forward, 1, M0, CompMap, [], CompsRev),
  reverse(CompsRev, Comps).

scheduler:build_components_([], _Forward, _I, M, M, Comps, Comps).
scheduler:build_components_([Members|Rest], Forward, I, M0, M, Comps0, Comps) :-
  scheduler:component_kind(Members, Forward, Kind),
  foldl(scheduler:compmap_put(I), Members, M0, M1),
  Comps1 = [comp(I, Kind, Members)|Comps0],
  I1 is I + 1,
  scheduler:build_components_(Rest, Forward, I1, M1, M, Comps1, Comps).

scheduler:compmap_put(Id, Node, In, Out) :-
  put_assoc(Node, In, Id, Out).

% Component kind:
% - merge_set: cyclic SCC of mergeable literals (:run or merge actions)
% - bad: cyclic SCC containing any other literal kind
% - single: singleton SCC with no self-loop
scheduler:component_kind(Members, Forward, Kind) :-
  ( Members = [Only] ->
      ( scheduler:self_loop(Only, Forward) ->
          ( scheduler:all_mergeable(Members) ->
              Kind = merge_set,
              scheduler:classify_scc_priority(Members)
          ; Kind = bad
          )
      ; Kind = single
      )
  ; % size > 1
    ( scheduler:all_mergeable(Members) ->
        Kind = merge_set,
        scheduler:classify_scc_priority(Members)
    ; Kind = bad
    )
  ).

% Track whether a merge-set SCC is runtime-only (all :run nodes) or contains
% build actions.  After the planner's relaxation pass, remaining runtime-only
% SCCs indicate cycles that could not be resolved even with relaxation.
scheduler:classify_scc_priority(Members) :-
  ( forall(member(M, Members), scheduler:is_run_literal(M)) ->
      flag(sch_perf_runtime_sccs, R0, R0+1)
  ;
      flag(sch_perf_build_sccs, B0, B0+1)
  ).

scheduler:self_loop(Node, Forward) :-
  get_assoc(Node, Forward, Ns),
  memberchk(Node, Ns).

scheduler:all_mergeable([]).
scheduler:all_mergeable([H|T]) :-
  scheduler:is_mergeable_literal(H),
  scheduler:all_mergeable(T).

scheduler:is_run_literal(_Repo://_Ebuild:run) :- !.
scheduler:is_run_literal(_Something:run) :- !.

% Merge actions that can be part of a merge set SCC.
scheduler:is_merge_action_literal(_Repo://_Ebuild:install) :- !.
scheduler:is_merge_action_literal(_Repo://_Ebuild:update) :- !.
scheduler:is_merge_action_literal(_Repo://_Ebuild:downgrade) :- !.
scheduler:is_merge_action_literal(_Repo://_Ebuild:reinstall) :- !.
scheduler:is_merge_action_literal(_Something:install) :- !.
scheduler:is_merge_action_literal(_Something:update) :- !.
scheduler:is_merge_action_literal(_Something:downgrade) :- !.
scheduler:is_merge_action_literal(_Something:reinstall) :- !.

scheduler:is_mergeable_literal(H) :-
  ( scheduler:is_run_literal(H)
  ; scheduler:is_merge_action_literal(H)
  ),
  !.

% Compute the set of components that are blocked (unschedulable):
% - all 'bad' cyclic components
% - all components that (transitively) depend on a bad component
scheduler:blocked_components(Comps, Forward, CompMap, BlockedIds) :-
  findall(Id, member(comp(Id, bad, _), Comps), BadIds0),
  sort(BadIds0, BadIds),
  scheduler:comp_edges(Forward, CompMap, Edges),
  scheduler:reverse_comp_edges(Edges, RevEdges),
  scheduler:closure_from(BadIds, RevEdges, BadClosure),
  sort(BadClosure, BlockedIds).

scheduler:comp_edges(Forward, CompMap, Edges) :-
  assoc:assoc_to_list(Forward, Pairs),
  findall(edge(CU, CV),
          ( member(U-Ns, Pairs),
            get_assoc(U, CompMap, CU),
            member(V, Ns),
            get_assoc(V, CompMap, CV),
            CU \= CV
          ),
          Edges0),
  sort(Edges0, Edges).

scheduler:reverse_comp_edges(Edges, RevEdges) :-
  findall(edge(To, From), member(edge(From, To), Edges), Rev0),
  sort(Rev0, RevEdges).

scheduler:closure_from(Seeds, RevEdges, Closure) :-
  empty_assoc(V0),
  scheduler:closure_queue(Seeds, RevEdges, V0, _V, [], Closure).

scheduler:closure_queue([], _RevEdges, V, V, Acc, Acc).
scheduler:closure_queue([X|Xs], RevEdges, V0, V, Acc0, Acc) :-
  ( get_assoc(X, V0, true) ->
      scheduler:closure_queue(Xs, RevEdges, V0, V, Acc0, Acc)
  ; put_assoc(X, V0, true, V1),
    findall(N, member(edge(X, N), RevEdges), Ns),
    append(Ns, Xs, Q1),
    scheduler:closure_queue(Q1, RevEdges, V1, V, [X|Acc0], Acc)
  ).

scheduler:schedulable_component_waves(Comps, Forward, CompMap, BlockedIds, Waves) :-
  scheduler:comp_edges(Forward, CompMap, Edges),
  findall(Id, (member(comp(Id, Kind, _), Comps), Kind \= bad, \+ memberchk(Id, BlockedIds)), Sched0),
  sort(Sched0, Sched),
  % The SCC condensation is a DAG; order it into parallel waves via the shared
  % Kahn engine. edge(From,To) means "From depends on To".
  kahn:waves(Sched, Edges, Waves).

scheduler:assoc_set_from_list(List, Set) :-
  empty_assoc(S0),
  foldl(scheduler:assoc_set_put, List, S0, Set),
  !.

scheduler:expand_component_waves_from_map([], _Comps, _Forward, _HeadRuleMap, []).
scheduler:expand_component_waves_from_map([WaveIds|Rest], Comps, Forward, HeadRuleMap, AllWaves) :-
  findall(CompWaves,
          ( member(Id, WaveIds),
            member(comp(Id, Kind, Members), Comps),
            scheduler:expand_component(Kind, Members, Forward, HeadRuleMap, CompWaves)
          ),
          ComponentWaveLists),
  scheduler:merge_component_waves(ComponentWaveLists, MergedWaves),
  scheduler:expand_component_waves_from_map(Rest, Comps, Forward, HeadRuleMap, RestWaves),
  append(MergedWaves, RestWaves, AllWaves).


%! scheduler:merge_component_waves(+ComponentWaveLists, -MergedWaves)
%
% Merge wave lists from independent components in the same Kahn wave.
% Single-wave components are collected into a shared first wave (legitimate
% parallelism). Multi-wave SCC components emit their waves sequentially.

scheduler:merge_component_waves(ComponentWaveLists, MergedWaves) :-
  partition(scheduler:is_single_wave, ComponentWaveLists, Singles, Multis),
  append(Singles, FlatSingleRules),
  append(FlatSingleRules, SharedWave),
  findall(W, (member(Ws, Multis), member(W, Ws)), MultiWaves),
  ( SharedWave == []
  -> MergedWaves = MultiWaves
  ; MergedWaves = [SharedWave | MultiWaves]
  ).

scheduler:is_single_wave([_]).

% Expand a single component into a list of waves (list of lists of rules).
% merge_set SCCs with multiple members get priority-aware linearization
% producing multiple waves; everything else is a single wave.

scheduler:expand_component(merge_set, Members, Forward, HeadRuleMap, Waves) :-
  length(Members, N), N > 1, !,
  scheduler:linearize_scc(Members, Forward, HeadRuleMap, Waves).
scheduler:expand_component(_Kind, Members, _Forward, HeadRuleMap, [Rules]) :-
  scheduler:scc_get_rules(Members, HeadRuleMap, Rules).

% Retrieve rules for a list of SCC member heads.
scheduler:scc_get_rules(Members, HeadRuleMap, Rules) :-
  findall(Rule,
          ( member(H, Members),
            get_assoc(H, HeadRuleMap, Rule)
          ),
          Rules).


% =============================================================================
%  Priority-aware SCC linearization (Portage-like progressive relaxation)
% =============================================================================
%
% Within a merge-set SCC, iteratively select "ready" nodes whose SCC-internal
% dependencies are all satisfied, using progressive relaxation:
%
%   Phase 1: All internal edges are hard constraints.
%   Phase 2: Relax :run edges (RDEPEND/PDEPEND) — these are soft.
%   Phase 3: Hard cycle (all remaining edges are build-time) — pick node
%            with fewest unsatisfied hard deps (best available).
%
% This matches Portage's _serialize_tasks() cycle linearization.

%! scheduler:linearize_scc(+Members, +Forward, +HeadRuleMap, -OrderedWaves)
%
% Returns a list of waves (list of lists of rules) respecting internal deps.

scheduler:linearize_scc(Members, Forward, HeadRuleMap, OrderedWaves) :-
  scheduler:scc_internal_forward(Members, Forward, IntFwd),
  scheduler:scc_internal_forward_no_run(Members, Forward, IntFwdNoRun),
  empty_assoc(Done0),
  scheduler:linearize_iter(Members, IntFwd, IntFwdNoRun, HeadRuleMap, Done0, [], OrderedWaves).

% Build SCC-internal forward edges: for each member, keep only deps that
% are also SCC members.
scheduler:scc_internal_forward(Members, Forward, IntFwd) :-
  scheduler:assoc_set_from_list(Members, MemberSet),
  empty_assoc(IF0),
  foldl(scheduler:scc_int_fwd_node(Forward, MemberSet), Members, IF0, IntFwd).

scheduler:scc_int_fwd_node(Forward, MemberSet, Node, In, Out) :-
  ( get_assoc(Node, Forward, AllDeps) -> true ; AllDeps = [] ),
  include(scheduler:in_member_set(MemberSet), AllDeps, InternalDeps),
  put_assoc(Node, In, InternalDeps, Out).

scheduler:in_member_set(MemberSet, Node) :-
  get_assoc(Node, MemberSet, _).

% Same as above but excluding :run deps.
scheduler:scc_internal_forward_no_run(Members, Forward, IntFwd) :-
  scheduler:assoc_set_from_list(Members, MemberSet),
  empty_assoc(IF0),
  foldl(scheduler:scc_int_fwd_node_no_run(Forward, MemberSet), Members, IF0, IntFwd).

scheduler:scc_int_fwd_node_no_run(Forward, MemberSet, Node, In, Out) :-
  ( get_assoc(Node, Forward, AllDeps) -> true ; AllDeps = [] ),
  include(scheduler:is_non_run_internal(MemberSet), AllDeps, InternalDeps),
  put_assoc(Node, In, InternalDeps, Out).

scheduler:is_non_run_internal(MemberSet, Dep) :-
  get_assoc(Dep, MemberSet, _),
  \+ scheduler:is_run_literal(Dep).

% Iterative linearization: extract ready nodes in waves.
%
% Phase 1: Schedule all nodes whose ALL SCC-internal deps are satisfied.
% Phase 2: Relax :run edges — pick ONE node at a time (fewest unsatisfied
%          :run deps first, matching Portage's one-at-a-time approach),
%          then re-enter phase 1 so completed :run deps can cascade.
% Phase 3: Hard cycle (all remaining edges are build-time) — pick the node
%          with fewest unsatisfied hard deps.
%
% Returns a list of waves (each wave is a list of rules that can be parallel).

scheduler:linearize_iter([], _, _, _, _, Acc, Acc) :- !.
scheduler:linearize_iter(Remaining, IntFwd, IntFwdNoRun, HRM, Done, Acc, Result) :-
  scheduler:scc_ready_nodes(Remaining, IntFwd, Done, Ready1),
  ( Ready1 \= [] ->
      sort(Ready1, ReadySorted),
      scheduler:scc_get_rules(ReadySorted, HRM, Rules),
      append(Acc, [Rules], Acc1),
      foldl(scheduler:mark_done, ReadySorted, Done, Done1),
      subtract(Remaining, ReadySorted, Remaining1),
      scheduler:linearize_iter(Remaining1, IntFwd, IntFwdNoRun, HRM, Done1, Acc1, Result)
  ;
      % Phase 2: relax :run edges — schedule ONE node, then re-enter phase 1
      scheduler:scc_ready_nodes(Remaining, IntFwdNoRun, Done, Ready2),
      ( Ready2 \= [] ->
          scheduler:pick_best_relaxed_node(Ready2, IntFwd, Done, Best),
          scheduler:scc_get_rules([Best], HRM, Rules),
          append(Acc, [Rules], Acc1),
          scheduler:mark_done(Best, Done, Done1),
          subtract(Remaining, [Best], Remaining1),
          scheduler:linearize_iter(Remaining1, IntFwd, IntFwdNoRun, HRM, Done1, Acc1, Result)
      ;
          % Phase 3: hard cycle — pick node with fewest unsatisfied hard deps
          scheduler:pick_best_cycle_node(Remaining, IntFwdNoRun, Done, Best),
          scheduler:scc_get_rules([Best], HRM, Rules),
          append(Acc, [Rules], Acc1),
          scheduler:mark_done(Best, Done, Done1),
          subtract(Remaining, [Best], Remaining1),
          scheduler:linearize_iter(Remaining1, IntFwd, IntFwdNoRun, HRM, Done1, Acc1, Result)
      )
  ).

% Nodes whose SCC-internal deps (per the given forward map) are all done.
scheduler:scc_ready_nodes(Remaining, IntFwd, Done, Ready) :-
  findall(N,
          ( member(N, Remaining),
            get_assoc(N, IntFwd, Deps),
            forall(member(D, Deps), get_assoc(D, Done, _))
          ),
          Ready).

scheduler:mark_done(Node, In, Out) :- put_assoc(Node, In, true, Out).

% Among relaxed-ready nodes, pick the one with fewest unsatisfied deps in
% the FULL internal graph (including :run).  This schedules the node that
% is closest to being fully ready, allowing its :run deps to cascade.
scheduler:pick_best_relaxed_node(Ready, IntFwd, Done, Best) :-
  findall(Count-Node,
          ( member(Node, Ready),
            get_assoc(Node, IntFwd, Deps),
            include(scheduler:not_done(Done), Deps, Unsatisfied),
            length(Unsatisfied, Count)
          ),
          Pairs),
  keysort(Pairs, [_-Best|_]).

% When all remaining nodes have unsatisfied hard deps, pick the one with
% the fewest — breaking the tightest cycle first (like Portage's
% smallest-cycle heuristic).
scheduler:pick_best_cycle_node(Remaining, IntFwdNoRun, Done, Best) :-
  findall(Count-Node,
          ( member(Node, Remaining),
            get_assoc(Node, IntFwdNoRun, Deps),
            include(scheduler:not_done(Done), Deps, Unsatisfied),
            length(Unsatisfied, Count)
          ),
          Pairs),
  keysort(Pairs, [_-Best|_]).

scheduler:not_done(Done, Node) :-
  \+ get_assoc(Node, Done, _).


scheduler:remainder_from_blocked_from_map([], _Comps, _HeadRuleMap, []).
scheduler:remainder_from_blocked_from_map(BlockedIds, Comps, HeadRuleMap, RemainderOut) :-
  findall(Rule,
          ( member(comp(Id, _Kind, Members), Comps),
            memberchk(Id, BlockedIds),
            member(H, Members),
            get_assoc(H, HeadRuleMap, Rule)
          ),
          RemainderOut0),
  sort(RemainderOut0, RemainderOut).


% -----------------------------------------------------------------------------
%  Proof access (same logic as planner:get_full_rule_from_proof/3)
% -----------------------------------------------------------------------------

scheduler:get_full_rule_from_proof(Literal, ProofAVL, FullRule) :-
  (   ProofKey = rule(Literal),
      get_assoc(ProofKey, ProofAVL, ProofValue)
  ;   ProofKey = assumed(rule(Literal)),
      get_assoc(ProofKey, ProofAVL, ProofValue)
  ;   ProofKey = rule(assumed(Literal)),
      get_assoc(ProofKey, ProofAVL, ProofValue)
  ),
  !,
  prover:canon_rule(FullRule, ProofKey, ProofValue).