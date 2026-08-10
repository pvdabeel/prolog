/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> ABIREBUILD
Sub-slot (`:=`) ABI rebuild propagation (portage-ng#89).

Native equivalent of Gentoo's @preserved-rebuild / haskell-updater pass.
When a transaction changes a provider's sub-slot (e.g. a dev-haskell
library rebuilt with a new GHC ABI hash, dev-lang/ocaml with a new ABI,
or a dev-lang/perl major bump), the already-installed reverse-deps that
bound to it through `:=` / `:slot=` break ghc-pkg check / findlib's
registry / perl's vendor tree and must be rebuilt after the provider.

The rebuilds are *proven*, not patched into the plan: this module is the
domain side of the prover's proof-obligation channel (the same channel
PDEPEND expansion uses — see heuristic:proof_obligation/4). After pass 1
proves a merge-action literal whose sub-slot differs from the installed
copy, abirebuild:obligations/3 contributes one same-version `:update`
goal per installed `:=` consumer. Each rebuild goal then receives a
regular pass-1 proof (re-walking its dependencies, so the changed
provider edge is in its body) and pass 2 orders it after the provider
through the ordinary planning laws — no plan post-processing anywhere.

Masked or keyword-filtered consumers cannot be planned without a
visibility assumption; re-proving them would escalate the whole proof
through the unmask tier (one masked perl-core module poisoned every
perl-touching target — portage-ng#118). They are contributed as
`assumed(...)` literals instead, which pass 1 proves through the
standard domain-assumption rule and the printer reports with the usual
assumption machinery.
*/

:- module(abirebuild, []).

% =============================================================================
%  ABIREBUILD declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Enablement
% -----------------------------------------------------------------------------

%! abirebuild:suspended is semidet.
%
% Dynamic flag. When asserted, sub-slot rebuild obligations are skipped.
% Set by the bulk per-entry test harnesses (pipeline:test_stats) so they
% keep their single-entry semantics and speed; real plan paths (merge /
% build / pretend / writer) leave it unset and get the obligations.

:- dynamic abirebuild:suspended/0.


%! abirebuild:enabled is semidet.
%
% True when rebuild obligations should be produced: config:subslot_rebuild/1
% is not false (defaults to enabled when unset), the mechanism is not
% suspended, and this is not an emptytree run (rebuilding installed
% consumers contradicts emptytree semantics — the VDB is ignored there).

abirebuild:enabled :-
  \+ abirebuild:suspended,
  \+ preference:flag(emptytree),
  ( catch(config:subslot_rebuild(Bool), _, fail) -> Bool == true ; true ).


% -----------------------------------------------------------------------------
%  Provider detection
% -----------------------------------------------------------------------------

%! abirebuild:provider_change(+Repo, +Entry, -C, -N, -Slot, -OldSub, -NewSub) is semidet.
%
% True when merging Repo://Entry changes the sub-slot of C/N in Slot:
% an installed copy exists in the same slot with a different sub-slot.
% Index-backed lookups only — this runs in the prover's obligation-key
% fast path, once per proven merge-action literal.

abirebuild:provider_change(Repo, Entry, C, N, Slot, OldSub, NewSub) :-
  Repo \== pkg,
  cache:ordered_entry(Repo, Entry, C, N, _),
  slotmeta:entry_slot_default(Repo, Entry, Slot),
  sets:entry_subslot(Repo://Entry, NewSub),
  cache:ordered_entry(pkg, OldEntry, C, N, _),
  slotmeta:entry_slot_default(pkg, OldEntry, Slot),
  sets:entry_subslot(pkg://OldEntry, OldSub),
  OldSub \== NewSub,
  !.


% -----------------------------------------------------------------------------
%  Rebuild obligations (domain side of heuristic:proof_obligation/4)
% -----------------------------------------------------------------------------

%! abirebuild:obligations(+AnchorCore, +Model, -ExtraLits) is det.
%
% ExtraLits are the rebuild literals owed after proving the merge-action
% literal AnchorCore (`Repo://Entry:Action`): one same-version `:update`
% goal per installed `:=` consumer of the changed provider, or an
% `assumed(...)` wrap for masked / keyword-filtered consumers. [] when
% the mechanism is disabled, the anchor changes no sub-slot, or every
% consumer is already merged in Model.

abirebuild:obligations(AnchorCore, Model, ExtraLits) :-
  AnchorCore = (Repo://Entry:_Action),
  ( abirebuild:enabled,
    abirebuild:provider_change(Repo, Entry, C, N, Slot, OldSub, NewSub)
  -> abirebuild:consumer_rebuilds(AnchorCore, C, N, Slot, OldSub, NewSub, Model, ExtraLits)
  ;  ExtraLits = []
  ).


%! abirebuild:consumer_rebuilds(+AnchorCore, +C, +N, +Slot, +OldSub, +NewSub, +Model, -ExtraLits) is det.
%
% Collects the installed `:=` consumers of provider C/N in Slot (once,
% deduplicated by VDB entry), drops those the proof already merges, and
% renders the rest as rebuild goals — `assumed(...)`-wrapped when the
% consumer is masked or keyword-filtered (portage-ng#118). Eligible goals
% carry a `rebuild_after(AnchorCore)` marker: rule expansion turns it
% into a `constraint(schedule_after(AnchorCore))` body literal, so pass 2
% places each rebuild in a wave after the provider whenever that closes
% no cycle.

abirebuild:consumer_rebuilds(AnchorCore, C, N, Slot, OldSub, NewSub, Model, ExtraLits) :-
  findall(c(ICEntry, TreeRepo, C/N, OldSub, NewSub),
          abirebuild:consumer_of(C, N, Slot, ICEntry, TreeRepo),
          Raw),
  sort(1, @<, Raw, Unique),
  findall(Lit,
          ( member(Cm, Unique),
            abirebuild:consumer_goal(Cm, Goal),
            \+ abirebuild:model_merges_entry(Model, Goal),
            ( abirebuild:consumer_skip_reason(Goal, Reason)
            -> abirebuild:skipped_assumption(Reason, Goal, Lit)
            ;  abirebuild:ordered_goal(AnchorCore, Goal, Lit)
            )
          ),
          ExtraLits).


%! abirebuild:consumer_of(+C, +N, +Slot, -ICEntry, -TreeRepo) is nondet.
%
% True for an installed package ICEntry (with a matching tree ebuild in
% TreeRepo) that is not C/N itself and whose tree *DEPEND declares a
% sub-slot-bound (`:=` / `:slot=`) dependency on C/N in slot Slot.

abirebuild:consumer_of(C, N, Slot, ICEntry, TreeRepo) :-
  vdb:installed_entry(ICEntry),
  cache:ordered_entry(pkg, ICEntry, ICC, ICN, _),
  \+ ( ICC == C, ICN == N ),
  cache:ordered_entry(TreeRepo, ICEntry, ICC, ICN, _),
  TreeRepo \== pkg,
  once(( member(Key, [rdepend, depend, bdepend, pdepend]),
         cache:entry_metadata(TreeRepo, ICEntry, Key, Dep),
         candidate:dep_contains_pkg_dep_on(Dep, C, N, _Op, _V, SlotReq),
         abirebuild:bound_slotspec(SlotReq, Slot)
       )).


%! abirebuild:bound_slotspec(+SlotReq, +Slot) is semidet.
%
% True when a parsed dependency slot restriction binds the consumer to the
% provider's sub-slot (a rebuild trigger) and is compatible with Slot:
%   `:=`       -> [any_same_slot]            (binds, any slot)
%   `:slot=`   -> [slot(S),equal]            (binds, requires S == Slot)
%   `:s/ss=`   -> [slot(S),subslot(_),equal] (binds, requires S == Slot)

abirebuild:bound_slotspec([any_same_slot], _Slot) :- !.
abirebuild:bound_slotspec(SlotReq, Slot) :-
  memberchk(equal, SlotReq),
  ( member(slot(S), SlotReq)
  -> slotmeta:canon_slot(S, Sc), Sc == Slot
  ;  true
  ).


%! abirebuild:consumer_goal(+Consumer, -Goal) is det.
%
% Builds the rebuild goal: a same-version `:update` of the installed
% consumer that replaces the VDB entry and carries the subslot_change
% reason. The update rule (target.pl) honors the incoming `replaces(...)`
% annotation, and the goal's own dependency proof orders the rebuild
% after the changed provider in pass 2.

abirebuild:consumer_goal(c(Entry, TreeRepo, Provider, OldSub, NewSub),
                         TreeRepo://Entry:update?{[replaces(pkg://Entry),
                                                  rebuild_reason(subslot_change(Provider, OldSub, NewSub))]}).


%! abirebuild:ordered_goal(+AnchorCore, +Goal0, -Goal) is det.
%
% Adds the `rebuild_after(AnchorCore)` ordering marker to an eligible
% rebuild goal's context. featureterm:get_rebuild_after/3 extracts it
% during rule expansion and emits the corresponding
% `constraint(schedule_after(AnchorCore))` condition — a plain anchoring
% preference (this rebuild after the provider). Deliberately NOT the
% after_only/order_after channel: that one doubles as the PDEPEND
% completion group, which would make every consumer of the provider wait
% for every rebuild and serialize the plan.

abirebuild:ordered_goal(AnchorCore, Repo://Entry:Action?{Ctx},
                        Repo://Entry:Action?{[rebuild_after(AnchorCore)|Ctx]}).


%! abirebuild:model_merges_entry(+Model, +Goal) is semidet.
%
% True when the model already contains a merge action for the consumer
% entry — the package is being merged anyway, which covers the rebuild.

abirebuild:model_merges_entry(Model, Repo://Entry:_Action?{_Ctx}) :-
  member(Action, [install, update, upgrade, downgrade, reinstall]),
  get_assoc(Repo://Entry:Action, Model, _),
  !.


%! abirebuild:consumer_skip_reason(+Goal, -Reason) is semidet.
%
% True when a same-version consumer rebuild cannot be planned without a
% visibility assumption. Reason is `masked` or `keyword_filtered`.

abirebuild:consumer_skip_reason(Repo://Entry:_Action?{_Ctx}, masked) :-
  preference:masked(Repo://Entry),
  !.
abirebuild:consumer_skip_reason(Repo://Entry:_Action?{_Ctx}, keyword_filtered) :-
  \+ acceptance:entry_has_accepted_keyword(Repo://Entry),
  !.


%! abirebuild:skipped_assumption(+Reason, +Goal, -Assumed) is det.
%
% Wraps a skipped rebuild goal as `assumed(Goal)` with
% `assumption_reason(Reason)` in the proof context list. Pass 1 proves
% the wrap through the standard domain-assumption rule
% (`rule(assumed(_),[])`), so the printer reports it without the proof
% escalating to the unmask tier.

abirebuild:skipped_assumption(Reason, Repo://Entry:Action?{Ctx0},
                              assumed(Repo://Entry:Action?{Ctx})) :-
  ( is_list(Ctx0) -> Ctx1 = Ctx0 ; Ctx1 = [] ),
  ( memberchk(assumption_reason(Reason), Ctx1)
  -> Ctx = Ctx1
  ;  Ctx = [assumption_reason(Reason)|Ctx1]
  ).
