/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> RANKING
Dependency ordering and ranking heuristics for the portage-ng resolver.

Split out of candidate.pl (issue #64). Contains the dependency ordering
heuristic (order_deps_for_proof/3, dep_priority/2), bracket-USE memo
seeding, equality-USE pin propagation, choice-group ranking
(prioritize_deps/2,3, prioritize_deps_keep_all/3, dep_choice_scores/3,
dep_rank/3), USE_EXPAND profile-match scoring, any_of_group preference
helpers, and provider-reuse candidate reordering.
*/

:- module(ranking, []).

% =============================================================================
%  Dependency ordering heuristic
% =============================================================================

%! ranking:order_deps_for_proof(+Action, +Deps, -Ordered)
%
% Sorts dependency groups for deterministic proof search. Tighter
% constraints (fewer candidates, installed packages, blockers) are
% proved first, reducing the backtracking search space. Uses a
% numeric priority key computed by dep_priority/2.

ranking:order_deps_for_proof(_Action, Deps, Ordered) :-
  maplist(dep_priority_kv, Deps, KVs),
  keysort(KVs, Sorted),
  pairs_values(Sorted, Ordered),
  !.


% -----------------------------------------------------------------------------
%  Bracket USE memo seeding (cross-phase, before dep proof order)
% -----------------------------------------------------------------------------

%! ranking:seed_bwu_memo_from_dep_tree(+Deps) is det
%
% Walk a merged dependency model and union every bracketed USE atom into
% memo:candidate_bwu_/3 before any grouped_dep is proved.  Fixes RDEPEND
% bracket USE (e.g. glib[dbus]) being discovered only after an install-phase
% subtree already scheduled the provider's first :install (issue #7).

ranking:seed_bwu_memo_from_dep_tree(Deps) :-
  is_list(Deps),
  !,
  forall(member(Dep, Deps), ranking:seed_bwu_memo_from_dep(Dep)).
ranking:seed_bwu_memo_from_dep_tree(_).


%! ranking:seed_bwu_memo_from_dep(+Dep) is det
%
% Recurse dependency groups; accumulate bracket USE on package_dependency
% leaves.  Ignores blockers and unknown node shapes.

ranking:seed_bwu_memo_from_dep(grouped_package_dependency(_, _, _, PackageDeps):_) :- !,
  ranking:seed_bwu_memo_from_dep_tree(PackageDeps).
ranking:seed_bwu_memo_from_dep(grouped_package_dependency(_, _, _, PackageDeps)) :- !,
  ranking:seed_bwu_memo_from_dep_tree(PackageDeps).
ranking:seed_bwu_memo_from_dep(grouped_package_dependency(_, _, _, PackageDeps):_Action?{_}) :- !,
  ranking:seed_bwu_memo_from_dep_tree(PackageDeps).
ranking:seed_bwu_memo_from_dep(package_dependency(_, _, C, N, _, _, _, UseReqs)) :-
  UseReqs \== [],
  !,
  ranking:seed_bwu_memo_for_cn(C, N, UseReqs).
ranking:seed_bwu_memo_from_dep(all_of_group(Deps)) :- !,
  ranking:seed_bwu_memo_from_dep_tree(Deps).
ranking:seed_bwu_memo_from_dep(any_of_group(Deps)) :- !,
  ranking:seed_bwu_memo_from_dep_tree(Deps).
ranking:seed_bwu_memo_from_dep(exactly_one_of_group(Deps)) :- !,
  ranking:seed_bwu_memo_from_dep_tree(Deps).
ranking:seed_bwu_memo_from_dep(at_most_one_of_group(Deps)) :- !,
  ranking:seed_bwu_memo_from_dep_tree(Deps).
ranking:seed_bwu_memo_from_dep(use_conditional_group(Pol, Use, RepoEntry, Deps)) :- !,
  ( ranking:seed_use_conditional_inactive(Pol, Use, RepoEntry) ->
      true
  ; ranking:seed_bwu_memo_from_dep_tree(Deps)
  ).
ranking:seed_bwu_memo_from_dep(_).


%! ranking:seed_use_conditional_inactive(+Polarity, +Use, +RepoEntry) is semidet
%
% True only when the USE-conditional guard is *positively determined* to be
% inactive for its owning entry, i.e. seeding its bracketed USE would be
% spurious.  A `flag? ( ... )` group is inactive when the entry has the flag
% off; a `!flag? ( ... )` group is inactive when the flag is on.  Fails (so
% the caller still recurses) when the state cannot be determined (flag not in
% IUSE, minus-wrapped, or RepoEntry unbound), keeping seeding conservative.

ranking:seed_use_conditional_inactive(positive, Use, Repo://Id) :-
  \+ Use =.. [minus, _],
  use:effective_use_for_entry(Repo://Id, Use, negative),
  !.
ranking:seed_use_conditional_inactive(negative, Use, Repo://Id) :-
  \+ Use =.. [minus, _],
  use:effective_use_for_entry(Repo://Id, Use, positive),
  !.


%! ranking:seed_bwu_memo_for_cn(+C, +N, +UseReqs) is det
%
% Build BWU from bracket atoms on one edge and merge into the (C,N) memo.
% Never fails the caller (conflicting edges leave the memo unchanged).
%
% Only context-independent directives (plain `[flag]` / `[-flag]`) are
% seeded.  Equality (`[flag=]`), inverse (`[!flag=]`) and optional
% (`[flag?]` / `[!flag?]`) directives resolve against the *parent's*
% effective USE state, which is not available during seeding (the seed
% runs with an empty parent context).  Resolving them here would make
% use:use_dep_requirement/4 fall back to the bracket's IUSE default --
% disabling a flag the parent actually enables -- and that bogus pin then
% conflicts with the correct value the prover derives later under the real
% parent context (e.g. net-misc/curl[curl_ssl_openssl=] from a consumer
% with openssl on, issue #82).  These directives are accumulated correctly
% at proof time via use:check_bwu_ed_conflict/3.

ranking:seed_bwu_memo_for_cn(C, N, UseReqs) :-
  include(ranking:context_independent_use_directive, UseReqs, SeedReqs),
  ( SeedReqs == [] ->
      true
  ; dependency:process_build_with_use(SeedReqs, [], Ctx, _, _),
    ( use:context_build_with_use_state(Ctx, BWU) ->
        ranking:seed_accumulate_bwu(C, N, BWU)
    ; true
    )
  ).


%! ranking:context_independent_use_directive(+Directive) is semidet
%
% True for bracketed USE directives whose enable/disable outcome does not
% depend on the parent ebuild's effective USE state, i.e. plain `enable`
% and `disable`.  Equality/inverse/optional directives are excluded.

ranking:context_independent_use_directive(use(enable(_), _)).
ranking:context_independent_use_directive(use(disable(_), _)).


%! ranking:seed_accumulate_bwu(+C, +N, +BWU) is det

ranking:seed_accumulate_bwu(_C, _N, use_state([], [])) :- !.
ranking:seed_accumulate_bwu(C, N, BWU) :-
  ( use:accumulate_candidate_bwu(C, N, BWU) -> true ; true ).


% -----------------------------------------------------------------------------
%  Equality-USE pin propagation ([flag=] / [!flag=])
% -----------------------------------------------------------------------------

%! ranking:equality_pins_from_providers(+RepoEntry, -PinState) is det
%
% Scan RepoEntry's own DEPEND/RDEPEND/BDEPEND for unconditional bracketed
% equality USE deps (`provider[flag=]` / `provider[!flag=]`).  For each such
% dep whose provider already has the flag pinned in memo:candidate_bwu_/3,
% derive the flag value this entry must itself build with, so that
% REQUIRED_USE resolution honours the bidirectional `=` constraint.
%
%   [flag=]  : this.flag == provider.flag    (provider on  -> enable here)
%   [!flag=] : this.flag == \+ provider.flag (provider on  -> disable here)
%
% Only top-level / all_of_group deps are scanned (no descent into USE-
% conditional or choice groups): a flag pinned from inside such a group is
% the very flag being resolved, so it is left to normal resolution.  A pin
% that would force a flag both on and off (independent providers disagree)
% is dropped, letting the existing conflict machinery report it.  Returns
% use_state([],[]) when nothing is pinned (the common case).

ranking:equality_pins_from_providers(Repo://Entry, PinState) :-
  findall(F-Mode,
          ( member(Key, [depend, rdepend, bdepend]),
            cache:entry_metadata(Repo, Entry, Key, Term),
            ranking:equality_pin_from_term(Term, F, Mode)
          ),
          Pins0),
  ( Pins0 == [] ->
      use:empty_use_state(PinState)
  ; sort(Pins0, Pins),
    findall(EF, member(EF-enable, Pins), En0), sort(En0, En),
    findall(DF, member(DF-disable, Pins), Dis0), sort(Dis0, Dis),
    ( ranking:pin_flags_conflict(En, Dis) ->
        use:empty_use_state(PinState)
    ; PinState = use_state(En, Dis)
    )
  ).


%! ranking:pin_flags_conflict(+Enable, +Disable) is semidet
%
% True when a flag appears in both the enable and disable pin sets.

ranking:pin_flags_conflict(En, Dis) :-
  member(F, En),
  memberchk(F, Dis),
  !.


%! ranking:equality_pin_from_term(+DepTerm, -Flag, -Mode) is nondet
%
% Yield Flag-Mode (Mode = enable|disable) for each unconditional equality
% bracket USE dep in DepTerm whose provider is pinned in the BWU memo.
% Descends only all_of_group; choice and USE-conditional groups are skipped.

ranking:equality_pin_from_term(package_dependency(_P, _S, PC, PN, _O, _V, _Slot, UseDeps), F, Mode) :-
  member(UseDep, UseDeps),
  ranking:equality_pin_from_usedep(PC, PN, UseDep, F, Mode).
ranking:equality_pin_from_term(all_of_group(Deps), F, Mode) :-
  member(D, Deps),
  ranking:equality_pin_from_term(D, F, Mode).


%! ranking:equality_pin_from_usedep(+PC, +PN, +UseDep, -Flag, -Mode) is nondet
%
% Map one `use(equal(F),_)` / `use(inverse(F),_)` directive on provider
% (PC,PN) to the flag value this entry must build with, given the provider's
% current BWU pin.

ranking:equality_pin_from_usedep(PC, PN, use(equal(F), _), F, Mode) :-
  ranking:provider_pin_mode(PC, PN, F, Mode).
ranking:equality_pin_from_usedep(PC, PN, use(inverse(F), _), F, Mode) :-
  ranking:provider_pin_mode(PC, PN, F, ProviderMode),
  ranking:invert_pin_mode(ProviderMode, Mode).


%! ranking:provider_pin_mode(+PC, +PN, +Flag, -Mode) is semidet
%
% Mode is enable/disable iff provider (PC,PN) has Flag pinned on/off in the
% BWU memo.  Fails when the flag is unpinned (equality resolves normally).

ranking:provider_pin_mode(PC, PN, F, enable) :-
  memo:candidate_bwu_(PC, PN, use_state(En, _Dis)),
  memberchk(F, En),
  !.
ranking:provider_pin_mode(PC, PN, F, disable) :-
  memo:candidate_bwu_(PC, PN, use_state(_En, Dis)),
  memberchk(F, Dis),
  !.


%! ranking:invert_pin_mode(+Mode, -Inverted) is det

ranking:invert_pin_mode(enable, disable).
ranking:invert_pin_mode(disable, enable).


%! ranking:apply_equality_pins(+RepoEntry, +BWU0, -BWU) is det
%
% Union the equality-USE pins derived from RepoEntry's providers into the
% candidate's own build_with_use state before REQUIRED_USE resolution.  When
% a pin contradicts the existing state, BWU0 is kept unchanged and the clash
% is left for check_bwu_cross_dep / REQUIRED_USE verification to report.

ranking:apply_equality_pins(Repo://Entry, BWU0, BWU) :-
  ranking:equality_pins_from_providers(Repo://Entry, PinState),
  ( PinState == use_state([], []) ->
      BWU = BWU0
  ; BWU0 == use_state([], []) ->
      BWU = PinState
  ; feature_unification:val_hook(BWU0, PinState, BWU) ->
      true
  ; BWU = BWU0
  ).


ranking:dep_priority_kv(Dep, K-Dep) :-
  dep_priority(Dep, K),
  !.

%! ranking:dep_priority(+DepLiteral, -Key)
%
% Computes a priority key for a dependency literal. Lower keys are
% proved first. Key is `key(BaseK, TightUpper, C, N)` where BaseK
% accounts for upper-bound tightness, tilde constraints, wildcard
% constraints, and slot specificity. Tilde deps (BaseK=4) are proved
% before wildcards (8) and unconstrained deps (999) so that
% selected_cn locks the version before a sibling picks a conflicting
% one.

ranking:dep_priority(grouped_package_dependency(_T,C,N,PackageDeps):Action?{_Context}, K) :-
  !,
  ( slotmeta:merge_slot_restriction(Action, C, N, PackageDeps, SlotReq) ->
      ( dep_tightest_upper_bound(C, N, PackageDeps, TightUpper) ->
          UpperK0 = 1
      ; TightUpper = none,
        ( cnselect:dep_has_tilde_constraint(C, N, PackageDeps)          -> UpperK0 = 4
        ; cnselect:dep_has_equal_wildcard_constraint(C, N, PackageDeps) -> UpperK0 = 8
        ; UpperK0 = 999
        )
      ),
      slotreq_priority(SlotReq, SlotK0),
      BaseK is min(UpperK0, SlotK0),
      K = key(BaseK, TightUpper, C, N)
  ; K = key(50, none, C, N)
  ).
ranking:dep_priority(_Other, key(90, none, zz, zz)) :- !.

ranking:slotreq_priority([slot(_),subslot(_)|_], 0) :- !.
ranking:slotreq_priority([slot(_)|_],             5) :- !.
ranking:slotreq_priority([any_same_slot],        10) :- !.
ranking:slotreq_priority([any_different_slot],   15) :- !.
ranking:slotreq_priority([],                     20) :- !.
ranking:slotreq_priority(_Other,                 30) :- !.

ranking:dep_tightest_upper_bound(C, N, PackageDeps, Tightest) :-
  member(package_dependency(_, no, C, N, Op0, _, _, _), PackageDeps),
  ( Op0 == smaller ; Op0 == smallerorequal ),
  !,
  findall(V,
          ( member(package_dependency(_Phase, no, C, N, Op, V, _S, _U), PackageDeps),
            ( Op == smaller ; Op == smallerorequal )
          ),
          [First|Rest]),
  foldl(min_version_bound_, Rest, First, Tightest).

ranking:min_version_bound_(V, Best0, Best) :-
  ( eapi:version_compare(<, V, Best0) ->
      Best = V
  ; Best = Best0
  ),
  !.


%! ranking:dep_extract_cn_packagedeps(+DepLiteral, -C, -N, -PackageDeps) is semidet.
%
% Extracts the category, name, and package deps from a grouped
% dependency literal. Handles both Context and bare forms.

ranking:dep_extract_cn_packagedeps(grouped_package_dependency(_T, C, N, PackageDeps):_Action?{_}, C, N, PackageDeps) :- !.
ranking:dep_extract_cn_packagedeps(grouped_package_dependency(_T, C, N, PackageDeps):_Action, C, N, PackageDeps) :- !.


% =============================================================================
%  Dependency ranking / prioritization
% =============================================================================

%! ranking:prioritize_deps(+Deps, -SortedDeps)
%
% Sorts dependency groups by priority class and sub-ranking (slot
% specificity, blocker status). Used at the rule level to present
% candidates in deterministic order.

ranking:prioritize_deps(Deps, SortedDeps) :-
  prioritize_deps(Deps, [], SortedDeps).

%! ranking:prioritize_deps(+Deps, +Context, -SortedDeps)
%
% Sorts dependency groups by rank using Context for installed/use checks.

ranking:prioritize_deps(Deps, Context, SortedDeps) :-
  predsort(ranking:compare_dep_rank(Context), Deps, SortedDeps).

%! ranking:prioritize_deps_keep_all(+Deps, +Context, -SortedDeps)
%
% Like prioritize_deps/3 but uses a multi-key ranking approximating
% emerge dep_zapdeps choice_bins. Keys (higher preferred; negated for
% keysort), then original index I for stable left-to-right ties:
%
%   LicOk, UseSat, UseUnmasked, Rank, SnapAll, SlotScore,
%   NoDowngrade, InstScore, Overlap, VerScore, UEScore, I
%
% See Documentation/Handbook/11-doc-rules.md ("Any-of (||) arm selection").
% VerScore covers Haskell-style same-CN ranges (portage-ng#112).

ranking:prioritize_deps_keep_all(Deps, Context, SortedDeps) :-
  setup_call_cleanup(
    ( empty_assoc(Empty),
      nb_setval(ranking_choice_cache, Empty)
    ),
    ranking:prioritize_deps_keep_all_body(Deps, Context, SortedDeps),
    catch(nb_delete(ranking_choice_cache), _, true)
  ).


ranking:prioritize_deps_keep_all_body(Deps, Context, SortedDeps) :-
  findall(NegLicOk-NegUseSat-NegUseUnmasked-NegRank-NegSnapAll-NegSlotScore-NegNoDowngrade-NegInstScore-NegOverlap-NegVerScore-NegUEScore-I-Dep,
          ( nth1(I, Deps, Dep),
            dep_rank(Context, Dep, Rank),
            dep_overlap_group_count(Context, Dep, OvRaw),
            ( OvRaw > 1 -> Overlap = OvRaw ; Overlap = 0 ),
            ( acceptance:dep_license_ok(Dep) -> LicOk = 1 ; LicOk = 0 ),
            dep_use_expand_profile_score(Dep, UEScore),
            ranking:dep_choice_scores(Context, Dep,
              scores(UseSat, UseUnmasked, SnapAll, SlotScore,
                     NoDowngrade, InstScore, VerScore)),
            NegLicOk is -LicOk,
            NegUseSat is -UseSat,
            NegUseUnmasked is -UseUnmasked,
            NegRank is -Rank,
            NegSnapAll is -SnapAll,
            NegSlotScore is -SlotScore,
            NegNoDowngrade is -NoDowngrade,
            NegInstScore is -InstScore,
            NegOverlap is -Overlap,
            NegVerScore is -VerScore,
            NegUEScore is -UEScore
          ),
          Ranked),
  keysort(Ranked, RankedSorted),
  findall(Dep, member(_-_-_-_-_-_-_-_-_-_-_-_-Dep, RankedSorted), SortedDeps0),
  ranking:boost_variant_preferred(SortedDeps0, SortedDeps),
  !.


%! ranking:boost_variant_preferred(+Deps, -Reordered) is det.
%
% When a thread-local variant:branch_prefer/1 override is active,
% moves matching deps to the front so the any_of_group cut selects them.

ranking:boost_variant_preferred(Deps, Reordered) :-
  ( variant:branch_prefer(Pref),
    partition(ranking:dep_matches_prefer(Pref), Deps, Front, Rest),
    Front \== []
  -> append(Front, Rest, Reordered)
  ;  Reordered = Deps
  ).


% -----------------------------------------------------------------------------
%  Choice-group arm scores (emerge dep_zapdeps alignment)
% -----------------------------------------------------------------------------

%! ranking:dep_choice_scores(+Context, +Dep, -Scores) is det.
%
% Single-pass arm analysis for prioritize_deps_keep_all/3. Scores is
% scores(UseSat, UseUnmasked, SnapAll, SlotScore, NoDowngrade, InstScore,
% VerScore). Uses a per-call nb_setval cache (ranking_choice_cache) for
% installed/snap lookups shared across arms.

ranking:dep_choice_scores(Context, Dep,
    scores(UseSat, UseUnmasked, SnapAll, SlotScore, NoDowngrade, InstScore, VerScore)) :-
  ranking:dep_arm_package_atoms(Dep, Atoms),
  ranking:dep_best_admitted(Dep, BestRE, BestVer, VerScore),
  ranking:dep_use_sat_scores(Context, Atoms, BestRE, UseSat, UseUnmasked),
  ranking:dep_snap_all_score(Atoms, SnapAll),
  ranking:dep_slot_score(Atoms, SlotScore),
  ranking:dep_no_downgrade_score(Dep, BestVer, NoDowngrade),
  ranking:dep_inst_score(Atoms, InstScore),
  !.


%! ranking:dep_arm_package_atoms(+Dep, -Atoms) is det.
%
% Flatten package_dependency leaves from an || arm (all_of_group /
% use_conditional_group recursion).

ranking:dep_arm_package_atoms(package_dependency(Ph, St, C, N, O, V, S, U),
                              [package_dependency(Ph, St, C, N, O, V, S, U)]) :- !.
ranking:dep_arm_package_atoms(all_of_group(Deps), Atoms) :-
  is_list(Deps),
  !,
  findall(A,
          ( member(D, Deps),
            ranking:dep_arm_package_atoms(D, As),
            member(A, As)
          ),
          Atoms).
ranking:dep_arm_package_atoms(use_conditional_group(_, _, _, Deps), Atoms) :-
  is_list(Deps),
  !,
  ranking:dep_arm_package_atoms(all_of_group(Deps), Atoms).
ranking:dep_arm_package_atoms(_, []).


%! ranking:dep_blocker_strength(+Strength) is semidet.

ranking:dep_blocker_strength(weak).
ranking:dep_blocker_strength(strong).


%! ranking:dep_use_sat_scores(+Context, +Atoms, +BestRE, -UseSat, -UseUnmasked) is det.
%
% UseSat=1 when bracket USE needs no change on BestRE (or no USE deps).
% UseUnmasked=1 when required flips respect profile use.mask/force.
% Neutral (1,1) when BestRE is unavailable so incomplete arms are not
% demoted spuriously.

ranking:dep_use_sat_scores(Context, Atoms, BestRE, UseSat, UseUnmasked) :-
  findall(U,
          ( member(package_dependency(_, Str, _, _, _, _, _, Us), Atoms),
            \+ ranking:dep_blocker_strength(Str),
            is_list(Us),
            member(U, Us)
          ),
          UseDeps),
  ( UseDeps == []
  -> UseSat = 1,
     UseUnmasked = 1
  ; BestRE == none
  -> UseSat = 1,
     UseUnmasked = 1
  ; catch(use:directives_to_bwu(Context, UseDeps, BWU), _,
          BWU = use_state([], [])),
    ( BWU = use_state([], [])
    -> UseSat = 1,
       UseUnmasked = 1
    ; use:build_with_use_changes(BWU, BestRE, Changes),
      ( Changes == [] -> UseSat = 1 ; UseSat = 0 ),
      ( catch(use:bwu_respects_profile_hard(BestRE, BWU), _, fail)
      -> UseUnmasked = 1
      ; UseUnmasked = 0
      )
    )
  ).


%! ranking:dep_snap_all_score(+Atoms, -SnapAll) is det.
%
% 1 iff every non-blocker, non-virtual package atom's (C,N) is present
% in the selected_cn snapshot (emerge all_in_graph stand-in).

ranking:dep_snap_all_score([], 0) :- !.
ranking:dep_snap_all_score(Atoms, SnapAll) :-
  ( ranking:dep_snap_all_ok(Atoms)
  -> SnapAll = 1
  ; SnapAll = 0
  ).


ranking:dep_snap_all_ok(Atoms) :-
  Atoms \== [],
  forall(member(package_dependency(_, Str, C, N, _, _, _, _), Atoms),
         ( ranking:dep_blocker_strength(Str) -> true
         ; C == virtual -> true
         ; cnselect:snapshot_selected_cn_candidates(C, N, _)
         )).


%! ranking:dep_slot_score(+Atoms, -SlotScore) is det.
%
% Max numeric explicit slot among arm atoms (llvm-style || slot prefer).

ranking:dep_slot_score(Atoms, SlotScore) :-
  findall(S,
          ( member(package_dependency(_, Str, _, _, _, _, SlotReq, _), Atoms),
            \+ ranking:dep_blocker_strength(Str),
            ranking:slot_req_numeric_score(SlotReq, S),
            S > 0
          ),
          Scores),
  ( Scores == [] -> SlotScore = 0 ; max_list(Scores, SlotScore) ).


%! ranking:slot_req_numeric_score(+SlotReq, -Score) is det.

ranking:slot_req_numeric_score([slot(S)|_], Score) :-
  ranking:slot_atom_numeric(S, Score),
  !.
ranking:slot_req_numeric_score(_, 0).


%! ranking:slot_atom_numeric(+Slot, -N) is semidet.

ranking:slot_atom_numeric(S, N) :-
  integer(S),
  !,
  N = S.
ranking:slot_atom_numeric(S, N) :-
  atom(S),
  atom_number(S, N),
  !.
ranking:slot_atom_numeric(S, N) :-
  atom(S),
  atom_codes(S, Codes),
  ranking:take_digits(Codes, Digits, _),
  Digits \== [],
  number_codes(N, Digits).


%! ranking:dep_no_downgrade_score(+Dep, +BestVer, -NoDowngrade) is det.
%
% 0 when the arm's newest admitted version is strictly below the highest
% installed or snap-selected version for that CN (emerge downgrade demotion).

ranking:dep_no_downgrade_score(Dep, BestVer, NoDowngrade) :-
  BestVer \== version_none,
  compound(BestVer),
  ranking:dep_cn_version_domain(Dep, C, N, _),
  C \== virtual,
  ranking:reference_highest_version(C, N, RefVer),
  RefVer \== version_none,
  !,
  ( BestVer @< RefVer -> NoDowngrade = 0 ; NoDowngrade = 1 ).
ranking:dep_no_downgrade_score(_, _, 1).


%! ranking:reference_highest_version(+C, +N, -Ver) is det.
%
% Highest of installed VDB version and snap-selected versions for (C,N).
% Cached per prioritize_deps_keep_all/3 call. version_none if neither.

ranking:reference_highest_version(C, N, Ver) :-
  ( ranking:choice_cache_get(refver(C-N), Ver) -> true
  ; ranking:lookup_reference_highest_version(C, N, Ver),
    ranking:choice_cache_put(refver(C-N), Ver)
  ).


ranking:lookup_reference_highest_version(C, N, Ver) :-
  findall(V, ranking:reference_version_candidate(C, N, V), Vers),
  ( Vers == []
  -> Ver = version_none
  ; sort(Vers, Sorted),
    last(Sorted, Ver)
  ).


ranking:reference_version_candidate(C, N, Ver) :-
  catch(( knowledgebase:vdb_repository(VdbRepo),
          query:search([name(N), category(C), installed(true)], VdbRepo://Id),
          query:search(version(Ver), VdbRepo://Id)
        ), _, fail).
ranking:reference_version_candidate(C, N, Ver) :-
  cnselect:snapshot_selected_cn_candidates(C, N, Cands),
  member(Repo://Entry, Cands),
  catch(query:search(version(Ver), Repo://Entry), _, fail).


%! ranking:dep_inst_score(+Atoms, -InstScore) is det.
%
% Count of non-blocker, non-virtual atoms whose CN is installed (partial
% other_installed fuzzy bin).

ranking:dep_inst_score(Atoms, InstScore) :-
  findall(1,
          ( member(package_dependency(_, Str, C, N, _, _, _, _), Atoms),
            \+ ranking:dep_blocker_strength(Str),
            C \== virtual,
            ranking:cn_is_installed(C, N)
          ),
          Hits),
  length(Hits, InstScore).


%! ranking:cn_is_installed(+C, +N) is semidet.

ranking:cn_is_installed(C, N) :-
  ( ranking:choice_cache_get(inst(C-N), Hit) -> Hit == true
  ; ( catch(( knowledgebase:vdb_repository(VdbRepo),
              query:search([name(N), category(C), installed(true)], VdbRepo://_)
            ), _, fail)
    -> ranking:choice_cache_put(inst(C-N), true),
       true
    ; ranking:choice_cache_put(inst(C-N), false),
      fail
    )
  ).


%! ranking:choice_cache_get(+Key, -Value) is semidet.
%! ranking:choice_cache_put(+Key, +Value) is det.

ranking:choice_cache_get(Key, Value) :-
  nb_current(ranking_choice_cache, AVL),
  get_assoc(Key, AVL, Value),
  !.

ranking:choice_cache_put(Key, Value) :-
  ( nb_current(ranking_choice_cache, AVL0) -> true ; empty_assoc(AVL0) ),
  put_assoc(Key, AVL0, Value, AVL1),
  nb_setval(ranking_choice_cache, AVL1).


%! ranking:dep_best_admitted(+Dep, -BestRE, -BestVer, -VerScore) is det.
%
% Newest tree version admitted by Dep's same-CN version domain, plus its
% Repo://Entry (or none / version_none / 0 when not applicable).

ranking:dep_best_admitted(Dep, BestRE, BestVer, VerScore) :-
  ranking:dep_cn_version_domain(Dep, C, N, Domain),
  \+ version_domain:domain_inconsistent(Domain),
  !,
  findall(Ver-(Repo://Entry),
          ( cache:ordered_entry(Repo, Entry, C, N, Ver),
            version_domain:domain_allows_candidate(Domain, Repo://Entry)
          ),
          Pairs),
  ( Pairs == []
  -> BestRE = none,
     BestVer = version_none,
     VerScore = 0
  ; sort(Pairs, Sorted),
    last(Sorted, BestVer-BestRE),
    ranking:version_to_sort_score(BestVer, VerScore)
  ).
ranking:dep_best_admitted(_Dep, none, version_none, 0).


%! ranking:dep_max_candidate_version_score(+Dep, -Score) is det.
%
% Score for ||-branch ordering: newest tree version admitted by Dep's
% version constraints on a single (C,N), packed as a comparable integer.
% Returns 0 when Dep is not a pure version-bounded package branch.

ranking:dep_max_candidate_version_score(Dep, Score) :-
  ranking:dep_best_admitted(Dep, _, _, Score).


%! ranking:dep_cn_version_domain(+Dep, -C, -N, -Domain) is semidet.
%
% Succeeds when Dep is a package_dependency or an all_of_group whose
% members are all package_dependency terms on the same (C,N).

ranking:dep_cn_version_domain(package_dependency(Ph, St, C, N, O, V, S, U), C, N, Domain) :-
  !,
  version_domain:domain_from_packagedeps(Ph, C, N,
    [package_dependency(Ph, St, C, N, O, V, S, U)], Domain).
ranking:dep_cn_version_domain(all_of_group(Deps), C, N, Domain) :-
  Deps \== [],
  Deps = [package_dependency(_, _, C, N, _, _, _, _)|_],
  forall(member(D, Deps),
         D = package_dependency(_, _, C, N, _, _, _, _)),
  !,
  version_domain:domain_from_packagedeps(run, C, N, Deps, Domain).


%! ranking:version_to_sort_score(+Version, -Score) is det.
%
% Packs the leading numeric components of a version/7 into a single
% integer so keysort can prefer newer versions (via NegVerScore).

ranking:version_to_sort_score(version(Nums, _, _, _, _, Rev, _), Score) :-
  !,
  ranking:version_nums_pad4(Nums, A, B, C, D),
  ( integer(Rev) -> R = Rev ; R = 0 ),
  Score is (((A * 1000 + B) * 1000 + C) * 1000 + D) * 1000 + R.
ranking:version_to_sort_score(_, 0).


ranking:version_nums_pad4([A,B,C,D|_], A, B, C, D) :- !.
ranking:version_nums_pad4([A,B,C], A, B, C, 0) :- !.
ranking:version_nums_pad4([A,B], A, B, 0, 0) :- !.
ranking:version_nums_pad4([A], A, 0, 0, 0) :- !.
ranking:version_nums_pad4([], 0, 0, 0, 0).


%! ranking:dep_matches_prefer(+Preferred, +Dep) is semidet.

ranking:dep_matches_prefer(Pref, Dep) :-
  Pref = package_dependency(_, _, PC, PN, _, _, _, _),
  Dep  = package_dependency(_, _, PC, PN, _, _, _, _).

%! ranking:dep_snapshot_selected(+Dep) is semidet.
%
% True when every non-virtual, non-blocker package atom in Dep is
% already in the selected_cn snapshot (SnapAll=1).

ranking:dep_snapshot_selected(Dep) :-
  ranking:dep_arm_package_atoms(Dep, Atoms),
  ranking:dep_snap_all_ok(Atoms),
  !.

ranking:dep_overlap_group_count(Context, package_dependency(_,_,C,N,_,_,_,_), Count) :-
  memberchk(self(Repo://Ebuild), Context),
  !,
  aggregate_all(count, (
    member(DepKey, [rdepend, depend, bdepend, pdepend, cdepend, idepend]),
    cache:entry_metadata(Repo, Ebuild, DepKey, DepEntry),
    dep_entry_active_any_of_with_cn(DepEntry, Repo://Ebuild, C, N)
  ), Count).
ranking:dep_overlap_group_count(_, _, 0).

ranking:dep_entry_active_any_of_with_cn(any_of_group(Deps), _, C, N) :-
  member(package_dependency(_, _, C, N, _, _, _, _), Deps), !.
ranking:dep_entry_active_any_of_with_cn(use_conditional_group(Pol, Use, RepoEntry, Deps), _, C, N) :-
  candidate:rdepend_self_use_conditional_active(Pol, Use, RepoEntry),
  member(D, Deps),
  dep_entry_active_any_of_with_cn(D, RepoEntry, C, N), !.
ranking:dep_entry_active_any_of_with_cn(all_of_group(Deps), RepoEntry, C, N) :-
  member(D, Deps),
  dep_entry_active_any_of_with_cn(D, RepoEntry, C, N), !.

ranking:compare_dep_rank(Context, Delta, A, B) :-
  dep_rank(Context, A, Ra),
  dep_rank(Context, B, Rb),
  compare(C, Rb, Ra),
  ( C == (<) -> Delta = (<)
  ; C == (>) -> Delta = (>)
  ; Delta = (=)
  ).

%! ranking:dep_rank(+Context, +Dep, -Rank)
%
% Computes a numeric rank for a dependency term. Higher rank = preferred.

ranking:dep_rank(Context, Dep, Rank) :-
  Dep \= package_dependency(_,_,_,_,_,_,_,_),
  ( is_preferred_dep(Context, Dep) -> Pref = 1 ; Pref = 0 ),
  dep_intrinsic_rank(Dep, Base),
  Rank is Pref*1000000000 + Base,
  !.

ranking:dep_rank(Context, package_dependency(Phase,Strength,C,N,O,V,S,U), Rank) :-
  ( self_cn(Context, C, N) -> Base0 = -100000000 ; Base0 = 0 ),
  installed_version_mismatch_penalty(package_dependency(Phase,Strength,C,N,O,V,S,U), BaseInst),
  ( is_preferred_dep(Context, package_dependency(Phase,Strength,C,N,O,V,S,U)) -> Pref = 1 ; Pref = 0 ),
  dep_intrinsic_rank(package_dependency(Phase,Strength,C,N,O,V,S,U), Base1),
  dep_favour_avoid_bonus(C, N, FavAvoid),
  Rank is Pref*1000000000 + Base0 + BaseInst + Base1 + FavAvoid,
  !.

ranking:self_cn(Context, C, N) :-
  memberchk(self(Repo://Id), Context),
  query:search([category(C),name(N)], Repo://Id),
  !.

ranking:dep_intrinsic_rank(required(Use), Rank) :-
  use_rank(Use, Rank),
  !.
ranking:dep_intrinsic_rank(required(minus(Use)), Rank) :-
  use_rank(Use, Rank),
  !.
ranking:dep_intrinsic_rank(package_dependency(_Phase,_Strength,_C,N,_O,_V,_S,_U), Rank) :-
  ( atom_concat(_, '-bootstrap', N) -> Rank = 50000
  ; Rank = 0
  ),
  !.
ranking:dep_intrinsic_rank(_, 0).


%! ranking:dep_favour_avoid_bonus(+Category, +Name, -Bonus) is det.
%
% Returns a large positive bonus for --favour'd packages and a large
% negative penalty for --avoid'd packages in || dep resolution.

ranking:dep_favour_avoid_bonus(C, N, Bonus) :-
  atomic_list_concat([C, '/', N], CN),
  ( config:dep_favour(CN) -> FavBonus = 500000000
  ; config:dep_favour(N)  -> FavBonus = 500000000
  ; FavBonus = 0
  ),
  ( config:dep_avoid(CN) -> AvoidPen = -500000000
  ; config:dep_avoid(N)  -> AvoidPen = -500000000
  ; AvoidPen = 0
  ),
  Bonus is FavBonus + AvoidPen.

ranking:use_rank(Use, Rank) :-
  atom(Use),
  ranking:use_expand_target_rank(Use, Rank),
  !.
ranking:use_rank(_, 0).


%! ranking:use_expand_target_rank(+Use, -Rank) is semidet.
%
% Derives a positive rank from the trailing version/slot digits of a
% USE_EXPAND target/slot flag, so that when a choice group offers several
% single-target alternatives and the profile has NOT forced one, the
% newest target/slot is preferred -- mirroring emerge's "highest available
% slot" behaviour.
%
% Generic across every USE_EXPAND family registered in eapi:use_expand/1
% (llvm_slot, lua_single_target, python_single_target, ruby_targets, ...):
% the family prefix is stripped and the remaining digit groups are packed
% into the rank, e.g. llvm_slot_20 -> 20,
% lua_single_target_lua5-4 -> key([5,4]),
% python_single_target_python3_13 -> key([3,13]). Flags with no numeric
% component (e.g. lua_single_target_luajit) fail and fall through to rank 0
% via use_rank/2. This was previously hardcoded for llvm_slot and lua5
% only -- the two families whose profile defaults were dropped -- which
% baked ecosystem-specific literals into the domain rules. Profile-selected
% targets are handled separately by is_preferred_dep/2 (Pref*1e9), which
% always dominates this tiebreaker.

ranking:use_expand_target_rank(Use, Rank) :-
  preference:use_expand_env(_EnvVar, Prefix),
  atom_concat(Prefix, '_', PrefixU),
  atom_concat(PrefixU, Value, Use),
  Value \== '',
  ranking:use_expand_version_key(Value, Rank),
  !.


%! ranking:use_expand_version_key(+Value, -Key) is semidet.
%
% Packs the maximal decimal-digit runs of Value into a single comparable
% integer (newer = larger; each component occupies a fixed field). Fails
% when Value carries no digits.

ranking:use_expand_version_key(Value, Key) :-
  atom_codes(Value, Codes),
  ranking:digit_groups(Codes, Groups),
  Groups \== [],
  ranking:pack_version_key(Groups, 0, Key).


%! ranking:digit_groups(+Codes, -Groups) is det.
%
% Extracts the maximal runs of decimal digits in Codes as integers, in
% left-to-right order. e.g. "python3_13" -> [3,13], "lua5-4" -> [5,4],
% "20" -> [20], "luajit" -> [].

ranking:digit_groups([], []) :- !.
ranking:digit_groups(Codes, [N|Rest]) :-
  ranking:take_digits(Codes, DigitCodes, Codes1),
  DigitCodes \== [],
  !,
  number_codes(N, DigitCodes),
  ranking:digit_groups(Codes1, Rest).
ranking:digit_groups([_|Codes], Rest) :-
  ranking:digit_groups(Codes, Rest).


%! ranking:take_digits(+Codes, -DigitCodes, -Rest) is det.

ranking:take_digits([C|Cs], [C|Ds], Rest) :-
  code_type(C, digit),
  !,
  ranking:take_digits(Cs, Ds, Rest).
ranking:take_digits(Cs, [], Cs).


%! ranking:pack_version_key(+Groups, +Acc, -Key) is det.

ranking:pack_version_key([], Key, Key).
ranking:pack_version_key([G|Gs], Acc0, Key) :-
  Acc1 is Acc0 * 10000 + G,
  ranking:pack_version_key(Gs, Acc1, Key).

%! ranking:is_preferred_dep(+Context, +Dep)
%
% True if a dependency is "preferred" based on USE flags, installed
% status, or all_of_group member satisfaction.

ranking:is_preferred_dep(_Context, use_conditional_group(positive, Use, RepoEntry, _Deps)) :-
  \+ Use =.. [minus,_],
  RepoEntry = _Repo://_Id,
  use:effective_use_for_entry(RepoEntry, Use, positive),
  !.
ranking:is_preferred_dep(_Context, use_conditional_group(negative, Use, RepoEntry, _Deps)) :-
  \+ Use =.. [minus,_],
  RepoEntry = _Repo://_Id,
  use:effective_use_for_entry(RepoEntry, Use, negative),
  !.

ranking:is_preferred_dep(Context, required(Use)) :-
  Use \= minus(_),
  ( preference:global_use(Use)
  ; use:effective_use_in_context(Context, Use, positive)
  ),
  !.
ranking:is_preferred_dep(Context, required(minus(Use))) :-
  ( preference:global_use(minus(Use))
  ; use:effective_use_in_context(Context, Use, negative)
  ),
  !.

ranking:is_preferred_dep(Context, all_of_group(Deps)) :-
  Deps \= [],
  forall(member(D, Deps), group_member_preferred(Context, D)),
  !.

ranking:is_preferred_dep(_Context, package_dependency(_Phase,_Strength,C,N,O,V,_S,_U)) :-
  knowledgebase:vdb_repository(VdbRepo),
  query:search([name(N),category(C),installed(true)], VdbRepo://Installed),
  ( O == none ; query:search(select(version, O, V), VdbRepo://Installed) ),
  !.


% =============================================================================
%  USE_EXPAND profile-match scoring for any_of_group ranking
% =============================================================================

%! ranking:dep_use_expand_profile_score(+Dep, -Score) is det.
%
% Scores how well a dep's USE_EXPAND USE-deps (e.g.
% [python_targets_python3_13(-)]) align with the active profile's
% USE_EXPAND selection (e.g. PYTHON_TARGETS="python3_13").
%
% Used as a tiebreaker in any_of_group / choice_group ranking so that
% portage-ng mirrors emerge's behaviour: prefer the alternative whose
% USE_EXPAND requirements are already satisfied by the profile defaults.
% Without this signal, branches with equal LicOk/Rank/Overlap/Snap fall
% back to left-to-right order and often pick a non-default Python (or
% Ruby/PHP/etc.) target slot, forcing rebuilds of build-helpers like
% python-gnupg, setuptools, gpep517 with the wrong PYTHON_TARGETS.
%
% Each matching USE_EXPAND USE-dep contributes +1; each mismatching
% one contributes -1; non-USE_EXPAND USE-deps contribute 0. The score
% recurses into all_of_group and use_conditional_group members; nested
% any_of_group takes the maximum branch score.

ranking:dep_use_expand_profile_score(Dep, Score) :-
  catch(ranking:use_expand_score(Dep, Score), _, Score = 0).


ranking:use_expand_score(package_dependency(_,_,_,_,_,_,_,U), Score) :-
  is_list(U),
  !,
  ranking:use_expand_score_list(U, 0, Score).
ranking:use_expand_score(all_of_group(Deps), Score) :-
  is_list(Deps),
  !,
  foldl(ranking:use_expand_score_acc, Deps, 0, Score).
ranking:use_expand_score(any_of_group(Deps), Score) :-
  is_list(Deps), Deps \== [],
  !,
  findall(S, ( member(D, Deps), ranking:use_expand_score(D, S) ), Scores),
  ( Scores == [] -> Score = 0 ; max_list(Scores, Score) ).
ranking:use_expand_score(use_conditional_group(_,_,_,Deps), Score) :-
  is_list(Deps),
  !,
  foldl(ranking:use_expand_score_acc, Deps, 0, Score).
ranking:use_expand_score(_, 0).


ranking:use_expand_score_acc(D, Acc0, Acc) :-
  ranking:use_expand_score(D, S),
  Acc is Acc0 + S.


ranking:use_expand_score_list([], Acc, Acc).
ranking:use_expand_score_list([U|Rest], Acc0, Acc) :-
  ( ranking:use_dep_use_expand_signal(U, Sig)
  -> Acc1 is Acc0 + Sig
  ;  Acc1 = Acc0
  ),
  ranking:use_expand_score_list(Rest, Acc1, Acc).


%! ranking:use_dep_use_expand_signal(+UseDep, -Signal) is semidet.
%
% Returns +1 when the USE-dep is satisfied by the profile's USE_EXPAND
% selection, -1 when it conflicts. Fails for non-USE_EXPAND or
% inconclusive directives (treated as 0 by use_expand_score_list).

ranking:use_dep_use_expand_signal(use(enable(Flag), _), Sig) :-
  ranking:flag_is_use_expand(Flag),
  ( preference:global_use(Flag) -> Sig = 1 ; Sig = -1 ).
ranking:use_dep_use_expand_signal(use(optenable(Flag), _), Sig) :-
  ranking:flag_is_use_expand(Flag),
  ( preference:global_use(Flag) -> Sig = 1 ; Sig = -1 ).
ranking:use_dep_use_expand_signal(use(disable(Flag), _), Sig) :-
  ranking:flag_is_use_expand(Flag),
  ( preference:global_use(Flag) -> Sig = -1 ; Sig = 1 ).
ranking:use_dep_use_expand_signal(use(optdisable(Flag), _), Sig) :-
  ranking:flag_is_use_expand(Flag),
  ( preference:global_use(Flag) -> Sig = -1 ; Sig = 1 ).


%! ranking:flag_is_use_expand(+Flag) is semidet.
%
% True if Flag (e.g. python_targets_python3_13) starts with a known
% USE_EXPAND prefix from eapi:use_expand/1.

ranking:flag_is_use_expand(Flag) :-
  atom(Flag),
  preference:use_expand_env(_EnvVar, Prefix),
  atom_concat(Prefix, '_', PrefixUnderscore),
  atom_concat(PrefixUnderscore, _, Flag),
  !.


% =============================================================================
%  any_of_group preference helpers (installed satisfaction)
% =============================================================================

%! ranking:group_member_preferred(+Context, +PackageDep)
%
% True if a package_dependency member is "preferred" -- i.e. already
% installed or previously selected in the proof. Used by any_of_group
% rules to try installed alternatives first.

ranking:group_member_preferred(Context, package_dependency(Phase,Strength,C,N,O,V,S,U)) :-
  installed_pkg_satisfies_dep(Context, package_dependency(Phase,Strength,C,N,O,V,S,U)),
  !.
ranking:group_member_preferred(Context, use_conditional_group(positive, Use, RepoEntry, Deps)) :-
  is_preferred_dep(Context, use_conditional_group(positive, Use, RepoEntry, Deps)),
  !.
ranking:group_member_preferred(Context, use_conditional_group(negative, Use, RepoEntry, Deps)) :-
  is_preferred_dep(Context, use_conditional_group(negative, Use, RepoEntry, Deps)),
  !.
ranking:group_member_preferred(Context, all_of_group(Deps)) :-
  Deps \= [],
  forall(member(D, Deps), group_member_preferred(Context, D)),
  !.
ranking:group_member_preferred(_Context, _Other) :-
  fail.

%! ranking:installed_pkg_satisfies_dep(+ParentContext, +PackageDep)
%
% True if an installed package satisfies the version and USE requirements
% of the given package_dependency term. ParentContext is the ?{Context}
% list of the parent literal.

ranking:installed_pkg_satisfies_dep(ParentContext,
                             package_dependency(_Phase,_Strength,C,N,O,V,_S,UseReqs)) :-
  knowledgebase:vdb_repository(VdbRepo),
  query:search([name(N),category(C),installed(true)], VdbRepo://InstalledId),
  ( O == none
  ; query:search(select(version, O, V), VdbRepo://InstalledId)
  ),
  use:installed_pkg_satisfies_use_reqs(ParentContext, VdbRepo://InstalledId, UseReqs),
  !.

%! ranking:installed_version_mismatch_penalty(+PackageDep, -Penalty)
%
% Returns a large negative penalty if a package is installed but the
% installed version does not match the constraint, indicating a forced upgrade.

ranking:installed_version_mismatch_penalty(package_dependency(_Phase,_Strength,C,N,O,V,_S,_U), Penalty) :-
  O \== none,
  knowledgebase:vdb_repository(VdbRepo),
  query:search([name(N),category(C),installed(true)], VdbRepo://_),
  \+ ( query:search([name(N),category(C),installed(true)], VdbRepo://InstalledId),
       query:search(select(version, O, V), VdbRepo://InstalledId)
     ),
  Penalty is -50000000,
  !.
ranking:installed_version_mismatch_penalty(_Dep, 0).


% =============================================================================
%  Provider-reuse candidate reordering (Portage-like)
% =============================================================================

%! ranking:candidates_prefer_proven_providers(+C, +N, +SlotReq, +Candidates, -Reordered)
%
% For virtual packages: reorders candidates to prefer providers whose
% dependencies have already been proven in the current proof tree.
% Non-virtual packages pass through unchanged. This mirrors Portage's
% behaviour of preferring virtual providers that are already being
% installed as part of the dependency closure.

ranking:candidates_prefer_proven_providers(virtual, _N, SlotReq, Candidates, Reordered) :-
  SlotReq \= [slot(_)|_],
  include(candidate_has_proven_provider, Candidates, Preferred),
  Preferred \== [],
  !,
  subtract(Candidates, Preferred, Rest),
  append(Preferred, Rest, Reordered).
ranking:candidates_prefer_proven_providers(_C, _N, _SlotReq, Candidates, Candidates).

%! ranking:candidate_has_proven_provider(+RepoEntry)
%
% True if the candidate's RDEPEND references a (C,N) that has already
% been selected in the current proof.

ranking:candidate_has_proven_provider(Repo://Entry) :-
  cache:entry_metadata(Repo, Entry, rdepend, Dep),
  dep_references_selected_cn(Dep),
  !.

%! ranking:dep_references_selected_cn(+DepTerm)
%
% True if a dependency term references a (C,N) pair that has been
% selected in the current proof snapshot.

ranking:dep_references_selected_cn(package_dependency(_Phase,_Str,C,N,_O,_V,Ss,_U)) :-
  cnselect:snapshot_selected_cn_candidates(C, N, SelCandidates),
  ( Ss = [slot(ReqSlot0)|_] ->
      slotmeta:canon_slot(ReqSlot0, ReqSlot),
      member(SelRepo://SelEntry, SelCandidates),
      query:search(slot(SelSlotRaw), SelRepo://SelEntry),
      slotmeta:canon_slot(SelSlotRaw, SelSlot),
      ReqSlot == SelSlot
  ; true
  ),
  !.
ranking:dep_references_selected_cn(any_of_group(Deps)) :-
  member(D, Deps),
  dep_references_selected_cn(D),
  !.
