/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

% -----------------------------------------------------------------------------
%  Action: VARIANTS (multi-variant pretend)
% -----------------------------------------------------------------------------

%! action:run_variants(+VariantsOpt, +Proposal, +BaseProof, +BasePlan, +BaseTriggers) is det.
%
% Detects pivot points and proves variant plans in parallel, then
% prints each variant sequentially with a diff summary.

action:run_variants(VariantsOpt, Proposal, BaseProof, BasePlan, _BaseTriggers) :-
  build_variant_specs(VariantsOpt, Proposal, BaseProof, Specs),
  ( Specs == []
  -> message:inform('No variant pivot points detected.')
  ;  length(Specs, N),
     nl,
     message:color(cyan),
     ( N > 1 -> Plural = 's' ; Plural = '' ),
     format('Proving ~w variant~w in parallel...', [N, Plural]),
     message:color(normal), nl,
     flush_output,
     pipeline:prove_variants_parallel(Proposal, Specs, Results),
     plan:print_variants(Results, BasePlan)
  ).


%! action:build_variant_specs(+Opt, +Proposal, +ProofAVL, -Specs) is det.
%
% Builds variant specifications from the --variants option value.

action:build_variant_specs(auto, Proposal, ProofAVL, Specs) :-
  !,
  variant:detect_pivots(ProofAVL, Proposal, 5, UsePivots, BranchPivots),
  variant:pivots_to_specs(UsePivots, BranchPivots, Specs).

action:build_variant_specs(all, Proposal, ProofAVL, Specs) :-
  !,
  variant:detect_use_pivots(ProofAVL, Proposal, 20, UsePivots),
  variant:pivots_to_specs(UsePivots, [], Specs).

action:build_variant_specs(FlagList, Proposal, ProofAVL, Specs) :-
  atomic_list_concat(Flags, ',', FlagList),
  variant:user_flags_to_specs(Flags, Proposal, ProofAVL, Specs).