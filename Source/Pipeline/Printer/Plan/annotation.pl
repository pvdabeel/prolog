/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> ANNOTATION
Single-pass proof traversal collecting printer annotations.

Printing one plan used to walk the proof AVL roughly ten times (blocker
notes, four pre-action sweeps, cycle-break injection, and the warning
suggestion collectors), each a fresh findall over assoc:gen_assoc, and
builder:build/0 triggered the pre-action sweeps a second time. This
module replaces those independent traversals with a single pass that
classifies every proof entry into a proof_annotations record consumed
by plan.pl, warning.pl and builder.pl.

The record is opaque to consumers; use the accessors below.
*/

:- module(annotation, []).

% =============================================================================
%  ANNOTATION declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Context unwrapping
% -----------------------------------------------------------------------------

%! annotation:unwrap_ctx(+Ctx0, -Ctx) is det.
%
% Extracts a plain list from a proof context (`?{Context}` list) term that
% may be wrapped in {}/1 or be the empty-context atom '{}'.

annotation:unwrap_ctx(Ctx0, Ctx) :-
  ( is_list(Ctx0) -> Ctx = Ctx0
  ; Ctx0 = {Inner}, is_list(Inner) -> Ctx = Inner
  ; Ctx = []
  ).


% -----------------------------------------------------------------------------
%  Single-pass collector
% -----------------------------------------------------------------------------

%! annotation:collect(+ProofAVL, -Annotations)
%
% Walks the proof AVL once and classifies every entry, producing an opaque
% proof_annotations record with the following (sorted) components:
%
% - Unmasks:           unmask(R, E, C, N) for proof entries tagged
%                      suggestion(unmask, _). Entries also tagged
%                      suggestion(accept_license, _) are excluded here;
%                      they appear under Licenses instead (the two tags
%                      are mutually exclusive per context in rules).
% - Licenses:          accept_license(R, E, C, N) for entries tagged
%                      suggestion(accept_license, _)
% - Keywords:          accept_keyword(R, E, C, N, K) for entries tagged
%                      suggestion(accept_keyword, K)
% - UseChanges:        use_change(R, E, C, N, Changes) for entries tagged
%                      suggestion(use_change, _, Changes)
% - BwuChanges:        use_change(R://E, Enables, Disables) — legacy
%                      build_with_use scan over context-bearing proof keys
% - BlockerNotes:      assoc mapping key(C, N, Phase) -> note(Strength,
%                      Origin) for blocker domain assumptions
% - CycleBreaks:       cycle_break(Content, Value) for prover cycle-break
%                      assumptions (proof key assumed(rule(Content)))
% - DomainAssumptions: Content for domain assumptions (proof key
%                      rule(assumed(Content)))

annotation:collect(ProofAVL, proof_annotations(Unmasks, Licenses, Keywords,
                                               UseChanges, BwuChanges,
                                               BlockerNotes, CycleBreaks,
                                               DomainAssumptions)) :-
  findall(Type-Term,
          ( assoc:gen_assoc(Key, ProofAVL, Value),
            annotation:proof_entry_tag(Key, Value, Type, Term)
          ),
          Tags),
  annotation:tagged(unmask,     Tags, Unmasks),
  annotation:tagged(license,    Tags, Licenses),
  annotation:tagged(keyword,    Tags, Keywords),
  annotation:tagged(usechange,  Tags, UseChanges),
  annotation:tagged(bwu,        Tags, BwuChanges),
  annotation:tagged(cyclebreak, Tags, CycleBreaks),
  annotation:tagged(domain,     Tags, DomainAssumptions),
  annotation:tagged(blocker,    Tags, BlockerPairs),
  empty_assoc(Empty),
  foldl(annotation:blocker_note_put, BlockerPairs, Empty, BlockerNotes).


%! annotation:tagged(+Type, +Tags, -Sorted)
%
% Extracts all terms of a given tag type from the classified tag list,
% sorted and deduplicated.

annotation:tagged(Type, Tags, Sorted) :-
  findall(Term, member(Type-Term, Tags), Terms),
  sort(Terms, Sorted).


%! annotation:proof_entry_tag(+Key, +Value, -Type, -Term) is nondet.
%
% Classifies a single proof entry. An entry may yield multiple tags
% (e.g. a domain assumption that is also a blocker, or a resolved entry
% carrying both keyword and USE-change suggestions).

% Domain assumptions (proof key rule(assumed(Content))) — also checked
% for blocker shape to feed the blocker note map.
annotation:proof_entry_tag(rule(assumed(Content)), _Value, Type, Term) :-
  !,
  ( Type = domain,
    Term = Content
  ; annotation:blocker_assumption_term(Content, Strength, Phase, C, N, Origin),
    Type = blocker,
    Term = key(C,N,Phase)-note(Strength,Origin)
  ).
% Prover cycle-break assumptions (proof key assumed(rule(Content))).
annotation:proof_entry_tag(assumed(rule(Content)), Value, cyclebreak,
                           cycle_break(Content, Value)) :- !.
% Fully resolved entries: suggestion tags live in the value context. The
% self-target's own unmask / keyword / license tags ride on its :install
% carrier (:run path) or a synthetic :annotate carrier (:fetchonly path,
% see rules.pl), so the collector never needs to re-derive them from the
% mask state — it just reads whatever the resolver placed in the context.
annotation:proof_entry_tag(rule(R://E:_Action), _?Ctx0, Type, Term) :-
  !,
  annotation:unwrap_ctx(Ctx0, Ctx),
  annotation:suggestion_tag(Ctx, R, E, Type, Term).
% Legacy build_with_use scan over context-bearing proof keys.
annotation:proof_entry_tag(Key, _Value, bwu, use_change(Entry, Enables, Disables)) :-
  annotation:proof_key_use_changes(Key, Entry, Enables, Disables),
  ( Enables \== [] ; Disables \== [] ).


%! annotation:suggestion_tag(+Ctx, +R, +E, -Type, -Term) is nondet.
%
% Yields one tag per suggestion(...) kind present in a resolved proof
% entry's context.

annotation:suggestion_tag(Ctx, R, E, unmask, unmask(R, E, C, N)) :-
  memberchk(suggestion(unmask, _), Ctx),
  \+ memberchk(suggestion(accept_license, _), Ctx),
  cache:ordered_entry(R, E, C, N, _).
annotation:suggestion_tag(Ctx, R, E, license, accept_license(R, E, C, N)) :-
  memberchk(suggestion(accept_license, _), Ctx),
  cache:ordered_entry(R, E, C, N, _).
annotation:suggestion_tag(Ctx, R, E, keyword, accept_keyword(R, E, C, N, K)) :-
  memberchk(suggestion(accept_keyword, K), Ctx),
  cache:ordered_entry(R, E, C, N, _).
annotation:suggestion_tag(Ctx, R, E, usechange, use_change(R, E, C, N, Changes)) :-
  memberchk(suggestion(use_change, _, Changes), Ctx),
  cache:ordered_entry(R, E, C, N, _).


% -----------------------------------------------------------------------------
%  Blocker note extraction
% -----------------------------------------------------------------------------

%! annotation:blocker_assumption_term(+Content, -Strength, -Phase, -C, -N, -Origin)
%
% Destructures a blocker domain assumption (with or without proof context)
% into its note components.

annotation:blocker_assumption_term(Content0, Strength, Phase, C, N, Origin) :-
  ( Content0 = '?'(blocker(Strength, Phase, C, N, _O, _V, _SlotReq), Ctx0),
    ( is_list(Ctx0) ->
        Ctx = Ctx0
    ; Ctx0 = {InnerList}, is_list(InnerList) ->
        Ctx = InnerList
    ; Ctx = []
    ),
    ( memberchk(self(Origin), Ctx) -> true ; Origin = unknown )
  )
  ;
  ( Content0 = blocker(Strength, Phase, C, N, _O2, _V2, _SlotReq2),
    Origin = unknown
  ),
  ( Strength == weak ; Strength == strong ),
  ( Phase == install ; Phase == run ).


%! annotation:blocker_note_put(+Pair, +NotesIn, -NotesOut)
%
% Inserts a blocker note pair into the note assoc, keeping the first
% note seen for a given key.

annotation:blocker_note_put(K-V, In, Out) :-
  ( get_assoc(K, In, _) ->
      Out = In
  ; put_assoc(K, In, V, Out)
  ).


% -----------------------------------------------------------------------------
%  Legacy build_with_use key scan
% -----------------------------------------------------------------------------

%! annotation:proof_key_use_changes(+Key, -Entry, -NeedEnable, -NeedDisable)
%
% For a context-bearing proof key with build_with_use state, computes which
% USE flags need enabling or disabling relative to the effective USE set.

annotation:proof_key_use_changes(Repo://Id:_Action?{Ctx0}, Repo://Id,
                                 NeedEnable, NeedDisable) :-
  annotation:unwrap_ctx(Ctx0, Ctx),
  memberchk(build_with_use:use_state(En, Dis), Ctx),
  use:entry_effective_use_set(Repo://Id, EffEnabled),
  findall(U, ( member(U, En), \+ memberchk(U, EffEnabled) ), NeedEnable0),
  findall(U, ( member(U, Dis), memberchk(U, EffEnabled) ), NeedDisable0),
  sort(NeedEnable0, NeedEnable),
  sort(NeedDisable0, NeedDisable).


% -----------------------------------------------------------------------------
%  Accessors
% -----------------------------------------------------------------------------

%! annotation:unmasks(+Annotations, -Unmasks)
%
% Sorted list of unmask(R, E, C, N) terms.

annotation:unmasks(proof_annotations(U,_,_,_,_,_,_,_), U).


%! annotation:licenses(+Annotations, -Licenses)
%
% Sorted list of accept_license(R, E, C, N) terms.

annotation:licenses(proof_annotations(_,L,_,_,_,_,_,_), L).


%! annotation:keywords(+Annotations, -Keywords)
%
% Sorted list of accept_keyword(R, E, C, N, K) terms.

annotation:keywords(proof_annotations(_,_,K,_,_,_,_,_), K).


%! annotation:use_changes(+Annotations, -UseChanges)
%
% Sorted list of use_change(R, E, C, N, Changes) terms.

annotation:use_changes(proof_annotations(_,_,_,UC,_,_,_,_), UC).


%! annotation:bwu_changes(+Annotations, -BwuChanges)
%
% Sorted list of legacy use_change(R://E, Enables, Disables) terms.

annotation:bwu_changes(proof_annotations(_,_,_,_,B,_,_,_), B).


%! annotation:blocker_notes(+Annotations, -Notes)
%
% Assoc mapping key(C, N, Phase) -> note(Strength, Origin).

annotation:blocker_notes(proof_annotations(_,_,_,_,_,N,_,_), N).


%! annotation:domain_assumptions(+Annotations, -DomainAssumptions)
%
% Sorted list of domain assumption contents (proof key rule(assumed(X))).

annotation:domain_assumptions(proof_annotations(_,_,_,_,_,_,_,D), D).


%! annotation:cycle_break_contents(+Annotations, -Contents)
%
% Sorted list of prover cycle-break contents (proof key assumed(rule(X))).

annotation:cycle_break_contents(proof_annotations(_,_,_,_,_,_,CBs,_), Contents) :-
  findall(C, member(cycle_break(C, _), CBs), Contents).


%! annotation:cycle_break_rules(+Annotations, -Rules)
%
% Sorted list of full-format cycle-break rules, as injected into the first
% plan step by plan:inject_cycle_break_verifies/3.

annotation:cycle_break_rules(proof_annotations(_,_,_,_,_,_,CBs,_), Rules) :-
  findall(Rule,
          ( member(cycle_break(Content, Value), CBs),
            prover:canon_rule(Rule, assumed(rule(Content)), Value)
          ),
          Rules0),
  sort(Rules0, Rules).


%! annotation:pre_actions(+Annotations, -PreActions)
%
% Concatenated unmask / license / keyword / USE-change pre-plan actions,
% in the order the plan printer displays them.

annotation:pre_actions(Annotations, PreActions) :-
  annotation:unmasks(Annotations, Unmasks),
  annotation:licenses(Annotations, Licenses),
  annotation:keywords(Annotations, Keywords),
  annotation:use_changes(Annotations, UseChanges),
  append([Unmasks, Licenses, Keywords, UseChanges], PreActions).
