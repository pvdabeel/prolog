/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

% -----------------------------------------------------------------------------
%  Action: MERGE
% -----------------------------------------------------------------------------

%! action:process_action(+Action, +Args, +Options) is det.
%
% Catch-all action dispatcher for merge/unmerge/fetchonly.
% These clauses MUST appear after the specific info/search/depclean
% clauses (ensured by include order in action.pl).

action:process_action(_Action,[],Options) :-
  !,
  ignore(message:failure('No targets specified.')),
  action:exit_on_invalid_targets(Options).

action:process_action(Action,ArgsSets,Options) :-
  interface:get_mode(Mode),
  interface:process_server(Host,Port),
  ( memberchk(pretend(true), Options) -> PretendMode = true ; PretendMode = false ),
  eapi:substitute_sets(ArgsSets,Args),
  interface:report_unresolvable_targets(Action, Args),
  findall(target(Q,Arg):Action?{[]},
          ( member(Arg,Args),
            atom_codes(Arg,Codes),
            phrase(eapi:qualified_target(Q),Codes),
            ( Action == uninstall
              -> once((kb:query(Q, R0://E0), kb:query(installed(true), R0://E0)))
              ;  once(kb:query(Q, _R://_E))
            )
          ),
          Proposal),!,
  message:log(['Proposal:  ',Proposal]),
  ( Proposal == [] ->
      ( action:empty_computed_set_message(ArgsSets, EmptyMsg) ->
          ignore(message:inform(EmptyMsg)),
          % Empty computed sets are success (nothing to do), not a bad atom.
          ( memberchk(ci(true), Options) -> halt(0) ; true )
      ; ignore(( config:llm_support(Prompt),
                 atomic_list_concat([Prompt|Args], Message),
                 config:llm_default(Service),
                 current_predicate(Service:Service/2),
                 explainer:call_llm(Service, Message, _) )),
        ignore(message:failure('No valid targets found.')),
        action:exit_on_invalid_targets(Options)
      )
  ; true
  ),
  (Mode == 'client' ->
    (client:rpc_execute(Host,Port,
     (pipeline:prove_plan_with_fallback(Proposal, ProofAVL, ModelAVL, Plan, Triggers, SCCs, _FallbackUsed),
      printer:print(Proposal,ModelAVL,ProofAVL,Plan,Triggers,SCCs)),
     Output),
     writeln(Output));
    ( ( memberchk(timeout(TimeLimitSec), Options) -> true ; TimeLimitSec = 0 ),
      ( memberchk(variants(VariantsOpt), Options) -> true ; VariantsOpt = none ),
      ( memberchk(explain(ExplainOpt), Options) -> true ; ExplainOpt = none ),
      ( TimeLimitSec =< 0 ->
          ( ( pipeline:prove_plan_with_fallback(Proposal, ProofAVL, ModelAVL, Plan, Triggers, SCCs, FallbackUsed) ->
                true
            ; message:bubble(red,'Error'),
              message:color(red),
              message:print(' Proof/planning failed. Check that the target is valid and all dependencies exist.'), nl,
              message:color(normal),
              flush_output,
              choicelog:maybe_dump,
              halt(1)
            ),
            printer:print(Proposal,ModelAVL,ProofAVL,Plan,Triggers,SCCs),
            ( VariantsOpt \== none, PretendMode == true
            -> run_variants(VariantsOpt, Proposal, ProofAVL, Plan, Triggers)
            ;  true
            ),
            ( ExplainOpt \== none, PretendMode == true
            -> run_explain(ExplainOpt, Proposal, ProofAVL, ModelAVL, Plan, Triggers)
            ;  true
            ),
            choicelog:maybe_dump
          )
      ; catch(
          call_with_time_limit(TimeLimitSec,
            ( ( pipeline:prove_plan_with_fallback(Proposal, ProofAVL, ModelAVL, Plan, Triggers, SCCs, FallbackUsed) ->
                  true
              ; message:bubble(red,'Error'),
                message:color(red),
                message:print(' Proof/planning failed. Check that the target is valid and all dependencies exist.'), nl,
                message:color(normal),
                flush_output,
                choicelog:maybe_dump,
                halt(1)
              ),
              printer:print(Proposal,ModelAVL,ProofAVL,Plan,Triggers,SCCs),
              ( VariantsOpt \== none, PretendMode == true
              -> run_variants(VariantsOpt, Proposal, ProofAVL, Plan, Triggers)
              ;  true
              ),
              ( ExplainOpt \== none, PretendMode == true
              -> run_explain(ExplainOpt, Proposal, ProofAVL, ModelAVL, Plan, Triggers)
              ;  true
              ),
              choicelog:maybe_dump
            )),
          time_limit_exceeded,
          ( message:bubble(red,'Error'),
            message:color(red),
            message:print(' Time limit exceeded while proving/planning. Try increasing --timeout or narrowing the target.'), nl,
            message:color(normal),
            flush_output,
            choicelog:maybe_dump,
            halt(1)
          )
        )
      ),
      ( memberchk(ci(true), Options) ->
          interface:ci_exit_code(ModelAVL, ProofAVL, ExitCode),
          halt(ExitCode)
      ; true
      ),
      ( FallbackUsed == false,
        PretendMode == false ->
            execute_world_plan(Plan),
            world:save
        ; true
        )
    )).


% -----------------------------------------------------------------------------
%  Side effects: execute planned world actions
% -----------------------------------------------------------------------------

%! action:execute_world_plan(+Plan) is det.
%
% Walks the plan (list of steps, each a list of rules) and executes any
% world(Atom):Action side effects (register/unregister packages in @world).

action:execute_world_plan([]) :- !.
action:execute_world_plan([Step|Rest]) :-
  execute_world_step(Step),
  execute_world_plan(Rest).

%! action:execute_world_step(+Step) is det.
%
% Processes a single plan step (list of rules), executing world
% side effects for any rule whose head is world(Atom):Action.

action:execute_world_step([]) :- !.
action:execute_world_step([Rule|Rest]) :-
  ( Rule = rule(Head,_Body),
    prover:canon_literal(Head, Core, _Ctx),
    Core = world(Atom):Action ->
      ( Action == register ->
          world:register(Atom)
      ; Action == unregister ->
          world:unregister(Atom)
      ; true
      )
  ; true
  ),
  execute_world_step(Rest).


%! action:empty_computed_set_message(+ArgsSets, -Message) is semidet.
%
% True when the original CLI targets were only computed sets that may
% legitimately expand to nothing. Binds a user-facing inform message so
% an empty proposal is reported as an empty set, not a bad atom.

action:empty_computed_set_message(ArgsSets, Message) :-
  ArgsSets \== [],
  findall(Name, (member(A, ArgsSets), action:computed_set_arg(A, Name)), Names),
  Names \== [],
  length(ArgsSets, NArgs),
  length(Names, NArgs),
  action:empty_computed_set_text(Names, Message).


%! action:computed_set_arg(+Arg, -Name) is semidet.
%
% True when Arg is a `@name` / `name` reference to a known computed set.

action:computed_set_arg(Arg, Name) :-
  ( atom_concat('@', Name, Arg) -> true ; Name = Arg ),
  current_predicate(sets:is_computed_set/1),
  sets:is_computed_set(Name).


%! action:empty_computed_set_text(+Names, -Message) is det.
%
% Informational text for an empty computed-set expansion.

action:empty_computed_set_text(Names, Message) :-
  ( Names = [Name] ->
      ( memberchk(Name, [security, affected, 'new-affected', 'new-glsa']) ->
          format(atom(Message),
                 'No vulnerable packages requiring a GLSA upgrade (@~w is empty).',
                 [Name])
      ; Name == 'preserved-rebuild' ->
          Message = 'No packages consume preserved libraries (@preserved-rebuild is empty).'
      ; Name == 'changed-deps' ->
          Message = 'No installed packages have outdated runtime dependencies (@changed-deps is empty).'
      ; format(atom(Message), 'Computed set @~w is empty.', [Name])
      )
  ; atomic_list_concat(Names, ', @', Joined),
    format(atom(Message), 'Computed sets @~w are empty.', [Joined])
  ).


%! action:empty_security_set(+ArgsSets) is semidet.
%
% Backward-compatible alias: true when every CLI target is a GLSA
% security computed set.

action:empty_security_set(ArgsSets) :-
  ArgsSets \== [],
  forall(member(A, ArgsSets),
         ( action:computed_set_arg(A, Name),
           memberchk(Name, [security, affected, 'new-affected', 'new-glsa'])
         )).