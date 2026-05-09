/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

% -----------------------------------------------------------------------------
%  Action: RESUME (skip args helper)
% -----------------------------------------------------------------------------

%! action:assert_resume_skip_args(+Args) is det.
%
% Asserts each positional argument as a config:skip_atom/1 fact.
% When --resume is active, positional args name packages to skip.

action:assert_resume_skip_args([]).

action:assert_resume_skip_args([A|Rest]) :-
  asserta(config:skip_atom(A)),
  assert_resume_skip_args(Rest).


% -----------------------------------------------------------------------------
%  Action: BUILD
% -----------------------------------------------------------------------------

%! action:process_build(+Args, +Options) is det.
%
% Handles the --build CLI flag. Resolves targets, proves a plan, then
% builds with live progress output.

action:process_build([], _Options) :-
  !,
  message:failure('No targets specified for --build.').

action:process_build(ArgsSets, Options) :-
  eapi:substitute_sets(ArgsSets, Args),
  interface:report_unresolvable_targets(run, Args),
  findall(target(Q,Arg):run?{[]},
          ( member(Arg, Args),
            atom_codes(Arg, Codes),
            phrase(eapi:qualified_target(Q), Codes),
            once(kb:query(Q, _R://_E))
          ),
          Proposal),
  !,
  ( Proposal == []
  -> message:failure('No valid targets found.')
  ;  builder:build(Proposal),
     action:maybe_ci_exit_on_build_failure(Options)
  ).


%! action:maybe_ci_exit_on_build_failure(+Options) is det.
%
% In `--ci` mode, propagate any sub-step build failure to the process
% exit code. Without this, a failed install/merge of a sub-dep is only
% reflected in the printed summary (`Failed: N`); the pipeline still
% exits 0 and downstream tooling cannot detect the problem.
%
% Exit codes:
%   - 0 : all actions completed successfully
%   - 3 : one or more actions failed (build/install/merge step failed
%         and execute_plan skipped the remainder).
%
% We deliberately use exit code 3 (not 1 or 2) to disambiguate from
% the prover-side codes that `--merge --ci` already produces:
%   - 0 : clean
%   - 1 : prover cycle-break assumptions
%   - 2 : domain assumptions
%   - 3 : at least one action failed during execution
%
% Only halts when both `--ci` is set AND `Failed > 0`.

action:maybe_ci_exit_on_build_failure(Options) :-
  ( memberchk(ci(true), Options),
    builder:last_build_status(_Completed, Failed, _Stubs),
    Failed > 0
  -> halt(3)
  ;  true
  ).
