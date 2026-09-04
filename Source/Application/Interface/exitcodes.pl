/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> EXITCODES
Centralized process exit-code lookup table plus the CI exit-code computation
from proof artifacts (included into the INTERFACE module via interface.pl).

The interface:exit_code/3 table is the single source of truth for the numeric
exit codes portage-ng returns. Extend the table here (and reference the symbolic
name elsewhere) rather than scattering literal integers across the codebase.
*/

% -----------------------------------------------------------------------------
%  Exit code lookup table
% -----------------------------------------------------------------------------

%! interface:exit_code(?Name, ?Code, ?Description) is nondet.
%
% Lookup table mapping a symbolic Name onto its numeric Code and a short
% Description. Callers should resolve codes by name
% (interface:exit_code(clean, C, _)) so the numbers live in exactly one place.

interface:exit_code(clean,              0, 'No assumptions (clean plan)').
interface:exit_code(cycle_breaks,       1, 'Only prover cycle-break assumptions').
interface:exit_code(domain_assumptions, 2, 'Domain assumptions present (missing/non-existent deps, etc.)').


% -----------------------------------------------------------------------------
%  CI helpers
% -----------------------------------------------------------------------------

%! interface:ci_exit_code(+ModelAVL, +ProofAVL, -ExitCode) is det.
%
% Computes the CI exit code from the proof artifacts:
%   0 = no assumptions (clean plan)
%   1 = only prover cycle-break assumptions
%   2 = domain assumptions present (missing/non-existent deps, etc.)

interface:ci_exit_code(ModelAVL, ProofAVL, ExitCode) :-
  ( interface:has_any_assumption(ModelAVL) ->
      ( interface:has_domain_assumptions(ProofAVL) -> interface:exit_code(domain_assumptions, ExitCode, _)
      ; interface:has_cycle_breaks(ProofAVL)       -> interface:exit_code(cycle_breaks, ExitCode, _)
      ; interface:exit_code(clean, ExitCode, _)   % only ineffective blocker records
      )
  ; interface:exit_code(clean, ExitCode, _)
  ).

%! interface:has_any_assumption(+ModelAVL) is semidet.
%
% Succeeds if the model contains any assumed(_) key.

interface:has_any_assumption(ModelAVL) :-
  assoc:gen_assoc(Key, ModelAVL, _),
  Key = assumed(_),
  !.

%! interface:has_domain_assumptions(+ProofAVL) is semidet.
%
% Succeeds if the proof contains at least one *effective* domain
% assumption (proof key of the form rule(assumed(_))). The check goes
% through annotation:collect/2 so that weak blocker records whose atom
% hits nothing in the plan (see annotation:blocker_effective/2) do not
% count — the printer does not report them, and neither should the exit
% code.

interface:has_domain_assumptions(ProofAVL) :-
  assoc:gen_assoc(rule(assumed(_)), ProofAVL, _),
  !,
  annotation:collect(ProofAVL, Annotations),
  annotation:domain_assumptions(Annotations, [_|_]).

%! interface:has_cycle_breaks(+ProofAVL) is semidet.
%
% Succeeds if the proof contains at least one prover cycle-break
% assumption (proof key of the form assumed(rule(_))).

interface:has_cycle_breaks(ProofAVL) :-
  assoc:gen_assoc(assumed(rule(_)), ProofAVL, _),
  !.
