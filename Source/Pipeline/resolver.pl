/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> RESOLVER
Resolve stage (pass 1): what to build.

Thin pipeline stage that proves the target configuration — versions,
USE flags, slots — by handing the generic prover the `resolving` rule
set (Source/Domain/Gentoo/Rules/resolving.pl).  The counterpart stage is the
orderer (Source/Pipeline/orderer.pl), which hands the same prover the
`ordering` rule set to derive when each step happens.

Also hosts the whole-repository test harnesses for the resolve stage
(resolver:test/1, resolver:test_latest/1, resolver:test_stats/1 and
friends).  These drive pipeline:prove_with_fallback, so they exercise
the same 5-tier fallback semantics as --pretend.
*/

:- module(resolver, []).

% =============================================================================
%  RESOLVER declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Stage entry point
% -----------------------------------------------------------------------------

%! resolver:resolve(+Target, +InProof, -OutProof, +InModel, -OutModel, +InCons, -OutCons, +InTriggers, -OutTriggers)
%
% Prove Target against the resolving rule set: full reprove harness,
% learned-constraint lifecycle, trigger construction.  The signature is
% the prover's, minus the rule module (which this stage owns).

resolver:resolve(Target, InProof, OutProof, InModel, OutModel, InCons, OutCons, InTriggers, OutTriggers) :-
  prover:prove(resolving, Target, InProof, OutProof, InModel, OutModel, InCons, OutCons, InTriggers, OutTriggers).


% -----------------------------------------------------------------------------
%  Automated testing helpers
% -----------------------------------------------------------------------------

%! resolver:test(+Repository) is det
%
% Run a whole-repo resolve test using the default test style.

resolver:test(Repository) :-
  config:test_style(Style),
  resolver:test(Repository,Style).


%! resolver:test(+Repository, +Style) is det
%
% Run a whole-repo resolve test with the given Style (sequential/parallel).

resolver:test(Repository,Style) :-
  config:proving_target(Action0),
  tester:test_action(Action0, Action),
  ( current_predicate(sampler:phase_perf_reset/0) ->
      sampler:phase_perf_reset
  ; true
  ),
  sampler:hook_counter_reset,
  tester:test(Style,
              'Proving',
              Repository://Entry,
              Repository:entry(Entry),
              ( Target = (Repository://Entry:Action?{[]}),
                pipeline:prove_with_fallback([Target], _Proof, _Model, _Triggers)
              )),
  sampler:hook_counter_report,
  ( current_predicate(sampler:phase_perf_report/0) ->
      sampler:phase_perf_report
  ; true
  ).


%! resolver:test_latest(+Repository) is det
%
% Run a resolve test over only the latest ebuild per package.

resolver:test_latest(Repository) :-
  config:test_style(Style),
  resolver:test_latest(Repository,Style).


%! resolver:test_latest(+Repository, +Style) is det
%
% Run a latest-ebuild resolve test with the given Style.

resolver:test_latest(Repository,Style) :-
  config:proving_target(Action0),
  tester:test_action(Action0, Action),
  tester:test(Style,
              'Proving',
              Repository://Entry,
              ( Repository:package(C,N),once(Repository:ebuild(Entry,C,N,_)) ),
              ( Target = (Repository://Entry:Action?{[]}),
                pipeline:prove_with_fallback([Target], _Proof, _Model, _Triggers)
              )).


% -----------------------------------------------------------------------------
%  Testing + statistics
% -----------------------------------------------------------------------------

%! resolver:timed_prove_and_record(+Target, +Action) is det
%
% Shared instrumentation body for the test_stats loops.  Proves
% Target (a Repository://Entry) via pipeline:prove_with_fallback,
% measuring wall time, inferences, rule calls and proof-context union
% costs, and records the outcome with the sampler (under the
% test_stats mutex, as the loops may run in parallel).  Failures are
% recorded as failed(other).  Always succeeds.

resolver:timed_prove_and_record(Repository://Entry, Action) :-
  sampler:reset_counters,
  statistics(inferences, I0),
  statistics(walltime, [T0,_]),
  Target = (Repository://Entry:Action?{[]}),
  ( pipeline:prove_with_fallback([Target], ProofAVL, ModelAVL, Triggers) ->
      Proved = true
  ; Proved = false
  ),
  statistics(walltime, [T1,_]),
  statistics(inferences, I1),
  TimeMs is T1 - T0,
  Inferences is I1 - I0,
  ( Proved == true ->
      sampler:counters(rule_calls(RuleCalls)),
      sampler:ctx_counters(ctx_union_calls(CtxUC), ctx_union_cost(CtxCost), ctx_max_len(CtxMax), ctx_union_ms_est(CtxMsEst)),
      sampler:ctx_distribution(ctx_len_hist(CtxHistPairs),
                               ctx_cost_mul(CtxMul),
                               ctx_cost_add(CtxAdd),
                               ctx_len_samples(CtxLenSamples)),
      with_mutex(test_stats,
        ( sampler:record(costs(Repository://Entry, TimeMs, Inferences, RuleCalls)),
          sampler:record(ctx_costs(Repository://Entry, CtxUC, CtxCost, CtxMax, CtxMsEst)),
          sampler:record(ctx_dist(CtxHistPairs, CtxMul, CtxAdd, CtxLenSamples))
        )),
      sampler:record(entry(Repository://Entry, ModelAVL, ProofAVL, Triggers, true))
  ; sampler:record(failed(other))
  ).


%! resolver:test_stats(+Repository) is det
%
% Run a whole-repo resolve test with detailed statistics recording
% and top-N reporting.

resolver:test_stats(Repository) :-
  config:test_style(Style),
  resolver:test_stats(Repository, Style).


%! resolver:test_stats(+Repository, +TopN) is det
%
% Like test_stats/1, but allows choosing the Top-N limit in the output.

resolver:test_stats(Repository, TopN) :-
  integer(TopN),
  !,
  config:test_style(Style),
  resolver:test_stats(Repository, Style, TopN).


%! resolver:test_stats(+Repository, +Style) is det
%
% Run test_stats with the given Style and default TopN.

resolver:test_stats(Repository, Style) :-
  ( config:test_stats_top_n(TopN) -> true ; TopN = 25 ),
  resolver:test_stats(Repository, Style, TopN).


%! resolver:test_stats(+Repository, +Style, +TopN) is det
%
% Core test_stats loop: proves each entry, records timing / inference /
% rule-call / context-union costs, classifies failures, and prints the
% TopN report at the end.

resolver:test_stats(Repository, Style, TopN) :-
  config:proving_target(Action0),
  tester:test_action(Action0, Action),
  aggregate_all(count, (Repository:entry(_E)), ExpectedTotal),
  sampler:reset('Proving', ExpectedTotal),
  aggregate_all(count, (Repository:package(_C,_N)), ExpectedPkgs),
  sampler:set_expected_pkgs(ExpectedPkgs),
  tester:test(Style,
              'Proving',
              Repository://Entry,
              Repository:entry(Entry),
              resolver:timed_prove_and_record(Repository://Entry, Action)),
  stats:test_stats_print(TopN).


% -----------------------------------------------------------------------------
%  Focused stats: run resolver:test_stats for a specific list of Category/Name pairs
% -----------------------------------------------------------------------------

%! resolver:test_stats_pkgs(+Repository, +Pkgs)
%
% Run test_stats for a specific list of packages, where Pkgs is a list of C-N pairs.
% This is intended for fast iteration on the slowest packages reported by test_stats/1.

resolver:test_stats_pkgs(Repository, Pkgs) :-
  config:test_style(Style),
  ( config:test_stats_top_n(TopN) -> true ; TopN = 25 ),
  resolver:test_stats_pkgs(Repository, Style, TopN, Pkgs).


%! resolver:test_stats_pkgs(+Repository, +Style, +TopN, +Pkgs) is det
%
% Inner loop for focused test_stats over a specific list of packages.

resolver:test_stats_pkgs(Repository, Style, TopN, Pkgs) :-
  is_list(Pkgs),
  config:proving_target(Action0),
  tester:test_action(Action0, Action),
  length(Pkgs, ExpectedTotal),
  sampler:reset('Proving', ExpectedTotal),
  sampler:set_expected_pkgs(ExpectedTotal),
  tester:test(Style,
              'Proving',
              Repository://Entry,
              ( member(C-N, Pkgs),
                once(Repository:ebuild(Entry, C, N, _))
              ),
              resolver:timed_prove_and_record(Repository://Entry, Action)),
  stats:test_stats_print(TopN).
