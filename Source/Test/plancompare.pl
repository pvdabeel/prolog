/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> PLANCOMPARE
Bulk before/after plan fingerprinting for regression checks.

Writes one TSV line per repository entry:
  EntryId<TAB>Status<TAB>Fingerprint

Status is `ok`, `fail`, or `timeout`.  Fingerprint covers fallback tier,
sorted assumption keys from the proof AVL, and the flattened scheduled plan.

Load explicitly (not part of the default standalone graph):

  ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell <<'PL'
  load_files(portage('Source/Test/plancompare'), [if(true)]).
  plancompare:run(portage, '/tmp/plan-compare.tsv').
  plancompare:diff('/tmp/before.tsv', '/tmp/after.tsv').
  halt.
  PL
*/

:- module(plancompare, []).

:- use_module(library(assoc),
              [ assoc_to_list/2, empty_assoc/1, gen_assoc/3
              , get_assoc/3, put_assoc/4
              ]).

:- dynamic plancompare_done_count/1.
:- volatile plancompare_done_count/1.
:- dynamic plancompare_total_count/1.
:- volatile plancompare_total_count/1.


% =============================================================================
%  PLANCOMPARE declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Public API
% -----------------------------------------------------------------------------

%! plancompare:run(+Repository, +OutPath) is det.
%
% Fingerprint the full pipeline plan for every entry in Repository and
% write results to OutPath (TSV).

plancompare:run(Repository, OutPath) :-
  config:proving_target(Action),
  config:time_limit(Limit),
  config:number_of_cpus(Cpus),
  aggregate_all(count, Repository:entry(_), Total),
  plancompare:reset_progress(Total),
  format('Plancompare: fingerprinting ~w entries -> ~w (~w threads)~n',
         [Total, OutPath, Cpus]),
  open(OutPath, write, Out),
  setup_call_cleanup(
    true,
    concurrent_forall(Repository:entry(Entry),
                      plancompare:entry_worker(Repository, Entry, Action, Limit, Out),
                      [threads(Cpus)]),
    close(Out)
  ).


plancompare:reset_progress(Total) :-
  with_mutex(plancompare_progress,
             ( retractall(plancompare_done_count(_)),
               retractall(plancompare_total_count(_)),
               asserta(plancompare_done_count(0)),
               asserta(plancompare_total_count(Total))
             )).


plancompare:entry_worker(Repository, Entry, Action, Limit, Out) :-
  catch(plancompare:entry_line(Repository, Entry, Action, Limit, Out),
        E,
        ( format(user_error, 'plancompare ~w: ~w~n', [Entry, E]),
          with_mutex(plancompare_out,
                     format(Out, '~w\tfail\t-~n', [Entry]))
        )),
  plancompare:tick_progress(Entry).


plancompare:tick_progress(Entry) :-
  with_mutex(plancompare_progress,
             ( retract(plancompare_done_count(N0)),
               N1 is N0 + 1,
               asserta(plancompare_done_count(N1)),
               ( N1 mod 1000 =:= 0 ->
                   plancompare_total_count(Total),
                   Percent is (100 * N1) / Total,
                   format(user_error, '~0f% (~w/~w) last=~w~n',
                          [Percent, N1, Total, Entry])
               ; true
               )
             )).


%! plancompare:diff(+BeforePath, +AfterPath) is det.
%
% Print a summary of mismatches between two TSV files from run/2.

plancompare:diff(BeforePath, AfterPath) :-
  plancompare:load_tsv(BeforePath, Before),
  plancompare:load_tsv(AfterPath, After),
  plancompare:diff_maps(Before, After).


%! plancompare:entry_line(+Repository, +Entry, +Action, +Limit, +Out) is det.

plancompare:entry_line(Repository, Entry, Action, Limit, Out) :-
  Target = Repository://Entry:Action?{[]},
  ( catch(call_with_time_limit(Limit,
              plancompare:prove_fingerprint(Target, Status, FP)),
          time_limit_exceeded,
          ( Status = timeout, FP = '-' ))
  -> true
  ;  Status = fail,
     FP = '-'
  ),
  with_mutex(plancompare_out,
             format(Out, '~w\t~w\t~w~n', [Entry, Status, FP])).


plancompare:prove_fingerprint(Target, ok, FP) :-
  pipeline:prove_plan_with_fallback([Target], Proof, _Model, Plan, _Triggers, Fallback),
  plancompare:fingerprint(Proof, Plan, Fallback, FP).


plancompare:fingerprint(Proof, Plan, Fallback, FP) :-
  assoc_to_list(Proof, Pairs),
  findall(Key,
          ( member(Key-_, Pairs),
            ( Key = rule(assumed(_))
            ; Key = assumed(rule(_))
            )
          ),
          AssKeys),
  sort(AssKeys, AssSorted),
  append(Plan, FlatPlan),
  term_hash(sign(Fallback, AssSorted, FlatPlan), FP).


plancompare:load_tsv(Path, Map) :-
  setup_call_cleanup(
    open(Path, read, In),
    ( empty_assoc(Empty),
      plancompare:read_tsv_loop(In, Empty, Map)
    ),
    close(In)
  ).


plancompare:read_tsv_loop(In, MapIn, MapOut) :-
  read_line_to_string(In, Line),
  ( Line == end_of_file ->
      MapOut = MapIn
  ; plancompare:parse_tsv_line(Line, Entry, Status, FP),
    put_assoc(Entry, MapIn, status_fp(Status, FP), Map1),
    plancompare:read_tsv_loop(In, Map1, MapOut)
  ).


plancompare:parse_tsv_line(Line, Entry, Status, FP) :-
  split_string(Line, "\t", "", [E, S, F]),
  atom_string(Entry, E),
  atom_string(Status, S),
  atom_string(FP, F).


plancompare:diff_maps(Before, After) :-
  assoc_to_list(Before, BPairs),
  assoc_to_list(After, APairs),
  findall(E, member(E-_, BPairs), BKeys),
  findall(E, member(E-_, APairs), AKeys),
  append(BKeys, AKeys, AllKeys),
  sort(AllKeys, Entries),
  length(Entries, N),
  format('Entries compared: ~w~n', [N]),
  plancompare:count_entry_mismatch(Entries, Before, After, 0, M),
  format('Mismatches: ~w~n', [M]),
  ( M > 0 ->
      format('~nFirst mismatches:~n', []),
      plancompare:show_entry_mismatches(Entries, Before, After, 25)
  ; format('All fingerprints identical.~n', [])
  ).


plancompare:count_entry_mismatch([], _, _, N, N).
plancompare:count_entry_mismatch([E|Es], Before, After, N0, N) :-
  plancompare:entry_sf(Before, E, SF1),
  plancompare:entry_sf(After, E, SF2),
  ( SF1 == SF2 -> N1 = N0 ; N1 is N0 + 1 ),
  plancompare:count_entry_mismatch(Es, Before, After, N1, N).


plancompare:entry_sf(Map, Entry, SF) :-
  ( get_assoc(Entry, Map, status_fp(Status, FP)) ->
      SF = sf(Status, FP)
  ; SF = missing
  ).


plancompare:show_entry_mismatches([], _, _, _).
plancompare:show_entry_mismatches(_, _, _, 0) :- !.
plancompare:show_entry_mismatches([E|Es], Before, After, Left) :-
  plancompare:entry_sf(Before, E, SF1),
  plancompare:entry_sf(After, E, SF2),
  ( SF1 == SF2 ->
      Left1 = Left
  ; format('  ~w~n    before: ~w~n    after:  ~w~n', [E, SF1, SF2]),
    Left1 is Left - 1
  ),
  plancompare:show_entry_mismatches(Es, Before, After, Left1).
