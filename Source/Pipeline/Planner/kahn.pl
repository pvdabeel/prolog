/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> KAHN
Kahn's algorithm for topological sorting. Two reusable entry points:

- kahn:toposort/4 - linear order over an adjacency-assoc graph (Node ->
  [Successor...]). Detects cycles and returns unprocessed nodes separately.
  Used by depclean for uninstall ordering.
- kahn:waves/3 - layered (parallel-wave) order over an edge(From,To) list,
  using a reverse-adjacency map so each step only touches nodes whose
  in-degree changes. Used by the scheduler to order the SCC condensation DAG.
*/

:- module(kahn, []).

% =============================================================================
%  KAHN declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Topological sort
% -----------------------------------------------------------------------------

%! kahn:toposort(+Nodes, +Edges, -Order, -Cyclic)
%
% Topological sort via Kahn's algorithm. Edges is an assoc mapping each
% node to its list of successors (dependencies). Returns the sorted Order
% and Cyclic = true if a cycle prevents full ordering (remaining nodes
% are appended to the order).

kahn:toposort(Nodes, Edges, Order, Cyclic) :-
  kahn:indegrees(Nodes, Edges, InDeg0),
  findall(N, (member(N, Nodes), get_assoc(N, InDeg0, 0)), Q0),
  kahn:process(Q0, Nodes, Edges, InDeg0, [], Order0, Remaining),
  ( Remaining == [] ->
      Cyclic = false,
      Order = Order0
  ; Cyclic = true,
    append(Order0, Remaining, Order)
  ).


% -----------------------------------------------------------------------------
%  In-degree computation
% -----------------------------------------------------------------------------

%! kahn:indegrees(+Nodes, +Edges, -InDeg)
%
% Build the initial in-degree map for all nodes.

kahn:indegrees(Nodes, Edges, InDeg) :-
  empty_assoc(Empty0),
  foldl([N,In,Out]>>put_assoc(N, In, 0, Out), Nodes, Empty0, Empty1),
  foldl(kahn:indegree_acc(Edges), Nodes, Empty1, InDeg).


%! kahn:indegree_acc(+Edges, +Node, +InDegIn, -InDegOut)
%
% Accumulate in-degrees contributed by Node's successors.

kahn:indegree_acc(Edges, A, In, Out) :-
  ( get_assoc(A, Edges, Bs) -> true ; Bs = [] ),
  foldl(kahn:inc_indeg, Bs, In, Out).


%! kahn:inc_indeg(+Node, +InDegIn, -InDegOut)
%
% Increment the in-degree counter for Node by one.

kahn:inc_indeg(B, In, Out) :-
  ( get_assoc(B, In, V0) ->
      V1 is V0 + 1,
      put_assoc(B, In, V1, Out)
  ; put_assoc(B, In, 1, Out)
  ).


% -----------------------------------------------------------------------------
%  Work loop
% -----------------------------------------------------------------------------

%! kahn:process(+Queue, +Nodes, +Edges, +InDeg, +Acc, -Order, -Remaining)
%
% Work loop. Processes zero-in-degree nodes, decrements neighbors, and
% collects the topological order. Remaining holds any nodes still
% unprocessed (cycle members).

kahn:process([], Nodes, _Edges, InDeg, Acc, Order, Remaining) :-
  reverse(Acc, Order),
  findall(N, (member(N, Nodes), get_assoc(N, InDeg, V), V > 0), Remaining).

kahn:process([N|Q], Nodes, Edges, InDeg0, Acc, Order, Remaining) :-
  ( get_assoc(N, Edges, Bs) -> true ; Bs = [] ),
  put_assoc(N, InDeg0, -1, InDeg1),
  kahn:dec_neighbors(Bs, InDeg1, InDeg2, NewZeros),
  append(Q, NewZeros, Q2),
  kahn:process(Q2, Nodes, Edges, InDeg2, [N|Acc], Order, Remaining).


%! kahn:dec_neighbors(+Neighbors, +InDegIn, -InDegOut, -NewZeros)
%
% Decrement in-degree for each neighbor; collect those that reach zero.

kahn:dec_neighbors([], InDeg, InDeg, []).

kahn:dec_neighbors([B|Bs], InDeg0, InDeg, NewZeros) :-
  ( get_assoc(B, InDeg0, V0),
    V0 >= 0 ->
      V1 is V0 - 1,
      put_assoc(B, InDeg0, V1, InDeg1),
      ( V1 =:= 0 -> NewZeros = [B|RestZeros] ; NewZeros = RestZeros ),
      kahn:dec_neighbors(Bs, InDeg1, InDeg, RestZeros)
  ; kahn:dec_neighbors(Bs, InDeg0, InDeg, NewZeros)
  ).


% -----------------------------------------------------------------------------
%  Wave-based topological sort (parallel levels)
% -----------------------------------------------------------------------------

%! kahn:waves(+Nodes, +Edges, -Waves)
%
% Layered topological sort. Edges is a list of edge(From, To) terms meaning
% "From depends on To" (To must be scheduled before From). Waves is a list of
% levels; each level is a list of nodes whose dependencies are all satisfied
% by earlier levels, so the nodes within a level may be processed in parallel.
% Nodes left in a cycle never become ready and are omitted -- callers that need
% them detect cycles separately (e.g. the scheduler feeds an SCC condensation
% DAG, which is acyclic by construction).
%
% Performance: uses a reverse-adjacency map so each step only touches nodes
% whose in-degree actually changes, rather than rescanning all nodes/edges per
% wave. This is the hot-path Kahn used by the scheduler.

kahn:waves(Nodes, Edges, Waves) :-
  kahn:wave_indegrees(Nodes, Edges, Indeg0),
  kahn:rev_adj_map(Nodes, Edges, RevAdj),
  kahn:set_from_list(Nodes, RemSet0),
  kahn:wave_ready(Nodes, Indeg0, Ready0),
  kahn:wave_loop(Ready0, RemSet0, RevAdj, Indeg0, [], WavesRev),
  reverse(WavesRev, Waves).


%! kahn:wave_indegrees(+Nodes, +Edges, -Indeg)
%
% In-degree (number of unsatisfied dependencies) per node, restricted to Nodes.

kahn:wave_indegrees(Nodes, Edges, Indeg) :-
  empty_assoc(E),
  foldl(kahn:wave_init_zero, Nodes, E, I0),
  foldl(kahn:wave_add_edge_indegree(Nodes), Edges, I0, Indeg).

kahn:wave_init_zero(N, In, Out) :- put_assoc(N, In, 0, Out).

kahn:wave_add_edge_indegree(Nodes, edge(From, To), In, Out) :-
  ( memberchk(From, Nodes), memberchk(To, Nodes) ->
      get_assoc(From, In, D0),
      D is D0 + 1,
      put_assoc(From, In, D, Out)
  ; Out = In
  ).


%! kahn:wave_ready(+Nodes, +Indeg, -Ready)
%
% Nodes whose in-degree is zero (no remaining dependencies).

kahn:wave_ready(Nodes, Indeg, Ready) :-
  findall(N, (member(N, Nodes), get_assoc(N, Indeg, 0)), Ready).


%! kahn:rev_adj_map(+Nodes, +Edges, -RevAdj)
%
% Reverse adjacency map To -> [From...] restricted to Nodes, so completing To
% can decrement the in-degree of exactly its dependents.

kahn:rev_adj_map(Nodes, Edges, RevAdj) :-
  empty_assoc(M0),
  foldl(kahn:rev_adj_put(Nodes), Edges, M0, RevAdj),
  !.

kahn:rev_adj_put(Nodes, edge(From, To), In, Out) :-
  ( memberchk(From, Nodes), memberchk(To, Nodes) ->
      ( get_assoc(To, In, L0) -> true ; L0 = [] ),
      ( memberchk(From, L0) -> L1 = L0 ; L1 = [From|L0] ),
      put_assoc(To, In, L1, Out)
  ; Out = In
  ).


%! kahn:set_from_list(+List, -Set)
%
% Build an assoc-backed set (Key -> true) from a list of keys.

kahn:set_from_list(List, Set) :-
  empty_assoc(S0),
  foldl(kahn:set_put, List, S0, Set),
  !.

kahn:set_put(K, A0, A) :- put_assoc(K, A0, true, A).

kahn:set_remove_all([], Set, Set) :- !.
kahn:set_remove_all([K|Ks], Set0, Set) :-
  ( del_assoc(K, Set0, _V, Set1) -> true ; Set1 = Set0 ),
  kahn:set_remove_all(Ks, Set1, Set).


%! kahn:wave_loop(+Ready, +RemSet, +RevAdj, +Indeg, +WavesIn, -WavesOut)
%
% Emit one wave (the current ready set), remove it from the remaining set,
% decrement the in-degree of its dependents, and recurse on the newly-ready
% nodes. WavesOut is accumulated in reverse (newest wave first).

kahn:wave_loop([], _RemSet, _RevAdj, _Indeg, Waves, Waves) :- !.
kahn:wave_loop(Ready0, RemSet0, RevAdj, Indeg0, Waves0, Waves) :-
  Wave = Ready0,
  kahn:set_remove_all(Wave, RemSet0, RemSet1),
  kahn:dec_dependents_for_wave(Wave, RemSet1, RevAdj, Indeg0, Indeg, NextReady),
  kahn:wave_loop(NextReady, RemSet1, RevAdj, Indeg, [Wave|Waves0], Waves).

kahn:dec_dependents_for_wave(Wave, RemSet, RevAdj, Indeg0, Indeg, NextReady) :-
  empty_assoc(R0),
  kahn:dec_dependents_for_wave_(Wave, RemSet, RevAdj, Indeg0, Indeg, R0, R),
  assoc:assoc_to_keys(R, NextReady).

kahn:dec_dependents_for_wave_([], _RemSet, _RevAdj, Indeg, Indeg, R, R) :- !.
kahn:dec_dependents_for_wave_([Dep|Deps], RemSet, RevAdj, Indeg0, Indeg, R0, R) :-
  ( get_assoc(Dep, RevAdj, Froms0) -> true ; Froms0 = [] ),
  kahn:dec_dependents_list(Froms0, RemSet, Indeg0, Indeg1, R0, R1),
  kahn:dec_dependents_for_wave_(Deps, RemSet, RevAdj, Indeg1, Indeg, R1, R).

kahn:dec_dependents_list([], _RemSet, Indeg, Indeg, R, R) :- !.
kahn:dec_dependents_list([N|Ns], RemSet, Indeg0, Indeg, R0, R) :-
  ( get_assoc(N, RemSet, true) ->
      ( get_assoc(N, Indeg0, D0) -> true ; D0 = 0 ),
      D1 is max(0, D0 - 1),
      put_assoc(N, Indeg0, D1, Indeg1),
      ( D1 =:= 0 ->
          put_assoc(N, R0, true, R1)
      ; R1 = R0
      )
  ; Indeg1 = Indeg0,
    R1 = R0
  ),
  kahn:dec_dependents_list(Ns, RemSet, Indeg1, Indeg, R1, R).