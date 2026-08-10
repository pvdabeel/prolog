/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> ORDERING
Order rule set (pass 2): planning laws + Gentoo bindings.

The rule set the orderer stage hands the generic prover
(`prover:prove_once(ordering, ...)`).  Two layers in one module:

The *planning laws* (rule/2) are domain-generic: a step can be placed
once everything it requires is available; a requirement is available
when an earlier plan step provides it, when the installed world already
provides it, or — failing both — by recording an unreachable assumption.

The *bindings* answer the domain questions those laws ask, by reading
the pass-1 proof and the VDB (installed packages):

  - step/1     : is this literal a pass-1 plan step?
  - requires/2 : what does a step require before it can be placed
                 (build-time deps, downloads, the :install under a :run)?
  - prefers/2  : what would a step like placed earlier without insisting
                 (runtime dep groups, PDEPEND ordering hints)?
  - world/2    : does the installed system, as it stands, already provide
                 this requirement for this consumer?

No graph algorithms live here: requires/prefers are a reading of the
pass-1 proof bodies (dependency provenance), world is a reading of the
VDB. See Documentation/Handbook/13-doc-planning.md (the planning laws
and the Gentoo bindings).
*/

:- module(ordering, []).

:- use_module(library(assoc)).

% =============================================================================
%  ORDERING declarations
% =============================================================================

% -----------------------------------------------------------------------------
% Rule set: the planning laws
% -----------------------------------------------------------------------------
%
% The laws own no Gentoo terms: what a step is, what it requires, and
% what the world already provides are answered by the bindings below.
%
% The availability tie-break "prefer an earlier plan step over the
% installed world" is an if-then-else, NOT alternative clauses: pass-2
% derivations must succeed without choicepoints so the prover's
% cycle-stack pops (setup_call_cleanup cleanups around each rule body)
% fire eagerly. A nondet rule would defer the pops, leave completed
% literals on the stack, and make the guard below see stale "ancestors".
% (Pass 1 tolerates nondet rules because completed literals short-circuit
% through the model before the cycle stack is ever consulted; this guard
% consults the stack directly, so it needs the stack accurate.)
%
% The `\+ prover:currently_proving/1` guard itself is load-bearing: the
% prover core never fails a cyclic goal (it succeeds benignly or as a
% cycle-break assumption), so without the guard the world case would be
% unreachable and every pass-2 cycle would silently become a cycle-break
% marker — the mechanism this engine retires.

%! ordering:rule(+Literal, -Body)
%
% The pass-2 rule set. One clause per literal functor, each
% deterministic (see above). The first clause strips the optional
% ?{Ctx} proof-context wrapper the prover's literal canonicalization
% may re-attach.

rule(Literal?{_}, Body) :-
  !,
  ordering:rule(Literal, Body).

% A step can be placed once everything it requires is available:
rule(scheduled(H), Conds) :-
  ordering:step(H),
  findall(available(H, D), ordering:requires(H, D), Conds).

% A requirement is available when an earlier plan step provides it, or —
% failing that — when the world as it stands already provides it, or —
% failing that too — by recording the bootstrap failure as a negative
% domain assumption instead of failing the pass:
rule(available(H, D), Body) :-
  (   ordering:step(D),
      \+ prover:currently_proving(scheduled(D))
  ->  Body = [scheduled(D)]
  ;   ordering:world(H, D)
  ->  Body = []
  ;   Body = [assumed(unreachable(H, D))]
  ).

rule(assumed(unreachable(_, _)), []).


% -----------------------------------------------------------------------------
% Pass-1 proof access
% -----------------------------------------------------------------------------

%! ordering:proof(-ProofAVL)
%
% The pass-1 proof published by orderer:with_ordering_pass/2.

proof(ProofAVL) :-
  nb_current(portage_ordering_proof, ProofAVL),
  ProofAVL \== t.


%! ordering:step(+H)
%
% H is a pass-1 plan step: it has a proof entry, either a regular rule
% or a cycle-break assumption (whose stored body still carries the
% dependency edges).

step(H) :-
  ordering:proof(Proof),
  ( get_assoc(rule(H), Proof, _) -> true
  ; get_assoc(assumed(rule(H)), Proof, _)
  ).


%! ordering:step_body(+H, -Body)
%
% The proof body of step H. Prefers the complete rule(H) entry over the
% assumed(rule(H)) cycle-break entry (whose body may be partial).

step_body(H, Body) :-
  ordering:proof(Proof),
  ( get_assoc(rule(H), Proof, dep(_, Body)?_ ) -> true
  ; get_assoc(assumed(rule(H)), Proof, dep(_, Body)?_ )
  ).


% -----------------------------------------------------------------------------
% Requirements and preferences: a reading of the proof bodies
% -----------------------------------------------------------------------------

%! ordering:requires(+H, -D)
%
% Hard requirement: D must be available before step H can be placed.
% Enumerated from H's pass-1 proof body — every body literal that is
% itself a step, except runtime dependency groups (grouped :run heads,
% RDEPEND provenance): a runtime dep does not gate the build, so it is a
% preference, never an obligation. Constraints and body literals without
% a proof entry carry no ordering information.

requires(H, D) :-
  ordering:step_body(H, Body),
  member(Literal, Body),
  prover:canon_literal(Literal, Core, _),
  \+ constraint:is_constraint(Core),
  Core \== H,
  \+ ordering:runtime_dep(Core),
  ordering:step(Core),
  D = Core.


%! ordering:prefers(+H, -D)
%
% Preference: H would like D placed earlier, without insisting. Five
% sources, all read from the pass-1 proof:
%
%   - runtime dependency groups (grouped :run heads) in H's body —
%     RDEPEND wants its providers early but cannot force them (Portage
%     relaxes exactly these edges inside cycles);
%   - `constraint(order_after(Anchor))` pseudo-constraints (the PDEPEND
%     ordering channel, Handbook chapter 12) — post-merge by nature,
%     best-effort by Portage semantics;
%   - `constraint(schedule_after(Anchor))` pseudo-constraints (plain
%     anchoring, portage-ng#89): the carrier alone goes after Anchor —
%     unlike order_after, NOT indexed as a PDEPEND completion group, so
%     the anchor's other consumers do not wait for the carrier (sub-slot
%     ABI rebuilds would otherwise serialize into one-per-wave chains);
%   - PDEPEND completion (portage-ng#18/#19): a step depending on a
%     provider P prefers P's PDEPEND targets placed earlier too — P is
%     only functionally complete once its post-install group is merged
%     (ruby gems wait for rubygems). Cycle safety (the clang/compiler-rt
%     case) is inherited from the projection's preference honoring: a
%     preference that would close a cycle is skipped;
%   - configure closure (portage-ng#21): the :install action of a package
%     prefers the runtime providers of its :run sibling placed earlier —
%     the package's configure phase already exercises them;
%   - assumed-dep alias (portage-ng#95): a dep that degraded to a domain
%     assumption (e.g. a REQUIRED_USE conflict on the provider) still
%     prefers the concretely planned action of the same package earlier,
%     when another proof path planned that package for the same phase.

prefers(H, D) :-
  ordering:step_body(H, Body),
  member(Literal, Body),
  prover:canon_literal(Literal, Core, _),
  ( Core = constraint(order_after(Anchor0):_) ->
      prover:canon_literal(Anchor0, D, _)
  ; Core = constraint(schedule_after(Anchor0):_) ->
      prover:canon_literal(Anchor0, D, _)
  ; \+ constraint:is_constraint(Core),
    ordering:runtime_dep(Core),
    D = Core
  ),
  D \== H,
  ordering:step(D).

% PDEPEND completion (portage-ng#18/#19).
prefers(H, T) :-
  ordering:step_body(H, Body),
  member(Literal, Body),
  prover:canon_literal(Literal, Core, _),
  ordering:dep_anchor_key(Core, Key),
  ordering:pdepend_anchor_index(Idx),
  get_assoc(Key, Idx, Targets),
  member(T, Targets),
  T \== H.

% Configure closure (portage-ng#21).
prefers(Repository://Entry:install, D) :-
  ordering:step(Repository://Entry:run),
  ordering:step_body(Repository://Entry:run, RunBody),
  member(Literal, RunBody),
  prover:canon_literal(Literal, Core, _),
  ordering:runtime_dep(Core),
  ordering:step(Core),
  D = Core.

% Assumed-dep alias (portage-ng#95).
prefers(H, D) :-
  ordering:step_body(H, Body),
  member(Literal, Body),
  prover:canon_literal(Literal, assumed(Inner), _),
  prover:canon_literal(Inner, G:Action, _),
  ordering:grouped_cn(G, C, N),
  ordering:phase_actions(Action, Actions),
  ordering:cn_action_index(Idx),
  member(A, Actions),
  get_assoc(cn(C-N)-A, Idx, Steps),
  member(D, Steps),
  D \== H.


%! ordering:runtime_dep(+Core)
%
% True for runtime dependency-group literals (RDEPEND provenance):
% ordered early when possible, never required.

runtime_dep(grouped_package_dependency(_, _, _, _):run).


% -----------------------------------------------------------------------------
% Per-pass indexes (PDEPEND anchors, concrete actions by package)
% -----------------------------------------------------------------------------
%
% Two lazily-built views over the published pass-1 proof, reset at the
% start of every ordering pass (orderer:with_ordering_pass calls
% ordering:prepare_pass/0); thread-local like the published proof itself.
%
%   - the PDEPEND anchor index maps a provider identity to the steps that
%     carry a `constraint(order_after(Provider))` marker — i.e. the
%     provider's PDEPEND group. Consumers of the provider look themselves
%     up here (via dep_anchor_key/2) to prefer the post-install group
%     earlier (portage-ng#18/#19);
%   - the concrete-action index maps cn(Category-Name)-Action to the
%     concrete plan steps for that package, so assumed grouped deps can
%     alias to the planned provider (portage-ng#95).

%! ordering:prepare_pass
%
% Reset the per-pass caches. Called by orderer:with_ordering_pass/2.

prepare_pass :-
  nb_setval(portage_ordering_pdepend_idx, none),
  nb_setval(portage_ordering_cnaction_idx, none).


%! ordering:pdepend_anchor_index(-Idx)
%
% The anchor index for the current pass, built on first use.

pdepend_anchor_index(Idx) :-
  ( nb_current(portage_ordering_pdepend_idx, Idx0), Idx0 \== none ->
      Idx = Idx0
  ; ordering:build_pdepend_anchor_index(Idx),
    nb_setval(portage_ordering_pdepend_idx, Idx)
  ).

build_pdepend_anchor_index(Idx) :-
  ordering:proof(Proof),
  findall(Key-T,
          ( assoc:gen_assoc(ProofKey, Proof, dep(_, Body)?_),
            ( ProofKey = rule(T) -> true ; ProofKey = assumed(rule(T)) ),
            member(Literal, Body),
            prover:canon_literal(Literal, constraint(order_after(Anchor0):_), _),
            prover:canon_literal(Anchor0, ACore, _),
            ordering:anchor_key(ACore, Key)
          ),
          Pairs0),
  sort(Pairs0, Pairs),
  empty_assoc(I0),
  foldl(ordering:anchor_index_put, Pairs, I0, Idx).

anchor_index_put(Key-T, In, Out) :-
  ( get_assoc(Key, In, Ts) -> true ; Ts = [] ),
  put_assoc(Key, In, [T|Ts], Out).


%! ordering:dep_anchor_key(+Core, -Key)
%
% The provider-identity key a dependency literal resolves to: concrete
% actions key on their entry, dependency groups on category-name.
% Blocker groups carry no provider identity.

dep_anchor_key(Repository://Entry:_, entry(Repository://Entry)) :- !.
dep_anchor_key(grouped_package_dependency(no, C, N, _):_, cn(C-N)) :- !.
dep_anchor_key(Core, entry(Core)) :-
  atom(Core).


%! ordering:anchor_key(+ACore, -Key)
%
% The keys an order_after anchor is indexed under: its entry, plus its
% category-name when the KB resolves it (so grouped consumer deps match).

anchor_key(Repository://Entry:_, Key) :-
  !,
  ( Key = entry(Repository://Entry)
  ; catch(query:search([category(C), name(N)], Repository://Entry), _, fail),
    Key = cn(C-N)
  ).
anchor_key(ACore, entry(ACore)) :-
  atom(ACore).


%! ordering:cn_action_index(-Idx)
%
% The concrete-action index for the current pass, built on first use:
% cn(Category-Name)-Action -> concrete plan steps of that package.

cn_action_index(Idx) :-
  ( nb_current(portage_ordering_cnaction_idx, Idx0), Idx0 \== none ->
      Idx = Idx0
  ; ordering:build_cn_action_index(Idx),
    nb_setval(portage_ordering_cnaction_idx, Idx)
  ).

build_cn_action_index(Idx) :-
  ordering:proof(Proof),
  findall((cn(C-N)-Action)-Head,
          ( assoc:gen_assoc(ProofKey, Proof, _),
            ( ProofKey = rule(Head) -> true ; ProofKey = assumed(rule(Head)) ),
            Head = Repository://Entry:Action,
            catch(query:search([category(C), name(N)], Repository://Entry), _, fail)
          ),
          Pairs0),
  sort(Pairs0, Pairs),
  empty_assoc(I0),
  foldl(ordering:anchor_index_put, Pairs, I0, Idx).


%! ordering:grouped_cn(+G, -C, -N)
%
% Category and name of a grouped dependency literal. Assumed grouped
% deps occur both with the 4-argument (strength-carrying) and the
% 3-argument (strength-stripped, phantom path) shape.

grouped_cn(grouped_package_dependency(_, C, N, _), C, N).
grouped_cn(grouped_package_dependency(C, N, _), C, N).


%! ordering:phase_actions(+DepAction, -PlanActions)
%
% The concrete plan actions that satisfy a dependency phase: an
% :install dep is satisfied by any merge-family action, a :run dep by
% the :run head.

phase_actions(install, [install, update, downgrade, reinstall]).
phase_actions(run, [run]).


% -----------------------------------------------------------------------------
% World: a reading of the VDB
% -----------------------------------------------------------------------------

%! ordering:world_override(+D)
%
% Test hook: unit tests assert world facts here to exercise the laws
% without a knowledge base or VDB (see Source/Test/unittest.pl).

:- dynamic world_override/1.


%! ordering:world(+H, +D)
%
% The installed system, as it stands, already provides requirement D of
% consumer H — the LFS argument: a fact about the present system, cited
% from the VDB. Three shapes:
%
%   - H is a dependency group and D its chosen provider (the cycle-bridge
%     case): the group's version and USE constraints — not the provider's
%     identity — are what H actually requires, so the world provides D
%     when an installed package satisfies the group;
%   - D is a dependency group: same test on D's own constraints;
%   - D is a concrete ebuild action: exactly that version is installed.
%
% Blocker groups (strength weak/strong) are never world-provided: their
% satisfaction is an absence, not a presence.

world(_H, D) :-
  ordering:world_override(D),
  !.

world(grouped_package_dependency(no, C, N, Deps):_, _Repository://_Entry:_Action) :-
  !,
  catch(ordering:installed_satisfies(C, N, Deps), _, fail).

world(_H, grouped_package_dependency(no, C, N, Deps):_) :-
  !,
  catch(ordering:installed_satisfies(C, N, Deps), _, fail).

world(_H, Repository://Entry:Action) :-
  memberchk(Action, [install, run]),
  !,
  catch(ordering:installed_same_version(Repository, Entry), _, fail).


%! ordering:installed_satisfies(+C, +N, +Deps)
%
% An installed package satisfies every package_dependency in Deps
% (version operators and USE requirements; delegates to the same
% predicate pass-1 candidate ranking trusts).

installed_satisfies(C, N, []) :-
  !,
  ranking:cn_is_installed(C, N).
installed_satisfies(_C, _N, Deps) :-
  forall(member(Dep, Deps),
         ( Dep = package_dependency(_, no, _, _, _, _, _, _),
           ranking:installed_pkg_satisfies_dep([], Dep)
         )).


%! ordering:installed_same_version(+Repository, +Entry)
%
% The exact version of Repository://Entry is already installed.

installed_same_version(Repository, Entry) :-
  cache:ordered_entry(Repository, Entry, C, N, Version),
  knowledgebase:vdb_repository(VdbRepo),
  query:search([name(N), category(C), installed(true)], VdbRepo://Installed),
  cache:ordered_entry(VdbRepo, Installed, _, _, Version),
  !.
