/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> KNOWLEDGE BASE
The Knowledge Base is a class that enables different repositories to register,
serialize their rules and facts to disk. This is used as a mechanism to maintain
state across application relaunches.

The knowledge base can be instantiated as a local proxy for a remotely running
knowledge base, using pengine rpc and 2-way SSL (https) communication.

The knowledge base query mechanism for local cache  is implemented by in the
Query module. The knowledge base is typically initialized as a singleton instance.
*/

:- module(knowledgebase,[]).

% =============================================================================
%  KNOWLEDGEBASE declarations
% =============================================================================

:- class.

% public interface

:- dpublic('knowledgebase'/2).
:- dpublic('knowledgebase'/0).
:- dpublic('~knowledgebase'/0).

:- dpublic(register/1).
:- dpublic(deregister/1).
:- dpublic(sync/0).
:- dpublic(sync/1).
:- dpublic(save/0).
:- dpublic(load/0).
:- dpublic(clear/0).
:- dpublic(compile/0).
:- dpublic(graph/0).
:- dpublic(graph_emerge/0).

:- dpublic(entry/1).
:- dpublic(query/2).

% protected interface

:- dprotected(proxy/0).
:- dprotected(rpc_wrapper/1).

% private interface

:- dprivate(repository/1).
:- dprivate(host/1).
:- dprivate(port/1).


%! Constructor(Host,Port)
%
% Public predicate
%
% Creates a local proxy for a remotely running
% knowledge base with a given Name.
% Running at Server:Port

'knowledgebase'(Host,Port) ::-
  <=host(Host),
  <=port(Port).


%! Constructor
%
% Public predicate
%
% Creates a local knowledge base.

'knowledgebase' ::-
  true.


%! Destructor
%
% Public predicate
%
% Destructs knowledge base.

'~knowledgebase' ::-
  true.


%! knowledgebase:register(+Repository)
%
% Public predicate
%
% Register a local repository with the knowledge base

register(Repository) ::-
  \+ proxy,
  <+repository(Repository),!.


%! knowledgebase:deregister(+Repository)
%
% Public predicate
%
% Deregister a local repository with the knowledge base

deregister(Repository) ::-
  \+ proxy,
  <-repository(Repository),!.


%! knowledgebase:sync
%
% Public predicate
%
% Sync all registered repositories

sync ::-
  proxy,!,
  ::host(Host),
  ::port(Port),
  client:execute_remotely(Host,Port,'/sync'),!.

sync ::-
  \+ proxy,!,
  with_mutex(sync,
  (aggregate_all(count, ::repository(_), Count),
   (Count == 1 ->
    message:topheader(['Syncing ',Count,' registered repository']);
    message:topheader(['Syncing ',Count,' registered repositories'])),
   forall(::repository(Repository),
 	 (message:header(['Syncing repository \"',Repository,'\"']),nl,
         Repository:sync)),!,
   true)).


%! knowledgebase:sync(+Name)
%
% Public predicate
%
% Sync a single registered repository by name (e.g. portage, pkg, overlay).

sync(Name) ::-
  proxy,!,
  ( sanitize:safe_path_component(Name) -> true
  ; throw(error(permission_error(sync, repository, Name),
                context(knowledgebase:sync/1, 'Invalid repository name')))
  ),
  ::host(Host),
  ::port(Port),
  atom_concat('/sync/', Name, Path),
  client:execute_remotely(Host,Port,Path),!.

sync(Name) ::-
  \+ proxy,!,
  ( ::repository(Name) ->
      with_mutex(sync,
        (message:topheader(['Syncing repository \"',Name,'\"']),
         message:header(['Syncing repository \"',Name,'\"']),nl,
         Name:sync,!,
         true))
  ; message:failure(['Unknown repository: ', Name, '. Registered: ']),
    forall(::repository(R),
           (message:print('  '), message:print(R), nl)),
    fail
  ).


%! knowledgebase:save
%
% Public predicate
%
% Save state to file

save ::-
  proxy,!,
  ::host(Host),
  ::port(Port),
  client:execute_remotely(Host,Port,'/save'),!.

save ::-
  \+ proxy,!,
  config:working_dir(Dir),
  directory_file_path(Dir,'Knowledge/kb.raw',Raw),
  lock:with_system_lock(kb_save(Dir),
    with_mutex(save,
      (setup_call_cleanup(
         open(Raw,write,Stream),
         (format(Stream,':- module(cache,[]).\n',[]),
          forall(current_predicate(cache:N/A),
                 (functor(H,N,A),
                  format(Stream,':- dynamic ~w/~w.\n',[N,A]),
                  forall(clause(cache:H,_),
                        ( ( ground(H)
                            -> true
                            ;  throw(error(instantiation_error,
                                           context(knowledgebase:save/0,
                                                   nonground_cache_fact(H)))) ),
                          write_canonical(Stream,H),
                          format(Stream,'.\n',[])))))),
         close(Stream)),
       qcompile(Raw)))),!.


%! knowledgebase:load
%
% Public predicate
%
% Load state from file

load ::-
  proxy,!,
  ::host(Host),
  ::port(Port),
  client:execute_remotely(Host,Port,'/load'),!.

load ::-
  \+ proxy,
  config:working_dir(Dir),
  directory_file_path(Dir,'Knowledge/kb.qlf',Qlf),
  exists_file(Qlf),!,
  ensure_loaded(Qlf),
  % Warm the JIT indexes off the critical path: index building is
  % mutex-protected per predicate, so an early query at worst blocks on
  % the build already in progress (same cost as warming synchronously,
  % but overlapped with the remainder of startup).
  thread_create(ignore(catch(knowledgebase:kb_warm_metadata_index, _, true)),
                _, [detached(true)]).

load ::-
  \+ proxy,
  true.


%! knowledgebase:clear
%
% Public predicate
%
% Clear state file

clear ::-
  proxy,!,
  ::host(Host),
  ::port(Port),
  client:execute_remotely(Host,Port,'/clear'),!.

clear ::-
  \+ proxy,
  exists_file('Knowledge/kb.qlf'),
  delete_file('Knowledge/kb.qlf'),
  fail.

clear ::-
  \+ proxy,
  exists_file('Knowledge/kb.raw'),!,
  delete_file('Knowledge/kb.raw').

clear ::-
  \+ proxy,
  true.


%! knowledgebase:graph
%
% Public predicate
%
% Create svg dependency graphs for all entries

graph ::-
  proxy,!,
  ::host(Host),
  ::port(Port),
  client:execute_remotely(Host,Port,'/graph'),!.

graph ::-
  \+ proxy,!,
  with_mutex(graph, % todo: this should iterate over all registered repositories
    (portage:graph)).


%! knowledgebase:graph_emerge
%
% Public predicate
%
% Generate .emerge files (via emerge-vp) for all entries in the
% registered repositories. Mirrors knowledgebase:graph/0 but writes
% only the .emerge artefacts.

graph_emerge ::-
  proxy,!,
  ::host(Host),
  ::port(Port),
  client:execute_remotely(Host,Port,'/graph_emerge'),!.

graph_emerge ::-
  \+ proxy,!,
  with_mutex(graph, % todo: this should iterate over all registered repositories
    (portage:graph_emerge)).


%! knowledgebase:compile
%
% Public predicate
%
% Save state to stand-alone program

compile ::-
  \+ proxy,
  with_mutex(compile,
   qsave_program('portage-ng',[stand_alone(true),goal(prolog)])).


%! knowledgebase:entry(?Repository://?Entry)
%
% Public predicate
%
% Knowledgebase entries

entry(Repository://Entry) ::-
  rpc_wrapper(cache:ordered_entry(Repository,Entry,_,_,_)).


%! knowledgebase:query(+Query,-Result)
%
% Public predicate
%
% Retrieves metadata cache ebuild that satisfies
% a given query
%
% kb:query/2 is the client-safe entry point: when this knowledge base
% instance is a proxy (client mode), the query is shipped to the remote
% server via pengine RPC. Use it only in code that may execute on a
% proxy client — i.e. the interface layer that resolves user targets
% before shipping work to the server (target.pl, Action/*.pl,
% Printer/info.pl). Code that only ever runs where the KB is local
% (prover, planner, printer plan rendering, builder) should call
% query:search/2 directly so the compile-time goal-expansion macros
% inline the cache lookups instead of paying instance dispatch +
% runtime expansion + meta-call on every query (issue #57).

query(Query,Repository://Result) ::-
  rpc_wrapper(query:search(Query,Repository://Result)).


%! knowledgebase:proxy
%
% Protected predicate
%
% Checks whether the repository is a proxy to a knowledge base
% running on a remote server

proxy ::-
  ::host(_).


%! knowledgebase:rpc_wrapper(Module:Term)
%
% Protected predicate
%
% Wrap predicates into a remote procedure call if host,
% port and proxy are set. Module is the target module atom
% (e.g. cache, query). In the local case the query macro
% (goal_expansion) is applied when one exists; otherwise the
% goal is executed directly.

rpc_wrapper(Module:Term) ::-
  ::host(Host),!,
  ::port(Port),
  client:rpc_execute(Host,Port,Module:Term).

rpc_wrapper(Module:Term) ::-
  \+ proxy,!,
  ( goal_expansion(Term,Expanded)
    -> Module:Expanded
    ;  Module:Term ).


%! knowledgebase:repository(?Repository)
%
% Private predicate
%
% Registered repositories

repository(_Repository) ::-
  true.


%! knowledgebase:host(+Host)
%
% Private predicate
%
% Hostname of remote knowledge base

host(Host) ::-
  atom(Host).


%! knowledgebase:port(+Port)
%
% Private predicate
%
% Port at which remote knowledge base is running

port(Port) ::-
  integer(Port).


%! knowledgebase:kb_warm_metadata_index is det.
%
% Prime the JIT indexes used by preference:init and the resolver after
% kb.qlf load.  SWI-Prolog builds a JIT index *completely* on the first
% call that needs it, so one bound probe per access pattern suffices:
%
%   1. cache:ordered_entry/5 with all args unbound (binds a witness entry)
%   2. cache:entry_metadata/4 with Repo+Id+key bound — forces the
%      (Id, Key) hash index over all metadata facts
%   3. cache:ordered_entry/5 with category/name bound — forces the
%      (C, N) index used by per-package iteration
%
% The probes are for their index-building side effect only; the soft-fail
% wrappers keep this predicate from failing when the witness entry has no
% slot metadata.

kb_warm_metadata_index :-
  once(cache:ordered_entry(Repo, Id, C, N, _)),
  ( cache:entry_metadata(Repo, Id, slot, slot(_)) -> true ; true ),
  ( cache:ordered_entry(Repo, _, C, N, _) -> true ; true ).