/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> STUBS
This file contains stub predicates for functions that are only available in
specific modes (e.g., client or server). This allows the application to be
compiled in standalone mode without generating warnings about undefined
predicates.
*/

% =============================================================================
%  STUBS declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Stubs for client calls.
% -----------------------------------------------------------------------------

:- dynamic client:rpc_execute/3.
:- dynamic client:rpc_execute/4.
:- dynamic client:execute_remotely/3.

:- if(\+ current_module(client)).

:- multifile client:rpc_execute/3.
:- multifile client:rpc_execute/4.
:- multifile client:execute_remotely/3.

client:rpc_execute(_,_,_) :-
    print_message(error,"Client module not loaded, cannot execute RPC call").

client:rpc_execute(_,_,_,_) :-
    print_message(error,"Client module not loaded, cannot execute RPC call").

client:execute_remotely(_,_,_) :-
    print_message(error,"Client module not loaded, cannot execute remotely").

:- endif.


% -----------------------------------------------------------------------------
%  Stubs for server calls.
% -----------------------------------------------------------------------------

% Note: git wrappers (git:head/2, git:checkout/2) live in the shared
% Source/Application/System/git.pl module, loaded in every mode, so they
% need no stub here.

:- dynamic server:start_server/0.
:- dynamic server:stop_server/0.

:- if(\+ current_module(server)).

:- multifile server:start_server/0.
:- multifile server:stop_server/0.

server:start_server :-
    print_message(error, "Server module not loaded, cannot start server").

server:stop_server :-
    print_message(error, "Server module not loaded, cannot stop server").

:- endif.


% -----------------------------------------------------------------------------
%  Stubs for semantic search calls.
% -----------------------------------------------------------------------------

:- dynamic semantic:search/3.
:- dynamic semantic:similar/4.
:- dynamic semantic:print_results/1.
:- dynamic semantic:build_index/0.

:- if(\+ current_module(semantic)).

:- multifile semantic:search/3.
:- multifile semantic:similar/4.
:- multifile semantic:print_results/1.
:- multifile semantic:build_index/0.

semantic:search(_, _, []) :-
    print_message(informational, "Semantic search module not loaded").

semantic:similar(_, _, _, []) :-
    print_message(informational, "Semantic search module not loaded").

semantic:print_results([]) :-
    print_message(informational, "Semantic search module not loaded").

semantic:build_index :-
    print_message(error, "Semantic search module not loaded, cannot build index").

:- endif.


% -----------------------------------------------------------------------------
%  Stubs for LLM knowledge pack (when LLM modules are not loaded).
% -----------------------------------------------------------------------------

:- dynamic llmknowledge:list_topics/0.
:- dynamic llmknowledge:print_topic/1.
:- dynamic llmknowledge:print_handbook/1.
:- dynamic llmknowledge:print_source/3.

:- if(\+ current_module(llmknowledge)).

:- multifile llmknowledge:list_topics/0.
:- multifile llmknowledge:print_topic/1.
:- multifile llmknowledge:print_handbook/1.
:- multifile llmknowledge:print_source/3.

llmknowledge:list_topics :-
    print_message(informational, "LLM knowledge module not loaded").

llmknowledge:print_topic(_) :-
    print_message(informational, "LLM knowledge module not loaded").

llmknowledge:print_handbook(_) :-
    print_message(informational, "LLM knowledge module not loaded").

llmknowledge:print_source(_, _, _) :-
    print_message(informational, "LLM knowledge module not loaded").

:- endif.


% -----------------------------------------------------------------------------
%  Stubs for metacircular LLM repair (when LLM modules are not loaded).
% -----------------------------------------------------------------------------

:- dynamic metacircular:diagnose_after_build/7.
:- dynamic metacircular:diagnose_cli/2.

:- if(\+ current_module(metacircular)).

:- multifile metacircular:diagnose_after_build/7.
:- multifile metacircular:diagnose_cli/2.

%! metacircular:diagnose_after_build(+Goals, +Proof, +Model, +Plan, +Triggers, +Fallback, -Applied) is det.
%
% Stub: no LLM modules — report zero applied actions so the builder
% replan gate (`Applied > 0`) stays closed.

metacircular:diagnose_after_build(_, _, _, _, _, _, 0).

%! metacircular:diagnose_cli(+Args, +Options) is det.
%
% Stub: warn that metacircular diagnose needs LLM modules.

metacircular:diagnose_cli(_, _) :-
    print_message(error, "Metacircular LLM module not loaded; cannot --diagnose").

:- endif.


% -----------------------------------------------------------------------------
%  Daemon bridge dynamics.
% -----------------------------------------------------------------------------

% These are populated per request on the daemon server side (daemon.pl) and
% read from interface.pl/config.pl in every mode. Declaring them here lets
% non-daemon modes (standalone, client, worker, server) resolve the reads
% without loading daemon.pl. daemon.pl declares the same dynamics itself.

:- dynamic daemon:client_env/2.
:- dynamic daemon:client_is_tty/0.
:- dynamic daemon:client_tty_size/2.
:- dynamic daemon:running/0.