/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2025, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> QUERY
An implementation of a query language for the knowledge base
*/

:- module(query,[]).


% ******************
% QUERY declarations
% ******************

% Query essentially queries the cache facts, which are maintained
% by the knowledge base.
%
% All access to cache should happen through this module, as queries
% are optimized for efficiency.
%
% We deal with queries from command line:
%
%    1. a list of qualified target searches
%       each qualified target search identifies a proposed knowledge
%       base entry that needs to be realised by the proof / build plan
%
%    2. a list of key=value search pairs, where = can be any of
%       <,>,<=,>=,!=,~,:=. The last two implement fuzzy search
%       and wildcard search respectively on the value provided.
%
% As well as other queries from ebuild, printer, grapher, builder
% through a flexible query language which includes:
%
%    - negation (filtering of results)
%    - all (collecting all results)
%    - model (backtracks over solutions for a given statement))
%
% We expect query to be called from the knowledge base, which may
% be instantiated as a local knowledge base (standalone, server mode)
% or a remote knowledge base (client or mixed mode)

% -------------
% Query: Search
% -------------


%! query:search(Query)
%
% Search - iterate over list
% Traverse a list of statements that narrow down the search results.

search([],_Repository://_Entry) :- !.

search([Statement|Rest],Repository://Entry) :-
  !,
  search(Statement,Repository://Entry),
  search(Rest,Repository://Entry).


% ----------------------
% Search meta predicates
% ----------------------

% Case : a not statement

search(not(Statement),Repository://Entry) :-
  !,
  cache:ordered_entry(Repository,Entry,_,_,_),
  \+(search(Statement,Repository://Entry)).


% Case : an all statement (single argument, contextualized)

search(all(Statement):Context,Repository://Entry) :-
  Statement =.. [Key,Values],
  !,
  findall(InnerValueA:Context,
          (InnerStatement =.. [Key,InnerValueA],
           search(InnerStatement,Repository://Entry)),
          Values).


% Case : an all statement (single argument, no context)

search(all(Statement),Repository://Entry) :-
  Statement =.. [Key,Values],
  !,
  findall(InnerValue,
          (InnerStatement =.. [Key,InnerValue],
           search(InnerStatement,Repository://Entry)),
          Values).


% Case : an all statement (dual argument, contextualized)

search(all(Statement):Context,Repository://Entry) :-
  Statement =.. [Key,Values,Filter],
  !,
  findall(InnerValueA:Context,
          (InnerStatement =.. [Key,InnerValueA,Filter],
           search(InnerStatement,Repository://Entry)),
          Values).


% Case : an all statement (dual argument, no context)

search(all(Statement),Repository://Entry) :-
  Statement =.. [Key,Values,Filter],
  !,
  findall(InnerValueA,
          (InnerStatement =.. [Key,InnerValueA,Filter],
           search(InnerStatement,Repository://Entry)),
          Values).


% Case : a model statement (dual argument, contextualized),

search(model(Statement):Context,Repository://Id) :-
  Statement =.. [Key,Model,Arg],
  !,
  StatementA =.. [Key,AllValues,Arg],
  search(all(StatementA),Repository://Id),
  prover:model(AllValues,ModelValues),
  findall(V:Context,
   (member(V,ModelValues),
    V =.. [package_dependency|_]),		% todo: make this a filter value, to ensure domain-independence
    %\+(V =.. [package_dependency|_]),
    %\+(V =.. [use_conditional_group|_]),
    %\+(V =.. [exactly_one_of_group|_]),
    %\+(V =.. [any_of_group|_]),
    %\+(V =.. [all_of_group|_])),
   Model).

% Case : a model statement (dual argument, no context),

search(model(Statement),Repository://Id) :-
  Statement =.. [Key,Model,Arg],
  !,
  StatementA =.. [Key,AllValues,Arg],
  search(all(StatementA),Repository://Id),
  prover:model(AllValues,ModelValues),
  findall(V,
   (member(V,ModelValues),
    V =.. [package_dependency|_]),		% todo: make this a filter value, to ensure domain-independence
    %\+(V =.. [package_dependency|_]),
    %\+(V =.. [use_conditional_group|_]),
    %\+(V =.. [exactly_one_of_group|_]),
    %\+(V =.. [any_of_group|_]),
    %\+(V =.. [all_of_group|_])),
   Model).


% Case : a model statement (single argument, contextualized)

search(model(Statement):Context,Repository://Id) :-
  Statement =.. [Key,Model],
  !,
  StatementA =.. [Key,AllValues],
  search(all(StatementA),Repository://Id),
  prover:model(AllValues,ModelValues),
  findall(V:Context,
   (member(V,ModelValues),
    V =.. [package_dependency|_]),		% todo: make this a filter value, to ensure domain-independence
    %\+(V =.. [package_dependency|_]),
    %\+(V =.. [use_conditional_group|_]),
    %\+(V =.. [exactly_one_of_group|_]),
    %\+(V =.. [any_of_group|_]),
    %\+(V =.. [all_of_group|_])),
   Model).

% Case : a model statement (single argument, no context)

search(model(Statement),Repository://Id) :-
  Statement =.. [Key,Model],
  !,
  StatementA =.. [Key,AllValues],
  search(all(StatementA),Repository://Id),
  prover:model(AllValues,ModelValues),
  findall(V,
   (member(V,ModelValues), 			% todo: make this a filter value, to ensure domain-independence
    \+(V =.. [package_dependency|_]),
    \+(V =.. [use_conditional_group|_]),
    \+(V =.. [exactly_one_of_group|_]),
    \+(V =.. [any_of_group|_]),
    \+(V =.. [all_of_group|_])),
   Model).


% Case : a latest statement, returs only latest version

search(latest(Statement),R://I) :-
  search(Statement,R://I),!.


% ------------------------
% Search domain predicates
% ------------------------

% Case : world

%search(world,R://I) :-
%  preference:world(World),
%  member(R://I:_,World).


% Case : set

%search(set(Name),R://I) :-
%  preference:set(Name,Set),
%  writeln('set found'),
%  member(Ta,Set),
%  atom_codes(Ta,Tc),
%  phrase(eapi:qualified_target(Q),Tc),
%  search(Q,R://I).


search(dependency(D,run_compile),R://I) :-
  !,
  ( cache:entry_metadata(R,I,bdepend,D)
  ; cache:entry_metadata(R,I,cdepend,D)
  ; cache:entry_metadata(R,I,depend,D)
  ; cache:entry_metadata(R,I,idepend,D)
  ; cache:entry_metadata(R,I,rdepend,D) ).

search(dependency(D,compile),R://I) :-
  !,
  ( cache:entry_metadata(R,I,bdepend,D)
  ; cache:entry_metadata(R,I,cdepend,D)
  ; cache:entry_metadata(R,I,depend,D) ).

search(dependency(D,run),R://I) :-
  !,
  ( cache:entry_metadata(R,I,idepend,D)
  ; cache:entry_metadata(R,I,rdepend,D) ).


% ------------------------------------
% Search: command line key=value pairs
% ------------------------------------

search(select(Key,Comparator,Value),R://I) :-
  select(Key,Comparator,Value,R://I).


% ------------------------
% Search: Qualified target
% ------------------------

% A Qualified target is defined in the EAPI spec as:
%
%  <operator><repository>://<category/<package>-<version>:<slot>[<usedeps>]
%
% or
%
%  <operator><category><package>-<version>:<slot>[<usedeps>]
%
% or
%
%  <operator><package>-<version>:<slot>[<usedeps>]
%
% where <operator>, <version>, <slot> and <usedeps are optional.
%
% The provided EAPI parser converts this to a
%
%  qualified_target(operator,repository,category,package,version,filters)
%
% where filters is a list of slot and usedep constraints.
% where operator is one of:
%
%    - greaterequal
%    - greater
%    - smallerequal
%    - smaller
%    - tilde
%    - equal
%    - notequal
%    - none
%
% In case repository is not provided, it is supplied as an unbound variable
% In case category is not provided, it is supplied as an unbound variable
%
% Version is defined in EAPI spec and is essentially a list of:
%
%    - Numberpart, (an atom. e.g. 6.5.4)
%    - Alphapart, (an atom e.g. dev)
%    - Suffixpart, (an atom. e.g. _alpha, _beta, _pre, _rc, -r)
%    - Fullversion, (an atom combining number, alpha and suffix. E.g. 6.5.4dev-r1
%
% In case version is not provided, it is supplied as a list of empty atoms i.e. ['','','','']
%
% Filter is a list that can be empty. It contains lists as elements, each with filters.
% e.g. [[slot(4)],[usedep([positive(useflag),negative(otheruseflag)])]]


% Instead of using a generator, generating a lot of backtrack points, and filtering
% out options, we try to maximize usage of the JIT indexing of prolog.
%
% Essentially, repository, category, package name and even version can be hashed,
% permitting hash indexed-lookup or cache predicates, limiting the choicepoints.
%
% This search based on qualified_target makes lookup initial lookup very fast. We
% apply filtering on the remaining choicepoints.


% Case 1: No operator, no version

search(qualified_target(none,R,C,P,[[],'','','',''],F),R://I) :-
   !,
   cache:ordered_entry(R,I,C,P,_),
   apply_filters(R://I,F).

% Case 2: No operator, version

search(qualified_target(none,R,C,P,V,F),R://I) :-
   !,
   cache:ordered_entry(R,I,C,P,V),
   apply_filters(R://I,F).

% Case 3: Operator, version

search(qualified_target(O,R,C,P,V,F),R://I) :-
   !,
   cache:ordered_entry(R,I,C,P,PV),
   apply_version_filter(O,PV,V),
   apply_filters(R://I,F).


% --------------
% Search: Ebuild
% --------------

search(ebuild(Ebuild),R://Ebuild) :-
  !,
  cache:ordered_entry(R,Ebuild,_,_,_).


% ----------------
% Search: Manifest
% ----------------

search(manifest(Scope,Type,Binary,Size),R://I) :-
   !,
   cache:ordered_entry(R,I,Category,Name,_),
   search(all(src_uri(Model)),R://I),
   model_member(Scope,uri(_,_,Binary),Model),
   cache:manifest(R,P,_,Category,Name),
   cache:manifest_metadata(R,P,Type,Binary,Size,_Checksums).


% ------------
% Search: iuse
% ------------

search(iuse(Iuse),R://I) :-
  !,
  cache:entry_metadata(R,I,iuse,Value),
  eapi:strip_use_default(Value,Iuse).


% --------------------------------
% Search: iuse with use flag state
% --------------------------------

search(iuse(Iuse,State:Reason),R://I) :-
  !,
  cache:entry_metadata(R,I,iuse,Value),
  eapi:categorize_use(Value,State,Reason),
  eapi:strip_use_default(Value,Iuse).


% -------------------------------
% Search: iuse without use_expand
% -------------------------------

search(iuse_filtered(Iuse),R://I) :-
  !,
  cache:entry_metadata(R,I,iuse,Arg),
  eapi:strip_use_default(Arg,Iuse),
  \+(eapi:check_use_expand_atom(Iuse)).


% ----------------------------------------------------
% Search: iuse without use_expand, with use flag state
% ----------------------------------------------------

search(iuse_filtered(Iuse,State:Reason),R://I) :-
  !,
  cache:entry_metadata(R,I,iuse,Arg),
  eapi:categorize_use(Arg,State,Reason),
  eapi:strip_use_default(Arg,Iuse),
  \+(eapi:check_use_expand_atom(Iuse)).


% ------------------
% Search: use expand
% ------------------

search(Statement,R://I) :-
  Statement =.. [Key,Value],
  eapi:use_expand(Key),!,
  cache:entry_metadata(R,I,iuse,Arg),
  eapi:strip_use_default(Arg,ArgB),
  eapi:check_prefix_atom(Key,ArgB),
  eapi:strip_prefix_atom(Key,ArgB,Value).


% --------------------------------------
% Search: use expand with use flag state
% --------------------------------------

search(Statement,R://I) :-
  Statement =.. [Key,Value,State:Reason],
  eapi:use_expand(Key),!,
  cache:entry_metadata(R,I,iuse,Arg),
  eapi:categorize_use(Arg,State,Reason),
  eapi:strip_use_default(Arg,ArgB),
  eapi:check_prefix_atom(Key,ArgB),
  eapi:strip_prefix_atom(Key,ArgB,Value).


% ----------------
% Search: Metadata
% ----------------

% metadata can be anything, so this needs to be at the bottom

search(Q,R://I) :-
  !,
  Q =.. [Key,Value],
  select(Key,equal,Value,R://I).
  %cache:entry_metadata(R,I,Key,Value).


% -------------------------
% Search: Filter predicates
% -------------------------

% Filter out versions based on comparison

apply_version_filter(greater,ProposedVersion,Version) :-
  !,
  system:compare(>,ProposedVersion,Version).

apply_version_filter(greaterequal,ProposedVersion,Version) :-
  system:compare(>,ProposedVersion,Version),!.

apply_version_filter(greaterequal,ProposedVersion,Version) :-
  !,
  system:compare(=,ProposedVersion,Version).

apply_version_filter(smaller,ProposedVersion,Version) :-
  !,
  system:compare(<,ProposedVersion,Version).

apply_version_filter(smallerequal,ProposedVersion,Version) :-
  system:compare(<,ProposedVersion,Version),!.

apply_version_filter(smallerequal,ProposedVersion,Version) :-
  !,
  symtem:compare(=,ProposedVersion,Version).

apply_version_filter(notequal,VersionA,VersionB) :-
  VersionA == VersionB -> fail;true.

apply_version_filter(equal,Version,Version) :-
  !.

apply_version_filter(tilde,[Version,_,_,_,_],[Version,_,_,_,_]) :-
  !.


% Filtering of slot & usedep for qualified_target

apply_filters(_R://_I,[]) :- !.

apply_filters(R://I,[H|T]) :-
  !,
  apply_filter(R://I,H),
  apply_filters(R://I,T).

apply_filter(_R://_I,[]) :- !.





% ------------------------------------------
% Searching via command line key=value pairs
% ------------------------------------------

% ------------------
% Entry - repository
% ------------------

select(repository,notequal,R,O://I) :-
  !,
  cache:ordered_entry(O,I,_,_,_),
  \+ R=O.

select(repository,equal,R,R://I) :-
  !,
  cache:ordered_entry(R,I,_,_,_).

select(repository,tilde,R,M://I) :-
  !,
  cache:ordered_entry(M,I,_,_,_),
  dwim_match(R,M).

select(repository,wildcard,R,M://I) :-
  !,
  cache:ordered_entry(M,I,_,_,_),
  wildcard_match(R,M).


% ----------------
% Entry - category
% ----------------

select(category,notequal,C,R://I) :-
  !,
  cache:ordered_entry(R,I,O,_,_),
  \+ C=O.

select(category,equal,C,R://I) :-
  !,
  cache:ordered_entry(R,I,C,_,_).

select(category,tilde,C,R://I) :-
  !,
  cache:ordered_entry(R,I,M,_,_),
  dwim_match(C,M).

select(category,wildcard,C,R://I) :-
  !,
  cache:ordered_entry(R,I,M,_,_),
  wildcard_match(C,M).


% ------------
% Entry - name
% ------------

select(name,notequal,N,R://I) :-
  !,
  cache:ordered_entry(R,I,_,O,_),
  \+ N=O.

select(name,equal,N,R://I) :-
  !,
  cache:ordered_entry(R,I,_,N,_).

select(name,tilde,N,R://I) :-
  !,
  cache:ordered_entry(R,I,_,M,_),
  dwim_match(N,M).

select(name,wildcard,N,R://I) :-
  !,
  cache:ordered_entry(R,I,_,M,_),
  wildcard_match(N,M).


% ---------------
% Entry - version
% ---------------

select(version,none,_,R://I) :-
  !,
  cache:ordered_entry(R,I,_,_,_).

select(version,wildcard,[_,_,_,V],R://I) :-
  !,
  cache:ordered_entry(R,I,_,_,[_,_,_,ProposedVersion]),
  wildcard_match(V,ProposedVersion).

select(version,tilde,[Version,_,_,_],R://I) :-
  !,
  cache:ordered_entry(R,I,_,_,[Version,_,_,_]).

select(version,Comparator,RequestedVersion,R://I) :-
  !,
  cache:ordered_entry(R,I,_,_,ProposedVersion),
  apply_version_filter(Comparator,ProposedVersion,RequestedVersion).


% -----------------------------
% Special case - set membership
% -----------------------------

select(set,notequal,S,R://I) :-
  !,
  preference:set(S,Set),
  findall(Rc://Ic,(member(Ta,Set),
                   atom_codes(Ta,Tc),
                   phrase(eapi:qualified_target(Q),Tc),
                   search(Q,Rc://Ic)),
          Candidates),
  cache:ordered_entry(R,I,_,_,_),
  \+(memberchk(R://I,Candidates)).

select(set,equal,S,R://I) :-
  !,
  preference:set(S,Set),
  member(Ta,Set),
  atom_codes(Ta,Tc),
  phrase(eapi:qualified_target(Q),Tc),
  search(Q,R://I).

select(set,tilde,N,R://I) :-
  !,
  preference:set(S,Set),
  dwim_match(N,S),
  member(Ta,Set),
  atom_codes(Ta,Tc),
  phrase(eapi:qualified_target(Q),Tc),
  search(Q,R://I).

select(set,wildcard,N,R://I) :-
  !,
  preference:set(S,Set),
  wildcard_match(N,S),
  member(Ta,Set),
  atom_codes(Ta,Tc),
  phrase(eapi:qualified_target(Q),Tc),
  search(Q,R://I).


% ---------------------------
% Special case - eapi version
% ---------------------------

select(eapi,notequal,[_,_,_,Version],R://I) :-
  !,
  \+cache:entry_metadata(R,I,eapi,[_,_,_,Version]).

select(eapi,equal,[_,_,_,Version],R://I) :-
  !,
  cache:entry_metadata(R,I,eapi,[_,_,_,Version]).

select(eapi,wildcard,[_,_,_,V],R://I) :-
  !,
  cache:entry_metadata(R,I,eapi,[_,_,_,ProposedVersion]),
  wildcard_match(V,ProposedVersion).

select(eapi,Comparator,RequestedVersion,R://I) :-
  !,
  cache:entry_metadata(R,I,eapi,ProposedVersion),
  apply_version_filter(Comparator,ProposedVersion,RequestedVersion).


% ---------------------
% Special case - eclass
% ---------------------

select(eclass,Operator,Eclass,R://I) :-
  !,
  select(eclasses,Operator,Eclass,R://I).


% -----------------------
% Special case - eclasses
% -----------------------

select(eclasses,notequal,Eclass,R://I) :-
  !,
  \+cache:entry_metadata(R,I,eclasses,[eclass(Eclass),_]).

select(eclasses,equal,Eclass,R://I) :-
  !,
  cache:entry_metadata(R,I,eclasses,[eclass(Eclass),_]).

select(eclasses,tilde,Eclass,R://I) :-
  !,
  cache:entry_metadata(R,I,eclasses,[eclass(Match),_]),
  dwim_match(Eclass,Match).

select(eclasses,wildcard,Eclass,R://I) :-
  !,
  cache:entry_metadata(R,I,eclasses,Match),
  wildcard_match(Eclass,Match).


% ----------------------------------------
% Special case - Entry metadata - download
% ----------------------------------------

select(download,notequal,Filename,R://I) :-
  !,
  \+cache:entry_metadata(R,I,src_uri,uri(_,_,Filename)).

select(download,equal,Filename,R://I) :-
  !,
  cache:entry_metadata(R,I,src_uri,uri(_,_,Filename)).

select(download,tilde,Filename,R://I) :-
  !,
  cache:entry_metadata(R,I,src_uri,uri(_,_,Match)),
  dwim_match(Filename,Match).

select(download,wildcard,Filename,R://I) :-
  !,
  cache:entry_metadata(R,I,src_uri,uri(_,_,Match)),
  wildcard_match(Filename,Match).


% ------------------------------------
% Special case - Entry Metadata - slot
% ------------------------------------

select(slot,notequal,Slot,R://I) :-
  !,
  \+cache:entry_metadata(R,I,slot,slot(Slot)).

select(slot,equal,Slot,R://I) :-
  !,
  cache:entry_metadata(R,I,slot,slot(Slot)).

select(slot,tilde,Slot,R://I) :-
  !,
  cache:entry_metadata(R,I,slot,slot(Match)),
  dwim_match(Slot,Match).

select(slot,wildcard,Slot,R://I) :-
  !,
  cache:entry_metadata(R,I,slot,slot(Match)),
  wildcard_match(Slot,Match).


% ---------------------------------------
% Special case - Entry Metadata - subslot
% ---------------------------------------

select(subslot,notequal,Slot,R://I) :-
  !,
  \+cache:entry_metadata(R,I,slot,slot(Slot)).

select(subslot,equal,Slot,R://I) :-
  !,
  cache:entry_metadata(R,I,slot,slot(Slot)).

select(subslot,tilde,Slot,R://I) :-
  !,
  cache:entry_metadata(R,I,slot,slot(Match)),
  dwim_match(Slot,Match).

select(subslot,wildcard,Slot,R://I) :-
  !,
  cache:entry_metadata(R,I,slot,slot(Match)),
  wildcard_match(Slot,Match).


% ---------------------------------------
% Special case - Entry Metadata - keyword
% ---------------------------------------

select(keyword,Operator,Key,R://I) :-
  !,
  select(keywords,Operator,Key,R://I).


% ------------------------
% Default - Entry Metadata
% ------------------------

select(Key,notequal,Value,R://I) :-
  !,
  \+cache:entry_metadata(R,I,Key,Value).

select(Key,equal,Value,R://I) :-
  !,
  cache:entry_metadata(R,I,Key,Value).

select(Key,tilde,Value,R://I) :-
  !,
  cache:entry_metadata(R,I,Key,Match),
  dwim_match(Value,Match).

select(Key,wildcard,Value,R://I) :-
  !,
  cache:entry_metadata(R,I,Key,Match),
  wildcard_match(Value,Match).


%! model_member(Type,Predicate,Model)
%
% Recursively searches model for a predicate, taking into account
% use_conditional_group

model_member(all,Predicate,Model) :-
  member(Predicate,Model);
  (member(use_conditional_group(_,_,_,Conditional),Model),
   model_member(all,Predicate,Conditional)).

model_member(preference,Predicate,Model) :-
  member(Predicate,Model);
  (member(use_conditional_group(Sign,Use,_,Conditional),Model),
   (Sign == positive -> preference:use(Use) ; preference:use(minus(Use))),
     model_member(preference,Predicate,Conditional)).


