/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> REQUESTS
Main request dispatch: maps recognised CLI flags onto the corresponding action
predicates (included into the INTERFACE module via interface.pl).
*/

% -----------------------------------------------------------------------------
%  Request processing
% -----------------------------------------------------------------------------

%! interface:process_requests(+Mode) is det.
%
% Main dispatch. Processes the parsed command-line options and maps each
% recognised flag (--sync, --graph, --merge, etc.) onto the corresponding
% action predicate. Falls through to halt(1) if no action matches.

interface:process_requests(server) :-
  !, prolog.

interface:process_requests(daemon) :-
  !, prolog.

interface:process_requests(worker) :-
  !, prolog.

interface:process_requests(Mode) :-
  interface:version(Version),

  interface:process_flags,
  interface:process_continue(Continue),
  interface:argv(Options,Args),

  message:log(['Args:      ',Args]),
  message:log(['Options:   ',Options]),

  set_prolog_flag(toplevel_prompt,'~m~d~l?- '),

  ( memberchk(snapshots(true),Options) -> (snapshot:list,                                           Continue) ;
    memberchk(rollback(RollbackId),Options), RollbackId \== none
                                       -> (action:process_rollback(RollbackId, Options),             Continue) ;
    memberchk(version(true),Options)  -> (message:logo(['::- portage-ng ',Version]),
                                         interface:print_version_repos,             Continue) ;
    memberchk(info(true),Options)     -> (action:process_action(info,Args,Options),                 Continue) ;
    memberchk(bugs(true),Options)     -> (action:process_bugs(Args,Options),                        Continue) ;
    memberchk(clear(true),Options)    -> (kb:clear,                                                 Continue) ;
    memberchk(graph(true),Options)    -> (action:process_graph(Args), nl,                           Continue) ;
    memberchk(deselect(true),Options) -> (action:process_deselect(Args),                            Continue) ;
    memberchk(unmerge(true),Options)  -> (action:process_action(uninstall,Args,Options),             Continue) ;
    memberchk(depclean(true),Options) -> (action:process_action(depclean,Args,Options),              Continue) ;
    memberchk(upgrade(true),Options)  -> (action:process_upgrade(Args,Options),                      Continue) ;
    % For a single target, Portage-style update behaves like a normal merge:
    % resolve full runtime closure and perform a transactional replace if needed.
    % In portage-ng the "full closure" corresponds to proving :run.
    memberchk(update(true),Options)   -> (action:process_action(run,Args,Options),                   Continue) ;
    memberchk(search(true),Options)   -> (action:process_action(search,Args,Options),                Continue) ;
    memberchk(listsets(true),Options) -> (action:process_list_sets,                                  Continue) ;
    memberchk(checknews(true),Options) -> (news:check,                                               Continue) ;
    memberchk(readnews(true),Options) -> (news:check,                                                Continue) ;
    memberchk(regen(true),Options)   -> (action:process_regen(Mode, Args),!,                         Continue) ;
    memberchk(metadata(true),Options) -> (action:process_regen(Mode, Args),!,                        Continue) ;
    memberchk(sync(true),Options)     -> (action:process_sync(Mode, Args),!,                         Continue) ;
    memberchk(save(true),Options)     -> (kb:save,!,                                                 Continue) ;
    memberchk(load(true),Options)     -> (kb:load,!,                                                 Continue) ;
    memberchk(fetchonly(true),Options) -> (action:process_action(fetchonly,Args,Options),            Continue) ;
    memberchk(resume(true),Options)  -> (action:assert_resume_skip_args(Args),
                                         builder:build_resume,
                                         action:maybe_ci_exit_on_build_failure(Options),           Continue) ;
    memberchk(build(true),Options)   -> (action:process_build(Args,Options),                         Continue) ;
    memberchk(contents(true),Options) -> (action:process_vdb_query(contents,Args),                   Continue) ;
    memberchk(owner(true),Options)   -> (action:process_vdb_query(owner,Args),                      Continue) ;
    memberchk(pkgsize(true),Options) -> (action:process_vdb_query(size,Args),                       Continue) ;
    memberchk(verify(true),Options)  -> (action:process_vdb_query(verify,Args),                     Continue) ;
    memberchk(executables(true),Options) -> (action:process_vdb_query(executables,Args),            Continue) ;
    memberchk(fixlinkage(true),Options) -> (action:process_fix_linkage(Args,Options),               Continue) ;
    memberchk(report(true),Options)  -> (action:process_report(Options),                            Continue) ;
    memberchk(rdeps(true),Options)   -> (action:process_rdeps(Args),                                Continue) ;
    memberchk(unuseddistfiles(true),Options) -> (action:process_unused_distfiles(Options),          Continue) ;
    memberchk(import(true),Options)  -> (action:process_import(Args,Options),                        Continue) ;
    memberchk(unmanagedfiles(true),Options) -> (action:process_unmanaged_files(Args),                Continue) ;
    memberchk(upstream(true),Options) -> (action:process_upstream(Args,Options),                     Continue) ;
    memberchk(searchbugs(true),Options) -> (action:process_search_bugs(Args,Options),                Continue) ;
    memberchk(trainmodel(true),Options) -> (action:process_train_model,                              Continue) ;
    memberchk(similar(true),Options)   -> (action:process_similar(Args),                             Continue) ;
    memberchk(estimate(true),Options)  -> (action:process_estimate(Args),                            Continue) ;
    action:extract_llm_opt(Options, LlmOpt)
                                      -> (action:process_llm_chat(LlmOpt),                          Continue) ;
    memberchk(shell(true),Options), Args \== []
                                      -> (action:process_action(run,Args,Options),                   Continue) ;
    memberchk(shell(true),Options)    -> Continue ;
    memberchk(merge(true),Options)    -> (action:process_action(run,Args,Options),                   Continue)),

  Continue.

interface:process_requests(_) :-
  ( catch(daemon:running, _, fail)
  -> true
  ;  halt(1)
  ).
