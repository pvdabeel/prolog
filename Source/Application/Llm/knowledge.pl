/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> LLMKNOWLEDGE
Read-only knowledge pack for LLM self-grounding on portage-ng.

Provides curated topic digests plus bounded readers for Handbook chapters
and Source/ excerpts. Intended for use from sandboxed `<call:swi_prolog>`
so the LLM can inspect how the resolver works without mutating state.
*/

:- module(llmknowledge, []).

% =============================================================================
%  LLMKNOWLEDGE declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Topic catalogue
% -----------------------------------------------------------------------------

%! llmknowledge:topics(-Topics) is det.
%
% Topics is a sorted list of curated topic atoms the LLM may request.

llmknowledge:topics(Topics) :-
  findall(T, llmknowledge:topic_text(T, _), Ts),
  sort(Ts, Topics).


%! llmknowledge:topic(+Name, -Text) is semidet.
%
% Returns the curated digest for Name. Prints Topics on failure to help
% the LLM recover when it invents a name.

llmknowledge:topic(Name, Text) :-
  atom(Name),
  llmknowledge:topic_text(Name, Text),
  !.

llmknowledge:topic(Name, Text) :-
  llmknowledge:topics(Topics),
  format(atom(Text),
         'Unknown topic ~w. Available: ~w~n',
         [Name, Topics]).


%! llmknowledge:print_topic(+Name) is det.
%
% Convenience for sandboxed LLM code: writes the topic to stdout.

llmknowledge:print_topic(Name) :-
  llmknowledge:topic(Name, Text),
  write(Text).


%! llmknowledge:list_topics is det.
%
% Writes the topic catalogue to stdout.

llmknowledge:list_topics :-
  llmknowledge:topics(Topics),
  format('Available llmknowledge topics:~n', []),
  forall(member(T, Topics), format('  - ~w~n', [T])).


% -----------------------------------------------------------------------------
%  Handbook reader
% -----------------------------------------------------------------------------

%! llmknowledge:handbook(+Chapter, -Text) is semidet.
%
% Reads a Handbook chapter. Chapter may be a short name (architecture,
% prover, assumptions, …), a basename (04-doc-architecture.md), or an
% absolute/relative path under Documentation/Handbook/. Text is capped.

llmknowledge:handbook(Chapter, Text) :-
  llmknowledge:resolve_handbook(Chapter, Path),
  llmknowledge:read_capped_file(Path, Text).


%! llmknowledge:print_handbook(+Chapter) is det.

llmknowledge:print_handbook(Chapter) :-
  ( llmknowledge:handbook(Chapter, Text)
  -> write(Text)
  ;  llmknowledge:handbook_catalogue(Cat),
     format('Unknown handbook chapter ~w.~n~w~n', [Chapter, Cat])
  ).


%! llmknowledge:handbook_catalogue(-Text) is det.

llmknowledge:handbook_catalogue(Text) :-
  findall(Line,
    ( llmknowledge:handbook_alias(Alias, File),
      format(atom(Line), '  ~w -> ~w~n', [Alias, File])
    ),
    Lines),
  atomic_list_concat(['Handbook aliases:\n'|Lines], Text).


%! llmknowledge:resolve_handbook(+Chapter, -Path) is semidet.

llmknowledge:resolve_handbook(Chapter, Path) :-
  atom(Chapter),
  ( llmknowledge:handbook_alias(Chapter, File)
  -> true
  ;  File = Chapter
  ),
  config:installation_dir(Root),
  atomic_list_concat(FileParts0, '/', File),
  exclude(=(''), FileParts0, FileParts),
  append(['Documentation', 'Handbook'], FileParts, Parts),
  ( llmknowledge:safe_join(Root, Parts, Path),
    exists_file(Path)
  -> true
  ;  append(Parts, [], Parts0),
     llmknowledge:safe_join(Root, Parts0, Path0),
     atom_concat(Path0, '.md', Path),
     exists_file(Path)
  ).


%! llmknowledge:handbook_alias(?Alias, ?File) is nondet.

llmknowledge:handbook_alias(architecture, '04-doc-architecture.md').
llmknowledge:handbook_alias(literals, '05-doc-proof-literals.md').
llmknowledge:handbook_alias(knowledgebase, '06-doc-knowledgebase.md').
llmknowledge:handbook_alias(eapi, '07-doc-eapi-grammar.md').
llmknowledge:handbook_alias(prover, '08-doc-prover.md').
llmknowledge:handbook_alias(assumptions, '09-doc-prover-assumptions.md').
llmknowledge:handbook_alias(versions, '10-doc-version-domains.md').
llmknowledge:handbook_alias(rules, '11-doc-rules.md').
llmknowledge:handbook_alias(resolution, '12-doc-resolution.md').
llmknowledge:handbook_alias(planning, '13-doc-planning.md').
llmknowledge:handbook_alias(output, '14-doc-output.md').
llmknowledge:handbook_alias(cli, '15-doc-cli.md').
llmknowledge:handbook_alias(building, '16-doc-building.md').
llmknowledge:handbook_alias(llm, '17-doc-llm.md').
llmknowledge:handbook_alias(context, '22-doc-context-terms.md').
llmknowledge:handbook_alias(policy, 'Policy/README.md').
llmknowledge:handbook_alias(policy_map, 'Policy/map.md').
llmknowledge:handbook_alias(policy_assumption, 'Policy/assumption.md').


% -----------------------------------------------------------------------------
%  Source excerpt reader
% -----------------------------------------------------------------------------

%! llmknowledge:source(+RelPath, +StartLine, +NumLines, -Text) is semidet.
%
% Reads NumLines of a Source/ (or Documentation/) file starting at
% StartLine (1-based). RelPath must stay under an allowed prefix.
% NumLines is clamped to config:llm_knowledge_max_source_lines/1.

llmknowledge:source(RelPath, StartLine, NumLines, Text) :-
  atom(RelPath),
  integer(StartLine), StartLine >= 1,
  integer(NumLines), NumLines > 0,
  llmknowledge:resolve_source(RelPath, Path),
  ( catch(config:llm_knowledge_max_source_lines(MaxL), _, fail), integer(MaxL)
  -> true
  ;  MaxL = 120
  ),
  Take is min(NumLines, MaxL),
  llmknowledge:read_line_range(Path, StartLine, Take, Text).


%! llmknowledge:print_source(+RelPath, +StartLine, +NumLines) is det.

llmknowledge:print_source(RelPath, StartLine, NumLines) :-
  ( llmknowledge:source(RelPath, StartLine, NumLines, Text)
  -> write(Text)
  ;  format('Cannot read source ~w (not found, outside whitelist, or bad range).~n',
            [RelPath])
  ).


%! llmknowledge:resolve_source(+RelPath, -Path) is semidet.

llmknowledge:resolve_source(RelPath, Path) :-
  llmknowledge:normalize_rel(RelPath, Norm),
  llmknowledge:allowed_prefix(Norm),
  config:installation_dir(Root),
  atomic_list_concat(Parts, '/', Norm),
  llmknowledge:safe_join(Root, Parts, Path),
  exists_file(Path).


%! llmknowledge:normalize_rel(+Rel, -Norm) is semidet.

llmknowledge:normalize_rel(Rel, Norm) :-
  atom_string(Rel, S0),
  normalize_space(string(S1), S0),
  atom_string(A1, S1),
  \+ sub_atom(A1, 0, _, _, '/'),
  \+ sub_atom(A1, _, _, _, '..'),
  Norm = A1.


%! llmknowledge:allowed_prefix(+Norm) is semidet.

llmknowledge:allowed_prefix(Norm) :-
  ( sub_atom(Norm, 0, _, _, 'Source/Pipeline/')
  ; sub_atom(Norm, 0, _, _, 'Source/Domain/Gentoo/')
  ; sub_atom(Norm, 0, _, _, 'Source/Knowledge/')
  ; sub_atom(Norm, 0, _, _, 'Source/Application/Llm/')
  ; sub_atom(Norm, 0, _, _, 'Source/Application/Interface/')
  ; sub_atom(Norm, 0, _, _, 'Source/Logic/')
  ; sub_atom(Norm, 0, _, _, 'Documentation/Handbook/')
  ; Norm == '.cursorrules'
  ; Norm == 'portage-ng.pl'
  ).


% -----------------------------------------------------------------------------
%  File helpers
% -----------------------------------------------------------------------------

%! llmknowledge:safe_join(+Root, +Parts, -Path) is semidet.

llmknowledge:safe_join(Root, Parts, Path) :-
  \+ ( member(P, Parts), ( P == '..' ; sub_atom(P, _, _, _, '..') )),
  os:compose_path([Root|Parts], Path),
  absolute_file_name(Root, AbsRoot, [file_type(directory)]),
  absolute_file_name(Path, AbsPath, [access(read), file_errors(fail)]),
  atom_concat(AbsRoot, _, AbsPath).


%! llmknowledge:read_capped_file(+Path, -Text) is semidet.

llmknowledge:read_capped_file(Path, Text) :-
  ( catch(config:llm_knowledge_max_bytes(Max), _, fail), integer(Max)
  -> true
  ;  Max = 12000
  ),
  setup_call_cleanup(
    open(Path, read, S),
    read_string(S, Max, Body),
    close(S)),
  atom_string(BodyAtom, Body),
  size_file(Path, Size),
  ( Size > Max
  -> atomic_list_concat([BodyAtom, '\n[truncated; use print_source/3 for later sections]\n'], Text)
  ;  Text = BodyAtom
  ).


%! llmknowledge:read_line_range(+Path, +Start, +Take, -Text) is semidet.

llmknowledge:read_line_range(Path, Start, Take, Text) :-
  setup_call_cleanup(
    open(Path, read, S),
    llmknowledge:skip_and_take(S, 1, Start, Take, [], LinesRev),
    close(S)),
  reverse(LinesRev, Lines),
  atomic_list_concat(Lines, Text).


%! llmknowledge:skip_and_take(+Stream, +Cur, +Start, +Left, +Acc, -Out) is det.

llmknowledge:skip_and_take(S, Cur, Start, Left, Acc, Out) :-
  ( Left =< 0
  -> Out = Acc
  ;  read_line_to_string(S, Line),
     ( Line == end_of_file
     -> Out = Acc
     ;  ( Cur < Start
        -> Cur1 is Cur + 1,
           llmknowledge:skip_and_take(S, Cur1, Start, Left, Acc, Out)
        ;  format(atom(Annotated), '~d|~s~n', [Cur, Line]),
           Left1 is Left - 1,
           Cur1 is Cur + 1,
           llmknowledge:skip_and_take(S, Cur1, Start, Left1, [Annotated|Acc], Out)
        )
     )
  ).


% -----------------------------------------------------------------------------
%  Curated digests
% -----------------------------------------------------------------------------

%! llmknowledge:topic_text(+Name, -Text) is semidet.
%
% Curated digest for a catalogue topic.

llmknowledge:topic_text(architecture, Text) :-
  atomic_list_concat(
    [ 'portage-ng pipeline (standalone):',
      'reader/parser (eapi.md5-cache) -> cache facts',
      'prover -> ProofAVL, ModelAVL, ConstraintsAVL, TriggersAVL',
      'orderer -> second prover pass over planning laws -> wave plan',
      'printer -> plan + assumptions',
      'builder -> ebuild phases; fixup + feedback may replan',
      '',
      'Entry points: pipeline:prove_plan_with_fallback/5,6 (full),',
      'pipeline:prove_with_fallback/4 (prover only). Fallback tiers:',
      'strict, keyword_acceptance, blockers, unmask, keyword_unmask.',
      '',
      'Not emerge: same tree/VDB/config, different solver. Never suggest',
      'emerge as a fix for portage-ng resolver behaviour.'
    ], '\n', Text).
llmknowledge:topic_text(proof, Text) :-
  atomic_list_concat(
    [ 'Proof literals: Repo://Entry:Action?{Context}.',
      'ProofAVL keys: rule(Lit) normal; rule(assumed(X)) domain assumption;',
      'assumed(rule(X)) prover cycle-break. ModelAVL: Lit or assumed(Lit).',
      'TriggersAVL: reverse deps (body -> heads). Context list holds',
      'required_use, build_with_use, self, constraint, slots, reasons.',
      'Introspection: explainer:why_in_proof/3,4; why_in_plan/5,6;',
      'why_assumption/4,5. Lists: prover:proof_to_list/2, model_to_list/2.'
    ], '\n', Text).
llmknowledge:topic_text(assumptions, Text) :-
  atomic_list_concat(
    [ 'Domain assumption polarity:',
      'POSITIVE/actionable: masked, keyword_filtered, license,',
      'blocker_assumption — suggest package.unmask / accept_keywords /',
      'package.license / unblock.',
      'NEGATIVE/blocking: non_existent_dependency, missing_dependency,',
      'required_use_violation, slot_conflict, version_no_candidate,',
      'version_conflict, unsatisfied_constraints, issue_with_model.',
      'Cycle breaks (*_cycle, naf_cycle) are separate (exit 1), not domain.',
      'CI: 0 clean, 1 cycles only, 2 any domain assumptions.',
      'Taxonomy: Printer/Plan/assumption.pl.'
    ], '\n', Text).
llmknowledge:topic_text(learning, Text) :-
  atomic_list_concat(
    [ 'Two learning stores — do not mix:',
      '1) prover:learn/3 — prove-time cn_domain / USE narrowing only.',
      '2) feedback:* — build-time durable edges (Knowledge/feedback.pl):',
      '   discovered_dep, discovered_usedep, excluded_version,',
      '   required_kernel_config; unresolved_diagnostic is backlog only.',
      'Plans are re-derived after feedback, never patched.',
      'Fixups: Exceptions/* register fixup:mechanism/1 and',
      'phase_retry_hook/10 (collision, ghcabi, ocamlabi, missing_provider,',
      'useenable, kernelconfig). Metacircular LLM proposes feedback or',
      'draft_fixup sketches under Knowledge/drafts/ after confirm.'
    ], '\n', Text).
llmknowledge:topic_text(code_map, Text) :-
  atomic_list_concat(
    [ 'Key Source files:',
      'Source/Pipeline/{prover,resolver,orderer,pipeline,printer,builder}.pl',
      'Source/Domain/Gentoo/Rules/ordering.pl (pass-2 rule set: laws + bindings)',
      'Source/Pipeline/Prover/{explainer,explanation}.pl',
      'Source/Domain/Gentoo/Rules/resolving.pl + Rules/Resolving/*.pl (pass-1 rule set)',
      'Source/Domain/Gentoo/Exceptions/{fixup,missing_provider,useenable,...}.pl',
      'Source/Domain/Gentoo/{eapi,version,vdb,profile}.pl',
      'Source/Knowledge/{cache,query,feedback,repository,knowledgebase}.pl',
      'Source/Application/{llm,interface}.pl + Llm/{explain,metacircular,',
      'knowledge,semantic}.pl',
      'Source/Config/<host>.local.pl — tree/VDB paths',
      '',
      'Naming: Source filenames use concatenated lowercase (no hyphens),',
      'except grandfathered portage-ng.pl and a few binpkg/ebuild_*.pl.',
      '',
      'Use llmknowledge:print_handbook(architecture) or',
      'llmknowledge:print_source(\'Source/Pipeline/prover.pl\', 1, 80).'
    ], '\n', Text).
llmknowledge:topic_text(context_words, Text) :-
  atomic_list_concat(
    [ 'Three meanings of "context" in this codebase:',
      '1) OO context — Source/Logic/context.pl (::, :this).',
      '2) Proof-term context — ?{Context} list on literals.',
      '3) Pengines/sandbox context — server mode thread-local.',
      'Always qualify which you mean.'
    ], '\n', Text).
llmknowledge:topic_text(howto_inspect, Text) :-
  atomic_list_concat(
    [ 'Sandboxed inspection recipe:',
      ':- llmknowledge:list_topics.',
      ':- llmknowledge:print_topic(architecture).',
      ':- llmknowledge:print_handbook(prover).',
      ':- llmknowledge:print_source(\'Source/Knowledge/feedback.pl\', 1, 60).',
      'KB queries: cache:ordered_entry/5, query:search/2.',
      'Why: explainer:why_in_proof/3 (needs live AVLs in shell).',
      'Do not call feedback:record_* or prover:learn from sandbox.'
    ], '\n', Text).
