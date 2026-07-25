/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> GLSA
Gentoo Linux Security Advisory knowledge store.

Parses advisories from a Portage tree's `metadata/glsa/` directory into
Prolog facts (optionally qcompiled as `Knowledge/glsa.qlf`, following the
profile-cache pattern).  Advisories are *not* a package repository: entry
identity remains CPVN on portage/pkg/binpkg.  This module is a sibling
knowledge artifact queried via `glsa:search/2`, with thin bridges into
package `query:search` (`vulnerable/1`, `glsa/1`).

Security computed sets (`@security`, …) expand through `sets:expand/2` by
calling `glsa:security_atoms/2`, which joins these facts against the VDB
and emits `=cat/name-version` remediation atoms (Portage NewAffectedSet
semantics by default).
*/

:- module(glsa, []).

% =============================================================================
%  GLSA declarations
% =============================================================================

:- dynamic glsa:advisory/2.
:- dynamic glsa:package/4.
:- dynamic glsa:range/7.
:- dynamic glsa:loaded/0.
:- dynamic glsa:cache_source/1.
:- dynamic glsa:injected_file_override/1.

% -----------------------------------------------------------------------------
%  Configuration and paths
% -----------------------------------------------------------------------------

%! glsa:directory(-Dir) is semidet.
%
% Returns the on-disk GLSA directory. Prefers `config:glsa_dir/1`, else
% `$PORTDIR/metadata/glsa` from the registered portage repository.

glsa:directory(Dir) :-
  current_predicate(config:glsa_dir/1),
  config:glsa_dir(Dir0),
  !,
  Dir = Dir0.
glsa:directory(Dir) :-
  catch(portage:get_location(Root), _, fail),
  !,
  atomic_list_concat([Root, '/metadata/glsa'], Dir),
  exists_directory(Dir).


%! glsa:injected_file(-File) is det.
%
% Path of the glsa_injected applied-ID file. Prefers
% `config:glsa_injected_file/1`, else a host-local Knowledge path.

glsa:injected_file(File) :-
  glsa:injected_file_override(File),
  !.
glsa:injected_file(File) :-
  current_predicate(config:glsa_injected_file/1),
  config:glsa_injected_file(File),
  !.
glsa:injected_file(File) :-
  config:installation_dir(Dir),
  config:hostname(Hostname),
  os:compose_path([Dir, 'Source/Knowledge/Sets/glsa_injected', Hostname], File).


%! glsa:cache_file(-File) is det.
%
% Path of the qcompiled GLSA cache (`Knowledge/glsa.qlf`).

glsa:cache_file(File) :-
  working_directory(Cwd, Cwd),
  directory_file_path(Cwd, 'Knowledge/glsa.qlf', File).


%! glsa:raw_file(-File) is det.
%
% Path of the textual GLSA cache source (`Knowledge/glsa.raw`).

glsa:raw_file(File) :-
  working_directory(Cwd, Cwd),
  directory_file_path(Cwd, 'Knowledge/glsa.raw', File).


% -----------------------------------------------------------------------------
%  Cache load / save / ensure
% -----------------------------------------------------------------------------

%! glsa:cache_available is semidet.
%
% Succeeds when Knowledge/glsa.qlf exists.

glsa:cache_available :-
  glsa:cache_file(File),
  exists_file(File).


%! glsa:cache_save is det.
%
% Parse `metadata/glsa/` and serialize facts to Knowledge/glsa.qlf.
% No-ops with a notice when the GLSA directory is missing.

glsa:cache_save :-
  ( glsa:directory(Dir) ->
      message:header(['Caching GLSAs from ', Dir]), nl,
      glsa:parse_directory(Dir, Advisories, Packages, Ranges),
      glsa:write_cache(Advisories, Packages, Ranges),
      length(Advisories, N),
      format('% GLSA cache saved (~w advisories) to Knowledge/glsa.qlf~n', [N])
  ; format(user_error, '% glsa:cache_save — no metadata/glsa directory, skipping.~n', [])
  ).


%! glsa:cache_load is semidet.
%
% Load Knowledge/glsa.qlf into the local dynamic store. Fails when the
% cache file is absent.

glsa:cache_load :-
  glsa:cache_file(File),
  exists_file(File),
  glsa:clear_facts,
  ensure_loaded(File),
  ( current_predicate(glsadata:advisory/2) ->
      forall(glsadata:advisory(Id, Title),
             assertz(glsa:advisory(Id, Title))),
      forall(glsadata:package(Id, C, N, Arch),
             assertz(glsa:package(Id, C, N, Arch))),
      forall(glsadata:range(Id, C, N, Kind, Op, Ver, Slot),
             assertz(glsa:range(Id, C, N, Kind, Op, Ver, Slot)))
  ; true
  ),
  retractall(glsa:loaded),
  assertz(glsa:loaded),
  retractall(glsa:cache_source(_)),
  assertz(glsa:cache_source(qlf)).


%! glsa:ensure_loaded is det.
%
% Ensures advisory facts are available: prefer qlf cache, else live-parse
% the GLSA directory. Idempotent within a process.

glsa:ensure_loaded :-
  glsa:loaded, !.
glsa:ensure_loaded :-
  ( glsa:cache_load -> true
  ; glsa:directory(Dir) ->
      glsa:clear_facts,
      glsa:parse_directory(Dir, Advisories, Packages, Ranges),
      forall(member(advisory(Id, Title), Advisories),
             assertz(glsa:advisory(Id, Title))),
      forall(member(package(Id, C, N, Arch), Packages),
             assertz(glsa:package(Id, C, N, Arch))),
      forall(member(range(Id, C, N, Kind, Op, Ver, Slot), Ranges),
             assertz(glsa:range(Id, C, N, Kind, Op, Ver, Slot))),
      retractall(glsa:loaded),
      assertz(glsa:loaded),
      retractall(glsa:cache_source(_)),
      assertz(glsa:cache_source(live))
  ; retractall(glsa:loaded),
    assertz(glsa:loaded),
    retractall(glsa:cache_source(_)),
    assertz(glsa:cache_source(empty))
  ).


%! glsa:clear_facts is det.
%
% Retracts all in-memory GLSA facts and the loaded flag.

glsa:clear_facts :-
  retractall(glsa:advisory(_, _)),
  retractall(glsa:package(_, _, _, _)),
  retractall(glsa:range(_, _, _, _, _, _, _)),
  retractall(glsa:loaded),
  retractall(glsa:cache_source(_)).


%! glsa:write_cache(+Advisories, +Packages, +Ranges) is det.
%
% Writes Knowledge/glsa.raw and qcompiles it to Knowledge/glsa.qlf.

glsa:write_cache(Advisories, Packages, Ranges) :-
  glsa:raw_file(RawFile),
  file_directory_name(RawFile, Dir),
  ( exists_directory(Dir) -> true ; make_directory_path(Dir) ),
  setup_call_cleanup(
    open(RawFile, write, Out, [encoding(utf8)]),
    ( format(Out, ':- module(glsadata, []).~n', []),
      format(Out, '% Auto-generated GLSA cache — do not edit.~n~n', []),
      format(Out, ':- dynamic advisory/2.~n', []),
      format(Out, ':- dynamic package/4.~n', []),
      format(Out, ':- dynamic range/7.~n~n', []),
      forall(member(advisory(Id, Title), Advisories),
             format(Out, '~q.~n', [advisory(Id, Title)])),
      forall(member(package(Id, C, N, Arch), Packages),
             format(Out, '~q.~n', [package(Id, C, N, Arch)])),
      forall(member(range(Id, C, N, Kind, Op, Ver, Slot), Ranges),
             format(Out, '~q.~n', [range(Id, C, N, Kind, Op, Ver, Slot)]))
    ),
    close(Out)
  ),
  catch(qcompile(RawFile), E,
        format(user_error, '% glsa:cache_save — qcompile failed: ~w~n', [E])),
  glsa:clear_facts,
  forall(member(advisory(Id, Title), Advisories),
         assertz(glsa:advisory(Id, Title))),
  forall(member(package(Id, C, N, Arch), Packages),
         assertz(glsa:package(Id, C, N, Arch))),
  forall(member(range(Id, C, N, Kind, Op, Ver, Slot), Ranges),
         assertz(glsa:range(Id, C, N, Kind, Op, Ver, Slot))),
  retractall(glsa:loaded),
  assertz(glsa:loaded),
  retractall(glsa:cache_source(_)),
  assertz(glsa:cache_source(qlf)).


% -----------------------------------------------------------------------------
%  Applied / injected tracking
% -----------------------------------------------------------------------------

%! glsa:applied(+Id) is semidet.
%
% True when Id appears in the glsa_injected file.

glsa:applied(Id) :-
  glsa:injected_file(File),
  exists_file(File),
  setup_call_cleanup(
    open(File, read, In, [encoding(utf8)]),
    glsa:stream_has_id(In, Id),
    close(In)
  ).


%! glsa:inject(+Id) is det.
%
% Appends Id to glsa_injected when not already present.

glsa:inject(Id) :-
  ( glsa:applied(Id) -> true
  ; glsa:injected_file(File),
    file_directory_name(File, Dir),
    ( exists_directory(Dir) -> true ; make_directory_path(Dir) ),
    setup_call_cleanup(
      open(File, append, Out, [encoding(utf8)]),
      format(Out, '~w~n', [Id]),
      close(Out)
    )
  ).


%! glsa:stream_has_id(+Stream, +Id) is semidet.
%
% Succeeds when a line in Stream equals Id (after whitespace normalize).

glsa:stream_has_id(In, Id) :-
  read_line_to_string(In, Line),
  Line \== end_of_file,
  normalize_space(atom(Tok), Line),
  ( Tok == Id -> true ; glsa:stream_has_id(In, Id) ).


% -----------------------------------------------------------------------------
%  XML parsing (DTD-safe, no load_structure)
% -----------------------------------------------------------------------------

%! glsa:parse_directory(+Dir, -Advisories, -Packages, -Ranges) is det.
%
% Parses every `glsa-*.xml` under Dir. Malformed files are skipped.

glsa:parse_directory(Dir, Advisories, Packages, Ranges) :-
  directory_files(Dir, Entries0),
  findall(Id-Path,
          ( member(F, Entries0),
            atom_concat('glsa-', Rest, F),
            atom_concat(Id, '.xml', Rest),
            atomic_list_concat([Dir, '/', F], Path)
          ),
          Pairs0),
  sort(Pairs0, Pairs),
  findall(parsed(Adv, Pkgs, Rngs),
          ( member(Id-Path, Pairs),
            catch(glsa:parse_file(Path, Id, Adv, Pkgs, Rngs), E,
                  ( print_message(warning, glsa_parse_error(Id, E)), fail ))
          ),
          Parsed),
  findall(Adv, member(parsed(Adv, _, _), Parsed), Advisories),
  findall(Pkg, (member(parsed(_, Pkgs, _), Parsed), member(Pkg, Pkgs)), Packages),
  findall(Rng, (member(parsed(_, _, Rngs), Parsed), member(Rng, Rngs)), Ranges).


%! glsa:parse_file(+Path, +Id, -Advisory, -Packages, -Ranges) is semidet.
%
% Parses one GLSA XML file into an advisory fact and package/range lists.

glsa:parse_file(Path, Id, advisory(Id, Title), Packages, Ranges) :-
  read_file_to_string(Path, Content, [encoding(utf8)]),
  ( glsa:xml_tag_text(Content, "title", TitleStr) -> atom_string(Title, TitleStr)
  ; Title = ''
  ),
  !,
  ( glsa:xml_tag_attr(Content, "product", "type", TypeStr) -> true ; TypeStr = "ebuild" ),
  TypeStr == "ebuild",
  !,
  glsa:extract_packages(Content, Id, Packages, Ranges),
  !.


%! glsa:extract_packages(+Content, +Id, -Packages, -Ranges) is det.
%
% Extracts `<package>` blocks and nested vulnerable/unaffected ranges.

glsa:extract_packages(Content, Id, Packages, Ranges) :-
  findall(package(Id, C, N, Arch)-PkgRanges,
          glsa:package_block(Content, C, N, Arch, PkgRanges),
          Pairs),
  findall(package(Id, C, N, Arch), member(package(Id, C, N, Arch)-_, Pairs), Packages),
  findall(range(Id, C, N, Kind, Op, Ver, Slot),
          ( member(package(Id, C, N, _)-PkgRanges, Pairs),
            member(range(Kind, Op, Ver, Slot), PkgRanges)
          ),
          Ranges).


%! glsa:package_block(+Content, -C, -N, -Arch, -Ranges) is nondet.
%
% Backtracks over each `<package …>…</package>` block in Content.

glsa:package_block(Content, C, N, Arch, Ranges) :-
  sub_string(Content, P0, _, _, "<package"),
  sub_string(Content, P0, _, 0, FromPkg),
  once((
    sub_string(FromPkg, PEnd, _, _, "</package>"),
    End is PEnd + 10,
    sub_string(FromPkg, 0, End, _, Block),
    glsa:xml_attr(Block, "name", NameStr)
  )),
  atom_string(NameAtom, NameStr),
  atomic_list_concat([C, N], '/', NameAtom),
  ( once(glsa:xml_attr(Block, "arch", ArchStr))
    -> atom_string(Arch, ArchStr)
    ;  Arch = '*'
  ),
  findall(range(Kind, Op, Ver, Slot),
          glsa:range_element(Block, Kind, Op, Ver, Slot),
          Ranges).


%! glsa:range_element(+Block, -Kind, -Op, -Ver, -Slot) is nondet.
%
% Extracts one `<vulnerable|unaffected range="…">version</…>` element.

glsa:range_element(Block, Kind, Op, Ver, Slot) :-
  member(Kind-Tag, [vulnerable-"vulnerable", unaffected-"unaffected"]),
  string_concat("<", Tag, Open0),
  sub_string(Block, P0, _, _, Open0),
  once((
    sub_string(Block, P0, _, 0, From),
    sub_string(From, PClose, _, _, ">"),
    OpenLen is PClose + 1,
    sub_string(From, 0, OpenLen, _, OpenTag),
    glsa:xml_attr(OpenTag, "range", OpStr),
    atom_string(Op0, OpStr),
    glsa:normalize_op(Op0, Op),
    ( glsa:xml_attr(OpenTag, "slot", SlotStr) ->
        atom_string(Slot0, SlotStr),
        ( Slot0 == '' -> Slot = '*' ; Slot = Slot0 )
    ; Slot = '*'
    ),
    string_concat("</", Tag, Close0),
    string_concat(Close0, ">", Close),
    sub_string(From, OpenLen, _, 0, AfterOpen),
    sub_string(AfterOpen, VLen, _, _, Close),
    sub_string(AfterOpen, 0, VLen, _, VerStr0),
    normalize_space(string(VerStr), VerStr0),
    atom_string(VerAtom, VerStr),
    glsa:parse_version_atom(VerAtom, Ver)
  )).


%! glsa:normalize_op(+Raw, -Op) is semidet.
%
% Accepts the GLSA range attribute tokens.

glsa:normalize_op(le, le).
glsa:normalize_op(lt, lt).
glsa:normalize_op(eq, eq).
glsa:normalize_op(gt, gt).
glsa:normalize_op(ge, ge).
glsa:normalize_op(rge, rge).
glsa:normalize_op(rle, rle).
glsa:normalize_op(rgt, rgt).
glsa:normalize_op(rlt, rlt).


%! glsa:parse_version_atom(+Atom, -Version) is semidet.
%
% Parses a bare version string into a version/7 term.

glsa:parse_version_atom(Atom, Version) :-
  atom_codes(Atom, Codes),
  phrase(eapi:version(Version), Codes, []).


%! glsa:xml_tag_text(+Content, +Tag, -Text) is semidet.
%
% Extracts the text content of the first `<Tag>…</Tag>`.

glsa:xml_tag_text(Content, Tag, Text) :-
  string_concat("<", Tag, Open0),
  string_concat(Open0, ">", Open),
  once((
    sub_string(Content, P0, _, _, Open),
    string_length(Open, OpenLen),
    Start is P0 + OpenLen,
    string_concat("</", Tag, Close0),
    string_concat(Close0, ">", Close),
    sub_string(Content, Start, _, 0, Rest),
    sub_string(Rest, Len, _, _, Close),
    sub_string(Rest, 0, Len, _, Text0),
    normalize_space(string(Text), Text0)
  )).


%! glsa:xml_tag_attr(+Content, +Tag, +Attr, -Value) is semidet.
%
% Extracts Attr from the first opening `<Tag …>` element.

glsa:xml_tag_attr(Content, Tag, Attr, Value) :-
  string_concat("<", Tag, Open0),
  once((
    sub_string(Content, P0, _, _, Open0),
    sub_string(Content, P0, _, 0, From),
    sub_string(From, End, _, _, ">"),
    End1 is End + 1,
    sub_string(From, 0, End1, _, OpenTag),
    glsa:xml_attr(OpenTag, Attr, Value)
  )).


%! glsa:xml_attr(+OpenTag, +Attr, -Value) is semidet.
%
% Reads Attr="Value" or Attr='Value' from an opening tag string.

glsa:xml_attr(OpenTag, Attr, Value) :-
  string_concat(Attr, "=\"", Needle),
  once((
    sub_string(OpenTag, P0, _, _, Needle),
    string_length(Needle, NLen),
    Start is P0 + NLen,
    sub_string(OpenTag, Start, _, 0, Rest),
    sub_string(Rest, Len, _, _, "\""),
    sub_string(Rest, 0, Len, _, Value)
  )),
  !.
glsa:xml_attr(OpenTag, Attr, Value) :-
  string_concat(Attr, "='", Needle),
  once((
    sub_string(OpenTag, P0, _, _, Needle),
    string_length(Needle, NLen),
    Start is P0 + NLen,
    sub_string(OpenTag, Start, _, 0, Rest),
    sub_string(Rest, Len, _, _, "'"),
    sub_string(Rest, 0, Len, _, Value)
  )).


% -----------------------------------------------------------------------------
%  Version / ARCH matching
% -----------------------------------------------------------------------------

%! glsa:host_arch(-Arch) is semidet.
%
% Host ARCH via `userconfig:current_arch/1` when available, else ARCH /
% ACCEPT_KEYWORDS from preference/env.

glsa:host_arch(Arch) :-
  current_predicate(userconfig:current_arch/1),
  catch(userconfig:current_arch(Arch), _, fail),
  !.
glsa:host_arch(Arch) :-
  catch(preference:getenv('ARCH', Arch0), _, fail),
  Arch0 \== '',
  !,
  Arch = Arch0.
glsa:host_arch(Arch) :-
  catch(preference:getenv('ACCEPT_KEYWORDS', KW), _, fail),
  KW \== '',
  atomic_list_concat([Tok|_], ' ', KW),
  Tok \== '',
  ( atom_concat('~', Arch, Tok) -> true ; Arch = Tok ).


%! glsa:arch_matches(+ArchSpec) is semidet.
%
% True when ArchSpec is `*` or lists the host ARCH. When ARCH is unknown,
% only `*` matches (conservative: do not claim vulnerability).

glsa:arch_matches('*') :- !.
glsa:arch_matches(ArchSpec) :-
  glsa:host_arch(Host),
  atomic_list_concat(Parts, ' ', ArchSpec),
  memberchk(Host, Parts).


%! glsa:version_matches(+Op, +Bound, +Candidate) is semidet.
%
% True when Candidate satisfies the GLSA range Op against Bound.

glsa:version_matches(le, Bound, Cand) :-
  !,
  \+ eapi:version_compare(>, Cand, Bound).
glsa:version_matches(lt, Bound, Cand) :-
  !,
  eapi:version_compare(<, Cand, Bound).
glsa:version_matches(eq, Bound, Cand) :-
  !,
  eapi:version_compare(=, Cand, Bound).
glsa:version_matches(gt, Bound, Cand) :-
  !,
  eapi:version_compare(>, Cand, Bound).
glsa:version_matches(ge, Bound, Cand) :-
  !,
  \+ eapi:version_compare(<, Cand, Bound).
glsa:version_matches(rge, Bound, Cand) :-
  !,
  glsa:same_base_version(Bound, Cand),
  glsa:revision_compare(>=, Cand, Bound).
glsa:version_matches(rle, Bound, Cand) :-
  !,
  glsa:same_base_version(Bound, Cand),
  glsa:revision_compare(=<, Cand, Bound).
glsa:version_matches(rgt, Bound, Cand) :-
  !,
  glsa:same_base_version(Bound, Cand),
  glsa:revision_compare(>, Cand, Bound).
glsa:version_matches(rlt, Bound, Cand) :-
  !,
  glsa:same_base_version(Bound, Cand),
  glsa:revision_compare(<, Cand, Bound).


%! glsa:same_base_version(+A, +B) is semidet.
%
% True when two version/7 terms share everything except revision/Full.

glsa:same_base_version(version(N, A, SR, SN, ST, _, _),
                       version(N, A, SR, SN, ST, _, _)).


%! glsa:revision_compare(+Op, +Cand, +Bound) is semidet.
%
% Compares the revision fields of two version/7 terms.

glsa:revision_compare(Op, version(_,_,_,_,_, RevC, _),
                          version(_,_,_,_,_, RevB, _)) :-
  ( Op == (>)  -> RevC > RevB
  ; Op == (<)  -> RevC < RevB
  ; Op == (>=) -> RevC >= RevB
  ; Op == (=<) -> RevC =< RevB
  ).


%! glsa:slot_matches(+Req, +EntrySlot) is semidet.
%
% Slot filter: `*` matches any; otherwise exact canonical slot match.

glsa:slot_matches('*', _) :- !.
glsa:slot_matches(Req, EntrySlot) :-
  slotmeta:canon_slot(Req, R),
  slotmeta:canon_slot(EntrySlot, E),
  R == E.


%! glsa:range_matches(+Id, +C, +N, +Kind, +Ver, +Slot) is semidet.
%
% True when some Kind range for Id/C/N matches Ver in Slot.

glsa:range_matches(Id, C, N, Kind, Ver, Slot) :-
  glsa:range(Id, C, N, Kind, Op, Bound, ReqSlot),
  glsa:slot_matches(ReqSlot, Slot),
  glsa:version_matches(Op, Bound, Ver).


% -----------------------------------------------------------------------------
%  Vulnerability and merge list
% -----------------------------------------------------------------------------

%! glsa:is_vulnerable(+Id) is semidet.
%
% True when the host has an installed package covered by a vulnerable
% range of Id (and not covered by an unaffected range), for a matching
% ARCH, with at least one tree upgrade available.

glsa:is_vulnerable(Id) :-
  glsa:ensure_loaded,
  glsa:package(Id, C, N, Arch),
  glsa:arch_matches(Arch),
  glsa:vulnerable_installed(Id, C, N, InstalledVer, Slot),
  glsa:least_upgrade(C, N, Slot, InstalledVer, Id, _Upgrade),
  !.


%! glsa:vulnerable_installed(+Id, +C, +N, -Ver, -Slot) is nondet.
%
% Installed versions of C/N that match a vulnerable range and do not
% match an unaffected range of Id.

glsa:vulnerable_installed(Id, C, N, Ver, Slot) :-
  knowledgebase:vdb_repository(Vdb),
  query:search([category(C), name(N), version(Ver)], Vdb://Entry),
  Ver \== version_none,
  slotmeta:entry_slot_default(Vdb, Entry, Slot),
  glsa:range_matches(Id, C, N, vulnerable, Ver, Slot),
  \+ glsa:range_matches(Id, C, N, unaffected, Ver, Slot).


%! glsa:entry_covered(+Id, +Repo://+Entry) is semidet.
%
% True when Entry's C/N/version/slot is covered by a vulnerable range of
% Id and not by an unaffected range (ARCH ignored — caller filters).

glsa:entry_covered(Id, Repo://Entry) :-
  glsa:ensure_loaded,
  query:search([category(C), name(N), version(Ver)], Repo://Entry),
  Ver \== version_none,
  slotmeta:entry_slot_default(Repo, Entry, Slot),
  glsa:package(Id, C, N, _),
  glsa:range_matches(Id, C, N, vulnerable, Ver, Slot),
  \+ glsa:range_matches(Id, C, N, unaffected, Ver, Slot).


%! glsa:least_upgrade(+C, +N, +Slot, +InstalledVer, +Id, -UpgradeEntry) is semidet.
%
% Smallest visible tree version in Slot that matches an unaffected range
% of Id and is greater than InstalledVer (Portage least-change).

glsa:least_upgrade(C, N, Slot, InstalledVer, Id, BestRepo://BestEntry) :-
  findall(Ver-(Repo://Entry),
          ( query:search([select(repository, notequal, pkg),
                          category(C), name(N), version(Ver)], Repo://Entry),
            \+ knowledgebase:is_vdb_repository(Repo),
            slotmeta:entry_slot_default(Repo, Entry, Slot),
            sets:entry_visible(Repo://Entry),
            eapi:version_compare(>, Ver, InstalledVer),
            glsa:range_matches(Id, C, N, unaffected, Ver, Slot)
          ),
          Pairs),
  Pairs \== [],
  glsa:min_version_pair(Pairs, _- (BestRepo://BestEntry)).


%! glsa:min_version_pair(+Pairs, -Min) is det.
%
% Selects the lowest-version Version-Entry pair.

glsa:min_version_pair([First|Rest], Min) :-
  foldl(glsa:keep_lower_version, Rest, First, Min).


%! glsa:keep_lower_version(+Cand, +Acc, -Best) is det.
%
% Fold step retaining the lower-versioned pair.

glsa:keep_lower_version(Ver-Entry, AccVer-AccEntry, Best) :-
  ( eapi:version_compare(<, Ver, AccVer)
    -> Best = Ver-Entry
    ;  Best = AccVer-AccEntry
  ).


%! glsa:merge_list(+Id, -Atoms) is det.
%
% Least-change upgrade atoms (`=cat/name-version`) for advisory Id.

glsa:merge_list(Id, Atoms) :-
  glsa:ensure_loaded,
  findall(Atom,
          ( glsa:package(Id, C, N, Arch),
            glsa:arch_matches(Arch),
            glsa:vulnerable_installed(Id, C, N, InstalledVer, Slot),
            glsa:least_upgrade(C, N, Slot, InstalledVer, Id, _://Entry),
            atom_concat('=', Entry, Atom)
          ),
          Atoms0),
  sort(Atoms0, Atoms).


% -----------------------------------------------------------------------------
%  Search API
% -----------------------------------------------------------------------------

%! glsa:search(+Query, -Id) is nondet.
%
% Search advisories. Query is a goal or list of goals among:
%   id(Id), title(Title), package(C,N), applied(Bool), vulnerable(Bool).

glsa:search(Query, Id) :-
  glsa:ensure_loaded,
  ( is_list(Query) -> Goals = Query ; Goals = [Query] ),
  glsa:advisory(Id, Title),
  glsa:search_goals(Goals, Id, Title).


%! glsa:search_goals(+Goals, +Id, +Title) is semidet.
%
% Applies each search constraint to Id/Title.

glsa:search_goals([], _, _).
glsa:search_goals([G|Gs], Id, Title) :-
  glsa:search_goal(G, Id, Title),
  glsa:search_goals(Gs, Id, Title).


%! glsa:search_goal(+Goal, +Id, +Title) is semidet.
%
% One search constraint.

glsa:search_goal(id(Id), Id, _).
glsa:search_goal(title(Title), _, Title).
glsa:search_goal(package(C, N), Id, _) :-
  glsa:package(Id, C, N, _).
glsa:search_goal(applied(true), Id, _) :-
  glsa:applied(Id).
glsa:search_goal(applied(false), Id, _) :-
  \+ glsa:applied(Id).
glsa:search_goal(vulnerable(true), Id, _) :-
  glsa:is_vulnerable(Id).
glsa:search_goal(vulnerable(false), Id, _) :-
  \+ glsa:is_vulnerable(Id).


% -----------------------------------------------------------------------------
%  Security set expansion
% -----------------------------------------------------------------------------

%! glsa:security_atoms(+Filter, -Atoms) is det.
%
% Expands a Portage security-set filter to sorted `=cpv` remediation atoms.
% Filter is one of: security, affected, new_glsa, new_affected.
%
% Driven from the VDB (installed packages) rather than scanning every
% advisory, so expansion stays near-linear in installed CPV count.

glsa:security_atoms(Filter, Atoms) :-
  glsa:ensure_loaded,
  findall(Atom,
          ( knowledgebase:vdb_repository(Vdb),
            query:search([category(C), name(N), version(Ver)], Vdb://Entry),
            Ver \== version_none,
            slotmeta:entry_slot_default(Vdb, Entry, Slot),
            glsa:package(Id, C, N, Arch),
            glsa:arch_matches(Arch),
            glsa:filter_allows(Filter, Id),
            glsa:range_matches(Id, C, N, vulnerable, Ver, Slot),
            \+ glsa:range_matches(Id, C, N, unaffected, Ver, Slot),
            glsa:least_upgrade(C, N, Slot, Ver, Id, _://UpEntry),
            atom_concat('=', UpEntry, Atom)
          ),
          Atoms0),
  glsa:reduce_atoms(Atoms0, Atoms).


%! glsa:filter_allows(+Filter, +Id) is semidet.
%
% Portage security set class filters. For atom expansion, `security` and
% `affected` coincide (only vulnerable installs yield atoms); `new_*`
% additionally require the advisory not be in glsa_injected.
% `is_vulnerable/1` remains available for `glsa:search`.

glsa:filter_allows(security, _).
glsa:filter_allows(affected, _).
glsa:filter_allows(new_glsa, Id) :-
  \+ glsa:applied(Id).
glsa:filter_allows(new_affected, Id) :-
  \+ glsa:applied(Id).


%! glsa:reduce_atoms(+Atoms, -Reduced) is det.
%
% Per cat/name:slot keep the highest-version `=cpv` atom (Portage `_reduce`).

glsa:reduce_atoms([], []) :- !.
glsa:reduce_atoms(Atoms0, Reduced) :-
  findall(Key-Atom,
          ( member(Atom, Atoms0),
            glsa:atom_cn_slot_ver(Atom, C, N, Slot, _Ver),
            Key = C-N-Slot
          ),
          Pairs0),
  keysort(Pairs0, Sorted),
  glsa:keep_highest_per_key(Sorted, Kept),
  findall(A, member(_-A, Kept), Atoms1),
  sort(Atoms1, Reduced).


%! glsa:atom_cn_slot_ver(+Atom, -C, -N, -Slot, -Ver) is semidet.
%
% Parses `=cat/name-version` and resolves slot from the tree entry.

glsa:atom_cn_slot_ver(Atom, C, N, Slot, Ver) :-
  atom_concat('=', Entry, Atom),
  cache:ordered_entry(Repo, Entry, C, N, Ver),
  \+ knowledgebase:is_vdb_repository(Repo),
  slotmeta:entry_slot_default(Repo, Entry, Slot),
  !.
glsa:atom_cn_slot_ver(Atom, C, N, '0', Ver) :-
  atom_concat('=', Entry, Atom),
  atomic_list_concat([C, Rest], '/', Entry),
  atom_codes(Rest, Codes),
  phrase((eapi:package(N), eapi:version0(Ver)), Codes, []),
  Ver \== version_none.


%! glsa:keep_highest_per_key(+SortedPairs, -Kept) is det.
%
% From keysorted Key-Atom pairs, keep the highest-version atom per Key.

glsa:keep_highest_per_key([], []).
glsa:keep_highest_per_key([K-A|Rest], [K-Best|Out]) :-
  glsa:take_key_group(Rest, K, [K-A], Group, Rest2),
  glsa:highest_atom(Group, Best),
  glsa:keep_highest_per_key(Rest2, Out).


%! glsa:take_key_group(+Rest, +K, +Acc, -Group, -Rest2) is det.
%
% Collects consecutive pairs sharing key K.

glsa:take_key_group([K-A|Rest], K, Acc, Group, Rest2) :-
  !,
  glsa:take_key_group(Rest, K, [K-A|Acc], Group, Rest2).
glsa:take_key_group(Rest, _, Acc, Acc, Rest).


%! glsa:highest_atom(+Group, -Atom) is det.
%
% Picks the `=cpv` with the highest version from a Key-Atom group.

glsa:highest_atom([_-A], A) :- !.
glsa:highest_atom(Group, Best) :-
  findall(Ver-A,
          ( member(_-A, Group),
            ( glsa:atom_cn_slot_ver(A, _, _, _, Ver) -> true ; Ver = version_none )
          ),
          Pairs),
  glsa:max_version_pair(Pairs, _-Best).


%! glsa:max_version_pair(+Pairs, -Max) is det.
%
% Highest-version Version-Atom pair.

glsa:max_version_pair([First|Rest], Max) :-
  foldl(glsa:keep_higher_version, Rest, First, Max).


%! glsa:keep_higher_version(+Cand, +Acc, -Best) is det.
%
% Fold step retaining the higher-versioned pair.

glsa:keep_higher_version(Ver-A, AccVer-AccA, Best) :-
  ( eapi:version_compare(>, Ver, AccVer)
    -> Best = Ver-A
    ;  Best = AccVer-AccA
  ).


% -----------------------------------------------------------------------------
%  Message hook
% -----------------------------------------------------------------------------

:- multifile prolog:message//1.

prolog:message(glsa_parse_error(Id, E)) -->
  ['GLSA ~w: parse skipped (~w)'-[Id, E]].
