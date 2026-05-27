/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> BINPKG_EXEC
USE-aware selection and qmerge invocation for Portage binary packages.

This is the binpkg-side counterpart to `ebuild_exec`. It is the *only*
module that knows how to drive Portage's `ebuild qmerge` from a
pre-extracted gpkg, and the only place that decides whether a binpkg is
acceptable in lieu of a source build.

# Public surface

  - `binpkg_exec:available_for(+SrcRepo, +SrcEntry, +Ctx, -BinpkgEntryId)`
    is the dispatch probe. Given the planner's resolved (SrcRepo,
    SrcEntry, Ctx), find a binpkg variant whose stored USE / SLOT /
    KEYWORDS are compatible with what the planner asked for. Fails (no
    side-effects) if no candidate fits or if the binpkg repo is not
    registered. When multiple candidates pass the filters, the highest
    BUILD_ID wins (mirrors emerge's "newer wins" tie-breaker). The
    probe also honors `config:binpkg_refresh/1` to optionally re-read
    the on-disk `Packages` index when an external producer has updated
    it since the last load.

  - `binpkg_exec:execute(+Action, +SrcRepo, +SrcEntry, +BinpkgEntryId, +Ctx, -Outcome)`
    performs the binary merge:
      1. Resolve gpkg path from the binpkg repo's location + the
         entry's `path` metadata.
      2. Resolve the source ebuild path from SrcRepo (qmerge's `ebuild`
         CLI insists on a path inside a real portage tree).
      3. Compose the build dir under `config:build_root/1`.
      4. Delegate extraction to `binpkg_extract:prepare_builddir/3`.
      5. Spawn `ebuild --skip-manifest <SRC_EBUILD> qmerge` with
         `MERGE_TYPE=binary`, `PORTAGE_BINPKG_FILE=<gpkg>`,
         `PORTAGE_BUILDDIR=<builddir>`, and the planner's USE.
      6. Bind Outcome to `done` on success, `failed(qmerge_exit(N))` on
         non-zero exit, or `failed(Reason)` for setup errors.

# Configuration knobs (all in `Source/config.pl`)

  - `config:use_binpkg(true|false)`         -- master switch
  - `config:binpkg_respect_use(strict|relaxed)` -- USE matching mode
  - `config:binpkg_changed_deps(skip|warn)`     -- RDEPEND-drift policy
  - `config:binpkg_refresh(manual|mtime)`       -- index refresh policy

# What this module does NOT do

  - Implement preinst/install/postinst/VDB-write itself. That logic
    (~6700 lines of Python) is delegated to `ebuild qmerge`, which calls
    `portage/dbapi/vartree.py:merge()` internally.
  - Re-fetch / verify the gpkg. We trust the `Packages` index hash
    fields. (qmerge does its own MD5 verification anyway.)
  - Mutate the binpkg repository. Production of binpkgs is owned by
    `ebuild_exec` (when `--buildpkg` is active) or external producers
    such as concurrent build-harness matrix sessions.

The end-to-end recipe was validated empirically against
`app-misc/jq-1.8.1` BUILD_IDs 8 (oniguruma=on) and 10 (oniguruma=off);
both produced the expected USE-distinct VDB entries and runtime library
linkages.
*/

:- module(binpkg_exec, []).

% =============================================================================
%  BINPKG_EXEC declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Dispatch probe: is there a USE-compatible binpkg for SrcRepo://SrcEntry?
% -----------------------------------------------------------------------------

%! binpkg_exec:available_for(+SrcRepo, +SrcEntry, +Ctx, -BinpkgEntryId) is semidet.
%
% Master entry point used by `ebuild_exec:execute/5`'s dispatch hook to
% decide whether to short-circuit a source build with a binary merge.
% Fails silently and cheaply if any of the following holds:
%   - `config:use_binpkg(false)`
%   - the `binpkg` repository isn't registered (`cache:repository(binpkg)` absent)
%   - the source entry's category/name/version can't be derived
%   - no binpkg variant of the same (cat, name, version) exists
%   - none of the candidates pass the USE / SLOT / keywords filters
%
% On success, BinpkgEntryId is the chosen variant's cache entry id
% (e.g. `'app-misc/jq-1.8.1-9'`). The decision policy when multiple
% candidates pass is "highest BUILD_ID wins".

binpkg_exec:available_for(SrcRepo, SrcEntry, Ctx, BinpkgEntryId) :-
  config:use_binpkg(true),
  cache:repository(binpkg),
  binpkg_exec:maybe_refresh_index,
  binpkg_exec:src_entry_cnv(SrcRepo, SrcEntry, Cat, Name, Version),
  findall(Bid-Eid,
          ( cache:ordered_entry(binpkg, Eid, Cat, Name, Version),
            cache:entry_metadata(binpkg, Eid, build_id, Bid),
            binpkg_exec:candidate_passes_filters(SrcRepo, SrcEntry, Eid, Ctx)
          ),
          Candidates),
  Candidates \== [],
  binpkg_exec:pick_best_candidate(Candidates, BinpkgEntryId).


% -----------------------------------------------------------------------------
%  Index refresh policy (config:binpkg_refresh/1)
% -----------------------------------------------------------------------------

%! binpkg_exec:last_index_mtime(?Repository, ?Mtime) is nondet.
%
% Per-repository baseline: the on-disk mtime of the `Packages` index as
% it was when we last loaded it into the in-memory cache. Used by the
% `mtime` refresh policy to detect external changes (new binpkgs dropped
% by a concurrent producer) without paying for a full re-parse on every
% probe. Mtime is a SWI-Prolog float (seconds since the epoch, as
% returned by `time_file/2`).

:- dynamic binpkg_exec:last_index_mtime/2.


%! binpkg_exec:maybe_refresh_index is det.
%
% Honors `config:binpkg_refresh/1` at the start of `available_for/4`:
%
%   - manual : never auto-refresh (default; cheapest, fully predictable).
%   - mtime  : stat the index file and, if its mtime is newer than the
%              recorded baseline, re-run `binpkg:sync(kb)` before answering.
%
% Always succeeds. Missing config or unknown policy degrades to `manual`
% so an operator typo never silently disables binpkg dispatch.

binpkg_exec:maybe_refresh_index :-
  ( config:binpkg_refresh(Policy) -> true ; Policy = manual ),
  binpkg_exec:apply_refresh_policy(Policy).


%! binpkg_exec:apply_refresh_policy(+Policy) is det.
%
% Dispatch on the policy atom. The `mtime` branch is serialized via a
% dedicated mutex so two concurrent probes don't race on the global
% binpkg cache assertions that `sync(kb)` retract-and-reasserts.

binpkg_exec:apply_refresh_policy(manual) :- !.

binpkg_exec:apply_refresh_policy(mtime) :- !,
  with_mutex(binpkg_exec_refresh,
             binpkg_exec:refresh_if_stale(binpkg)).

binpkg_exec:apply_refresh_policy(_).


%! binpkg_exec:refresh_if_stale(+Repository) is det.
%
% Compare the index file's current mtime against the recorded baseline:
%
%   - First observation: record the baseline and do NOT re-sync. The
%     initial load was already performed by `kb:register(Repository)` at
%     startup, so the in-memory cache is already current.
%   - Newer mtime than baseline: update the baseline and call
%     `Repository:sync(kb)` to re-load the in-memory cache from disk.
%   - Same or older mtime: no-op.
%
% Exceptions during stat or sync are caught so a transient failure
% (e.g. the index file disappearing mid-probe) degrades to "use the
% cache we have" rather than breaking dispatch. Always succeeds.

binpkg_exec:refresh_if_stale(Repository) :-
  catch(binpkg_exec:probe_index_mtime(Repository, Disk), _, fail), !,
  ( binpkg_exec:last_index_mtime(Repository, Prev)
  -> ( Disk > Prev
     -> retractall(binpkg_exec:last_index_mtime(Repository, _)),
        assertz(binpkg_exec:last_index_mtime(Repository, Disk)),
        catch(Repository:sync(kb), _, true)
     ;  true
     )
  ;  assertz(binpkg_exec:last_index_mtime(Repository, Disk))
  ).

binpkg_exec:refresh_if_stale(_).


%! binpkg_exec:probe_index_mtime(+Repository, -Mtime) is semidet.
%
% Resolve the repository's `Packages` index path via its OO `get_cache/1`
% accessor, confirm the file exists, and return its current mtime.
% Fails if the binpkg instance is uninitialized or the index is missing.

binpkg_exec:probe_index_mtime(Repository, Mtime) :-
  Repository:get_cache(IndexFile),
  exists_file(IndexFile),
  time_file(IndexFile, Mtime).


%! binpkg_exec:src_entry_cnv(+SrcRepo, +SrcEntry, -Cat, -Name, -Version) is semidet.
%
% Derives (Category, Name, Version) for a source entry id, in the same
% form used by `cache:ordered_entry/5`. Looks up the entry's existing
% cache row (cheap, indexed) instead of re-parsing the atom.

binpkg_exec:src_entry_cnv(SrcRepo, SrcEntry, Cat, Name, Version) :-
  cache:ordered_entry(SrcRepo, SrcEntry, Cat, Name, Version), !.


%! binpkg_exec:pick_best_candidate(+Candidates, -BestEid) is det.
%
% Candidates is a list of `BuildId-Eid` pairs. Sort descending on BuildId
% (msort + reverse beats keysort because BuildId is an integer; we want
% standard numeric order, not term order). Return the head's Eid.

binpkg_exec:pick_best_candidate(Candidates, BestEid) :-
  predsort(binpkg_exec:bid_desc, Candidates, Sorted),
  Sorted = [_-BestEid | _].


%! binpkg_exec:bid_desc(-Order, +A, +B) is det.
%
% predsort/3 comparator: descending integer BuildId. Distinct Eids with
% the same BuildId are kept in input order (predsort would dedupe them
% otherwise, but BuildId is unique per binpkg so this is moot).

binpkg_exec:bid_desc(Order, BidA-_, BidB-_) :-
  ( BidA  >  BidB -> Order = (<)
  ; BidA  <  BidB -> Order = (>)
  ;                  Order = (=)
  ).


% -----------------------------------------------------------------------------
%  Candidate filters
% -----------------------------------------------------------------------------

%! binpkg_exec:candidate_passes_filters(+SrcRepo, +SrcEntry, +BinpkgEid, +Ctx) is semidet.
%
% Conjunction of all eligibility checks for a candidate binpkg:
%   1. On-disk gpkg archive present (index entry may be stale)
%   2. SLOT compatibility
%   3. KEYWORDS acceptability for the host ARCH
%   4. USE compatibility (modulated by `config:binpkg_respect_use/1`)
%   5. Subslot-pin (`:slot/subslot=`) compatibility against the live VDB
%   6. RDEPEND drift (modulated by `config:binpkg_changed_deps/1`)
%
% Each check is a separate predicate so individual policies stay
% testable in isolation.

binpkg_exec:candidate_passes_filters(SrcRepo, SrcEntry, BinpkgEid, Ctx) :-
  binpkg_exec:gpkg_on_disk(BinpkgEid),
  binpkg_exec:slot_compatible(SrcRepo, SrcEntry, BinpkgEid),
  binpkg_exec:keywords_acceptable(BinpkgEid),
  binpkg_exec:use_compatible(SrcRepo, SrcEntry, BinpkgEid, Ctx),
  binpkg_exec:subslot_pins_compatible(BinpkgEid),
  binpkg_exec:rdepend_acceptable(SrcRepo, SrcEntry, BinpkgEid).


%! binpkg_exec:gpkg_on_disk(+BinpkgEntryId) is semidet.
%
% Succeeds when the on-disk gpkg archive for this index entry exists.
% The Packages index can list variants whose files were removed or never
% synced into this session (common on tinderbox-ng shared binpkg caches).

binpkg_exec:gpkg_on_disk(BinpkgEntryId) :-
  binpkg_exec:gpkg_path(BinpkgEntryId, GpkgPath),
  exists_file(GpkgPath).


%! binpkg_exec:slot_compatible(+SrcRepo, +SrcEntry, +BinpkgEid) is semidet.
%
% Both SLOT and (when present) SUBSLOT must match. Storage shapes differ
% per repo type:
%   - binpkg cache: a single `slot` field with combined value (e.g. `'0/1'`).
%                   Crucially, Portage *omits* the SLOT line entirely from
%                   the Packages index when SLOT="0" (the default), as a
%                   space optimization. We treat a missing field as if the
%                   binpkg recorded `SLOT="0"`.
%   - eapi  cache: split into `slot/1` + `subslot/1` rows (e.g. `slot('0')`,
%                  `subslot('1')`).
%
% We split the binpkg's combined value on `/`, then compare the slot
% halves first (always required) and the subslot halves only if both
% sides supply one.

binpkg_exec:slot_compatible(SrcRepo, SrcEntry, BinpkgEid) :-
  ( cache:entry_metadata(binpkg, BinpkgEid, slot, BinpkgSlotFull)
  -> binpkg_exec:split_slot(BinpkgSlotFull, BinpkgSlot, BinpkgSubSlot)
  ;  BinpkgSlot = '0', BinpkgSubSlot = ''     % Portage convention
  ),
  ( query:search(slot(SrcSlot), SrcRepo://SrcEntry)
  -> BinpkgSlot == SrcSlot,
     ( query:search(subslot(SrcSubSlot), SrcRepo://SrcEntry),
       BinpkgSubSlot \== ''
     -> BinpkgSubSlot == SrcSubSlot
     ;  true
     )
  ;  true   % source repo missing slot info -> don't block
  ).


%! binpkg_exec:split_slot(+Combined, -Slot, -SubSlot) is det.
%
% Splits `'0/1'` into `'0'` and `'1'`. If there is no `/`, SubSlot is
% bound to the empty atom.

binpkg_exec:split_slot(Combined, Slot, SubSlot) :-
  ( atomic_list_concat([Slot, SubSlot], '/', Combined) -> true
  ; Slot = Combined, SubSlot = ''
  ).


%! binpkg_exec:keywords_acceptable(+BinpkgEid) is semidet.
%
% Check the binpkg's stored KEYWORDS list contains at least one entry
% acceptable to the host. We approximate by requiring the host ARCH (or
% `~ARCH`) to appear; full ACCEPT_KEYWORDS handling is a follow-up.
% Defensive: if KEYWORDS metadata is missing, accept (assume the
% producer knew what it was doing).

binpkg_exec:keywords_acceptable(BinpkgEid) :-
  ( cache:entry_metadata(binpkg, BinpkgEid, keywords, KwAtom)
  -> binpkg_exec:any_arch_match(KwAtom)
  ;  true
  ).


%! binpkg_exec:any_arch_match(+KwAtom) is semidet.
%
% Splits a `'~alpha amd64 ~arm arm64'` style KEYWORDS atom and succeeds
% iff at least one entry matches the host ARCH (with or without a
% `~` prefix). Host ARCH is resolved via `preference:arch/1` if
% available; falls back to `amd64` for sandboxed test contexts.

binpkg_exec:any_arch_match(KwAtom) :-
  binpkg_exec:host_arch(Arch),
  atomic_list_concat(Tokens, ' ', KwAtom),
  ( memberchk(Arch, Tokens) ; atom_concat('~', Arch, Unstable), memberchk(Unstable, Tokens) ),
  !.


%! binpkg_exec:host_arch(-Arch) is det.
%
% Best-effort host arch lookup. `preference:arch/1` is the canonical
% source on production hosts; we fall back to `amd64` when running in
% standalone test harnesses that don't load the preference module.

binpkg_exec:host_arch(Arch) :-
  ( catch(preference:arch(Arch), _, fail)
  -> true
  ;  Arch = amd64
  ).


%! binpkg_exec:use_compatible(+SrcRepo, +SrcEntry, +BinpkgEid, +Ctx) is semidet.
%
% Compares the planner's resolved USE flag set against the binpkg's
% stored USE, restricted to the ebuild's IUSE so that profile-level
% flags (abi_x86_64, elibc_glibc, kernel_linux, ...) don't pollute the
% comparison.
%
% Modes (see `config:binpkg_respect_use/1`):
%   strict  : sets must be exactly equal (intersected with IUSE)
%   relaxed : binpkg's positive set must be a superset of the planner's
%             positive set (i.e. the binpkg has at least all flags the
%             planner wants enabled; extras are tolerated)
%
% Defensive: if the binpkg has no `use` metadata, accept; if the source
% has no IUSE, accept (degenerate ebuild with no USE flags).

binpkg_exec:use_compatible(SrcRepo, SrcEntry, BinpkgEid, Ctx) :-
  ( cache:entry_metadata(binpkg, BinpkgEid, use, BinpkgUseAtom)
  -> binpkg_exec:tokenize_use(BinpkgUseAtom, BinpkgUseSet)
  ;  BinpkgUseSet = []
  ),
  binpkg_exec:planner_positive_use(SrcRepo, SrcEntry, Ctx, PlannerSet),
  binpkg_exec:ebuild_iuse(SrcRepo, SrcEntry, IuseSet),
  ( IuseSet == []
  -> true
  ;  intersection(BinpkgUseSet, IuseSet, BinpkgIuse),
     intersection(PlannerSet,   IuseSet, PlannerIuse),
     config:binpkg_respect_use(Mode),
     binpkg_exec:use_sets_match(Mode, BinpkgIuse, PlannerIuse)
  ).


%! binpkg_exec:use_sets_match(+Mode, +BinpkgIuse, +PlannerIuse) is semidet.

binpkg_exec:use_sets_match(strict, A, B)  :- !, msort(A, S), msort(B, S).
binpkg_exec:use_sets_match(relaxed, A, B) :- subtract(B, A, []).


%! binpkg_exec:tokenize_use(+UseAtom, -Tokens) is det.
%
% Splits a space-separated USE atom (as stored in the index) into a
% sorted list of atoms with `-` prefixes stripped (binpkg USE only
% records *positive* flags; absence implies negative).

binpkg_exec:tokenize_use(UseAtom, Tokens) :-
  atomic_list_concat(Raw, ' ', UseAtom),
  exclude(==(''), Raw, NonEmpty),
  sort(NonEmpty, Tokens).


%! binpkg_exec:planner_positive_use(+SrcRepo, +SrcEntry, +Ctx, -Tokens) is det.
%
% Reuses `ebuild_exec:collect_use_string/4` (the same logic that builds
% the USE env var for a source build), then keeps only the positive
% tokens (those without a `-` prefix). This guarantees the binpkg
% comparison uses the *exact* USE the planner would have shipped to
% `ebuild merge`.

binpkg_exec:planner_positive_use(SrcRepo, SrcEntry, Ctx, Tokens) :-
  ebuild_exec:collect_use_string(SrcRepo, SrcEntry, Ctx, UseAtom),
  atomic_list_concat(Raw, ' ', UseAtom),
  include(binpkg_exec:is_positive_token, Raw, Positive),
  sort(Positive, Tokens).


%! binpkg_exec:is_positive_token(+Tok) is semidet.

binpkg_exec:is_positive_token(Tok) :-
  Tok \== '',
  \+ atom_concat('-', _, Tok).


%! binpkg_exec:ebuild_iuse(+SrcRepo, +SrcEntry, -IuseSet) is det.
%
% Returns the set of USE flags declared by the ebuild's IUSE. The eapi
% cache stores `+default` flags as the compound term `plus(F)` and plain
% flags as bare atoms; we normalize to a sorted set of atoms.

binpkg_exec:ebuild_iuse(SrcRepo, SrcEntry, IuseSet) :-
  findall(F,
          ( cache:entry_metadata(SrcRepo, SrcEntry, iuse, IuseRaw),
            binpkg_exec:iuse_flag_name(IuseRaw, F)
          ),
          Raw),
  sort(Raw, IuseSet).


%! binpkg_exec:iuse_flag_name(+Raw, -Name) is det.
%
% Normalizes one cache row to its bare flag name. Handles:
%   - `plus(F)`    -> F   (the `+default` form)
%   - `minus(F)`   -> F   (defensive; eapi storage allows this for
%                          profile-stripped flags)
%   - `F` (atom)   -> F   (plain flag)

binpkg_exec:iuse_flag_name(plus(F),  F) :- !.
binpkg_exec:iuse_flag_name(minus(F), F) :- !.
binpkg_exec:iuse_flag_name(F,        F).


% -----------------------------------------------------------------------------
%  Subslot-pin (`:slot/subslot=`) validation against the live VDB
% -----------------------------------------------------------------------------

%! binpkg_exec:subslot_pins_compatible(+BinpkgEid) is semidet.
%
% Mirrors emerge's binpkg-acceptance rule for slot-operator (`:=`)
% dependencies (PMS 8.2 / EAPI 7): when a binpkg was produced its
% recorded DEPEND-family fields embed the resolved
% `<category>/<name>:<slot>/<subslot>=` of every dep that carried a
% `:=` operator. The binpkg is only acceptable if every such pin
% still matches the live VDB record of the depended-on package; a
% subslot mismatch indicates the producer of the binpkg linked
% against an ABI that no longer exists on the consuming host (e.g.
% an OCaml `.cmi` baked against `dev-lang/ocaml:0/5.3.0` is unreadable
% by the live `dev-lang/ocaml:0/5.4.0` compiler).
%
% We walk DEPEND, RDEPEND, PDEPEND, BDEPEND and IDEPEND so any pin in
% any dep field is honoured. Within each field we recurse through
% group constructors (`all_of_group`, `any_of_group`,
% `use_conditional_group`, `exactly_one_of_group`,
% `at_most_one_of_group`) so conditional / nested deps are not
% silently skipped. Per-atom rejection rule is conservative: we only
% reject when we can positively confirm a mismatch (recorded subslot
% present, live install for the dep at the same slot exists, and
% their canonicalised subslots differ). Missing live install, missing
% live subslot or parse failure all degrade to "accept" so this check
% only ever causes regressions when emerge would also reject.

binpkg_exec:subslot_pins_compatible(BinpkgEid) :-
  forall(
    member(Key, [depend, rdepend, pdepend, bdepend, idepend]),
    binpkg_exec:dep_field_subslot_compatible(BinpkgEid, Key)).


%! binpkg_exec:dep_field_subslot_compatible(+BinpkgEid, +Key) is semidet.
%
% Reads one dep field (lowercased atom: `depend`, `rdepend`, ...) from
% the binpkg cache, parses it with the EAPI dependency grammar, and
% requires every embedded `:slot/subslot=` pin to be satisfied. A
% missing field accepts. A parse failure also accepts (defensive: the
% binpkg may be in a slightly newer format than our grammar handles,
% and we would rather defer to emerge's logic than over-reject).

binpkg_exec:dep_field_subslot_compatible(BinpkgEid, Key) :-
  ( cache:entry_metadata(binpkg, BinpkgEid, Key, ValueAtom),
    ValueAtom \== ''
  -> ( binpkg_exec:parse_dep_value(ValueAtom, Deps)
     -> binpkg_exec:walk_deps_subslot(Deps)
     ;  true
     )
  ;  true
  ).


%! binpkg_exec:parse_dep_value(+ValueAtom, -Deps) is semidet.
%
% Parses a verbatim DEPEND-family atom (as stored in the binpkg
% `Packages` index) into a list of EAPI dependency terms. The
% pseudo `R://E` context expected by `eapi:depend//2` is filled with
% anonymous variables -- it is only used by the grammar to thread
% provenance back into error messages and is not consulted by the
% structural shape we walk here.

binpkg_exec:parse_dep_value(ValueAtom, Deps) :-
  ( atom(ValueAtom)   -> atom_codes(ValueAtom, Codes)
  ; string(ValueAtom) -> string_codes(ValueAtom, Codes)
  ),
  catch(phrase(eapi:depend(_://_, Deps), Codes), _, fail).


%! binpkg_exec:walk_deps_subslot(+Deps) is semidet.
%
% Walks a parsed dependency list, descending into group constructors
% and validating any atom-level `:slot/subslot=` pin against the live
% VDB. Succeeds iff every confirmed pin matches.

binpkg_exec:walk_deps_subslot([]).
binpkg_exec:walk_deps_subslot([D|Ds]) :-
  binpkg_exec:walk_dep_subslot(D),
  binpkg_exec:walk_deps_subslot(Ds).


%! binpkg_exec:walk_dep_subslot(+Dep) is semidet.
%
% Per-node visitor. Group constructors recurse into their child list;
% `package_dependency/8` atoms are validated against the live VDB;
% anything else (blocker-only atoms, virtuals already lowered, etc.)
% passes through.

binpkg_exec:walk_dep_subslot(package_dependency(_, _, C, P, _, _, Slot, _)) :- !,
  binpkg_exec:check_atom_subslot_pin(C, P, Slot).

binpkg_exec:walk_dep_subslot(all_of_group(D)) :- !,
  binpkg_exec:walk_deps_subslot(D).

binpkg_exec:walk_dep_subslot(any_of_group(D)) :- !,
  binpkg_exec:walk_deps_subslot(D).

binpkg_exec:walk_dep_subslot(use_conditional_group(_, _, _, D)) :- !,
  binpkg_exec:walk_deps_subslot(D).

binpkg_exec:walk_dep_subslot(exactly_one_of_group(D)) :- !,
  binpkg_exec:walk_deps_subslot(D).

binpkg_exec:walk_dep_subslot(at_most_one_of_group(D)) :- !,
  binpkg_exec:walk_deps_subslot(D).

binpkg_exec:walk_dep_subslot(_).


%! binpkg_exec:check_atom_subslot_pin(+Category, +Name, +Slot) is semidet.
%
% Validates one dependency atom's slot list. The grammar emits Slot
% as a list that, for a resolved `:N/M=` pin, contains both
% `subslot(M)` and the `equal` marker. Anything else (plain `:N`,
% bare `:=`, `:*`, missing slot list) carries no concrete subslot
% pin and is accepted unconditionally. When a pin IS present, we
% require either no live install of (Category, Name) at the recorded
% slot, no recorded live subslot, or live-vs-recorded subslot
% equality (after `canon_slot` normalisation).

binpkg_exec:check_atom_subslot_pin(Cat, Name, Slot) :-
  ( is_list(Slot),
    memberchk(equal, Slot),
    memberchk(subslot(RecSubRaw), Slot)
  -> ( memberchk(slot(RecSlotRaw), Slot)
     -> candidate:canon_slot(RecSlotRaw, RecSlot)
     ;  RecSlot = (-)
     ),
     candidate:canon_slot(RecSubRaw, RecSub),
     binpkg_exec:live_subslot_matches(Cat, Name, RecSlot, RecSub)
  ;  true
  ).


%! binpkg_exec:live_subslot_matches(+Category, +Name, +RecSlot, +RecSub) is semidet.
%
% Looks up the live (VDB) install of (Category, Name). If RecSlot is
% bound to a concrete atom we restrict to installs in that slot;
% otherwise the first installed (Category, Name) is considered.
% Succeeds (accept) when there is no install or the install records
% no subslot; otherwise requires `RecSub` to equal the live subslot.

binpkg_exec:live_subslot_matches(Cat, Name, RecSlot, RecSub) :-
  ( binpkg_exec:find_live_install_for_slot(Cat, Name, RecSlot, LiveEntry)
  -> ( query:search(subslot(LiveSubRaw), pkg://LiveEntry)
     -> candidate:canon_slot(LiveSubRaw, LiveSub),
        LiveSub == RecSub
     ;  true
     )
  ;  true
  ).


%! binpkg_exec:find_live_install_for_slot(+Cat, +Name, +RecSlot, -Entry) is semidet.
%
% Resolves the live VDB entry that matches the recorded slot. When
% `RecSlot == (-)` (no slot constraint recorded) the first installed
% entry wins. Otherwise we backtrack through every installed entry
% of (Cat, Name) and pick the first whose canonicalised slot equals
% RecSlot.

binpkg_exec:find_live_install_for_slot(Cat, Name, (-), Entry) :- !,
  query:search([name(Name), category(Cat), installed(true)], pkg://Entry).

binpkg_exec:find_live_install_for_slot(Cat, Name, RecSlot, Entry) :-
  query:search([name(Name), category(Cat), installed(true)], pkg://Entry),
  ( query:search(slot(LiveSlotRaw), pkg://Entry)
  -> candidate:canon_slot(LiveSlotRaw, LiveSlot),
     LiveSlot == RecSlot
  ;  true
  ),
  !.


%! binpkg_exec:rdepend_acceptable(+SrcRepo, +SrcEntry, +BinpkgEid) is semidet.
%
% Checks whether the binpkg's recorded RDEPEND matches the current
% ebuild's RDEPEND. Mirrors emerge's `--binpkg-changed-deps`. Modes:
%   skip : refuse the binpkg on RDEPEND drift
%   warn : accept but log
%
% This is a defensive check for cache-rot scenarios (ebuild updated
% post-binpkg-build). For the initial cut we always accept (warn-only
% behaviour); a stricter implementation requires DEPEND atom parsing
% which is out of scope for phase 4.

binpkg_exec:rdepend_acceptable(_SrcRepo, _SrcEntry, _BinpkgEid).


% -----------------------------------------------------------------------------
%  Execution: drive `ebuild qmerge`
% -----------------------------------------------------------------------------

%! binpkg_exec:execute(+Action, +SrcRepo, +SrcEntry, +BinpkgEntryId, +Ctx, -Outcome) is det.
%
% Performs the actual binary merge for a chosen binpkg. Outcome is one
% of:
%   done                     -- qmerge exited 0; package is installed
%   failed(qmerge_exit(N))   -- qmerge exited non-zero
%   failed(Reason)           -- setup step failed (missing gpkg, extract
%                               error, etc.); Reason is a printable atom
%
% The Action argument is accepted but not currently used for branching:
% install/reinstall/update/downgrade all map to the same qmerge call
% (qmerge handles the unmerge-old / merge-new sequence internally via
% `dblink.merge`).

binpkg_exec:execute(_Action, SrcRepo, SrcEntry, BinpkgEntryId, Ctx, Outcome) :-
  catch(
    ( binpkg_exec:execute_inner(SrcRepo, SrcEntry, BinpkgEntryId, Ctx, Outcome)
    -> true
    ;  Outcome = failed(inner_aborted)
    ),
    Err,
    ( format(user_error, 'binpkg_exec:execute exception: ~q~n', [Err]),
      Outcome = failed(exception(Err))
    )
  ).


%! binpkg_exec:execute_inner(+SrcRepo, +SrcEntry, +BinpkgEntryId, +Ctx, -Outcome) is det.
%
% The non-catch'd body of `execute/6`. Splits resolution from execution
% so unit tests can stub one half without touching the other.

binpkg_exec:execute_inner(SrcRepo, SrcEntry, BinpkgEntryId, Ctx, Outcome) :-
  % --- resolve all paths ---------------------------------------------------
  binpkg_exec:gpkg_path(BinpkgEntryId, GpkgPath),
  binpkg_exec:source_ebuild_path(SrcRepo, SrcEntry, EbuildPath),
  binpkg_exec:builddir_for(SrcRepo, SrcEntry, BuildDir),
  binpkg_exec:inner_name_for(BinpkgEntryId, InnerName),

  % --- preconditions + qmerge (single branch tree; never fall through) -----
  ( \+ exists_file(GpkgPath)
  -> Outcome = failed(missing_gpkg(GpkgPath))
  ; \+ exists_file(EbuildPath)
  -> Outcome = failed(missing_ebuild(EbuildPath))
  ; binpkg_extract:prepare_builddir(GpkgPath, InnerName, BuildDir)
  -> ebuild_exec:collect_use_string(SrcRepo, SrcEntry, Ctx, UseString),
     binpkg_exec:run_qmerge(EbuildPath, GpkgPath, BuildDir, UseString, ExitCode),
     ( ExitCode =:= 0
     -> Outcome = done
     ;  Outcome = failed(qmerge_exit(ExitCode))
     )
  ;  Outcome = failed(extract_failed(GpkgPath))
  ).


% -----------------------------------------------------------------------------
%  Path resolution
% -----------------------------------------------------------------------------

%! binpkg_exec:gpkg_path(+BinpkgEntryId, -GpkgPath) is semidet.
%
% Composes the absolute path to the gpkg file from the binpkg repo's
% location and the entry's `path` metadata field (relative path inside
% the cache, e.g. `'app-misc/jq/jq-1.8.1-9.gpkg.tar'`).

binpkg_exec:gpkg_path(BinpkgEntryId, GpkgPath) :-
  cache:entry_metadata(binpkg, BinpkgEntryId, path, RelPathAtom),
  binpkg:get_location(Root),
  os:compose_path(Root, RelPathAtom, GpkgPath).


%! binpkg_exec:source_ebuild_path(+SrcRepo, +SrcEntry, -EbuildPath) is semidet.
%
% Defers to the source repository's `get_ebuild_file/2` method to obtain
% the absolute path of the source ebuild. The `ebuild` CLI requires this
% path to live inside a recognized portage tree, so we cannot simply
% point at a copy in build-info/.

binpkg_exec:source_ebuild_path(SrcRepo, SrcEntry, EbuildPath) :-
  SrcRepo:get_ebuild_file(SrcEntry, EbuildPath).


%! binpkg_exec:builddir_for(+SrcRepo, +SrcEntry, -BuildDir) is det.
%
% Computes `<config:build_root>/<category>/<pf>`, the canonical
% PORTAGE_BUILDDIR layout. Uses the source entry (not the binpkg entry)
% for cat/pf so concurrent binpkg merges of different BUILD_IDs share a
% builddir -- they would race anyway, since they merge to the same VDB.

binpkg_exec:builddir_for(SrcRepo, SrcEntry, BuildDir) :-
  cache:ordered_entry(SrcRepo, SrcEntry, Cat, Name, Version),
  eapi:version_full(Version, Pv),
  atomic_list_concat([Name, '-', Pv], Pf),
  config:build_root(Root),
  os:compose_path([Root, Cat, Pf], BuildDir).


%! binpkg_exec:inner_name_for(+BinpkgEntryId, -InnerName) is det.
%
% Strips the leading `<category>/` from a binpkg entry id to recover the
% inner directory name embedded in the gpkg (e.g.
% `'app-misc/jq-1.8.1-9'` -> `'jq-1.8.1-9'`).

binpkg_exec:inner_name_for(BinpkgEntryId, InnerName) :-
  atomic_list_concat([_Cat, Inner], '/', BinpkgEntryId),
  InnerName = Inner.


% -----------------------------------------------------------------------------
%  qmerge invocation
% -----------------------------------------------------------------------------

%! binpkg_exec:run_qmerge(+EbuildPath, +GpkgPath, +BuildDir, +UseString, -ExitCode) is det.
%
% Spawns `ebuild --skip-manifest <EbuildPath> qmerge` with the canonical
% binpkg env var set:
%   MERGE_TYPE=binary           tells ebuild.sh this is a binary merge
%   PORTAGE_BINPKG_FILE=<gpkg>  binpkg path (used for VDB BINPKGMD5)
%   PORTAGE_BUILDDIR=<dir>      where image/, build-info/, temp/ live
%   USE=<planner USE>           planner's resolved USE (matches binpkg's)
%
% Stdout/stderr inherit the parent's terminal so qmerge's progress
% messages flow through (matches `ebuild_exec:run_phases/4` style).
% PATH and HOME are passed through but other environment is NOT
% sanitized -- if the user has portage-related env vars set we honor
% them, mirroring how `ebuild ... merge` already behaves under
% `ebuild_exec`.

binpkg_exec:run_qmerge(EbuildPath, GpkgPath, BuildDir, UseString, ExitCode) :-
  config:ebuild_command(EbuildCmd),
  process_create(
    path(EbuildCmd),
    ['--skip-manifest', EbuildPath, 'qmerge'],
    [process(Pid),
     environment(['MERGE_TYPE'='binary',
                  'PORTAGE_BINPKG_FILE'=GpkgPath,
                  'PORTAGE_BUILDDIR'=BuildDir,
                  'USE'=UseString])]),
  process_wait(Pid, exit(ExitCode)).
