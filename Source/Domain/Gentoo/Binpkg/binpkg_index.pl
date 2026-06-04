/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> BINPKG_INDEX
RFC822-style parser for Portage's `$PKGDIR/Packages` binary package index.

The Packages file is a single flat file at the root of `$PKGDIR` that lists
every binary package present in the cache. Each binpkg variant (one per
`BUILD_ID` for `binpkg-multi-instance` setups) becomes a record. Records
are separated by blank lines, and each record consists of `KEY: VALUE`
lines (one per metadata field).

The first record in the file is the header (it carries `ARCH`,
`ACCEPT_KEYWORDS`, etc., but no `CPV`). Every subsequent record describes
one BUILD_ID and contains at minimum `CPV`, `PATH`, `USE`, `IUSE`, `SLOT`
and integrity fields (`MD5`, `SIZE`, `MTIME`).

This module is a *pure parser*: it reads the file and yields an in-memory
representation. It performs no cache writes, no asserts, and no validation
beyond `KEY: VALUE` shape recognition. Type-specific normalization
(integer parsing for `BUILD_ID`/`SIZE`/`MTIME`, list splitting for
`USE`/`KEYWORDS`, etc.) is the responsibility of the caller -- typically
`repository:sync(kb)` for type=`binpkg`.

Each record is returned as a list of `Key-Value` pairs where `Key` is a
lowercased atom (e.g. `cpv`, `use`, `build_id`) and `Value` is the
verbatim atom from the file. Duplicate keys preserve order.
*/

:- module(binpkg_index, []).

% =============================================================================
%  BINPKG_INDEX declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Top-level file parser
% -----------------------------------------------------------------------------

%! binpkg_index:parse_file(+Path, -Header, -Records) is det.
%
% Reads `$PKGDIR/Packages` at Path and splits it into Header and Records.
% Header is the leading record (no `cpv` field, contains repository-wide
% metadata such as `ARCH`, `ACCEPT_KEYWORDS`, `VERSION`, `TIMESTAMP`).
% Records is a list of per-BUILD_ID records, each a list of `Key-Value`
% pairs. If the file does not exist or contains no records, both Header
% and Records are bound to the empty list.

binpkg_index:parse_file(Path, Header, Records) :-
  ( exists_file(Path)
  -> setup_call_cleanup(
       open(Path, read, Stream, [encoding(utf8)]),
       binpkg_index:parse_loop(Stream, [], [], AllRecords),
       close(Stream)
     )
  ;  AllRecords = []
  ),
  ( AllRecords = [Header | Records]
  -> true
  ;  Header = [], Records = []
  ).


%! binpkg_index:parse_stream(+Stream, -Header, -Records) is det.
%
% Variant of parse_file/3 that reads from an already-open Stream. Useful
% for tests and for parsing fragments piped through `process_create`.

binpkg_index:parse_stream(Stream, Header, Records) :-
  binpkg_index:parse_loop(Stream, [], [], AllRecords),
  ( AllRecords = [Header | Records]
  -> true
  ;  Header = [], Records = []
  ).


% -----------------------------------------------------------------------------
%  Streaming line accumulator
% -----------------------------------------------------------------------------

%! binpkg_index:parse_loop(+Stream, +CurrentLinesRev, +AccRecordsRev, -AllRecords) is det.
%
% Tail-recursive line reader. `CurrentLinesRev` accumulates lines of the
% record currently being read (in reverse order). On a blank line we
% finalize the current record and push it onto `AccRecordsRev`. On EOF we
% finalize the trailing record (if any) and reverse the accumulator to
% restore source order.

binpkg_index:parse_loop(Stream, Cur, Acc, All) :-
  read_line_to_string(Stream, Line),
  ( Line == end_of_file
  -> binpkg_index:finalize(Cur, Acc, All)
  ;  Line == ""
  -> binpkg_index:flush_record(Cur, Acc, NewAcc),
     binpkg_index:parse_loop(Stream, [], NewAcc, All)
  ;  binpkg_index:parse_loop(Stream, [Line | Cur], Acc, All)
  ).


%! binpkg_index:flush_record(+CurrentLinesRev, +AccRecordsRev, -NewAccRev) is det.
%
% If CurrentLinesRev is empty (consecutive blank lines) the accumulator
% passes through unchanged. Otherwise the lines are reversed back into
% source order, parsed into a record, and the record is prepended to the
% accumulator.

binpkg_index:flush_record([], Acc, Acc) :- !.

binpkg_index:flush_record(Cur, Acc, [Record | Acc]) :-
  reverse(Cur, OrderedLines),
  binpkg_index:lines_to_record(OrderedLines, Record).


%! binpkg_index:finalize(+CurrentLinesRev, +AccRecordsRev, -AllRecords) is det.
%
% End-of-stream finalizer: flush the trailing in-progress record (if any)
% and reverse the accumulator so records appear in source order.

binpkg_index:finalize(Cur, Acc, All) :-
  binpkg_index:flush_record(Cur, Acc, FinalAcc),
  reverse(FinalAcc, All).


% -----------------------------------------------------------------------------
%  Record line parser
% -----------------------------------------------------------------------------

%! binpkg_index:lines_to_record(+Lines, -Record) is det.
%
% Convert a list of `KEY: VALUE` lines into a list of `Key-Value` pairs.
% Lines that don't match the expected shape are silently skipped (this
% matches Portage's tolerance for blank/comment lines mid-record).

binpkg_index:lines_to_record([], []).

binpkg_index:lines_to_record([Line | Rest], [Key-Value | Pairs]) :-
  binpkg_index:split_kv(Line, Key, Value), !,
  binpkg_index:lines_to_record(Rest, Pairs).

binpkg_index:lines_to_record([_BadLine | Rest], Pairs) :-
  binpkg_index:lines_to_record(Rest, Pairs).


%! binpkg_index:split_kv(+Line, -Key, -Value) is semidet.
%
% Splits a `"KEY: VALUE"` line. Key is normalized to a lowercase atom so
% callers can match it directly (e.g. `cpv`, `use`, `build_id`). Value is
% the verbatim atom from the file (no further normalization). Fails on
% lines that lack the `: ` separator.

binpkg_index:split_kv(Line, Key, Value) :-
  sub_string(Line, Sep, 2, _, ": "), !,
  sub_string(Line, 0, Sep, _, KeyStr),
  AfterSep is Sep + 2,
  sub_string(Line, AfterSep, _, 0, ValueStr),
  string_lower(KeyStr, KeyLower),
  atom_string(Key, KeyLower),
  atom_string(Value, ValueStr).


% -----------------------------------------------------------------------------
%  Record helpers (CPV / BUILD_ID / category-name-version derivation)
% -----------------------------------------------------------------------------

%! binpkg_index:record_get(+Record, +Key, -Value) is semidet.
%
% Look up the first occurrence of Key in Record. Fails if Key is absent.

binpkg_index:record_get(Record, Key, Value) :-
  memberchk(Key-Value, Record).


%! binpkg_index:record_cpv(+Record, -Cpv) is semidet.
%
% Returns the package's CPV (e.g. `'app-misc/jq-1.8.1'`) from the `cpv`
% field. The header record has no `cpv` field and therefore fails -- the
% caller can use this to filter the header out cheaply.

binpkg_index:record_cpv(Record, Cpv) :-
  binpkg_index:record_get(Record, cpv, Cpv).


%! binpkg_index:record_build_id(+Record, -BuildId) is semidet.
%
% Extracts the integer BUILD_ID. Two paths are tried, in order:
%
%   1. The explicit `build_id` field (always present for binpkgs produced
%      under `FEATURES=binpkg-multi-instance`, which is the default for
%      modern Portage profiles).
%   2. The trailing `-N` suffix in the `path` field (e.g.
%      `app-misc/jq/jq-1.8.1-9.gpkg.tar` -> 9), as a fallback for legacy
%      single-instance binpkgs that omit `build_id`.
%
% Fails if neither path yields an integer.

binpkg_index:record_build_id(Record, BuildId) :-
  binpkg_index:record_get(Record, build_id, BidAtom), !,
  atom_number(BidAtom, BuildId),
  integer(BuildId).

binpkg_index:record_build_id(Record, BuildId) :-
  binpkg_index:record_get(Record, path, Path),
  binpkg_index:path_build_id(Path, BuildId).


%! binpkg_index:path_build_id(+Path, -BuildId) is semidet.
%
% Parse a relative gpkg path like `'app-misc/jq/jq-1.8.1-9.gpkg.tar'` and
% extract the trailing BUILD_ID integer. Fails on paths that don't follow
% the multi-instance naming convention.

binpkg_index:path_build_id(Path, BuildId) :-
  atom_string(Path, PathStr),
  ( sub_string(PathStr, _, 9, 0, ".gpkg.tar")
  -> Suffix = ".gpkg.tar"
  ;  sub_string(PathStr, _, 5, 0, ".xpak"),
     Suffix = ".xpak"
  ),
  string_length(Suffix, SLen),
  string_length(PathStr, PLen),
  BaseLen is PLen - SLen,
  sub_string(PathStr, 0, BaseLen, _, Base),
  binpkg_index:trailing_dash_int(Base, BuildId).


%! binpkg_index:trailing_dash_int(+Atom, -Int) is semidet.
%
% Extract the integer that follows the last `-` in Atom (e.g. `"jq-1.8.1-9"`
% -> 9). Fails if there is no trailing integer or no `-` separator.

binpkg_index:trailing_dash_int(Atom, Int) :-
  atom_string(Atom, Str),
  string_length(Str, Len),
  binpkg_index:last_dash_position(Str, Len, Pos),
  Pos > 0,
  After is Pos + 1,
  TailLen is Len - After,
  TailLen > 0,
  sub_string(Str, After, TailLen, 0, TailStr),
  number_string(Int, TailStr),
  integer(Int).


%! binpkg_index:last_dash_position(+Str, +Len, -Pos) is semidet.
%
% Find the rightmost `-` in Str. Pos is its 0-based index. Fails if Str
% contains no dash.

binpkg_index:last_dash_position(Str, Len, Pos) :-
  Last is Len - 1,
  binpkg_index:last_dash_scan(Str, Last, Pos).

binpkg_index:last_dash_scan(_Str, -1, _Pos) :- !, fail.
binpkg_index:last_dash_scan(Str, Idx, Idx) :-
  sub_string(Str, Idx, 1, _, "-"), !.
binpkg_index:last_dash_scan(Str, Idx, Pos) :-
  Prev is Idx - 1,
  binpkg_index:last_dash_scan(Str, Prev, Pos).


%! binpkg_index:record_entry_id(+Record, -EntryId) is semidet.
%
% Compose the cache entry id for a record: `<CPV>-<BUILD_ID>` (e.g.
% `'app-misc/jq-1.8.1-9'`). This naturally matches the gpkg filename and
% gives every multi-instance variant a unique id under the
% `cache:ordered_entry/5` model. Fails on the header record (no cpv).

binpkg_index:record_entry_id(Record, EntryId) :-
  binpkg_index:record_cpv(Record, Cpv),
  binpkg_index:record_build_id(Record, Bid),
  atomic_list_concat([Cpv, '-', Bid], EntryId).


%! binpkg_index:record_split_cpv(+Record, -Category, -Name, -Version) is semidet.
%
% Splits the record's `cpv` into Category, Name, and parsed Version
% (a `version/7` compound, as produced by `eapi:packageversion/3`).
% Fails on the header record.

binpkg_index:record_split_cpv(Record, Category, Name, Version) :-
  binpkg_index:record_cpv(Record, Cpv),
  binpkg_index:split_cpv(Cpv, Category, Pf),
  eapi:packageversion(Pf, Name, Version).


%! binpkg_index:split_cpv(+Cpv, -Category, -Pf) is semidet.
%
% Splits `'app-misc/jq-1.8.1'` into `'app-misc'` and `'jq-1.8.1'`. Fails
% if Cpv contains no `/`.

binpkg_index:split_cpv(Cpv, Category, Pf) :-
  atomic_list_concat([Category, Pf], '/', Cpv),
  Category \== Cpv.


% -----------------------------------------------------------------------------
%  Diagnostics
% -----------------------------------------------------------------------------

%! binpkg_index:record_count(+Path, -Count) is det.
%
% Convenience: count the records (excluding the header) in the index at
% Path. Useful for quick sanity checks during development.

binpkg_index:record_count(Path, Count) :-
  binpkg_index:parse_file(Path, _Header, Records),
  length(Records, Count).
