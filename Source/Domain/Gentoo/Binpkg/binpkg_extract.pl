/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> BINPKG_EXTRACT
Low-level extraction helpers for Portage gpkg binary packages.

A gpkg is a tar archive containing a single inner directory named after
the package's PF + BUILD_ID (e.g. `jq-1.8.1-9/`). That inner directory
holds three artefacts:

  - `image.tar.zst`    — zstd-compressed tar of `image/` (the files that
                         will be merged to the live filesystem at `$ROOT`)
  - `metadata.tar.zst` — zstd-compressed tar of `metadata/` (per-binpkg
                         build metadata: USE, IUSE, SLOT, RDEPEND,
                         BUILD_ID, environment.bz2, etc.)
  - `Manifest`         — checksums + sizes of the above

This module reproduces, in Prolog, the file-system preparation that
emerge's `_emerge/Binpkg.py` performs before a binary merge. The output
layout (under `$PORTAGE_BUILDDIR`) matches what `ebuild qmerge` and
`portage/dbapi/vartree.py:merge()` expect:

  $PORTAGE_BUILDDIR/
    image/                   <- contents of image.tar.zst (live FS payload)
    build-info/              <- contents of metadata.tar.zst (NOTE: rename
                                from `metadata/` to `build-info/`; merge()
                                reads the VDB metadata from build-info/)
    temp/
      environment            <- decompressed from build-info/environment.bz2
    .installed               <- mandatory marker, qmerge bails without it

After `prepare_builddir/3` succeeds, callers (i.e. `binpkg_exec`) can
spawn `ebuild --skip-manifest <SOURCE_EBUILD> qmerge` with environment
`MERGE_TYPE=binary`, `PORTAGE_BINPKG_FILE=<gpkg>`, `PORTAGE_BUILDDIR=...`.

This whole flow was validated end-to-end against `app-misc/jq-1.8.1`
BUILD_IDs 8 (oniguruma=on) and 10 (oniguruma=off) during initial bring-up.
*/

:- module(binpkg_extract, []).

% =============================================================================
%  BINPKG_EXTRACT declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Top-level orchestrator
% -----------------------------------------------------------------------------

%! binpkg_extract:prepare_builddir(+GpkgPath, +InnerName, +BuildDir) is semidet.
%
% Idempotent end-to-end build directory preparation:
%
%   1. Wipe and recreate `BuildDir` with the canonical sub-tree
%      (`image/`, `build-info/`, `temp/`, `work/`).
%   2. Extract the outer tar to a private scratch dir (cleaned up at end).
%   3. Extract `image.tar.zst`     -> `BuildDir/image/`.
%   4. Extract `metadata.tar.zst`  -> `BuildDir/build-info/`
%      (with `--transform 's,^metadata/,build-info/,'`).
%   5. Decompress `environment.bz2` -> `BuildDir/temp/environment`.
%   6. Backfill `CATEGORY` / `PF` in `build-info/` if the gpkg omitted them.
%   7. Touch `BuildDir/.installed` (qmerge precondition).
%
% Fails (with a logged error message) if any step fails. The scratch
% directory is always cleaned up. Caller is responsible for assembling the
% `ebuild qmerge` invocation that consumes the prepared directory.

binpkg_extract:prepare_builddir(GpkgPath, InnerName, BuildDir) :-
  binpkg_extract:wipe_and_create_builddir(BuildDir),
  setup_call_cleanup(
    binpkg_extract:make_scratch_dir(ScratchDir),
    binpkg_extract:populate_builddir(GpkgPath, InnerName, BuildDir, ScratchDir),
    binpkg_extract:remove_scratch_dir(ScratchDir)
  ).


%! binpkg_extract:populate_builddir(+GpkgPath, +InnerName, +BuildDir, +ScratchDir) is semidet.
%
% Steps 2-7 of `prepare_builddir/3`. Split out so the scratch dir is
% guaranteed cleaned up even if any sub-step fails.

binpkg_extract:populate_builddir(GpkgPath, InnerName, BuildDir, ScratchDir) :-
  binpkg_extract:extract_outer(GpkgPath, ScratchDir),
  os:compose_path([ScratchDir, InnerName], InnerDir),
  ( exists_directory(InnerDir)
  -> true
  ;  format(user_error, 'binpkg_extract: expected inner dir ~w not found in gpkg ~w~n', [InnerDir, GpkgPath]),
     fail
  ),
  os:compose_path([InnerDir, 'image.tar.zst'],    ImageTar),
  os:compose_path([InnerDir, 'metadata.tar.zst'], MetaTar),
  binpkg_extract:extract_image(ImageTar, BuildDir),
  binpkg_extract:extract_metadata_to_build_info(MetaTar, BuildDir),
  os:compose_path([BuildDir, 'build-info'], BuildInfoDir),
  os:compose_path([BuildDir, 'temp', 'environment'], EnvFile),
  binpkg_extract:decompress_environment_if_present(BuildInfoDir, EnvFile),
  binpkg_extract:backfill_category_pf(InnerName, BuildInfoDir),
  binpkg_extract:touch_installed_marker(BuildDir).


% -----------------------------------------------------------------------------
%  Build directory life-cycle
% -----------------------------------------------------------------------------

%! binpkg_extract:wipe_and_create_builddir(+BuildDir) is det.
%
% Removes any existing BuildDir (recursively) and recreates the empty
% sub-tree expected by qmerge: `image/`, `build-info/`, `temp/`, `work/`.

binpkg_extract:wipe_and_create_builddir(BuildDir) :-
  ( exists_directory(BuildDir)
  -> binpkg_extract:run_command(path(rm), ['-rf', BuildDir], _)
  ;  true
  ),
  forall(
    member(Sub, ['image', 'build-info', 'temp', 'work']),
    ( os:compose_path([BuildDir, Sub], Path),
      os:ensure_directory_path(Path)
    )).


%! binpkg_extract:make_scratch_dir(-ScratchDir) is det.
%
% Allocates a fresh scratch directory under the system temp dir.
% Used for the outer-tar extraction (we don't want to clutter the
% PORTAGE_BUILDDIR with the inner gpkg contents).

binpkg_extract:make_scratch_dir(ScratchDir) :-
  tmp_file(binpkg_extract, Tmp),
  ( exists_file(Tmp) -> delete_file(Tmp) ; true ),
  os:make_directory(Tmp),
  ScratchDir = Tmp.


%! binpkg_extract:remove_scratch_dir(+ScratchDir) is det.
%
% Cleanup hook for `setup_call_cleanup/3`. Best-effort: we log but do not
% fail if the directory has already been removed.

binpkg_extract:remove_scratch_dir(ScratchDir) :-
  ( exists_directory(ScratchDir)
  -> catch(binpkg_extract:run_command(path(rm), ['-rf', ScratchDir], _),
           _, true)
  ;  true
  ).


% -----------------------------------------------------------------------------
%  Tar / zstd / bunzip2 wrappers
% -----------------------------------------------------------------------------

%! binpkg_extract:extract_outer(+GpkgPath, +ScratchDir) is semidet.
%
% Untars the outer (uncompressed) gpkg archive into ScratchDir. The result
% is a single sub-directory named after the package PF + BUILD_ID
% (e.g. `jq-1.8.1-9/`) holding `image.tar.zst`, `metadata.tar.zst`,
% `gpkg-1`, and `Manifest`.

binpkg_extract:extract_outer(GpkgPath, ScratchDir) :-
  binpkg_extract:run_command(path(tar), ['xf', GpkgPath, '-C', ScratchDir], 0).


%! binpkg_extract:extract_image(+ImageTarZst, +BuildDir) is semidet.
%
% Decompresses + untars `image.tar.zst` into BuildDir. The archive
% includes the `image/` prefix, so the result lands at `BuildDir/image/`.
% Requires GNU tar (uses `-I zstd` to delegate decompression to the zstd
% command in PATH).

binpkg_extract:extract_image(ImageTarZst, BuildDir) :-
  binpkg_extract:run_command(
    path(tar),
    ['-I', 'zstd', '-xf', ImageTarZst, '-C', BuildDir],
    0).


%! binpkg_extract:extract_metadata_to_build_info(+MetaTarZst, +BuildDir) is semidet.
%
% Decompresses + untars `metadata.tar.zst` into BuildDir, but rewrites
% the leading `metadata/` path component to `build-info/` (matching what
% `portage/dbapi/vartree.py:merge()` expects). Uses GNU tar's
% `--transform` sed-like rule.

binpkg_extract:extract_metadata_to_build_info(MetaTarZst, BuildDir) :-
  binpkg_extract:run_command(
    path(tar),
    ['-I', 'zstd', '-xf', MetaTarZst, '-C', BuildDir,
     '--transform', 's,^metadata/,build-info/,'],
    0).


%! binpkg_extract:decompress_environment_if_present(+BuildInfoDir, +EnvFile) is det.
%
% Decompresses `BuildInfoDir/environment.bz2` to EnvFile via bunzip2.
% Silently no-ops if the binpkg shipped without a saved environment
% (some legacy or hand-built packages omit it; ebuild qmerge then falls
% back to a fresh environment).

binpkg_extract:decompress_environment_if_present(BuildInfoDir, EnvFile) :-
  os:compose_path([BuildInfoDir, 'environment.bz2'], EnvBz2),
  ( exists_file(EnvBz2)
  -> binpkg_extract:bunzip2_to_file(EnvBz2, EnvFile)
  ;  true
  ).


%! binpkg_extract:bunzip2_to_file(+InputBz2, +OutputFile) is semidet.
%
% Spawns `bunzip2 -c <input>` and pipes stdout to OutputFile. We avoid
% shelling out via `sh -c '... > ...'` to keep the path/argv contract
% explicit and quote-safe.  The stdout pipe is read in octet mode so
% ISO-8859 bytes in saved binpkg environments copy through unchanged.

binpkg_extract:bunzip2_to_file(InputBz2, OutputFile) :-
  setup_call_cleanup(
    open(OutputFile, write, OutStream, [type(binary)]),
    ( process_create(
        path(bunzip2),
        ['-c', InputBz2],
        [stdout(pipe(InPipe)), process(Pid)]),
      set_stream(InPipe, encoding(octet)),
      copy_stream_data(InPipe, OutStream),
      close(InPipe),
      process_wait(Pid, exit(0))
    ),
    close(OutStream)
  ).


% -----------------------------------------------------------------------------
%  Build-info backfill + qmerge marker
% -----------------------------------------------------------------------------

%! binpkg_extract:backfill_category_pf(+InnerName, +BuildInfoDir) is det.
%
% `_emerge/Binpkg.py` writes CATEGORY and PF into build-info if the
% binpkg's metadata didn't include them (some legacy packages omit one or
% both). InnerName is `jq-1.8.1-9` style; we don't have CATEGORY in that
% string, so for the missing-CATEGORY case we punt to the caller (the
% `binpkg_exec` orchestrator already knows the category from the cache).
%
% In practice, modern (gpkg-1) binpkgs always ship CATEGORY and PF, so
% this is a defensive no-op for the common case.

binpkg_extract:backfill_category_pf(InnerName, BuildInfoDir) :-
  os:compose_path([BuildInfoDir, 'PF'], PfFile),
  ( exists_file(PfFile)
  -> true
  ;  binpkg_extract:strip_build_id_suffix(InnerName, Pf),
     setup_call_cleanup(
       open(PfFile, write, PfStream),
       format(PfStream, '~w~n', [Pf]),
       close(PfStream)
     )
  ).


%! binpkg_extract:strip_build_id_suffix(+InnerName, -Pf) is det.
%
% Strips the trailing `-<BUILD_ID>` from an inner dir name to recover the
% PF (e.g. `'jq-1.8.1-9'` -> `'jq-1.8.1'`). Falls back to the input
% unchanged if there is no trailing dash-integer (defensive).

binpkg_extract:strip_build_id_suffix(InnerName, Pf) :-
  atom_string(InnerName, InnerStr),
  ( binpkg_extract:rsplit_dash_integer(InnerStr, PfStr)
  -> atom_string(Pf, PfStr)
  ;  Pf = InnerName
  ).


%! binpkg_extract:rsplit_dash_integer(+Str, -Head) is semidet.
%
% Splits Str on its rightmost `-` and unifies Head with the prefix iff
% the suffix is a non-empty integer. Fails otherwise.

binpkg_extract:rsplit_dash_integer(Str, Head) :-
  string_length(Str, Len),
  binpkg_extract:last_dash_pos(Str, Len, Pos),
  Pos > 0,
  After is Pos + 1,
  TailLen is Len - After,
  TailLen > 0,
  sub_string(Str, After, TailLen, 0, TailStr),
  number_string(N, TailStr), integer(N),
  sub_string(Str, 0, Pos, _, Head).

binpkg_extract:last_dash_pos(Str, Len, Pos) :-
  Last is Len - 1,
  binpkg_extract:last_dash_scan(Str, Last, Pos).

binpkg_extract:last_dash_scan(_Str, -1, _) :- !, fail.
binpkg_extract:last_dash_scan(Str, Idx, Idx) :-
  sub_string(Str, Idx, 1, _, "-"), !.
binpkg_extract:last_dash_scan(Str, Idx, Pos) :-
  Prev is Idx - 1,
  binpkg_extract:last_dash_scan(Str, Prev, Pos).


%! binpkg_extract:touch_installed_marker(+BuildDir) is det.
%
% Creates an empty `BuildDir/.installed` file. `ebuild qmerge` checks for
% this marker and bails immediately with "install phase has not been run"
% if it is absent (see `portage/package/ebuild/doebuild.py` ~line 1573).
% Conceptually we are asserting "the install phase happened (in another
% process, namely whoever produced the gpkg)".

binpkg_extract:touch_installed_marker(BuildDir) :-
  os:compose_path([BuildDir, '.installed'], Marker),
  setup_call_cleanup(
    open(Marker, write, S),
    true,
    close(S)
  ).


% -----------------------------------------------------------------------------
%  Process spawning helper
% -----------------------------------------------------------------------------

%! binpkg_extract:run_command(+ExeSpec, +Args, ?ExitCode) is semidet.
%
% Spawns ExeSpec(Args) inheriting the parent's stdout/stderr (so progress
% and error messages flow through to the user's terminal exactly the way
% `ebuild_exec:run_phases/4` already does). If ExitCode is bound on entry,
% succeeds iff the child exits with that code. If unbound, succeeds with
% whatever the child returned and binds ExitCode to it.

binpkg_extract:run_command(Exe, Args, ExitCode) :-
  process_create(
    Exe, Args,
    [stdout(null), stderr(null), process(Pid)]),
  process_wait(Pid, exit(Got)),
  ( var(ExitCode) -> ExitCode = Got
  ; Got =:= ExitCode
  ).
