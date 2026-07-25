/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> SANITIZE
Input validation and sanitization helpers for defense-in-depth against
command injection, path traversal, and other input-based attacks.
*/

:- module(sanitize, []).

% =============================================================================
%  SANITIZE declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Path component validation
% -----------------------------------------------------------------------------

%! sanitize:safe_path_component(+Atom) is semidet.
%
% True when Atom is a safe single path component: no slashes, no '..',
% no NUL bytes, and non-empty.

sanitize:safe_path_component(Atom) :-
  atom(Atom),
  atom_length(Atom, Len), Len > 0,
  atom_string(Atom, S),
  \+ sub_string(S, _, _, _, "/"),
  \+ sub_string(S, _, _, _, ".."),
  \+ sub_string(S, _, _, _, "\x00\").


%! sanitize:safe_filename(+Atom) is semidet.
%
% True when Atom is a safe filename: no directory separators, no '..',
% no NUL bytes, and non-empty.

sanitize:safe_filename(Atom) :-
  sanitize:safe_path_component(Atom).


%! sanitize:safe_portage_category(+Cat) is semidet.
%
% True when Cat is safe to use as a single path component (no slashes,
% no '..', no NUL bytes, non-empty). This is NOT a full PMS category
% name check: shell metacharacters (e.g. '-rf', '$(cmd)', ';reboot')
% pass. Callers must pass Cat as a process_create/3 argument-vector
% element, never interpolate it into a shell command string.

sanitize:safe_portage_category(Cat) :-
  sanitize:safe_path_component(Cat).


%! sanitize:safe_portage_name(+Name) is semidet.
%
% True when Name is safe to use as a single path component (no slashes,
% no '..', no NUL bytes, non-empty). This is NOT a full PMS package
% name check: shell metacharacters (e.g. '-rf', '$(cmd)', ';reboot')
% pass. Callers must pass Name as a process_create/3 argument-vector
% element, never interpolate it into a shell command string.

sanitize:safe_portage_name(Name) :-
  sanitize:safe_path_component(Name).


% -----------------------------------------------------------------------------
%  Snapshot ID validation
% -----------------------------------------------------------------------------

%! sanitize:safe_snapshot_id(+Id) is semidet.
%
% True when Id is a safe snapshot identifier: alphanumeric plus
% hyphens, underscores, and dots (no slashes, no '..').

sanitize:safe_snapshot_id(Id) :-
  atom(Id),
  atom_string(Id, S),
  string_length(S, Len), Len > 0, Len =< 255,
  \+ sub_string(S, _, _, _, "/"),
  \+ sub_string(S, _, _, _, "\\"),
  \+ sub_string(S, _, _, _, ".."),
  \+ sub_string(S, _, _, _, "\x00\").


%! sanitize:safe_git_commit(+Commit) is semidet.
%
% True when Commit is a full Git object name: 40 (SHA-1) or 64 (SHA-256)
% lowercase/uppercase hexadecimal characters. Rejects short SHAs, branch
% names, and anything with path/shell metacharacters.

sanitize:safe_git_commit(Commit) :-
  atom(Commit),
  atom_string(Commit, S),
  string_length(S, Len),
  memberchk(Len, [40, 64]),
  string_lower(S, Lower),
  re_match('^[0-9a-f]+$', Lower),
  !.


% -----------------------------------------------------------------------------
%  File integrity (SHA-256 sidecars)
% -----------------------------------------------------------------------------

%! sanitize:sha256_sidecar(+Path, -SidePath) is det.
%
% Path of the SHA-256 sidecar written next to Path (`Path.sha256`).

sanitize:sha256_sidecar(Path, SidePath) :-
  atom_concat(Path, '.sha256', SidePath).


%! sanitize:file_sha256(+Path, -Hash) is semidet.
%
% Unify Hash with the lowercase hex SHA-256 of Path's contents.

sanitize:file_sha256(Path, Hash) :-
  process_create(path(openssl), ['dgst', '-sha256', Path],
                 [stdout(pipe(Out)), stderr(null), process(Pid)]),
  call_cleanup(
    read_string(Out, _, Raw),
    ( close(Out), process_wait(Pid, Status) )
  ),
  Status == exit(0),
  split_string(Raw, "=", " \t\n\r", Parts),
  last(Parts, HashStr),
  HashStr \== "",
  string_lower(HashStr, Lower),
  atom_string(Hash, Lower),
  atom_length(Hash, 64).


%! sanitize:write_sha256_sidecar(+Path) is det.
%
% Write Path.sha256 containing the SHA-256 of Path. Best-effort: failures
% are swallowed so a digest write never aborts the caller.

sanitize:write_sha256_sidecar(Path) :-
  catch(
    ( sanitize:file_sha256(Path, Hash),
      sanitize:sha256_sidecar(Path, Side),
      setup_call_cleanup(
        open(Side, write, S),
        format(S, '~w~n', [Hash]),
        close(S))
    ),
    _, true).


%! sanitize:verify_sha256_sidecar(+Path) is semidet.
%
% Succeeds when Path.sha256 exists and matches the current SHA-256 of Path.

sanitize:verify_sha256_sidecar(Path) :-
  sanitize:sha256_sidecar(Path, Side),
  exists_file(Side),
  sanitize:file_sha256(Path, Got),
  setup_call_cleanup(
    open(Side, read, S),
    read_string(S, _, Raw),
    close(S)),
  split_string(Raw, "\n", " \t\n\r", [ExpectedStr|_]),
  atom_string(Expected, ExpectedStr),
  Got == Expected.


%! sanitize:ensure_file_integrity(+Path) is semidet.
%
% Enforce config:file_integrity/1 against Path:
%   * require — sidecar must verify (fail otherwise)
%   * prefer  — verify when sidecar exists; warn and succeed when missing
%   * off     — always succeed
% Default is prefer.

sanitize:ensure_file_integrity(Path) :-
  ( current_predicate(config:file_integrity/1),
    config:file_integrity(Mode0)
  -> Mode = Mode0
  ;  Mode = prefer
  ),
  sanitize:ensure_file_integrity(Path, Mode).

sanitize:ensure_file_integrity(_Path, off) :- !.
sanitize:ensure_file_integrity(Path, require) :-
  !,
  ( sanitize:verify_sha256_sidecar(Path)
  -> true
  ;  message:failure(['Integrity check failed for ', Path,
                      ' (missing or mismatched .sha256 sidecar). ',
                      'Regenerate the file with a current portage-ng save/write path.']),
     !, fail
  ).
sanitize:ensure_file_integrity(Path, prefer) :-
  sanitize:sha256_sidecar(Path, Side),
  ( exists_file(Side)
  -> ( sanitize:verify_sha256_sidecar(Path)
     -> true
     ;  message:failure(['Integrity check failed for ', Path,
                         ' (.sha256 mismatch). Refusing to load.']),
        !, fail
     )
  ;  message:warning(['No integrity sidecar for ', Path,
                      '; loading without verification. ',
                      'Re-save to create ', Side, '.']),
     true
  ).


% -----------------------------------------------------------------------------
%  Ebuild phase validation
% -----------------------------------------------------------------------------

%! sanitize:safe_phase(+Phase) is semidet.
%
% True when Phase is a known ebuild phase name.

sanitize:safe_phase(clean).
sanitize:safe_phase(setup).
sanitize:safe_phase(unpack).
sanitize:safe_phase(prepare).
sanitize:safe_phase(configure).
sanitize:safe_phase(compile).
sanitize:safe_phase(test).
sanitize:safe_phase(install).
sanitize:safe_phase(package).
sanitize:safe_phase(merge).
sanitize:safe_phase(unmerge).
sanitize:safe_phase(preinst).
sanitize:safe_phase(postinst).
sanitize:safe_phase(prerm).
sanitize:safe_phase(postrm).
sanitize:safe_phase(config).
sanitize:safe_phase(info).
sanitize:safe_phase(nofetch).


% -----------------------------------------------------------------------------
%  File existence validation
% -----------------------------------------------------------------------------

%! sanitize:existing_file(+Path) is semidet.
%
% True when Path points to an existing regular file (not a symlink
% to a directory, not a device node).

sanitize:existing_file(Path) :-
  exists_file(Path),
  \+ exists_directory(Path).


% -----------------------------------------------------------------------------
%  Daemon request term validation
% -----------------------------------------------------------------------------

%! sanitize:safe_daemon_request(+Term) is semidet.
%
% True when Term matches one of the allowed daemon request structures.

sanitize:safe_daemon_request(shutdown).

sanitize:safe_daemon_request(request(Args, Cols, Rows)) :-
  is_list(Args),
  maplist(atom, Args),
  integer(Cols),
  integer(Rows).

sanitize:safe_daemon_request(request(Args, Cols, Rows, Env)) :-
  is_list(Args),
  maplist(atom, Args),
  integer(Cols),
  integer(Rows),
  is_list(Env).


% -----------------------------------------------------------------------------
%  Symlink safety check
% -----------------------------------------------------------------------------

%! sanitize:not_symlink(+Path) is semidet.
%
% True when Path exists but is not a symbolic link.

sanitize:not_symlink(Path) :-
  catch(
    ( read_link(Path, _, _) -> fail ; true ),
    _, true
  ).