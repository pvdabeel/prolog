/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> GIT
Shared git command wrappers. Used by server, worker and client modes to
identify and synchronize portage tree snapshots.
*/

:- module(git, []).

% =============================================================================
%  GIT declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Reading the HEAD commit
% -----------------------------------------------------------------------------

%! git:head(+Dir, -Commit)
%
% Read the short git HEAD commit hash of a directory.

git:head(Dir, Commit) :-
  git:rev_parse(Dir, ['--short', 'HEAD'], Commit).


%! git:head_full(+Dir, -Commit)
%
% Read the full git HEAD commit hash of a directory.

git:head_full(Dir, Commit) :-
  git:rev_parse(Dir, ['HEAD'], Commit).


%! git:rev_parse(+Dir, +Args, -Commit)
%
% Run `git rev-parse` with the given arguments in Dir and return the first
% line of output as an atom.

git:rev_parse(Dir, Args, Commit) :-
  process_create(path(git), ['rev-parse'|Args],
                 [stdout(pipe(Out)), cwd(Dir), process(Pid)]),
  call_cleanup(
    ( read_string(Out, _, Raw),
      split_string(Raw, "\n", "\n \t", [CommitStr|_]),
      atom_string(Commit, CommitStr)
    ),
    ( close(Out), process_wait(Pid, _) )
  ).


% -----------------------------------------------------------------------------
%  Checking out a commit
% -----------------------------------------------------------------------------

%! git:checkout(+Dir, +Commit)
%
% Checkout a specific commit in a git repository. Commit must be a full
% hex object name that already exists in Dir's object store (no fetch).
% Refuses shell-metacharacter / short-ref / branch-name inputs so a
% hostile advertiser cannot inject checkout arguments.

git:checkout(Dir, Commit) :-
  ( sanitize:safe_git_commit(Commit)
  -> true
  ;  message:failure(['Refusing git checkout of non-hex commit id: ', Commit]),
     !, fail
  ),
  ( git:commit_exists(Dir, Commit)
  -> true
  ;  message:failure(['Git object not present locally (no fetch): ', Commit,
                      ' in ', Dir, '. Sync the tree from your trusted remote first.']),
     !, fail
  ),
  process_create(path(git), ['checkout', '--detach', Commit],
                 [stdout(null), stderr(null), cwd(Dir), process(Pid)]),
  process_wait(Pid, Status),
  ( Status == exit(0) -> true
  ; message:failure(['git checkout failed for ', Commit, ' in ', Dir])
  ).


%! git:commit_exists(+Dir, +Commit) is semidet.
%
% True when Commit names an object already present in Dir (git cat-file -e).

git:commit_exists(Dir, Commit) :-
  process_create(path(git), ['cat-file', '-e', Commit],
                 [stdout(null), stderr(null), cwd(Dir), process(Pid)]),
  process_wait(Pid, Status),
  Status == exit(0).
