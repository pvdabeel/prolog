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
% Checkout a specific commit in a git repository.

git:checkout(Dir, Commit) :-
  process_create(path(git), ['checkout', Commit],
                 [stdout(null), stderr(null), cwd(Dir), process(Pid)]),
  process_wait(Pid, Status),
  ( Status == exit(0) -> true
  ; message:failure(['git checkout failed for ', Commit, ' in ', Dir])
  ).
