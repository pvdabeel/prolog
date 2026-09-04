/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> GITLOCK
Retry git-r3 unpack when a shared git3-src cache races on HEAD.lock.

Live ebuilds unpack through git-r3 into $DISTDIR/git3-src/. tinderbox-ng
(and any other parallel builder) bind-mounts that cache across sessions,
so concurrent `git symbolic-ref HEAD` / `git fetch` on the same bare
repo die with:

  error: couldn't close 'HEAD.lock'
  Unable to update HEAD

The failure is environmental, not an ebuild bug. This mechanism retries
the unpack phase a few times with backoff so the lock holder can finish.
Real unpack/compile failures keep their original exit code.

Registered with the generic fixup registry (Source/Domain/Gentoo/
Exceptions/fixup.pl); the builder and printer have no knowledge of this
mechanism.
*/

:- module(gitlock, []).

% =============================================================================
%  GITLOCK declarations
% =============================================================================

:- multifile fixup:mechanism/1.
:- multifile fixup:mechanism_note/3.
:- multifile fixup:phase_retry_hook/10.

fixup:mechanism(gitlock).


% -----------------------------------------------------------------------------
%  Failure signature
% -----------------------------------------------------------------------------

%! gitlock:retry_phase(+Phase) is semidet.
%
% git-r3 fetch/checkout runs during src_unpack.

gitlock:retry_phase(unpack).


%! gitlock:phase_error(+LogPath, +SizeBefore) is semidet.
%
% True when the log segment written by the failed phase carries the
% git-r3 HEAD.lock / Unable to update HEAD signature. The message is
% emitted right at the die, so the trailing 64KB is enough.

gitlock:phase_error(LogPath, SizeBefore) :-
  fixup:log_tail(LogPath, SizeBefore, 65536, Tail),
  ( sub_string(Tail, _, _, _, "HEAD.lock")
  ; sub_string(Tail, _, _, _, "Unable to update HEAD")
  ),
  !.


% -----------------------------------------------------------------------------
%  Per-phase retry hook
% -----------------------------------------------------------------------------

%! fixup:phase_retry_hook(+gitlock, +EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.
%
% On a non-zero unpack exit whose log segment matches the git-r3 lock
% signature, re-runs unpack up to five times with linear backoff.
% Otherwise passes ExitCode0 through unchanged.

fixup:phase_retry_hook(gitlock, EbuildPath, Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode) :-
  ( gitlock:retry_phase(Phase),
    ExitCode0 =\= 0,
    gitlock:phase_error(LogPath, SizeBefore)
  -> gitlock:retry(EbuildPath, Entry, Phase, LogPath, UseString, Callback, ExitCode0, 1, ExitCode)
  ;  ExitCode = ExitCode0
  ).


%! gitlock:retry(+EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +ExitCode0, +Attempt, -ExitCode) is det.
%
% One backoff-and-rerun step. Stops on success, a non-lock failure, or
% after five attempts.

:- meta_predicate gitlock:retry(+, +, +, +, +, 2, +, +, -).

gitlock:retry(_EbuildPath, _Entry, _Phase, _LogPath, _UseString, _Callback, ExitCode0, Attempt, ExitCode0) :-
  Attempt > 5, !.

gitlock:retry(EbuildPath, Entry, Phase, LogPath, UseString, Callback, ExitCode0, Attempt, ExitCode) :-
  Delay is 2.0 * Attempt,
  sleep(Delay),
  fixup:log_marker(LogPath, '~w failed (exit ~w) with git-r3 HEAD.lock; retry ~w/5',
                   [Phase, ExitCode0, Attempt]),
  size_file(LogPath, SizeBefore),
  ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString, Pid),
  ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode1),
  ( ExitCode1 =:= 0
  -> fixup:record(gitlock, Entry, head_lock),
     ExitCode = 0
  ; gitlock:phase_error(LogPath, SizeBefore)
  -> Attempt1 is Attempt + 1,
     gitlock:retry(EbuildPath, Entry, Phase, LogPath, UseString, Callback, ExitCode1, Attempt1, ExitCode)
  ;  ExitCode = ExitCode1
  ).


% -----------------------------------------------------------------------------
%  Build summary note
% -----------------------------------------------------------------------------

%! fixup:mechanism_note(+gitlock, +Count, -Lines) is semidet.

fixup:mechanism_note(gitlock, N, [Line1, Line2]) :-
  fixup:packages_word(N, Word),
  format(atom(Line1), 'Git cache: unpack retried after a shared git3-src HEAD.lock for ~d ~w', [N, Word]),
  Line2 = '           (parallel live clones racing on the same bare repo):'.
