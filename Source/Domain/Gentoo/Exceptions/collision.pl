/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> COLLISION
Collision-protect deconfliction exception (portage-ng#90).

Traditional `emerge` refuses, at the resolver/plan stage, to install a
package whose files are already owned by a different installed provider --
it is told so by an explicit blocker atom in metadata (e.g. installed
`sys-apps/util-linux[hardlink]` carries `!app-arch/hardlink`; installed
`dev-libs/elfutils` carries `!dev-libs/libelf`; the candidate
`dev-libs/libiconv` itself carries `!sys-libs/glibc`). portage-ng plans
and dispatches the build instead, and the conflict only surfaces at MERGE
time as Portage's `pkg_preinst` collision-protect abort:

  * Detected file collision(s):
  *   /usr/bin/hardlink
  * sys-apps/util-linux-2.41.4-r1:0::gentoo
  * Package 'app-arch/hardlink-0.3.2' NOT merged due to file collisions.

The real fix is an ebuild/metadata-level blocker atom; until the tree
carries one, this exception mechanism -- gated by
config:deconflict_collisions/1 -- recovers the signature-based failure by
re-running the `merge` phase with `FEATURES=-collision-protect
-protect-owned`, letting the package overwrite the colliding file(s). The
action is logged (build log marker) and recorded (fixup:record/3) so it is
visible, never silent. The override env uses Portage's incremental
FEATURES semantics: `-token` in the child environment removes that feature
from the accumulated set.

Registered with the generic fixup registry (Source/Domain/Gentoo/
Exceptions/fixup.pl); the builder and printer have no knowledge of this
mechanism.
*/

:- module(collision, []).

% =============================================================================
%  COLLISION declarations
% =============================================================================

:- multifile fixup:mechanism/1.
:- multifile fixup:mechanism_note/3.
:- multifile fixup:phase_retry_hook/10.

fixup:mechanism(collision).


% -----------------------------------------------------------------------------
%  Deconfliction mode
% -----------------------------------------------------------------------------

%! collision:deconflict_mode(-Mode) is det.
%
% Resolves config:deconflict_collisions/1 (off|report|override), defaulting
% to `override` when unset (tinderbox-oriented default).

collision:deconflict_mode(Mode) :-
  ( catch(config:deconflict_collisions(M), _, fail), ground(M)
  -> Mode = M
  ;  Mode = override
  ).


% -----------------------------------------------------------------------------
%  Failure signature
% -----------------------------------------------------------------------------

%! collision:phase_error(+LogPath, +SizeBefore) is semidet.
%
% True when the log content appended after byte offset SizeBefore (i.e. by
% the phase that just failed) carries Portage's collision-protect abort
% signature. Only the trailing 256KB of the segment is examined (the
% collision report is emitted at the end of pkg_preinst), keeping the check
% cheap on large logs.

collision:phase_error(LogPath, SizeBefore) :-
  catch(
    ( exists_file(LogPath),
      size_file(LogPath, Size),
      Size > SizeBefore,
      Start is max(SizeBefore, Size - 262144),
      Len is Size - Start,
      setup_call_cleanup(
        open(LogPath, read, S, [type(binary)]),
        ( seek(S, Start, bof, _),
          read_string(S, Len, Tail)
        ),
        close(S))
    ),
    _, fail),
  ( sub_string(Tail, _, _, _, "NOT merged due to file collisions")
  ; sub_string(Tail, _, _, _, "Detected file collision(s)")
  ),
  !.


%! collision:log_retry(+LogPath, +Phase, +ExitCode) is det.
%
% Writes a marker line to the build log so the deconfliction is visible
% when inspecting the build.

collision:log_retry(LogPath, Phase, ExitCode) :-
  catch(
    ( open(LogPath, append, S),
      format(S, '~n=== ~w failed (exit ~w) with file-collision signature; retrying with FEATURES=-collision-protect -protect-owned (portage-ng#90 deconfliction) ===~n',
             [Phase, ExitCode]),
      close(S)
    ), _, true).


% -----------------------------------------------------------------------------
%  Per-phase retry hook
% -----------------------------------------------------------------------------

%! fixup:phase_retry_hook(+collision, +EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.
%
% On a non-zero exit of the `merge` phase whose log segment (bytes after
% SizeBefore) matches the collision-protect signature, and when
% config:deconflict_collisions/1 is `override`, re-runs that single phase
% with collision protection disabled and returns the retry's exit code;
% otherwise passes ExitCode0 through unchanged. Records the override
% (fixup:record/3) for the build summary.

fixup:phase_retry_hook(collision, EbuildPath, Entry, Phase, LogPath, UseString, Callback, SizeBefore, ExitCode0, ExitCode) :-
  ( Phase == merge,
    collision:deconflict_mode(override),
    collision:phase_error(LogPath, SizeBefore)
  -> collision:log_retry(LogPath, Phase, ExitCode0),
     fixup:record(collision, Entry, collision_protect),
     ebuild_exec:start_phase_async(EbuildPath, Phase, LogPath, UseString,
                                   ['FEATURES'='-collision-protect -protect-owned'], Pid),
     ebuild_exec:poll_phase_spinning(Pid, Phase, Callback, ExitCode)
  ;  ExitCode = ExitCode0
  ).


% -----------------------------------------------------------------------------
%  Build summary note
% -----------------------------------------------------------------------------

%! fixup:mechanism_note(+collision, +Count, -Lines) is semidet.

fixup:mechanism_note(collision, N, [Line1, Line2]) :-
  ( N =:= 1 -> Word = 'package' ; Word = 'packages' ),
  format(atom(Line1), 'Deconfliction: collision protection was disabled to merge ~d ~w over', [N, Word]),
  Line2 = '               files owned by other installed packages (portage-ng#90):'.
