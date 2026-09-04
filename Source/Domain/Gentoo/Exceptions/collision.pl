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
% True when the log segment the failed phase wrote (fixup:log_tail/3)
% carries Portage's collision-protect abort signature (the collision
% report is emitted at the end of pkg_preinst).

collision:phase_error(LogPath, SizeBefore) :-
  fixup:log_tail(LogPath, SizeBefore, Tail),
  ( sub_string(Tail, _, _, _, "NOT merged due to file collisions")
  ; sub_string(Tail, _, _, _, "Detected file collision(s)")
  ),
  !.


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
  -> fixup:log_marker(LogPath,
       '~w failed (exit ~w) with file-collision signature; retrying with FEATURES=-collision-protect -protect-owned (portage-ng#90 deconfliction)',
       [Phase, ExitCode0]),
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
  fixup:packages_word(N, Word),
  format(atom(Line1), 'Deconfliction: collision protection was disabled to merge ~d ~w over', [N, Word]),
  Line2 = '               files owned by other installed packages (portage-ng#90):'.
