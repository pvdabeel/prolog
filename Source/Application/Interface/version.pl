/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> VERSION
Version strings for the interface (included into the INTERFACE module via
interface.pl). Rendering of the --version and --info overviews lives in
Source/Pipeline/Printer/info.pl.
*/

% -----------------------------------------------------------------------------
%  Interface version
% -----------------------------------------------------------------------------

%! interface:version(-Version) is det.
%
% Unifies Version with the current portage-ng version string, obtained by
% executing the `version` script.

interface:version(V) :-
  script:exec('version',V).


%! interface:repo_git_version(+Dir, -Version) is det.
%
% Git date+hash for Dir, or 'unknown' when git is unavailable.

interface:repo_git_version(Dir, Version) :-
  catch(
    ( process_create(path(git),
                     ['--no-pager', log, '-1',
                      '--date=format:%Y.%m.%d', '--pretty=%cd (%h)'],
                     [stdout(pipe(Out)), stderr(null), cwd(Dir), process(Pid)]),
      call_cleanup(
        ( read_string(Out, _, Raw),
          split_string(Raw, "\n", "\n \t", [VerStr|_]),
          atom_string(Version, VerStr)
        ),
        ( close(Out), process_wait(Pid, _) )
      )
    ),
    _, Version = unknown
  ).


%! interface:status(-Status) is det.
%
% Unifies Status with the current release stage
% (one of alpha, beta, testing, development, release).

interface:status(S) :-
  S = 'development'.
