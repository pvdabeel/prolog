/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> VERSION
Version strings and system information for the interface (included into the
INTERFACE module via interface.pl).
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


%! interface:print_version_repos is det.
%
% Prints registered repositories with name, git version, and path
% in aligned columns.

interface:print_version_repos :-
  findall(Name-Loc-Ver,
    ( context:instances(repository, Name),
      Name:get_location(Loc),
      interface:repo_git_version(Loc, Ver)
    ),
    Repos),
  ( Repos == []
  -> true
  ;  aggregate_all(max(L), (member(N-_-_, Repos), atom_length(N, L)), MaxN),
     aggregate_all(max(L), (member(_-_-V, Repos), atom_length(V, L)), MaxV),
     Col1 is MaxN + 4,
     Col2 is Col1 + MaxV + 2,
     forall(member(N-Loc-V, Repos),
       format('  ~w~t~*|~w~t~*|~w~n', [N, Col1, V, Col2, Loc])
     )
  ).


%! interface:print_system_info is det.
%
% Prints system information when --info is called without arguments,
% similar to emerge --info: profile, repositories, key system
% packages, and USE flags.

interface:print_system_info :-
  interface:version(Version),
  current_prolog_flag(version, PrologVer),
  format(atom(PrologVerAtom), '~w', [PrologVer]),
  ( catch(config:hostname(Hostname), _, Hostname = unknown) -> true ; Hostname = unknown ),
  ( catch(config:gentoo_profile(Profile), _, Profile = unknown) -> true ; Profile = unknown ),
  nl,
  format('portage-ng ~w (SWI-Prolog ~w, ~w)~n', [Version, PrologVerAtom, Profile]),
  format('================================================================~n'),
  format('System hostname: ~w~n', [Hostname]),
  ( catch(config:installation_dir(Dir), _, fail)
  -> format('Install dir:     ~w~n', [Dir])
  ;  true
  ),
  ( catch(config:printing_tty_size(H, W), _, fail)
  -> format('Terminal size:   ~wx~w~n', [W, H])
  ;  format('Terminal size:   (not a TTY)~n')
  ),
  nl,
  format('Repositories:~n'),
  forall(
    ( context:instances(repository, Name),
      Name:get_location(Loc)
    ),
    format('  ~w~t~30|~w~n', [Name, Loc])
  ),
  nl,
  format('World set:~n'),
  ( catch(forall(world::entry(E), format('  ~w~n', [E])), _, true)
  -> true
  ;  format('  (not loaded)~n')
  ).


%! interface:status(-Status) is det.
%
% Unifies Status with the current release stage
% (one of alpha, beta, testing, development, release).

interface:status(S) :-
  S = 'development'.
