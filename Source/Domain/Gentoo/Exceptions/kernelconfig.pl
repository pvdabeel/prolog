/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> KERNELCONFIG
Kernel CONFIG_CHECK learning exception (portage-ng#105).

When a build dies in the `setup` phase because Gentoo's linux-info.eclass
`CONFIG_CHECK` found the running kernel source configuration missing (or
carrying) a required option -- e.g.

  CONFIG_SECURITY_APPARMOR:	 is not set when it should be.
  ERROR: sys-apps/apparmor-4.1.7::gentoo failed (setup phase):
    Incorrect kernel configuration options

the fault is not an ebuild bug and not a resolver gap: the kernel source
tree simply lacks an option the package needs. Like the missing-provider
mechanism (#102), this one never repairs in place and threads the phase's
exit code through unchanged; it turns the failure into learned knowledge
and lets the pipeline re-derive a plan that includes a kernel-config-change
action ordered before the failing package.

The learned requirement is recorded via feedback:record_kernel_config/3
(durable, surfaced as a plan pre-action on the next pass) and fixup:record/3
(so it shows in the build summary). On the re-derived plan the builder
applies the option to the kernel source .config (scripts/config +
`make olddefconfig`) before building, so the CONFIG_CHECK passes.

Scope: satisfying CONFIG_CHECK unblocks userspace packages (apparmor,
apparmor-utils, ...). Kernel-module packages that need a rebuilt kernel are
only partly helped (the option is set and `modules_prepare` is run, but no
kernel rebuild/reboot happens). Two other cluster-C sub-types are out of
scope by construction and correctly left to fail: a kernel that is newer
than a package's supported ceiling (a version bound, not an option) and an
unset PYTHON_SINGLE_TARGET (a profile choice, not a kernel option).

Registered with the generic fixup registry (Source/Domain/Gentoo/
Exceptions/fixup.pl); the builder and printer stay generic.
*/

:- module(kernelconfig, []).

% =============================================================================
%  KERNELCONFIG declarations
% =============================================================================

:- multifile fixup:mechanism/1.
:- multifile fixup:mechanism_note/3.
:- multifile fixup:phase_retry_hook/10.

fixup:mechanism(kernelconfig).


% -----------------------------------------------------------------------------
%  Gate
% -----------------------------------------------------------------------------

%! kernelconfig:enabled is semidet.
%
% True unless config:kernel_config_repair/1 is explicitly false.

kernelconfig:enabled :-
  ( catch(config:kernel_config_repair(V), _, fail), ground(V)
  -> V == true
  ;  true
  ).


%! kernelconfig:kernel_source_dir(-Dir) is det.
%
% Kernel source directory whose .config the CONFIG_CHECK reads. Overridable
% via config:kernel_source_dir/1; defaults to the eselect-managed
% /usr/src/linux symlink.

kernelconfig:kernel_source_dir(Dir) :-
  ( catch(config:kernel_source_dir(D), _, fail), atom(D), D \== ''
  -> Dir = D
  ;  Dir = '/usr/src/linux'
  ).


% -----------------------------------------------------------------------------
%  Per-phase retry hook (diagnose, never repair)
% -----------------------------------------------------------------------------

%! fixup:phase_retry_hook(+kernelconfig, +EbuildPath, +Entry, +Phase, +LogPath, +UseString, :Callback, +SizeBefore, +ExitCode0, -ExitCode) is det.
%
% On a non-zero exit of the setup phase, scans the log segment the phase
% appended for linux-info CONFIG_CHECK lines and records the required
% kernel options as learned knowledge. Always passes ExitCode0 through
% unchanged -- the phase is never re-run here; the builder's replan loop
% re-derives the plan with a kernel-config-change action ordered before the
% target.

fixup:phase_retry_hook(kernelconfig, _EbuildPath, Entry, Phase, LogPath, _UseString, _Callback, SizeBefore, ExitCode0, ExitCode0) :-
  ( kernelconfig:enabled,
    ExitCode0 =\= 0,
    Phase == setup,
    kernelconfig:tree_entry(Entry, Repo, _C, _N),
    kernelconfig:scan_log(LogPath, SizeBefore, Lines),
    Lines \== [],
    kernelconfig:detect_options(Lines, Options, EvidenceLines),
    Options \== []
  -> once(kernelconfig:record_requirement(Repo, Entry, Phase, ExitCode0, Options, EvidenceLines))
  ;  true
  ).


%! kernelconfig:tree_entry(+Entry, -Repo, -C, -N) is semidet.
%
% Resolves a build entry (Category/Name-Version) to its tree repository
% and Category/Name. Fails for a VDB-only (pkg) entry.

kernelconfig:tree_entry(Entry, Repo, C, N) :-
  cache:ordered_entry(Repo, Entry, C, N, _),
  Repo \== pkg,
  !.


% -----------------------------------------------------------------------------
%  Log scanning
% -----------------------------------------------------------------------------

%! kernelconfig:scan_log(+LogPath, +SizeBefore, -Lines) is det.
%
% Returns the lines the failed phase appended after byte offset SizeBefore,
% limited to the trailing 256KB (the CONFIG_CHECK die is at the end).
% Errors and a non-grown log yield [].

kernelconfig:scan_log(LogPath, SizeBefore, Lines) :-
  ( catch(
      ( exists_file(LogPath),
        size_file(LogPath, Size),
        Size > SizeBefore,
        Start is max(SizeBefore, Size - 262144),
        Len is Size - Start,
        setup_call_cleanup(
          open(LogPath, read, S, [type(binary)]),
          ( seek(S, Start, bof, _),
            read_string(S, Len, Tail) ),
          close(S)) ),
      _, fail)
  -> split_string(Tail, "\n", "\r", Lines)
  ;  Lines = []
  ).


% -----------------------------------------------------------------------------
%  Detection
% -----------------------------------------------------------------------------

%! kernelconfig:detect_options(+Lines, -Options, -EvidenceLines) is det.
%
% Runs the CONFIG_CHECK detectors over the log lines, deduplicating on the
% option name (first evidence line wins). Options is a sorted list of
% config(Name, State) with State in {y, n}.

kernelconfig:detect_options(Lines, Options, EvidenceLines) :-
  findall(config(Name, State)-Line,
          kernelconfig:detector(Lines, config(Name, State), Line),
          Pairs0),
  kernelconfig:dedup_options(Pairs0, Pairs),
  findall(Opt, member(Opt-_, Pairs), Options0),
  sort(Options0, Options),
  findall(Line, member(_-Line, Pairs), EvidenceLines0),
  sort(EvidenceLines0, EvidenceLines).


%! kernelconfig:dedup_options(+Pairs0, -Pairs) is det.
%
% Keeps the first evidence line per distinct config-option name (a later
% line asking for a different state of the same option is ignored -- the
% first CONFIG_CHECK verdict wins).

kernelconfig:dedup_options(Pairs0, Pairs) :-
  kernelconfig:dedup_options_(Pairs0, [], Pairs).

kernelconfig:dedup_options_([], _, []).
kernelconfig:dedup_options_([config(Name, State)-Line|Rest], Seen, Out) :-
  ( memberchk(Name, Seen)
  -> kernelconfig:dedup_options_(Rest, Seen, Out)
  ;  Out = [config(Name, State)-Line|More],
     kernelconfig:dedup_options_(Rest, [Name|Seen], More)
  ).


%! kernelconfig:detector(+Lines, -Option, -Line) is nondet.
%
% linux-info.eclass emits one line per unsatisfied option:
%   "CONFIG_FOO:\t is not set when it should be."  -> enable  (config(Name,y))
%   "CONFIG_FOO:\t is set when it should not be."  -> disable (config(Name,n))

kernelconfig:detector(Lines, config(Name, y), Line) :-
  member(Line, Lines),
  sub_string(Line, _, _, _, "is not set when it should be"),
  kernelconfig:config_token(Line, Name).

kernelconfig:detector(Lines, config(Name, n), Line) :-
  member(Line, Lines),
  sub_string(Line, _, _, _, "is set when it should not be"),
  \+ sub_string(Line, _, _, _, "is not set when it should be"),
  kernelconfig:config_token(Line, Name).


%! kernelconfig:config_token(+Line, -Name) is semidet.
%
% Extracts the CONFIG_<NAME> token (the text from the first "CONFIG_"
% occurrence up to the following colon) and validates it is a single
% config-option identifier.

kernelconfig:config_token(Line, Name) :-
  sub_string(Line, Before, _, 0, FromConfig0),
  sub_string(FromConfig0, 0, 7, _, "CONFIG_"),
  !,
  kernelconfig:config_token_at(Line, Before, Name).
kernelconfig:config_token(Line, Name) :-
  sub_string(Line, Before, _, _, "CONFIG_"),
  kernelconfig:config_token_at(Line, Before, Name).

kernelconfig:config_token_at(Line, Before, Name) :-
  sub_string(Line, Before, _, 0, FromConfig),
  split_string(FromConfig, ":", " \t", [Tok|_]),
  Tok \== "",
  \+ sub_string(Tok, _, _, _, " "),
  sub_string(Tok, 0, 7, _, "CONFIG_"),
  kernelconfig:name_string(Tok),
  atom_string(Name, Tok).


%! kernelconfig:name_string(+Str) is semidet.
%
% True when every character of Str is a config-option character
% (alphanumeric or underscore).

kernelconfig:name_string(Str) :-
  string_chars(Str, Chars),
  forall(member(Ch, Chars), kernelconfig:name_char(Ch)).

kernelconfig:name_char(Ch) :- char_type(Ch, alnum), !.
kernelconfig:name_char('_').


% -----------------------------------------------------------------------------
%  Recording
% -----------------------------------------------------------------------------

%! kernelconfig:record_requirement(+Repo, +Entry, +Phase, +ExitCode, +Options, +EvidenceLines) is det.
%
% Records the learned kernel-config requirement as durable feedback (so the
% next proof pass surfaces it as a pre-action and future runs plan it
% proactively) and as a fixup record (for the build summary).

kernelconfig:record_requirement(Repo, Entry, Phase, ExitCode, Options, EvidenceLines) :-
  Evidence = evidence(kernel_config(Options),
                      phase(Phase),
                      exit(ExitCode),
                      log(EvidenceLines)),
  feedback:record_kernel_config(Repo://Entry, Options, Evidence),
  fixup:record(kernelconfig, Entry, kernel_config(Options)),
  kernelconfig:log_requirement(Entry, Options).


%! kernelconfig:log_requirement(+Entry, +Options) is det.

kernelconfig:log_requirement(Entry, Options) :-
  kernelconfig:options_atom(Options, OptAtom),
  message:color(yellow),
  format('>>> kernel config: ~w needs ~w; recorded as learned requirement, re-deriving plan (#105)~n',
         [Entry, OptAtom]),
  message:color(normal).


%! kernelconfig:options_atom(+Options, -Atom) is det.
%
% Human-readable rendering of a config-option list, e.g.
% "CONFIG_SECURITY_APPARMOR=y CONFIG_FOO=n".

kernelconfig:options_atom(Options, Atom) :-
  findall(Tok,
          ( member(config(Name, State), Options),
            format(atom(Tok), '~w=~w', [Name, State]) ),
          Toks),
  atomic_list_concat(Toks, ' ', Atom).


% -----------------------------------------------------------------------------
%  Applying a kernel-config pre-action
% -----------------------------------------------------------------------------

%! kernelconfig:apply_planned(+PreActions) is det.
%
% Applies every kernel_config(Options, _Evidence) pre-action in PreActions
% to the kernel source .config, then normalizes the tree. Called by the
% builder before it executes the plan steps, so the CONFIG_CHECK of the
% package that learned the requirement now passes. Best-effort and
% defensive: a missing kernel tree, missing scripts/config, or a failing
% make never aborts the build (the package's own CONFIG_CHECK will just
% fail again with a clear message).

kernelconfig:apply_planned(PreActions) :-
  ( kernelconfig:enabled,
    findall(Opt,
            ( member(kernel_config(Options, _Evidence), PreActions),
              member(Opt, Options) ),
            Opts0),
    sort(Opts0, Opts),
    Opts \== []
  -> kernelconfig:apply_options(Opts)
  ;  true
  ).


%! kernelconfig:apply_options(+Options) is det.
%
% Enables/disables each config(Name, State) in the kernel source .config
% via scripts/config, then runs `make olddefconfig` (to resolve option
% dependencies) and `make modules_prepare` (so out-of-tree modules can
% still build against the tree). All best-effort.

kernelconfig:apply_options(Options) :-
  kernelconfig:kernel_source_dir(KDir),
  atom_concat(KDir, '/.config', ConfigPath),
  ( exists_file(ConfigPath)
  -> kernelconfig:build_apply_script(KDir, ConfigPath, Options, Script),
     kernelconfig:log_apply(KDir, Options),
     catch(
       ( process_create(path(sh), ['-c', Script],
                        [stdout(null), stderr(null), process(Pid)]),
         process_wait(Pid, _Status) ),
       _Error,
       true)
  ;  message:color(red),
     format('!!! kernel config: ~w has no .config; cannot apply learned options (#105)~n', [ConfigPath]),
     message:color(normal)
  ).


%! kernelconfig:build_apply_script(+KDir, +ConfigPath, +Options, -Script) is det.
%
% Composes a single sh script that runs scripts/config per option followed
% by `make olddefconfig` and `make modules_prepare`. The kernel's
% scripts/config takes the option name WITHOUT the CONFIG_ prefix.

kernelconfig:build_apply_script(KDir, ConfigPath, Options, Script) :-
  findall(Cmd,
          ( member(config(Name, State), Options),
            kernelconfig:strip_prefix(Name, Suffix),
            ( State == y -> Op = '--enable' ; Op = '--disable' ),
            format(atom(Cmd),
                   'if [ -x ~w/scripts/config ]; then ~w/scripts/config --file ~w ~w ~w; fi; ',
                   [KDir, KDir, ConfigPath, Op, Suffix]) ),
          CfgCmds),
  atomic_list_concat(CfgCmds, CfgScript),
  format(atom(MakeScript),
         'make -C ~w olddefconfig >/dev/null 2>&1 || true; make -C ~w modules_prepare >/dev/null 2>&1 || true; true',
         [KDir, KDir]),
  atomic_list_concat([CfgScript, MakeScript], Script).


%! kernelconfig:strip_prefix(+Name, -Suffix) is det.
%
% Drops a leading CONFIG_ from a config-option name (scripts/config wants
% the bare suffix).

kernelconfig:strip_prefix(Name, Suffix) :-
  ( atom_concat('CONFIG_', Suffix0, Name)
  -> Suffix = Suffix0
  ;  Suffix = Name
  ).


%! kernelconfig:log_apply(+KDir, +Options) is det.

kernelconfig:log_apply(KDir, Options) :-
  kernelconfig:options_atom(Options, OptAtom),
  message:color(green),
  format('>>> kernel config: applying ~w to ~w/.config (#105)~n', [OptAtom, KDir]),
  message:color(normal).


% -----------------------------------------------------------------------------
%  Build summary note
% -----------------------------------------------------------------------------

%! fixup:mechanism_note(+kernelconfig, +Count, -Lines) is semidet.

fixup:mechanism_note(kernelconfig, N, [Line1, Line2]) :-
  ( N =:= 1 -> Word = 'package' ; Word = 'packages' ),
  format(atom(Line1), 'Kernel config: ~d ~w required kernel options that were learned at', [N, Word]),
  Line2 = '               setup time and applied to the kernel source .config (portage-ng#105):'.
