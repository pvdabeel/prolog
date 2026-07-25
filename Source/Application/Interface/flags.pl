/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> FLAGS
Option handling: translating parsed CLI flags into runtime preferences and
config overrides, plus derived accessors (mode, port, server, tty init)
(included into the INTERFACE module via interface.pl).
*/

% -----------------------------------------------------------------------------
%  Option handling
% -----------------------------------------------------------------------------

%! interface:process_flags is det.
%
% Translates boolean CLI flags into runtime preference assertions
% (e.g. --deep asserts preference:local_flag(deep)) and sets
% config overrides for verbose mode and printing style.

interface:process_flags :-
  interface:argv(Options, _),
  (lists:memberchk(deep(true),              Options) -> asserta(preference:local_flag(deep)) ; true),
  (lists:memberchk(emptytree(true),         Options) -> asserta(preference:local_flag(emptytree)) ; true),
  (lists:memberchk(depclean(true),          Options) -> asserta(preference:local_flag(depclean)) ; true),
  (lists:memberchk(newuse(true),            Options) -> asserta(preference:local_flag(newuse)) ; true),
  (lists:memberchk(changeduse(true),        Options) -> asserta(preference:local_flag(changeduse)) ; true),
  (lists:memberchk(changeddeps(true),       Options) -> asserta(preference:local_flag(changeddeps)) ; true),
  (lists:memberchk(changedslot(true),       Options) -> asserta(preference:local_flag(changedslot)) ; true),
  (lists:memberchk(selective(true),         Options) -> asserta(preference:local_flag(selective)) ; true),
  (lists:memberchk(noreplace(true),         Options) -> asserta(preference:local_flag(noreplace)) ; true),
  (lists:memberchk(nodeps(true),            Options) -> asserta(preference:local_flag(nodeps)) ; true),
  (lists:memberchk(onlydeps(true),          Options) -> asserta(preference:local_flag(onlydeps)) ; true),
  (lists:memberchk(dynamicdeps(false),      Options) -> asserta(preference:local_flag(nodynamicdeps)) ; true),
  (lists:memberchk(rebuildnewrev(true),     Options) -> asserta(preference:local_flag(rebuildnewrev)) ; true),
  (lists:memberchk(rebuildnewver(true),     Options) -> asserta(preference:local_flag(rebuildnewver)) ; true),
  (lists:memberchk(rebuildnewslot(true),    Options) -> asserta(preference:local_flag(rebuildnewslot)) ; true),
  (lists:memberchk(rebuildunbuilt(true),    Options) -> asserta(preference:local_flag(rebuildunbuilt)) ; true),
  (lists:memberchk(updateifinstalled(true), Options) -> asserta(preference:local_flag(updateifinstalled)) ; true),
  (lists:memberchk(readnews(true),          Options) -> asserta(preference:local_flag(readnews)) ; true),
  (lists:memberchk(withbdeps(n),            Options) -> asserta(preference:local_flag(nobdeps)) ; true),
  (lists:memberchk(withtestdeps(y),         Options) -> asserta(preference:local_flag(withtestdeps)) ; true),
  (lists:memberchk(pretend(true),           Options) -> asserta(preference:local_flag(pretend)) ; true),
  (lists:memberchk(oneshot(true),           Options) -> asserta(preference:local_flag(oneshot)) ; true),
  (lists:memberchk(select(false),           Options) -> asserta(preference:local_flag(oneshot)) ; true),
  (lists:memberchk(buildpkg(true),          Options) -> asserta(preference:local_flag(buildpkg)) ; true),
  (lists:memberchk(buildpkgonly(true),      Options) -> asserta(preference:local_flag(buildpkgonly)) ; true),
  (lists:memberchk(usepkg(true),            Options) -> asserta(preference:local_flag(usepkg)) ; true),
  (lists:memberchk(usepkgonly(true),        Options) -> asserta(preference:local_flag(usepkgonly)) ; true),
  (lists:memberchk(getbinpkg(true),         Options) -> asserta(preference:local_flag(getbinpkg)) ; true),
  (lists:memberchk(getbinpkgonly(true),     Options) -> asserta(preference:local_flag(getbinpkgonly)) ; true),
  (lists:memberchk(fetchall(true),          Options) -> asserta(preference:local_flag(fetchall)) ; true),
  (lists:memberchk(failclean(true),         Options) -> asserta(preference:local_flag(failclean)) ; true),
  (lists:memberchk(usepkgexcludelive(true), Options) -> asserta(preference:local_flag(usepkgexcludelive)) ; true),
  (lists:memberchk(binpkgchangeddeps(true), Options) -> asserta(preference:local_flag(binpkgchangeddeps)) ; true),
  (lists:memberchk(binpkgrespectuse(true),  Options) -> asserta(preference:local_flag(binpkgrespectuse)) ; true),
  (lists:memberchk(rebuiltbinaries(true),   Options) -> asserta(preference:local_flag(rebuiltbinaries)) ; true),
  (lists:memberchk(ask(true),              Options) -> asserta(preference:local_flag(ask)) ; true),
  (lists:memberchk(alert(true),            Options) -> asserta(preference:local_flag(alert)) ; true),
  (lists:memberchk(quiet(true),             Options) -> asserta(preference:local_flag(quiet)) ; true),
  (lists:memberchk(verbose(true),           Options) -> asserta(config:verbose(true)) ; true),
  (lists:memberchk(choicelog(true),         Options)
    -> ( asserta(config:choice_log(true)),
         choicelog:arm,
         choicelog:reset
       )
    ; true),
  (lists:memberchk(profile(true),           Options) -> set_prolog_flag(instrumentation, true) ; true),
  (lists:memberchk(logs(true),              Options) -> asserta(config:show_build_logs(true)) ; true),
  (lists:memberchk(ci(true),                Options) -> asserta(config:cli_ci(true)) ; true),
  (lists:memberchk(style(Style),            Options) -> interface:assert_valid_style(Style) ; true),
  ((lists:memberchk(jobs(J),                Options), J > 0) -> asserta(config:cli_jobs(J)) ; true),
  ((lists:memberchk(loadavg(L),             Options), L > 0.0) -> asserta(config:cli_load_average(L)) ; true),
  (lists:memberchk(permitdowngrade(true),   Options) -> asserta(preference:local_flag(permitdowngrade)) ; true),
  (lists:memberchk(color(n),                Options) -> retractall(config:color_output) ; true),
  (lists:memberchk(showdescriptions(SD),    Options), SD \== none
                                                   -> asserta(config:show_use_descriptions(SD)) ; true),
  (lists:memberchk(continuefailure(CF),     Options), CF \== never
                                                   -> asserta(config:continue_on_failure(CF)) ; true),
  % convenience presets
  (lists:memberchk(lazy(true), Options) ->
    asserta(preference:local_flag(noreplace)),
    asserta(preference:local_flag(nodeps)) ; true),
  (lists:memberchk(complete(true), Options) ->
    asserta(preference:local_flag(deep)),
    asserta(preference:local_flag(newuse)),
    retractall(preference:local_flag(nobdeps)) ; true),
  (lists:memberchk(everything(true), Options) ->
    asserta(preference:local_flag(emptytree)),
    asserta(preference:local_flag(deep)) ; true),
  interface:process_repeated_flags,
  interface:process_snapshot_flag.


%! interface:process_repeated_flags is det.
%
% Scans the raw argv for repeated value-taking flags and asserts
% each value. This bypasses optparse's keeplast behaviour, allowing
% e.g. --skip pkg1 --skip pkg2 without shell quoting.

interface:process_repeated_flags :-
  current_prolog_flag(argv, RawArgs),
  interface:collect_flag_values(RawArgs, '--skip', Skips),
  forall(member(S, Skips), asserta(config:skip_atom(S))),
  interface:collect_flag_values(RawArgs, '--exclude', Excludes),
  forall(member(E, Excludes), asserta(config:excluded_atom(E))),
  interface:collect_flag_values(RawArgs, '--usepkg-exclude', UExcl),
  forall(member(U, UExcl), asserta(config:usepkg_exclude_atom(U))),
  interface:collect_flag_values(RawArgs, '--usepkg-include', UIncl),
  forall(member(I, UIncl), asserta(config:usepkg_include_atom(I))),
  interface:collect_flag_values(RawArgs, '--favour', Favours),
  forall(member(Fv, Favours), asserta(config:dep_favour(Fv))),
  interface:collect_flag_values(RawArgs, '--avoid', Avoids),
  forall(member(Av, Avoids), asserta(config:dep_avoid(Av))),
  interface:collect_flag_values(RawArgs, '--preset', Presets),
  forall(member(Pr, Presets), asserta(config:dep_preset(Pr))),
  interface:collect_flag_values(RawArgs, '--hide', Hides),
  forall(member(Hi, Hides), asserta(config:dep_hide(Hi))),
  interface:collect_flag_values(RawArgs, '--early', Earlys),
  forall(member(Ea, Earlys), asserta(config:dep_early(Ea))),
  interface:collect_flag_values(RawArgs, '--late', Lates),
  forall(member(La, Lates), asserta(config:dep_late(La))).


%! interface:collect_flag_values(+ArgList, +Flag, -Values) is det.
%
% Walks the argument list and collects the value following each
% occurrence of Flag.

interface:collect_flag_values([], _, []).

interface:collect_flag_values([Flag, Value|Rest], Flag, [Value|More]) :-
  !,
  interface:collect_flag_values(Rest, Flag, More).

interface:collect_flag_values([_|Rest], Flag, Values) :-
  interface:collect_flag_values(Rest, Flag, Values).


%! interface:assert_valid_style(+Style) is det.
%
% Asserts the given printing style if it is one of the known values
% (fancy, column, short). Falls back to 'fancy' with a warning otherwise.

interface:assert_valid_style(Style) :-
  memberchk(Style, ['fancy', 'column', 'short']), !,
  asserta(config:interface_printing_style(Style)).

interface:assert_valid_style(Style) :-
  format(atom(Msg), 'Unknown printing style "~w", falling back to "fancy"', [Style]),
  message:warning(Msg),
  asserta(config:interface_printing_style('fancy')).


%! interface:process_snapshot_flag is det.
%
% Activates snapshot mode if --snapshot was passed or config:snapshot_enabled
% is asserted. Generates an ID from timestamp if none provided.

interface:process_snapshot_flag :-
  interface:argv(Options, _),
  ( memberchk(snapshot(SnapVal), Options), SnapVal \== none
  -> ( SnapVal == true
     -> snapshot:generate_id(Id)
     ;  Id = SnapVal
     ),
     assertz(snapshot:active_id(Id))
  ;  config:snapshot_enabled
  -> snapshot:generate_id(Id),
     assertz(snapshot:active_id(Id))
  ;  true
  ).


%! interface:get_mode(-Mode) is det.
%
% Unifies Mode with the --mode value from the command line
% (standalone, client, server, or worker).

interface:get_mode(Mode) :-
  interface:argv(Options,_),
  lists:memberchk(mode(Mode),Options).


%! interface:process_continue(-Continue) is det.
%
% Determines the continuation after the dispatched action completes.
% Unifies Continue with `halt`, `prolog`, or `true` depending on mode
% and whether --shell was requested.

interface:process_continue(Continue) :-
  !,
  interface:argv(Options,_),
  interface:version(Version),
  lists:memberchk(mode(Mode),Options),

  (lists:memberchk(mode(server),Options)
   ->  message:logo(['::- portage-ng ',Version],Mode),
       Continue = true
   ;   (lists:memberchk(shell(true),Options)
        -> Continue = (message:logo(['::- portage-ng ',Version],Mode), prolog)
        ;  ( catch(daemon:running, _, fail)
           -> Continue = true
           ;  Continue = halt))).


%! interface:get_port(-Port) is det.
%
% Unifies Port with the --port value from the command line, falling back
% to config:server_port/1 if not specified.

interface:get_port(Port) :-
  interface:argv(Options,_),
  ( lists:memberchk(port(Port), Options) -> true
  ; config:server_port(Port)
  ),
  !.

%! interface:process_server(-Host, -Port) is det.
%
% Unifies Host and Port with the --host and --port values from the
% command line, falling back to config:server_host/1 and
% config:server_port/1 respectively.

interface:process_server(Host,Port) :-
  interface:argv(Options,_),
  (lists:memberchk(host(Host),  Options) ; config:server_host(Host)),
  interface:get_port(Port),
  !.


%! interface:server_reachable(+Host, +Port) is semidet.
%
% Succeeds if a TCP connection to Host:Port can be established.

interface:server_reachable(Host, Port) :-
  catch(
    ( tcp_socket(Socket),
      tcp_connect(Socket, Host:Port),
      tcp_close_socket(Socket) ),
    _, fail).


%! interface:require_tls_files(+Role, +Hostname, +CaCert, +Cert, +Key) is det.
%
% Fail with a clear message if TLS material is missing for the given Role
% (client or server). Certificate generation is kept out of runtime: use
% `make certs HOST=<hostname>`. Shared by client.pl and server.pl.

interface:require_tls_files(Role, Hostname, CaCert, Cert, Key) :-
  findall(File,
          ( member(File, [CaCert, Cert, Key]),
            \+ exists_file(File)
          ),
          Missing),
  ( Missing == []
  -> true
  ;  message:failure(['Missing TLS files for ', Role, ' mode: ', Missing, '\n',
                      'Expected CA cert:      ', CaCert, '\n',
                      'Expected ', Role, ' cert:  ', Cert, '\n',
                      'Expected ', Role, ' key:   ', Key, '\n\n',
                      'To generate them locally, run:\n',
                      '  make certs HOST=', Hostname, '\n',
                      'If your hostname includes a .local suffix, ensure HOST matches `config:hostname/1`.\n'
                     ])
  ).


%! interface:init_tty
%
% Initialize TTY-related features (editline, history). Safe to call when
% stdout is redirected; silently skips when not on a real terminal.

interface:init_tty :-
  ( stream_property(user_input, tty(true)),
    stream_property(user_output, tty(true))
  -> ensure_loaded(library('editline')),
     catch(prolog_history(enable), _, true)
  ; true
  ).
