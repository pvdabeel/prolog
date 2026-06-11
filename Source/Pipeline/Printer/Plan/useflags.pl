/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

/** <module> USEFLAGS
USE flag and configuration rendering for build plans.

Renders the per-entry configuration block shown below plan steps: USE flag
diffs (with change annotations relative to the installed version), USE_EXPAND
variables, SLOT info, and download lines. Shared by the plan printer
(plan.pl) and the build display (builder.pl / Printer/Build/build.pl).
*/

:- module(useflags, []).

% =============================================================================
%  USEFLAGS declarations
% =============================================================================

% -----------------------------------------------------------------------------
%  Right-edge indicator helper
% -----------------------------------------------------------------------------

%! useflags:right_edge_ok is det.
%
% Print a green checkmark at the right edge of the terminal (1 space in).

useflags:right_edge_ok :-
  \+ config:output_tty, !.

useflags:right_edge_ok :-
  config:printing_tty_size(_, W),
  Col is W - 1,
  format("\e[~dG", [Col]),
  message:color(green),
  message:print('\u2713'),
  message:color(normal).


% -----------------------------------------------------------------------------
%  Config item prefixes
% -----------------------------------------------------------------------------

%! useflags:print_config_prefix(+Word)
%
% prints the prefix for a config item

% -------------------------------
% CASE: Fancy build plan printing
% -------------------------------

useflags:print_config_prefix(Word) :-
  config:printing_style('fancy'),!,
  nl,write('             │           '),
  message:color(darkgray),
  message:print('└─ '),
  message:print(Word),
  message:print(' ─┤ '),
  message:color(normal).

% -------------------------------
% CASE: Short build plan printing
% -------------------------------

useflags:print_config_prefix(_Word) :-
  config:printing_style('short'),!,
  nl,write('             │           ').

% --------------------------------
% CASE: Column build plan printing
% --------------------------------

useflags:print_config_prefix(file) :-
  config:printing_style('column'),!,
  message:column(104,' ').

useflags:print_config_prefix(live) :-
  config:printing_style('column'),!,
  message:column(104,' ').

useflags:print_config_prefix('conf') :-
  config:printing_style('column'), !,
  message:column(104,' ').


%! useflags:print_config_prefix
%
% prints the prefix for a config item

useflags:print_config_prefix :-
  config:printing_style('fancy'),!,
  nl,write('             │          '),
  message:color(darkgray),
  message:print('          │ '),
  message:color(normal).

useflags:print_config_prefix :-
  config:printing_style('short'),!,
  nl,write('             │           ').

useflags:print_config_prefix :-
  config:printing_style('column'),!,
  nl,write('             │ '),
  message:column(104,' ').


% -----------------------------------------------------------------------------
%  Slot resolution
% -----------------------------------------------------------------------------

%! useflags:resolve_slot(+Repository://Entry, +Context, -Slot)
%
% Extract slot info from proof context; fall back to KB query when absent.
% Returns [] for default slot 0 or when no slot is available.
%
% This module renders plans, so it only ever runs where the KB is local
% (standalone, or server-side in client mode — see Action/merge.pl). Direct
% query:search/2 calls let the compile-time macros inline the cache lookups
% (issue #57); kb:query would pay the instance-dispatch + runtime-expansion
% meta layers on every call.

useflags:resolve_slot(Repository://Entry, Context, Slot) :-
  ( memberchk(slot(_,_,Slot):{Repository://Entry}, Context)
  -> true
  ; query:search(slot(S), Repository://Entry)
  -> ( query:search(subslot(Sub), Repository://Entry)
     -> Slot = [slot(S), subslot(Sub)]
     ;  Slot = [slot(S)]
     )
  ; Slot = []
  ).


% -----------------------------------------------------------------------------
%  Config display entry point
% -----------------------------------------------------------------------------

%! useflags:print_config(+Repository://+Entry:+Action:+Context)
%
% Prints the configuration for a given repository entry (USE flags, USE expand, ...)

% ----------------------
% CASE: fetchonly action
% ----------------------

% iuse empty

useflags:print_config(Repository://Entry:fetchonly?{_Context}) :-
  \+(query:search(iuse(_),Repository://Entry)),!.

% use flags to show

useflags:print_config(Repository://Entry:fetchonly?{Context}) :-
 !,
 useflags:print_config(Repository://Entry:install?{Context}).



% ---------------------
% CASE: download action
% ---------------------

% live downloads

useflags:print_config(Repository://Ebuild:download?{_Context}) :-
  ebuild:is_live(Repository://Ebuild),!,
  useflags:print_config_prefix('live'),
  useflags:print_config_item('download','git repository','live').


% no downloads

useflags:print_config(Repository://Ebuild:download?{_Context}) :-
  \+(query:search(manifest(preference,_,_,_),Repository://Ebuild)),!.


% at least one download

useflags:print_config(Repository://Ebuild:download?{_Context}) :-
  !,
  findall([File,Size],query:search(manifest(preference,_,File,Size),Repository://Ebuild),Downloads),
  sort(Downloads,[[FirstFile,FirstSize]|Rest]),
  useflags:print_config_prefix('file'),
  useflags:print_config_item('download',FirstFile,FirstSize),
  forall(member([RestFile,RestSize],Rest),
         (useflags:print_config_prefix,
          useflags:print_config_item('download',RestFile,RestSize))).


% --------------------
% CASE: Install action
% --------------------

% iuse empty

useflags:print_config(Repository://Entry:install?{Context}) :-
  \+(query:search(iuse(_),Repository://Entry)),!,
  useflags:resolve_slot(Repository://Entry, Context, Slot),
  (Slot \== [], Slot \== [slot('0')]
  -> useflags:print_config_prefix('conf'),
     useflags:print_config_item('slot',Slot)
  ;  true).

% use flags to show

useflags:print_config(Repository://Entry:install?{Context}) :-
  !,
  useflags:collect_context_assumed_use(Context, Assumed),

  useflags:set_old_use_context(Repository://Entry, Context),

  % Get regular USE flags (filtered, excluding USE_EXPAND)
  findall([Reason,Group], group_by(Reason, Use, query:search(iuse_filtered(Use,Reason),Repository://Entry), Group), Useflags),

  % Get all USE flags (including USE_EXPAND ones) for USE_EXPAND processing
  findall(Use, query:search(iuse(Use, _Reason), Repository://Entry), AllUseFlags),

  % Separate regular USE flags from USE_EXPAND flags
  partition(useflags:is_use_expand_flag, AllUseFlags, UseExpandFlags, _RegularUseFlags),

  % Group USE_EXPAND flags by expand key and reason
  findall([ExpandKey, ExpandFlags],
          useflags:group_use_expand_flags(UseExpandFlags, ExpandKey, ExpandFlags, Repository://Entry),
          UseExpandVariables),

  % Filter out empty USE_EXPAND variables
  include(useflags:valid_use_expand, UseExpandVariables, ValidUseExpandVariables),

  useflags:resolve_slot(Repository://Entry, Context, Slot),
  ( Slot \== [], Slot \== [slot('0')]
  -> ( Useflags == [], ValidUseExpandVariables == []
     -> useflags:print_config_prefix('conf'),
        useflags:print_config_item('slot',Slot)
     ;  useflags:print_config_prefix('conf'),
        useflags:print_config_items_aligned(Useflags, ValidUseExpandVariables, Assumed, Slot)
     )
  ;  ( Useflags == [], ValidUseExpandVariables == []
     -> true
     ;  useflags:print_config_prefix('conf'),
        useflags:print_config_items_aligned(Useflags, ValidUseExpandVariables, Assumed, [])
     )
  ),!.


% --------------------
% CASE: Update action
% --------------------
%
% Print the same configuration block as for installs (USE flags, USE_EXPAND, slot).
% Update actions are transactional same-slot replacements, so the config shown is
% for the *new* version being merged.

useflags:print_config(Repository://Entry:update?{Context}) :-
  !,
  useflags:print_config(Repository://Entry:install?{Context}).

useflags:print_config(Repository://Entry:downgrade?{Context}) :-
  !,
  useflags:print_config(Repository://Entry:install?{Context}).

useflags:print_config(Repository://Entry:reinstall?{Context}) :-
  !,
  useflags:print_config(Repository://Entry:install?{Context}).


% ----------------
% CASE: Run action
% ----------------

useflags:print_config(_://_:run?{_Context}) :- !.


% -------------------
% CASE: Other actions
% -------------------

useflags:print_config(_://_:_?_) :- !.


% -----------------------------------------------------------------------------
%  USE_EXPAND helpers
% -----------------------------------------------------------------------------

%! useflags:is_use_expand_flag(+UseFlag)
%
% True when UseFlag is prefixed by a known USE_EXPAND key.

useflags:is_use_expand_flag(UseFlag) :-
  eapi:use_expand(ExpandKey),
  eapi:check_prefix_atom(ExpandKey, UseFlag).


%! useflags:group_use_expand_flags(+UseExpandFlags, -ExpandKey, -ExpandFlags, +Repository://Entry)
%
% Group UseExpandFlags by their USE_EXPAND key, stripping the prefix and
% looking up each flag's reason (positive/negative:source) via IUSE metadata.
% Skips hidden expand keys.

useflags:group_use_expand_flags(UseExpandFlags, ExpandKey, ExpandFlags, Repository://Entry) :-
  eapi:use_expand(ExpandKey),
  \+ config:use_expand_hidden(ExpandKey),
  findall(UseFlag,
          (member(UseFlag, UseExpandFlags),
           eapi:check_prefix_atom(ExpandKey, UseFlag)),
          MatchingFlags),
  MatchingFlags \== [],
  % Group by reason and extract suffix
  findall([Reason, Group],
          group_by(Reason, Suffix,
                   (member(UseFlag, MatchingFlags),
                    eapi:strip_prefix_atom(ExpandKey, UseFlag, Suffix),
                    query:search(iuse(UseFlag, Reason), Repository://Entry)),
                   Group),
          ExpandFlags).


%! useflags:valid_use_expand(+KeyFlagsPair)
%
% True when the USE_EXPAND variable has at least one flag.

useflags:valid_use_expand([_Key, Flags]) :-
  Flags \== [].


% -----------------------------------------------------------------------------
%  Aligned config item printing
% -----------------------------------------------------------------------------

%! useflags:print_config_items_aligned(+Useflags, +ValidUseExpandVariables, +Assumed, +Slot)
%
% Print USE flags, USE_EXPAND variables, and SLOT with aligned formatting.

useflags:print_config_items_aligned(Useflags, ValidUseExpandVariables, Assumed, Slot) :-

  % 1. First print USE flags with proper formatting and alignment
  nb_setval(plan_use_expand_prefix, ''),
  useflags:print_config_item_aligned('use', Useflags, Assumed),

  % 2. Second print USE_EXPAND variables with proper formatting and alignment
  (ValidUseExpandVariables == [] -> true ;
   forall(member([Key, Keyflags], ValidUseExpandVariables),
          (atom_concat(Key, '_', ExpandPrefix),
           nb_setval(plan_use_expand_prefix, ExpandPrefix),
           useflags:print_config_prefix,
           useflags:print_config_item_aligned(Key, Keyflags, Assumed),
           nb_setval(plan_use_expand_prefix, '')))),

  % 3. Lastly print SLOT with proper formatting and alignment
  (Slot == [] -> true ;
   (useflags:print_config_prefix,
    useflags:print_config_item_aligned('slot', Slot, []))).


%! useflags:print_config_item_aligned(+Key, +Value, +Assumed)
%
% Print a single KEY = "value" configuration line with bubble formatting.

% Helper predicate: Print Use flags
useflags:print_config_item_aligned('use', List, Assumed) :-
  !,
  upcase_atom('use', KeyU),
  message:bubble(darkgray,KeyU),
  message:print(' = "'),
  catch(
      ( config:printing_tty_size(_, TermWidth),
        line_position(current_output, StartCol),
        useflags:collect_all_flags(List, Assumed, AllFlags),
        useflags:print_flags_wrapped(AllFlags, StartCol, TermWidth)
      ),
      error(io_error(check, stream(_)), _),
      ( useflags:collect_all_flags(List, Assumed, AllFlags),
        useflags:print_flags_unwrapped(AllFlags)
      )
  ),
  message:print('"'),
  useflags:maybe_print_use_descriptions(AllFlags).


useflags:print_config_item_aligned('slot', Slot, _) :-
  !,
  upcase_atom('slot', KeyU),
  message:bubble(darkgray,KeyU),
  message:print(' = "'),
  message:color(darkgray),
  useflags:print_slot_value(Slot),
  message:color(normal),
  message:print('"').

useflags:print_config_item_aligned(Key, Keyflags, Assumed) :-
  eapi:use_expand(Key),
  !,
  upcase_atom(Key, KeyU),
  message:bubble(darkgray,KeyU),
  message:print(' = "'),
  config:printing_tty_size(_, TermWidth),
  line_position(current_output, StartCol),
  useflags:collect_all_flags(Keyflags, Assumed, AllFlags),
  useflags:print_flags_wrapped(AllFlags,StartCol,TermWidth),
  message:print('"').


%! useflags:print_config_item(+Key,+Value)
%
% Prints a configuration item for a given repository entry

useflags:print_config_item('download',File,'live') :-
  !,
  message:color(magenta),
  message:print_bytes('live'),
  message:color(normal),
  message:print(' '),
  message:print(File).

useflags:print_config_item('download',File,Size) :-
  !,
  message:color(magenta),
  message:print_bytes(Size),
  message:color(normal),
  message:print(' '),
  message:print(File),
  ( distfiles:present(File)
  -> useflags:right_edge_ok
  ;  true
  ).

useflags:print_config_item('slot',Slot) :- !,
  upcase_atom('slot',KeyS),
  message:bubble(darkgray,KeyS),
  message:print(' = "'),
  message:color(darkgray),
  useflags:print_slot_value(Slot),
  message:color(normal),
  message:print('"').


%! useflags:print_slot_value(+Slot)
%
% Prints the slot value in a readable format

useflags:print_slot_value([slot(Slot)]) :-
  !,
  message:print(Slot).

useflags:print_slot_value([slot(Slot),subslot(Subslot)]) :-
  !,
  message:print(Slot),
  message:print('/'),
  message:print(Subslot).

useflags:print_slot_value([slot(Slot),subslot(Subslot),equal]) :-
  !,
  message:print(Slot),
  message:print('/'),
  message:print(Subslot),
  message:print('=').

useflags:print_slot_value([slot(Slot),equal]) :-
  !,
  message:print(Slot),
  message:print('=').

useflags:print_slot_value(Slot) :-
  message:print(Slot).


% -----------------------------------------------------------------------------
%  Flag wrapping
% -----------------------------------------------------------------------------

%! useflags:print_flags_wrapped(+AllFlags, +StartCol, +TermWidth)
%
% Prints a list of flags wrapped to the terminal width.

useflags:print_flags_wrapped(AllFlags, StartCol, TermWidth) :-
    foldl(useflags:print_one_flag_wrapped(StartCol,TermWidth),
          AllFlags,
          [StartCol, true],
          _).


%! useflags:print_one_flag_wrapped(+StartCol, +TermWidth, +FlagTerm, +StateIn, -StateOut)
%
% Prints a single flag wrapped to the terminal width.

useflags:print_one_flag_wrapped(StartCol, TermWidth, flag(Type, Flag, Assumed), [ColIn, IsFirst], [ColOut, false]) :-
    useflags:get_flag_length(Type, Flag, Assumed, FlagLen),
    (IsFirst -> SpaceLen = 0 ; SpaceLen = 1),
    (
        ( ColIn + SpaceLen + FlagLen > TermWidth )
    ->  % Wrap
        (
            useflags:print_continuation_prefix(StartCol),      % go to next line, print prefix, jump to start position
            useflags:print_use_flag(Type, Flag, Assumed),      % print flag
            ColOut is StartCol + FlagLen
        )
    ;   % No wrap
        (
            (IsFirst -> true ; write(' ')),
            useflags:print_use_flag(Type, Flag, Assumed),
            ColOut is ColIn + SpaceLen + FlagLen
        )
    ).


%! useflags:print_continuation_prefix(+IndentColumn)
%
% Prints the continuation prefix for wrapped flags.

useflags:print_continuation_prefix(StartColumn) :-
    nl,

    ( config:printing_style('short')  ->
        write('             │ '),
        NewStartColumn is StartColumn - 1,
        message:column(NewStartColumn,'')
    );

    ( config:printing_style('column') ->
        write('             │ '),
        NewStartColumn is StartColumn - 1,
        message:column(NewStartColumn,'')
    );
    ( config:printing_style('fancy')  ->
        write('             │                    '),
        message:color(darkgray),
        write('│ '),
        NewStartColumn is StartColumn - 1,
        message:column(NewStartColumn,'')
    );
    true.


% -----------------------------------------------------------------------------
%  Flag collection
% -----------------------------------------------------------------------------

%! useflags:collect_all_flags(+List, +Assumed, -AllFlags)
%
% Collect flags from all reason categories (positive/negative ×
% ebuild/preference/package_use/profile) into a flat list of flag terms.
% Used for both regular USE flags and USE_EXPAND variables.

useflags:collect_all_flags(List, Assumed, AllFlags) :-
    (memberchk([negative:default,NegDefa],List);    NegDefa=[]),
    (memberchk([negative:ebuild,NegEbui],List);     NegEbui=[]),
    (memberchk([negative:preference,NegPref],List); NegPref=[]),
    (memberchk([negative:package_use,NegPkgUse],List); NegPkgUse=[]),
    (memberchk([negative:profile_package_use_mask,NegProfileMask],List); NegProfileMask=[]),
    (memberchk([positive:ebuild,PosEbui],List);     PosEbui=[]),
    (memberchk([positive:preference,PosPref],List); PosPref=[]),
    (memberchk([positive:package_use,PosPkgUse],List); PosPkgUse=[]),
    (memberchk([positive:profile_package_use_force,PosProfileForce],List); PosProfileForce=[]),
    sort(PosPref, OPosPref),
    sort(PosEbui, OPosEbui),
    sort(PosPkgUse, OPosPkgUse),
    sort(PosProfileForce, OPosProfileForce),
    sort(NegPref, ONegPref),
    sort(NegEbui, ONegEbui),
    sort(NegPkgUse, ONegPkgUse),
    sort(NegProfileMask, ONegProfileMask),
    sort(NegDefa, ONegDefa),
    maplist(useflags:to_flag_term(positive:preference, Assumed), OPosPref, FlagsPosPref),
    maplist(useflags:to_flag_term(positive:package_use, Assumed), OPosPkgUse, FlagsPosPkgUse),
    maplist(useflags:to_flag_term(positive:profile_package_use_force, Assumed), OPosProfileForce, FlagsPosProfileForce),
    maplist(useflags:to_flag_term(positive:ebuild, Assumed), OPosEbui, FlagsPosEbui),
    maplist(useflags:to_flag_term(negative:preference, Assumed), ONegPref, FlagsNegPref),
    maplist(useflags:to_flag_term(negative:package_use, Assumed), ONegPkgUse, FlagsNegPkgUse),
    maplist(useflags:to_flag_term(negative:profile_package_use_mask, Assumed), ONegProfileMask, FlagsNegProfileMask),
    maplist(useflags:to_flag_term(negative:ebuild, Assumed), ONegEbui, FlagsNegEbui),
    maplist(useflags:to_flag_term(negative:default, Assumed), ONegDefa, FlagsNegDefa),
    append([FlagsPosPref, FlagsPosPkgUse, FlagsPosProfileForce, FlagsPosEbui,
            FlagsNegPref, FlagsNegPkgUse, FlagsNegProfileMask, FlagsNegEbui, FlagsNegDefa],
           AllFlags).


%! useflags:to_flag_term(+Type, +Assumed, +Flag, -FlagTerm)
%
% Converts a flag to a flag term.

useflags:to_flag_term(Type, Assumed, Flag, flag(Type, Flag, Assumed)).


%! useflags:collect_context_assumed_use(+Context, -Assumed) is det.
%
% Collect USE overrides for plan display: legacy required_use/build_with_use
% lists, canonical build_with_use:use_state/2, and suggestion(use_change).

useflags:collect_context_assumed_use(Context, Assumed) :-
  findall(A, useflags:context_assumed_use_atom(Context, A), Assumed0),
  useflags:use_changes_to_assumed(Context, SuggAssumed),
  append(Assumed0, SuggAssumed, AssumedDup),
  sort(AssumedDup, Assumed).


useflags:context_assumed_use_atom(Context, Use) :-
  member(Term, Context),
  ( Term = required_use(Uses) ; Term = build_with_use(Uses) ),
  is_list(Uses),
  member(assumed(Use), Uses).
useflags:context_assumed_use_atom(Context, Flag) :-
  memberchk(build_with_use:use_state(En, _Dis), Context),
  member(Flag, En).
useflags:context_assumed_use_atom(Context, minus(Flag)) :-
  memberchk(build_with_use:use_state(_En, Dis), Context),
  member(Flag, Dis).


%! useflags:use_changes_to_assumed(+Context, -Assumed)
%
% Extract USE flag changes from suggestion(use_change, ...) in the Context
% and convert them to the Assumed list format used by print_use_flag.
% For USE_EXPAND flags, both the full prefixed name and the stripped suffix
% are included so matching works in both regular USE and USE_EXPAND displays.

useflags:use_changes_to_assumed(Context, Assumed) :-
  ( is_list(Context),
    memberchk(suggestion(use_change, _, Changes), Context),
    is_list(Changes)
  ->
    findall(A,
            ( member(Change, Changes),
              useflags:use_change_to_assumed_atom(Change, A)
            ),
            Assumed)
  ; Assumed = []
  ).

useflags:use_change_to_assumed_atom(use_change(F, enable), F).
useflags:use_change_to_assumed_atom(use_change(F, enable), Stripped) :-
  eapi:use_expand(Key),
  eapi:strip_prefix_atom(Key, F, Stripped).
useflags:use_change_to_assumed_atom(use_change(F, disable), minus(F)).
useflags:use_change_to_assumed_atom(use_change(F, disable), minus(Stripped)) :-
  eapi:use_expand(Key),
  eapi:strip_prefix_atom(Key, F, Stripped).


%! useflags:print_flags_unwrapped(+AllFlags)
%
% Prints a list of flags unwrapped.

useflags:print_flags_unwrapped([]) :- !.
useflags:print_flags_unwrapped([flag(Type, Flag, Assumed)|Rest]) :-
    useflags:print_use_flag(Type, Flag, Assumed),
    (Rest == [] -> true ; write(' ')),
    useflags:print_flags_unwrapped(Rest).


% -----------------------------------------------------------------------------
%  Flag lengths (for wrapping)
% -----------------------------------------------------------------------------

%! useflags:get_flag_length(+Type, +Flag, +Assumed, -Length)
%
% Gets the length of a flag.

useflags:get_flag_length(Type, Flag, Assumed, Length) :-
    (   memberchk(minus(Flag), Assumed)
    ->  atom_length(Flag, L), Length is L + 1
    ;   memberchk(Flag, Assumed)
    ->  atom_length(Flag, Length)
    ;   useflags:get_flag_length_typed(Type, Flag, BaseLen),
        useflags:get_change_extra_length(Type, Flag, ChangeExtra),
        Length is BaseLen + ChangeExtra
    ).


%! useflags:get_change_extra_length(+Type, +Flag, -ExtraLen)
%
% Returns the extra length from change annotations for line wrapping.

useflags:get_change_extra_length(positive:_, Flag, Extra) :-
    !,
    useflags:change_annotation_length(Flag, positive, Extra).
useflags:get_change_extra_length(negative:_, Flag, Extra) :-
    !,
    useflags:change_annotation_length(Flag, negative, Extra).
useflags:get_change_extra_length(_, _, 0).

useflags:get_flag_length_typed(positive:preference, Flag, Length) :-
    atom_length(Flag, L),
    ( preference:global_use(Flag,env) -> EnvExtra = 1 ; EnvExtra = 0),
    ( preference:profile_forced_use_flag(Flag) -> ProfileExtra = 1 ; ProfileExtra = 0),
    Length is L + EnvExtra + ProfileExtra.

useflags:get_flag_length_typed(positive:package_use, Flag, Length) :-
    atom_length(Flag, Length).

useflags:get_flag_length_typed(positive:profile_package_use_force, Flag, Length) :-
    atom_length(Flag, L),
    Length is L + 2. % parentheses

useflags:get_flag_length_typed(positive:ebuild, Flag, Length) :-
    atom_length(Flag, Length).

useflags:get_flag_length_typed(negative:preference, Flag, Length) :-
    atom_length(Flag, L),
    ( preference:global_use(minus(Flag),env) -> EnvExtra = 1 ; EnvExtra = 0), % '*' marker
    ( preference:profile_masked_use_flag(Flag) -> ProfileExtra = 1 ; ProfileExtra = 0), % '%' marker
    Length is L + 1 + EnvExtra + ProfileExtra.

useflags:get_flag_length_typed(negative:package_use, Flag, Length) :-
    atom_length(Flag, L),
    Length is L + 1.

useflags:get_flag_length_typed(negative:profile_package_use_mask, Flag, Length) :-
    atom_length(Flag, L),
    Length is L + 3. % (-flag)

useflags:get_flag_length_typed(negative:ebuild, Flag, Length) :-
    atom_length(Flag, L),
    Length is L + 1.

useflags:get_flag_length_typed(negative:default, Flag, Length) :-
    atom_length(Flag, L),
    Length is L + 1.


% -----------------------------------------------------------------------------
%  Old USE/IUSE comparison helpers
% -----------------------------------------------------------------------------

%! useflags:set_old_use_context(+RepositoryEntry, +Context)
%
% Looks up the installed version's USE/IUSE sets for the given entry
% and stores them via nb_setval for use by print_use_flag/3.
% For new packages (no installed version), sets IsNew = true.

useflags:set_old_use_context(Repository://Entry, Context) :-
    cache:ordered_entry(Repository, Entry, Category, Name, _),
    (memberchk(slot(_,_,SlotList):{Repository://Entry}, Context),
     SlotList = [slot(SlotAtom)|_]
    -> true
    ;  SlotAtom = _),
    (cache:ordered_entry(pkg, InstalledEntry, Category, Name, _),
     (nonvar(SlotAtom)
     -> cache:entry_metadata(pkg, InstalledEntry, slot, slot(SlotAtom))
     ;  true)
    -> use:vdb_enabled_use_set(pkg://InstalledEntry, OldUse),
       use:entry_iuse_set(pkg://InstalledEntry, OldIuse),
       nb_setval(plan_old_use_info, old_use_info(false, OldUse, OldIuse))
    ;  nb_setval(plan_old_use_info, old_use_info(true, [], []))
    ),
    nb_setval(plan_use_expand_prefix, ''),
    !.
useflags:set_old_use_context(_, _) :-
    nb_setval(plan_old_use_info, old_use_info(true, [], [])),
    nb_setval(plan_use_expand_prefix, '').


%! useflags:use_flag_full_name(+Flag, -FullFlag)
%
% Reconstructs the full USE flag name by prepending the current
% USE_EXPAND prefix (if any).

useflags:use_flag_full_name(Flag, FullFlag) :-
    nb_current(plan_use_expand_prefix, Prefix),
    Prefix \== '',
    !,
    atom_concat(Prefix, Flag, FullFlag).
useflags:use_flag_full_name(Flag, Flag).


%! useflags:use_flag_change_type(+Flag, +Polarity, -ChangeType)
%
% Determines the change type of a USE flag relative to the installed version.
% ChangeType is one of: steady, changed, new_flag.

useflags:use_flag_change_type(Flag, Polarity, ChangeType) :-
    nb_current(plan_old_use_info, old_use_info(IsNew, OldUse, OldIuse)),
    !,
    useflags:use_flag_full_name(Flag, FullFlag),
    useflags:classify_flag_change(FullFlag, Polarity, IsNew, OldUse, OldIuse, ChangeType).
useflags:use_flag_change_type(_, _, steady).


%! useflags:classify_flag_change(+Flag, +Polarity, +IsNew, +OldUse, +OldIuse, -ChangeType)
%
% Core classification: compares a flag against the installed USE/IUSE sets.

useflags:classify_flag_change(_, _, true, _, _, steady) :- !.
useflags:classify_flag_change(Flag, positive, false, OldUse, OldIuse, ChangeType) :-
    !,
    (\+ memberchk(Flag, OldIuse)
    -> ChangeType = new_flag
    ;  \+ memberchk(Flag, OldUse)
    -> ChangeType = changed
    ;  ChangeType = steady).
useflags:classify_flag_change(Flag, negative, false, OldUse, OldIuse, ChangeType) :-
    !,
    (\+ memberchk(Flag, OldIuse)
    -> ChangeType = new_flag
    ;  memberchk(Flag, OldUse)
    -> ChangeType = changed
    ;  ChangeType = steady).
useflags:classify_flag_change(_, _, _, _, _, steady).


%! useflags:change_annotation_length(+Flag, +Polarity, -ExtraLen)
%
% Returns the extra character length added by change annotations.

useflags:change_annotation_length(Flag, Polarity, ExtraLen) :-
    useflags:use_flag_change_type(Flag, Polarity, ChangeType),
    (ChangeType == changed
    -> ExtraLen = 1
    ;  ChangeType == new_flag, Polarity == positive
    -> ExtraLen = 2
    ;  ChangeType == new_flag
    -> ExtraLen = 1
    ;  ExtraLen = 0).


%! useflags:maybe_print_change_annotation(+Flag, +Polarity)
%
% Prints a change annotation suffix when the flag state differs from
% the installed version: * for changed, %* for new enabled, % for new disabled.

useflags:maybe_print_change_annotation(Flag, Polarity) :-
    useflags:use_flag_change_type(Flag, Polarity, ChangeType),
    (ChangeType == changed
    -> message:print('*')
    ;  ChangeType == new_flag, Polarity == positive
    -> message:print('%*')
    ;  ChangeType == new_flag, Polarity == negative
    -> message:print('%')
    ;  true).


% -----------------------------------------------------------------------------
%  Flag rendering
% -----------------------------------------------------------------------------

%! useflags:print_easy_positive(+ChangeType, +Flag)
%
% Prints an enabled USE flag with easy-palette coloring based on change type.

useflags:print_easy_positive(steady, Flag) :-
    message:color(red),
    message:style(bold),
    message:print(Flag),
    message:color(normal).
useflags:print_easy_positive(changed, Flag) :-
    message:color(green),
    message:style(bold),
    message:print(Flag),
    message:color(normal),
    message:print('*').
useflags:print_easy_positive(new_flag, Flag) :-
    message:color(lightorange),
    message:style(bold),
    message:print(Flag),
    message:color(normal),
    message:print('%*').


%! useflags:print_easy_negative(+ChangeType, +Flag)
%
% Prints a disabled USE flag with easy-palette coloring based on change type.

useflags:print_easy_negative(steady, Flag) :-
    message:color(blue),
    message:style(bold),
    message:print('-'),
    message:print(Flag),
    message:color(normal).
useflags:print_easy_negative(changed, Flag) :-
    message:color(green),
    message:style(bold),
    message:print('-'),
    message:print(Flag),
    message:color(normal),
    message:print('*').
useflags:print_easy_negative(new_flag, Flag) :-
    message:color(lightorange),
    message:style(bold),
    message:print('-'),
    message:print(Flag),
    message:color(normal),
    message:print('%').


%! useflags:print_use_flag(+Reason,+Flag,Assumed)
%
% Prints a single flag.

useflags:print_use_flag(_Reason, Flag, Assumed) :-
  memberchk(minus(Flag), Assumed), !,
  message:color(orange),
  %message:style(bold),
  message:print('-'),
  message:print(Flag),
  message:color(normal).

useflags:print_use_flag(_Reason, Flag, Assumed) :-
  memberchk(Flag, Assumed), !,
  message:color(orange),
  %message:style(bold),
  message:print(Flag),
  message:color(normal).

useflags:print_use_flag(positive:Reason, Flag, _Assumed) :-
  config:color_palette(easy),
  Reason \== profile_package_use_force, !,
  useflags:use_flag_change_type(Flag, positive, ChangeType),
  useflags:print_easy_positive(ChangeType, Flag).

useflags:print_use_flag(negative:Reason, Flag, _Assumed) :-
  config:color_palette(easy),
  Reason \== profile_package_use_mask, !,
  useflags:use_flag_change_type(Flag, negative, ChangeType),
  useflags:print_easy_negative(ChangeType, Flag).

useflags:print_use_flag(positive:preference, Flag, _Assumed) :-
  preference:global_use(Flag,env), !,
  message:color(green),
  message:style(bold),
  message:print(Flag),
  message:color(normal),
  ( preference:profile_forced_use_flag(Flag) -> message:print('%') ; true ),
  message:print('*').

useflags:print_use_flag(positive:profile_package_use_force, Flag, _Assumed) :-
  !,
  message:color(green),
  message:style(bold),
  message:print('('),
  message:print(Flag),
  message:print(')'),
  message:color(normal).

useflags:print_use_flag(positive:preference, Flag, _Assumed) :-
  !,
  message:color(red),
  message:style(bold),
  message:print(Flag),
  message:color(normal),
  ( preference:profile_forced_use_flag(Flag) -> message:print('%') ; true ).

useflags:print_use_flag(positive:package_use, Flag, _Assumed) :-
  !,
  message:color(red),
  message:style(bold),
  message:print(Flag),
  message:color(normal),
  useflags:maybe_print_change_annotation(Flag, positive).

useflags:print_use_flag(positive:ebuild, Flag, _Assumed) :-
  !,
  message:color(red),
  message:style(italic),
  message:print(Flag),
  message:color(normal),
  useflags:maybe_print_change_annotation(Flag, positive).

useflags:print_use_flag(negative:preference, Flag, _Assumed) :-
  preference:global_use(minus(Flag),env), !,
  message:color(green),
  message:style(bold),
  message:print('-'),
  message:print(Flag),
  message:color(normal),
  ( preference:profile_masked_use_flag(Flag) -> message:print('%') ; true ),
  message:print('*').

useflags:print_use_flag(negative:profile_package_use_mask, Flag, _Assumed) :-
  !,
  message:color(green),
  message:style(bold),
  message:print('('),
  message:print('-'),
  message:print(Flag),
  message:print(')'),
  message:color(normal).

useflags:print_use_flag(negative:preference, Flag, _Assumed) :-
  !,
  message:color(blue),
  message:style(bold),
  message:print('-'),
  message:print(Flag),
  message:color(normal),
  ( preference:profile_masked_use_flag(Flag) -> message:print('%') ; true ).

useflags:print_use_flag(negative:package_use, Flag, _Assumed) :-
  !,
  message:color(blue),
  message:style(bold),
  message:print('-'),
  message:print(Flag),
  message:color(normal),
  useflags:maybe_print_change_annotation(Flag, negative).

useflags:print_use_flag(negative:ebuild, Flag, _Assumed) :-
  !,
  message:color(lightblue),
  message:style(italic),
  message:print('-'),
  message:print(Flag),
  message:color(normal),
  useflags:maybe_print_change_annotation(Flag, negative).

useflags:print_use_flag(negative:default, Flag, _Assumed) :-
  !,
  message:color(darkgray),
  message:style(italic),
  message:print('-'),
  message:print(Flag),
  message:color(normal),
  useflags:maybe_print_change_annotation(Flag, negative).


% -----------------------------------------------------------------------------
%  USE flag descriptions
% -----------------------------------------------------------------------------

%! useflags:maybe_print_use_descriptions(+AllFlags) is det.
%
% When --show-descriptions is active, prints a compact description
% block below the USE flag line.

useflags:maybe_print_use_descriptions(AllFlags) :-
  ( config:show_use_descriptions(Mode) ->
    include(useflags:flag_has_description(Mode), AllFlags, Described),
    ( Described \== [] ->
      nl,
      forall(member(flag(_, Flag, _), Described),
        ( profile:use_description(Flag, Desc) ->
          format('             │   '),
          message:color(darkgray),
          format('~w: ~w', [Flag, Desc]),
          message:color(normal),
          nl
        ; true
        ))
    ; true
    )
  ; true
  ).


%! useflags:flag_has_description(+Mode, +FlagTerm) is semidet.
%
% Filter for which flags to show descriptions. Mode is 'all'
% or 'new' (only flags from positive:ebuild or negative:ebuild).

useflags:flag_has_description(all, flag(_, Flag, _)) :-
  profile:use_description(Flag, _).
useflags:flag_has_description(new, flag(positive:ebuild, Flag, _)) :-
  profile:use_description(Flag, _).
useflags:flag_has_description(new, flag(negative:ebuild, Flag, _)) :-
  profile:use_description(Flag, _).
useflags:flag_has_description(new, flag(negative:default, Flag, _)) :-
  profile:use_description(Flag, _).
useflags:flag_has_description(changed, flag(positive:ebuild, Flag, _)) :-
  profile:use_description(Flag, _).
useflags:flag_has_description(changed, flag(negative:ebuild, Flag, _)) :-
  profile:use_description(Flag, _).
