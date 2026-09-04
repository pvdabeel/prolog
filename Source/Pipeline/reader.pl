/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> READER
The reader reads lines from a file.
Input:  A full path to a file
Output: A list of strings, each string represents a line.

It also provides the one reading of Gentoo-style configuration files
(profiles, /etc/portage, layout.conf): `#` comments dropped, whitespace
normalized, blank lines removed (reader:config_lines/2).
*/

:- module(reader, []).

% =============================================================================
%  READER declarations
% =============================================================================

%! reader:invoke(+File, -Contents)
%
% File: The full path of the cache.
%
% Contents: A list of strings, each string represents a line

reader:invoke(Stream, Contents) :-
  is_stream(Stream), !,
  reader:read_lines_to_strings(Stream, Contents),
  close(Stream).

reader:invoke(File, Contents) :-
  exists_file(File), !,
  open(File, read, Stream, [lock(none)]),
  reader:read_lines_to_strings(Stream, Contents),
  close(Stream).

reader:invoke(_, []) :-
  !.


%! reader:read_lines_to_strings(+Stream, -Lines)
%
% Given a stream, reads all lines from the stream and returns them as a list of 
% strings

reader:read_lines_to_strings(Stream, Lines) :-
  read_line_to_string(Stream, L),
  ( L == end_of_file
    ->  Lines = []
    ;   Lines = [L|Rest],
        reader:read_lines_to_strings(Stream, Rest) ).

%! reader:read_lines_to_codes(+Stream, -Lines)
%
% Retained for compatibility with code expecting character codes
% Given a stream, reads all lines from the stream and returns as a list of codes

reader:read_lines_to_codes(Stream, Lines) :-
  read_line_to_codes(Stream, L),
  ( L == end_of_file
    ->  Lines = []
    ;   Lines = [L|Rest],
        reader:read_lines_to_codes(Stream, Rest)
    ).


%! reader:config_lines(+File, -Lines)
%
% The meaningful lines of a Gentoo-style configuration file, as strings in
% file order: `#` comments are dropped, whitespace is normalized and blank
% lines are removed. A missing or unreadable file yields [].

reader:config_lines(File, Lines) :-
  catch(read_file_to_string(File, S, []), _, S = ""),
  reader:string_config_lines(S, Lines).


%! reader:string_config_lines(+String, -Lines)
%
% As reader:config_lines/2, for configuration content already in memory.

reader:string_config_lines(S, Lines) :-
  split_string(S, "\n", "\r", Raw),
  findall(L, ( member(R, Raw), reader:config_line(R, L), L \== "" ), Lines).


%! reader:config_line(+Raw, -Line)
%
% Normalize one configuration line: drop a `#` comment and collapse
% whitespace. Line is "" for blank and comment-only lines.

reader:config_line(Raw, Line) :-
  ( sub_string(Raw, Before, _, _, "#")
  -> sub_string(Raw, 0, Before, _, S0)
  ;  S0 = Raw
  ),
  normalize_space(string(Line), S0).


%! reader:test(+Repository)
%
% Predicate tests whether all cache entries belonging to a given repository
% instance can be read correctly.
%
% Repository: The repository instance from which to read all cache entries.

reader:test(Repository) :-
  time(forall(Repository:get_cache_file(E, C),
              (reader:invoke(C, _); (message:failure(E)))
            )),
  Repository:get_size(S), !,
  message:inform(['read ', S, ' ', Repository, ' entries.']).