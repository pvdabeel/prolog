% *******************
% READER declarations
% *******************
%
% The provided reader is capable of reading portage metadata
% cache from a local portage tree. Supports the newer md5 cache format.
%
% Input:  The location of a cache entry. 
%
% Output: A nested list of codes, each sublist represents a line.


% reader:invoke(+Cache,+Entry,-Contents)
%
% Cache: The location of the cache.
%
% Entry: The cache entry to read.
%
% Contents: A contents of the cache entry.
%
% public predicate

reader:invoke(Cache,Entry,Contents) :-
  os:compose_path(Cache,Entry,File),
  exists_file(File),
  open(File,read,Stream,[lock(none)]),
  reader:read_lines(Stream,Contents),
  close(Stream).


% reader:read_lines(+Stream,-Lines)
%
% Given a stream, reads all lines from the stream.
%
% private predicate

reader:read_lines(Stream,[]) :-
  at_end_of_stream(Stream),
  !.

reader:read_lines(Stream,[L|R]) :-
  % not(system:at_end_of_stream(Stream)),
  !,
  read_line_to_codes(Stream,L),
  reader:read_lines(Stream,R).


% reader:read_timestamp(File,Time)
%
% Given a timestamp file, reads the timestamp.
%
% public predicate

reader:read_timestamp(File,Time) :-
  exists_file(File),
  open(File,read,Stream,[lock(none)]),
  reader:read_lines(Stream,[Line|_]),
  phrase(eapi:timestamp(Codes),Line,_),
  close(Stream),
  number_codes(Time,Codes).


% reader:test(+Repository)
%
% Predicate tests whether all cache entries in a given repository
% can be read correctly.
%
% Repository: The repository from which to read all cache entries.
%
% public predicate

reader:test(Repository) :-
  Repository:get_cache(C),
  time(forall(Repository:entry(E),
               (reader:invoke(C,E,_);(message:failure(E)))
	     )),
  Repository:get_size(S),
  message:inform(['read ',S,' cache entries.']).
