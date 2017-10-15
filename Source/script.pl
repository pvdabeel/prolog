% *******************
% SCRIPT declarations
% *******************

% Call scripts inside the scripts directory

:- module(script,[]).

% script
%
% Call script with arguments
%
% public predicate

script:exec(S,Args) :-
  is_list(Args),
  atomic_list_concat(['source/scripts/',S],Call),
  atomic_list_concat([Call|Args],' ',Cmd),
  shell(Cmd),!.
