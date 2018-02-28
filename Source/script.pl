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
  atomic_list_concat(['Source/Scripts/',S],Call),
  atomic_list_concat([Call|Args],' ',Cmd),
  shell(Cmd),!.
