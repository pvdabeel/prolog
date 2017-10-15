% Copyright 2005-2016 Pieter Van den Abeele 
%
% Author: pvdabeel@mac.com
%
% $Header$

% ensure loaded eapi

% *******************
% EBUILD declarations
% *******************

% Case 1: The ebuild has Metadata for the requested key

ebuild:get(Key,Entry,Content) :-
  cache:entry(_,Entry,_,_,_,_,Metadata),
  eapi:elem(Key,Metadata,Content).


% Case 2: The ebuild does not have Metadata for the requested key

ebuild:get(Key,Entry,[]) :-
  cache:entry(_,Entry,_,_,_,_,Metadata),
  not(eapi:elem(Key,Metadata,_)).
