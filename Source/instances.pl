% **********************
% INSTANCES declarations
% **********************

% For a given class, retrieves all its instances

instances(Class,Instance) :-
  current_module(Instance),
  clause(Instance:'$__meta'(type(instance(Class))),true).

