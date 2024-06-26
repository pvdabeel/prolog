/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2024, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> CONFIG
The config context contains general facts and rules related to the prolog
configuration. The parameters described typically do not change at runtime.
*/

:- module(config, []).

% *******************
% CONFIG declarations
% *******************

%! config:name(?Name)
%
% Declares the name of this program

config:name('portage-ng-dev').


%! config:dry_run_build(?Bool)
%
% Declare config:dry_run_build to avoid actually building software.

config:dry_run_build(true).


%! config:installation_dir(?FullPath)
%
% Declaration of the installation directory of the application source code.
% Needs to be a full path. We serialise some Prolog code to this directory.
% May change in a a later release.

config:installation_dir('/Users/pvdabeel/Desktop/Prolog').


%! config:graph_directory(?FullPath)
%
% This application is capable of writing Graphviz dot files and will turn
% them into interactive scalable vector graphics (svg) to enable you to
% browse through a dependency graph.
%
% We store the generated dot and svg files in the following directory.

config:graph_directory('/Volumes/Disk 1/Graph/').


%! config:pkg_directory(?FullPath)
%
% Declaration of the pkg db directory on a system. This holds metadata
% for all packages installed on a system

config:pkg_directory('/Volumes/Disk 1/Repository/pkg').


%! config:number_of_cpus(?Count)
%
% This application parallellizes parsing, proving, planning and building.
% SWI prolog is automatically able to determine the maximum cpu count, but
% sometimes we may want to change this to a value of our choice.

config:number_of_cpus(C) :- current_prolog_flag(cpu_count,C).


%! config:number_of_cpus(?Count)
%
% Different verbosity levels may be configured for printing runtime information

config:verbosity(debug).


%! config:time_limit(?Limit)
%
% When parsing, proving or planning, use the specified time limit to automatically stop
% computation if it takes too long. In seconds.

config:time_limit(6000).


%! config:time_limit(?Limit)
%
% When executing a plan (i.e. building) use the specified time limit to automatically stop
% the build process if it takes too long. Note this limit applies to an entire step in a plan.
% In seconds.

config:time_limit_build(6000).


%! config:graph_modified_only(?Bool)
%
% Set when you want Graphviz dot file to be created for new ebuilds only

config:graph_modified_only(true).


%! config:proving_target(?Target)
%
% Fact which controls the test target for prover, planner, printer and builder
% Set to either:
%
%  - 'install' : Proof using compile-time dependencies only
%  - 'run': Proof using compile- and run-time dependencies

config:proving_target(run).


%! config:test_style(?Style)
%
% Sets the default test style for executing tests. Can be either:
% - single_verbose
% - parallel_verbose
% - parallel_fast

config:test_style(parallel_verbose).


%! config:failsilenton(?Key)
%
% Fails silently (i.e. without messaging failure on the termimal.
% Key can be:
% - version : fails silently when trying to parse versions from git repositories

config:failsilenton(version).
