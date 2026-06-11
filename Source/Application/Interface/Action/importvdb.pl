/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

% Included into the ACTION module via action.pl (see Source/Application/interface.pl
% for dispatch). Implements the --import-vdb request (issue #78).

% -----------------------------------------------------------------------------
%  VDB import (client mode)
% -----------------------------------------------------------------------------

%! action:process_import_vdb(+Mode) is det.
%
% Parses the local VDB into cache: facts and ships them to the server,
% where they are registered as a per-client installed-state repository
% (pkg@<clienthost>). Client mode only: standalone mode proves against
% the local VDB directly and has nothing to import.

action:process_import_vdb(client) :-
  !,
  interface:process_server(Host, Port),
  client:import_vdb(Host, Port).

action:process_import_vdb(_Mode) :-
  message:failure(['--import-vdb is only available in client mode ',
                   '(launch with --mode client).']).
