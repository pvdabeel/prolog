/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2025, Pieter Van den Abeele
  
  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

% =============================================================================
%  Hostname-specific configuration - vm-linux.local
% =============================================================================


% -----------------------------------------------------------------------------
%  /etc/portage configuration directory
% -----------------------------------------------------------------------------
%
% Real /etc/portage. userconfig:load reads make.conf, package.use,
% package.mask, package.unmask, package.accept_keywords, package.license
% from this directory. builder:execute_suggestion writes prover-derived
% overrides into the package.{use,unmask,accept_keywords}/00portage-ng-auto
% file under it.

config:portage_confdir('/etc/portage').


% -----------------------------------------------------------------------------
%  Portage repository - sync via git
% -----------------------------------------------------------------------------

:- portage:newinstance(repository).
:- portage:init('/usr/portage','/usr/portage/metadata/md5-cache',
                'https://github.com/gentoo-mirror/gentoo','git','eapi').
:- kb:register(portage).


% -----------------------------------------------------------------------------
%  Installed package database snapshot (vdb layout)
% -----------------------------------------------------------------------------

:- pkg:newinstance(repository).
:- pkg:init('/var/db/pkg','', '', 'local','vdb').
:- kb:register(pkg).


% -----------------------------------------------------------------------------
%  Local distfiles directory
% -----------------------------------------------------------------------------

:- distfiles:newinstance(repository).
:- distfiles:init('/var/cache/distfiles','', '', 'local','distfiles').
:- kb:register(distfiles).


% -----------------------------------------------------------------------------
%  Overlay repository - local sync
% -----------------------------------------------------------------------------

% :- overlay:newinstance(repository).
% :- overlay:init('/root/repository/overlay',
%                 '/root/repository/overlay/metadata/md5-cache',
%                 '/root/prolog/Repository/overlay/','rsync','eapi').
% :- kb:register(overlay).