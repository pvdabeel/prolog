/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2026, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/

% =============================================================================
%  Packaged-install default configuration (sys-apps/portage-ng)
% =============================================================================
%
% Used when no Source/Config/<hostname>.pl is present. Points at the usual
% Gentoo system locations so a fresh emerge of portage-ng works out of the
% box. Override per-host by adding Source/Config/<hostname>.pl next to this
% file under /usr/lib/portage-ng.

% -----------------------------------------------------------------------------
%  Host-specific paths
% -----------------------------------------------------------------------------

config:portage_confdir('/etc/portage').
config:pkg_directory('/var/db/pkg').


% -----------------------------------------------------------------------------
%  Portage repository
% -----------------------------------------------------------------------------

:- portage:newinstance(repository).
:- portage:init('/var/db/repos/gentoo',
                '/var/db/repos/gentoo/metadata/md5-cache',
                'https://github.com/gentoo-mirror/gentoo', 'git', 'eapi').
:- kb:register(portage).


% -----------------------------------------------------------------------------
%  Installed package database (VDB)
% -----------------------------------------------------------------------------

:- pkg:newinstance(repository).
:- pkg:init('/var/db/pkg', '', '', 'local', 'vdb').
:- kb:register(pkg).


% -----------------------------------------------------------------------------
%  Local distfiles directory
% -----------------------------------------------------------------------------

:- distfiles:newinstance(repository).
:- distfiles:init('/var/cache/distfiles', '', '', 'local', 'distfiles').
:- kb:register(distfiles).
