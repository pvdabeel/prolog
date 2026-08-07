#!/bin/sh
# Launcher for the packaged sys-apps/portage-ng install.
# Mirrors Source/Application/Wrapper/portage-ng-dev, but points -p portage=
# at the installed application tree under /usr/lib/portage-ng.
set -eu

LIBDIR="@LIBDIR@"

exec swipl -O \
	--stack-limit=32G \
	-f "${LIBDIR}/portage-ng.pl" \
	-p "portage=${LIBDIR}" \
	-Dverbose_autoload=false \
	-g main -- "$@"
