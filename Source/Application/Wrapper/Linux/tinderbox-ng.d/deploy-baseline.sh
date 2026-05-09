#!/bin/bash
#
# deploy-baseline.sh - safely deploy a tinderbox-ng template to a remote host.
#
# tinderbox-ng's cp_template() substitutes @NPROC@, @GENTOO_PROFILE@ and
# @PORTAGE_NG_ROOT@ at bootstrap time. Raw scp of a template to the remote
# host bypasses this substitution, leaving the file with literal "@NPROC@"
# placeholders that crash emerge with `Invalid --jobs parameter: '@NPROC@'`
# (and similarly broken behaviour for the other tokens). Hit during the
# 1000-package matrix on 2026-05-09 -- contaminated 218 of 233 post-KW
# entries' emerge data before the in-place sed recovery.
#
# Use this helper to perform the same substitution before scp. Substitution
# values are read from the env (with the same defaults cp_template uses) and
# NPROC is resolved against the *remote* host so it matches the deploy
# target, not whatever machine you happen to run this from.
#
# Usage:
#   deploy-baseline.sh <template-name> <user@host:remote-path>
#
# Env overrides:
#   NPROC            - integer (default: ssh remote nproc, then local nproc, then 4)
#   GENTOO_PROFILE   - default: default/linux/amd64/23.0/split-usr/no-multilib
#   PORTAGE_NG_ROOT  - default: /opt/portage-ng
#
# Examples:
#   deploy-baseline.sh baseline.make.conf \
#       root@vm-linux.local:/srv/tinderbox-ng/baseline/etc/portage/make.conf
#   deploy-baseline.sh baseline.repos.conf \
#       root@vm-linux.local:/srv/tinderbox-ng/baseline/etc/portage/repos.conf/gentoo.conf
#

set -euo pipefail

LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  cat >&2 <<EOF
usage: deploy-baseline.sh <template-name> <user@host:remote-path>

Available templates in $LIB_DIR:
EOF
  ls -1 "$LIB_DIR" | sed 's/^/  /' >&2
  cat >&2 <<EOF

Templates known to need substitution:
  baseline.make.conf   (@NPROC@)
  portage-ng-dev.in    (@PORTAGE_NG_ROOT@)

Plain templates (substitution is a no-op but safe to send via this script):
  baseline.repos.conf
EOF
  exit 1
}

[[ $# -eq 2 ]] || usage

template="$1"
remote="$2"

src="$LIB_DIR/$template"
[[ -f "$src" ]] || { echo "deploy-baseline.sh: missing template $src" >&2; exit 1; }
[[ "$remote" == *:* ]] || { echo "deploy-baseline.sh: remote must be user@host:path (got '$remote')" >&2; exit 1; }

remote_host="${remote%%:*}"
remote_path="${remote#*:}"
remote_dir="$(dirname "$remote_path")"

NPROC="${NPROC:-$(ssh -o BatchMode=yes -o ConnectTimeout=5 "$remote_host" 'nproc 2>/dev/null' 2>/dev/null || nproc 2>/dev/null || echo 4)}"
GENTOO_PROFILE="${GENTOO_PROFILE:-default/linux/amd64/23.0/split-usr/no-multilib}"
PORTAGE_NG_ROOT="${PORTAGE_NG_ROOT:-/opt/portage-ng}"

case "$NPROC" in
  ''|*[!0-9]*) echo "deploy-baseline.sh: refusing to substitute non-numeric NPROC='$NPROC'" >&2; exit 1 ;;
esac

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

sed -e "s|@NPROC@|$NPROC|g" \
    -e "s|@GENTOO_PROFILE@|$GENTOO_PROFILE|g" \
    -e "s|@PORTAGE_NG_ROOT@|$PORTAGE_NG_ROOT|g" \
    "$src" > "$tmp"

if grep -q '@[A-Z_]\{1,\}@' "$tmp"; then
  echo "deploy-baseline.sh: refusing to deploy -- template still has unsubstituted @TOKEN@:" >&2
  grep -n '@[A-Z_]\{1,\}@' "$tmp" | sed 's/^/  /' >&2
  echo "  add the missing token handling to deploy-baseline.sh and cp_template." >&2
  exit 1
fi

echo "deploy-baseline.sh: $template -> $remote"
echo "  NPROC=$NPROC  GENTOO_PROFILE=$GENTOO_PROFILE  PORTAGE_NG_ROOT=$PORTAGE_NG_ROOT"

ssh "$remote_host" "install -d -m 0755 $(printf %q "$remote_dir")"
scp -q "$tmp" "$remote"
ssh "$remote_host" "chmod 0644 $(printf %q "$remote_path")"

echo "deploy-baseline.sh: ok"
