#!/bin/bash
#
# deploy-host.sh - one-shot host install for tinderbox-ng
#
# Run from a dev checkout (the directory containing tinderbox-ng + tinderbox-ng.d).
# Pushes the script + templates to a remote VM, symlinks the entry point onto
# $PATH, runs `tinderbox-ng doctor` to confirm prerequisites, and (optionally)
# kicks off `bootstrap` followed by `selftest`.
#
# This is steps 1-3 (host install) of the lifecycle described in README.md
# wrapped in a single command. The bootstrap step itself can take hours and
# is gated behind --bootstrap so this script can also be used as a fast
# "rsync new tinderbox-ng + retest" for development iteration.
#
# Usage:
#   ./deploy-host.sh user@vm-host                    # install + doctor only
#   ./deploy-host.sh --bootstrap user@vm-host        # install + doctor + bootstrap + selftest
#   ./deploy-host.sh --refresh-portage-ng user@vm-host
#                                                    # install + push new portage-ng src
#                                                    # into existing baseline
#
# Idempotent. Safe to re-run after every git pull on the dev machine.

set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: deploy-host.sh [options] user@vm-host

Options:
  --bootstrap            After install + doctor, run `tinderbox-ng bootstrap`
                         on the remote (long-running; ~hours). Then run
                         `tinderbox-ng selftest` to confirm the result.
  --refresh-portage-ng   After install, run `tinderbox-ng refresh-portage-ng`
                         on the remote with PORTAGE_NG_LOCAL set to the
                         baseline-internal /opt/portage-ng (re-uses the in-baseline
                         shipped source). Use this for fast resolver-only updates
                         that do NOT need a kb.qlf rebuild.
  --selftest             Run `tinderbox-ng selftest` on the remote after
                         install (independent of --bootstrap).
  --remote-prefix DIR    Where to install the script + templates on the remote
                         (default: /usr/local/share/tinderbox-ng).
  --link DIR             Where to symlink the entry point on the remote
                         (default: /usr/local/sbin/tinderbox-ng).
  -h, --help             This message.

Environment forwarded to remote bootstrap:
  TINDERBOX_CCACHE_MAX_SIZE        ccache size cap        (default 100G)
  TINDERBOX_SESSIONS_TMPFS_SIZE    sessions tmpfs cap     (default 100G)
  STAGE3_VARIANT, STAGE3_ARCH      stage3 selection       (defaults: openrc/splitusr/amd64)
  PORTAGE_TREE_PIN                 pin tree to a commit   (default: latest)
  GENTOO_PROFILE                   profile to set         (matches Source/config.pl)
  TINDERBOX_REBOOTSTRAP=1          let bootstrap overwrite an existing baseline

Examples:
  # First-time install on a fresh VM:
  TINDERBOX_BOOTSTRAP_SELFTEST=1 ./deploy-host.sh --bootstrap root@vm-linux.local

  # After `git pull` on the dev machine: refresh script + templates + portage-ng src:
  ./deploy-host.sh --refresh-portage-ng root@vm-linux.local
USAGE
}

# Defaults
DO_BOOTSTRAP=0
DO_REFRESH_PNG=0
DO_SELFTEST=0
REMOTE_PREFIX=/usr/local/share/tinderbox-ng
REMOTE_LINK=/usr/local/sbin/tinderbox-ng
REMOTE=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --bootstrap)          DO_BOOTSTRAP=1; shift ;;
    --refresh-portage-ng) DO_REFRESH_PNG=1; shift ;;
    --selftest)           DO_SELFTEST=1; shift ;;
    --remote-prefix)      REMOTE_PREFIX="$2"; shift 2 ;;
    --link)               REMOTE_LINK="$2"; shift 2 ;;
    -h|--help)            usage; exit 0 ;;
    --)                   shift; break ;;
    -*)                   echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
    *)
      if [[ -z "$REMOTE" ]]; then
        REMOTE="$1"; shift
      else
        echo "unexpected positional arg: $1" >&2; usage >&2; exit 2
      fi
      ;;
  esac
done

[[ -n "$REMOTE" ]] || { usage >&2; echo >&2; echo "error: missing user@vm-host" >&2; exit 2; }

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
[[ -f "$SCRIPT_DIR/tinderbox-ng" ]] || { echo "error: $SCRIPT_DIR/tinderbox-ng not found" >&2; exit 2; }
[[ -d "$SCRIPT_DIR/tinderbox-ng.d" ]] || { echo "error: $SCRIPT_DIR/tinderbox-ng.d not found" >&2; exit 2; }

# Forward bootstrap-relevant env. ssh's `env` strips most variables; pass the
# small whitelist explicitly so the remote bootstrap honours them.
collect_env() {
  local v
  local out=""
  for v in TINDERBOX_CCACHE_MAX_SIZE TINDERBOX_SESSIONS_TMPFS_SIZE \
           TINDERBOX_BOOTSTRAP_SELFTEST TINDERBOX_REBOOTSTRAP \
           TINDERBOX_SELFTEST_TARGET TINDERBOX_MIN_FREE_MIB \
           TINDERBOX_MDNS_HOSTS TINDERBOX_SKIP_DOCTOR \
           STAGE3_VARIANT STAGE3_ARCH STAGE3_BASE_URL \
           GENTOO_PROFILE GENTOO_LOCALE GENTOO_LOCALE_NAME \
           PORTAGE_TREE_URL PORTAGE_TREE_PIN \
           PORTAGE_NG_URL PORTAGE_NG_REF PORTAGE_NG_LOCAL; do
    if [[ -n "${!v:-}" ]]; then
      printf -v esc '%q' "${!v}"
      out+="$v=$esc "
    fi
  done
  printf '%s' "$out"
}

ENV_PREFIX="$(collect_env)"

remote() {
  ssh -o BatchMode=yes "$REMOTE" "$@"
}

remote_root() {
  # Ensure we run as root if the remote login isn't already root. The VM
  # configuration tested here logs in as root directly; sudo is only used
  # when needed.
  if [[ "${REMOTE%@*}" == "root" ]] || [[ "$REMOTE" != *@* ]]; then
    remote "$@"
  else
    remote "sudo -E bash -c $(printf '%q' "$*")"
  fi
}

log() { printf '[deploy-host] %s\n' "$*" >&2; }

log "remote: $REMOTE"
log "prefix: $REMOTE_PREFIX  link: $REMOTE_LINK"

# 1. rsync the script + templates onto the remote.
log "step 1/4: rsync $SCRIPT_DIR/ -> $REMOTE:$REMOTE_PREFIX/"
remote_root "install -d $(printf '%q' "$REMOTE_PREFIX")"

rsync -a --delete \
  --exclude '.DS_Store' \
  --exclude '__pycache__' \
  --exclude 'deploy-host.sh' \
  --rsync-path="sudo -n rsync 2>/dev/null || rsync" \
  "$SCRIPT_DIR/" "$REMOTE:$REMOTE_PREFIX/" || \
rsync -a --delete \
  --exclude '.DS_Store' \
  --exclude '__pycache__' \
  --exclude 'deploy-host.sh' \
  "$SCRIPT_DIR/" "$REMOTE:$REMOTE_PREFIX/"

# 2. Symlink the entry point.
log "step 2/4: symlink $REMOTE_LINK -> $REMOTE_PREFIX/tinderbox-ng"
remote_root "install -d $(dirname $(printf '%q' "$REMOTE_LINK")) && \
             ln -sfn $(printf '%q' "$REMOTE_PREFIX/tinderbox-ng") $(printf '%q' "$REMOTE_LINK") && \
             chmod +x $(printf '%q' "$REMOTE_PREFIX/tinderbox-ng")"

# 3. Run doctor to confirm host prerequisites.
log "step 3/4: $REMOTE_LINK doctor"
if ! remote_root "$ENV_PREFIX $(printf '%q' "$REMOTE_LINK") doctor"; then
  echo "[deploy-host] doctor reported errors; fix them and re-run." >&2
  exit 1
fi

# 4. Optional follow-ups.
if (( DO_REFRESH_PNG )); then
  log "step 4/4: $REMOTE_LINK refresh-portage-ng"
  log "  (this rsyncs your latest portage-ng source into the existing baseline)"
  remote_root "$ENV_PREFIX PORTAGE_NG_LOCAL=$REMOTE_PREFIX/../portage-ng \
               $(printf '%q' "$REMOTE_LINK") refresh-portage-ng" || \
    echo "[deploy-host] refresh-portage-ng failed (baseline may not exist yet)" >&2
fi

if (( DO_BOOTSTRAP )); then
  log "step 4/4: $REMOTE_LINK bootstrap (long-running; ~hours)"
  remote_root "$ENV_PREFIX $(printf '%q' "$REMOTE_LINK") bootstrap"
fi

if (( DO_SELFTEST )) || (( DO_BOOTSTRAP )) && [[ -n "${TINDERBOX_BOOTSTRAP_SELFTEST:-}" ]]; then
  : # selftest already ran inside bootstrap if TINDERBOX_BOOTSTRAP_SELFTEST=1
elif (( DO_SELFTEST )); then
  log "step 4/4: $REMOTE_LINK selftest"
  remote_root "$ENV_PREFIX $(printf '%q' "$REMOTE_LINK") selftest"
fi

log "done."
