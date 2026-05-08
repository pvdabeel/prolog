#!/bin/bash
# easy-pkgs.sh - run a small set of "easy" packages, each in its own session
# usage: easy-pkgs.sh [pkg1 pkg2 ...]   (defaults to a curated list)
set -uo pipefail

PKGS=("${@}")
if [[ ${#PKGS[@]} -eq 0 ]]; then
  PKGS=(
    app-misc/hello
    app-misc/figlet
    app-text/tree
    app-text/sl
    games-misc/cowsay
    dev-vcs/tig
  )
fi

LOGDIR="/srv/tinderbox-ng/logs"
SUMMARY="$LOGDIR/easy-pkgs-summary.log"
mkdir -p "$LOGDIR"

stamp() { date '+%Y-%m-%d %H:%M:%S'; }

# Per-package wall-clock cap (emerge inside chroot). Override via env var.
PER_PKG_TIMEOUT="${PER_PKG_TIMEOUT:-600}"

{
  echo "=========================================================="
  echo "easy-pkgs run started: $(stamp)"
  echo "packages: ${PKGS[*]}"
  echo "per-pkg timeout: ${PER_PKG_TIMEOUT}s"
  echo "=========================================================="
} | tee "$SUMMARY"

declare -a RESULTS

for pkg in "${PKGS[@]}"; do
  # Sanitize for session name: strip slash, dots etc.
  name="pkg-${pkg//\//_}"

  echo
  echo "--- [$(stamp)] $pkg  (session=$name) ---" | tee -a "$SUMMARY"

  # Clean any pre-existing session of the same name (idempotent reruns)
  tinderbox-ng destroy "$name" >/dev/null 2>&1 || true

  if ! tinderbox-ng new "$name" >/dev/null 2>&1; then
    echo "  STATUS: FAIL (could not create session)" | tee -a "$SUMMARY"
    RESULTS+=("FAIL session-create $pkg")
    continue
  fi

  # Drive the build with portage-ng (the project under test). --build is
  # the flag that prints the plan AND executes ebuild.sh per step;
  # without it, default --merge only runs the resolver/planner.
  # --ci runs non-interactively. The chroot's wrapper at /usr/local/bin
  # forwards into /opt/portage-ng.
  start_ts=$(date +%s)
  if timeout --preserve-status "$PER_PKG_TIMEOUT" \
       tinderbox-ng exec "$name" -- \
         "portage-ng-dev --mode standalone --ci --build $pkg" \
       >/dev/null 2>&1; then
    rc=0
  else
    rc=$?
  fi
  end_ts=$(date +%s)
  dur=$(( end_ts - start_ts ))

  # Find the most recent exec log for this session
  logfile=$(ls -1t /srv/tinderbox-ng/sessions/"$name"/logs/exec-*.log 2>/dev/null | head -1)

  case "$rc" in
    0)   status="OK"   ;;
    124) status="TIMEOUT" ;;
    *)   status="FAIL[rc=$rc]" ;;
  esac

  echo "  STATUS: $status  (${dur}s)" | tee -a "$SUMMARY"
  echo "  log: $logfile" | tee -a "$SUMMARY"
  if [[ "$status" != "OK" && -f "$logfile" ]]; then
    echo "  last 8 log lines:" | tee -a "$SUMMARY"
    tail -8 "$logfile" | sed 's/^/    /' | tee -a "$SUMMARY"
  fi
  RESULTS+=("$status $pkg ${dur}s")
done

{
  echo
  echo "=========================================================="
  echo "easy-pkgs run finished: $(stamp)"
  echo "----------------------------------------------------------"
  printf "%s\n" "${RESULTS[@]}" | column -t
  echo "=========================================================="
} | tee -a "$SUMMARY"
