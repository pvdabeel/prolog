#!/bin/bash
#
# tinderbox-matrix -- layered test runner, designed to be invoked inside a
# tinderbox-ng session via:
#
#   tinderbox-ng exec <name> -- tinderbox-matrix <tier> <manifest>
#
# Manifest format (one atom per line, # for comments):
#   sys-apps/portage
#   dev-lang/swi-prolog
#   # category/name-version is fine too:
#   =app-editors/neovim-0.10.2
#
# Tiers:
#   metadata  - pkgcheck scan + pkgdev manifest (fastest; finds repo regressions)
#   resolver  - emerge -vp + portage-ng-dev --pretend per atom; no merges
#   merge     - emerge --jobs=N <atoms>
#   test      - FEATURES=test emerge --oneshot <atoms>; per-atom timeout
#   emptytree - emerge --emptytree --pretend @world (full plan diff)
#
# Each invocation writes to /var/log/tinderbox-matrix/<tier>/ and an
# aggregated TSV summary at /var/log/tinderbox-matrix/<tier>/summary.tsv.

set -euo pipefail

LOG_ROOT="${LOG_ROOT:-/var/log/tinderbox-matrix}"
TIMEOUT_DEFAULT="${TIMEOUT_DEFAULT:-1800}"

usage() {
  cat <<'EOF'
tinderbox-matrix <tier> [manifest]

Tiers:
  metadata   pkgcheck/pkgdev sweep over manifest atoms
  resolver   emerge -vp + portage-ng-dev --pretend per atom
  merge      emerge atoms (real merge)
  test       FEATURES=test emerge --oneshot per atom (with timeout)
  emptytree  emerge --emptytree --pretend @world (manifest ignored)

Environment:
  LOG_ROOT          default /var/log/tinderbox-matrix
  TIMEOUT_DEFAULT   default 1800 (seconds, used for per-atom test runs)
EOF
}

[[ $# -ge 1 ]] || { usage; exit 2; }
tier="$1"; shift
manifest="${1:-}"

case "$tier" in
  metadata|resolver|merge|test) [[ -n "$manifest" ]] || { usage; exit 2; } ;;
  emptytree) ;;
  -h|--help|help) usage; exit 0 ;;
  *) usage; echo "unknown tier: $tier" >&2; exit 2 ;;
esac

logdir="$LOG_ROOT/$tier"
mkdir -p "$logdir"
summary="$logdir/summary.tsv"
[[ -f "$summary" ]] || printf 'atom\ttier\texit\tduration_s\tlog\n' >"$summary"

read_atoms() {
  grep -Ev '^\s*(#|$)' "$manifest"
}

run_atom() {
  local atom="$1" cmd="$2" log
  log="$logdir/$(echo "$atom" | tr '/=<>~ ' '____').log"
  local start_ts end_ts dur rc
  start_ts=$(date +%s)
  set +e
  ( eval "$cmd" ) >"$log" 2>&1
  rc=$?
  set -e
  end_ts=$(date +%s)
  dur=$((end_ts - start_ts))
  printf '%s\t%s\t%d\t%d\t%s\n' "$atom" "$tier" "$rc" "$dur" "$log" >>"$summary"
  if [[ $rc -eq 0 ]]; then
    printf '  [ok]   %-40s %ds\n' "$atom" "$dur"
  else
    printf '  [FAIL] %-40s %ds (rc=%d, log=%s)\n' "$atom" "$dur" "$rc" "$log"
  fi
}

case "$tier" in
  metadata)
    if ! command -v pkgcheck >/dev/null 2>&1; then
      echo "pkgcheck not installed; emerge dev-util/pkgcheck inside the session first" >&2
      exit 3
    fi
    while read -r atom; do
      run_atom "$atom" "pkgcheck scan -p '$atom'"
    done < <(read_atoms)
    ;;

  resolver)
    while read -r atom; do
      run_atom "$atom" "
        echo '== emerge -vp =='
        emerge -vp '$atom'
        rc1=\$?
        echo '== portage-ng-dev --pretend =='
        portage-ng-dev --mode standalone --ci --pretend '$atom'
        rc2=\$?
        # Bubble up the worse of the two for triage
        [[ \$rc1 -ge \$rc2 ]] && exit \$rc1 || exit \$rc2
      "
    done < <(read_atoms)
    ;;

  merge)
    atoms=()
    while read -r a; do atoms+=("$a"); done < <(read_atoms)
    [[ ${#atoms[@]} -gt 0 ]] || { echo "manifest empty"; exit 2; }
    log="$logdir/batch-$(date +%Y%m%dT%H%M%S).log"
    start_ts=$(date +%s)
    set +e
    emerge --jobs="$(nproc)" --keep-going=y --quiet-build=y "${atoms[@]}" \
      >"$log" 2>&1
    rc=$?
    set -e
    end_ts=$(date +%s)
    dur=$((end_ts - start_ts))
    for atom in "${atoms[@]}"; do
      printf '%s\t%s\t%d\t%d\t%s\n' "$atom" "$tier" "$rc" "$dur" "$log" >>"$summary"
    done
    [[ $rc -eq 0 ]] && echo "[ok] batch merge ($dur s)" \
                    || echo "[FAIL] batch merge (rc=$rc, $dur s, log=$log)"
    exit "$rc"
    ;;

  test)
    while read -r atom; do
      run_atom "$atom" "
        FEATURES='\${FEATURES:-} test' \
          timeout '$TIMEOUT_DEFAULT' \
          emerge --oneshot --quiet-build=y '$atom'
      "
    done < <(read_atoms)
    ;;

  emptytree)
    log="$logdir/emptytree-$(date +%Y%m%dT%H%M%S).log"
    start_ts=$(date +%s)
    set +e
    emerge --emptytree --pretend @world >"$log" 2>&1
    rc=$?
    set -e
    end_ts=$(date +%s)
    dur=$((end_ts - start_ts))
    printf '@world\t%s\t%d\t%d\t%s\n' "$tier" "$rc" "$dur" "$log" >>"$summary"
    [[ $rc -eq 0 ]] && echo "[ok] emptytree pretend ($dur s)" \
                    || echo "[FAIL] emptytree pretend (rc=$rc, $dur s, log=$log)"
    exit "$rc"
    ;;
esac
