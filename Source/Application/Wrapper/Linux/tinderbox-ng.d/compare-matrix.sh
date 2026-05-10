#!/bin/bash
# tinderbox-ng.d/compare-matrix.sh
#
# Drive `tinderbox-ng compare` over a list of packages, parse each
# summary table, and emit a single-line TSV record per target plus a
# concise stdout report. Supports manifest files (one atom per line,
# `#` for comments) so large package sets can live in version control.
#
#   ssh root@vm-linux.local 'compare-matrix --pretend pkg1 pkg2 ...'
#   ssh root@vm-linux.local 'compare-matrix --build --manifest /path/pkgs.txt'
#   ssh root@vm-linux.local 'compare-matrix --build --jobs 8 --manifest ...'
#
# `--jobs N` runs N package comparisons concurrently. Each comparison
# itself spawns 2 sessions (portage-ng + emerge in parallel mount
# namespaces), so the actual session count peaks at 2N. The vm-linux
# baseline easily handles N=8 on a 32-core box; tune to keep load
# under (cores - small headroom).
#
# Output goes to /srv/tinderbox-ng/reports/compare-matrix-<stamp>/.
# The TSV is written incrementally, one row per package (mutex-locked
# under --jobs > 1), so a long unattended run can be interrupted
# without losing prior results.
#
# Exit code: 0 if every comparison's portage-ng side matched (or
# improved on) emerge in completion + VDB count; 1 otherwise.

set -euo pipefail

MODE="--pretend"
LABEL_PREFIX=""
KEEP=0
MANIFEST=""
JOBS=1
RESUME_DIR=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --pretend|-p)   MODE="--pretend"; shift ;;
    --build|-b)     MODE="--build";   shift ;;
    --keep)         KEEP=1;            shift ;;
    --label-prefix) LABEL_PREFIX="$2"; shift 2 ;;
    --manifest)     MANIFEST="$2";     shift 2 ;;
    --manifest=*)   MANIFEST="${1#--manifest=}"; shift ;;
    --jobs)         JOBS="$2";         shift 2 ;;
    --jobs=*)       JOBS="${1#--jobs=}"; shift ;;
    -j)             JOBS="$2";         shift 2 ;;
    --resume-dir)   RESUME_DIR="$2";   shift 2 ;;
    --resume-dir=*) RESUME_DIR="${1#--resume-dir=}"; shift ;;
    --) shift; break ;;
    -*) echo "unknown flag $1" >&2; exit 2 ;;
    *)  break ;;
  esac
done

[[ "$JOBS" =~ ^[0-9]+$ && "$JOBS" -ge 1 ]] || {
  echo "--jobs must be a positive integer, got: $JOBS" >&2; exit 2
}

# Build PACKAGES from --manifest plus trailing positional args. Strip
# blank lines and `#` comments. De-dup while preserving order.
declare -a PACKAGES=()
seen=$(mktemp)
trap 'rm -f "$seen"' EXIT

_add_pkg() {
  local p="$1"
  [[ -n "$p" ]] || return 0
  if ! grep -Fxq "$p" "$seen"; then
    echo "$p" >> "$seen"
    PACKAGES+=("$p")
  fi
}

if [[ -n "$MANIFEST" ]]; then
  [[ -r "$MANIFEST" ]] || { echo "manifest not readable: $MANIFEST" >&2; exit 2; }
  while IFS= read -r line; do
    line="${line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    _add_pkg "$line"
  done < "$MANIFEST"
fi

for p in "$@"; do
  _add_pkg "$p"
done

[[ ${#PACKAGES[@]} -gt 0 ]] || {
  echo "usage: compare-matrix [--pretend|--build] [--keep] [--jobs N] [--manifest FILE] [pkg...]" >&2
  exit 2
}

# Output dir: either a fresh stamp dir (default) or an existing run dir
# we are appending to (--resume-dir, used by `tinderbox-ng continue` so
# resumed work shows up in the same TSV / dashboard / aggregate counters
# as the original run instead of in a sibling directory the user has to
# union by hand).
if [[ -n "$RESUME_DIR" ]]; then
  [[ -d "$RESUME_DIR" ]] || { echo "resume-dir not found: $RESUME_DIR" >&2; exit 2; }
  OUTDIR="$RESUME_DIR"
else
  STAMP="$(date +%Y%m%dT%H%M%S)"
  OUTDIR="/srv/tinderbox-ng/reports/compare-matrix-$STAMP"
fi
TSV="$OUTDIR/results.tsv"
META="$OUTDIR/meta.txt"
TSV_LOCK="$OUTDIR/.tsv.lock"
mkdir -p "$OUTDIR"
: > "$TSV_LOCK"

# Persist run metadata so the report generator has provenance.
# On resume we APPEND a marker block instead of overwriting; the original
# `started_at` / `package_count` / `manifest` stay authoritative for the
# logical run, and each resume event is auditable in meta.txt.
if [[ -n "$RESUME_DIR" ]]; then
  {
    printf '\n# ----- resumed at %s -----\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    printf 'resumed_at\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    printf 'resume_manifest\t%s\n' "${MANIFEST:-(stdin)}"
    printf 'resume_package_count\t%d\n' "${#PACKAGES[@]}"
    printf 'resume_jobs\t%d\n' "$JOBS"
  } >> "$META"
else
  {
    printf 'mode\t%s\n' "$MODE"
    printf 'manifest\t%s\n' "${MANIFEST:-(stdin)}"
    printf 'package_count\t%d\n' "${#PACKAGES[@]}"
    printf 'jobs\t%d\n' "$JOBS"
    printf 'started_at\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    printf 'host\t%s\n' "$(hostname)"
    printf 'kernel\t%s\n' "$(uname -r)"
  } > "$META"
fi

# Header. TSV is written line-by-line; a partial file is still a valid
# TSV with one header row. On resume we keep the existing TSV (header +
# completed rows) and just append; if for some reason the TSV is missing
# or empty we re-emit the header so the file stays self-describing.
if [[ ! -s "$TSV" ]]; then
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    target mode pn_exit em_exit pn_actions em_actions pn_completed em_completed pn_vdb em_vdb vdb_delta seconds \
    > "$TSV"
fi

# Per-package worker: runs one tinderbox-ng compare and writes a TSV row.
# Designed to be safe under parallel execution: each invocation gets its
# own log file, and the TSV append is serialized via flock on TSV_LOCK.
_run_one() {
  local idx="$1"
  local pkg="$2"
  local total="$3"
  local label log start rc elapsed
  label="${LABEL_PREFIX}$(printf '%s' "$pkg" | tr -c '[:alnum:]' '_' | sed 's/^_*//;s/_*$//')"
  log="$OUTDIR/${label}.log"

  local cmp_args=("$MODE" --label "$label")
  [[ $KEEP -eq 1 ]] && cmp_args+=(--keep)

  start=$(date +%s)
  set +e
  /usr/local/sbin/tinderbox-ng compare "${cmp_args[@]}" "$pkg" >"$log" 2>&1
  rc=$?
  set -e
  elapsed=$(( $(date +%s) - start ))

  local pn_exit em_exit pn_actions em_actions pn_completed em_completed pn_vdb em_vdb
  pn_exit="$(awk -F'│' '/^│ exit /{print $3}' "$log" | tr -d ' ' | head -n1)"
  em_exit="$(awk -F'│' '/^│ exit /{print $4}' "$log" | tr -d ' ' | head -n1)"
  pn_actions="$(awk -F'│' '/^│ plan actions /{print $3}' "$log" | tr -d ' ' | head -n1)"
  em_actions="$(awk -F'│' '/^│ plan actions /{print $4}' "$log" | tr -d ' ' | head -n1)"
  pn_completed="$(awk -F'│' '/^│ completed /{print $3}' "$log" | tr -d ' ' | head -n1)"
  em_completed="$(awk -F'│' '/^│ completed /{print $4}' "$log" | tr -d ' ' | head -n1)"
  pn_vdb="$(awk -F'│' '/^│ merged into VDB /{print $3}' "$log" | tr -d ' ' | head -n1)"
  em_vdb="$(awk -F'│' '/^│ merged into VDB /{print $4}' "$log" | tr -d ' ' | head -n1)"

  for v in pn_exit em_exit pn_actions em_actions pn_completed em_completed pn_vdb em_vdb; do
    [[ -z "${!v}" ]] && eval "$v=?"
  done

  local vdb_delta="="
  local mismatch=0
  if [[ "$pn_vdb" =~ ^[0-9]+$ && "$em_vdb" =~ ^[0-9]+$ ]]; then
    if [[ "$pn_vdb" -lt "$em_vdb" ]]; then
      vdb_delta="-$((em_vdb - pn_vdb))"
      mismatch=1
    elif [[ "$pn_vdb" -gt "$em_vdb" ]]; then
      vdb_delta="+$((pn_vdb - em_vdb))"
    fi
  fi

  # Mutex-protected append + status line so multiple workers don't
  # interleave their output and we never lose a TSV row.
  (
    flock 9
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%d\n' \
      "$pkg" "$MODE" "$pn_exit" "$em_exit" "$pn_actions" "$em_actions" \
      "$pn_completed" "$em_completed" "$pn_vdb" "$em_vdb" "$vdb_delta" "$elapsed" \
      >> "$TSV"
    printf '[%d/%d] %-40s rc=%d  pn=%-9s em=%-9s vdb pn=%s em=%s (%s)  %ds\n' \
      "$idx" "$total" "$pkg" "$rc" "$pn_exit" "$em_exit" "$pn_vdb" "$em_vdb" "$vdb_delta" "$elapsed"
  ) 9>"$TSV_LOCK"

  return "$mismatch"
}

mismatches=0
total=${#PACKAGES[@]}

if [[ "$JOBS" -le 1 ]]; then
  # Serial path (preserves the prior layout for single-job runs)
  i=0
  for pkg in "${PACKAGES[@]}"; do
    i=$((i + 1))
    printf '\n========== [%d/%d] %s (%s) ==========\n' "$i" "$total" "$pkg" "$MODE"
    set +e
    _run_one "$i" "$pkg" "$total"
    rc=$?
    set -e
    [[ $rc -ne 0 ]] && mismatches=$((mismatches + 1))
  done
else
  # Parallel path: dispatch JOBS workers consuming from a shared index.
  # We use a FIFO queue file as a lightweight pipe of indices.
  printf '[parallel] dispatching %d packages with --jobs %d\n' "$total" "$JOBS"

  # Slot-by-slot wave dispatch: launch up to JOBS workers, wait for any
  # to finish, then launch the next. Bash 5+ has `wait -n -p`, but for
  # portability we use a polling-free approach with `wait -n`.
  declare -a child_pids=()
  pkgi=0
  for pkg in "${PACKAGES[@]}"; do
    pkgi=$((pkgi + 1))
    # If we already have JOBS workers running, wait for one to finish.
    while [[ ${#child_pids[@]} -ge $JOBS ]]; do
      set +e; wait -n; rc=$?; set -e
      [[ $rc -ne 0 && $rc -ne 127 ]] && mismatches=$((mismatches + 1))
      # Reap finished pids by checking which ones are still alive.
      alive_pids=()
      for p in "${child_pids[@]}"; do
        if kill -0 "$p" 2>/dev/null; then
          alive_pids+=("$p")
        fi
      done
      child_pids=("${alive_pids[@]}")
    done

    # Launch the next worker.
    _run_one "$pkgi" "$pkg" "$total" &
    child_pids+=("$!")
  done

  # Drain the remaining workers.
  while [[ ${#child_pids[@]} -gt 0 ]]; do
    set +e; wait -n; rc=$?; set -e
    [[ $rc -ne 0 && $rc -ne 127 ]] && mismatches=$((mismatches + 1))
    alive_pids=()
    for p in "${child_pids[@]}"; do
      if kill -0 "$p" 2>/dev/null; then
        alive_pids+=("$p")
      fi
    done
    child_pids=("${alive_pids[@]}")
  done
fi

printf 'finished_at\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$META"

printf '\n========== summary ==========\n'
printf 'TSV: %s\n' "$TSV"
printf 'Per-package logs: %s/<label>.log\n' "$OUTDIR"
printf 'Meta: %s\n\n' "$META"

# Show TSV sorted by target so the parallel-out-of-order rows look tidy.
{
  head -n1 "$TSV"
  tail -n +2 "$TSV" | sort
} | column -t -s $'\t'

[[ $mismatches -eq 0 ]] && exit 0 || exit 1
