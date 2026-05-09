#!/bin/bash
# tinderbox-ng.d/compare-matrix.sh
#
# Drive `tinderbox-ng compare` over a list of packages, parse each
# summary table, and emit a single-line TSV record per target plus a
# concise stdout report. Supports manifest files (one atom per line,
# `#` for comments) so large package sets can live in version control.
#
#   ssh root@vm-linux.local 'compare-matrix --mode pretend pkg1 pkg2 ...'
#   ssh root@vm-linux.local 'compare-matrix --build --manifest /path/to/pkgs.txt'
#
# Output goes to /srv/tinderbox-ng/reports/compare-matrix-<stamp>/.
# The TSV is written incrementally, one row per package, so a long
# unattended run can be interrupted without losing prior results.
#
# Exit code: 0 if every comparison's portage-ng side matched (or
# improved on) emerge in completion + VDB count; 1 otherwise.

set -euo pipefail

MODE="--pretend"
LABEL_PREFIX=""
KEEP=0
MANIFEST=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --pretend|-p)   MODE="--pretend"; shift ;;
    --build|-b)     MODE="--build";   shift ;;
    --keep)         KEEP=1;            shift ;;
    --label-prefix) LABEL_PREFIX="$2"; shift 2 ;;
    --manifest)     MANIFEST="$2";     shift 2 ;;
    --manifest=*)   MANIFEST="${1#--manifest=}"; shift ;;
    --) shift; break ;;
    -*) echo "unknown flag $1" >&2; exit 2 ;;
    *)  break ;;
  esac
done

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

[[ ${#PACKAGES[@]} -gt 0 ]] || { echo "usage: compare-matrix [--pretend|--build] [--keep] [--manifest FILE] [pkg...]" >&2; exit 2; }

STAMP="$(date +%Y%m%dT%H%M%S)"
OUTDIR="/srv/tinderbox-ng/reports/compare-matrix-$STAMP"
TSV="$OUTDIR/results.tsv"
META="$OUTDIR/meta.txt"
mkdir -p "$OUTDIR"

# Persist run metadata so the report generator has provenance.
{
  printf 'mode\t%s\n' "$MODE"
  printf 'manifest\t%s\n' "${MANIFEST:-(stdin)}"
  printf 'package_count\t%d\n' "${#PACKAGES[@]}"
  printf 'started_at\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  printf 'host\t%s\n' "$(hostname)"
  printf 'kernel\t%s\n' "$(uname -r)"
} > "$META"

# Header. TSV is written line-by-line; a partial file is still a valid
# TSV with one header row.
printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
  target mode pn_exit em_exit pn_actions em_actions pn_completed em_completed pn_vdb em_vdb vdb_delta seconds \
  > "$TSV"

mismatches=0
total=${#PACKAGES[@]}
i=0
for pkg in "${PACKAGES[@]}"; do
  i=$((i + 1))
  label="${LABEL_PREFIX}$(printf '%s' "$pkg" | tr -c '[:alnum:]' '_' | sed 's/^_*//;s/_*$//')"
  log="$OUTDIR/${label}.log"

  printf '\n========== [%d/%d] %s (%s) ==========\n' "$i" "$total" "$pkg" "$MODE"

  cmp_args=("$MODE" --label "$label")
  [[ $KEEP -eq 1 ]] && cmp_args+=(--keep)

  start=$(date +%s)
  set +e
  /usr/local/sbin/tinderbox-ng compare "${cmp_args[@]}" "$pkg" >"$log" 2>&1
  rc=$?
  set -e
  elapsed=$(( $(date +%s) - start ))

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

  vdb_delta="="
  if [[ "$pn_vdb" =~ ^[0-9]+$ && "$em_vdb" =~ ^[0-9]+$ ]]; then
    if [[ "$pn_vdb" -lt "$em_vdb" ]]; then
      vdb_delta="-$((em_vdb - pn_vdb))"
      mismatches=$((mismatches + 1))
    elif [[ "$pn_vdb" -gt "$em_vdb" ]]; then
      vdb_delta="+$((pn_vdb - em_vdb))"
    fi
  fi

  # Append immediately so an interrupted run still leaves a usable TSV.
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%d\n' \
    "$pkg" "$MODE" "$pn_exit" "$em_exit" "$pn_actions" "$em_actions" \
    "$pn_completed" "$em_completed" "$pn_vdb" "$em_vdb" "$vdb_delta" "$elapsed" \
    >> "$TSV"

  printf '  -> rc=%d  exit pn=%s em=%s  vdb pn=%s em=%s (delta %s)  %ds\n' \
    "$rc" "$pn_exit" "$em_exit" "$pn_vdb" "$em_vdb" "$vdb_delta" "$elapsed"
done

printf 'finished_at\t%s\n' "$(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$META"

printf '\n========== summary ==========\n'
printf 'TSV: %s\n' "$TSV"
printf 'Per-package logs: %s/<label>.log\n' "$OUTDIR"
printf 'Meta: %s\n\n' "$META"

column -t -s $'\t' "$TSV"

[[ $mismatches -eq 0 ]] && exit 0 || exit 1
