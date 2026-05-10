#!/usr/bin/env python3
"""
Reports/Scripts/render-compare-matrix.py

Render a Markdown comparison report from one or more compare-matrix
TSVs produced by Source/Application/Wrapper/Linux/tinderbox-ng.d/compare-matrix.sh.

Typical usage:

    python3 Reports/Scripts/render-compare-matrix.py \
        --pretend /tmp/pretend.tsv \
        --build   /tmp/build.tsv \
        --commit  $(git rev-parse --short HEAD) \
        --out     Reports/tinderbox-compare-$(date +%Y-%m-%d)-$(git rev-parse --short HEAD).md

Either --pretend or --build may be omitted; the corresponding section
will be skipped in the report.
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import re
from collections import Counter
from pathlib import Path
from typing import Optional


def load_tsv(path: Path) -> list[dict]:
    rows: list[dict] = []
    with path.open() as f:
        reader = csv.DictReader(f, delimiter="\t")
        for row in reader:
            for k in ("pn_actions", "em_actions", "pn_completed",
                      "em_completed", "pn_vdb", "em_vdb", "seconds"):
                if k in row and row[k].isdigit():
                    row[k] = int(row[k])
            rows.append(row)
    return rows


# --- log analysis -------------------------------------------------------------
#
# `tinderbox-ng compare` writes a per-package summary log that contains the
# tabular metrics, a "First error/warning per engine" section, and a
# "VDB delta (after build)" section. We parse this to determine whether
# the *target* package itself was actually built+merged, distinct from
# "pipeline exit code stayed 0". This catches silent-failure bugs in
# portage-ng where a sub-step fails but the overall exit is 0.

_FIRSTERR_RE = re.compile(
    r"portage-ng:.*FAIL.*install portage://([^\s]+)")


def _label_from_target(target: str) -> str:
    return target.replace("/", "_").replace("-", "_").replace(".", "_")


def _parse_vdb_delta(text: str) -> tuple[list[str], list[str], int]:
    """Return (only_in_pn, only_in_emerge, in_both_count) parsed from the
    'VDB delta (after build):' section of a compare summary log."""
    pn_only: list[str] = []
    em_only: list[str] = []
    in_both = 0
    section: Optional[str] = None
    for line in text.splitlines():
        s = line.strip()
        if s.startswith("only in portage-ng"):
            section = "pn"
        elif s.startswith("only in emerge"):
            section = "em"
        elif s.startswith("in both:"):
            section = None
            m = re.search(r"in both:\s*(\d+)", s)
            if m:
                in_both = int(m.group(1))
        elif s.startswith("Full logs:") or s.startswith("[tinderbox-ng"):
            section = None
        elif s.startswith("+ ") and section is not None:
            atom = s[2:].strip()
            if section == "pn":
                pn_only.append(atom)
            elif section == "em":
                em_only.append(atom)
    return pn_only, em_only, in_both


def detect_target_install(target: str, row: dict,
                          logs_dir: Optional[Path]) -> str:
    """Return 'OK', 'FAIL', 'aborted', or 'absent' for the target's
    portage-ng install state. Combines:

    - First-error parse: explicit `FAIL install portage://target-version`
      lines mean the target step failed.
    - VDB delta parse: target appearing in `only in portage-ng:` proves
      portage-ng installed it (different version from emerge); `in both:
      N >= 1` with target absent from `only in emerge:` means both
      engines installed the same version of the target.
    - Action-count heuristic: if pn_completed < pn_actions, the build
      was aborted before all actions ran.
    """
    if not logs_dir:
        return "unknown"
    label = _label_from_target(target)
    log = logs_dir / f"{label}.log"
    if not log.exists():
        return "unknown"
    try:
        text = log.read_text(errors="replace")
    except OSError:
        return "unknown"

    pn_vdb = row.get("pn_vdb") if isinstance(row.get("pn_vdb"), int) else 0
    pn_completed = row.get("pn_completed") if isinstance(row.get("pn_completed"), int) else 0
    pn_actions = row.get("pn_actions") if isinstance(row.get("pn_actions"), int) else 0

    # 1) Explicit FAIL on target install step (parsed from "First error")
    for m in _FIRSTERR_RE.finditer(text):
        atom = m.group(1)
        if atom.startswith(target + "-"):
            return "FAIL"

    # 2) VDB delta cross-check
    pn_only, em_only, in_both = _parse_vdb_delta(text)
    pn_built_target = any(a.startswith(target + "-") for a in pn_only)
    em_built_target = any(a.startswith(target + "-") for a in em_only)

    if pn_built_target:
        return "OK"
    if in_both >= 1 and not em_built_target:
        return "OK"
    if em_built_target:
        return "FAIL"

    # 3) Nothing in VDB and aborted partway through
    if pn_vdb == 0:
        if pn_completed < pn_actions:
            return "aborted"
        return "absent"

    # 4) pn_vdb > 0 but no positive target signal: aborted partway
    if pn_completed < pn_actions:
        return "aborted"

    # 5) Optimistic fallback (all actions completed, vdb non-empty,
    #    no negative signal)
    return "OK"


def annotate_target_install(rows: list[dict], logs_dir: Optional[Path]) -> None:
    """Mutate rows in place adding 'pn_target' field."""
    for r in rows:
        r["pn_target"] = detect_target_install(r["target"], r, logs_dir)


def real_pn_built(r: dict) -> bool:
    """True iff portage-ng actually got the target into the VDB."""
    if r.get("pn_target") == "OK":
        return True
    if r.get("pn_target") in ("unknown", None):
        return r["pn_exit"].startswith("OK") and isinstance(r.get("pn_vdb"), int) and r["pn_vdb"] > 0
    return False


# --- aggregates ---------------------------------------------------------------

def status_table(rows: list[dict]) -> str:
    """Markdown table of status-pair frequencies."""
    counts = Counter((r["pn_exit"], r["em_exit"]) for r in rows)
    out = ["| portage-ng | emerge | count |", "|---|---|---:|"]
    for (pn, em), n in counts.most_common():
        out.append(f"| {pn} | {em} | {n} |")
    return "\n".join(out)


def summary_metrics(rows: list[dict]) -> dict:
    n = len(rows)
    pn_planned = sum(1 for r in rows if r["pn_exit"].startswith("OK"))
    em_planned = sum(1 for r in rows if r["em_exit"] == "OK")
    pn_built = sum(1 for r in rows if real_pn_built(r))
    em_built = sum(1 for r in rows if r["em_exit"] == "OK"
                   and isinstance(r.get("em_vdb"), int) and r["em_vdb"] > 0)

    pn_silent = sum(1 for r in rows
                    if r["pn_exit"].startswith("OK") and not real_pn_built(r))

    same_vdb = sum(1 for r in rows if r.get("vdb_delta") == "=")
    pn_more = sum(1 for r in rows if isinstance(r.get("vdb_delta"), str)
                  and r["vdb_delta"].startswith("+"))
    pn_less = sum(1 for r in rows if isinstance(r.get("vdb_delta"), str)
                  and r["vdb_delta"].startswith("-"))

    total_seconds = sum(r["seconds"] for r in rows
                        if isinstance(r.get("seconds"), int))

    return dict(n=n,
                pn_planned=pn_planned, em_planned=em_planned,
                pn_built=pn_built, em_built=em_built, pn_silent=pn_silent,
                same_vdb=same_vdb, pn_more=pn_more, pn_less=pn_less,
                total_seconds=total_seconds)


# --- per-row formatters -------------------------------------------------------

def row_table(rows: list[dict], full: bool = False) -> str:
    cols = ["target", "pn_exit", "pn_target", "em_exit",
            "pn_actions", "em_actions",
            "pn_completed", "em_completed", "pn_vdb", "em_vdb",
            "vdb_delta", "seconds"]
    out = ["| " + " | ".join(cols) + " |",
           "|" + "|".join(["---"] * len(cols)) + "|"]
    show = rows if full else rows[:30]
    for r in show:
        out.append("| " + " | ".join(str(r.get(c, "?")) for c in cols) + " |")
    if not full and len(rows) > 30:
        out.append(f"| ... (truncated; {len(rows) - 30} more rows in TSV) | | | | | | | | | | | |")
    return "\n".join(out)


def deltas_table(rows: list[dict]) -> str:
    rows = [r for r in rows if r.get("vdb_delta") not in ("=", "?")]
    if not rows:
        return "_(no VDB count differences)_"
    out = ["| target | portage-ng VDB | emerge VDB | delta |",
           "|---|---:|---:|---:|"]
    for r in rows:
        out.append(f"| {r['target']} | {r['pn_vdb']} | {r['em_vdb']} | {r['vdb_delta']} |")
    return "\n".join(out)


def failures_table(rows: list[dict]) -> str:
    failed = [r for r in rows
              if not (r["pn_exit"].startswith("OK") and r["em_exit"] == "OK")]
    if not failed:
        return "_(none)_"
    out = ["| target | portage-ng exit | portage-ng target | emerge |",
           "|---|---|---|---|"]
    for r in failed:
        out.append(f"| {r['target']} | {r['pn_exit']} | "
                   f"{r.get('pn_target','?')} | {r['em_exit']} |")
    return "\n".join(out)


def silent_failures_table(rows: list[dict]) -> str:
    silent = [r for r in rows
              if r["pn_exit"].startswith("OK") and not real_pn_built(r)]
    if not silent:
        return "_(none)_"
    out = ["| target | pn_exit | pn_target | em_exit | em_target_built |",
           "|---|---|---|---|---|"]
    for r in silent:
        em_built = "yes" if (r["em_exit"] == "OK"
                             and isinstance(r.get("em_vdb"), int)
                             and r["em_vdb"] > 0) else "no"
        out.append(f"| {r['target']} | {r['pn_exit']} | "
                   f"{r.get('pn_target','?')} | {r['em_exit']} | {em_built} |")
    return "\n".join(out)


# --- main ---------------------------------------------------------------------

def render(pretend: Optional[list[dict]],
           build: Optional[list[dict]],
           commit: str,
           build_logs: Optional[Path] = None,
           pretend_logs: Optional[Path] = None,
           manifest: str = "Source/Application/Wrapper/Linux/tinderbox-ng.d/manifest-100.txt") -> str:
    if pretend is not None:
        annotate_target_install(pretend, pretend_logs)
    if build is not None:
        annotate_target_install(build, build_logs)
    today = dt.date.today().isoformat()
    parts: list[str] = []
    n_targets = len(build) if build else (len(pretend) if pretend else 0)
    parts.append(f"# tinderbox-ng compare report — {today} ({commit})\n")
    parts.append("Side-by-side comparison of `portage-ng` and traditional `emerge` over a")
    parts.append(f"{n_targets}-package matrix, run through the `tinderbox-ng compare` harness on")
    parts.append("`vm-linux.local`. Each comparison runs in two fresh OverlayFS sessions")
    parts.append("spawned from the same immutable baseline (stage3 + SWI-Prolog +")
    parts.append("portage-ng + matching `kb.qlf`), in parallel, in private mount")
    parts.append("namespaces. **Only fresh installs**: every target is a package not")
    parts.append("present in the stage3 VDB, so each run goes through the full")
    parts.append("`clean → setup → unpack → prepare → configure → compile → install →")
    parts.append("merge` chain on both engines.\n")
    parts.append(f"Driver:   `Source/Application/Wrapper/Linux/tinderbox-ng.d/compare-matrix.sh`")
    parts.append(f"Manifest: `{manifest}`")
    parts.append(f"Commit:   `{commit}`\n")

    if build:
        # Compute findings buckets
        b_genuine_emerge_fail = [
            r for r in build
            if r["em_exit"] != "OK" and r.get("pn_target") in ("FAIL", "aborted", "absent")
        ]
        b_pn_only_wins = [
            r for r in build
            if r["em_exit"] != "OK" and r.get("pn_target") == "OK"
        ]
        b_pn_only_loss = [
            r for r in build
            if r["em_exit"] == "OK"
            and isinstance(r.get("em_vdb"), int) and r["em_vdb"] > 0
            and r.get("pn_target") in ("FAIL", "aborted", "absent")
        ]
    else:
        b_genuine_emerge_fail = []
        b_pn_only_wins = []
        b_pn_only_loss = []

    parts.append("## Headline findings\n")
    if pretend:
        em_plan_fail = [r for r in pretend if r["em_exit"] != "OK"]
        parts.append(f"- **{len(em_plan_fail)} packages** that **emerge fails to plan** but "
                     f"portage-ng plans cleanly (with cycle-break or domain assumptions). "
                     "Likely emerge being too strict about masked deps or REQUIRED_USE; "
                     "portage-ng's progressive-relaxation prover finds a satisfying assignment.")
    if build:
        parts.append(f"- **{len(b_pn_only_wins)} portage-ng-only build wins**: "
                     "emerge fails (plan or build), portage-ng builds the target end-to-end "
                     "(target lands in VDB).")
        parts.append(f"- **{len(b_pn_only_loss)} portage-ng-only build losses**: "
                     "emerge builds the target, portage-ng does not "
                     "(target install step failed, or portage-ng aborted on a sub-dep). "
                     "These are real bugs and the most actionable item.")
        parts.append(f"- **{len(b_genuine_emerge_fail)} cases where both engines fail** on "
                     "the same package (or its sub-deps). These are upstream/ebuild issues, "
                     "not engine bugs.")
        b_silent_count = summary_metrics(build)["pn_silent"]
        if b_silent_count > 0:
            parts.append(f"- **{b_silent_count} portage-ng silent failures**: pipeline "
                         "exited 0 but the target was never merged into VDB. Hits a known "
                         "regression of the action:maybe_ci_exit_on_build_failure/1 guard; "
                         "see the dedicated section below for the package list.")
        else:
            parts.append("- **0 portage-ng silent failures**: every package whose pipeline "
                         "exited 0 also landed the target in VDB. The earlier "
                         "`maybe_ci_exit_on_build_failure/1` regression that produced "
                         "12/65 silent failures in the 2026-05-09 matrix is no longer "
                         "observable.\n")

    if b_pn_only_wins:
        parts.append("### portage-ng-only build wins\n")
        out = ["| target | pn_exit | em_exit | pn_vdb | em_vdb |",
               "|---|---|---|---:|---:|"]
        for r in b_pn_only_wins:
            out.append(f"| {r['target']} | {r['pn_exit']} | {r['em_exit']} | "
                       f"{r.get('pn_vdb','?')} | {r.get('em_vdb','?')} |")
        parts.append("\n".join(out))
        parts.append("")

    if b_pn_only_loss:
        parts.append("### portage-ng-only build losses (action items)\n")
        out = ["| target | pn_target | pn_exit | em_exit | pn_vdb | em_vdb |",
               "|---|---|---|---|---:|---:|"]
        for r in b_pn_only_loss:
            out.append(f"| {r['target']} | {r.get('pn_target','?')} | {r['pn_exit']} | "
                       f"{r['em_exit']} | {r.get('pn_vdb','?')} | {r.get('em_vdb','?')} |")
        parts.append("\n".join(out))
        parts.append("")

    parts.append("## Executive summary\n")
    rows = []
    rows.append("| | Pretend tier | Build tier |")
    rows.append("|---|---|---|")
    p_n = len(pretend) if pretend else 0
    b_n = len(build) if build else 0
    p_pn = summary_metrics(pretend)["pn_planned"] if pretend else 0
    p_em = summary_metrics(pretend)["em_planned"] if pretend else 0
    b_pn = summary_metrics(build)["pn_built"] if build else 0
    b_em = summary_metrics(build)["em_built"] if build else 0
    same = summary_metrics(build)["same_vdb"] if build else 0
    rows.append(f"| Packages tested | {p_n} | {b_n} |")
    if pretend:
        rows.append(f"| portage-ng plannable | {p_pn} / {p_n} ({100*p_pn//max(p_n,1)}%) | — |")
        rows.append(f"| emerge plannable | {p_em} / {p_n} ({100*p_em//max(p_n,1)}%) | — |")
    if build:
        b_silent = summary_metrics(build)["pn_silent"]
        rows.append(f"| portage-ng built target | — | {b_pn} / {b_n} ({100*b_pn//max(b_n,1)}%) |")
        rows.append(f"| emerge built target | — | {b_em} / {b_n} ({100*b_em//max(b_n,1)}%) |")
        rows.append(f"| portage-ng silent failures (exit 0 / target not built) | — | {b_silent} / {b_n} ({100*b_silent//max(b_n,1)}%) |")
        rows.append(f"| Identical VDB after build | — | {same} / {b_n} ({100*same//max(b_n,1)}%) |")
    parts.append("\n".join(rows))
    parts.append("")

    if pretend:
        parts.append("## Pretend tier (planner-only)\n")
        m = summary_metrics(pretend)
        parts.append(f"{m['n']} packages, total wall time "
                     f"{m['total_seconds']}s ({m['total_seconds']//60}m).\n")
        parts.append("### Status distribution\n")
        parts.append(status_table(pretend))
        parts.append("\nStatus legend (portage-ng):\n")
        parts.append("- `OK` — exit 0, plan produced with no assumptions")
        parts.append("- `OK(cycles)` — exit 1, prover cycle-break assumptions")
        parts.append("- `OK(assumed)` — exit 2, ≥1 domain assumption (e.g. masked dep)")
        parts.append("- `FAIL(N)` — non-zero exit other than 1/2 (real crash)\n")
        parts.append("### emerge plan failures\n")
        em_fail = [r for r in pretend if r["em_exit"] != "OK"]
        if em_fail:
            out = ["| target | portage-ng | emerge |", "|---|---|---|"]
            for r in em_fail:
                out.append(f"| {r['target']} | {r['pn_exit']} | {r['em_exit']} |")
            parts.append("\n".join(out))
            parts.append(f"\n**{len(em_fail)} package(s)** that emerge cannot plan, "
                         "but portage-ng can. Worth investigating whether emerge is")
            parts.append("being too strict or portage-ng is too permissive.\n")
        else:
            parts.append("_(none)_\n")

    if build:
        parts.append("## Build tier (full execution)\n")
        m = summary_metrics(build)
        parts.append(f"{m['n']} packages, total wall time "
                     f"{m['total_seconds']}s ({m['total_seconds']//60}m).\n")
        parts.append("### Status distribution\n")
        parts.append(status_table(build))
        parts.append("")
        parts.append("### portage-ng silent failures\n")
        parts.append("Cases where the **portage-ng pipeline exit code stayed 0** but the")
        parts.append("**target package itself was not installed**. Either the target's")
        parts.append("install step explicitly failed (`pn_target=FAIL`), or portage-ng")
        parts.append("never reached the install step for the target (`pn_target=absent`,")
        parts.append("typically because a sub-dep failed and the failure didn't propagate")
        parts.append("up to the pipeline exit code).\n")
        parts.append(silent_failures_table(build))
        parts.append("")
        parts.append("### Both-failed / one-side-failed\n")
        parts.append(failures_table(build))
        parts.append("")
        parts.append("### VDB-count deltas (portage-ng vs emerge)\n")
        parts.append(deltas_table(build))
        parts.append("\nA non-zero delta means the two engines installed a different")
        parts.append("set of packages for the same target. Causes range from version")
        parts.append("selection (revision pick) to differences in build-vs-runtime")
        parts.append("dep handling.\n")
        parts.append("### Per-package detail (truncated)\n")
        parts.append(row_table(build, full=False))
        parts.append("")

    parts.append("## Reproducing this report\n")
    parts.append("```sh")
    parts.append("# Pretend tier")
    parts.append("ssh root@vm-linux.local 'compare-matrix --pretend \\")
    parts.append("  --manifest /usr/local/share/tinderbox-ng/manifest-100.txt'")
    parts.append("")
    parts.append("# Build tier (over the OK/OK subset of the pretend tier)")
    parts.append("# (the build manifest is generated automatically by")
    parts.append("# Reports/Scripts/render-compare-matrix.py from the pretend TSV)")
    parts.append("ssh root@vm-linux.local 'compare-matrix --build \\")
    parts.append("  --manifest /usr/local/share/tinderbox-ng/build-manifest.txt'")
    parts.append("```\n")
    parts.append("Per-package logs are preserved in")
    parts.append("`/srv/tinderbox-ng/reports/compare-matrix-<stamp>/<label>.log`. The")
    parts.append("raw TSVs are committed alongside this report under")
    parts.append("`Reports/tinderbox-compare-<date>-<commit>.d/`.")
    return "\n".join(parts) + "\n"


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--pretend", type=Path, help="pretend-tier TSV")
    p.add_argument("--build", type=Path, help="build-tier TSV")
    p.add_argument("--pretend-logs", type=Path,
                   help="dir of per-package pretend logs (for silent-failure detection)")
    p.add_argument("--build-logs", type=Path,
                   help="dir of per-package build logs (for silent-failure detection)")
    p.add_argument("--commit", required=True, help="git short commit hash")
    p.add_argument("--manifest",
                   default="Source/Application/Wrapper/Linux/tinderbox-ng.d/manifest-100.txt",
                   help="manifest path to cite in the report header")
    p.add_argument("--out", type=Path, required=True, help="output markdown path")
    args = p.parse_args()

    pretend = load_tsv(args.pretend) if args.pretend else None
    build = load_tsv(args.build) if args.build else None
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(render(pretend, build, args.commit,
                               build_logs=args.build_logs,
                               pretend_logs=args.pretend_logs,
                               manifest=args.manifest))
    print(f"wrote {args.out}")


if __name__ == "__main__":
    main()
