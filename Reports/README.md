# Reports

Regression analysis scripts and result data for portage-ng.

## Scripts

### Merge-vs-emerge comparison (moved to tinderbox-ng)

The `.merge` vs `.emerge` comparison harness — Jaccard accuracy metrics
(CN, CN+V, CN+V+U), ordering concordance, and assumption counts — now
lives in the [tinderbox-ng](https://github.com/pvdabeel/tinderbox-ng)
repository. It is driven by the higher-level `tinderbox-ng` command,
which runs target packages through both engines in identical sessions
and produces both per-target reports and matrix-wide analyses.

```sh
# Per-target compare (planner only, both sessions cleaned up):
tinderbox-ng compare www-servers/apache

# Matrix-wide analysis over an existing log directory:
sudo tinderbox-ng analyze --logdir /srv/tinderbox-ng/logs/compare-<stamp>
```

The underlying Python lives at
`share/tinderbox-ng/compare-merge-emerge.py` inside that repository and
is invoked automatically by `tinderbox-ng analyze`.

### `Reports/Scripts/compare-prover-failset.py`

Diff failed packages between two `prover:test(portage)` logs.

```bash
python3 Reports/Scripts/compare-prover-failset.py \
  --baseline baseline.log \
  --candidate candidate.log \
  --out Reports/prover_failset_compare.json
```

### `Reports/Scripts/compare-md5-cache.py`

Validate ebuild metadata against md5-cache entries.

### `Reports/Scripts/extract-timing.py`

Build a timing database from graph output files for build time estimation.

## Report data

Historical `compare-<YYYY-MM-DD>-<short-commit-hash>.json.gz` snapshots
that lived here have been pruned; current matrix-wide outputs land in
`/srv/tinderbox-ng/reports/compare-matrix-*` and the tinderbox-ng
repository itself.
