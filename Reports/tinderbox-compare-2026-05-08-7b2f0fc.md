# tinderbox-ng compare report — 2026-05-08 (7b2f0fc)

Side-by-side comparison of `portage-ng` and traditional `emerge` over a
diverse package set, run through the `tinderbox-ng compare` harness on
`vm-linux.local` (Gentoo Prefix-style chroot, identical Portage tree
and VDB on both sides). Every comparison runs in two fresh OverlayFS
sessions spawned from the same immutable baseline, in parallel, in
private mount namespaces.

Driver:    `Source/Application/Wrapper/Linux/tinderbox-ng.d/compare-matrix.sh`
Harness:   `tinderbox-ng compare [--pretend|--build]`
Commit:    `7b2f0fc2` (post fix for silent install no-op)

## Executive summary

| | Pretend tier | Build tier |
|---|---|---|
| Packages tested | 19 | 9 |
| portage-ng produced a plan | 19 / 19 (**100%**) | 9 / 9 (**100%**) |
| emerge produced a plan / built | 17 / 19 (89%) | 9 / 9 (100%) |
| Identical VDB after build | n/a | 8 / 9 (89%) |

- **Both engines now build apache end-to-end** (regression fixed in
  commit `7b2f0fc2`; before this commit, portage-ng silently skipped
  the install of the chain-tail target whenever the ebuild had a
  duplicate IUSE declaration coming from inherited eclasses).
- **emerge fails on `nginx` and `graphviz` at plan time** (`FAIL(1)`
  from emerge's resolver), while portage-ng plans both with domain
  assumptions. Worth investigating what blocks emerge here.
- **One real behaviour delta**: for `net-irc/irssi`, portage-ng pulls
  in a fresh `dev-lang/perl-5.42.2` build-dep that emerge omits, and
  selects `irssi-1.4.5-r2` where emerge picks `irssi-1.4.5`. Cause
  not yet diagnosed (likely missing build-time-only dep filtering or
  an unmet preference predicate; tracked separately).

## Pretend tier (planner-only)

19 packages, broad coverage from trivial libs to complex web servers.
Every comparison was driven by

```sh
ssh root@vm-linux.local 'compare-matrix --pretend <pkg>...'
```

| Target | portage-ng | emerge | PN actions | EM actions |
|---|---|---|---:|---:|
| app-arch/bzip2 | OK | OK | 2 | 1 |
| app-arch/xz-utils | OK | OK | 2 | 1 |
| sys-libs/zlib | OK | OK | 2 | 1 |
| sys-apps/coreutils | OK(assumed) | OK | 3 | 1 |
| dev-libs/openssl | OK(assumed) | OK | 3 | 1 |
| dev-libs/glib | OK(assumed) | OK | 49 | 1 |
| net-misc/curl | OK | OK | 2 | 1 |
| net-misc/wget | OK | OK | 3 | 1 |
| app-editors/vim | OK(assumed) | OK | 53 | 6 |
| app-editors/nano | OK | OK | 3 | 1 |
| app-shells/zsh | OK | OK | 3 | 1 |
| app-misc/tmux | OK(assumed) | OK | 6 | 2 |
| sys-process/htop | OK | OK | 3 | 1 |
| net-irc/irssi | OK | OK | 5 | 1 |
| www-servers/apache | OK | OK | 16 | 6 |
| www-servers/nginx | OK(assumed) | **FAIL(1)** | 9 | ? |
| media-gfx/graphviz | OK(assumed) | **FAIL(1)** | 114 | ? |
| dev-vcs/git | OK(assumed) | OK | 24 | 1 |
| sys-apps/portage | OK | OK | 3 | 1 |

Status legend (portage-ng):
- `OK` — exit 0, plan produced with no assumptions
- `OK(cycles)` — exit 1, plan produced with prover cycle-break assumptions
- `OK(assumed)` — exit 2, plan produced with at least one domain assumption
  (e.g. masked dependency, missing ebuild for an exact version constraint)
- `FAIL(N)` — non-zero exit other than 1/2 (a real crash); none observed

emerge counts ebuilds; portage-ng counts actions (download / install /
run / register), so its action numbers are higher even when the
underlying plan is the same. The two are reconciled by the build tier's
"merged into VDB" column.

## Build tier (full execution)

9 packages, every one of which planned cleanly on both engines. Each
build runs the complete phase chain (`clean → setup → unpack → prepare
→ configure → compile → install → merge`) and the resulting upper-layer
VDB is diffed.

| Target | portage-ng | emerge | PN completed | EM completed | PN VDB | EM VDB | Delta |
|---|---|---|---:|---:|---:|---:|---:|
| app-arch/bzip2 | OK | OK | 3 | 1 | 1 | 1 | = |
| sys-libs/zlib | OK | OK | 3 | 1 | 1 | 1 | = |
| app-arch/xz-utils | OK | OK | 3 | 1 | 1 | 1 | = |
| net-misc/wget | OK | OK | 4 | 1 | 1 | 1 | = |
| app-editors/nano | OK | OK | 4 | 1 | 1 | 1 | = |
| app-shells/zsh | OK | OK | 4 | 1 | 1 | 1 | = |
| sys-process/htop | OK | OK | 4 | 1 | 1 | 1 | = |
| net-irc/irssi | OK | OK | 6 | 1 | 2 | 1 | **+1** |
| www-servers/apache | OK | OK | 17 | 6 | 6 | 6 | = |

`completed` for portage-ng counts every phase-completion line (download,
install, run, register) and so is always larger than emerge's "packages
installed" count even when the actual outcome is identical.

### Investigation: irssi VDB delta

A `--keep` rerun captured the actual difference:

```text
VDB delta (after build):
  only in portage-ng:
    + dev-lang/perl-5.42.2
    + net-irc/irssi-1.4.5-r2
  only in emerge:
    + net-irc/irssi-1.4.5
```

Two distinct phenomena:

1. **Extra build-dep**: portage-ng installs `dev-lang/perl-5.42.2` as
   part of irssi's chain; emerge does not. perl is part of the stage3
   so already on the live filesystem of the chroot — both sessions
   start from the same baseline. Likely root cause: portage-ng treats
   the build-time perl dep as "needs to be in VDB" even when the
   binary is already on disk, while emerge consults `vartree` and
   skips. Worth tracing through `prover` / `vdb` interaction.
2. **Different version selected**: portage-ng selects `irssi-1.4.5-r2`,
   emerge selects `irssi-1.4.5`. `-r2` is the higher revision and is
   the natural choice for a fresh install; emerge's pick of `1.4.5`
   may reflect a vdb-installed version preference. Not necessarily a
   bug — could be either side's preference predicate at work.

Neither is a build correctness problem; both VDB entries are valid
installations with working binaries. Recorded for follow-up.

## Notable absence: silent install/merge no-op

A bug that previously caused `www-servers/apache` to be silently
skipped at install time has been fixed in this commit
(`pairs_to_assoc_dedup` for IUSE flag pairs, plus jobserver
instrumentation that surfaces any future silent-drop). Apache now
appears in the VDB on both sides with identical contents.

The harness was instrumental in finding it: before commit
`7b2f0fc2`, an apache `--build` showed `Total: 16 completed`,
`[step 9] OK   register www-servers/apache`, and exit 0 — yet no
apache binary on disk and no VDB entry. The compare against emerge
(which *did* install apache) made the regression undeniable.

## Reproducing this report

```sh
# Pretend tier
ssh root@vm-linux.local 'compare-matrix --pretend \
  app-arch/bzip2 app-arch/xz-utils sys-libs/zlib sys-apps/coreutils \
  dev-libs/openssl dev-libs/glib net-misc/curl net-misc/wget \
  app-editors/vim app-editors/nano app-shells/zsh app-misc/tmux \
  sys-process/htop net-irc/irssi www-servers/apache www-servers/nginx \
  media-gfx/graphviz dev-vcs/git sys-apps/portage'

# Build tier (only packages where pretend is OK/OK on both engines)
ssh root@vm-linux.local 'compare-matrix --build \
  app-arch/bzip2 sys-libs/zlib app-arch/xz-utils net-misc/wget \
  app-editors/nano app-shells/zsh sys-process/htop net-irc/irssi \
  www-servers/apache'
```

Per-package logs are preserved in
`/srv/tinderbox-ng/reports/compare-matrix-<stamp>/<label>.log`.
TSVs are in `results.tsv` in the same directory.

## Open follow-ups

1. **emerge plan failure on `nginx` and `graphviz`**: portage-ng plans
   both. Find out what emerge is rejecting and decide whether
   portage-ng should reject too.
2. **irssi VDB +1 (perl)**: investigate why portage-ng pulls in a
   fresh perl when it's already on the live filesystem.
3. **irssi version split (1.4.5 vs 1.4.5-r2)**: check whether one
   side is using a pinned/preferred version.
4. **Build tier is small**: extend to ≥30 packages once we trust the
   harness to run unattended for several hours (CCACHE shared across
   sessions makes repeat runs much cheaper).
