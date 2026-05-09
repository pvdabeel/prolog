# tinderbox-ng compare report — 2026-05-09 (6494920f)

Side-by-side comparison of `portage-ng` and traditional `emerge` over a
100+ package matrix, run through the `tinderbox-ng compare` harness on
`vm-linux.local`. Each comparison runs in two fresh OverlayFS sessions
spawned from the same immutable baseline (stage3 + SWI-Prolog +
portage-ng + matching `kb.qlf`), in parallel, in private mount
namespaces. **Only fresh installs**: every target is a package not
present in the stage3 VDB, so each run goes through the full
`clean → setup → unpack → prepare → configure → compile → install →
merge` chain on both engines.

Driver:   `Source/Application/Wrapper/Linux/tinderbox-ng.d/compare-matrix.sh`
Manifest: `Source/Application/Wrapper/Linux/tinderbox-ng.d/manifest-100.txt`
Commit:   `6494920f`

## Headline findings

- **9 packages** that **emerge fails to plan** but portage-ng plans cleanly (with cycle-break or domain assumptions). Likely emerge being too strict about masked deps or REQUIRED_USE; portage-ng's progressive-relaxation prover finds a satisfying assignment.
- **2 portage-ng-only build wins**: emerge fails (plan or build), portage-ng builds the target end-to-end (target lands in VDB).
- **5 portage-ng-only build losses**: emerge builds the target, portage-ng does not (target install step failed, or portage-ng aborted on a sub-dep). These are real bugs and the most actionable item.
- **7 cases where both engines fail** on the same package (or its sub-deps). These are upstream/ebuild issues, not engine bugs — but portage-ng currently masks them as `exit 0`.
- **Silent-failure bug in portage-ng**: when a sub-dep install step fails, the failure does not always propagate to the pipeline exit code. 12/65 build-tier runs reported `exit 0` while the target was never merged into VDB. Compare-matrix detection relies on parsing the VDB-delta and first-error sections, which is now done by `render-compare-matrix.py`.

### portage-ng-only build wins

| target | pn_exit | em_exit | pn_vdb | em_vdb |
|---|---|---|---:|---:|
| net-analyzer/tcpdump | OK | FAIL(1) | 2 | 3 |
| net-vpn/openvpn | OK | FAIL(1) | 5 | 4 |

### portage-ng-only build losses (action items)

| target | pn_target | pn_exit | em_exit | pn_vdb | em_vdb |
|---|---|---|---|---:|---:|
| app-misc/mc | FAIL | OK | OK | 6 | 2 |
| net-analyzer/iftop | FAIL | OK | OK | 5 | 2 |
| net-dns/dnsmasq | FAIL | OK | OK | 6 | 3 |
| www-client/w3m | FAIL | OK | OK | 5 | 3 |
| dev-vcs/subversion | FAIL | OK | OK | 5 | 9 |

## Executive summary

| | Pretend tier | Build tier |
|---|---|---|
| Packages tested | 104 | 65 |
| portage-ng plannable | 104 / 104 (100%) | — |
| emerge plannable | 95 / 104 (91%) | — |
| portage-ng built target | — | 53 / 65 (81%) |
| emerge built target | — | 56 / 65 (86%) |
| portage-ng silent failures (exit 0 / target not built) | — | 12 / 65 (18%) |
| Identical VDB after build | — | 49 / 65 (75%) |

## Pretend tier (planner-only)

104 packages, total wall time 784s (13m).

### Status distribution

| portage-ng | emerge | count |
|---|---|---:|
| OK | OK | 65 |
| OK(assumed) | OK | 30 |
| OK(cycles) | FAIL(1) | 8 |
| OK(assumed) | FAIL(1) | 1 |

Status legend (portage-ng):

- `OK` — exit 0, plan produced with no assumptions
- `OK(cycles)` — exit 1, prover cycle-break assumptions
- `OK(assumed)` — exit 2, ≥1 domain assumption (e.g. masked dep)
- `FAIL(N)` — non-zero exit other than 1/2 (real crash)

### emerge plan failures

| target | portage-ng | emerge |
|---|---|---|
| dev-util/strace | OK(cycles) | FAIL(1) |
| dev-util/ltrace | OK(cycles) | FAIL(1) |
| dev-util/cmake | OK(cycles) | FAIL(1) |
| dev-util/ninja | OK(cycles) | FAIL(1) |
| dev-util/meson | OK(cycles) | FAIL(1) |
| sys-apps/tree | OK(cycles) | FAIL(1) |
| app-text/pandoc-cli | OK(assumed) | FAIL(1) |
| app-doc/doxygen | OK(cycles) | FAIL(1) |
| sys-apps/at | OK(cycles) | FAIL(1) |

**9 package(s)** that emerge cannot plan, but portage-ng can. Worth investigating whether emerge is
being too strict or portage-ng is too permissive.

## Build tier (full execution)

65 packages, total wall time 3743s (62m).

### Status distribution

| portage-ng | emerge | count |
|---|---|---:|
| OK | OK | 56 |
| OK | FAIL(1) | 9 |

### portage-ng silent failures

Cases where the **portage-ng pipeline exit code stayed 0** but the
**target package itself was not installed**. Either the target's
install step explicitly failed (`pn_target=FAIL`), or portage-ng
never reached the install step for the target (`pn_target=absent`,
typically because a sub-dep failed and the failure didn't propagate
up to the pipeline exit code).

| target | pn_exit | pn_target | em_exit | em_target_built |
|---|---|---|---|---|
| app-misc/mc | OK | FAIL | OK | yes |
| sys-process/atop | OK | aborted | FAIL(1) | no |
| app-arch/p7zip | OK | aborted | FAIL(1) | no |
| app-arch/cabextract | OK | aborted | FAIL(1) | no |
| net-analyzer/iftop | OK | FAIL | OK | yes |
| net-dns/dnsmasq | OK | FAIL | OK | yes |
| net-irc/weechat | OK | aborted | FAIL(1) | no |
| mail-client/alpine | OK | FAIL | FAIL(1) | no |
| www-client/w3m | OK | FAIL | OK | yes |
| dev-vcs/subversion | OK | FAIL | OK | yes |
| media-libs/freetype | OK | FAIL | FAIL(1) | no |
| dev-libs/jansson | OK | FAIL | FAIL(1) | no |

### Both-failed / one-side-failed

| target | portage-ng exit | portage-ng target | emerge |
|---|---|---|---|
| sys-process/atop | OK | aborted | FAIL(1) |
| app-arch/p7zip | OK | aborted | FAIL(1) |
| app-arch/cabextract | OK | aborted | FAIL(1) |
| net-analyzer/tcpdump | OK | OK | FAIL(1) |
| net-irc/weechat | OK | aborted | FAIL(1) |
| mail-client/alpine | OK | FAIL | FAIL(1) |
| media-libs/freetype | OK | FAIL | FAIL(1) |
| net-vpn/openvpn | OK | OK | FAIL(1) |
| dev-libs/jansson | OK | FAIL | FAIL(1) |

### VDB-count deltas (portage-ng vs emerge)

| target | portage-ng VDB | emerge VDB | delta |
|---|---:|---:|---:|
| app-editors/jed | 4 | 2 | +2 |
| app-misc/mc | 6 | 2 | +4 |
| sys-process/parallel | 5 | 4 | +1 |
| net-misc/aria2 | 2 | 1 | +1 |
| net-analyzer/tcpdump | 2 | 3 | -1 |
| net-analyzer/iftop | 5 | 2 | +3 |
| net-dns/dnsmasq | 6 | 3 | +3 |
| net-irc/irssi | 2 | 1 | +1 |
| net-irc/weechat | 1 | 0 | +1 |
| www-servers/lighttpd | 4 | 6 | -2 |
| www-client/w3m | 5 | 3 | +2 |
| dev-vcs/mercurial | 5 | 1 | +4 |
| dev-vcs/subversion | 5 | 9 | -4 |
| sys-apps/usbutils | 5 | 4 | +1 |
| media-libs/freetype | 10 | 1 | +9 |
| net-vpn/openvpn | 5 | 4 | +1 |

A non-zero delta means the two engines installed a different
set of packages for the same target. Causes range from version
selection (revision pick) to differences in build-vs-runtime
dep handling.

### Per-package detail (truncated)

| target | pn_exit | pn_target | em_exit | pn_actions | em_actions | pn_completed | em_completed | pn_vdb | em_vdb | vdb_delta | seconds |
|---|---|---|---|---|---|---|---|---|---|---|---|
| app-editors/joe | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 35 |
| app-editors/mg | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 44 |
| app-editors/jed | OK | OK | OK | 12 | 2 | 13 | 2 | 4 | 2 | +2 | 112 |
| app-shells/zsh | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 120 |
| app-shells/dash | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 25 |
| app-shells/mksh | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 37 |
| app-misc/screen | OK | OK | OK | 5 | 2 | 6 | 2 | 2 | 2 | = | 45 |
| app-misc/jq | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 57 |
| app-misc/mc | OK | FAIL | OK | 41 | 2 | 21 | 2 | 6 | 2 | +4 | 111 |
| sys-process/htop | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 27 |
| sys-process/lsof | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 30 |
| sys-process/numactl | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 32 |
| sys-process/atop | OK | aborted | FAIL(1) | 6 | 0 | 2 | 0 | 0 | 0 | = | 18 |
| sys-process/parallel | OK | OK | OK | 14 | 4 | 15 | 4 | 5 | 4 | +1 | 226 |
| app-arch/p7zip | OK | aborted | FAIL(1) | 5 | 0 | 2 | 0 | 0 | 0 | = | 19 |
| app-arch/unrar | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 33 |
| app-arch/zip | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 24 |
| app-arch/lz4 | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 26 |
| app-arch/cabextract | OK | aborted | FAIL(1) | 6 | 0 | 2 | 0 | 0 | 0 | = | 17 |
| net-misc/socat | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 46 |
| net-misc/whois | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 21 |
| net-misc/iperf | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 50 |
| net-misc/aria2 | OK | OK | OK | 6 | 1 | 7 | 1 | 2 | 1 | +1 | 77 |
| net-analyzer/tcpdump | OK | OK | FAIL(1) | 59 | 1 | 19 | 3 | 2 | 3 | -1 | 30 |
| net-analyzer/mtr | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 28 |
| net-analyzer/iftop | OK | FAIL | OK | 47 | 2 | 21 | 2 | 5 | 2 | +3 | 75 |
| net-analyzer/iptraf-ng | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 22 |
| net-analyzer/netcat | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 22 |
| net-dns/dnsmasq | OK | FAIL | OK | 48 | 3 | 22 | 3 | 6 | 3 | +3 | 48 |
| net-irc/irssi | OK | OK | OK | 5 | 1 | 6 | 1 | 2 | 1 | +1 | 216 |
| ... (truncated; 35 more rows in TSV) | | | | | | | | | | | |

## Reproducing this report

```sh
# Pretend tier
ssh root@vm-linux.local 'compare-matrix --pretend \
  --manifest /usr/local/share/tinderbox-ng/manifest-100.txt'

# Build tier (over the OK/OK subset of the pretend tier)
# (the build manifest is generated automatically by
# Reports/Scripts/render-compare-matrix.py from the pretend TSV)
ssh root@vm-linux.local 'compare-matrix --build \
  --manifest /usr/local/share/tinderbox-ng/build-manifest.txt'
```

Per-package logs are preserved in
`/srv/tinderbox-ng/reports/compare-matrix-<stamp>/<label>.log`. The
raw TSVs are committed alongside this report under
`Reports/tinderbox-compare-<date>-<commit>.d/`.
