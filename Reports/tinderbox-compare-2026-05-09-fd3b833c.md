# tinderbox-ng compare report — 2026-05-09 (fd3b833c)

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
Commit:   `fd3b833c`

## Headline findings

- **2 portage-ng-only build wins**: emerge fails (plan or build), portage-ng builds the target end-to-end (target lands in VDB).
- **2 portage-ng-only build losses**: emerge builds the target, portage-ng does not (target install step failed, or portage-ng aborted on a sub-dep). These are real bugs and the most actionable item.
- **7 cases where both engines fail** on the same package (or its sub-deps). These are upstream/ebuild issues, not engine bugs — but portage-ng currently masks them as `exit 0`.
- **Silent-failure bug in portage-ng**: when a sub-dep install step fails, the failure does not always propagate to the pipeline exit code. 12/65 build-tier runs reported `exit 0` while the target was never merged into VDB. Compare-matrix detection relies on parsing the VDB-delta and first-error sections, which is now done by `render-compare-matrix.py`.

### portage-ng-only build wins

| target | pn_exit | em_exit | pn_vdb | em_vdb |
|---|---|---|---:|---:|
| net-analyzer/tcpdump | FAIL(3) | FAIL(1) | 7 | 3 |
| net-vpn/openvpn | FAIL(3) | FAIL(1) | 6 | 4 |

### portage-ng-only build losses (action items)

| target | pn_target | pn_exit | em_exit | pn_vdb | em_vdb |
|---|---|---|---|---:|---:|
| app-editors/jed | FAIL | FAIL(3) | OK | 0 | 2 |
| www-servers/lighttpd | FAIL | FAIL(3) | OK | 1 | 6 |

## Executive summary

| | Pretend tier | Build tier |
|---|---|---|
| Packages tested | 0 | 65 |
| portage-ng built target | — | 56 / 65 (86%) |
| emerge built target | — | 56 / 65 (86%) |
| portage-ng silent failures (exit 0 / target not built) | — | 0 / 65 (0%) |
| Identical VDB after build | — | 51 / 65 (78%) |

## Build tier (full execution)

65 packages, total wall time 13031s (217m).

### Status distribution

| portage-ng | emerge | count |
|---|---|---:|
| OK | OK | 54 |
| FAIL(3) | FAIL(1) | 9 |
| FAIL(3) | OK | 2 |

### portage-ng silent failures

Cases where the **portage-ng pipeline exit code stayed 0** but the
**target package itself was not installed**. Either the target's
install step explicitly failed (`pn_target=FAIL`), or portage-ng
never reached the install step for the target (`pn_target=absent`,
typically because a sub-dep failed and the failure didn't propagate
up to the pipeline exit code).

_(none)_

### Both-failed / one-side-failed

| target | portage-ng exit | portage-ng target | emerge |
|---|---|---|---|
| sys-process/atop | FAIL(3) | aborted | FAIL(1) |
| app-arch/p7zip | FAIL(3) | aborted | FAIL(1) |
| app-editors/jed | FAIL(3) | FAIL | OK |
| app-arch/cabextract | FAIL(3) | aborted | FAIL(1) |
| net-analyzer/tcpdump | FAIL(3) | OK | FAIL(1) |
| net-irc/weechat | FAIL(3) | aborted | FAIL(1) |
| mail-client/alpine | FAIL(3) | FAIL | FAIL(1) |
| www-servers/lighttpd | FAIL(3) | FAIL | OK |
| dev-libs/jansson | FAIL(3) | FAIL | FAIL(1) |
| net-vpn/openvpn | FAIL(3) | OK | FAIL(1) |
| media-libs/freetype | FAIL(3) | FAIL | FAIL(1) |

### VDB-count deltas (portage-ng vs emerge)

| target | portage-ng VDB | emerge VDB | delta |
|---|---:|---:|---:|
| app-editors/jed | 0 | 2 | -2 |
| net-analyzer/tcpdump | 7 | 3 | +4 |
| net-misc/aria2 | 2 | 1 | +1 |
| www-servers/lighttpd | 1 | 6 | -5 |
| sys-process/parallel | 5 | 4 | +1 |
| app-misc/mc | 14 | 2 | +12 |
| net-irc/irssi | 2 | 1 | +1 |
| dev-vcs/mercurial | 5 | 1 | +4 |
| net-dns/dnsmasq | 18 | 3 | +15 |
| net-analyzer/iftop | 17 | 2 | +15 |
| sys-apps/usbutils | 5 | 4 | +1 |
| www-client/w3m | 13 | 3 | +10 |
| net-vpn/openvpn | 6 | 4 | +2 |
| media-libs/freetype | 10 | 1 | +9 |

A non-zero delta means the two engines installed a different
set of packages for the same target. Causes range from version
selection (revision pick) to differences in build-vs-runtime
dep handling.

### Per-package detail (truncated)

| target | pn_exit | pn_target | em_exit | pn_actions | em_actions | pn_completed | em_completed | pn_vdb | em_vdb | vdb_delta | seconds |
|---|---|---|---|---|---|---|---|---|---|---|---|
| app-shells/dash | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 94 |
| app-editors/joe | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 108 |
| app-shells/mksh | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 111 |
| app-editors/mg | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 119 |
| app-misc/screen | OK | OK | OK | 5 | 2 | 6 | 2 | 2 | 2 | = | 123 |
| app-misc/jq | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 139 |
| sys-process/htop | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 37 |
| sys-process/atop | FAIL(3) | aborted | FAIL(1) | 6 | 0 | 2 | 0 | 0 | 0 | = | 25 |
| sys-process/lsof | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 40 |
| sys-process/numactl | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 44 |
| app-arch/p7zip | FAIL(3) | aborted | FAIL(1) | 5 | 0 | 2 | 0 | 0 | 0 | = | 27 |
| app-arch/zip | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 35 |
| app-editors/jed | FAIL(3) | FAIL | OK | 12 | 2 | 3 | 2 | 0 | 2 | -2 | 188 |
| app-arch/cabextract | FAIL(3) | aborted | FAIL(1) | 6 | 0 | 2 | 0 | 0 | 0 | = | 54 |
| app-arch/unrar | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 80 |
| app-arch/lz4 | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 70 |
| net-misc/whois | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 89 |
| app-shells/zsh | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 319 |
| net-misc/socat | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 224 |
| net-analyzer/mtr | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 139 |
| net-misc/iperf | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 198 |
| net-analyzer/tcpdump | FAIL(3) | OK | FAIL(1) | 59 | 1 | 27 | 3 | 7 | 3 | +4 | 211 |
| net-analyzer/netcat | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 34 |
| net-analyzer/iptraf-ng | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 41 |
| net-irc/weechat | FAIL(3) | aborted | FAIL(1) | 11 | 0 | 3 | 0 | 0 | 0 | = | 43 |
| net-misc/aria2 | OK | OK | OK | 6 | 1 | 7 | 1 | 2 | 1 | +1 | 265 |
| mail-client/alpine | FAIL(3) | FAIL | FAIL(1) | 3 | 1 | 1 | 0 | 0 | 0 | = | 89 |
| mail-client/mutt | OK | OK | OK | 9 | 3 | 10 | 3 | 3 | 3 | = | 352 |
| www-servers/lighttpd | FAIL(3) | FAIL | OK | 10 | 6 | 2 | 6 | 1 | 6 | -5 | 257 |
| sys-process/parallel | OK | OK | OK | 14 | 4 | 15 | 4 | 5 | 4 | +1 | 705 |
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
