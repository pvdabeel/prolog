# tinderbox-ng compare report — 2026-05-10 (ade8318c)

Side-by-side comparison of `portage-ng` and traditional `emerge` over a
1004-package matrix, run through the `tinderbox-ng compare` harness on
`vm-linux.local`. Each comparison runs in two fresh OverlayFS sessions
spawned from the same immutable baseline (stage3 + SWI-Prolog +
portage-ng + matching `kb.qlf`), in parallel, in private mount
namespaces. **Only fresh installs**: every target is a package not
present in the stage3 VDB, so each run goes through the full
`clean → setup → unpack → prepare → configure → compile → install →
merge` chain on both engines.

Driver:   `Source/Application/Wrapper/Linux/tinderbox-ng.d/compare-matrix.sh`
Manifest: `Source/Application/Wrapper/Linux/tinderbox-ng.d/manifest-1000.txt`
Commit:   `ade8318c`

## Headline findings

- **95 portage-ng-only build wins**: emerge fails (plan or build), portage-ng builds the target end-to-end (target lands in VDB).
- **6 portage-ng-only build losses**: emerge builds the target, portage-ng does not (target install step failed, or portage-ng aborted on a sub-dep). These are real bugs and the most actionable item.
- **312 cases where both engines fail** on the same package (or its sub-deps). These are upstream/ebuild issues, not engine bugs.
- **9 portage-ng silent failures**: pipeline exited 0 but the target was never merged into VDB. Hits a known regression of the action:maybe_ci_exit_on_build_failure/1 guard; see the dedicated section below for the package list.
### portage-ng-only build wins

| target | pn_exit | em_exit | pn_vdb | em_vdb |
|---|---|---|---:|---:|
| app-benchmarks/cpuburn | OK | FAIL(1) | 1 | 0 |
| app-containers/lxc | OK | FAIL(1) | 4 | 0 |
| app-dicts/stardict-xdict-zh-en-gb | FAIL(3) | FAIL(1) | 3 | 6 |
| app-dicts/stardict-freedict-eng-ita | FAIL(3) | FAIL(1) | 3 | 6 |
| app-accessibility/emacspeak-ss | FAIL(3) | FAIL(1) | 21 | 27 |
| app-emacs/lsp-treemacs | OK | FAIL(1) | 22 | 0 |
| app-cdr/mirage2iso | FAIL(3) | FAIL(1) | 15 | 39 |
| app-emacs/lsp-java | OK | FAIL(1) | 29 | 0 |
| app-emacs/osm | OK | FAIL(1) | 7 | 0 |
| app-forensics/rkhunter | OK | FAIL(1) | 2 | 0 |
| app-emulation/virtiofsd | FAIL(3) | FAIL(1) | 2 | 3 |
| app-emacs/treesit-auto | OK | FAIL(1) | 7 | 0 |
| app-cdr/burncdda | FAIL(3) | FAIL(1) | 33 | 51 |
| app-i18n/jfbterm | FAIL(3) | FAIL(1) | 12 | 15 |
| app-vim/pydoc | OK | FAIL(1) | 7 | 0 |
| app-shells/posh-z | FAIL(3) | FAIL(1) | 4 | 6 |
| app-xemacs/cc-mode | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/ecb | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/edebug | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/fortran-modes | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/guided-tour | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/lookup | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/mule-base | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/sh-script | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/skk | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/supercite | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/text-modes | FAIL(3) | FAIL(1) | 1 | 2 |
| app-xemacs/view-process | FAIL(3) | FAIL(1) | 1 | 2 |
| app-misc/recoll | FAIL(3) | FAIL(1) | 1 | 2 |
| dev-embedded/arduino | FAIL(3) | FAIL(1) | 24 | 0 |
| app-vim/ant_menu | FAIL(3) | FAIL(1) | 19 | 32 |
| app-crypt/sequoia-chameleon-gnupg | FAIL(124) | FAIL(124) | 14 | 14 |
| dev-haskell/hdbc-postgresql | FAIL(124) | FAIL(1) | 8 | 12 |
| dev-java/aspectj | FAIL(3) | FAIL(1) | 17 | 27 |
| dev-java/eclipse-jdt-annotation | FAIL(3) | FAIL(1) | 16 | 26 |
| dev-java/byte-buddy | FAIL(3) | FAIL(1) | 17 | 29 |
| dev-java/hamcrest-generator | FAIL(3) | FAIL(1) | 17 | 27 |
| dev-java/hashcash | FAIL(3) | FAIL(1) | 16 | 26 |
| dev-java/iso-relax | FAIL(3) | FAIL(1) | 16 | 26 |
| dev-java/jakarta-json-api | FAIL(3) | FAIL(1) | 16 | 26 |
| dev-java/javahelp | FAIL(3) | FAIL(1) | 16 | 26 |
| dev-java/javassist | FAIL(3) | FAIL(1) | 16 | 26 |
| dev-java/pebble | FAIL(3) | FAIL(1) | 17 | 27 |
| dev-java/slf4j-reload4j | FAIL(3) | FAIL(1) | 17 | 27 |
| dev-java/woodstox-core | FAIL(3) | FAIL(1) | 17 | 27 |
| dev-java/jdbc-mysql | FAIL(3) | FAIL(1) | 21 | 31 |
| dev-libs/libunibreak | FAIL(3) | FAIL(1) | 10 | 13 |
| dev-perl/Apache-Reload | OK | FAIL(1) | 11 | 0 |
| dev-ml/fmt | FAIL(3) | FAIL(1) | 11 | 34 |
| dev-ml/uuidm | FAIL(3) | FAIL(1) | 11 | 34 |
| dev-ml/gapi-ocaml | FAIL(3) | FAIL(1) | 7 | 53 |
| dev-perl/Image-Imlib2 | FAIL(3) | FAIL(1) | 8 | 12 |
| dev-python/pyopengl-accelerate | OK | FAIL(1) | 26 | 0 |
| dev-python/requests-credssp | FAIL(3) | FAIL(1) | 6 | 12 |
| dev-util/ROPgadget | OK | FAIL(1) | 2 | 0 |
| games-roguelike/angband | FAIL(3) | FAIL(1) | 9 | 12 |
| games-arcade/retrobattle | OK | FAIL(1) | 18 | 0 |
| dev-vcs/tkcvs | FAIL(3) | FAIL(1) | 19 | 26 |
| mail-mta/postfix | FAIL(3) | FAIL(1) | 4 | 5 |
| media-fonts/font-misc-cyrillic | FAIL(3) | FAIL(1) | 9 | 12 |
| media-libs/zmusic | FAIL(3) | FAIL(1) | 5 | 6 |
| media-plugins/vdr-ffnetdev | FAIL(3) | FAIL(1) | 7 | 14 |
| media-plugins/vdr-satip | FAIL(3) | FAIL(1) | 8 | 15 |
| media-plugins/vdr-svdrpservice | FAIL(3) | FAIL(1) | 7 | 14 |
| media-plugins/libvisual-plugins | FAIL(3) | FAIL(1) | 12 | 20 |
| media-tv/shoutcast2vdr | FAIL(3) | FAIL(1) | 6 | 13 |
| media-tv/dtv-scan-tables | OK | FAIL(1) | 3 | 0 |
| media-sound/sox | FAIL(3) | FAIL(1) | 13 | 16 |
| media-video/ffmpeg | FAIL(3) | FAIL(1) | 11 | 14 |
| net-analyzer/raddump | FAIL(3) | FAIL(1) | 2 | 3 |
| net-analyzer/tcpdump | FAIL(3) | FAIL(1) | 2 | 3 |
| dev-python/vpython | FAIL(124) | FAIL(124) | 106 | 23 |
| media-sound/xineadump | FAIL(3) | FAIL(1) | 13 | 32 |
| net-vpn/openvpn | FAIL(3) | FAIL(1) | 3 | 4 |
| net-wireless/wavemon | OK | FAIL(1) | 2 | 0 |
| media-sound/supercollider | FAIL(3) | FAIL(1) | 24 | 56 |
| sci-libs/aotriton-bin | OK | FAIL(1) | 14 | 0 |
| net-fs/btfs | FAIL(3) | FAIL(1) | 4 | 5 |
| sys-apps/lcdsplash | OK | FAIL(1) | 1 | 0 |
| sys-boot/nettrom | OK | FAIL(1) | 1 | 0 |
| sys-fs/jdiskreport-bin | FAIL(3) | FAIL(1) | 16 | 26 |
| www-apache/modsecurity-crs | OK | FAIL(1) | 8 | 0 |
| sys-process/parallel | OK | FAIL(1) | 5 | 0 |
| www-apps/ttyd | OK | FAIL(1) | 2 | 0 |
| www-apache/mod_musicindex | FAIL(3) | FAIL(1) | 6 | 10 |
| x11-libs/libXft | FAIL(3) | FAIL(1) | 9 | 14 |
| x11-apps/xfd | FAIL(3) | FAIL(1) | 11 | 22 |
| www-apps/drupal | OK | FAIL(1) | 23 | 0 |
| x11-misc/netwmpager | FAIL(3) | FAIL(1) | 9 | 14 |
| sys-fs/google-drive-ocamlfuse | FAIL(3) | FAIL(1) | 9 | 67 |
| x11-misc/picom | FAIL(3) | FAIL(1) | 9 | 19 |
| x11-misc/xsr | FAIL(3) | FAIL(1) | 8 | 16 |
| x11-plugins/wmsysmon | FAIL(3) | FAIL(1) | 9 | 16 |
| x11-terms/xterm | FAIL(3) | FAIL(1) | 19 | 31 |
| sys-block/thin-provisioning-tools | FAIL(124) | FAIL(124) | 24 | 24 |

### portage-ng-only build losses (action items)

| target | pn_target | pn_exit | em_exit | pn_vdb | em_vdb |
|---|---|---|---|---:|---:|
| app-backup/btrbk | FAIL | FAIL(3) | OK | 1 | 19 |
| app-shells/fish | FAIL | FAIL(3) | OK | 1 | 16 |
| dev-libs/cusparselt | FAIL | FAIL(3) | OK | 0 | 1 |
| games-util/xboxdrv | FAIL | FAIL(3) | OK | 2 | 19 |
| sec-keys/openpgp-keys-gentoo-developers | FAIL | FAIL(3) | OK | 1 | 3 |
| sys-fs/btrfs-progs | FAIL | FAIL(3) | OK | 1 | 16 |

## Executive summary

| | Pretend tier | Build tier |
|---|---|---|
| Packages tested | 0 | 1004 |
| portage-ng built target | — | 686 / 1004 (68%) |
| emerge built target | — | 597 / 1004 (59%) |
| portage-ng silent failures (exit 0 / target not built) | — | 9 / 1004 (0%) |
| Identical VDB after build | — | 720 / 1004 (71%) |

## Build tier (full execution)

1004 packages, total wall time 548733s (9145m).

### Status distribution

| portage-ng | emerge | count |
|---|---|---:|
| OK | OK | 591 |
| FAIL(3) | FAIL(1) | 351 |
| OK | FAIL(1) | 21 |
| FAIL(124) | FAIL(1) | 11 |
| FAIL(124) | FAIL(124) | 10 |
| OK(cycles) | FAIL(1) | 9 |
| FAIL(3) | OK | 6 |
| FAIL(3) | FAIL(124) | 5 |

### portage-ng silent failures

Cases where the **portage-ng pipeline exit code stayed 0** but the
**target package itself was not installed**. Either the target's
install step explicitly failed (`pn_target=FAIL`), or portage-ng
never reached the install step for the target (`pn_target=absent`,
typically because a sub-dep failed and the failure didn't propagate
up to the pipeline exit code).

| target | pn_exit | pn_target | em_exit | em_target_built |
|---|---|---|---|---|
| app-doc/doxygen | OK(cycles) | absent | FAIL(1) | no |
| dev-java/log4j | OK(cycles) | absent | FAIL(1) | no |
| dev-util/cmake | OK(cycles) | absent | FAIL(1) | no |
| dev-util/ltrace | OK(cycles) | absent | FAIL(1) | no |
| dev-util/meson | OK(cycles) | absent | FAIL(1) | no |
| dev-util/ninja | OK(cycles) | absent | FAIL(1) | no |
| dev-util/strace | OK(cycles) | absent | FAIL(1) | no |
| sys-apps/at | OK(cycles) | absent | FAIL(1) | no |
| sys-apps/tree | OK(cycles) | absent | FAIL(1) | no |

### Both-failed / one-side-failed

| target | portage-ng exit | portage-ng target | emerge |
|---|---|---|---|
| app-arch/cabextract | FAIL(3) | aborted | FAIL(1) |
| app-arch/engrampa | FAIL(3) | aborted | FAIL(1) |
| app-admin/fam | FAIL(3) | FAIL | FAIL(1) |
| app-arch/p7zip | FAIL(3) | aborted | FAIL(1) |
| app-benchmarks/cpuburn | OK | OK | FAIL(1) |
| app-backup/deja-dup | FAIL(3) | aborted | FAIL(1) |
| app-crypt/hashcat | FAIL(3) | aborted | FAIL(1) |
| app-crypt/asedriveiiie-usb | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-af | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-ast | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-bn | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-da | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-el | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-et | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-it | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-or | FAIL(3) | aborted | FAIL(1) |
| app-dicts/aspell-pt-br | FAIL(3) | aborted | FAIL(1) |
| app-doc/doxygen | OK(cycles) | absent | FAIL(1) |
| app-containers/lxc | OK | OK | FAIL(1) |
| app-dicts/stardict-xdict-zh-en-gb | FAIL(3) | OK | FAIL(1) |
| app-dicts/stardict-freedict-eng-ita | FAIL(3) | OK | FAIL(1) |
| app-backup/btrbk | FAIL(3) | FAIL | OK |
| app-crypt/coolkey | FAIL(3) | FAIL | FAIL(1) |
| app-editors/qhexedit2 | FAIL(3) | aborted | FAIL(1) |
| app-accessibility/emacspeak-ss | FAIL(3) | OK | FAIL(1) |
| app-emacs/lsp-treemacs | OK | OK | FAIL(1) |
| app-cdr/mirage2iso | FAIL(3) | OK | FAIL(1) |
| app-emacs/lsp-java | OK | OK | FAIL(1) |
| app-emulation/virt-manager | FAIL(3) | aborted | FAIL(1) |
| app-emacs/osm | OK | OK | FAIL(1) |
| app-i18n/ibus-anthy | FAIL(3) | aborted | FAIL(1) |
| app-i18n/ibus-table-chinese | FAIL(3) | aborted | FAIL(1) |
| app-forensics/scalpel | FAIL(3) | FAIL | FAIL(1) |
| app-i18n/ibus-table-extraphrase | FAIL(3) | aborted | FAIL(1) |
| app-forensics/rkhunter | OK | OK | FAIL(1) |
| app-emulation/virtiofsd | FAIL(3) | OK | FAIL(1) |
| app-misc/anki | FAIL(3) | aborted | FAIL(1) |
| app-i18n/fcitx-configtool | FAIL(3) | aborted | FAIL(1) |
| app-misc/bb | FAIL(3) | aborted | FAIL(1) |
| app-emacs/treesit-auto | OK | OK | FAIL(1) |
| app-misc/gramps | FAIL(3) | aborted | FAIL(1) |
| app-misc/sphinx | FAIL(3) | FAIL | FAIL(1) |
| app-office/denaro | FAIL(3) | aborted | FAIL(1) |
| app-portage/unsymlink-lib | FAIL(3) | aborted | FAIL(1) |
| app-cdr/burncdda | FAIL(3) | OK | FAIL(1) |
| app-i18n/jfbterm | FAIL(3) | OK | FAIL(1) |
| app-text/vilistextum | FAIL(3) | FAIL | FAIL(1) |
| app-shells/fish | FAIL(3) | FAIL | OK |
| app-vim/pydoc | OK | OK | FAIL(1) |
| app-shells/posh-z | FAIL(3) | OK | FAIL(1) |
| app-xemacs/cc-mode | FAIL(3) | OK | FAIL(1) |
| app-xemacs/ecb | FAIL(3) | OK | FAIL(1) |
| app-xemacs/edebug | FAIL(3) | OK | FAIL(1) |
| app-xemacs/fortran-modes | FAIL(3) | OK | FAIL(1) |
| app-xemacs/guided-tour | FAIL(3) | OK | FAIL(1) |
| app-xemacs/lookup | FAIL(3) | OK | FAIL(1) |
| app-xemacs/mule-base | FAIL(3) | OK | FAIL(1) |
| app-xemacs/sh-script | FAIL(3) | OK | FAIL(1) |
| app-xemacs/skk | FAIL(3) | OK | FAIL(1) |
| app-xemacs/supercite | FAIL(3) | OK | FAIL(1) |
| app-xemacs/text-modes | FAIL(3) | OK | FAIL(1) |
| app-xemacs/view-process | FAIL(3) | OK | FAIL(1) |
| app-misc/recoll | FAIL(3) | OK | FAIL(1) |
| dev-db/libiodbc | FAIL(3) | FAIL | FAIL(1) |
| dev-embedded/avr-libc | FAIL(3) | FAIL | FAIL(1) |
| app-misc/brewtarget | FAIL(3) | aborted | FAIL(1) |
| dev-embedded/avrdude | FAIL(3) | aborted | FAIL(1) |
| dev-embedded/arduino | FAIL(3) | OK | FAIL(1) |
| app-vim/ant_menu | FAIL(3) | OK | FAIL(1) |
| app-crypt/sequoia-chameleon-gnupg | FAIL(124) | OK | FAIL(124) |
| dev-haskell/data-default | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/hspec-wai | FAIL(3) | aborted | FAIL(1) |
| app-text/pandoc-cli | FAIL(3) | aborted | FAIL(124) |
| dev-ada/aunit | FAIL(124) | aborted | FAIL(1) |
| dev-cpp/benchmark | FAIL(124) | aborted | FAIL(124) |
| dev-debug/bpftrace | FAIL(124) | aborted | FAIL(1) |
| dev-haskell/hsyaml | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/html | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/async | FAIL(124) | aborted | FAIL(1) |
| dev-haskell/bencode | FAIL(124) | aborted | FAIL(1) |
| dev-haskell/crypto-cipher-types | FAIL(124) | aborted | FAIL(124) |
| dev-haskell/crypto-random | FAIL(3) | aborted | FAIL(124) |
| dev-haskell/ed25519 | FAIL(124) | aborted | FAIL(1) |
| dev-haskell/exceptions | FAIL(124) | aborted | FAIL(1) |
| dev-haskell/gtk2hs-buildtools | FAIL(124) | aborted | FAIL(124) |
| dev-haskell/hdbc-postgresql | FAIL(124) | OK | FAIL(1) |
| dev-java/aspectj | FAIL(3) | OK | FAIL(1) |
| dev-java/eclipse-jdt-annotation | FAIL(3) | OK | FAIL(1) |
| dev-haskell/hspec-contrib | FAIL(3) | aborted | FAIL(124) |
| dev-java/byte-buddy | FAIL(3) | OK | FAIL(1) |
| dev-haskell/http-api-data | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/lift-type | FAIL(3) | aborted | FAIL(1) |
| dev-java/hamcrest-generator | FAIL(3) | OK | FAIL(1) |
| dev-java/hashcash | FAIL(3) | OK | FAIL(1) |
| dev-java/log4j | OK(cycles) | absent | FAIL(1) |
| dev-java/iso-relax | FAIL(3) | OK | FAIL(1) |
| dev-java/jakarta-json-api | FAIL(3) | OK | FAIL(1) |
| dev-java/javahelp | FAIL(3) | OK | FAIL(1) |
| dev-haskell/magic | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/old-time | FAIL(3) | aborted | FAIL(1) |
| dev-java/javassist | FAIL(3) | OK | FAIL(1) |
| dev-haskell/ordered-containers | FAIL(3) | aborted | FAIL(1) |
| dev-java/pebble | FAIL(3) | OK | FAIL(1) |
| dev-java/slf4j-reload4j | FAIL(3) | OK | FAIL(1) |
| dev-libs/d0_blind_id | FAIL(3) | FAIL | FAIL(1) |
| dev-java/woodstox-core | FAIL(3) | OK | FAIL(1) |
| dev-java/jdbc-mysql | FAIL(3) | OK | FAIL(1) |
| dev-libs/jansson | FAIL(3) | FAIL | FAIL(1) |
| dev-libs/cusparselt | FAIL(3) | FAIL | OK |
| dev-libs/libee | FAIL(3) | aborted | FAIL(1) |
| dev-libs/libgnome-games-support | FAIL(3) | aborted | FAIL(1) |
| dev-libs/librelp | FAIL(3) | FAIL | FAIL(1) |
| dev-haskell/quickcheck-io | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/these | FAIL(3) | aborted | FAIL(1) |
| dev-libs/mimetic | FAIL(3) | FAIL | FAIL(1) |
| dev-libs/m17n-lib | FAIL(3) | FAIL | FAIL(1) |
| dev-libs/libopenrazer | FAIL(3) | aborted | FAIL(1) |
| dev-libs/libstroke | FAIL(3) | FAIL | FAIL(1) |
| dev-libs/libunibreak | FAIL(3) | OK | FAIL(1) |
| dev-libs/xapian-bindings | FAIL(3) | FAIL | FAIL(1) |
| dev-libs/libdbusmenu | FAIL(3) | FAIL | FAIL(1) |
| dev-haskell/yaml | FAIL(3) | aborted | FAIL(1) |
| dev-perl/Apache-Reload | OK | OK | FAIL(1) |
| dev-perl/Cairo | FAIL(3) | aborted | FAIL(1) |
| dev-ml/fmt | FAIL(3) | OK | FAIL(1) |
| dev-libs/starpu | FAIL(3) | aborted | FAIL(1) |
| dev-ml/uuidm | FAIL(3) | OK | FAIL(1) |
| dev-ml/gapi-ocaml | FAIL(3) | OK | FAIL(1) |
| dev-perl/Lab-Zhinst | FAIL(3) | aborted | FAIL(1) |
| dev-perl/Image-Imlib2 | FAIL(3) | OK | FAIL(1) |
| dev-haskell/lifted-base | FAIL(124) | aborted | FAIL(1) |
| dev-perl/PDL-GSL | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/psqueues | FAIL(3) | aborted | FAIL(124) |
| dev-perl/Wx | FAIL(3) | aborted | FAIL(1) |
| dev-haskell/split | FAIL(3) | aborted | FAIL(124) |
| dev-haskell/unix-time | FAIL(124) | aborted | FAIL(1) |
| dev-python/libvirt-python | FAIL(3) | aborted | FAIL(1) |
| dev-python/pulsectl-asyncio | FAIL(3) | aborted | FAIL(1) |
| dev-python/pycairo | FAIL(3) | aborted | FAIL(1) |
| dev-python/pyacoustid | FAIL(3) | aborted | FAIL(1) |
| dev-python/pyopengl-accelerate | OK | OK | FAIL(1) |
| dev-python/wxpython | FAIL(3) | aborted | FAIL(1) |
| dev-python/requests-credssp | FAIL(3) | OK | FAIL(1) |
| dev-ruby/action_text-trix | FAIL(3) | aborted | FAIL(1) |
| dev-python/pygame | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/activeldap | FAIL(3) | aborted | FAIL(1) |
| dev-qt/qtwebsockets | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/afm | FAIL(3) | aborted | FAIL(1) |
| dev-qt/qtconnectivity | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/appraisal | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/aws-partitions | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/bacon | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/binding_of_caller | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/bson | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/certificate_authority | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/concurrent-ruby | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/domain_name | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/crass | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/elasticsearch-transport | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/equatable | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/highline | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/introspection | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/loofah | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/letter_opener | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/mixlib-shellout | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/pcaprub | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/rack-attack | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/rails | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/rbpdf-font | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/rdiscount | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/readline | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/regexp_property_values | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/rouge | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/rspec-collection_matchers | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/ruby_engine | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/semantic_puppet | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/sawyer | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/simplecov | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/simplecov_json_formatter | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/slop | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/stringex | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/systemu | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/test-unit-ruby-core | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/time | FAIL(3) | aborted | FAIL(1) |
| dev-ruby/tty-which | FAIL(3) | aborted | FAIL(1) |
| dev-tex/foiltex | FAIL(3) | aborted | FAIL(1) |
| dev-tex/culmus-latex | FAIL(3) | aborted | FAIL(1) |
| dev-python/python-systemd | FAIL(3) | aborted | FAIL(1) |
| dev-tex/latex2html | FAIL(3) | aborted | FAIL(1) |
| dev-tex/latex2pydata | FAIL(3) | aborted | FAIL(1) |
| dev-texlive/texlive-formatsextra | FAIL(3) | aborted | FAIL(1) |
| dev-texlive/texlive-langitalian | FAIL(3) | aborted | FAIL(1) |
| dev-util/cmake | OK(cycles) | absent | FAIL(1) |
| dev-util/ROPgadget | OK | OK | FAIL(1) |
| dev-util/ltrace | OK(cycles) | absent | FAIL(1) |
| dev-util/meson | OK(cycles) | absent | FAIL(1) |
| dev-util/ninja | OK(cycles) | absent | FAIL(1) |
| dev-util/nsight-systems | FAIL(3) | aborted | FAIL(1) |
| dev-util/strace | OK(cycles) | absent | FAIL(1) |
| games-action/descent2-vertigo | FAIL(3) | FAIL | FAIL(1) |
| games-arcade/commandergenius | FAIL(3) | aborted | FAIL(1) |
| games-arcade/kajaani-kombat | FAIL(3) | aborted | FAIL(1) |
| games-arcade/holotz-castle | FAIL(3) | aborted | FAIL(1) |
| games-arcade/late | FAIL(3) | aborted | FAIL(1) |
| games-arcade/syobon | FAIL(3) | aborted | FAIL(1) |
| games-board/four-in-a-row | FAIL(3) | aborted | FAIL(1) |
| dev-util/qdevicemonitor | FAIL(3) | aborted | FAIL(1) |
| games-board/freedoko | FAIL(3) | aborted | FAIL(1) |
| games-board/pasang-emas | FAIL(3) | aborted | FAIL(1) |
| games-misc/lolcat | FAIL(3) | aborted | FAIL(1) |
| games-puzzle/gnome2048 | FAIL(3) | aborted | FAIL(1) |
| games-puzzle/amoebax | FAIL(3) | aborted | FAIL(1) |
| games-puzzle/gnurobbo | FAIL(3) | aborted | FAIL(1) |
| games-rpg/comi | FAIL(3) | aborted | FAIL(1) |
| games-rpg/crosscode | FAIL(3) | aborted | FAIL(1) |
| games-rpg/crosscode-a-new-home | FAIL(3) | aborted | FAIL(1) |
| games-rpg/lure | FAIL(3) | aborted | FAIL(1) |
| games-util/gamepick | FAIL(3) | aborted | FAIL(1) |
| gui-apps/gnome-console | FAIL(3) | aborted | FAIL(1) |
| gui-apps/wf-recorder | FAIL(3) | aborted | FAIL(1) |
| gui-libs/libadwaita | FAIL(3) | aborted | FAIL(1) |
| games-roguelike/angband | FAIL(3) | OK | FAIL(1) |
| games-arcade/retrobattle | OK | OK | FAIL(1) |
| gui-libs/vte-common | FAIL(3) | aborted | FAIL(1) |
| lxde-base/lxappearance-obconf | FAIL(3) | aborted | FAIL(1) |
| games-sports/speed-dreams | FAIL(3) | aborted | FAIL(1) |
| dev-vcs/tkcvs | FAIL(3) | OK | FAIL(1) |
| mail-client/alpine | FAIL(3) | FAIL | FAIL(1) |
| mail-client/claws-mail | FAIL(3) | aborted | FAIL(1) |
| games-util/xboxdrv | FAIL(3) | FAIL | OK |
| mate-base/mate-applets-meta | FAIL(3) | aborted | FAIL(1) |
| mate-extra/mate-power-manager | FAIL(3) | aborted | FAIL(1) |
| mail-mta/postfix | FAIL(3) | OK | FAIL(1) |
| media-fonts/font-misc-ethiopic | FAIL(3) | aborted | FAIL(1) |
| media-gfx/gnome-screenshot | FAIL(3) | aborted | FAIL(1) |
| media-gfx/ephoto | FAIL(3) | aborted | FAIL(1) |
| mail-filter/gld | FAIL(3) | FAIL | FAIL(1) |
| media-gfx/mandelbulber | FAIL(3) | aborted | FAIL(1) |
| media-gfx/rawtherapee | FAIL(3) | aborted | FAIL(1) |
| media-gfx/ristretto | FAIL(3) | aborted | FAIL(1) |
| media-gfx/prusaslicer | FAIL(3) | aborted | FAIL(1) |
| media-gfx/symboleditor | FAIL(3) | aborted | FAIL(1) |
| media-libs/alsa-oss | FAIL(3) | aborted | FAIL(1) |
| media-libs/exempi | FAIL(3) | FAIL | FAIL(1) |
| media-gfx/svg2rlg | FAIL(3) | aborted | FAIL(1) |
| media-libs/freetype | FAIL(3) | FAIL | FAIL(1) |
| media-libs/id3lib | FAIL(3) | FAIL | FAIL(1) |
| media-fonts/font-misc-cyrillic | FAIL(3) | OK | FAIL(1) |
| media-libs/libvpx | FAIL(3) | aborted | FAIL(1) |
| media-libs/quesoglc | FAIL(3) | aborted | FAIL(1) |
| media-libs/waffle | FAIL(3) | aborted | FAIL(1) |
| media-libs/sdl3-mixer | FAIL(3) | aborted | FAIL(1) |
| media-plugins/gst-plugins-aom | FAIL(3) | aborted | FAIL(1) |
| media-plugins/gst-plugins-sctp | FAIL(3) | aborted | FAIL(1) |
| media-plugins/gst-plugins-soundtouch | FAIL(3) | aborted | FAIL(1) |
| media-plugins/kodi-game-libretro-bnes | FAIL(3) | aborted | FAIL(1) |
| media-plugins/kodi-imagedecoder-raw | FAIL(3) | aborted | FAIL(1) |
| media-plugins/kodi-screensaver-biogenesis | FAIL(3) | aborted | FAIL(1) |
| media-plugins/kodi-screensaver-greynetic | FAIL(3) | aborted | FAIL(1) |
| media-plugins/kodi-screensaver-stars | FAIL(3) | aborted | FAIL(1) |
| media-gfx/libredwg | FAIL(3) | aborted | FAIL(1) |
| media-libs/zmusic | FAIL(3) | OK | FAIL(1) |
| media-plugins/x42-avldrums | FAIL(3) | aborted | FAIL(1) |
| media-sound/amarok | FAIL(3) | aborted | FAIL(1) |
| media-sound/darksnow | FAIL(3) | aborted | FAIL(1) |
| media-sound/deadbeef-mpris2-plugin | FAIL(3) | aborted | FAIL(1) |
| media-sound/gnome-sound-recorder | FAIL(3) | aborted | FAIL(1) |
| media-plugins/vdr-ffnetdev | FAIL(3) | OK | FAIL(1) |
| media-sound/gstreamripper | FAIL(3) | aborted | FAIL(1) |
| media-plugins/vdr-satip | FAIL(3) | OK | FAIL(1) |
| media-plugins/vdr-svdrpservice | FAIL(3) | OK | FAIL(1) |
| media-sound/jack-keyboard | FAIL(3) | aborted | FAIL(1) |
| media-sound/mikmod | FAIL(3) | aborted | FAIL(1) |
| media-sound/quodlibet | FAIL(3) | aborted | FAIL(1) |
| media-sound/pulseeffects | FAIL(3) | aborted | FAIL(1) |
| media-radio/kochmorse | FAIL(3) | aborted | FAIL(1) |
| media-sound/timemachine | FAIL(3) | aborted | FAIL(1) |
| media-plugins/libvisual-projectm | FAIL(3) | aborted | FAIL(1) |
| media-sound/meterbridge | FAIL(3) | aborted | FAIL(1) |
| media-plugins/libvisual-plugins | FAIL(3) | OK | FAIL(1) |
| media-tv/mythtv | FAIL(3) | aborted | FAIL(1) |
| net-analyzer/argus-clients | FAIL(3) | aborted | FAIL(1) |
| media-tv/shoutcast2vdr | FAIL(3) | OK | FAIL(1) |
| media-tv/dtv-scan-tables | OK | OK | FAIL(1) |
| media-video/videotrans | FAIL(3) | aborted | FAIL(1) |
| media-sound/sox | FAIL(3) | OK | FAIL(1) |
| media-video/ffmpeg | FAIL(3) | OK | FAIL(1) |
| net-analyzer/raddump | FAIL(3) | OK | FAIL(1) |
| net-analyzer/tcpdump | FAIL(3) | OK | FAIL(1) |
| net-dialup/sercd | FAIL(3) | FAIL | FAIL(1) |
| dev-python/vpython | FAIL(124) | OK | FAIL(124) |
| net-dns/djbdns | FAIL(3) | FAIL | FAIL(1) |
| net-dns/resolvconf-symlink | FAIL(3) | FAIL | FAIL(1) |
| media-sound/xineadump | FAIL(3) | OK | FAIL(1) |
| net-fs/nfs-utils | FAIL(3) | FAIL | FAIL(1) |
| net-im/fractal | FAIL(3) | aborted | FAIL(1) |
| net-im/rocketchat-desktop-bin | FAIL(3) | aborted | FAIL(1) |
| net-irc/weechat | FAIL(3) | aborted | FAIL(1) |
| net-libs/libblkmaker | FAIL(3) | aborted | FAIL(1) |
| net-irc/ircmap | FAIL(3) | aborted | FAIL(1) |
| net-libs/libetpan | FAIL(3) | FAIL | FAIL(1) |
| net-libs/libcrafter | FAIL(3) | FAIL | FAIL(1) |
| net-libs/msgraph | FAIL(3) | aborted | FAIL(1) |
| net-im/libcommuni | FAIL(3) | aborted | FAIL(1) |
| net-mail/mailgraph | FAIL(3) | aborted | FAIL(1) |
| net-irc/kvirc | FAIL(3) | aborted | FAIL(1) |
| dev-scheme/guile-colorized | FAIL(124) | aborted | FAIL(124) |
| dev-scheme/guile-libyaml | FAIL(124) | aborted | FAIL(124) |
| dev-scheme/guile-lzlib | FAIL(124) | aborted | FAIL(124) |
| net-mail/vpopmail | FAIL(3) | aborted | FAIL(1) |
| net-misc/connman-ui | FAIL(3) | aborted | FAIL(1) |
| net-misc/oidc-agent | FAIL(3) | aborted | FAIL(1) |
| net-misc/putty | FAIL(3) | aborted | FAIL(1) |
| net-misc/openntpd | FAIL(3) | FAIL | FAIL(1) |
| net-vpn/openvpn | FAIL(3) | OK | FAIL(1) |
| sci-geosciences/gpsbabel | FAIL(3) | aborted | FAIL(1) |
| net-libs/kdsoap | FAIL(3) | aborted | FAIL(1) |
| net-wireless/gr-scopy | FAIL(3) | aborted | FAIL(1) |
| net-wireless/wavemon | OK | OK | FAIL(1) |
| sci-astronomy/scamp | FAIL(3) | aborted | FAIL(1) |
| sci-libs/gts | FAIL(3) | FAIL | FAIL(1) |
| sci-libs/libnova | FAIL(3) | FAIL | FAIL(1) |
| sci-libs/ta-lib | FAIL(3) | FAIL | FAIL(1) |
| sci-geosciences/gpxsee | FAIL(3) | aborted | FAIL(1) |
| sci-visualization/ggobi | FAIL(3) | aborted | FAIL(1) |
| sci-libs/jkqtplotter | FAIL(3) | aborted | FAIL(1) |
| media-sound/supercollider | FAIL(3) | OK | FAIL(1) |
| sci-visualization/quickplot | FAIL(3) | aborted | FAIL(1) |
| sec-keys/openpgp-keys-gentoo-developers | FAIL(3) | FAIL | OK |
| sci-libs/aotriton-bin | OK | OK | FAIL(1) |
| sci-visualization/kst | FAIL(3) | aborted | FAIL(1) |
| sci-libs/linux-gpib-modules | FAIL(3) | FAIL | FAIL(1) |
| sec-policy/selinux-calamaris | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-courier | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-cyphesis | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-finger | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-docker | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-loadkeys | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-matrixd | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-mozilla | FAIL(3) | aborted | FAIL(1) |
| sys-apps/at | OK(cycles) | absent | FAIL(1) |
| sec-policy/selinux-oddjob | FAIL(3) | aborted | FAIL(1) |
| net-fs/btfs | FAIL(3) | OK | FAIL(1) |
| sec-policy/selinux-pcscd | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-qmail | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-rpcbind | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-sasl | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-rpm | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-ulogd | FAIL(3) | aborted | FAIL(1) |
| sec-policy/selinux-vde | FAIL(3) | aborted | FAIL(1) |
| sys-apps/lcdsplash | OK | OK | FAIL(1) |
| sys-apps/kcheck | FAIL(3) | FAIL | FAIL(1) |
| sys-apps/tree | OK(cycles) | absent | FAIL(1) |
| sys-auth/microsoft-identity-broker | FAIL(3) | aborted | FAIL(1) |
| sys-block/sas3flash | FAIL(3) | aborted | FAIL(1) |
| sys-block/sas3ircu | FAIL(3) | aborted | FAIL(1) |
| sys-boot/nettrom | OK | OK | FAIL(1) |
| sys-auth/sssd | FAIL(3) | aborted | FAIL(1) |
| gnustep-apps/zipper | FAIL(124) | aborted | FAIL(1) |
| gnustep-base/gnustep-updater | FAIL(124) | aborted | FAIL(124) |
| sys-fs/dislocker | FAIL(3) | aborted | FAIL(1) |
| llvm-runtimes/libgcc | FAIL(124) | aborted | FAIL(1) |
| sys-fs/iprutils | FAIL(3) | FAIL | FAIL(1) |
| sys-fs/mergerfs | FAIL(3) | FAIL | FAIL(1) |
| sys-fs/lufis | FAIL(3) | aborted | FAIL(1) |
| sys-fs/lufs | FAIL(3) | aborted | FAIL(1) |
| sys-libs/gwenhywfar | FAIL(3) | FAIL | FAIL(1) |
| sys-fs/btrfs-progs | FAIL(3) | FAIL | OK |
| sys-process/atop | FAIL(3) | aborted | FAIL(1) |
| sys-power/sandmann-bin | FAIL(3) | aborted | FAIL(1) |
| www-apps/nanoc-spec | FAIL(3) | aborted | FAIL(1) |
| sys-block/seekwatcher | FAIL(3) | aborted | FAIL(1) |
| www-apps/sfpg | FAIL(3) | aborted | FAIL(1) |
| www-apps/jekyll-sitemap | FAIL(3) | aborted | FAIL(1) |
| www-client/fetch | FAIL(3) | aborted | FAIL(1) |
| www-client/httrack | FAIL(3) | FAIL | FAIL(1) |
| www-apps/ikiwiki | FAIL(3) | aborted | FAIL(1) |
| sys-fs/jdiskreport-bin | FAIL(3) | OK | FAIL(1) |
| www-apache/modsecurity-crs | OK | OK | FAIL(1) |
| sys-process/parallel | OK | OK | FAIL(1) |
| www-apps/ttyd | OK | OK | FAIL(1) |
| www-apache/mod_musicindex | FAIL(3) | OK | FAIL(1) |
| x11-libs/libQGLViewer | FAIL(3) | aborted | FAIL(1) |
| www-apps/mythweb | FAIL(3) | aborted | FAIL(1) |
| net-voip/murmur | FAIL(3) | aborted | FAIL(1) |
| x11-libs/libXft | FAIL(3) | OK | FAIL(1) |
| x11-misc/bumblebee | FAIL(3) | aborted | FAIL(1) |
| x11-misc/grub2-theme-preview | FAIL(3) | aborted | FAIL(1) |
| x11-misc/idesk-extras | FAIL(3) | aborted | FAIL(1) |
| x11-misc/kronometer | FAIL(3) | aborted | FAIL(1) |
| x11-misc/lightdm | FAIL(3) | aborted | FAIL(1) |
| x11-misc/nitrogen | FAIL(3) | aborted | FAIL(1) |
| x11-apps/xfd | FAIL(3) | OK | FAIL(1) |
| x11-misc/screen-message | FAIL(3) | aborted | FAIL(1) |
| x11-misc/snixembed | FAIL(3) | aborted | FAIL(1) |
| x11-misc/xdaliclock | FAIL(3) | aborted | FAIL(1) |
| x11-plugins/wmMatrix | FAIL(3) | aborted | FAIL(1) |
| x11-misc/obmenu-generator | FAIL(3) | aborted | FAIL(1) |
| www-apps/drupal | OK | OK | FAIL(1) |
| x11-misc/netwmpager | FAIL(3) | OK | FAIL(1) |
| x11-libs/xbae | FAIL(3) | aborted | FAIL(1) |
| x11-themes/greybird | FAIL(3) | aborted | FAIL(1) |
| x11-themes/human-icon-theme | FAIL(3) | aborted | FAIL(1) |
| sys-fs/google-drive-ocamlfuse | FAIL(3) | OK | FAIL(1) |
| xfce-extra/xfce4-fsguard-plugin | FAIL(3) | aborted | FAIL(1) |
| x11-misc/picom | FAIL(3) | OK | FAIL(1) |
| x11-misc/xsr | FAIL(3) | OK | FAIL(1) |
| x11-wm/i3 | FAIL(3) | aborted | FAIL(1) |
| x11-plugins/wmXName | FAIL(3) | FAIL | FAIL(1) |
| x11-wm/windowmaker | FAIL(3) | aborted | FAIL(1) |
| x11-plugins/wmsysmon | FAIL(3) | OK | FAIL(1) |
| x11-terms/xterm | FAIL(3) | OK | FAIL(1) |
| sys-block/thin-provisioning-tools | FAIL(124) | OK | FAIL(124) |

### VDB-count deltas (portage-ng vs emerge)

| target | portage-ng VDB | emerge VDB | delta |
|---|---:|---:|---:|
| app-arch/engrampa | 1 | 0 | +1 |
| app-benchmarks/cpuburn | 1 | 0 | +1 |
| app-backup/deja-dup | 3 | 0 | +3 |
| app-containers/lxc | 4 | 0 | +4 |
| app-dicts/stardict-xdict-zh-en-gb | 3 | 6 | -3 |
| app-dicts/stardict-freedict-eng-ita | 3 | 6 | -3 |
| app-backup/btrbk | 1 | 19 | -18 |
| app-editors/qhexedit2 | 26 | 0 | +26 |
| app-accessibility/emacspeak-ss | 21 | 27 | -6 |
| app-emacs/lsp-treemacs | 22 | 0 | +22 |
| app-cdr/mirage2iso | 15 | 39 | -24 |
| app-emacs/lsp-java | 29 | 0 | +29 |
| app-emulation/virt-manager | 5 | 0 | +5 |
| app-emacs/osm | 7 | 0 | +7 |
| app-i18n/ibus-anthy | 2 | 0 | +2 |
| app-i18n/ibus-table-chinese | 2 | 0 | +2 |
| app-i18n/ibus-table-extraphrase | 2 | 0 | +2 |
| app-forensics/rkhunter | 2 | 0 | +2 |
| app-emulation/virtiofsd | 2 | 3 | -1 |
| app-misc/anki | 2 | 0 | +2 |
| app-i18n/fcitx-configtool | 27 | 0 | +27 |
| app-emacs/treesit-auto | 7 | 0 | +7 |
| app-misc/gramps | 2 | 0 | +2 |
| app-office/denaro | 2 | 0 | +2 |
| app-cdr/burncdda | 33 | 51 | -18 |
| app-i18n/jfbterm | 12 | 15 | -3 |
| app-shells/fish | 1 | 16 | -15 |
| app-vim/pydoc | 7 | 0 | +7 |
| app-shells/posh-z | 4 | 6 | -2 |
| app-xemacs/cc-mode | 1 | 2 | -1 |
| app-xemacs/ecb | 1 | 2 | -1 |
| app-xemacs/edebug | 1 | 2 | -1 |
| app-xemacs/fortran-modes | 1 | 2 | -1 |
| app-xemacs/guided-tour | 1 | 2 | -1 |
| app-xemacs/lookup | 1 | 2 | -1 |
| app-xemacs/mule-base | 1 | 2 | -1 |
| app-xemacs/sh-script | 1 | 2 | -1 |
| app-xemacs/skk | 1 | 2 | -1 |
| app-xemacs/supercite | 1 | 2 | -1 |
| app-xemacs/text-modes | 1 | 2 | -1 |
| app-xemacs/view-process | 1 | 2 | -1 |
| app-misc/recoll | 1 | 2 | -1 |
| dev-embedded/avr-libc | 2 | 0 | +2 |
| app-misc/brewtarget | 33 | 0 | +33 |
| dev-embedded/arduino | 24 | 0 | +24 |
| app-vim/ant_menu | 19 | 32 | -13 |
| dev-haskell/data-default | 0 | 1 | -1 |
| dev-haskell/hspec-wai | 0 | 1 | -1 |
| app-text/pandoc-cli | 0 | 1 | -1 |
| dev-ada/aunit | 2 | 0 | +2 |
| dev-debug/bpftrace | 22 | 0 | +22 |
| dev-haskell/hsyaml | 0 | 1 | -1 |
| dev-haskell/html | 0 | 1 | -1 |
| dev-haskell/async | 0 | 1 | -1 |
| dev-haskell/bencode | 0 | 1 | -1 |
| dev-haskell/crypto-cipher-types | 0 | 1 | -1 |
| dev-haskell/crypto-random | 0 | 1 | -1 |
| dev-haskell/ed25519 | 0 | 1 | -1 |
| dev-haskell/exceptions | 0 | 1 | -1 |
| dev-haskell/gtk2hs-buildtools | 0 | 1 | -1 |
| dev-haskell/hdbc-postgresql | 8 | 12 | -4 |
| dev-java/aspectj | 17 | 27 | -10 |
| dev-java/eclipse-jdt-annotation | 16 | 26 | -10 |
| dev-haskell/hspec-contrib | 0 | 1 | -1 |
| dev-java/byte-buddy | 17 | 29 | -12 |
| dev-haskell/http-api-data | 0 | 1 | -1 |
| dev-haskell/lift-type | 0 | 1 | -1 |
| dev-java/hamcrest-generator | 17 | 27 | -10 |
| dev-java/hashcash | 16 | 26 | -10 |
| dev-java/iso-relax | 16 | 26 | -10 |
| dev-java/jakarta-json-api | 16 | 26 | -10 |
| dev-java/javahelp | 16 | 26 | -10 |
| dev-haskell/magic | 0 | 1 | -1 |
| dev-haskell/old-time | 0 | 1 | -1 |
| dev-java/javassist | 16 | 26 | -10 |
| dev-haskell/ordered-containers | 0 | 1 | -1 |
| dev-java/pebble | 17 | 27 | -10 |
| dev-java/slf4j-reload4j | 17 | 27 | -10 |
| dev-java/woodstox-core | 17 | 27 | -10 |
| dev-java/jdbc-mysql | 21 | 31 | -10 |
| dev-libs/cusparselt | 0 | 1 | -1 |
| dev-libs/libgnome-games-support | 2 | 0 | +2 |
| dev-haskell/quickcheck-io | 0 | 1 | -1 |
| dev-haskell/these | 0 | 1 | -1 |
| dev-libs/libopenrazer | 25 | 0 | +25 |
| dev-libs/libunibreak | 10 | 13 | -3 |
| dev-libs/xapian-bindings | 2 | 0 | +2 |
| dev-haskell/yaml | 0 | 1 | -1 |
| dev-perl/Apache-Reload | 11 | 0 | +11 |
| dev-ml/fmt | 11 | 34 | -23 |
| dev-ml/uuidm | 11 | 34 | -23 |
| dev-ml/gapi-ocaml | 7 | 53 | -46 |
| dev-perl/Image-Imlib2 | 8 | 12 | -4 |
| dev-haskell/lifted-base | 0 | 1 | -1 |
| dev-perl/PDL-GSL | 48 | 0 | +48 |
| dev-haskell/psqueues | 0 | 1 | -1 |
| dev-perl/Wx | 1 | 0 | +1 |
| dev-haskell/split | 0 | 1 | -1 |
| dev-haskell/unix-time | 0 | 1 | -1 |
| dev-python/libvirt-python | 5 | 0 | +5 |
| dev-python/pulsectl-asyncio | 1 | 0 | +1 |
| dev-python/pyacoustid | 18 | 0 | +18 |
| dev-python/pyopengl-accelerate | 26 | 0 | +26 |
| dev-python/wxpython | 2 | 0 | +2 |
| dev-python/requests-credssp | 6 | 12 | -6 |
| dev-python/pygame | 23 | 0 | +23 |
| dev-qt/qtwebsockets | 25 | 0 | +25 |
| dev-qt/qtconnectivity | 27 | 0 | +27 |
| dev-tex/foiltex | 27 | 0 | +27 |
| dev-tex/culmus-latex | 29 | 0 | +29 |
| dev-python/python-systemd | 28 | 0 | +28 |
| dev-tex/latex2html | 33 | 0 | +33 |
| dev-tex/latex2pydata | 39 | 0 | +39 |
| dev-texlive/texlive-formatsextra | 27 | 0 | +27 |
| dev-texlive/texlive-langitalian | 27 | 0 | +27 |
| dev-util/ROPgadget | 2 | 0 | +2 |
| dev-util/nsight-systems | 2 | 0 | +2 |
| dev-util/ccache | 4 | 1 | +3 |
| games-arcade/kajaani-kombat | 4 | 0 | +4 |
| games-arcade/holotz-castle | 8 | 0 | +8 |
| games-arcade/late | 3 | 0 | +3 |
| games-arcade/syobon | 4 | 0 | +4 |
| games-board/four-in-a-row | 1 | 0 | +1 |
| dev-util/qdevicemonitor | 38 | 0 | +38 |
| games-board/freedoko | 1 | 0 | +1 |
| games-board/pasang-emas | 1 | 0 | +1 |
| games-puzzle/gnome2048 | 2 | 0 | +2 |
| games-puzzle/amoebax | 4 | 0 | +4 |
| games-puzzle/gnurobbo | 4 | 0 | +4 |
| games-rpg/comi | 1 | 0 | +1 |
| games-rpg/crosscode | 3 | 0 | +3 |
| games-rpg/crosscode-a-new-home | 3 | 0 | +3 |
| games-rpg/lure | 1 | 0 | +1 |
| games-util/gamepick | 1 | 0 | +1 |
| gui-apps/gnome-console | 2 | 0 | +2 |
| gui-apps/wf-recorder | 1 | 0 | +1 |
| gui-libs/libadwaita | 2 | 0 | +2 |
| games-roguelike/angband | 9 | 12 | -3 |
| games-arcade/retrobattle | 18 | 0 | +18 |
| gui-libs/vte-common | 2 | 0 | +2 |
| lxde-base/lxappearance-obconf | 1 | 0 | +1 |
| games-sports/speed-dreams | 19 | 0 | +19 |
| gui-libs/neatvnc | 4 | 6 | -2 |
| dev-vcs/tkcvs | 19 | 26 | -7 |
| mail-client/claws-mail | 1 | 0 | +1 |
| games-util/xboxdrv | 2 | 19 | -17 |
| mate-base/mate-applets-meta | 2 | 0 | +2 |
| mate-extra/mate-power-manager | 2 | 0 | +2 |
| mail-mta/postfix | 4 | 5 | -1 |
| media-gfx/gnome-screenshot | 1 | 0 | +1 |
| media-gfx/ephoto | 2 | 0 | +2 |
| media-gfx/mandelbulber | 1 | 0 | +1 |
| media-gfx/rawtherapee | 1 | 0 | +1 |
| media-gfx/ristretto | 1 | 0 | +1 |
| media-gfx/prusaslicer | 2 | 0 | +2 |
| media-gfx/symboleditor | 4 | 0 | +4 |
| media-gfx/svg2rlg | 9 | 0 | +9 |
| media-fonts/font-misc-cyrillic | 9 | 12 | -3 |
| media-libs/quesoglc | 10 | 0 | +10 |
| media-libs/waffle | 1 | 0 | +1 |
| media-plugins/gst-plugins-aom | 1 | 0 | +1 |
| media-plugins/gst-plugins-sctp | 1 | 0 | +1 |
| media-plugins/gst-plugins-soundtouch | 1 | 0 | +1 |
| media-plugins/kodi-game-libretro-bnes | 1 | 0 | +1 |
| media-plugins/kodi-imagedecoder-raw | 1 | 0 | +1 |
| media-plugins/kodi-screensaver-biogenesis | 1 | 0 | +1 |
| media-plugins/kodi-screensaver-greynetic | 1 | 0 | +1 |
| media-plugins/kodi-screensaver-stars | 1 | 0 | +1 |
| media-libs/zmusic | 5 | 6 | -1 |
| media-sound/amarok | 4 | 0 | +4 |
| media-sound/darksnow | 1 | 0 | +1 |
| media-sound/deadbeef-mpris2-plugin | 1 | 0 | +1 |
| media-sound/gnome-sound-recorder | 3 | 0 | +3 |
| media-plugins/vdr-ffnetdev | 7 | 14 | -7 |
| media-sound/gstreamripper | 1 | 0 | +1 |
| media-plugins/vdr-satip | 8 | 15 | -7 |
| media-plugins/vdr-svdrpservice | 7 | 14 | -7 |
| media-sound/jack-keyboard | 2 | 0 | +2 |
| media-sound/quodlibet | 1 | 0 | +1 |
| media-sound/pulseeffects | 3 | 0 | +3 |
| media-radio/kochmorse | 27 | 0 | +27 |
| media-sound/timemachine | 2 | 0 | +2 |
| media-plugins/libvisual-projectm | 15 | 0 | +15 |
| media-sound/meterbridge | 12 | 0 | +12 |
| media-plugins/libvisual-plugins | 12 | 20 | -8 |
| media-tv/mythtv | 30 | 0 | +30 |
| net-analyzer/argus-clients | 1 | 0 | +1 |
| media-tv/shoutcast2vdr | 6 | 13 | -7 |
| media-tv/dtv-scan-tables | 3 | 0 | +3 |
| media-video/videotrans | 17 | 0 | +17 |
| media-sound/sox | 13 | 16 | -3 |
| media-video/ffmpeg | 11 | 14 | -3 |
| net-analyzer/raddump | 2 | 3 | -1 |
| net-analyzer/tcpdump | 2 | 3 | -1 |
| dev-python/vpython | 106 | 23 | +83 |
| media-sound/xineadump | 13 | 32 | -19 |
| net-im/fractal | 4 | 0 | +4 |
| net-im/rocketchat-desktop-bin | 3 | 0 | +3 |
| net-irc/ircmap | 10 | 0 | +10 |
| net-libs/msgraph | 2 | 0 | +2 |
| net-im/libcommuni | 26 | 0 | +26 |
| net-mail/mailgraph | 13 | 0 | +13 |
| net-irc/kvirc | 27 | 0 | +27 |
| net-misc/connman-ui | 1 | 0 | +1 |
| net-misc/oidc-agent | 2 | 0 | +2 |
| net-misc/putty | 1 | 0 | +1 |
| net-vpn/openvpn | 3 | 4 | -1 |
| sci-geosciences/gpsbabel | 1 | 0 | +1 |
| net-libs/kdsoap | 27 | 0 | +27 |
| net-wireless/gr-scopy | 41 | 0 | +41 |
| net-wireless/wavemon | 2 | 0 | +2 |
| sci-astronomy/scamp | 2 | 0 | +2 |
| sci-geosciences/gpxsee | 21 | 0 | +21 |
| sci-visualization/ggobi | 1 | 0 | +1 |
| sci-libs/jkqtplotter | 39 | 0 | +39 |
| media-sound/supercollider | 24 | 56 | -32 |
| sci-visualization/quickplot | 1 | 0 | +1 |
| sec-keys/openpgp-keys-gentoo-developers | 1 | 3 | -2 |
| sci-libs/aotriton-bin | 14 | 0 | +14 |
| sci-visualization/kst | 25 | 0 | +25 |
| sec-policy/selinux-calamaris | 16 | 0 | +16 |
| sec-policy/selinux-courier | 16 | 0 | +16 |
| sec-policy/selinux-cyphesis | 16 | 0 | +16 |
| sec-policy/selinux-finger | 16 | 0 | +16 |
| sec-policy/selinux-docker | 16 | 0 | +16 |
| sec-policy/selinux-loadkeys | 16 | 0 | +16 |
| sec-policy/selinux-matrixd | 16 | 0 | +16 |
| sec-policy/selinux-mozilla | 16 | 0 | +16 |
| sec-policy/selinux-oddjob | 16 | 0 | +16 |
| net-fs/btfs | 4 | 5 | -1 |
| sec-policy/selinux-pcscd | 16 | 0 | +16 |
| sec-policy/selinux-qmail | 16 | 0 | +16 |
| sec-policy/selinux-rpcbind | 16 | 0 | +16 |
| sec-policy/selinux-sasl | 16 | 0 | +16 |
| sec-policy/selinux-rpm | 16 | 0 | +16 |
| sec-policy/selinux-ulogd | 16 | 0 | +16 |
| sec-policy/selinux-vde | 16 | 0 | +16 |
| sys-apps/lcdsplash | 1 | 0 | +1 |
| sys-apps/kcheck | 1 | 0 | +1 |
| sys-auth/microsoft-identity-broker | 2 | 0 | +2 |
| sys-boot/nettrom | 1 | 0 | +1 |
| sys-auth/sssd | 18 | 0 | +18 |
| gnustep-apps/zipper | 10 | 0 | +10 |
| gnustep-base/gnustep-updater | 0 | 7 | -7 |
| llvm-runtimes/libgcc | 8 | 0 | +8 |
| sys-fs/iprutils | 5 | 0 | +5 |
| sys-fs/btrfs-progs | 1 | 16 | -15 |
| sys-power/sandmann-bin | 50 | 0 | +50 |
| sys-block/seekwatcher | 22 | 0 | +22 |
| www-apps/sfpg | 2 | 0 | +2 |
| www-apps/ikiwiki | 24 | 0 | +24 |
| sys-fs/jdiskreport-bin | 16 | 26 | -10 |
| www-apache/modsecurity-crs | 8 | 0 | +8 |
| sys-process/parallel | 5 | 0 | +5 |
| www-apps/ttyd | 2 | 0 | +2 |
| www-apache/mod_musicindex | 6 | 10 | -4 |
| x11-libs/libQGLViewer | 19 | 0 | +19 |
| www-apps/mythweb | 29 | 0 | +29 |
| net-voip/murmur | 40 | 0 | +40 |
| x11-libs/libXft | 9 | 14 | -5 |
| x11-misc/bumblebee | 5 | 0 | +5 |
| x11-misc/idesk-extras | 1 | 0 | +1 |
| x11-misc/kronometer | 3 | 0 | +3 |
| x11-misc/lightdm | 3 | 0 | +3 |
| x11-misc/nitrogen | 1 | 0 | +1 |
| x11-apps/xfd | 11 | 22 | -11 |
| x11-misc/screen-message | 1 | 0 | +1 |
| x11-misc/snixembed | 1 | 0 | +1 |
| x11-misc/xdaliclock | 1 | 0 | +1 |
| x11-plugins/wmMatrix | 2 | 0 | +2 |
| x11-misc/obmenu-generator | 17 | 0 | +17 |
| www-apps/drupal | 23 | 0 | +23 |
| x11-misc/netwmpager | 9 | 14 | -5 |
| x11-themes/greybird | 1 | 0 | +1 |
| x11-themes/human-icon-theme | 1 | 0 | +1 |
| sys-fs/google-drive-ocamlfuse | 9 | 67 | -58 |
| xfce-extra/xfce4-fsguard-plugin | 1 | 0 | +1 |
| x11-misc/picom | 9 | 19 | -10 |
| x11-misc/xsr | 8 | 16 | -8 |
| x11-wm/i3 | 14 | 0 | +14 |
| x11-plugins/wmXName | 15 | 0 | +15 |
| x11-wm/windowmaker | 11 | 0 | +11 |
| x11-plugins/wmsysmon | 9 | 16 | -7 |
| x11-terms/xterm | 19 | 31 | -12 |

A non-zero delta means the two engines installed a different
set of packages for the same target. Causes range from version
selection (revision pick) to differences in build-vs-runtime
dep handling.

### Per-package detail (truncated)

| target | pn_exit | pn_target | em_exit | pn_actions | em_actions | pn_completed | em_completed | pn_vdb | em_vdb | vdb_delta | seconds |
|---|---|---|---|---|---|---|---|---|---|---|---|
| app-arch/cabextract | FAIL(3) | aborted | FAIL(1) | 6 | 0 | 2 | 0 | 0 | 0 | = | 45 |
| app-admin/genromfs | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 52 |
| app-admin/verynice | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 55 |
| app-arch/engrampa | FAIL(3) | aborted | FAIL(1) | 189 | ? | 65 | 0 | 1 | 0 | +1 | 55 |
| app-admin/ranpwd | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 62 |
| app-admin/pwcrypt | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 64 |
| app-admin/tmpwatch | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 67 |
| app-arch/lz4 | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 67 |
| app-admin/fam | FAIL(3) | FAIL | FAIL(1) | 6 | 2 | 4 | 1 | 1 | 1 | = | 87 |
| app-arch/p7zip | FAIL(3) | aborted | FAIL(1) | 5 | 0 | 2 | 0 | 0 | 0 | = | 42 |
| app-arch/mscompress | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 56 |
| app-arch/pxz | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 45 |
| app-arch/zip | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 49 |
| app-arch/lzip | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 79 |
| app-arch/lz5 | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 140 |
| app-benchmarks/cpuburn | OK | OK | FAIL(1) | 4 | ? | 4 | 0 | 1 | 0 | +1 | 51 |
| app-arch/par2cmdline | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 108 |
| app-backup/deja-dup | FAIL(3) | aborted | FAIL(1) | 377 | ? | 126 | 0 | 3 | 0 | +3 | 74 |
| app-containers/containers-shortnames | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 57 |
| app-admin/logrotate | OK | OK | OK | 20 | 8 | 21 | 8 | 8 | 8 | = | 359 |
| app-arch/pax | OK | OK | OK | 9 | 3 | 10 | 3 | 3 | 3 | = | 316 |
| app-crypt/hashcat | FAIL(3) | aborted | FAIL(1) | 30 | ? | 9 | 0 | 0 | 0 | = | 65 |
| app-arch/gxz | OK | OK | OK | 7 | 3 | 8 | 3 | 3 | 3 | = | 502 |
| app-crypt/aespipe | OK | OK | OK | 6 | 2 | 7 | 2 | 2 | 2 | = | 343 |
| app-crypt/asedriveiiie-usb | FAIL(3) | aborted | FAIL(1) | 24 | 0 | 18 | 7 | 7 | 7 | = | 361 |
| app-crypt/pius | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 64 |
| app-arch/unrar | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 522 |
| app-crypt/ubuntu-keyring | OK | OK | OK | 3 | 1 | 4 | 1 | 1 | 1 | = | 56 |
| app-admin/sudo | OK | OK | OK | 17 | 7 | 18 | 7 | 7 | 7 | = | 606 |
| app-dicts/aspell-af | FAIL(3) | aborted | FAIL(1) | 9 | 0 | 3 | 0 | 0 | 0 | = | 43 |
| ... (truncated; 974 more rows in TSV) | | | | | | | | | | | |

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

---

## Follow-up: Class F + Class G rerun of the 6 PN-only losses (post-matrix)

Re-ran just the six action-item rows above against `tinderbox-ng compare
--build` with the post-matrix master HEAD (Class F USE_EXPAND-aware
any_of_group ranking + Class G race-safe distfile staging + USE-conditional
SRC_URI traversal). Results dir:
`Reports/tinderbox-compare-2026-05-10-ade8318c.d/rerun-classfg.tsv`
(per-package logs under `rerun-classfg-logs/`).

| target                                  | matrix pn | matrix plan | rerun pn | rerun plan | em plan | resolved? |
|---|---|---:|---|---:|---:|---|
| dev-libs/cusparselt                     | FAIL(3)   | 3   | **OK**     | 3  | 1  | YES — Class G |
| sys-fs/btrfs-progs                      | FAIL(3)   | 73  | **OK**     | 47 | 16 | YES — Class F shrunk plan, build OK |
| app-backup/btrbk                        | FAIL(3)   | 81  | **OK**     | 55 | 19 | YES — Class F shrunk plan, build OK |
| sec-keys/openpgp-keys-gentoo-developers | FAIL(3)   | 12  | FAIL(3)    | 2  | 3  | NO — plan now 2; build-step regression on `reinstall` |
| app-shells/fish                         | FAIL(3)   | 71  | FAIL(3)    | 39 | 16 | NO — plan halved; build-step still fails |
| games-util/xboxdrv                      | FAIL(3)   | 54  | FAIL(3)    | 13 | 19 | NO — plan now SMALLER than emerge; build-step fails |

**Net**: 3 of 6 PN-only losses resolved by Class F + G. The remaining 3 are no
longer planning regressions; they have moved into a new bucket of build-step
regressions under shrunken plans (most likely PN omitting a sub-dep that
emerge correctly identifies, or trying to `reinstall` a stage3-provided
package that fails the install phase). Open follow-up.
