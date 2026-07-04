# Building and Execution

portage-ng is a self-contained dependency resolver and planner.  It
ships its own code for every stage up to the point where source code
must actually be compiled:

- **Cache generation** — portage-ng includes its own md5-cache
  generator, so it does not depend on Portage's `egencache` or any
  other external tool to produce the cache files it reasons over.
- **Dependency resolution and planning** — the prover, planner, and
  scheduler are entirely internal (see Chapters 8-12).
- **Downloading** — source archive fetching, mirror selection, hash
  verification, and resume are handled by portage-ng's own download
  module (see [Download management](#download-management) below).

The only point where Portage is needed is the **execution of ebuild
build phases** (unpack, compile, install, qmerge, etc.).  These phases
rely on Portage's `ebuild` command and its ecosystem of eclasses and
phase functions.  portage-ng delegates to that infrastructure so that
the full ebuild ecosystem works unchanged, but everything before and
after the build steps — dependency calculation, plan ordering,
downloading, and output — is handled independently.


## Build delegation

When executing a plan (via `--merge` rather than `--pretend`), the
builder module invokes the `ebuild` command for each action in the
plan.  The command is configurable via `config:ebuild_command/1`
(default: `ebuild`).

The builder processes the plan wave by wave, respecting the
parallelism computed by the planner.  Within each wave, independent
actions can run concurrently.


## Ebuild phase execution

The `ebuild_exec.pl` module handles the actual invocation of ebuild
phases:

| **Phase** | **ebuild command** | **When** |
| :--- | :--- | :--- |
| `setup` | `ebuild <path> setup` | Before building |
| `unpack` | `ebuild <path> unpack` | Extract source archives |
| `prepare` | `ebuild <path> prepare` | Apply patches |
| `configure` | `ebuild <path> configure` | Run configure scripts |
| `compile` | `ebuild <path> compile` | Build from source |
| `install` | `ebuild <path> install` | Install to staging area |
| `qmerge` | `ebuild <path> qmerge` | Merge to live filesystem |

Phases are executed via `process_create` with output captured for logging.
The builder uses `sh` to wrap `ebuild` calls with redirection for
asynchronous, logged execution.


## Build resilience: the per-phase retry chain

A failed phase is not necessarily a failed build.  After every phase,
`ebuild_exec` runs a chain of retry hooks, each keyed on a *signature*
found in the log segment written by the failed phase (never earlier
phases), so deterministic build failures never match and keep their
original semantics.  The chain has two layers:

1. **Environmental retries** (in `ebuild_exec.pl` itself) — failures
   caused by the build environment, not by the package:

   | **Retry** | **Signature** | **Recovery** | **Gate** |
   | :--- | :--- | :--- | :--- |
   | Transient (bash PID reuse) | `wait: pid N is not a child of this shell` | re-run the phase once | `config:build_transient_retry/1` |
   | Serial make (parallel-make race) | failed compile/test/install phase | re-run with `MAKEOPTS=-j1` | `config:build_serial_retry/1` |

2. **Domain exception fixups** (see next section) — failures caused by
   a problem that should really be fixed at the ebuild or metadata
   level.  The chain ends in a single generic dispatch,
   `fixup:maybe_phase_retry/9`, which offers the failure to every
   registered exception mechanism.

Every retry appends a marker line to the build log, so a recovered
build is never silent about how it recovered.


## Domain exception fixups

Some build failures are *packaging exceptions*: the build is failing
because of a gap in the ebuild or its metadata, not because of the
environment or the user's configuration.  Traditional emerge either
refuses such packages up front or fails and defers to a manual repair
tool.  portage-ng recovers them in-transaction through a small
registry of **exception mechanisms** under
`Source/Domain/Gentoo/Exceptions/`:

- **`fixup.pl`** — the generic registry and dispatcher.  A mechanism
  registers itself with three multifile hooks:
  - `fixup:mechanism/1` — identity (load order is dispatch and
    display order);
  - `fixup:phase_retry_hook/10` — the repair-and-retry logic for a
    failed phase;
  - `fixup:mechanism_note/3` — the note printed above the affected
    packages in the build summary.

  Applied fixups are recorded via `fixup:record/3` and reported
  generically by the build printer — adding a new exception mechanism
  never touches the builder or the printer.

### Collision deconfliction (`collision.pl`)

Traditional emerge refuses, at the plan stage, to install a package
whose files are owned by a different installed provider — it is told
so by an explicit blocker atom in metadata (e.g. installed
`sys-apps/util-linux[hardlink]` carries `!app-arch/hardlink`).  When
that blocker atom is *missing*, the conflict only surfaces at merge
time as Portage's `pkg_preinst` collision-protect abort.  Gated by
`config:deconflict_collisions/1` (`off` | `report` | `override`), the
mechanism recognises the collision signature and re-runs the merge
with `FEATURES="-collision-protect -protect-owned"`, letting the
package overwrite the colliding files.  The plan printer already
announces this behaviour next to the soft-blocker list, and the build
summary lists every package that needed it.  The same recovery is
applied to binary-package `qmerge` merges.

### GHC ABI repair (`ghcabi.pl`) — the native haskell-updater

Gentoo encodes a Haskell package's identity in `ghc-pkg`'s ABI hash
(the suffix in e.g. `bifunctors-5.6.3-9AmA3NO9963FDwV9BBcxcZ`), not in
the ebuild sub-slot.  When a `dev-haskell` library is rebuilt, its
installed reverse-dependencies keep referencing the old hash, and the
next Haskell consumer aborts in `pkg_setup`/`configure` with
haskell-cabal.eclass's check:

```
installed package semigroupoids-5.3.7 is broken due to missing package
bifunctors-5.6.3-9AmA3NO9963FDwV9BBcxcZ
 * Detected broken packages: semigroupoids-5.3.7 semialign-1.3
 * //==-- Please, run 'haskell-updater' to fix broken packages --==//
```

Because the hash lives only in ghc-pkg's registry, there is no
sub-slot delta for the resolver to observe, and traditional emerge
fails the same configure and defers to a manual `haskell-updater`
run.  portage-ng does better: gated by `config:ghc_abi_repair/1`, the
mechanism parses the broken package list from the failed phase's log,
rebuilds each broken package from source at its installed version and
with its VDB-recorded USE configuration (never from a binary package —
a stale binpkg ABI is exactly what may be broken), and re-runs the
failed phase.  One additional bounded round covers cascading breakage
exposed by the repair itself.

The mechanism is bounded and observable: each package is rebuilt at
most once per session (it can never loop), repairs are serialized
across parallel build workers, and every repair leaves markers in
both the consumer's and the rebuilt package's build logs plus an
entry in the build summary.

### OCaml ABI repair (`ocamlabi.pl`)

OCaml has the same problem: package identity lives in the compiled
interface digests (`.cmi` CRCs) checked by the compiler and in
findlib's registry, not in the ebuild sub-slot.  Unlike Haskell there
is no single eclass check enumerating the broken packages — a stale
consumer fails with heterogeneous compiler and ocamlfind messages:

```
Error: The files /usr/lib64/ocaml/site-lib/res/res.cmi
       and /usr/lib64/ocaml/stdlib.cmi
       make inconsistent assumptions over interface Stdlib
Error: Unbound module Camlp5
ocamlfind: Package `camlp5' not found
```

Gated by `config:ocaml_abi_repair/1`, the mechanism extracts the
stale compiled-unit paths and findlib package names from the failed
phase's log, maps them to their installed owners through the VDB
CONTENTS records (the active enumerator this domain lacks an eclass
check for), rebuilds those owners from source at their installed
version, and re-runs the failed phase — with the same boundedness
guarantees as the GHC repair: at most one rebuild per package per
session, at most two retry rounds, repairs serialized across workers,
and markers in every involved build log plus the build summary.  The
package being built and `dev-lang/ocaml` itself are never rebuild
candidates.

### Build summary reporting

At the end of a build, the printer renders one block per mechanism
that applied fixups, using the mechanism's own note:

```
Total: 46 completed.

Deconfliction: collision protection was disabled to merge 1 package over
               files owned by other installed packages (portage-ng#90):
  - app-arch/hardlink-0.3.2

GHC ABI repair: 2 broken packages rebuilt in-transaction after a
                dependency ABI-hash change (portage-ng#93, haskell-updater equivalent):
  - dev-haskell/semialign-1.3
  - dev-haskell/semigroupoids-5.3.7
```


## Live build display

During a `--merge` run, portage-ng keeps the terminal display
up-to-date so you can see exactly where the build process stands at
any moment.  The static plan that was printed during the `--pretend`
phase is reprinted once, and below it a live "Executing" area shows
the current state of every active build slot.

The following example shows the pretend output for
`sys-kernel/gentoo-sources`.  The plan has three steps: download the
source tarball plus patches, install the package, and register the
runtime phase.

```
>>> Emerging : portage://sys-kernel/gentoo-sources-6.19.11:run?{[]}

These are the packages that would be merged, in order:

Calculating dependencies... done!

 └─[step  1]─┤ download  portage://sys-kernel/gentoo-sources-6.19.11
             │           └─ file ─┤ 877.73 Kb   genpatches-6.19-10.base.tar.xz
             │                    │ 4.22 Kb      genpatches-6.19-10.extras.tar.xz
             │                    │ 148.84 Mb    linux-6.19.tar.xz

 └─[step  2]─┤ install   portage://sys-kernel/gentoo-sources-6.19.11
             │           └─ conf ─┤ USE = "build symlink -experimental"
             │                    │ SLOT = "6.19.11"

 └─[step  3]─┤ run       portage://sys-kernel/gentoo-sources-6.19.11

Total: 3 actions (1 download, 1 install, 1 run), grouped into 3 steps.
       149.70 Mb to be downloaded.
```

When the same target is merged with `--merge`, the display turns
into a live view.  Each step gains a phase line showing the
individual ebuild phases and their current state.  A snapshot
mid-build might look like this:

```
These are the packages being merged, in order:

Executing 3 actions, grouped into 3 steps...

 └─[step  1]─┤ download  portage://sys-kernel/gentoo-sources-6.19.11    ✓
             │           └─ file ─┤ 877.73 Kb   genpatches-6.19-10.base.tar.xz     ✓
             │                    │ 4.22 Kb      genpatches-6.19-10.extras.tar.xz   ✓
             │                    │ 148.84 Mb    linux-6.19.tar.xz                  ✓

 └─[step  2]─┤ install   portage://sys-kernel/gentoo-sources-6.19.11    ⣾
             │           └─ exec ─┤ ACTION = setup → unpack → prepare   (42%) 2/7
             │                    │ LOG = /var/log/portage/sys-kernel:gentoo-sources.log

 └─[step  3]─┤ run       portage://sys-kernel/gentoo-sources-6.19.11
```

In this snapshot, step 1 (download) has completed — each file shows
a green check mark on the right edge.  Step 2 (install) is active:
the action line shows a spinning indicator, and the phase line
reveals that `setup` and `unpack` have finished (shown in cyan)
while `prepare` is the current phase.  The right edge displays the
accumulated progress (`42%`) and a phase counter (`2/7`).  Step 3
(run) is still pending in dark grey, waiting for the install to
finish.

### Slot states and colours

Each slot in the live display represents one concurrent build.  The
slot line changes colour and icon as the build progresses:

| **State** | **Colour** | **Indicator** |
| :--- | :--- | :--- |
| Pending | Dark grey | Waiting for prerequisites |
| Active | Cyan (action) + green (target) | Spinning indicator on the right edge |
| Done | Green | Check mark |
| Failed | Red | Exclamation mark |
| Stub | Grey | Phase skipped (already satisfied) |

### Per-ebuild phase tracking

Below each slot line, the display shows the individual ebuild phases
(setup, unpack, prepare, configure, compile, install, qmerge) with
their current status.  Each phase word is coloured independently:

- **Dark grey** — pending (not yet started)
- **Cyan** — active or in progress
- **Green** — completed successfully
- **Red** — failed

The builder tracks phase state through `builder:exec_phase_state/3`,
which is updated by a callback from the ebuild execution module as
each phase starts, progresses, and finishes.

### Progress indicators

portage-ng shows progress at multiple levels:

- **Per-phase percentage** — during long phases like `compile`, the
  builder polls the build log every 0.5 seconds and computes a
  progress estimate.  This blends two signals: the growth of the log
  file (bytes written) and historical data from previous builds of
  the same package (stored in `Knowledge/phase_stats.pl`).
- **Overall progress** — the right edge of the display shows an
  accumulated percentage and a counter (`Current/Total`) reflecting
  how many actions have completed out of the total plan.  Stub
  actions (already satisfied) are excluded from the total.
- **Download progress** — for parallel downloads, each file shows a
  percentage and transfer speed.  Git clones show a separate
  percentage based on the git progress output.

### Log file locations

Each build action writes its output to a log file.  The path is
computed from the build log directory (`config:build_log_dir/1`) and
the ebuild name.  When `--logs` is enabled, the log path is displayed
below the phase line for each slot.  If a phase fails, the log path
turns red so you can quickly find the relevant output.

### Terminal refresh

The live display uses ANSI cursor movement to update individual lines
in place: the builder moves the cursor up to the target line,
redraws it, and moves back down.  This avoids flooding the terminal
with repeated full-screen redraws.  All display mutations go through
a `build_display` mutex to prevent concurrent workers from
interleaving their output.

In non-TTY environments (e.g. CI pipelines), cursor movement is
disabled and the builder falls back to sparse status lines.


## Build time estimation

The `buildtime.pl` module predicts build duration from two data sources:

1. **VDB sizes** — the installed file sizes from `/var/db/pkg/*/SIZE`
   correlate with build complexity.

2. **emerge.log history** — historical build times from
   `/var/log/emerge.log` provide empirical timing data for packages that
   have been built before.

The `--estimate` CLI option shows predicted build times in the plan output.


## Jobserver

The `jobserver.pl` module manages parallel build execution.  It implements
a token-based jobserver that limits concurrent builds to the number of
available cores (or a user-specified `--jobs` count).


## Download management

The `download.pl` module handles source archive fetching:

- Mirror layout detection via `curl`
- Parallel downloads across multiple mirrors
- Hash verification via `openssl dgst`
- Resume support for interrupted downloads

Downloads are scheduled as early as possible in the plan — the planner
treats `:download` actions as the first wave, so packages can download
while others are building.


## Snapshot support

Upgrades can go wrong — a new version may fail to compile, introduce
regressions, or break other packages.  portage-ng's snapshot module
(`Source/Pipeline/Builder/snapshot.pl`) lets you freeze the current
system state before a merge and roll back to it afterwards.

### How a snapshot is created

When a merge begins, portage-ng automatically creates a snapshot
identified by a timestamp (e.g. `20260405-143012`).  The snapshot
directory contains three files:

- **`manifest.pl`** — a Prolog fact file listing every package
  currently installed in the VDB, with category, name, version, and
  slot.
- **`world`** — a copy of the current world set file, so the set of
  explicitly requested packages can be restored exactly.
- **`actions.pl`** — the planned actions for the merge, recorded so
  that a rollback knows which packages were touched.

### Quickpkg: preserving the old version

The key to rollback is preserving the **binary package** of each
package that is about to be replaced.  Before portage-ng merges a
new version, the builder calls `snapshot:quickpkg_old/2`.  This runs
`ebuild --skip-manifest <old-ebuild> package` with `PKGDIR` pointed
at the snapshot's `binpkgs/` directory.  The result is a tarball
(`.tbz2` or `.gpkg.tar`) that contains the currently installed files
of the old version — essentially the same operation that Gentoo's
`quickpkg` tool performs.

Because this happens **per package, just before the upgrade**, the
snapshot accumulates exactly the set of binary packages needed to
reverse the merge.  Packages that were not touched are not
quickpkg'd; they remain unchanged on the system.

### Listing and diffing snapshots

`--snapshot list` shows all available snapshots with their
timestamp, installed package count, and the number of binary
packages stored:

```
Available snapshots:
  20260405-143012       2026-04-05 14:30:12   1847 pkgs   12 binpkgs
  20260402-091544       2026-04-02 09:15:44   1843 pkgs    5 binpkgs
```

`--snapshot diff <id>` compares a snapshot's manifest against the
current VDB and shows what changed — packages installed since the
snapshot, packages removed, and packages whose version changed:

```
Diff against snapshot "20260405-143012":

  Installed since snapshot (2):
    + dev-libs/newlib-4.5.0
    + dev-util/newtool-1.0

  Version changed since snapshot (3):
    ~ sys-libs/glibc  2.40-r2 -> 2.41
    ~ dev-lang/python  3.12.8 -> 3.13.1
    ~ app-editors/vim  9.1.1652-r2 -> 9.1.1700

  Summary: +2 -0 ~3 (3 binpkgs available for rollback)
```

### Rolling back

`--snapshot rollback <id>` reinstalls the saved binary packages
from the snapshot's `binpkgs/` directory and restores the world set
file.  Each binary package is merged back onto the system via
`ebuild <binpkg> merge`, downgrading the affected packages to their
pre-upgrade versions.  Combined with `--pretend`, the rollback
shows what would be reinstalled without actually making changes.

### Lifecycle

After the merge completes (whether successfully or not), the
snapshot remains on disk so it can be used for rollback at any
later time.  Old snapshots can be removed with
`--snapshot delete <id>` to reclaim disk space.


## Further reading

- [Chapter 12: Planning and Scheduling](12-doc-planning.md) — how the
  plan is constructed
- [Chapter 13: Output and Visualization](13-doc-output.md) — plan
  display and `.merge` file generation
- [Chapter 14: Command-Line Interface](14-doc-cli.md) — `--merge`,
  `--jobs`, `--estimate` flags
