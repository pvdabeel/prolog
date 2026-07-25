# Gentoo Linux Security Advisories (GLSA)

Gentoo publishes **GLSAs** — XML advisories that describe which package
versions are vulnerable and which versions are safe. Traditional Portage
exposes them mainly through `glsa-check` and the `@security` package set.
portage-ng treats the same advisories as a **first-class knowledge
artifact**: parsed into Prolog facts, optionally qcompiled for fast
reload, queryable from the shell, and expanded into ordinary remediation
atoms that the existing prove/plan pipeline consumes unchanged.


## Design choice: knowledge, not a repository

A natural first idea is to register GLSAs as another
`repository://entry` and search them with `query:search`. That fights
the architecture.

Package repositories (`portage`, `pkg`, `binpkg`) identify entries by
**CPVN** — category, package name, and `version/7` — via
`cache:ordered_entry/5`. Every prover, planner, and rules consumer
assumes that shape. A GLSA id such as `202501-03` is not a package
version; inventing a fake `glsa://…` CPVN would either pollute those
paths or require permanent exclusion filters.

Non-package knowledge already lives outside the package-repo model:

| Concern | Pattern |
| :-- | :-- |
| Profile | Own facts + `Knowledge/profile.qlf` |
| News | Filesystem read (display-only) |
| Named / computed sets | `preference:set/2` / `sets:expand/2` |

**GLSAs follow the profile pattern:** a sibling knowledge store with its
own facts and cache file, plus a small query surface. Package repos stay
the only `Repo://Entry` units. Security sets are a *view* over that
store, not the primary API.

```
  metadata/glsa/*.xml
        │
        ▼
  glsa:cache_save  ──────►  Knowledge/glsa.qlf
        │
        ▼
  glsa:advisory / package / range facts
        │
        ├── glsa:search/2          (advisory queries)
        ├── query:search bridges   (vulnerable/1, glsa/1 on pkg entries)
        └── sets:expand/2          (@security → =cat/pkg-ver atoms)
                                        │
                                        ▼
                              prove_plan_with_fallback
```


## On-disk source and cache

Advisories live in the Portage tree at `$PORTDIR/metadata/glsa/` as
files named `glsa-YYYYMM-NN.xml`. Override the directory with
`config:glsa_dir/1` when needed.

During `--sync`, portage-ng calls `glsa:cache_save/0` next to
`profile:cache_save/0`. That walk:

1. Parses every `glsa-*.xml` (DTD-safe string extraction — no network
   DTD fetch, same rule as `metadata.xml` maintainers).
2. Writes `Knowledge/glsa.raw` and qcompiles `Knowledge/glsa.qlf`.
3. Loads the facts into the running process.

At runtime, `glsa:ensure_loaded/0` prefers the qlf cache and falls back
to a live parse of `metadata/glsa/` when the cache is missing. Cold live
parse of a full tree (~3.8k advisories) is well under a second; qlf
reload is near-instant.

Parsing skips non-`ebuild` product types and tolerates individual
malformed files so one bad advisory never aborts `@security` or sync.


## Fact schema

The hot store is three dynamic predicates (also serialized into the
`glsadata` module inside `glsa.qlf`):

```prolog
glsa:advisory(Id, Title).
glsa:package(Id, Category, Name, ArchSpec).
glsa:range(Id, Category, Name, Kind, Op, Version, Slot).
```

| Field | Meaning |
| :-- | :-- |
| `Id` | Advisory id (`'202501-03'`) |
| `Kind` | `vulnerable` or `unaffected` |
| `Op` | GLSA range token: `le`, `lt`, `eq`, `gt`, `ge`, `rge`, `rle`, `rgt`, `rlt` |
| `Version` | Bound as a `version/7` term |
| `Slot` | Slot atom, or `*` for any |
| `ArchSpec` | `*` or a space-separated arch list |

Synopsis and body text are intentionally omitted from the hot store;
set expansion and vulnerability checks only need package/range rows. A
future dump/CLI can add richer fields without changing the set path.


## Matching installed packages

Vulnerability is decided by joining advisory ranges against the **VDB**
(installed packages) and the **tree** (visible upgrades):

1. **ARCH** — `*` always matches; otherwise the host arch from
   `userconfig:current_arch/1` (or `ARCH` / `ACCEPT_KEYWORDS`) must
   appear in the package’s arch list. Unknown arch ⇒ only `*` matches
   (conservative).
2. **Vulnerable range** — installed version matches a `vulnerable`
   range for that C/N/slot.
3. **Not unaffected** — the same installed version must *not* match an
   `unaffected` range.
4. **Upgrade exists** — a visible tree ebuild in the same slot matches
   an `unaffected` range and is greater than the installed version.
   Among such upgrades, portage-ng picks the **least-change**
   (lowest) version, matching Portage’s `getMergeList(least_change=true)`.

Ordinary comparisons (`le`/`lt`/`eq`/`gt`/`ge`) use
`eapi:version_compare/3` on `version/7` terms. Revision-limited ops
(`rge`/`rle`/`rgt`/`rlt`) require the same base version and compare
only the revision field — Portage’s `revisionMatch` semantics.

Remediation atoms are exact pins: `=category/name-version`. Those atoms
enter the normal target rules; the prover is not GLSA-aware.


## Security computed sets

Portage’s `sets.conf` defaults `@security` to `NewAffectedSet`.
portage-ng registers four computed set names in
`Source/Domain/Gentoo/Preference/sets.pl`:

| Set | Portage class | Meaning |
| :-- | :-- | :-- |
| `@security` | `NewAffectedSet` (default) | Vulnerable installs from GLSAs not yet applied |
| `@new-affected` | `NewAffectedSet` | Same, explicit name |
| `@affected` | `AffectedSet` | Vulnerable installs, including applied GLSAs |
| `@new-glsa` | `NewGlsaSet` | Unapplied GLSAs (atoms still only appear when an upgrade exists) |

Related non-GLSA computed sets live in the same registry
(`Preference/sets.pl`); see [Chapter 14: CLI — Package sets](14-doc-cli.md#package-sets)
for the full table (`@preserved-rebuild`, `@changed-deps`, `@installed`, …).

Expansion is **VDB-driven**: walk installed packages, look up matching
advisory rows, emit upgrade atoms, then reduce per `cat/name:slot` to
the highest remediation version (Portage `_reduce`). This stays near-
linear in installed CPV count rather than scanning every advisory for
`is_vulnerable`.

```bash
portage-ng --mode standalone --pretend @security
portage-ng --mode standalone --list-sets   # includes security sets
```

When no installed package is vulnerable (or every matching GLSA is
already injected), `@security` expands to the empty list and the CLI
reports that the set is empty — exit 0, not a hard failure.


## Applied / injected tracking

Portage records applied GLSA ids in `$EROOT/var/lib/portage/glsa_injected`.
portage-ng mirrors that with `config:glsa_injected_file/1` (default:
`Source/Knowledge/Sets/glsa_injected/<hostname>`).

| Predicate | Role |
| :-- | :-- |
| `glsa:applied(+Id)` | True when `Id` is listed in the inject file |
| `glsa:inject(+Id)` | Append `Id` if not already present |

`NewAffectedSet` / `NewGlsaSet` filters consult this file. A dedicated
`glsa-check`-style inject CLI is not required for set expansion; the
predicates are ready for a thin follow-up action.


## Query surface

### Advisory search — `glsa:search/2`

```prolog
glsa:search([package('dev-python', pip), applied(false), vulnerable(true)], Id).
glsa:search(title(Title), Id).
```

Accepted constraints: `id/1`, `title/1`, `package/2`, `applied/1`,
`vulnerable/1`. Queries run against `glsa:*` facts (and VDB joins for
`vulnerable`), not against `cache:ordered_entry`.

### Package bridges — `query:search`

Two compile-time sugar keys join advisories onto an existing
`Repo://Entry` (typically the VDB):

| Query | Meaning |
| :-- | :-- |
| `vulnerable(true)` | Some non-filtered GLSA covers this installed entry and an upgrade exists |
| `glsa(Id)` | Advisory `Id` covers this entry’s C/N/version/slot |

```prolog
?- knowledgebase:vdb_repository(V),
   query:search([vulnerable(true), category(C), name(N)], V://E).
```

These bridges do **not** invent `glsa://` entries. Prefer `glsa:search/2`
inside set logic so the package query hot path stays free of advisory
scans unless asked.


## Module map

| File | Role |
| :-- | :-- |
| `Source/Domain/Gentoo/glsa.pl` | Parse, facts, cache, match, search, set atoms |
| `Source/Domain/Gentoo/Preference/sets.pl` | Registers `@security` and siblings |
| `Source/Knowledge/query.pl` | `vulnerable/1` and `glsa/1` bridges |
| `Source/Application/Interface/Action/sync.pl` | Calls `glsa:cache_save` during `--sync`; lists computed sets |
| `Source/config.pl` | `config:glsa_dir/1`, `config:glsa_injected_file/1` |

Loaded with the domain modules (`loader:group(domain_modules)`), after
preference and before `sets.pl`.


## What this is not

- **Not a package repository.** No `kb:register(glsa)`, no GLSA rows in
  `kb.qlf`.
- **Not prover logic.** No new rules, assumptions, or fallback tiers.
  Remediations are ordinary `=cpv` targets.
- **Not a full `glsa-check` clone (yet).** List/dump/mail/fix modes can
  wrap the same facts later; set expansion and search are the v1 surface.
- **Not network-fetched.** Advisories arrive with the tree after the user
  runs `--sync`.


## Further reading

- [Chapter 6: Knowledge Base and Cache](06-doc-knowledgebase.md) — package
  `kb.qlf` vs sibling caches such as `profile.qlf` / `glsa.qlf`
- [Chapter 3: Configuration](03-doc-configuration.md) — sync, profile
  cache, and host-local paths
- [Chapter 14: Command-Line Interface](14-doc-cli.md) — `--pretend`,
  `--list-sets`, target `@set` syntax
- [Chapter 10: Version Domains](10-doc-version-domains.md) — `version/7`
  comparison used by GLSA range matching
- Portage reference: `lib/portage/glsa.py`, `lib/portage/_sets/security.py`
