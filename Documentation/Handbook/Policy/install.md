# Policy: Install obligations

**Concern:** What must hold for `Repo://Ebuild:install`?

**PMS / Portage:** Building/installing a package requires fetch (when
needed), satisfied REQUIRED_USE, build-time deps (DEPEND / BDEPEND), and
a coherent slot occupancy. Already-installed packages with matching USE
are skipped (unless emptytree / forced rebuild).

**Literals:**

- `Repo://Ebuild:install?{Ctx}`
- Typical body themes: download / required_use / compile-time grouped
  deps / slot constraints / `selected_cn` domain locks
- May rewrite to `:update` when installed USE ≠ requested `build_with_use`

**Owns:** `rules.pl` (`:install`), `Rules/candidate.pl` (`eligible`,
`resolve`), `Rules/use.pl`, `Rules/dependency.pl`.

**Invariants:**

- `:install` obligates **build-time** deps, not RDEPEND (those are `:run`).
- Installed + matching plan USE ⇒ empty body (nomerge / keep).
- Installed + mismatched USE ⇒ transactional `:update` with
  `rebuild_reason(build_with_use)`.
- `--nodeps` empties the obligation list; it does not invent new meaning.

**Examples:** [test01](examples.md#test01), [test50](examples.md#test50),
[test65](examples.md#test65), [test67](examples.md#test67),
[test76](examples.md#test76).  
**See also:** [Run](run.md), [USE](use.md), [REQUIRED_USE](requireduse.md).
