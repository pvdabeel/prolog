# Policy: Run and PDEPEND

**Concern:** What must hold for `Repo://Ebuild:run`?

**PMS / Portage:** A package can “run” when it is installed (or will be)
and its RDEPEND graph is satisfied. PDEPEND is post-merge oriented in
Portage narrative; portage-ng still resolves it in the same prove pass
via a prover literal hook (always on).

**Literals:**

- `Repo://Ebuild:run?{Ctx}`
- Body themes: install (if needed) + runtime grouped deps
- PDEPEND injected through `rules:literal_hook/4` (not a separate preference)

**Owns:** `rules.pl` (`:run`), `Rules/candidate.pl`, `Rules/dependency.pl`,
prover hook path in `Source/Pipeline/prover.pl`.

**Invariants:**

- `:run` obligates **RDEPEND** (and hook-resolved PDEPEND), not BDEPEND.
- Same installed-USE short-circuit / `:update` rewrite as `:install`.
- PDEPEND cycles are handled like other cycles (benign break or plan),
  not by disabling PDEPEND.
- Runtime missing packages are **negative** domain assumptions.

**Examples:** [test01](examples.md#test01), [test10](examples.md#test10),
[test66](examples.md#test66), [test79](examples.md#test79).  
**See also:** [Install](install.md), [Cycle breaks](cycle.md).
