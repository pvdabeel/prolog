# Policy: Dependency atoms

**Concern:** How does a package dependency atom become one selected entry?

**PMS / Portage:** Atoms carry category/name, version operator, slot
operator, and optional USE deps. Multiple constraints on the same CN
intersect; one version (per slot policy) is chosen.

**Literals:**

- `package_dependency(Phase, …):config?{Ctx}`
- `grouped_package_dependency(Strength, C, N, PackageDeps):Action?{Ctx}`
- Learned constraints: `cn_domain(C, N, Slot)` via `prover:learn/3`

**Owns:** `Rules/dependency.pl`, `Rules/candidate.pl`, `Rules/cnselect.pl`,
`Rules/ranking.pl`, version domain meet ([Chapter 10](../10-doc-version-domains.md)).

**Invariants:**

- Local atom domain ∩ context domain ∩ learned domain = effective domain.
- Tighter atoms are proved before looser ones (`dep_priority`) so
  `selected_cn` locks early.
- No candidate in the effective domain ⇒ **negative** assumption
  (`version_no_candidate` / related).
- Parent narrowing / wildcard learning refine domains across reproves;
  they do not invent versions outside the tree.

**Examples:** [test02](examples.md#test02), [test13](examples.md#test13),
[test55](examples.md#test55), [test69](examples.md#test69),
[test70](examples.md#test70), [test80](examples.md#test80).  
**See also:** [Choice](choice.md), [Slots](slot.md), [Visibility](visibility.md).
