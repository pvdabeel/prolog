# Policy: Blockers

**Concern:** What do soft `!` and hard `!!` blockers mean in the plan?

**PMS / Portage:**

- Soft `!cat/pkg` — prefer not to have the blocker installed; may
  uninstall or avoid selecting it
- Hard `!!cat/pkg` — forbid co-installation

Versioned forms (`!<pkg-ver`) restrict which versions are blocked.

**Literals:**

- Blocker-strength in `grouped_package_dependency(weak|strong, …)`
- Domain assumption: `blocker_assumption` (often **positive** /
  actionable: unmerge or pick another `||` arm)
- Ranking interaction: blocked arms demoted or excluded

**Owns:** `rules.pl` grouped dependency clauses, ranking / candidate
filters, printer blocker reporting.

**Invariants:**

- Strong blockers remove or forbid the conflicting selection; soft
  blockers prefer alternatives and may record an assumption.
- Inside `||`, a versioned soft blocker should push ranking toward a
  non-blocked arm when one exists (see test60 / newest-admitted preference).
- Blocker assumptions are **positive** when a config / unmerge action
  restores satisfiability.

**Examples:** [test26](examples.md#test26), [test27](examples.md#test27),
[test60](examples.md#test60).  
**See also:** [Choice](choice.md), [Assumptions](assumption.md).
