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

**Owns:** `resolving.pl` grouped dependency clauses, ranking / candidate
filters, printer blocker reporting.

**Invariants:**

- Strong blockers remove or forbid the conflicting selection; soft
  blockers prefer alternatives and may record an assumption.
- Inside `||`, a versioned soft blocker should push ranking toward a
  non-blocked arm when one exists (see test60 / newest-admitted preference).
- Blocker assumptions are **positive** when a config / unmerge action
  restores satisfiability.
- A recorded soft blocker is only *reported* (blocker section, inline
  `(blocked: soft by …)` note, CI exit code) when its atom — operator,
  version, slot **and** sub-slot — actually hits a planned merge of the
  blocked CN, or an installed copy the plan leaves in place. Pass 1
  records every soft blocker it walks past because the blocked CN's
  candidate may be selected later in the proof; relevance is decided
  post-proof in `annotation:collect/2` (`annotation:blocker_effective/2`),
  reusing the strong-blocker matching core. `!dev-ml/findlib:0/0`
  against a planned findlib `0/1`, or `!<dev-util/ragel-7.0.3` against
  a planned `7.0.4`, block nothing and are silent (portage-ng#119
  follow-up).

**Examples:** [test26](examples.md#test26), [test27](examples.md#test27),
[test60](examples.md#test60).  
**See also:** [Choice](choice.md), [Assumptions](assumption.md).
