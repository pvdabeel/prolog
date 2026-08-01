# Policy: REQUIRED_USE

**Concern:** What hard USE constraints does an ebuild impose on itself?

**PMS / Portage:** `REQUIRED_USE` is a propositional constraint over IUSE
(`||`, `^^`, `??`, `flag? ( … )`, …). Unsatisfied constraints block the
build; emerge often suggests USE changes (autounmask).

**Literals:**

- `required(Use)` / `required(minus(Use))`
- `blocking(Use)` / `blocking(minus(Use))`
- Choice-group `*:validate` for REQUIRED_USE cardinality
- Assumptions: `assumed(conflict(required,…))`, `required_use_violation`

**Owns:** `Rules/Resolving/use.pl`, `resolving.pl` required/blocking clauses.

**Invariants:**

- REQUIRED_USE is evaluated against the **effective** USE for that
  ebuild (IUSE defaults + config + `build_with_use`).
- Violations that cannot be repaired by progressive relaxation are
  **negative** domain assumptions.
- Actionable USE flips appear as `suggestion(use_change, …)` when the
  plan proposes a config change.
- REQUIRED_USE is stricter than soft USE defaults from parents.

**Examples:** [test32](examples.md#test32), [test40](examples.md#test40),
[test49](examples.md#test49), [test51](examples.md#test51),
[test63](examples.md#test63).  
**See also:** [USE](use.md), [Assumptions](assumption.md).
