# Policy: Domain assumptions

**Concern:** When is a missing proof an assumption rather than failure,
and is it actionable?

**PMS / Portage:** Emerge may autounmask, suggest USE, or error on
missing packages. portage-ng records domain assumptions in the proof
(`rule(assumed(X))`) and continues under progressive relaxation so the
printed plan shows *what would work* after a stated change.

**Literals:**

- Proof key: `rule(assumed(X))` (domain)
- Not the same as prover cycle-breaks: `assumed(rule(X))`

**Owns:** `rules.pl` assumed catch-alls, fallback tiers in
`pipeline.pl` / prover, classification in
`Pipeline/Printer/Plan/assumption.pl`.

**Invariants (polarity):**

- **Positive / actionable** — plan is satisfiable after a config action:
  `masked`, `keyword_filtered`, license acceptance, `blocker_assumption`.
  Phrase as “change this in `/etc/portage`…”.
- **Negative / blocking** — structurally unsatisfiable as stated:
  `non_existent_dependency`, `missing_dependency`,
  `required_use_violation`, `slot_conflict`, `version_no_candidate`,
  `version_conflict`, `unsatisfied_constraints`, `issue_with_model`.
- Exit codes today: `0` clean, `1` cycle-breaks only, `2` any domain
  assumption (finer positive/negative split is planned, not implemented).

**Examples:** [test09](examples.md#test09) (negative missing),
[test12](examples.md#test12) (positive keyword),
[test27](examples.md#test27) (blocker),
[test51](examples.md#test51) (REQUIRED_USE / USE conflict).  
**See also:** [Cycle breaks](cycle.md),
[Chapter 9](../09-doc-prover-assumptions.md).
