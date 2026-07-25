# Policy: Visibility

**Concern:** When may a repository entry be selected as a candidate?

**PMS / Portage:** An ebuild is usable only if it is not masked, has an
accepted keyword for the target arch (or is accepted via
`package.accept_keywords`), and is not blocked by a license refusal.
Unmask / keyword / license acceptance are config actions, not tree edits.

**Literals:**

- Eligibility guards on `Repo://Ebuild:install|run|…`
- Domain assumptions: `assumed(masked(…))`, `assumed(keyword_filtered(…))`,
  license-related assumptions (printed with `suggestion/2` tags)

**Owns:** `Rules/candidate.pl` (eligibility), preference / profile mask
and keyword layers, printer assumption suggestions.

**Invariants:**

- Masked or keyword-rejected candidates are not silently chosen when a
  visible alternative exists for an unconstrained CN query.
- Explicit version pins may still surface unmask / keyword assumptions
  for that exact version.
- Visibility failure under progressive relaxation becomes a **positive**
  domain assumption (actionable config), not a hard crash.

**Examples:** [test12](examples.md#test12) (keywords).  
**See also:** [Assumptions](assumption.md), [Target](target.md),
[Chapter 9](../09-doc-prover-assumptions.md).
