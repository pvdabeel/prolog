# Policy: Cycle breaks

**Concern:** What happens when the dependency graph has a cycle?

**PMS / Portage:** Emerge often proceeds through bootstrap / circular
runtime edges with little ceremony. portage-ng’s inductive prover must
**break** a cycle to finish the proof; that break is recorded explicitly.

**Literals:**

- Prover cycle-break proof key: `assumed(rule(Lit))`
- Model side: `assumed(Lit)`
- Distinct from domain `rule(assumed(X))`

**Owns:** `Source/Pipeline/prover.pl` (cycle detection / assume),
benignity checks in `Rules/Resolving/heuristic.pl`, printer cycle section
(`config:print_prover_cycles/1`).

**Invariants:**

- Cycle-breaks are **not** domain assumptions; do not lump them into
  “assumptions” counts without saying which kind.
- Self-deps and mutual compile/runtime cycles typically produce benign
  breaks (CI exit `1` when alone).
- Domain assumptions and cycle-breaks can co-exist; exit `2` if any
  domain assumption is present.
- PDEPEND cycles use the same break mechanism; PDEPEND is never disabled
  to avoid them.

**Examples:** [test03](examples.md#test03) and
[test05](examples.md#test05) (actual cycle-break assumptions);
[test06](examples.md#test06), [test47](examples.md#test47),
[test61](examples.md#test61) and [test79](examples.md#test79)
(cycles dissolved by the install/run action split — no break needed).  
**See also:** [Assumptions](assumption.md), [Run](run.md),
[Chapter 9](../09-doc-prover-assumptions.md).
