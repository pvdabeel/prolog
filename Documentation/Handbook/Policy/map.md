# One-page map: `rule/2` → schema → example → card

Use this table before changing rules code: find the head pattern, read
the obligation schema (what must hold), open one specimen, then the
policy card. Procedural “how” lives in the **Owns** column of each card.

| `rule/2` head (pattern) | Obligation schema (what) | Specimen | Card |
| :--- | :--- | :--- | :--- |
| `target(Q,Arg):run\|uninstall` | Resolve query → ebuild action (+ world side effects) | [test01](examples.md#test01), [test71](examples.md#test71) | [Target](target.md) |
| `Repo://E:install` | Eligible ∧ (keep \| update-for-USE \| resolve build deps) | [test01](examples.md#test01), [test67](examples.md#test67) | [Install](install.md) |
| `Repo://E:run` | Eligible ∧ (keep \| update-for-USE \| install + RDEPEND) | [test01](examples.md#test01), [test66](examples.md#test66) | [Run](run.md) |
| `Repo://E:download` | Fetch obligations (`--fetchonly` keeps these from the `:run` plan) | [test71](examples.md#test71) | [Target](target.md) |
| `Repo://E:update\|upgrade\|downgrade` | Replace installed with selected under constraints | [test73](../../Tests/test73/README.md), [test74](examples.md#test74) | [Install](install.md) |
| `Repo://E:depclean\|uninstall\|reinstall` | VDB-oriented maintenance actions | [test75](../../Tests/test75/README.md), [test77](../../Tests/test77/README.md) | [Install](install.md) |
| `grouped_package_dependency(no,…)` | Select one CN under version/slot/USE domain | [test13](examples.md#test13), [test55](examples.md#test55) | [Dependency](dependency.md) |
| `grouped_package_dependency(weak\|strong,…)` | Soft / hard blocker semantics | [test27](examples.md#test27), [test26](examples.md#test26) | [Blocker](blocker.md) |
| `any_of_group(…)` | Admit arms → rank → commit one | [test20](examples.md#test20), [test60](examples.md#test60) | [Choice](choice.md) |
| `exactly_one_of_group(…)` | Cardinality 1 among arms | [test17](examples.md#test17) | [Choice](choice.md) |
| `at_most_one_of_group(…)` | Cardinality ≤ 1 among arms | [test23](examples.md#test23) | [Choice](choice.md) |
| `all_of_group(…)` | Conjunction of members | [test16](../../Tests/test16/README.md) | [Dependency](dependency.md) |
| `use_conditional_group(±,…)` | Include body iff USE condition holds | [test14](examples.md#test14) | [USE](use.md) |
| `required(…)` / `blocking(…)` | REQUIRED_USE literals | [test40](examples.md#test40), [test49](examples.md#test49) | [REQUIRED_USE](requireduse.md) |
| Eligibility inside resolve | Not masked / keyword / license OK | [test12](examples.md#test12) | [Visibility](visibility.md) |
| Slot ops on atoms | `:N`, `:*`, `:=`, sub-slot | [test41](examples.md#test41)–[test44](../../Tests/test44/README.md) | [Slots](slot.md) |
| `rule(assumed(X))` | Domain assumption (pos/neg polarity) | [test09](examples.md#test09), [test12](examples.md#test12) | [Assumptions](assumption.md) |
| `assumed(rule(Lit))` | Prover cycle-break (benign axis) | [test03](examples.md#test03), [test05](examples.md#test05) | [Cycle](cycle.md) |


## Where “how” still lives

| Concern | Search / tactic modules (OK to be procedural) |
| :--- | :--- |
| Candidate enum + reprove | `Rules/Resolving/candidate.pl`, `Rules/Resolving/heuristic.pl` |
| `\|\|` preference fold/sort | `Rules/Resolving/ranking.pl` |
| USE eval + REQUIRED_USE | `Rules/Resolving/use.pl` |
| Dep model / ctx threading | `Rules/Resolving/dependency.pl`, `query.pl` |
| Learned domains | `prover:learn/3`, `Rules/Resolving/cnselect.pl` |
| Progressive relaxation | `pipeline:prove_*_with_fallback` |


## Sync checklist

When you change policy:

1. Update the relevant **card** invariants (one sentence if needed).
2. Point at an overlay specimen (add a row here if it is curriculum-worthy).
3. Keep polarity language aligned with
   `Source/Pipeline/Printer/Plan/assumption.pl`.
4. Do **not** move procedural detail into cards — link to the module instead.
