# Policy: Choice groups (`||`, `^^`, `??`)

**Concern:** How are PMS choice groups resolved?

**PMS / Portage:**

- `|| ( … )` — any one arm
- `^^ ( … )` — exactly one arm
- `?? ( … )` — at most one arm

Emerge’s `dep_zapdeps` prefers arms that are already satisfied / better
aligned (USE, installed, newer, …). portage-ng encodes the same idea as
a multi-key ranking on admitted arms.

**Literals:**

- `any_of_group(Deps):Action|validate?{Ctx}`
- `exactly_one_of_group(Deps):…`
- `at_most_one_of_group(Deps):…`

**Owns:** `resolving.pl` choice section, `Rules/Resolving/ranking.pl`
(`prioritize_deps_keep_all`), Handbook 11 “Any-of (`||`) arm selection”.

**Diagnostics:** `--choice-log` via `portage-ng-dev` sets
`-Dchoice_log=true` (compile-time) and arms `choicelog` at runtime (see
`Source/Application/Performance/choicelog.pl`). Hot-path emit/wrap sites
are `goal_expansion`'d to nothing when the define is absent. Records
trying/succeeded/failed for `||` arms and alternative version binds,
plus sparse reject/learn/reprove/assumption events. Dump goes to stderr
after prove; `choicelog:events/1` returns the term list for shell or LLM
use.

**Invariants:**

- Only **admitted** arms (visible + domain-feasible) enter ranking.
- Ranking is a total order over preference keys (USE sat, unmasked,
  snap, slot, no-downgrade, installed, license, …) — not left-to-right
  source order alone.
- `validate` actions check cardinality; solve actions commit one arm.
- Soft blockers can demote an arm; strong blockers remove it.

**Examples:** [test20](examples.md#test20), [test17](examples.md#test17),
[test23](examples.md#test23), [test59](examples.md#test59),
[test60](examples.md#test60).  
**See also:** [Dependency](dependency.md), [Blockers](blocker.md),
[Chapter 12 § Any-of](../12-doc-resolution.md).
