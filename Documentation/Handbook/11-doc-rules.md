# Rules and Domain Logic

## Prover and domain

The prover ([Chapter 8](08-doc-prover.md)) works with abstract literals
and rules.  It does not know what `:install` means for Gentoo — it only
knows how to find a matching `rule/2` clause and prove its body.  The
**rules layer** is the bridge between that abstract proof search and a
concrete domain: ebuilds, USE flags, version constraints, planning laws,
or uninstall claims.

When the prover encounters a literal, it calls into whichever rule
module it was given:

```prolog
prover:prove(Rules, ...)
prover:prove_once(Rules, ...)
```

`Rules` is a module atom — typically `resolving`, `ordering`, or
`unmerging`.  The prover expands `Rules:rule(Head, Body)` and treats
`Body` as the next obligations.  It never interprets what the literals
mean; all domain knowledge lives inside the rule clauses and the hooks
they register.

That separation is deliberate.  The same proof engine answers three
different questions by loading a different rule module:

| **Question** | **Rule module** | **Stage wrapper** |
| :--- | :--- | :--- |
| What configuration satisfies the request? | `resolving` | `resolver:resolve/9` |
| When can each action run? | `ordering` | `orderer:order/5` |
| In what order may packages be removed? | `unmerging` | depclean / unmerge pass |

Callers outside a proving pass (for example query-side model
construction) fall back to `config:default_rules/1`, which is
`resolving`.


## The `rule/2` contract

Every rule module exports the same interface:

```prolog
Module:rule(+Head, -Body)
```

The prover passes a literal as `Head`.  The module returns a list of
sub-literals `Body` that must be proved to justify it.  Failure means
"this expansion does not apply"; success commits those obligations to
the proof search.  Domain assumptions appear as
`rule(assumed(X), [])` — an empty body that records a justified gap
rather than aborting the proof.

Because the contract is uniform, Gentoo-specific vocabulary never
enters the prover core.  A new concern is almost always a new clause
(or a new rule module), not engine surgery in `prover.pl`.


## Rule modules in the pipeline

The pipeline chains two proving passes over one goal set
([Chapter 4](04-doc-architecture.md), [Chapter 8](08-doc-prover.md)):

1. **Pass 1 — configuration.** `resolver:resolve/9` hands `resolving`
   to `prover:prove/10`.  Choice lives here: versions, slots, USE
   flags, OR-group arms.  The output is a Proof / Model / Constraints /
   Triggers quadruple — a justified configuration.
2. **Pass 2 — plan.** `orderer:order/5` hands `ordering` to
   `prover:prove_once/…` over generic planning laws plus Gentoo
   bindings.  The output is a second proof object; wave projection
   reads it into a parallel plan.

Depclean's uninstall order reuses the same planning laws with
`unmerging` bindings: steps are `:unmerge` actions, requirements are
claim releases, and the installed world provides no escape hatch.

So "rules and domain logic" is not a synonym for dependency resolution.
Resolution is one rule set.  Ordering and unmerging are others.  The
chapters that follow treat the two constructive passes as peers:

- [Chapter 12: Resolution — Configuration as Proofs](12-doc-resolution.md)
  — pass 1: the `resolving` rule set and Gentoo policy
- [Chapter 13: Ordering — Plans as Proofs](13-doc-planning.md) — pass 2:
  planning laws, Gentoo bindings, wave projection


## What a rule body carries

Rule bodies are more than flat dependency lists.  They thread
**proof-term context** (`?{…}` lists on literals — see
[Chapter 5](05-doc-proof-literals.md) and
[Chapter 22](22-doc-context-terms.md)): `build_with_use/1`,
`constraint/1`, `after/1`, slot information, and suggestion tags for
assumptions.  Context is how parent requirements and local policy meet
without the prover understanding Gentoo.

Heads themselves encode domain speech acts.  For `resolving`, typical
patterns include user `target/2` literals, action literals
(`Repo://Ebuild:install`, `:run`, `:download`), grouped dependency
atoms, REQUIRED_USE validation literals, and the `assumed/1`
catch-all.  For `ordering`, the language shrinks to `scheduled/1`,
`available/2`, and `assumed(unreachable/2)`.  The full head tables live
in the pass-specific chapters.


## Domain hooks at the prover boundary

Besides `rule/2`, the domain may answer prover callbacks through
`heuristic:*` hooks (implemented for Gentoo under
`Rules/Resolving/heuristic.pl` and consulted during prove):

- **`proof_obligation/4`** — inject derived obligations after a literal
  succeeds (PDEPEND is handled this way in a single resolve pass).
- **`cycle_benign/2`** — classify a proof-search cycle as benign before
  the prover records a cycle-break assumption.
- **Constraint guards / reprove helpers** — learn domains or request a
  reprove when selected versions conflict (see
  [Chapter 9](09-doc-prover-assumptions.md)).

Hooks keep cross-cutting behaviour out of the generic search loop while
still letting the domain steer search.  Resolution and ordering each
rely on them differently; the mechanism is shared.


## Where the Gentoo resolve rules live

The `resolving` module is the public entry point
(`Source/Domain/Gentoo/Rules/resolving.pl`).  Its implementation is
split across focused submodules under
`Source/Domain/Gentoo/Rules/Resolving/` — candidate selection, USE
evaluation, ranking, CN selection, target resolution, and so on.  The
inventory and the end-to-end resolve narrative belong in
[Chapter 12](12-doc-resolution.md).

The `ordering` and `unmerging` modules sit beside them under
`Source/Domain/Gentoo/Rules/`.  They bind the generic planning laws to
Gentoo dependency classes and to VDB facts; see
[Chapter 13](13-doc-planning.md).


## Twin framing: configuration proofs and plan proofs

A successful run produces two proofs, not one algorithm output with a
post-pass:

- **Configuration as proof** — every chosen version, USE set, and
  dependency edge is justified by a `resolving` rule expansion (or an
  explicit domain assumption / cycle break).
- **Plan as proof** — every wave placement is justified by an
  `ordering` (or `unmerging`) expansion: a step is scheduled because
  its requirements are available from earlier steps or from the
  installed world.

Reading the handbook in that order — rules contract, then resolution,
then ordering — matches how the pipeline itself thinks.


## Further reading

- [Chapter 8: The Prover](08-doc-prover.md) — proof search, models,
  triggers, pipeline entry points
- [Chapter 9: Assumptions and Constraint Learning](09-doc-prover-assumptions.md) —
  domain assumptions, cycle breaks, progressive relaxation
- [Chapter 10: Version Domains](10-doc-version-domains.md) — version
  constraint representation used by resolve
- [Chapter 12: Resolution — Configuration as Proofs](12-doc-resolution.md) —
  the `resolving` rule set in depth
- [Chapter 13: Ordering — Plans as Proofs](13-doc-planning.md) —
  planning laws and wave projection
