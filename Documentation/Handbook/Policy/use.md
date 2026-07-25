# Policy: USE

**Concern:** When do USE conditionals fire, and how do bracketed USE deps
propagate?

**PMS / Portage:** `flag? ( deps )` / `!flag? ( deps )` gate dependency
lists. Bracketed deps (`pkg[flag]`, `[flag=]`, `[flag?]`, …) constrain
the child’s effective USE. Multiple parents may require different flags
on a shared child; the effective set is a meet / merge.

**Literals:**

- `use_conditional_group(positive|negative, Use, R://E, Deps):Action?{Ctx}`
- Proof-term context: `build_with_use/1`, `required_use/1`,
  `suggestion(use_change, …)`

**Owns:** `Rules/use.pl`, `Rules/featureterm.pl`, `rules.pl` USE section,
feature unification for context meet.

**Invariants:**

- Positive conditional includes deps only when `Use` is enabled in the
  effective set for that ebuild/path; negative is the dual.
- Bracketed USE on a child becomes `build_with_use` in the child’s
  proof-term context — not a silent global PROFILE change.
- Shared CN: USE requirements from distinct parents must **unify**;
  irreconcilable meets become domain assumptions / conflicts.
- Soft defaults (`flag(+)`) lose to hard REQUIRED_USE / explicit forbid.

**Examples:** [test14](examples.md#test14), [test33](examples.md#test33),
[test35](examples.md#test35), [test49](examples.md#test49),
[test52](examples.md#test52).  
**See also:** [REQUIRED_USE](requireduse.md), [Install](install.md).
