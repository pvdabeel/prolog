# Policy: Target resolution

**Concern:** How does a user / set atom become a concrete ebuild action?

**PMS / Portage:** CLI atoms (`cat/pkg`, versioned atoms, `@sets`) resolve
to one or more package versions; emerge then plans install/run for that
selection. Sets expand before resolution.

**Literals:**

- `target(Q, Arg):run|fetchonly|uninstall?{Ctx}`
- Expands to `Repo://Ebuild:Action?{Ctx}` (and optional `world/1` side effects)

**Owns:** `Rules/target.pl`, `rules.pl` TARGET section, `Preference/sets.pl`
(set expansion before prove).

**Invariants:**

- Unconstrained CN targets prefer **visible** candidates before unmasking.
- Explicit versions try candidates in standard order (user pin wins).
- `:fetchonly` does not register `@world`; `:uninstall` may unregister
  unless `--oneshot`.
- Set atoms (`@world`, `@security`, …) expand to ordinary package atoms
  before target rules run.

**Examples:** [test01](examples.md#test01), [test71](examples.md#test71),
[test78](examples.md#test78).  
**See also:** [Visibility](visibility.md), [Install](install.md).
