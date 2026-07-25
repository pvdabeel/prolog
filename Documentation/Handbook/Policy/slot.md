# Policy: Slots

**Concern:** How do slot operators constrain co-installation and rebuilds?

**PMS / Portage:** Slots allow multiple versions of a CN; operators
include `:slot`, `:*`, `:=` (slot equality / rebuild on sub-slot change),
and sub-slots (`:1/A`). Same slot, different versions conflict.

**Literals:**

- Slot component inside `package_dependency` / candidate selection
- Slot occupancy constraints attached during `:install` resolve
- Assumptions: `slot_conflict`, related model issues

**Owns:** `Rules/candidate.pl`, `Rules/cnselect.pl`, slot-aware ranking
keys, version/slot facts from the knowledge cache.

**Invariants:**

- Explicit `:N` admits only that slot; `:*` admits any slot (prefer
  newest eligible).
- `:=` binds the parent to the child’s chosen slot/sub-slot for rebuild
  semantics.
- Two selected versions of the same CN in the **same** slot is a
  conflict (negative unless resolved by replace/upgrade path).
- Distinct slots of the same CN may co-install when policy allows
  (multi-slot).

**Examples:** [test41](examples.md#test41), [test42](examples.md#test42),
[test43](examples.md#test43), [test48](examples.md#test48),
[test68](examples.md#test68).  
**See also:** [Dependency](dependency.md), [Choice](choice.md).
