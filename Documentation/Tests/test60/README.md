# test60 — Versioned soft blocker !<pkg-ver / newest `||` arm

**Category:** Blocker

This test case checks the handling of versioned soft blockers (`!<pkg-version`)
together with an any-of version choice. The `app-1.0` package blocks any version
of `windows` less than 2.0. The any-of group on `os-1.0` offers both
`windows-1.0` and `windows-2.0`. The solver should select `windows-2.0` and
avoid `windows-1.0`.

**Expected:** Select `windows-2.0` (emerge agrees). Portage-ng reaches this via
newest-admitted `||` ranking (portage-ng#112 / emerge `dep_zapdeps` upgrade
preference), matching emerge’s plan. Formerly XFAIL when left-to-right `||`
order locked `windows-1.0` and only recorded a soft-blocker assumption.

![test60](test60.svg)

<details>
<summary><b>emerge</b></summary>

```
These are the packages that would be merged, in order:

Calculating dependencies  ... done!
Dependency resolution took 0.75 s (backtrack: 0/20).

[ebuild  N     ] test60/windows-2.0::overlay  0 KiB
[ebuild  N     ] test60/os-1.0::overlay  0 KiB
[ebuild  N     ] test60/app-1.0::overlay  0 KiB
[ebuild  N     ] test60/db-1.0::overlay  0 KiB
[ebuild  N     ] test60/web-1.0::overlay  0 KiB

Total: 5 packages (5 new), Size of downloads: 0 KiB
```

</details>

<details>
<summary><b>portage-ng</b></summary>

```

>>> Emerging : overlay://test60/web-1.0:run?{[]}

These are the packages that would be merged, in order:

Calculating dependencies... done!

 └─step  1─┤ download  overlay://test60/windows-2.0
             │ download  overlay://test60/web-1.0
             │ download  overlay://test60/os-1.0
             │ download  overlay://test60/db-1.0
             │ download  overlay://test60/app-1.0

 └─step  2─┤ install   overlay://test60/windows-2.0

 └─step  3─┤ run       overlay://test60/windows-2.0

 └─step  4─┤ install   overlay://test60/os-1.0

 └─step  5─┤ run       overlay://test60/os-1.0

 └─step  6─┤ install   overlay://test60/app-1.0
             │ install   overlay://test60/db-1.0

 └─step  7─┤ run       overlay://test60/db-1.0
             │ run       overlay://test60/app-1.0

 └─step  8─┤ install   overlay://test60/web-1.0

 └─step  9─┤ run     overlay://test60/web-1.0

Total: 15 actions (5 downloads, 5 installs, 5 runs), grouped into 9 steps.
       0.00 Kb to be downloaded.

```

</details>
