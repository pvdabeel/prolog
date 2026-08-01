# AGENTS.md

General architecture, coding conventions, and the canonical run/test/regression
workflow live in `.cursorrules` and `CONTRIBUTING.md`. Read those first. This
file only records durable, non-obvious guidance for cloud agents.

## Cursor Cloud specific instructions

### Environment
- Runtime is **SWI-Prolog** (installed system-wide via `ppa:swi-prolog/devel`,
  `swipl` on `PATH`). It persists in the VM snapshot, so the update script does
  not reinstall it. `portage-ng` has no package-manager (npm/pip/uv)
  dependencies — SWI-Prolog plus the standard CLI tools (bash, git, python3)
  are all that is needed.
- The startup/update script recreates the two gitignored private config files
  from their templates (`Source/Config/Private/{api_key,passwords}.pl`). These
  are optional (config guards them with `exists_source/1` and falls back to
  empty values), so their absence is not fatal; recreating them just mirrors CI
  and enables LLM/server features.

### No Gentoo tree in the sandbox (important)
- There is **no full Gentoo Portage tree and no `Knowledge/kb.qlf`** here. The
  hostname resolves to `cursor`, so the default host config
  (`Source/Config/default.pl`) points the `portage` repo at
  `Repository/portage-git`, which does **not** exist. Real
  `--pretend <category/package>` against the `portage` repo will therefore find
  no candidates in this environment.
- Per `.cursorrules`, do **not** run `--sync` or `--graph` inside the sandbox
  (they mutate state / need network + a real tree); the user runs those
  externally.

### How to test / demonstrate in the sandbox
Everything runs through the wrapper `./Source/Application/Wrapper/portage-ng-dev`
(never raw `swipl`). The in-repo synthetic overlay (`Repository/Overlay`, ships
its own md5-cache) is the substitute for a real tree:
- Boot check: `--mode standalone --shell` and `halt.` (prints the banner).
- PLUnit suite (fast, no tree): `make test` (518 tests).
- Overlay regression (full resolve+order+print pipeline, 80 cases):
  ```sh
  ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell --timeout 900 <<'PL'
  ( catch(test:run(cases,batch),E,(print_message(error,E),fail)) -> halt(0) ; halt(1) ).
  PL
  ```
- Hello-world build plan against the overlay (registers the overlay first via
  `test:ensure_overlay`, then runs the canonical pipeline entry point):
  ```sh
  ./Source/Application/Wrapper/portage-ng-dev --mode standalone --shell --timeout 120 <<'PL'
  test:ensure_overlay,
  Target = overlay://'test47/api-docs-1.0':run?{[]},
  pipeline:prove_plan_with_fallback([Target],Proof,Model,Plan,Triggers,SCCs,_),
  printer:print([Target],Model,Proof,Plan,Triggers,SCCs),
  halt.
  PL
  ```

### Building
- `make build` produces the standalone `portage-ng` binary (gitignored via
  `make clean`); day-to-day development uses the wrapper, not the binary.
