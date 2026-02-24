# Test Suite Remaining Tasks

Tasks are ordered by priority — earlier items unblock or simplify later ones.

---

## 1. Remove stale `@ts-ignore` in `helpers/executor.ts` ⚡ quick fix

`helpers/executor.ts` has two `// @ts-ignore — Apollo Client version mismatch between dependencies` comments before `new Ad4mClient(...)` calls. These were rendered unnecessary by the `pnpm.overrides` graphql-ws/react dedup fix. Remove them.

---

## 2. Centralise `global.fetch = fetch` into a shared setup file

Almost every standalone test file contains:

```typescript
import fetch from "node-fetch";
//@ts-ignore
global.fetch = fetch;
```

Extract this into a single `tests/setup.ts` and load it via mocha's `--require` flag (same pattern already used by `tests/model/hooks.ts`). All per-file `import fetch` + `global.fetch` blocks can then be deleted.

Implementation:

1. Create `tests/setup.ts` containing just the fetch polyfill
2. Add `--require tests/setup.ts` to each script in `package.json` (or use a `.mocharc.cjs`)
3. Remove the per-file blocks from all test files

Do this before the `startAgent` migration below so migrated files start clean.

---

## 3. Migrate remaining files to `startAgent` helper

`helpers/executor.ts` exports `startAgent(agentName)` which handles dynamic port allocation, directory creation, executor startup, agent generation, and teardown via `.stop()`. It was introduced during the `multi-user/` split but only applied to 6 of the 10 multi-user files.

**Files to migrate** (currently use verbose raw `startExecutor` with hardcoded ports and manual kill loops):

- `tests/multi-user/multi-user-auth.test.ts`
- `tests/multi-user/multi-user-connect.test.ts`
- `tests/auth/app.test.ts`
- `tests/auth/authentication.test.ts`
- `tests/auth/email-verification.test.ts`

**Legitimate exceptions** (need `runHcLocalServices()` or multiple executors — keep raw pattern):

- `tests/multi-user/multi-user-neighbourhood.test.ts`
- `tests/multi-user/multi-user-multi-node.test.ts`
- `tests/integration/integration.test.ts`

The `startAgent` helper will also need a `token` option so auth tests (which pass a hardcoded admin token) can use it.

---

## 4. Script reorganisation (`package.json`)

Once the above migrations are done, tidy up the scripts:

- Add `test-auth` as a combined script chaining `test-app`, `test-auth-single`, and `test-email-verification` (rename existing `test-auth` → `test-auth-single` first)
- Add `test-sdna-all` chaining `test-sdna` and `test-smart-literal`
- Fold `test-from-json-schema` into `test-model` (it's already a `model/` file; remove the standalone script)
- Simplify `test-run` (currently `test-all`) to chain folder-level scripts rather than individual files — reduces it from a 200+ char line to ~8 calls
- Keep `test-run:windows` in sync

---

## 5. Script naming (`test-main` / `test-all`)

Rename after the reorganisation above so you're renaming the final shape, not an intermediate one:

| Current            | Suggested          | Rationale                                                     |
| ------------------ | ------------------ | ------------------------------------------------------------- |
| `test-main`        | `test`             | Standard npm convention for the full CI lifecycle entry point |
| `test-all`         | `test-run`         | Makes clear it skips setup and just executes the suites       |
| `test-all:windows` | `test-run:windows` | Consistent                                                    |

---

## 6. HTML test reports via `mochawesome` 🎨 nice to have

Mocha's default `spec` reporter produces no persistent output. `mochawesome` generates a self-contained HTML report with pass/fail visualisation, timing per test, and full error details.

Implementation:

1. `pnpm add -D mochawesome`
2. Add a `test-report` script running `test-run` with `--reporter mochawesome --reporter-options reportDir=test-reports,reportFilename=results`
3. Add `test-reports/` to `.gitignore`

---

## 7. Script ordering in `package.json` (cosmetic, low priority)

Currently "entry points first" (`test-main`/`test-all` at top, `prepare-test` buried near bottom). A "workflow order" would put `prepare-test` first, then individual suites, then combined runners, then lifecycle wrappers.

JSON has no comment support so neither ordering can be annotated with section headers. The naming fix in task 5 makes the hierarchy obvious regardless of order — reordering alone adds little value.

---

## 8. `integration/` sub-file naming convention (cosmetic, low priority)

`integration/` mixes two kinds of files:

- `integration.test.ts` — standalone orchestrator (runnable directly)
- `agent.ts`, `perspective.ts`, `runtime.ts`, etc. — suite modules (not standalone, export a function)

Options:

- Rename suite modules to `*.suite.ts` (e.g. `perspective.suite.ts`)
- Move them into `integration/suites/`
- Leave as-is (convention understood by context)

The npm scripts are functional but could be tidied up:

- **`auth/` folder** — add a combined `test-auth-all` script chaining `test-app`, `test-auth`, and `test-email-verification` (mirrors the pattern used by `test-multi-user`)
- **`sdna/` folder** — add a combined `test-sdna-all` script chaining `test-sdna` and `test-smart-literal`
- **`test-from-json-schema`** — this script points at `tests/model/model-from-json-schema.test.ts` but is listed as a standalone script rather than just being part of `test-model`; fold it in and remove the separate entry
- **`test-all`** — simplify by chaining folder-level scripts (`test-auth-all`, `test-sdna-all`, `test-model`, etc.) rather than individual file scripts; currently a 200+ character line that's hard to read/maintain
- **`test-all:windows`** — same as above, keep in sync with `test-all`

Note: `integration/` sub-files (e.g. `agent.ts`, `language.ts`, etc.) are **not** standalone — they export test suite functions composed by `integration.test.ts`. No individual scripts needed for those.

## Script naming (`test-main` vs `test-all`)

The current naming is confusing:

- **`test-main`** — sounds like it runs the "main" tests, but is actually the full lifecycle wrapper: `cleanTestingData` → `prepare-test` (build languages, set up dirs, fetch built-in langs) → `test-all` → `cleanTestingData`
- **`test-all`** — sounds like the top-level entry point, but is actually just the test execution phase (assumes `prepare-test` has already run)

**Recommended rename:**

| Current            | Suggested                 | Rationale                                                                  |
| ------------------ | ------------------------- | -------------------------------------------------------------------------- |
| `test-main`        | `test`                    | Standard npm convention for the full "run everything from scratch" command |
| `test-all`         | `test-run` or `test-only` | Makes clear it skips setup and just executes the suites                    |
| `test-all:windows` | `test-run:windows`        | Consistent with above                                                      |

This follows the common pattern of `pnpm test` being the CI-safe full lifecycle command, and `pnpm test-run` being the faster "already prepared" shortcut for local development iteration.

## Script ordering (low priority)

The current order is "entry points first" (`test-main`/`test-all` at the top, `prepare-test` buried near the bottom). A "workflow order" alternative would put `prepare-test` scripts first, followed by individual test scripts, then combined runners, then the top-level lifecycle wrappers — making the file read like a workflow from top to bottom.

However, JSON doesn't support comments so you can't add section headers either way. The naming fix above (`test-main` → `test`, `test-all` → `test-run`) will make the hierarchy obvious regardless of order, making reordering a very low priority cosmetic change.

## Setup pattern inconsistency — migrate remaining files to `startAgent` (medium priority)

`helpers/executor.ts` exports a clean `startAgent(agentName)` helper that handles dynamic port allocation, directory creation, executor startup, agent generation, and teardown via `.stop()`. It was introduced during the `multi-user/` split but only applied to 6 of the 10 multi-user files.

**Still using the old raw `startExecutor` pattern** (verbose, hardcoded ports, manual kill loops):

- `tests/multi-user/multi-user-auth.test.ts`
- `tests/multi-user/multi-user-connect.test.ts`
- `tests/multi-user/multi-user-neighbourhood.test.ts`
- `tests/auth/app.test.ts`
- `tests/auth/authentication.test.ts`
- `tests/auth/email-verification.test.ts`
- `tests/integration/integration.test.ts` (intentional exception — needs full lifecycle control)

`multi-user-neighbourhood.test.ts` and `multi-user-multi-node.test.ts` are also legitimate exceptions since they need `runHcLocalServices()` for Holochain. The `auth/` files and `multi-user-auth`/`connect` are straightforward migrations.

The `startAgent` helper may also need a `token` option adding so auth tests (which pass a hardcoded admin token to `startExecutor`) can use it too.

## Stale `@ts-ignore` in `helpers/executor.ts` (quick fix)

`helpers/executor.ts` has two `// @ts-ignore — Apollo Client version mismatch between dependencies` comments before `new Ad4mClient(...)` calls. These were rendered unnecessary by the `pnpm.overrides` graphql-ws/react dedup fix. Remove them.

## Centralise `global.fetch = fetch` (low-medium priority)

Almost every standalone test file contains:

```typescript
import fetch from "node-fetch";
//@ts-ignore
global.fetch = fetch;
```

This can be extracted into a single `tests/setup.ts` file and loaded via mocha's `--require` flag (same pattern already used by `tests/model/hooks.ts`). All the individual imports and `@ts-ignore` lines can then be removed from each file.

Implementation:

1. Create `tests/setup.ts` containing just the fetch polyfill
2. Add `--require tests/setup.ts` to the shared mocha options in each script (or add it to a `.mocharc.cjs`)
3. Remove the per-file `import fetch` + `global.fetch` blocks

## HTML test reports via `mochawesome` (nice to have)

Mocha's default `spec` reporter is fine for terminals but produces no persistent output. `mochawesome` generates a self-contained HTML report with pass/fail visualisation, timing per test, and full error details — no custom UI to build.

Implementation:

1. `pnpm add -D mochawesome`
2. Add a `test-report` script that runs `test-run` (or `test-all`) with `--reporter mochawesome --reporter-options reportDir=test-reports,reportFilename=results`
3. Add `test-reports/` to `.gitignore`

## `integration/` sub-file naming convention (low priority)

The `integration/` folder mixes two kinds of files:

- `integration.test.ts` — the standalone orchestrator (runnable directly)
- `agent.ts`, `perspective.ts`, `runtime.ts`, etc. — suite modules (not standalone, export a function)

The `.ts` extension on suite modules makes them look like they might be runnable, which can be confusing. Options:

- Rename to `*.suite.ts` (e.g. `perspective.suite.ts`)
- Move suite modules into an `integration/suites/` subfolder
- Leave as-is and accept the convention is understood by context (lowest effort)
