# Test Suite Restructuring Plan

Audit performed 2026-02-24. The `model/` folder is already well-organised and
serves as the target pattern for the rest of the suite.

---

## Changes already made

| What                                   | Before                                 | After                                        |
| -------------------------------------- | -------------------------------------- | -------------------------------------------- |
| Smoke test renamed                     | `tests/simple.test.ts`                 | `tests/smoke.test.ts`                        |
| fromJSONSchema moved into model/       | `tests/model-from-json-schema.test.ts` | `tests/model/model-from-json-schema.test.ts` |
| fromJSONSchema added to test-model run | —                                      | `test-model` script now includes it          |
| Shared executor for model suite        | 8 HC startups per `pnpm test-model`    | 1 HC startup via Root Hooks Plugin           |

---

## Shared executor pattern (Root Hooks Plugin)

Each test _group_ that runs multiple files in one `ts-mocha` invocation can
share a single executor across all of them. The pattern is:

### 1. Add a `hooks.ts` next to the test files

```typescript
// tests/<group>/hooks.ts
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";

let _sharedAgent: AgentHandle | null = null;

/** Returns the shared agent, or null if run without --require hooks.ts */
export function getSharedAgent(): AgentHandle | null {
  return _sharedAgent;
}

export const mochaHooks = {
  async beforeAll(this: Mocha.Context) {
    this.timeout(120_000);
    _sharedAgent = await startAgent("<group>-suite");
  },
  async afterAll() {
    if (_sharedAgent) {
      await _sharedAgent.stop();
      _sharedAgent = null;
    }
  },
};
```

### 2. Update each test file's before/after to use the fallback pattern

```typescript
import { getSharedAgent } from "./hooks.js";

let ownStop: (() => Promise<void>) | null = null;
let ad4m: Ad4mClient;

before(async () => {
  const shared = getSharedAgent();
  if (shared) {
    ad4m = shared.client; // running as part of the suite
  } else {
    const agent = await startAgent("file-name"); // standalone fallback
    ad4m = agent.client;
    ownStop = agent.stop;
  }
  // ... perspective setup
});

after(async () => {
  if (ownStop) await ownStop(); // no-op when using shared agent
});
```

### 3. Add `--require` to the npm script

```json
"test-<group>": "ts-mocha -p tsconfig.json --timeout 120000 --serial --exit --require tests/<group>/hooks.ts tests/<group>/*.test.ts"
```

### Which test groups are good candidates?

| Group                  | Should share? | Reason                                                         |
| ---------------------- | ------------- | -------------------------------------------------------------- |
| `model/`               | ✅ done       | `wipePerspective` gives per-test isolation; no first-run state |
| `sdna/` (future)       | ✅ yes        | Same pattern once extracted                                    |
| `auth/` (future)       | ❌ no         | `authentication.test.ts` tests a fresh unconfigured agent      |
| `multi-user/` (future) | maybe         | Depends on whether sub-files need clean agent state            |
| `integration/`         | ❌ no         | Already uses its own shared TestContext across sub-suites      |

---

## Current problem areas

### 1. Suite modules mixed with standalone test files

The bare `.ts` files at the top level (`agent.ts`, `perspective.ts`,
`runtime.ts`, etc.) are **not standalone tests**. They export a function that
`integration.test.ts` registers as a sub-suite under a shared `TestContext`.
They use no `.test.` suffix deliberately, but the lack of any folder separation
makes them visually indistinguishable from real test files.

### 2. `multi-user-simple.test.ts` is 3,700+ lines

The name is misleading — it is the _most_ comprehensive multi-user test file,
not the simplest. It covers at least four distinct concerns that should each
live in their own file.

### 3. `sdna.test.ts` still contains "Active record implementation"

The `describe("Active record implementation")` block tests `Recipe` model
features (local links, resolveLanguage, transform, long values, emoji handling)
that belong in `model/` rather than alongside SDNA/SHACL generation tests.
The batch operations tests within it are already fully covered by
`model-transactions.test.ts` and can be deleted.

---

## Proposed target structure

```
tests/
│
├── model/                               ← already clean
│   ├── models.ts
│   ├── model-core.test.ts
│   ├── model-query.test.ts
│   ├── model-subscriptions.test.ts
│   ├── model-transactions.test.ts
│   ├── model-inheritance.test.ts
│   ├── model-prolog.test.ts
│   ├── model-where-operators.test.ts
│   ├── model-from-json-schema.test.ts   ← done ✅
│   └── model-advanced.test.ts           ← TODO: extract from sdna.test.ts
│
├── sdna/                                ← TODO: new folder
│   ├── sdna.test.ts                     ← TODO: move + trim (decorators, getter, isInstance only)
│   └── smart-literal.test.ts           ← TODO: move
│
├── integration/                         ← TODO: new folder
│   ├── integration.test.ts              ← entry point, imports all suite modules below
│   ├── agent.ts
│   ├── agent-language.ts
│   ├── ai.ts
│   ├── direct-messages.ts
│   ├── expression.ts
│   ├── language.ts
│   ├── neighbourhood.ts
│   ├── perspective.ts
│   ├── runtime.ts
│   ├── social-dna-flow.ts
│   └── triple-agent-test.ts
│
├── auth/                                ← TODO: new folder
│   ├── app.test.ts
│   ├── authentication.test.ts
│   └── email-verification.test.ts
│
├── multi-user/                          ← TODO: new folder + split
│   ├── multi-user-auth.test.ts          ← rename from multi-user.test.ts
│   ├── multi-user-connect.test.ts
│   ├── multi-user-config.test.ts        ← split from multi-user-simple.test.ts
│   ├── multi-user-isolation.test.ts     ← split from multi-user-simple.test.ts
│   └── multi-user-sdna.test.ts         ← split from multi-user-simple.test.ts
│
└── smoke.test.ts                        ← done ✅
```

---

## Recommended order of work

### Phase 1 — Extract from `sdna.test.ts` (medium effort, high value)

Extract `describe("Active record implementation")` into
`model/model-advanced.test.ts`. This block tests:

- `local: true` property round-trip
- SurrealDB `condition` filter in `@HasMany`
- `resolveLanguage` round-trip (literal and non-literal)
- `transform` property option
- Very long property values
- `get()` / `getData()` completeness
- `findAll()` with resolved literal constraints
- Emoji and special character handling (Prolog UTF-8 pipeline)
- Subscription with emoji content

The batch operations test within it is a **duplicate** of
`model-transactions.test.ts` and should simply be deleted.

After extraction, `sdna.test.ts` becomes focused: SDNA generation/comparison,
SHACL decorator tests, getter feature tests, and isInstance filtering tests.

### Phase 2 — Move `integration/` cluster (low value, mechanical)

Move `integration.test.ts` + all bare `.ts` suite modules into an
`integration/` subfolder. Requires updating all relative import paths inside
the suite modules (`./integration.test` → `./integration.test` stays the same
since they're in the same folder, but the `package.json` script path changes).

Low priority — purely cosmetic.

### Phase 3 — Split `multi-user-simple.test.ts` (high value)

The file is 3,700+ lines covering:

- `describe("Multi-User Configuration")` — enable/disable, user listing, timestamps
- `describe.skip("Basic Multi-User Functionality")` — create/login/persistence/errors
- `describe("Perspective Isolation")` — per-user perspective isolation
- `describe("Link Authoring and Signatures")` — DID-based authorship
- `describe("Subject Creation and SDNA Operations")` — SDNA in multi-user context

Each of these maps directly to a separate file in `multi-user/`.

### Phase 4 — Consolidate `auth/` and `sdna/` folders (cosmetic)

Group `app.test.ts`, `authentication.test.ts`, `email-verification.test.ts`
into `auth/`.

Move `sdna.test.ts` and `smart-literal.test.ts` into `sdna/`.
