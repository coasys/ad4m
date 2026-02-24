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
| integration/ folder created            | `tests/integration.test.ts` + bare `.ts` suite modules at top level | `tests/integration/` subfolder |
| auth/ folder created                   | `tests/app.test.ts`, `tests/authentication.test.ts`, `tests/email-verification.test.ts` | `tests/auth/` |
| sdna/ folder created                   | `tests/sdna.test.ts`, `tests/smart-literal.test.ts` | `tests/sdna/` |
| multi-user/ folder created             | `tests/multi-user*.test.ts` at top level | `tests/multi-user/` subfolder |
| multi-user.test.ts renamed             | `tests/multi-user.test.ts`             | `tests/multi-user/multi-user-auth.test.ts`   |
| Dynamic ports for sdna tests           | Hardcoded 16600-16602                  | `startAgent` dynamic ports                   |

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
├── model/                               ← ✅ done
│   ├── hooks.ts
│   ├── model-core.test.ts
│   ├── model-query.test.ts
│   ├── model-subscriptions.test.ts
│   ├── model-transactions.test.ts
│   ├── model-inheritance.test.ts
│   ├── model-prolog.test.ts
│   ├── model-where-operators.test.ts
│   ├── model-from-json-schema.test.ts
│   └── model-advanced.test.ts
│
├── sdna/                                ← ✅ done
│   ├── sdna.test.ts
│   └── smart-literal.test.ts
│
├── integration/                         ← ✅ done
│   ├── integration.test.ts
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
├── auth/                                ← ✅ done
│   ├── app.test.ts
│   ├── authentication.test.ts
│   └── email-verification.test.ts
│
├── multi-user/                          ← folder done ✅; split TODO
│   ├── multi-user-auth.test.ts          ← ✅ renamed from multi-user.test.ts
│   ├── multi-user-connect.test.ts       ← ✅ moved
│   ├── multi-user-simple.test.ts        ← moved; split into focused files below is TODO
│   ├── multi-user-config.test.ts        ← TODO: split from multi-user-simple.test.ts
│   ├── multi-user-isolation.test.ts     ← TODO: split from multi-user-simple.test.ts
│   ├── multi-user-sdna.test.ts          ← TODO: split from multi-user-simple.test.ts
│   ├── multi-user-profiles.test.ts      ← TODO: split from multi-user-simple.test.ts
│   ├── multi-user-neighbourhood.test.ts ← TODO: split from multi-user-simple.test.ts
│   ├── multi-user-subscriptions.test.ts ← TODO: split from multi-user-simple.test.ts
│   └── multi-user-notifications.test.ts ← TODO: split from multi-user-simple.test.ts
│
└── smoke.test.ts                        ← ✅ done
```

---

## Recommended order of work

### ~~Phase 1 — Extract from `sdna.test.ts`~~ ✅ DONE

### ~~Phase 2 — Move `integration/` cluster~~ ✅ DONE

### ~~Phase 4 — Consolidate `auth/` and `sdna/` folders~~ ✅ DONE

### Phase 3 — Split `multi-user-simple.test.ts` (high value)

The file is 3,700+ lines covering 10 distinct describe blocks. Each maps to a
separate file in `multi-user/`. All blocks share a single `before/after` that
starts one executor — splitting means each new file gets its own startup,
which is slower but gives proper isolation and makes failures easier to locate.

Describe blocks and target files:

| Lines | Block | Target file |
|-------|-------|-------------|
| 109–301 | `Multi-User Configuration` | `multi-user-config.test.ts` |
| 302–432 | `Basic Multi-User Functionality` (skipped) | include in config |
| 433–608 | `Perspective Isolation` | `multi-user-isolation.test.ts` |
| 609–682 | `Link Authoring and Signatures` | `multi-user-isolation.test.ts` |
| 683–801 | `Subject Creation and SDNA Operations` | `multi-user-sdna.test.ts` |
| 802–1350 | `Agent Profiles and Status` | `multi-user-profiles.test.ts` |
| 1351–2187 | `Multi-User Neighbourhood Sharing` | `multi-user-neighbourhood.test.ts` |
| 2188–2955 | `Multi-Node Multi-User Integration` | `multi-user-neighbourhood.test.ts` |
| 2956–3376 | `Perspective Subscriptions` | `multi-user-subscriptions.test.ts` |
| 3377–3689 | `Multi-User Notifications` | `multi-user-notifications.test.ts` |
