# AD4M Language Interface Refactor — Revised Design

*Created: 2026-04-07*
*Updated from original proposal based on team discussion*

---

## Core Revisions (from original proposal)

1. **No `capabilities` array** — capability is determined by function presence
2. **No WASM-specific framing** — we're simplifying JS languages now; WASM is future
3. **Delegates are imports** (passed in), not exports
4. **Context passed as JSON string** (simpler for both JS and future WASM)

---

## New Architecture

### Language Module Structure

```
Language Module (JS or future WASM)
├── IMPORTS (passed from executor at init)
│   ├── LanguageContext (JSON string, parsed by module)
│   │   ├── agent: AgentService
│   │   ├── storageDirectory: string
│   │   ├── customSettings: object
│   │   └── holochain: HolochainDelegate (for Holochain-backed languages)
│   └── SignalCallback (function for emitting signals)
│
├── EXPORTS (what language provides)
│   ├── name: string
│   ├── version: string (optional)
│   ├── init(contextJson: string): Promise<void>
│   ├── teardown?(): Promise<void>
│   │
│   ├── LINK SYNC CAPABILITY (if linkSyncSync exists)
│   │   ├── linkSyncSync(): Promise<PerspectiveDiff>
│   │   ├── linkSyncCommit(diff: PerspectiveDiff): Promise<string>
│   │   ├── linkSyncRender(): Promise<Perspective>
│   │   ├── linkSyncCurrentRevision(): Promise<string>
│   │   ├── linkSyncOthers(): Promise<DID[]>
│   │   ├── linkSyncWritable(): boolean
│   │   ├── linkSyncPublic(): boolean
│   │   └── linkSyncAddCallback(callback): void
│   │   └── linkSyncRemoveCallback?(callback): void
│   ├── EXPRESSION CAPABILITY (if expressionGet exists)
│   │   ├── expressionGet(address: Address): Promise<Expression | null>
│   │   ├── expressionCreatePublic(content: object): Promise<Address>
│   │   └── expressionAddressOf?(content: object): Promise<Address>
│   │
│   ├── TELEPRESENCE CAPABILITY (if telepresenceSetOnlineStatus exists)
│   │   ├── telepresenceSetOnlineStatus(status: PerspectiveExpression): Promise<void>
│   │   ├── telepresenceGetOnlineAgents(): Promise<OnlineAgent[]>
│   │   ├── telepresenceSendSignal(remoteAgentDid: string, payload: PerspectiveExpression): Promise<object>
│   │   ├── telepresenceSendBroadcast(payload: PerspectiveExpression): Promise<object>
│   │   └── telepresenceRegisterSignalCallback(callback): Promise<void>
│   │
│   ├── DIRECT MESSAGE CAPABILITY (if directMessageRecipient exists)
│   │   ├── directMessageRecipient(): DID
│   │   ├── directMessageStatus(): Promise<PerspectiveExpression | void>
│   │   ├── directMessageSendP2P(message: Perspective): Promise<PerspectiveExpression | void>
│   │   ├── directMessageSendInbox(message: Perspective): Promise<PerspectiveExpression | void>
│   │   ├── directMessageSetStatus(status: PerspectiveExpression): void
│   │   ├── directMessageInbox(filter?: string): Promise<PerspectiveExpression[]>
│   │   └── directMessageAddMessageCallback(callback): void
│   │
│   ├── LANGUAGE CAPABILITY (if languageGetSource exists)
│   │   └── languageGetSource(address: Address): Promise<string>
│   │
│   └── INTERACTIONS (if interactions exists)
│       └── interactions(expression: Address): Interaction[]
```

---

## Capability Detection

**Function presence = capability.** No separate declaration needed.

```typescript
// If these functions exist → has LinkSync capability
const hasLinkSync = typeof module.linkSyncSync === 'function' &&
                    typeof module.linkSyncCommit === 'function' &&
                    typeof module.linkSyncRender === 'function';

// If this function exists → has Expression capability
const hasExpression = typeof module.expressionGet === 'function';
```

**Mapping from TypeScript interfaces to function sets:**

| Interface | Required Functions |
|-----------|-------------------|
| LinkSyncAdapter | `linkSyncSync`, `linkSyncCommit`, `linkSyncRender`, `linkSyncCurrentRevision`, `linkSyncOthers`, `linkSyncWritable`, `linkSyncPublic`, `linkSyncAddCallback`, `linkSyncRemoveCallback` |
| ExpressionAdapter | `expressionGet`, `expressionCreatePublic` |
| TelepresenceAdapter | `telepresenceSetOnlineStatus`, `telepresenceGetOnlineAgents`, `telepresenceSendSignal`, `telepresenceRegisterSignalCallback` |
| DirectMessageAdapter | `directMessageRecipient`, `directMessageStatus`, `directMessageSendP2P`, `directMessageSendInbox`, `directMessageSetStatus`, `directMessageInbox`, `directMessageAddMessageCallback` |
| LanguageAdapter | `languageGetSource` |
| GetByAuthorAdapter | `getByAuthor` |
| GetAllAdapter | `getAll` |

---

## Error Handling

**Design: Union result type or exceptions with string messages**

For cross-boundary calls (JS↔WASM), complex error types don't work well. Two options:

**Option A: Union result (typed)**
```typescript
function linkSyncSync(): Promise<{ ok: PerspectiveDiff } | { error: string }>
```

**Option B: Exceptions with string message**
```typescript
async function linkSyncSync(): Promise<PerspectiveDiff> {
    try {
        // ... do work
    } catch (e) {
        throw new Error(`linkSyncSync failed: ${e.message || e}`);
    }
}
```

**Recommendation:** Option B (exceptions) for simplicity. Use try/catch in the executor to handle errors. For WASM, convert to/from string errors at the boundary.

---

## Context Passing

### For JS Languages (now)

Context passed as object, same as today (legacy create() pattern):
```javascript
await globalThis.languageConstructor({
    agent: agentProxy,
    storageDirectory: "...",
    customSettings: {...},
    Holochain: holochainDelegate,
    ad4mSignal: signalFunction
});
```

### For Flat Export Languages (new pattern)

Context passed as JSON string via `init()`:
```javascript
await mod.init(JSON.stringify({
    storageDirectory: "...",
    customSettings: {...},
    languageAddress: "...",
}));
```

Non-serializable delegates (agent, holochain, signal) are set on `globalThis` before `init()` is called:
- `globalThis.__agentProxy__` — agent service proxy
- `globalThis.__holochainDelegate__` — Holochain delegate
- `globalThis.__ad4mSignal__` — signal callback

This allows flat export languages to access delegates via globalThis while keeping the init() context serializable (WASM-compatible).

### For WASM Languages (future)

Same as flat export JS: context as JSON string, delegates via globalThis (imported from WASM host).

---

## Migration Plan

### Phase 1: Support Flat Exports (backward compatible) — ✅ DONE

**Files modified:**
1. `rust-executor/src/languages/language_runtime.rs:131` — pattern detection ✅
2. `rust-executor/src/js_core/language_bootstrap.js` — adapter wrapper ✅
3. `core/src/language/Language.ts` — update TypeScript types (TODO)

**Changes:**
- Detect flat exports vs legacy create() pattern ✅
- Flat export language: create adapter wrapper that maps flat functions → nested adapters ✅
- Legacy language: continue using existing create() pattern ✅
- Fixed missing mappings: `addressOf`, `setLocalAgents` ✅
- Run tests after each step

### Phase 2: WASM Delegate Design

**Deferred until actual WASM language is built.**

Will need:
- Executor-side import functions (Holochain, Agent, Signal)
- WASM host wrapper code
- Context serialization/deserialization

### Phase 3: TypeScript Types Update

Update `core/src/language/Language.ts` to reflect new flat pattern.

### Phase 4: Migrate Languages

1. `p-diff-sync` (reference implementation)
2. `agent-language`
3. `neighbourhood-language`
4. Other bootstrap languages

After all migrated, remove legacy create() support.

---

## Key Files

- `rust-executor/src/languages/language_runtime.rs` — where languageConstructor is captured
- `rust-executor/src/js_core/language_bootstrap.js` — initLanguage() function
- `rust-executor/src/js_core/agent_extension.js` — AGENT ops wrapper
- `rust-executor/src/js_core/holochain_service/` — HOLOCHAIN_SERVICE ops
- `core/src/language/Language.ts` — TypeScript interface definitions

---

## What's NOT in scope for now

- Test runner changes (mentioned in original doc, not needed for this work)
- Multiple backend delegates (Holochain is the only one right now)
- WASM implementation (Phase 2 deferred)