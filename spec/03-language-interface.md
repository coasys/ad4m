# 3. Language Interface

## 3.1 Overview

A **Language** in AD4M is a plugin that implements one or more *capabilities* via a **flat export model**. Languages are modules — JavaScript/TypeScript running under Deno, or Rust compiled to `wasm32-unknown-unknown` — that export named functions at the module level. There are no adapter classes, no `create()` factory, no context parameter, and no callback registration.

Languages are the core extensibility mechanism — they define how data is stored, retrieved, and shared. Every piece of content in AD4M (including agents, neighbourhoods, and other languages) is accessed through a Language.

> **Normative source of truth:** The canonical interface definition lives in [`ad4m-lang.wit`](../docs-src/ad4m-lang.wit) (WIT format, ~770 lines). The companion document [`language-interface-spec.md`](../docs-src/language-interface-spec.md) provides prose explanation, examples, and lifecycle semantics. Where WIT and prose disagree, the WIT wins.

### The Two Directions

```
RUNTIME → LANGUAGE         (the runtime calls a function the Language exports)
    Used for: capability calls (perspectiveSyncSync, perspectiveCommit,
              expressionCreate, …), lifecycle (init, teardown),
              event delivery (handleHolochainSignal, handleTelepresenceSignal).

LANGUAGE → RUNTIME         (the Language calls a function the runtime provides)
    Used for: services (agentDid, agentSign, holochainCall, storageGet, …),
              event emission (emitPerspectiveDiff, emitTelepresenceSignal, …).
```

There is no third "callback registration" subsystem. When a Language has new data (diff, signal, etc.) to publish, it calls an `emit*` import; the runtime fans it out to subscribers. The Language never holds a reference to a runtime callback.

## 3.2 Lifecycle

### One Instance per Perspective

Every neighbourhood has its own unique link Language; cloning a Language and pointing two perspectives at the same instance would put them in the same neighbourhood. Per-perspective instantiation is the natural unit:

- **JS/Deno:** The runtime imports the Language module under a fresh module identity per perspective (cache-busted URL). Each perspective gets its own copy of every top-level `let` binding, closures, and state.
- **WASM:** The runtime instantiates a new WASM instance per perspective. Each instance has its own linear memory and `thread_local!` state.

Per-instance state therefore lives at module level (JS) or in `thread_local!` / `OnceCell` (Rust). No state must be threaded through method arguments.

### Init / Teardown

```
1. Runtime creates a fresh module instance for the perspective.
2. Runtime reads `name` and `version` (statically — BEFORE init).
3. Runtime calls `init()`. The Language fetches its context lazily via
   the languageAddress / languageSettings / storage imports and stashes
   anything it needs in module-level state.
4. Runtime calls capability functions for the lifetime of the perspective.
5. When the perspective is removed, runtime calls `teardown()` and
   discards the module instance.
```

`init()` takes **no arguments**. Runtime services (storage directory, custom settings, language address) are accessed via the `ad4m:host` import module.

### Required Lifecycle Exports

| Export | Returns | Description |
|--------|---------|-------------|
| `name` | `string` | Language name. **Statically discoverable** — read before `init()`. |
| `version` | `string` | Semver string. Same staticness rule. |
| `isPublic()` | `boolean` | Privacy hint: are expressions/links publicly readable on the network? Static. |
| `init()` | `Promise<void>` | Initialize per-instance state. No arguments. |
| `teardown()` | `Promise<void>` | Release resources. Called when the perspective is destroyed. |

## 3.3 Capability Discovery

The runtime determines what kind of Language a module is by **looking at which functions it exports**. There are no manifest files, no capability flags, no `supports_*()` queries.

- **JS:** The runtime checks `typeof module.perspectiveSyncSync === 'function'`, etc. Both shapes are accepted:
  - Top-level named exports (`export const perspectiveSyncSync = …`).
  - `export default` of an object whose keys are the flat names.
- **WASM:** The runtime inspects the WASM instance's export table for canonical function names.

A capability is "present" if and only if **all** its required exports are present. Partial implementations (e.g., `perspectiveSyncSync` without `perspectiveSyncRender`) are a load-time error.

The three perspective capabilities (`perspective-commit`, `perspective-query`, `perspective-sync`) are independently detected — a Language can export any subset.

### Capability Enum

The executor registers detected capabilities per Language. The canonical set from the Rust `Capability` enum:

```rust
pub enum Capability {
    ExpressionCreate,
    ExpressionGet,
    PerspectiveCommit,
    PerspectiveSync,
    PerspectiveRender,
    PerspectiveCurrentRevision,
    PerspectiveQuery,
    PeersLocal,
    PeersRemote,
    TelepresenceSetStatus,
    TelepresenceGetAgents,
    TelepresenceSendSignal,
    TelepresenceSendBroadcast,
    LanguageGetSource,
    HolochainSignal,
}
```

## 3.4 Expression Capability

Expression Languages store and retrieve content. A Language exports **either** `expressionCreate` (for Languages that mint new content) **or** `expressionAddressOf` (for read-only Languages where addresses are deterministically derived from content), or both. Capability presence distinguishes the two — no separate flag.

| Export | Parameters | Returns |
|--------|------------|---------|
| `expressionGet(address)` | `string` | `Promise<Expression \| null>` |
| `expressionCreate(content)` | `object` | `Promise<string>` (address) |
| `expressionAddressOf(content)` | `object` | `Promise<string>` |
| `isImmutableExpression(address)` | `string` | `boolean` (per-expression cache hint) |

`isImmutableExpression` is a **per-expression cache hint**: if a Language returns `true` for an address, the runtime caches that Expression aggressively and bypasses subsequent `expressionGet` calls. Languages MAY omit this export entirely (host treats as always-`false`), MAY return `true` only for provably immutable addresses, or MAY blanket-return `true` for content-addressed Languages.

### Expression UI

| Export | Returns |
|--------|---------|
| `expressionIcon()` | `string` (web component JS source) |
| `expressionConstructorIcon()` | `string` |
| `settingsIcon()` | `string` |

## 3.5 Perspective Capabilities (Commit / Query / Sync)

Perspective access is split into **three orthogonal capabilities**, each independently exported. A Language exports whichever subset it supports:

| Capability | Purpose | Exports |
|------------|---------|---------|
| `perspective-commit` | Write diffs into the shared state | `perspectiveCommit(diff)` |
| `perspective-query` | Answer reads without requiring a full local replica | `perspectiveQueryRun(request)`, `perspectiveQuerySupportedKinds()` |
| `perspective-sync` | Bidirectional full-replica CRDT convergence | `perspectiveSyncSync()`, `perspectiveSyncRender()`, `perspectiveSyncCurrentRevision()` |

And one peer-fabric interface:

| Capability | Purpose | Exports |
|------------|---------|---------|
| `peers` | Local-agent membership push-in, remote-agent enumeration | `peersSetLocal(agents)`, `peersRemote()` |

### Composition Patterns

The three perspective capabilities are genuinely orthogonal:

| Language Pattern | Exports |
|------------------|---------|
| Full-sync Neighbourhood (p-diff-sync) | `commit` + `query` + `sync` + `peers` |
| DM inbox (sender view) | `commit` |
| DM inbox (owner view) | `commit` + `query` + `sync` + `peers` |
| Read-only DHT-backed knowledge graph | `query` |
| Public wiki / forum | `commit` + `query` |
| Append-only archive / log | `commit` + `query` |

### 3.5.1 perspective-commit

| Export | Parameters | Returns |
|--------|------------|---------|
| `perspectiveCommit(diff)` | `PerspectiveDiff` | `Promise<void>` |

Fire-and-forget from the caller's point of view. No revision is returned — Languages that track revisions expose them via `perspectiveSyncCurrentRevision`.

**Signing is implicit.** `perspectiveCommit` takes no signer parameter. The runtime sets an ambient "acting agent" context before dispatching into any Language export. The Language signs via `agentSign` / `agentCreateSignedExpression` imports, which consult that context. See [§3.8 Ambient Acting-Agent Contract](#38-ambient-acting-agent-contract).

### 3.5.2 perspective-query

| Export | Parameters | Returns |
|--------|------------|---------|
| `perspectiveQuerySupportedKinds()` | — | `QueryKind[]` |
| `perspectiveQueryRun(request)` | `QueryRequest` | `Promise<QueryResponse>` |

`QueryRequest` is a tagged variant covering these kinds:

```typescript
enum QueryKind {
  ExpressionsByAuthor,   // Paginated by-author lookup
  ExpressionsAll,        // List all expressions, paginated
  LinkPattern,           // Pattern-match links by source/predicate/target/timestamps
  SPARQL,                // SPARQL 1.1 query string (RECOMMENDED)
  Prolog,                // Prolog query string (back-compat)
}
```

`QueryResponse` matches the request variant:
- `by-author` / `all` → `expressions: Expression[]`
- `link-pattern` → `links: LinkExpression[]`
- `sparql` → `sparql-results: string` (SPARQL 1.1 JSON Results format)
- `prolog` → `prolog-bindings: string` (JSON-encoded list of variable maps)

A Language MUST return an error with code `not-implemented` for any kind it did not advertise via `perspectiveQuerySupportedKinds`. In v1.0 **SPARQL 1.1 is the RECOMMENDED query language**; structured link-pattern queries are first-class; Prolog is supported for backwards compatibility.

**`perspective-query` does NOT imply the existence of a local replica.** A Language can answer queries against a remote DHT/SPARQL endpoint without replicating state locally.

### 3.5.3 perspective-sync

| Export | Parameters | Returns |
|--------|------------|---------|
| `perspectiveSyncSync()` | — | `Promise<PerspectiveDiff>` |
| `perspectiveSyncRender()` | — | `Promise<Perspective>` |
| `perspectiveSyncCurrentRevision()` | — | `Promise<string \| null>` |

The runtime calls `perspectiveSyncSync` on a timer; the Language fetches new diffs from its underlying transport, returns the most recent one, and **also** calls `emitPerspectiveDiff(diff)` for every diff it observes asynchronously (e.g., from `handleHolochainSignal`).

### 3.5.4 peers

| Export | Parameters | Returns |
|--------|------------|---------|
| `peersSetLocal(agents)` | `string[]` (DIDs) | `Promise<void>` |
| `peersRemote()` | — | `Promise<string[]>` (remote agent DIDs) |

`peers` is the membership fabric that `perspective-sync` and `telepresence` both depend on. The runtime pushes local agents **in** via `peersSetLocal` and pulls remote participants **out** via `peersRemote`.

**`peersSetLocal` is called at instance creation AND whenever a local agent joins or leaves the node.** It is not one-shot — a second user logging into the same node after the Language instance is loaded MUST cause a fresh `peersSetLocal` call. The Language uses this for:

- Routing incoming telepresence signals to the right local agent.
- Advertising local membership to remote peers via `peersRemote`.
- Excluding all local agents from its own `peersRemote` result.

**`peersSetLocal` is NOT used for commit signing** — that is handled by the ambient acting-agent context (§3.8).

## 3.6 Telepresence Capability

For real-time presence and signaling within a Neighbourhood:

| Export | Parameters | Returns |
|--------|------------|---------|
| `telepresenceSetOnlineStatus(status)` | `PerspectiveExpression` | `Promise<void>` |
| `telepresenceGetOnlineAgents()` | — | `Promise<OnlineAgent[]>` |
| `telepresenceSendSignal(remoteAgentDid, payload)` | `string, PerspectiveExpression` | `Promise<object>` |
| `telepresenceSendBroadcast(payload)` | `PerspectiveExpression` | `Promise<object>` |

Incoming signals are delivered via the `handleTelepresenceSignal` event handler (see §3.9).

## 3.7 Direct Messages (NOT a Capability)

**There is no `direct-message` capability in v1.0.** A DM "inbox" is just a Language exporting `perspective-commit` (for senders to drop messages) plus, for the owner's multi-device case, `perspective-sync` + `peers` (so the owner's other devices pull the backlog).

The recipient DID is baked into the source at template-clone time, so the Language's internal logic enforces per-caller access. This is enforced **inside** the Language, not at the spec level.

See the [Social Conventions document](../docs-src/ad4m-social-conventions.md) for the DM-as-inbox pattern, the `ad4m://inbox` predicate for inbox discovery, and the `ad4m://friend-of` predicate for friends-as-a-perspective.

## 3.8 Ambient Acting-Agent Contract

When the runtime calls any Language export, it first sets an internal "acting agent" context to the DID of the local agent on whose behalf the call is being made. Any `agentDid()` / `agentSign()` / `agentCreateSignedExpression()` import calls during that export invocation consult this context and return values scoped to that agent.

This is how multi-user nodes work: a single Language instance shared by a perspective with multiple local agents will see the acting agent change from call to call. Alice commits a diff, then Bob commits a diff — the same `perspectiveCommit` signs each diff with the correct key without a signer parameter.

**Language implementations MUST NOT cache the result of `agentDid()` across export calls.** The acting agent can differ on the next call.

**`peers.setLocal` is a different concept.** `setLocal` tells the Language about the *set of local agents on the node* (used for routing and remote membership). The ambient acting agent is *which one of those is currently acting*.

## 3.9 Event Handler Exports (Runtime → Language)

The runtime delivers asynchronous events by calling exports directly — no callback registration.

### Telepresence Signal

| Export | Parameters | Description |
|--------|------------|-------------|
| `handleTelepresenceSignal(payload, recipientDid?)` | `PerspectiveExpression, string?` | Incoming telepresence signal. `recipientDid` is set for directed signals; absent for broadcasts. |

### Lifecycle Hooks (Optional)

Resource-constrained hosts (browser, mobile) MAY call:

| Export | Description |
|--------|-------------|
| `onPause()` | Host is suspending; release timers and connections. |
| `onResume()` | Host is resuming. |
| `onMemoryPressure()` | Release caches. |

### Holochain Signal (Extension)

| Export | Parameters | Description |
|--------|------------|-------------|
| `handleHolochainSignal(dnaNick, agentDid, signalData)` | `string, string, string` | Signal from a registered Holochain DNA. Part of the Holochain extension, not core. |

## 3.10 Imports (Language → Runtime)

The runtime provides these services. JavaScript Languages import them from `@coasys/ad4m-ldk` (or `ad4m:host`); Rust Languages declare them as `extern "C"`.

### 3.10.1 Agent Identity

| Import | Returns |
|--------|---------|
| `agentDid()` | `string` (DID of current acting agent) |
| `agentSigningKeyId()` | `string` |
| `agentSign(data: Uint8Array)` | `Uint8Array` |
| `agentSignStringHex(data: string)` | `string` |
| `agentCreateSignedExpression(data)` | `Expression` |
| `agentGetAllLocalUserDids()` | `string[]` |
| `agentDidForUser(email: string)` | `string` |
| `agentCreateSignedExpressionForUser(email, data)` | `Expression` |

### 3.10.2 Language Context

| Import | Returns |
|--------|---------|
| `languageAddress()` | `string` (content-address hash of this Language) |
| `languageSettings()` | `string` (raw JSON of instance-specific settings) |
| `languageStorageDirectory()` | `string` — **legacy**, kept for backward compatibility. New Languages SHOULD use core KV storage or the optional file I/O extension. |

### 3.10.3 Persistent Key/Value Storage (Core)

Per-Language scoped key/value persistence. The runtime namespaces keys by `languageAddress()`. Values are arbitrary bytes (strings in JS); Languages serialize structured data themselves.

| Import | Parameters | Returns |
|--------|------------|---------|
| `storageGet(key)` | `string` | `string \| null` |
| `storagePut(key, value)` | `string, string` | `void` |
| `storageDelete(key)` | `string` | `void` |
| `storageListKeys(prefix?)` | `string?` | `string[]` |

This API is **core** — every compliant runtime provides it. Durability is best-effort: a runtime without persistent storage MAY implement this as in-memory-only.

### 3.10.4 Event Emission

The Language pushes events to the runtime; the runtime fans out internally.

| Import | Parameters | Description |
|--------|------------|-------------|
| `emitPerspectiveDiff(diff)` | `PerspectiveDiff` | New diff available. |
| `emitSyncStateChange(state)` | `string` | Sync state changed. |
| `emitTelepresenceSignal(payload, recipientDid?)` | `PerspectiveExpression, string?` | Forward incoming telepresence signal. |
| `emitSignal(data)` | `unknown` | General-purpose signal-bus emission. |

All `emit*` functions are fire-and-forget.

### 3.10.5 Runtime Utilities

| Import | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `hash(data)` | `string` | `string` | Canonical AD4M content-address hash: **SHA-256 → CIDv1 (DAG-Protobuf) → base58btc**, prefixed with `"Qm"`. Runtimes MUST produce identical output for identical input. |

### 3.10.6 Holochain Extension (Optional)

Languages that use Holochain as their transport import these. Runtimes without Holochain simply omit the extension.

| Import | Returns | Description |
|--------|---------|-------------|
| `holochainRegisterDnas(dnas)` | `AppInfo[]` | Register DNA bundles. Runtime records DnaHash → instance for signal routing. |
| `holochainCall(dnaNick, zome, fnName, params)` | `unknown` | Single zome call (synchronous FIFO queue). |
| `holochainCallAsync(calls, timeoutMs?)` | `unknown[]` | Batched parallel zome calls. Read-only operations only. |

### 3.10.7 Storage File I/O Extension (Optional)

Raw read/write access to a filesystem-like storage layer. Paths are opaque strings; the runtime maps them to a backend.

| Import | Parameters | Returns |
|--------|------------|---------|
| `readStorageFile(path)` | `string` | `string` (UTF-8) |
| `writeStorageFile(path, content)` | `string, string` | `void` |

Use only when core KV cannot express the needed semantics (custom layouts, large blobs, shared paths). Runtimes not supporting this extension throw a clear error at call time.

## 3.11 `ad4m:host` Module and Host Contract

The `ad4m:host` ES module (`rust-executor/src/js_core/host.js`) exposes all runtime imports to Languages. It delegates to three core globals that the host environment MUST install:

| Global | Purpose |
|--------|---------|
| `globalThis.AGENT` | Agent identity and signing operations |
| `globalThis.LANGUAGE_CONTROLLER` | Language context, event dispatch, storage |
| `globalThis.UTILS` | Runtime utilities (canonical hash function) |

Optional extension globals:

| Global | Purpose |
|--------|---------|
| `globalThis.__holochainDelegate__` | Holochain DNA management and zome calls (per-Language) |

The host contract specifies:
1. Core globals MUST be installed before any Language is loaded.
2. The `ad4m:host` module MUST be registered so `import { ... } from "ad4m:host"` resolves.
3. Holochain extension is installed per-Language, just before `init()`.
4. Language's `init()` is called.

See [`host-contract.md`](../docs-src/host-contract.md) for the complete host contract specification.

## 3.12 AD4M Language Development Kit (ALDK)

The ALDK provides ergonomic authoring without sacrificing the flat export model. Two implementations exist:

### 3.12.1 JavaScript ALDK (`@coasys/ad4m-ldk`)

`defineLanguage(spec)` takes a grouped object (one nested sub-object per capability) and returns an object whose keys are the flat exported names:

```js
import { defineLanguage, agentDid, holochainCall,
         emitPerspectiveDiff } from '@coasys/ad4m-ldk';

let myDid;

const lang = defineLanguage({
    name: "@coasys/note-store",
    version: "1.0.0",
    isPublic: () => false,

    async init() {
        myDid = agentDid();
    },
    teardown() { /* ... */ },

    expression: {
        async create(content) { return await holochainCall(/*...*/); },
        async get(address)    { return await holochainCall(/*...*/); },
    },

    commit: {
        async commit(diff) { await holochainCall(/*...*/); },
    },

    sync: {
        async sync()          { /* ... */ },
        async render()        { /* ... */ },
        currentRevision: async () => null,
    },

    peers: {
        setLocal(agents) { /* ... */ },
        async remote()   { return []; },
    },

    handleHolochainSignal(signal) {
        if (signal.payload?.kind === "diff") {
            emitPerspectiveDiff(signal.payload.diff);
        }
    },
});

// Ship as named exports or default export:
export const { name, version, init, teardown, expressionCreate, expressionGet,
               perspectiveCommit, perspectiveSyncSync, perspectiveSyncRender,
               perspectiveSyncCurrentRevision, peersSetLocal, peersRemote,
               handleHolochainSignal } = lang;
```

`defineLanguage` is a **pure transform** — it renames grouped methods to flat canonical names. It does not create state, register anything, or call the runtime.

### 3.12.2 Rust ALDK (`ad4m-ldk` crate)

Capabilities are traits. The Language author implements one trait per capability and lists them in the `ad4m_language!` macro, which emits `#[no_mangle] extern "C"` shims **only** for listed capabilities:

```rust
use ad4m_ldk::prelude::*;

struct NoteStore;

impl Language for NoteStore {
    const NAME: &'static str = "@coasys/note-store";
    const VERSION: &'static str = "1.0.0";
    const IS_PUBLIC: bool = false;

    fn init() { /* ... */ }
}

impl ExpressionCapability for NoteStore {
    fn expression_create(&mut self, content: serde_json::Value) -> LanguageResult<Address> { /* ... */ }
    fn expression_get(&mut self, address: Address) -> LanguageResult<Option<Expression>> { /* ... */ }
}

impl PerspectiveCommitCapability for NoteStore {
    fn perspective_commit(&mut self, diff: PerspectiveDiff) -> LanguageResult<()> { /* ... */ }
}

impl PerspectiveSyncCapability for NoteStore {
    fn perspective_sync_sync(&mut self) -> LanguageResult<PerspectiveDiff> { /* ... */ }
    fn perspective_sync_render(&mut self) -> LanguageResult<Perspective> { /* ... */ }
    fn perspective_sync_current_revision(&mut self) -> LanguageResult<Option<String>> { None }
}

impl PeersCapability for NoteStore {
    fn peers_set_local(&mut self, _agents: Vec<String>) -> LanguageResult<()> { Ok(()) }
    fn peers_remote(&mut self) -> LanguageResult<Vec<String>> { Ok(vec![]) }
}

ad4m_language! {
    NoteStore {
        capabilities: [Expression, PerspectiveCommit, PerspectiveSync, Peers],
    }
}
```

The Rust ALDK provides a `State<T>` helper for per-instance state via `thread_local!` / `OnceCell`, and declares all runtime imports as safe Rust wrappers over `extern "C"`.

### WASM ABI

The WASM boundary uses JSON-marshalled null-terminated UTF-8 strings (`*const c_char` for inputs, `*mut c_char` for outputs). Primitive scalars pass directly. The ALDK hides all marshalling — Language authors only see Rust types.

## 3.13 WIT Worlds

The WIT file defines several worlds for different Language requirements:

| World | Description |
|-------|-------------|
| `ad4m-language-minimal` | Just lifecycle + core imports. For tests and templates. |
| `ad4m-language` | Canonical world. Lists every capability as an export; host introspects actual exports. |
| `ad4m-language-holochain` | Adds `holochain-ext` import and `holochain-events` export. |
| `ad4m-language-fs` | Adds `storage-fs-ext` import for file I/O. |
| `ad4m-language-holochain-fs` | Both Holochain and file I/O extensions. |

A Language declares which extensions it needs by targeting the matching world; runtimes that don't ship an extension refuse to load Languages that need it.

## 3.14 Language Metadata

Languages are registered with metadata:

```typescript
interface LanguageMeta {
  address: string;
  author: string;
  description?: string;
  name: string;
  possibleTemplateParams?: string[];
  sourceCodeLink?: string;
  templateAppliedParams?: string;
  templateSourceLanguageAddress?: string;
  templated?: boolean;
}
```

### Language Templating

Languages can be **templated** — a base Language is instantiated with parameters to create a new Language. This is how link Languages are created for new Neighbourhoods: the p-diff-sync template is instantiated with a new Holochain DNA, producing a unique Language for that Neighbourhood.

## 3.15 Other Capabilities

### Language Source

For the Language Language (stores other Languages' source bundles):

| Export | Parameters | Returns |
|--------|------------|---------|
| `languageGetSource(address)` | `string` | `Promise<string>` |

### Interactions

Languages can define user-invocable actions on expressions:

| Export | Parameters | Returns |
|--------|------------|---------|
| `interactions(address)` | `string` | `Interaction[]` |

```typescript
interface Interaction {
  label: string;
  name: string;
  parameters: InteractionParameter[];
}

interface InteractionParameter {
  name: string;
  type: string;
}
```

## 3.16 Design Decisions

- **Capability flags / manifest files** — capability is determined exclusively by export presence.
- **Callback registration** — no `addCallback`, no `removeCallback`. Languages emit events via `emit*` imports.
- **No factory function** — Languages are per-perspective module instances; runtime services are accessed via `ad4m:host` imports.
- **No `DirectMessageAdapter`** — DM is a composition pattern over existing capabilities (see §3.7).
- **`this` pointer in JavaScript** — all exports are top-level functions; state lives in module-level bindings.
