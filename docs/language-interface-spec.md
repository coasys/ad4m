# AD4M Language Development Kit — Interface Spec

**Version:** 0.5-draft  
**Date:** 2026-04-09  
**Status:** Draft — for discussion

> **Changes from 0.4.1:** Added PerspectiveQuery exports (linkQuery / prolog).
> Defined telepresence signal delivery (`handleTelepresenceSignal`). Fixed
> `directMessageRecipient` semantics. Reconciled sync/async signatures with the
> existing `PerspectiveSyncAdapter`. Resolved the `linksTriggerCallback`
> question. Clarified that `holochainCallAsync` is batched. Added
> remove-callback exports. Documented how the old `LanguageInitContext` is now
> fetched via imports.

---

## The Two Directions

Every cross-boundary call is either:

```
LANGUAGE EXPORTS ← RUNTIME CALLS          (runtime → language)
    The runtime calls a function that the language provides.
    Example: handleHolochainSignal(dnaNick, data)
    Example: linkSyncSync()

LANGUAGE IMPORTS ← RUNTIME PROVIDES        (language → runtime)
    The language calls a function that the runtime provides.
    Example: agentDid()
    Example: linksTriggerCallback(diff)      ← language fires this to notify runtime
    Example: holochainCall(dnaNick, zome, fnName, params)
```

In JavaScript there is no separate registration step — the runtime hands the
language a function and the language calls it. In WASM, the language cannot
hold a `JsValue`, so the runtime registers a callback by passing an `i32`
identifier (e.g. via `linkSyncAddCallback(id)`); the language stores the id and
later invokes a `*TriggerCallback(id, …)` import which the host dispatches to
the JS function. Both flavors are described as plain function calls, not as a
distinct "callback registration" subsystem.

---

## Runtime Calls Language (Exports)

The runtime calls these functions that the language exports.

### Required

| Export | Parameters | Description |
|--------|-----------|-------------|
| `name` | — | `string` — language name |
| `version` | — | `string` — semver |
| `init()` | — | `Promise<void>` — initialize. No arguments. The context that used to be passed as `LanguageInitContext` (`storageDirectory`, `customSettings`, `languageAddress`) is now fetched lazily via the `languageStorageDirectory()` / `languageSettings()` / `languageAddress()` imports. |
| `teardown()` | — | `Promise<void>` — clean up |

### Holochain Signal Arrived

When a Holochain DNA emits a signal, the runtime calls this. The `dnaNick` identifies which DNA (a language can manage multiple DNAs).

| Export | Parameters | Description |
|--------|-----------|-------------|
| `handleHolochainSignal(dnaNick, signalData)` | `string, unknown` | Holochain DNA emitted a signal |

### Telepresence Signal Arrived

When another agent sends us a telepresence signal (point-to-point or broadcast),
the runtime calls this. Replaces the old `TelepresenceAdapter.registerSignalCallback`.

| Export | Parameters | Description |
|--------|-----------|-------------|
| `handleTelepresenceSignal(payload, recipientDid?)` | `PerspectiveExpression, string?` | Incoming signal from another agent. `recipientDid` is set for directed signals; absent/null for broadcasts. |

### Link Sync

| Export | Parameters | Returns | Description |
|--------|-----------|---------|-------------|
| `linkSyncSync()` | — | `Promise<PerspectiveDiff>` | Sync with network, return current diff |
| `linkSyncCommit(diff)` | `PerspectiveDiff` | `Promise<string>` | Commit a diff, return new revision hash |
| `linkSyncRender()` | — | `Promise<Perspective>` | Full perspective snapshot at current revision |
| `linkSyncCurrentRevision()` | — | `Promise<string\|null>` | Current revision hash |
| `linkSyncOthers()` | — | `Promise<string[]>` | Other synced agent DIDs |
| `linkSyncWritable()` | — | `boolean` | Whether accepting new links |
| `linkSyncPublic()` | — | `boolean` | Whether links are publicly readable |
| `linkSyncSetLocalAgents(agents)` | `string[]` | `void` | Register local agent DIDs |
| `linkSyncAddCallback(callbackId)` | `i32` | `void` | **WASM only.** Register a callback id; the language stores it and later invokes `linksTriggerCallback(callbackId, diff)`. In JavaScript, the runtime passes a function directly (see "How It Works"). |
| `linkSyncRemoveCallback(callbackId)` | `i32` | `void` | Unregister a previously registered link callback. |
| `linkSyncAddSyncStateChangeCallback(callbackId)` | `i32` | `void` | Register a sync-state-change callback id. |

> **Note on sync vs. async:** signatures match the existing
> `PerspectiveSyncAdapter` in `core/src/language/Language.ts`. Methods that
> historically returned `Promise` stay async — the WASM bridge returns a
> `Promise` from the host side even when the WASM function itself is
> synchronous.

### Expression

| Export | Parameters | Returns | Description |
|--------|-----------|---------|-------------|
| `expressionCreate(content)` | `object` | `Promise<string>` | Store expression, return address |
| `expressionGet(address)` | `string` | `Promise<Expression\|null>` | Retrieve expression |
| `expressionAddressOf(data)` | `object` | `string` | Deterministic address for data |
| `expressionIcon()` | — | `string` | Icon (base64 SVG or URL) |
| `expressionConstructorIcon()` | — | `string` | Constructor icon |
| `settingsIcon()` | — | `string` | Settings icon |

### Telepresence

| Export | Parameters | Returns | Description |
|--------|-----------|---------|-------------|
| `telepresenceSetOnlineStatus(status)` | `PerspectiveExpression` | `Promise<void>` | Set online status |
| `telepresenceGetOnlineAgents()` | — | `Promise<OnlineAgent[]>` | Get online agents |
| `telepresenceSendSignal(agentDid, payload)` | `string, PerspectiveExpression` | `Promise<object>` | Send signal to agent |
| `telepresenceSendBroadcast(payload)` | `PerspectiveExpression` | `Promise<object>` | Broadcast to all |

### Direct Message

A DM language instance is configured per peer. `directMessageRecipient()` returns
the DID of **the other agent** (whom we are DMing) — not the local agent.
`sendP2P` / `sendInbox` therefore take only the message; the recipient is
implicit in the language instance.

| Export | Parameters | Returns | Description |
|--------|-----------|---------|-------------|
| `directMessageRecipient()` | — | `string` | DID of the peer this DM language is configured for |
| `directMessageStatus()` | — | `Promise<PerspectiveExpression\|void>` | Get peer's DM status |
| `directMessageSendP2P(message)` | `PerspectiveExpression` | `Promise<PerspectiveExpression\|void>` | Send P2P DM to the configured recipient |
| `directMessageSendInbox(message)` | `PerspectiveExpression` | `Promise<PerspectiveExpression\|void>` | Send inbox DM to the configured recipient |
| `directMessageSetStatus(status)` | `PerspectiveExpression` | `Promise<void>` | Set local DM status |
| `directMessageInbox(filter?)` | `string?` | `Promise<PerspectiveExpression[]>` | Get inbox |
| `dmAddMessageCallback(callbackId)` | `i32` | `void` | **WASM only.** Register a DM callback id (JS passes a function directly). |
| `dmRemoveMessageCallback(callbackId)` | `i32` | `void` | Unregister a DM callback. |

### Language Source

| Export | Parameters | Returns |
|--------|-----------|---------|
| `languageGetSource(address)` | `string` | `Promise<string>` |

### Get-By-Author / Get-All

| Export | Parameters | Returns |
|--------|-----------|---------|
| `getByAuthor(author, count, page)` | `string, number, number` | `Promise<Expression[]\|null>` |
| `getAll(filter?, count, page)` | `any?, number, number` | `Promise<Expression[]\|null>` |
| `isImmutableExpression(address)` | `string` | `boolean` |

### Perspective Query (linkQuery / Prolog)

Replaces the old `PerspectiveQueryAdapter`. These let other languages and the
runtime read links from a Language without forcing a full sync — used for
back-links and Prolog queries.

| Export | Parameters | Returns | Description |
|--------|-----------|---------|-------------|
| `linkQuery(query)` | `LinkQuery` | `Promise<Perspective>` | Same semantic as `PerspectiveProxy.get(LinkQuery)` |
| `supportsPrologQueries()` | — | `boolean` | If `false`, the runtime falls back to running its own Prolog over the result of an all-`linkQuery`. |
| `infer(prologQuery)` | `string` | `Promise<any>` | Plain Prolog inference. Only called if `supportsPrologQueries()` is `true`. |
| `prologQuery(query)` | `string` | `Promise<Perspective>` | Returns a Perspective whose links are solutions to a Prolog query with unbound `Source`/`Predicate`/`Target`/`Author`/`Timestamp`. |

### Interactions

| Export | Parameters | Returns |
|--------|-----------|---------|
| `interactions(address)` | `string` | `Interaction[]` |

---

## Language Calls Runtime (Imports)

The language calls these functions that the runtime provides. Import from `ad4m:runtime` in JavaScript. In Rust/WASM they are `extern "C"` declarations.

### Agent Identity

| Import | Returns | Description |
|--------|---------|-------------|
| `agentDid()` | `string` | Current agent's DID |
| `agentSigningKeyId()` | `string` | Current signing key ID |
| `agentSign(data: Uint8Array)` | `Uint8Array` | Sign arbitrary bytes |
| `agentSignStringHex(data: string)` | `string` | Sign a hex string |
| `agentCreateSignedExpression(data)` | `object` | Create a signed expression |
| `agentGetAllLocalUserDids()` | `string[]` | All local user DIDs |
| `agentDidForUser(email: string)` | `string` | Get DID for a user |
| `agentCreateSignedExpressionForUser(email, data)` | `object` | Signed expression for a user |

### Holochain

| Import | Returns | Description |
|--------|---------|-------------|
| `holochainRegisterDnas(dnas)` | `AppInfo[]` | Register DNA bundles with the conductor. Holochain signals are delivered to the language via the `handleHolochainSignal` export — no per-registration callback. |
| `holochainCall(dnaNick, zome, fnName, params)` | `unknown` | Single zome call. Underlying impl puts these into a sync FIFO queue. |
| `holochainCallAsync(calls, timeoutMs?)` | `unknown[]` | **Batched** parallel zome calls. `calls` is `{dnaNick, zome, fnName, params}[]`. Read-only operations only, to avoid source-chain mutation races. |

### Language Context

| Import | Returns | Description |
|--------|---------|-------------|
| `languageStorageDirectory()` | `string` | Persistent storage path for this language instance |
| `languageAddress()` | `string` | This language's address on the network |
| `languageSettings()` | `string` | Custom settings as raw JSON. Returned as a string because the WASM boundary cannot pass arbitrary objects; the JS ALDK shim parses this and re-exposes it as an object to JS-authored languages. |

### Notify Runtime of Events

When the language produces data that the runtime needs to handle, it calls these.

| Import | Parameters | Description |
|--------|-----------|-------------|
| `linksTriggerCallback(diff)` | `PerspectiveDiff` | Notify runtime of new/received links. **Required** even though `linkSyncSync()` returns a diff: the language also fires this from inside `handleHolochainSignal` when a remote diff arrives asynchronously, and that path has no return value to thread through. |
| `linksTriggerSyncState(state)` | `string` | Notify runtime of sync state change ("Synced", "NotSynced", etc.) |
| `dmTriggerCallback(message)` | `PerspectiveExpression` | Notify runtime of new DM |
| `signalEmit(data)` | `unknown` | Emit to the AD4M signal bus |

> **Note:** The runtime handles `linksTriggerCallback` by invoking the perspective's registered link callback (set by the perspective proxy). In WASM, the callback id was previously handed to the language via `linkSyncAddCallback(id)`; the language passes that same id back through `linksTriggerCallback`, and the host looks it up.

---

## How It Works

### JavaScript — Direct Function Storage

In JavaScript, the language stores callback functions directly in its own state. The runtime doesn't need to register anything — when the language calls `linksTriggerCallback(diff)`, the runtime delivers the diff.

```javascript
class MyLanguage {
    _linkCallback = null;

    // Runtime calls this during perspective setup to give the language a callback
    linkSyncAddCallback(callback) {
        this._linkCallback = callback;
    }

    // When links change, the language notifies the runtime
    async linkSyncSync() {
        const diff = await this.fetchFromHolochain();
        if (diff && this._linkCallback) {
            this._linkCallback(diff);  // notify runtime
        }
        return diff;
    }
}
```

### Rust/WASM — i32 ID + Trigger Import

In WASM, the language can't store `JsValue`. Instead, the runtime registers a callback by passing an i32 ID. The language stores the ID and calls `linksTriggerCallback(diff)` when it has new links. The WASM host looks up the ID and invokes the actual JS function.

```rust
struct MyLanguage {
    link_cb_id: Option<i32>,
}

impl MyLanguage {
    // Runtime calls this to register a callback by ID
    fn linkSyncAddCallback(&mut self, callback_id: i32) {
        self.link_cb_id = Some(callback_id);
    }

    fn linkSyncSync(&mut self) -> JsValue {
        // When links change:
        let diff_json = serde_json::to_string(&diff).unwrap();
        // Language calls the trigger import → runtime invokes the JS callback
        self.link_cb_manager.trigger_links(&diff_json);
        JsValue::NULL
    }
}
```

The WASM host implements `linksTriggerCallback(diffJson)` by calling the JS callback that was registered with the same callback ID.

---

## Complete Data Flow: p-diff-sync Pattern

### Periodic sync (runtime → language → runtime)

```
1. Runtime timer fires, calls: language.linkSyncSync()
2. Language calls: holochainCall("sync", myDid)
3. Holochain returns diff
4. Language calls: linksTriggerCallback(diff)    ← language → runtime notification
5. Runtime delivers diff to perspective's link callback
```

### Holochain signal arrives (external → runtime → language → runtime)

```
1. Holochain DNA emits WebSocket signal
2. Runtime receives it, calls: language.handleHolochainSignal(dnaNick, signalData)
3. Language processes signal, may call: linksTriggerCallback(diff)
4. Runtime delivers diff to perspective's link callback
```

---

## JavaScript ALDK — `@ad4m/ldk`

### `defineLanguage()` — Nested Object Support

```javascript
import { defineLanguage } from '@ad4m/ldk';

export default defineLanguage({
    name: "@coasys/my-language",
    version: "1.0.0",

    init() {
        const storage = languageStorageDirectory();
        const langAddr = languageAddress();
    },

    // Lifecycle
    teardown() { },

    // Expression — nested
    expression: {
        async create(content) { /* ... */ },
        async get(address) { /* ... */ },
        addressOf(data) { /* ... */ },
    },

    // Link sync — nested
    links: {
        async sync() { /* ... */ },
        async commit(diff) { /* ... */ },
        async render() { return { links: [] }; },
        currentRevision() { return null; },
        others() { return []; },
        writable() { return true; },
        public() { return false; },
        setLocalAgents(agents) { /* ... */ },
        // Callback: runtime passes function directly, language stores it
        addCallback(cb) { this._linkCb = cb; },
        removeCallback(cb) { if (this._linkCb === cb) this._linkCb = null; },
        addSyncStateChangeCallback(cb) { this._stateCb = cb; },
    },

    // Telepresence — nested
    telepresence: {
        async setOnlineStatus(status) { /* ... */ },
        async getOnlineAgents() { return []; },
        async sendSignal(agentDid, payload) { /* ... */ },
        async sendBroadcast(payload) { /* ... */ },
        registerSignalCallback(cb) { this._signalCb = cb; },
    },

    // Direct message — nested. recipient() returns the PEER's DID
    // (this DM language instance is configured per peer).
    dm: {
        recipient() { return this._peerDid; },
        async status() { /* ... */ },
        async sendP2P(message) { /* ... */ },
        async sendInbox(message) { /* ... */ },
        async setStatus(status) { /* ... */ },
        async inbox(filter) { return []; },
        addMessageCallback(cb) { this._dmCb = cb; },
        removeMessageCallback(cb) { /* ... */ },
    },

    // Flat exports
    expressionIcon() { return ""; },
    expressionConstructorIcon() { return ""; },
    settingsIcon() { return ""; },
    interactions() { return []; },

    // External event handlers (runtime → language)
    handleHolochainSignal(dnaNick, signalData) { /* ... */ },
    handleTelepresenceSignal(payload, recipientDid) { /* ... */ },
});
```

---

## Rust ALDK — `ad4m-ldk` Crate

### Capability Traits

```rust
use wasm_bindgen::JsValue;

pub trait Language: Sized {
    const NAME: &'static str;
    const VERSION: &'static str;
    fn init(&mut self) { }
    fn teardown(&mut self) { }
}

// Callback registration traits — language stores i32 IDs
pub trait LinkSyncCallbacks: Language {
    fn linkSyncAddCallback(&mut self, callback_id: i32);
    fn linkSyncRemoveCallback(&mut self, callback_id: i32);
    fn linkSyncAddSyncStateChangeCallback(&mut self, callback_id: i32);
}

pub trait DirectMessageCallbacks: Language {
    fn dmAddMessageCallback(&mut self, callback_id: i32);
    fn dmRemoveMessageCallback(&mut self, callback_id: i32);
}

pub trait SignalCallbacks: Language {
    fn signalSetCallback(&mut self, callback_id: i32);
}

// Capability traits with default implementations
pub trait LinkSyncCapability: Language + LinkSyncCallbacks {
    fn linkSyncSync(&mut self) -> JsValue { JsValue::NULL }
    fn linkSyncCommit(&mut self, diff: &JsValue) -> Result<String, LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn linkSyncRender(&mut self) -> JsValue {
        JsValue::from_serde(&serde_json::json!({ "links": [] })).unwrap()
    }
    fn linkSyncCurrentRevision(&self) -> String { String::new() }
    fn linkSyncOthers(&mut self) -> Vec<String> { vec![] }
    fn linkSyncWritable(&self) -> bool { false }
    fn linkSyncPublic(&self) -> bool { false }
    fn linkSyncSetLocalAgents(&mut self, agents: &JsValue) { }
}

pub trait TelepresenceCapability: Language + SignalCallbacks {
    fn telepresenceSetOnlineStatus(&mut self, status: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn telepresenceGetOnlineAgents(&mut self) -> Result<Vec<JsValue>, LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn telepresenceSendSignal(&mut self, agent: &str, payload: &JsValue) -> Result<JsValue, LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn telepresenceSendBroadcast(&mut self, payload: &JsValue) -> Result<JsValue, LdkError> {
        Err(LdkError::new("not implemented"))
    }
}

pub trait DirectMessageCapability: Language + DirectMessageCallbacks {
    fn directMessageRecipient(&self) -> String { String::new() }
    fn directMessageStatus(&mut self) -> Result<JsValue, LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn directMessageSendP2P(&mut self, recipient: &str, data: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn directMessageSendInbox(&mut self, recipient: &str, data: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn directMessageSetStatus(&mut self, status: &JsValue) -> Result<(), LdkError> {
        Err(LdkError::new("not implemented"))
    }
    fn directMessageInbox(&mut self) -> Result<Vec<JsValue>, LdkError> {
        Err(LdkError::new("not implemented"))
    }
}

pub trait ExpressionCapability: Language {
    fn expressionCreate(&mut self, content: &JsValue) -> Result<String, LdkError>;
    fn expressionGet(&mut self, address: &str) -> JsValue;
    fn expressionAddressOf(&mut self, data: &JsValue) -> String;
}
```

### Callback Manager (Rust)

```rust
// The language stores i32 IDs. When it needs to fire a callback,
// it calls the trigger import and the WASM host dispatches to the JS function.

extern "C" {
    fn linksTriggerCallback(callback_id: i32, diff_json: *const c_char);
    fn linksTriggerSyncState(callback_id: i32, state: *const c_char);
    fn dmTriggerCallback(callback_id: i32, msg_json: *const c_char);
    fn signalTriggerCallback(callback_id: i32, data_json: *const c_char);
}

pub struct CallbackManager {
    ids: std::collections::HashMap<String, i32>,
}

impl CallbackManager {
    pub fn new() -> Self { Self { ids: std::collections::HashMap::new() } }

    pub fn set(&mut self, key: &str, id: i32) { self.ids.insert(key.to_string(), id); }
    pub fn get(&self, key: &str) -> Option<i32> { self.ids.get(key).copied() }

    pub fn trigger_links(&self, diff: &str) {
        if let Some(id) = self.get("link") {
            unsafe {
                let cstr = std::ffi::CString::new(diff).unwrap();
                linksTriggerCallback(id, cstr.as_ptr());
                std::ffi::CString::from_raw(cstr.into_raw());
            }
        }
    }
}
```

### Complete Rust Example

```rust
use wasm_bindgen::prelude::*;
use ad4m_ldk::prelude::*;

#[wasm_bindgen]
pub struct MyLanguage {
    link_cbs: CallbackManager,
}

#[wasm_bindgen]
impl MyLanguage {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self { Self { link_cbs: CallbackManager::new() } }
}

impl Language for MyLanguage {
    const NAME: &'static str = "@coasys/my-language";
    const VERSION: &'static str = "0.1.0";
    fn init(&mut self) {
        let storage = languageStorageDirectory();
    }
}

impl LinkSyncCallbacks for MyLanguage {
    fn linkSyncAddCallback(&mut self, callback_id: i32) {
        self.link_cbs.set("link", callback_id);
    }
    fn linkSyncRemoveCallback(&mut self, _callback_id: i32) {
        self.link_cbs.set("link", -1);
    }
    fn linkSyncAddSyncStateChangeCallback(&mut self, callback_id: i32) {
        self.link_cbs.set("sync_state", callback_id);
    }
}

impl LinkSyncCapability for MyLanguage {
    fn linkSyncSync(&mut self) -> JsValue {
        let diff = self.do_sync();
        self.link_cbs.trigger_links(&serde_json::to_string(&diff).unwrap());
        JsValue::NULL
    }
}

#[ad4m_language]
impl Language for MyLanguage { }
```

---

## Rust WASM Imports (what the runtime provides)

```rust
extern "C" {
    // Agent
    fn agentDid() -> *mut c_char;
    fn agentSign(payload: *const u8, len: usize) -> *mut c_char;
    fn agentSignStringHex(payload: *const c_char) -> *mut c_char;
    fn agentCreateSignedExpression(data: *const c_char) -> *mut c_char;
    fn agentGetAllLocalUserDids() -> *mut c_char;
    fn agentDidForUser(email: *const c_char) -> *mut c_char;
    fn agentCreateSignedExpressionForUser(email: *const c_char, data: *const c_char) -> *mut c_char;

    // Holochain
    fn holochainRegisterDnas(dnas_json: *const c_char);
    fn holochainCall(dna_nick: *const c_char, zome: *const c_char, fn_name: *const c_char, params: *const c_char) -> *mut c_char;
    fn holochainCallAsync(calls: *const c_char, timeout_ms: u32) -> *mut c_char;

    // Language context
    fn languageStorageDirectory() -> *mut c_char;
    fn languageAddress() -> *mut c_char;
    fn languageSettings() -> *mut c_char;

    // Notify runtime (language → runtime)
    fn linksTriggerCallback(callback_id: i32, diff_json: *const c_char);
    fn linksTriggerSyncState(callback_id: i32, state: *const c_char);
    fn dmTriggerCallback(callback_id: i32, msg_json: *const c_char);
    fn signalEmit(data: *const c_char);
}
```

---

## Resolved Decisions

1. **`linksTriggerCallback` is required.** `linkSyncSync()` returning a diff
   covers the polled case. The async case — a remote diff arriving via
   `handleHolochainSignal` — has no return value, so the language must be able
   to push the diff to the runtime out-of-band. We keep the trigger import for
   that path.

2. **Telepresence signals get their own export.** Incoming signals from other
   agents arrive via `handleTelepresenceSignal(payload, recipientDid?)`, not
   via `handleHolochainSignal`. The `recipientDid?` parameter distinguishes
   directed signals (set) from broadcasts (absent), preserving the old
   `TelepresenceSignalCallback` semantics.
