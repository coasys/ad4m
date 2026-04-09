# AD4M Language Development Kit — Interface Spec

**Version:** 0.6-draft
**Date:** 2026-04-09
**Status:** Draft — for discussion

> **Major rewrite from 0.5.** Removed all callback registration (`addCallback`,
> `registerSignalCallback`, callback ids). Replaced with `emit*` imports the
> language calls to push events to the runtime. Pinned the per-perspective
> instance lifecycle. Documented Holochain signal routing. Removed JS class
> examples. Added explicit capability-discovery section. JS and WASM now use
> exactly the same conceptual model — only the marshalling differs.

---

## 1. Concept

A language is a **module of flat exports**. The runtime calls the functions
the module exports; the module calls runtime-provided imports to do work and to
push events back to the runtime. There is no `create()` factory, no context
parameter, and no callback registration.

The same model applies to JavaScript languages running under Deno and to Rust
languages compiled to `wasm32-unknown-unknown`. The only differences are
marshalling at the boundary (objects on the JS side, JSON strings + raw
pointers on the WASM side) and the ergonomic helpers each ALDK provides.

### The two directions

```
RUNTIME → LANGUAGE         (the runtime calls a function the language exports)
    Used for: capability calls (linkSyncSync, expressionCreate, …),
              lifecycle (init, teardown),
              event delivery (handleHolochainSignal, handleTelepresenceSignal).

LANGUAGE → RUNTIME         (the language calls a function the runtime provides)
    Used for: services (agentDid, holochainCall, languageStorageDirectory, …),
              event emission (emitPerspectiveDiff, emitDirectMessage, …).
```

There is no third "callback registration" subsystem. Whenever the language has
a new diff / DM / signal to publish, it calls an `emit*` import; the runtime
fans it out to whoever is subscribed. The language never holds a reference to
a runtime callback.

---

## 2. Lifecycle

**One language module instance per perspective.** Every neighbourhood has its
own unique link language; cloning a language and pointing two perspectives at
the same instance would put them in the same neighbourhood. So per-perspective
instantiation is the natural unit, not an additional constraint.

- **JS/Deno:** the runtime imports the language module under a fresh module
  identity per perspective (cache-busted URL). Each perspective gets its own
  copy of every top-level `let` binding, its own closures from
  `defineLanguage`, its own everything.
- **WASM:** the runtime instantiates a new wasm instance per perspective. Each
  instance has its own linear memory and its own `thread_local!` state.

Per-instance state therefore lives at module level (JS) or in `thread_local!` /
`OnceCell` (Rust). No state has to be threaded through method arguments.

### Init / teardown

```
1. Runtime creates a fresh module instance for the perspective.
2. Runtime reads `name` and `version` (statically — before init).
3. Runtime calls `init()`. The module fetches its context lazily via the
   languageStorageDirectory / languageAddress / languageSettings imports
   and stashes anything it needs in module-level state.
4. Runtime calls capability functions for the lifetime of the perspective.
5. When the perspective is removed, runtime calls `teardown()` and discards
   the module instance.
```

`init()` takes **no arguments**. The old `LanguageInitContext` (storage
directory, custom settings, language address) is now fetched via imports.

---

## 3. Capability discovery

The runtime determines what kind of language a module is by **looking at which
functions it exports**. There are no manifest files, no capability flags, no
`supports_*()` queries.

- **JS:** the runtime checks `typeof module.linkSyncSync === 'function'` etc.
  The bootstrap accepts both shapes:
  - Top-level named exports (`export const linkSyncSync = …`).
  - `export default` of an object whose keys are the flat names (idiomatic
    when using the JS ALDK).
- **WASM:** the runtime inspects the wasm instance's export table and checks
  for the canonical exported function names.

A capability is "present" if and only if **all** its required exports are
present. Partial implementations (e.g. `linkSyncSync` without
`linkSyncCommit`) are a load-time error.

The Rust ALDK only emits `#[no_mangle] extern "C"` shims for capability traits
the language actually `impl`s — there are no defaulted no-op exports that
would falsely advertise capability.

---

## 4. Required exports

| Export | Returns | Description |
|---|---|---|
| `name` | `string` | Language name. **Statically discoverable** — the runtime reads this before `init()`. In Rust, an associated constant on the `Language` trait. |
| `version` | `string` | Semver. Same staticness rule. |
| `init()` | `Promise<void>` | Initialise per-instance state. Use the `language*()` and `agent*()` imports inside. No arguments. |
| `teardown()` | `Promise<void>` | Release resources. Called when the perspective is destroyed. |

---

## 5. Capability exports (RUNTIME → LANGUAGE)

### 5.1 Expression

Implement these to be an Expression Language. Either `expressionCreate` (for
languages that can mint new expressions) **or** `expressionAddressOf` (for
read-only languages that map content deterministically to an address) is
required; implement both only if you really mean it.

| Export | Parameters | Returns |
|---|---|---|
| `expressionGet(address)` | `string` | `Promise<Expression \| null>` |
| `expressionCreate(content)` | `object` | `Promise<string>` (address) |
| `expressionAddressOf(content)` | `object` | `Promise<string>` |
| `isImmutableExpression(address)` | `string` | `boolean` |

### 5.2 Link Sync (PerspectiveSyncAdapter)

Implement these to be a Link Language. The runtime calls `linkSyncSync` on a
timer; the language fetches new diffs from its underlying transport
(Holochain or otherwise), returns the most recent one, and **also** calls
`emitPerspectiveDiff(diff)` for every diff it observes asynchronously
(e.g. from `handleHolochainSignal`).

| Export | Parameters | Returns |
|---|---|---|
| `linkSyncSync()` | — | `Promise<PerspectiveDiff>` |
| `linkSyncCommit(diff)` | `PerspectiveDiff` | `Promise<string>` (new revision) |
| `linkSyncRender()` | — | `Promise<Perspective>` |
| `linkSyncCurrentRevision()` | — | `Promise<string \| null>` |
| `linkSyncOthers()` | — | `Promise<string[]>` (other agent DIDs) |
| `linkSyncWritable()` | — | `boolean` |
| `linkSyncPublic()` | — | `boolean` |
| `linkSyncSetLocalAgents(agents)` | `string[]` | `void` |

> **Note on `setLocalAgents`:** temporary hack to support multiple users on
> one node joining the same neighbourhood. Once each user gets their own
> language instance per neighbourhood (which the per-perspective lifecycle
> now enables), this can go away.

### 5.3 Telepresence

| Export | Parameters | Returns |
|---|---|---|
| `telepresenceSetOnlineStatus(status)` | `PerspectiveExpression` | `Promise<void>` |
| `telepresenceGetOnlineAgents()` | — | `Promise<OnlineAgent[]>` |
| `telepresenceSendSignal(remoteAgentDid, payload)` | `string, PerspectiveExpression` | `Promise<object>` |
| `telepresenceSendBroadcast(payload)` | `PerspectiveExpression` | `Promise<object>` |

The runtime calls `telepresenceSendSignal` / `telepresenceSendBroadcast` when
an AD4M client wants to send a signal — the language is the only thing that
knows how to actually transport it. **Incoming** signals from other agents
are delivered separately via the `handleTelepresenceSignal` event handler
(see §6).

### 5.4 Direct Message

A DM language is a **template**. Every agent has their own DM language, derived
by cloning a template and substituting the agent's DID into the source so that
`directMessageRecipient()` is a hard-coded literal:

```js
// In a cloned DM language:
export const directMessageRecipient = () => "did:key:z6MkjP…";
```

Because the recipient is baked in at clone time, `sendP2P` / `sendInbox` take
no recipient parameter — the instance is already configured for one specific
peer.

| Export | Parameters | Returns |
|---|---|---|
| `directMessageRecipient()` | — | `string` (the **peer's** DID, hard-coded) |
| `directMessageStatus()` | — | `Promise<PerspectiveExpression \| void>` |
| `directMessageSendP2P(message)` | `PerspectiveExpression` | `Promise<PerspectiveExpression \| void>` |
| `directMessageSendInbox(message)` | `PerspectiveExpression` | `Promise<PerspectiveExpression \| void>` |
| `directMessageSetStatus(status)` | `PerspectiveExpression` | `Promise<void>` |
| `directMessageInbox(filter?)` | `string?` | `Promise<PerspectiveExpression[]>` |

Incoming DMs are pushed to the runtime via `emitDirectMessage(message)` (§7),
typically from inside `handleHolochainSignal`.

### 5.5 Perspective Query (linkQuery / Prolog)

For Languages that allow other Languages and the runtime to read links without
forcing a full sync — used for back-links and Prolog queries.

| Export | Parameters | Returns |
|---|---|---|
| `linkQuery(query)` | `LinkQuery` | `Promise<Perspective>` |
| `supportsPrologQueries()` | — | `boolean` |
| `infer(prologQuery)` | `string` | `Promise<any>` |
| `prologQuery(query)` | `string` | `Promise<Perspective>` |

If `supportsPrologQueries()` returns `false`, the runtime falls back to
running its own Prolog over the result of an all-`linkQuery`.

### 5.6 Get-By-Author / Get-All

| Export | Parameters | Returns |
|---|---|---|
| `getByAuthor(author, count, page)` | `string, number, number` | `Promise<Expression[] \| null>` |
| `getAll(filter?, count, page)` | `any?, number, number` | `Promise<Expression[] \| null>` |

### 5.7 Language Source

For Languages that store other languages (the Language Language).

| Export | Parameters | Returns |
|---|---|---|
| `languageGetSource(address)` | `string` | `Promise<string>` |

### 5.8 Icons & Settings UI

| Export | Returns |
|---|---|
| `expressionIcon()` | `string` (web component JS) |
| `expressionConstructorIcon()` | `string` |
| `settingsIcon()` | `string` |

### 5.9 Interactions

| Export | Parameters | Returns |
|---|---|---|
| `interactions(address)` | `string` | `Interaction[]` |

---

## 6. Event handler exports (RUNTIME → LANGUAGE for asynchronous events)

These are how the runtime delivers asynchronous events from the outside world
to the language. They are pure exports — no registration needed; the language
just defines them.

### 6.1 Holochain signal

| Export | Parameters | Description |
|---|---|---|
| `handleHolochainSignal(dnaNick, agentDid, signalData)` | `string, string, unknown` | A signal arrived from a Holochain DNA the language registered. |

`dnaNick` identifies which DNA inside the language (a single language can
register multiple DNAs). `agentDid` identifies which **local** agent the
signal arrived for (matters for multi-user setups where one node holds several
local agents on the same neighbourhood). The language usually parses
`signalData` and forwards via `emitPerspectiveDiff`, `emitDirectMessage`, or
`emitTelepresenceSignal` depending on what kind of signal it was.

See §8 for how the runtime knows *which* language to deliver a given Holochain
signal to.

### 6.2 Telepresence signal

| Export | Parameters | Description |
|---|---|---|
| `handleTelepresenceSignal(payload, recipientDid?)` | `PerspectiveExpression, string?` | Incoming telepresence signal from another agent. `recipientDid` is set for directed signals; absent/null for broadcasts. |

(Note: most language implementations will receive these via Holochain and
actually fire them from inside `handleHolochainSignal` → `emitTelepresenceSignal`.
This export exists for languages whose transport delivers telepresence signals
through a separate runtime path.)

---

## 7. Imports (LANGUAGE → RUNTIME)

The runtime provides these. JavaScript languages import them from
`@coasys/ad4m-ldk` (or read them off `globalThis`); Rust languages declare them
as `extern "C"`.

### 7.1 Agent identity

| Import | Returns |
|---|---|
| `agentDid()` | `string` |
| `agentSigningKeyId()` | `string` |
| `agentSign(data: Uint8Array)` | `Uint8Array` |
| `agentSignStringHex(data: string)` | `string` |
| `agentCreateSignedExpression(data)` | `Expression` |
| `agentGetAllLocalUserDids()` | `string[]` |
| `agentDidForUser(email: string)` | `string` |
| `agentCreateSignedExpressionForUser(email, data)` | `Expression` |

### 7.2 Holochain

| Import | Returns | Description |
|---|---|---|
| `holochainRegisterDnas(dnas)` | `AppInfo[]` | Register DNA bundles. The runtime records the resulting DnaHashes against this language instance so it can route incoming signals back via `handleHolochainSignal` (§8). No callback parameter. |
| `holochainCall(dnaNick, zome, fnName, params)` | `unknown` | Single zome call. Underlying impl puts these into a sync FIFO queue. |
| `holochainCallAsync(calls, timeoutMs?)` | `unknown[]` | **Batched** parallel zome calls; `calls` is `{dnaNick, zome, fnName, params}[]`. Read-only operations only — concurrent writes will race the source chain. |

### 7.3 Language context

| Import | Returns |
|---|---|
| `languageStorageDirectory()` | `string` |
| `languageAddress()` | `string` |
| `languageSettings()` | `string` (raw JSON; the JS ALDK re-parses to an object for JS-authored languages) |

### 7.4 Event emission (the language pushes events to the runtime)

The runtime fans out internally to whoever is subscribed; the language
doesn't track subscribers and doesn't hold callback references.

| Import | Parameters | Description |
|---|---|---|
| `emitPerspectiveDiff(diff)` | `PerspectiveDiff` | A new diff is available. Called on the polled path (from inside `linkSyncSync` if you also want to emit early) and on the async path (from inside `handleHolochainSignal`). |
| `emitSyncStateChange(state)` | `string` | Sync state changed (`"Synced"` / `"NotSynced"` / etc.). |
| `emitDirectMessage(message)` | `PerspectiveExpression` | A new DM arrived (DM language). |
| `emitTelepresenceSignal(payload, recipientDid?)` | `PerspectiveExpression, string?` | Forward an incoming telepresence signal to AD4M subscribers. |
| `emitSignal(data)` | `unknown` | General-purpose AD4M signal-bus emission. |

All `emit*` functions are fire-and-forget; they return immediately after the
runtime has enqueued the event for fan-out.

---

## 8. Holochain signal routing

Per-DNA signal delivery is built without per-call callbacks.

```
1. Language calls holochainRegisterDnas(dnas) inside init().
2. The runtime resolves each DNA bundle to a DnaHash, installs it in the
   conductor, and records: DnaHash → this language instance.
3. Holochain emits a signal for some cell. The runtime receives it, looks
   up the DnaHash → instance map, and calls handleHolochainSignal(dnaNick,
   agentDid, signalData) on that instance. dnaNick is the same string the
   language passed to holochainRegisterDnas; agentDid is the local agent the
   cell belongs to.
4. The language parses signalData and decides what to emit:
       emitPerspectiveDiff(...)        if it's a link diff
       emitDirectMessage(...)          if it's a DM
       emitTelepresenceSignal(...)     if it's a telepresence signal
       emitSignal(...)                 anything else worth bus-publishing
```

A single language instance can register multiple DNAs and disambiguate them
via `dnaNick` inside `handleHolochainSignal`. Multiple language instances on
the same node never collide because each has its own DnaHash → instance entry.

---

## 9. JavaScript ALDK — `@coasys/ad4m-ldk`

The ALDK gives the language author **ergonomic grouped authoring** without
giving up flat exports. `defineLanguage` takes a grouped object (one nested
sub-object per capability — the JavaScript analogue of Rust's "one trait per
capability" pattern), and returns an object whose keys are the flat exported
names. Per-instance state lives in module-level `let` bindings (which are
naturally per-instance because the runtime imports the module fresh per
perspective).

### 9.1 Authoring example

```js
import {
    defineLanguage,
    agentDid, holochainCall, holochainRegisterDnas,
    languageStorageDirectory, languageSettings,
    emitPerspectiveDiff, emitSyncStateChange,
} from '@coasys/ad4m-ldk';

let storage;
let myDid;
let dnas;

const lang = defineLanguage({
    name: "@coasys/note-store",
    version: "1.0.0",

    async init() {
        storage = languageStorageDirectory();
        myDid = agentDid();
        dnas = holochainRegisterDnas([{ nick: "store", source: { type: "path", value: "./store.dna" } }]);
    },

    teardown() { /* ... */ },

    expression: {
        async create(content) {
            const address = await holochainCall("store", "store_zome", "put", content);
            return address;
        },
        async get(address) {
            return await holochainCall("store", "store_zome", "get", address);
        },
    },

    links: {
        async sync() {
            const diff = await holochainCall("store", "sync_zome", "pull", myDid);
            if (diff) emitPerspectiveDiff(diff);
            return diff;
        },
        async commit(diff) {
            return await holochainCall("store", "sync_zome", "commit", diff);
        },
        async render() { /* ... */ },
        currentRevision: async () => null,
        others: async () => [],
        writable: () => true,
        public: () => false,
        setLocalAgents(agents) { /* ... */ },
    },

    handleHolochainSignal(dnaNick, signalAgent, data) {
        if (dnaNick === "store" && data.kind === "diff") {
            emitPerspectiveDiff(data.diff);
            emitSyncStateChange("Synced");
        }
    },
});

// Two equally valid ways to ship the language:

// (a) Explicit named flat exports — what the runtime introspects directly.
export const {
    name, version, init, teardown,
    expressionCreate, expressionGet,
    linkSyncSync, linkSyncCommit, linkSyncRender,
    linkSyncCurrentRevision, linkSyncOthers,
    linkSyncWritable, linkSyncPublic, linkSyncSetLocalAgents,
    handleHolochainSignal,
} = lang;

// (b) Default-export the whole language record — also accepted by the bootstrap.
export default lang;
```

The bootstrap looks at top-level named exports first; if none are present and
`export default` is an object with the right keys, it uses that. Either style
gives identical observable behaviour.

### 9.2 What `defineLanguage` does

`defineLanguage(spec)` is a pure transform:

- Takes the grouped object.
- Walks the known capability sub-objects (`expression`, `links`, `telepresence`,
  `dm`, `query`) and renames their methods to the flat canonical names
  (`expression.create` → `expressionCreate`, `links.sync` → `linkSyncSync`, …).
- Passes lifecycle and event-handler exports (`name`, `version`, `init`,
  `teardown`, `handleHolochainSignal`, `handleTelepresenceSignal`) through
  unchanged.
- Returns the resulting flat object.

It does **not** create any state, register anything, or call the runtime.
State lives in the language module's own `let` bindings; the closures in the
grouped object capture them naturally.

### 9.3 Imports surface

The ALDK re-exports every runtime import (§7) as a typed function. Under the
hood each is a thin wrapper around the corresponding `globalThis.*` that the
JS bootstrap installs. Authors get full TypeScript types and never touch
`globalThis` directly.

---

## 10. Rust ALDK — `ad4m-ldk` crate

Capabilities are traits. The language author implements one trait per
capability and lists them in the `ad4m_language!` macro. The macro emits
`#[no_mangle] extern "C"` shims **only** for the listed capabilities — so
capability presence in the wasm export table truthfully reflects what the
language implements.

### 10.1 Authoring example

```rust
use ad4m_ldk::prelude::*;

struct NoteStore;

impl Language for NoteStore {
    const NAME: &'static str = "@coasys/note-store";
    const VERSION: &'static str = "1.0.0";

    fn init() {
        let storage = language_storage_directory();
        let my_did  = agent_did();
        let _dnas   = holochain_register_dnas(&[
            DnaSpec { nick: "store".into(), source: DnaSource::path("./store.dna") }
        ]);
        State::set(StateData { storage, my_did });
    }

    fn handle_holochain_signal(dna_nick: &str, agent_did: &str, data: &serde_json::Value) {
        if dna_nick == "store" && data["kind"] == "diff" {
            emit_perspective_diff(&data["diff"]);
            emit_sync_state_change("Synced");
        }
    }
}

impl ExpressionCapability for NoteStore {
    fn create(content: &serde_json::Value) -> String {
        holochain_call("store", "store_zome", "put", content).as_str().unwrap().to_string()
    }
    fn get(address: &str) -> Option<Expression> {
        holochain_call("store", "store_zome", "get", &json!(address)).into()
    }
}

impl LinkSyncCapability for NoteStore {
    fn sync() -> PerspectiveDiff {
        let s = State::get();
        let diff: PerspectiveDiff = holochain_call("store", "sync", "pull", &json!(s.my_did)).into();
        emit_perspective_diff(&diff);
        diff
    }
    fn commit(diff: &PerspectiveDiff) -> String { /* … */ }
    fn render() -> Perspective { /* … */ }
    fn current_revision() -> Option<String> { None }
    fn others() -> Vec<String> { vec![] }
    fn writable() -> bool { true }
    fn public() -> bool { false }
    fn set_local_agents(_agents: &[String]) {}
}

ad4m_language! {
    NoteStore {
        capabilities: [Expression, LinkSync],
    }
}
```

### 10.2 Per-instance state

Per-perspective isolation comes from one wasm instance per perspective (§2).
Inside that instance, state lives in a `thread_local!` `RefCell<Option<…>>`
(or `OnceCell`) that the language sets in `init()`. The ALDK provides a
`State<T>` helper that wraps this pattern:

```rust
ad4m_state! {
    StateData {
        storage: String,
        my_did: String,
    }
}
```

### 10.3 Import declarations

The ALDK declares every runtime import in one place as `extern "C"` and
exposes safe Rust wrappers. Languages never write `extern "C"` themselves.
Marshalling at the WASM boundary uses JSON strings via `*const c_char` for
anything more complex than primitive values.

---

## 11. WASM ABI notes

The WASM boundary cannot pass arbitrary objects, so the canonical encoding for
non-primitive arguments and return values is **JSON** marshalled through
null-terminated UTF-8 strings:

- Inputs: `*const c_char` (caller owns the buffer; the host copies before
  returning).
- Outputs: `*mut c_char` allocated inside the wasm instance via an exported
  allocator (`ad4m_ldk::alloc`); the host reads, copies, then calls a paired
  `ad4m_ldk::free` to release it.
- Primitive scalars (`i32`, `u32`, `f64`, `bool` as `i32`) pass directly.

The ALDK hides all of this. Language authors only see Rust types
(`String`, `serde_json::Value`, typed structs).

---

## 12. Things deliberately omitted

- **Capability flags / manifest files.** Capability is determined exclusively
  by export presence.
- **Callback registration.** No `addCallback`, no `removeCallback`, no
  callback ids. Languages emit events via `emit*` imports; the runtime
  handles fan-out.
- **`create(context)` factory.** Replaced by per-perspective module
  instantiation + lazy context fetch via imports.
- **A `this` pointer in JavaScript.** All exports are top-level functions;
  per-instance state lives in module-level bindings (which are themselves
  per-perspective).
