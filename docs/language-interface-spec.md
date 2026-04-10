# AD4M Language Development Kit — Interface Spec

**Version:** 0.8-draft
**Date:** 2026-04-10
**Status:** Draft — for discussion

> **Canonical interface definition lives in [`ad4m-lang.wit`](./ad4m-lang.wit).**
> That file is the normative source of truth — the type signatures, the
> operation names, the request/response shapes. This document explains the
> *why* and the *how*: lifecycle, semantics, examples, ALDK ergonomics,
> Holochain routing. Where prose and WIT disagree, the WIT wins.
>
> **Changes from 0.7:**
> - **Split `link-sync` into three independent capabilities**:
>   `perspective-commit` (write), `perspective-query` (read without
>   requiring a full local replica), `perspective-sync` (bidirectional
>   full-replica convergence). A Language exports whichever subset it
>   supports. Full-sync Neighbourhoods export all three; a DM inbox
>   exports commit (+ typically sync); a read-only DHT-backed knowledge
>   graph exports only query.
> - **Extracted `peers` interface** from link-sync. Contains
>   `peers.set-local` (runtime pushes local agent set) and `peers.remote`
>   (Language returns remote participant DIDs). Shared by
>   `perspective-sync` and `telepresence`.
> - **Deleted the `direct-message` capability.** DM is now just a
>   link-sync + telepresence composition with the recipient DID baked
>   into a template clone. See `docs/ad4m-social-conventions.md` for
>   the DM-as-inbox pattern and friends-as-perspective convention.
> - **Moved `is-public` to the `lifecycle` interface** as a static
>   privacy hint. `writable` deleted (redundant with export presence of
>   `perspective-commit`).
> - **`commit` is signatureless.** The runtime sets an ambient acting-
>   agent context before dispatching into any Language export, and the
>   Language signs via the `agent` import, which consults that context.
>   New §7 prose note documenting this contract.
> - Query interface renamed from `query` to `perspective-query` for
>   consistency with the other perspective capabilities.
>
> **Changes from 0.6:**
> - Coalesced all query operations into a single capability with a
>   tagged `request` / `response` variant. SPARQL is now the recommended
>   query language; Prolog is supported for back-compat but optional.
>   `getByAuthor`, `getAll`, `linkQuery`, `prologQuery`, `infer`, and
>   `supportsPrologQueries` are gone — folded into `perspective-query.run`.
> - Replaced `isImmutableExpression(address)` with `expression.character`,
>   then reverted to `isImmutableExpression` as a per-expression cache
>   hint.
> - Pointed at the WIT file as the source of truth.

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
    Used for: capability calls (perspectiveSyncSync, perspectiveCommit,
                                 expressionCreate, …),
              lifecycle (init, teardown),
              event delivery (handleHolochainSignal, handleTelepresenceSignal).

LANGUAGE → RUNTIME         (the language calls a function the runtime provides)
    Used for: services (agentDid, agentSign, holochainCall, storageGet, …),
              event emission (emitPerspectiveDiff, emitTelepresenceSignal, …).
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

- **JS:** the runtime checks `typeof module.perspectiveSyncSync === 'function'`
  etc. The bootstrap accepts both shapes:
  - Top-level named exports (`export const perspectiveSyncSync = …`).
  - `export default` of an object whose keys are the flat names (idiomatic
    when using the JS ALDK).
- **WASM:** the runtime inspects the wasm instance's export table and checks
  for the canonical exported function names.

A capability is "present" if and only if **all** its required exports are
present. Partial implementations (e.g. `perspectiveSyncSync` without
`perspectiveSyncRender`) are a load-time error.

Note that the three perspective capabilities (`perspective-commit`,
`perspective-query`, `perspective-sync`) are independently detected —
a Language can export any subset.

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

Implement these to be an Expression Language. A Language exports **either**
`expressionCreate` (for Languages that mint new content) **or**
`expressionAddressOf` (for read-only Languages where addresses are
deterministically derived from content), or both. Capability presence
distinguishes the two — no separate flag.

| Export | Parameters | Returns |
|---|---|---|
| `expressionGet(address)` | `string` | `Promise<Expression \| null>` |
| `expressionCreate(content)` | `object` | `Promise<string>` (address) |
| `expressionAddressOf(content)` | `object` | `Promise<string>` |
| `isImmutableExpression(address)` | `string` | `boolean` (per-expression cache hint) |

`isImmutableExpression` is a **per-expression cache hint**: if a Language
returns `true` for an address, the runtime caches that Expression
aggressively and bypasses subsequent `expressionGet` calls. Languages MAY
omit this export entirely (the host treats it as always-`false` and never
caches), MAY return `true` only for addresses it can prove immutable, or
MAY blanket-return `true` for content-addressed Languages where addresses
encode content hashes.

### 5.2 Perspective capabilities (commit / query / sync)

Perspective access is split into **three orthogonal capabilities**, each
independently exported. A Language exports whichever subset it supports:

| Capability | Purpose | Exports |
|---|---|---|
| `perspective-commit` | Write diffs into the shared state | `perspectiveCommit(diff)` |
| `perspective-query` | Answer reads without requiring a full local replica | `perspectiveQueryRun(request)`, `perspectiveQuerySupportedKinds()` |
| `perspective-sync` | Bidirectional full-replica CRDT convergence | `perspectiveSyncSync()`, `perspectiveSyncRender()`, `perspectiveSyncCurrentRevision()` |

And one peer-fabric interface the first and third usually pair with:

| Capability | Purpose | Exports |
|---|---|---|
| `peers` | Local-agent membership push-in, remote-agent enumeration pull-out | `peersSetLocal(agents)`, `peersRemote()` |

The three perspective capabilities are genuinely orthogonal:

- **`commit` without `sync`** is a **write-only drop box** — senders push
  diffs in and never observe the resulting state. The sender-side view
  of a DM inbox is exactly this.
- **`query` without `sync`** is a **remote-backed read interface** — a
  DHT, remote SPARQL endpoint, or archive that answers queries without
  the client needing a local replica. This is the original
  PerspectiveQuery intent made honest.
- **`sync` without `commit`** is a **read-only replicator** — unusual,
  but possible (e.g., a public broadcast feed that everyone converges
  on but only the owner writes to).

And composing them gives every real use case:

| Language pattern | Exports |
|---|---|
| Current full-sync Neighbourhood (p-diff-sync) | `commit` + `query` + `sync` + `peers` |
| DM inbox (sender view: drops a message) | `commit` |
| DM inbox (owner view: multi-device replica) | `commit` + `query` + `sync` + `peers` |
| Read-only DHT-backed knowledge graph | `query` |
| Public wiki / forum | `commit` + `query` |
| Append-only archive / log | `commit` + `query` |

Same source code, same exports — the **owner vs. sender** asymmetry
for a DM inbox is not spec-level. Both run the same Language; the
Language's internal logic rejects non-owner attempts to render/sync
based on the DID check it performs against the template-baked
recipient. Capability detection via export presence tells the runtime
what the Language *can* do; runtime behavior enforces *effective*
permissions per caller.

#### `perspective-commit`

| Export | Parameters | Returns |
|---|---|---|
| `perspectiveCommit(diff)` | `PerspectiveDiff` | `Promise<void>` |

`perspectiveCommit` is fire-and-forget from the caller's point of view.
No revision is returned — Languages that track a revision expose it
via `perspectiveSyncCurrentRevision`.

**Signing is implicit.** `perspectiveCommit` takes no signer parameter
because the runtime sets an ambient "acting agent" context before
dispatching into any Language export. The Language signs via
`agentSign` / `agentCreateSignedExpression` imports, which consult
that context and return values scoped to the current acting agent.
See §7 for the full contract.

#### `perspective-query`

| Export | Parameters | Returns |
|---|---|---|
| `perspectiveQuerySupportedKinds()` | — | `QueryKind[]` (statically advertises which kinds the Language serves) |
| `perspectiveQueryRun(request)` | `QueryRequest` | `Promise<QueryResponse>` |

`QueryRequest` is a tagged variant: `by-author` / `all` / `link-pattern` /
`sparql` / `prolog`. `QueryResponse` is the matching variant. See
[`ad4m-lang.wit`](./ad4m-lang.wit) `interface perspective-query` for
the exact shapes.

Crucially, **`perspective-query` does NOT imply the existence of a local
replica.** A Language can answer queries against a remote DHT/SPARQL
endpoint without ever replicating state locally. That's the whole
point of the split: earlier drafts conflated "query a local replica"
with "query the shared state," and the split makes the distinction
honest.

A Language that exports `perspective-sync` (and therefore has a local
replica) typically ALSO exports `perspective-query` to answer queries
against that replica. A Language that exports `perspective-query`
without `perspective-sync` is explicitly a remote-backed backend.

A Language MUST return an error with code `not-implemented` for any
kind it did not advertise via `perspectiveQuerySupportedKinds`. In v1.0
SPARQL 1.1 is the recommended query language; structured link-pattern
queries are first-class; Prolog is supported for back-compat.

#### `perspective-sync`

| Export | Parameters | Returns |
|---|---|---|
| `perspectiveSyncSync()` | — | `Promise<PerspectiveDiff>` |
| `perspectiveSyncRender()` | — | `Promise<Perspective>` |
| `perspectiveSyncCurrentRevision()` | — | `Promise<string \| null>` |

The runtime calls `perspectiveSyncSync` on a timer; the Language
fetches new diffs from its underlying transport (Holochain or
otherwise), returns the most recent one, and **also** calls
`emitPerspectiveDiff(diff)` for every diff it observes asynchronously
(e.g., from `handleHolochainSignal`).

Note the absence of `commit` here — writing is `perspective-commit`'s
job. A full-sync Language exports both.

#### `peers`

| Export | Parameters | Returns |
|---|---|---|
| `peersSetLocal(agents)` | `string[]` | `Promise<void>` |
| `peersRemote()` | — | `Promise<string[]>` (remote agent DIDs, excluding all local agents) |

`peers` is the membership fabric that `perspective-sync` and
`telepresence` both depend on. The runtime pushes the set of local
agents **in** via `peersSetLocal` and pulls the set of remote
participants **out** via `peersRemote`.

**`peersSetLocal` is called at instance creation AND whenever a local
agent joins or leaves the node.** It's not one-shot init — a second
user logging into the same node after the Language instance is
already loaded must cause a fresh `peersSetLocal` call. The Language
uses this for:

- Routing incoming telepresence signals to the right local agent.
- Advertising local membership to remote peers via `peersRemote`.
- Excluding all local agents from its own `peersRemote` result.

**`peersSetLocal` is NOT used for commit signing.** That's handled by
the runtime's ambient acting-agent context (§7). The "local agent
set" and the "currently acting agent" are distinct concepts.

**Why a separate interface?** Because a pure commit-only drop box or
pure query-only remote backend doesn't need `peers` at all. Making it
independent lets those Languages opt out. The spec does not enforce
that `perspective-sync` or `telepresence` require `peers` — a Language
can export either without `peers` — but the behavior in that case is
degenerate (no multi-local-agent support, no remote enumeration).
Treat `peers` as a de-facto prerequisite for peer-fabric capabilities.

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

### 5.4 Direct Messages (NOT a capability)

**There is no `direct-message` capability in v1.0.** A DM "inbox" is
just a Language exporting `perspective-commit` (for senders to drop
messages) plus, for the owner's multi-device case,
`perspective-sync` + `peers` (so the owner's other devices pull the
backlog).

A DM inbox Language is a **template**: the owner's DID is baked into
the source at clone time, so the Language's internal logic knows
exactly one agent is allowed to `perspectiveSyncSync` / `render`, and
everyone else gets commit-only behavior. This is enforced inside the
Language, not at the spec level.

**Online fast-path** is handled by `telepresence.sendSignal` — when the
recipient is online, the sender also (or instead) pushes the message
through telepresence for instant delivery. **Offline delivery** falls
through to `perspective-commit`, and the sender's node / friend relay
/ DHT holds it until the recipient's node runs sync.

> **Runtime divergence:** The AD4M runtime retains a flat
> `direct-message` capability (`dmSendMessage`, `dmInbox`, etc.) and
> its `DirectMessageAdapter` dispatch path for backward compatibility
> with `direct-message-language`. This is a pragmatic runtime-level
> feature not reflected in the v1.0 spec or WIT. It will be removed
> once the social-layer conventions (`ad4m-social-conventions.md`) are
> implemented and deployed.

See `docs/ad4m-social-conventions.md` for the full pattern: the
well-known `ad4m://inbox` predicate for inbox discovery via the
agent's public perspective, the `ad4m://friend-of` predicate for
friends-as-a-perspective, and the friend-relay approach for offline
delivery.

### 5.5 Language Source

For Languages that store other languages (the Language Language).

| Export | Parameters | Returns |
|---|---|---|
| `languageGetSource(address)` | `string` | `Promise<string>` |

### 5.6 Icons & Settings UI

| Export | Returns |
|---|---|
| `expressionIcon()` | `string` (web component JS) |
| `expressionConstructorIcon()` | `string` |
| `settingsIcon()` | `string` |

### 5.7 Interactions

| Export | Parameters | Returns |
|---|---|---|
| `interactions(address)` | `string` | `Interaction[]` |

---

## 6. Event handler exports (RUNTIME → LANGUAGE for asynchronous events)

These are how the runtime delivers asynchronous events from the outside world
to the language. They are pure exports — no registration needed; the language
just defines them.

Extension-specific event handlers live alongside their extension, not in
the core. `handleHolochainSignal` is part of the **Holochain extension**
(see §8) and is only meaningful for Languages that import `holochain-ext`.

### 6.1 Telepresence signal

| Export | Parameters | Description |
|---|---|---|
| `handleTelepresenceSignal(payload, recipientDid?)` | `PerspectiveExpression, string?` | Incoming telepresence signal from another agent. `recipientDid` is set for directed signals; absent/null for broadcasts. |

Most Languages will receive incoming telepresence signals via their
underlying transport (e.g. inside `handleHolochainSignal` for Holochain
Languages) and forward them with `emitTelepresenceSignal`. This export
exists for Languages whose transport delivers telepresence signals through
a separate runtime path.

### 6.2 Lifecycle hooks (optional)

Resource-constrained hosts (browser, mobile) MAY call these to let the
Language pause work, release caches, etc.

| Export | Description |
|---|---|
| `onPause()` | Host is suspending the Language; release timers and connections. |
| `onResume()` | Host is resuming the Language. |
| `onMemoryPressure()` | Host is under memory pressure; release caches. |

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

#### Ambient acting-agent contract

`agentDid()` / `agentSign()` / `agentCreateSignedExpression()` return
values scoped to whichever local agent is **currently acting**. This
is not visible from the import signatures alone, so the contract is
stated explicitly here:

> When the runtime calls any Language export, it first sets an internal
> "acting agent" context to the DID of the local agent on whose behalf
> the call is being made. Any `agentDid()` / `agentSign()` /
> `agentCreateSignedExpression()` import calls made during that export
> invocation consult this context and return values scoped to that
> agent.

This is how multi-user nodes work. A single Language instance shared by
a perspective that multiple local agents participate in will see the
acting agent change from call to call — Alice commits a diff on her
behalf, then a moment later Bob commits a diff on his, and the same
`perspectiveCommit` implementation signs each diff with the correct
key without any signer parameter being threaded through.

**Language implementations MUST NOT cache the result of `agentDid()`
across export calls.** The acting agent can differ on the next call,
even for the same instance. If a Language needs to remember "my
primary agent" for some bootstrapping reason, it should do so during
`init()` — but per-call operations that produce signed output must
consult `agentDid()` / `agentSign()` fresh each time.

**`peers.setLocal` is a different concept.** `setLocal` tells the
Language about the *set of local agents that exist on this node*
(used for routing and remote membership advertisement). The ambient
acting agent is *which one of those local agents is currently
acting*. Both concepts are needed; neither substitutes for the other.

#### Multi-user imports

`agentGetAllLocalUserDids()`, `agentDidForUser(email)`, and
`agentCreateSignedExpressionForUser(email, data)` are the escape
hatch for rare cases where a Language needs to act on behalf of a
*specific* local user by identifier rather than "whoever is
currently acting." Most Languages never call these — they're for
server-ish components that batch-process work for multiple local
users at once.

### 7.2 Holochain

| Import | Returns | Description |
|---|---|---|
| `holochainRegisterDnas(dnas)` | `AppInfo[]` | Register DNA bundles. The runtime records the resulting DnaHashes against this language instance so it can route incoming signals back via `handleHolochainSignal` (§8). No callback parameter. |
| `holochainCall(dnaNick, zome, fnName, params)` | `unknown` | Single zome call. Underlying impl puts these into a sync FIFO queue. |
| `holochainCallAsync(calls, timeoutMs?)` | `unknown[]` | **Batched** parallel zome calls; `calls` is `{dnaNick, zome, fnName, params}[]`. Read-only operations only — concurrent writes will race the source chain. |

> **Runtime note:** The JS ALDK (`@coasys/ad4m-ldk`) and the runtime's
> `flat_wasm_imports.ts` both expose `holochainCallAsync` as a **single-call
> convenience wrapper** `(dnaNick, zome, fnName, params) → unknown` that
> delegates to the batch API internally. Languages needing true batch
> parallelism should call the underlying `__holochainDelegate__.callAsync`
> directly until the ALDK exposes a batch variant.

### 7.3 Language context

| Import | Returns |
|---|---|
| `languageStorageDirectory()` | `string` |
| `languageAddress()` | `string` |
| `languageSettings()` | `string` (raw JSON; the JS ALDK re-parses to an object for JS-authored languages) |

### 7.4 Persistent key/value storage

Per-Language scoped key/value persistence. The runtime namespaces every
key by `languageAddress()`, so two Language instances cannot read each
other's values even if they pick the same key. Values are arbitrary
strings — Languages serialize structured data themselves.

| Import | Parameters | Returns |
|---|---|---|
| `storageGet(key)` | `string` | `string \| null` |
| `storagePut(key, value)` | `string, string` | `void` |
| `storageDelete(key)` | `string` | `void` |
| `storageListKeys(prefix?)` | `string?` | `string[]` (un-namespaced — the language address scope is stripped before return) |

Storage operations are synchronous from the Language's perspective.
The runtime is free to back them with any persistent store; Languages
must not assume durability semantics beyond "writes are visible to
subsequent reads from the same Language instance."

### 7.5 Event emission (the language pushes events to the runtime)

The runtime fans out internally to whoever is subscribed; the language
doesn't track subscribers and doesn't hold callback references.

| Import | Parameters | Description |
|---|---|---|
| `emitPerspectiveDiff(diff)` | `PerspectiveDiff` | A new diff is available. Called on the polled path (from inside `perspectiveSyncSync` if you also want to emit early) and on the async path (from inside `handleHolochainSignal`). |
| `emitSyncStateChange(state)` | `string` | Sync state changed (`"Synced"` / `"NotSynced"` / etc.). |
| `emitTelepresenceSignal(payload, recipientDid?)` | `PerspectiveExpression, string?` | Forward an incoming telepresence signal to AD4M subscribers. |
| `emitSignal(data)` | `unknown` | General-purpose AD4M signal-bus emission. |

All `emit*` functions are fire-and-forget; they return immediately after the
runtime has enqueued the event for fan-out.

> **Current limitation:** `emitSignal(data)` currently only delivers signals
> whose payload can be deserialized as a `PerspectiveExpression`. Payloads
> with other shapes are logged as warnings and dropped. A dedicated
> `AD4M_SIGNAL_TOPIC` + GraphQL subscription is needed to support arbitrary
> signal payloads (tracked as a follow-up).

---

## 8. Holochain signal routing (Holochain extension)

`handleHolochainSignal` is **not** a core event handler. It is exported by
Languages that use the Holochain extension (`holochain-events` interface in
the WIT, exported from the `ad4m-language-holochain` world). Languages that
don't touch Holochain never implement it.

Per-DNA signal delivery is built without per-call callbacks.

```
1. Language calls holochainRegisterDnas(dnas) inside init().
2. The runtime resolves each DNA bundle to a DnaHash, installs it in the
   conductor, and records: DnaHash → this language instance.
3. Holochain emits a signal for some cell. The runtime receives it, looks
   up the DnaHash → instance map, and calls handleHolochainSignal(signal)
   on that instance. `signal` is an object:
   `{ cell_id: [dnaHash, agentPubkey], zome_name: string, payload: any }`.
4. The language parses signal.payload and decides what to emit:
       emitPerspectiveDiff(...)        if it's a link diff (including
                                       DM inbox commits — DMs are just
                                       diffs in v1.0)
       emitTelepresenceSignal(...)     if it's a telepresence signal
       emitSignal(...)                 anything else worth bus-publishing
```

A single language instance can register multiple DNAs and disambiguate them
via `signal.zome_name` inside `handleHolochainSignal`. Multiple language instances on
the same node never collide because each has its own DnaHash → instance entry.

> **Runtime divergence note:** The WIT definition (`ad4m-lang.wit`) specifies
> `handle-holochain-signal(dna-nick, agent-did, signal-data)` — three named
> arguments. The current JS runtime passes a single signal object
> `{ cell_id, zome_name, payload }` directly from the Holochain conductor.
> A future runtime version may decompose the object into the WIT's three-arg
> form for WASM languages; JS languages should accept either shape.

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

    isPublic: () => false,

    expression: {
        async create(content) {
            const address = await holochainCall("store", "store_zome", "put", content);
            return address;
        },
        async get(address) {
            return await holochainCall("store", "store_zome", "get", address);
        },
    },

    // perspective-commit capability
    commit: {
        async commit(diff) {
            await holochainCall("store", "sync_zome", "commit", diff);
        },
    },

    // perspective-sync capability
    sync: {
        async sync() {
            const diff = await holochainCall("store", "sync_zome", "pull", myDid);
            if (diff) emitPerspectiveDiff(diff);
            return diff;
        },
        async render() { /* ... */ },
        currentRevision: async () => null,
    },

    // peers capability
    peers: {
        setLocal(agents) { /* record the local agent set */ },
        async remote() { return []; },
    },

    handleHolochainSignal(signal) {
        // signal: { cell_id: [dnaHash, agentPubkey], zome_name: string, payload: any }
        if (signal.payload?.kind === "diff") {
            emitPerspectiveDiff(signal.payload.diff);
            emitSyncStateChange("Synced");
        }
    },
});

// Two equally valid ways to ship the language:

// (a) Explicit named flat exports — what the runtime introspects directly.
export const {
    name, version, isPublic, init, teardown,
    expressionCreate, expressionGet,
    perspectiveCommit,
    perspectiveSyncSync, perspectiveSyncRender, perspectiveSyncCurrentRevision,
    peersSetLocal, peersRemote,
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
- Walks the known capability sub-objects (`expression`, `commit`, `query`,
  `sync`, `peers`, `telepresence`) and renames their methods to the flat
  canonical names (`expression.create` → `expressionCreate`,
  `sync.sync` → `perspectiveSyncSync`, `commit.commit` → `perspectiveCommit`,
  `peers.setLocal` → `peersSetLocal`, …).
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
    const IS_PUBLIC: bool = false;

    fn init() {
        let storage = language_storage_directory();
        let my_did  = agent_did();
        let _dnas   = holochain_register_dnas(&[
            DnaSpec { nick: "store".into(), source: DnaSource::path("./store.dna") }
        ]);
        State::set(StateData { storage, my_did });
    }

    fn handle_holochain_signal(signal: &serde_json::Value) {
        // signal: { cell_id: [dnaHash, agentPubkey], zome_name, payload }
        if signal["payload"]["kind"] == "diff" {
            emit_perspective_diff(&signal["payload"]["diff"]);
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

impl PerspectiveCommitCapability for NoteStore {
    fn commit(diff: &PerspectiveDiff) {
        let _ = holochain_call("store", "sync", "commit", &json!(diff));
    }
}

impl PerspectiveSyncCapability for NoteStore {
    fn sync() -> PerspectiveDiff {
        let s = State::get();
        let diff: PerspectiveDiff = holochain_call("store", "sync", "pull", &json!(s.my_did)).into();
        emit_perspective_diff(&diff);
        diff
    }
    fn render() -> Perspective { /* … */ }
    fn current_revision() -> Option<String> { None }
}

impl PeersCapability for NoteStore {
    fn set_local(_agents: &[String]) { /* record local agent set */ }
    fn remote() -> Vec<String> { vec![] }
}

ad4m_language! {
    NoteStore {
        capabilities: [Expression, PerspectiveCommit, PerspectiveSync, Peers],
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
