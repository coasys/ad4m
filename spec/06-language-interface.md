# 6. Language Interface

A **Language** is a plugin loaded into the executor that implements one or more *capabilities*: storing and retrieving Expressions, hosting a shared Perspective, providing peer discovery, etc.

This section specifies the **conceptual model** of the interface — the lifecycle, the capability discovery rule, the "what is on each side" boundary. The **normative function signatures** live in the WIT file [`../docs-src/ad4m-lang.wit`](../docs-src/ad4m-lang.wit); the WIT file is authoritative. This chapter intentionally does not duplicate signatures.

## 6.1 The flat-export model

A Language is a module that **exports named functions at the module level**:

- JavaScript/TypeScript modules run under Deno and export functions normally (`export function foo() {...}` or `export const foo = ...`).
- Rust crates compile to `wasm32-unknown-unknown` and expose functions through the WASM ABI.

The export surface *is* the interface. A capability is *detected* by introspecting the exports: if a Language exports the set of functions a capability defines, that Language is treated as supporting that capability ("presence = capability"). No separate manifest or capability flag is required.

## 6.2 The two directions

```text
RUNTIME → LANGUAGE      The executor calls a function the Language exports.
                        Used for: capability calls (perspectiveCommit,
                        expressionCreate, perspectiveQueryRun, …),
                        lifecycle (init, teardown), event delivery
                        (handleTelepresenceSignal, handleHolochainSignal).

LANGUAGE → RUNTIME      The Language calls a function the executor provides.
                        Used for: services (agentDid, agentSign, storageGet,
                        languageAddress, …), and event emission
                        (emitPerspectiveDiff, emitTelepresenceSignal, …).
```

When a Language has new data to publish (a diff, a signal), it calls an `emit*` import; the runtime fans the result out to subscribers. The Language does not hold a reference to a runtime callback.

The full set of imports and exports for each direction is enumerated in the WIT file. The conceptual groupings are:

- **Imports from runtime** (Language calls these): agent identity & signing, Language context (own address, own settings), persistent KV storage, event emission, runtime utilities including the canonical `hash()` function, plus optional Holochain and storage-file-I/O extensions.
- **Exports to runtime** (Language provides these): lifecycle (`name`, `version`, `isPublic`, `init`, `teardown`), the capability function sets enumerated in §6.5, and event-handler exports (`handleTelepresenceSignal`, optional `handleHolochainSignal`, etc.).

## 6.3 Capability discovery

A conforming executor MUST detect capabilities by introspecting a Language's exports against the WIT capability definitions. The export set is treated as set-typed:

- If the Language exports the *complete* function set for a capability, that capability is available.
- If the Language exports a subset, the capability is *not* available — partial implementations are an error.
- Order of declaration is irrelevant.

Detection MUST happen exactly once per Language load (after `init()` returns successfully) and the result MUST be cached for the lifetime of the Language instance.

## 6.4 Lifecycle

Every Language MUST export the following lifecycle members:

- `name: string`
- `version: string`
- `isPublic: bool`
- `init(): Promise<void>` (or equivalent in WIT)
- `teardown(): Promise<void>` (or equivalent in WIT)

`init` is called by the executor exactly once per Language instance after loading; it MUST complete before any capability calls are dispatched. `teardown` is called when the Language is being unloaded (e.g. Perspective deletion); the Language MUST release any non-managed resources before returning.

### 6.4.1 One instance per Perspective

A Link Language is instantiated **per Perspective**. Cloning a Link Language and pointing two Perspectives at the same instance would put them in the same Neighbourhood — so per-Perspective instantiation is the natural unit of isolation. Expression Languages (e.g. the Agent Language, the Language Language) follow the same single-instance-per-executor pattern; they are not Perspective-scoped.

## 6.5 Capabilities

The protocol defines six standard capabilities. A Language MAY implement any combination; the executor uses the detected set to decide what role the Language can play.

| Capability | Purpose | Required exports |
|---|---|---|
| `expression`           | Store and retrieve Expressions by address | `expressionCreate`, `expressionGet` |
| `perspective-commit`   | Write LinkExpressions into a shared graph | `perspectiveCommit` |
| `perspective-query`    | Run SPARQL / link queries against the shared graph | `perspectiveQueryRun` |
| `perspective-sync`     | Bring a Perspective up to date with the network | `perspectiveSyncSync`, `perspectiveSyncRender` |
| `peers`                | Enumerate and route to other agents in the Neighbourhood | `peersSetLocal`, `peersRemote` |
| `telepresence`         | Real-time online-status + signal channel | `telepresenceSetStatus`, `telepresenceGetAgents`, `telepresenceSendSignal`, `telepresenceSendBroadcast` |

The exact signatures (parameter types, return types, error conventions) are in the WIT file.

### 6.5.1 Composition patterns

The three perspective capabilities are independently composable. Useful combinations:

| Pattern | Exports |
|---|---|
| Full-sync Neighbourhood | `commit` + `query` + `sync` + `peers` |
| DM inbox (sender view) | `commit` |
| DM inbox (owner view) | `commit` + `query` + `sync` + `peers` |
| Read-only knowledge graph | `query` |
| Public wiki / forum | `commit` + `query` |
| Append-only archive | `commit` + `query` |

Direct Messages are **not a separate capability**; a DM inbox is just a templated Link Language exporting `perspective-commit` with the recipient DID baked into the template clone. See [Social Conventions](../docs-src/ad4m-social-conventions.md) for the `ad4m://inbox` discovery pattern.

## 6.6 Ambient acting-agent contract

`perspectiveCommit` and other capability calls take no `signer` parameter. Instead, the runtime sets an **ambient "acting agent" context** before dispatching into any Language export. The Language signs Expressions via the `agentSign` / `agentCreateSignedExpression` imports, which consult that context.

This avoids passing identity material through every call. Languages MUST treat the acting-agent context as read-only and MUST NOT cache it across calls.

## 6.7 Event handler exports

The runtime MAY deliver events to a Language via specific exports it watches for:

- `handleTelepresenceSignal(signal)` — required if the Language exports `telepresence`.
- `handleHolochainSignal(signal)` — optional; used only if the executor exposes the Holochain extension and the Language uses Holochain DNAs.
- Lifecycle hooks for events the Language opted into (Perspective-link events, etc.).

A Language MUST process events synchronously or queue them; the runtime does not guarantee re-delivery if an event handler throws.

## 6.8 The `ad4m:host` module

Language imports are exposed in the WASM build through a host module conventionally named `ad4m:host`. JavaScript Languages get the same imports through a runtime-injected `globalThis.ad4m` object. Either way, the contract is the WIT file plus the prose host-contract document [`../docs-src/host-contract.md`](../docs-src/host-contract.md).

## 6.9 Language Templating

Languages can be **templated** — a base Language is instantiated with parameters to produce a new Language. This is how Link Languages are typically created for new Neighbourhoods.

The templating contract:

1. A base Language declares `possibleTemplateParams` in its metadata.
2. To instantiate, a client calls `language.applyTemplateAndPublish` (RPC) with the source Language address and the template parameter values.
3. The executor fetches the source Language, applies parameter substitution (the substitution algorithm is the Language's own concern), and publishes the result as a new Language with `templated: true` and `templateSourceLanguageAddress` set.

For a Holochain-backed Link Language template, templating typically produces a new Holochain DNA with unique network properties, ensuring each Neighbourhood has its own DHT. Other backend types substitute differently.

## 6.10 What this section deliberately omits

- **Function signatures.** See [`../docs-src/ad4m-lang.wit`](../docs-src/ad4m-lang.wit).
- **Specific Languages.** Reference Language implementations of the capabilities above are not part of the protocol's wire surface and not specified here.
- **Reference dev kits.** Helper libraries for authoring Languages in JavaScript or Rust are not the protocol; a Language can be authored without them.
