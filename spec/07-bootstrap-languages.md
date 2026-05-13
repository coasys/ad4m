# 7. Bootstrap & System Languages

## 7.1 Overview

AD4M bootstraps itself using a set of **system languages** — special Languages that provide core infrastructure. These are bundled with the executor and loaded at startup.

The bootstrapping process creates a self-referential system: Languages are stored and retrieved via the Language Language, agents are registered via the Agent Language, and neighbourhoods are published via the Neighbourhood Language.

All bootstrap Languages have been migrated to the **ALDK** (AD4M Language Development Kit) — JS and Rust implementations using the flat export model described in [§3](./03-language-interface.md).

## 7.2 System Languages

### Language Language

**Purpose:** Stores and retrieves Language source code (JavaScript bundles).

- **Type:** Expression Language with Language management capabilities
- **Storage:** Cloudflare Workers KV (via a gateway proxy at `https://bootstrap-store-gateway.perspect3vism.workers.dev`)
- **Address scheme:** Content hash of the Language bundle
- **Capabilities exported:** `expressionCreate`, `expressionGet`, `languageGetSource`

When a Language is published:
1. The source bundle is uploaded to the KV store
2. The content hash becomes the Language's address
3. Other executors can retrieve it by address

### Agent Language

**Purpose:** Stores agent profiles (DID → Agent Expression mapping).

- **Type:** Expression Language backed by Holochain
- **Address scheme:** The agent's DID (e.g., `did:key:z6Mk...`)
- **Capabilities exported:** `expressionCreate`, `expressionGet`
- **Holochain DNA:** `agent-language` — stores `Expression<Agent>` entries keyed by DID
- **Implementation:** Ported to **Rust ALDK** (`ad4m-ldk` crate)

When an agent updates their profile:
1. Create a signed `Expression<Agent>` containing their DID and profile links
2. Store it in the Holochain DHT via the Agent Language
3. Other agents retrieve it by DID

> **v1.0 change:** The Agent Expression no longer contains a `directMessageLanguage` field. See [§2.3](./02-agent-model.md#23-agent-expression).

### Neighbourhood Language

**Purpose:** Stores neighbourhood definitions (how to join a shared Perspective).

- **Type:** Expression Language
- **Storage:** Cloudflare Workers KV (same gateway as Language Language)
- **Address scheme:** Content hash of the neighbourhood definition
- **Capabilities exported:** `expressionCreate`, `expressionGet`

A `NeighbourhoodExpression` contains:
- The address of the Link Language to install
- Metadata about the Neighbourhood (as a Perspective of links)

### Perspective Language

**Purpose:** Stores serialized Perspective snapshots.

- **Type:** Expression Language
- **Details:** Implementation-defined. Used for sharing Perspective state.

### Direct Message Language

> **Removed in v1.0.** Direct Messages are no longer a bootstrap language. DM functionality is now achieved through the composition of `perspective-commit` + `perspective-sync` + `peers` capabilities, with the recipient DID baked into a template clone. See [Language Interface §3.7](./03-language-interface.md#37-direct-messages-not-a-capability) and [Social Conventions](../docs-src/ad4m-social-conventions.md) for the DM-as-inbox pattern.

### File Storage Language

**Purpose:** Stores binary files (images, documents, etc.).

- **Type:** Expression Language backed by Holochain
- **Holochain DNA:** `file-storage`

### P-Diff-Sync (Link Language Template)

**Purpose:** Template for creating Link Languages that power Neighbourhoods.

- **Type:** Link Language with `perspective-commit`, `perspective-sync`, `perspective-query`, `peers`, and `telepresence` capabilities
- **Holochain DNA:** `perspective_diff_sync`
- **Implementation:** Updated for flat v1.0 exports + ALDK
- **See:** [P-Diff-Sync Protocol](./05-p-diff-sync.md)

This is a **template language** — each Neighbourhood gets its own instance with a unique Holochain DNA. The template is instantiated by applying parameters (primarily the DNA hash).

## 7.3 Bootstrap Flow

```
1. Executor starts
2. Load/generate agent keys (Wallet)
3. Initialize Holochain runtime
4. Load system languages:
   a. Language Language     — for retrieving other languages
   b. Agent Language        — for agent identity
   c. Neighbourhood Language — for neighbourhood definitions
   d. Perspective Language  — for perspective snapshots
   e. File Storage Language
   f. P-Diff-Sync template
5. Generate agent DID (if first run)
6. Publish agent expression to Agent Language
7. Start WebSocket RPC server
8. Ready for client connections
```

The bootstrap flow has 6 system languages. The API surface is WebSocket RPC (see [§6](./06-websocket-rpc-api.md)).

## 7.4 Language Installation

When a Language address is encountered (e.g., in a Neighbourhood definition):

1. Check local cache for the Language bundle
2. If not cached, fetch from the Language Language by address
3. Load the module (JavaScript under Deno, or WASM instance)
4. Runtime introspects exports for capability detection (see [§3.3](./03-language-interface.md#33-capability-discovery))
5. Call `init()` to initialize the Language instance
6. Register detected capabilities with the executor

## 7.5 Language Templates

Languages support **templating** for creating parameterized variants:

1. A base Language declares `possibleTemplateParams` in its metadata
2. To instantiate, call `languageApplyTemplateAndPublish` with the source Language address and template data
3. The executor:
   a. Fetches the source Language
   b. Applies template parameters (implementation-defined substitution)
   c. Publishes the result as a new Language
4. The new Language has `templated: true` and references `templateSourceLanguageAddress`

For p-diff-sync, templating creates a new Holochain DNA with unique network properties, ensuring each Neighbourhood has its own DHT.

## 7.6 Local Persistence Languages

Two local-only languages handle persistence:

- **local-language-persistence** — Persists installed Language metadata to disk
- **local-neighbourhood-persistence** — Persists joined Neighbourhood state to disk

These are JavaScript modules (not full Languages) that run within the executor.

## 7.7 Centralized Variants

For environments without Holochain, centralized fallback implementations exist:

### Centralized Agent Language

- `centralized-agent-language` — Agent profiles via HTTP
- **Implementation:** Ported to **Rust ALDK** (`ad4m-ldk` crate)
- `centralized-p-diff-sync` — Link sync via a central server
- `centralized-file-storage` — File storage via HTTP

These implement the same interfaces but use centralized infrastructure instead of Holochain DHTs.
