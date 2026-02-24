# 7. Bootstrap & System Languages

## 7.1 Overview

AD4M bootstraps itself using a set of **system languages** — special Languages that provide core infrastructure. These are bundled with the executor and loaded at startup.

The bootstrapping process creates a self-referential system: Languages are stored and retrieved via the Language Language, agents are registered via the Agent Language, and neighbourhoods are published via the Neighbourhood Language.

## 7.2 System Languages

### Language Language

**Purpose:** Stores and retrieves Language source code (JavaScript bundles).

- **Type:** Expression Language with `LanguageAdapter`
- **Storage:** Cloudflare Workers KV (via a gateway proxy at `https://bootstrap-store-gateway.perspect3vism.workers.dev`)
- **Address scheme:** Content hash of the Language bundle
- **Interfaces implemented:** `ExpressionAdapter`, `LanguageAdapter`

When a Language is published:
1. The source bundle is uploaded to the KV store
2. The content hash becomes the Language's address
3. Other executors can retrieve it by address

### Agent Language

**Purpose:** Stores agent profiles (DID → Agent Expression mapping).

- **Type:** Expression Language backed by Holochain
- **Address scheme:** The agent's DID (e.g., `did:key:z6Mk...`)
- **Interfaces implemented:** `ExpressionAdapter` with `PublicSharing`
- **Holochain DNA:** `agent-language` — stores `Expression<Agent>` entries keyed by DID

When an agent updates their profile:
1. Create a signed `Expression<Agent>` containing their DID, profile links, and DM language address
2. Store it in the Holochain DHT via the Agent Language
3. Other agents retrieve it by DID

### Neighbourhood Language

**Purpose:** Stores neighbourhood definitions (how to join a shared Perspective).

- **Type:** Expression Language
- **Storage:** Cloudflare Workers KV (same gateway as Language Language)
- **Address scheme:** Content hash of the neighbourhood definition
- **Interfaces implemented:** `ExpressionAdapter` with `PublicSharing`

A `NeighbourhoodExpression` contains:
- The address of the Link Language to install
- Metadata about the Neighbourhood (as a Perspective of links)

### Perspective Language

**Purpose:** Stores serialized Perspective snapshots.

- **Type:** Expression Language
- **Details:** Implementation-defined. Used for sharing Perspective state.

### Direct Message Language

**Purpose:** Enables private peer-to-peer messaging between agents.

- **Type:** Language with `DirectMessageAdapter`
- **Holochain DNA:** `direct-message-language`

### File Storage Language

**Purpose:** Stores binary files (images, documents, etc.).

- **Type:** Expression Language backed by Holochain
- **Holochain DNA:** `file-storage`

### P-Diff-Sync (Link Language Template)

**Purpose:** Template for creating Link Languages that power Neighbourhoods.

- **Type:** Link Language with `LinkSyncAdapter` and `TelepresenceAdapter`
- **Holochain DNA:** `perspective_diff_sync`
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
   e. Direct Message Language
   f. File Storage Language
   g. P-Diff-Sync template
5. Generate agent DID (if first run)
6. Publish agent expression to Agent Language
7. Start GraphQL server
8. Ready for client connections
```

## 7.4 Language Installation

When a Language address is encountered (e.g., in a Neighbourhood definition):

1. Check local cache for the Language bundle
2. If not cached, fetch from the Language Language by address
3. Load the JavaScript bundle in the Deno runtime
4. Call the `create(context)` function to instantiate the Language
5. Register adapters with the executor

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

- `centralized-agent-language` — Agent profiles via HTTP
- `centralized-p-diff-sync` — Link sync via a central server
- `centralized-file-storage` — File storage via HTTP

These implement the same interfaces but use centralized infrastructure instead of Holochain DHTs.
