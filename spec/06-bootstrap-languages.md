# 6. Bootstrap & System Languages

## 6.1 Overview

AD4M bootstraps itself using a set of **system languages** — special Languages that provide core infrastructure. These are bundled with the executor and loaded at startup.

The bootstrapping process creates a self-referential system: Languages are stored and retrieved via the Language Language, agents are registered via the Agent Language, and neighbourhoods are published via the Neighbourhood Language.

All bootstrap Languages have been migrated to the **ALDK** (AD4M Language Development Kit) — JS and Rust implementations using the flat export model described in [§3](./03-language-interface.md).

## 6.2 System Languages

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

> The Agent Expression does not contain a `directMessageLanguage` field. Inbox discovery uses the `ad4m://inbox` predicate. See [§2.3](./02-agent-model.md#23-agent-expression).

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

> Direct Messages are not a bootstrap language. DM functionality is achieved through the composition of `perspective-commit` + `perspective-sync` + `peers` capabilities, with the recipient DID baked into a template clone. See [Language Interface §3.7](./03-language-interface.md#37-direct-messages-not-a-capability) and [Social Conventions](../docs-src/ad4m-social-conventions.md) for the DM-as-inbox pattern.

## 6.3 Bootstrap Seed

The set of system Languages an executor uses to come online is described by a **bootstrap seed** — a JSON file loaded by the executor at startup (and produced by `ad4m-cli publish-bootstrap`).

### Format

```jsonc
{
  // DIDs whose published Expressions are trusted by this executor at boot
  // (used e.g. for code-signing checks on installed Languages — see §6.5).
  "trustedAgents": ["did:key:z6Mk..."],

  // Addresses of Link Language templates the executor knows about and can
  // clone to create new neighbourhoods. Mutable at runtime via the
  // runtime.addLinkLanguageTemplates / removeLinkLanguageTemplates RPCs.
  "knownLinkLanguages": ["Qm...link-language-template-address"],

  // Address of the bundled DM language template (optional).
  "directMessageLanguage": "Qm...",

  // Addresses of the system Languages described in §6.2.
  "agentLanguage":         "Qm...",
  "perspectiveLanguage":   "Qm...",
  "neighbourhoodLanguage": "Qm...",

  // The Language Language is bootstrapped from its raw source bundle
  // (not from an address), because it is the Language that resolves
  // all other Language addresses.
  "languageLanguageBundle": "<base64-encoded JS bundle source>"
}
```

### Lifecycle

1. The executor reads the seed at startup.
2. The `languageLanguageBundle` is loaded directly (it is the only Language not fetched via an address).
3. The remaining system Languages (`agentLanguage`, `perspectiveLanguage`, `neighbourhoodLanguage`, optionally `directMessageLanguage`) are fetched by address via the Language Language.
4. `trustedAgents` is used to validate signed Expressions on Language installation (§6.5).
5. `knownLinkLanguages` populates the runtime's list of templates from which new Neighbourhoods can be created.

### Conformance

Alternative implementations MUST be able to consume this seed format (or an equivalent superset) so that a single set of published system Languages can be used across implementations.

## 6.4 Bootstrap Flow

```
1. Executor starts
2. Load/generate agent keys (Wallet)
3. Initialize Holochain runtime
4. Read the bootstrap seed (§6.3)
5. Load system Languages:
   a. Language Language     — loaded from the inline source bundle
   b. Agent Language        — fetched by address via Language Language
   c. Neighbourhood Language — fetched by address
   d. Perspective Language  — fetched by address
6. Generate agent DID (if first run)
7. Publish agent expression to Agent Language
8. Start WebSocket RPC server
9. Ready for client connections
```

The API surface is WebSocket RPC (see [§5](./05-websocket-rpc-api.md)).

## 6.5 Language Installation

When a Language address is encountered (e.g., in a Neighbourhood definition):

1. Check local cache for the Language bundle.
2. If not cached, fetch the signed `Expression<LanguageSource>` from the Language Language by address.
3. **Verify the signature** on the Expression and check that the author DID is in the configured `trustedAgents` set (from the bootstrap seed, §6.3). This is the protocol's basic **code-signing** mechanism: only Language source code signed by a trusted author is loaded and executed. An untrusted or invalid signature MUST cause the installation to fail.
4. Load the module (JavaScript under Deno, or WASM instance).
5. Runtime introspects exports for capability detection (see [§3.3](./03-language-interface.md#33-capability-discovery)).
6. Call `init()` to initialize the Language instance.
7. Register detected capabilities with the executor.

## 6.6 Language Templates

Languages support **templating** for creating parameterized variants:

1. A base Language declares `possibleTemplateParams` in its metadata
2. To instantiate, call `languageApplyTemplateAndPublish` with the source Language address and template data
3. The executor:
   a. Fetches the source Language
   b. Applies template parameters (implementation-defined substitution)
   c. Publishes the result as a new Language
4. The new Language has `templated: true` and references `templateSourceLanguageAddress`

For a Holochain-backed Link Language template, templating typically creates a new Holochain DNA with unique network properties, ensuring each Neighbourhood has its own DHT.

## 6.7 Centralized Variants

For environments without Holochain, centralized fallback implementations of the system Languages may exist (e.g. agent profiles served over HTTP via `centralized-agent-language`). These implement the same Language interfaces as their Holochain-backed counterparts but use centralized infrastructure instead of DHTs.
