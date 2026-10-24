# 8. Bootstrap

This section specifies how a conforming executor starts up: which system Languages it needs to come online, the JSON file format that locates those Languages, and the trust check applied when installing any Language.

## 8.1 The bootstrap problem

An AD4M executor on its own has no schemas, no shared spaces, no peers — just a wallet and a network stack. Everything else is delivered through Languages. But Languages themselves need to be fetched from somewhere, so the executor needs a small set of pre-known Languages it can rely on to bootstrap further Languages. That set is the **system Languages**, and the file pointing to them is the **bootstrap seed**.

## 8.2 System Languages

A conforming executor MUST come online with these four system Languages installed.

### 8.2.1 Language Language

| Capability set | `expression` plus `languageGetSource` |
|---|---|
| Address scheme | Content hash of the bundled source |
| Purpose | Stores and retrieves the *source code* of other Languages, keyed by content hash. Every other Language address in AD4M is dereferenced through here. |

The Language Language is the recursive base case of the bootstrap: it is the one Language the executor cannot itself fetch from the Language Language (because there is nothing yet to fetch from). It is therefore delivered **inline** in the bootstrap seed as a raw source bundle, not as an address.

### 8.2.2 Agent Language

| Capability set | `expression` |
|---|---|
| Address scheme | The agent's DID (e.g. `did:key:z6Mk...`) |
| Purpose | Stores `Expression<Agent>` payloads keyed by DID. Resolving a DID URL (`did:key:...`) goes through this Language. |

### 8.2.3 Neighbourhood Language

| Capability set | `expression` |
|---|---|
| Address scheme | Content hash of the `NeighbourhoodExpression` payload |
| Purpose | Stores `NeighbourhoodExpression` payloads (the published descriptors of shared Perspectives). Resolving a `neighbourhood://<address>` URL goes through here. |

### 8.2.4 Perspective Language

| Capability set | `expression` |
|---|---|
| Address scheme | Implementation-defined |
| Purpose | Stores serialized Perspective snapshots for occasional out-of-band sharing. |

> Direct-Message functionality is **not** a separate system Language. DMs are achieved by templating a Link Language with the recipient DID baked in. See [§6.5](./06-language-interface.md#65-capabilities) and [Social Conventions](../docs-src/ad4m-social-conventions.md).

## 8.3 Bootstrap seed file

The bootstrap seed is a single JSON file the executor loads at startup.

```jsonc
{
  // DIDs whose published Expressions are trusted for code-signing checks (§8.5).
  "trustedAgents": ["did:key:z6Mk..."],

  // Addresses of Link Language templates the executor knows about and can
  // clone to create new Neighbourhoods. Mutable at runtime via
  // runtime.addLinkLanguageTemplates / removeLinkLanguageTemplates.
  "knownLinkLanguages": ["Qm...link-language-template-address"],

  // Addresses of the system Languages (§8.2).
  "agentLanguage":         "Qm...",
  "perspectiveLanguage":   "Qm...",
  "neighbourhoodLanguage": "Qm...",

  // The Language Language is bootstrapped from its raw source bundle
  // (not from an address) because it is the Language that resolves
  // all other Language addresses.
  "languageLanguageBundle": "<base64-or-inline JS source bundle>"
}
```

Reference: [`BootstrapSeed`](../rust-executor/src/runtime_service/mod.rs) (Rust struct, `#[serde]` rename rules apply).

### 8.3.1 Field requirements

| Field | Required | Notes |
|---|---|---|
| `trustedAgents` | MUST | At least one DID. Used to validate Language source on install (§8.5). |
| `knownLinkLanguages` | MUST | MAY be empty initially; runtime mutations add/remove. |
| `agentLanguage` | MUST | Address only — must be resolvable via the Language Language. |
| `perspectiveLanguage` | MUST | Same. |
| `neighbourhoodLanguage` | MUST | Same. |
| `languageLanguageBundle` | MUST | Inline source — bytes loaded directly, not by address. |

A conforming executor MUST be able to consume this exact format. Implementations MAY accept supersets (additional fields) but MUST NOT require them for interoperability.

## 8.4 Bootstrap flow

The sequence of operations the executor MUST perform at startup:

```text
1. Read the bootstrap seed.
2. Load (or generate) the agent's wallet.
3. Initialize any backing storage (databases, DHT runtimes).
4. Load the Language Language directly from `languageLanguageBundle`.
5. Fetch and install, by address via the Language Language:
   - Agent Language
   - Neighbourhood Language
   - Perspective Language
   - knownLinkLanguages (registered, not necessarily instantiated)
   Each install runs the §8.5 signing check.
6. If the agent is unlocked, publish the agent's Expression to the
   Agent Language (idempotent — only on first run, or when the
   profile changed).
7. Start the WebSocket RPC server (§7).
8. Begin accepting client connections.
```

Steps 1–4 are pre-conditions for the runtime to do anything useful and MUST complete before the RPC server starts. Steps 5–6 MAY proceed asynchronously while the RPC server is starting, provided that any RPC requiring a not-yet-installed Language returns a clear error.

## 8.5 Language installation & code-signing

Every Language address an executor installs — at bootstrap, on Neighbourhood join, or via direct user action — MUST be verified against the trusted-agents set:

```text
1. Check the local cache for the Language bundle.
2. If not cached, fetch the signed Expression<LanguageSource> from the
   Language Language by address.
3. Verify the signature on the Expression (§3.4). If invalid, the install
   MUST fail.
4. Check that the Expression's `author` DID is present in the executor's
   `trustedAgents` set. If not, the install MUST fail.
5. Load the module (JavaScript under Deno or WASM instance).
6. Introspect exports for capability detection (§6.3).
7. Call `init()` (§6.4). If `init` throws, MUST treat the install as failed
   and clean up partial state.
8. Register the detected capabilities with the executor.
```

This is the protocol's only built-in **code-signing** mechanism. The trust root is the wallet of whoever signed the Language source; that DID must be in `trustedAgents`. The mechanism is intentionally narrow — see §10 for the threat model discussion (what it protects against, what it doesn't).

Implementations MAY allow runtime mutation of `trustedAgents` via the `runtime.addTrustedAgents` / `runtime.removeTrustedAgents` RPCs, gated by the `runtime.trusted_agents` capability domain (§4.3).

## 8.6 Language templating

For Languages that expose `possibleTemplateParams`, an executor MUST support cloning them via the `language.applyTemplateAndPublish` RPC:

1. Fetch the source Language (subject to the §8.5 install check).
2. Apply the supplied parameter substitution (the substitution algorithm is the source Language's own concern).
3. Publish the result as a new Language with `templated: true` and `templateSourceLanguageAddress` set to the source's address.

The new Language is, from the executor's perspective, just another Language — it also goes through §8.5 on install elsewhere.

For Holochain-backed Link Language templates, instantiation typically yields a unique DNA with separate DHT network properties, ensuring each Neighbourhood has its own isolated DHT. Other backend types may substitute differently.

## 8.7 What this section deliberately omits

- **Reference-implementation centralized variants** (`centralized-agent-language`, etc.). These are alternative deployments of the same wire contract; nothing in the protocol depends on them.
- **Specific Holochain DNAs** (`perspective_diff_sync`, `file-storage`, etc.). These are implementations of the corresponding Language capabilities, not part of the wire protocol.
- **Bootstrap-seed distribution.** How seed files reach executors (download, baked into binary, fetched from a registry) is deployment-specific.
