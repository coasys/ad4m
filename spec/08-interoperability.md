# 8. Interoperability Requirements

## 8.1 Overview

This section defines what an alternative AD4M implementation MUST and SHOULD support to achieve interoperability with the reference implementation and other conforming implementations.

## 8.2 MUST Requirements

### 8.2.1 Identity

- **MUST** use `did:key` method with Ed25519 keys for agent identity.
- **MUST** generate DID Documents conforming to the [did:key specification](https://w3c-ccg.github.io/did-method-key/).
- **MUST** support Ed25519 signature verification for expression proofs.

### 8.2.2 Expression Signing

- **MUST** implement the signing scheme described in [Core Data Model §1.2](./01-core-data-model.md#12-expression):
  - SHA-256 hash of `json_serialize(data) || rfc3339_millis_utc(timestamp)`
  - Ed25519 signature of the hash
  - Hex-encoded signature in `proof.signature`
- **MUST** verify signatures on received expressions.

### 8.2.3 Data Types

- **MUST** implement the core types: `Link`, `LinkExpression`, `Perspective`, `PerspectiveDiff`, `Neighbourhood`, `NeighbourhoodExpression`.
- **MUST** use camelCase JSON serialization for all types.
- **MUST** normalize Link predicates (empty string → null/omitted).
- **MUST** validate that Link source and target are valid URIs.

### 8.2.4 GraphQL API

- **MUST** expose a GraphQL API compatible with the schema in [GraphQL API](./06-graphql-api.md).
- **MUST** support both HTTP POST and WebSocket (`graphql-transport-ws`) transports.
- **MUST** implement the core query/mutation/subscription types for perspectives, agents, and neighbourhoods.

### 8.2.5 Language Interface

- **MUST** support loading JavaScript Language modules via a `create(context)` function.
- **MUST** provide `LanguageContext` with `AgentService` and `SignaturesService`.
- **MUST** support `ExpressionAdapter` (get/put) and `LinkSyncAdapter` interfaces.

### 8.2.6 Neighbourhood Protocol

- **MUST** understand `neighbourhood://` URIs.
- **MUST** be able to resolve neighbourhood expressions from the Neighbourhood Language.
- **MUST** install and use the specified Link Language for sync.

### 8.2.7 Expression URLs

- **MUST** parse and generate expression URLs in the format `<language_address>://<expression_address>`.
- **MUST** handle `literal://` scheme for inline data.
- **MUST** handle `did:` scheme as references to the Agent Language.

## 8.3 SHOULD Requirements

### 8.3.1 SDNA

- **MUST** support SHACL-based SDNA for subject class definitions and instance resolution.
- **SHOULD** support SurrealDB for performant queries over the link graph.
- **MAY** support custom SHACL rules (SPARQL-based constraints) for advanced reasoning.

### 8.3.2 Capability Tokens

- **SHOULD** implement JWT-based capability tokens with the claims structure in [Agent Model §2.5](./02-agent-model.md#25-capability-tokens).
- **SHOULD** enforce capability checks on all GraphQL operations.

### 8.3.3 Holochain Integration

- **SHOULD** support Holochain-backed Languages via `HolochainLanguageDelegate`.
- **SHOULD** be able to run p-diff-sync DNA for neighbourhood sync.
- Without Holochain support, an implementation can still interoperate using centralized Language variants.

### 8.3.4 Telepresence

- **SHOULD** support `TelepresenceAdapter` for online status and signaling.
- **SHOULD** support the `neighbourhoodSignal` subscription.

### 8.3.5 Language Templating

- **SHOULD** support Language templating for creating parameterized Language variants.

### 8.3.6 Direct Messages

- **SHOULD** support `DirectMessageAdapter` for peer-to-peer messaging.

## 8.4 MAY Requirements

- **MAY** implement AI/LLM integration (models, tasks, transcription).
- **MAY** implement the Embedding Vector Language.
- **MAY** implement alternative Link Language backends (not p-diff-sync).
- **MAY** implement the notification system.
- **MAY** implement entanglement proofs for cross-system identity binding.
- **MAY** implement multi-user mode (multiple agents on one executor).

## 8.5 Wire Format Compatibility

### JSON Serialization

All JSON MUST use camelCase field names. Key serialization rules:

| Type | Serialization |
|------|---------------|
| Timestamps | RFC 3339 with millisecond precision, UTC (`2024-01-15T10:30:00.000Z`) |
| DIDs | Full DID string (`did:key:z6Mk...`) |
| Signatures | Hex-encoded bytes (lowercase) |
| Public keys | Same as DID (`did:key:z6Mk...`) in `proof.key` |
| UUIDs | Lowercase with hyphens (`550e8400-e29b-41d4-a716-446655440000`) |
| Null optionals | Omitted from JSON or explicit `null` — both MUST be accepted |

### Expression URL Format

```
<language_address>://<expression_address>
```

Where:
- `language_address` is any string matching `[^:^\s]+`
- `expression_address` is the remainder after `://`

Special cases:
- `literal://<content>` — inline literal content
- `did:<method>:<id>` — DID references (no `://`)

## 8.6 Conformance Testing

> **TBD:** A conformance test suite should be developed to validate alternative implementations. The test suite in `test-runner/` provides integration test patterns that can serve as a starting point.
