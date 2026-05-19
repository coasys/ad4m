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

### 8.2.4 WebSocket RPC API

- **MUST** expose WebSocket RPC at `/api/v1/ws` with the JSON-RPC wire format described in [§6.2](./06-websocket-rpc-api.md#62-rpc-protocol).
- **MUST** expose WebSocket events at `/api/v1/ws/events` with multiplexed event delivery.
- **MUST** implement the core RPC operations for agent, perspectives, languages, neighbourhoods, and runtime domains.
- **MUST** authenticate via `token` query parameter on WebSocket upgrade.

### 8.2.5 Language Interface

- **MUST** support loading JavaScript Language modules that export flat functions at the module level (the v1.0 flat export model).
- **MUST** support capability detection via export introspection (the "presence = capability" rule).
- **MUST** provide the core runtime imports: `agent` (identity/signing), `language-context`, `storage` (core KV), `event-emission`, and `runtime-utils` (including the canonical `hash()` function).
- **MUST** support the `lifecycle` exports (`name`, `version`, `isPublic`, `init`, `teardown`).
- **MUST** support the `expression` capability exports (`expressionCreate`, `expressionGet`).
- **MUST** support the `perspective-commit`, `perspective-sync`, and `perspective-query` capability exports.
- **MUST** support the `peers` capability exports (`peersSetLocal`, `peersRemote`).
- **MUST** implement the `ad4m:host` module (or equivalent) for Language imports.
- **SHOULD** support the Holochain extension (`holochain-ext`) for Holochain-backed Languages.
- **SHOULD** support the Storage File I/O extension (`storage-fs-ext`).
- Implementations MUST NOT use the `create(context)` factory or `LanguageContext` parameter for Language loading. Context is provided via import functions (`languageAddress()`, `languageSettings()`, `storageGet/Put`, etc.).

### 8.2.6 Neighbourhood Protocol

- **MUST** understand `neighbourhood://` URIs.
- **MUST** be able to resolve neighbourhood expressions from the Neighbourhood Language.
- **MUST** install and use the specified Link Language for sync.

### 8.2.7 Expression URLs

- **MUST** parse and generate expression URLs in the format `<language_address>://<expression_address>`.
- **MUST** handle `literal:` scheme for inline data (v1.0 format: `literal:<type>:<encoded_value>`, no `//`).
- **MUST** reject the `literal://` format (invalid; correct format is `literal:`).
- **MUST** handle `did:` scheme as references to the Agent Language.
- All AD4M URIs MUST be valid IRIs without requiring `to_iri` / `from_iri` transformation.

## 8.3 SHOULD Requirements

### 8.3.1 SDNA

- **MUST** support SHACL-based SDNA for subject class definitions and instance resolution.
- **MUST** support SPARQL 1.1 queries over the link graph (via Oxigraph or an equivalent engine).
- **SHOULD** implement the custom SPARQL functions using full AD4M IRIs: `<ad4m://fn/parse_literal>` and `<ad4m://fn/strip_html>` for literal value filtering.
- **MAY** support custom SHACL rules (SPARQL-based constraints) for advanced reasoning.

### 8.3.2 Capability Tokens

- **SHOULD** implement JWT-based capability tokens with the claims structure in [Agent Model §2.5](./02-agent-model.md#25-capability-tokens).
- **SHOULD** enforce capability checks on all RPC operations.

### 8.3.3 Holochain Integration

- **SHOULD** support Holochain-backed Languages via the Holochain extension (`holochainRegisterDnas`, `holochainCall`, `holochainCallAsync`).
- **SHOULD** be able to run p-diff-sync DNA for neighbourhood sync.
- **SHOULD** route Holochain signals to Languages via the `handleHolochainSignal` export.
- Without Holochain support, an implementation can still interoperate using centralized Language variants.

### 8.3.4 Telepresence

- **SHOULD** support telepresence capabilities (`telepresenceSetStatus`, `telepresenceGetAgents`, `telepresenceSendSignal`, `telepresenceSendBroadcast`) for online status and signaling.
- **SHOULD** support the `signal` event type for neighbourhood signaling.

### 8.3.5 Language Templating

- **SHOULD** support Language templating for creating parameterized Language variants.

### 8.3.6 Direct Messages

- **SHOULD** support the DM-as-inbox pattern: Languages exporting `perspective-commit` for senders, with the recipient DID baked into a template clone.
- **SHOULD** support the `ad4m://inbox` predicate for inbox discovery.
- **SHOULD** support the `ad4m://friend-of` predicate for friend management.

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

```text
<language_address>://<expression_address>
```

Where:
- `language_address` is any string matching `[^:^\s]+`
- `expression_address` is the remainder after `://`

Special cases:
- `literal:<type>:<encoded_value>` — inline literal content (v1.0 format, no `//`)
- `did:<method>:<id>` — DID references (no `://`)

### IRI Compatibility

All AD4M URIs are valid IRIs (Internationalized Resource Identifiers). The `literal:` format (without `//`) ensures RFC 3986 compliance.

## 8.6 Conformance Testing

> **TBD:** A conformance test suite should be developed to validate alternative implementations. The test suite in `test-runner/` provides integration test patterns that can serve as a starting point.
