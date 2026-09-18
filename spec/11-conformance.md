# 11. Conformance Index

This section is a single-page cross-reference of every normative requirement in the spec. It exists so an implementer can grep the spec at a glance, and so a conformance test suite can be built against a stable index.

Every row links to the chapter where the requirement is introduced and explained — the chapter remains the authoritative source.

## 11.1 What conformance means

A **conforming AD4M executor** satisfies every MUST in §§ 2–8 and §10, and exposes the WebSocket RPC surface defined in §7. It MAY satisfy SHOULD and MAY items partially, in which case it SHOULD document which.

A **conforming Link Language** exports the four perspective capabilities defined in §6.5 (`commit`, `query`, `sync`, `peers`).

A **conforming Subject Class implementation** stores and reads SDNA using the SHACL JSON wire format (§5.1) and the link-encoding rules in §5.2.

## 11.2 MUST requirements

### Identity & signing (§§3, 4)

| ID | Requirement | Reference |
|---|---|---|
| M-ID-1 | Support `did:key` with Ed25519 as the baseline DID method | [§3.1](./03-identity-and-signing.md#31-identity-model) |
| M-ID-2 | Generate DID Documents per the `did:key` specification | [§3.1](./03-identity-and-signing.md#31-identity-model) |
| M-ID-3 | Maintain an encrypted local wallet | [§3.2](./03-identity-and-signing.md#32-wallet) |
| M-ID-4 | Apply the signing scheme in §3.3 to produce Expression signatures | [§3.3](./03-identity-and-signing.md#33-signing) |
| M-ID-5 | Verify signatures per the algorithm in §3.4 | [§3.4](./03-identity-and-signing.md#34-verification) |
| M-ID-6 | Exclude `status` from LinkExpression signature computation | [§3.3.3](./03-identity-and-signing.md#333-status-field-exclusion) |
| M-CAP-1 | Issue JWT capability tokens of the shape in §4.1 | [§4.1](./04-capability-tokens.md#41-token-format) |
| M-CAP-2 | Validate tokens on every WebSocket RPC | [§4.6](./04-capability-tokens.md#46-revocation) |
| M-CAP-3 | Honor token revocation on existing connections | [§4.6](./04-capability-tokens.md#46-revocation) |
| M-CAP-4 | Reject expired tokens (`exp` past) | [§4.7](./04-capability-tokens.md#47-expiration) |

### Wire data types (§2)

| ID | Requirement | Reference |
|---|---|---|
| M-WIRE-1 | Implement `Link`, `Expression`, `LinkExpression`, `Perspective`, `Neighbourhood` per §2 | [§2](./02-core-data-model.md) |
| M-WIRE-2 | Validate `source` and `target` URIs against the scheme regex | [§2.1.1](./02-core-data-model.md#211-uri-validation) |
| M-WIRE-3 | Normalize empty-string predicates to `null` | [§2.1.2](./02-core-data-model.md#212-predicate-normalization) |
| M-WIRE-4 | Produce only IRIs that need no further escaping | [§2.1.3](./02-core-data-model.md#213-iri-compatibility) |
| M-WIRE-5 | Reject `literal://` URLs (only single-colon `literal:` is valid) | [§2.8](./02-core-data-model.md#28-literal-uri-format) |
| M-WIRE-6 | Parse/produce both `<lang>://<expr>` and `did:` and `literal:` URLs | [§2.7](./02-core-data-model.md#27-expressionref-expression-urls) |
| M-WIRE-7 | Use camelCase JSON field names except where §7.8.2 documents otherwise | [§7.8.1](./07-websocket-rpc.md#781-json-conventions) |
| M-WIRE-8 | Use RFC 3339 ms-UTC for all wire timestamps | [§7.8.1](./07-websocket-rpc.md#781-json-conventions) |
| M-WIRE-9 | Use hex (lowercase) for all signature bytes | [§7.8.1](./07-websocket-rpc.md#781-json-conventions) |

### Storage model (§2.10, §5)

| ID | Requirement | Reference |
|---|---|---|
| M-STORE-1 | Project each link to the RDF-1.2 reifier model in §2.10 | [§2.10](./02-core-data-model.md#210-link-storage-model--rdf-12-reifiers) |
| M-STORE-2 | Compute reifier IRIs using the exact concatenation in §2.10.1 | [§2.10.1](./02-core-data-model.md#2101-reifier-iri) |
| M-STORE-3 | Emit all six metadata predicates per link (author, timestamp, proofKey, proofSignature, proofValid, status) | [§2.10.2](./02-core-data-model.md#2102-metadata-ontology) |

### SDNA / SHACL / SPARQL (§5)

| ID | Requirement | Reference |
|---|---|---|
| M-SDNA-1 | Accept and produce SHACL shapes in the JSON form of §5.1 | [§5.1](./05-sdna.md#51-shacl-shape-json) |
| M-SDNA-2 | Encode SHACL shapes as the link sub-graph in §5.2 when registered | [§5.2](./05-sdna.md#52-shacl-link-encoding) |
| M-SDNA-3 | Round-trip SHACL JSON ↔ SHACL link sub-graph | [§5.2.4](./05-sdna.md#524-round-trip) |
| M-SDNA-4 | Preserve unknown SHACL/AD4M extension predicates on sync | [§5.2.4](./05-sdna.md#524-round-trip) |
| M-SPARQL-1 | Support SPARQL 1.1 query evaluation over the link graph | [§5.4](./05-sdna.md#54-sparql-query-semantics) |
| M-SPARQL-2 | Reject update-style SPARQL forms in query operations | [§5.4.2](./05-sdna.md#542-read-only-enforcement) |

### Language interface (§6)

| ID | Requirement | Reference |
|---|---|---|
| M-LANG-1 | Detect capabilities by export-set introspection | [§6.3](./06-language-interface.md#63-capability-discovery) |
| M-LANG-2 | Cache the detected capability set per Language instance | [§6.3](./06-language-interface.md#63-capability-discovery) |
| M-LANG-3 | Require every Language to export the lifecycle members in §6.4 | [§6.4](./06-language-interface.md#64-lifecycle) |
| M-LANG-4 | Set an ambient acting-agent context before any export call | [§6.6](./06-language-interface.md#66-ambient-acting-agent-contract) |

### WebSocket RPC (§7)

| ID | Requirement | Reference |
|---|---|---|
| M-RPC-1 | Expose `/api/v1/ws` | [§7.1](./07-websocket-rpc.md#71-endpoints) |
| M-RPC-2 | Accept the `?token=` query parameter for auth | [§7.2](./07-websocket-rpc.md#72-authentication) |
| M-RPC-3 | Use the request/response/event envelope in §7.3 | [§7.3](./07-websocket-rpc.md#73-message-envelope) |
| M-RPC-4 | Echo `id` verbatim on the matching response | [§7.3.1](./07-websocket-rpc.md#731-request-client--executor) |
| M-RPC-5 | Distinguish events from responses by absence of `id` | [§7.3.4](./07-websocket-rpc.md#734-event-executor--client-unsolicited) |
| M-RPC-6 | Use the documented error codes (`400`, `401`, `404`, `500`) | [§7.3.3](./07-websocket-rpc.md#733-response--error-executor--client) |
| M-RPC-7 | Dispatch concurrent requests asynchronously | [§7.5](./07-websocket-rpc.md#75-concurrency) |

### Bootstrap (§8)

| ID | Requirement | Reference |
|---|---|---|
| M-BOOT-1 | Install the four system Languages from §8.2 on startup | [§8.2](./08-bootstrap.md#82-system-languages) |
| M-BOOT-2 | Consume the bootstrap-seed format in §8.3 | [§8.3](./08-bootstrap.md#83-bootstrap-seed-file) |
| M-BOOT-3 | Apply the §8.5 install signing+trusted-agent check on every Language install | [§8.5](./08-bootstrap.md#85-language-installation--code-signing) |
| M-BOOT-4 | Refuse installs whose Expression author is not in `trustedAgents` | [§8.5](./08-bootstrap.md#85-language-installation--code-signing) |
| M-BOOT-5 | Support Language templating via `applyTemplateAndPublish` | [§8.6](./08-bootstrap.md#86-language-templating) |

### Security (§10)

| ID | Requirement | Reference |
|---|---|---|
| M-SEC-1 | Generate cryptographically-strong token nonces | [§10.3](./10-security-considerations.md#103-capability-token-security) |
| M-SEC-2 | Strict `exp` enforcement | [§10.3](./10-security-considerations.md#103-capability-token-security) |
| M-SEC-3 | Disable admin credential by default in production builds | [§10.3](./10-security-considerations.md#103-capability-token-security) |
| M-SEC-4 | Treat the seed file as integrity-critical | [§10.4](./10-security-considerations.md#104-bootstrap-trust) |
| M-SEC-5 | Apply the §8.5 install check on system Languages too | [§10.4](./10-security-considerations.md#104-bootstrap-trust) |

## 11.3 SHOULD requirements

| ID | Requirement | Reference |
|---|---|---|
| S-ID-1 | Resolve `proof.key` against the DID Document (don't shortcut for `did:key`) | [§3.4](./03-identity-and-signing.md#34-verification) |
| S-CAP-1 | Log every use of the admin credential | [§4.5](./04-capability-tokens.md#45-admin-credential) |
| S-SPARQL-1 | Pre-register the default prefix set in §5.4.3 | [§5.4.3](./05-sdna.md#543-default-prefixes) |
| S-SPARQL-2 | Implement all `<ad4m://fn/...>` custom SPARQL functions | [§5.4.4](./05-sdna.md#544-custom-ad4m-sparql-functions) |
| S-LANG-1 | Support Holochain-backed Languages via the Holochain extension | [§6](./06-language-interface.md) |
| S-LANG-2 | Support telepresence capability | [§6.5](./06-language-interface.md#65-capabilities) |
| S-SEC-1 | Use OS-level protected memory for wallet key material | [§10.3.1](./10-security-considerations.md#1031-token-signing-key-sensitivity) |
| S-SEC-2 | Sign and verify the bootstrap seed file | [§10.4](./10-security-considerations.md#104-bootstrap-trust) |
| S-SEC-3 | Use a memory-hard KDF and authenticated cipher for wallet encryption | [§10.6](./10-security-considerations.md#106-wallet-encryption) |
| S-SEC-4 | Show full capability list of active tokens in the management UI | [§10.8](./10-security-considerations.md#108-capability-scope-and-least-privilege) |

## 11.4 MAY requirements

| ID | Requirement | Reference |
|---|---|---|
| O-ID-1 | Support additional DID methods (`did:web`, `did:pkh`, etc.) | [§3.1](./03-identity-and-signing.md#31-identity-model) |
| O-ID-2 | Support entanglement proofs | [§3.6](./03-identity-and-signing.md#36-entanglement-proofs-optional) |
| O-SDNA-1 | Process custom (non-SHACL) SDNA entries | [§5.3](./05-sdna.md#53-custom-sdna) |
| O-LANG-1 | Implement AI/LLM integration (PROMPT, TRANSCRIBE) | [§4.4](./04-capability-tokens.md#44-operations) |
| O-BOOT-1 | Allow runtime mutation of `trustedAgents` | [§8.5](./08-bootstrap.md#85-language-installation--code-signing) |
| O-MULTI-1 | Implement multi-user mode (sub-accounts) | [§4.3](./04-capability-tokens.md#43-capability-domains) |

## 11.5 Cross-implementation parity (informative)

The reference Rust/Deno executor is the spec's calibration target. A second implementation (TypeScript/Deno) has demonstrated cross-impl interop on the following areas (informative — not an authoritative test suite):

| Area | Parity status |
|---|---|
| Expression signing & verification (did:key) | Achieved for matching struct layouts; full canonical-JSON support pending (see §3.3.1 Implementation Note). |
| Link RPC operations (add, remove, get, query) | Achieved. |
| SDNA SHACL JSON round-trip | Achieved. |
| SPARQL query evaluation | Achieved via Oxigraph in both impls. |
| Reifier IRI parity | Achieved. |
| Capability tokens (HS256) | Self-issued + self-verified — not externally portable (intentional). |
| Bootstrap seed consumption | Achieved. |
| Conformance test suite | **Not yet implemented.** |

A future conformance test suite SHOULD exercise every MUST in this index against a candidate executor over the WebSocket RPC. Each test SHOULD reference the M-… ID it covers.
