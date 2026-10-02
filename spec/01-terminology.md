# 1. Terminology & Conformance Language

## 1.1 Conformance keywords

The key words **MUST**, **MUST NOT**, **REQUIRED**, **SHALL**, **SHALL NOT**, **SHOULD**, **SHOULD NOT**, **RECOMMENDED**, **MAY**, and **OPTIONAL** in this document are to be interpreted as described in [RFC 2119](https://www.rfc-editor.org/rfc/rfc2119) when, and only when, they appear in ALL CAPITALS.

A **conforming AD4M executor** is a software component that satisfies every MUST in §§ 2–8 and §10, and exposes the WebSocket RPC surface defined in §7. A conforming Link Language is a Language plugin that exports the four perspective capabilities defined in §6.5.

[§11](./11-conformance.md) lists every normative requirement in one place.

## 1.2 Implementation Notes

Inline boxes labelled **Implementation Note** describe places where the current reference-implementation behaviour deviates from what the spec requires for cross-implementation interoperability. An Implementation Note is not a normative requirement; it is a flag that the reference implementation is expected to tighten up.

## 1.3 Glossary

The following terms are load-bearing. Where the spec uses these words it means exactly the definition below; informal synonyms (e.g. "user" for agent, "group" for neighbourhood) are avoided.

### Identity, signing, authentication

**Agent**
: A participant in the protocol, identified by a single DID. Conceptually a person or autonomous process; technically just a key-pair that controls a DID.

**DID** *(Decentralized Identifier)*
: A W3C-standard identifier of the form `did:<method>:<id>`. AD4M's baseline method is `did:key` with Ed25519 keys. See §3.1.

**DID Document**
: The structured public-key material a DID resolves to. For `did:key`, derived deterministically from the DID itself; for other methods, fetched via a method-specific resolver.

**Verification method**
: A named key inside a DID Document. `proof.key` in an Expression names which verification method to use when checking the signature.

**Wallet**
: The executor's encrypted local keystore. Holds the agent's private key(s); decrypted in-memory after the agent unlocks it with a passphrase. See §3.2.

**Capability token**
: A JWT issued by the executor to an app, listing which capability domains and operations the app is allowed to invoke. See §4.

**Capability domain**
: A namespace of operations, e.g. `agent`, `perspective`, `language`, `runtime`. The capability token enumerates `(domain, operations[])` pairs.

### Data model

**Link**
: An ordered triple `{source, predicate?, target}` of URIs. The atom of the AD4M data model. See §2.1.

**Expression**
: A signed envelope `{author, timestamp, data, proof}` attributing some payload `data` to an agent at a point in time. See §2.2.

**LinkExpression**
: `Expression<Link>`. A signed link.

**Perspective**
: A local, per-agent collection of LinkExpressions. The agent's personal subjective graph. See §2.4.

**PerspectiveHandle**
: Runtime metadata for an instantiated Perspective held by an executor — UUID, name, optional shared-Neighbourhood reference, sync state.

**PerspectiveDiff**
: An additive/subtractive change set over a Perspective — `{additions: LinkExpression[], removals: LinkExpression[]}`. Used as the unit of synchronization in Neighbourhoods.

**Neighbourhood**
: A Perspective that has been published with a Link Language to enable multi-agent synchronization. Two agents joined to the same Neighbourhood see the same shared link graph (modulo sync state). See §2.5.

**Subject Class**
: A typed entity defined in SDNA as a SHACL `NodeShape`. Subject Classes give structured-data semantics to subgraphs of links. See §5.

### URIs and addresses

**ExpressionRef** / **Expression URL**
: A URI of the form `<language_address>://<expression_address>` referring to a specific Expression resolvable through a specific Language. Two well-known schemes (`did:`, `literal:`) are handled specially; see §2.7.

**Address**
: An opaque string identifying an Expression within a Language. Format is Language-specific (content hash, DID, etc.).

**`literal:` URI**
: An inline-data URI that encodes a value (string, number, boolean, or JSON object) without needing a backing Language. Format: `literal:<type>:<percent-encoded-value>`. Note: single colon, no `//`. See §2.8.

### Languages

**Language**
: A plugin loaded into the executor that implements one or more *capabilities*. Languages define how content is stored, retrieved, and shared. Reference implementations run JavaScript under Deno or compile Rust to WASM. See §6.

**Capability** *(of a Language)*
: A coherent set of operations a Language exports. Examples: `expression`, `perspective-commit`, `perspective-sync`, `perspective-query`, `peers`, `telepresence`. A Language is detected to support a capability if it exports the corresponding functions ("presence = capability"). See §6.3.

**Link Language**
: A Language that exports the `perspective-commit`, `perspective-sync`, `perspective-query`, and `peers` capabilities. Used to back a Neighbourhood.

**System Language**
: One of a small set of bundled Languages required for the executor to come online — Language Language, Agent Language, Neighbourhood Language, Perspective Language. See §8.2.

**Language Language**
: The system Language that stores and retrieves *other Languages'* source bundles. The recursive base case of the bootstrap.

**Template / templated Language**
: A Language that exposes parameters via `possibleTemplateParams` and can be instantiated by applying parameter values to produce a new Language. The typical way Link Languages are created for new Neighbourhoods. See §8.6.

**Bootstrap seed**
: A JSON file holding the addresses of the system Languages plus a list of trusted-author DIDs. Loaded by the executor at startup. See §8.3.

### Schema and queries (SDNA)

**SDNA** *(Social DNA)*
: The collection of schema definitions stored as links in a Perspective. SHACL-based; defines Subject Classes, property shapes, and flows.

**SHACL** *(Shapes Constraint Language)*
: A W3C standard for declaring graph shapes. AD4M uses SHACL `NodeShape` for Subject Classes and SHACL `PropertyShape` for their properties.

**SPARQL**
: The W3C standard graph query language. AD4M evaluates SPARQL 1.1 queries (via Oxigraph in the reference impl) over the link graph stored in the RDF-1.2-reifier model. See §5.4.

**Reifier IRI**
: The IRI of a metadata node that "reifies" a triple (attaches per-link metadata: author, timestamp, signature, etc.) under the RDF 1.2 reification model. Format: `link:<sha256-hash>` where the hash is derived deterministically from link fields. See §2.10.

**Custom SPARQL function**
: An AD4M-defined function callable in SPARQL via its full IRI (e.g. `<ad4m://fn/parse_literal>`, `<ad4m://fn/strip_html>`). See §5.4.4.

### RPC

**WebSocket RPC**
: The single-socket JSON-RPC-style protocol the executor exposes for client apps. Endpoint: `/api/v1/ws`. See §7.

**Correlation id**
: The client-chosen `id` field threaded through a request/response pair on the WS RPC channel.

**Event**
: An unsolicited message pushed from executor to client over the same socket. Distinguished by having a `type` field but no `id`.

### Versioning

**Protocol version**
: The version of *this specification* an executor implements. Returned in `runtime.info` via `ad4mProtocolVersion` (separate from `ad4mExecutorVersion`, which is the implementation's own build number). See §9.

**SemVer**
: [Semantic Versioning](https://semver.org/) — MAJOR.MINOR.PATCH. Used both for protocol version and executor version.
