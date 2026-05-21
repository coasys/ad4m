# AD4M Protocol Specification

**Status:** Draft (Version 1.0.0)

## Purpose

This specification defines the wire-level surface of the AD4M protocol — the data types, identity model, RPC envelope, schema language, and bootstrap procedure that an alternative executor MUST implement to interoperate with the reference Rust/Deno implementation.

The spec deliberately covers **only the interop surface**. Reference-implementation details (Holochain DNAs, specific ORMs, dev kits like ALDK), language-authoring guides, and application-developer APIs live in separate companion documents — they are useful, but they are not the protocol.

## Audience & non-goals

The spec is written for **alternative-executor implementers** — people building a second AD4M-compatible runtime in a different language, runtime, or substrate. Other audiences are served by companion docs:

| Audience | Read |
|---|---|
| Alternative-executor implementer | This spec |
| Language plugin author | [`../docs-src/language-interface-spec.md`](../docs-src/language-interface-spec.md) and [`../docs-src/ad4m-lang.wit`](../docs-src/ad4m-lang.wit) |
| AD4M application developer | [`../docs-src/pages/`](../docs-src/pages/) |

Non-goals of this document: ORM design, decorator APIs, specific dev kits (e.g. ALDK), Holochain DNA internals, deployment topology, governance.

## Document map

| § | Title | Purpose |
|---|---|---|
| [0](./00-introduction.md) | **Introduction & Motivation** | What AD4M is, who runs it, what "agent-centric" means here. |
| [1](./01-terminology.md) | **Terminology & Conformance Language** | RFC 2119 keywords + glossary of every load-bearing term. |
| [2](./02-core-data-model.md) | **Core Data Model** | `Link`, `Expression`, `Perspective`, `Neighbourhood`, the RDF reifier storage model, the `literal:` URI scheme. |
| [3](./03-identity-and-signing.md) | **Identity & Signing** | DIDs, the wallet, the Ed25519 signing scheme, verification. |
| [4](./04-capability-tokens.md) | **Capability Tokens** | JWT format, claim shape, capability domains and operations. |
| [5](./05-sdna.md) | **Social DNA (SHACL + SPARQL)** | SHACL shape JSON wire format, SHACL link encoding, SPARQL 1.1 query semantics, custom `<ad4m://fn/...>` functions. |
| [6](./06-language-interface.md) | **Language Interface** | Concepts only: flat exports, capabilities-as-presence, lifecycle, ambient acting-agent contract. Normative interface signatures live in [`ad4m-lang.wit`](../docs-src/ad4m-lang.wit). |
| [7](./07-websocket-rpc.md) | **WebSocket RPC Wire Format** | Endpoint, auth, envelope, error model, event multiplexing, keepalive. Operation reference in [Appendix A](./appendix-a-rpc-reference.md). |
| [8](./08-bootstrap.md) | **Bootstrap** | The seed file format, the bootstrap flow, and the signing-based code-trust mechanism for installed Languages. |
| [9](./09-versioning.md) | **Versioning & Compatibility** | Protocol version anchor, SemVer rules, evolution strategy. |
| [10](./10-security-considerations.md) | **Security Considerations** | Threat model, signature scope, replay protection, capability-token risks, key rotation, bootstrap trust. |
| [11](./11-conformance.md) | **Conformance Index** | Cross-reference table of every MUST / SHOULD / MAY in this spec. |
| [A](./appendix-a-rpc-reference.md) | **Appendix A: RPC Reference** | Per-domain table of WebSocket RPC operations (informative). |
| [B](./appendix-b-worked-example.md) | **Appendix B: Worked Example** | End-to-end: generate keys → join a Neighbourhood → write and query a Link. |

## Reading order

If you're auditing for conformance, read §§ 0–11 in order, then use Appendix A as a reference.

If you're implementing, read §§ 0–4 first (you need these to do anything), then jump to §7 (you need an RPC surface to test against), then return to §§ 5, 6, 8, 10.

## Conventions

- **MUST**, **SHOULD**, **MAY** follow [RFC 2119](https://www.rfc-editor.org/rfc/rfc2119) semantics — see §1.
- Type signatures use TypeScript syntax for readability. Rust struct equivalents are referenced in-line where they're the authoritative wire definition (e.g. `rust-executor/src/types/...`).
- URIs follow [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986). All AD4M URIs are valid IRIs without further escaping.
- JSON wire payloads use **camelCase** field names.

## Normative references

- [W3C Decentralized Identifiers (DIDs) v1.0](https://www.w3.org/TR/did-core/)
- [`did:key` method specification](https://w3c-ccg.github.io/did-method-key/)
- [RFC 2119 — Keywords for use in RFCs](https://www.rfc-editor.org/rfc/rfc2119)
- [RFC 3339 — Date and Time on the Internet](https://www.rfc-editor.org/rfc/rfc3339)
- [RFC 3986 — Uniform Resource Identifier](https://www.rfc-editor.org/rfc/rfc3986)
- [RFC 7519 — JSON Web Token (JWT)](https://www.rfc-editor.org/rfc/rfc7519)
- [SHACL — Shapes Constraint Language](https://www.w3.org/TR/shacl/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [RDF 1.2 (reifying triples)](https://www.w3.org/TR/rdf12-concepts/)

## Informative references

- [Oxigraph](https://oxigraph.org/) — the SPARQL engine used by the reference implementation
- [WebAssembly Interface Types (WIT)](https://component-model.bytecodealliance.org/design/wit.html) — the schema format used for the Language interface
- [`ad4m-lang.wit`](../docs-src/ad4m-lang.wit) — the normative Language-interface signatures
- [Reference implementation](https://github.com/coasys/ad4m)
