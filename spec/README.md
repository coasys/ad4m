# AD4M Protocol Specification

**Version:** 1.0.0 (Draft)
**Status:** Work in Progress
**Date:** 2026-05-13

## Purpose

This specification defines the AD4M (Agent-Centric Distributed Application Meta-ontology) protocol in sufficient detail for alternative implementations to achieve interoperability with the reference Rust/Deno implementation.

AD4M has been demonstrated as a protocol (not just an implementation) through the successful creation of an alternative TypeScript/Deno implementation that achieved near-interoperability with the official Rust executor.

## Document Structure

1. **[Core Data Model](./01-core-data-model.md)** — Links, Expressions, Perspectives, Neighbourhoods, Literal URI format, RDF 1.2 reifier storage model
2. **[Agent Model](./02-agent-model.md)** — DID-based identity, key management, capability tokens, social conventions
3. **[Language Interface](./03-language-interface.md)** — Flat export model, capability discovery, ALDK, WIT definition, `ad4m:host` module
4. **[Social DNA (SDNA)](./04-social-dna.md)** — Subject classes, flows, collections, and SHACL-based schemas
5. **[WebSocket RPC API](./05-websocket-rpc-api.md)** — Executor WebSocket RPC and events interface for clients
6. **[Bootstrap & System Languages](./06-bootstrap-languages.md)** — System bootstrap and core language types
7. **[Interoperability Requirements](./07-interoperability.md)** — MUST/SHOULD requirements for alternative implementations
8. **[Versioning & Compatibility](./08-versioning.md)** — Protocol versioning strategy
9. **[Ad4mModel](./09-ad4m-model.md)** — Application data model API (decorator-based ORM over links, SHACL, and SPARQL)

## Companion Documents

- **[Language Interface Spec](../docs-src/language-interface-spec.md)** — Detailed prose companion to the WIT definition
- **[AD4M Language WIT](../docs-src/ad4m-lang.wit)** — Normative WIT interface definition (canonical source of truth for the Language interface)
- **[Host Contract](../docs-src/host-contract.md)** — Runtime globals contract for `ad4m:host`
- **[Social Conventions](../docs-src/ad4m-social-conventions.md)** — Social-layer vocabulary and patterns (`ad4m://` predicates, DM inbox pattern, friends)

## Conventions

- **MUST**, **SHOULD**, **MAY** follow [RFC 2119](https://www.rfc-editor.org/rfc/rfc2119) semantics.
- Type definitions use TypeScript syntax for readability; Rust equivalents are noted where they diverge.
- URIs follow [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986). All AD4M URIs are valid IRIs.
- JSON serialization uses camelCase field names throughout.
- **RPC parameter naming:** Most operations use camelCase parameter names (consistent with `#[serde(rename_all = "camelCase")]` on request structs). The exceptions are the model operations (`perspective.modelQuery`, `perspective.modelSubscribe`, `perspective.evaluateGetters`) which use snake_case parameters (`class_name`, `query_json`, `shape_json`, `instance_ids`, `property_names`).
- The WIT file (`ad4m-lang.wit`) is the normative source of truth for the Language interface. Where WIT and prose disagree, the WIT wins.

## References

- [W3C Decentralized Identifiers (DIDs)](https://www.w3.org/TR/did-core/)
- [ZCAP-LD](https://w3c-ccg.github.io/zcap-spec/)
- [Holochain](https://developer.holochain.org/)
- [WebAssembly Interface Types (WIT)](https://component-model.bytecodealliance.org/design/wit.html)
- [SHACL — Shapes Constraint Language](https://www.w3.org/TR/shacl/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Oxigraph](https://oxigraph.org/)
- [AD4M Source Repository](https://github.com/coasys/ad4m)
