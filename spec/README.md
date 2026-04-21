# AD4M Protocol Specification

**Version:** 1.0.0 (Draft)
**Status:** Work in Progress
**Date:** 2026-04-21

## Purpose

This specification defines the AD4M (Agent-Centric Distributed Application Meta-ontology) protocol in sufficient detail for alternative implementations to achieve interoperability with the reference Rust/Deno implementation.

AD4M has been demonstrated as a protocol (not just an implementation) through the successful creation of an alternative TypeScript/Deno implementation that achieved near-interoperability with the official Rust executor.

## Document Structure

1. **[Core Data Model](./01-core-data-model.md)** — Links, Expressions, Perspectives, Neighbourhoods, Literal URI format, Named Graph storage model
2. **[Agent Model](./02-agent-model.md)** — DID-based identity, key management, capability tokens, social conventions
3. **[Language Interface](./03-language-interface.md)** — Flat export model, capability discovery, ALDK, WIT definition, `ad4m:host` module
4. **[Social DNA (SDNA)](./04-social-dna.md)** — Subject classes, flows, collections, and SHACL-based schemas
5. **[P-Diff-Sync Protocol](./05-p-diff-sync.md)** — Neighbourhood synchronization via Holochain
6. **[GraphQL API](./06-graphql-api.md)** — Executor API schema for clients
7. **[Bootstrap & System Languages](./07-bootstrap-languages.md)** — System bootstrap and core language types
8. **[Interoperability Requirements](./08-interoperability.md)** — MUST/SHOULD requirements for alternative implementations
9. **[Versioning & Compatibility](./09-versioning.md)** — Protocol versioning strategy
10. **[Ad4mModel](./10-ad4m-model.md)** — Application data model API (decorator-based ORM over links, SHACL, and SPARQL)

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
- The WIT file (`ad4m-lang.wit`) is the normative source of truth for the Language interface. Where WIT and prose disagree, the WIT wins.

## Key Changes in v1.0

- **Language Interface v1.0:** Flat export model replaces adapter classes. `LinkSyncAdapter` split into `perspective-commit`, `perspective-sync`, `perspective-query`. WIT is the normative source. ALDK provides JS and Rust authoring toolkits.
- **SPARQL as sole backend:** SurrealDB removed. Oxigraph in-process SPARQL 1.1 engine with disk persistence. Direct triple + named graph storage model.
- **Literal URI format:** `literal://` → `literal:` for RFC 3986 compliance. `to_iri`/`from_iri` removed.
- **Social conventions:** `directMessageLanguage` removed from Agent shape. DM inbox, friends, profile, and presence use `ad4m://` predicates in perspectives.
- **Ad4mModel:** New decorator API with WeakMap registry. SPARQL query engine. Transactions, eager loading (`include`), reactive queries (`subscribe`).
- **P-Diff-Sync:** `latest_revision` zome extern. Gossip uses `revisionHexes`. Exponential backoff, active peer discovery.

## References

- [W3C Decentralized Identifiers (DIDs)](https://www.w3.org/TR/did-core/)
- [ZCAP-LD](https://w3c-ccg.github.io/zcap-spec/)
- [Holochain](https://developer.holochain.org/)
- [WebAssembly Interface Types (WIT)](https://component-model.bytecodealliance.org/design/wit.html)
- [SHACL — Shapes Constraint Language](https://www.w3.org/TR/shacl/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Oxigraph](https://oxigraph.org/)
- [AD4M Source Repository](https://github.com/coasys/ad4m)
