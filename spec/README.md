# AD4M Protocol Specification

**Version:** 0.1.0 (Draft)
**Status:** Work in Progress
**Date:** 2026-02-19

## Purpose

This specification defines the AD4M (Agent-Centric Distributed Application Meta-ontology) protocol in sufficient detail for alternative implementations to achieve interoperability with the reference Rust/Deno implementation.

AD4M has been demonstrated as a protocol (not just an implementation) through the successful creation of an alternative TypeScript/Deno implementation that achieved near-interoperability with the official Rust executor.

## Document Structure

1. **[Core Data Model](./01-core-data-model.md)** — Links, Expressions, Perspectives, Neighbourhoods
2. **[Agent Model](./02-agent-model.md)** — DID-based identity, key management, capability tokens
3. **[Language Interface](./03-language-interface.md)** — Plugin architecture for Expression, Link, and Perspective languages
4. **[Social DNA (SDNA)](./04-social-dna.md)** — Subject classes, flows, collections, and Prolog-based schemas
5. **[P-Diff-Sync Protocol](./05-p-diff-sync.md)** — Neighbourhood synchronization via Holochain
6. **[GraphQL API](./06-graphql-api.md)** — Executor API schema for clients
7. **[Bootstrap & System Languages](./07-bootstrap-languages.md)** — System bootstrap and core language types
8. **[Interoperability Requirements](./08-interoperability.md)** — MUST/SHOULD requirements for alternative implementations
9. **[Versioning & Compatibility](./09-versioning.md)** — Protocol versioning strategy

## Conventions

- **MUST**, **SHOULD**, **MAY** follow [RFC 2119](https://www.rfc-editor.org/rfc/rfc2119) semantics.
- Type definitions use TypeScript syntax for readability; Rust equivalents are noted where they diverge.
- URIs follow [RFC 3986](https://www.rfc-editor.org/rfc/rfc3986).
- JSON serialization uses camelCase field names throughout.

## References

- [W3C Decentralized Identifiers (DIDs)](https://www.w3.org/TR/did-core/)
- [W3C Verifiable Credentials](https://www.w3.org/TR/vc-data-model/)
- [ZCAP-LD](https://w3c-ccg.github.io/zcap-spec/)
- [Holochain](https://developer.holochain.org/)
- [AD4M Source Repository](https://github.com/coasys/ad4m)
