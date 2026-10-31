# 0. Introduction & Motivation

## 0.1 What AD4M is

**AD4M** — Agent-centric Distributed Application Meta-ontology — is a protocol for building distributed applications where the agent, not the application, owns the data.

In a conventional application stack, a server holds a schema, an app writes records into it under that schema, and other apps can read those records only by going through the same server. In AD4M:

- A user (an **agent**) holds a personal, local-first graph of links — a **Perspective**.
- Any application the user runs can read from and write to that Perspective, subject to capability checks.
- The meaning of the data — what classes, predicates, and shapes exist — is itself stored in the graph as a schema layer called **Social DNA** (SDNA, expressed in SHACL). Apps discover schemas at runtime by reading the graph.
- Perspectives can be shared between agents by attaching a **Link Language** — a plugin that synchronizes link diffs between participants. A shared Perspective is a **Neighbourhood**.
- All writes are signed by the originating agent using a DID-bound key. Any reader can verify provenance without a central authority.

"Agent-centric" means: identity, data, and consent live with the user, not the app. An app is a viewport onto data the agent owns.

## 0.2 What this document specifies

This is a **wire spec** for the protocol surface that two independent executor implementations must agree on to interoperate. Concretely:

- The shape of `Link`, `Expression`, `Perspective`, `Neighbourhood`, and the URI schemes they use (§2).
- The identity model, signing scheme, and signature verification (§3).
- The capability-token format apps use to authenticate to an executor (§4).
- The SDNA schema language (SHACL JSON), the SPARQL 1.1 query semantics over the link graph, and the RDF reifier storage model (§5).
- The boundary between an executor and a Language plugin (§6); the normative function signatures live in [`../docs-src/ad4m-lang.wit`](../docs-src/ad4m-lang.wit).
- The WebSocket JSON-RPC envelope clients use to talk to an executor (§7).
- The bootstrap-seed file format and the code-signing rule applied when installing Languages (§8).
- The protocol versioning and compatibility model (§9).
- The threat model and security obligations (§10).

## 0.3 What this document is *not*

It is not:

- **An API reference** for the reference TypeScript client (`@coasys/ad4m`). That lives in [`../docs-src/pages/`](../docs-src/pages/).
- **A language-authoring guide.** That lives alongside the WIT in [`../docs-src/language-interface-spec.md`](../docs-src/language-interface-spec.md).
- **A description of any specific Link Language.** P-diff-sync, the reference Link Language used in the bundled executor, is one such implementation; it is not normative. A conforming Link Language is anything that exports the four perspective capabilities defined in §6.
- **A specification of the reference executor's internals** — its Holochain DNAs, its embedded SQLite store, its V8/Deno runtime, its specific ORM (the `Ad4mModel` decorator API), or its development kit (ALDK). These are implementation choices, not protocol surface.

## 0.4 Reference implementation

The reference executor is the Rust/Deno implementation in [github.com/coasys/ad4m](https://github.com/coasys/ad4m). Where this spec is ambiguous, the reference implementation's observable behaviour over the WebSocket RPC surface is authoritative until the spec is amended.

The presence of a second interoperating implementation (TypeScript/Deno) was the primary motivation for extracting this spec out of the reference codebase. Concrete cross-implementation parity is tracked in [§11 Conformance Index](./11-conformance.md).

## 0.5 Conformance levels

A "conforming AD4M executor" is one that satisfies every **MUST** in §§ 2–8 and §10, and exposes the RPC surface in §7. Such an executor can:

- Receive, store, sign, and verify Expressions.
- Maintain Perspectives and host shared Neighbourhoods.
- Talk to client applications over the WebSocket RPC.
- Bootstrap from a published seed file.
- Install and run Language plugins through the interface in §6.

**SHOULD** items are strongly recommended for full feature parity but not required for basic interop. **MAY** items are optional capabilities.

[§11 Conformance Index](./11-conformance.md) is a single-page cross-reference of every normative claim in the document and where it lives.

## 0.6 Status of this draft

This draft is the first attempt to extract the AD4M protocol from the reference codebase. Several sections call out cases where the reference implementation's current behaviour differs from what an interoperable spec should require — typically because the current behaviour was implicit (e.g. relying on Rust struct field order for signature determinism) and a multi-language protocol can't safely depend on it.

Such gaps are marked **Implementation Note** in-line. They represent things the reference impl is expected to tighten up before a 1.0.0 release.
