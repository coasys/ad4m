# 9. Versioning & Compatibility

This section defines how the AD4M protocol is versioned, how executors advertise their version, and how the protocol evolves.

## 9.1 Two distinct version numbers

There are two version numbers that matter, and they are distinct:

| Version | What it identifies | Where it appears |
|---|---|---|
| **Protocol version** | The version of *this specification* an executor implements. | RPC `runtime.info` → `ad4mProtocolVersion`. |
| **Executor version** | The version of the executor *binary*. Specific to one implementation. | RPC `runtime.info` → `ad4mExecutorVersion`. |

A client SHOULD inspect `ad4mProtocolVersion` for compatibility negotiation. `ad4mExecutorVersion` is informational (useful for support and bug reports).

> **Implementation Note.** As of this writing, the reference implementation returns only `ad4mExecutorVersion` from `runtime.info`. Adding `ad4mProtocolVersion` to the wire is required for full conformance to this spec at version 1.0.0 and is tracked as an open item against the reference implementation. Until it is wired, clients SHOULD assume the protocol version equals the lowest version that contains every operation they observe responding successfully.

## 9.2 SemVer

Both version numbers follow [Semantic Versioning 2.0.0](https://semver.org/):

- **MAJOR** — incompatible wire-format changes.
- **MINOR** — backwards-compatible additions (new RPCs, new optional fields, new capabilities).
- **PATCH** — backwards-compatible fixes (no observable wire change).

A client written against protocol `X.Y.Z` MUST work against any executor implementing protocol `X.Y'.Z'` with `Y' ≥ Y`. A client targeting a feature introduced in `X.Y.Z` SHOULD use feature detection (§9.5) before relying on it.

## 9.3 Compatibility coupling matrix

Different parts of the protocol have different rates of change. The compatibility coupling expected for each:

| Component | Bump on change | Notes |
|---|---|---|
| Expression signing scheme | MAJOR | Any change breaks all signature verification across the network. |
| Link / Expression / Perspective wire types | MAJOR | Core data model changes break interop. |
| Reifier IRI computation | MAJOR | Changes which reifier IRIs identify "the same link." |
| WebSocket RPC envelope | MAJOR | Changes how every operation is framed. |
| RPC operation set | MINOR | New operations are additive. Deprecation requires a MINOR; removal requires a MAJOR. |
| SDNA SHACL shapes | MINOR | New shape predicates / properties are additive. |
| SPARQL custom functions | MINOR | New `<ad4m://fn/...>` functions are additive. |
| Language interface (WIT) | MINOR | New capabilities are optional exports. |
| Bootstrap seed format | MINOR | New optional fields are additive. Required field changes are MAJOR. |
| Capability domains / operations | MINOR | New domains / operations are additive. Existing semantics changes are MAJOR. |

## 9.4 Wire-format stability rules

The following invariants MUST hold within a MAJOR version:

- A signed Expression MUST remain verifiable for as long as the protocol MAJOR is unchanged. New MINORs MUST NOT change the signing algorithm, hash function, byte ordering, or timestamp format.
- A link's reifier IRI MUST remain stable for the life of the MAJOR. Two implementations of the same MAJOR MUST compute identical reifier IRIs for identical link inputs.
- All RPC operations and their `params`/`result` shapes that exist in a MINOR MUST continue to be accepted (with the same semantics) in every subsequent MINOR until removal in the next MAJOR.

The following MAY change within a MINOR:

- New RPC operations added (with new `type` values).
- New optional fields added to existing `params` and `result` shapes.
- New events added.
- New capability domains and operations defined.
- New SPARQL custom functions added under new IRIs.

## 9.5 Feature detection

Clients SHOULD prefer feature detection over version comparison. The contract:

- A `404` error from the executor on an RPC `type` means the executor does not implement that operation. Clients SHOULD interpret this as a missing optional feature rather than an error.
- For optional fields in `result`, clients MUST tolerate the field being absent or `null`.
- For optional fields in `params`, executors MUST treat the field as absent if not provided.

This avoids brittle "if version < X.Y" checks for incremental adoption.

## 9.6 Deprecation pathway

A protocol element can be deprecated without immediate removal:

1. **Deprecation announcement** (MINOR bump). The element is marked **DEPRECATED** in the spec, with the recommended replacement and the target removal version. Executors continue to honor it; clients SHOULD migrate.
2. **Soft removal** (next MAJOR). The element is removed from the spec text. Executors MAY still implement it for backwards compatibility, but conformance no longer requires it.
3. **Hard removal** (subsequent MAJOR). Conforming executors MUST NOT accept the deprecated form.

## 9.7 Migration strategies

### 9.7.1 Neighbourhood migration

A MAJOR-breaking change to the Link Language used in a Neighbourhood requires a fresh Neighbourhood — there is no protocol-level migration mechanism. Apps SHOULD provide an export/import flow if their data is to be preserved.

### 9.7.2 Agent migration

An agent's DID (and therefore its identity across signed history) is stable for the life of the key. To rotate keys without losing identity, see §3.4.1 — publish a new DID Document with the new verification method, retire the old.

For irrecoverable key loss, an agent cannot recover the prior identity; they MUST generate a new DID. Applications that need durable identity SHOULD plan for this with their own out-of-band recovery (e.g. seed-phrase backup, social recovery).

## 9.8 Cross-implementation compatibility

A second-implementation executor claiming conformance to this spec at version `X.Y.Z`:

- MUST advertise `ad4mProtocolVersion: "X.Y.Z"` on `runtime.info`.
- MUST satisfy every MUST in this spec at that version.
- MAY satisfy SHOULD / MAY items partially; the §11 Conformance Index lists which.
- SHOULD pass the conformance test suite (see §11.4 — to be developed).
