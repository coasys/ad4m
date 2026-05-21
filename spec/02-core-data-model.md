# 2. Core Data Model

This section defines the wire-level data types that all conforming executors share: `Link`, `Expression`, the signed variant `LinkExpression`, `Perspective`, `PerspectiveDiff`, `Neighbourhood`, the URI schemes used to address things, and the RDF storage model into which links are projected for SPARQL queries.

All types are presented as TypeScript interfaces for readability. Authoritative Rust definitions live in [`rust-executor/src/types/`](../rust-executor/src/types/) and core types in [`core/src/`](../core/src/).

## 2.1 Link

A **Link** is an ordered triple — the atom of the AD4M data model.

```typescript
interface Link {
  source: string;      // REQUIRED. Valid URI.
  target: string;      // REQUIRED. Valid URI.
  predicate?: string;  // OPTIONAL. If present and non-empty, MUST be a valid URI.
}
```

Rust equivalent: `core/src/links/Links.ts` (TS) / `rust-executor/src/types/` (Rust).

### 2.1.1 URI validation

`source` and `target` MUST be valid URIs beginning with a scheme matching `[a-zA-Z][a-zA-Z0-9+\-._]*:`. Examples:

- `did:key:z6Mk...`
- `expression://Qm...`
- `literal:string:Hello%20World`
- `ad4m://self`

If a `predicate` is supplied and non-empty, it MUST also be a valid URI by the same rule.

### 2.1.2 Predicate normalization

An empty-string predicate (`""`) MUST be normalized to `null` (or the host language's equivalent absent value) before storage, signing, hashing, and over-the-wire serialization. Implementations MUST accept either `null` or `""` on input and produce one of them — never both shapes — on output.

This matters because empty-string and absent predicates would otherwise hash differently in the reifier model (§2.10).

### 2.1.3 IRI compatibility

Every URI an executor produces (links, expression refs, literals, DIDs) MUST be a valid IRI usable directly in RDF without further percent-encoding or `to_iri` / `from_iri` transformation. This is what enables the storage model in §2.10 to put link URIs straight into SPARQL.

## 2.2 Expression

An **Expression** is a signed envelope attributing some payload to an agent at a point in time.

```typescript
interface Expression<T> {
  author: string;          // DID of the creating agent, e.g. "did:key:z6Mk..."
  timestamp: string;       // RFC 3339 with millisecond precision, UTC: "YYYY-MM-DDTHH:MM:SS.mmmZ"
  data: T;                 // The payload (Link, Agent, Neighbourhood, application data, ...)
  proof: ExpressionProof;
}

interface ExpressionProof {
  key: string;             // Verification-method ID, e.g. "did:key:z6Mk...#z6Mk..."
  signature: string;       // Hex-encoded signature bytes (lowercase)
}
```

Rust equivalent: [`rust-executor/src/types/...`](../rust-executor/src/types/); Expression type used in [`rust-executor/src/agent/signatures.rs`](../rust-executor/src/agent/signatures.rs).

The signing scheme and verification algorithm are specified in [§3.3 Signing](./03-identity-and-signing.md#33-signing) and [§3.4 Verification](./03-identity-and-signing.md#34-verification). This section just defines the wire shape.

## 2.3 LinkExpression

A **LinkExpression** is `Expression<Link>` plus an optional local `status` flag.

```typescript
interface LinkExpression extends Expression<Link> {
  status?: "shared" | "local";   // local metadata; NOT part of the signature
}
```

`status` distinguishes a link that should be propagated to the Neighbourhood (`"shared"`, default) from one that is kept private to this executor (`"local"`). The field MUST NOT be included in signature computation; it is local-only routing metadata.

### 2.3.1 DecoratedLinkExpression

When an executor returns a LinkExpression to a client, it MAY decorate the proof with computed validity flags:

```typescript
interface DecoratedExpressionProof extends ExpressionProof {
  valid?: boolean;     // signature verified locally
  invalid?: boolean;   // signature failed locally
}
```

These flags are computed locally each time and MUST NOT be trusted as authoritative when received over the wire from another agent.

## 2.4 Perspective

A **Perspective** is a local collection of LinkExpressions — the agent's personal subjective graph.

```typescript
interface Perspective {
  links: LinkExpression[];
}
```

A Perspective is a graph, not a tree, and not strictly a set: links are identified by `(author, timestamp, source, predicate, target)` and duplicates with different signatures are possible (e.g. two agents asserting the same fact).

A Perspective is initially private; it has no network footprint unless the agent attaches a Link Language (see §2.5).

### 2.4.1 PerspectiveHandle

The runtime metadata an executor holds about an instantiated Perspective:

```typescript
interface PerspectiveHandle {
  uuid: string;                                       // UUID v4
  name?: string;                                      // human-readable label
  neighbourhood?: DecoratedNeighbourhoodExpression;   // if shared
  sharedUrl?: string;                                 // "neighbourhood://..." if shared
  state: PerspectiveState;
  owners?: string[];                                  // owner DIDs
}

enum PerspectiveState {
  Private = "Private",
  NeighbourhoodCreationInitiated = "NeighbourhoodCreationInitiated",
  NeighbourhoodJoinInitiated = "NeighbourhoodJoinInitiated",
  LinkLanguageFailedToInstall = "LinkLanguageFailedToInstall",
  LinkLanguageInstalledButNotSynced = "LinkLanguageInstalledButNotSynced",
  Synced = "Synced"
}
```

### 2.4.2 PerspectiveDiff

The unit of synchronization in Neighbourhoods:

```typescript
interface PerspectiveDiff {
  additions: LinkExpression[];
  removals: LinkExpression[];
}
```

A diff is **commutative and idempotent**: applying the same diff twice MUST be equivalent to applying it once. Link Languages SHOULD be diff-based and CRDT-friendly; the reference Link Language (p-diff-sync) maintains a Merkle DAG of these diffs, but that is an implementation choice.

## 2.5 Neighbourhood

A **Neighbourhood** is a Perspective shared between agents via a Link Language.

```typescript
interface Neighbourhood {
  linkLanguage: string;   // address of the Link Language
  meta: Perspective;      // metadata about the Neighbourhood (as links)
}

type NeighbourhoodExpression = Expression<Neighbourhood>;
```

Neighbourhoods are addressed by `neighbourhood://<expression_address>`, where the address resolves through the Neighbourhood Language (see §8.2).

### 2.5.1 Join procedure

When an agent joins a Neighbourhood:

1. Resolve the `NeighbourhoodExpression` from the Neighbourhood Language by its address.
2. Verify the signature on the Expression (§3.4). MUST fail the join if invalid.
3. Install the Language whose address appears in `linkLanguage`, applying the standard install procedure including code-signing checks (§8.5).
4. Create a local PerspectiveHandle backed by the now-installed Link Language.
5. Begin syncing via the Link Language's `perspective-sync` capability (§6.5).

## 2.6 Agent

An **Agent** is the public profile data associated with a DID. It is itself an Expression payload.

```typescript
interface Agent {
  did: string;            // e.g. "did:key:z6Mk..."
  perspective?: Perspective;   // public-facing profile links
}

type AgentExpression = Expression<Agent>;
```

The `perspective` field carries application-defined profile properties (name, avatar, social-convention predicates such as `ad4m://inbox`, etc.) as ordinary links. DM-inbox discovery is the `ad4m://inbox` predicate; there is no first-class DM field on the Agent payload.

## 2.7 ExpressionRef (Expression URLs)

An **Expression URL** addresses a specific Expression resolvable through a specific Language:

```
<language_address>://<expression_address>
```

Two schemes are handled specially:

| Scheme | Meaning |
|---|---|
| `did:` | Reference to the **Agent Language**. The DID is the expression address. |
| `literal:` | Inline data; no Language resolution required (§2.8). |

For all other schemes, the part before `://` is treated as an opaque Language address, and the part after as an opaque Expression address. The Language address is dereferenced through the Language Language (§8.2) to obtain the Language, which then resolves the Expression address.

### 2.7.1 URL format rules

- The general form `<lang>://<expr>` MUST be parsed and produced by every conforming implementation.
- The `literal:` scheme MUST use a single colon — `literal:<type>:<value>` — and implementations MUST reject `literal://...` with an explicit error. See §2.8.
- The `did:` scheme uses the standard DID syntax — no `://` — and MUST be recognised as an Agent Language reference.
- All URLs MUST be valid IRIs.

## 2.8 Literal URI format

The `literal:` scheme encodes inline data without a backing Language.

```
literal:<type>:<rfc3986-percent-encoded-value>
```

| Type | Example |
|---|---|
| `string`  | `literal:string:Hello%20World` |
| `number`  | `literal:number:42` |
| `boolean` | `literal:boolean:true` |
| `json`    | `literal:json:%7B%22key%22%3A%22value%22%7D` |

The encoding function is RFC 3986 percent-encoding with the strict variant (`!'()*` also escaped) — see [`core/src/Literal.ts`](../core/src/Literal.ts):

```javascript
function encodeRFC3986URIComponent(str) {
  return encodeURIComponent(str).replace(
    /[!'()*]/g,
    c => `%${c.charCodeAt(0).toString(16).toUpperCase()}`
  );
}
```

The `literal://...` form (with authority component) MUST be rejected with a clear error message — it is not RFC 3986 compliant.

## 2.9 Addresses

An **address** is an opaque string identifying an Expression *within a particular Language*. There is no protocol-level format; Languages choose.

Common patterns:

- Content-addressed Languages typically use `Qm…` (base58btc CID) or `sha256-…` hashes.
- The Agent Language uses the agent's DID as the address.
- The Language Language uses content hashes of Language source bundles.

Conforming executors MUST NOT make any structural assumptions about the address string beyond passing it through to the relevant Language.

## 2.10 Link storage model — RDF 1.2 reifiers

For the purpose of SPARQL evaluation (§5.4), each LinkExpression is projected into the SPARQL store as 8 quads in the **default graph** using the RDF 1.2 reification model.

```turtle
# 1. Direct triple
<source> <predicate> <target> .

# 2. Reifier binding — attaches a per-link metadata node
<link:HASH> rdf:reifies <<( <source> <predicate> <target> )>> .

# 3–8. Metadata on the reifier node
<link:HASH> <ad4m://ontology/author>         "did:key:..." .
<link:HASH> <ad4m://ontology/timestamp>      "2026-05-20T10:30:00.000Z" .
<link:HASH> <ad4m://ontology/proofKey>       "did:key:...#..." .
<link:HASH> <ad4m://ontology/proofSignature> "deadbeef..." .
<link:HASH> <ad4m://ontology/proofValid>     "true" .
<link:HASH> <ad4m://ontology/status>         "Shared" .
```

Reference: [`rust-executor/src/perspectives/sparql_store.rs`](../rust-executor/src/perspectives/sparql_store.rs).

### 2.10.1 Reifier IRI

```
link:<first 32 hex chars of SHA256(author || source || predicate || target || timestamp)>
```

Concretely:

```
hash_input  = utf8(author)
            || utf8(source)
            || utf8(predicate or "")
            || utf8(target)
            || utf8(timestamp)            # RFC 3339 millis-UTC, same as in the Expression
reifier_iri = "link:" + hex(SHA256(hash_input))[0:32]
```

When `predicate` is `null` (or normalized empty-string per §2.1.2), the **empty byte sequence** is used in the concatenation — i.e. the predicate field contributes zero bytes. See [`sparql_store.rs:115`](../rust-executor/src/perspectives/sparql_store.rs) — `link.data.predicate.as_deref().unwrap_or("").as_bytes()`.

Two agents who observe the same link will compute the same reifier IRI. This is the only way to identify "this link" canonically across the network.

### 2.10.2 Metadata ontology

Every link contributes exactly six metadata quads. The predicates are fixed:

| Predicate | Value |
|---|---|
| `ad4m://ontology/author` | author DID |
| `ad4m://ontology/timestamp` | RFC 3339 millis-UTC timestamp |
| `ad4m://ontology/proofKey` | the verification-method ID from `proof.key` |
| `ad4m://ontology/proofSignature` | hex signature bytes |
| `ad4m://ontology/proofValid` | local verification result, `"true"` or `"false"` |
| `ad4m://ontology/status` | `"Shared"` or `"Local"` |

All six MUST be present for every link. Source, predicate, and target appear only in the direct triple and the reifier binding — they are not duplicated as metadata quads.

### 2.10.3 Querying

Simple queries that just read link data use the direct triple pattern:

```sparql
SELECT ?todo WHERE {
  ?todo <todo://state> <todo://done> .
}
```

Queries that need per-link metadata use the reifier pattern:

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?source ?target ?author ?timestamp WHERE {
  ?reifier rdf:reifies <<( ?source <todo://state> ?target )>> .
  ?reifier <ad4m://ontology/author>    ?author .
  ?reifier <ad4m://ontology/timestamp> ?timestamp .
}
```

SPARQL semantics, custom functions, and SDNA integration are specified in §5.
