# 1. Core Data Model

## 1.1 Link

A **Link** is a semantic triple representing an association between two URIs with an optional predicate.

```typescript
interface Link {
  source: string;      // REQUIRED. Valid URI (RFC 3986 scheme required)
  target: string;      // REQUIRED. Valid URI (RFC 3986 scheme required)
  predicate?: string;  // OPTIONAL. Valid URI if present and non-empty
}
```

### URI Validation

All `source` and `target` values MUST be valid URIs beginning with a scheme matching the pattern `[a-zA-Z][a-zA-Z0-9+\-._]*:`. Examples:
- `did:key:z6Mk...`
- `expression://Qm...`
- `literal:json(%7B%22key%22%3A%22value%22%7D)`
- `ad4m://self`

The predicate, if present and non-empty, MUST also be a valid URI.

### Normalization

An empty string predicate (`""`) MUST be normalized to `null`/`None` before storage or signing.

## 1.2 Expression

An **Expression** is a signed datum attributed to an agent at a point in time.

```typescript
interface Expression<T> {
  author: string;       // DID of the creating agent (e.g., "did:key:z6Mk...")
  timestamp: string;    // ISO 8601 / RFC 3339 datetime with millisecond precision
  data: T;              // The payload
  proof: ExpressionProof;
}

interface ExpressionProof {
  key: string;          // Public key of the signer (same as author DID)
  signature: string;    // Hex-encoded signature bytes
}
```

### Signing Scheme

Expressions are signed as follows:

1. Serialize `data` to JSON using `serde_json` canonical serialization (sorted keys in structs, but order-preserving for already-defined struct fields).
2. Serialize `timestamp` to RFC 3339 with millisecond precision and UTC timezone: `YYYY-MM-DDTHH:MM:SS.mmmZ`
3. Compute SHA-256 hash: `SHA256(json_bytes(data) || timestamp_string_bytes)`
4. Sign the 32-byte hash with the agent's Ed25519 private key (from `did:key` method).
5. Hex-encode the signature bytes.

### Verification

To verify an Expression:
1. Parse `author` as a `did:key` to extract the Ed25519 public key.
2. Recompute the SHA-256 hash of `data` and `timestamp` as above.
3. Verify the signature in `proof.signature` (hex-decoded) against the hash using the public key.

## 1.3 LinkExpression

A **LinkExpression** is an `Expression<Link>` — a signed link attributed to an agent.

```typescript
interface LinkExpression {
  author: string;
  timestamp: string;
  data: Link;
  proof: ExpressionProof;
  status?: LinkStatus;  // "shared" | "local"
}

enum LinkStatus {
  Shared = "shared",   // Synced to neighbourhood (default)
  Local = "local"      // Local-only, not shared
}
```

The `status` field is local metadata and is NOT included in the signature computation.

### DecoratedLinkExpression

A **DecoratedLinkExpression** extends the proof with verification status:

```typescript
interface DecoratedExpressionProof {
  key: string;
  signature: string;
  valid?: boolean;
  invalid?: boolean;
}
```

These `valid`/`invalid` fields are computed locally and never transmitted as authoritative.

## 1.4 Perspective

A **Perspective** is a local-first collection of LinkExpressions.

```typescript
interface Perspective {
  links: LinkExpression[];
}
```

A Perspective is fundamentally a subjective, local graph of links. It exists independently of any network or shared state. Think of it as a personal knowledge graph.

### PerspectiveHandle

A **PerspectiveHandle** is the runtime metadata for an instantiated Perspective:

```typescript
interface PerspectiveHandle {
  uuid: string;                                    // UUID v4
  name?: string;                                   // Human-readable name
  neighbourhood?: DecoratedNeighbourhoodExpression; // If shared
  sharedUrl?: string;                              // "neighbourhood://..." if shared
  state: PerspectiveState;
  owners?: string[];                               // List of owner DIDs
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

### PerspectiveDiff

Changes to perspectives are expressed as diffs:

```typescript
interface PerspectiveDiff {
  additions: LinkExpression[];
  removals: LinkExpression[];
}
```

## 1.5 Neighbourhood

A **Neighbourhood** is a shared Perspective — a Perspective that has been published with a Link Language enabling multi-agent synchronization.

```typescript
interface Neighbourhood {
  linkLanguage: string;  // Address of the Link Language used for sync
  meta: Perspective;     // Metadata about the neighbourhood
}

interface NeighbourhoodExpression {
  author: string;
  data: Neighbourhood;
  proof: ExpressionProof;
  timestamp: string;
}
```

Neighbourhoods are identified by their expression address in the Neighbourhood Language, formatted as: `neighbourhood://<address>`

### Joining a Neighbourhood

When an agent joins a neighbourhood:
1. Resolve the neighbourhood expression from the Neighbourhood Language
2. Install the Link Language specified in `linkLanguage`
3. Create a local Perspective backed by the Link Language
4. Begin syncing via the Link Language's `perspective-sync` capability

## 1.6 ExpressionRef (Expression URLs)

Expressions are referenced by URIs of the form `<language_address>://<expression_address>`:

```
QmHashOfLanguage://QmHashOfExpression
did:key:z6MkExample
literal:json(%7B%22key%22%3A%22value%22%7D)
```

The special `literal:` scheme (see §1.8) encodes data inline without requiring a Language. The `did:` scheme is treated as a reference to the Agent Language.

> **All AD4M URIs are valid IRIs.** The v1.0 literal format change (§1.8) ensures that no AD4M URI requires escaping or transformation to be used as an RDF IRI. The `to_iri` / `from_iri` conversion functions from earlier versions have been removed.

## 1.7 Address

An Address is simply a `string` — the interpretation is Language-specific. For content-addressed Languages, this is typically a hash (SHA-256 → CIDv1 → base58btc, prefixed with `"Qm"`). For the Agent Language, it's a DID.

## 1.8 Literal URI Format

The `literal:` scheme encodes data inline without requiring a Language:

```
literal:<type>:<rfc3986_percent_encoded_value>
```

Supported types:

| Type | Example |
|------|--------|
| `string` | `literal:string:Hello%20World` |
| `number` | `literal:number:42` |
| `boolean` | `literal:boolean:true` |
| `json` | `literal:json:%7B%22key%22%3A%22value%22%7D` |

Values are encoded using RFC 3986 percent-encoding via `encodeRFC3986URIComponent()`.

> **v1.0 format change:** The `literal://` format (with double-slash authority component) is no longer supported. The v1.0 format uses `literal:` (no `//`) for RFC 3986 compliance — `literal://` implied an authority component which was semantically incorrect. The `Literal.fromUrl()` constructor MUST reject `literal://` URLs with a clear error message.

```typescript
// v0.x (DEPRECATED — no longer accepted):
"literal://string:Hello%20World"

// v1.0:
"literal:string:Hello%20World"
```

## 1.9 Link Storage Model (Named Graphs)

In the reference implementation, each AD4M link is stored in the SPARQL triple store (Oxigraph) using a **named graph** model:

1. The direct triple `(source, predicate, target)` is stored in a **named graph** whose IRI is derived from the SHA-256 hash of the link data + timestamp (e.g., `link:<hash32>`).
2. Metadata about the link (author, timestamp, proof, status) is stored as additional triples in the **default graph**, keyed by the named graph IRI.

Metadata ontology URIs:

| URI | Purpose |
|-----|---------|
| `ad4m://ontology/author` | DID of the link author |
| `ad4m://ontology/timestamp` | ISO 8601 timestamp |
| `ad4m://ontology/proofKey` | Public key from proof |
| `ad4m://ontology/proofSignature` | Signature bytes (hex) |
| `ad4m://ontology/proofValid` | Whether signature verification passed |
| `ad4m://ontology/status` | `"Shared"` or `"Local"` |

This model enables efficient SPARQL queries over both link data and metadata, supports named graph-scoped operations, and aligns with standard RDF quad storage.
