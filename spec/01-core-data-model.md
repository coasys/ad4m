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
  key: string;          // Verification method ID from DID document (e.g., "did:key:z6Mk...#z6Mk...")
  signature: string;    // Hex-encoded signature bytes
}
```

### Signing Scheme

Expressions are signed as follows:

1. Serialize `data` to JSON using canonical JSON (RFC 8785) for deterministic serialization: lexicographic key ordering by Unicode code point, no insignificant whitespace, and consistent number formatting. This ensures the same Expression always produces the same signature.
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

> **All AD4M URIs are valid IRIs.** The literal format (§1.8) ensures that no AD4M URI requires escaping or transformation to be used as an RDF IRI.

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

The `literal:` format uses no `//` (authority component) for RFC 3986 compliance. The `Literal.fromUrl()` constructor MUST reject `literal://` URLs with a clear error message.

```typescript
// Correct:
"literal:string:Hello%20World"

// Invalid (MUST be rejected):
"literal://string:Hello%20World"
```

## 1.9 Link Storage Model (RDF 1.2 Reifiers)

Each AD4M link is stored in the SPARQL triple store (Oxigraph 0.5.7) using the **RDF 1.2 reifier** model. Each link produces 8 quads in the **default graph**:

```turtle
s p o .                                                  # 1. direct triple
<link:HASH> rdf:reifies <<( s p o )>> .                  # 2. reifier binding
<link:HASH> ad4m://ontology/author "did:key:..." .       # 3-8. metadata
<link:HASH> ad4m://ontology/timestamp "2026-..." .
<link:HASH> ad4m://ontology/proofKey "did:key:...#..." .
<link:HASH> ad4m://ontology/proofSignature "abcdef..." .
<link:HASH> ad4m://ontology/proofValid "true" .
<link:HASH> ad4m://ontology/status "Shared" .
```

### Reifier IRI

The reifier IRI is deterministic: `link:<SHA256(author||source||predicate||target||timestamp)[0:32]>` — the SHA-256 hash of the concatenation of the author DID, source, predicate, target, and timestamp, truncated to the first 32 hex characters (16 bytes). This ensures the same link always produces the same reifier identifier regardless of which agent stores it.

**Predicate Normalization:** When a link's predicate is normalized to `null`/`None` (i.e., empty string predicates), the literal ASCII string `"null"` is substituted in the concatenation. For example, if a link has an empty predicate, the hash input becomes: `author||source||"null"||target||timestamp`. This ensures all implementations compute identical reifier IRIs.

### Metadata Ontology

| URI | Purpose |
|-----|---------|
| `ad4m://ontology/author` | DID of the link author |
| `ad4m://ontology/timestamp` | ISO 8601 timestamp |
| `ad4m://ontology/proofKey` | Verification method ID from DID document (e.g., `did:key:z6Mk...#z6Mk...`) |
| `ad4m://ontology/proofSignature` | Signature bytes, hex-encoded |
| `ad4m://ontology/proofValid` | Whether signature verification passed (`"true"` or `"false"`) |
| `ad4m://ontology/status` | `"Shared"` or `"Local"` |

> **Note:** All six metadata fields (author, timestamp, proofKey, proofSignature, proofValid, status) are always present for every link — none are optional. The source, predicate, and target are represented solely in the direct triple and the reifier binding, not as separate metadata quads.

### Querying

Simple data queries use direct triple patterns in the default graph:

```sparql
SELECT ?todo WHERE {
  ?todo <todo://state> <todo://done> .
}
```

Queries requiring metadata access use the reifier pattern:

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?source ?target ?author ?timestamp WHERE {
  ?reifier rdf:reifies <<( ?source <todo://state> ?target )>> .
  ?reifier <ad4m://ontology/author> ?author .
  ?reifier <ad4m://ontology/timestamp> ?timestamp .
}
```

This model enables efficient SPARQL queries over both link data and metadata, stores everything in the default graph (no named graphs required), and aligns with the RDF 1.2 standard for triple annotation.
