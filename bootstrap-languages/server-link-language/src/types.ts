/**
 * Shared types for the Server Link Language.
 *
 * Split into two families:
 *   - AD4M-facing types (Expression, LinkExpression, PerspectiveDiff, ...)
 *     mirror the subset of @coasys/ad4m-ldk types needed by a link
 *     language. Kept local so pure/core modules can be imported and
 *     tested without pulling in the ad4m:host runtime.
 *   - Server-facing "wire" types describe the JSON shapes exchanged
 *     with link-server over HTTP and WebSocket. When a room has
 *     E2E encryption enabled, wire link expressions carry an opaque
 *     `encrypted` blob instead of plaintext `data` — see src/encryption.ts.
 */

// ---------------------------------------------------------------------------
// AD4M core types
// ---------------------------------------------------------------------------

export type DID = string;
export type Address = string;

export interface ExpressionProof {
    signature: string;
    key: string;
    valid?: boolean;
    invalid?: boolean;
}

export interface Expression<T = unknown> {
    author: DID;
    timestamp: string;
    data: T;
    proof: ExpressionProof;
}

export interface Link {
    source: string;
    target: string;
    predicate?: string;
}

export interface LinkExpression extends Expression<Link> {
    status?: string;
}

export interface PerspectiveDiff {
    additions: LinkExpression[];
    removals: LinkExpression[];
}

export interface Perspective {
    links: LinkExpression[];
}

export interface QueryRequest {
    kind: string;
    payload: unknown;
}

export interface QueryResponse {
    kind: string;
    payload: unknown;
}

export interface LinkQuery {
    source?: string;
    target?: string;
    predicate?: string;
}

/** Telepresence online-agent record, keyed by DID with an opaque status payload. */
export interface OnlineAgent {
    did: DID;
    status?: unknown;
}

// ---------------------------------------------------------------------------
// Server wire types (HTTP + WebSocket JSON shapes)
// ---------------------------------------------------------------------------

/**
 * A LinkExpression as it travels over the wire. In a plaintext room this
 * is structurally identical to LinkExpression. In an E2E room, `data` is
 * replaced by an opaque `encrypted` envelope (hex-encoded nonce+ciphertext)
 * and `data` is omitted — see src/encryption.ts encryptLinkForWire /
 * decryptLinkFromWire.
 */
export interface WireLinkExpression {
    author: DID;
    timestamp: string;
    proof: ExpressionProof;
    status?: string;
    data?: Link;
    /** Present instead of `data` when the room has E2E encryption enabled. */
    encrypted?: string;
}

export interface WirePerspectiveDiff {
    additions: WireLinkExpression[];
    removals: WireLinkExpression[];
}

export interface AuthChallengeResponse {
    challenge: string;
}

export interface AuthTokenResponse {
    token: string;
}

export interface CommitResponse {
    revision?: string;
    sequence?: number;
}

/**
 * One entry in the `/sync` response's `diffs[]` array. The exact shape
 * link-server uses for per-entry framing isn't nailed down by the
 * spec this language was built against, so both reasonable shapes are
 * accepted and normalized in src/sync.ts:
 *   - nested:  { diff: { additions, removals }, revision?, sequence? }
 *   - flat:    { additions, removals, revision?, sequence? }
 * Missing per-entry `revision`/`sequence` fall back to the response's
 * top-level values.
 */
export interface SyncDiffEntry {
    diff?: WirePerspectiveDiff;
    additions?: WireLinkExpression[];
    removals?: WireLinkExpression[];
    revision?: string;
    sequence?: number;
}

export interface SyncResponse {
    diffs: SyncDiffEntry[];
    revision: string;
    sequence: number;
}

export interface RenderResponse {
    links: WireLinkExpression[];
    revision: string;
}

export interface PeersResponse {
    peers: DID[];
}

export interface RevisionResponse {
    revision: string;
    sequence: number;
}

export interface AclResponse {
    admin: DID;
    members: DID[];
}

export interface KeysResponse {
    encryptedKey: string;
    version: number;
}

// ---------------------------------------------------------------------------
// WebSocket message types
// ---------------------------------------------------------------------------

export type ServerWsMessage =
    | { type: "diff"; payload: WirePerspectiveDiff; revision: string; sequence: number }
    | { type: "telepresence-signal"; fromDid: DID; payload: unknown }
    | { type: "telepresence-broadcast"; fromDid: DID; payload: unknown }
    | { type: "online-agents"; agents: OnlineAgent[] }
    | { type: "peer-joined"; did: DID }
    | { type: "peer-left"; did: DID };

export type ClientWsMessage =
    | { type: "telepresence-signal"; toDid: DID; payload: unknown }
    | { type: "telepresence-broadcast"; payload: unknown }
    | { type: "set-online-status"; status: unknown };

// ---------------------------------------------------------------------------
// E2E encryption envelope
// ---------------------------------------------------------------------------

/**
 * Sealed-box style envelope used to deliver the room's shared symmetric
 * key to a single recipient, addressed to their X25519 public key.
 * See src/encryption.ts for the full scheme.
 */
export interface SealedRoomKeyEnvelope {
    /** Hex-encoded ephemeral X25519 public key used for this seal. */
    ephemeralPublicKey: string;
    /** Hex-encoded 12-byte AES-GCM nonce. */
    nonce: string;
    /** Hex-encoded AES-256-GCM ciphertext (includes the auth tag). */
    ciphertext: string;
}
