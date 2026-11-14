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
 *     E2E encryption enabled, encrypted rooms carry an `EncryptedLinkData`
 *     shape (`{ciphertext, nonce}`) in the `data` field — see src/encryption.ts.
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

/** Encrypted link payload — matches link-server's EncryptedLinkData shape. */
export interface EncryptedLinkData {
    ciphertext: string;
    nonce: string;
}

export function isEncryptedLinkData(
    data: Link | EncryptedLinkData | null | undefined
): data is EncryptedLinkData {
    return (
        !!data &&
        typeof data === "object" &&
        typeof (data as EncryptedLinkData).ciphertext === "string" &&
        typeof (data as EncryptedLinkData).nonce === "string"
    );
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
 * A LinkExpression as it travels over the wire. Every room is end-to-end
 * encrypted: `data` carries an `EncryptedLinkData` shape (`{ciphertext,
 * nonce}`) and author, timestamp and proof are inside the ciphertext. A
 * plaintext `Link` in `data` is what only a server could add; the language
 * skips it. See src/encryption.ts encryptLinkForWire / decryptLinkFromWire.
 */
export interface WireLinkExpression {
    author?: DID;
    timestamp?: string;
    proof?: ExpressionProof;
    status?: string;
    data?: Link | EncryptedLinkData;
    /** Client-computed SHA-256 of the canonical plaintext, for OR-Set
     * dedup/removal when author/timestamp are encrypted away. */
    link_hash?: string;
    /** Key version used to encrypt this link (absent → version 1). */
    key_version?: number;
}

export interface WirePerspectiveDiff {
    additions: WireLinkExpression[];
    removals: WireLinkExpression[];
}

export interface CommitResponse {
    revision?: string;
    sequence?: number;
}

/** One entry in the `/sync` page's `diffs[]`: a diff and its sequence in the room's log. */
export interface SyncDiffEntry {
    additions: WireLinkExpression[];
    removals: WireLinkExpression[];
    sequence: number;
}

export interface SyncResponse {
    diffs: SyncDiffEntry[];
    revision: string;
    sequence: number;
    /** The `since` of the next page; null on the last page. */
    next: number | null;
}

export interface RenderResponse {
    links: WireLinkExpression[];
    revision: string;
    sequence: number;
    /** The sequence the snapshot is pinned at: tail `/sync` from here. */
    upto: number;
    /** Cursor of the next page; null on the last page. */
    next: string | null;
}

export interface PeersResponse {
    peers: DID[];
}

// ---------------------------------------------------------------------------
// Protocol 2: auth, membership, members
// ---------------------------------------------------------------------------

/** `POST /rooms/:roomId/auth` (protocol 2). */
export interface AuthRequest {
    did: DID;
    /** The server's origin the signature names (scheme, host and port; `serverOrigin`). */
    origin: string;
    /** Hex: the key's signature over `coasys-link-server:auth:2:<origin>:<roomId>:<timestamp>`, as `agentSignStringHex` signs. */
    signature: string;
    /** ms since epoch; the server allows 5 min of skew. */
    timestamp: number;
    /** Hex: this key's room key X25519 public key. */
    x25519PublicKey: string;
    /** Hex: Ed25519 over the `coasys:room-key` signing bytes of `{x25519PublicKey, did}` (K7). */
    x25519Signature: string;
}

export interface AuthResponse {
    token: string;
    expiresAt?: string | number;
    /** Whether the room admits this key now (a member, or a bot that is one). */
    admitted: boolean;
}

/** A membership link as the link server carries it: plain and signed, never encrypted. */
export interface WireMembershipLink {
    author: DID;
    timestamp: string;
    data: { source: string; predicate: string; target: string };
    proof: { key: string; signature: string; anchor?: string };
}

/** One membership link's outcome, from the server or the executor. */
export interface MembershipLinkResult {
    hash: string;
    counted: boolean;
    reason?: string;
}

export interface MembershipResponse {
    links: WireMembershipLink[];
    seq: number;
}

/** An admitted key and the X25519 key it signed (`GET /members`). */
export interface MemberKey {
    did: DID;
    person?: string | null;
    byBot?: boolean;
    x25519PublicKey: string;
    x25519Signature: string;
}

export interface MembersResponse {
    members: unknown[];
    keys: MemberKey[];
    /** The room's latest key version, 0 before the first rotation: a rotation seals for the next. */
    latestKeyVersion: number;
}

export interface KeysResponseEntry {
    encryptedKey: SealedBox;
    version: number;
    /** The rotation's commitment to the version's key; absent for a version from before commitments. */
    commitment?: string;
}

export interface KeysResponse {
    keys: KeysResponseEntry[];
}

export interface MemberMissingKeys {
    did: string;
    missingVersions: number[];
}

export interface MissingKeysResponse {
    membersNeedingHistoricalKeys: MemberMissingKeys[];
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
    | { type: "peer-left"; did: DID }
    | { type: "status-changed"; did: DID; status: unknown }
    | { type: "keys-granted"; versions: number[] }
    | { type: "membership"; links: WireMembershipLink[]; seq: number }
    | { type: "rotate-needed"; reason: "removed" | "revoked" | "joined"; removedKeys: string[] }
    | { type: "auth-error"; error: string };

export type ClientWsMessage =
    | { type: "auth"; token: string }
    | { type: "telepresence-signal"; toDid: DID; payload: unknown }
    | { type: "telepresence-broadcast"; payload: unknown }
    | { type: "set-online-status"; status: unknown };

// ---------------------------------------------------------------------------
// E2E encryption envelope
// ---------------------------------------------------------------------------

/**
 * A room key version sealed to one member's X25519 key by `sealRoomKeyTo` (src/encryption.ts:
 * ad4m-keyring's room-key seal v2, ephemeral X25519 ECDH + HKDF-SHA256 + AES-256-GCM), bound
 * to its room and version (`coasys:room-key:<roomId>:<version>` as associated data, K9). Only
 * that member opens it, from its `agentRoomKeySeed`, and only as that room and version.
 */
export interface SealedBox {
    /** Hex-encoded ephemeral X25519 public key used for this seal. */
    ephemeralPublicKey: string;
    /** Hex-encoded 12-byte AES-GCM nonce. */
    nonce: string;
    /** Hex-encoded AES-256-GCM ciphertext (includes the auth tag). */
    ciphertext: string;
}
