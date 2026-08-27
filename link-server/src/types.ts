import { createHash } from "node:crypto";

/**
 * Shared types for the ADAM Simple Server.
 *
 * These mirror AD4M's wire shapes so link-language clients can serialize
 * against this server without translation:
 *   Link            = { source, predicate, target }
 *   LinkExpression   = Link wrapped with author/timestamp/proof
 *   PerspectiveDiff  = additions/removals of LinkExpressions
 *
 * Removals carry the *exact* LinkExpression being removed (same author,
 * timestamp, data and proof as when it was added) so that `linkHash()`
 * of a removal entry matches the hash of the link it removes. This lets
 * the server implement OR-Set semantics ("remove by hash") without a
 * separate tombstone type.
 */

/** Plaintext link payload, used when a room does not have E2E enabled. */
export interface LinkData {
  source: string;
  predicate: string | null;
  target: string;
}

/**
 * Opaque, encrypted link payload used when a room has E2E enabled.
 * `ciphertext`/`nonce` are hex-encoded AES-256-GCM output over the
 * plaintext LinkData (see encryption.ts). The server never sees the
 * plaintext source/predicate/target for these links.
 */
export interface EncryptedLinkData {
  ciphertext: string;
  nonce: string;
}

export function isEncryptedLinkData(
  data: LinkData | EncryptedLinkData | null | undefined
): data is EncryptedLinkData {
  return (
    !!data &&
    typeof data === "object" &&
    typeof (data as EncryptedLinkData).ciphertext === "string" &&
    typeof (data as EncryptedLinkData).nonce === "string"
  );
}

export interface ExpressionProof {
  /** hex-encoded ed25519 signature */
  signature: string;
  /** DID key id that produced the signature, e.g. "did:key:z6Mk...#primary" */
  key: string;
}

export interface LinkExpression {
  /** DID of the signing agent, e.g. "did:key:z6Mk..." */
  author: string;
  /** ISO-8601 timestamp */
  timestamp: string;
  data: LinkData | EncryptedLinkData;
  proof: ExpressionProof;
}

export interface PerspectiveDiff {
  additions: LinkExpression[];
  removals: LinkExpression[];
}

export interface OnlineAgent {
  did: string;
  status?: unknown;
}

/** Server -> client WebSocket push messages. */
export type ServerWsMessage =
  | { type: "diff"; payload: PerspectiveDiff; revision: string; sequence: number }
  | { type: "telepresence-signal"; fromDid: string; payload: unknown }
  | { type: "telepresence-broadcast"; fromDid: string; payload: unknown }
  | { type: "online-agents"; agents: OnlineAgent[] }
  | { type: "peer-joined"; did: string }
  | { type: "peer-left"; did: string }
  | { type: "status-changed"; did: string; status: unknown }
  | { type: "auth-error"; error: string };

/** Client -> server WebSocket messages. */
export type ClientWsMessage =
  | { type: "auth"; token: string }
  | { type: "telepresence-signal"; toDid: string; payload: unknown }
  | { type: "telepresence-broadcast"; payload: unknown }
  | { type: "set-online-status"; status: unknown };

/**
 * Canonical, order-stable JSON payload used for both signing and hashing a
 * LinkExpression. For plaintext links this covers {source,predicate,target,
 * author,timestamp}; for encrypted links the ciphertext/nonce stand in for
 * source/predicate/target so the server can still content-address the link
 * (OR-Set merge) without ever seeing plaintext.
 */
export function canonicalLinkPayload(link: LinkExpression): string {
  if (isEncryptedLinkData(link.data)) {
    return JSON.stringify({
      ciphertext: link.data.ciphertext,
      nonce: link.data.nonce,
      author: link.author,
      timestamp: link.timestamp,
    });
  }
  return JSON.stringify({
    source: link.data.source,
    predicate: link.data.predicate ?? null,
    target: link.data.target,
    author: link.author,
    timestamp: link.timestamp,
  });
}

export function sha256Hex(input: string): string {
  return createHash("sha256").update(input, "utf8").digest("hex");
}

/** Deterministic content hash of a LinkExpression (used for OR-Set membership). */
export function linkHash(link: LinkExpression): string {
  return sha256Hex(canonicalLinkPayload(link));
}

/** Empty-room revision: 64 hex zeros (32 zero bytes). */
export const EMPTY_REVISION = "0".repeat(64);

/**
 * XOR a revision with a link hash. XOR is commutative, associative,
 * and self-inverse, so this gives O(1) incremental revision updates:
 *   add link:    revision = xorHex(revision, linkHash)
 *   remove link: revision = xorHex(revision, linkHash)  — XOR undoes itself
 * Two rooms with the same active links always converge to the same
 * revision regardless of the order links were added/removed in.
 */
export function xorHex(a: string, b: string): string {
  const aBuf = Buffer.from(a, "hex");
  const bBuf = Buffer.from(b, "hex");
  const result = Buffer.alloc(32);
  for (let i = 0; i < 32; i++) {
    result[i] = aBuf[i] ^ bBuf[i];
  }
  return result.toString("hex");
}

/**
 * Compute the XOR revision of a set of link hashes. Used by tests —
 * the server maintains the revision incrementally via xorHex() in
 * applyDiffAndAppend, never calling this at runtime.
 */
export function computeRevision(linkHashes: string[]): string {
  let rev = EMPTY_REVISION;
  for (const hash of linkHashes) {
    rev = xorHex(rev, hash);
  }
  return rev;
}

export interface RoomParams {
  roomId: string;
}

export interface AuthClaims {
  did: string;
  roomId: string;
  token: string;
}

// Augment FastifyRequest so preValidation/preHandler hooks can stash verified
// auth claims for downstream handlers without `any` casts at every call site.
declare module "fastify" {
  interface FastifyRequest {
    authClaims?: AuthClaims;
  }
}

export interface FederateRequestBody {
  diff: PerspectiveDiff;
  sequence: number;
  revision: string;
  timestamp: string;
  serverPublicKey: string;
  serverSignature: string;
  serverUrl?: string;
}

export interface ReconcileRequestBody {
  sinceSequence: number;
  revision: string;
  timestamp: string;
  serverPublicKey: string;
  serverSignature: string;
  serverUrl?: string;
}

export interface ReconcileResponseBody {
  diffs: PerspectiveDiff[];
  revision: string;
  sequence: number;
}

/**
 * Canonical payload signed by a server's identity key for federation calls.
 * Includes a timestamp (ISO-8601) for replay protection — receivers reject
 * payloads older than `FEDERATION_PAYLOAD_MAX_AGE_MS`.
 */
/**
 * Canonical payload signed by a server's identity key for federation calls.
 * `body` is nested under its own key (not spread) so that body fields can
 * never shadow the positional `kind`/`roomId`/`timestamp` keys.
 */
export function canonicalFederationPayload(
  kind: "federate" | "reconcile",
  roomId: string,
  body: Record<string, unknown>,
  timestamp?: string
): string {
  return JSON.stringify({ kind, roomId, timestamp: timestamp ?? new Date().toISOString(), body });
}

/** Maximum age of a signed federation payload before it gets rejected (5 minutes). */
export const FEDERATION_PAYLOAD_MAX_AGE_MS = 5 * 60 * 1000;
