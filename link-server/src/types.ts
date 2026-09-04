import { createHash } from "node:crypto";

/**
 * Shared types for the link-server.
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
  /** DID of the signing agent — absent for fully-encrypted links. */
  author?: string;
  /** ISO-8601 timestamp — absent for fully-encrypted links. */
  timestamp?: string;
  data: LinkData | EncryptedLinkData;
  /** Expression proof — absent for fully-encrypted links. */
  proof?: ExpressionProof;
  /** Client-computed SHA-256 of the canonical plaintext, for OR-Set
   * dedup/removal when author/timestamp are encrypted away. */
  link_hash?: string;
  /** Key version used to encrypt this link (absent → version 1). */
  key_version?: number;
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
 * Canonical, order-stable JSON payload used for hashing a LinkExpression.
 * For fully-encrypted links (link_hash present): returns the client-supplied
 * hash directly — the server cannot compute the canonical form because all
 * fields are encrypted.
 * For plaintext links: {source,predicate,target,author,timestamp}.
 */
export function canonicalLinkPayload(link: LinkExpression): string {
  if (link.link_hash) {
    return link.link_hash;
  }
  return JSON.stringify({
    source: (link.data as LinkData).source,
    predicate: (link.data as LinkData).predicate ?? null,
    target: (link.data as LinkData).target,
    author: link.author ?? "",
    timestamp: link.timestamp ?? "",
  });
}

export function sha256Hex(input: string): string {
  return createHash("sha256").update(input, "utf8").digest("hex");
}

/** Deterministic content hash of a LinkExpression (used for OR-Set membership). */
export function linkHash(link: LinkExpression): string {
  if (link.link_hash) return link.link_hash;
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

