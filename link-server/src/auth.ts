import { createHash, randomBytes as nodeRandomBytes } from "node:crypto";
import * as ed from "@noble/ed25519";
import { base58 } from "@scure/base";
import { SignJWT, jwtVerify, errors as joseErrors } from "jose";
import type { LinkServerDB } from "./db.js";


/**
 * DID key handling, ed25519 sign/verify, challenge-response and JWT session
 * issuance/verification.
 *
 * AD4M identities are `did:key:z...` strings. The multibase 'z' prefix means
 * base58btc; decoding that payload yields a 2-byte multicodec prefix
 * (0xed, 0x01 for ed25519-pub) followed by the raw 32-byte public key.
 */

const ED25519_MULTICODEC_PREFIX = new Uint8Array([0xed, 0x01]);

/** Strip an optional "#fragment" key-id suffix from a DID URL, e.g. "did:key:z6Mk...#primary". */
function didBase(did: string): string {
  const hashIdx = did.indexOf("#");
  return hashIdx === -1 ? did : did.slice(0, hashIdx);
}

/**
 * Extracts the raw 32-byte ed25519 public key from a `did:key:z...` string
 * (fragment, if present, is ignored).
 */
export function didToPublicKey(did: string): Uint8Array {
  const base = didBase(did);
  const prefix = "did:key:";
  if (!base.startsWith(prefix)) {
    throw new Error(`not a did:key DID: ${did}`);
  }
  const multibase = base.slice(prefix.length);
  if (!multibase.startsWith("z")) {
    throw new Error(`unsupported multibase (expected base58btc 'z'): ${did}`);
  }
  const decoded = base58.decode(multibase.slice(1));
  if (
    decoded.length !== 34 ||
    decoded[0] !== ED25519_MULTICODEC_PREFIX[0] ||
    decoded[1] !== ED25519_MULTICODEC_PREFIX[1]
  ) {
    throw new Error(`unsupported did:key type or malformed key: ${did}`);
  }
  return decoded.slice(2);
}

/** Builds a `did:key:z...` string from a raw 32-byte ed25519 public key. */
export function publicKeyToDid(pubkey: Uint8Array): string {
  if (pubkey.length !== 32) {
    throw new Error("ed25519 public key must be 32 bytes");
  }
  const prefixed = new Uint8Array(34);
  prefixed.set(ED25519_MULTICODEC_PREFIX, 0);
  prefixed.set(pubkey, 2);
  return `did:key:z${base58.encode(prefixed)}`;
}

function toBytes(message: string | Uint8Array): Uint8Array {
  return typeof message === "string" ? new TextEncoder().encode(message) : message;
}

/**
 * Hash a message using SHA-256 to match the AD4M executor's signing
 * convention. The executor's `agentSignStringHex` signs
 * `SHA-256(message.as_bytes())`, NOT the raw message bytes. Any
 * verification of an executor-produced signature must hash the message
 * first.
 */
export function hashMessageForVerify(message: string): Uint8Array {
  return new Uint8Array(createHash("sha256").update(message).digest());
}

export async function signHex(
  privateKey: Uint8Array,
  message: string | Uint8Array
): Promise<string> {
  const sig = await ed.signAsync(toBytes(message), privateKey);
  return ed.etc.bytesToHex(sig);
}

export async function verifyHex(
  publicKey: Uint8Array,
  message: string | Uint8Array,
  signatureHex: string
): Promise<boolean> {
  try {
    return await ed.verifyAsync(signatureHex, toBytes(message), publicKey);
  } catch {
    return false;
  }
}

/** Random hex nonce used as a challenge in DID challenge-response auth. */
export function generateChallenge(): string {
  return nodeRandomBytes(32).toString("hex");
}

interface PendingChallenge {
  challenge: string;
  expiresAt: number;
}

/**
 * In-memory, single-use challenge store keyed by (roomId, did). Challenges
 * expire after `ttlMs` and are consumed (deleted) on first use whether
 * verification succeeds or fails, preventing replay.
 */
export class ChallengeStore {
  private pending = new Map<string, PendingChallenge>();
  private ttlMs: number;
  private sweepTimer: NodeJS.Timeout;

  constructor(ttlMs = 5 * 60_000) {
    this.ttlMs = ttlMs;
    this.sweepTimer = setInterval(() => this.sweep(), Math.min(ttlMs, 60_000));
    this.sweepTimer.unref();
  }

  private key(roomId: string, did: string): string {
    return `${roomId}:${did}`;
  }

  create(roomId: string, did: string): string {
    const challenge = generateChallenge();
    this.pending.set(this.key(roomId, did), {
      challenge,
      expiresAt: Date.now() + this.ttlMs,
    });
    return challenge;
  }

  /** Consumes (removes) the pending challenge, returning whether it matched and was unexpired. */
  consume(roomId: string, did: string, providedChallenge: string): boolean {
    const k = this.key(roomId, did);
    const entry = this.pending.get(k);
    this.pending.delete(k);
    if (!entry) return false;
    if (Date.now() > entry.expiresAt) return false;
    return entry.challenge === providedChallenge;
  }

  private sweep(): void {
    const now = Date.now();
    for (const [k, v] of this.pending) {
      if (now > v.expiresAt) this.pending.delete(k);
    }
  }

  close(): void {
    clearInterval(this.sweepTimer);
  }
}

/**
 * Server's own ed25519 identity keypair, generated on first run and
 * persisted in `<data-dir>/data.sqlite` (server_identity table, key_type
 * "server-ed25519"). Used to sign outbound federation requests.
 */
export async function ensureServerIdentity(
  db: LinkServerDB
): Promise<{ publicKey: string; privateKey: string }> {
  const existing = db.getIdentity("server-ed25519");
  if (existing) {
    return { publicKey: existing.public_key, privateKey: existing.private_key };
  }
  const priv = ed.utils.randomPrivateKey();
  const pub = await ed.getPublicKeyAsync(priv);
  const publicKeyHex = ed.etc.bytesToHex(pub);
  const privateKeyHex = ed.etc.bytesToHex(priv);
  db.setIdentity("server-ed25519", publicKeyHex, privateKeyHex);
  return { publicKey: publicKeyHex, privateKey: privateKeyHex };
}

function ensureJwtSecret(db: LinkServerDB): Uint8Array {
  const existing = db.getIdentity("jwt-secret");
  if (existing) return ed.etc.hexToBytes(existing.private_key);
  const secret = nodeRandomBytes(32);
  const hex = secret.toString("hex");
  db.setIdentity("jwt-secret", "", hex);
  return new Uint8Array(secret);
}

export type AuthResult =
  | { ok: true; did: string; roomId: string; token: string }
  | { ok: false; status: 401 | 403; error: string };

const DEFAULT_JWT_EXPIRY_SECONDS = 24 * 60 * 60;

/** Issues and verifies JWT-backed room sessions, and enforces live ACL membership. */
export class AuthManager {
  private db: LinkServerDB;
  private secret: Uint8Array;
  private expirySeconds: number;

  constructor(db: LinkServerDB, opts: { jwtExpirySeconds?: number } = {}) {
    this.db = db;
    this.secret = ensureJwtSecret(db);
    this.expirySeconds = opts.jwtExpirySeconds ?? DEFAULT_JWT_EXPIRY_SECONDS;
  }

  async issueSession(did: string, roomId: string): Promise<{ token: string; expiresAt: string }> {
    const expiresAt = new Date(Date.now() + this.expirySeconds * 1000).toISOString();
    const token = await new SignJWT({ did, roomId })
      .setProtectedHeader({ alg: "HS256" })
      .setIssuedAt()
      .setExpirationTime(Math.floor(Date.now() / 1000) + this.expirySeconds)
      .sign(this.secret);
    this.db.createSession(token, roomId, did, expiresAt);
    return { token, expiresAt };
  }

  async authenticate(token: string | undefined, roomId: string): Promise<AuthResult> {
    if (!token) {
      return { ok: false, status: 401, error: "missing bearer token" };
    }
    let did: string;
    try {
      const { payload } = await jwtVerify(token, this.secret);
      if (typeof payload.did !== "string" || typeof payload.roomId !== "string") {
        return { ok: false, status: 401, error: "malformed token" };
      }
      if (payload.roomId !== roomId) {
        return { ok: false, status: 401, error: "token not valid for this room" };
      }
      did = payload.did;
    } catch (err) {
      if (err instanceof joseErrors.JWTExpired) {
        return { ok: false, status: 401, error: "token expired" };
      }
      return { ok: false, status: 401, error: "invalid token" };
    }

    const session = this.db.getSession(token);
    if (!session || new Date(session.expires_at).getTime() < Date.now()) {
      return { ok: false, status: 401, error: "session expired or revoked" };
    }

    if (!this.db.isMember(roomId, did)) {
      return { ok: false, status: 403, error: "not authorized for this room" };
    }

    return { ok: true, did, roomId, token };
  }

  revokeSessionsForDid(roomId: string, did: string): void {
    this.db.deleteSessionsForDid(roomId, did);
  }
}
