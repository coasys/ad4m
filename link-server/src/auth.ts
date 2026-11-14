import { createHash, randomBytes as nodeRandomBytes } from "node:crypto";
import * as ed from "@noble/ed25519";
import { base58 } from "@scure/base";
import { SignJWT, jwtVerify, errors as joseErrors } from "jose";
import type { LinkServerDB } from "./db.js";


/**
 * DID key handling, ed25519 sign/verify, the protocol 2 auth message and JWT session
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

/** The tolerance of a protocol 2 auth timestamp against the server's clock (ms). */
export const AUTH_SKEW_MS = 5 * 60_000;

/** How long a used auth signature is remembered (ms): past the skew on both sides. */
const AUTH_REPLAY_MS = 2 * AUTH_SKEW_MS;

/**
 * What a key signs to authenticate to a room of the server at `origin` (protocol 2). Like every
 * executor signature, the signature is over SHA-256 of these bytes (`agentSignStringHex`). The
 * origin (`publicOrigin`) binds the signature to this server: one made for another server, that
 * a server could relay here within the clock window, names another origin and is refused.
 */
export function authMessage(origin: string, roomId: string, timestamp: number): string {
  return `coasys-link-server:auth:2:${origin}:${roomId}:${timestamp}`;
}

/**
 * The origin of a server URL as the auth message names it: scheme, host and port, lower case,
 * no trailing slash, the scheme's default port dropped (`https://links.example.org`,
 * `http://127.0.0.1:3456`). Throws for anything but an http(s) origin: a path, query, fragment
 * or user part is refused rather than dropped, so a configured value means what it says.
 */
export function publicOrigin(value: string): string {
  let url: URL;
  try {
    url = new URL(value);
  } catch {
    throw new Error(`not an origin: "${value}" (expected scheme://host[:port], e.g. https://links.example.org)`);
  }
  if (url.protocol !== "https:" && url.protocol !== "http:") {
    throw new Error(`not an http(s) origin: "${value}"`);
  }
  if (url.pathname !== "/" || url.search !== "" || url.hash !== "" || url.username !== "" || url.password !== "") {
    throw new Error(`an origin has no path, query, fragment or user: "${value}"`);
  }
  // Lower-cases the scheme and host and drops the default port and the trailing slash.
  return url.origin;
}

/**
 * Remembers each (did, room, timestamp) an auth signature was accepted for, so a captured auth
 * body cannot be sent again within the time it would still pass the clock check.
 */
export class AuthReplayGuard {
  private seen = new Map<string, number>();
  private sweepTimer: NodeJS.Timeout;

  constructor() {
    this.sweepTimer = setInterval(() => this.sweep(), 60_000);
    this.sweepTimer.unref();
  }

  /** True the first time; false for a body already used. */
  use(did: string, roomId: string, timestamp: number): boolean {
    const key = `${did}\u0000${roomId}\u0000${timestamp}`;
    if (this.seen.has(key)) return false;
    this.seen.set(key, Date.now() + AUTH_REPLAY_MS);
    return true;
  }

  private sweep(): void {
    const now = Date.now();
    for (const [k, until] of this.seen) {
      if (now > until) this.seen.delete(k);
    }
  }

  close(): void {
    clearInterval(this.sweepTimer);
  }
}

function ensureJwtSecret(db: LinkServerDB): Uint8Array {
  const existing = db.getIdentity("jwt-secret");
  if (existing) return ed.etc.hexToBytes(existing.private_key);
  const secret = nodeRandomBytes(32);
  const hex = secret.toString("hex");
  db.setIdentity("jwt-secret", "", hex);
  return new Uint8Array(secret);
}

/** A session: the key, whom it speaks for, and whether that admits it to the room now. */
export interface Session {
  did: string;
  roomId: string;
  token: string;
  /** The person the key spoke for at auth; undefined for a device-only key. */
  person?: string;
  byBot: boolean;
  admitted: boolean;
}

export type AuthResult =
  | ({ ok: true } & Session)
  | { ok: false; status: 401 | 403 | 404 | 410 | 423; error: string };

/**
 * Whether a key is admitted to a room now: every key on an auto-admit server; otherwise a
 * person's own key while the person is a current member, a bot key while the bot is one.
 * Admission follows the membership: a session made before a join is admitted after it.
 */
export type Admission = (roomId: string, key: { did: string; person?: string; byBot: boolean }) => boolean;

const DEFAULT_JWT_EXPIRY_SECONDS = 24 * 60 * 60;

/** Issues and verifies JWT-backed room sessions; admission is read live from the membership. */
export class AuthManager {
  private db: LinkServerDB;
  private secret: Uint8Array;
  private expirySeconds: number;
  private admits: Admission;

  constructor(db: LinkServerDB, admits: Admission, opts: { jwtExpirySeconds?: number } = {}) {
    this.db = db;
    this.admits = admits;
    this.secret = ensureJwtSecret(db);
    this.expirySeconds = opts.jwtExpirySeconds ?? DEFAULT_JWT_EXPIRY_SECONDS;
  }

  async issueSession(
    did: string,
    roomId: string,
    person?: string,
    byBot = false
  ): Promise<{ token: string; expiresAt: string }> {
    const expiresAt = new Date(Date.now() + this.expirySeconds * 1000).toISOString();
    const token = await new SignJWT({ did, roomId })
      .setProtectedHeader({ alg: "HS256" })
      .setIssuedAt()
      .setJti(nodeRandomBytes(16).toString("hex"))
      .setExpirationTime(Math.floor(Date.now() / 1000) + this.expirySeconds)
      .sign(this.secret);
    this.db.createSession(token, roomId, did, expiresAt, person ?? null, byBot);
    return { token, expiresAt };
  }

  /** Whether `key` is admitted to `roomId` now. */
  admitted(roomId: string, key: { did: string; person?: string; byBot: boolean }): boolean {
    return this.admits(roomId, key);
  }

  /**
   * Checks a bearer token for a room: a valid JWT for it, a live session, a room that is not
   * suspended and, unless `requireAdmitted` is false, a key admitted now.
   */
  async authenticate(
    token: string | undefined,
    roomId: string,
    opts: { requireAdmitted?: boolean } = {}
  ): Promise<AuthResult> {
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
    // A key revoked here (by the key refresh, or in this room) has no session left, whatever
    // the session table says.
    if (this.db.getMemberKey(roomId, did)?.revoked === 1) {
      return { ok: false, status: 401, error: "this key is revoked" };
    }
    const room = this.db.getRoom(roomId);
    if (!room) return { ok: false, status: 404, error: "no such room" };
    if (room.legacy) return { ok: false, status: 410, error: "this room predates protocol 2 and is closed" };
    if (room.suspended) return { ok: false, status: 423, error: "this room is suspended" };

    const key = { did, person: session.person ?? undefined, byBot: session.by_bot === 1 };
    const admitted = this.admits(roomId, key);
    if (opts.requireAdmitted !== false && !admitted) {
      return { ok: false, status: 403, error: "not admitted to this room" };
    }
    return { ok: true, did, roomId, token, person: key.person, byBot: key.byBot, admitted };
  }

  revokeSessionsForDid(roomId: string, did: string): void {
    this.db.deleteSessionsForDid(roomId, did);
  }
}
