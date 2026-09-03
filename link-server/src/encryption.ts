import { hkdfSync, randomBytes as nodeRandomBytes } from "node:crypto";
import * as ed from "@noble/ed25519";
import { x25519 } from "@noble/curves/ed25519.js";
import { gcm } from "@noble/ciphers/aes.js";
import type { LinkServerDB } from "./db.js";

/**
 * E2E encryption for rooms.
 *
 * Trust model: the server generates each room's AES-256-GCM symmetric key
 * (so it can seal it to every current member's derived X25519 public key
 * in one pass) but never persists the plaintext key — only the per-member
 * sealed copies (room_keys table) are stored. From that point on the
 * server only ever handles ciphertext for that room's link data.
 *
 * Key wrapping uses a one-shot ECIES-style construction: an ephemeral
 * X25519 keypair does ECDH with the recipient's X25519 public key, the
 * shared secret runs through HKDF-SHA256 (salt = ephPub || recipPub,
 * info = domain-separated tag) to produce the AES-256-GCM key that seals
 * the room key. This matches the server-link-language's `deriveSealKey`
 * so sealed keys can cross the wire between server and language client.
 */

const { bytesToHex, hexToBytes } = ed.etc;

const ROOM_KEY_SEAL_INFO = "adam-server-link-language:room-key-seal:v1";
const AES_KEY_BYTES = 32;

function concatBytes(...chunks: Uint8Array[]): Uint8Array {
  const total = chunks.reduce((sum, c) => sum + c.length, 0);
  const out = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) {
    out.set(chunk, offset);
    offset += chunk.length;
  }
  return out;
}

function deriveSealKey(
  sharedSecret: Uint8Array,
  ephemeralPublicKey: Uint8Array,
  recipientPublicKey: Uint8Array
): Uint8Array {
  const salt = concatBytes(ephemeralPublicKey, recipientPublicKey);
  return new Uint8Array(
    hkdfSync("sha256", sharedSecret, salt, ROOM_KEY_SEAL_INFO, AES_KEY_BYTES)
  );
}

export interface EncryptedKeyPayload {
  ephemeralPublicKey: string;
  nonce: string;
  ciphertext: string;
}

export function generateRoomKey(): Uint8Array {
  return new Uint8Array(nodeRandomBytes(32));
}

/**
 * Seals a room key to a recipient's X25519 public key via one-shot ECIES
 * (ephemeral X25519 + HKDF-SHA256 + AES-256-GCM).
 *
 * `recipientX25519Pub` comes from the ACL table's `x25519_public_key`
 * column, populated during DID auth when the language sends its derived
 * public key. The server never derives this value itself — clients use a
 * signing-capability-based derivation that produces a different keypair
 * than the textbook Ed25519→X25519 Montgomery conversion.
 */
export function encryptRoomKeyForRecipient(
  roomKey: Uint8Array,
  recipientX25519Pub: Uint8Array
): EncryptedKeyPayload {
  const ephemeralPriv = x25519.utils.randomSecretKey();
  const ephemeralPub = x25519.getPublicKey(ephemeralPriv);
  const shared = x25519.getSharedSecret(ephemeralPriv, recipientX25519Pub);
  const symKey = deriveSealKey(shared, ephemeralPub, recipientX25519Pub);
  const nonce = new Uint8Array(nodeRandomBytes(12));
  const ciphertext = gcm(symKey, nonce).encrypt(roomKey);
  return {
    ephemeralPublicKey: bytesToHex(ephemeralPub),
    nonce: bytesToHex(nonce),
    ciphertext: bytesToHex(ciphertext),
  };
}

/**
 * Unseals a room key using the recipient's X25519 private key. Used
 * client-side or in tests. The recipient reconstructs their own public
 * key from the private key to feed the salt for HKDF.
 */
export function decryptRoomKeyWithX25519(
  payload: EncryptedKeyPayload,
  recipientX25519Priv: Uint8Array
): Uint8Array {
  const ephemeralPub = hexToBytes(payload.ephemeralPublicKey);
  const recipientPub = x25519.getPublicKey(recipientX25519Priv);
  const shared = x25519.getSharedSecret(recipientX25519Priv, ephemeralPub);
  const symKey = deriveSealKey(shared, ephemeralPub, recipientPub);
  const nonce = hexToBytes(payload.nonce);
  const ciphertext = hexToBytes(payload.ciphertext);
  return new Uint8Array(gcm(symKey, nonce).decrypt(ciphertext));
}


export interface MemberKeyGap {
  did: string;
  missingVersions: number[];
  x25519PublicKey: string;
}

export interface RotateResult {
  version: number;
  recipients: string[];
  /** Members in the ACL who lack one or more historical key versions. */
  membersNeedingHistoricalKeys: MemberKeyGap[];
}

/**
 * Generates a fresh room key, seals it to every current ACL member that
 * has a registered X25519 public key, stores the sealed copies, marks the
 * room E2E-enabled, and discards the plaintext key. This serves as both
 * "enable E2E" (first call) and "rotate" (subsequent calls).
 *
 * Key ring model: each rotation creates a new version. Members receive
 * the new version sealed to their X25519 public key. Historical versions
 * remain in `room_keys` — `GET /rooms/:roomId/keys` returns all versions
 * a member has access to, so they can decrypt links encrypted under any
 * past version they were present for.
 *
 * Forward secrecy: a removed member keeps versions 1–N (already
 * delivered) but never receives N+1. New members added after a rotation
 * can only decrypt links from the version they first received onward.
 *
 * Members whose X25519 public key has not yet been registered are
 * skipped — they receive their sealed copy on the next rotation after
 * they authenticate.
 */
export function rotateRoomKey(db: LinkServerDB, roomId: string): RotateResult {
  const aclRows = db.getAcl(roomId);
  const roomKey = generateRoomKey();
  const version = db.getLatestKeyVersion(roomId) + 1;
  const recipients: string[] = [];
  for (const row of aclRows) {
    if (!row.x25519_public_key) continue;
    const recipientPub = hexToBytes(row.x25519_public_key);
    const encrypted = encryptRoomKeyForRecipient(roomKey, recipientPub);
    db.addRoomKey(roomId, row.did, version, JSON.stringify(encrypted));
    recipients.push(row.did);
  }
  db.setE2eEnabled(roomId, true);

  // Detect members missing historical versions so the admin can grant them.
  const membersNeedingHistoricalKeys: MemberKeyGap[] = [];
  if (version > 1) {
    const allVersions = db.getAllMemberKeyVersions(roomId);
    const expectedVersions = Array.from({ length: version }, (_, i) => i + 1);
    for (const row of aclRows) {
      if (!row.x25519_public_key) continue;
      const memberVersions = new Set(allVersions.get(row.did) ?? []);
      const missing = expectedVersions.filter((v) => !memberVersions.has(v));
      if (missing.length > 0) {
        membersNeedingHistoricalKeys.push({
          did: row.did,
          missingVersions: missing,
          x25519PublicKey: row.x25519_public_key,
        });
      }
    }
  }

  return { version, recipients, membersNeedingHistoricalKeys };
}
