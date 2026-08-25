import { createHash, hkdfSync, randomBytes as nodeRandomBytes } from "node:crypto";
import * as ed from "@noble/ed25519";
import { x25519, edwardsToMontgomeryPub, edwardsToMontgomeryPriv } from "@noble/curves/ed25519.js";
import { gcm } from "@noble/ciphers/aes.js";
import { didToPublicKey } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import type { EncryptedLinkData, LinkData } from "./types.js";

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

/** Derives a member's X25519 public key from their `did:key` ed25519 identity. */
export function x25519PublicKeyFromDid(did: string): Uint8Array {
  return edwardsToMontgomeryPub(didToPublicKey(did));
}

/** Derives an X25519 private key from a raw ed25519 private key (seed). Used client-side. */
export function x25519PrivateKeyFromEd25519(ed25519PrivateKey: Uint8Array): Uint8Array {
  return edwardsToMontgomeryPriv(ed25519PrivateKey);
}

/**
 * Seals a room key to a recipient's X25519 public key via one-shot ECIES
 * (ephemeral X25519 + HKDF-SHA256 + AES-256-GCM).
 *
 * `recipientX25519Pub` should come from the ACL table's `x25519_public_key`
 * column (populated during DID auth when the language sends its derived
 * key). Falls back to `edwardsToMontgomeryPub(didToPublicKey(did))` for
 * clients that can do the standard Ed25519→X25519 scalar conversion.
 */
export function encryptRoomKeyForRecipient(
  roomKey: Uint8Array,
  recipientX25519Pub: Uint8Array
): EncryptedKeyPayload {
  const ephemeralPriv = x25519.utils.randomSecretKey();
  const ephemeralPub = x25519.getPublicKey(ephemeralPriv);
  const shared = x25519.getSharedSecret(ephemeralPriv, recipientX25519Pub);
  const symKey = deriveSealKey(shared, ephemeralPub, recipientX25519Pub);
  const nonce = nodeRandomBytes(12);
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

/**
 * Convenience wrapper: converts a raw Ed25519 private key to X25519 via
 * edwardsToMontgomeryPriv, then unseals. Used by tests and any client
 * that holds the raw Ed25519 key material (as opposed to sandboxed
 * clients that derive X25519 from a signing capability).
 */
export function decryptRoomKeyForDid(
  payload: EncryptedKeyPayload,
  recipientEd25519PrivateKey: Uint8Array
): Uint8Array {
  const recipientX25519Priv = x25519PrivateKeyFromEd25519(recipientEd25519PrivateKey);
  return decryptRoomKeyWithX25519(payload, recipientX25519Priv);
}

/** Encrypts a link's {source,predicate,target} with the room's AES-256-GCM key. */
export function encryptLinkData(roomKey: Uint8Array, data: LinkData): EncryptedLinkData {
  const nonce = new Uint8Array(nodeRandomBytes(12));
  const plaintext = new TextEncoder().encode(JSON.stringify(data));
  const ciphertext = gcm(roomKey, nonce).encrypt(plaintext);
  return { ciphertext: bytesToHex(ciphertext), nonce: bytesToHex(nonce) };
}

/** Decrypts a link's data with the room's AES-256-GCM key. Used client-side (server never does this). */
export function decryptLinkData(roomKey: Uint8Array, encrypted: EncryptedLinkData): LinkData {
  const nonce = hexToBytes(encrypted.nonce);
  const ciphertext = hexToBytes(encrypted.ciphertext);
  const plaintext = gcm(roomKey, nonce).decrypt(ciphertext);
  return JSON.parse(new TextDecoder().decode(plaintext)) as LinkData;
}

export interface RotateResult {
  version: number;
  recipients: string[];
}

/**
 * Generates a fresh room key, seals it to every current ACL member, stores
 * the sealed copies, marks the room E2E-enabled, and discards the
 * plaintext key. This is both "enable E2E" (first call) and "rotate"
 * (subsequent calls) — a room becomes E2E-enabled the first time an admin
 * rotates its key.
 *
 * For each member, uses their stored X25519 public key if available
 * (registered during DID auth by the server-link-language), otherwise
 * falls back to deriving it from their Ed25519 DID key via
 * edwardsToMontgomeryPub.
 *
 * Members added to the ACL *after* a rotation cannot decrypt history until
 * the next rotation — the server does not retain plaintext keys to reseal
 * on demand, by design.
 */
export function rotateRoomKey(db: LinkServerDB, roomId: string): RotateResult {
  const aclRows = db.getAcl(roomId);
  const roomKey = generateRoomKey();
  const version = db.getLatestKeyVersion(roomId) + 1;
  const recipients: string[] = [];
  for (const row of aclRows) {
    let recipientPub: Uint8Array;
    if (row.x25519_public_key) {
      recipientPub = hexToBytes(row.x25519_public_key);
    } else {
      recipientPub = x25519PublicKeyFromDid(row.did);
    }
    const encrypted = encryptRoomKeyForRecipient(roomKey, recipientPub);
    db.addRoomKey(roomId, row.did, version, JSON.stringify(encrypted));
    recipients.push(row.did);
  }
  db.setE2eEnabled(roomId, true);
  return { version, recipients };
}
