import { randomBytes as nodeRandomBytes, createHash } from "node:crypto";
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
 * X25519 keypair does ECDH with the recipient's X25519 public key (derived
 * from their ed25519 DID key via edwardsToMontgomeryPub), SHA-256 of the
 * shared secret becomes the AES-256-GCM key, which seals the room key.
 */

function bytesToHex(bytes: Uint8Array): string {
  return Buffer.from(bytes).toString("hex");
}

function hexToBytes(hex: string): Uint8Array {
  return new Uint8Array(Buffer.from(hex, "hex"));
}

function kdf(sharedSecret: Uint8Array): Uint8Array {
  return new Uint8Array(createHash("sha256").update(sharedSecret).digest());
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

/** Seals a room key to a member's DID via one-shot ECIES (ephemeral X25519 + AES-256-GCM). */
export function encryptRoomKeyForDid(
  roomKey: Uint8Array,
  recipientDid: string
): EncryptedKeyPayload {
  const recipientPub = x25519PublicKeyFromDid(recipientDid);
  const ephemeralPriv = x25519.utils.randomSecretKey();
  const ephemeralPub = x25519.getPublicKey(ephemeralPriv);
  const shared = x25519.getSharedSecret(ephemeralPriv, recipientPub);
  const symKey = kdf(shared);
  const nonce = nodeRandomBytes(12);
  const ciphertext = gcm(symKey, nonce).encrypt(roomKey);
  return {
    ephemeralPublicKey: bytesToHex(ephemeralPub),
    nonce: bytesToHex(nonce),
    ciphertext: bytesToHex(ciphertext),
  };
}

/** Unseals a room key using the recipient's raw ed25519 private key. Used client-side. */
export function decryptRoomKeyForDid(
  payload: EncryptedKeyPayload,
  recipientEd25519PrivateKey: Uint8Array
): Uint8Array {
  const recipientMontPriv = x25519PrivateKeyFromEd25519(recipientEd25519PrivateKey);
  const ephemeralPub = hexToBytes(payload.ephemeralPublicKey);
  const shared = x25519.getSharedSecret(recipientMontPriv, ephemeralPub);
  const symKey = kdf(shared);
  const nonce = hexToBytes(payload.nonce);
  const ciphertext = hexToBytes(payload.ciphertext);
  return new Uint8Array(gcm(symKey, nonce).decrypt(ciphertext));
}

/** Encrypts a link's {source,predicate,target} with the room's AES-256-GCM key. */
export function encryptLinkData(roomKey: Uint8Array, data: LinkData): EncryptedLinkData {
  const nonce = nodeRandomBytes(12);
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
 * Members added to the ACL *after* a rotation cannot decrypt history until
 * the next rotation — the server does not retain plaintext keys to reseal
 * on demand, by design.
 */
export function rotateRoomKey(db: LinkServerDB, roomId: string): RotateResult {
  const members = db.getAcl(roomId).map((a) => a.did);
  const roomKey = generateRoomKey();
  const version = db.getLatestKeyVersion(roomId) + 1;
  for (const did of members) {
    const encrypted = encryptRoomKeyForDid(roomKey, did);
    db.addRoomKey(roomId, did, version, JSON.stringify(encrypted));
  }
  db.setE2eEnabled(roomId, true);
  return { version, recipients: members };
}
