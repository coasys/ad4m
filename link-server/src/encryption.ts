import { hkdfSync, randomBytes as nodeRandomBytes } from "node:crypto";
import * as ed from "@noble/ed25519";
import { x25519 } from "@noble/curves/ed25519.js";
import { gcm } from "@noble/ciphers/aes.js";

/**
 * E2E encryption helpers for rooms.
 *
 * Trust model: the admin's language client generates each room's
 * AES-256-GCM symmetric key, seals it to every current member's X25519
 * public key, and POSTs only the sealed envelopes to the server. The
 * server never sees the plaintext key — it stores sealed copies in the
 * `room_keys` table and relays them to members via `GET /keys`.
 *
 * Key wrapping uses a one-shot ECIES-style construction: an ephemeral
 * X25519 keypair does ECDH with the recipient's X25519 public key, the
 * shared secret runs through HKDF-SHA256 (salt = ephPub || recipPub,
 * info = domain-separated tag) to produce the AES-256-GCM key that seals
 * the room key. This matches the server-link-language's `deriveSealKey`
 * so sealed keys can cross the wire between server and language client.
 *
 * `generateRoomKey` and `encryptRoomKeyForRecipient` remain exported for
 * use in tests (verifying sealed envelopes round-trip correctly). In
 * production, key generation and sealing happen exclusively client-side
 * in the server-link-language.
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


