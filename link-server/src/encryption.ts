import { hkdfSync, randomBytes as nodeRandomBytes } from "node:crypto";
import * as ed from "@noble/ed25519";
import { ed25519, x25519 } from "@noble/curves/ed25519.js";
import { gcm } from "@noble/ciphers/aes.js";
import { didToPublicKey } from "./auth.js";

/**
 * Room keys sealed to members' DIDs.
 *
 * Trust model: the room admin's language client generates each room's
 * AES-256-GCM key and seals it to every ACL member; any member holding a
 * version may seal it to a member lacking it. The server stores the sealed
 * boxes in `room_keys` and relays them via `GET /keys`; it never sees a room
 * key.
 *
 * A box is the AD4M executor's sealed box (`rust-executor/src/agent/sealed_box.rs`,
 * `docs-src/host-contract.md` "Sealed boxes"): the recipient's X25519 key is
 * the Montgomery form of the Ed25519 key in its `did:key`; an ephemeral X25519
 * keypair does ECDH with it, HKDF-SHA256 (salt = ephPub || recipPub, info =
 * "ad4m:sealed-box:v1") gives the AES-256-GCM key. The server only checks a
 * box's shape; `sealToDid` / `openSealed` are here for the tests, which act
 * as clients.
 */

const { bytesToHex, hexToBytes } = ed.etc;

const SEALED_BOX_INFO = "ad4m:sealed-box:v1";

/** `{ ephemeralPublicKey, nonce, ciphertext }`, hex. */
export interface SealedBox {
  ephemeralPublicKey: string;
  nonce: string;
  ciphertext: string;
}

function sealKey(shared: Uint8Array, ephemeralPublic: Uint8Array, recipientPublic: Uint8Array): Uint8Array {
  const salt = new Uint8Array(64);
  salt.set(ephemeralPublic);
  salt.set(recipientPublic, 32);
  return new Uint8Array(hkdfSync("sha256", shared, salt, SEALED_BOX_INFO, 32));
}

export function generateRoomKey(): Uint8Array {
  return new Uint8Array(nodeRandomBytes(32));
}

/** Seals `plaintext` to the agent `did`, as the executor's `agentSealToDid` does. */
export function sealToDid(did: string, plaintext: Uint8Array): SealedBox {
  const recipientPublic = ed25519.utils.toMontgomery(didToPublicKey(did));
  const ephemeralSecret = x25519.utils.randomSecretKey();
  const ephemeralPublic = x25519.getPublicKey(ephemeralSecret);
  const shared = x25519.getSharedSecret(ephemeralSecret, recipientPublic);
  const nonce = new Uint8Array(nodeRandomBytes(12));
  const ciphertext = gcm(sealKey(shared, ephemeralPublic, recipientPublic), nonce).encrypt(plaintext);
  return {
    ephemeralPublicKey: bytesToHex(ephemeralPublic),
    nonce: bytesToHex(nonce),
    ciphertext: bytesToHex(ciphertext),
  };
}

/** Opens a box sealed to the agent whose Ed25519 secret key is `ed25519Secret`. */
export function openSealed(sealed: SealedBox, ed25519Secret: Uint8Array): Uint8Array {
  const secret = ed25519.utils.toMontgomerySecret(ed25519Secret);
  const recipientPublic = ed25519.utils.toMontgomery(ed25519.getPublicKey(ed25519Secret));
  const ephemeralPublic = hexToBytes(sealed.ephemeralPublicKey);
  const shared = x25519.getSharedSecret(secret, ephemeralPublic);
  return new Uint8Array(
    gcm(sealKey(shared, ephemeralPublic, recipientPublic), hexToBytes(sealed.nonce)).decrypt(
      hexToBytes(sealed.ciphertext)
    )
  );
}
