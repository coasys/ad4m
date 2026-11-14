/**
 * The room-key seal v2 (K9) written out in noble, apart from `ad4m-keyring`: the reference that
 * `ad4m-keyring/vectors/room-key-seal-v2.json` was made with and is checked against
 * (tests/keyring.test.ts). The keyring's Rust tests and its wasm build must give the same bytes.
 *
 * X25519 of an ephemeral secret and the recipient's key; `HKDF-SHA256(shared, salt = ephemeral
 * public || recipient public, info = "ad4m:room-key-seal:v2")` as an AES-256-GCM key; a 12-byte
 * nonce; the context (`coasys:room-key:<roomId>:<version>`, UTF-8) as the associated data.
 */

import { gcm } from "@noble/ciphers/aes";
import { x25519 } from "@noble/curves/ed25519";
import { hkdf } from "@noble/hashes/hkdf";
import { sha256 } from "@noble/hashes/sha256";

import { bytesToHex, hexToBytes } from "../src/encryption.js";
import type { SealedBox } from "../src/types.js";

export const ROOM_KEY_SEAL_V2_INFO = "ad4m:room-key-seal:v2";

function sealKey(shared: Uint8Array, ephemeralPublic: Uint8Array, recipientPublic: Uint8Array): Uint8Array {
    const salt = new Uint8Array(64);
    salt.set(ephemeralPublic);
    salt.set(recipientPublic, 32);
    return hkdf(sha256, shared, salt, ROOM_KEY_SEAL_V2_INFO, 32);
}

/** Seals `roomKey` to `recipientX25519` under `context`, with the ephemeral secret and nonce given. */
export function referenceSeal(
    roomKey: Uint8Array,
    recipientX25519: Uint8Array,
    context: string,
    ephemeralSecret: Uint8Array,
    nonce: Uint8Array,
): SealedBox {
    const ephemeralPublic = x25519.getPublicKey(ephemeralSecret);
    const shared = x25519.getSharedSecret(ephemeralSecret, recipientX25519);
    const aad = new TextEncoder().encode(context);
    const ciphertext = gcm(sealKey(shared, ephemeralPublic, recipientX25519), nonce, aad).encrypt(roomKey);
    return {
        ephemeralPublicKey: bytesToHex(ephemeralPublic),
        nonce: bytesToHex(nonce),
        ciphertext: bytesToHex(ciphertext),
    };
}

/** Opens a v2 seal with the recipient's X25519 secret under `context`; throws when it does not open. */
export function referenceOpen(sealed: SealedBox, x25519Secret: Uint8Array, context: string): Uint8Array {
    const ephemeralPublic = hexToBytes(sealed.ephemeralPublicKey);
    const recipientPublic = x25519.getPublicKey(x25519Secret);
    const shared = x25519.getSharedSecret(x25519Secret, ephemeralPublic);
    const aad = new TextEncoder().encode(context);
    return gcm(sealKey(shared, ephemeralPublic, recipientPublic), hexToBytes(sealed.nonce), aad).decrypt(
        hexToBytes(sealed.ciphertext),
    );
}
