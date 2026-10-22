/**
 * The executor's opening of a sealed box (`rust-executor/src/agent/sealed_box.rs`),
 * for the mock agents of these tests. The language seals itself
 * (`sealToDid` in src/encryption.ts) and opens with the `agentOpenSealed`
 * host import.
 */

import { ed25519, x25519 } from "@noble/curves/ed25519";
import { gcm } from "@noble/ciphers/aes";

import { deriveSealKey, hexToBytes } from "../src/encryption.js";
import type { SealedBox } from "../src/types.js";

const BASE58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

function base58Encode(bytes: Uint8Array): string {
    const digits: number[] = [0];
    for (const byte of bytes) {
        let carry = byte;
        for (let j = 0; j < digits.length; j++) {
            carry += digits[j] << 8;
            digits[j] = carry % 58;
            carry = (carry / 58) | 0;
        }
        while (carry > 0) {
            digits.push(carry % 58);
            carry = (carry / 58) | 0;
        }
    }
    let out = "";
    for (const byte of bytes) {
        if (byte !== 0) break;
        out += "1";
    }
    for (let i = digits.length - 1; i >= 0; i--) out += BASE58_ALPHABET[digits[i]];
    return out;
}

/** The `did:key` of an Ed25519 public key (multicodec 0xed01, base58btc). */
export function didForEd25519PublicKey(publicKey: Uint8Array): string {
    const prefixed = new Uint8Array(34);
    prefixed.set([0xed, 0x01]);
    prefixed.set(publicKey, 2);
    return `did:key:z${base58Encode(prefixed)}`;
}

/** Opens a box sealed to the DID of the Ed25519 secret key `ed25519Secret`. */
export function openSealed(sealed: SealedBox, ed25519Secret: Uint8Array): Uint8Array {
    const secret = ed25519.utils.toMontgomerySecret(ed25519Secret);
    const recipientPublic = ed25519.utils.toMontgomery(ed25519.getPublicKey(ed25519Secret));
    const ephemeralPublic = hexToBytes(sealed.ephemeralPublicKey);
    const shared = x25519.getSharedSecret(secret, ephemeralPublic);
    return gcm(deriveSealKey(shared, ephemeralPublic, recipientPublic), hexToBytes(sealed.nonce)).decrypt(
        hexToBytes(sealed.ciphertext),
    );
}
