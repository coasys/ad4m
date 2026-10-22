/**
 * Reference implementation of the executor's sealed box
 * (`rust-executor/src/agent/sealed_box.rs`), for the mock agents of these
 * tests. The language itself never seals or opens: it calls the
 * `agentSealToDid` / `agentOpenSealed` host imports.
 *
 * Format: the recipient's X25519 key is the Montgomery form of the Ed25519
 * key in its `did:key` (RFC 7748 map, libsodium
 * `crypto_sign_ed25519_pk_to_curve25519`); the sender makes an ephemeral
 * X25519 keypair, `key = HKDF-SHA256(ECDH, salt = ephPub || recipPub,
 * info = "ad4m:sealed-box:v1")`, and encrypts with AES-256-GCM. All fields hex.
 */

import { ed25519, x25519 } from "@noble/curves/ed25519";
import { gcm } from "@noble/ciphers/aes";
import { sha256 } from "@noble/hashes/sha2";
import { hkdf } from "@noble/hashes/hkdf";

import { bytesToHex, hexToBytes, randomBytes } from "../src/encryption.js";
import type { SealedBox } from "../src/types.js";

export const SEALED_BOX_INFO = "ad4m:sealed-box:v1";

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

function base58Decode(input: string): Uint8Array {
    const bytes: number[] = [0];
    for (const ch of input) {
        const val = BASE58_ALPHABET.indexOf(ch);
        if (val < 0) throw new Error(`base58: invalid character '${ch}'`);
        let carry = val;
        for (let j = 0; j < bytes.length; j++) {
            carry += bytes[j] * 58;
            bytes[j] = carry & 0xff;
            carry >>= 8;
        }
        while (carry > 0) {
            bytes.push(carry & 0xff);
            carry >>= 8;
        }
    }
    let leadingZeros = 0;
    for (const ch of input) {
        if (ch !== "1") break;
        leadingZeros++;
    }
    const result = new Uint8Array(leadingZeros + bytes.length);
    result.set(bytes.reverse(), leadingZeros);
    return result;
}

/** The `did:key` of an Ed25519 public key (multicodec 0xed01, base58btc). */
export function didForEd25519PublicKey(publicKey: Uint8Array): string {
    const prefixed = new Uint8Array(34);
    prefixed.set([0xed, 0x01]);
    prefixed.set(publicKey, 2);
    return `did:key:z${base58Encode(prefixed)}`;
}

export function ed25519PublicKeyOfDid(did: string): Uint8Array {
    const decoded = base58Decode(did.slice("did:key:z".length));
    if (decoded.length !== 34 || decoded[0] !== 0xed || decoded[1] !== 0x01) {
        throw new Error(`not an Ed25519 did:key: ${did}`);
    }
    return decoded.slice(2);
}

function sealKey(shared: Uint8Array, ephemeralPublic: Uint8Array, recipientPublic: Uint8Array): Uint8Array {
    const salt = new Uint8Array(64);
    salt.set(ephemeralPublic);
    salt.set(recipientPublic, 32);
    return hkdf(sha256, shared, salt, new TextEncoder().encode(SEALED_BOX_INFO), 32);
}

/** Seals `plaintext` to `did`. `ephemeralSecret` and `nonce` are for test vectors only. */
export function sealToDid(
    did: string,
    plaintext: Uint8Array,
    ephemeralSecret: Uint8Array = randomBytes(32),
    nonce: Uint8Array = randomBytes(12),
): SealedBox {
    const recipientPublic = ed25519.utils.toMontgomery(ed25519PublicKeyOfDid(did));
    const ephemeralPublic = x25519.getPublicKey(ephemeralSecret);
    const shared = x25519.getSharedSecret(ephemeralSecret, recipientPublic);
    const ciphertext = gcm(sealKey(shared, ephemeralPublic, recipientPublic), nonce).encrypt(plaintext);
    return {
        ephemeralPublicKey: bytesToHex(ephemeralPublic),
        nonce: bytesToHex(nonce),
        ciphertext: bytesToHex(ciphertext),
    };
}

/** Opens a box sealed to the DID of the Ed25519 secret key `ed25519Secret`. */
export function openSealed(sealed: SealedBox, ed25519Secret: Uint8Array): Uint8Array {
    const secret = ed25519.utils.toMontgomerySecret(ed25519Secret);
    const recipientPublic = ed25519.utils.toMontgomery(ed25519.getPublicKey(ed25519Secret));
    const ephemeralPublic = hexToBytes(sealed.ephemeralPublicKey);
    const shared = x25519.getSharedSecret(secret, ephemeralPublic);
    return gcm(sealKey(shared, ephemeralPublic, recipientPublic), hexToBytes(sealed.nonce)).decrypt(
        hexToBytes(sealed.ciphertext),
    );
}
