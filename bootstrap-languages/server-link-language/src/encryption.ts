/**
 * E2E encryption for room links.
 *
 * ## Room keys, sealed to members' X25519 keys
 *
 * A room key version is a random AES-256-GCM key. Every key that authenticates to the room
 * publishes an X25519 key, the public half of its `agentRoomKeySeed`, with the key's
 * signature over it (src/room.ts, K7). Whoever holds a version seals it to such a key with
 * the keyring binding's `sealRoomKey` (`keyring/keyring.js`, the wasm build of
 * `ad4m-keyring`: ephemeral X25519 ECDH, HKDF-SHA256, AES-256-GCM), and the member opens it
 * with `openRoomKey` from its own seed (K9). The rotator seals each new version to every
 * admitted key, and any member holding a version can grant it to a member lacking it. The
 * server stores and relays the boxes and never sees a room key.
 *
 * ## The link hash (K4)
 *
 * An encrypted link carries `link_hash`, HMAC-SHA256 of its canonical plaintext fields under
 * the room's link-hash key (HKDF of room key version 1, `linkHashKey`). Every member computes
 * the same hash for a link, so removals match additions across key versions; the server,
 * without the key, cannot test a guessed link against it.
 *
 * ## Link confidentiality
 *
 * Once a room key is available, `encryptLinkForWire` / `decryptLinkFromWire`
 * protect the ENTIRE LinkExpression (author, timestamp, proof, and data)
 * crossing the wire. No metadata leaks — the server only sees ciphertext
 * plus a client-computed `link_hash` for OR-Set dedup. The local store
 * always holds decrypted links (see src/sync.ts); only the wire
 * representation is ciphertext.
 */

import { gcm } from "@noble/ciphers/aes";
import { sha256 } from "@noble/hashes/sha2";
import { hmac } from "@noble/hashes/hmac";

import {
    linkHashKey,
    openRoomKey as keyringOpenRoomKey,
    sealRoomKey as keyringSealRoomKey,
} from "../keyring/keyring.js";
import type {
    KeysResponseEntry,
    Link,
    LinkExpression,
    SealedBox,
    WireLinkExpression,
} from "./types.js";
import { isEncryptedLinkData } from "./types.js";

export const AES_KEY_BYTES = 32; // AES-256
export const AES_NONCE_BYTES = 12; // standard GCM nonce size

// ---------------------------------------------------------------------------
// Byte / hex helpers
// ---------------------------------------------------------------------------

export function bytesToHex(bytes: Uint8Array): string {
    let out = "";
    for (let i = 0; i < bytes.length; i++) {
        out += bytes[i].toString(16).padStart(2, "0");
    }
    return out;
}

const HEX_PAIR = /^[0-9a-fA-F]{2}$/;

export function hexToBytes(hex: string): Uint8Array {
    const clean = hex.trim();
    if (clean.length % 2 !== 0) {
        throw new Error(`hexToBytes: odd-length hex string (${clean.length} chars)`);
    }
    const out = new Uint8Array(clean.length / 2);
    for (let i = 0; i < out.length; i++) {
        const byte = clean.substring(i * 2, i * 2 + 2);
        if (!HEX_PAIR.test(byte)) {
            throw new Error(`hexToBytes: invalid hex byte "${byte}" at offset ${i * 2}`);
        }
        out[i] = parseInt(byte, 16);
    }
    return out;
}

export function bytesToBase64(bytes: Uint8Array): string {
    let binary = "";
    for (let i = 0; i < bytes.length; i++) binary += String.fromCharCode(bytes[i]);
    return btoa(binary);
}

export function base64ToBytes(b64: string): Uint8Array {
    const binary = atob(b64.trim());
    const out = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) out[i] = binary.charCodeAt(i);
    return out;
}

function utf8ToBytes(str: string): Uint8Array {
    return new TextEncoder().encode(str);
}

function bytesToUtf8(bytes: Uint8Array): string {
    return new TextDecoder().decode(bytes);
}

/** Cryptographically secure random bytes via the WebCrypto global — present
 * in both the Deno language sandbox (deno_runtime bundles deno_crypto) and
 * Node.js 20+ test environments. */
export function randomBytes(length: number): Uint8Array {
    const out = new Uint8Array(length);
    globalThis.crypto.getRandomValues(out);
    return out;
}

// ---------------------------------------------------------------------------
// Base58btc decoder (minimal — avoids @scure/base dependency)
// ---------------------------------------------------------------------------

const BASE58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
const BASE58_MAP = new Map<string, number>();
for (let i = 0; i < BASE58_ALPHABET.length; i++) BASE58_MAP.set(BASE58_ALPHABET[i], i);

function base58Decode(input: string): Uint8Array {
    if (input.length === 0) return new Uint8Array(0);
    const bytes: number[] = [0];
    for (const ch of input) {
        const val = BASE58_MAP.get(ch);
        if (val === undefined) throw new Error(`base58: invalid character '${ch}'`);
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
    // Preserve leading zeros (base58 '1' == 0x00)
    let leadingZeros = 0;
    for (const ch of input) { if (ch === "1") leadingZeros++; else break; }
    const result = new Uint8Array(leadingZeros + bytes.length);
    bytes.reverse();
    result.set(bytes, leadingZeros);
    return result;
}

/** Base58btc of `bytes` (leading zero bytes as '1'). */
export function base58Encode(bytes: Uint8Array): string {
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
    // A non-empty input of zeros only leaves the single 0 digit, already written as '1's.
    if (bytes.some((b) => b !== 0)) {
        for (let i = digits.length - 1; i >= 0; i--) out += BASE58_ALPHABET[digits[i]];
    }
    return out;
}

// ---------------------------------------------------------------------------
// DID:key → Ed25519 public key extraction
// ---------------------------------------------------------------------------

const ED25519_MULTICODEC = new Uint8Array([0xed, 0x01]);

/**
 * Extracts the raw 32-byte Ed25519 public key from a `did:key:z...` string.
 * Only supports ed25519 keys (multicodec prefix 0xed01). Strips any
 * `#fragment` key-id suffix.
 */
export function didToEd25519PublicKey(did: string): Uint8Array {
    const base = did.includes("#") ? did.slice(0, did.indexOf("#")) : did;
    const prefix = "did:key:";
    if (!base.startsWith(prefix)) throw new Error(`not a did:key DID: ${did}`);
    const multibase = base.slice(prefix.length);
    if (!multibase.startsWith("z")) throw new Error(`unsupported multibase: ${did}`);
    const decoded = base58Decode(multibase.slice(1));
    if (decoded.length !== 34 || decoded[0] !== ED25519_MULTICODEC[0] || decoded[1] !== ED25519_MULTICODEC[1]) {
        throw new Error(`unsupported did:key type: ${did}`);
    }
    return decoded.slice(2);
}

// ---------------------------------------------------------------------------
// Room keys sealed to X25519 keys (the keyring binding)
// ---------------------------------------------------------------------------

/** Generates a fresh random 32-byte AES-256 room key. */
export function generateRoomKey(): Uint8Array {
    return randomBytes(AES_KEY_BYTES);
}

export const ROOM_KEY_COMMITMENT_INFO = "ad4m:room-key-commitment:v1";

/**
 * The admin's commitment to a room key version, published with the rotation:
 * SHA-256 of a domain tag and the key, hex. A member keeps an opened key only
 * if it matches, so a granter cannot hand it a wrong key that decrypts.
 */
export function roomKeyCommitment(roomKey: Uint8Array): string {
    const tag = utf8ToBytes(ROOM_KEY_COMMITMENT_INFO);
    const input = new Uint8Array(tag.length + roomKey.length);
    input.set(tag);
    input.set(roomKey, tag.length);
    return bytesToHex(sha256(input));
}

const X25519_HEX = /^[0-9a-f]{64}$/;

/** Seals `roomKey` to the X25519 key `x25519PublicKey` (64 lowercase hex characters). */
export function sealRoomKeyTo(x25519PublicKey: string, roomKey: Uint8Array): SealedBox {
    if (!X25519_HEX.test(x25519PublicKey)) throw new Error("an X25519 key is 32 bytes of lowercase hex");
    return keyringSealRoomKey(roomKey, hexToBytes(x25519PublicKey)) as SealedBox;
}

/**
 * Opens a room key sealed to the X25519 key of `seed` (`agentRoomKeySeed`). Throws for a box
 * sealed to anyone else, an altered box, or a sealed value that is not a 32-byte key.
 */
export function openRoomKey(sealed: SealedBox, seed: Uint8Array): Uint8Array {
    const roomKey = keyringOpenRoomKey(sealed, seed);
    if (roomKey.length !== AES_KEY_BYTES) {
        throw new Error(`a room key has ${AES_KEY_BYTES} bytes, the box held ${roomKey.length}`);
    }
    return roomKey;
}

// ---------------------------------------------------------------------------
// Key ring
// ---------------------------------------------------------------------------

/** Ordered map of key version → decrypted room key. */
export type KeyRing = Map<number, Uint8Array>;

/**
 * The versions sealed to this agent whose box opens to the key the rotation
 * committed to. A box that does not open, or opens to another key (a member
 * granted a wrong one), is `spoiled`: left out with a log line, so it costs
 * that version, not the room, and the caller drops it so it is granted again.
 * When none opens, the ring is empty and the caller treats the agent as
 * holding no key ("pending"). `seed` is this agent's `agentRoomKeySeed`.
 */
export function buildKeyRing(
    entries: KeysResponseEntry[],
    seed: Uint8Array,
): { ring: KeyRing; spoiled: number[] } {
    const ring: KeyRing = new Map();
    const spoiled: number[] = [];
    const failures: string[] = [];
    for (const entry of entries) {
        try {
            const key = openRoomKey(entry.encryptedKey, seed);
            if (roomKeyCommitment(key) !== entry.commitment) {
                throw new Error("the key does not match the rotation's commitment");
            }
            ring.set(entry.version, key);
        } catch (err) {
            spoiled.push(entry.version);
            failures.push(`version ${entry.version}: ${err instanceof Error ? err.message : String(err)}`);
        }
    }
    if (failures.length > 0) {
        console.error(`[server-link-language] room key versions that do not open: ${failures.join("; ")}`);
    }
    return { ring, spoiled };
}

export function latestKeyVersion(ring: KeyRing): number {
    let max = 0;
    for (const v of ring.keys()) {
        if (v > max) max = v;
    }
    return max;
}

// ---------------------------------------------------------------------------
// Link expression encryption (room key, AES-256-GCM)
// ---------------------------------------------------------------------------

/**
 * `status` is optional on LinkExpression/WireLinkExpression. Spreading this
 * in (rather than always assigning `status: link.status`) keeps an absent
 * status truly absent from the reconstructed object instead of leaving a
 * `status: undefined` own-property behind — which would otherwise fail
 * strict deep-equality checks and any `"status" in link` guard elsewhere.
 */
export function statusField(status: string | undefined): { status?: string } {
    return status !== undefined ? { status } : {};
}

/** The room's link-hash key: `linkHashKey` of room key version 1, or null while the ring lacks it. */
export function linkHashKeyOf(ring: KeyRing): Uint8Array | null {
    const v1 = ring.get(1);
    return v1 ? linkHashKey(v1) : null;
}

/**
 * The `link_hash` an encrypted wire link carries (K4): HMAC-SHA256 under the room's
 * link-hash key (`linkHashKeyOf`) of the canonical plaintext fields. Identifies a link across
 * its encrypted copies (each has a fresh nonce) for members only.
 */
export function wireLinkHash(link: LinkExpression, hashKey: Uint8Array): string {
    const canonical = JSON.stringify({
        source: link.data.source,
        predicate: link.data.predicate ?? null,
        target: link.data.target,
        author: link.author,
        timestamp: link.timestamp,
    });
    return bytesToHex(hmac(sha256, hashKey, utf8ToBytes(canonical)));
}

/** The keys a link is encrypted with: the room key version, and the room's link-hash key. */
export interface WireKeys {
    roomKey: Uint8Array;
    /** The room key's version, written as `key_version` (absent: version 1). */
    version?: number;
    hashKey: Uint8Array;
}

/**
 * Encrypts the entire LinkExpression (author, timestamp, proof, data) for
 * the wire. No metadata remains in cleartext — the server receives only
 * `{data: {ciphertext, nonce}, link_hash}`. The `link_hash` (`wireLinkHash`)
 * lets the server content-address links for OR-Set dedup/removal matching
 * without seeing the plaintext.
 */
export function encryptLinkForWire(link: LinkExpression, keys: WireKeys): WireLinkExpression {
    const { roomKey, version: keyVersion } = keys;
    const linkHash = wireLinkHash(link, keys.hashKey);

    const fullPayload: Record<string, unknown> = {
        author: link.author,
        timestamp: link.timestamp,
        proof: link.proof,
        data: link.data,
    };
    if (link.status !== undefined) fullPayload.status = link.status;
    const plaintext = utf8ToBytes(JSON.stringify(fullPayload));
    const nonce = randomBytes(AES_NONCE_BYTES);
    const ciphertext = gcm(roomKey, nonce).encrypt(plaintext);

    return {
        data: { ciphertext: bytesToBase64(ciphertext), nonce: bytesToBase64(nonce) },
        link_hash: linkHash,
        ...(keyVersion !== undefined ? { key_version: keyVersion } : {}),
    };
}

/**
 * Reverses encryptLinkForWire: the ciphertext holds `{author, timestamp,
 * proof, data}`. Throws if `roomKey` is wrong, the ciphertext was tampered
 * with (AES-GCM auth tag failure), or the payload is not that shape.
 */
export function decryptLinkFromWire(wireLink: WireLinkExpression, roomKeyOrRing: Uint8Array | KeyRing): LinkExpression {
    if (!isEncryptedLinkData(wireLink.data)) {
        throw new Error("decryptLinkFromWire: wire link has no encrypted data (expected {ciphertext, nonce} in data)");
    }
    let roomKey: Uint8Array;
    if (roomKeyOrRing instanceof Map) {
        const version = wireLink.key_version ?? 1;
        const key = roomKeyOrRing.get(version);
        if (!key) {
            throw new Error(
                `decryptLinkFromWire: no key for version ${version} in ring ` +
                `(have versions: ${[...roomKeyOrRing.keys()].join(", ")})`,
            );
        }
        roomKey = key;
    } else {
        roomKey = roomKeyOrRing;
    }
    const nonce = base64ToBytes(wireLink.data.nonce);
    const ciphertext = base64ToBytes(wireLink.data.ciphertext);
    const plaintext = gcm(roomKey, nonce).decrypt(ciphertext);
    const parsed = JSON.parse(bytesToUtf8(plaintext));

    if (
        !parsed || typeof parsed !== "object" || !parsed.author || typeof parsed.timestamp !== "string" ||
        !parsed.proof || typeof parsed.proof !== "object" || !parsed.data || typeof parsed.data !== "object"
    ) {
        throw new Error("decryptLinkFromWire: the decrypted payload is not {author, timestamp, proof, data}");
    }
    return {
        author: parsed.author,
        timestamp: parsed.timestamp,
        proof: parsed.proof,
        ...statusField(parsed.status),
        data: parsed.data as Link,
    };
}
