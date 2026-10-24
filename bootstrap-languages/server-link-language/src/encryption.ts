/**
 * E2E encryption for room links.
 *
 * ## Room keys, sealed to members' DIDs
 *
 * A room key version is a random AES-256-GCM key. Whoever holds it seals it
 * to a member with `sealToDid`: the X25519 form of the Ed25519 key in the
 * member's `did:key` (RFC 7748 map), an ephemeral X25519 ECDH, HKDF-SHA256
 * and AES-256-GCM (docs-src/host-contract.md "Sealed boxes"). The member's
 * executor opens it with the `agentOpenSealed` host import, from the agent's
 * own key; the language never holds an agent secret. Any member's X25519
 * key follows from its DID, so the room admin seals each new version to
 * every ACL member, including one that never connected, and any member
 * holding a version can grant it to a member lacking it. The server stores
 * and relays the boxes and never sees a room key.
 *
 * ## Link confidentiality
 *
 * Once a room key is available, `encryptLinkForWire` / `decryptLinkFromWire`
 * protect the ENTIRE LinkExpression (author, timestamp, proof, and data)
 * crossing the wire. No metadata leaks — the server only sees ciphertext
 * plus a client-computed `link_hash` for OR-Set dedup. The local store
 * always holds decrypted links (see src/sync.ts); only the wire
 * representation is ciphertext.
 *
 * `decryptLinkFromWire` handles both the current full-encryption format and
 * the legacy data-only format (for backward compatibility with links
 * encrypted before the full-encryption change).
 */

import { ed25519, x25519 } from "@noble/curves/ed25519";
import { gcm } from "@noble/ciphers/aes";
import { sha256 } from "@noble/hashes/sha2";
import { hkdf } from "@noble/hashes/hkdf";

import type { AgentAdapter } from "./adapters.js";
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

// ---------------------------------------------------------------------------
// DID:key → Ed25519 public key extraction
// ---------------------------------------------------------------------------

const ED25519_MULTICODEC = new Uint8Array([0xed, 0x01]);

/**
 * Extracts the raw 32-byte Ed25519 public key from a `did:key:z...` string.
 * Only supports ed25519 keys (multicodec prefix 0xed01). Strips any
 * `#fragment` key-id suffix.
 */
function didToEd25519PublicKey(did: string): Uint8Array {
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
// Room keys sealed to DIDs (the executor's sealed boxes)
// ---------------------------------------------------------------------------

/** HKDF `info` of every sealed box, as `rust-executor/src/agent/sealed_box.rs` has it. */
export const SEALED_BOX_INFO = "ad4m:sealed-box:v1";

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

export function deriveSealKey(sharedSecret: Uint8Array, ephemeralPublicKey: Uint8Array, recipientPublicKey: Uint8Array): Uint8Array {
    const salt = new Uint8Array(64);
    salt.set(ephemeralPublicKey);
    salt.set(recipientPublicKey, 32);
    return hkdf(sha256, sharedSecret, salt, utf8ToBytes(SEALED_BOX_INFO), AES_KEY_BYTES);
}

/**
 * Seals `plaintext` to the agent `did`, which only that agent's executor
 * opens. Needs only the DID. `ephemeralSecret` and `nonce` are for test
 * vectors only.
 */
export function sealToDid(
    did: string,
    plaintext: Uint8Array,
    ephemeralSecret: Uint8Array = randomBytes(32),
    nonce: Uint8Array = randomBytes(AES_NONCE_BYTES),
): SealedBox {
    const recipientPublicKey = ed25519.utils.toMontgomery(didToEd25519PublicKey(did));
    const ephemeralPublicKey = x25519.getPublicKey(ephemeralSecret);
    const sharedSecret = x25519.getSharedSecret(ephemeralSecret, recipientPublicKey);
    const ciphertext = gcm(deriveSealKey(sharedSecret, ephemeralPublicKey, recipientPublicKey), nonce).encrypt(plaintext);
    return {
        ephemeralPublicKey: bytesToHex(ephemeralPublicKey),
        nonce: bytesToHex(nonce),
        ciphertext: bytesToHex(ciphertext),
    };
}

/**
 * Opens a room key sealed to this agent. Throws for a box sealed to anyone
 * else, an altered box, or a sealed value that is not a 32-byte key.
 */
export function openRoomKey(agent: Pick<AgentAdapter, "openSealed">, sealed: SealedBox): Uint8Array {
    const roomKey = hexToBytes(agent.openSealed(sealed));
    if (roomKey.length !== AES_KEY_BYTES) {
        throw new Error(`a room key has ${AES_KEY_BYTES} bytes, the box held ${roomKey.length}`);
    }
    return roomKey;
}

/**
 * Whether this agent's executor opens a box sealed to `did` right now. While it
 * cannot use the agent's key (a locked agent), every box fails to open, and
 * none of them is spoiled.
 */
export function opensOwnBoxes(agent: Pick<AgentAdapter, "openSealed">, did: string): boolean {
    const probe = generateRoomKey();
    try {
        return bytesToHex(openRoomKey(agent, sealToDid(did, probe))) === bytesToHex(probe);
    } catch {
        return false;
    }
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
 * holding no key ("pending").
 */
export function buildKeyRing(
    entries: KeysResponseEntry[],
    agent: Pick<AgentAdapter, "openSealed">,
): { ring: KeyRing; spoiled: number[] } {
    const ring: KeyRing = new Map();
    const spoiled: number[] = [];
    const failures: string[] = [];
    for (const entry of entries) {
        try {
            const key = openRoomKey(agent, entry.encryptedKey);
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

/**
 * Encrypts the entire LinkExpression (author, timestamp, proof, data) for
 * the wire. No metadata remains in cleartext — the server receives only
 * `{data: {ciphertext, nonce}, link_hash}`. The `link_hash` is a SHA-256
 * of the canonical plaintext fields, computed client-side so the server
 * can still content-address links for OR-Set dedup/removal matching
 * without seeing the plaintext.
 */
export function encryptLinkForWire(link: LinkExpression, roomKey: Uint8Array, keyVersion?: number): WireLinkExpression {
    const canonical = JSON.stringify({
        source: link.data.source,
        predicate: link.data.predicate ?? null,
        target: link.data.target,
        author: link.author,
        timestamp: link.timestamp,
    });
    const linkHashHex = bytesToHex(sha256(utf8ToBytes(canonical)));

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
        data: { ciphertext: bytesToHex(ciphertext), nonce: bytesToHex(nonce) },
        link_hash: linkHashHex,
        ...(keyVersion !== undefined ? { key_version: keyVersion } : {}),
    };
}

/**
 * Reverses encryptLinkForWire. Handles both formats:
 * - Full encryption (current): ciphertext contains `{author, timestamp, proof, data}`
 * - Legacy data-only: ciphertext contains `{source, predicate, target}`
 * Throws if `roomKey` is wrong or the ciphertext was tampered with
 * (AES-GCM auth tag failure).
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
    const nonce = hexToBytes(wireLink.data.nonce);
    const ciphertext = hexToBytes(wireLink.data.ciphertext);
    const plaintext = gcm(roomKey, nonce).decrypt(ciphertext);
    const parsed = JSON.parse(bytesToUtf8(plaintext));

    if (parsed.author && parsed.data && typeof parsed.data === "object") {
        return {
            author: parsed.author,
            timestamp: parsed.timestamp,
            proof: parsed.proof,
            ...statusField(parsed.status),
            data: parsed.data as Link,
        };
    }

    return {
        author: wireLink.author ?? "",
        timestamp: wireLink.timestamp ?? "",
        proof: wireLink.proof ?? { signature: "", key: "" },
        ...statusField(wireLink.status),
        data: parsed as Link,
    };
}
