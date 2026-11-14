/**
 * Tests for src/encryption.ts: room keys sealed to members' X25519 keys with the keyring
 * binding (AR-08 K9), the HMAC link hash (K4), and AES-256-GCM link encryption. No mocking of
 * the crypto primitives: members open with the keyring binding from their own seed, so a
 * broken wire format or a swapped key fails the round trip.
 */

import { readFileSync } from "node:fs";
import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { gcm } from "@noble/ciphers/aes";
import { hkdf } from "@noble/hashes/hkdf";
import { hmac } from "@noble/hashes/hmac";
import { sha256 } from "@noble/hashes/sha256";

import { x25519Public } from "../keyring/keyring.js";
import {
    AES_KEY_BYTES,
    buildKeyRing,
    bytesToBase64,
    bytesToHex,
    decryptLinkFromWire,
    encryptLinkForWire,
    generateRoomKey,
    hexToBytes,
    latestKeyVersion,
    linkHashKeyOf,
    openRoomKey,
    randomBytes,
    roomKeyCommitment,
    sealRoomKeyTo,
    wireLinkHash,
} from "../src/encryption.js";
import type { KeysResponseEntry, LinkExpression } from "../src/types.js";
import { isEncryptedLinkData } from "../src/types.js";

/** A member: the seed `agentRoomKeySeed` gives it, and the X25519 key it publishes at auth. */
function member(seed: Uint8Array = randomBytes(32)): { seed: Uint8Array; x25519: string } {
    return { seed, x25519: bytesToHex(x25519Public(seed)) };
}

/** A room key's encryption and link-hash keys, as a ring with this key as version 1 gives them. */
function keys(roomKey: Uint8Array = generateRoomKey(), version?: number) {
    const hashKey = linkHashKeyOf(new Map([[1, roomKey]]))!;
    return { roomKey, hashKey, ...(version !== undefined ? { version } : {}) };
}

// ---------------------------------------------------------------------------
// hex helpers
// ---------------------------------------------------------------------------

describe("encryption: hex helpers", () => {
    it("round-trips bytes through hex", () => {
        const bytes = randomBytes(32);
        assert.deepEqual(hexToBytes(bytesToHex(bytes)), bytes);
    });

    it("throws on odd-length hex", () => {
        assert.throws(() => hexToBytes("abc"));
    });

    it("throws on invalid hex characters", () => {
        assert.throws(() => hexToBytes("zz"));
    });
});

// ---------------------------------------------------------------------------
// Room keys sealed to X25519 keys (K9)
// ---------------------------------------------------------------------------

describe("encryption: sealRoomKeyTo / openRoomKey", () => {
    it("opens ad4m-keyring's vectors from the recipient's seed", () => {
        // `ad4m-keyring` (Rust and its wasm build) seals and opens these same envelopes.
        const file = new URL("../../../ad4m-keyring/vectors/room-key-seal.json", import.meta.url);
        const { vectors } = JSON.parse(readFileSync(file, "utf8"));
        assert.ok(vectors.length > 0);
        for (const v of vectors) {
            assert.equal(bytesToHex(openRoomKey(v.sealed, hexToBytes(v.x25519Secret))), v.roomKey);
        }
    });

    it("seals a room key to a member's X25519 key, and the member opens it with its seed", () => {
        const m = member();
        const roomKey = generateRoomKey();
        assert.deepEqual(openRoomKey(sealRoomKeyTo(m.x25519, roomKey), m.seed), roomKey);
    });

    it("produces a different box each time (fresh ephemeral key + nonce)", () => {
        const m = member();
        const roomKey = generateRoomKey();
        const box1 = sealRoomKeyTo(m.x25519, roomKey);
        const box2 = sealRoomKeyTo(m.x25519, roomKey);
        assert.notEqual(box1.ephemeralPublicKey, box2.ephemeralPublicKey);
        assert.notEqual(box1.ciphertext, box2.ciphertext);
        assert.deepEqual(openRoomKey(box1, m.seed), roomKey);
        assert.deepEqual(openRoomKey(box2, m.seed), roomKey);
    });

    it("another member cannot open it", () => {
        const sealed = sealRoomKeyTo(member().x25519, generateRoomKey());
        assert.throws(() => openRoomKey(sealed, member().seed));
    });

    it("refuses an X25519 key that is not 32 bytes of hex", () => {
        assert.throws(() => sealRoomKeyTo("11".repeat(31), generateRoomKey()));
        assert.throws(() => sealRoomKeyTo("zz".repeat(32), generateRoomKey()));
    });

    it("generateRoomKey returns AES_KEY_BYTES (32) random bytes", () => {
        const key = generateRoomKey();
        assert.equal(key.length, AES_KEY_BYTES);
        assert.notDeepEqual(key, generateRoomKey());
    });
});

// ---------------------------------------------------------------------------
// The link hash (K4)
// ---------------------------------------------------------------------------

function makeLink(overrides?: Partial<LinkExpression["data"]>): LinkExpression {
    return {
        author: "did:key:z6MkAuthor",
        timestamp: "2026-01-01T00:00:00.000Z",
        data: {
            source: "channel://main",
            target: "expr://msg-001",
            predicate: "flux://has_message",
            ...overrides,
        },
        proof: { signature: "sig", key: "key" },
    };
}

describe("encryption: the link hash (K4)", () => {
    it("is HMAC-SHA256 under the link-hash key of room key version 1, over the canonical link", () => {
        const roomKey = generateRoomKey();
        const hashKey = linkHashKeyOf(new Map([[1, roomKey], [2, generateRoomKey()]]));
        assert.deepEqual(hashKey, hkdf(sha256, roomKey, undefined, "adam-server-link-language:link-hash:v1", 32));
        const link = makeLink();
        const canonical =
            '{"source":"channel://main","predicate":"flux://has_message","target":"expr://msg-001",' +
            '"author":"did:key:z6MkAuthor","timestamp":"2026-01-01T00:00:00.000Z"}';
        assert.equal(wireLinkHash(link, hashKey!), bytesToHex(hmac(sha256, hashKey!, new TextEncoder().encode(canonical))));
        assert.notEqual(
            wireLinkHash(link, hashKey!),
            bytesToHex(sha256(new TextEncoder().encode(canonical))),
            "the server cannot compute it from a guessed link",
        );
    });

    it("differs between rooms with different keys", () => {
        const link = makeLink();
        assert.notEqual(wireLinkHash(link, keys().hashKey), wireLinkHash(link, keys().hashKey));
    });

    it("has no key until the ring holds version 1", () => {
        assert.equal(linkHashKeyOf(new Map()), null);
        assert.equal(linkHashKeyOf(new Map([[2, generateRoomKey()]])), null);
    });
});

// ---------------------------------------------------------------------------
// Link expression wire encryption
// ---------------------------------------------------------------------------

describe("encryption: encryptLinkForWire / decryptLinkFromWire", () => {
    it("round-trips a link through the room key (full encryption)", () => {
        const k = keys();
        const link = makeLink();

        const wire = encryptLinkForWire(link, k);
        assert.ok(isEncryptedLinkData(wire.data));
        assert.equal(typeof (wire.data as any).ciphertext, "string");
        assert.equal(typeof (wire.data as any).nonce, "string");
        assert.equal(wire.author, undefined);
        assert.equal(wire.timestamp, undefined);
        assert.equal(wire.proof, undefined);
        assert.equal(wire.link_hash, wireLinkHash(link, k.hashKey));

        const decrypted = decryptLinkFromWire(wire, k.roomKey);
        assert.deepEqual(decrypted, link);
    });

    it("encrypts everything — no metadata in the clear", () => {
        const link = makeLink({ source: "s3cr3t://topic" });
        const wire = encryptLinkForWire(link, keys());

        assert.equal(wire.author, undefined);
        assert.equal(wire.timestamp, undefined);
        assert.equal(wire.proof, undefined);
        assert.equal(Buffer.from((wire.data as any).ciphertext, "base64").includes(Buffer.from("s3cr3t")), false);
    });

    it("writes ciphertext and nonce in canonical base64", () => {
        const wire = encryptLinkForWire(makeLink(), keys(undefined, 1));
        const data = wire.data as { ciphertext: string; nonce: string };
        assert.match(data.nonce, /^[A-Za-z0-9+/]{16}$/, "a 12-byte nonce is 16 base64 characters");
        const ciphertextBytes = Buffer.from(data.ciphertext, "base64");
        assert.equal(ciphertextBytes.toString("base64"), data.ciphertext, "the ciphertext is canonical base64");
    });

    it("uses a fresh nonce per call (different ciphertext for identical input)", () => {
        const k = keys();
        const link = makeLink();
        const wire1 = encryptLinkForWire(link, k);
        const wire2 = encryptLinkForWire(link, k);
        assert.notEqual((wire1.data as any).ciphertext, (wire2.data as any).ciphertext);
    });

    it("fails to decrypt with the wrong room key", () => {
        const wire = encryptLinkForWire(makeLink(), keys());
        assert.throws(() => decryptLinkFromWire(wire, generateRoomKey()));
    });

    it("throws when asked to decrypt a wire link with no encrypted data in data field", () => {
        const roomKey = generateRoomKey();
        assert.throws(() => decryptLinkFromWire({ author: "a", timestamp: "t", proof: { signature: "s", key: "k" } } as any, roomKey));
    });

    it("throws when the key opens the ciphertext but the payload is only a link's data", () => {
        // Author, timestamp and proof always travel inside the ciphertext: a data-only payload is not a link.
        const roomKey = generateRoomKey();
        const nonce = randomBytes(12);
        const payload = new TextEncoder().encode(JSON.stringify({ source: "s", predicate: "p", target: "t" }));
        const wire = {
            author: "did:key:z6MkCleartext",
            timestamp: "2026-01-01T00:00:00.000Z",
            data: { ciphertext: bytesToBase64(gcm(roomKey, nonce).encrypt(payload)), nonce: bytesToBase64(nonce) },
        };
        assert.throws(() => decryptLinkFromWire(wire, roomKey), /not \{author, timestamp, proof, data\}/);
    });

    it("throws when the payload has author and data but no timestamp or proof", () => {
        const roomKey = generateRoomKey();
        const nonce = randomBytes(12);
        const payload = new TextEncoder().encode(
            JSON.stringify({ author: "did:key:z6MkA", data: { source: "s", predicate: "p", target: "t" } }),
        );
        const wire = { data: { ciphertext: bytesToBase64(gcm(roomKey, nonce).encrypt(payload)), nonce: bytesToBase64(nonce) } };
        assert.throws(() => decryptLinkFromWire(wire as any, roomKey), /not \{author, timestamp, proof, data\}/);
    });

    it("attaches key_version when provided", () => {
        const wire = encryptLinkForWire(makeLink(), keys(undefined, 3));
        assert.equal(wire.key_version, 3);
    });

    it("omits key_version when not provided", () => {
        const wire = encryptLinkForWire(makeLink(), keys());
        assert.equal("key_version" in wire, false);
    });

    it("decrypts with a KeyRing, selecting key by version", () => {
        const key1 = generateRoomKey();
        const key2 = generateRoomKey();
        const ring = new Map([[1, key1], [2, key2]]);
        const hashKey = linkHashKeyOf(ring)!;
        const link = makeLink();

        const wire1 = encryptLinkForWire(link, { roomKey: key1, version: 1, hashKey });
        const wire2 = encryptLinkForWire(link, { roomKey: key2, version: 2, hashKey });

        assert.deepEqual(decryptLinkFromWire(wire1, ring), link);
        assert.deepEqual(decryptLinkFromWire(wire2, ring), link);
    });

    it("defaults to version 1 when wire link has no key_version", () => {
        const k = keys();
        const ring = new Map([[1, k.roomKey]]);
        const link = makeLink();
        assert.deepEqual(decryptLinkFromWire(encryptLinkForWire(link, k), ring), link);
    });

    it("throws when KeyRing lacks the required version", () => {
        const ring = new Map([[1, generateRoomKey()]]);
        const wire = encryptLinkForWire(makeLink(), keys(undefined, 2));
        assert.throws(() => decryptLinkFromWire(wire, ring), /no key for version 2/);
    });
});

// ---------------------------------------------------------------------------
// Key ring helpers
// ---------------------------------------------------------------------------

describe("encryption: buildKeyRing / latestKeyVersion", () => {
    const entry = (x25519: string, key: Uint8Array, version: number, committed = key): KeysResponseEntry => ({
        encryptedKey: sealRoomKeyTo(x25519, key),
        version,
        commitment: roomKeyCommitment(committed),
    });

    it("builds a KeyRing from the boxes sealed to this agent's X25519 key", () => {
        const me = member();
        const rk1 = generateRoomKey();
        const rk2 = generateRoomKey();
        const { ring, spoiled } = buildKeyRing([entry(me.x25519, rk1, 1), entry(me.x25519, rk2, 2)], me.seed);
        assert.equal(ring.size, 2);
        assert.deepEqual(ring.get(1), rk1);
        assert.deepEqual(ring.get(2), rk2);
        assert.deepEqual(spoiled, []);
    });

    it("spoils a version that does not open, so one bad grant does not lock the room", () => {
        const me = member();
        const rk1 = generateRoomKey();
        const { ring, spoiled } = buildKeyRing(
            [entry(me.x25519, rk1, 1), entry(member().x25519, generateRoomKey(), 2)],
            me.seed,
        );
        assert.deepEqual([...ring.keys()], [1]);
        assert.deepEqual(ring.get(1), rk1);
        assert.deepEqual(spoiled, [2]);
    });

    it("spoils a version whose box opens to a key other than the committed one", () => {
        const me = member();
        // A member holding version 1 sealed a key of its own under it.
        const { ring, spoiled } = buildKeyRing([entry(me.x25519, generateRoomKey(), 1, generateRoomKey())], me.seed);
        assert.equal(ring.size, 0);
        assert.deepEqual(spoiled, [1]);
    });

    it("spoils a version with no commitment", () => {
        const me = member();
        const { ring, spoiled } = buildKeyRing(
            [{ encryptedKey: sealRoomKeyTo(me.x25519, generateRoomKey()), version: 1 }],
            me.seed,
        );
        assert.equal(ring.size, 0);
        assert.deepEqual(spoiled, [1]);
    });

    it("is empty when no version opens: the agent waits for a key", () => {
        const { ring } = buildKeyRing([entry(member().x25519, generateRoomKey(), 1)], member().seed);
        assert.equal(ring.size, 0);
    });

    it("latestKeyVersion returns the highest version", () => {
        const ring = new Map([[1, generateRoomKey()], [3, generateRoomKey()], [2, generateRoomKey()]]);
        assert.equal(latestKeyVersion(ring), 3);
    });

    it("latestKeyVersion returns 0 for an empty ring", () => {
        assert.equal(latestKeyVersion(new Map()), 0);
    });
});

// ---------------------------------------------------------------------------
// Contract: link_hash consistency across key versions
// ---------------------------------------------------------------------------

describe("encryption: link_hash invariant", () => {
    it("encryptLinkForWire produces identical link_hash regardless of key version", () => {
        const ring = new Map([[1, generateRoomKey()], [2, generateRoomKey()]]);
        const hashKey = linkHashKeyOf(ring)!;
        const link = makeLink();

        const wire1 = encryptLinkForWire(link, { roomKey: ring.get(1)!, version: 1, hashKey });
        const wire2 = encryptLinkForWire(link, { roomKey: ring.get(2)!, version: 2, hashKey });

        assert.equal(wire1.link_hash, wire2.link_hash, "same plaintext must produce same link_hash across key versions");
        assert.notEqual(
            (wire1.data as any).ciphertext,
            (wire2.data as any).ciphertext,
            "ciphertext should differ (different keys + nonces)",
        );
    });

    it("encrypted removal matches original addition by link_hash across key rotations", () => {
        const ring = new Map([[1, generateRoomKey()], [2, generateRoomKey()]]);
        const hashKey = linkHashKeyOf(ring)!;
        const link = makeLink();

        const wireAdd = encryptLinkForWire(link, { roomKey: ring.get(1)!, version: 1, hashKey });
        const wireRemove = encryptLinkForWire(link, { roomKey: ring.get(2)!, version: 2, hashKey });

        assert.equal(wireAdd.link_hash, wireRemove.link_hash, "removal link_hash must match addition link_hash");

        const decryptedAdd = decryptLinkFromWire(wireAdd, ring);
        const decryptedRemove = decryptLinkFromWire(wireRemove, ring);
        assert.deepEqual(decryptedAdd, decryptedRemove, "both decrypt to the same plaintext link");
    });
});
