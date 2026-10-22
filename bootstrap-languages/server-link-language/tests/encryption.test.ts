/**
 * Tests for src/encryption.ts — room keys sealed to members' DIDs in the
 * executor's sealed-box format, and AES-256-GCM link encryption. No mocking
 * of the crypto primitives: the mock agents open with the noble reference
 * of the executor's opening (tests/sealed-box.ts), so a broken wire format
 * or a swapped key fails the round trip.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { ed25519, x25519 } from "@noble/curves/ed25519";

import type { AgentAdapter } from "../src/adapters.js";
import {
    AES_KEY_BYTES,
    buildKeyRing,
    bytesToHex,
    decryptLinkFromWire,
    encryptLinkForWire,
    generateRoomKey,
    hexToBytes,
    latestKeyVersion,
    openRoomKey,
    randomBytes,
    sealToDid,
} from "../src/encryption.js";
import type { LinkExpression, SealedBox } from "../src/types.js";
import { isEncryptedLinkData } from "../src/types.js";
import { didForEd25519PublicKey, openSealed } from "./sealed-box.js";

/** An agent whose executor opens like the real one. */
class OpeningAgent implements AgentAdapter {
    readonly secret: Uint8Array;
    readonly didValue: string;
    constructor(secret: Uint8Array = ed25519.utils.randomPrivateKey()) {
        this.secret = secret;
        this.didValue = didForEd25519PublicKey(ed25519.getPublicKey(secret));
    }
    did(): string {
        return this.didValue;
    }
    signStringHex(): string {
        throw new Error("room keys need no signature");
    }
    openSealed(sealed: SealedBox): string {
        return bytesToHex(openSealed(sealed, this.secret));
    }
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
// The executor's sealed box, pinned on both sides
// ---------------------------------------------------------------------------

describe("encryption: the sealed box format", () => {
    // `rust-executor/src/agent/sealed_box.rs` pins the same values. Changing
    // either side makes every room key sealed so far unreadable.
    const secret = new Uint8Array(32).fill(1);

    it("maps a did:key's Ed25519 key to X25519 like the executor", () => {
        const did = didForEd25519PublicKey(ed25519.getPublicKey(secret));
        assert.equal(did, "did:key:z6Mkon3Necd6NkkyfoGoHxid2znGc59LU3K7mubaRcFbLfLX");
        const x25519Public = ed25519.utils.toMontgomery(ed25519.getPublicKey(secret));
        const x25519Secret = ed25519.utils.toMontgomerySecret(secret);
        assert.equal(bytesToHex(x25519Public), "1b1b58dd50ea14b60da17b790cd02754d970c9bab864ebb3c0f3016fe51d3f57");
        assert.equal(bytesToHex(x25519Secret), "58e86efb75fa4e2c410f46e16de9f6acae1a1703528651b69bc176c088bef36e");
        assert.deepEqual(x25519.getPublicKey(x25519Secret), x25519Public, "the two maps agree");
    });

    it("seals the box the executor opens", () => {
        const did = didForEd25519PublicKey(ed25519.getPublicKey(secret));
        const sealed = sealToDid(
            did,
            new TextEncoder().encode("ad4m room key v1"),
            new Uint8Array(32).fill(2),
            new Uint8Array(12).fill(3),
        );
        assert.deepEqual(sealed, {
            ephemeralPublicKey: "ce8d3ad1ccb633ec7b70c17814a5c76ecd029685050d344745ba05870e587d59",
            nonce: "030303030303030303030303",
            ciphertext: "11b99acbd7bdf173901c669b2ed8c4794cb801ac9c2ac9642531fd873dfde2cc",
        });
        assert.equal(new TextDecoder().decode(openSealed(sealed, secret)), "ad4m room key v1");
    });
});

// ---------------------------------------------------------------------------
// Room keys sealed to DIDs
// ---------------------------------------------------------------------------

describe("encryption: sealToDid / openRoomKey", () => {
    it("seals a room key to a member by DID alone, and the member opens it", () => {
        const member = new OpeningAgent();
        const roomKey = generateRoomKey();

        const sealed = sealToDid(member.did(), roomKey);
        assert.deepEqual(openRoomKey(member, sealed), roomKey);
    });

    it("produces a different box each time (fresh ephemeral key + nonce)", () => {
        const member = new OpeningAgent();
        const roomKey = generateRoomKey();
        const box1 = sealToDid(member.did(), roomKey);
        const box2 = sealToDid(member.did(), roomKey);
        assert.notEqual(box1.ephemeralPublicKey, box2.ephemeralPublicKey);
        assert.notEqual(box1.ciphertext, box2.ciphertext);
        assert.deepEqual(openRoomKey(member, box1), roomKey);
        assert.deepEqual(openRoomKey(member, box2), roomKey);
    });

    it("another agent cannot open it", () => {
        const member = new OpeningAgent();
        const impostor = new OpeningAgent();
        const sealed = sealToDid(member.did(), generateRoomKey());
        assert.throws(() => openRoomKey(impostor, sealed));
    });

    it("refuses a sealed value that is not a room key", () => {
        const member = new OpeningAgent();
        const sealed = sealToDid(member.did(), new Uint8Array(16));
        assert.throws(() => openRoomKey(member, sealed), /32 bytes/);
    });

    it("generateRoomKey returns AES_KEY_BYTES (32) random bytes", () => {
        const key = generateRoomKey();
        assert.equal(key.length, AES_KEY_BYTES);
        assert.notDeepEqual(key, generateRoomKey());
    });
});

// ---------------------------------------------------------------------------
// Link expression wire encryption
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

describe("encryption: encryptLinkForWire / decryptLinkFromWire", () => {
    it("round-trips a link through the room key (full encryption)", () => {
        const roomKey = generateRoomKey();
        const link = makeLink();

        const wire = encryptLinkForWire(link, roomKey);
        assert.ok(isEncryptedLinkData(wire.data));
        assert.equal(typeof (wire.data as any).ciphertext, "string");
        assert.equal(typeof (wire.data as any).nonce, "string");
        assert.equal(wire.author, undefined);
        assert.equal(wire.timestamp, undefined);
        assert.equal(wire.proof, undefined);
        assert.equal(typeof wire.link_hash, "string");

        const decrypted = decryptLinkFromWire(wire, roomKey);
        assert.deepEqual(decrypted, link);
    });

    it("encrypts everything — no metadata in the clear", () => {
        const roomKey = generateRoomKey();
        const link = makeLink({ source: "s3cr3t://topic" });
        const wire = encryptLinkForWire(link, roomKey);

        assert.equal(wire.author, undefined);
        assert.equal(wire.timestamp, undefined);
        assert.equal(wire.proof, undefined);
        assert.equal((wire.data as any).ciphertext.includes(Buffer.from("s3cr3t").toString("hex")), false);
    });

    it("uses a fresh nonce per call (different ciphertext for identical input)", () => {
        const roomKey = generateRoomKey();
        const link = makeLink();
        const wire1 = encryptLinkForWire(link, roomKey);
        const wire2 = encryptLinkForWire(link, roomKey);
        assert.notEqual((wire1.data as any).ciphertext, (wire2.data as any).ciphertext);
    });

    it("fails to decrypt with the wrong room key", () => {
        const link = makeLink();
        const wire = encryptLinkForWire(link, generateRoomKey());
        assert.throws(() => decryptLinkFromWire(wire, generateRoomKey()));
    });

    it("throws when asked to decrypt a wire link with no encrypted data in data field", () => {
        const roomKey = generateRoomKey();
        assert.throws(() => decryptLinkFromWire({ author: "a", timestamp: "t", proof: { signature: "s", key: "k" } } as any, roomKey));
    });

    it("attaches key_version when provided", () => {
        const roomKey = generateRoomKey();
        const link = makeLink();
        const wire = encryptLinkForWire(link, roomKey, 3);
        assert.equal(wire.key_version, 3);
    });

    it("omits key_version when not provided", () => {
        const roomKey = generateRoomKey();
        const link = makeLink();
        const wire = encryptLinkForWire(link, roomKey);
        assert.equal("key_version" in wire, false);
    });

    it("decrypts with a KeyRing, selecting key by version", () => {
        const key1 = generateRoomKey();
        const key2 = generateRoomKey();
        const ring = new Map([[1, key1], [2, key2]]);
        const link = makeLink();

        const wire1 = encryptLinkForWire(link, key1, 1);
        const wire2 = encryptLinkForWire(link, key2, 2);

        assert.deepEqual(decryptLinkFromWire(wire1, ring), link);
        assert.deepEqual(decryptLinkFromWire(wire2, ring), link);
    });

    it("defaults to version 1 when wire link has no key_version", () => {
        const key1 = generateRoomKey();
        const ring = new Map([[1, key1]]);
        const link = makeLink();
        const wire = encryptLinkForWire(link, key1);
        assert.deepEqual(decryptLinkFromWire(wire, ring), link);
    });

    it("throws when KeyRing lacks the required version", () => {
        const key1 = generateRoomKey();
        const key2 = generateRoomKey();
        const ring = new Map([[1, key1]]);
        const link = makeLink();
        const wire = encryptLinkForWire(link, key2, 2);
        assert.throws(() => decryptLinkFromWire(wire, ring), /no key for version 2/);
    });
});

// ---------------------------------------------------------------------------
// Key ring helpers
// ---------------------------------------------------------------------------

describe("encryption: buildKeyRing / latestKeyVersion", () => {
    it("builds a KeyRing from the boxes sealed to this agent", () => {
        const agent = new OpeningAgent();
        const rk1 = generateRoomKey();
        const rk2 = generateRoomKey();
        const entries = [
            { encryptedKey: sealToDid(agent.did(), rk1), version: 1 },
            { encryptedKey: sealToDid(agent.did(), rk2), version: 2 },
        ];
        const ring = buildKeyRing(entries, agent);
        assert.equal(ring.size, 2);
        assert.deepEqual(ring.get(1), rk1);
        assert.deepEqual(ring.get(2), rk2);
    });

    it("leaves out a version that does not open, so one bad grant does not lock the room", () => {
        const agent = new OpeningAgent();
        const other = new OpeningAgent();
        const rk1 = generateRoomKey();
        const ring = buildKeyRing(
            [
                { encryptedKey: sealToDid(agent.did(), rk1), version: 1 },
                { encryptedKey: sealToDid(other.did(), generateRoomKey()), version: 2 },
            ],
            agent,
        );
        assert.deepEqual([...ring.keys()], [1]);
        assert.deepEqual(ring.get(1), rk1);
    });

    it("is empty when no version opens: the agent waits for a key, and an admin rotates", () => {
        const agent = new OpeningAgent();
        const other = new OpeningAgent();
        const ring = buildKeyRing(
            [{ encryptedKey: sealToDid(other.did(), generateRoomKey()), version: 1 }],
            agent,
        );
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
        const key1 = generateRoomKey();
        const key2 = generateRoomKey();
        const link = makeLink();

        const wire1 = encryptLinkForWire(link, key1, 1);
        const wire2 = encryptLinkForWire(link, key2, 2);

        assert.equal(wire1.link_hash, wire2.link_hash, "same plaintext must produce same link_hash across key versions");
        assert.notEqual(
            (wire1.data as any).ciphertext,
            (wire2.data as any).ciphertext,
            "ciphertext should differ (different keys + nonces)",
        );
    });

    it("encrypted removal matches original addition by link_hash across key rotations", () => {
        const key1 = generateRoomKey();
        const key2 = generateRoomKey();
        const ring = new Map([[1, key1], [2, key2]]);
        const link = makeLink();

        const wireAdd = encryptLinkForWire(link, key1, 1);
        const wireRemove = encryptLinkForWire(link, key2, 2);

        assert.equal(wireAdd.link_hash, wireRemove.link_hash, "removal link_hash must match addition link_hash");

        const decryptedAdd = decryptLinkFromWire(wireAdd, ring);
        const decryptedRemove = decryptLinkFromWire(wireRemove, ring);
        assert.deepEqual(decryptedAdd, decryptedRemove, "both decrypt to the same plaintext link");
    });
});

