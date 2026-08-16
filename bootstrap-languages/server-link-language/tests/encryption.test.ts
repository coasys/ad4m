/**
 * Tests for src/encryption.ts — X25519 key derivation, sealed-box room-key
 * exchange, and AES-256-GCM link encryption. No mocking of the crypto
 * primitives themselves: these run the real @noble/* implementations
 * end-to-end so a broken wire format or a swapped key would actually fail
 * the round trip, not just satisfy a mock.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { createHash } from "node:crypto";

import {
    AES_KEY_BYTES,
    bytesToHex,
    decodeSealedEnvelope,
    decryptLinkFromWire,
    deriveX25519KeyPair,
    encodeSealedEnvelope,
    encryptLinkForWire,
    generateRoomKey,
    hexToBytes,
    openRoomKeyEnvelope,
    randomBytes,
    sealRoomKeyForRecipient,
} from "../src/encryption.js";
import type { LinkExpression } from "../src/types.js";

/** Deterministic stand-in for AgentAdapter.signStringHex — mimics the one
 * property real EdDSA signing guarantees that this module depends on:
 * the same input always produces the same output for a given "agent". */
function mockSigner(seedTag: string) {
    return (payload: string) => createHash("sha256").update(`${seedTag}:${payload}`).digest("hex");
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
// X25519 key derivation
// ---------------------------------------------------------------------------

describe("encryption: deriveX25519KeyPair", () => {
    it("is deterministic for the same signer", () => {
        const signer = mockSigner("did:key:zAgentA");
        const kp1 = deriveX25519KeyPair(signer);
        const kp2 = deriveX25519KeyPair(signer);
        assert.deepEqual(kp1.privateKey, kp2.privateKey);
        assert.deepEqual(kp1.publicKey, kp2.publicKey);
    });

    it("produces different keys for different agents", () => {
        const kpA = deriveX25519KeyPair(mockSigner("did:key:zAgentA"));
        const kpB = deriveX25519KeyPair(mockSigner("did:key:zAgentB"));
        assert.notDeepEqual(kpA.privateKey, kpB.privateKey);
        assert.notDeepEqual(kpA.publicKey, kpB.publicKey);
    });

    it("produces 32-byte keys", () => {
        const kp = deriveX25519KeyPair(mockSigner("did:key:zAgentA"));
        assert.equal(kp.privateKey.length, 32);
        assert.equal(kp.publicKey.length, 32);
    });
});

// ---------------------------------------------------------------------------
// Sealed room-key envelope (X25519 ECDH + HKDF + AES-256-GCM)
// ---------------------------------------------------------------------------

describe("encryption: sealRoomKeyForRecipient / openRoomKeyEnvelope", () => {
    it("round-trips a room key to its intended recipient", () => {
        const recipient = deriveX25519KeyPair(mockSigner("did:key:zRecipient"));
        const roomKey = generateRoomKey();

        const envelope = sealRoomKeyForRecipient(roomKey, recipient.publicKey);
        const opened = openRoomKeyEnvelope(envelope, recipient.privateKey);

        assert.deepEqual(opened, roomKey);
    });

    it("produces a different envelope each time (fresh ephemeral key + nonce)", () => {
        const recipient = deriveX25519KeyPair(mockSigner("did:key:zRecipient"));
        const roomKey = generateRoomKey();

        const envelope1 = sealRoomKeyForRecipient(roomKey, recipient.publicKey);
        const envelope2 = sealRoomKeyForRecipient(roomKey, recipient.publicKey);

        assert.notEqual(envelope1.ephemeralPublicKey, envelope2.ephemeralPublicKey);
        assert.notEqual(envelope1.ciphertext, envelope2.ciphertext);

        // Both still open correctly despite differing ciphertext.
        assert.deepEqual(openRoomKeyEnvelope(envelope1, recipient.privateKey), roomKey);
        assert.deepEqual(openRoomKeyEnvelope(envelope2, recipient.privateKey), roomKey);
    });

    it("fails to open with the wrong recipient private key", () => {
        const recipient = deriveX25519KeyPair(mockSigner("did:key:zRecipient"));
        const impostor = deriveX25519KeyPair(mockSigner("did:key:zImpostor"));
        const roomKey = generateRoomKey();

        const envelope = sealRoomKeyForRecipient(roomKey, recipient.publicKey);
        assert.throws(() => openRoomKeyEnvelope(envelope, impostor.privateKey));
    });

    it("generateRoomKey returns AES_KEY_BYTES (32) random bytes", () => {
        const key = generateRoomKey();
        assert.equal(key.length, AES_KEY_BYTES);
        assert.notDeepEqual(key, generateRoomKey());
    });

    it("round-trips through the encodeSealedEnvelope wire framing", () => {
        const recipient = deriveX25519KeyPair(mockSigner("did:key:zRecipient"));
        const roomKey = generateRoomKey();

        const envelope = sealRoomKeyForRecipient(roomKey, recipient.publicKey);
        const encoded = encodeSealedEnvelope(envelope);
        assert.equal(typeof encoded, "string");

        const decoded = decodeSealedEnvelope(encoded);
        assert.deepEqual(decoded, envelope);

        const opened = openRoomKeyEnvelope(decoded, recipient.privateKey);
        assert.deepEqual(opened, roomKey);
    });

    it("decodeSealedEnvelope rejects malformed input", () => {
        assert.throws(() => decodeSealedEnvelope(Buffer.from(JSON.stringify({ nonce: "ab" })).toString("base64")));
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
    it("round-trips a link's data through the room key", () => {
        const roomKey = generateRoomKey();
        const link = makeLink();

        const wire = encryptLinkForWire(link, roomKey);
        assert.ok(wire.encrypted);
        assert.equal(wire.data, undefined);
        assert.equal(wire.author, link.author);
        assert.equal(wire.timestamp, link.timestamp);
        assert.deepEqual(wire.proof, link.proof);

        const decrypted = decryptLinkFromWire(wire, roomKey);
        assert.deepEqual(decrypted, link);
    });

    it("leaves author/timestamp/proof in the clear (metadata, not payload)", () => {
        const roomKey = generateRoomKey();
        const link = makeLink({ source: "s3cr3t://topic" });
        const wire = encryptLinkForWire(link, roomKey);

        // The plaintext source string must not appear anywhere in the ciphertext blob.
        assert.equal(wire.encrypted!.includes(Buffer.from("s3cr3t").toString("hex")), false);
    });

    it("uses a fresh nonce per call (different ciphertext for identical input)", () => {
        const roomKey = generateRoomKey();
        const link = makeLink();
        const wire1 = encryptLinkForWire(link, roomKey);
        const wire2 = encryptLinkForWire(link, roomKey);
        assert.notEqual(wire1.encrypted, wire2.encrypted);
    });

    it("fails to decrypt with the wrong room key", () => {
        const link = makeLink();
        const wire = encryptLinkForWire(link, generateRoomKey());
        assert.throws(() => decryptLinkFromWire(wire, generateRoomKey()));
    });

    it("throws when asked to decrypt a wire link with no `encrypted` field", () => {
        const roomKey = generateRoomKey();
        assert.throws(() => decryptLinkFromWire({ author: "a", timestamp: "t", proof: { signature: "s", key: "k" } }, roomKey));
    });
});
