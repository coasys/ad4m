/**
 * The keyring binding (AR-08 K9): `keyring/keyring.js`, the `--target deno` build of
 * `ad4m-keyring/wasm` with the wasm inlined (`ad4m-keyring/wasm/build.sh` writes it). It must
 * seal and open ad4m-keyring's vectors byte for byte, as `sealToDid` does, and its X25519 and
 * link-hash keys must equal noble's.
 *
 * The seal draws from `crypto.getRandomValues`; `serveRandom` makes the next draws hand out a
 * vector's ephemeral secret, then its nonce, so the export itself gives the vector's envelope.
 */

import { readFileSync } from "node:fs";
import { afterEach, describe, it } from "node:test";
import assert from "node:assert/strict";
import { x25519 } from "@noble/curves/ed25519";
import { hkdf } from "@noble/hashes/hkdf";
import { sha256 } from "@noble/hashes/sha256";

import { linkHashKey, openRoomKey, randomRoomKey, sealRoomKey, x25519Public } from "../keyring/keyring.js";
import { bytesToHex, hexToBytes } from "../src/encryption.js";

const file = new URL("../../../ad4m-keyring/vectors/room-key-seal.json", import.meta.url);
const { vectors } = JSON.parse(readFileSync(file, "utf8")) as {
    vectors: {
        x25519Public: string;
        x25519Secret: string;
        roomKey: string;
        ephemeralSecret: string;
        nonce: string;
        sealed: { ephemeralPublicKey: string; nonce: string; ciphertext: string };
    }[];
};

const crypto = globalThis.crypto as Crypto & { getRandomValues: Crypto["getRandomValues"] };

/** The next draws from `crypto.getRandomValues` hand out `bytes`, in order. */
function serveRandom(bytes: Uint8Array): void {
    const queue = Array.from(bytes);
    Object.defineProperty(crypto, "getRandomValues", {
        configurable: true,
        writable: true,
        value: <T extends ArrayBufferView | null>(buffer: T): T => {
            const view = new Uint8Array(buffer!.buffer, buffer!.byteOffset, buffer!.byteLength);
            if (queue.length < view.length) throw new Error("the seal drew more than the language does");
            for (let i = 0; i < view.length; i++) view[i] = queue.shift()!;
            return buffer;
        },
    });
}

afterEach(() => {
    delete (crypto as { getRandomValues?: unknown }).getRandomValues;
});

describe("keyring binding: the room-key seal (K9)", () => {
    it("has vectors", () => assert.ok(vectors.length > 0));

    it("seals every vector byte for byte", () => {
        for (const v of vectors) {
            serveRandom(new Uint8Array([...hexToBytes(v.ephemeralSecret), ...hexToBytes(v.nonce)]));
            assert.deepEqual(sealRoomKey(hexToBytes(v.roomKey), hexToBytes(v.x25519Public)), v.sealed);
        }
    });

    it("opens every vector to its room key", () => {
        for (const v of vectors) {
            assert.equal(bytesToHex(openRoomKey(v.sealed, hexToBytes(v.x25519Secret))), v.roomKey);
        }
    });

    it("gives the X25519 public key of a secret as noble does", () => {
        for (const v of vectors) {
            const secret = hexToBytes(v.x25519Secret);
            assert.equal(bytesToHex(x25519Public(secret)), v.x25519Public);
            assert.deepEqual(x25519Public(secret), x25519.getPublicKey(secret));
        }
    });

    it("opens a fresh seal for its recipient only", () => {
        const [mine, theirs] = vectors;
        const roomKey = randomRoomKey();
        assert.equal(roomKey.length, 32);
        const sealed = sealRoomKey(roomKey, hexToBytes(mine.x25519Public));
        assert.deepEqual(openRoomKey(sealed, hexToBytes(mine.x25519Secret)), roomKey);
        assert.throws(() => openRoomKey(sealed, hexToBytes(theirs.x25519Secret)));
    });

    it("refuses keys that are not 32 bytes", () => {
        const v = vectors[0];
        assert.throws(() => sealRoomKey(new Uint8Array(31), hexToBytes(v.x25519Public)), /32 bytes/);
        assert.throws(() => openRoomKey(v.sealed, new Uint8Array(33)), /32 bytes/);
        assert.throws(() => x25519Public(new Uint8Array(16)), /32 bytes/);
    });
});

describe("keyring binding: the link-hash key (K4)", () => {
    it("is HKDF-SHA256 of room key v1 with no salt and the link-hash info", () => {
        for (const v of vectors) {
            const roomKey = hexToBytes(v.roomKey);
            const expected = hkdf(sha256, roomKey, undefined, "adam-server-link-language:link-hash:v1", 32);
            assert.deepEqual(linkHashKey(roomKey), expected);
        }
    });
});
