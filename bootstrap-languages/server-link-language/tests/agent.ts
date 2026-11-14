/**
 * A test agent with a real Ed25519 key, signing as the executor's agent does:
 * - `signStringHex(s)`: Ed25519 over SHA-256 of the UTF-8 string, as hex (`agentSignStringHex`);
 * - `sign(bytes)`: Ed25519 over the bytes themselves (`agentSign`);
 * - `roomKeySeed()`: HKDF-SHA256(the signing seed, no salt,
 *   "adam-server-link-language:x25519-seed:v1"), 32 bytes (`agentRoomKeySeed`, K8).
 */

import { ed25519 } from "@noble/curves/ed25519";
import { hkdf } from "@noble/hashes/hkdf";
import { sha256 } from "@noble/hashes/sha256";

import type { AgentAdapter } from "../src/adapters.js";
import { base58Encode, bytesToHex } from "../src/encryption.js";

export const ROOM_KEY_SEED_INFO = "adam-server-link-language:x25519-seed:v1";

export function didOfSeed(seed: Uint8Array): string {
    const pub = ed25519.getPublicKey(seed);
    return "did:key:z" + base58Encode(new Uint8Array([0xed, 0x01, ...pub]));
}

export class TestAgent implements AgentAdapter {
    readonly seed: Uint8Array;
    readonly didValue: string;
    /** Every string signed with `signStringHex`. */
    signed: string[] = [];
    /** Every payload signed with `sign`. */
    signedBytes: Uint8Array[] = [];

    constructor(seed: Uint8Array | number) {
        this.seed = typeof seed === "number" ? new Uint8Array(32).fill(seed) : seed;
        this.didValue = didOfSeed(this.seed);
    }

    did(): string {
        return this.didValue;
    }

    signStringHex(payload: string): string {
        this.signed.push(payload);
        return bytesToHex(ed25519.sign(sha256(new TextEncoder().encode(payload)), this.seed));
    }

    sign(payload: Uint8Array): Uint8Array {
        this.signedBytes.push(payload);
        return ed25519.sign(payload, this.seed);
    }

    async roomKeySeed(): Promise<Uint8Array> {
        return hkdf(sha256, this.seed, undefined, ROOM_KEY_SEED_INFO, 32);
    }
}
