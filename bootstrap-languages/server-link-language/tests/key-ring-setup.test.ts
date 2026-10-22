/**
 * Tests for loadKeyRing (src/key-ring-setup.ts): setupKeyRing's body in
 * index.ts. Boxes are real sealed boxes opened by the executor's reference
 * opening (tests/sealed-box.ts), so the spoiled versions come from the real
 * buildKeyRing; only /keys, the DELETE and the module state are stubs.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { ed25519 } from "@noble/curves/ed25519";

import type { AgentAdapter } from "../src/adapters.js";
import { bytesToHex, generateRoomKey, roomKeyCommitment, sealToDid, type KeyRing } from "../src/encryption.js";
import { adminRotates, type KeyRingStatus } from "../src/key-ring-policy.js";
import { loadKeyRing } from "../src/key-ring-setup.js";
import type { KeysResponse, KeysResponseEntry } from "../src/types.js";
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
    openSealed(sealed: Parameters<AgentAdapter["openSealed"]>[0]): string {
        return bytesToHex(openSealed(sealed, this.secret));
    }
}

/** The same agent while its executor cannot use its key: every box fails to open. */
const locked: Pick<AgentAdapter, "openSealed"> = {
    openSealed: () => {
        throw new Error("main key not found");
    },
};

const entry = (did: string, key: Uint8Array, version: number, committed = key): KeysResponseEntry => ({
    encryptedKey: sealToDid(did, key),
    version,
    commitment: roomKeyCommitment(committed),
});

interface Run {
    deletes: { token: string; version: number }[];
    applied: { status: KeyRingStatus; ring: KeyRing | null }[];
}

async function run(
    keys: KeysResponse | null | Error,
    agent: Pick<AgentAdapter, "openSealed">,
    did: string,
    opts: { failDelete?: number; supersedeDuringFetch?: boolean } = {},
): Promise<Run> {
    const result: Run = { deletes: [], applied: [] };
    let superseded = false;
    await loadKeyRing({
        getToken: async () => "token-1",
        fetchRoomKeys: async (token) => {
            assert.equal(token, "token-1");
            if (opts.supersedeDuringFetch) superseded = true;
            if (keys instanceof Error) throw keys;
            return keys;
        },
        deleteRoomKey: async (token, version) => {
            result.deletes.push({ token, version });
            if (version === opts.failDelete) throw new Error("server down");
        },
        getAgent: () => agent,
        myDid: () => did,
        superseded: () => superseded,
        apply: (status, ring) => result.applied.push({ status, ring }),
    });
    return result;
}

describe("loadKeyRing: spoiled boxes", () => {
    it("drops no box while the executor opens none (a locked agent), and errors", async () => {
        const me = new OpeningAgent();
        const other = new OpeningAgent();
        // Good boxes sealed to this agent, and one sealed to another DID: all fail to open now.
        const keys = [entry(me.did(), generateRoomKey(), 1), entry(me.did(), generateRoomKey(), 2), entry(other.did(), generateRoomKey(), 3)];

        const { deletes, applied } = await run({ keys, e2e_enabled: true }, locked, me.did());

        assert.deepEqual(deletes, [], "no DELETE: the boxes are not spoiled, the executor is locked");
        assert.deepEqual(applied, [{ status: "error", ring: null }]);
    });

    it("drops each spoiled version once while the executor opens its own boxes, and keeps the rest", async () => {
        const me = new OpeningAgent();
        const other = new OpeningAgent();
        const rk1 = generateRoomKey();
        const rk4 = generateRoomKey();
        const keys = [
            entry(me.did(), rk1, 1),
            entry(other.did(), generateRoomKey(), 2), // sealed to another DID
            entry(me.did(), generateRoomKey(), 3, generateRoomKey()), // opens to a key other than the committed one
            entry(me.did(), rk4, 4),
        ];

        const { deletes, applied } = await run({ keys, e2e_enabled: true }, me, me.did());

        assert.deepEqual(deletes, [{ token: "token-1", version: 2 }, { token: "token-1", version: 3 }]);
        assert.equal(applied.length, 1);
        assert.equal(applied[0].status, "ready");
        assert.deepEqual([...applied[0].ring!.keys()].sort((a, b) => a - b), [1, 4]);
        assert.deepEqual(applied[0].ring!.get(1), rk1);
        assert.deepEqual(applied[0].ring!.get(4), rk4);
    });

    it("drops the next spoiled version when one DELETE fails", async () => {
        const me = new OpeningAgent();
        const other = new OpeningAgent();
        const rk1 = generateRoomKey();
        const keys = [entry(me.did(), rk1, 1), entry(other.did(), generateRoomKey(), 2), entry(other.did(), generateRoomKey(), 3)];

        const { deletes, applied } = await run({ keys, e2e_enabled: true }, me, me.did(), { failDelete: 2 });

        assert.deepEqual(deletes.map((d) => d.version), [2, 3]);
        assert.equal(applied.length, 1);
        assert.equal(applied[0].status, "ready");
        assert.deepEqual([...applied[0].ring!.keys()], [1]);
    });

    it("waits for a new key once every version was spoiled and dropped", async () => {
        const me = new OpeningAgent();
        const other = new OpeningAgent();
        const keys = [entry(other.did(), generateRoomKey(), 1), entry(other.did(), generateRoomKey(), 2)];

        const { deletes, applied } = await run({ keys, e2e_enabled: true }, me, me.did());

        assert.deepEqual(deletes.map((d) => d.version), [1, 2]);
        assert.deepEqual(applied, [{ status: "pending", ring: null }]);
    });

    it("drops nothing and sets nothing once teardown started a new lifecycle during /keys", async () => {
        const me = new OpeningAgent();
        const other = new OpeningAgent();
        const keys = [entry(other.did(), generateRoomKey(), 1)];

        const { deletes, applied } = await run({ keys, e2e_enabled: true }, me, me.did(), { supersedeDuringFetch: true });

        assert.deepEqual(deletes, []);
        assert.deepEqual(applied, []);
    });
});

describe("loadKeyRing: rooms without boxes", () => {
    it("is none without E2E, pending without keys yet, error when /keys fails", async () => {
        const me = new OpeningAgent();
        assert.deepEqual((await run(null, me, me.did())).applied, [{ status: "none", ring: null }]);
        assert.deepEqual((await run({ keys: [], e2e_enabled: false }, me, me.did())).applied, [{ status: "none", ring: null }]);
        assert.deepEqual((await run({ keys: [], e2e_enabled: true }, me, me.did())).applied, [{ status: "pending", ring: null }]);
        assert.deepEqual((await run(new Error("401"), me, me.did())).applied, [{ status: "error", ring: null }]);
    });
});

describe("loadKeyRing → adminRotates (index.ts init and commit)", () => {
    it("an admin whose executor opens no box does not rotate", async () => {
        const me = new OpeningAgent();
        const keys = [entry(me.did(), generateRoomKey(), 1)];

        const { applied } = await run({ keys, e2e_enabled: true }, locked, me.did());

        const status = applied.at(-1)!.status;
        assert.equal(status, "error");
        assert.equal(adminRotates(status, false), false, "another version would only pile up unopened keys");
    });

    it("an admin whose every box is spoiled does rotate", async () => {
        const me = new OpeningAgent();
        const other = new OpeningAgent();
        const keys = [entry(other.did(), generateRoomKey(), 1)];

        const { applied } = await run({ keys, e2e_enabled: true }, me, me.did());

        const status = applied.at(-1)!.status;
        assert.equal(status, "pending");
        assert.equal(adminRotates(status, false), true);
    });
});
