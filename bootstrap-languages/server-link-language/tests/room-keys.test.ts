/**
 * Tests for src/room-keys.ts: who mints a room key (K2), whom a key is sealed to (K1: only
 * keys whose X25519 statement verifies and that the executor's membership admits), the retry
 * when the server's admitted keys differ (409), and grants to members' X25519 keys. Members
 * are real Ed25519 keys (tests/agent.ts); each box is opened with the member's own seed.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";

import type { RoomConfig, Transport, TransportResponse } from "../src/adapters.js";
import { initAdapters, resetAdapters } from "../src/adapters.js";
import { RotateMismatch } from "../src/api.js";
import { bytesToHex, generateRoomKey, openRoomKey, roomKeyCommitment } from "../src/encryption.js";
import { grantMissingRoomKeys, mintsRoomKey, rotateRoomKey } from "../src/room-keys.js";
import { roomKeySigningBytes } from "../src/room.js";
import type { MemberKey, SealedBox } from "../src/types.js";
import { x25519Public } from "../keyring/keyring.js";
import { TestAgent } from "./agent.js";

const config: RoomConfig = { serverUrl: "https://server.example", roomId: "room-1" };

async function keyOf(agent: TestAgent, signer: TestAgent = agent): Promise<MemberKey> {
    const x25519PublicKey = bytesToHex(x25519Public(await agent.roomKeySeed()));
    return {
        did: agent.did(),
        person: null,
        byBot: false,
        x25519PublicKey,
        x25519Signature: bytesToHex(signer.sign(roomKeySigningBytes(agent.did(), x25519PublicKey))),
    };
}

class Server implements Transport {
    calls: Array<{ url: string; method: string; body: string }> = [];
    members: MemberKey[][] = [];
    /** The room's latest key version, as `/members` names it. */
    latestKeyVersion = 0;
    rotateAnswers: TransportResponse[] = [];
    missing: Array<{ did: string; missingVersions: number[] }> = [];
    grants: Array<{ targetDid: string; keys: Array<{ version: number; encryptedKey: SealedBox }> }> = [];

    async fetch(url: string, method: string, _headers: Record<string, string>, body: string): Promise<TransportResponse> {
        this.calls.push({ url, method, body });
        const ok = (b: unknown): TransportResponse => ({ status: 200, headers: {}, body: JSON.stringify(b) });
        if (url.endsWith("/members")) {
            const keys = this.members.length > 1 ? this.members.shift()! : this.members[0];
            return ok({ members: [], keys, latestKeyVersion: this.latestKeyVersion });
        }
        if (url.endsWith("/keys/rotate")) {
            const { version } = JSON.parse(body);
            return this.rotateAnswers.shift() ?? ok({ version, recipients: [], membersNeedingHistoricalKeys: [] });
        }
        if (url.endsWith("/keys/missing")) return ok({ membersNeedingHistoricalKeys: this.missing });
        if (url.endsWith("/keys/grant")) {
            const grant = JSON.parse(body);
            this.grants.push(grant);
            return ok({ granted: grant.keys.map((k: { version: number }) => k.version) });
        }
        throw new Error(`no route for ${method} ${url}`);
    }

    rotations(): Array<{ keys: Array<{ did: string; encryptedKey: SealedBox }>; commitment: string; version: number }> {
        return this.calls.filter((c) => c.url.endsWith("/keys/rotate")).map((c) => JSON.parse(c.body));
    }
}

function setup(server: Server): void {
    resetAdapters();
    initAdapters({ transport: server, config });
}

describe("room keys: each box is bound to its room and version (K9)", () => {
    it("seals the next version for the room the server names, and the box opens only as that", async () => {
        const alice = new TestAgent(1);
        const server = new Server();
        server.members = [[await keyOf(alice)]];
        server.latestKeyVersion = 3;
        setup(server);
        const outcome = await rotateRoomKey(config, "token", async () => true);
        assert.ok(outcome);
        const [rotation] = server.rotations();
        assert.equal(rotation.version, 4);
        const box = rotation.keys[0].encryptedKey;
        const seed = await alice.roomKeySeed();
        assert.equal(rotation.commitment, roomKeyCommitment(openRoomKey(box, seed, config.roomId, 4)));
        assert.throws(() => openRoomKey(box, seed, config.roomId, 3), "another version");
        assert.throws(() => openRoomKey(box, seed, config.roomId, 5), "another version");
        assert.throws(() => openRoomKey(box, seed, "room-2", 4), "another room");
    });

    it("refuses a rotation the server stored under another version than the one sealed for", async () => {
        const alice = new TestAgent(1);
        const server = new Server();
        server.members = [[await keyOf(alice)]];
        server.latestKeyVersion = 1;
        server.rotateAnswers = [
            { status: 200, headers: {}, body: JSON.stringify({ version: 7, recipients: [alice.did()] }) },
        ];
        setup(server);
        await assert.rejects(() => rotateRoomKey(config, "token", async () => true), /version 7.*version 2/);
    });
});

describe("room keys: who mints one (K2)", () => {
    const me = "did:key:zMe";
    it("any admitted member holding no key mints one when the room has no version", () => {
        assert.equal(mintsRoomKey({ status: "pending", rotatedThisRun: false, admitted: true }, [], me), true);
    });

    it("waits for a grant when the room has versions this key lacks", () => {
        const missing = [{ did: me, missingVersions: [1] }];
        assert.equal(mintsRoomKey({ status: "pending", rotatedThisRun: false, admitted: true }, missing, me), false);
        const others = [{ did: "did:key:zOther", missingVersions: [1] }];
        assert.equal(mintsRoomKey({ status: "pending", rotatedThisRun: false, admitted: true }, others, me), true);
    });

    it("does not mint when not admitted, after minting this run, or while it holds a key", () => {
        assert.equal(mintsRoomKey({ status: "pending", rotatedThisRun: false, admitted: false }, [], me), false);
        assert.equal(mintsRoomKey({ status: "pending", rotatedThisRun: true, admitted: true }, [], me), false);
        assert.equal(mintsRoomKey({ status: "ready", rotatedThisRun: false, admitted: true }, [], me), false);
        assert.equal(mintsRoomKey({ status: "error", rotatedThisRun: false, admitted: true }, [], me), false);
    });
});

describe("room keys: rotation seals to verified, admitted keys only (K1)", () => {
    it("seals the new key to each admitted key whose statement verifies, and each opens it", async () => {
        const [alice, bob, carol, mallory] = [new TestAgent(1), new TestAgent(2), new TestAgent(3), new TestAgent(4)];
        const server = new Server();
        server.members = [[
            await keyOf(alice),
            await keyOf(bob),
            // Carol's X25519 key, signed by Mallory: a server's own key under Carol's name.
            await keyOf(carol, mallory),
            // Mallory's statement verifies, but the space does not admit her.
            await keyOf(mallory),
        ]];
        setup(server);
        const asked: string[] = [];
        const admits = async (did: string) => {
            asked.push(did);
            return did !== mallory.did();
        };

        const outcome = await rotateRoomKey(config, "token", admits);
        assert.ok(outcome);
        const [rotation] = server.rotations();
        assert.deepEqual(rotation.keys.map((k) => k.did), [alice.did(), bob.did()]);
        assert.equal(rotation.version, 1, "the first version of a room with none");
        assert.equal(outcome.result.version, 1);
        const opened = await Promise.all(
            rotation.keys.map(async (k, i) =>
                openRoomKey(k.encryptedKey, await [alice, bob][i].roomKeySeed(), config.roomId, 1),
            ),
        );
        assert.deepEqual(opened[0], opened[1], "one room key for both");
        assert.equal(rotation.commitment, roomKeyCommitment(opened[0]));
        assert.deepEqual(outcome.skipped.sort(), [carol.did(), mallory.did()].sort());
        assert.ok(!asked.includes(carol.did()), "a key whose statement fails is not even asked about");
    });

    it("rotates nothing when no key can be sealed to", async () => {
        const server = new Server();
        const mallory = new TestAgent(4);
        server.members = [[await keyOf(mallory)]];
        setup(server);
        assert.equal(await rotateRoomKey(config, "token", async () => false), null);
        assert.equal(server.rotations().length, 0);
    });

    it("on 409 reads the members again (after the hook) and seals again", async () => {
        const [alice, bob] = [new TestAgent(1), new TestAgent(2)];
        const server = new Server();
        server.members = [[await keyOf(alice)], [await keyOf(alice), await keyOf(bob)]];
        server.rotateAnswers = [
            { status: 409, headers: {}, body: JSON.stringify({ error: "mismatch", missing: [bob.did()], extra: [] }) },
        ];
        setup(server);
        let hooks = 0;
        const outcome = await rotateRoomKey(config, "token", async () => true, { beforeRetry: async () => { hooks++; } });
        assert.ok(outcome);
        assert.equal(hooks, 1);
        const rotations = server.rotations();
        assert.equal(rotations.length, 2);
        assert.deepEqual(rotations[1].keys.map((k) => k.did), [alice.did(), bob.did()]);
    });

    it("gives up with the mismatch after three refusals", async () => {
        const alice = new TestAgent(1);
        const server = new Server();
        server.members = [[await keyOf(alice)]];
        const refusal = { status: 409, headers: {}, body: JSON.stringify({ missing: ["did:key:zX"], extra: [] }) };
        server.rotateAnswers = [refusal, refusal, refusal, refusal];
        setup(server);
        await assert.rejects(
            () => rotateRoomKey(config, "token", async () => true),
            (e: unknown) => e instanceof RotateMismatch && e.missing[0] === "did:key:zX",
        );
        assert.equal(server.rotations().length, 3);
    });
});

describe("room keys: grants (K1)", () => {
    it("seals the versions a member lacks to its verified X25519 key, and skips the rest", async () => {
        const [me, bob, carol, mallory] = [new TestAgent(1), new TestAgent(2), new TestAgent(3), new TestAgent(4)];
        const server = new Server();
        server.members = [[await keyOf(me), await keyOf(bob), await keyOf(carol, mallory), await keyOf(mallory)]];
        server.missing = [
            { did: me.did(), missingVersions: [2] },
            { did: bob.did(), missingVersions: [1, 2] },
            { did: carol.did(), missingVersions: [1] },
            { did: mallory.did(), missingVersions: [1] },
            { did: "did:key:zGone", missingVersions: [1] },
        ];
        setup(server);
        const ring = new Map([[1, generateRoomKey()]]);
        const grants = await grantMissingRoomKeys(config, "token", ring, me.did(), async (did) => did !== mallory.did());
        assert.deepEqual(grants, [{ did: bob.did(), versions: [1] }]);
        assert.equal(server.grants.length, 1);
        const [grant] = server.grants;
        assert.equal(grant.targetDid, bob.did());
        const bobSeed = await bob.roomKeySeed();
        assert.deepEqual(openRoomKey(grant.keys[0].encryptedKey, bobSeed, config.roomId, 1), ring.get(1));
        assert.throws(() => openRoomKey(grant.keys[0].encryptedKey, bobSeed, config.roomId, 2), "bound to version 1");
        assert.throws(() => openRoomKey(grant.keys[0].encryptedKey, bobSeed, "room-2", 1), "bound to its room");
    });

    it("reads nothing more when no member lacks a version", async () => {
        const server = new Server();
        setup(server);
        const grants = await grantMissingRoomKeys(config, "token", new Map([[1, generateRoomKey()]]), "did:key:zMe", async () => true);
        assert.deepEqual(grants, []);
        assert.ok(!server.calls.some((c) => c.url.endsWith("/members")));
    });
});
