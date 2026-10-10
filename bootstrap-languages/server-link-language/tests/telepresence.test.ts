/**
 * Tests for src/telepresence.ts — the peer/roster caching module that
 * mediates between ws-client's presence pushes and the language's
 * telepresence capability calls.
 *
 * The critical shape guarantee here is `getOnlineAgents()`:
 *   1. self is never returned (server rebroadcasts the full roster including us);
 *   2. peers seen via peer-joined but who have never called setOnlineStatus
 *      are returned with an EMPTY PerspectiveExpression as status — the
 *      executor's OnlineAgent schema treats status as non-optional (rejects
 *      bare `{did}` with `RpcError: missing field \`status\``), and the
 *      product decision is "presence should be visible even before status".
 */

import { describe, it, beforeEach } from "node:test";
import assert from "node:assert/strict";

import * as telepresence from "../src/telepresence.js";
import type { ClientWsMessage } from "../src/types.js";

const MY_DID = "did:key:zMe";
const ALICE = "did:key:zAlice";
const BOB = "did:key:zBob";

let sent: ClientWsMessage[];

beforeEach(() => {
    sent = [];
    telepresence.initTelepresence({
        send: (msg) => sent.push(msg),
        getMyDid: () => MY_DID,
    });
});

describe("telepresence: getOnlineAgents", () => {
    it("returns an empty PerspectiveExpression for peers who joined but never called setOnlineStatus", async () => {
        telepresence.handlePeerJoined({ type: "peer-joined", did: ALICE });

        const agents = await telepresence.getOnlineAgents();
        assert.equal(agents.length, 1);
        assert.equal(agents[0].did, ALICE);
        // Empty status must satisfy the executor's PerspectiveExpression
        // shape — see rust-executor/src/types/domain.rs::PerspectiveExpression
        // (derives Default: author="", data.links=[], proof empty, timestamp="").
        assert.deepEqual(agents[0].status, {
            author: "",
            data: { links: [] },
            proof: { key: "", signature: "" },
            timestamp: "",
        });
    });

    it("returns the real status once a peer's online-status message arrives", async () => {
        const status = {
            author: ALICE,
            data: { links: [] },
            proof: { key: "k", signature: "s" },
            timestamp: "2026-01-01T00:00:00.000Z",
        };
        telepresence.handleOnlineAgentsMessage({
            type: "online-agents",
            agents: [{ did: ALICE, status }],
        });

        const agents = await telepresence.getOnlineAgents();
        assert.equal(agents.length, 1);
        assert.deepEqual(agents[0].status, status);
    });

    it("drops self from the returned roster (server rebroadcasts include us)", async () => {
        const status = {
            author: MY_DID,
            data: { links: [] },
            proof: { key: "k", signature: "s" },
            timestamp: "2026-01-01T00:00:00.000Z",
        };
        telepresence.handleOnlineAgentsMessage({
            type: "online-agents",
            agents: [
                { did: MY_DID, status },
                { did: ALICE, status: { ...status, author: ALICE } },
            ],
        });

        const agents = await telepresence.getOnlineAgents();
        assert.deepEqual(agents.map((a) => a.did), [ALICE]);
    });

    it("mixes: keeps real status for one peer, synthesises empty for another", async () => {
        // Alice fully online with status; Bob has just joined and hasn't announced.
        const aliceStatus = {
            author: ALICE,
            data: { links: [] },
            proof: { key: "k", signature: "s" },
            timestamp: "2026-01-01T00:00:00.000Z",
        };
        telepresence.handleOnlineAgentsMessage({
            type: "online-agents",
            agents: [{ did: ALICE, status: aliceStatus }],
        });
        telepresence.handlePeerJoined({ type: "peer-joined", did: BOB });

        const agents = await telepresence.getOnlineAgents();
        const byDid = new Map(agents.map((a) => [a.did, a.status]));
        assert.equal(agents.length, 2);
        assert.deepEqual(byDid.get(ALICE), aliceStatus);
        assert.deepEqual(byDid.get(BOB), {
            author: "",
            data: { links: [] },
            proof: { key: "", signature: "" },
            timestamp: "",
        });
    });

    it("drops peers after peer-left", async () => {
        telepresence.handlePeerJoined({ type: "peer-joined", did: ALICE });
        telepresence.handlePeerLeft({ type: "peer-left", did: ALICE });
        const agents = await telepresence.getOnlineAgents();
        assert.equal(agents.length, 0);
    });

    it("clearOnlineAgents wipes the roster (called on ws disconnect)", async () => {
        telepresence.handlePeerJoined({ type: "peer-joined", did: ALICE });
        telepresence.clearOnlineAgents();
        const agents = await telepresence.getOnlineAgents();
        assert.equal(agents.length, 0);
    });
});

describe("telepresence: outbound signals", () => {
    it("setOnlineStatus forwards a set-online-status frame", async () => {
        await telepresence.setOnlineStatus({ hello: "world" });
        assert.deepEqual(sent, [{ type: "set-online-status", status: { hello: "world" } }]);
    });

    it("sendSignal forwards a telepresence-signal frame", async () => {
        await telepresence.sendSignal(ALICE, { ping: 1 });
        assert.deepEqual(sent, [{ type: "telepresence-signal", toDid: ALICE, payload: { ping: 1 } }]);
    });

    it("sendBroadcast forwards a telepresence-broadcast frame", async () => {
        await telepresence.sendBroadcast({ event: "hi" });
        assert.deepEqual(sent, [{ type: "telepresence-broadcast", payload: { event: "hi" } }]);
    });
});
