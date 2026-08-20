/**
 * Tests for src/sync.ts — most importantly applyInboundWireDiff, the
 * single choke point that MUST call emitDiff for every inbound diff
 * (see the "CRITICAL TRAP" note in src/sync.ts and the task brief: 7 of
 * 13 existing link languages got this wrong).
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";

import type { RoomConfig, StorageAdapter, Transport, TransportResponse } from "../src/adapters.js";
import { initConfig, initStorage, initTransport, resetAdapters } from "../src/adapters.js";
import * as store from "../src/store.js";
import * as syncModule from "../src/sync.js";
import { encryptLinkForWire, generateRoomKey } from "../src/encryption.js";
import type { LinkExpression, PerspectiveDiff } from "../src/types.js";

// ---------------------------------------------------------------------------
// Mock adapters
// ---------------------------------------------------------------------------

class MockStorage implements StorageAdapter {
    private data = new Map<string, string>();
    get(key: string): string | null { return this.data.get(key) ?? null; }
    put(key: string, value: string): void { this.data.set(key, value); }
    delete(key: string): void { this.data.delete(key); }
    listKeys(prefix?: string): string[] {
        return [...this.data.keys()].filter((k) => !prefix || k.startsWith(prefix));
    }
}

function simpleHash(data: string): string {
    let h = 0;
    for (let i = 0; i < data.length; i++) h = ((h << 5) - h + data.charCodeAt(i)) | 0;
    return `Qm${Math.abs(h).toString(16)}`;
}

class MockTransport implements Transport {
    calls: Array<{ url: string; method: string; body: string }> = [];
    private routes: Array<{
        test: (url: string, method: string) => boolean;
        handle: (url: string, method: string, body: string) => TransportResponse;
    }> = [];

    route(
        test: (url: string, method: string) => boolean,
        handle: (url: string, method: string, body: string) => TransportResponse,
    ): void {
        this.routes.push({ test, handle });
    }

    async fetch(url: string, method: string, _headers: Record<string, string>, body: string): Promise<TransportResponse> {
        this.calls.push({ url, method, body });
        const route = this.routes.find((r) => r.test(url, method));
        if (!route) throw new Error(`MockTransport: no route for ${method} ${url}`);
        return route.handle(url, method, body);
    }
}

// ---------------------------------------------------------------------------
// Fixtures / harness
// ---------------------------------------------------------------------------

const config: RoomConfig = { serverUrl: "https://server.example", roomId: "room-1" };

function makeLink(overrides?: Partial<LinkExpression["data"]>): LinkExpression {
    return {
        author: "did:key:zAuthor",
        timestamp: "2026-01-01T00:00:00.000Z",
        data: { source: "a", target: "b", predicate: "p", ...overrides },
        proof: { signature: "sig", key: "key" },
    };
}

let emittedDiffs: PerspectiveDiff[];
let syncStates: string[];
let roomKey: Uint8Array | null;

function setup(transport: MockTransport): void {
    resetAdapters();
    initStorage(new MockStorage());
    initTransport(transport);
    initConfig(config);
    store.initStore(simpleHash);

    emittedDiffs = [];
    syncStates = [];
    roomKey = null;

    syncModule.initSync({
        config,
        getToken: async () => "test-token",
        emitDiff: (diff) => emittedDiffs.push(diff),
        emitSyncState: (state) => syncStates.push(state),
        getRoomKey: () => roomKey,
    });
}

// ---------------------------------------------------------------------------
// applyInboundWireDiff — the critical emitPerspectiveDiff trap
// ---------------------------------------------------------------------------

describe("sync: applyInboundWireDiff (the emitPerspectiveDiff trap)", () => {
    it("applies the diff to the local store AND calls emitDiff", () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink();

        const result = syncModule.applyInboundWireDiff({ additions: [link], removals: [] }, 5, "rev-5");

        assert.deepEqual(store.allLinks().links, [link]);
        assert.equal(store.getSequence(), 5);
        assert.equal(store.getRevision(), "rev-5");
        assert.equal(emittedDiffs.length, 1);
        assert.deepEqual(emittedDiffs[0], { additions: [link], removals: [] });
        assert.deepEqual(result, { additions: [link], removals: [] });
    });

    it("applies removals and still emits", () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink();
        store.putLink(link);

        syncModule.applyInboundWireDiff({ additions: [], removals: [link] }, 1, "rev-1");

        assert.equal(store.allLinks().links.length, 0);
        assert.equal(emittedDiffs.length, 1);
        assert.deepEqual(emittedDiffs[0].removals, [link]);
    });

    it("decrypts wire links before applying/emitting when a room key is set", () => {
        const transport = new MockTransport();
        setup(transport);
        roomKey = generateRoomKey();
        const link = makeLink({ source: "secret-source" });
        const wireLink = encryptLinkForWire(link, roomKey);

        syncModule.applyInboundWireDiff({ additions: [wireLink], removals: [] }, 1, "rev-1");

        assert.equal(emittedDiffs.length, 1);
        assert.deepEqual(emittedDiffs[0].additions[0], link);
        assert.deepEqual(store.allLinks().links[0], link);
    });
});

// ---------------------------------------------------------------------------
// catchUp / performSync
// ---------------------------------------------------------------------------

describe("sync: catchUp / performSync", () => {
    it("fetches since=<lastSequence>, applies + emits every diff, and advances the cursor", async () => {
        const transport = new MockTransport();
        setup(transport);
        const linkA = makeLink({ source: "a" });
        const linkB = makeLink({ source: "b" });

        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=0"),
            () => ({
                status: 200,
                headers: {},
                body: JSON.stringify({
                    diffs: [
                        { additions: [linkA], removals: [] },
                        { additions: [linkB], removals: [] },
                    ],
                    revision: "rev-2",
                    sequence: 2,
                }),
            }),
        );

        const last = await syncModule.catchUp();

        assert.equal(emittedDiffs.length, 2);
        assert.equal(store.allLinks().links.length, 2);
        assert.equal(store.getSequence(), 2);
        assert.equal(store.getRevision(), "rev-2");
        assert.deepEqual(last.additions[0].data, linkB.data);
        assert.ok(syncStates.includes("Synced"));
    });

    it("is idempotent: replaying against an unchanged server does not re-emit", async () => {
        const transport = new MockTransport();
        setup(transport);
        const linkA = makeLink({ source: "a" });
        const sinceSeen: string[] = [];

        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            (url) => {
                const since = new URL(url).searchParams.get("since")!;
                sinceSeen.push(since);
                if (since === "0") {
                    return {
                        status: 200,
                        headers: {},
                        body: JSON.stringify({ diffs: [{ additions: [linkA], removals: [] }], revision: "rev-1", sequence: 1 }),
                    };
                }
                return { status: 200, headers: {}, body: JSON.stringify({ diffs: [], revision: "rev-1", sequence: 1 }) };
            },
        );

        await syncModule.catchUp();
        await syncModule.catchUp();

        assert.deepEqual(sinceSeen, ["0", "1"]);
        assert.equal(emittedDiffs.length, 1);
    });

    it("normalizes nested-shape diff entries ({diff: {additions, removals}})", async () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink();

        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => ({
                status: 200,
                headers: {},
                body: JSON.stringify({
                    diffs: [{ diff: { additions: [link], removals: [] }, revision: "rev-1", sequence: 1 }],
                    revision: "rev-1",
                    sequence: 1,
                }),
            }),
        );

        await syncModule.catchUp();
        assert.equal(emittedDiffs.length, 1);
        assert.deepEqual(emittedDiffs[0].additions[0].data, link.data);
        assert.equal(store.getSequence(), 1);
    });

    it("performSync never throws — logs and reports LinkLanguageInstalledButNotSynced on failure", async () => {
        const transport = new MockTransport();
        setup(transport);
        transport.route(() => true, () => ({ status: 500, headers: {}, body: "server error" }));

        const result = await syncModule.performSync();
        assert.deepEqual(result, { additions: [], removals: [] });
        // Must be a valid `PerspectiveState` variant on the executor side —
        // see the comment in src/sync.ts::performSync.
        assert.ok(syncStates.includes("LinkLanguageInstalledButNotSynced"));
    });
});

// ---------------------------------------------------------------------------
// commit
// ---------------------------------------------------------------------------

describe("sync: commit", () => {
    it("posts a plaintext wire diff when there is no room key", async () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink();
        let posted: any = null;

        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            (_url, _method, body) => {
                posted = JSON.parse(body);
                return { status: 200, headers: {}, body: "{}" };
            },
        );

        await syncModule.commit({ additions: [link], removals: [] });

        assert.ok(posted);
        assert.deepEqual(posted.additions[0].data, link.data);
        assert.equal(posted.additions[0].encrypted, undefined);
    });

    it("posts an encrypted wire diff when a room key is set", async () => {
        const transport = new MockTransport();
        setup(transport);
        roomKey = generateRoomKey();
        const link = makeLink();
        let posted: any = null;

        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            (_url, _method, body) => {
                posted = JSON.parse(body);
                return { status: 200, headers: {}, body: "{}" };
            },
        );

        await syncModule.commit({ additions: [link], removals: [] });

        assert.ok(posted.additions[0].encrypted);
        assert.equal(posted.additions[0].data, undefined);
    });

    it("propagates a network failure to the caller", async () => {
        const transport = new MockTransport();
        setup(transport);
        transport.route(() => true, () => ({ status: 500, headers: {}, body: "boom" }));

        await assert.rejects(() => syncModule.commit({ additions: [makeLink()], removals: [] }));
    });
});

// ---------------------------------------------------------------------------
// bootstrap
// ---------------------------------------------------------------------------

describe("sync: bootstrap", () => {
    it("pulls a full render snapshot into the local store and sets the cursor, without emitting", async () => {
        const transport = new MockTransport();
        setup(transport);
        const linkA = makeLink({ source: "a" });
        const linkB = makeLink({ source: "b" });

        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => ({ status: 200, headers: {}, body: JSON.stringify({ links: [linkA, linkB], revision: "rev-snap" }) }),
        );
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/revision"),
            () => ({ status: 200, headers: {}, body: JSON.stringify({ revision: "rev-snap", sequence: 7 }) }),
        );

        await syncModule.bootstrap();

        assert.equal(store.allLinks().links.length, 2);
        assert.equal(store.getRevision(), "rev-snap");
        assert.equal(store.getSequence(), 7);
        assert.equal(emittedDiffs.length, 0);
    });
});

// ---------------------------------------------------------------------------
// render / currentRevision
// ---------------------------------------------------------------------------

describe("sync: render / currentRevision", () => {
    it("render() returns the local store contents", () => {
        const transport = new MockTransport();
        setup(transport);
        store.putLink(makeLink());
        assert.equal(syncModule.render().links.length, 1);
    });

    it("currentRevision() returns the stored revision, or empty string when unset", () => {
        const transport = new MockTransport();
        setup(transport);
        assert.equal(syncModule.currentRevision(), "");
        store.setRevision("rev-x");
        assert.equal(syncModule.currentRevision(), "rev-x");
    });
});
