/**
 * Tests for src/sync.ts — most importantly the single inbound path, which
 * MUST call emitDiff for every link that enters the store, once (see the
 * "CRITICAL RULE" note in src/sync.ts: 7 of 13 existing link languages got
 * this wrong), and must not lose a link it cannot decrypt yet.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";

import type { RoomConfig, StorageAdapter, Transport, TransportResponse } from "../src/adapters.js";
import { initAdapters, resetAdapters } from "../src/adapters.js";
import * as store from "../src/store.js";
import * as syncModule from "../src/sync.js";
import { encryptLinkForWire, generateRoomKey, type KeyRing } from "../src/encryption.js";
import type { LinkExpression, PerspectiveDiff } from "../src/types.js";
import { isEncryptedLinkData } from "../src/types.js";

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
let keyRing: KeyRing | null;

function setup(transport: MockTransport): void {
    resetAdapters();
    // Wipe any leftover batch state so a previous test's queued diff can't
    // flush against this test's transport.
    syncModule._resetBatchStateForTests();
    initAdapters({ storage: new MockStorage(), transport, config });
    store.initStore(simpleHash);

    emittedDiffs = [];
    syncStates = [];
    keyRing = null;

    syncModule.initSync({
        config,
        getToken: async () => "test-token",
        emitDiff: (diff) => emittedDiffs.push(diff),
        emitSyncState: (state) => syncStates.push(state),
        getKeyRing: () => keyRing,
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

    it("decrypts wire links before applying/emitting when a key ring is set", () => {
        const transport = new MockTransport();
        setup(transport);
        const rk = generateRoomKey();
        keyRing = new Map([[1, rk]]);
        const link = makeLink({ source: "secret-source" });
        const wireLink = encryptLinkForWire(link, rk, 1);

        syncModule.applyInboundWireDiff({ additions: [wireLink], removals: [] }, 1, "rev-1");

        assert.equal(emittedDiffs.length, 1);
        assert.deepEqual(emittedDiffs[0].additions[0], link);
        assert.deepEqual(store.allLinks().links[0], link);
    });

    it("holds an encrypted link without a key ring, advances the cursor, and emits nothing", () => {
        const transport = new MockTransport();
        setup(transport);
        // keyRing stays null — simulates a freshly joined member awaiting key grant
        const rk = generateRoomKey();
        const link = makeLink({ source: "encrypted-no-ring" });
        const wireLink = encryptLinkForWire(link, rk, 1);

        syncModule.applyInboundWireDiff({ additions: [wireLink], removals: [] }, 5, "rev-5");

        assert.equal(store.allLinks().links.length, 0, "undecryptable link must not appear in store");
        assert.equal(store.getSequence(), 5, "sequence must advance — the link is held, not lost");
        assert.equal(emittedDiffs.length, 0, "nothing changed in the store, nothing is emitted");
        assert.ok(syncModule.hasHeldLinks());
    });

    it("holds a link encrypted with a key version it lacks until that version arrives", () => {
        const transport = new MockTransport();
        setup(transport);
        const rk1 = generateRoomKey();
        const rk2 = generateRoomKey();
        keyRing = new Map([[1, rk1]]);
        const link = makeLink({ source: "wrong-version" });
        const wireLink = encryptLinkForWire(link, rk2, 2); // encrypted with version 2, ring only has 1

        syncModule.applyInboundWireDiff({ additions: [wireLink], removals: [] }, 5, "rev-5");
        assert.equal(store.allLinks().links.length, 0, "undecryptable link must not appear in store");
        assert.equal(store.getSequence(), 5, "sequence must advance — the link is held, not lost");
        assert.deepEqual(syncModule.retryHeld(), { additions: [], removals: [] }, "version 1 does not open it");

        keyRing = new Map([[1, rk1], [2, rk2]]);
        assert.deepEqual(syncModule.retryHeld(), { additions: [link], removals: [] });
        assert.deepEqual(emittedDiffs, [{ additions: [link], removals: [] }]);
        assert.equal(syncModule.hasHeldLinks(), false);
    });

    it("applies plaintext links and skips undecryptable ones in a mixed batch", () => {
        const transport = new MockTransport();
        setup(transport);
        const rk = generateRoomKey();
        keyRing = new Map([[1, rk]]);
        const plaintextLink = makeLink({ source: "visible" });
        const encryptedLink = encryptLinkForWire(makeLink({ source: "secret" }), rk, 2); // version 2, ring only has 1

        syncModule.applyInboundWireDiff(
            { additions: [plaintextLink, encryptedLink], removals: [] }, 5, "rev-5",
        );

        assert.equal(store.allLinks().links.length, 1, "plaintext link must be applied");
        assert.deepEqual(store.allLinks().links[0].data.source, "visible");
        assert.equal(emittedDiffs.length, 1, "diff emitted with the plaintext link");
        assert.ok(syncModule.hasHeldLinks(), "the undecryptable link is held");
        assert.equal(store.getSequence(), 5, "sequence advances past the batch");
    });

    it("encrypted removal across key versions removes the correct link", () => {
        const transport = new MockTransport();
        setup(transport);
        const rk1 = generateRoomKey();
        const rk2 = generateRoomKey();
        keyRing = new Map([[1, rk1], [2, rk2]]);
        const link = makeLink({ source: "will-be-removed" });

        // Add with version 1
        const wireAdd = encryptLinkForWire(link, rk1, 1);
        syncModule.applyInboundWireDiff({ additions: [wireAdd], removals: [] }, 1, "rev-1");
        assert.equal(store.allLinks().links.length, 1);

        // Remove with version 2 (after key rotation)
        const wireRemove = encryptLinkForWire(link, rk2, 2);
        syncModule.applyInboundWireDiff({ additions: [], removals: [wireRemove] }, 2, "rev-2");
        assert.equal(store.allLinks().links.length, 0, "removal must match by plaintext identity, not by key version");
    });
});

// ---------------------------------------------------------------------------
// toWireLink / toWireDiff — outbound encryption contracts
// ---------------------------------------------------------------------------

describe("sync: toWireLink contracts", () => {
    it("produces plaintext wire link when key ring is null", async () => {
        const transport = new MockTransport();
        setup(transport);
        // keyRing stays null
        let capturedBody = "";
        transport.route(
            (url, method) => method === "POST" && url.includes("/commit"),
            (_url, _method, body) => {
                capturedBody = body;
                return { status: 200, headers: {}, body: JSON.stringify({ sequence: 1, revision: "rev-1" }) };
            },
        );

        const link = makeLink();
        await syncModule.commit({ additions: [link], removals: [] });
        const sent = JSON.parse(capturedBody);
        assert.equal(sent.additions[0].author, "did:key:zAuthor", "plaintext link must retain author");
        assert.equal(sent.additions[0].data.source, "a", "plaintext link must retain data fields");
        assert.equal(isEncryptedLinkData(sent.additions[0].data), false, "data must not be encrypted");
    });

    it("encrypts wire link when key ring has keys", async () => {
        const transport = new MockTransport();
        setup(transport);
        const rk = generateRoomKey();
        keyRing = new Map([[1, rk]]);
        let capturedBody = "";
        transport.route(
            (url, method) => method === "POST" && url.includes("/commit"),
            (_url, _method, body) => {
                capturedBody = body;
                return { status: 200, headers: {}, body: JSON.stringify({ sequence: 1, revision: "rev-1" }) };
            },
        );

        const link = makeLink();
        await syncModule.commit({ additions: [link], removals: [] });
        const sent = JSON.parse(capturedBody);
        assert.ok(isEncryptedLinkData(sent.additions[0].data), "data must be encrypted");
        assert.equal(sent.additions[0].author, undefined, "author must be absent on encrypted wire link");
        assert.equal(sent.additions[0].key_version, 1, "key_version must be set");
        assert.equal(typeof sent.additions[0].link_hash, "string", "link_hash must be present");
    });

    it("throws when key ring exists but has no keys (empty Map)", async () => {
        const transport = new MockTransport();
        setup(transport);
        keyRing = new Map(); // non-null but empty

        const link = makeLink();
        await assert.rejects(
            () => syncModule.commit({ additions: [link], removals: [] }),
            /key ring has no keys/,
        );
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

        const emitted = await syncModule.catchUp();

        assert.equal(emittedDiffs.length, 2);
        assert.equal(store.allLinks().links.length, 2);
        assert.equal(store.getSequence(), 2);
        assert.equal(store.getRevision(), "rev-2");
        assert.deepEqual(emitted, { additions: [linkA, linkB], removals: [] });
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

    it("re-reads the key ring on every catch-up while links are held, and emits them once a key opens them", async () => {
        // A member's first catch-up meets an encrypted diff before any member
        // has granted it the key. The cursor advances past the diff; the link
        // is held. A later catch-up's key ring read finds the grant.
        const transport = new MockTransport();
        setup(transport);
        const rk = generateRoomKey();
        const link = makeLink({ source: "encrypted-retry" });
        const wireLink = encryptLinkForWire(link, rk, 1);

        let refreshCallCount = 0;
        syncModule.initSync({
            config,
            getToken: async () => "test-token",
            emitDiff: (diff) => emittedDiffs.push(diff),
            emitSyncState: (state) => syncStates.push(state),
            getKeyRing: () => keyRing,
            refreshKeyRing: async () => {
                refreshCallCount++;
                if (refreshCallCount === 1) throw new Error("timed out after 10000 ms");
                if (refreshCallCount === 3) keyRing = new Map([[1, rk]]);
            },
        });
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            (url) => {
                const since = new URL(url).searchParams.get("since");
                const diffs = since === "0" ? [{ additions: [wireLink], removals: [] }] : [];
                return jsonResponse({ diffs, revision: "rev-1", sequence: 1 });
            },
        );

        await syncModule.catchUp(); // refresh throws
        await syncModule.catchUp(); // refresh finds no key
        assert.equal(emittedDiffs.length, 0);
        await syncModule.catchUp(); // refresh finds the key
        await syncModule.catchUp(); // nothing held: no refresh

        assert.equal(refreshCallCount, 3);
        assert.deepEqual(emittedDiffs, [{ additions: [link], removals: [] }]);
        assert.equal(store.getSequence(), 1);
    });

    it("does not re-read the key ring when no link is held", async () => {
        const transport = new MockTransport();
        setup(transport);
        let refreshCalled = false;
        syncModule.initSync({
            config,
            getToken: async () => "test-token",
            emitDiff: (diff) => emittedDiffs.push(diff),
            emitSyncState: (state) => syncStates.push(state),
            getKeyRing: () => keyRing,
            refreshKeyRing: async () => { refreshCalled = true; },
        });
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => jsonResponse({ diffs: [{ additions: [makeLink()], removals: [] }], revision: "rev-1", sequence: 1 }),
        );

        await syncModule.catchUp();
        assert.equal(refreshCalled, false);
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
        assert.ok(!isEncryptedLinkData(posted.additions[0].data));
    });

    it("posts an encrypted wire diff when a key ring is set", async () => {
        const transport = new MockTransport();
        setup(transport);
        keyRing = new Map([[1, generateRoomKey()]]);
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

        assert.ok(isEncryptedLinkData(posted.additions[0].data));
    });

    it("propagates a network failure to the caller", async () => {
        const transport = new MockTransport();
        setup(transport);
        transport.route(() => true, () => ({ status: 500, headers: {}, body: "boom" }));

        await assert.rejects(() => syncModule.commit({ additions: [makeLink()], removals: [] }));
    });
});

// ---------------------------------------------------------------------------
// commit: the store follows the server
// ---------------------------------------------------------------------------

/** A link-server stand-in: an OR-Set keyed by link identity (as the real
 *  server keys by link hash), one sequence number per accepted POST. */
class MockLinkServer {
    links = new Map<string, LinkExpression>();
    sequence = 0;
    /** Answers the next N POSTs with a 500 instead of accepting them. */
    failNext = 0;

    constructor(transport: MockTransport) {
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            (_url, _method, body) => {
                if (this.failNext > 0) {
                    this.failNext--;
                    return { status: 500, headers: {}, body: "{\"error\":\"down\"}" };
                }
                const diff = JSON.parse(body) as PerspectiveDiff;
                for (const l of diff.additions) this.links.set(identity(l), l);
                for (const l of diff.removals) this.links.delete(identity(l));
                this.sequence++;
                return {
                    status: 200,
                    headers: {},
                    body: JSON.stringify({ sequence: this.sequence, revision: `rev-${this.sequence}` }),
                };
            },
        );
    }
}

function identity(l: LinkExpression): string {
    return JSON.stringify([l.author, l.timestamp, l.data.source, l.data.predicate ?? null, l.data.target]);
}

function manyLinks(n: number, prefix = "s"): LinkExpression[] {
    return Array.from({ length: n }, (_, i) => makeLink({ source: `${prefix}-${i}` }));
}

describe("sync: commit keeps the store in step with the server", () => {
    it("a commit the server rejects leaves the store unchanged", async () => {
        const transport = new MockTransport();
        setup(transport);
        const server = new MockLinkServer(transport);
        server.failNext = 1;

        await assert.rejects(() => syncModule.commit({ additions: manyLinks(3), removals: [] }));

        assert.equal(store.allLinks().links.length, 0, "links the server never got must not be stored");
        assert.equal(server.links.size, 0);
    });

    it("a retry after recovery stores each link once, and the server has them all", async () => {
        const transport = new MockTransport();
        setup(transport);
        const server = new MockLinkServer(transport);
        const diff = { additions: manyLinks(3), removals: [] };
        server.failNext = 2;

        await assert.rejects(() => syncModule.commit(diff));
        await assert.rejects(() => syncModule.commit(diff));
        await syncModule.commit(diff);

        assert.equal(store.allLinks().links.length, 3);
        assert.equal(server.links.size, 3);
    });

    it("an accepted commit is in the store and is not echoed back to the executor", async () => {
        const transport = new MockTransport();
        setup(transport);
        new MockLinkServer(transport);

        await syncModule.commit({ additions: manyLinks(2), removals: [] });

        assert.equal(store.allLinks().links.length, 2);
        assert.equal(emittedDiffs.length, 0, "the executor already holds its own links");
    });

    it("an accepted removal leaves the store", async () => {
        const transport = new MockTransport();
        setup(transport);
        new MockLinkServer(transport);
        const [a, b] = manyLinks(2);
        await syncModule.commit({ additions: [a, b], removals: [] });

        await syncModule.commit({ additions: [], removals: [a] });

        assert.deepEqual(store.allLinks().links, [b]);
    });

    it("advances the sync cursor past its own commit when nothing came in between", async () => {
        const transport = new MockTransport();
        setup(transport);
        const server = new MockLinkServer(transport);

        await syncModule.commit({ additions: manyLinks(1), removals: [] });
        assert.equal(store.getSequence(), 1);
        assert.equal(store.getRevision(), "rev-1");

        // Another agent's diff (sequence 2) has not reached us yet: our
        // commit lands at 3, so the cursor must stay at 1 or catch-up
        // would skip sequence 2.
        server.sequence++;
        await syncModule.commit({ additions: manyLinks(1, "t"), removals: [] });
        assert.equal(store.getSequence(), 1);
    });
});

// ---------------------------------------------------------------------------
// bootstrap
// ---------------------------------------------------------------------------

describe("sync: bootstrap", () => {
    it("pulls a full render snapshot into the local store, emits it, and sets the cursor", async () => {
        const transport = new MockTransport();
        setup(transport);
        const linkA = makeLink({ source: "a" });
        const linkB = makeLink({ source: "b" });

        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => ({ status: 200, headers: {}, body: JSON.stringify({ links: [linkA, linkB], revision: "rev-snap", sequence: 7 }) }),
        );

        await syncModule.bootstrap();

        assert.equal(store.allLinks().links.length, 2);
        assert.equal(store.getRevision(), "rev-snap");
        assert.equal(store.getSequence(), 7);
        assert.deepEqual(emittedDiffs, [{ additions: [linkA, linkB], removals: [] }]);
    });

    it("holds the snapshot's undecryptable links, so catchUp re-reads the key ring", async () => {
        const transport = new MockTransport();
        resetAdapters();
        syncModule._resetBatchStateForTests();
        initAdapters({ storage: new MockStorage(), transport, config });
        store.initStore(simpleHash);
        emittedDiffs = [];
        syncStates = [];
        keyRing = null;

        let refreshCalled = false;
        syncModule.initSync({
            config,
            getToken: async () => "test-token",
            emitDiff: (diff) => emittedDiffs.push(diff),
            emitSyncState: (state) => syncStates.push(state),
            getKeyRing: () => keyRing,
            refreshKeyRing: async () => { refreshCalled = true; return false; },
        });

        const rk = generateRoomKey();
        const plainLink = makeLink({ source: "plain" });
        const encLink = encryptLinkForWire(makeLink({ source: "secret" }), rk, 3);

        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => ({
                status: 200,
                headers: {},
                body: JSON.stringify({ links: [plainLink, encLink], revision: "rev-1", sequence: 1 }),
            }),
        );

        await syncModule.bootstrap();

        assert.equal(store.allLinks().links.length, 1, "only the plaintext link should be stored");
        assert.equal(store.allLinks().links[0].data.source, "plain");

        // catchUp with empty diffs — should still attempt refresh because
        // bootstrap held the version 3 link.
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => ({ status: 200, headers: {}, body: JSON.stringify({ diffs: [], revision: "rev-1", sequence: 1 }) }),
        );

        await syncModule.catchUp();
        assert.equal(refreshCalled, true, "catchUp should retry key ring refresh for versions tracked by bootstrap");
    });

    it("replaces stale local links with the server snapshot", async () => {
        const transport = new MockTransport();
        setup(transport);

        // Pre-populate with a stale link that the server no longer has.
        const staleLink = makeLink({ source: "stale" });
        store.putLink(staleLink);
        assert.equal(store.allLinks().links.length, 1);

        const freshLink = makeLink({ source: "fresh" });
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => ({ status: 200, headers: {}, body: JSON.stringify({ links: [freshLink], revision: "rev-2", sequence: 5 }) }),
        );

        await syncModule.bootstrap();

        const links = store.allLinks().links;
        assert.equal(links.length, 1, "stale link should have been removed");
        assert.equal(links[0].data.source, "fresh");
        assert.deepEqual(emittedDiffs, [{ additions: [freshLink], removals: [staleLink] }]);
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

// ---------------------------------------------------------------------------
// coalesceDiffs — pure logic
// ---------------------------------------------------------------------------

describe("sync: coalesceDiffs (batch merge safety)", () => {
    function link(overrides: Partial<LinkExpression["data"]> & { sig?: string; author?: string }): LinkExpression {
        const { sig, author, ...data } = overrides;
        return {
            author: author ?? "did:key:zAuthor",
            timestamp: "2026-01-01T00:00:00.000Z",
            data: { source: "s", target: "t", predicate: "p", ...data },
            proof: { signature: sig ?? "", key: "key" },
        };
    }

    it("merges consecutive pure additions into a single segment (bulk-add hot path)", () => {
        const diffs = Array.from({ length: 1500 }, (_, i) => ({
            additions: [link({ source: `s${i}` })],
            removals: [] as LinkExpression[],
        }));
        const segments = syncModule.coalesceDiffs(diffs);
        assert.equal(segments.length, 1, "1500 pure additions should collapse to one POST");
        assert.equal(segments[0].additions.length, 1500);
        assert.equal(segments[0].removals.length, 0);
    });

    it("splits when a later diff adds a link that an earlier diff removed (remove-then-add preserves ordering)", () => {
        // User intent: remove X, then re-add X → X should exist afterwards.
        // Merging would give additions:[X],removals:[X]. Server applies adds
        // then removes, and X would be gone. Split forces two POSTs so the
        // server applies them in caller order.
        const X = link({ source: "x", sig: "sig-X" });
        const segments = syncModule.coalesceDiffs([
            { additions: [], removals: [X] },
            { additions: [X], removals: [] },
        ]);
        assert.equal(segments.length, 2);
        assert.deepEqual(segments[0].removals, [X]);
        assert.deepEqual(segments[1].additions, [X]);
    });

    it("splits when a later diff removes a link that an earlier diff added", () => {
        // User intent: add X, then remove X → X should be gone.
        // Merging here would happen to give the right answer (adds-then-removes
        // in server order), but caller-visible ordering is still preserved by
        // splitting — critical for anything relying on emitted-diff sequence.
        const X = link({ source: "x", sig: "sig-X" });
        const segments = syncModule.coalesceDiffs([
            { additions: [X], removals: [] },
            { additions: [], removals: [X] },
        ]);
        assert.equal(segments.length, 2);
    });

    it("merges when link identities never collide across the boundary", () => {
        const X = link({ source: "x", sig: "sig-X" });
        const Y = link({ source: "y", sig: "sig-Y" });
        const segments = syncModule.coalesceDiffs([
            { additions: [X], removals: [] },
            { additions: [Y], removals: [] },
            { additions: [], removals: [X] },
        ]);
        // First two diffs merge (X-add + Y-add, no collision). Third splits
        // because removing X collides with the segment's X-add.
        assert.equal(segments.length, 2);
        assert.deepEqual(segments[0].additions.map((l) => l.data.source), ["x", "y"]);
        assert.deepEqual(segments[1].removals.map((l) => l.data.source), ["x"]);
    });

    it("uses canonical fields (not signature) — same link with different signatures collides", () => {
        // Two links with identical canonical fields but different
        // signatures ARE the same link from the server's perspective
        // (linkHash ignores the signature). A signature-based identity
        // would let remove(oldSig) + add(newSig) merge into one segment
        // and the server would apply add-then-remove → X gone, contrary
        // to caller intent. Canonical-only identity forces the split.
        const oldSig = link({ source: "x", sig: "sig-old" });
        const reSigned = link({ source: "x", sig: "sig-new" });
        const segments = syncModule.coalesceDiffs([
            { additions: [], removals: [oldSig] },
            { additions: [reSigned], removals: [] },
        ]);
        assert.equal(segments.length, 2, "identity must treat re-signed links as the same link");
    });

    it("works for signature-less fixtures via canonical fields", () => {
        // Same canonical fields, no signature at all → same identity.
        const X = link({ source: "x", sig: "" });
        const segments = syncModule.coalesceDiffs([
            { additions: [], removals: [X] },
            { additions: [X], removals: [] },
        ]);
        assert.equal(segments.length, 2, "signature-less links must still collide by canonical fields");
    });

    it("collides regardless of whether one side has a signature and the other doesn't", () => {
        // Locks in the identity invariant across the mixed-signature case:
        // remove(unsigned copy) then add(signed copy) of the same logical
        // link must still split into two segments, because canonical
        // identity ignores the signature entirely.
        const signed = link({ source: "x", sig: "sig-X" });
        const unsigned = link({ source: "x", sig: "" });
        const segments = syncModule.coalesceDiffs([
            { additions: [], removals: [unsigned] },
            { additions: [signed], removals: [] },
        ]);
        assert.equal(segments.length, 2, "same link identity must split regardless of signature presence");
    });

    it("distinguishes links whose fields would collide under naïve pipe-serialisation", () => {
        // author="a|b" timestamp="t" vs author="a" timestamp="b|t" would
        // collide under `${author}|${timestamp}`. JSON serialisation
        // escapes the boundary so identities stay distinct — meaning a
        // remove of A must NOT be treated as a conflict with a segment
        // containing B, and vice versa.
        const A: LinkExpression = {
            author: "a|b",
            timestamp: "t",
            data: { source: "s", target: "t", predicate: "p" },
            proof: { signature: "", key: "k" },
        };
        const B: LinkExpression = {
            author: "a",
            timestamp: "b|t",
            data: { source: "s", target: "t", predicate: "p" },
            proof: { signature: "", key: "k" },
        };
        // Adding both together shouldn't count as a self-conflict when a
        // later diff removes A: only A's segment should conflict.
        const segments = syncModule.coalesceDiffs([
            { additions: [A, B], removals: [] },
            { additions: [], removals: [A] },
        ]);
        assert.equal(segments.length, 2);
        assert.equal(segments[1].removals[0].author, "a|b");
    });
});

// ---------------------------------------------------------------------------
// enqueueCommitBatched / drainCommitBatch — flush behavior
// ---------------------------------------------------------------------------

describe("sync: enqueueCommitBatched", () => {
    function makeCommitTransport(): { transport: MockTransport; posts: any[] } {
        const transport = new MockTransport();
        const posts: any[] = [];
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            (_url, _method, body) => {
                posts.push(JSON.parse(body));
                return { status: 200, headers: {}, body: "{}" };
            },
        );
        return { transport, posts };
    }

    it("collapses a burst of 1500 addLinks into ONE POST (the stress-test hot path)", async () => {
        const { transport, posts } = makeCommitTransport();
        setup(transport);
        for (let i = 0; i < 1500; i++) {
            syncModule.enqueueCommitBatched({
                additions: [{
                    author: "did:key:zAuthor",
                    timestamp: `2026-01-01T00:00:00.${String(i).padStart(3, "0")}Z`,
                    data: { source: `s${i}`, target: "t", predicate: "p" },
                    proof: { signature: `sig-${i}`, key: "k" },
                }],
                removals: [],
            });
        }
        await syncModule.drainCommitBatch();
        assert.equal(posts.length, 1, "microtask flush should coalesce the burst into one POST");
        assert.equal(posts[0].additions.length, 1500);
    });

    it("splits into two POSTs when remove(X) is followed by add(X)", async () => {
        const { transport, posts } = makeCommitTransport();
        setup(transport);
        const X: LinkExpression = {
            author: "did:key:zAuthor",
            timestamp: "2026-01-01T00:00:00.000Z",
            data: { source: "x", target: "t", predicate: "p" },
            proof: { signature: "sig-X", key: "k" },
        };
        syncModule.enqueueCommitBatched({ additions: [], removals: [X] });
        syncModule.enqueueCommitBatched({ additions: [X], removals: [] });
        await syncModule.drainCommitBatch();
        assert.equal(posts.length, 2, "must not swap remove-then-add into add-then-remove");
        assert.deepEqual(posts[0].removals.length, 1);
        assert.deepEqual(posts[1].additions.length, 1);
    });

    it("drainCommitBatch flushes anything still pending (used by teardown)", async () => {
        const { transport, posts } = makeCommitTransport();
        setup(transport);
        syncModule.enqueueCommitBatched({
            additions: [{
                author: "did:key:zAuthor",
                timestamp: "2026-01-01T00:00:00.000Z",
                data: { source: "s", target: "t", predicate: "p" },
                proof: { signature: "sig", key: "k" },
            }],
            removals: [],
        });
        await syncModule.drainCommitBatch();
        assert.equal(posts.length, 1);
    });

    it("retries a failing segment with backoff and succeeds when the server recovers", async () => {
        const transport = new MockTransport();
        let calls = 0;
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            () => {
                calls++;
                if (calls < 3) return { status: 500, headers: {}, body: "transient" };
                return { status: 200, headers: {}, body: "{}" };
            },
        );
        setup(transport);
        syncModule.enqueueCommitBatched({
            additions: [{
                author: "did:key:zAuthor",
                timestamp: "2026-01-01T00:00:00.000Z",
                data: { source: "s", target: "t", predicate: "p" },
                proof: { signature: "sig", key: "k" },
            }],
            removals: [],
        });
        await syncModule.drainCommitBatch();
        assert.equal(calls, 3, "must retry up to MAX_COMMIT_ATTEMPTS on transient failure");
        assert.ok(!syncStates.includes("LinkLanguageInstalledButNotSynced"),
            "successful retry should not emit the not-synced state");
    });

    it("a throwing emitSyncState during a failed flush does not poison the flush chain", async () => {
        // Regression: _inflight is a single promise chain. If a flush
        // rejects (e.g. because emitSyncState throws after resetAdapters),
        // every subsequent `_inflight.then(flushBatch)` would be skipped
        // and no future enqueue would ever POST. The terminal `.catch()`
        // on the chain — plus emitSyncStateSafe wrapping the emit — must
        // keep the chain live.
        const transport = new MockTransport();
        const posts: any[] = [];
        let shouldFail = true;
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            (_url, _method, body) => {
                posts.push(JSON.parse(body));
                if (shouldFail) return { status: 500, headers: {}, body: "hard failure" };
                return { status: 200, headers: {}, body: "{}" };
            },
        );
        resetAdapters();
        syncModule._resetBatchStateForTests();
        initAdapters({ storage: new MockStorage(), transport, config });
        store.initStore(simpleHash);
        emittedDiffs = [];
        syncStates = [];
        keyRing = null;
        // Throw from emitSyncState — simulates a torn-down runtime.
        syncModule.initSync({
            config,
            getToken: async () => "test-token",
            emitDiff: (diff) => emittedDiffs.push(diff),
            emitSyncState: () => { throw new Error("runtime torn down"); },
            getKeyRing: () => keyRing,
        });

        // First flush: hard-fails all retries → tries to emit → emitter throws.
        const X: LinkExpression = {
            author: "did:key:zAuthor",
            timestamp: "2026-01-01T00:00:00.000Z",
            data: { source: "x", target: "t", predicate: "p" },
            proof: { signature: "sig-X", key: "k" },
        };
        syncModule.enqueueCommitBatched({ additions: [X], removals: [] });
        // Await one flush cycle only — drainCommitBatch would re-schedule
        // the permanently-failing segment indefinitely.
        await syncModule._awaitInflightForTests();
        assert.equal(posts.length, 3, "first flush must attempt all 3 retries");

        // Now the server recovers. The chain must still be live — the
        // next enqueue must actually POST. The re-enqueued X from the
        // failed flush coalesces with Y into one successful commit.
        shouldFail = false;
        const Y: LinkExpression = {
            author: "did:key:zAuthor",
            timestamp: "2026-01-01T00:00:00.001Z",
            data: { source: "y", target: "t", predicate: "p" },
            proof: { signature: "sig-Y", key: "k" },
        };
        syncModule.enqueueCommitBatched({ additions: [Y], removals: [] });
        await syncModule.drainCommitBatch();
        assert.ok(posts.length > 3, "chain must still be live after the poisoning attempt");
        const successfulAdds = posts.slice(3).flatMap((p: any) => p.additions);
        assert.ok(
            successfulAdds.some((a: any) => a.data.source === "y"),
            "Y must appear in a successful commit after the chain survived",
        );
    });

    it("re-enqueues failed segments for the next flush cycle", async () => {
        // When a segment fails all retries, it and all remaining
        // downstream segments get re-enqueued into the pending queue
        // so the next flush picks them up automatically.
        const transport = new MockTransport();
        const posts: any[] = [];
        let failCount = 0;
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            (_url, _method, body) => {
                posts.push(JSON.parse(body));
                failCount++;
                // Fail the first 3 attempts (one full retry cycle),
                // then succeed so the drain can settle.
                if (failCount <= 3) return { status: 500, headers: {}, body: "hard failure" };
                return { status: 200, headers: {}, body: "{}" };
            },
        );
        setup(transport);
        const X: LinkExpression = {
            author: "did:key:zAuthor",
            timestamp: "2026-01-01T00:00:00.000Z",
            data: { source: "x", target: "t", predicate: "p" },
            proof: { signature: "sig-X", key: "k" },
        };
        // Forces three segments: remove(X); add(X); remove(X).
        syncModule.enqueueCommitBatched({ additions: [], removals: [X] });
        syncModule.enqueueCommitBatched({ additions: [X], removals: [] });
        syncModule.enqueueCommitBatched({ additions: [], removals: [X] });
        await syncModule.drainCommitBatch();
        // First flush: segment 1 fails 3 times → re-enqueued with segments 2+3.
        // Second flush: segment 1 succeeds (attempt 4), then segments 2+3.
        assert.ok(posts.length > 3,
            `re-enqueued segments should produce more POSTs than the initial 3 retries (got ${posts.length})`);
        assert.ok(syncStates.includes("LinkLanguageInstalledButNotSynced"),
            "must report LinkLanguageInstalledButNotSynced on first failure");
    });
});

// ---------------------------------------------------------------------------
// No history is lost (BUG-103) and every stored link is emitted once
// ---------------------------------------------------------------------------

/** How often each link (by source) was emitted as an addition / a removal. */
function emitCounts(kind: "additions" | "removals"): Map<string, number> {
    const counts = new Map<string, number>();
    for (const diff of emittedDiffs) {
        for (const link of diff[kind]) counts.set(link.data.source, (counts.get(link.data.source) ?? 0) + 1);
    }
    return counts;
}

function jsonResponse(body: unknown): TransportResponse {
    return { status: 200, headers: {}, body: JSON.stringify(body) };
}

describe("sync: no history is lost", () => {
    it("links that arrive before their key are emitted once the key is held, though /render fails", async () => {
        const transport = new MockTransport();
        setup(transport);
        const roomKey = generateRoomKey();
        const granted: KeyRing = new Map([[1, roomKey]]);
        // The member holds no key yet; the next key ring read finds the grant.
        syncModule.initSync({
            config,
            getToken: async () => "test-token",
            emitDiff: (diff) => emittedDiffs.push(diff),
            emitSyncState: (state) => syncStates.push(state),
            getKeyRing: () => keyRing,
            refreshKeyRing: async () => {
                const grew = keyRing === null;
                keyRing = granted;
                return grew;
            },
        });
        const wsLink = encryptLinkForWire(makeLink({ source: "pushed" }), roomKey, 1);
        const syncedLink = encryptLinkForWire(makeLink({ source: "caught-up" }), roomKey, 1);
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=1"),
            () => jsonResponse({ diffs: [{ additions: [syncedLink], removals: [] }], revision: "rev-2", sequence: 2 }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=2"),
            () => jsonResponse({ diffs: [], revision: "rev-2", sequence: 2 }),
        );
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => { throw new Error("timed out after 10000 ms"); },
        );

        // A WebSocket push the member cannot decrypt, then a catch-up batch it
        // cannot decrypt either; the key arrives during that catch-up.
        syncModule.applyInboundWireDiff({ additions: [wsLink], removals: [] }, 1, "rev-1");
        await syncModule.catchUp();
        await syncModule.catchUp();
        await syncModule.catchUp();

        const added = emitCounts("additions");
        assert.equal(added.get("pushed"), 1, "the WebSocket link is emitted exactly once");
        assert.equal(added.get("caught-up"), 1, "the catch-up link is emitted exactly once");
        assert.deepEqual(
            store.allLinks().links.map((l) => l.data.source).sort(),
            ["caught-up", "pushed"],
        );
    });

    it("a member holding its key at start gets the room's history from the startup snapshot", async () => {
        const transport = new MockTransport();
        setup(transport);
        const roomKey = generateRoomKey();
        keyRing = new Map([[1, roomKey]]);
        const a = encryptLinkForWire(makeLink({ source: "a" }), roomKey, 1);
        const b = encryptLinkForWire(makeLink({ source: "b" }), roomKey, 1);
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => jsonResponse({ links: [a, b], revision: "rev-7", sequence: 7 }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=7"),
            () => jsonResponse({ diffs: [], revision: "rev-7", sequence: 7 }),
        );

        await syncModule.bootstrap();
        await syncModule.catchUp();

        const added = emitCounts("additions");
        assert.equal(added.get("a"), 1);
        assert.equal(added.get("b"), 1);
        assert.equal(store.getSequence(), 7);
    });

    it("the startup snapshot before the key arrives holds the history, and emits it once the key is held", async () => {
        const transport = new MockTransport();
        setup(transport);
        const roomKey = generateRoomKey();
        const a = encryptLinkForWire(makeLink({ source: "a" }), roomKey, 1);
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => jsonResponse({ links: [a], revision: "rev-1", sequence: 1 }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=1"),
            () => jsonResponse({ diffs: [], revision: "rev-1", sequence: 1 }),
        );

        await syncModule.bootstrap();
        assert.equal(emitCounts("additions").get("a"), undefined, "nothing to emit without the key");

        keyRing = new Map([[1, roomKey]]);
        await syncModule.catchUp();
        await syncModule.catchUp();

        assert.equal(emitCounts("additions").get("a"), 1);
    });

    it("a later removal of a held link cancels it: it is never emitted", () => {
        const transport = new MockTransport();
        setup(transport);
        const v1 = generateRoomKey();
        const v2 = generateRoomKey();
        keyRing = new Map([[1, v1]]);
        const link = makeLink({ source: "short-lived" });

        syncModule.applyInboundWireDiff({ additions: [encryptLinkForWire(link, v2, 2)], removals: [] }, 1, "rev-1");
        syncModule.applyInboundWireDiff({ additions: [], removals: [encryptLinkForWire(link, v1, 1)] }, 2, "rev-2");
        keyRing = new Map([[1, v1], [2, v2]]);
        syncModule.retryHeld();

        assert.equal(emitCounts("additions").get("short-lived"), undefined);
        assert.equal(emitCounts("removals").get("short-lived"), undefined);
        assert.equal(store.allLinks().links.length, 0);
    });

    it("a link that arrives twice (own commit fetched back, a WS push and a catch-up) is emitted once", () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink({ source: "twice" });

        syncModule.applyInboundWireDiff({ additions: [link], removals: [] }, 1, "rev-1");
        syncModule.applyInboundWireDiff({ additions: [link], removals: [] }, 1, "rev-1");

        assert.equal(emitCounts("additions").get("twice"), 1);
    });

    it("a snapshot while the key ring is not loaded does not remove links the member already holds", async () => {
        const transport = new MockTransport();
        setup(transport);
        const roomKey = generateRoomKey();
        const kept = makeLink({ source: "kept" });
        const gone = makeLink({ source: "gone" });
        store.applyDiff({ additions: [kept, gone], removals: [] });
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => jsonResponse({ links: [encryptLinkForWire(kept, roomKey, 1)], revision: "rev-3", sequence: 3 }),
        );

        await syncModule.bootstrap();

        assert.deepEqual(store.allLinks().links.map((l) => l.data.source), ["kept"]);
        assert.equal(emitCounts("removals").get("gone"), 1, "a link deleted while offline is removed from the executor");
        assert.equal(emitCounts("removals").get("kept"), undefined);
        assert.equal(emitCounts("additions").get("kept"), undefined);
    });
});
