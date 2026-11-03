/**
 * Tests for src/sync.ts — most importantly the single inbound path, which
 * MUST call emitDiff for every link that enters the store, once (see the
 * "CRITICAL RULE" note in src/sync.ts: 7 of 13 existing link languages got
 * this wrong), and must not lose a link it cannot decrypt yet.
 */

import { afterEach, describe, it, mock } from "node:test";
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
    syncModule._resetRenderStateForTests();
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
    /** Every POST /commit, accepted or not. */
    posts = 0;

    constructor(transport: MockTransport) {
        transport.route(
            (url, method) => method === "POST" && url.endsWith("/commit"),
            (_url, _method, body) => {
                this.posts++;
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

    it("a refused commit leaves nothing behind for a restart: the executor's retry is its only send", async () => {
        // The executor keeps a diff the language refused in its own
        // pending-diffs table and retries it after a restart; the language
        // keeps no outbox, so a restarted instance sends nothing by itself.
        const transport = new MockTransport();
        const storage = new MockStorage();
        const server = new MockLinkServer(transport);
        const start = () => {
            resetAdapters();
            initAdapters({ storage, transport, config });
            store.initStore(simpleHash);
            syncModule.initSync({
                config,
                getToken: async () => "test-token",
                emitDiff: (diff) => emittedDiffs.push(diff),
                getKeyRing: () => null,
            });
        };
        const diff = { additions: manyLinks(3), removals: [] };
        start();
        server.failNext = 1;
        await assert.rejects(() => syncModule.commit(diff));
        assert.deepEqual(storage.listKeys(""), [], "a refused commit writes nothing to the language's storage");

        start(); // the restart: a new instance over the same storage
        assert.equal(server.posts, 1, "a restarted instance sends nothing by itself");
        await syncModule.commit(diff); // the executor's retry
        assert.equal(server.posts, 2);
        assert.equal(server.sequence, 1, "the server accepted the diff once");
        assert.equal(store.allLinks().links.length, 3);
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

describe("sync: a link no key opens costs that link only", () => {
    /** `link` encrypted with `key`, its ciphertext altered so the key it names
     *  does not open it (an AES-GCM tag failure). */
    function unopenable(link: LinkExpression, key: Uint8Array, version: number) {
        const wire = encryptLinkForWire(link, key, version);
        const data = wire.data as { ciphertext: string; nonce: string };
        const flipped = (parseInt(data.ciphertext.slice(0, 2), 16) ^ 0xff).toString(16).padStart(2, "0");
        return { ...wire, data: { ...data, ciphertext: flipped + data.ciphertext.slice(2) } };
    }

    it("held links around one the key does not open are all emitted once the key is held", () => {
        const transport = new MockTransport();
        setup(transport);
        const roomKey = generateRoomKey();
        // Held in this order under a missing key: valid, unopenable, valid.
        syncModule.applyInboundWireDiff(
            { additions: [encryptLinkForWire(makeLink({ source: "a" }), roomKey, 1)], removals: [] }, 1, "rev-1");
        syncModule.applyInboundWireDiff(
            { additions: [unopenable(makeLink({ source: "b" }), roomKey, 1)], removals: [] }, 2, "rev-2");
        syncModule.applyInboundWireDiff(
            { additions: [encryptLinkForWire(makeLink({ source: "c" }), roomKey, 1)], removals: [] }, 3, "rev-3");
        assert.equal(emittedDiffs.length, 0);

        keyRing = new Map([[1, roomKey]]);
        for (let i = 0; i < 2; i++) {
            try { syncModule.retryHeld(); } catch { /* the defect: a throw here must not lose a link */ }
        }

        const added = emitCounts("additions");
        assert.equal(added.get("a"), 1, "a valid held link before the unopenable one reaches the executor");
        assert.equal(added.get("c"), 1, "a valid held link after the unopenable one reaches the executor");
        assert.equal(added.get("b"), undefined);
        assert.equal(syncModule.hasHeldLinks(), false, "nothing stays held for a key the member already has");
    });

    it("a catch-up batch with a link the key does not open still applies the rest of the batch", async () => {
        const transport = new MockTransport();
        setup(transport);
        const roomKey = generateRoomKey();
        keyRing = new Map([[1, roomKey]]);
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=0"),
            () => jsonResponse({
                diffs: [
                    { additions: [encryptLinkForWire(makeLink({ source: "a" }), roomKey, 1)], removals: [] },
                    { additions: [unopenable(makeLink({ source: "b" }), roomKey, 1)], removals: [] },
                    { additions: [encryptLinkForWire(makeLink({ source: "c" }), roomKey, 1)], removals: [] },
                ],
                revision: "rev-3",
                sequence: 3,
            }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=3"),
            () => jsonResponse({ diffs: [], revision: "rev-3", sequence: 3 }),
        );

        for (let i = 0; i < 2; i++) {
            try { await syncModule.catchUp(); } catch { /* the defect: a throw here must not lose a link */ }
        }

        const added = emitCounts("additions");
        assert.equal(added.get("a"), 1);
        assert.equal(added.get("c"), 1, "the entry after the unopenable one is applied");
        assert.equal(store.getSequence(), 3);
    });
});

// ---------------------------------------------------------------------------
// The startup render is retried until it succeeds (BUG-115)
// ---------------------------------------------------------------------------

describe("sync: a failed startup render is retried", () => {
    afterEach(() => mock.timers.reset());

    /** A transport whose `/render` answers come from `renders`, one per call
     *  (an Error is thrown, as the host's httpFetch does on its deadline), and
     *  whose `/sync` reports the head at `head` with no new diffs. */
    function renderServer(renders: Array<Error | object | Promise<TransportResponse>>, head: number) {
        const transport = new MockTransport();
        setup(transport);
        let renderCalls = 0;
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => {
                const next = renders[Math.min(renderCalls, renders.length - 1)];
                renderCalls++;
                if (next instanceof Error) throw next;
                if (next instanceof Promise) return next as unknown as TransportResponse;
                return jsonResponse(next);
            },
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => jsonResponse({ diffs: [], revision: `rev-${head}`, sequence: head }),
        );
        return { transport, renderCalls: () => renderCalls };
    }

    const timeout = () => new Error("httpFetch GET https://server.example/rooms/room-1/render timed out after 10000 ms");

    it("a render that fails then succeeds is retried by catch-up, and its links are emitted", async () => {
        mock.timers.enable({ apis: ["Date"], now: 1_000_000 });
        const early = makeLink({ source: "early" });
        const late = makeLink({ source: "late" });
        const server = renderServer([timeout(), { links: [early, late], revision: "rev-9", sequence: 9 }], 9);
        // The cursor already sits past the hole: catch-up alone never fetches `early`.
        syncModule.applyInboundWireDiff({ additions: [late], removals: [] }, 9, "rev-9");

        await assert.rejects(syncModule.bootstrap(), /timed out/);
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();
        assert.equal(server.renderCalls(), 1, "no retry before the backoff elapses");

        mock.timers.tick(syncModule.RENDER_RETRY_BASE_MS);
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();

        assert.equal(server.renderCalls(), 2);
        assert.equal(emitCounts("additions").get("early"), 1, "the history the cursor skipped is emitted");
        assert.equal(emitCounts("additions").get("late"), 1, "a link already held is not emitted again");
        assert.deepEqual(store.allLinks().links.map((l) => l.data.source).sort(), ["early", "late"]);
        assert.equal(store.getSequence(), 9);

        // Once a render succeeded it is never asked for again.
        mock.timers.tick(60 * 60 * 1000);
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();
        assert.equal(server.renderCalls(), 2);
    });

    it("a successful first render is not retried", async () => {
        mock.timers.enable({ apis: ["Date"], now: 1_000_000 });
        const server = renderServer([{ links: [makeLink({ source: "a" })], revision: "rev-1", sequence: 1 }], 1);
        syncModule.needRender();

        await syncModule.bootstrap();
        for (let i = 0; i < 3; i++) {
            mock.timers.tick(60 * 60 * 1000);
            await syncModule.catchUp();
            await syncModule._awaitRenderRetryForTests();
        }

        assert.equal(server.renderCalls(), 1);
        assert.equal(emitCounts("additions").get("a"), 1);
    });

    it("backs off: each failure doubles the wait before the next attempt", async () => {
        mock.timers.enable({ apis: ["Date"], now: 1_000_000 });
        const base = syncModule.RENDER_RETRY_BASE_MS;
        const server = renderServer([timeout(), timeout(), { links: [], revision: "rev-0", sequence: 0 }], 0);
        const attempt = async (ms: number) => {
            mock.timers.tick(ms);
            await syncModule.catchUp();
            await syncModule._awaitRenderRetryForTests();
            return server.renderCalls();
        };

        await assert.rejects(syncModule.bootstrap());
        assert.equal(await attempt(base), 2, "first retry after the base delay");
        assert.equal(await attempt(base), 2, "the second wait is twice as long");
        assert.equal(await attempt(base), 3);
        assert.equal(await attempt(10 * base), 3, "no attempt after a success");
    });

    it("a render that never ran at startup (authentication failed first) is run by catch-up", async () => {
        const a = makeLink({ source: "a" });
        const server = renderServer([{ links: [a], revision: "rev-1", sequence: 1 }], 1);

        syncModule.needRender();
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();

        assert.equal(server.renderCalls(), 1);
        assert.equal(emitCounts("additions").get("a"), 1);
    });

    it("a catch-up while the startup render is still out does not start a second one", async () => {
        let answerRender!: (res: TransportResponse) => void;
        const slowRender = new Promise<TransportResponse>((resolve) => { answerRender = resolve; });
        const server = renderServer([slowRender], 1);
        syncModule.needRender();

        const startup = syncModule.bootstrap();
        await new Promise((resolve) => setImmediate(resolve));
        await syncModule.catchUp();
        await new Promise((resolve) => setImmediate(resolve));
        assert.equal(server.renderCalls(), 1);

        answerRender(jsonResponse({ links: [makeLink({ source: "a" })], revision: "rev-1", sequence: 1 }));
        await startup;
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();
        assert.equal(server.renderCalls(), 1);
        assert.equal(emitCounts("additions").get("a"), 1);
    });

    it("catch-up does not wait for a retried render: new diffs are applied while it is in flight", async () => {
        mock.timers.enable({ apis: ["Date"], now: 1_000_000 });
        let answerRender!: (res: TransportResponse) => void;
        const slowRender = new Promise<TransportResponse>((resolve) => { answerRender = resolve; });
        const transport = new MockTransport();
        setup(transport);
        let renderCalls = 0;
        transport.route(
            (url, method) => method === "GET" && url.endsWith("/render"),
            () => {
                renderCalls++;
                if (renderCalls === 1) throw timeout();
                return slowRender as unknown as TransportResponse;
            },
        );
        const fresh = makeLink({ source: "fresh" });
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=0"),
            () => jsonResponse({ diffs: [{ additions: [fresh], removals: [], sequence: 1 }], revision: "rev-1", sequence: 1 }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => jsonResponse({ diffs: [], revision: "rev-1", sequence: 1 }),
        );

        await assert.rejects(syncModule.bootstrap());
        mock.timers.tick(syncModule.RENDER_RETRY_BASE_MS);
        const emitted = await syncModule.catchUp();
        await new Promise((resolve) => setImmediate(resolve));

        assert.equal(renderCalls, 2, "the retry is in flight");
        assert.deepEqual(emitted.additions, [fresh], "catch-up returned its diffs without waiting for the render");
        assert.equal(emitCounts("additions").get("fresh"), 1);

        // A second catch-up while the render is still out does not start another.
        mock.timers.tick(60 * 60 * 1000);
        await syncModule.catchUp();
        assert.equal(renderCalls, 2);

        const old = makeLink({ source: "old" });
        answerRender(jsonResponse({ links: [old, fresh], revision: "rev-1", sequence: 1 }));
        await syncModule._awaitRenderRetryForTests();
        assert.equal(emitCounts("additions").get("old"), 1);
        assert.equal(emitCounts("additions").get("fresh"), 1);
    });
});
