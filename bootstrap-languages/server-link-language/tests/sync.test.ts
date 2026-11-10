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
import { decryptLinkFromWire, encryptLinkForWire, generateRoomKey, wireLinkHash, type KeyRing } from "../src/encryption.js";
import type { LinkExpression, PerspectiveDiff, WireLinkExpression } from "../src/types.js";
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

/** The room key `setup()` gives the agent: every room is E2E, so a member holding its key is the normal state. */
const TEST_KEY = generateRoomKey();

/** `link` as a member commits it: encrypted with the room key. */
function wire(link: LinkExpression): WireLinkExpression {
    return encryptLinkForWire(link, TEST_KEY, 1);
}

function setup(transport: MockTransport): void {
    resetAdapters();
    syncModule._resetRenderStateForTests();
    initAdapters({ storage: new MockStorage(), transport, config });
    store.initStore(simpleHash);

    emittedDiffs = [];
    syncStates = [];
    keyRing = new Map([[1, TEST_KEY]]);

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
        store.setSequence(4);

        const result = syncModule.applyInboundWireDiff({ additions: [wire(link)], removals: [] }, 5, "rev-5");

        assert.deepEqual(store.allLinks().links, [link]);
        assert.equal(store.getSequence(), 5);
        assert.equal(store.getRevision(), "rev-5");
        assert.equal(emittedDiffs.length, 1);
        assert.deepEqual(emittedDiffs[0], { additions: [link], removals: [] });
        assert.deepEqual(result, { additions: [link], removals: [] });
    });

    it("a push ahead of the next sequence is applied but leaves the cursor, so the tail fetches the diffs in between", () => {
        const transport = new MockTransport();
        setup(transport);
        store.setSequence(4);
        store.setRevision("rev-4");
        const link = makeLink();

        syncModule.applyInboundWireDiff({ additions: [wire(link)], removals: [] }, 7, "rev-7");

        assert.deepEqual(store.allLinks().links, [link]);
        assert.equal(store.getSequence(), 4, "diffs 5 and 6 are still to come");
        assert.equal(store.getRevision(), "rev-4");
    });

    it("a push at or below the cursor is ignored, so it cannot re-add a link a later diff removed", () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink();
        // The tail applied 5 (add) and 6 (remove); the push of 5 arrives late.
        store.setSequence(6);
        store.setRevision("rev-6");

        // Below the cursor (a late push), and equal to it (the echo of our own commit).
        for (const sequence of [5, 6]) {
            const result = syncModule.applyInboundWireDiff({ additions: [wire(link)], removals: [] }, sequence, `rev-${sequence}`);
            assert.deepEqual(result, { additions: [], removals: [] });
        }

        assert.deepEqual(store.allLinks().links, []);
        assert.equal(store.getSequence(), 6);
        assert.equal(store.getRevision(), "rev-6");
    });

    it("applies removals and still emits", () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink();
        store.putLink(link);

        syncModule.applyInboundWireDiff({ additions: [], removals: [wire(link)] }, 1, "rev-1");

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
        keyRing = null; // a freshly joined member awaiting key grant
        const rk = generateRoomKey();
        const link = makeLink({ source: "encrypted-no-ring" });
        const wireLink = encryptLinkForWire(link, rk, 1);

        store.setSequence(4);
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

        store.setSequence(4);
        syncModule.applyInboundWireDiff({ additions: [wireLink], removals: [] }, 5, "rev-5");
        assert.equal(store.allLinks().links.length, 0, "undecryptable link must not appear in store");
        assert.equal(store.getSequence(), 5, "sequence must advance — the link is held, not lost");
        assert.deepEqual(syncModule.retryHeld(), { additions: [], removals: [] }, "version 1 does not open it");

        keyRing = new Map([[1, rk1], [2, rk2]]);
        assert.deepEqual(syncModule.retryHeld(), { additions: [link], removals: [] });
        assert.deepEqual(emittedDiffs, [{ additions: [link], removals: [] }]);
        assert.equal(syncModule.hasHeldLinks(), false);
    });

    it("never applies a plaintext link: every room this build reads is end-to-end encrypted", () => {
        const transport = new MockTransport();
        setup(transport);
        const rk = generateRoomKey();
        keyRing = new Map([[1, rk]]);
        const injected = makeLink({ source: "injected" }); // plaintext: what a server, not a member, could add
        const member = makeLink({ source: "from-a-member" });

        store.setSequence(4);
        syncModule.applyInboundWireDiff(
            { additions: [injected, encryptLinkForWire(member, rk, 1)], removals: [] }, 5, "rev-5",
        );

        assert.deepEqual(store.allLinks().links, [member], "only the encrypted link is applied");
        assert.deepEqual(emittedDiffs, [{ additions: [member], removals: [] }]);
        assert.equal(syncModule.hasHeldLinks(), false, "a plaintext link is not held for a key either");
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
    it("refuses to commit without a key ring, and posts nothing: there is no plaintext mode", async () => {
        const transport = new MockTransport();
        setup(transport);
        keyRing = null; // a member no one has granted the room key yet
        transport.route(
            (url, method) => method === "POST" && url.includes("/commit"),
            () => ({ status: 200, headers: {}, body: JSON.stringify({ sequence: 1, revision: "rev-1" }) }),
        );

        await assert.rejects(
            () => syncModule.commit({ additions: [makeLink()], removals: [makeLink({ source: "gone" })] }),
            /no room key/,
        );
        assert.deepEqual(transport.calls, [], "nothing reaches the server");
        assert.equal(store.allLinks().links.length, 0, "the store does not take a refused commit");
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
                        { additions: [wire(linkA)], removals: [], sequence: 1 },
                        { additions: [wire(linkB)], removals: [], sequence: 2 },
                    ],
                    revision: "rev-2",
                    sequence: 2,
                    next: null,
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
                        body: JSON.stringify({ diffs: [{ additions: [wire(linkA)], removals: [], sequence: 1 }], revision: "rev-1", sequence: 1, next: null }),
                    };
                }
                return { status: 200, headers: {}, body: JSON.stringify({ diffs: [], revision: "rev-1", sequence: 1, next: null }) };
            },
        );

        await syncModule.catchUp();
        await syncModule.catchUp();

        assert.deepEqual(sinceSeen, ["0", "1"]);
        assert.equal(emittedDiffs.length, 1);
    });

    it("refuses an answer without the paged fields instead of reading it as a page", async () => {
        const transport = new MockTransport();
        setup(transport);
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => jsonResponse({ diffs: [{ additions: [wire(makeLink())], removals: [] }], revision: "rev-1", sequence: 1 }),
        );

        await assert.rejects(() => syncModule.catchUp(), /without a paged body/);
        assert.equal(emittedDiffs.length, 0);
        assert.equal(store.getSequence(), 0);
    });

    it("re-reads the key ring on every catch-up while links are held, and emits them once a key opens them", async () => {
        // A member's first catch-up meets an encrypted diff before any member
        // has granted it the key. The cursor advances past the diff; the link
        // is held. A later catch-up's key ring read finds the grant.
        const transport = new MockTransport();
        setup(transport);
        keyRing = null;
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
                const diffs = since === "0" ? [{ additions: [wireLink], removals: [], sequence: 1 }] : [];
                return jsonResponse({ diffs, revision: "rev-1", sequence: 1, next: null });
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
            () => jsonResponse({ diffs: [{ additions: [wire(makeLink())], removals: [], sequence: 1 }], revision: "rev-1", sequence: 1, next: null }),
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

function identity(l: LinkExpression | WireLinkExpression): string {
    // Encrypted links carry their identity as link_hash, as the real server keys them.
    if ("link_hash" in l && l.link_hash) return l.link_hash;
    const link = l as LinkExpression;
    return JSON.stringify([link.author, link.timestamp, link.data.source, link.data.predicate ?? null, link.data.target]);
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
                getKeyRing: () => new Map([[1, TEST_KEY]]),
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
            (url, method) => method === "GET" && url.includes("/render"),
            () => jsonResponse({ links: [wire(linkA), wire(linkB)], revision: "rev-snap", sequence: 7, upto: 7, next: null }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=7"),
            () => jsonResponse({ diffs: [], revision: "rev-snap", sequence: 7, next: null }),
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
        keyRing = new Map([[1, TEST_KEY]]);

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
        const opens = wire(makeLink({ source: "opens" }));
        const encLink = encryptLinkForWire(makeLink({ source: "secret" }), rk, 3);

        transport.route(
            (url, method) => method === "GET" && url.includes("/render"),
            () => jsonResponse({ links: [opens, encLink], revision: "rev-1", sequence: 1, upto: 1, next: null }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => jsonResponse({ diffs: [], revision: "rev-1", sequence: 1, next: null }),
        );

        await syncModule.bootstrap();

        assert.equal(store.allLinks().links.length, 1, "only the link the key ring opens is stored");
        assert.equal(store.allLinks().links[0].data.source, "opens");

        // catchUp with empty diffs — should still attempt refresh because
        // bootstrap held the version 3 link.
        refreshCalled = false;
        await syncModule.catchUp();
        assert.equal(refreshCalled, true, "catchUp should retry key ring refresh for versions tracked by bootstrap");
    });

    it("a member with a cursor above 0 sends no /render: it tails /sync from its cursor", async () => {
        const transport = new MockTransport();
        setup(transport);
        const kept = makeLink({ source: "kept" });
        const added = makeLink({ source: "added" });
        store.putLink(kept);
        store.setSequence(4);

        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=4"),
            () => jsonResponse({ diffs: [{ additions: [wire(added)], removals: [wire(kept)], sequence: 5 }], revision: "rev-5", sequence: 5, next: null }),
        );

        await syncModule.bootstrap();

        assert.deepEqual(transport.calls.map((c) => new URL(c.url).pathname + new URL(c.url).search), [
            "/rooms/room-1/sync?since=4&limit=100",
        ]);
        assert.equal(store.getRenderProgress(), null, "no render is stored");
        assert.deepEqual(store.allLinks().links, [added]);
        assert.equal(store.getSequence(), 5);
        assert.deepEqual(emittedDiffs, [{ additions: [added], removals: [kept] }]);
        assert.ok(syncStates.includes("Synced"));
    });
});

// ---------------------------------------------------------------------------
// render / currentRevision
// ---------------------------------------------------------------------------

describe("sync: render / currentRevision", () => {
    it("render() returns the local store contents once they are the room's, else null (unknown)", () => {
        const transport = new MockTransport();
        setup(transport);
        const link = makeLink();
        store.putLink(link);
        assert.equal(syncModule.render(), null, "no revision from the server yet");
        store.setSequence(1);
        store.setRevision(roomRevision([link]));
        assert.equal(syncModule.render()?.links.length, 1);
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

/** The last (here: only) `/render` page of a snapshot at `sequence`. */
function renderPage(links: unknown[], sequence: number) {
    return { links, revision: `rev-${sequence}`, sequence, upto: sequence, next: null };
}

/** The last `/sync` page: `diffs` (each with its sequence) and the room's head at `sequence`. */
function syncPage(diffs: unknown[], sequence: number) {
    return { diffs, revision: `rev-${sequence}`, sequence, next: null };
}

describe("sync: no history is lost", () => {
    it("links that arrive before their key are emitted once the key is held, though /render fails", async () => {
        const transport = new MockTransport();
        setup(transport);
        const roomKey = generateRoomKey();
        const granted: KeyRing = new Map([[1, roomKey]]);
        // The member holds no key yet; the next key ring read finds the grant.
        keyRing = null;
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
            () => jsonResponse(syncPage([{ additions: [syncedLink], removals: [], sequence: 2 }], 2)),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=2"),
            () => jsonResponse(syncPage([], 2)),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/render"),
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
            (url, method) => method === "GET" && url.includes("/render"),
            () => jsonResponse(renderPage([a, b], 7)),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=7"),
            () => jsonResponse(syncPage([], 7)),
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
        keyRing = null;
        const roomKey = generateRoomKey();
        const a = encryptLinkForWire(makeLink({ source: "a" }), roomKey, 1);
        transport.route(
            (url, method) => method === "GET" && url.includes("/render"),
            () => jsonResponse(renderPage([a], 1)),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=1"),
            () => jsonResponse(syncPage([], 1)),
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

        syncModule.applyInboundWireDiff({ additions: [wire(link)], removals: [] }, 1, "rev-1");
        syncModule.applyInboundWireDiff({ additions: [wire(link)], removals: [] }, 1, "rev-1");

        assert.equal(emitCounts("additions").get("twice"), 1);
    });
});

describe("sync: a link no key opens costs that link only", () => {
    /** `link` encrypted with `key`, its ciphertext altered so the key it names
     *  does not open it (an AES-GCM tag failure). */
    function unopenable(link: LinkExpression, key: Uint8Array, version: number) {
        const wire = encryptLinkForWire(link, key, version);
        const data = wire.data as { ciphertext: string; nonce: string };
        const bytes = Buffer.from(data.ciphertext, "base64");
        bytes[0] ^= 0xff;
        return { ...wire, data: { ...data, ciphertext: bytes.toString("base64") } };
    }

    it("held links around one the key does not open are all emitted once the key is held", () => {
        const transport = new MockTransport();
        setup(transport);
        keyRing = null;
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
                    { additions: [encryptLinkForWire(makeLink({ source: "a" }), roomKey, 1)], removals: [], sequence: 1 },
                    { additions: [unopenable(makeLink({ source: "b" }), roomKey, 1)], removals: [], sequence: 2 },
                    { additions: [encryptLinkForWire(makeLink({ source: "c" }), roomKey, 1)], removals: [], sequence: 3 },
                ],
                revision: "rev-3",
                sequence: 3,
                next: null,
            }),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=3"),
            () => jsonResponse(syncPage([], 3)),
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
     *  whose `/sync` reports the head at `head` with no new diffs. A member
     *  set up here is fresh (cursor 0), the only one that renders. */
    function renderServer(renders: Array<Error | object | Promise<TransportResponse>>, head: number) {
        const transport = new MockTransport();
        setup(transport);
        let renderCalls = 0;
        transport.route(
            (url, method) => method === "GET" && url.includes("/render"),
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
            () => jsonResponse(syncPage([], head)),
        );
        return { transport, renderCalls: () => renderCalls };
    }

    const timeout = () => new Error("httpFetch GET https://server.example/rooms/room-1/render timed out after 10000 ms");

    it("a render that fails then succeeds is retried by catch-up, and its links are emitted", async () => {
        mock.timers.enable({ apis: ["Date"], now: 1_000_000 });
        const early = makeLink({ source: "early" });
        const late = makeLink({ source: "late" });
        const server = renderServer([timeout(), renderPage([wire(early), wire(late)], 9)], 9);

        await assert.rejects(syncModule.bootstrap(), /timed out/);
        // A push while the render is stored is left to the render: the cursor
        // stays at 0 and nothing is emitted yet.
        syncModule.applyInboundWireDiff({ additions: [wire(late)], removals: [] }, 9, "rev-9");
        assert.equal(emitCounts("additions").get("late"), undefined);
        assert.equal(store.getSequence(), 0);
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();
        assert.equal(server.renderCalls(), 1, "no retry before the backoff elapses");

        mock.timers.tick(syncModule.RENDER_RETRY_BASE_MS);
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();

        assert.equal(server.renderCalls(), 2);
        assert.equal(emitCounts("additions").get("early"), 1);
        assert.equal(emitCounts("additions").get("late"), 1, "the pushed link arrives once, with the snapshot");
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
        const server = renderServer([renderPage([wire(makeLink({ source: "a" }))], 1)], 1);
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
        const server = renderServer([timeout(), timeout(), renderPage([], 0)], 0);
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
        const server = renderServer([renderPage([wire(a)], 1)], 1);

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

        answerRender(jsonResponse(renderPage([wire(makeLink({ source: "a" }))], 1)));
        await startup;
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();
        assert.equal(server.renderCalls(), 1);
        assert.equal(emitCounts("additions").get("a"), 1);
    });

    it("catch-up does not wait for a retried render: it returns while the render is in flight, and a diff pushed meanwhile arrives with the render's tail", async () => {
        mock.timers.enable({ apis: ["Date"], now: 1_000_000 });
        let answerRender!: (res: TransportResponse) => void;
        const slowRender = new Promise<TransportResponse>((resolve) => { answerRender = resolve; });
        const transport = new MockTransport();
        setup(transport);
        let renderCalls = 0;
        transport.route(
            (url, method) => method === "GET" && url.includes("/render"),
            () => {
                renderCalls++;
                if (renderCalls === 1) throw timeout();
                return slowRender as unknown as TransportResponse;
            },
        );
        const fresh = makeLink({ source: "fresh" });
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync?since=1"),
            () => jsonResponse(syncPage([{ additions: [wire(fresh)], removals: [], sequence: 2 }], 2)),
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => jsonResponse(syncPage([], 2)),
        );
        const syncGets = () => transport.calls.filter((c) => c.url.includes("/sync?")).map((c) => new URL(c.url).search);

        await assert.rejects(syncModule.bootstrap());
        mock.timers.tick(syncModule.RENDER_RETRY_BASE_MS);
        const emitted = await syncModule.catchUp();
        await new Promise((resolve) => setImmediate(resolve));

        assert.equal(renderCalls, 2, "the retry is in flight");
        assert.deepEqual(emitted, { additions: [], removals: [] }, "catch-up returned without waiting for the render");

        // A diff the WebSocket pushes while the render is out waits for the
        // render: it is after the snapshot, so the tail from upto brings it.
        syncModule.applyInboundWireDiff({ additions: [wire(fresh)], removals: [] }, 2, "rev-2");
        assert.equal(emitCounts("additions").get("fresh"), undefined);

        // A second catch-up while the render is still out does not start another.
        mock.timers.tick(60 * 60 * 1000);
        await syncModule.catchUp();
        assert.equal(renderCalls, 2);
        assert.deepEqual(syncGets(), [], "no /sync from cursor 0 while the render is pending");

        // The snapshot is pinned at 1, before the pushed diff; the tail from 1 brings `fresh`.
        const old = makeLink({ source: "old" });
        answerRender(jsonResponse(renderPage([wire(old)], 1)));
        await syncModule._awaitRenderRetryForTests();
        assert.equal(emitCounts("additions").get("old"), 1);
        assert.equal(emitCounts("additions").get("fresh"), 1);
        assert.deepEqual(store.allLinks().links.map((l) => l.data.source).sort(), ["fresh", "old"]);
        assert.deepEqual(syncGets(), ["?since=1&limit=100"], "the render's own completion tails from its upto");
        assert.equal(store.getSequence(), 2);
    });

    it("a failed render page is not re-requested by every catch-up before its backoff elapses", async () => {
        mock.timers.enable({ apis: ["Date"], now: 1_000_000 });
        const server = renderServer([timeout(), renderPage([], 0)], 0);

        await assert.rejects(syncModule.bootstrap());
        for (let i = 0; i < 3; i++) {
            mock.timers.tick(3_000);
            await syncModule.catchUp();
            await syncModule._awaitRenderRetryForTests();
        }
        assert.equal(server.renderCalls(), 1, "three 3 s syncs inside the 10 s backoff send no render");
        assert.notEqual(store.getRenderProgress(), null, "the failed render stays stored, to be resumed");

        mock.timers.tick(syncModule.RENDER_RETRY_BASE_MS);
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();
        assert.equal(server.renderCalls(), 2);
        assert.equal(store.getRenderProgress(), null);
    });
});

// ---------------------------------------------------------------------------
// An incomplete mirror is not the server's state, and heals
// ---------------------------------------------------------------------------

/** The link server's room revision over `links`: the XOR of their link hashes. */
function roomRevision(links: LinkExpression[]): string {
    const acc = new Uint8Array(32);
    for (const link of links) {
        const h = wireLinkHash(link);
        for (let i = 0; i < 32; i++) acc[i] ^= parseInt(h.slice(i * 2, i * 2 + 2), 16);
    }
    return [...acc].map((b) => b.toString(16).padStart(2, "0")).join("");
}

describe("sync: an incomplete mirror is not presented as the server's state", () => {
    afterEach(() => syncModule._setPageLimitsForTests());

    const room = [1, 2, 3, 4, 5].map((i) => makeLink({ source: `room-${i}` }));

    /** A link server holding `links` at `head`: paged `/render` (pages of
     *  `pageSize`), `/sync` with nothing after `head`. */
    function roomServer(links: WireLinkExpression[], head: number, revision: string, pageSize = 2) {
        const transport = new MockTransport();
        setup(transport);
        syncModule._setPageLimitsForTests(pageSize);
        transport.route(
            (url, method) => method === "GET" && url.includes("/render"),
            (url) => {
                const from = Number(new URL(url).searchParams.get("next") ?? "0");
                const to = from + pageSize;
                return jsonResponse({
                    links: links.slice(from, to),
                    revision,
                    sequence: head,
                    upto: head,
                    next: to < links.length ? String(to) : null,
                });
            },
        );
        transport.route(
            (url, method) => method === "GET" && url.includes("/sync"),
            () => jsonResponse({ diffs: [], revision, sequence: head, next: null }),
        );
        return transport;
    }

    const renders = (transport: MockTransport) =>
        transport.calls.filter((c) => new URL(c.url).pathname.endsWith("/render")).length;

    it("render() answers the mirror when it hashes to the room's revision at the cursor", () => {
        setup(new MockTransport());
        for (const link of room) store.putLink(link);
        store.setSequence(3745);
        store.setRevision(roomRevision(room));

        assert.equal(syncModule.render()?.links.length, 5);
    });

    it("render() is unknown (null) while a render is stored, first page or half done", () => {
        setup(new MockTransport());
        store.putLink(room[0]);
        store.setRevision(roomRevision([room[0]]));
        store.setRenderProgress({ upto: null, next: null, page: 0 });
        assert.equal(syncModule.render(), null, "a render stored, no page yet");

        store.putLink(room[1]);
        store.setRenderProgress({ upto: 9, next: "2", page: 1 });
        assert.equal(syncModule.render(), null, "a render half done");
    });

    it("render() is unknown (null) when the mirror holds fewer links than the room's revision covers", () => {
        // The staging case: cursor 3745, the server holds the whole room,
        // the mirror only a part of it.
        setup(new MockTransport());
        store.putLink(room[0]);
        store.putLink(room[1]);
        store.setSequence(3745);
        store.setRevision(roomRevision(room));

        assert.equal(syncModule.render(), null);
    });

    it("render() counts a held link as the room's: a member waiting for a key is not short", () => {
        setup(new MockTransport());
        const otherKey = generateRoomKey();
        const waiting = makeLink({ source: "waiting" });
        store.putLink(room[0]);
        syncModule.applyInboundWireDiff({ additions: [encryptLinkForWire(waiting, otherKey, 2)], removals: [] }, 1, "");
        store.setSequence(1);
        store.setRevision(roomRevision([room[0], waiting]));

        assert.equal(syncModule.hasHeldLinks(), true);
        assert.deepEqual(syncModule.render()?.links, [room[0]]);
    });

    it("a member with a cursor whose mirror is short re-renders, paged, and then answers the whole room", async () => {
        const revision = roomRevision(room);
        const transport = roomServer(room.map(wire), 3745, revision);
        store.putLink(room[0]);
        store.putLink(room[3]);
        store.setSequence(3745);
        store.setRevision(revision);

        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();

        assert.equal(renders(transport), 3, "five links in pages of two");
        assert.equal(store.getRenderProgress(), null, "the heal render finished");
        assert.equal(store.getSequence(), 3745);
        assert.deepEqual(
            store.allLinks().links.map((l) => l.data.source).sort(),
            room.map((l) => l.data.source).sort(),
        );
        assert.deepEqual(
            emitCounts("additions"),
            new Map([["room-2", 1], ["room-3", 1], ["room-5", 1]]),
            "only the links the mirror lacked reach the executor",
        );
        assert.equal(syncModule.render()?.links.length, 5);

        // Healed: later catch-ups only tail.
        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();
        assert.equal(renders(transport), 3);
    });

    it("a complete mirror does not re-render", async () => {
        const revision = roomRevision(room);
        const transport = roomServer(room.map(wire), 3745, revision);
        for (const link of room) store.putLink(link);
        store.setSequence(3745);
        store.setRevision(revision);

        await syncModule.catchUp();
        await syncModule._awaitRenderRetryForTests();

        assert.equal(renders(transport), 0);
        assert.equal(syncModule.render()?.links.length, 5);
    });

    it("heals at most once per start: a mismatch the render does not fix stays unknown, without a render loop", async () => {
        // The server counts a link no member can open: no render fixes the mirror.
        const unopenable = { ...wire(makeLink({ source: "spoiled" })), data: { ciphertext: "AAAA", nonce: "AAAA" } };
        const revision = roomRevision([...room, makeLink({ source: "spoiled" })]);
        const transport = roomServer([...room.map(wire), unopenable], 3745, revision);
        store.putLink(room[0]);
        store.setSequence(3745);
        store.setRevision(revision);

        for (let i = 0; i < 3; i++) {
            await syncModule.catchUp();
            await syncModule._awaitRenderRetryForTests();
        }

        assert.equal(renders(transport), 3, "one heal render (three pages), not one per catch-up");
        assert.equal(store.getRenderProgress(), null);
        assert.equal(store.allLinks().links.length, 5);
        assert.equal(syncModule.render(), null, "still short of the room's revision: unknown");
    });
});
