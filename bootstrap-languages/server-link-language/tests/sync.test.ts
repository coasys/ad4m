/**
 * Tests for src/sync.ts — most importantly applyInboundWireDiff, the
 * single choke point that MUST call emitDiff for every inbound diff
 * (see the "CRITICAL TRAP" note in src/sync.ts and the task brief: 7 of
 * 13 existing link languages got this wrong).
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";

import type { RoomConfig, StorageAdapter, Transport, TransportResponse } from "../src/adapters.js";
import { initAdapters, resetAdapters } from "../src/adapters.js";
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
    // Wipe any leftover batch state so a previous test's queued diff can't
    // flush against this test's transport.
    syncModule._resetBatchStateForTests();
    initAdapters({ storage: new MockStorage(), transport, config });
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
        roomKey = null;
        // Throw from emitSyncState — simulates a torn-down runtime.
        syncModule.initSync({
            config,
            getToken: async () => "test-token",
            emitDiff: (diff) => emittedDiffs.push(diff),
            emitSyncState: () => { throw new Error("runtime torn down"); },
            getRoomKey: () => roomKey,
        });

        // First flush: hard-fails all retries → tries to emit → emitter throws.
        const X: LinkExpression = {
            author: "did:key:zAuthor",
            timestamp: "2026-01-01T00:00:00.000Z",
            data: { source: "x", target: "t", predicate: "p" },
            proof: { signature: "sig-X", key: "k" },
        };
        syncModule.enqueueCommitBatched({ additions: [X], removals: [] });
        await syncModule.drainCommitBatch();
        assert.equal(posts.length, 3, "first flush must attempt all 3 retries");

        // Now the server recovers. The chain must still be live — the
        // next enqueue must actually POST.
        shouldFail = false;
        const Y: LinkExpression = {
            author: "did:key:zAuthor",
            timestamp: "2026-01-01T00:00:00.001Z",
            data: { source: "y", target: "t", predicate: "p" },
            proof: { signature: "sig-Y", key: "k" },
        };
        syncModule.enqueueCommitBatched({ additions: [Y], removals: [] });
        await syncModule.drainCommitBatch();
        assert.equal(posts.length, 4, "chain must still be live after the poisoning attempt");
        assert.deepEqual(posts[3].additions[0].data, Y.data);
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
