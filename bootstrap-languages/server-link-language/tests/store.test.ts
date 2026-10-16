/**
 * Tests for the local link store module, including the sync-cursor
 * (revision + sequence) bookkeeping this language adds on top of the
 * template's store.
 */

import { describe, it, beforeEach } from "node:test";
import assert from "node:assert/strict";

import type { StorageAdapter } from "../src/adapters.js";
import { initAdapters } from "../src/adapters.js";
import * as store from "../src/store.js";
import type { LinkExpression } from "../src/types.js";
import { encryptLinkForWire, generateRoomKey } from "../src/encryption.js";

// ---------------------------------------------------------------------------
// Mock adapters
// ---------------------------------------------------------------------------

class MockStorage implements StorageAdapter {
    private data = new Map<string, string>();
    get(key: string): string | null { return this.data.get(key) ?? null; }
    put(key: string, value: string): void { this.data.set(key, value); }
    delete(key: string): void { this.data.delete(key); }
    listKeys(prefix?: string): string[] {
        return [...this.data.keys()].filter(k => !prefix || k.startsWith(prefix));
    }
}

function simpleHash(data: string): string {
    let h = 0;
    for (let i = 0; i < data.length; i++) {
        h = ((h << 5) - h + data.charCodeAt(i)) | 0;
    }
    return `Qm${Math.abs(h).toString(16)}`;
}

function setup(): void {
    initAdapters({ storage: new MockStorage() });
    store.initStore(simpleHash);
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

function makeLink(overrides?: Partial<LinkExpression["data"]>): LinkExpression {
    return {
        author: "did:key:z6MkStore",
        timestamp: "2026-01-01T00:00:00.000Z",
        data: {
            source: "channel://main",
            target: "expr://msg-001",
            predicate: "flux://has_message",
            ...overrides,
        },
        proof: { signature: "sig", key: "key" },
    };
}

// ---------------------------------------------------------------------------
// putLink
// ---------------------------------------------------------------------------

describe("store: putLink", () => {
    beforeEach(setup);

    it("stores a link retrievable via queryLinks", () => {
        const link = makeLink();
        const hash = store.putLink(link);
        assert.ok(hash);
        const results = store.queryLinks({ source: link.data.source, target: link.data.target });
        assert.equal(results.length, 1);
        assert.deepEqual(results[0].data, link.data);
    });

    it("is idempotent (same link stored twice → same hash, single entry)", () => {
        const link = makeLink();
        const h1 = store.putLink(link);
        const h2 = store.putLink(link);
        assert.equal(h1, h2);
        assert.equal(store.allLinks().links.length, 1);
    });
});

// ---------------------------------------------------------------------------
// removeLink
// ---------------------------------------------------------------------------

describe("store: removeLink", () => {
    beforeEach(setup);

    it("removes a previously stored link", () => {
        const link = makeLink();
        store.putLink(link);
        assert.equal(store.allLinks().links.length, 1);
        store.removeLink(link);
        assert.equal(store.allLinks().links.length, 0);
    });

    it("is a no-op for links that don't exist", () => {
        const link = makeLink({ target: "nonexistent://x" });
        store.removeLink(link);
    });
});

// ---------------------------------------------------------------------------
// queryLinks
// ---------------------------------------------------------------------------

describe("store: queryLinks", () => {
    beforeEach(setup);

    it("queries by source", () => {
        store.putLink(makeLink({ source: "a", target: "x", predicate: "p" }));
        store.putLink(makeLink({ source: "a", target: "y", predicate: "q" }));
        store.putLink(makeLink({ source: "b", target: "z", predicate: "r" }));

        assert.equal(store.queryLinks({ source: "a" }).length, 2);
        assert.equal(store.queryLinks({ source: "b" }).length, 1);
        assert.equal(store.queryLinks({ source: "c" }).length, 0);
    });

    it("queries by target", () => {
        store.putLink(makeLink({ source: "s", target: "T1", predicate: "p" }));
        store.putLink(makeLink({ source: "s", target: "T2", predicate: "p" }));

        assert.equal(store.queryLinks({ target: "T1" }).length, 1);
        assert.equal(store.queryLinks({ target: "T2" }).length, 1);
    });

    it("queries by predicate", () => {
        store.putLink(makeLink({ predicate: "pred://A" }));
        store.putLink(makeLink({ predicate: "pred://B" }));

        assert.equal(store.queryLinks({ predicate: "pred://A" }).length, 1);
    });

    it("intersection: source + predicate", () => {
        store.putLink(makeLink({ source: "s", target: "a", predicate: "p1" }));
        store.putLink(makeLink({ source: "s", target: "b", predicate: "p2" }));

        const results = store.queryLinks({ source: "s", predicate: "p1" });
        assert.equal(results.length, 1);
        assert.equal(results[0].data.target, "a");
    });

    it("returns all links when no filter params given", () => {
        store.putLink(makeLink({ source: "a", target: "b", predicate: "c" }));
        store.putLink(makeLink({ source: "x", target: "y", predicate: "z" }));
        const all = store.queryLinks({});
        assert.equal(all.length, 2);
    });
});

// ---------------------------------------------------------------------------
// allLinks
// ---------------------------------------------------------------------------

describe("store: allLinks", () => {
    beforeEach(setup);

    it("returns empty when no links stored", () => {
        assert.equal(store.allLinks().links.length, 0);
    });

    it("returns all stored links", () => {
        store.putLink(makeLink({ source: "a", target: "b", predicate: "c" }));
        store.putLink(makeLink({ source: "d", target: "e", predicate: "f" }));
        store.putLink(makeLink({ source: "g", target: "h", predicate: "i" }));
        assert.equal(store.allLinks().links.length, 3);
    });
});

// ---------------------------------------------------------------------------
// hashLink
// ---------------------------------------------------------------------------

describe("store: hashLink", () => {
    beforeEach(setup);

    it("produces deterministic hashes", () => {
        const link = makeLink();
        assert.equal(store.hashLink(link), store.hashLink(link));
    });

    it("produces different hashes for different links", () => {
        const l1 = makeLink({ source: "a" });
        const l2 = makeLink({ source: "b" });
        assert.notEqual(store.hashLink(l1), store.hashLink(l2));
    });
});

// ---------------------------------------------------------------------------
// applyDiff
// ---------------------------------------------------------------------------

describe("store: applyDiff", () => {
    beforeEach(setup);

    it("applies additions", () => {
        const link = makeLink();
        store.applyDiff({ additions: [link], removals: [] });
        assert.equal(store.allLinks().links.length, 1);
    });

    it("applies removals", () => {
        const link = makeLink();
        store.putLink(link);
        store.applyDiff({ additions: [], removals: [link] });
        assert.equal(store.allLinks().links.length, 0);
    });

    it("applies additions and removals together", () => {
        const old = makeLink({ source: "old" });
        const fresh = makeLink({ source: "new" });
        store.putLink(old);

        store.applyDiff({ additions: [fresh], removals: [old] });
        assert.equal(store.allLinks().links.length, 1);
        assert.equal(store.allLinks().links[0].data.source, "new");
    });
});

// ---------------------------------------------------------------------------
// revision tracking
// ---------------------------------------------------------------------------

describe("store: revision tracking", () => {
    beforeEach(setup);

    it("returns null initially", () => {
        assert.equal(store.getRevision(), null);
    });

    it("stores and retrieves revision", () => {
        store.setRevision("rev-42");
        assert.equal(store.getRevision(), "rev-42");
    });

    it("overwrites previous revision", () => {
        store.setRevision("rev-1");
        store.setRevision("rev-2");
        assert.equal(store.getRevision(), "rev-2");
    });

    it("ignores an empty-string revision (keeps the previous value)", () => {
        store.setRevision("rev-1");
        store.setRevision("");
        assert.equal(store.getRevision(), "rev-1");
    });
});

// ---------------------------------------------------------------------------
// sequence tracking (sync cursor)
// ---------------------------------------------------------------------------

describe("store: sequence tracking", () => {
    beforeEach(setup);

    it("returns 0 initially", () => {
        assert.equal(store.getSequence(), 0);
    });

    it("stores and retrieves a sequence number", () => {
        store.setSequence(42);
        assert.equal(store.getSequence(), 42);
    });

    it("overwrites the previous sequence number, including with 0", () => {
        store.setSequence(10);
        store.setSequence(0);
        assert.equal(store.getSequence(), 0);
    });

    it("survives round-tripping through storage as a string", () => {
        store.setSequence(123456);
        assert.equal(store.getSequence(), 123456);
        assert.equal(typeof store.getSequence(), "number");
    });
});

// ---------------------------------------------------------------------------
// persistence (format 2) and migration from format 1
// ---------------------------------------------------------------------------

describe("store: persistence", () => {
    it("a new instance over the same storage sees the links, and queries them", () => {
        const storage = new MockStorage();
        initAdapters({ storage });
        store.initStore(simpleHash);
        const a = makeLink({ target: "expr://a" });
        const b = makeLink({ source: "channel://other", target: "expr://b" });
        store.applyDiff({ additions: [a, b], removals: [] });

        store.initStore(simpleHash);

        assert.equal(store.allLinks().links.length, 2);
        assert.deepEqual(store.queryLinks({ source: "channel://other" }), [b]);
        assert.deepEqual(store.queryLinks({ target: "expr://a" }), [a]);
    });

    it("one diff of many links is one storage write", () => {
        let puts = 0;
        const storage = new MockStorage();
        const put = storage.put.bind(storage);
        storage.put = (k, v) => { puts++; put(k, v); };
        initAdapters({ storage });
        store.initStore(simpleHash);
        const links = Array.from({ length: 500 }, (_, i) => makeLink({ target: `expr://${i}` }));

        store.applyDiff({ additions: links, removals: links.slice(0, 100) });

        assert.equal(puts, 1);
        assert.equal(store.allLinks().links.length, 400);
    });

    it("reads a format-1 store (one key per link plus index keys) and keeps its links", () => {
        const storage = new MockStorage();
        const a = makeLink({ target: "expr://a" });
        const b = makeLink({ predicate: "flux://other", target: "expr://b" });
        for (const [h, link] of [["QmOldA", a], ["QmOldB", b]] as const) {
            storage.put(`links/${h}`, JSON.stringify(link));
            storage.put(`links-by-source/${link.data.source}/${h}`, h);
            storage.put(`links-by-target/${link.data.target}/${h}`, h);
            storage.put(`links-by-pred/${link.data.predicate}/${h}`, h);
        }
        storage.put("sequence", "7");
        initAdapters({ storage });
        store.initStore(simpleHash);

        assert.equal(store.allLinks().links.length, 2);
        assert.deepEqual(store.queryLinks({ predicate: "flux://other" }), [b]);
        assert.equal(store.getSequence(), 7);
        assert.ok(storage.get(store.LINKS_KEY), "migrated to format 2");

        store.removeLink(a);
        store.initStore(simpleHash);
        assert.deepEqual(store.allLinks().links, [b], "format-1 keys are not read again after the migration");
    });

    it("an unreadable link set starts empty and clears the sync cursor, so the next sync refetches", () => {
        const storage = new MockStorage();
        storage.put(store.LINKS_KEY, "{not json");
        storage.put("sequence", "9");
        storage.put("revision", "rev-9");
        initAdapters({ storage });
        store.initStore(simpleHash);

        assert.equal(store.allLinks().links.length, 0);
        assert.equal(store.getSequence(), 0);
        assert.equal(store.getRevision(), null);
    });

    it("hashes a link by default as the wire link_hash (SHA-256 of the canonical payload)", () => {
        initAdapters({ storage: new MockStorage() });
        store.initStore();
        const link = makeLink();

        assert.equal(store.putLink(link), encryptLinkForWire(link, generateRoomKey()).link_hash);
    });
});
