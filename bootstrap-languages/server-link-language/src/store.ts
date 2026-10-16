/**
 * Local link store — keeps the language's links in memory, indexed by
 * source / target / predicate, and persists them through the storage
 * adapter's KV API as ONE value per change, plus sync-cursor bookkeeping
 * (revision + sequence number) so incremental HTTP sync can resume after
 * restart.
 *
 * Key scheme (format 2):
 *   links-v2  → JSON object {link-hash: LinkExpression}, the whole link set
 *   revision  → last known server revision hash
 *   sequence  → last applied server sequence number
 *
 * Why one value: the executor's KV (rust-executor/src/js_core/host.js,
 * storagePut/storageDelete) rewrites its whole JSON file on every put and
 * delete. Format 1 wrote four keys per link (the link and three index
 * entries) plus a `hash` host call, so a commit of N links did ~4N full
 * rewrites of a file that grows with N: 16 s for 1,833 links. Now an
 * applied diff is one put, whatever its size.
 *
 * Format 1 (still read, once): `links/{hash}` → LinkExpression, plus
 * `links-by-source|target|pred/{value}/{hash}` → hash. The first load
 * without `links-v2` imports every `links/*` value and writes `links-v2`.
 * The format-1 keys are left in place: deleting them costs one full KV
 * rewrite each, which is the cost this format removes.
 *
 * Link hash: SHA-256 hex of the canonical payload, the same value the
 * wire carries as `link_hash` and the link server computes
 * (link-server/src/types.ts linkHash). Computed in JS, no host call.
 */

import { sha256 } from "@noble/hashes/sha2";
import { bytesToHex, utf8ToBytes } from "@noble/hashes/utils";

import { getStorage } from "./adapters.js";
import type { StorageAdapter } from "./adapters.js";
import type { LinkExpression, PerspectiveDiff, Perspective } from "./types.js";

function sha256Hex(data: string): string {
    return bytesToHex(sha256(utf8ToBytes(data)));
}

let _hashFn: (data: string) => string = sha256Hex;

/**
 * Resets the in-memory link set (it reloads from storage on next use).
 * `hashFn` overrides the link hash (tests); default SHA-256 hex.
 */
export function initStore(hashFn?: (data: string) => string): void {
    _hashFn = hashFn ?? sha256Hex;
    _cache = null;
}

// ---------------------------------------------------------------------------
// Keys
// ---------------------------------------------------------------------------

export const LINKS_KEY = "links-v2";
const LEGACY_LINK_PREFIX = "links/";
const REVISION_KEY = "revision";
const SEQUENCE_KEY = "sequence";

// ---------------------------------------------------------------------------
// In-memory link set
// ---------------------------------------------------------------------------

interface Entry {
    /** The link as stored (JSON); parsed fresh for every read, as format 1 did. */
    raw: string;
    source: string;
    target: string;
    predicate: string;
}

interface Cache {
    storage: StorageAdapter;
    links: Map<string, Entry>;
    bySource: Map<string, Set<string>>;
    byTarget: Map<string, Set<string>>;
    byPred: Map<string, Set<string>>;
}

let _cache: Cache | null = null;

function indexAdd(index: Map<string, Set<string>>, value: string, h: string): void {
    if (!value) return;
    let set = index.get(value);
    if (!set) index.set(value, (set = new Set()));
    set.add(h);
}

function indexRemove(index: Map<string, Set<string>>, value: string, h: string): void {
    if (!value) return;
    const set = index.get(value);
    if (!set) return;
    set.delete(h);
    if (set.size === 0) index.delete(value);
}

function insert(c: Cache, h: string, link: LinkExpression, raw: string): void {
    if (c.links.has(h)) drop(c, h);
    const entry: Entry = {
        raw,
        source: link.data?.source || "",
        target: link.data?.target || "",
        predicate: link.data?.predicate || "",
    };
    c.links.set(h, entry);
    indexAdd(c.bySource, entry.source, h);
    indexAdd(c.byTarget, entry.target, h);
    indexAdd(c.byPred, entry.predicate, h);
}

function drop(c: Cache, h: string): void {
    const entry = c.links.get(h);
    if (!entry) return;
    c.links.delete(h);
    indexRemove(c.bySource, entry.source, h);
    indexRemove(c.byTarget, entry.target, h);
    indexRemove(c.byPred, entry.predicate, h);
}

function emptyCache(storage: StorageAdapter): Cache {
    return { storage, links: new Map(), bySource: new Map(), byTarget: new Map(), byPred: new Map() };
}

/** The link set for the current storage adapter, loaded (or migrated) on first use. */
function cache(): Cache {
    const storage = getStorage();
    if (_cache && _cache.storage === storage) return _cache;
    const c = emptyCache(storage);
    const raw = storage.get(LINKS_KEY);
    if (raw !== null) {
        let parsed: Record<string, LinkExpression> | null = null;
        try {
            parsed = JSON.parse(raw);
        } catch (err) {
            // Unreadable link set: start empty and clear the cursor, so the
            // next sync fetches the room's links from the server again.
            console.warn(`[server-link-language] store: unreadable ${LINKS_KEY}, refetching: ${err}`);
            storage.delete(SEQUENCE_KEY);
            storage.delete(REVISION_KEY);
        }
        if (parsed && typeof parsed === "object") {
            for (const [h, link] of Object.entries(parsed)) insert(c, h, link, JSON.stringify(link));
        }
    } else {
        const legacy = storage.listKeys(LEGACY_LINK_PREFIX);
        for (const key of legacy) {
            const value = storage.get(key);
            if (!value) continue;
            const link = JSON.parse(value) as LinkExpression;
            insert(c, hashLink(link), link, value);
        }
        if (legacy.length > 0) persist(c);
    }
    _cache = c;
    return c;
}

function persist(c: Cache): void {
    const parts: string[] = [];
    for (const [h, entry] of c.links) parts.push(`${JSON.stringify(h)}:${entry.raw}`);
    c.storage.put(LINKS_KEY, `{${parts.join(",")}}`);
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export function hashLink(link: LinkExpression): string {
    // Normalize predicate to null when missing/undefined — the same
    // canonical payload as encryptLinkForWire's link_hash and the server's
    // canonicalLinkPayload (link-server/src/types.ts).
    const content = JSON.stringify({
        source: link.data.source,
        predicate: link.data.predicate ?? null,
        target: link.data.target,
        author: link.author,
        timestamp: link.timestamp,
    });
    return _hashFn(content);
}

export function putLink(link: LinkExpression): string {
    const c = cache();
    const h = hashLink(link);
    insert(c, h, link, JSON.stringify(link));
    persist(c);
    return h;
}

export function removeLink(link: LinkExpression): void {
    const c = cache();
    const h = hashLink(link);
    if (!c.links.has(h)) return;
    drop(c, h);
    persist(c);
}

/** Additions, then removals; one storage write for the whole diff (none if nothing changed). */
export function applyDiff(diff: PerspectiveDiff): void {
    const c = cache();
    let changed = false;
    for (const addition of diff.additions) {
        insert(c, hashLink(addition), addition, JSON.stringify(addition));
        changed = true;
    }
    for (const removal of diff.removals) {
        const h = hashLink(removal);
        if (!c.links.has(h)) continue;
        drop(c, h);
        changed = true;
    }
    if (changed) persist(c);
}

/** Replaces the whole link set with `links` in one storage write. */
export function replaceAll(links: LinkExpression[]): void {
    const c = cache();
    const next = emptyCache(c.storage);
    for (const link of links) insert(next, hashLink(link), link, JSON.stringify(link));
    c.links = next.links;
    c.bySource = next.bySource;
    c.byTarget = next.byTarget;
    c.byPred = next.byPred;
    persist(c);
}

// ---------------------------------------------------------------------------
// Query
// ---------------------------------------------------------------------------

export interface LinkQuery {
    source?: string;
    target?: string;
    predicate?: string;
}

export function queryLinks(query: LinkQuery): LinkExpression[] {
    const { source, target, predicate } = query;
    const c = cache();

    let candidates: Iterable<string>;
    if (source) candidates = c.bySource.get(source) ?? [];
    else if (target) candidates = c.byTarget.get(target) ?? [];
    else if (predicate) candidates = c.byPred.get(predicate) ?? [];
    else candidates = c.links.keys();

    const results: LinkExpression[] = [];
    for (const h of candidates) {
        const entry = c.links.get(h);
        if (!entry) continue;
        if (source && entry.source !== source) continue;
        if (target && entry.target !== target) continue;
        if (predicate && entry.predicate !== predicate) continue;
        results.push(JSON.parse(entry.raw) as LinkExpression);
    }
    return results;
}

export function allLinks(): Perspective {
    const links: LinkExpression[] = [];
    for (const entry of cache().links.values()) links.push(JSON.parse(entry.raw) as LinkExpression);
    return { links };
}

// ---------------------------------------------------------------------------
// Revision / sequence tracking (sync cursor)
// ---------------------------------------------------------------------------

export function getRevision(): string | null {
    return getStorage().get(REVISION_KEY);
}

export function setRevision(rev: string): void {
    if (!rev) return;
    getStorage().put(REVISION_KEY, rev);
}

/** Last server sequence number this instance has applied. 0 if never synced. */
export function getSequence(): number {
    const raw = getStorage().get(SEQUENCE_KEY);
    if (!raw) return 0;
    const n = parseInt(raw, 10);
    return Number.isFinite(n) ? n : 0;
}

export function setSequence(seq: number): void {
    if (!Number.isFinite(seq)) return;
    getStorage().put(SEQUENCE_KEY, String(seq));
}
