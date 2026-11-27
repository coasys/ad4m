/**
 * Local link store — wraps the storage adapter KV API to maintain
 * a link store with indexes, plus sync-cursor bookkeeping (revision +
 * sequence number) so incremental HTTP sync can resume after restart.
 *
 * Key scheme:
 *   links/{link-hash}                → serialized LinkExpression
 *   links-by-source/{source}/{hash}  → link-hash
 *   links-by-target/{target}/{hash}  → link-hash
 *   links-by-pred/{predicate}/{hash} → link-hash
 *   links-xor                        → XOR of the `wireLinkHash` of every
 *                                      stored link, hex (see linksRevision)
 *   revision                         → last known server revision hash
 *   sequence                         → last applied server sequence number
 *   held/{link_hash}                 → {op, link}: an inbound wire link this
 *                                      agent holds no key for yet (see sync.ts)
 *   render/progress                  → {upto, next, page}: a fresh member's
 *                                      paged /render in progress, so a restart
 *                                      resumes it
 */

import { getStorage, type StorageAdapter } from "./adapters.js";
import { bytesToHex, hexToBytes, wireLinkHash } from "./encryption.js";
import type { LinkExpression, PerspectiveDiff, Perspective, WireLinkExpression } from "./types.js";

let _hashFn: ((data: string) => string) | null = null;

export function initStore(hashFn?: (data: string) => string): void {
    _hashFn = hashFn ?? null;
}

function getHashFn(): (data: string) => string {
    if (!_hashFn) {
        throw new Error(
            "Store not initialized with a hash function. Call initStore(hashFn) during language init().",
        );
    }
    return _hashFn;
}

// ---------------------------------------------------------------------------
// Key helpers
// ---------------------------------------------------------------------------

function linkKey(linkHash: string): string {
    return `links/${linkHash}`;
}

function sourceIndexKey(source: string, linkHash: string): string {
    return `links-by-source/${source}/${linkHash}`;
}

function targetIndexKey(target: string, linkHash: string): string {
    return `links-by-target/${target}/${linkHash}`;
}

function predIndexKey(predicate: string, linkHash: string): string {
    return `links-by-pred/${predicate}/${linkHash}`;
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export function hashLink(link: LinkExpression): string {
    // Normalize predicate to null when missing/undefined — must match the
    // server's canonicalLinkPayload (link-server/src/types.ts) exactly so
    // client-side hashes equal server-side hashes for the same link.
    const content = JSON.stringify({
        source: link.data.source,
        predicate: link.data.predicate ?? null,
        target: link.data.target,
        author: link.author,
        timestamp: link.timestamp,
    });
    return getHashFn()(content);
}

export function putLink(link: LinkExpression): string {
    const h = hashLink(link);
    const storage = getStorage();
    if (storage.get(linkKey(h)) === null) addToRevision(storage, link);
    storage.put(linkKey(h), JSON.stringify(link));

    const source = link.data.source || "";
    const target = link.data.target || "";
    const predicate = link.data.predicate || "";

    if (source) storage.put(sourceIndexKey(source, h), h);
    if (target) storage.put(targetIndexKey(target, h), h);
    if (predicate) storage.put(predIndexKey(predicate, h), h);

    return h;
}

export function removeLink(link: LinkExpression): void {
    const h = hashLink(link);
    const storage = getStorage();
    if (storage.get(linkKey(h)) !== null) removeFromRevision(storage, link);
    storage.delete(linkKey(h));

    const source = link.data.source || "";
    const target = link.data.target || "";
    const predicate = link.data.predicate || "";

    if (source) storage.delete(sourceIndexKey(source, h));
    if (target) storage.delete(targetIndexKey(target, h));
    if (predicate) storage.delete(predIndexKey(predicate, h));
}

function getLink(linkHash: string): LinkExpression | null {
    const raw = getStorage().get(linkKey(linkHash));
    if (!raw) return null;
    return JSON.parse(raw) as LinkExpression;
}

export function hasLink(link: LinkExpression): boolean {
    return getStorage().get(linkKey(hashLink(link))) !== null;
}

export function applyDiff(diff: PerspectiveDiff): void {
    for (const addition of diff.additions) {
        putLink(addition);
    }
    for (const removal of diff.removals) {
        removeLink(removal);
    }
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
    const storage = getStorage();

    let candidateHashes: string[];

    if (source) {
        const keys = storage.listKeys(`links-by-source/${source}/`);
        candidateHashes = keys.map((k: string) => storage.get(k) || "").filter(Boolean);
    } else if (target) {
        const keys = storage.listKeys(`links-by-target/${target}/`);
        candidateHashes = keys.map((k: string) => storage.get(k) || "").filter(Boolean);
    } else if (predicate) {
        const keys = storage.listKeys(`links-by-pred/${predicate}/`);
        candidateHashes = keys.map((k: string) => storage.get(k) || "").filter(Boolean);
    } else {
        const keys = storage.listKeys("links/");
        candidateHashes = keys.map((k: string) => k.replace("links/", ""));
    }

    const results: LinkExpression[] = [];
    const seen = new Set<string>();

    for (const h of candidateHashes) {
        if (seen.has(h)) continue;
        seen.add(h);

        const link = getLink(h);
        if (!link) continue;

        if (source && link.data.source !== source) continue;
        if (target && link.data.target !== target) continue;
        if (predicate && link.data.predicate !== predicate) continue;

        results.push(link);
    }

    return results;
}

export function allLinks(): Perspective {
    const keys = getStorage().listKeys("links/");
    const links: LinkExpression[] = [];

    for (const key of keys) {
        const raw = getStorage().get(key);
        if (raw) {
            links.push(JSON.parse(raw) as LinkExpression);
        }
    }

    return { links };
}

// ---------------------------------------------------------------------------
// Revision / sequence tracking (sync cursor)
// ---------------------------------------------------------------------------

const REVISION_KEY = "revision";
const SEQUENCE_KEY = "sequence";

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

// ---------------------------------------------------------------------------
// Held wire links: inbound, not decryptable yet
// ---------------------------------------------------------------------------

export type HeldOp = "add" | "remove";

export interface HeldLink {
    id: string;
    op: HeldOp;
    link: WireLinkExpression;
}

const HELD_PREFIX = "held/";

/** Keeps `link` until a key opens it. A later op on the same link replaces it. */
export function holdLink(id: string, op: HeldOp, link: WireLinkExpression): void {
    const storage = getStorage();
    storage.put(HELD_PREFIX + id, JSON.stringify({ op, link }));
    revisionCache(storage).held?.set(id, { op, hash: link.link_hash });
}

export function releaseHeld(id: string): void {
    const storage = getStorage();
    storage.delete(HELD_PREFIX + id);
    revisionCache(storage).held?.delete(id);
}

export function heldLinks(): HeldLink[] {
    const storage = getStorage();
    const held: HeldLink[] = [];
    for (const key of storage.listKeys(HELD_PREFIX)) {
        const raw = storage.get(key);
        if (!raw) continue;
        const { op, link } = JSON.parse(raw) as { op: HeldOp; link: WireLinkExpression };
        held.push({ id: key.slice(HELD_PREFIX.length), op, link });
    }
    return held;
}

export function clearHeld(): void {
    const storage = getStorage();
    for (const key of storage.listKeys(HELD_PREFIX)) storage.delete(key);
    revisionCache(storage).held?.clear();
}

// ---------------------------------------------------------------------------
// The store's revision (see sync.ts, storeIsRoom)
// ---------------------------------------------------------------------------

const XOR_KEY = "links-xor";

/** What `linksRevision` keeps in memory, per storage: the held ops (read
 *  once), and the wire hashes of the stored links, only while links are held. */
interface RevisionCache {
    held: Map<string, { op: HeldOp; hash: string | undefined }> | null;
    stored: Set<string> | null;
}

const _revisionCaches = new WeakMap<StorageAdapter, RevisionCache>();

function revisionCache(storage: StorageAdapter): RevisionCache {
    let cache = _revisionCaches.get(storage);
    if (!cache) {
        cache = { held: null, stored: null };
        _revisionCaches.set(storage, cache);
    }
    return cache;
}

function xorInto(acc: Uint8Array, hex: string): void {
    const bytes = hexToBytes(hex);
    for (let i = 0; i < 32; i++) acc[i] ^= bytes[i];
}

/** The stored XOR. A store written before it existed computes it once from
 *  `allLinks()` and keeps it. */
function linksXor(storage: StorageAdapter): Uint8Array {
    const raw = storage.get(XOR_KEY);
    if (raw !== null) return hexToBytes(raw);
    const acc = new Uint8Array(32);
    for (const link of allLinks().links) xorInto(acc, wireLinkHash(link));
    storage.put(XOR_KEY, bytesToHex(acc));
    return acc;
}

/** Called by putLink for a link the store did not hold. */
function addToRevision(storage: StorageAdapter, link: LinkExpression): void {
    const h = wireLinkHash(link);
    const acc = linksXor(storage);
    xorInto(acc, h);
    storage.put(XOR_KEY, bytesToHex(acc));
    revisionCache(storage).stored?.add(h);
}

/** Called by removeLink for a link the store held. */
function removeFromRevision(storage: StorageAdapter, link: LinkExpression): void {
    const h = wireLinkHash(link);
    const acc = linksXor(storage);
    xorInto(acc, h);
    storage.put(XOR_KEY, bytesToHex(acc));
    revisionCache(storage).stored?.delete(h);
}

/**
 * The revision the room has if it holds what this store holds: the XOR of the
 * `wireLinkHash` of the stored links, with each held op applied (a held add is
 * the room's, a held removal is not). The link server keeps its revision the
 * same way, over the `link_hash` each member computes with `wireLinkHash`.
 * Null when a held link carries no `link_hash`.
 *
 * Reads no key list: the XOR is kept by putLink and removeLink, the held ops
 * are read once per storage, and the stored hashes are listed once, only
 * while links are held.
 */
export function linksRevision(): string | null {
    const storage = getStorage();
    const acc = linksXor(storage);
    const cache = revisionCache(storage);
    if (!cache.held) {
        cache.held = new Map(heldLinks().map((h) => [h.id, { op: h.op, hash: h.link.link_hash }]));
    }
    if (cache.held.size === 0) {
        cache.stored = null;
        return bytesToHex(acc);
    }
    for (const { hash } of cache.held.values()) if (!hash) return null;
    cache.stored ??= new Set(allLinks().links.map(wireLinkHash));
    for (const { op, hash } of cache.held.values()) {
        if ((op === "add") !== cache.stored.has(hash!)) xorInto(acc, hash!);
    }
    return bytesToHex(acc);
}

// ---------------------------------------------------------------------------
// Paged /render in progress (see sync.ts, bootstrap)
// ---------------------------------------------------------------------------

export interface RenderProgress {
    /** The snapshot's pinned sequence; null until the first page came back. */
    upto: number | null;
    /** Cursor of the next page; null for the first. */
    next: string | null;
    /** Pages applied so far. */
    page: number;
}

const RENDER_PROGRESS_KEY = "render/progress";

export function getRenderProgress(): RenderProgress | null {
    const raw = getStorage().get(RENDER_PROGRESS_KEY);
    return raw ? (JSON.parse(raw) as RenderProgress) : null;
}

export function setRenderProgress(progress: RenderProgress): void {
    getStorage().put(RENDER_PROGRESS_KEY, JSON.stringify(progress));
}

/** Ends a render: drops its progress. */
export function clearRender(): void {
    getStorage().delete(RENDER_PROGRESS_KEY);
}
