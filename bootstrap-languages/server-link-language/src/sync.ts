/**
 * Sync logic — HTTP commit/catch-up + the single shared application path
 * for every inbound diff, however it arrived.
 *
 * ## THE CRITICAL RULE
 *
 * The AD4M executor discards the return value of `perspectiveSyncSync()`.
 * The *only* way an inbound link becomes queryable is `emitPerspectiveDiff`.
 * `applyInboundWireDiff` below is the single choke point every inbound
 * diff — HTTP catch-up batch entries AND WebSocket `"diff"` pushes alike —
 * must funnel through, and it is the only function in this module that
 * calls `deps().emitDiff`. Do not add a second inbound-diff code path.
 */

import * as store from "./store.js";
import * as api from "./api.js";
import type { RoomConfig } from "./adapters.js";
import type {
    LinkExpression,
    PerspectiveDiff,
    SyncDiffEntry,
    WireLinkExpression,
    WirePerspectiveDiff,
} from "./types.js";
import { decryptLinkFromWire, encryptLinkForWire, statusField } from "./encryption.js";

export interface SyncDeps {
    config: RoomConfig;
    /** Resolves to a guaranteed-fresh bearer token, re-authenticating as needed. */
    getToken: () => Promise<string>;
    /** MUST be called for every inbound diff — see module doc above. */
    emitDiff: (diff: PerspectiveDiff) => void;
    emitSyncState?: (state: string) => void;
    /** Returns the current room key, or null for a plaintext (non-E2E) room. */
    getRoomKey: () => Uint8Array | null;
}

let _deps: SyncDeps | null = null;

export function initSync(deps: SyncDeps): void {
    _deps = deps;
}

function deps(): SyncDeps {
    if (!_deps) {
        throw new Error("sync module not initialized. Call initSync() during language init().");
    }
    return _deps;
}

// ---------------------------------------------------------------------------
// Wire <-> local translation (encryption is a no-op for plaintext rooms)
// ---------------------------------------------------------------------------

function toWireLink(link: LinkExpression): WireLinkExpression {
    const roomKey = deps().getRoomKey();
    if (!roomKey) {
        return {
            author: link.author,
            timestamp: link.timestamp,
            proof: link.proof,
            ...statusField(link.status),
            data: link.data,
        };
    }
    return encryptLinkForWire(link, roomKey);
}

function fromWireLink(wireLink: WireLinkExpression): LinkExpression {
    const roomKey = deps().getRoomKey();
    if (wireLink.encrypted && roomKey) {
        return decryptLinkFromWire(wireLink, roomKey);
    }
    if (wireLink.encrypted && !roomKey) {
        throw new Error(
            "sync: received an encrypted link but no room key is available yet " +
            "(E2E key fetch may still be in flight, or this instance failed to decrypt it)",
        );
    }
    return {
        author: wireLink.author,
        timestamp: wireLink.timestamp,
        proof: wireLink.proof,
        ...statusField(wireLink.status),
        data: wireLink.data ?? { source: "", target: "" },
    };
}

function toWireDiff(diff: PerspectiveDiff): WirePerspectiveDiff {
    return {
        additions: diff.additions.map(toWireLink),
        removals: diff.removals.map(toWireLink),
    };
}

function fromWireDiff(wire: WirePerspectiveDiff): PerspectiveDiff {
    return {
        additions: (wire.additions ?? []).map(fromWireLink),
        removals: (wire.removals ?? []).map(fromWireLink),
    };
}

function normalizeSyncEntry(
    entry: SyncDiffEntry,
    fallbackRevision: string,
    fallbackSequence: number,
): { diff: WirePerspectiveDiff; revision: string; sequence: number } {
    const diff: WirePerspectiveDiff = entry.diff ?? {
        additions: entry.additions ?? [],
        removals: entry.removals ?? [],
    };
    return {
        diff,
        revision: entry.revision ?? fallbackRevision,
        sequence: typeof entry.sequence === "number" ? entry.sequence : fallbackSequence,
    };
}

// ---------------------------------------------------------------------------
// Inbound — the single choke point (see module doc)
// ---------------------------------------------------------------------------

export function applyInboundWireDiff(wireDiff: WirePerspectiveDiff, sequence: number, revision: string): PerspectiveDiff {
    const diff = fromWireDiff(wireDiff);
    store.applyDiff(diff);
    if (revision) store.setRevision(revision);
    if (Number.isFinite(sequence)) store.setSequence(sequence);
    deps().emitDiff(diff); // <-- CRITICAL. Do not remove. Do not bypass.
    return diff;
}

// ---------------------------------------------------------------------------
// Outbound — commit
// ---------------------------------------------------------------------------

/**
 * Direct single-shot commit. Callers that need error propagation (test
 * harnesses, non-batching write paths) can await this. Used by
 * `enqueueCommitBatched` below to send the accumulated batch.
 */
export async function commit(diff: PerspectiveDiff): Promise<void> {
    const { config, getToken } = deps();
    const token = await getToken();
    await api.commitDiff(config, token, toWireDiff(diff));
    deps().emitSyncState?.("Synced");
}

// ---------------------------------------------------------------------------
// Batched commit — coalesce contiguous synchronous commits into one POST
//
// Motivation: `alice.perspective.addLink()` × 1500 in a tight loop produced
// 1500 sequential POSTs to /rooms/<room>/commit under Holochain, each
// blocking the next `addLink`. The stress-test integration ran ~200s and
// timed out. Holochain's link language does not have this shape because
// commits go into a local Holochain source-chain first and network
// propagation is decoupled from the caller's await.
//
// Design:
//   - Every `enqueueCommitBatched(diff)` merges additions/removals into a
//     single pending diff and schedules a microtask flush if not already
//     scheduled. Returns after enqueuing — the actual POST happens later.
//   - The microtask flush drains the whole pending batch in ONE POST so the
//     server sees `additions[]` as long as the batch grew. Preserves ordering
//     within the batch.
//   - Send errors are logged; the language does not retry batches (a batch
//     that failed is dropped from the pending queue rather than blocking
//     subsequent writes). Callers relying on strict commit-error semantics
//     must use the direct `commit()` above.
// ---------------------------------------------------------------------------

let _pendingBatch: PerspectiveDiff | null = null;
let _flushScheduled = false;
let _inflight: Promise<void> = Promise.resolve();

/** Enqueue a diff to be flushed in a single POST at the next microtask. */
export function enqueueCommitBatched(diff: PerspectiveDiff): void {
    if (!_pendingBatch) {
        _pendingBatch = { additions: [], removals: [] };
    }
    _pendingBatch.additions.push(...diff.additions);
    _pendingBatch.removals.push(...diff.removals);
    if (!_flushScheduled) {
        _flushScheduled = true;
        // Serialize flushes: each flush waits for the previous to finish
        // before starting, so we never have two overlapping POSTs stepping on
        // sequence order.
        _inflight = _inflight.then(() => flushBatch());
    }
}

async function flushBatch(): Promise<void> {
    // Snapshot + reset FIRST so any commits arriving during the POST land in
    // the next batch, not this one.
    const batch = _pendingBatch;
    _pendingBatch = null;
    _flushScheduled = false;
    if (!batch || (batch.additions.length === 0 && batch.removals.length === 0)) {
        return;
    }
    try {
        await commit(batch);
    } catch (err) {
        console.error(
            `[server-link-language] batched commit failed (${batch.additions.length} adds, ` +
            `${batch.removals.length} removes) — this batch is NOT retried; ` +
            `writes are safe locally but the server missed them.`,
            err,
        );
        deps().emitSyncState?.("LinkLanguageInstalledButNotSynced");
    }
}

/** Test/teardown hook: await any in-flight flush + one pending flush so a
 * subsequent teardown or read sees a settled server state. Not called on
 * the hot path. */
export async function drainCommitBatch(): Promise<void> {
    await _inflight;
    if (_pendingBatch) {
        await flushBatch();
    }
    await _inflight;
}

// ---------------------------------------------------------------------------
// Cold-start bootstrap — full snapshot instead of replaying every diff
// ---------------------------------------------------------------------------

export async function bootstrap(): Promise<void> {
    const { config, getToken } = deps();
    const token = await getToken();

    const rendered = await api.fetchRender(config, token);
    const additions = rendered.links.map(fromWireLink);
    store.applyDiff({ additions, removals: [] });

    const rev = await api.fetchRevision(config, token);
    if (rev.revision) store.setRevision(rev.revision);
    store.setSequence(rev.sequence);
}

// ---------------------------------------------------------------------------
// HTTP catch-up (used both by sync() and by ws-client's onOpen/reconnect hook)
// ---------------------------------------------------------------------------

/**
 * Fetches everything since our last known sequence and applies it.
 * Idempotent by construction: `since` only advances after a diff is
 * successfully applied, so a WebSocket that already delivered everything
 * live simply gets back an empty `diffs[]` here — there's no separate
 * "skip because WS is connected" branch to get wrong.
 */
export async function catchUp(): Promise<PerspectiveDiff> {
    const { config, getToken } = deps();
    const token = await getToken();
    const since = store.getSequence();
    const res = await api.fetchSync(config, token, since);

    let last: PerspectiveDiff = { additions: [], removals: [] };
    for (const rawEntry of res.diffs) {
        const entry = normalizeSyncEntry(rawEntry, res.revision, res.sequence);
        last = applyInboundWireDiff(entry.diff, entry.sequence, entry.revision);
    }

    if (res.diffs.length === 0 && res.revision) {
        store.setRevision(res.revision);
        store.setSequence(res.sequence);
    }

    deps().emitSyncState?.("Synced");
    return last;
}

/** perspective-sync.sync() (see index.ts). Never throws — sync failures
 * are logged and reported via emitSyncState so a flaky server can't crash
 * the runtime's polling loop. */
export async function performSync(): Promise<PerspectiveDiff> {
    try {
        return await catchUp();
    } catch (err) {
        console.error("[server-link-language] sync failed:", err);
        // See index.ts comment on the same value — must be a valid
        // PerspectiveState variant on the executor side.
        deps().emitSyncState?.("LinkLanguageInstalledButNotSynced");
        return { additions: [], removals: [] };
    }
}

export function currentRevision(): string {
    return store.getRevision() || "";
}

export function render() {
    return store.allLinks();
}
