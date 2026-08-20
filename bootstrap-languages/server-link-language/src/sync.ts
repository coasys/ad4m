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
// Batched commit — coalesce contiguous synchronous commits into as few POSTs
// as possible WITHOUT reordering user-visible operations.
//
// Motivation: `alice.perspective.addLink()` × 1500 in a tight loop produced
// 1500 sequential POSTs to /rooms/<room>/commit, each blocking the next
// `addLink`. The stress-test integration ran ~200s and timed out.
//
// Design:
//   - Every `enqueueCommitBatched(diff)` appends the diff to a queue and
//     schedules a microtask flush if not already scheduled. Returns after
//     enqueuing — the actual POST(s) happen later.
//   - The flush walks the queue in order and greedily merges adjacent diffs
//     into "segments" where each segment can be sent as one POST without
//     losing caller-visible ordering. A merge is UNSAFE iff any link
//     identity appears in one merged side's additions and the other's
//     removals — in that case we start a new segment and send a separate
//     POST so the server applies them in caller order.
//     (The server applies additions before removals within a single POST:
//     link-server/src/db.ts::applyDiffAndAppend. So merging `remove(X);
//     add(X)` into `additions:[X], removals:[X]` would silently swap the
//     order → X gone when the caller expected X to remain.)
//   - Common bulk-add case (1500 unique addLinks) → 1 segment → 1 POST.
//   - Send errors are logged per segment; a failed segment is dropped and
//     subsequent segments still POST (they are ordering-independent from a
//     dropped one by construction — the split points guarantee it).
//     Callers relying on strict commit-error semantics must use `commit()`.
// ---------------------------------------------------------------------------

let _pendingQueue: PerspectiveDiff[] = [];
let _flushScheduled = false;
let _inflight: Promise<void> = Promise.resolve();

/** Enqueue a diff to be flushed on the next microtask. */
export function enqueueCommitBatched(diff: PerspectiveDiff): void {
    _pendingQueue.push(diff);
    if (!_flushScheduled) {
        _flushScheduled = true;
        // Serialize flushes: each waits for the previous to finish before
        // starting, so we never have two overlapping POSTs stepping on
        // sequence order.
        _inflight = _inflight.then(() => flushBatch());
    }
}

async function flushBatch(): Promise<void> {
    // Snapshot + reset FIRST so any commits arriving during the POST land in
    // the next batch, not this one.
    const queue = _pendingQueue;
    _pendingQueue = [];
    _flushScheduled = false;
    if (queue.length === 0) return;

    const segments = coalesceDiffs(queue);
    for (const segment of segments) {
        if (segment.additions.length === 0 && segment.removals.length === 0) continue;
        try {
            await commit(segment);
        } catch (err) {
            console.error(
                `[server-link-language] batched commit failed (${segment.additions.length} adds, ` +
                `${segment.removals.length} removes) — this segment is NOT retried; ` +
                `writes are safe locally but the server missed them.`,
                err,
            );
            deps().emitSyncState?.("LinkLanguageInstalledButNotSynced");
        }
    }
}

/**
 * Coalesce a queue of diffs into the minimum number of segments that
 * preserve observable server-side ordering. See the block comment above
 * for the merge-safety rule.
 *
 * Identity: `proof.signature` — a signature over the link's canonical
 * payload, so two links with the same signature are the same link from
 * every consumer's perspective. Empty signatures fall back to a
 * data+author+timestamp key so test fixtures without real signatures still
 * split correctly.
 */
export function coalesceDiffs(queue: PerspectiveDiff[]): PerspectiveDiff[] {
    const segments: PerspectiveDiff[] = [];
    for (const next of queue) {
        const current = segments[segments.length - 1];
        if (!current) {
            segments.push({ additions: [...next.additions], removals: [...next.removals] });
            continue;
        }
        const currentAddIds = new Set(current.additions.map(linkIdentity));
        const currentRemIds = new Set(current.removals.map(linkIdentity));
        const conflict =
            next.additions.some((l) => currentRemIds.has(linkIdentity(l))) ||
            next.removals.some((l) => currentAddIds.has(linkIdentity(l)));
        if (conflict) {
            segments.push({ additions: [...next.additions], removals: [...next.removals] });
        } else {
            current.additions.push(...next.additions);
            current.removals.push(...next.removals);
        }
    }
    return segments;
}

function linkIdentity(link: LinkExpression): string {
    const sig = link.proof?.signature;
    if (sig) return `sig:${sig}`;
    // Deterministic fallback matching the fields link-server uses to hash
    // (see link-server/src/types.ts::canonicalLinkPayload). Ensures
    // signature-less test fixtures still collide correctly.
    return `raw:${link.author}|${link.timestamp}|${link.data?.source ?? ""}|${link.data?.predicate ?? ""}|${link.data?.target ?? ""}`;
}

/** Test/teardown hook: await any in-flight flush + one pending flush so a
 * subsequent teardown or read sees a settled server state. Not called on
 * the hot path. */
export async function drainCommitBatch(): Promise<void> {
    await _inflight;
    if (_pendingQueue.length > 0) {
        await flushBatch();
    }
    await _inflight;
}

/** Test-only: clear all batch state without draining. Used by unit tests
 * that spin up fresh mock adapters between cases and don't want stale
 * queued diffs from the previous case flushing against them. */
export function _resetBatchStateForTests(): void {
    _pendingQueue = [];
    _flushScheduled = false;
    _inflight = Promise.resolve();
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
