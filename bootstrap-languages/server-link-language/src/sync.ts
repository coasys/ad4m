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
    Link,
    LinkExpression,
    PerspectiveDiff,
    SyncDiffEntry,
    WireLinkExpression,
    WirePerspectiveDiff,
} from "./types.js";
import { isEncryptedLinkData } from "./types.js";
import { decryptLinkFromWire, encryptLinkForWire, latestKeyVersion, statusField, type KeyRing } from "./encryption.js";

export interface SyncDeps {
    config: RoomConfig;
    /** Resolves to a guaranteed-fresh bearer token, re-authenticating as needed. */
    getToken: () => Promise<string>;
    /** MUST be called for every inbound diff — see module doc above. */
    emitDiff: (diff: PerspectiveDiff) => void;
    emitSyncState?: (state: string) => void;
    /** Returns the current key ring, or null for a plaintext (non-E2E) room. */
    getKeyRing: () => KeyRing | null;
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
    const keyRing = deps().getKeyRing();
    if (!keyRing) {
        return {
            author: link.author,
            timestamp: link.timestamp,
            proof: link.proof,
            ...statusField(link.status),
            data: link.data,
        };
    }
    const version = latestKeyVersion(keyRing);
    const key = keyRing.get(version);
    if (!key) {
        throw new Error("toWireLink: key ring has no keys");
    }
    return encryptLinkForWire(link, key, version);
}

function fromWireLink(wireLink: WireLinkExpression): LinkExpression {
    const keyRing = deps().getKeyRing();
    if (isEncryptedLinkData(wireLink.data) && keyRing) {
        return decryptLinkFromWire(wireLink, keyRing);
    }
    if (isEncryptedLinkData(wireLink.data) && !keyRing) {
        throw new Error(
            "sync: received an encrypted link but no key ring available yet " +
            "(E2E key fetch may still be in flight, or this instance failed to decrypt it)",
        );
    }
    return {
        author: wireLink.author ?? "",
        timestamp: wireLink.timestamp ?? "",
        proof: wireLink.proof ?? { signature: "", key: "" },
        ...statusField(wireLink.status),
        data: (wireLink.data as Link) ?? { source: "", target: "" },
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
    emitSyncStateSafe("Synced");
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
//   - Per-segment bounded retries with exponential backoff (replaces the
//     old `commitWithRetry`). If a segment still fails after
//     `MAX_COMMIT_ATTEMPTS`, the failed segment and all remaining
//     downstream segments get re-enqueued into `_pendingQueue` so the
//     next flush cycle picks them up automatically. The existing
//     coalescing handles dedup if diffs overlap, and the microtask
//     scheduling triggers the retry naturally. We emit
//     `LinkLanguageInstalledButNotSynced` so the executor knows the
//     language lags until the re-queued segments land.
// ---------------------------------------------------------------------------

const MAX_COMMIT_ATTEMPTS = 3;
const RETRY_BASE_DELAY_MS = 200;

const MAX_RETRY_DELAY_MS = 5 * 60 * 1000;
let _pendingQueue: PerspectiveDiff[] = [];
let _flushScheduled = false;
let _inflight: Promise<void> = Promise.resolve();
let _retryTimer: ReturnType<typeof setTimeout> | null = null;
let _retryDelay = 2_500;

/** Enqueue a diff to be flushed on the next microtask. */
export function enqueueCommitBatched(diff: PerspectiveDiff): void {
    _pendingQueue.push(diff);
    if (!_flushScheduled) {
        _flushScheduled = true;
        // Serialize flushes: each waits for the previous to finish before
        // starting, so we never have two overlapping POSTs stepping on
        // sequence order. The terminal `.catch` is load-bearing: `_inflight`
        // is a single reusable chain, and if a flush rejects (e.g. deps()
        // throws because the module was torn down mid-microtask), every
        // subsequent `.then(flushBatch)` would be skipped, silently killing
        // all future commits. The catch swallows the rejection so the chain
        // stays live.
        _inflight = _inflight
            .then(() => flushBatch())
            .catch((err) => {
                console.error(
                    "[server-link-language] batch flush crashed; keeping the flush chain live for subsequent enqueues:",
                    err,
                );
            });
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
    for (let i = 0; i < segments.length; i++) {
        const segment = segments[i];
        if (segment.additions.length === 0 && segment.removals.length === 0) continue;
        const ok = await commitSegmentWithRetries(segment);
        if (!ok) {
            // Re-enqueue the failed segment + all remaining downstream
            // segments. They land at the front of the pending queue so
            // the next flush cycle retries them in original order.
            const requeue = segments.slice(i);
            const requeueSummary = requeue.map((s, idx) =>
                `  segment ${i + 1 + idx}: ${s.additions.length} adds, ${s.removals.length} removes`)
                .join("\n");
            console.warn(
                `[server-link-language] segment ${i + 1}/${segments.length} failed after ` +
                `${MAX_COMMIT_ATTEMPTS} attempts; re-enqueueing ${requeue.length} segment(s) ` +
                `for next flush cycle:\n${requeueSummary}`,
            );
            _pendingQueue.unshift(...requeue);
            if (!_retryTimer) {
                _retryDelay = Math.min(_retryDelay * 2, MAX_RETRY_DELAY_MS);
                _retryTimer = setTimeout(() => {
                    _retryTimer = null;
                    if (_pendingQueue.length > 0 && !_flushScheduled) {
                        _flushScheduled = true;
                        _inflight = _inflight
                            .then(() => flushBatch())
                            .catch((err) => {
                                console.error(
                                    "[server-link-language] retry flush crashed; keeping the flush chain live:",
                                    err,
                                );
                            });
                    }
                }, _retryDelay);
            }
            emitSyncStateSafe("LinkLanguageInstalledButNotSynced");
            return;
        }
    }
    _retryDelay = 2_500;
}

/**
 * `deps()` can throw once the language has been torn down (resetAdapters
 * clears the module state), and a throwing `emitSyncState` (e.g. because
 * getRuntime() no longer exists) would reject the entire flushBatch —
 * poisoning `_inflight` if that rejection escapes the chain's terminal
 * catch. Route every sync-state emission through this so an emit failure
 * during teardown stays local instead of taking the flush chain with it.
 */
function emitSyncStateSafe(state: string): void {
    try {
        _deps?.emitSyncState?.(state);
    } catch (err) {
        console.error("[server-link-language] emitSyncState failed (likely post-teardown):", err);
    }
}

async function commitSegmentWithRetries(segment: PerspectiveDiff): Promise<boolean> {
    let delay = RETRY_BASE_DELAY_MS;
    for (let attempt = 1; attempt <= MAX_COMMIT_ATTEMPTS; attempt++) {
        try {
            await commit(segment);
            return true;
        } catch (err) {
            const isLast = attempt === MAX_COMMIT_ATTEMPTS;
            console.error(
                `[server-link-language] batched commit attempt ${attempt}/${MAX_COMMIT_ATTEMPTS} ` +
                `failed (${segment.additions.length} adds, ${segment.removals.length} removes)` +
                (isLast ? " — giving up." : `; retrying in ${delay}ms.`),
                err,
            );
            if (isLast) return false;
            await new Promise<void>((resolve) => setTimeout(resolve, delay));
            delay *= 2;
        }
    }
    return false;
}

/**
 * Coalesce a queue of diffs into the minimum number of segments that
 * preserve observable server-side ordering. See the block comment above
 * for the merge-safety rule.
 *
 * Identity: canonical fields (`author`, `timestamp`, `source`, `predicate`,
 * `target`) serialised via JSON.stringify. Matches the server's own
 * linkHash inputs — see `linkIdentity` below for why signature is
 * deliberately excluded.
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
    // Must match the SAME canonical field set (and key order) as the
    // server's canonicalLinkPayload (link-server/src/types.ts) so that two
    // links the server considers identical also collapse here. We
    // deliberately do NOT include proof.signature: two links with
    // identical canonical fields but different signatures (e.g. re-signed
    // after a key rotation) are the SAME link from the server's
    // perspective, and treating them as different identities would let a
    // remove(oldSig) + add(newSig) sequence merge into one segment →
    // additions:[X], removals:[X] → server applies add-then-remove → X
    // gone, contrary to the caller's intent.
    return JSON.stringify({
        source: link.data?.source ?? "",
        predicate: link.data?.predicate ?? null,
        target: link.data?.target ?? "",
        author: link.author,
        timestamp: link.timestamp,
    });
}

/** Test/teardown hook: await any in-flight flush + drain everything still
 * queued so a subsequent teardown or read sees a settled server state. Not
 * called on the hot path.
 *
 * Loops rather than making one pass: a diff enqueued during the final
 * await (by a concurrent caller or by teardown code that itself commits)
 * would otherwise stay unflushed. The bound is a runaway-loop guard, not
 * an expected iteration count — a settled system exits after 1 or 2 laps.
 * Enqueues are routed through the normal `enqueueCommitBatched` path so
 * they land in `_inflight` — direct `flushBatch()` calls would break the
 * serialisation invariant that other flushes rely on.
 */
export async function drainCommitBatch(): Promise<void> {
    // Cap at 10 laps (not 100) — each lap can trigger up to 3 retry POSTs
    // per segment. 10 laps × 3 attempts = 30 POSTs max, which keeps the
    // teardown/outage burst bounded while still giving a genuine queue a
    // reasonable chance to flush.
    const MAX_LAPS = 10;
    for (let lap = 0; lap < MAX_LAPS; lap++) {
        await _inflight;
        if (_pendingQueue.length === 0) return;
        if (!_flushScheduled) {
            _flushScheduled = true;
            _inflight = _inflight
                .then(() => flushBatch())
                .catch((err) => {
                    console.error(
                        "[server-link-language] batch flush crashed during drain:",
                        err,
                    );
                });
        }
    }
    if (_pendingQueue.length > 0) {
        console.warn(
            `[server-link-language] drainCommitBatch: bailed after ${MAX_LAPS} laps with ` +
            `${_pendingQueue.length} segment(s) still pending — they will flush on the next ` +
            `enqueueCommitBatched call.`,
        );
    }
}

/** Test-only: clear all batch state without draining. Used by unit tests
 * that spin up fresh mock adapters between cases and don't want stale
 * queued diffs from the previous case flushing against them. */
export function _resetBatchStateForTests(): void {
    _pendingQueue = [];
    _flushScheduled = false;
    _inflight = Promise.resolve();
    if (_retryTimer) { clearTimeout(_retryTimer); _retryTimer = null; }
    _retryDelay = 2_500;
}

/** Test-only: await the current in-flight flush without the drain loop.
 * Unlike drainCommitBatch (which re-schedules until the queue empties),
 * this resolves after exactly one flush cycle — useful when testing
 * failure paths where the queue never empties. */
export async function _awaitInflightForTests(): Promise<void> {
    await _inflight;
}

// ---------------------------------------------------------------------------
// Cold-start bootstrap — full snapshot instead of replaying every diff
// ---------------------------------------------------------------------------

export async function bootstrap(): Promise<void> {
    const { config, getToken } = deps();
    const token = await getToken();

    // The render response now includes revision + sequence, so we avoid the
    // extra fetchRevision round-trip that the old code made.
    const rendered = await api.fetchRender(config, token);
    const additions = rendered.links.map(fromWireLink);

    // Replace the local link set atomically: remove any stale links left
    // from a previous session, then apply the authoritative server snapshot.
    // Without this, links deleted while the language was stopped would
    // remain visible locally.
    const existing = store.allLinks();
    store.applyDiff({ additions: [], removals: existing.links });
    store.applyDiff({ additions, removals: [] });

    if (rendered.revision) store.setRevision(rendered.revision);
    if (typeof rendered.sequence === "number") store.setSequence(rendered.sequence);
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

    emitSyncStateSafe("Synced");
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
        // Route through emitSyncStateSafe to honour performSync's
        // "never throws" contract even if the runtime torn down between
        // catchUp() rejecting and this emit landing.
        emitSyncStateSafe("LinkLanguageInstalledButNotSynced");
        return { additions: [], removals: [] };
    }
}

export function currentRevision(): string {
    return store.getRevision() || "";
}

export function render() {
    return store.allLinks();
}
