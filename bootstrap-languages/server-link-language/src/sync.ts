/**
 * Sync logic — HTTP commit/catch-up + the single shared application path
 * for every inbound diff, however it arrived.
 *
 * ## THE CRITICAL RULE
 *
 * The AD4M executor discards the return value of `perspectiveSyncSync()`.
 * The *only* way an inbound link becomes queryable is `emitPerspectiveDiff`.
 * Every inbound link — HTTP catch-up entries, WebSocket `"diff"` pushes and
 * the startup `/render` snapshot alike — goes through `openOrHold`, and every
 * change to the store from them goes through `applyAndEmit`, the only
 * function in this module that calls `deps().emitDiff`. It emits exactly what
 * changed in the store, so a link is emitted once however often it arrives.
 *
 * A wire link this agent holds no key for is never dropped: it is kept under
 * `held/` in the store and opened by `retryHeld` once a key opens it. So the
 * sequence cursor may move past it. Do not add a second inbound-diff path.
 */

import * as store from "./store.js";
import * as api from "./api.js";
import type { RoomConfig } from "./adapters.js";
import type {
    Link,
    LinkExpression,
    PerspectiveDiff,
    RenderResponse,
    SyncDiffEntry,
    WireLinkExpression,
    WirePerspectiveDiff,
} from "./types.js";
import { isEncryptedLinkData } from "./types.js";
import {
    decryptLinkFromWire,
    encryptLinkForWire,
    latestKeyVersion,
    statusField,
    wireLinkHash,
    type KeyRing,
} from "./encryption.js";

export interface SyncDeps {
    config: RoomConfig;
    /** Resolves to a guaranteed-fresh bearer token, re-authenticating as needed. */
    getToken: () => Promise<string>;
    /** Drops the cached token so the next getToken() re-authenticates. Called
     *  when the server answers 401: it revoked or no longer knows the session. */
    invalidateToken?: () => void;
    /** MUST be called for every inbound diff — see module doc above. */
    emitDiff: (diff: PerspectiveDiff) => void;
    emitSyncState?: (state: string) => void;
    /** Returns the current key ring, or null for a plaintext (non-E2E) room. */
    getKeyRing: () => KeyRing | null;
    /** Re-reads the key ring from the server. Catch-up calls it while links
     *  wait for a key, then retries them (`retryHeld`) whatever it found. */
    refreshKeyRing?: () => Promise<void>;
    /** Called after every successful sync cycle. A member holding keys uses
     *  it to grant them to members lacking them — the periodic HTTP sync is
     *  the fallback discovery path when onPeerJoined never fires. */
    onPostSync?: () => Promise<void>;
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

/**
 * Runs `fn` with a bearer token. A 401 means the server dropped the session
 * (revocation, expiry sweep, data reset), so the cached token is discarded
 * and `fn` runs once more with a fresh one. Without this the dead token is
 * reused until the executor restarts and nothing reaches the server.
 */
async function withToken<T>(fn: (token: string) => Promise<T>): Promise<T> {
    const d = deps();
    try {
        return await fn(await d.getToken());
    } catch (err) {
        if (!(err instanceof api.ApiError) || err.status !== 401 || !d.invalidateToken) throw err;
        d.invalidateToken();
        return fn(await d.getToken());
    }
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

/** A wire link the key it names does not open. */
const UNOPENABLE = Symbol("unopenable");

/**
 * The link a wire link carries, or null when it is encrypted with a key
 * version this agent does not hold (yet). A link the key it names does not
 * open (malformed ciphertext or payload) is logged and `UNOPENABLE`: a key
 * version is fixed by its rotation's commitment, so no later key opens it
 * either, and it costs that one link, not the diff, the catch-up batch or
 * the held links around it.
 */
function openWireLink(wireLink: WireLinkExpression): LinkExpression | null | typeof UNOPENABLE {
    if (isEncryptedLinkData(wireLink.data)) {
        const keyRing = deps().getKeyRing();
        const version = wireLink.key_version ?? 1;
        if (!keyRing || !keyRing.has(version)) return null;
        try {
            return decryptLinkFromWire(wireLink, keyRing);
        } catch (err) {
            console.error(
                `[server-link-language] skipping link ${heldId(wireLink)}: key version ${version} does not open it:`,
                err instanceof Error ? err.message : err,
            );
            return UNOPENABLE;
        }
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

// ---------------------------------------------------------------------------
// Inbound — the single path (see module doc)
// ---------------------------------------------------------------------------

/** The key a held wire link is stored under: its link_hash, so a later op on
 *  the same link replaces it. */
function heldId(wireLink: WireLinkExpression): string {
    if (wireLink.link_hash) return wireLink.link_hash;
    const data = wireLink.data as { nonce?: string };
    return `nonce-${data.nonce ?? ""}`;
}

/**
 * Opens `wireLink`, or holds it until a key opens it. Either way the newest
 * op on a link wins: opening one drops any older held op on the same link.
 */
function openOrHold(wireLink: WireLinkExpression, op: store.HeldOp): LinkExpression | null {
    const link = openWireLink(wireLink);
    if (link === UNOPENABLE) return null;
    if (!link) {
        store.holdLink(heldId(wireLink), op, wireLink);
        return null;
    }
    store.releaseHeld(wireLinkHash(link));
    return link;
}

/**
 * Applies `diff` to the store and emits what it changed: additions not yet
 * stored, removals of stored links. The ONLY caller of `emitDiff` (see the
 * module doc). Returns the emitted diff.
 */
function applyAndEmit(diff: PerspectiveDiff): PerspectiveDiff {
    const added = new Map<string, LinkExpression>();
    const removals: LinkExpression[] = [];
    for (const link of diff.additions) {
        if (!store.hasLink(link)) {
            store.putLink(link);
            added.set(store.hashLink(link), link);
        }
    }
    for (const link of diff.removals) {
        if (!store.hasLink(link)) continue;
        store.removeLink(link);
        // Added and removed in one diff: the executor never needs to see it.
        if (!added.delete(store.hashLink(link))) removals.push(link);
    }
    const emitted = { additions: [...added.values()], removals };
    if (emitted.additions.length > 0 || emitted.removals.length > 0) {
        deps().emitDiff(emitted); // <-- CRITICAL. Do not remove. Do not bypass.
    }
    return emitted;
}

function openOrHoldDiff(wire: WirePerspectiveDiff): PerspectiveDiff {
    const additions: LinkExpression[] = [];
    const removals: LinkExpression[] = [];
    for (const wireLink of wire.additions ?? []) {
        const link = openOrHold(wireLink, "add");
        if (link) additions.push(link);
    }
    for (const wireLink of wire.removals ?? []) {
        const link = openOrHold(wireLink, "remove");
        if (link) removals.push(link);
    }
    return { additions, removals };
}

/** One inbound diff (a catch-up entry or a WebSocket push) at `sequence`.
 *  Returns what it changed, which is what was emitted.
 *
 *  While a render is stored it applies nothing and leaves the cursor: catch-up
 *  does not tail then, so only a WebSocket push gets here, and the render
 *  brings its links anyway. A push before the first page is inside the
 *  snapshot; one after it is after `upto`, in the tail that follows the last
 *  page. Applying it early would let a lost later push leave a link the
 *  snapshot no longer has. */
export function applyInboundWireDiff(wireDiff: WirePerspectiveDiff, sequence: number, revision: string): PerspectiveDiff {
    if (store.getRenderProgress()) return { additions: [], removals: [] };
    const emitted = applyAndEmit(openOrHoldDiff(wireDiff));
    if (revision) store.setRevision(revision);
    if (Number.isFinite(sequence)) store.setSequence(sequence);
    return emitted;
}

/** Whether inbound links wait for a key this agent does not hold yet. */
export function hasHeldLinks(): boolean {
    return store.heldLinks().length > 0;
}

/**
 * Opens every held link the current key ring opens, applies and emits them.
 * Call it whenever the key ring is loaded; catch-up calls it every cycle.
 */
export function retryHeld(): PerspectiveDiff {
    if (!deps().getKeyRing()) return { additions: [], removals: [] };
    const additions: LinkExpression[] = [];
    const removals: LinkExpression[] = [];
    for (const held of store.heldLinks()) {
        const link = openWireLink(held.link);
        if (!link) continue;
        // Opened, or never will be: either way it no longer waits for a key.
        store.releaseHeld(held.id);
        if (link === UNOPENABLE) continue;
        (held.op === "add" ? additions : removals).push(link);
    }
    if (additions.length + removals.length > 0) {
        console.log(
            `[server-link-language] opened ${additions.length + removals.length} held link(s) with the key ring`,
        );
    }
    return applyAndEmit({ additions, removals });
}

// ---------------------------------------------------------------------------
// Outbound — commit
// ---------------------------------------------------------------------------

/**
 * Direct single-shot commit. Callers that need error propagation (test
 * harnesses, non-batching write paths) can await this. Used by
 * `enqueueCommitBatched` below to send the accumulated batch.
 *
 * The store changes only after the server accepted the diff, so a failed
 * POST leaves no local-only links. Nothing is emitted back: the executor
 * committed these links and already holds them. When the server put the
 * commit right after our cursor, the cursor moves past it so catch-up does
 * not fetch our own links back (otherwise another agent's diff is between).
 */
export async function commit(diff: PerspectiveDiff): Promise<void> {
    const { config } = deps();
    console.log(
        `[server-link-language] POST /commit: ${diff.additions.length} adds, ` +
        `${diff.removals.length} removes → room=${config.roomId}`,
    );
    const wire = toWireDiff(diff);
    const res = await withToken((token) => api.commitDiff(config, token, wire));
    store.applyDiff(diff);
    if (typeof res.sequence === "number" && res.sequence === store.getSequence() + 1) {
        store.setSequence(res.sequence);
        if (res.revision) store.setRevision(res.revision);
    }
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
    _renderDueAt = 0;
    _renderRetryDelay = RENDER_RETRY_BASE_MS;
    _rendering = null;
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

/** The wait before the first retry of a failed render; it doubles per failure. */
export const RENDER_RETRY_BASE_MS = 10_000;
/** The longest wait between render attempts. */
const RENDER_RETRY_MAX_MS = 5 * 60 * 1000;

/** `Date.now()` before which catch-up does not retry the render. */
let _renderDueAt = 0;
let _renderRetryDelay = RENDER_RETRY_BASE_MS;
/** The render in flight (its pages, then the tail from `upto`), from `init`
 *  or a retry: there is never a second one. */
let _rendering: Promise<void> | null = null;

/**
 * Stores a render for a fresh member: one whose cursor is 0 and that has no
 * render stored. A stored render (a first render cut off by a failure or a
 * restart) is kept and resumed. A member whose cursor is above 0 never
 * renders: the server keeps the whole diff log, so `/sync` from its cursor
 * brings every change. The stored progress is the "render needed" state:
 * until the last page clears it, catch-up does not tail `/sync` and retries
 * the render with backoff (`retryRenderIfDue`). `init` calls it before its
 * startup sequence, so a render that never ran (authentication failed
 * first) is run by catch-up too.
 */
export function needRender(): void {
    if (_rendering || store.getRenderProgress() || store.getSequence() > 0) return;
    store.clearHeld();
    store.setRenderProgress({ upto: null, next: null, page: 0 });
}

/** The render, one at a time: `bootstrap` and `retryRenderIfDue` share it. */
function startRender(): Promise<void> {
    if (!_rendering) {
        const p = runRender().finally(() => {
            if (_rendering === p) _rendering = null;
        });
        _rendering = p;
    }
    return _rendering;
}

/**
 * The stored render's remaining pages, then the tail from `upto`. A failed
 * page keeps its progress and sets the backoff, which doubles per failure up
 * to `RENDER_RETRY_MAX_MS`; the last page resets it.
 */
async function runRender(): Promise<void> {
    try {
        await renderPages();
    } catch (err) {
        _renderDueAt = Date.now() + _renderRetryDelay;
        _renderRetryDelay = Math.min(_renderRetryDelay * 2, RENDER_RETRY_MAX_MS);
        throw err;
    }
    _renderDueAt = 0;
    _renderRetryDelay = RENDER_RETRY_BASE_MS;
    await catchUp();
}

/**
 * While a render is stored and its backoff has elapsed, resumes it in the
 * background and returns: catch-up does not wait for it. Its own tail
 * brings the diffs after its `upto`.
 */
function retryRenderIfDue(): void {
    if (_rendering || !store.getRenderProgress() || Date.now() < _renderDueAt) return;
    console.log("[server-link-language] resuming the room render at its stored cursor");
    startRender().then(
        () => console.log("[server-link-language] room render finished on retry"),
        (err) => console.error(
            "[server-link-language] room render retry, or the catch-up after it, failed; " +
            "catch-up resumes the render after a backoff:",
            err,
        ),
    );
}

/** Test-only: await the render in flight, if any, ignoring its failure. */
export async function _awaitRenderRetryForTests(): Promise<void> {
    await _rendering?.catch(() => undefined);
}

/**
 * Brings the store up to the room's state on startup. A member with a cursor
 * above 0 catches up from it. A fresh member loads a paged `/render`
 * snapshot pinned at `upto`, then the diff log after `upto` (`catchUp`).
 * Each page is applied and emitted as its own diff, and the cursor is stored
 * after it, so a restart (or a retry) resumes at the next page. The snapshot
 * is the room's state at `upto` and the log after it carries every later add
 * and remove, so a fresh store needs nothing removed: what it holds besides
 * the pages are its own commits, which are on the server and in the snapshot
 * or the tail. Snapshot links no key opens yet are held.
 *
 * Single-flight: a call while a render is out joins it. A failed page keeps
 * its progress, and catch-up resumes it with backoff (`retryRenderIfDue`)
 * until the last page.
 */
export async function bootstrap(): Promise<void> {
    needRender();
    if (!store.getRenderProgress()) {
        await catchUp();
        return;
    }
    await startRender();
}

/** Links per `/render` page, diffs per `/sync` page. A page of 500 encrypted
 *  links is about 0.5 MB, 0.33 MB compressed (link-server `npm run bench`),
 *  so a page fits the host's 10 s fetch deadline down to about 40 kB/s. */
export const RENDER_PAGE_LIMIT = 500;
export const SYNC_PAGE_LIMIT = 100;
let _renderPageLimit = RENDER_PAGE_LIMIT;
let _syncPageLimit = SYNC_PAGE_LIMIT;

/** Test-only: small pages, so a test room of a few links spans several. */
export function _setPageLimitsForTests(render = RENDER_PAGE_LIMIT, sync = SYNC_PAGE_LIMIT): void {
    _renderPageLimit = render;
    _syncPageLimit = sync;
}

async function renderPages(): Promise<void> {
    const { config } = deps();
    const stored = store.getRenderProgress();
    if (!stored) return;
    let progress: store.RenderProgress = stored;
    let upto = progress.upto;
    for (;;) {
        const cursor = { limit: _renderPageLimit, next: progress.next };
        const page: RenderResponse = await withToken((token) => api.fetchRender(config, token, cursor));
        upto ??= page.upto;

        const additions: LinkExpression[] = [];
        for (const wireLink of page.links) {
            const link = openOrHold(wireLink, "add");
            if (link) additions.push(link);
        }
        applyAndEmit({ additions, removals: [] });
        progress = { upto, next: page.next, page: progress.page + 1 };
        store.setRenderProgress(progress);
        if (page.next === null) break;
    }

    const held = store.heldLinks().length;
    if (held > 0) {
        console.warn(
            `[server-link-language] bootstrap: ${held} link(s) wait for a room key this agent ` +
            "does not hold yet; they are applied once it does",
        );
    }
    console.log(
        `[server-link-language] bootstrap: snapshot at sequence ${upto} loaded in ${progress.page} page(s)`,
    );
    store.setSequence(upto ?? 0);
    store.clearRender();
}

// ---------------------------------------------------------------------------
// HTTP catch-up (used both by sync() and by ws-client's onOpen/reconnect hook)
// ---------------------------------------------------------------------------

/**
 * Fetches everything since our last known sequence and applies it, a page
 * at a time. Idempotent by construction: `since` only advances after a diff
 * is applied (or held), so a WebSocket that already delivered everything
 * live simply gets back an empty `diffs[]` here — there's no separate
 * "skip because WS is connected" branch to get wrong. While links are held
 * for a key, it re-reads the key ring; it then retries the held links
 * against whatever ring it has. Returns what it emitted.
 *
 * While a paged render is stored (`bootstrap`), it does not tail: a render
 * is stored only at cursor 0, and reading the log from 0 is the full replay
 * the render replaces. It resumes the render instead, once its
 * backoff has elapsed, without waiting for it (`retryRenderIfDue`); the
 * render's last page is followed by the tail from its `upto`.
 */
export async function catchUp(): Promise<PerspectiveDiff> {
    const { config } = deps();

    const emitted: PerspectiveDiff = { additions: [], removals: [] };
    const collect = (diff: PerspectiveDiff) => {
        emitted.additions.push(...diff.additions);
        emitted.removals.push(...diff.removals);
    };
    let since = store.getSequence();
    while (!store.getRenderProgress()) {
        const res = await withToken((token) => api.fetchSync(config, token, since, _syncPageLimit));
        console.log(
            `[server-link-language] catchUp: since=${since}, received ${res.diffs.length} diff(s), ` +
            `revision=${res.revision}, sequence=${res.sequence}` + (res.next !== null ? `, next=${res.next}` : ""),
        );
        // The room's revision belongs to its latest sequence: only the last page sets it.
        const last = res.next === null;
        for (const { additions, removals, sequence } of res.diffs) {
            collect(applyInboundWireDiff({ additions, removals }, sequence, last ? res.revision : ""));
        }
        if (res.diffs.length === 0 && res.revision) {
            store.setRevision(res.revision);
            store.setSequence(res.sequence);
        }
        if (res.next === null) break;
        since = res.next;
    }

    const { refreshKeyRing } = deps();
    if (refreshKeyRing && hasHeldLinks()) {
        try {
            await refreshKeyRing();
        } catch (err) {
            console.error("[server-link-language] key ring refresh failed:", err);
        }
    }
    collect(retryHeld());
    if (store.getRenderProgress()) {
        // Like the key ring above: until the render's last page, every
        // catch-up resumes it, gated by its backoff.
        retryRenderIfDue();
        emitSyncStateSafe("LinkLanguageInstalledButNotSynced");
        return emitted;
    }

    emitSyncStateSafe("Synced");
    return emitted;
}

/** perspective-sync.sync() (see index.ts). Never throws — sync failures
 * are logged and reported via emitSyncState so a flaky server can't crash
 * the runtime's polling loop. */
export async function performSync(): Promise<PerspectiveDiff> {
    try {
        const result = await catchUp();
        // Post-sync admin duties: grant historical keys to members who
        // joined while the WebSocket was down. Without this, key grants
        // depend entirely on the WS `peer-joined` event, which may
        // never fire if the WebSocket can't connect (CI, firewalls,
        // transient outages). Running here makes every successful HTTP
        // sync a fallback discovery path for new members.
        if (_deps?.onPostSync) {
            void _deps.onPostSync().catch((err) => {
                console.error("[server-link-language] post-sync callback failed:", err);
            });
        }
        return result;
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
