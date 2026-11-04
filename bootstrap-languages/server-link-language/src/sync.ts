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
    LinkExpression,
    PerspectiveDiff,
    SyncDiffEntry,
    WireLinkExpression,
    WirePerspectiveDiff,
} from "./types.js";
import { isEncryptedLinkData } from "./types.js";
import {
    decryptLinkFromWire,
    encryptLinkForWire,
    latestKeyVersion,
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
    /** Returns the current key ring, or null while this agent holds no room key. */
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
// Wire <-> local translation: every link on the wire is encrypted
// ---------------------------------------------------------------------------

function toWireLink(link: LinkExpression): WireLinkExpression {
    const keyRing = deps().getKeyRing();
    if (!keyRing) {
        // There is no plaintext mode: a link without a key does not leave.
        throw new Error("toWireLink: no room key to encrypt with yet");
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
 * version this agent does not hold (yet). A plaintext link is `UNOPENABLE`:
 * the link server refuses plaintext in every room, so no member committed it. A link the key it names
 * does not open (malformed ciphertext or payload) is logged and `UNOPENABLE`: a key
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
    // Rooms refuse plaintext from creation, so no member sent this: skip it.
    console.error(`[server-link-language] skipping link ${heldId(wireLink)}: it is not encrypted`);
    return UNOPENABLE;
}

function toWireDiff(diff: PerspectiveDiff): WirePerspectiveDiff {
    return {
        additions: diff.additions.map(toWireLink),
        removals: diff.removals.map(toWireLink),
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
 *  Returns what it changed, which is what was emitted. */
export function applyInboundWireDiff(wireDiff: WirePerspectiveDiff, sequence: number, revision: string): PerspectiveDiff {
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
 * The language's perspective-commit: one POST per diff the executor commits.
 * A failed POST rejects, and the executor keeps the diff in its pending-diffs
 * queue (its database: retried with backoff, oldest first, across restarts),
 * so the language keeps no outbox of its own. The executor batches bursts
 * before they get here.
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

/**
 * `deps()` can throw once the language has been torn down (resetAdapters
 * clears the module state), and a throwing `emitSyncState` (e.g. because
 * getRuntime() no longer exists) would turn a finished commit or sync into a
 * failure. Route every sync-state emission through this so an emit failure
 * during teardown stays local.
 */
function emitSyncStateSafe(state: string): void {
    try {
        _deps?.emitSyncState?.(state);
    } catch (err) {
        console.error("[server-link-language] emitSyncState failed (likely post-teardown):", err);
    }
}

// ---------------------------------------------------------------------------
// Cold-start bootstrap — full snapshot instead of replaying every diff
// ---------------------------------------------------------------------------

/**
 * Loads the server's current link set (`/render`) as the authoritative state:
 * stored links it lacks are removed, its links are added, both emitted. Held
 * links from before are dropped (the snapshot supersedes them); the
 * snapshot's own links that no key opens yet are held. A stored link whose
 * encrypted copy is held stays: the key ring not being loaded yet must not
 * delete links the executor holds.
 */
export async function bootstrap(): Promise<void> {
    const { config } = deps();

    const rendered = await withToken((token) => api.fetchRender(config, token));

    store.clearHeld();
    const additions: LinkExpression[] = [];
    const heldIds = new Set<string>();
    for (const wireLink of rendered.links) {
        const link = openOrHold(wireLink, "add");
        if (link) additions.push(link);
        else heldIds.add(heldId(wireLink));
    }
    if (heldIds.size > 0) {
        console.warn(
            `[server-link-language] bootstrap: ${heldIds.size} link(s) wait for a room key this agent ` +
            "does not hold yet; they are applied once it does",
        );
    }

    const inSnapshot = new Set(additions.map(store.hashLink));
    const removals = store
        .allLinks()
        .links.filter((link) => !inSnapshot.has(store.hashLink(link)) && !heldIds.has(wireLinkHash(link)));

    applyAndEmit({ additions, removals });
    if (rendered.revision) store.setRevision(rendered.revision);
    if (typeof rendered.sequence === "number") store.setSequence(rendered.sequence);
}

// ---------------------------------------------------------------------------
// HTTP catch-up (used both by sync() and by ws-client's onOpen/reconnect hook)
// ---------------------------------------------------------------------------

/**
 * Fetches everything since our last known sequence and applies it.
 * Idempotent by construction: `since` only advances after a diff is
 * applied (or held), so a WebSocket that already delivered everything
 * live simply gets back an empty `diffs[]` here — there's no separate
 * "skip because WS is connected" branch to get wrong. While links are held
 * for a key, it re-reads the key ring; it then retries the held links
 * against whatever ring it has. Returns what it emitted.
 */
export async function catchUp(): Promise<PerspectiveDiff> {
    const { config } = deps();
    const since = store.getSequence();
    const res = await withToken((token) => api.fetchSync(config, token, since));
    console.log(
        `[server-link-language] catchUp: since=${since}, received ${res.diffs.length} diff(s), ` +
        `revision=${res.revision}, sequence=${res.sequence}`,
    );

    const emitted: PerspectiveDiff = { additions: [], removals: [] };
    const collect = (diff: PerspectiveDiff) => {
        emitted.additions.push(...diff.additions);
        emitted.removals.push(...diff.removals);
    };
    for (const rawEntry of res.diffs) {
        const entry = normalizeSyncEntry(rawEntry, res.revision, res.sequence);
        collect(applyInboundWireDiff(entry.diff, entry.sequence, entry.revision));
    }

    if (res.diffs.length === 0 && res.revision) {
        store.setRevision(res.revision);
        store.setSequence(res.sequence);
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
