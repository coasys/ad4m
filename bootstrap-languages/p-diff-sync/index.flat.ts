/**
 * # Perspective Diff Sync — Flat Export Language
 * 
 * AD4M link language that syncs Perspectives via Holochain DNA.
 * This is a **flat export language** — no factory, no wrapper object.
 * Each exported function is called directly by the AD4M runtime.
 * 
 * ## How to write a flat export language
 * 
 * 1. Export `name` and `version` — required metadata
 * 2. Export `init()` — NEW: takes NO arguments. Context is accessed via:
 *    - globalThis.languageStorageDirectory() — returns storage directory path
 *    - globalThis.languageAddress() — returns this language's address
 *    - globalThis.languageSettings() — returns settings JSON string
 *    Delegates are on globalThis before init is called:
 *    - `globalThis.__agentProxy__` — agent identity & signing
 *    - `globalThis.__holochainDelegate__` — Holochain DNA registration & zome calls
 *    - `globalThis.__ad4mSignal__` — emit signals to the signal bus
 * 3. Export capability functions directly (expressionCreate, linkSyncSync, etc.)
 * 4. Export `teardown()` to clean up when the language is unloaded
 * 
 * That's it. No adapter objects. No factory. Just functions.
 * 
 * ## Exports
 * 
 * Lifecycle:     name, version, init, teardown, interactions
 * Link sync:     linkSyncSync, linkSyncCommit, linkSyncRender,
 *                linkSyncCurrentRevision, linkSyncOthers,
 *                linkSyncWritable, linkSyncPublic,
 *                linkSyncAddCallback, linkSyncRemoveCallback,
 *                linkSyncAddSyncStateChangeCallback, linkSyncSetLocalAgents
 * Telepresence:  telepresenceSetOnlineStatus, telepresenceGetOnlineAgents,
 *                telepresenceSendSignal, telepresenceSendBroadcast,
 *                telepresenceRegisterSignalCallback
 * Signal:        handleHolochainSignal
 */

import { BUNDLE, DNA_ROLE, ZOME_NAME } from './build/happ.js';
import { Mutex } from "https://esm.sh/v135/async-mutex@0.4.0";

// =============================================================================
// Required metadata
// =============================================================================

export const name = "@coasys/perspective-diff-sync";
export const version = "0.13.0-test-1";

// =============================================================================
// Module-level state (no class — just module vars)
// =============================================================================

const dnaRole = DNA_ROLE;
const zomeName = ZOME_NAME;
const dnaBundle = Buffer.from(BUNDLE, "base64");

// Holochain delegate (set in init)
let hc: any = null;

// Agent DID (set in init)
let myDid: string = "";

// Link sync state
let linkCallback: ((diff: any) => void) | null = null;
let syncStateChangeCallback: ((state: string) => void) | null = null;
let myRevision: string | null = null;

// Gossip peers: DID → { currentRevision, lastSeen }
const peers = new Map<string, { currentRevision: string | null; lastSeen: Date }>();

// Prevent concurrent sync/commit operations
const syncMutex = new Mutex();

// Count gossip rounds (log every 10th)
let gossipRound = 0;

// =============================================================================
// init — required lifecycle function
// NEW INTERFACE: init() takes NO arguments.
// Context is accessed via flat import functions:
// - languageStorageDirectory() — returns storage directory path
// - languageAddress() — returns this language's address (DID)
// - languageSettings() — returns settings JSON string
// Agent & Holochain are available on globalThis:
// - __agentProxy__ — agent identity & signing (legacy, still works)
// - __holochainDelegate__ — Holochain DNA registration & calls (legacy)
// =============================================================================

export async function init(): Promise<void> {
    // NEW: Get language context via flat import functions
    const storageDir = languageStorageDirectory();
    const languageAddress = languageAddress();
    const languageSettingsJson = languageSettings();
    const languageSettings = languageSettingsJson ? JSON.parse(languageSettingsJson) : {};

    // Delegates are already on globalThis — grab them once here
    const agent: any = (globalThis as any).__agentProxy__;
    const holochain: any = (globalThis as any).__holochainDelegate__;

    myDid = agent.did;
    hc = holochain;

    // Register the DNA with the Holochain conductor
    await hc.registerDNAs([{ nick: dnaRole, bundle: dnaBundle }]);
}

// =============================================================================
// teardown — required lifecycle function
// =============================================================================

export async function teardown(): Promise<void> {
    peers.clear();
    linkCallback = null;
    syncStateChangeCallback = null;
    myRevision = null;
    gossipRound = 0;
    hc = null;
    myDid = "";
}

// =============================================================================
// interactions — what actions this language can perform
// =============================================================================

export function interactions(): any[] {
    return [];
}

// =============================================================================
// LINK SYNC CAPABILITY
// =============================================================================

/**
 * Sync with the network — fetches latest state from all peers
 * and returns the current diff (additions + removals).
 */
export async function linkSyncSync(): Promise<PerspectiveDiff> {
    await ensureDidLink();
    await acquireRevision();
    await gossip();
    return new PerspectiveDiff();
}

/**
 * Commit a diff (additions and removals) to the network.
 * Returns the new revision hash.
 */
export async function linkSyncCommit(diff: PerspectiveDiff): Promise<string> {
    const prepDiff = {
        additions: diff.additions.map(prepareLink),
        removals: diff.removals.map(prepareLink),
    };

    // Retry up to 5 times on transient failures
    for (let attempt = 0; attempt < 5; attempt++) {
        try {
            const revision: string = await hc.call(dnaRole, zomeName, "commit", {
                diff: prepDiff,
                my_did: myDid,
            });
            if (!revision || revision.length === 0) throw new Error("empty revision");
            myRevision = revision;
            return revision;
        } catch (e) {
            if (attempt < 4) await sleep(100 * (attempt + 1));
            else throw e;
        }
    }
    throw new Error("unreachable");
}

/**
 * Return the current full state as a list of links (for snapshot/rendering).
 */
export async function linkSyncRender(): Promise<{ links: any[] }> {
    const res: any = await hc.call(dnaRole, zomeName, "render", null);
    return { links: res?.links || [] };
}

/** Current revision hash, or null if never synced. */
export function linkSyncCurrentRevision(): string | null {
    return myRevision;
}

/** List of other agents this agent is synced with. */
export async function linkSyncOthers(): Promise<string[]> {
    return Array.from(peers.keys());
}

export function linkSyncWritable(): boolean { return true; }
export function linkSyncPublic(): boolean { return false; }

/** Register a callback for incoming link diffs (from other agents). */
export function linkSyncAddCallback(callback: (diff: PerspectiveDiff) => void): number {
    linkCallback = callback;
    return 1;
}

export function linkSyncRemoveCallback(callback: (diff: PerspectiveDiff) => void): number {
    if (linkCallback === callback) linkCallback = null;
    return 1;
}

/** Register a callback for sync state changes (e.g. "Synced", "NotSynced"). */
export function linkSyncAddSyncStateChangeCallback(callback: (state: string) => void): number {
    syncStateChangeCallback = callback;
    return 1;
}

/** Tell the DNA which agent we are locally. */
export async function linkSyncSetLocalAgents(): Promise<void> {
    await hc.call(dnaRole, zomeName, "add_active_agent_link", null);
}

// =============================================================================
// TELEPRESENCE CAPABILITY
// =============================================================================

export async function telepresenceSetOnlineStatus(status: unknown): Promise<void> {
    await hc.call(dnaRole, zomeName, "set_online_status", status);
}

export async function telepresenceGetOnlineAgents(): Promise<any[]> {
    const active: any[] = await hc.call(dnaRole, zomeName, "get_active_agents", null);
    const calls = active.map((agent: any) => ({
        dnaNick: dnaRole,
        zomeName,
        fnName: "get_agents_status",
        params: agent,
    }));
    return await hc.callAsync(calls, 1000);
}

export async function telepresenceSendSignal(remoteDid: string, payload: unknown): Promise<object> {
    return await hc.call(dnaRole, zomeName, "send_signal", {
        remote_agent_did: remoteDid,
        payload,
    });
}

export async function telepresenceSendBroadcast(payload: unknown): Promise<object> {
    return await hc.call(dnaRole, zomeName, "send_broadcast", payload);
}

export async function telepresenceRegisterSignalCallback(callback: any): Promise<void> {
    // Signal registration is handled by the runtime via handleHolochainSignal
}

// =============================================================================
// SIGNAL HANDLING
// =============================================================================

/**
 * Called by the AD4M runtime when a Holochain signal arrives for this DNA.
 * Routes the signal to the link callback or updates peer state.
 */
export function handleHolochainSignal(signal: any): void {
    const { reference_hash, reference, broadcast_author } = signal.payload || {};

    if (broadcast_author && reference_hash) {
        // Signal from another agent with their current revision — update peer
        peers.set(broadcast_author, { currentRevision: reference_hash, lastSeen: new Date() });
    } else if (reference && linkCallback) {
        // Signal contains link data (came from a pull response)
        linkCallback(signal.payload);
    }
}

// =============================================================================
// Private helpers
// =============================================================================

/** Create a DID anchor link for this agent in the DNA (idempotent). */
async function ensureDidLink(): Promise<void> {
    try {
        await hc.call(dnaRole, zomeName, "create_did_link", { did: myDid });
    } catch (_) {
        // Already exists — ignore
    }
}

/** Fetch current revision from the DNA and update local state. */
async function acquireRevision(): Promise<void> {
    const release = await syncMutex.acquire();
    try {
        const rev: Uint8Array = await hc.call(dnaRole, zomeName, "sync", myDid);
        if (rev instanceof Uint8Array) {
            myRevision = new TextDecoder().decode(rev);
        }
    } catch (e) {
        console.error("[p-diff-sync] sync error:", e);
    } finally {
        release();
    }
}

/** Exchange revisions with peers and pull any missing state. */
async function gossip(): Promise<void> {
    gossipRound++;
    const release = await syncMutex.acquire();
    try {
        // Mark stale peers as lost (no heartbeat in 10s)
        for (const [did, info] of peers) {
            if (Date.now() - info.lastSeen.getTime() > 10_000) {
                peers.delete(did);
            }
        }

        // Determine if we're the scribe (lexically first peer)
        const allPeers = [...peers.keys(), myDid].sort();
        const isScribe = allPeers[0] === myDid;

        // Collect all peer revisions
        const revisions = new Set<string>();
        for (const { currentRevision } of peers.values()) {
            if (currentRevision) revisions.add(currentRevision);
        }

        const myRev = myRevision;
        const sameRevisions = [...revisions].filter(r => r === myRev);
        const differentRevisions = [...revisions].filter(r => r !== myRev);

        // Notify on sync state change
        if (syncStateChangeCallback) {
            const state = sameRevisions.length > 0 || differentRevisions.length > 0
                ? (sameRevisions.length <= differentRevisions.length
                    ? "LinkLanguageInstalledButNotSynced"
                    : "Synced")
                : "Installed";
            await syncStateChangeCallback(state);
        }

        // Pull any revisions we don't have
        for (const hash of revisions) {
            if (hash === myRev) continue;
            try {
                const result: any = await hc.call(dnaRole, zomeName, "pull", { hash, is_scribe });
                if (result?.current_revision) {
                    myRevision = result.current_revision;
                }
            } catch (e) {
                console.error("[p-diff-sync] pull error:", e);
            }
        }

        // Log every 10th gossip round
        if (gossipRound === 10) {
            console.log(gossipSummary(allPeers, isScribe, revisions));
            gossipRound = 0;
        }
    } finally {
        release();
    }
}

function gossipSummary(peersList: string[], isScribe: boolean, revisions: Set<string>): string {
    return `
==========
GOSSIP
--
me: ${myDid}
is scribe: ${isScribe}
--
others: ${peersList.filter(p => p !== myDid).join(', ') || '(none)'}
--
${[...peers.entries()].map(([did, { currentRevision, lastSeen }]) =>
        `${did}: ${currentRevision} (${lastSeen.toISOString()})`).join('\n')}
--
revisions: ${[...revisions].join(', ') || '(none)'}
==========`;
}

/** Normalize a link expression for commit (null string → null). */
function prepareLink(link: any): object {
    const data = { ...link.data };
    for (const key of ['source', 'target', 'predicate'] as const) {
        if (data[key] === "") data[key] = null;
    }
    return { ...link, data };
}

function sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
}

// =============================================================================
// PerspectiveDiff — returned by linkSyncSync()
// =============================================================================

class PerspectiveDiff {
    additions: any[] = [];
    removals: any[] = [];
}
