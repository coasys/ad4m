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
 * 3. Export capability functions directly (perspectiveCommit, perspectiveSyncSync, etc.)
 * 4. Export `teardown()` to clean up when the language is unloaded
 * 
 * That's it. No adapter objects. No factory. Just functions.
 * 
 * ## Exports
 *
 * Lifecycle:        name, version, isPublic, init, teardown, interactions
 * Perspective sync: perspectiveSyncSync, perspectiveSyncRender,
 *                   perspectiveSyncCurrentRevision
 * Perspective commit: perspectiveCommit
 * Peers:            peersRemote, peersSetLocal
 * Telepresence:     telepresenceSetOnlineStatus, telepresenceGetOnlineAgents,
 *                   telepresenceSendSignal, telepresenceSendBroadcast,
 *                   telepresenceRegisterSignalCallback
 * Signal:           handleHolochainSignal
 *
 * NB: link callback registration is gone — diffs are emitted via the
 * runtime's `emitPerspectiveDiff` import. Sync-state changes use
 * `emitSyncStateChange`. Phase B wires the runtime side.
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
    // Pull language context via flat import functions installed by the
    // executor on globalThis (spec §7.3). Pre-existing bug fixed: the
    // local consts no longer shadow the import functions.
    const storageDir = (globalThis as any).languageStorageDirectory();
    const langAddress = (globalThis as any).languageAddress();
    const settingsJson = (globalThis as any).languageSettings();
    const settings = settingsJson ? JSON.parse(settingsJson) : {};

    // Delegates are already on globalThis — grab them once here.
    // (Phase B will replace direct globalThis access with @coasys/ad4m-ldk
    // typed wrappers once the bootstrap bundler can resolve workspace
    // packages — currently the Deno esbuild plugin can't.)
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
// LIFECYCLE-LEVEL PRIVACY HINT
// =============================================================================

export function isPublic(): boolean { return false; }

// =============================================================================
// PERSPECTIVE-SYNC + PERSPECTIVE-COMMIT CAPABILITIES
// =============================================================================

/**
 * Sync with the network — fetches latest state from all peers
 * and returns the current diff (additions + removals).
 */
export async function perspectiveSyncSync(): Promise<PerspectiveDiff> {
    await ensureDidLink();
    await acquireRevision();
    await gossip();
    return new PerspectiveDiff();
}

/**
 * Commit a diff (additions and removals) to the network.
 * Returns the new revision hash.
 */
export async function perspectiveCommit(diff: PerspectiveDiff): Promise<string> {
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
export async function perspectiveSyncRender(): Promise<{ links: any[] }> {
    const res: any = await hc.call(dnaRole, zomeName, "render", null);
    return { links: res?.links || [] };
}

/** Current revision hash, or null if never synced. */
export function perspectiveSyncCurrentRevision(): string | null {
    return myRevision;
}

// =============================================================================
// PEERS CAPABILITY
// =============================================================================

/** List of other (non-local) agents this language sees in the network. */
export async function peersRemote(): Promise<string[]> {
    return Array.from(peers.keys());
}

/** Tell the DNA which agents we represent locally. */
export async function peersSetLocal(_agents: string[]): Promise<void> {
    await hc.call(dnaRole, zomeName, "add_active_agent_link", null);
}

// -----------------------------------------------------------------------------
// Phase 0 transitional: callback registration is still consumed by the Rust
// runtime via the legacy linksAdapter.addCallback path. These stubs let the
// runtime keep registering callbacks during Phase 0; Phase B replaces them
// with `emitPerspectiveDiff` / `emitSyncStateChange` runtime imports.
// -----------------------------------------------------------------------------

export function linkSyncAddCallback(callback: (diff: PerspectiveDiff) => void): number {
    linkCallback = callback;
    return 1;
}

export function linkSyncRemoveCallback(callback: (diff: PerspectiveDiff) => void): number {
    if (linkCallback === callback) linkCallback = null;
    return 1;
}

export function linkSyncAddSyncStateChangeCallback(callback: (state: string) => void): number {
    syncStateChangeCallback = callback;
    return 1;
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
            let state: string;
            if (peers.size === 0) {
                // Solo agent - no peers to sync with, so we're synced
                state = "Synced";
            } else if (differentRevisions.length > sameRevisions.length) {
                state = "LinkLanguageInstalledButNotSynced";
            } else {
                state = "Synced";
            }
            await syncStateChangeCallback(state);
        }

        // Pull any revisions we don't have
        for (const hash of revisions) {
            if (hash === myRev) continue;
            try {
                const result: any = await hc.call(dnaRole, zomeName, "pull", { hash, is_scribe: isScribe });
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
// Grouped default export — mirrors the @coasys/ad4m-ldk `defineLanguage`
// shape (spec §9). Bootstrap bundlers cannot yet resolve workspace pkgs,
// so the LDK is not imported directly; this object documents the
// authoring style and is consumed by any caller that prefers the grouped
// shape over the named flat exports above (the runtime dispatcher reads
// the named exports).
// =============================================================================

const lang = {
    name,
    version,
    isPublic,
    init,
    teardown,
    interactions,

    commit: { commit: perspectiveCommit },
    sync: {
        sync: perspectiveSyncSync,
        render: perspectiveSyncRender,
        currentRevision: perspectiveSyncCurrentRevision,
    },
    peers: {
        setLocal: peersSetLocal,
        remote: peersRemote,
    },
    telepresence: {
        setOnlineStatus: telepresenceSetOnlineStatus,
        getOnlineAgents: telepresenceGetOnlineAgents,
        sendSignal: telepresenceSendSignal,
        sendBroadcast: telepresenceSendBroadcast,
        registerSignalCallback: telepresenceRegisterSignalCallback,
    },
    handleHolochainSignal,
};

export default lang;

// =============================================================================
// PerspectiveDiff — returned by perspectiveSyncSync()
// =============================================================================

class PerspectiveDiff {
    additions: any[] = [];
    removals: any[] = [];
}
