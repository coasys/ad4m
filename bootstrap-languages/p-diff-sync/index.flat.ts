/**
 * # Perspective Diff Sync Language — Flat Export Format
 * 
 * AD4M link language for syncing Perspectives via Holochain DNA.
 * Uses flat export interface (no create() factory).
 * 
 * ## Flat Export Interface
 * 
 * - `init(contextJson)` — sets up adapters and registers DNA
 * - Link adapter functions: linkSyncSync, linkSyncCommit, linkSyncRender, etc.
 * - Telepresence functions: telepresenceSetOnlineStatus, telepresenceGetOnlineAgents, etc.
 * 
 * ## Context
 * 
 * init() receives serializable context as JSON string:
 * { storageDirectory, customSettings, languageAddress }
 * 
 * Non-serializable delegates available via globalThis:
 * - __agentProxy__ — agent identity & signing
 * - __holochainDelegate__ — Holochain DNA registration & zome calls
 * - __ad4mSignal__ — signal emission
 */

import { BUNDLE, DNA_ROLE, ZOME_NAME } from './build/happ.js';
import { Mutex } from "https://esm.sh/v135/async-mutex@0.4.0";

// =============================================================================
// Flat exports (primary interface)
// =============================================================================

export const name = "@coasys/perspective-diff-sync";
export const version = "0.13.0-test-1";

// =============================================================================
// Module-level state (replaces class instance fields)
// =============================================================================

let hcDna: any = null; // Holochain delegate (set in init)
let me = ""; // My DID (set in init)

// Link adapter state
let linkCallback: any = null;
let syncStateChangeCallback: any = null;
let myCurrentRevision: string | null = null;
let gossipLogCount = 0;

// Gossip peers: DID -> { currentRevision, lastSeen }
let peers = new Map<string, { currentRevision: string | null; lastSeen: Date }>();

// Global mutex for sync/commit operations
const generalMutex = new Mutex();

// =============================================================================
// init — sets up the language
// =============================================================================

export async function init(contextJson: string): Promise<void> {
    const context = JSON.parse(contextJson);
    me = globalThis.__agentProxy__.did as string;
    hcDna = (globalThis as any).__holochainDelegate__;

    // Register DNA bundle
    const bundle = Uint8Array.from(atob(BUNDLE), c => c.charCodeAt(0));
    await hcDna.registerDNAs([{ bundle, nick: DNA_ROLE }]);
}

// =============================================================================
// Link adapter — flat functions
// =============================================================================

export function linkSyncSync(): any {
    return sync();
}

export function linkSyncCommit(diff: any): string {
    return commit(diff);
}

export function linkSyncRender(): any {
    return render();
}

export function linkSyncCurrentRevision(): string | null {
    return myCurrentRevision;
}

export async function linkSyncOthers(): Promise<string[]> {
    return others();
}

export function linkSyncWritable(): boolean {
    return true;
}

export function linkSyncPublic(): boolean {
    return false;
}

export function linkSyncAddCallback(callback: any): number {
    linkCallback = callback;
    return 1;
}

export function linkSyncRemoveCallback(callback: any): number {
    if (linkCallback === callback) {
        linkCallback = null;
    }
    return 1;
}

export function linkSyncAddSyncStateChangeCallback(callback: any): number {
    syncStateChangeCallback = callback;
    return 1;
}

export function linkSyncSetLocalAgents(): any {
    return setLocalAgents();
}

// =============================================================================
// Telepresence adapter — flat functions
// =============================================================================

export async function telepresenceSetOnlineStatus(status: any): Promise<void> {
    await hcDna.call(DNA_ROLE, ZOME_NAME, "set_online_status", status);
}

export async function telepresenceGetOnlineAgents(): Promise<any[]> {
    //@ts-ignore
    const getActiveAgents = await hcDna.call(DNA_ROLE, ZOME_NAME, "get_active_agents", null);
    let calls = [];
    for (const activeAgent of getActiveAgents) {
        calls.push({dnaNick: DNA_ROLE, zomeName: ZOME_NAME, fnName: "get_agents_status", params: activeAgent});
    };
    return await hcDna.callAsync(calls, 1000);
}

export async function telepresenceSendSignal(remoteAgentDid: string, payload: any): Promise<object> {
    try {
        let res = await hcDna.call(DNA_ROLE, ZOME_NAME, "send_signal", {remote_agent_did: remoteAgentDid, payload});
        return res;
    } catch (error) {
        console.error(`🔔 SEND SIGNAL: Error sending signal: ${error}`);
        throw error;
    }
}

export async function telepresenceSendBroadcast(payload: any): Promise<object> {
    let res = await hcDna.call(DNA_ROLE, ZOME_NAME, "send_broadcast", payload);
    return res;
}

export async function telepresenceRegisterSignalCallback(callback: any): Promise<void> {
    // Telepresence signals are handled via the global signal handler
    // This is called to register a callback for telepresence signals
    // The actual registration happens via addCallback / handleHolochainSignal
}

// =============================================================================
// Interactions
// =============================================================================

export function interactions(): any[] {
    return [];
}

// =============================================================================
// Teardown
// =============================================================================

export async function teardown(): Promise<void> {
    peers.clear();
    linkCallback = null;
    syncStateChangeCallback = null;
    myCurrentRevision = null;
    gossipLogCount = 0;
}

// =============================================================================
// Private: local agent link management
// =============================================================================

async function setLocalAgents(): Promise<any> {
    if (!hcDna) return;
    try {
        // @ts-ignore
        const did = globalThis.__agentProxy__.did;
        if (!did) return;
        const result = await hcDna.call(
            DNA_ROLE,
            ZOME_NAME,
            "add_active_agent_link",
            null
        );
        return result;
    } catch (e) {
        console.error(`[p-diff-sync] Error in setLocalAgents:`, e);
        return null;
    }
}

// =============================================================================
// Private: sync logic (formerly sync())
// =============================================================================

async function sync(): Promise<any> {
    if (!hcDna) {
        console.warn("[p-diff-sync] sync() called but hcDna not set");
        return new PerspectiveDiff();
    }

    // Create DID link if needed
    try {
        const did = globalThis.__agentProxy__.did;
        if (did) {
            await hcDna.call(DNA_ROLE, ZOME_NAME, "create_did_link", { did });
        }
    } catch (e) {
        console.error(`[p-diff-sync LinkAdapter] Failed to create DID link for ${did}:`, e);
    }

    const release = await generalMutex.acquire();
    try {
        //@ts-ignore
        let current_revision = await hcDna.call(DNA_ROLE, ZOME_NAME, "sync", me);
        if (current_revision && current_revision instanceof Uint8Array) {
            myCurrentRevision = new TextDecoder().decode(current_revision);
        }
    } catch (e) {
        console.error("[p-diff-sync] sync() error", e);
    } finally {
        release();
    }
    await gossip();
    return new PerspectiveDiff();
}

// =============================================================================
// Private: gossip logic (formerly gossip())
// =============================================================================

async function gossip(): Promise<void> {
    gossipLogCount += 1;
    let lostPeers: string[] = [];

    const release = await generalMutex.acquire();
    try {
        peers.forEach((peerInfo, peer) => {
            if (peerInfo.lastSeen.getTime() + 10000 < new Date().getTime()) {
                lostPeers.push(peer);
            }
        });

        for (const peer of lostPeers) {
            peers.delete(peer);
        }

        // flatten the map into an array of peers
        let peersList = Array.from(peers.keys());
        peersList.push(me);

        // Lexically sort the peers
        peersList = peersList.sort();

        // If we are the first peer, we are the scribe
        let is_scribe = (peersList[0] == me);

        // Get a deduped set of all peer's current revisions
        let revisions = new Set<string>();
        for (const peerInfo of peers.values()) {
            if (peerInfo.currentRevision) revisions.add(peerInfo.currentRevision);
        }

        let sameRevisions: string[] = [];
        let differentRevisions: string[] = [];

        function generateRevisionStates() {
            sameRevisions = revisions.size == 0 ? [] : Array.from(revisions).filter((revision) => {
                return myCurrentRevision && (revision == myCurrentRevision);
            });
            if (myCurrentRevision) {
                sameRevisions.push(myCurrentRevision);
            }
            differentRevisions = revisions.size == 0 ? [] : Array.from(revisions).filter((revision) => {
                return myCurrentRevision && !(revision == myCurrentRevision);
            });
        }

        async function checkSyncStateCallback(callback: any) {
            if (sameRevisions.length > 0 || differentRevisions.length > 0) {
                if (sameRevisions.length <= differentRevisions.length) {
                    await callback(PerspectiveState.LinkLanguageInstalledButNotSynced);
                } else {
                    await callback(PerspectiveState.Synced);
                }
            }
        }

        generateRevisionStates();

        //@ts-ignore
        await checkSyncStateCallback(syncStateChangeCallback);

        for (const hash of Array.from(revisions)) {
            if (!hash) continue;
            if (myCurrentRevision && (hash == myCurrentRevision)) continue;

            let pullResult = await hcDna.call(DNA_ROLE, ZOME_NAME, "pull", {
                hash,
                is_scribe
            });

            if (pullResult) {
                if (pullResult.current_revision) {
                    let myRevision = pullResult.current_revision;
                    myCurrentRevision = myRevision;

                    //@ts-ignore
                    generateRevisionStates();
                    await checkSyncStateCallback(syncStateChangeCallback);
                }
            }
        }

        //Only show the gossip log every 10th iteration
        if (gossipLogCount == 10) {
            let others = await others();
            console.log(`
            ======
            GOSSIP
            --
            me: ${me}
            is scribe: ${is_scribe}
            --
            others: ${others.join(', ')}
            --
            ${Array.from(peers.entries()).map(([peer, peerInfo]) => {
                return `${peer}: ${peerInfo.currentRevision} ${peerInfo.lastSeen.toISOString()}\n`
            })}
            --
            revisions: ${Array.from(revisions).map((hash) => {
                return hash
            })}
            `);
            gossipLogCount = 0;
        }
    } catch (e) {
        console.error("[p-diff-sync] gossip() error", e);
    } finally {
        release();
    }
}

// =============================================================================
// Private: render (formerly render())
// =============================================================================

async function render(): Promise<any> {
    //@ts-ignore
    let res = await hcDna.call(DNA_ROLE, ZOME_NAME, "render", null);
    return { links: res.links || [] };
}

// =============================================================================
// Private: commit (formerly commit())
// =============================================================================

async function commit(diff: any): Promise<string> {
    const prep_diff = {
        additions: diff.additions.map((le: any) => prepareLinkExpression(le)),
        removals: diff.removals.map((le: any) => prepareLinkExpression(le))
    };

    let attempts = 0;
    const maxAttempts = 5;
    let lastError;

    while (attempts < maxAttempts) {
        try {
            let res = await hcDna.call(DNA_ROLE, ZOME_NAME, "commit", {
                diff: prep_diff,
                my_did: me
            });
            if (!res) {
                throw new Error("Got undefined from Holochain commit zome function");
            }
            if (res.length === 0 || res.byteLength === 0) {
                throw new Error("Got an empty buffer from Holochain commit zome function");
            }
            myCurrentRevision = res;
            return res;
        } catch (e) {
            lastError = e;
            attempts++;
            if (attempts < maxAttempts) {
                console.warn(`[p-diff-sync] commit() attempt ${attempts} failed, retrying...`, e);
                await new Promise(resolve => setTimeout(resolve, 100 * attempts));
            }
        }
    }

    console.error(`[p-diff-sync] commit() failed after ${maxAttempts} attempts`, lastError);
    throw lastError;
}

// =============================================================================
// Private: others (formerly others())
// =============================================================================

async function others(): Promise<string[]> {
    let othersList = Array.from(peers.keys());
    return othersList;
}

// =============================================================================
// Private: handleHolochainSignal (called by bootstrap signal handler)
// =============================================================================

export function handleHolochainSignal(signal: any): void {
    const { reference_hash, reference, broadcast_author } = signal.payload;

    // Check if this signal came from another agent & contains a reference and reference_hash
    if (reference && reference_hash && broadcast_author) {
        try {
            peers.set(broadcast_author, { currentRevision: reference_hash, lastSeen: new Date() });
        } catch (e) {
            console.error("[p-diff-sync] handleHolochainSignal error setting peer:", e);
        }
    } else {
        // This signal only contains link data — came from us in a pull
        if (linkCallback) {
            linkCallback(signal.payload);
        }
    }
}

// =============================================================================
// Private: prepare link expression (helper)
// =============================================================================

function prepareLinkExpression(link: any): object {
    const data = Object.assign({}, link);
    if (data.data.source == "") data.data.source = null;
    if (data.data.target == "") data.data.target = null;
    if (data.data.predicate == "") data.data.predicate = null;
    if (data.data.source == undefined) data.data.source = null;
    if (data.data.target == undefined) data.data.target = null;
    if (data.data.predicate == undefined) data.data.predicate = null;
    return data;
}

// =============================================================================
// Private: PerspectiveState enum
// =============================================================================

const PerspectiveState = {
    Installed: "Installed",
    Synced: "Synced",
    Initializing: "Initializing",
    LinkLanguageInstalledButNotSynced: "LinkLanguageInstalledButNotSynced",
    Error: "Error",
};

// =============================================================================
// Private: PerspectiveDiff stub (returned by sync)
// =============================================================================

class PerspectiveDiff {
    additions: any[] = [];
    removals: any[] = [];
}
