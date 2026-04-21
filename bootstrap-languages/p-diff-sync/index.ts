/**
 * # Perspective Diff Sync
 *
 * AD4M link language that syncs Perspectives via Holochain DNA.
 * Authored against the @coasys/ad4m-ldk `defineLanguage` shape
 * (spec §9). The capability groups nest under `perspectiveSync`,
 * `perspectiveCommit`, `peers`, `telepresence`; the flat named
 * exports the runtime dispatcher reads are projected at the bottom.
 */

import { BUNDLE, DNA_ROLE, ZOME_NAME } from './build/happ.js';
import { Mutex } from "https://esm.sh/v135/async-mutex@0.4.0";
import {
    defineLanguage,
    agentDid,
    holochainRegisterDnas,
    holochainCall,
    holochainCallAsync,
    languageStorageDirectory,
    languageAddress,
    languageSettings,
} from "@coasys/ad4m-ldk";

// =============================================================================
// Module-level state
// =============================================================================

const dnaRole = DNA_ROLE;
const zomeName = ZOME_NAME;
const dnaBundle = Buffer.from(BUNDLE, "base64");

function hashToHex(hash: any): string | null {
    if (!hash) return null;
    if (typeof hash === 'string') return hash;
    const bytes = hash instanceof Uint8Array ? hash : (hash.length != null ? Array.from(hash) : Object.values(hash));
    return Array.from(bytes as number[]).map((b: number) => b.toString(16).padStart(2, '0')).join('');
}

let myDid: string = "";
let linkCallback: ((diff: any) => void) | null = null;
let syncStateChangeCallback: ((state: string) => void) | null = null;
const telepresenceSignalCallbacks: ((signal: any, recipientDid?: string) => void)[] = [];
let myRevision: string | null = null;

const peers = new Map<string, { currentRevision: string | null; originalHash: any; lastSeen: Date }>();
const localAgents = new Set<string>();
const didLinksCreated = new Set<string>();

const syncMutex = new Mutex();
let gossipRound = 0;

// =============================================================================
// Private helpers
// =============================================================================

async function ensureDidLink(): Promise<void> {
    const dids = localAgents.size > 0 ? Array.from(localAgents) : (myDid ? [myDid] : []);
    for (const did of dids) {
        if (didLinksCreated.has(did)) continue;
        try {
            await holochainCall(dnaRole, zomeName, "create_did_pub_key_link", did);
            didLinksCreated.add(did);
        } catch (_) {
            // Already exists — ignore
        }
    }
}

async function acquireRevision(): Promise<void> {
    const release = await syncMutex.acquire();
    try {
        const rev: any = await holochainCall(dnaRole, zomeName, "sync", myDid);
        if (rev) {
            myRevision = hashToHex(rev);
        }
    } catch (e) {
        console.error("[p-diff-sync] sync error:", e);
    } finally {
        release();
    }
}

async function gossip(): Promise<void> {
    gossipRound++;
    const release = await syncMutex.acquire();
    try {
        try {
            const syncResult: any = await holochainCall(dnaRole, zomeName, "sync", myDid);
            if (syncResult) {
                myRevision = hashToHex(syncResult);
            }
        } catch (e) {
            console.error("[p-diff-sync] sync zome call error:", e);
        }

        try {
            const activeAgents: any[] = (await holochainCall(dnaRole, zomeName, "get_online_agents", null)) as any[];
            if (activeAgents && activeAgents.length > 0) {
                for (const agent of activeAgents) {
                    const did = agent.did || agent.agent;
                    if (did && did !== myDid) {
                        const existing = peers.get(did);
                        peers.set(did, {
                            currentRevision: existing?.currentRevision || null,
                            originalHash: existing?.originalHash || null,
                            lastSeen: new Date(),
                        });
                    }
                }
            }
        } catch (_e) {
            // get_online_agents may not be available
        }

        for (const [did, info] of peers) {
            if (Date.now() - info.lastSeen.getTime() > 30_000) {
                peers.delete(did);
            }
        }

        const allPeers = [...peers.keys(), myDid].sort();
        const isScribe = allPeers[0] === myDid;

        const revisionMap = new Map<string, any>();
        for (const { currentRevision, originalHash } of peers.values()) {
            if (currentRevision) revisionMap.set(currentRevision, originalHash);
        }
        const revisionHexes = new Set<string>(revisionMap.keys());

        const myRev = myRevision;
        const sameRevisions = [...revisionHexes].filter(r => r === myRev);
        const differentRevisions = [...revisionHexes].filter(r => r !== myRev);

        if (syncStateChangeCallback) {
            let state: string;
            if (peers.size === 0) {
                state = "Synced";
            } else if (differentRevisions.length > sameRevisions.length) {
                state = "LinkLanguageInstalledButNotSynced";
            } else {
                state = "Synced";
            }
            await syncStateChangeCallback(state);
        }

        if (peers.size > 0 && revisionHexes.size === 0) {
            try {
                const latestRef: any = await holochainCall(dnaRole, zomeName, "latest_revision", null);
                console.log("[p-diff-sync] gossip: latest_revision result:", latestRef ? "found" : "null", JSON.stringify(latestRef)?.substring(0, 200));
                const latestHex = latestRef ? hashToHex(latestRef.hash) : null;
                if (latestHex && latestHex !== myRev) {
                    console.log("[p-diff-sync] gossip: network latest_revision=", latestHex?.substring(0, 40));
                    revisionHexes.add(latestHex);
                    revisionMap.set(latestHex, latestRef.hash);
                }
            } catch (e) {
                console.error("[p-diff-sync] latest_revision fallback error:", e);
            }
        }

        for (const hex of revisionHexes) {
            if (hex === myRev) continue;
            const pullHash = revisionMap.get(hex) || hex;
            try {
                const result: any = await holochainCall(dnaRole, zomeName, "pull", { hash: pullHash, is_scribe: isScribe });
                if (result?.current_revision) {
                    myRevision = hashToHex(result.current_revision);
                }
                if (result?.diff && linkCallback) {
                    linkCallback(result.diff);
                }
            } catch (e) {
                console.error("[p-diff-sync] pull error:", e);
            }
        }

        if (gossipRound === 10) {
            console.log(gossipSummary(allPeers, isScribe, revisionHexes));
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
// Language spec
// =============================================================================

const language = defineLanguage({
    name: "@coasys/perspective-diff-sync",
    version: "0.13.0-test-1",

    isPublic: false,

    async init() {
        // Touch the language-context imports so the runtime surfaces them
        // on load, even though p-diff-sync does not consume their values.
        const _storageDir = languageStorageDirectory();
        const _langAddress = languageAddress();
        const _settingsJson = languageSettings();

        myDid = agentDid();

        await holochainRegisterDnas([{ nick: dnaRole, bundle: dnaBundle } as any]);
    },

    async teardown() {
        peers.clear();
        linkCallback = null;
        syncStateChangeCallback = null;
        telepresenceSignalCallbacks.length = 0;
        myRevision = null;
        gossipRound = 0;
        myDid = "";
    },

    interactions() { return []; },

    sync: {
        async sync() {
            await ensureDidLink();
            await acquireRevision();
            await gossip();
            return new PerspectiveDiff();
        },

        async render() {
            const res: any = await holochainCall(dnaRole, zomeName, "render", null);
            return { links: res?.links || [] };
        },

        currentRevision() {
            return myRevision;
        },
    },

    commit: {
        async commit(diff: any) {
            const prepDiff = {
                additions: diff.additions.map(prepareLink),
                removals: diff.removals.map(prepareLink),
            };

            for (let attempt = 0; attempt < 5; attempt++) {
                try {
                    const revision: any = await holochainCall(dnaRole, zomeName, "commit", {
                        diff: prepDiff,
                        my_did: myDid,
                    });
                    if (!revision) throw new Error("empty revision");
                    const hex = hashToHex(revision) || "";
                    myRevision = hex;
                    return hex;
                } catch (e) {
                    if (attempt < 4) await sleep(100 * (attempt + 1));
                    else throw e;
                }
            }
            throw new Error("unreachable");
        },
    },

    peers: {
        async remote() {
            try {
                const all: string[] = (await holochainCall(dnaRole, zomeName, "get_others", null)) as string[];
                return all || [];
            } catch (e) {
                console.error("[p-diff-sync] peersRemote error:", e);
                return [];
            }
        },

        async setLocal(agents: string[]) {
            for (const did of agents) localAgents.add(did);
            for (const did of agents) {
                if (didLinksCreated.has(did)) continue;
                try {
                    await holochainCall(dnaRole, zomeName, "create_did_pub_key_link", did);
                    didLinksCreated.add(did);
                } catch (e) {
                    console.error(`[p-diff-sync] create_did_pub_key_link failed for ${did}:`, e);
                }
            }
        },
    },

    telepresence: {
        async setOnlineStatus(status: unknown) {
            await holochainCall(dnaRole, zomeName, "set_online_status", status);
        },

        async getOnlineAgents() {
            const active: any[] = (await holochainCall(dnaRole, zomeName, "get_active_agents", null)) as any[];
            const calls = active.map((agent: any) => ({
                dnaNick: dnaRole,
                zomeName,
                fnName: "get_agents_status",
                params: agent,
            }));
            // holochainCallAsync exposes a single-call wrapper; loop one-at-a-time to preserve semantics.
            const results: any[] = [];
            for (const call of calls) {
                results.push(await holochainCallAsync(call.dnaNick, call.zomeName, call.fnName, call.params));
            }
            return results;
        },

        async sendSignal(remoteDid: string, payload: unknown) {
            return (await holochainCall(dnaRole, zomeName, "send_signal", {
                remote_agent_did: remoteDid,
                payload,
            })) as object;
        },

        async sendBroadcast(payload: unknown) {
            return (await holochainCall(dnaRole, zomeName, "send_broadcast", payload)) as object;
        },

        async registerSignalCallback(callback: any) {
            telepresenceSignalCallbacks.push(callback);
        },
    },

    async handleHolochainSignal(signal: any) {
        const payload = signal.payload || {};

        if (payload.reference && payload.reference_hash && payload.broadcast_author) {
            peers.set(payload.broadcast_author, {
                currentRevision: hashToHex(payload.reference_hash),
                originalHash: payload.reference_hash,
                lastSeen: new Date(),
            });
            return;
        }

        if (payload.additions || payload.removals) {
            if (linkCallback) {
                linkCallback(payload);
            }
            return;
        }

        if (payload.recipient_did) {
            const perspectiveExpression = {
                author: payload.author,
                data: payload.data,
                timestamp: payload.timestamp,
                proof: payload.proof,
            };
            for (const cb of telepresenceSignalCallbacks) {
                await cb(perspectiveExpression, payload.recipient_did);
            }
            return;
        }

        if (payload.author && payload.data) {
            for (const cb of telepresenceSignalCallbacks) {
                await cb(payload);
            }
            return;
        }
    },
});

export const {
    name,
    version,
    init,
    teardown,
    interactions,
    isPublic,
    perspectiveSyncSync,
    perspectiveSyncRender,
    perspectiveSyncCurrentRevision,
    perspectiveCommit,
    peersRemote,
    peersSetLocal,
    telepresenceSetOnlineStatus,
    telepresenceGetOnlineAgents,
    telepresenceSendSignal,
    telepresenceSendBroadcast,
    telepresenceRegisterSignalCallback,
    handleHolochainSignal,
} = language;

// =============================================================================
// Phase 0 transitional: callback registration — not in v1 capability spec,
// read directly by the runtime.
// =============================================================================

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
// PerspectiveDiff — returned by perspectiveSync.sync()
// =============================================================================

class PerspectiveDiff {
    additions: any[] = [];
    removals: any[] = [];
}
