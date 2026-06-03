/**
 * # holograph-link
 *
 * AD4M LinkLanguage backed by the holograph substrate. All persistence,
 * networking, and op-DAG ordering live in `rust-executor/crates/holograph`
 * (sled `KvOpStore` + Kitsune2 `DynSpace` + the substrate-agnostic
 * `perspective-diff-algorithm`); this JS module is a thin facade that
 * delegates to the `holograph*` host imports.
 *
 * The Step-3 `HolographIntegrationQueue` already does cascade promotion
 * + multi-peer fallback + restart resume. The Step-4 `HolographSpace`
 * already does `inform_ops_stored` + `publish_ops` on local commits.
 * Both run inside Rust; this module does not poll, does not run
 * `setInterval`, does not walk peer revisions in JS — the subscribe
 * loop awaits the Rust-side mpsc receiver directly via
 * `holographNextEmitted`, so there's no JS-side scheduler.
 *
 * Spec: SPIKE.md §2.2 Step 5. Address scheme: `hash("@coasys/holograph-link@VERSION")`.
 */

import {
    defineLanguage,
    agentDid,
    agentSign,
    hash,
    languageStorageDirectory,
    languageAddress,
    emitPerspectiveDiff,
    emitSyncStateChange,
    emitTelepresenceSignal,
    holographCreateNeighborhood,
    holographCommit,
    holographRender,
    holographNextEmitted,
    holographJoinAgent,
    holographCurrentRevision,
    holographLatestRevision,
    holographCloseNeighborhood,
    EmittedOpWire,
    WireDiff,
} from "@coasys/ad4m-ldk";

// =============================================================================
// Module-level state
// =============================================================================

const VERSION = "0.1.0";

// Set by init(); used by every other capability method.
let myDid = "";
let handle: number | null = null;
let subscriberAbort: AbortController | null = null;

// Subscriber callbacks the runtime registers via the
// `linkSyncAdd*Callback` exports — same shape as p-diff-sync to keep
// the runtime dispatcher happy.
let linkCallback: ((diff: PerspectiveDiff) => void) | null = null;
let syncStateChangeCallback: ((state: string) => void) | null = null;
const telepresenceSignalCallbacks: ((signal: any, recipientDid?: string) => void)[] = [];

// Local agent membership — for `peers.setLocal`.
const localAgents = new Set<string>();

// =============================================================================
// Helpers
// =============================================================================

function assertHandle(): number {
    if (handle == null) {
        throw new Error("[holograph-link] init() must be called before any other Language method");
    }
    return handle;
}

/** Convert a Language-facing PerspectiveDiff into the substrate's WireDiff
 *  shape. Step 6e moved envelope (CBOR + timestamp + signature) construction
 *  to Rust, so the wire takes typed diff data directly. */
function toWireDiff(diff: PerspectiveDiff): WireDiff {
    return {
        additions: diff.additions || [],
        removals: diff.removals || [],
    };
}

function fromWireDiff(w: WireDiff): PerspectiveDiff {
    const out = new PerspectiveDiff();
    out.additions = w.additions || [];
    out.removals = w.removals || [];
    return out;
}

async function runSubscriberLoop(): Promise<void> {
    while (subscriberAbort && !subscriberAbort.signal.aborted) {
        try {
            const next: EmittedOpWire | null = await holographNextEmitted(assertHandle());
            if (!next) {
                // The Rust side awaits the underlying mpsc receiver so
                // a null return means the channel closed (i.e., the
                // neighborhood is being torn down). Exit the loop.
                return;
            }
            const diff = fromWireDiff(next.diff);
            if (linkCallback) linkCallback(diff);
            emitPerspectiveDiff(diff);
        } catch (e: any) {
            const msg = String(e && e.message ? e.message : e);
            if (msg.indexOf("__holographDelegate__") >= 0) {
                console.warn("[holograph-link] subscriber loop ending: " + msg);
                return;
            }
            console.error("[holograph-link] subscriber loop error:", e);
            return;
        }
    }
}

// =============================================================================
// PerspectiveDiff — same shape p-diff-sync ships
// =============================================================================

class PerspectiveDiff {
    additions: any[] = [];
    removals: any[] = [];
}

// =============================================================================
// Language spec
// =============================================================================

const language = defineLanguage({
    name: "@coasys/holograph-link",
    version: VERSION,
    isPublic: false,

    async init() {
        myDid = agentDid();
        const storageDir = languageStorageDirectory();
        // Stable per-Language space-id: the language address (which is
        // the canonical AD4M content-address hash over package metadata)
        // doubles as the K2 SpaceId.
        const spaceId = languageAddress() || hash(`@coasys/holograph-link@${VERSION}`);

        handle = await holographCreateNeighborhood(spaceId, storageDir);

        // Touch agentSign so the runtime keeps it warm — production
        // signing flows route through here once Step 6's commit path
        // takes a real signature.
        const _agentSign: typeof agentSign = agentSign;
        void _agentSign;

        // Join the local agent. The agent key is the DID bytes
        // base64-encoded; the Rust side maps it onto a K2 AgentId
        // (full plumbing arrives in Step 7).
        const didBytes = new TextEncoder().encode(myDid);
        let didB64 = "";
        for (let i = 0; i < didBytes.length; i++) didB64 += String.fromCharCode(didBytes[i]);
        didB64 = btoa(didB64);
        try {
            await holographJoinAgent(handle, didB64);
        } catch (e) {
            console.warn("[holograph-link] holographJoinAgent failed:", String(e));
        }

        // Spawn the subscriber loop on the next microtask so init()
        // returns promptly; the loop blocks on the mpsc receiver
        // inside Rust (Step 6).
        subscriberAbort = new AbortController();
        queueMicrotask(() => { runSubscriberLoop(); });
    },

    async teardown() {
        if (subscriberAbort) {
            subscriberAbort.abort();
            subscriberAbort = null;
        }
        if (handle != null) {
            try { await holographCloseNeighborhood(handle); } catch (_) { /* ignore */ }
            handle = null;
        }
        linkCallback = null;
        syncStateChangeCallback = null;
        telepresenceSignalCallbacks.length = 0;
        localAgents.clear();
        myDid = "";
    },

    interactions() { return []; },

    sync: {
        async sync() {
            // No-op sync: the Step-3 queue + Step-4 publish/fetch path
            // drives propagation in Rust. Returning an empty diff is
            // intentional and stable — the runtime calls sync() on a
            // schedule but we don't need it to do anything because the
            // subscriber loop pushes diffs in real time.
            if (syncStateChangeCallback) syncStateChangeCallback("Synced");
            return new PerspectiveDiff();
        },

        async render() {
            try {
                const result = await holographRender(assertHandle());
                return { links: result.links || [] };
            } catch (e) {
                // Step 5 stub path; the runtime tolerates an empty render.
                console.warn("[holograph-link] render fell back to empty:", String(e));
                return { links: [] };
            }
        },

        async currentRevision() {
            try {
                return await holographCurrentRevision(assertHandle());
            } catch (_) {
                return null;
            }
        },
    },

    commit: {
        async commit(diff: PerspectiveDiff) {
            // Rust side wraps the diff in an OpEnvelope (CBOR + timestamp
            // + signature) before storing. We just hand the typed diff
            // across the wire.
            return await holographCommit(assertHandle(), toWireDiff(diff));
        },
    },

    peers: {
        async remote() {
            // The Rust side exposes the K2 peer store. v1 returns DIDs
            // when Step 6 adds a `holographListPeers` host fn; until
            // then return an empty list (the AD4M runtime tolerates
            // this — see p-diff-sync's own `peersRemote` for the same
            // "no peers known yet" branch).
            return [];
        },

        async setLocal(agents: string[]) {
            for (const did of agents) localAgents.add(did);
            // Step 6: pipe these through to holograph_wires::join_agent
            // for each. For the stub we just record them locally.
        },
    },

    telepresence: {
        async setOnlineStatus(_status: unknown) {
            // Step 6 wires SpaceHandler::send_notify; Step 5 is a no-op.
        },

        async getOnlineAgents() {
            return [];
        },

        async sendSignal(remoteDid: string, payload: unknown) {
            // Step 6 routes through Space::send_notify; emit locally
            // for the smoke test so the surface is exercisable.
            emitTelepresenceSignal({ author: myDid, data: payload, recipientDid: remoteDid });
            return { ok: true };
        },

        async sendBroadcast(payload: unknown) {
            emitTelepresenceSignal({ author: myDid, data: payload });
            return { ok: true };
        },

        async registerSignalCallback(callback: any) {
            telepresenceSignalCallbacks.push(callback);
        },
    },

    async handleHolochainSignal(_signal: any) {
        // holograph doesn't go through Holochain — no-op.
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
// Callback registration — read directly by the runtime, same as p-diff-sync
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

/**
 * Latest-revision accessor for the runtime — peeks at the substrate's
 * sled-backed `revisions` tree. Exported separately because the
 * defineLanguage capability shape doesn't include it; older AD4M
 * runtimes read it via this flat-export name.
 */
export async function perspectiveSyncLatestRevision(): Promise<string | null> {
    try {
        return await holographLatestRevision(assertHandle());
    } catch (_) {
        return null;
    }
}
