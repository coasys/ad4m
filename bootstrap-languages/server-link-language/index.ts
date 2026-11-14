/**
 * # Server Link Language
 *
 * AD4M link language that syncs a perspective through a self-hosted
 * `link-server` instance (protocol 2) over HTTP (auth, commit, catch-up sync,
 * membership, peers, E2E key exchange) and native WebSocket (real-time diff
 * and membership push, telepresence, presence). Implements every link-language
 * capability: perspective-commit, perspective-sync, perspective-query,
 * peers, and telepresence.
 *
 * The room is the space's: its id is `roomIdOf(CREATOR, UID)`, and the
 * creator's first membership commit makes it (genesis). Who is in the room is
 * the executor's membership of the space (`membershipLinks`,
 * `membershipAdmits`); the language holds no membership state.
 *
 * See README.md for architecture and AGENTS.md for operational notes /
 * known limitations.
 */

import { defineLanguage, hash } from "@coasys/ad4m-ldk";

import type { KeysResponse, LinkExpression, PerspectiveDiff } from "./src/types.js";
import * as store from "./src/store.js";
import * as api from "./src/api.js";
import * as auth from "./src/auth.js";
import * as syncModule from "./src/sync.js";
import * as telepresenceModule from "./src/telepresence.js";
import { WsClient, CLOSE_KEY_REMOVED, CLOSE_ROOM_SUSPENDED } from "./src/ws-client.js";
import { buildKeyRing, type KeyRing } from "./src/encryption.js";
import { isMembershipLink, roomIdOf } from "./src/room.js";
import {
    afterFailedKeyRead,
    grantMissingRoomKeys,
    mintsRoomKey,
    rotateRoomKey,
    type KeyRingStatus,
} from "./src/room-keys.js";

import {
    initAdapters,
    getAgent,
    getConfig,
    getRuntime,
    resetAdapters,
} from "./src/adapters.js";

import {
    DenoAgentAdapter,
    DenoRuntimeAdapter,
    DenoStorageAdapter,
    DenoTransport,
    DenoWebSocketFactory,
} from "./src/adapters-deno.js";

// ---------------------------------------------------------------------------
// Template Variables
// ---------------------------------------------------------------------------
// Replaced at publish time by the executor. See README.md § Template
// Variables and possibleTemplateParams below.

//!@ad4m-template-variable
const SERVER_URL = "https://link.ad4m.dev";

//!@ad4m-template-variable
const UID = "<to-be-filled>";

/** The person DID (`did:scid:coasys:1:…`) that made the space: with UID, the room's genesis. */
//!@ad4m-template-variable
const CREATOR = "<to-be-filled>";

// ---------------------------------------------------------------------------
// Module state (fresh per perspective instance — see language-interface-spec.md §2)
// ---------------------------------------------------------------------------

let myDid: string = "";
let configured = false;
let localAgents: string[] = [];
let wsClient: WsClient | null = null;

/** The room's versioned key ring (version → room key). Null while this agent holds no room key it can open. */
let keyRing: KeyRing | null = null;
/**
 * Every room is end-to-end encrypted: plain commits wait for "ready".
 * - "pending": this agent holds no key it can open: none minted yet, the room
 *   does not admit this key, no member holding one has been online since it
 *   joined, or none of its boxes opens (a spoiled grant). An admitted member
 *   in this state mints one when the room has none (K2); otherwise it waits
 *   for a grant.
 * - "ready": key ring acquired and opened.
 * - "error": we could not get/open the keys.
 */
let keyRingStatus: KeyRingStatus = "pending";
/** Whether the room admitted this key at its last auth (the server decides it per request). */
let admitted = false;
/** Set once this run has minted a room key version (K2: once a run). */
let rotatedThisRun = false;
/** In-flight setupKeyRing promise for single-flight deduplication. */
let keyRingInflight: Promise<void> | null = null;
/** Monotonic lifecycle counter — incremented on teardown to invalidate
 *  in-flight async operations from a previous lifecycle. */
let lifecycleGen = 0;

function isPlaceholder(value: string): boolean {
    return !value || value === "<to-be-filled>";
}

function setupKeyRingCoalesced(): Promise<void> {
    if (!keyRingInflight) {
        const p = setupKeyRing().finally(() => {
            if (keyRingInflight === p) keyRingInflight = null;
        });
        keyRingInflight = p;
    }
    return keyRingInflight;
}

/** K1: whether the space admits a key now, as the executor's membership says. */
async function admits(did: string): Promise<boolean> {
    const answer = await getRuntime().membershipAdmits(did);
    return answer.admitted;
}

// ---------------------------------------------------------------------------
// Startup helpers
// ---------------------------------------------------------------------------

async function setupKeyRing(): Promise<void> {
    const gen = lifecycleGen;
    const config = getConfig();
    try {
        const token = await auth.getValidToken();
        let keysRes: KeysResponse;
        try {
            keysRes = await api.fetchRoomKeys(config, token);
        } catch (err) {
            if (err instanceof api.ApiError && err.status === 403) {
                // The room does not admit this key (yet): it holds no room key.
                if (gen !== lifecycleGen) return;
                admitted = false;
                keyRingStatus = "pending";
                keyRing = null;
                console.log("[server-link-language] the room does not admit this key yet — it holds no room key");
                return;
            }
            throw err;
        }
        if (gen !== lifecycleGen) return;
        if (keysRes.keys.length === 0) {
            keyRingStatus = "pending";
            keyRing = null;
            console.log(
                "[server-link-language] this agent holds no room key yet — plain commits wait until a member " +
                "mints one or a member holding one grants it",
            );
            return;
        }
        // Reading the seed throws while this executor cannot derive it (a locked agent):
        // then no box is judged spoiled, and the read is tried again later.
        const seed = await getAgent().roomKeySeed();
        const { ring, spoiled } = buildKeyRing(keysRes.keys, seed);
        // A spoiled box would block its version for good: drop it, and a member holding the
        // version grants it again.
        for (const version of spoiled) {
            await api.deleteRoomKey(config, token, version).catch((err) =>
                console.error(`[server-link-language] could not drop the spoiled box for version ${version}:`, err),
            );
        }
        if (gen !== lifecycleGen) return;
        keyRing = ring;
        if (keyRing.size === 0) {
            // Boxes sealed to this agent, none of which open (spoiled grants):
            // the same as holding none.
            keyRingStatus = "pending";
            keyRing = null;
            console.log("[server-link-language] no room key version opens for this agent — waiting for a new one");
            return;
        }
        keyRingStatus = "ready";
        const versions = [...keyRing.keys()].sort((a, b) => a - b);
        console.log(`[server-link-language] E2E key ring acquired (${versions.length} version(s): ${versions.join(", ")})`);
    } catch (err) {
        if (gen !== lifecycleGen) return;
        ({ status: keyRingStatus, ring: keyRing } = afterFailedKeyRead(keyRingStatus, keyRing));
        console.error(
            keyRingStatus === "ready"
                ? "[server-link-language] key ring refresh failed — keeping the key ring already loaded:"
                : "[server-link-language] failed to acquire/decrypt E2E key ring — " +
                  "commits wait until this resolves:",
            err,
        );
        return;
    }
    // Links that arrived before this key are held, not lost: open them now.
    try {
        syncModule.retryHeld();
    } catch (err) {
        console.error("[server-link-language] opening held links failed:", err);
    }
}

/** Authenticates again, so `admitted` is what the room says now. */
async function refreshAdmitted(): Promise<void> {
    auth.invalidateSession();
    const session = await auth.getValidSession();
    admitted = session.admitted;
}

/**
 * K2: an admitted member holding no room key mints version 1 when the room has
 * none (`mintsRoomKey`), once a run. A member `/keys/missing` names (the room has
 * versions it lacks) waits for a grant instead.
 */
async function mintRoomKeyIfNone(): Promise<void> {
    if (keyRingStatus === "ready" || rotatedThisRun) return;
    await setupKeyRingCoalesced();
    if (keyRingStatus !== "pending" || !admitted) return;
    const token = await auth.getValidToken();
    const { membersNeedingHistoricalKeys } = await api.fetchMissingKeys(getConfig(), token);
    if (!mintsRoomKey({ status: keyRingStatus, rotatedThisRun, admitted }, membersNeedingHistoricalKeys, myDid)) return;
    console.log("[server-link-language] the room has no key yet — minting version 1");
    await performRotation([]);
}

/**
 * If this agent holds room key versions, seal them to every admitted key
 * lacking them that the space admits (K1). Any member does this: a joiner gets
 * the room's history from whichever member holding it is online first. Runs
 * after setupKeyRing, after every sync and on peer-joined events; the server
 * rate-limits the calls.
 */
async function performKeyGrants(): Promise<void> {
    if (!keyRing || keyRing.size === 0) return;
    try {
        const token = await auth.getValidToken();
        const grants = await grantMissingRoomKeys(getConfig(), token, keyRing, myDid, admits);
        for (const grant of grants) {
            console.log(
                `[server-link-language] granted key versions [${grant.versions.join(", ")}] to ${grant.did}`,
            );
        }
    } catch (err) {
        console.error("[server-link-language] key grant failed (non-fatal):", err);
    }
}

/**
 * Any admitted member: a new room key version, generated here and sealed to every
 * key the room lists and the space admits (K1). The server never sees the key. A
 * 409 (the room's admitted keys changed) catches up on membership and seals again.
 * The executor hears `roomKeyEvent({kind: "rotated"})` with the keys the
 * rotation left out (`removedKeys`).
 */
async function performRotation(removedKeys: string[]): Promise<void> {
    const token = await auth.getValidToken();
    const outcome = await rotateRoomKey(getConfig(), token, admits, {
        beforeRetry: () => syncModule.catchUpMembership(),
    });
    if (!outcome) {
        console.log("[server-link-language] no admitted key to seal a room key to — skipping rotation");
        return;
    }
    rotatedThisRun = true;
    console.log(
        `[server-link-language] key rotation complete: version ${outcome.result.version}, ` +
        `${outcome.result.recipients.length} recipient(s)`,
    );
    try {
        await getRuntime().roomKeyEvent({ kind: "rotated", removedKeys, version: outcome.result.version });
    } catch (err) {
        console.error("[server-link-language] roomKeyEvent(rotated) failed:", err);
    }
    await setupKeyRingCoalesced();
}

/**
 * K3: the room's admitted keys changed (`rotate-needed`). A member holding a key
 * catches up on membership, so the executor knows who is in now, and rotates.
 */
async function rotateAfterMembershipChange(removedKeys: string[]): Promise<void> {
    await syncModule.catchUpMembership();
    await refreshAdmitted();
    if (!admitted) return;
    if (keyRingStatus !== "ready") {
        await mintRoomKeyIfNone();
        return;
    }
    await performRotation(removedKeys);
}

function splitMembership(diff: PerspectiveDiff): { membership: LinkExpression[]; plain: PerspectiveDiff } {
    const membership = diff.additions.filter(isMembershipLink);
    return {
        membership,
        plain: {
            additions: diff.additions.filter((l) => !isMembershipLink(l)),
            removals: diff.removals,
        },
    };
}

// ---------------------------------------------------------------------------
// Language definition
// ---------------------------------------------------------------------------

const language = defineLanguage({
    name: "server-link-language",
    version: "0.1.0",

    isPublic: true,

    async init() {
        initAdapters({
            storage: new DenoStorageAdapter(),
            transport: new DenoTransport(),
            agent: new DenoAgentAdapter(),
            runtime: new DenoRuntimeAdapter(),
            wsFactory: new DenoWebSocketFactory(),
        });

        myDid = getAgent().did();
        store.initStore(hash);

        configured = !isPlaceholder(SERVER_URL) && !isPlaceholder(UID) && !isPlaceholder(CREATOR);
        if (!configured) {
            console.log(
                `[server-link-language] init: did=${myDid}, template variables not filled in — ` +
                "running inert until published with SERVER_URL, UID and CREATOR.",
            );
            return;
        }

        initAdapters({
            config: { serverUrl: SERVER_URL, roomId: roomIdOf(CREATOR, UID), creator: CREATOR, uid: UID },
        });
        const config = getConfig();

        syncModule.initSync({
            config,
            getToken: () => auth.getValidToken(),
            invalidateToken: () => auth.invalidateSession(),
            emitDiff: (diff) => getRuntime().emitPerspectiveDiff(diff),
            emitSyncState: (state) => getRuntime().emitSyncStateChange(state),
            getKeyRing: () => keyRing,
            refreshKeyRing: () => setupKeyRingCoalesced(),
            // K5: the membership links the room counted go to the executor's
            // membership of the space; the language keeps none.
            membershipLinks: (links) => getRuntime().membershipLinks(links),
            // Periodic key grants — fallback for when the WS
            // `peer-joined` event never fires (WS down, CI, firewalls).
            // Each successful HTTP sync triggers a check so a member
            // holding keys discovers members who lack them.
            onPostSync: () => performKeyGrants(),
        });

        wsClient = new WsClient({
            getUrl: async () => api.wsUrl(config),
            getToken: () => auth.getValidToken(),
            invalidateToken: () => auth.invalidateSession(),
            handlers: {
                onDiff(msg) {
                    // A link no key opens yet is held, and the next catch-up
                    // re-reads the key ring for it.
                    syncModule.applyInboundWireDiff(msg.payload, msg.sequence, msg.revision);
                },
                onMembership(msg) {
                    void syncModule.onMembershipPush(msg);
                },
                onRotateNeeded(msg) {
                    void rotateAfterMembershipChange(msg.removedKeys).catch((err) => {
                        console.error(`[server-link-language] rotation after '${msg.reason}' failed:`, err);
                    });
                },
                onRoomClosed(code, reason) {
                    if (code === CLOSE_KEY_REMOVED) {
                        // This key was removed from the room or revoked: no reconnect
                        // until the next init, and the executor drops its room key.
                        console.error(`[server-link-language] the room removed this key (${reason})`);
                        keyRing = null;
                        keyRingStatus = "pending";
                        admitted = false;
                        void getRuntime()
                            .roomKeyEvent({ kind: "dropped", removedKeys: [myDid] })
                            .catch((err) => console.error("[server-link-language] roomKeyEvent(dropped) failed:", err));
                    } else if (code === CLOSE_ROOM_SUSPENDED) {
                        console.error(`[server-link-language] the room is suspended by its operator (${reason}); reconnecting with backoff`);
                    }
                },
                onTelepresenceSignal(msg) {
                    // Directed at us specifically — this connection is
                    // authenticated as myDid, so we are the local recipient.
                    getRuntime().emitTelepresenceSignal(msg.payload, myDid);
                },
                onTelepresenceBroadcast(msg) {
                    getRuntime().emitTelepresenceSignal(msg.payload);
                },
                onOnlineAgents(msg) {
                    telepresenceModule.handleOnlineAgentsMessage(msg);
                },
                onPeerJoined(msg) {
                    telepresenceModule.handlePeerJoined(msg);
                    // Grant historical keys to members who lack them. Do not
                    // rotate here: peer-joined is presence (reconnect, second
                    // device, wake), not membership; the room says
                    // `rotate-needed` when its admitted keys change.
                    void performKeyGrants().catch((err) => {
                        console.error("[server-link-language] peer-joined key grant failed:", err);
                    });
                },
                onPeerLeft(msg) {
                    telepresenceModule.handlePeerLeft(msg);
                },
                onStatusChanged(msg) {
                    telepresenceModule.handleStatusChanged(msg);
                },
                onKeysGranted() {
                    // Load the granted key (which opens the held links), then
                    // catch up on what arrived since.
                    void setupKeyRingCoalesced().then(() => syncModule.catchUp()).catch((err) => {
                        console.error("[server-link-language] catch-up after key grant failed:", err);
                    });
                },
                onOpen() {
                    getRuntime().emitSyncStateChange("Synced");
                    // Belt-and-braces catch-up: covers any diff that landed
                    // during the connect/reconnect window before the push
                    // channel was live. Idempotent — see sync.catchUp().
                    void syncModule.catchUp().catch((err) => {
                        console.error("[server-link-language] post-connect catch-up failed:", err);
                    });
                },
                onClose() {
                    telepresenceModule.clearOnlineAgents();
                    // Must match a PerspectiveState variant the executor accepts
                    // (rust-executor/src/types/domain.rs::PerspectiveState).
                    // "NotSynced" does not exist — the correct variant for
                    // "language installed, known to be behind" follows.
                    getRuntime().emitSyncStateChange("LinkLanguageInstalledButNotSynced");
                },
                onReconnecting(attempt, delayMs) {
                    console.log(
                        `[server-link-language] websocket reconnecting (attempt ${attempt}) in ${Math.round(delayMs)}ms`,
                    );
                },
            },
        });

        telepresenceModule.initTelepresence({
            send: (msg) => wsClient!.send(msg),
            getMyDid: () => myDid,
        });

        // A fresh member (cursor 0) stores its render, or keeps the one a
        // previous run left unfinished, before anything can fail: until its
        // last page, every catch-up resumes it with backoff, whichever step
        // below fails first. A member with a cursor above 0 only catches up.
        syncModule.needRender();
        try {
            try {
                admitted = (await auth.authenticate()).admitted;
            } catch (err) {
                if (err instanceof api.ApiError && err.status === 404) {
                    // Nobody made the room yet: the creator's first membership
                    // commit makes it (genesis). Until then there is nothing to read.
                    console.log(
                        "[server-link-language] the room does not exist yet — the creator's first membership " +
                        "commit makes it",
                    );
                    return;
                }
                throw err;
            }

            await setupKeyRingCoalesced();
            // K2: an admitted member holding no key mints version 1 when the
            // room has none, before its first plain commit.
            await mintRoomKeyIfNone();

            // The room's current links (a fresh member's snapshot, or the
            // diffs after the cursor), emitted to the executor; those no key
            // opens yet are held until one does. Membership comes first.
            await syncModule.bootstrap();

            // With a key ring, grant the versions we hold to any member
            // who joined while we were offline.
            void performKeyGrants().catch((err) => {
                console.error("[server-link-language] initial key grant check failed:", err);
            });
        } catch (err) {
            console.error(
                "[server-link-language] initial startup sequence failed — will keep retrying via " +
                "websocket reconnect backoff and the runtime's periodic sync(), which also retries " +
                "the room render with backoff until it succeeds:",
                err,
            );
        } finally {
            // Always attempt to connect, even if the steps above failed: getUrl()
            // re-authenticates internally, and the reconnect loop retries with backoff.
            void wsClient.connect().catch((err) => {
                console.error("[server-link-language] initial websocket connect failed:", err);
            });
            console.log(`[server-link-language] init complete: did=${myDid}, room=${config.roomId}`);
        }
    },

    async teardown() {
        lifecycleGen++;
        if (wsClient) {
            wsClient.close();
            wsClient = null;
        }
        myDid = "";
        configured = false;
        localAgents = [];
        keyRing = null;
        keyRingStatus = "pending";
        admitted = false;
        rotatedThisRun = false;
        keyRingInflight = null;
        auth.resetAuth();
        resetAdapters();
        console.log("[server-link-language] teardown");
    },

    interactions() {
        return [];
    },

    // -----------------------------------------------------------------------
    // perspective-commit
    // -----------------------------------------------------------------------
    commit: {
        async commit(diff: PerspectiveDiff) {
            if (!configured) {
                throw new Error(
                    "server-link-language: not configured (SERVER_URL/UID/CREATOR template variables unfilled)",
                );
            }
            const { membership, plain } = splitMembership(diff);

            // K5: membership links go first, plain and signed, and need no room
            // key: the creator's first one makes the room (genesis), and a
            // joiner's self-join is what admits it.
            if (membership.length > 0) {
                await syncModule.commit({ additions: membership, removals: [] });
                if (!admitted || keyRingStatus !== "ready") {
                    await refreshAdmitted();
                    await mintRoomKeyIfNone();
                }
            }

            const hasPlain = plain.additions.length > 0 || plain.removals.length > 0;
            if (!hasPlain) return "";

            if (keyRingStatus === "pending" || keyRingStatus === "error") {
                // Loading the key also opens any links held for it.
                await setupKeyRingCoalesced();
            }
            if (keyRingStatus === "error") {
                throw new Error(
                    "server-link-language: cannot commit yet — this room's E2E key ring could not be " +
                    "acquired/decrypted. Retry once connectivity/auth recovers.",
                );
            }
            // An admitted member holding no key mints one when the room has none
            // (K2); a failed mint throws, and the executor keeps the diff for
            // retry. There is no plaintext mode: otherwise it waits for a grant.
            await mintRoomKeyIfNone();
            if (keyRingStatus !== "ready") {
                throw new Error(
                    "server-link-language: cannot commit yet — this agent holds no room key it can open. " +
                    "A member holding one grants it when it is next online.",
                );
            }

            console.log(
                `[server-link-language] commit: ${plain.additions.length} adds, ` +
                `${plain.removals.length} removes (keyRingStatus=${keyRingStatus})`,
            );

            // Push the diff to the server; syncModule.commit stores it
            // locally only once the server accepted it, and does not echo
            // it back (the executor committed it and already holds it).
            // Awaiting the POST ensures the executor only clears its
            // pending-diff DB entry AFTER the server accepted the commit.
            await syncModule.commit(plain);

            return "";
        },
    },

    // -----------------------------------------------------------------------
    // perspective-sync
    // -----------------------------------------------------------------------
    sync: {
        async sync() {
            if (!configured) return { additions: [], removals: [] };
            return syncModule.performSync();
        },

        async render() {
            return syncModule.render();
        },

        async currentRevision() {
            return syncModule.currentRevision();
        },
    },

    // -----------------------------------------------------------------------
    // perspective-query
    // -----------------------------------------------------------------------
    query: {
        supportedKinds() {
            return ["link-pattern"];
        },

        async run(req: { kind: string; payload: unknown }) {
            if (req.kind !== "link-pattern") {
                return { kind: "error", payload: `Unsupported query kind: ${req.kind}` };
            }
            const pattern = (req.payload ?? {}) as { source?: string; target?: string; predicate?: string };
            const links = store.queryLinks(pattern);
            return { kind: "links", payload: links };
        },
    },

    // -----------------------------------------------------------------------
    // peers
    // -----------------------------------------------------------------------
    peers: {
        async setLocal(agents: string[]) {
            localAgents = agents;
        },

        async remote() {
            if (!configured) return [];
            const config = getConfig();
            const token = await auth.getValidToken();
            const allPeers = await api.fetchPeers(config, token);
            return allPeers.filter((did) => did !== myDid && !localAgents.includes(did));
        },
    },

    // -----------------------------------------------------------------------
    // telepresence
    // -----------------------------------------------------------------------
    telepresence: {
        async setOnlineStatus(status: unknown) {
            return telepresenceModule.setOnlineStatus(status);
        },

        async getOnlineAgents() {
            return telepresenceModule.getOnlineAgents();
        },

        async sendSignal(remoteAgentDid: string, payload: unknown) {
            return telepresenceModule.sendSignal(remoteAgentDid, payload);
        },

        async sendBroadcast(payload: unknown) {
            return telepresenceModule.sendBroadcast(payload);
        },
    },
});

// ---------------------------------------------------------------------------
// Flat exports (required by the ALDK runtime)
// ---------------------------------------------------------------------------

export const {
    name,
    version,
    isPublic,
    init,
    teardown,
    interactions,
    perspectiveCommit,
    perspectiveSyncSync,
    perspectiveSyncRender,
    perspectiveSyncCurrentRevision,
    perspectiveQuerySupportedKinds,
    perspectiveQueryRun,
    peersSetLocal,
    peersRemote,
    telepresenceSetOnlineStatus,
    telepresenceGetOnlineAgents,
    telepresenceSendSignal,
    telepresenceSendBroadcast,
} = language;

export default language;

// ---------------------------------------------------------------------------
// Template params metadata (for language.publish / LanguageMeta)
// ---------------------------------------------------------------------------

export const possibleTemplateParams: string[] = ["SERVER_URL", "UID", "CREATOR"];
