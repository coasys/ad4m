/**
 * # Server Link Language
 *
 * AD4M link language that syncs a perspective through a self-hosted
 * `link-server` instance over HTTP (auth, commit, catch-up sync,
 * peers, ACL, E2E key exchange) and native WebSocket (real-time diff
 * push, telepresence, presence). Implements every link-language
 * capability: perspective-commit, perspective-sync, perspective-query,
 * peers, and telepresence.
 *
 * See README.md for architecture and AGENTS.md for operational notes /
 * known limitations.
 */

import { defineLanguage, hash } from "@coasys/ad4m-ldk";

import type { PerspectiveDiff } from "./src/types.js";
import * as store from "./src/store.js";
import * as api from "./src/api.js";
import * as auth from "./src/auth.js";
import * as syncModule from "./src/sync.js";
import * as telepresenceModule from "./src/telepresence.js";
import { WsClient } from "./src/ws-client.js";
import { decodeSealedEnvelope, deriveX25519KeyPair, openRoomKeyEnvelope } from "./src/encryption.js";

import {
    initAgent,
    initConfig,
    initRuntime,
    initStorage,
    initTransport,
    initWebSocketFactory,
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
const SERVER_URL = "<to-be-filled>";

//!@ad4m-template-variable
const ROOM_ID = "<to-be-filled>";

// ---------------------------------------------------------------------------
// Module state (fresh per perspective instance — see language-interface-spec.md §2)
// ---------------------------------------------------------------------------

let myDid: string = "";
let configured = false;
let localAgents: string[] = [];
let wsClient: WsClient | null = null;

/** The room's shared AES-256-GCM key, once fetched + decrypted. Null for a
 * plaintext room OR while E2E setup is still in flight. */
let roomKey: Uint8Array | null = null;
type RoomKeyStatus = "none" | "ready" | "error";
/** "error" means an E2E key almost certainly exists but we couldn't get/open
 * it — commit() refuses to send plaintext in that state rather than risk a
 * confidentiality leak. See setupRoomKey(). */
let roomKeyStatus: RoomKeyStatus = "none";

function isPlaceholder(value: string): boolean {
    return !value || value === "<to-be-filled>";
}

// ---------------------------------------------------------------------------
// Startup helpers
// ---------------------------------------------------------------------------

async function setupRoomKey(): Promise<void> {
    const config = getConfig();
    try {
        const token = await auth.getValidToken();
        const keysRes = await api.fetchRoomKey(config, token);
        if (!keysRes) {
            roomKeyStatus = "none";
            roomKey = null;
            return;
        }
        const envelope = decodeSealedEnvelope(keysRes.encryptedKey);
        const { privateKey } = deriveX25519KeyPair((payload) => getAgent().signStringHex(payload));
        roomKey = openRoomKeyEnvelope(envelope, privateKey);
        roomKeyStatus = "ready";
        console.log(`[server-link-language] E2E room key acquired (version ${keysRes.version})`);
    } catch (err) {
        roomKeyStatus = "error";
        roomKey = null;
        console.error(
            "[server-link-language] failed to acquire/decrypt the E2E room key — " +
            "refusing to commit plaintext until this resolves:",
            err,
        );
    }
}

// ---------------------------------------------------------------------------
// Language definition
// ---------------------------------------------------------------------------

const language = defineLanguage({
    name: "server-link-language",
    version: "0.1.0",

    isPublic: true,

    async init() {
        initStorage(new DenoStorageAdapter());
        initTransport(new DenoTransport());
        initAgent(new DenoAgentAdapter());
        initRuntime(new DenoRuntimeAdapter());
        initWebSocketFactory(new DenoWebSocketFactory());

        myDid = getAgent().did();
        store.initStore(hash);

        configured = !isPlaceholder(SERVER_URL) && !isPlaceholder(ROOM_ID);
        if (!configured) {
            console.log(
                `[server-link-language] init: did=${myDid}, template variables not filled in — ` +
                "running inert until published with SERVER_URL/ROOM_ID.",
            );
            return;
        }

        initConfig({ serverUrl: SERVER_URL, roomId: ROOM_ID });
        const config = getConfig();

        syncModule.initSync({
            config,
            getToken: () => auth.getValidToken(),
            emitDiff: (diff) => getRuntime().emitPerspectiveDiff(diff),
            emitSyncState: (state) => getRuntime().emitSyncStateChange(state),
            getRoomKey: () => roomKey,
        });

        wsClient = new WsClient({
            getUrl: async () => {
                const token = await auth.getValidToken();
                return api.wsUrl(config, token);
            },
            handlers: {
                onDiff(msg) {
                    syncModule.applyInboundWireDiff(msg.payload, msg.sequence, msg.revision);
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
                },
                onPeerLeft(msg) {
                    telepresenceModule.handlePeerLeft(msg);
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
                    // PerspectiveState variant name — the executor deserialises this against
// its enum (rust-executor/src/types/domain.rs::PerspectiveState) and rejects
// anything it doesn't know. "NotSynced" is not a variant; the semantically
// correct value for "we own the language, we know we're behind" is
// LinkLanguageInstalledButNotSynced. The SCREAMING_SNAKE form is the
// primary serde name, but the PascalCase alias is also accepted.
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

        try {
            await auth.authenticate();
            await setupRoomKey();
            await syncModule.bootstrap();
        } catch (err) {
            console.error(
                "[server-link-language] initial startup sequence failed — will keep retrying via " +
                "websocket reconnect backoff and the runtime's periodic sync():",
                err,
            );
        }

        // Always attempt to connect, even if the steps above failed: getUrl()
        // re-authenticates internally, and the reconnect loop retries with backoff.
        void wsClient.connect();

        console.log(`[server-link-language] init complete: did=${myDid}, room=${ROOM_ID}`);
    },

    async teardown() {
        if (wsClient) {
            wsClient.close();
            wsClient = null;
        }
        myDid = "";
        configured = false;
        localAgents = [];
        roomKey = null;
        roomKeyStatus = "none";
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
                    "server-link-language: not configured (SERVER_URL/ROOM_ID template variables unfilled)",
                );
            }
            if (roomKeyStatus === "error") {
                throw new Error(
                    "server-link-language: refusing to commit — this room's E2E key could not be " +
                    "acquired/decrypted, and sending plaintext to a possibly-encrypted room would be unsafe. " +
                    "Retry once connectivity/auth recovers.",
                );
            }

            // 1. Store links locally (plaintext, always — see src/sync.ts module doc).
            store.applyDiff(diff);

            // 2. Queue the push to the server. `enqueueCommitBatched` returns
            //    immediately after appending to the batch; the actual POST
            //    happens on a microtask flush, so a tight loop of addLink()
            //    calls collapses into one POST (see the batching block in
            //    sync.ts for why).
            syncModule.enqueueCommitBatched(diff);

            // 3. Emit so local subscribers see it immediately.
            getRuntime().emitPerspectiveDiff(diff);

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

export const possibleTemplateParams: string[] = ["SERVER_URL", "ROOM_ID"];
