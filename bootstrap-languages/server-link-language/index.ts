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

import { defineLanguage } from "@coasys/ad4m-ldk";

import type { PerspectiveDiff } from "./src/types.js";
import * as store from "./src/store.js";
import * as api from "./src/api.js";
import * as auth from "./src/auth.js";
import * as syncModule from "./src/sync.js";
import * as telepresenceModule from "./src/telepresence.js";
import { WsClient } from "./src/ws-client.js";
import { deriveX25519KeyPair } from "./src/encryption.js";
import { KeyRingManager } from "./src/key-ring-manager.js";

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
import { commitNeedsKeyRetry } from "./src/key-ring-policy.js";

// ---------------------------------------------------------------------------
// Template Variables
// ---------------------------------------------------------------------------
// Replaced at publish time by the executor. See README.md § Template
// Variables and possibleTemplateParams below.

//!@ad4m-template-variable
const SERVER_URL = "https://link.ad4m.dev";

//!@ad4m-template-variable
const UID = "<to-be-filled>";

// ---------------------------------------------------------------------------
// Module state (fresh per perspective instance — see language-interface-spec.md §2)
// ---------------------------------------------------------------------------

let myDid: string = "";
let configured = false;
let localAgents: string[] = [];
let wsClient: WsClient | null = null;

/** The room's E2E key ring, admin duties and the pending-grant poll (see
 *  src/key-ring-manager.ts). Created in init() once the room is configured. */
let keys: KeyRingManager | null = null;

function isPlaceholder(value: string): boolean {
    return !value || value === "<to-be-filled>";
}

// ---------------------------------------------------------------------------
// Startup helpers
// ---------------------------------------------------------------------------

/**
 * Background key-ring refresh (WS diff with an unknown key version). If new
 * versions appeared, re-reads the room so previously undecryptable links
 * appear. Returns true when new key versions were obtained.
 */
async function refreshKeyRingIfNeeded(): Promise<boolean> {
    if (!keys) return false;
    const gotNew = await keys.refresh();
    if (!gotNew) return false;
    console.log("[server-link-language] key ring refreshed — re-bootstrapping");
    await reReadRoom();
    return true;
}

/** Replaces the store with the server's snapshot and hands every link to
 *  the executor: links that could not be decrypted before now can. */
async function reReadRoom(): Promise<void> {
    await syncModule.bootstrap();
    const recovered = syncModule.render();
    if (recovered.links.length > 0) {
        getRuntime().emitPerspectiveDiff({ additions: recovered.links, removals: [] });
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
        initAdapters({
            storage: new DenoStorageAdapter(),
            transport: new DenoTransport(),
            agent: new DenoAgentAdapter(),
            runtime: new DenoRuntimeAdapter(),
            wsFactory: new DenoWebSocketFactory(),
        });

        myDid = getAgent().did();
        store.initStore();

        configured = !isPlaceholder(SERVER_URL) && !isPlaceholder(UID);
        if (!configured) {
            console.log(
                `[server-link-language] init: did=${myDid}, template variables not filled in — ` +
                "running inert until published with SERVER_URL/UID.",
            );
            return;
        }

        initAdapters({ config: { serverUrl: SERVER_URL, roomId: UID } });
        const config = getConfig();

        const keyMgr = new KeyRingManager({
            config,
            getToken: () => auth.getValidToken(),
            x25519PrivateKey: () =>
                deriveX25519KeyPair((payload) => getAgent().signStringHex(payload)).privateKey,
            // The pending poll got this agent's first keys: re-read the room
            // so the links it could not decrypt reach the executor now.
            onNewKeys: () => reReadRoom(),
        });
        keys = keyMgr;

        syncModule.initSync({
            config,
            getToken: () => auth.getValidToken(),
            invalidateToken: () => auth.invalidateSession(),
            emitDiff: (diff) => getRuntime().emitPerspectiveDiff(diff),
            emitSyncState: (state) => getRuntime().emitSyncStateChange(state),
            getKeyRing: () => keyMgr.keyRing,
            refreshKeyRing: () => keyMgr.refresh(),
            // Periodic admin key grants — fallback for when the WS
            // `peer-joined` event never fires (WS down, CI, firewalls).
            // Each successful HTTP sync triggers a check so the admin
            // discovers new members who need historical keys.
            onPostSync: () => keyMgr.grantMissing(),
        });

        wsClient = new WsClient({
            getUrl: async () => api.wsUrl(config),
            getToken: () => auth.getValidToken(),
            invalidateToken: () => auth.invalidateSession(),
            handlers: {
                onDiff(msg) {
                    const result = syncModule.applyInboundWireDiff(msg.payload, msg.sequence, msg.revision);
                    // If the live push contained links we couldn't decrypt,
                    // bridge the missing versions into the HTTP-sync retry
                    // set so `catchUp()` retries on the next cycle, then
                    // kick off an immediate background refresh attempt.
                    if (result.missingVersions.size > 0) {
                        syncModule.trackMissingKeyVersions(result.missingVersions);
                        void refreshKeyRingIfNeeded().catch((err) => {
                            console.error("[server-link-language] WS diff key ring refresh failed:", err);
                        });
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
                    // Grant historical keys to members who lack them.
                    // Do NOT rotate here — onPeerJoined is a presence
                    // event (reconnect, second device, wake) not an ACL
                    // event. Rotating on presence would mint a new version
                    // on every reconnect. Rotation belongs on ACL changes
                    // or explicit admin action.
                    // A member still waiting for its first keys restarts
                    // its fast /keys poll instead: the peer may be the
                    // admin coming online and granting them.
                    keyMgr.onPeerJoined();
                },
                onPeerLeft(msg) {
                    telepresenceModule.handlePeerLeft(msg);
                },
                onStatusChanged(msg) {
                    telepresenceModule.handleStatusChanged(msg);
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

        try {
            await auth.authenticate();

            // Determine admin status — used by the admin's key grants.
            await keyMgr.detectAdmin(myDid);

            // Reads /keys. A member the admin has not granted keys yet
            // ends "pending" and starts the fast /keys poll, which
            // re-reads the room as soon as the grant lands.
            await keyMgr.setup();

            // E2E is mandatory — if admin and no E2E exists yet,
            // generate the initial room key. Every room gets encrypted
            // automatically; there is no plaintext mode.
            if (keyMgr.isAdmin && keyMgr.status === "none") {
                console.log(
                    "[server-link-language] admin, no E2E yet — generating initial room key",
                );
                await keyMgr.rotate();
            }

            await syncModule.bootstrap();

            // If admin and key ring ready, grant keys to any members who
            // joined while we were offline.
            void keyMgr.grantMissing();
        } catch (err) {
            console.error(
                "[server-link-language] initial startup sequence failed — will keep retrying via " +
                "websocket reconnect backoff and the runtime's periodic sync():",
                err,
            );
        }

        // Always attempt to connect, even if the steps above failed: getUrl()
        // re-authenticates internally, and the reconnect loop retries with backoff.
        void wsClient.connect().catch((err) => {
            console.error("[server-link-language] initial websocket connect failed:", err);
        });

        console.log(`[server-link-language] init complete: did=${myDid}, uid=${UID}`);
    },

    async teardown() {
        // Stop the /keys poll now, but keep the key ring until the queued
        // commits are drained: they must still be encrypted.
        keys?.stopPendingPoll();
        try {
            await syncModule.drainCommitBatch();
        } catch (err) {
            console.error(
                "[server-link-language] teardown: draining batched commits failed; " +
                "some writes may not have reached the server:",
                err,
            );
        }
        if (wsClient) {
            wsClient.close();
            wsClient = null;
        }
        myDid = "";
        configured = false;
        localAgents = [];
        keys?.teardown();
        keys = null;
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
                    "server-link-language: not configured (SERVER_URL/UID template variables unfilled)",
                );
            }
            const k = keys;
            if (!k) {
                throw new Error("server-link-language: commit before init finished");
            }
            if (commitNeedsKeyRetry(k.status)) {
                console.log(
                    `[server-link-language] retrying E2E key ring acquisition before commit (status: ${k.status})...`,
                );
                await k.loadCoalesced();
                if (k.status === "ready") {
                    k.stopPendingPoll();
                    console.log(
                        "[server-link-language] key ring acquired — re-bootstrapping to recover skipped links",
                    );
                    await reReadRoom();
                }
            }
            if (k.status === "error") {
                throw new Error(
                    "server-link-language: refusing to commit — this room's E2E key ring could not be " +
                    "acquired/decrypted, and sending plaintext to a possibly-encrypted room would be unsafe. " +
                    "Retry once connectivity/auth recovers.",
                );
            }
            if (k.status === "pending") {
                throw new Error(
                    "server-link-language: refusing to commit — this room has E2E encryption enabled " +
                    "but this agent has not received room keys yet. The room admin must grant keys " +
                    "before this agent can write.",
                );
            }

            // E2E is mandatory — if admin and the status is still
            // "none" (init's rotation failed or was skipped), retry
            // rotation before committing. Non-admin "none" means the room
            // genuinely has no E2E yet — plaintext is correct for the
            // first commit that triggers the admin flow.
            if (k.isAdmin && k.status === "none") {
                console.log(
                    "[server-link-language] admin with keyRingStatus=none — retrying rotation before commit",
                );
                await k.rotate();
                // rotate() throws on server/auth failure → executor keeps
                // the diff for retry. If it returns (success or no-op), the
                // status may now be "ready" or still "none" (no members
                // with X25519 keys yet).
            }

            console.log(
                `[server-link-language] commit: ${diff.additions.length} adds, ` +
                `${diff.removals.length} removes (keyRingStatus=${k.status})`,
            );

            // Push the diff to the server, in chunks. Awaiting it ensures
            // the executor only clears its pending-diff DB entry AFTER the
            // server accepted the commit. syncModule.commit writes each
            // accepted chunk to the language's store (never before the
            // server has it) and does not echo the diff back: the executor
            // committed these links and already holds them.
            await syncModule.commit(diff);

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

export const possibleTemplateParams: string[] = ["SERVER_URL", "UID"];
