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
import { buildKeyRing, type KeyRing } from "./src/encryption.js";
import { grantMissingRoomKeys, rotateRoomKey } from "./src/room-keys.js";

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
import { commitNeedsKeyRetry, refreshSkippedByCooldown, type KeyRingStatus } from "./src/key-ring-policy.js";

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

/** The room's versioned key ring (version → decrypted AES-256-GCM key).
 * Null for a plaintext room OR while E2E setup is still in flight. */
let keyRing: KeyRing | null = null;
/**
 * - "none": room has no E2E — plaintext commits are fine.
 * - "ready": key ring acquired and decrypted — encrypted commits ready.
 * - "pending": room HAS E2E but this agent holds no key it can open: no
 *   member holding one has been online since it joined, or none of its
 *   boxes opens (an older format). An admin in this state rotates.
 *   commit() refuses to send plaintext.
 * - "error": E2E keys almost certainly exist but we could not get/open
 *   them. commit() refuses to send plaintext.
 */
let keyRingStatus: KeyRingStatus = "none";
/** True when this agent has been identified as the room admin. */
let isRoomAdmin = false;
/** Cooldown for key ring retries — shared across WS and HTTP paths (ms since epoch). */
let lastKeyRingRetry = 0;
const KEY_RING_RETRY_COOLDOWN_MS = 10_000;
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

// ---------------------------------------------------------------------------
// Startup helpers
// ---------------------------------------------------------------------------

/** No room key this agent can open: none exists yet ("none"), or the room has
 *  keys but none sealed to this agent opens ("pending"). */
function adminHoldsNoKey(): boolean {
    return keyRingStatus === "none" || keyRingStatus === "pending";
}

async function setupKeyRing(): Promise<void> {
    const gen = lifecycleGen;
    const config = getConfig();
    try {
        const token = await auth.getValidToken();
        const keysRes = await api.fetchRoomKeys(config, token);
        if (gen !== lifecycleGen) return;
        if (!keysRes) {
            keyRingStatus = "none";
            keyRing = null;
            return;
        }
        if (keysRes.keys.length === 0 && keysRes.e2e_enabled) {
            keyRingStatus = "pending";
            keyRing = null;
            console.log(
                "[server-link-language] room has E2E enabled but this agent has no keys yet — " +
                "commits blocked until a member holding the room key grants it",
            );
            return;
        }
        if (keysRes.keys.length === 0) {
            keyRingStatus = "none";
            keyRing = null;
            return;
        }
        keyRing = buildKeyRing(keysRes.keys, getAgent());
        if (keyRing.size === 0) {
            // Boxes sealed to this agent, none of which open (sealed in an older
            // format, or a spoiled grant): the same as holding none.
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
        keyRingStatus = "error";
        keyRing = null;
        console.error(
            "[server-link-language] failed to acquire/decrypt E2E key ring — " +
            "refusing to commit plaintext until this resolves:",
            err,
        );
    }
}

/**
 * Refresh the key ring from the server. If new versions appeared,
 * re-bootstrap the full link set so previously-undecryptable links
 * get picked up. Returns true when new key versions were obtained.
 */
async function refreshKeyRingIfNeeded(): Promise<boolean> {
    const now = Date.now();
    if (now - lastKeyRingRetry < KEY_RING_RETRY_COOLDOWN_MS) {
        return false;
    }
    lastKeyRingRetry = now;
    const prevSize = keyRing?.size ?? 0;
    await setupKeyRingCoalesced();
    const newSize = keyRing?.size ?? 0;
    if (newSize > prevSize) {
        console.log("[server-link-language] key ring refreshed — re-bootstrapping");
        await syncModule.bootstrap();
        const recovered = syncModule.render();
        if (recovered.links.length > 0) {
            getRuntime().emitPerspectiveDiff({ additions: recovered.links, removals: [] });
        }
        return true;
    }
    return false;
}

/**
 * If this agent holds room key versions, seal them to every ACL member
 * lacking them. Any member does this, not only the admin: a member's key
 * follows from its DID, so a joiner gets the room's history from whichever
 * member holding it is online first. Runs after setupKeyRing, after every
 * sync and on peer-joined events; the server rate-limits the calls.
 */
async function performKeyGrants(): Promise<void> {
    if (!keyRing || keyRing.size === 0) return;
    try {
        const token = await auth.getValidToken();
        const grants = await grantMissingRoomKeys(getConfig(), token, getAgent(), keyRing, myDid);
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
 * Client-side key rotation (room admin only): generates the room key
 * locally, seals it to the DID of every ACL member, including members that
 * never connected, and sends only the sealed boxes. The server never
 * touches plaintext key material. Called from the language's init flow.
 */
async function performRotation(): Promise<void> {
    if (!isRoomAdmin) return;
    try {
        const token = await auth.getValidToken();
        const result = await rotateRoomKey(getConfig(), token, getAgent());
        if (!result) {
            console.log("[server-link-language] no ACL member to seal a room key to — skipping rotation");
            return;
        }
        console.log(
            `[server-link-language] client-side key rotation complete: version ${result.version}, ` +
            `${result.recipients.length} recipient(s)`,
        );
        await setupKeyRing();
    } catch (err) {
        console.error("[server-link-language] key rotation failed:", err);
        throw err;
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
        store.initStore(hash);

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

        syncModule.initSync({
            config,
            getToken: () => auth.getValidToken(),
            invalidateToken: () => auth.invalidateSession(),
            emitDiff: (diff) => getRuntime().emitPerspectiveDiff(diff),
            emitSyncState: (state) => getRuntime().emitSyncStateChange(state),
            getKeyRing: () => keyRing,
            refreshKeyRing: async () => {
                const now = Date.now();
                if (refreshSkippedByCooldown(keyRingStatus, now - lastKeyRingRetry, KEY_RING_RETRY_COOLDOWN_MS)) {
                    return null;
                }
                lastKeyRingRetry = now;
                const prevSize = keyRing?.size ?? 0;
                await setupKeyRingCoalesced();
                return (keyRing?.size ?? 0) > prevSize;
            },
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

            // Determine admin status — only the admin rotates the room key.
            try {
                const aclInfo = await api.fetchAclInfo(config, await auth.getValidToken());
                isRoomAdmin = aclInfo.admin === myDid;
            } catch {
                isRoomAdmin = false;
            }

            await setupKeyRing();

            // E2E is mandatory: an admin that holds no room key it can open
            // (none exists yet, or none opens) rotates, sealing a new version
            // to every ACL member. There is no plaintext mode.
            if (isRoomAdmin && adminHoldsNoKey()) {
                console.log(
                    `[server-link-language] admin holds no room key (${keyRingStatus}) — generating a new one`,
                );
                await performRotation();
            }

            await syncModule.bootstrap();

            // E2E timing fix: if keyRingStatus is still "pending" after
            // bootstrap, a member may have granted keys between our
            // setupKeyRing() call and now. Retry once — if keys arrived,
            // re-bootstrap so any encrypted links in the room's history
            // get picked up immediately instead of waiting for the next
            // sync cycle.
            // Cast needed: setupKeyRing() mutates the module-level
            // keyRingStatus across an await boundary; TS literal
            // narrowing doesn't track that.
            if ((keyRingStatus as KeyRingStatus) === "pending") {
                await setupKeyRing();
                if ((keyRingStatus as KeyRingStatus) === "ready") {
                    console.log(
                        "[server-link-language] keys acquired after init grant — re-bootstrapping",
                    );
                    await syncModule.bootstrap();
                }
            }

            // With a key ring, grant the versions we hold to any member
            // who joined while we were offline.
            void performKeyGrants().catch((err) => {
                console.error("[server-link-language] initial key grant check failed:", err);
            });
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
        lifecycleGen++;
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
        keyRing = null;
        keyRingStatus = "none";
        isRoomAdmin = false;
        lastKeyRingRetry = 0;
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
                    "server-link-language: not configured (SERVER_URL/UID template variables unfilled)",
                );
            }
            if (commitNeedsKeyRetry(keyRingStatus)) {
                console.log(
                    `[server-link-language] retrying E2E key ring acquisition before commit (status: ${keyRingStatus})...`,
                );
                await setupKeyRingCoalesced();
                if (keyRingStatus === "ready") {
                    console.log(
                        "[server-link-language] key ring acquired — re-bootstrapping to recover skipped links",
                    );
                    await syncModule.bootstrap();
                    const recovered = syncModule.render();
                    if (recovered.links.length > 0) {
                        getRuntime().emitPerspectiveDiff({ additions: recovered.links, removals: [] });
                    }
                }
            }
            if (keyRingStatus === "error") {
                throw new Error(
                    "server-link-language: refusing to commit — this room's E2E key ring could not be " +
                    "acquired/decrypted, and sending plaintext to a possibly-encrypted room would be unsafe. " +
                    "Retry once connectivity/auth recovers.",
                );
            }
            // E2E is mandatory: an admin that still holds no room key it can
            // open (init's rotation failed or was skipped) rotates before it
            // commits. Non-admin "none" means the room genuinely has no E2E
            // yet — plaintext is correct for the first commit that triggers
            // the admin flow.
            if (isRoomAdmin && adminHoldsNoKey()) {
                console.log(
                    `[server-link-language] admin holds no room key (${keyRingStatus}) — rotating before commit`,
                );
                await performRotation();
                // performRotation throws on server/auth failure →
                // executor keeps the diff for retry. If it returns
                // (success or no-op), keyRingStatus may now be "ready"
                // or still "none" (no ACL member to seal to).
            }
            if (keyRingStatus === "pending") {
                throw new Error(
                    "server-link-language: refusing to commit — this room has E2E encryption enabled " +
                    "but this agent holds no room key it can open. A member holding one grants it " +
                    "when it is next online.",
                );
            }

            console.log(
                `[server-link-language] commit: ${diff.additions.length} adds, ` +
                `${diff.removals.length} removes (keyRingStatus=${keyRingStatus})`,
            );

            // Push the diff to the server; syncModule.commit stores it
            // locally only once the server accepted it, and does not echo
            // it back (the executor committed it and already holds it).
            // Awaiting the POST ensures the executor only clears its
            // pending-diff DB entry AFTER the server accepted the commit.
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
