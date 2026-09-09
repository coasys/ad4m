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
import {
    buildKeyRing,
    deriveX25519KeyPair,
    generateRoomKey,
    hexToBytes,
    sealRoomKeyForRecipient,
    type KeyRing,
} from "./src/encryption.js";

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

/** The room's versioned key ring (version → decrypted AES-256-GCM key).
 * Null for a plaintext room OR while E2E setup is still in flight. */
let keyRing: KeyRing | null = null;
type KeyRingStatus = "none" | "ready" | "pending" | "error";
/**
 * - "none": room has no E2E — plaintext commits are fine.
 * - "ready": key ring acquired and decrypted — encrypted commits ready.
 * - "pending": room HAS E2E but this agent has no keys yet (freshly
 *   added member awaiting grant). commit() refuses to send plaintext.
 * - "error": E2E keys almost certainly exist but we could not get/open
 *   them. commit() refuses to send plaintext.
 */
let keyRingStatus: KeyRingStatus = "none";
/** True when this agent has been identified as the room admin. */
let isRoomAdmin = false;

function isPlaceholder(value: string): boolean {
    return !value || value === "<to-be-filled>";
}

// ---------------------------------------------------------------------------
// Startup helpers
// ---------------------------------------------------------------------------

async function setupKeyRing(): Promise<void> {
    const config = getConfig();
    try {
        const token = await auth.getValidToken();
        const keysRes = await api.fetchRoomKeys(config, token);
        if (!keysRes) {
            // 404 — room has no E2E at all.
            keyRingStatus = "none";
            keyRing = null;
            return;
        }
        if (keysRes.keys.length === 0 && keysRes.e2e_enabled) {
            // Room HAS E2E but this agent has no keys yet — a freshly
            // added member awaiting grant from the admin. Refuse to
            // commit plaintext into an encrypted room.
            keyRingStatus = "pending";
            keyRing = null;
            console.log(
                "[server-link-language] room has E2E enabled but this agent has no keys yet — " +
                "commits blocked until the room admin grants keys",
            );
            return;
        }
        if (keysRes.keys.length === 0) {
            keyRingStatus = "none";
            keyRing = null;
            return;
        }
        const { privateKey } = deriveX25519KeyPair((payload) => getAgent().signStringHex(payload));
        keyRing = buildKeyRing(keysRes.keys, privateKey);
        keyRingStatus = "ready";
        const versions = [...keyRing.keys()].sort((a, b) => a - b);
        console.log(`[server-link-language] E2E key ring acquired (${versions.length} version(s): ${versions.join(", ")})`);
    } catch (err) {
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
    const prevSize = keyRing?.size ?? 0;
    await setupKeyRing();
    const newSize = keyRing?.size ?? 0;
    if (newSize > prevSize) {
        console.log("[server-link-language] key ring refreshed — re-bootstrapping");
        await syncModule.bootstrap();
        // bootstrap() replaces the store but does not emit (by design —
        // cold-start callers query the store directly).  Recovery callers
        // must emit so the executor's perspective layer surfaces the
        // recovered links.
        const recovered = syncModule.render();
        if (recovered.links.length > 0) {
            getRuntime().emitPerspectiveDiff({ additions: recovered.links, removals: [] });
        }
        syncModule.clearPendingMissingVersions();
        return true;
    }
    return false;
}

/**
 * If this agent holds admin rights and the key ring has decrypted keys,
 * detect members missing historical key versions and re-seal those
 * versions for them. Runs after setupKeyRing and on peer-joined events.
 *
 * This closes the "late-member history" problem: when a member joins an
 * encrypted room, the admin's language instance automatically grants
 * them every historical key version the admin holds, so the new member
 * can decrypt the room's full link history.
 */
async function performAdminKeyGrants(): Promise<void> {
    if (!isRoomAdmin || !keyRing || keyRing.size === 0) return;
    const config = getConfig();
    try {
        const token = await auth.getValidToken();
        const missingRes = await api.fetchMissingKeys(config, token);
        const members = missingRes.membersNeedingHistoricalKeys;
        if (members.length === 0) return;

        for (const member of members) {
            const recipientPub = hexToBytes(member.x25519PublicKey);
            const sealedKeys: Array<{ version: number; encryptedKey: ReturnType<typeof sealRoomKeyForRecipient> }> = [];
            for (const version of member.missingVersions) {
                const roomKey = keyRing.get(version);
                if (!roomKey) continue; // admin also lacks this version — skip
                sealedKeys.push({
                    version,
                    encryptedKey: sealRoomKeyForRecipient(roomKey, recipientPub),
                });
            }
            if (sealedKeys.length === 0) continue;
            const granted = await api.grantKeys(config, token, member.did, sealedKeys);
            if (granted.length > 0) {
                console.log(
                    `[server-link-language] admin auto-granted key versions [${granted.join(", ")}] to ${member.did}`,
                );
            }
        }
    } catch (err) {
        console.error("[server-link-language] admin auto-grant failed (non-fatal):", err);
    }
}

/**
 * Client-side key rotation — generates the room key locally, seals it to
 * every ACL member's X25519 public key, and sends only the sealed
 * envelopes to the server. The server never touches plaintext key
 * material. Exposed for programmatic use but currently called from the
 * language's init flow or admin tooling.
 */
async function performRotation(): Promise<void> {
    if (!isRoomAdmin) return;
    const config = getConfig();
    try {
        const token = await auth.getValidToken();
        const aclRes = await api.fetchAclInfo(config, token);

        const roomKey = generateRoomKey();

        const sealedKeys: api.RotateKeyEntry[] = [];
        for (const member of aclRes.members) {
            if (!member.x25519PublicKey) continue;
            const recipientPub = hexToBytes(member.x25519PublicKey);
            sealedKeys.push({
                did: member.did,
                encryptedKey: sealRoomKeyForRecipient(roomKey, recipientPub),
            });
        }

        if (sealedKeys.length === 0) {
            console.log("[server-link-language] no members with X25519 keys — skipping rotation");
            return;
        }

        const result = await api.rotateKeys(config, token, sealedKeys);
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

        configured = !isPlaceholder(SERVER_URL) && !isPlaceholder(ROOM_ID);
        if (!configured) {
            console.log(
                `[server-link-language] init: did=${myDid}, template variables not filled in — ` +
                "running inert until published with SERVER_URL/ROOM_ID.",
            );
            return;
        }

        initAdapters({ config: { serverUrl: SERVER_URL, roomId: ROOM_ID } });
        const config = getConfig();

        syncModule.initSync({
            config,
            getToken: () => auth.getValidToken(),
            emitDiff: (diff) => getRuntime().emitPerspectiveDiff(diff),
            emitSyncState: (state) => getRuntime().emitSyncStateChange(state),
            getKeyRing: () => keyRing,
            refreshKeyRing: async () => {
                const prevSize = keyRing?.size ?? 0;
                await setupKeyRing();
                return (keyRing?.size ?? 0) > prevSize;
            },
            // Periodic admin key grants — fallback for when the WS
            // `peer-joined` event never fires (WS down, CI, firewalls).
            // Each successful HTTP sync triggers a check so the admin
            // discovers new members who need historical keys.
            onPostSync: () => performAdminKeyGrants(),
        });

        wsClient = new WsClient({
            getUrl: async () => api.wsUrl(config),
            getToken: () => auth.getValidToken(),
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
                    void performAdminKeyGrants().catch((err) => {
                        console.error("[server-link-language] peer-joined admin grant failed:", err);
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

            // Determine admin status — used by performAdminKeyGrants.
            try {
                const aclInfo = await api.fetchAclInfo(config, await auth.getValidToken());
                isRoomAdmin = aclInfo.admin === myDid;
            } catch {
                isRoomAdmin = false;
            }

            await setupKeyRing();

            // E2E is mandatory — if admin and no E2E exists yet,
            // generate the initial room key. Every room gets encrypted
            // automatically; there is no plaintext mode.
            if (isRoomAdmin && keyRingStatus === "none") {
                console.log(
                    "[server-link-language] admin, no E2E yet — generating initial room key",
                );
                await performRotation();
            }

            await syncModule.bootstrap();

            // E2E timing fix: if keyRingStatus is still "pending" after
            // bootstrap, the admin may have granted keys between our
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

            // If admin and key ring ready, grant historical keys to any
            // members who joined while we were offline.
            void performAdminKeyGrants().catch((err) => {
                console.error("[server-link-language] initial admin grant check failed:", err);
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

        console.log(`[server-link-language] init complete: did=${myDid}, room=${ROOM_ID}`);
    },

    async teardown() {
        // Drain any pending batched commits BEFORE we tear down auth/adapters.
        // The main commit path now awaits the POST directly, but the batch
        // infrastructure still exists (used by unit tests, retry timers) and
        // may hold stale segments from a failed flush cycle.
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
            if (keyRingStatus === "error" || keyRingStatus === "pending") {
                console.log(
                    `[server-link-language] retrying E2E key ring acquisition before commit (status: ${keyRingStatus})...`,
                );
                await setupKeyRing();
            }
            if (keyRingStatus === "error") {
                throw new Error(
                    "server-link-language: refusing to commit — this room's E2E key ring could not be " +
                    "acquired/decrypted, and sending plaintext to a possibly-encrypted room would be unsafe. " +
                    "Retry once connectivity/auth recovers.",
                );
            }
            if (keyRingStatus === "pending") {
                throw new Error(
                    "server-link-language: refusing to commit — this room has E2E encryption enabled " +
                    "but this agent has not received room keys yet. The room admin must grant keys " +
                    "before this agent can write.",
                );
            }

            // E2E is mandatory — if admin and keyRingStatus is still
            // "none" (init's performRotation failed or was skipped),
            // retry rotation before committing. Non-admin "none" means
            // the room genuinely has no E2E yet — plaintext is correct
            // for the first commit that triggers the admin flow.
            if (isRoomAdmin && keyRingStatus === "none") {
                console.log(
                    "[server-link-language] admin with keyRingStatus=none — retrying rotation before commit",
                );
                await performRotation();
                // performRotation throws on server/auth failure →
                // executor keeps the diff for retry. If it returns
                // (success or no-op), keyRingStatus may now be "ready"
                // or still "none" (no members with X25519 keys yet).
            }

            console.log(
                `[server-link-language] commit: ${diff.additions.length} adds, ` +
                `${diff.removals.length} removes (keyRingStatus=${keyRingStatus})`,
            );

            // 1. Store links locally (plaintext, always — see src/sync.ts module doc).
            store.applyDiff(diff);

            // 2. Push the diff to the server. Awaiting the POST ensures
            //    the executor only clears its pending-diff DB entry AFTER
            //    the server accepted the commit. The executor's own
            //    batching (pending_diffs_loop: 1s inactivity / 3s max /
            //    150 max count) handles coalescence for burst scenarios
            //    — the language-level fire-and-forget batch is no longer
            //    needed on this path.
            await syncModule.commit(diff);

            // 3. Emit so local subscribers see it after the POST landed.
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
