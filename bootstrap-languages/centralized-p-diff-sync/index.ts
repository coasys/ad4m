/**
 * # Centralized Perspective Diff Sync
 *
 * Link language that syncs Perspectives via a centralized socket.io server.
 * Implements perspective-sync, perspective-commit, peers, and telepresence.
 */

import { io } from "https://esm.sh/v135/socket.io-client@4.7.2";
import { Mutex, withTimeout } from "https://esm.sh/v135/async-mutex@0.4.0";
import axiod from "https://deno.land/x/axiod/mod.ts";
import { defineLanguage, agentDid } from "@coasys/ad4m-ldk";

//!@ad4m-template-variable
const uid = "centralized-perspective-diff-sync-uuid";

let myDid: string = "";
let socketClient: any = null;
let linkCallback: ((diff: any) => void) | null = null;
let syncStateChangeCallback: ((state: string) => void) | null = null;
let myCurrentTime: any = null;
let hasCalledSync = false;
const generalMutex: Mutex = withTimeout(new Mutex(), 10000, new Error("PerspectiveDiffSync: generalMutex timeout"));

function updateServerSyncState(): void {
    if (myCurrentTime) {
        socketClient.emit(
            "update-sync-state",
            { did: myDid, date: myCurrentTime, linkLanguageUUID: uid },
            (err: any, _signal: any) => {
                if (err) {
                    console.error("Error in update-sync-state call", err);
                }
            }
        );
    }
}

function handleSignal(signal: any): void {
    if (linkCallback) {
        linkCallback(signal);
    }
}

function prepareLinkExpression(link: any): object {
    const data = Object.assign(link);
    if (data.data.source == "") data.data.source = null;
    if (data.data.target == "") data.data.target = null;
    if (data.data.predicate == "") data.data.predicate = null;
    if (data.data.source == undefined) data.data.source = null;
    if (data.data.target == undefined) data.data.target = null;
    if (data.data.predicate == undefined) data.data.predicate = null;
    return data;
}

const language = defineLanguage({
    //!@ad4m-template-variable
    name: "centralized-perspective-diff-sync",
    version: "0.1.0",

    isPublic: false,

    async init() {
        myDid = agentDid();

        socketClient = io("https://socket.ad4m.dev", {
            transports: ["websocket", "polling"],
            autoConnect: true,
            query: { did: myDid, linkLanguageUUID: uid },
        });
        console.log("Created socket connection");

        socketClient.on("error", (error: any) => {
            console.error("Error:", error);
        });

        socketClient.on("connect", async () => {
            console.log("Connected to the server");
            try {
                console.log("Trying to join room", uid);
                socketClient.emit("join-room", uid);
                console.log("Sent the join-room signal");
            } catch (e) {
                console.error("Error in socket connection: ", e);
            }
        });

        socketClient.on("signal-emit", async (signal: any) => {
            try {
                const serverRecordTimestamp = signal.serverRecordTimestamp;
                if (myCurrentTime && myCurrentTime < serverRecordTimestamp) {
                    myCurrentTime = serverRecordTimestamp;
                    updateServerSyncState();
                    handleSignal(signal.payload);
                }
            } catch (e) {
                console.error("PerspectiveDiffSync.signal-emit(); got error", e);
            }
        });

        socketClient.on("disconnect", () => {
            console.log("Disconnected from the server");
        });

        socketClient.on("connect_error", (error: any) => {
            console.error("Connection Error:", error);
        });

        socketClient.on("reconnect", (_attemptNumber: any) => {
            console.log("Reconnected to the server on attempt:", _attemptNumber);
            hasCalledSync = false;
            language.perspectiveSyncSync?.();
        });

        socketClient.on("reconnect_attempt", () => {
            console.log("Trying to reconnect...");
        });

        socketClient.on("telepresence-signal", (_payload: any) => {
            // Phase 0 transitional — telepresence signals will route via emitTelepresenceSignal.
        });
    },

    async teardown() {
        if (socketClient) {
            socketClient.disconnect();
            socketClient = null;
        }
        myDid = "";
        linkCallback = null;
        syncStateChangeCallback = null;
        myCurrentTime = null;
        hasCalledSync = false;
    },

    interactions() { return []; },

    perspectiveSync: {
        async sync() {
            if (!hasCalledSync) {
                try {
                    socketClient.emit(
                        "sync",
                        { linkLanguageUUID: uid, did: myDid },
                        (err: any, signal: any) => {
                            if (err) {
                                console.error("Error in sync call", err);
                                throw Error(err);
                            }

                            myCurrentTime = signal.serverRecordTimestamp;
                            updateServerSyncState();
                            hasCalledSync = true;

                            if (signal.payload.additions.length > 0 || signal.payload.removals.length > 0) {
                                handleSignal(signal.payload);
                            }

                            if (syncStateChangeCallback) {
                                syncStateChangeCallback("Synced");
                            }
                        }
                    );
                } catch (e) {
                    console.error("PerspectiveDiffSync.sync(); got error", e);
                }
            }
            return { additions: [], removals: [] };
        },

        async render() {
            return new Promise((resolve, reject) => {
                socketClient.emit("render", { linkLanguageUUID: uid }, (err: any, signal: any) => {
                    if (err) {
                        console.error("Error in render call", err);
                        return reject(err);
                    }
                    myCurrentTime = signal.serverRecordTimestamp;
                    updateServerSyncState();
                    resolve({ links: signal.payload });
                });
            });
        },

        async currentRevision() {
            let result;
            try {
                result = await axiod.post("https://socket.ad4m.dev/currentRevision", {
                    linkLanguageUUID: uid,
                    did: myDid,
                });
                if (result.status === 200) {
                    result = result.data;
                } else {
                    console.error("Error in currentRevision call, got status", result.status);
                    result = null;
                }
            } catch (e) {
                console.log("Error in currentRevision call", e);
                result = null;
            }

            if (result) {
                if (result.currentRevision === null) {
                    return "";
                } else {
                    return result.currentRevision;
                }
            }
            return "";
        },
    },

    perspectiveCommit: {
        async commit(diff: any) {
            try {
                const preppedDiff = {
                    additions: diff.additions.map(prepareLinkExpression),
                    removals: diff.removals.map(prepareLinkExpression),
                    linkLanguageUUID: uid,
                    did: myDid,
                };

                const signal: any = await new Promise((resolve, reject) => {
                    socketClient.emit("commit", preppedDiff, (err: any, signal: any) => {
                        if (err) {
                            console.error("Error in commit call", err);
                            reject(err);
                        } else {
                            resolve(signal);
                        }
                    });
                });

                if (signal.status === "Ok") {
                    myCurrentTime = signal.serverRecordTimestamp;
                    updateServerSyncState();
                    return "";
                } else {
                    throw new Error("Commit failed with non-Ok status");
                }
            } catch (e) {
                console.error("PerspectiveDiffSync.commit(); got error", e);
                throw e;
            }
        },
    },

    peers: {
        async remote() {
            const others = await axiod.get("https://socket.ad4m.dev/getOthers", {
                params: { linkLanguageUUID: uid },
            });
            if (others.status === 200) {
                const othersIndex = others.data.indexOf(myDid);
                if (othersIndex > -1) {
                    others.data.splice(othersIndex, 1);
                }
                return others.data;
            } else {
                console.error("Error fetching others in linkAdapter, got status", others.status);
                return [];
            }
        },

        async setLocal(_agents: string[]) {
            // Centralized sync doesn't need local agent tracking
        },
    },

    telepresence: {
        async setOnlineStatus(status: any) {
            const res = await axiod.post("https://socket.ad4m.dev/setAgentStatus", {
                did: myDid,
                status: status,
                linkLanguageUUID: uid,
            });
            if (res.status === 200) {
                console.log("setOnlineStatus: success");
            }
        },

        async getOnlineAgents() {
            const result = await axiod.get("https://socket.ad4m.dev/getOnlineAgents", {
                params: { did: myDid, linkLanguageUUID: uid },
            });
            if (result.status === 200) {
                return result.data;
            } else {
                return [];
            }
        },

        async sendSignal(remoteAgentDid: string, payload: any) {
            socketClient.emit(
                "send-signal",
                { remoteAgentDid, linkLanguageUUID: uid, payload },
                (err: any, _signal: any) => {
                    if (err) {
                        console.log("sendSignal: failed");
                        console.dir(err);
                    }
                }
            );
            return {};
        },

        async sendBroadcast(payload: any) {
            socketClient.emit(
                "send-broadcast",
                { linkLanguageUUID: uid, payload },
                (err: any, _signal: any) => {
                    if (err) {
                        console.log("sendBroadcast: failed");
                        console.dir(err);
                    }
                }
            );
            return {};
        },

        async registerSignalCallback(_callback: any) {
            // Signal registration handled by runtime via handleTelepresenceSignal
        },
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
} = language;

// =============================================================================
// Phase 0 transitional: callback registration — not in v1 capability spec,
// read directly by the runtime.
// =============================================================================

export function linkSyncAddCallback(callback: (diff: any) => void): number {
    linkCallback = callback;
    return 1;
}

export function linkSyncRemoveCallback(callback: (diff: any) => void): number {
    if (linkCallback === callback) linkCallback = null;
    return 1;
}

export function linkSyncAddSyncStateChangeCallback(callback: (state: string) => void): number {
    syncStateChangeCallback = callback;
    return 1;
}
