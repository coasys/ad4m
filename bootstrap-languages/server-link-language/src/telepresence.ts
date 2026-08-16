/**
 * Telepresence state management. Pure module: holds the locally cached
 * online-agents roster (kept current via WebSocket push from ws-client.ts)
 * and forwards outbound signal/broadcast/status calls through an injected
 * `send` function — never touches ad4m:host or the WebSocket directly.
 */

import type { ClientWsMessage, DID, OnlineAgent, ServerWsMessage } from "./types.js";

export interface TelepresenceDeps {
    send: (msg: ClientWsMessage) => void;
}

let _deps: TelepresenceDeps | null = null;
let onlineAgents: Map<DID, OnlineAgent> = new Map();

export function initTelepresence(deps: TelepresenceDeps): void {
    _deps = deps;
    onlineAgents = new Map();
}

function deps(): TelepresenceDeps {
    if (!_deps) {
        throw new Error("telepresence module not initialized. Call initTelepresence() during language init().");
    }
    return _deps;
}

// ---------------------------------------------------------------------------
// Inbound (from ws-client.ts message handlers)
// ---------------------------------------------------------------------------

export function handleOnlineAgentsMessage(msg: Extract<ServerWsMessage, { type: "online-agents" }>): void {
    onlineAgents = new Map(msg.agents.map((a) => [a.did, a]));
}

export function handlePeerJoined(msg: Extract<ServerWsMessage, { type: "peer-joined" }>): void {
    if (!onlineAgents.has(msg.did)) {
        onlineAgents.set(msg.did, { did: msg.did });
    }
}

export function handlePeerLeft(msg: Extract<ServerWsMessage, { type: "peer-left" }>): void {
    onlineAgents.delete(msg.did);
}

/** Clears the roster — call on disconnect since presence is only valid
 * while the WebSocket is actually live. */
export function clearOnlineAgents(): void {
    onlineAgents = new Map();
}

// ---------------------------------------------------------------------------
// perspective telepresence capability (see index.ts)
// ---------------------------------------------------------------------------

export async function setOnlineStatus(status: unknown): Promise<void> {
    deps().send({ type: "set-online-status", status });
}

export async function getOnlineAgents(): Promise<OnlineAgent[]> {
    return Array.from(onlineAgents.values());
}

export async function sendSignal(remoteAgentDid: DID, payload: unknown): Promise<object> {
    deps().send({ type: "telepresence-signal", toDid: remoteAgentDid, payload });
    return {};
}

export async function sendBroadcast(payload: unknown): Promise<object> {
    deps().send({ type: "telepresence-broadcast", payload });
    return {};
}
