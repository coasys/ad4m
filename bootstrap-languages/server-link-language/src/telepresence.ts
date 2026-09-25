/**
 * Telepresence state management. Pure module: holds the locally cached
 * online-agents roster (kept current via WebSocket push from ws-client.ts)
 * and forwards outbound signal/broadcast/status calls through an injected
 * `send` function — never touches ad4m:host or the WebSocket directly.
 */

import type { ClientWsMessage, DID, OnlineAgent, ServerWsMessage } from "./types.js";

export interface TelepresenceDeps {
    send: (msg: ClientWsMessage) => void;
    /** Local DID. The server broadcasts the full room roster (including us)
     * on every `set-online-status`, so we must filter self out client-side.
     * `""` (never set) means no filter — safe fallback for tests. */
    getMyDid: () => string;
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

export function handleStatusChanged(msg: Extract<ServerWsMessage, { type: "status-changed" }>): void {
    const existing = onlineAgents.get(msg.did);
    if (existing) {
        existing.status = msg.status;
    } else {
        onlineAgents.set(msg.did, { did: msg.did, status: msg.status });
    }
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
    // - Drop self. The server rebroadcasts the FULL room roster (including
    //   the caller) on every `set-online-status`, so without this the local
    //   agent appears in its own `onlineAgents` list.
    // - Substitute an empty PerspectiveExpression for peers we've only seen
    //   via `peer-joined` but who haven't yet called `setOnlineStatus`.
    //   Semantics: they ARE online and should be visible, they just don't
    //   have a status to show yet. Also required by the executor's schema
    //   (rust-executor/src/types/domain.rs::OnlineAgent), which serialises
    //   `status: PerspectiveExpression` as non-optional — returning a bare
    //   `{did}` triggers `RpcError: RPC error 500: missing field \`status\``.
    const myDid = deps().getMyDid();
    return Array.from(onlineAgents.values())
        .filter((a) => a.did !== myDid)
        .map((a) => (a.status !== undefined ? a : { did: a.did, status: emptyPerspectiveExpression() }));
}

/**
 * A structurally-valid but empty `PerspectiveExpression` — matches
 * `#[derive(Default)]` on the executor's Rust type
 * (rust-executor/src/types/domain.rs::PerspectiveExpression). Used as the
 * placeholder status for peers we know are online but who haven't yet
 * broadcast one.
 */
function emptyPerspectiveExpression(): Record<string, unknown> {
    return {
        author: "",
        data: { links: [] },
        proof: { key: "", signature: "" },
        timestamp: "",
    };
}

// Return type mandated by @coasys/ad4m-ldk TelepresenceCapability — no
// meaningful response data; the empty object satisfies the interface.
export async function sendSignal(remoteAgentDid: DID, payload: unknown): Promise<object> {
    deps().send({ type: "telepresence-signal", toDid: remoteAgentDid, payload });
    return {};
}

export async function sendBroadcast(payload: unknown): Promise<object> {
    deps().send({ type: "telepresence-broadcast", payload });
    return {};
}
