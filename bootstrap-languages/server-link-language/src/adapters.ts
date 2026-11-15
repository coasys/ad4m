/**
 * Adapter interfaces and registry for cross-runtime abstraction.
 *
 * Combines Transport (HTTP), Storage (KV), Agent (DID + signing),
 * Runtime (event emission), WebSocketFactory, and Config into a single
 * AdapterRegistry. No ad4m:host imports here — safe for cross-runtime
 * testing. Deno-specific implementations live in adapters-deno.ts and
 * get wired up from index.ts during init().
 *
 * Usage:
 *   initAdapters({ storage, transport, agent, runtime, wsFactory, config });
 *   // later…
 *   const s = getStorage();
 *
 * Tests can init a subset — only the adapters they exercise:
 *   initAdapters({ storage: new MockStorage() });
 */

import type { LinkExpression, MembershipLinkResult } from "./types.js";

// ---------------------------------------------------------------------------
// Transport (HTTP)
// ---------------------------------------------------------------------------

export interface TransportResponse {
    status: number;
    headers: Record<string, string>;
    body: string;
}

export interface Transport {
    fetch(
        url: string,
        method: string,
        headers: Record<string, string>,
        body: string,
    ): Promise<TransportResponse>;
}

// ---------------------------------------------------------------------------
// Storage (KV)
// ---------------------------------------------------------------------------

export interface StorageAdapter {
    get(key: string): string | null;
    put(key: string, value: string): void;
    delete(key: string): void;
    listKeys(prefix?: string): string[];
}

// ---------------------------------------------------------------------------
// Agent (identity + signing)
// ---------------------------------------------------------------------------

/**
 * Thin wrapper around the `agent*()` ad4m:host imports this language
 * actually needs. Deliberately narrow — the language never signs
 * commits itself (the runtime signs diffs upstream before they reach
 * `commit()`); it only needs its own DID (for auth + peer filtering),
 * the two signature primitives auth uses, and the seed of its room key
 * X25519 key (room keys are sealed to that key, see src/encryption.ts).
 */
export interface AgentAdapter {
    did(): string;
    /** `agentSignStringHex`: Ed25519 over SHA-256 of the string, hex. */
    signStringHex(payload: string): string;
    /** `agentSign`: Ed25519 over the bytes themselves. */
    sign(payload: Uint8Array): Uint8Array;
    /**
     * `agentRoomKeySeed` (K8): the 32-byte secret of this agent's room key X25519 key. No
     * signing import signs a message in its derivation's domain.
     */
    roomKeySeed(): Promise<Uint8Array>;
}

// ---------------------------------------------------------------------------
// Runtime (event emission back into AD4M)
// ---------------------------------------------------------------------------

export interface RuntimeAdapter {
    emitPerspectiveDiff(diff: unknown): void;
    emitSyncStateChange(state: string): void;
    emitTelepresenceSignal(payload: unknown, recipientDid?: string): void;
    /**
     * `membershipLinks`: the membership links the room counted, for the executor's membership
     * of this space (the language keeps no membership state). A result per link, in order.
     */
    membershipLinks(links: LinkExpression[]): Promise<MembershipLinkResult[]>;
    /** `membershipAdmits`: whether the space admits the key now, as the executor's membership says. */
    membershipAdmits(didKey: string): Promise<{ admitted: boolean; reason?: string }>;
    /** `roomKeyEvent`: the room key rotated, or the room dropped this agent's key. */
    roomKeyEvent(event: RoomKeyEvent): Promise<void>;
}

export interface RoomKeyEvent {
    kind: "dropped" | "rotated";
    removedKeys: string[];
    version?: number;
}

// ---------------------------------------------------------------------------
// WebSocket
// ---------------------------------------------------------------------------

/** A single open (or opening) WebSocket-like connection. */
export interface WSConnection {
    send(data: string): void;
    close(code?: number, reason?: string): void;
    onOpen(cb: () => void): void;
    onMessage(cb: (data: string) => void): void;
    onClose(cb: (code: number, reason: string) => void): void;
    onError(cb: (err: unknown) => void): void;
}

export interface WebSocketFactory {
    connect(url: string): WSConnection;
}

// ---------------------------------------------------------------------------
// Config (template-variable-derived, per-instance)
// ---------------------------------------------------------------------------

export interface RoomConfig {
    serverUrl: string;
    /** `base58(sha256(creator ‖ 0x00 ‖ uid))` (src/room.ts `roomIdOf`). */
    roomId: string;
    /** The person who made the space (template parameter `CREATOR`): the room's genesis. */
    creator?: string;
    /** The space's salt (template parameter `UID`): the room's genesis. */
    uid?: string;
}

// ---------------------------------------------------------------------------
// Registry
// ---------------------------------------------------------------------------

export interface AdapterRegistry {
    transport?: Transport;
    storage?: StorageAdapter;
    agent?: AgentAdapter;
    runtime?: RuntimeAdapter;
    wsFactory?: WebSocketFactory;
    config?: RoomConfig;
}

let _registry: AdapterRegistry = {};

/**
 * Merges the supplied adapters into the registry. Accepts a partial set
 * so tests can init only what they exercise. Calling with `{ config }`
 * normalises the serverUrl (strips trailing slashes).
 */
export function initAdapters(adapters: AdapterRegistry): void {
    if (adapters.config) {
        adapters = {
            ...adapters,
            config: {
                serverUrl: adapters.config.serverUrl.replace(/\/+$/, ""),
                roomId: adapters.config.roomId,
                ...(adapters.config.creator !== undefined ? { creator: adapters.config.creator } : {}),
                ...(adapters.config.uid !== undefined ? { uid: adapters.config.uid } : {}),
            },
        };
    }
    Object.assign(_registry, adapters);
}

// ---------------------------------------------------------------------------
// Typed getters (throw if the adapter has not been registered)
// ---------------------------------------------------------------------------

export function getTransport(): Transport {
    if (!_registry.transport) {
        throw new Error("Transport not initialized. Call initAdapters() during language init().");
    }
    return _registry.transport;
}

export function getStorage(): StorageAdapter {
    if (!_registry.storage) {
        throw new Error("StorageAdapter not initialized. Call initAdapters() during language init().");
    }
    return _registry.storage;
}

export function getAgent(): AgentAdapter {
    if (!_registry.agent) {
        throw new Error("AgentAdapter not initialized. Call initAdapters() during language init().");
    }
    return _registry.agent;
}

export function getRuntime(): RuntimeAdapter {
    if (!_registry.runtime) {
        throw new Error("RuntimeAdapter not initialized. Call initAdapters() during language init().");
    }
    return _registry.runtime;
}

export function getWebSocketFactory(): WebSocketFactory {
    if (!_registry.wsFactory) {
        throw new Error("WebSocketFactory not initialized. Call initAdapters() during language init().");
    }
    return _registry.wsFactory;
}

export function getConfig(): RoomConfig {
    if (!_registry.config) {
        throw new Error("RoomConfig not initialized. Call initAdapters() during language init().");
    }
    return _registry.config;
}

// ---------------------------------------------------------------------------
// Test / teardown helpers
// ---------------------------------------------------------------------------

/** Clears every adapter. Used by tests between cases and by teardown(). */
export function resetAdapters(): void {
    _registry = {};
}
