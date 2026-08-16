/**
 * Adapter interfaces and singletons for cross-runtime abstraction.
 *
 * Combines Transport (HTTP), Storage (KV), Agent (DID + signing),
 * WebSocketFactory, and Config singletons. No ad4m:host imports here —
 * safe for cross-runtime testing. Deno-specific implementations live in
 * adapters-deno.ts and are only wired up from index.ts during init().
 */

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

let _transport: Transport | null = null;

export function initTransport(transport: Transport): void {
    _transport = transport;
}

export function getTransport(): Transport {
    if (!_transport) {
        throw new Error(
            "Transport not initialized. Call initTransport() during language init().",
        );
    }
    return _transport;
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

let _storage: StorageAdapter | null = null;

export function initStorage(adapter: StorageAdapter): void {
    _storage = adapter;
}

export function getStorage(): StorageAdapter {
    if (!_storage) {
        throw new Error(
            "StorageAdapter not initialized. Call initStorage() during language init().",
        );
    }
    return _storage;
}

// ---------------------------------------------------------------------------
// Agent (identity + signing)
// ---------------------------------------------------------------------------

/**
 * Thin wrapper around the `agent*()` ad4m:host imports this language
 * actually needs. Deliberately narrow — the language never signs
 * commits itself (the runtime signs diffs upstream before they reach
 * `commit()`); it only needs its own DID (for auth + peer filtering)
 * and a deterministic signature primitive (for auth + X25519 key
 * derivation, see src/encryption.ts).
 */
export interface AgentAdapter {
    did(): string;
    signStringHex(payload: string): string;
}

let _agent: AgentAdapter | null = null;

export function initAgent(adapter: AgentAdapter): void {
    _agent = adapter;
}

export function getAgent(): AgentAdapter {
    if (!_agent) {
        throw new Error(
            "AgentAdapter not initialized. Call initAgent() during language init().",
        );
    }
    return _agent;
}

// ---------------------------------------------------------------------------
// Runtime (event emission back into AD4M)
// ---------------------------------------------------------------------------

export interface RuntimeAdapter {
    emitPerspectiveDiff(diff: unknown): void;
    emitSyncStateChange(state: string): void;
    emitTelepresenceSignal(payload: unknown, recipientDid?: string): void;
}

let _runtime: RuntimeAdapter | null = null;

export function initRuntime(adapter: RuntimeAdapter): void {
    _runtime = adapter;
}

export function getRuntime(): RuntimeAdapter {
    if (!_runtime) {
        throw new Error(
            "RuntimeAdapter not initialized. Call initRuntime() during language init().",
        );
    }
    return _runtime;
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

let _wsFactory: WebSocketFactory | null = null;

export function initWebSocketFactory(factory: WebSocketFactory): void {
    _wsFactory = factory;
}

export function getWebSocketFactory(): WebSocketFactory {
    if (!_wsFactory) {
        throw new Error(
            "WebSocketFactory not initialized. Call initWebSocketFactory() during language init().",
        );
    }
    return _wsFactory;
}

// ---------------------------------------------------------------------------
// Config (template-variable-derived, per-instance)
// ---------------------------------------------------------------------------

export interface RoomConfig {
    serverUrl: string;
    roomId: string;
}

let _config: RoomConfig | null = null;

export function initConfig(config: RoomConfig): void {
    // Strip a trailing slash so `${serverUrl}/rooms/...` never double-slashes.
    _config = {
        serverUrl: config.serverUrl.replace(/\/+$/, ""),
        roomId: config.roomId,
    };
}

export function getConfig(): RoomConfig {
    if (!_config) {
        throw new Error(
            "RoomConfig not initialized. Call initConfig() during language init().",
        );
    }
    return _config;
}

// ---------------------------------------------------------------------------
// Test / teardown helpers
// ---------------------------------------------------------------------------

/** Clears every singleton. Used by tests between cases and by teardown(). */
export function resetAdapters(): void {
    _transport = null;
    _storage = null;
    _agent = null;
    _runtime = null;
    _wsFactory = null;
    _config = null;
}
