/**
 * WebSocket client for the link-server real-time channel
 * (`/rooms/:roomId/ws?token=<jwt>`), with auto-reconnect (exponential
 * backoff + jitter) and an outbound send queue that flushes on
 * (re)connect.
 *
 * Pure module: depends only on the WebSocketFactory singleton from
 * adapters.ts (a native `WebSocket` wrapper in production, a mock in
 * tests) and plain typed message handlers — never imports ad4m:host.
 */

import { getWebSocketFactory } from "./adapters.js";
import type { WSConnection } from "./adapters.js";
import type { ClientWsMessage, ServerWsMessage } from "./types.js";

export interface WsClientHandlers {
    onDiff(msg: Extract<ServerWsMessage, { type: "diff" }>): void;
    onTelepresenceSignal(msg: Extract<ServerWsMessage, { type: "telepresence-signal" }>): void;
    onTelepresenceBroadcast(msg: Extract<ServerWsMessage, { type: "telepresence-broadcast" }>): void;
    onOnlineAgents(msg: Extract<ServerWsMessage, { type: "online-agents" }>): void;
    onPeerJoined(msg: Extract<ServerWsMessage, { type: "peer-joined" }>): void;
    onPeerLeft(msg: Extract<ServerWsMessage, { type: "peer-left" }>): void;
    /** Called on every successful (re)connect — the natural point to kick
     * off an HTTP catch-up sync in case anything was missed while offline. */
    onOpen?(): void;
    onClose?(): void;
    onReconnecting?(attempt: number, delayMs: number): void;
}

export interface WsClientOptions {
    /** Resolves to a fresh, fully-qualified ws(s):// URL (including a
     * valid token). Called before every connection attempt, including
     * reconnects, so it should refresh an expired token itself. */
    getUrl: () => Promise<string>;
    handlers: WsClientHandlers;
    minBackoffMs?: number;
    maxBackoffMs?: number;
}

const DEFAULT_MIN_BACKOFF_MS = 500;
const DEFAULT_MAX_BACKOFF_MS = 30_000;

export class WsClient {
    private readonly factory = getWebSocketFactory();
    private readonly opts: WsClientOptions;
    private readonly minBackoffMs: number;
    private readonly maxBackoffMs: number;

    private conn: WSConnection | null = null;
    private connected = false;
    private closedByUser = true;
    private reconnectAttempt = 0;
    private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
    private sendQueue: string[] = [];

    constructor(opts: WsClientOptions) {
        this.opts = opts;
        this.minBackoffMs = opts.minBackoffMs ?? DEFAULT_MIN_BACKOFF_MS;
        this.maxBackoffMs = opts.maxBackoffMs ?? DEFAULT_MAX_BACKOFF_MS;
    }

    async connect(): Promise<void> {
        this.closedByUser = false;
        this.reconnectAttempt = 0;
        await this.openConnection();
    }

    close(): void {
        this.closedByUser = true;
        if (this.reconnectTimer) {
            clearTimeout(this.reconnectTimer);
            this.reconnectTimer = null;
        }
        if (this.sendQueue.length > 0) {
            console.warn(
                `[server-link-language] WsClient.close(): dropping ${this.sendQueue.length} ` +
                "queued outbound message(s) — they were enqueued while the socket was disconnected " +
                "and will not be delivered.",
            );
        }
        this.sendQueue = [];
        this.connected = false;
        if (this.conn) {
            this.conn.close(1000, "teardown");
            this.conn = null;
        }
    }

    send(msg: ClientWsMessage): void {
        const data = JSON.stringify(msg);
        if (this.conn && this.connected) {
            this.conn.send(data);
        } else {
            this.sendQueue.push(data);
        }
    }

    isOpen(): boolean {
        return this.connected;
    }

    // -------------------------------------------------------------------
    // Internals
    // -------------------------------------------------------------------

    private async openConnection(): Promise<void> {
        if (this.closedByUser) return;

        let url: string;
        try {
            url = await this.opts.getUrl();
        } catch (err) {
            console.error("[server-link-language] failed to resolve websocket URL:", err);
            this.scheduleReconnect();
            return;
        }
        if (this.closedByUser) return; // closed while awaiting getUrl()

        const conn = this.factory.connect(url);
        this.conn = conn;

        conn.onOpen(() => {
            this.connected = true;
            this.reconnectAttempt = 0;
            this.flushQueue();
            this.opts.handlers.onOpen?.();
        });

        conn.onMessage((data) => this.handleMessage(data));

        conn.onClose(() => {
            const wasConnected = this.connected;
            this.connected = false;
            this.conn = null;
            if (wasConnected) this.opts.handlers.onClose?.();
            if (!this.closedByUser) this.scheduleReconnect();
        });

        conn.onError((err) => {
            console.error("[server-link-language] websocket error:", err);
        });
    }

    private handleMessage(data: string): void {
        let msg: ServerWsMessage;
        try {
            msg = JSON.parse(data) as ServerWsMessage;
        } catch (err) {
            console.error("[server-link-language] failed to parse websocket message:", err);
            return;
        }

        switch (msg.type) {
            case "diff":
                this.opts.handlers.onDiff(msg);
                break;
            case "telepresence-signal":
                this.opts.handlers.onTelepresenceSignal(msg);
                break;
            case "telepresence-broadcast":
                this.opts.handlers.onTelepresenceBroadcast(msg);
                break;
            case "online-agents":
                this.opts.handlers.onOnlineAgents(msg);
                break;
            case "peer-joined":
                this.opts.handlers.onPeerJoined(msg);
                break;
            case "peer-left":
                this.opts.handlers.onPeerLeft(msg);
                break;
            default:
                console.warn(
                    "[server-link-language] unknown websocket message type:",
                    (msg as { type?: unknown }).type,
                );
        }
    }

    private scheduleReconnect(): void {
        if (this.closedByUser || this.reconnectTimer) return;
        const attempt = this.reconnectAttempt++;
        const backoff = Math.min(this.minBackoffMs * 2 ** attempt, this.maxBackoffMs);
        const jitter = Math.random() * Math.min(250, backoff);
        const delay = backoff + jitter;
        this.opts.handlers.onReconnecting?.(attempt + 1, delay);
        this.reconnectTimer = setTimeout(() => {
            this.reconnectTimer = null;
            void this.openConnection();
        }, delay);
    }

    private flushQueue(): void {
        if (!this.conn) return;
        for (const data of this.sendQueue.splice(0)) {
            this.conn.send(data);
        }
    }
}
