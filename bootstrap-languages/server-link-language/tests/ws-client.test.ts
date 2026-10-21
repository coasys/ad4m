/**
 * Tests for src/ws-client.ts with a mock WebSocket factory: the
 * first-message auth handshake and what happens when the server rejects
 * the token.
 */

import { describe, it, afterEach } from "node:test";
import assert from "node:assert/strict";

import type { WebSocketFactory, WSConnection } from "../src/adapters.js";
import { initAdapters, resetAdapters } from "../src/adapters.js";
import { WsClient, type WsClientHandlers } from "../src/ws-client.js";

class MockConnection implements WSConnection {
    sent: string[] = [];
    closed: { code?: number; reason?: string } | null = null;
    private openCb: (() => void) | null = null;
    private messageCb: ((data: string) => void) | null = null;
    private closeCb: ((code: number, reason: string) => void) | null = null;

    send(data: string): void { this.sent.push(data); }
    close(code?: number, reason?: string): void {
        if (this.closed) return;
        this.closed = { code, reason };
        this.closeCb?.(code ?? 1000, reason ?? "");
    }
    onOpen(cb: () => void): void { this.openCb = cb; }
    onMessage(cb: (data: string) => void): void { this.messageCb = cb; }
    onClose(cb: (code: number, reason: string) => void): void { this.closeCb = cb; }
    onError(_cb: (err: unknown) => void): void {}

    open(): void { this.openCb?.(); }
    receive(msg: unknown): void { this.messageCb?.(JSON.stringify(msg)); }
    authToken(): string | undefined {
        const first = this.sent[0];
        return first ? (JSON.parse(first) as { token?: string }).token : undefined;
    }
}

class MockFactory implements WebSocketFactory {
    connections: MockConnection[] = [];
    connect(_url: string): WSConnection {
        const c = new MockConnection();
        this.connections.push(c);
        return c;
    }
}

const noopHandlers: WsClientHandlers = {
    onDiff() {},
    onTelepresenceSignal() {},
    onTelepresenceBroadcast() {},
    onOnlineAgents() {},
    onPeerJoined() {},
    onPeerLeft() {},
};

async function until(cond: () => boolean, label: string, timeoutMs = 2000): Promise<void> {
    const start = Date.now();
    while (!cond()) {
        if (Date.now() - start > timeoutMs) throw new Error(`timed out waiting for ${label}`);
        await new Promise((r) => setTimeout(r, 5));
    }
}

let client: WsClient | null = null;

afterEach(() => {
    client?.close();
    client = null;
    resetAdapters();
});

describe("ws-client: auth-error", () => {
    it("drops the rejected token and authenticates the reconnect with a new one", async () => {
        const factory = new MockFactory();
        initAdapters({ wsFactory: factory });

        // A token cache like auth.getValidToken: the same token until it is
        // invalidated, then a fresh one.
        let issued = 0;
        let cached: string | null = null;
        let invalidations = 0;
        client = new WsClient({
            getUrl: async () => "wss://server.example/rooms/r/ws",
            getToken: async () => (cached ??= `token-${++issued}`),
            invalidateToken: () => { invalidations++; cached = null; },
            handlers: noopHandlers,
            minBackoffMs: 1,
            maxBackoffMs: 5,
        });

        await client.connect();
        const first = factory.connections[0];
        first.open();
        await until(() => first.sent.length > 0, "first auth frame");
        assert.equal(first.authToken(), "token-1");

        first.receive({ type: "auth-error", error: "session expired or revoked" });
        assert.ok(first.closed, "the rejected socket is closed");
        assert.equal(invalidations, 1);

        await until(() => factory.connections.length === 2, "reconnect");
        const second = factory.connections[1];
        second.open();
        await until(() => second.sent.length > 0, "second auth frame");
        assert.equal(second.authToken(), "token-2", "the reconnect must not reuse the rejected token");
    });

    it("keeps the token when the socket closes without an auth-error", async () => {
        const factory = new MockFactory();
        initAdapters({ wsFactory: factory });
        let invalidations = 0;
        client = new WsClient({
            getUrl: async () => "wss://server.example/rooms/r/ws",
            getToken: async () => "token-1",
            invalidateToken: () => { invalidations++; },
            handlers: noopHandlers,
            minBackoffMs: 1,
            maxBackoffMs: 5,
        });

        await client.connect();
        const first = factory.connections[0];
        first.open();
        await until(() => first.sent.length > 0, "auth frame");
        first.receive({ type: "online-agents", agents: [] });
        first.close(1006, "network");

        await until(() => factory.connections.length === 2, "reconnect");
        assert.equal(invalidations, 0);
    });
});
