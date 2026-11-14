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
    onKeysGranted() {},
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

describe("ws-client: keys-granted", () => {
    it("hands a key grant to onKeysGranted", async () => {
        const factory = new MockFactory();
        initAdapters({ wsFactory: factory });
        const grants: number[][] = [];
        client = new WsClient({
            getUrl: async () => "wss://server.example/rooms/r/ws",
            getToken: async () => "token-1",
            handlers: { ...noopHandlers, onKeysGranted: (msg) => grants.push(msg.versions) },
        });

        await client.connect();
        const conn = factory.connections[0];
        conn.open();
        await until(() => conn.sent.length > 0, "auth frame");
        conn.receive({ type: "online-agents", agents: [] });
        conn.receive({ type: "keys-granted", versions: [1, 2] });

        assert.deepEqual(grants, [[1, 2]]);
    });
});

describe("ws-client: protocol 2 messages", () => {
    async function connected(handlers: Partial<WsClientHandlers>, factory: MockFactory): Promise<MockConnection> {
        client = new WsClient({
            getUrl: async () => "wss://server.example/rooms/r/ws",
            getToken: async () => "token-1",
            handlers: { ...noopHandlers, ...handlers },
            minBackoffMs: 1,
            maxBackoffMs: 5,
        });
        await client.connect();
        const conn = factory.connections[factory.connections.length - 1];
        conn.open();
        await until(() => conn.sent.length > 0, "auth frame");
        conn.receive({ type: "online-agents", agents: [] });
        return conn;
    }

    it("hands membership links and rotate-needed to their handlers", async () => {
        const factory = new MockFactory();
        initAdapters({ wsFactory: factory });
        const membership: Array<{ links: unknown[]; seq: number }> = [];
        const rotations: Array<{ reason: string; removedKeys: string[] }> = [];
        const conn = await connected(
            {
                onMembership: (msg) => membership.push({ links: msg.links, seq: msg.seq }),
                onRotateNeeded: (msg) => rotations.push({ reason: msg.reason, removedKeys: msg.removedKeys }),
            },
            factory,
        );
        const link = {
            author: "did:key:zA",
            timestamp: "2026-11-15T00:00:00.000Z",
            data: { source: "did:scid:coasys:1:A", predicate: "coasys://membership/member", target: "coasys://membership/space" },
            proof: { key: "did:key:zA", signature: "00" },
        };
        conn.receive({ type: "membership", links: [link], seq: 4 });
        conn.receive({ type: "rotate-needed", reason: "removed", removedKeys: ["did:key:zGone"] });

        assert.deepEqual(membership, [{ links: [link], seq: 4 }]);
        assert.deepEqual(rotations, [{ reason: "removed", removedKeys: ["did:key:zGone"] }]);
    });

    it("on close 4005 (this key removed or revoked) reports it and does not reconnect", async () => {
        const factory = new MockFactory();
        initAdapters({ wsFactory: factory });
        const dropped: Array<{ code: number; reason: string }> = [];
        const conn = await connected({ onRoomClosed: (code, reason) => dropped.push({ code, reason }) }, factory);
        conn.close(4005, "removed");

        await new Promise((r) => setTimeout(r, 50));
        assert.deepEqual(dropped, [{ code: 4005, reason: "removed" }]);
        assert.equal(factory.connections.length, 1, "a removed key does not reconnect");
    });

    it("on close 4006 (room suspended) reports it and reconnects", async () => {
        const factory = new MockFactory();
        initAdapters({ wsFactory: factory });
        const dropped: number[] = [];
        const conn = await connected({ onRoomClosed: (code) => dropped.push(code) }, factory);
        conn.close(4006, "suspended");

        await until(() => factory.connections.length === 2, "reconnect after a suspension");
        assert.deepEqual(dropped, [4006]);
    });
});
