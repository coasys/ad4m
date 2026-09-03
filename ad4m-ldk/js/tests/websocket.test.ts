import { describe, it, beforeEach, afterEach } from "node:test";
import assert from "node:assert/strict";
import { DenoWebSocketFactory, type WSConnection, type WebSocketFactory } from "../src/websocket.js";

class MockWebSocket {
    url: string;
    listeners: Record<string, Array<(e: any) => void>> = {};
    sentMessages: string[] = [];
    closed = false;
    closeCode?: number;
    closeReason?: string;
    readyState = 1; // OPEN

    constructor(url: string) {
        this.url = url;
    }

    send(data: string): void {
        this.sentMessages.push(data);
    }

    close(code?: number, reason?: string): void {
        this.closed = true;
        this.closeCode = code;
        this.closeReason = reason;
    }

    addEventListener(type: string, listener: (e: any) => void): void {
        (this.listeners[type] ??= []).push(listener);
    }

    emit(type: string, event: any): void {
        for (const cb of this.listeners[type] ?? []) cb(event);
    }
}

let originalWebSocket: any;
let lastMock: MockWebSocket | undefined;

beforeEach(() => {
    originalWebSocket = (globalThis as any).WebSocket;
    (globalThis as any).WebSocket = class extends MockWebSocket {
        constructor(url: string) {
            super(url);
            lastMock = this;
        }
    };
});

afterEach(() => {
    (globalThis as any).WebSocket = originalWebSocket;
    lastMock = undefined;
});

describe("websocket: interfaces", () => {
    it("WSConnection shape matches the contract", () => {
        const conn: WSConnection = {
            send(_d: string) {},
            close(_c?: number, _r?: string) {},
            onOpen(_cb) {},
            onMessage(_cb) {},
            onClose(_cb) {},
            onError(_cb) {},
        };
        assert.equal(typeof conn.send, "function");
        assert.equal(typeof conn.close, "function");
        assert.equal(typeof conn.onOpen, "function");
        assert.equal(typeof conn.onMessage, "function");
        assert.equal(typeof conn.onClose, "function");
        assert.equal(typeof conn.onError, "function");
    });

    it("WebSocketFactory shape matches the contract", () => {
        const factory: WebSocketFactory = {
            connect(_url: string): WSConnection {
                return {
                    send() {},
                    close() {},
                    onOpen() {},
                    onMessage() {},
                    onClose() {},
                    onError() {},
                };
            },
        };
        assert.equal(typeof factory.connect, "function");
    });
});

describe("websocket: DenoWebSocketFactory", () => {
    it("connect() creates a WebSocket with the given URL", () => {
        const factory = new DenoWebSocketFactory();
        factory.connect("ws://localhost:3456/ws");
        assert.equal(lastMock?.url, "ws://localhost:3456/ws");
    });

    it("send() forwards data through the WebSocket", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        conn.send("hello");
        conn.send("world");
        assert.deepEqual(lastMock?.sentMessages, ["hello", "world"]);
    });

    it("close() calls WebSocket.close with code and reason", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        conn.close(1000, "done");
        assert.equal(lastMock?.closed, true);
        assert.equal(lastMock?.closeCode, 1000);
        assert.equal(lastMock?.closeReason, "done");
    });

    it("close() is a no-op when the socket is already CLOSING or CLOSED", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        const mock = lastMock!;
        mock.close = () => { throw new Error("should not be called"); };
        mock.readyState = 3; // CLOSED
        assert.doesNotThrow(() => conn.close());
        assert.equal(mock.closed, false);
    });

    it("close() forwards unexpected close errors to onError instead of dropping them", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        const mock = lastMock!;
        const thrown = new Error("write after end");
        mock.close = () => { throw thrown; };
        let received: unknown = null;
        conn.onError((err) => { received = err; });
        assert.doesNotThrow(() => conn.close());
        assert.equal(received, thrown);
    });

    it("onOpen fires when the WebSocket open event dispatches", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        let fired = false;
        conn.onOpen(() => { fired = true; });
        lastMock!.emit("open", {});
        assert.equal(fired, true);
    });

    it("onMessage passes string data to the callback", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        const received: string[] = [];
        conn.onMessage((data) => received.push(data));
        lastMock!.emit("message", { data: "text payload" });
        assert.deepEqual(received, ["text payload"]);
    });

    it("onMessage stringifies non-string, non-binary data", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        const received: string[] = [];
        conn.onMessage((data) => received.push(data));
        lastMock!.emit("message", { data: 42 });
        assert.deepEqual(received, ["42"]);
    });

    it("onMessage forwards binary frames to onError instead of mangling them", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        const received: string[] = [];
        let error: unknown = null;
        conn.onMessage((data) => received.push(data));
        conn.onError((err) => { error = err; });
        lastMock!.emit("message", { data: new ArrayBuffer(4) });
        assert.deepEqual(received, []);
        assert.ok(error instanceof Error);
        assert.match((error as Error).message, /binary/i);
    });

    it("onClose passes code and reason", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        let closeCode = 0;
        let closeReason = "";
        conn.onClose((code, reason) => { closeCode = code; closeReason = reason; });
        lastMock!.emit("close", { code: 1001, reason: "going away" });
        assert.equal(closeCode, 1001);
        assert.equal(closeReason, "going away");
    });

    it("onError forwards the event to the callback", () => {
        const factory = new DenoWebSocketFactory();
        const conn = factory.connect("ws://localhost:3456/ws");
        let received: unknown = null;
        conn.onError((err) => { received = err; });
        const errEvent = { type: "error" };
        lastMock!.emit("error", errEvent);
        assert.equal(received, errEvent);
    });

    it("implements WebSocketFactory interface", () => {
        const factory: WebSocketFactory = new DenoWebSocketFactory();
        assert.equal(typeof factory.connect, "function");
    });
});
