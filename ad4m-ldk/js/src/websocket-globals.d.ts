/**
 * Minimal ambient declarations for WebSocket globals available in the
 * executor's Deno language sandbox. The executor runs a full
 * deno_runtime MainWorker (not a bare deno_core isolate), so
 * deno_web/deno_net/deno_websocket are all present at runtime.
 *
 * Only the subset used by DenoWebSocketFactory appears here —
 * adding "DOM" to lib would pull in thousands of unused types.
 */

declare class WebSocket {
    constructor(url: string | URL);
    send(data: string | ArrayBuffer | Blob | ArrayBufferView): void;
    close(code?: number, reason?: string): void;
    addEventListener(type: "open", listener: (event: Event) => void): void;
    addEventListener(type: "message", listener: (event: MessageEvent) => void): void;
    addEventListener(type: "close", listener: (event: CloseEvent) => void): void;
    addEventListener(type: "error", listener: (event: Event) => void): void;
}

declare class Event {
    readonly type: string;
}

declare class MessageEvent extends Event {
    readonly data: unknown;
}

declare class CloseEvent extends Event {
    readonly code: number;
    readonly reason: string;
}

declare class Blob {}
