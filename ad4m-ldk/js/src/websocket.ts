/**
 * WebSocket adapter interfaces and Deno implementation.
 *
 * Provides a platform-agnostic WebSocket abstraction so link languages
 * can open WebSocket connections without directly depending on the Deno
 * or browser WebSocket global. The DenoWebSocketFactory wraps the native
 * WebSocket global available in the executor's language sandbox.
 */

/** A single open (or opening) WebSocket-like connection. */
export interface WSConnection {
    send(data: string): void;
    close(code?: number, reason?: string): void;
    onOpen(cb: () => void): void;
    onMessage(cb: (data: string) => void): void;
    onClose(cb: (code: number, reason: string) => void): void;
    onError(cb: (err: unknown) => void): void;
}

/** Factory for creating WebSocket connections. */
export interface WebSocketFactory {
    connect(url: string): WSConnection;
}

const READY_STATE_CLOSING = 2;
const READY_STATE_CLOSED = 3;

function isBinaryPayload(data: unknown): boolean {
    return (
        (typeof ArrayBuffer !== "undefined" && data instanceof ArrayBuffer) ||
        (typeof Blob !== "undefined" && data instanceof Blob) ||
        ArrayBuffer.isView(data)
    );
}

/**
 * Wraps the native WebSocket global available in the executor's Deno
 * language sandbox. The executor runs a full deno_runtime MainWorker
 * (not a bare deno_core isolate), so deno_web/deno_net/deno_websocket
 * are all present.
 */
class DenoWSConnection implements WSConnection {
    private readonly ws: WebSocket;
    private errorCallback?: (err: unknown) => void;

    constructor(url: string) {
        this.ws = new WebSocket(url);
    }

    send(data: string): void {
        this.ws.send(data);
    }

    close(code?: number, reason?: string): void {
        if (this.ws.readyState === READY_STATE_CLOSING || this.ws.readyState === READY_STATE_CLOSED) {
            return;
        }
        try {
            this.ws.close(code, reason);
        } catch (err) {
            // Forward unexpected close failures instead of dropping them silently.
            this.errorCallback?.(err);
        }
    }

    onOpen(cb: () => void): void {
        this.ws.addEventListener("open", () => cb());
    }

    onMessage(cb: (data: string) => void): void {
        this.ws.addEventListener("message", (event: MessageEvent) => {
            const data = event.data;
            if (typeof data === "string") {
                cb(data);
                return;
            }
            if (isBinaryPayload(data)) {
                // No binary path in this interface (link languages are text/JSON only) —
                // report it instead of silently mangling the payload via String(data).
                this.errorCallback?.(new Error("DenoWebSocketFactory: received a binary WebSocket frame, which is not supported"));
                return;
            }
            cb(String(data));
        });
    }

    onClose(cb: (code: number, reason: string) => void): void {
        this.ws.addEventListener("close", (event: CloseEvent) => cb(event.code, event.reason));
    }

    onError(cb: (err: unknown) => void): void {
        this.errorCallback = cb;
        this.ws.addEventListener("error", (event: Event) => cb(event));
    }
}

export class DenoWebSocketFactory implements WebSocketFactory {
    connect(url: string): WSConnection {
        return new DenoWSConnection(url);
    }
}
