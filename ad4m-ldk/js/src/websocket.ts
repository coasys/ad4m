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

/**
 * Wraps the native WebSocket global available in the executor's Deno
 * language sandbox. The executor runs a full deno_runtime MainWorker
 * (not a bare deno_core isolate), so deno_web/deno_net/deno_websocket
 * are all present.
 */
export class DenoWebSocketFactory implements WebSocketFactory {
    connect(url: string): WSConnection {
        const ws = new WebSocket(url);

        return {
            send(data: string): void {
                ws.send(data);
            },
            close(code?: number, reason?: string): void {
                try {
                    ws.close(code, reason);
                } catch {
                    // Already closed/closing
                }
            },
            onOpen(cb: () => void): void {
                ws.addEventListener("open", () => cb());
            },
            onMessage(cb: (data: string) => void): void {
                ws.addEventListener("message", (event: MessageEvent) => {
                    cb(typeof event.data === "string" ? event.data : String(event.data));
                });
            },
            onClose(cb: (code: number, reason: string) => void): void {
                ws.addEventListener("close", (event: CloseEvent) => cb(event.code, event.reason));
            },
            onError(cb: (err: unknown) => void): void {
                ws.addEventListener("error", (event: Event) => cb(event));
            },
        };
    }
}
