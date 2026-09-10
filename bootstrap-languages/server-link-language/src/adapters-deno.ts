/**
 * Deno-specific adapter implementations.
 * Wraps ad4m:host functions from @coasys/ad4m-ldk, plus the native
 * `WebSocket` global (confirmed available in the executor's language
 * sandbox — it runs a full deno_runtime MainWorker, not a bare deno_core
 * isolate, so deno_web/deno_net/deno_websocket are present).
 *
 * Only imported by index.ts — never by core modules or tests.
 */

import {
    httpFetch,
    storageGet,
    storagePut,
    storageDelete,
    storageListKeys,
    agentDid,
    agentSignStringHex,
    emitPerspectiveDiff,
    emitSyncStateChange,
    emitTelepresenceSignal,
} from "@coasys/ad4m-ldk";

import type {
    AgentAdapter,
    RuntimeAdapter,
    StorageAdapter,
    Transport,
    TransportResponse,
    WSConnection,
    WebSocketFactory,
} from "./adapters.js";

export class DenoTransport implements Transport {
    async fetch(
        url: string,
        method: string,
        headers: Record<string, string>,
        body: string,
    ): Promise<TransportResponse> {
        try {
            const res = await httpFetch(
                url,
                method,
                JSON.stringify(headers),
                body,
            );
            // Executor versions before the 2b65ebbf0 fix return a plain
            // string (response body) on success and throw for non-ok HTTP.
            // Current dev returns { status, body }. Handle both.
            if (typeof res === "string") {
                return { status: 200, headers: {}, body: res };
            }
            return {
                status: res.status,
                headers: {},
                body: res.body || "",
            };
        } catch (err: unknown) {
            // Old executors throw for non-ok HTTP status with the format:
            //   "http_fetch METHOD URL -> STATUS: body"
            // Current dev returns { status, body } for all responses, so
            // only genuine network errors (DNS, connection refused) land here.
            const errMsg = err instanceof Error ? err.message : String(err);
            const statusMatch = errMsg.match(/-> (\d+):/);
            const status = statusMatch ? parseInt(statusMatch[1], 10) : 0;
            const bodyIdx = statusMatch ? errMsg.indexOf(": ", errMsg.indexOf("-> ")) + 2 : -1;
            const responseBody = bodyIdx > 1 ? errMsg.substring(bodyIdx) : errMsg;
            if (status === 0) {
                console.error(`[transport] httpFetch network error: ${errMsg}`);
            }
            return { status, headers: {}, body: responseBody };
        }
    }
}

export class DenoStorageAdapter implements StorageAdapter {
    get(key: string): string | null {
        return storageGet(key);
    }

    put(key: string, value: string): void {
        storagePut(key, value);
    }

    delete(key: string): void {
        storageDelete(key);
    }

    listKeys(prefix?: string): string[] {
        return storageListKeys(prefix);
    }
}

export class DenoAgentAdapter implements AgentAdapter {
    did(): string {
        return agentDid();
    }

    signStringHex(payload: string): string {
        return agentSignStringHex(payload);
    }
}

export class DenoRuntimeAdapter implements RuntimeAdapter {
    emitPerspectiveDiff(diff: unknown): void {
        emitPerspectiveDiff(diff);
    }

    emitSyncStateChange(state: string): void {
        emitSyncStateChange(state);
    }

    emitTelepresenceSignal(payload: unknown, recipientDid?: string): void {
        emitTelepresenceSignal(payload, recipientDid);
    }
}

/**
 * Wraps the native `WebSocket` global. This is what replaces socket.io in
 * this language: a plain, standards-compliant WebSocket client talking to
 * link-server's `/rooms/:roomId/ws` endpoint directly, no
 * client library, no polling-transport fallback.
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
                    // Already closed/closing — nothing to do.
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
