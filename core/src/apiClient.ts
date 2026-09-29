/** Shape of event data pushed via WebSocket. Callers can narrow via generics. */
export interface WsEvent {
    type: string
    [key: string]: unknown
}

/** Error thrown by ApiClient when an RPC call fails. */
export class RpcError extends Error {
    /** Error code (maps to HTTP status semantics: 400, 401, 403, 404, 500). */
    readonly status: number
    /** Raw error message from server. */
    readonly body: string

    constructor(status: number, body: string) {
        super(`RPC error ${status}: ${body}`)
        this.name = "RpcError"
        this.status = status
        this.body = body
    }
}

/** Options accepted by [`ApiClient.call`]. */
export interface CallOptions {
    /**
     * AbortSignal that, when fired, sends a `request.cancel` to the
     * executor and rejects the call with a DOMException (`name === 'AbortError'`).
     *
     * Use this for long-running operations like `querySparql` so a UI
     * teardown or a newer query supersession can stop the in-flight one
     * without paying the network + deserialise tax on its result.
     *
     * Cancellation is best-effort on the executor: the SPARQL evaluation
     * itself isn't preempted (Oxigraph can't be interrupted), but the
     * reply is discarded and the network traffic + JSON parsing on the
     * client are skipped.
     */
    signal?: AbortSignal

    /**
     * Per-call timeout override in ms. Defaults to [[DEFAULT_TIMEOUT_MS]].
     * Use for long-running calls (LLM prompts, holochain ops) that
     * legitimately exceed the default.
     */
    timeoutMs?: number
}

/** Default RPC call timeout in milliseconds (30 seconds). */
const DEFAULT_TIMEOUT_MS = 30_000

/** Default timeout for calls that can run for minutes: LLM work, Holochain, publishing. */
export const LONG_TIMEOUT_MS = 20 * 60 * 1000

/** `options` with {@link LONG_TIMEOUT_MS} unless the caller set a timeout. */
export function longCall(options?: CallOptions): CallOptions {
    return { ...options, timeoutMs: options?.timeoutMs ?? LONG_TIMEOUT_MS }
}

/** Maximum reconnect delay in ms. */
const MAX_RECONNECT_DELAY_MS = 30_000

/** Initial reconnect delay in ms. */
const INITIAL_RECONNECT_DELAY_MS = 500

let _idCounter = 0
function nextId(): string {
    return String(++_idCounter)
}

/**
 * Idempotent reads. One of these that was sent when the socket dropped goes
 * out again, once, on the next socket. Other calls reject with 503: the
 * executor may already have applied them.
 */
const RETRYABLE_READS = new Set([
    'agent.get',
    'agent.status',
    'expression.get',
    'language.get',
    'perspective.all',
    'perspective.get',
    'perspective.queryLinks',
    'perspective.snapshot',
    'runtime.info',
])

interface PendingCall {
    message: string
    /** True once the message went out on the current socket. */
    sent: boolean
    /** True while a retryable read has its one retry left. */
    retry: boolean
    resolve: (value: unknown) => void
    reject: (reason: unknown) => void
}

const closedError = () => new RpcError(503, 'WebSocket connection closed')

export class ApiClient {
    private baseUrl: string
    private token?: string
    private readonly _webSocketImpl?: new (url: string) => WebSocket
    private readonly _fetchImpl?: typeof fetch

    constructor(baseUrl: string, token?: string, webSocketImpl?: new (url: string) => WebSocket, fetchImpl?: typeof fetch) {
        this.baseUrl = baseUrl
        this.token = token
        this._webSocketImpl = webSocketImpl
        this._fetchImpl = fetchImpl
    }

    getBaseUrl(): string { return this.baseUrl }
    getToken(): string | undefined { return this.token }

    /** Perform an HTTP request, using an injected fetch implementation if provided. */
    doFetch(url: string, init: RequestInit): Promise<Response> {
        return this._fetchImpl ? this._fetchImpl(url, init) : fetch(url, init)
    }

    // ── WebSocket ───────────────────────────────────────────────────────────

    /** The connecting or open socket; null otherwise. */
    private _ws: WebSocket | null = null
    /** Settles when `_ws` opens (resolve) or closes first (reject). */
    private _wsOpen: Promise<void> | null = null
    private _wsCallbacks = new Set<(data: unknown) => void>()
    private _reconnectCallbacks = new Set<() => void>()
    private _hasConnectedOnce = false
    private _pendingCalls = new Map<string, PendingCall>()
    private _wsReconnectTimer: ReturnType<typeof setTimeout> | null = null
    private _wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    private _wsPingTimer: ReturnType<typeof setInterval> | null = null

    private _getWsUrl(): string {
        const wsBase = this.baseUrl
            .replace(/^http:\/\//, 'ws://')
            .replace(/^https:\/\//, 'wss://')
        const tokenParam = this.token ? `?token=${encodeURIComponent(this.token)}` : ''
        return `${wsBase}/api/v1/ws${tokenParam}`
    }

    private _ensureWs(): void {
        if (this._ws) return
        const WsImpl = this._webSocketImpl ?? globalThis.WebSocket
        const ws = new WsImpl(this._getWsUrl())
        this._ws = ws
        this._wsOpen = new Promise<void>((resolve, reject) => {
            ws.onopen = () => {
                resolve()
                this._onOpen(ws)
            }
            ws.onclose = () => {
                reject(closedError())
                this._onClose(ws)
            }
        })
        // Only waitForSubscription() awaits this; a close before open is not an error otherwise.
        this._wsOpen.catch(() => {})

        ws.onmessage = (event) => {
            let parsed: Record<string, unknown>
            try { parsed = JSON.parse(event.data) } catch (e) {
                console.error('Error parsing WebSocket data:', e)
                return
            }
            // Events carry a `type`; RPC responses never do.
            if (parsed.type === undefined) {
                const pending = this._pendingCalls.get(parsed.id as string)
                if (!pending) return // late reply to a timed-out or aborted call, or a cancel ack
                const error = parsed.error as { code?: number; message?: string } | undefined
                if (error) pending.reject(new RpcError(error.code ?? 500, error.message ?? 'Unknown error'))
                else pending.resolve(parsed.result)
                return
            }
            if (parsed.type === 'pong') return
            for (const cb of this._wsCallbacks) cb(parsed)
        }

        ws.onerror = (e) => {
            console.error('WebSocket error:', e)
        }
    }

    private _onOpen(ws: WebSocket): void {
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        this._startPing()
        for (const pending of this._pendingCalls.values()) {
            if (!pending.sent) {
                ws.send(pending.message)
                pending.sent = true
            }
        }
        if (this._hasConnectedOnce) {
            for (const cb of this._reconnectCallbacks) {
                try { cb() } catch (e) {
                    console.error('Error in reconnect callback:', e)
                }
            }
        }
        this._hasConnectedOnce = true
    }

    private _onClose(ws: WebSocket): void {
        // A socket closed by _closeWs() can report after a new one exists.
        if (this._ws !== ws) return
        this._stopPing()
        this._ws = null
        this._wsOpen = null
        for (const pending of this._pendingCalls.values()) {
            if (pending.sent && pending.retry) {
                pending.sent = false
                pending.retry = false
            } else {
                pending.reject(closedError())
            }
        }
        if (this._wsCallbacks.size > 0 || this._pendingCalls.size > 0) this._scheduleReconnect()
    }

    private _startPing(): void {
        this._stopPing()
        this._wsPingTimer = setInterval(() => {
            if (this._ws?.readyState === 1 /* OPEN */) {
                this._ws.send(JSON.stringify({ type: 'ping' }))
            }
        }, 30_000)
    }

    private _stopPing(): void {
        if (this._wsPingTimer) {
            clearInterval(this._wsPingTimer)
            this._wsPingTimer = null
        }
    }

    private _scheduleReconnect(): void {
        if (this._wsReconnectTimer) return
        const delay = this._wsReconnectDelay
        this._wsReconnectDelay = Math.min(this._wsReconnectDelay * 2, MAX_RECONNECT_DELAY_MS)
        this._wsReconnectTimer = setTimeout(() => {
            this._wsReconnectTimer = null
            this._ensureWs()
        }, delay)
    }

    // ── RPC call method ─────────────────────────────────────────────────────

    /**
     * Send an RPC call over the WebSocket, connecting first if needed.
     * @param type - The operation type (e.g. 'agent.get', 'perspective.all')
     * @param params - Optional parameters to include in the message
     * @param options - `signal` to cancel, `timeoutMs` to override the
     *   default timeout. The timeout covers connecting and the reply.
     */
    call<T>(type: string, params?: Record<string, unknown>, options?: CallOptions): Promise<T> {
        const signal = options?.signal
        if (signal?.aborted) {
            return Promise.reject(new DOMException('Aborted', 'AbortError'))
        }
        const timeoutMs = options?.timeoutMs ?? DEFAULT_TIMEOUT_MS
        const id = nextId()
        // Params go under "params" so they cannot clash with "id" and "type".
        const message = JSON.stringify({ id, type, params: params || {} })

        return new Promise<T>((resolve, reject) => {
            const settle = (fn: () => void) => {
                if (!this._pendingCalls.delete(id)) return
                clearTimeout(timer)
                signal?.removeEventListener('abort', onAbort)
                fn()
            }
            const timer = setTimeout(
                () => settle(() => reject(new RpcError(408, `RPC call '${type}' timed out after ${timeoutMs}ms`))),
                timeoutMs,
            )
            // Best effort: the executor drops the reply; it cannot always stop the work.
            const onAbort = () => {
                if (pending.sent && this._ws?.readyState === 1 /* OPEN */) {
                    this._ws.send(JSON.stringify({ id: nextId(), type: 'request.cancel', params: { targetId: id } }))
                }
                settle(() => reject(new DOMException('Aborted', 'AbortError')))
            }
            const pending: PendingCall = {
                message,
                sent: false,
                retry: RETRYABLE_READS.has(type),
                resolve: (value) => settle(() => resolve(value as T)),
                reject: (reason) => settle(() => reject(reason)),
            }
            this._pendingCalls.set(id, pending)
            signal?.addEventListener('abort', onAbort, { once: true })

            try {
                this._ensureWs()
            } catch (e) {
                pending.reject(e)
                return
            }
            if (this._ws!.readyState === 1 /* OPEN */) {
                this._ws!.send(message)
                pending.sent = true
            }
        })
    }

    // ── Event subscriptions ─────────────────────────────────────────────────

    subscribe<T = WsEvent>(callback: (data: T) => void): () => void {
        this._wsCallbacks.add(callback as (data: unknown) => void)
        this._ensureWs()

        return () => {
            this._wsCallbacks.delete(callback as (data: unknown) => void)
            if (this._wsCallbacks.size === 0 && this._pendingCalls.size === 0) {
                this._closeWs()
            }
        }
    }

    /** Wait until the WebSocket connection is established. */
    async waitForSubscription(): Promise<void> {
        this._ensureWs()
        if (this._ws!.readyState !== 1 /* OPEN */) await this._wsOpen
    }

    private _closeWs(): void {
        this._stopPing()
        if (this._wsReconnectTimer) {
            clearTimeout(this._wsReconnectTimer)
            this._wsReconnectTimer = null
        }
        const ws = this._ws
        this._ws = null
        this._wsOpen = null
        ws?.close()
    }

    /** Register a callback that fires after a successful WebSocket reconnect.
     *  Does NOT fire on the initial connection — only on reconnects.
     *  Returns an unsubscribe function. */
    onReconnect(callback: () => void): () => void {
        this._reconnectCallbacks.add(callback)
        return () => { this._reconnectCallbacks.delete(callback) }
    }

    /** Close the WebSocket and reject pending calls. */
    closeAll(): void {
        for (const pending of this._pendingCalls.values()) {
            pending.reject(new RpcError(503, 'Client closed'))
        }
        this._closeWs()
        this._wsCallbacks.clear()
        this._reconnectCallbacks.clear()
        // A reused client must not fire onReconnect on its next first open.
        this._hasConnectedOnce = false
    }
}
