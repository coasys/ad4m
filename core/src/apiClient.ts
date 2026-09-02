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

/** Maximum reconnect delay in ms. */
const MAX_RECONNECT_DELAY_MS = 30_000

/** Initial reconnect delay in ms. */
const INITIAL_RECONNECT_DELAY_MS = 500

/** Counter for generating unique request IDs. */
let _idCounter = 0
function nextId(): string {
    return String(++_idCounter)
}

interface PendingCall {
    resolve: (value: unknown) => void
    reject: (reason: unknown) => void
    timer: ReturnType<typeof setTimeout>
}

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

    setToken(token: string) {
        this.token = token
    }

    // ── WebSocket RPC core ──────────────────────────────────────────────────

    private _ws: WebSocket | null = null
    private _wsCallbacks = new Set<(data: unknown) => void>()
    private _pendingCalls = new Map<string, PendingCall>()
    private _wsReady: Promise<void> | null = null
    private _wsReadyResolve: (() => void) | null = null
    private _wsReconnectTimer: ReturnType<typeof setTimeout> | null = null
    private _wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    private _wsClosed = false
    private _wsPingTimer: ReturnType<typeof setInterval> | null = null
    private _ignoredResponseIds = new Set<string>()

    private _getWsUrl(): string {
        const wsBase = this.baseUrl
            .replace(/^http:\/\//, 'ws://')
            .replace(/^https:\/\//, 'wss://')
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const path = '/api/v1/ws'
        return tokenParam ? `${wsBase}${path}?${tokenParam}` : `${wsBase}${path}`
    }

    private _ensureWs(): void {
        if (this._ws && (this._ws.readyState === 1 /* OPEN */ || this._ws.readyState === 0 /* CONNECTING */)) {
            return
        }
        // Prevent duplicate connections from concurrent callers
        if (this._wsReady) return

        this._wsClosed = false

        this._wsReady = new Promise<void>((resolve) => {
            this._wsReadyResolve = resolve
        })

        const url = this._getWsUrl()
        // Fallback order: injected impl → globalThis.WebSocket (browsers, Node ≥ 22) → require('ws') (Node ≤ 20).
        // globalThis lookup avoids a ReferenceError on Node 18 where `WebSocket` is not a global.
        const globalWs = (globalThis as any).WebSocket as (new (url: string) => WebSocket) | undefined
        let WsImpl: new (url: string) => WebSocket
        if (this._webSocketImpl) {
            WsImpl = this._webSocketImpl
        } else if (globalWs) {
            WsImpl = globalWs
        } else {
            // Lazy CommonJS require so bundlers targeting browsers don't try to bundle `ws`.
            // eslint-disable-next-line @typescript-eslint/no-var-requires
            const req = eval('require') as NodeRequire
            WsImpl = req('ws') as new (url: string) => WebSocket
        }
        const ws = new WsImpl(url)
        this._ws = ws

        ws.onopen = () => {
            this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
            if (this._wsReadyResolve) {
                this._wsReadyResolve()
                this._wsReadyResolve = null
            }
            this._startPing()
        }

        ws.onmessage = (event) => {
            let parsed: Record<string, unknown>
            try { parsed = JSON.parse(event.data) } catch (e) {
                console.error('Error parsing WebSocket data:', e)
                return
            }

            // Ignore pong messages
            if (parsed.type === 'pong') return

            // Check if this is a response to a pending RPC call (has an id field)
            const id = parsed.id as string | undefined
            if (id && this._pendingCalls.has(id)) {
                const pending = this._pendingCalls.get(id)!
                this._pendingCalls.delete(id)
                clearTimeout(pending.timer)

                if (parsed.error) {
                    const err = parsed.error as { code?: number; message?: string }
                    pending.reject(new RpcError(err.code ?? 500, err.message ?? 'Unknown error'))
                } else {
                    pending.resolve(parsed.result)
                }
                return
            }

            // Discard ack responses to cancel requests we sent — they have
            // an id that was never in _pendingCalls, so without this guard
            // they'd leak into subscriber callbacks as spurious events.
            if (id && this._ignoredResponseIds.has(id)) {
                this._ignoredResponseIds.delete(id)
                return
            }

            // Server-push event (no id, or id not in pending) → route to subscribers
            for (const cb of this._wsCallbacks) cb(parsed)
        }

        ws.onerror = (e) => {
            console.error('WebSocket error:', e)
        }

        ws.onclose = () => {
            this._stopPing()
            this._ws = null
            // Reset the readiness promise so future calls reconnect
            this._wsReady = null

            // Reject all pending calls
            const hadPendingCalls = this._pendingCalls.size > 0
            for (const [id, pending] of this._pendingCalls) {
                clearTimeout(pending.timer)
                pending.reject(new RpcError(503, 'WebSocket connection closed'))
            }
            this._pendingCalls.clear()

            if (!this._wsClosed && (this._wsCallbacks.size > 0 || hadPendingCalls)) {
                this._scheduleReconnect()
            }
        }
    }

    private _startPing(): void {
        this._stopPing()
        this._wsPingTimer = setInterval(() => {
            if (this._ws && this._ws.readyState === 1 /* OPEN */) {
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
            if (!this._wsClosed) {
                this._ensureWs()
            }
        }, delay)
    }

    /** Ensure WS is connected and ready. Lazy — connects on first use. */
    private async _ready(): Promise<void> {
        this._ensureWs()
        if (this._wsReady) await this._wsReady
    }

    /**
     * Like `_ready()`, but races the connection against an AbortSignal so
     * that callers don't hang if the signal fires while connecting.
     */
    private async _readyOrAbort(signal?: AbortSignal): Promise<void> {
        if (signal?.aborted) {
            throw new DOMException('Aborted', 'AbortError')
        }

        await new Promise<void>((resolve, reject) => {
            let settled = false

            const finish = (fn: () => void) => {
                if (settled) return
                settled = true
                if (signal && onAbort) {
                    signal.removeEventListener('abort', onAbort)
                }
                fn()
            }

            const onAbort = signal
                ? () => finish(() => reject(new DOMException('Aborted', 'AbortError')))
                : null

            if (onAbort) {
                signal!.addEventListener('abort', onAbort, { once: true })
            }

            this._ready().then(
                () => finish(resolve),
                (error) => finish(() => reject(error)),
            )
        })
    }

    // ── RPC call method ─────────────────────────────────────────────────────

    /**
     * Send an RPC call over the WebSocket connection.
     * @param type - The operation type (e.g. 'agent.get', 'perspective.all')
     * @param params - Optional parameters to include in the message
     * @param options - Optional call options: `signal` (AbortSignal for
     *   cancellation) and/or `timeoutMs` (per-call timeout override,
     *   defaults to [[DEFAULT_TIMEOUT_MS]] — use for long-running calls
     *   like LLM prompts or holochain ops that legitimately exceed it).
     * @returns Promise that resolves with the result from the server
     */
    async call<T>(type: string, params?: Record<string, unknown>, options?: CallOptions): Promise<T> {
        // Fast-path: if the caller already aborted, bail out before
        // opening the socket. Matches fetch()'s behaviour.
        const signal = options?.signal
        if (signal?.aborted) {
            throw new DOMException('Aborted', 'AbortError')
        }

        await this._readyOrAbort(signal)

        const id = nextId()
        // Put params under a "params" key to avoid collision with
        // protocol fields "id" and "type" (e.g. params might contain
        // { id: modelId } or { type: "db" }).
        const message: Record<string, unknown> = { id, type, params: params || {} }
        const effectiveTimeout = options?.timeoutMs ?? DEFAULT_TIMEOUT_MS

        return new Promise<T>((resolve, reject) => {
            let abortHandler: (() => void) | null = null

            const cleanup = () => {
                if (signal && abortHandler) {
                    signal.removeEventListener('abort', abortHandler)
                    abortHandler = null
                }
            }

            const timer = setTimeout(() => {
                this._pendingCalls.delete(id)
                cleanup()
                reject(new RpcError(408, `RPC call '${type}' timed out after ${effectiveTimeout}ms`))
            }, effectiveTimeout)

            // Wrap resolve/reject so we always clean up the abort listener.
            this._pendingCalls.set(id, {
                resolve: (value: unknown) => { cleanup(); resolve(value as T) },
                reject: (reason: unknown) => { cleanup(); reject(reason) },
                timer,
            })

            if (signal) {
                abortHandler = () => {
                    // Drop the pending entry first so the late server
                    // reply (if any) routes to the event subscribers,
                    // not to a stale resolver.
                    this._pendingCalls.delete(id)
                    clearTimeout(timer)
                    cleanup()

                    // Best-effort `request.cancel` — the executor will
                    // race the cancellation token against the in-flight
                    // handler. If the socket is already gone there's
                    // nothing useful we can send; the client side is
                    // still aborted either way.
                    if (this._ws && this._ws.readyState === 1 /* OPEN */) {
                        try {
                            const cancelId = nextId()
                            this._ignoredResponseIds.add(cancelId)
                            // Safety-net eviction: if the executor's ack never
                            // arrives (e.g. the socket drops right after we
                            // send request.cancel), the normal cleanup path
                            // (deleting the id when its response lands) never
                            // fires and the entry would live in this set for
                            // the lifetime of the client. Bound it instead —
                            // DEFAULT_TIMEOUT_MS is already the ceiling every
                            // other in-flight call uses, so an ack that hasn't
                            // shown up by then isn't coming.
                            setTimeout(() => {
                                this._ignoredResponseIds.delete(cancelId)
                            }, DEFAULT_TIMEOUT_MS)
                            this._ws.send(JSON.stringify({
                                id: cancelId,
                                type: 'request.cancel',
                                params: { targetId: id },
                            }))
                        } catch (e) {
                            // Swallow — the user-visible failure mode is the
                            // AbortError below; a socket send failure here
                            // doesn't change client semantics.
                        }
                    }

                    reject(new DOMException('Aborted', 'AbortError'))
                }
                signal.addEventListener('abort', abortHandler, { once: true })
            }

            if (this._ws && this._ws.readyState === 1 /* OPEN */) {
                this._ws.send(JSON.stringify(message))
            } else {
                this._pendingCalls.delete(id)
                clearTimeout(timer)
                cleanup()
                reject(new RpcError(503, 'WebSocket not connected'))
            }
        })
    }

    // ── Event subscriptions (same interface as before) ──────────────────────

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
        await this._ready()
    }

    // ── Explicit connection ─────────────────────────────────────────────────

    /** Explicitly connect the WebSocket (optional — connection is lazy). */
    connect(): void {
        this._ensureWs()
    }

    private _closeWs(): void {
        this._stopPing()
        if (this._wsReconnectTimer) {
            clearTimeout(this._wsReconnectTimer)
            this._wsReconnectTimer = null
        }
        if (this._ws) {
            this._wsClosed = true
            this._ws.close()
            this._ws = null
            this._wsReady = null
        }
    }

    /** Close all open WebSocket connections and reject pending calls. */
    closeAll(): void {
        // Reject pending calls
        for (const [id, pending] of this._pendingCalls) {
            clearTimeout(pending.timer)
            pending.reject(new RpcError(503, 'Client closed'))
        }
        this._pendingCalls.clear()
        this._closeWs()
        this._wsCallbacks.clear()
    }
}
