/** Shape of event data pushed via WebSocket. Callers can narrow via generics. */
export interface WsEvent {
    type: string
    [key: string]: unknown
}

/** Error thrown by ApiClient when an RPC call fails. */
export class RpcError extends Error {
    /**
     * Error code (maps to HTTP status semantics: 400, 401, 403, 404, 500).
     * Client-side failures: 408 call timeout, 503 connection lost or client
     * closed, CONNECT_FAILED_STATUS (504) no connection within the budget.
     */
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

/**
 * Maximum reconnect delay in ms. Equal to CONNECT_TIMEOUT_MS by coincidence,
 * not tuning: no retry wait inside one connect budget reaches it (the 7th
 * attempt would fall past the budget). It only sets the pause before a
 * subscriber-only client starts its next cycle.
 */
const MAX_RECONNECT_DELAY_MS = 30_000

/** Initial reconnect delay in ms. */
const INITIAL_RECONNECT_DELAY_MS = 500

/**
 * How long a caller waits for a WebSocket connection before `call()` rejects.
 * Within this budget, failed connect attempts are retried with backoff, so a
 * client created a moment before the executor binds its port still connects.
 * After it, the waiting calls reject with CONNECT_FAILED_STATUS instead of
 * hanging.
 */
const CONNECT_TIMEOUT_MS = 30_000

/**
 * `RpcError.status` when no WebSocket connection opened within the connect
 * budget. Distinct from 503, which the client uses for a connection that was
 * lost or closed, so a caller can tell "the executor is not there" apart by
 * status alone. 504 because the client gave up waiting (a timeout); a 5xx, so
 * `status >= 500` retry checks still treat it as a server-side failure.
 */
export const CONNECT_FAILED_STATUS = 504

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
    private _reconnectCallbacks = new Set<() => void>()
    private _hasConnectedOnce = false
    private _pendingCalls = new Map<string, PendingCall>()
    // A connection cycle starts when a caller needs the socket and ends when
    // a socket opens (resolve) or the connect budget runs out (reject).
    // Failed connect attempts inside a cycle are retried and keep the same
    // promise, so every caller awaiting it is settled exactly once. The
    // resolve/reject pair is non-null only while the cycle is pending.
    private _wsReady: Promise<void> | null = null
    private _wsReadyResolve: (() => void) | null = null
    private _wsReadyReject: ((reason: unknown) => void) | null = null
    private _wsConnectTimer: ReturnType<typeof setTimeout> | null = null
    // Callers awaiting _wsReady. They are not in _pendingCalls yet, so
    // unsubscribe() must count them before it closes the socket under them.
    private _readyWaiters = 0
    private _wsReconnectTimer: ReturnType<typeof setTimeout> | null = null
    private _wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    private _wsClosed = false
    private _wsPingTimer: ReturnType<typeof setInterval> | null = null
    private _ignoredResponseIds = new Set<string>()

    /** The WS endpoint without the token, safe to put in error messages. */
    private _getWsEndpoint(): string {
        const wsBase = this.baseUrl
            .replace(/^http:\/\//, 'ws://')
            .replace(/^https:\/\//, 'wss://')
        return `${wsBase}/api/v1/ws`
    }

    private _getWsUrl(): string {
        const endpoint = this._getWsEndpoint()
        return this.token ? `${endpoint}?token=${encodeURIComponent(this.token)}` : endpoint
    }

    private _ensureWs(): void {
        if (this._ws && (this._ws.readyState === 1 /* OPEN */ || this._ws.readyState === 0 /* CONNECTING */)) {
            return
        }
        if (this._wsReady) {
            // Connected, and the old socket's onclose hasn't run yet: it
            // resets _wsReady, and the next caller starts a new cycle.
            if (!this._wsReadyReject) return
            // Pending cycle between attempts: the scheduled retry opens the
            // next socket. Opening one now would skip the backoff.
            if (this._wsReconnectTimer) return
            this._openSocket()
            return
        }

        this._wsClosed = false
        // Every cycle restarts the backoff. A call made after a failed cycle
        // then retries within its budget: at the cap, its first retry would
        // land at the budget's end.
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        this._wsReady = new Promise<void>((resolve, reject) => {
            this._wsReadyResolve = resolve
            this._wsReadyReject = reject
        })
        // Callers await _wsReady and see the rejection. This handler only
        // keeps a cycle nobody awaits (subscribe() or connect() alone) from
        // raising an unhandledRejection.
        this._wsReady.catch(() => {})
        this._wsConnectTimer = setTimeout(() => this._failConnect(), CONNECT_TIMEOUT_MS)
        this._openSocket()
    }

    /** End a pending connection cycle whose budget ran out. */
    private _failConnect(): void {
        this._wsConnectTimer = null
        const reject = this._wsReadyReject
        if (!reject) return
        this._wsReadyResolve = null
        this._wsReadyReject = null
        this._wsReady = null
        if (this._wsReconnectTimer) {
            clearTimeout(this._wsReconnectTimer)
            this._wsReconnectTimer = null
        }
        // A socket still CONNECTING here is stuck (e.g. a dropped SYN).
        // Detach it first, so its onclose is ignored as stale.
        const stuck = this._ws
        this._ws = null
        stuck?.close()

        reject(new RpcError(CONNECT_FAILED_STATUS, `WebSocket could not connect to ${this._getWsEndpoint()} within ${CONNECT_TIMEOUT_MS}ms`))

        // Subscribers have no call to fail. Keep retrying for them, as
        // onclose does after an established connection drops.
        if (!this._wsClosed && this._wsCallbacks.size > 0) {
            this._scheduleReconnect()
        }
    }

    private _openSocket(): void {
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
            if (ws !== this._ws) return // detached by _failConnect/_closeWs
            this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
            if (this._wsConnectTimer) {
                clearTimeout(this._wsConnectTimer)
                this._wsConnectTimer = null
            }
            if (this._wsReadyResolve) {
                this._wsReadyResolve()
                this._wsReadyResolve = null
                this._wsReadyReject = null
            }
            this._startPing()

            // Fire reconnect callbacks only on reconnect (not first connect)
            if (this._hasConnectedOnce) {
                for (const cb of this._reconnectCallbacks) {
                    try { cb() } catch (e) {
                        console.error('Error in reconnect callback:', e)
                    }
                }
            }
            this._hasConnectedOnce = true
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
            // A socket we already detached (closeAll, a failed cycle) may
            // report its close late, after a newer socket took its place.
            // Its close must not null the new socket or reject the new
            // socket's calls.
            if (ws !== this._ws) return
            this._stopPing()
            this._ws = null

            // Reject all pending calls
            const hadPendingCalls = this._pendingCalls.size > 0
            for (const [id, pending] of this._pendingCalls) {
                clearTimeout(pending.timer)
                pending.reject(new RpcError(503, 'WebSocket connection closed'))
            }
            this._pendingCalls.clear()

            if (this._wsReadyReject) {
                // The socket closed without opening: a failed connect attempt,
                // e.g. ECONNREFUSED because the server is not listening yet.
                // Keep the cycle and its waiters, and retry. _failConnect ends
                // the cycle when the connect budget runs out.
                if (!this._wsClosed) this._scheduleReconnect()
                return
            }

            // Reset the readiness promise so future calls reconnect
            this._wsReady = null
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

    /**
     * Ensure WS is connected and ready. Lazy — connects on first use.
     * Rejects with CONNECT_FAILED_STATUS if no connection opens within
     * CONNECT_TIMEOUT_MS, or with a 503 if the client is closed while waiting.
     */
    private async _ready(): Promise<void> {
        this._ensureWs()
        const ready = this._wsReady
        if (!ready) return
        this._readyWaiters++
        try {
            await ready
        } finally {
            this._readyWaiters--
        }
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
            if (this._wsCallbacks.size === 0 && this._pendingCalls.size === 0 && this._readyWaiters === 0) {
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
        this._wsClosed = true
        if (this._wsReconnectTimer) {
            clearTimeout(this._wsReconnectTimer)
            this._wsReconnectTimer = null
        }
        if (this._wsConnectTimer) {
            clearTimeout(this._wsConnectTimer)
            this._wsConnectTimer = null
        }
        // Settle callers still waiting for the connection, which otherwise
        // wait on a promise nothing will ever resolve.
        const reject = this._wsReadyReject
        this._wsReadyResolve = null
        this._wsReadyReject = null
        this._wsReady = null
        reject?.(new RpcError(503, 'Client closed'))
        // Detach before close(): the socket's own onclose then sees itself
        // as stale and leaves state alone (see onclose).
        const ws = this._ws
        this._ws = null
        ws?.close()
    }

    /** Register a callback that fires after a successful WebSocket reconnect.
     *  Does NOT fire on the initial connection — only on reconnects.
     *  Returns an unsubscribe function. */
    onReconnect(callback: () => void): () => void {
        this._reconnectCallbacks.add(callback)
        return () => { this._reconnectCallbacks.delete(callback) }
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
        this._reconnectCallbacks.clear()
        // Reset the first-connect gate so a reused client (closeAll() →
        // later call()/subscribe() reopening via _ensureWs) does not fire
        // freshly registered onReconnect callbacks on the initial open of
        // the new connection — see the contract on onReconnect().
        this._hasConnectedOnce = false
    }
}
