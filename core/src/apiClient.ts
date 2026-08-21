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
    private _reconnectCallbacks = new Set<() => void>()
    private _hasConnectedOnce = false
    private _pendingCalls = new Map<string, PendingCall>()
    private _wsReady: Promise<void> | null = null
    private _wsReadyResolve: (() => void) | null = null
    private _wsReconnectTimer: ReturnType<typeof setTimeout> | null = null
    private _wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    private _wsClosed = false
    private _wsPingTimer: ReturnType<typeof setInterval> | null = null

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
        const WsImpl = this._webSocketImpl ?? WebSocket
        const ws = new WsImpl(url)
        this._ws = ws

        ws.onopen = () => {
            this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
            if (this._wsReadyResolve) {
                this._wsReadyResolve()
                this._wsReadyResolve = null
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

    // ── RPC call method ─────────────────────────────────────────────────────

    /**
     * Send an RPC call over the WebSocket connection.
     * @param type - The operation type (e.g. 'agent.get', 'perspective.all')
     * @param params - Optional parameters to include in the message
     * @param timeoutMs - Optional per-call timeout override in ms. Defaults to
     *   [[DEFAULT_TIMEOUT_MS]]. Use for long-running calls (LLM prompts,
     *   holochain ops) that legitimately exceed the default.
     * @returns Promise that resolves with the result from the server
     */
    async call<T>(type: string, params?: Record<string, unknown>, timeoutMs?: number): Promise<T> {
        await this._ready()

        const id = nextId()
        // Put params under a "params" key to avoid collision with
        // protocol fields "id" and "type" (e.g. params might contain
        // { id: modelId } or { type: "db" }).
        const message: Record<string, unknown> = { id, type, params: params || {} }
        const effectiveTimeout = timeoutMs ?? DEFAULT_TIMEOUT_MS

        return new Promise<T>((resolve, reject) => {
            const timer = setTimeout(() => {
                this._pendingCalls.delete(id)
                reject(new RpcError(408, `RPC call '${type}' timed out after ${effectiveTimeout}ms`))
            }, effectiveTimeout)

            this._pendingCalls.set(id, {
                resolve: resolve as (value: unknown) => void,
                reject,
                timer,
            })

            if (this._ws && this._ws.readyState === 1 /* OPEN */) {
                this._ws.send(JSON.stringify(message))
            } else {
                this._pendingCalls.delete(id)
                clearTimeout(timer)
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
    }
}
