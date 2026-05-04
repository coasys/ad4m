/** Shape of event data pushed via WebSocket. Callers can narrow via generics. */
export interface WsEvent {
    type: string
    [key: string]: unknown
}

/** Error thrown by RestClient when an RPC call fails. */
export class RestError extends Error {
    /** Error code (maps to HTTP status semantics: 400, 401, 403, 404, 500). */
    readonly status: number
    /** Raw error message from server. */
    readonly body: string

    constructor(status: number, body: string) {
        super(`RPC error ${status}: ${body}`)
        this.name = 'RestError'
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

export class RestClient {
    private baseUrl: string
    private token?: string

    constructor(baseUrl: string, token?: string) {
        this.baseUrl = baseUrl
        this.token = token
    }

    getBaseUrl(): string { return this.baseUrl }
    getToken(): string | undefined { return this.token }

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

    private _getWsUrl(): string {
        const wsBase = this.baseUrl
            .replace(/^http:\/\//, 'ws://')
            .replace(/^https:\/\//, 'wss://')
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const path = '/api/v1/ws'
        return tokenParam ? `${wsBase}${path}?${tokenParam}` : `${wsBase}${path}`
    }

    private _ensureWs(): void {
        if (this._ws && (this._ws.readyState === WebSocket.OPEN || this._ws.readyState === WebSocket.CONNECTING)) {
            return
        }

        this._wsClosed = false

        this._wsReady = new Promise<void>((resolve) => {
            this._wsReadyResolve = resolve
        })

        const url = this._getWsUrl()
        const ws = new WebSocket(url)
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
                    pending.reject(new RestError(err.code ?? 500, err.message ?? 'Unknown error'))
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
            for (const [id, pending] of this._pendingCalls) {
                clearTimeout(pending.timer)
                pending.reject(new RestError(503, 'WebSocket connection closed'))
            }
            this._pendingCalls.clear()

            if (!this._wsClosed && (this._wsCallbacks.size > 0 || this._pendingCalls.size > 0)) {
                this._scheduleReconnect()
            }
        }
    }

    private _startPing(): void {
        this._stopPing()
        this._wsPingTimer = setInterval(() => {
            if (this._ws && this._ws.readyState === WebSocket.OPEN) {
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
     * @returns Promise that resolves with the result from the server
     */
    async call<T>(type: string, params?: Record<string, unknown>): Promise<T> {
        await this._ready()

        const id = nextId()
        // Put params under a "params" key to avoid collision with
        // protocol fields "id" and "type" (e.g. params might contain
        // { id: modelId } or { type: "db" }).
        const message: Record<string, unknown> = { id, type, params: params || {} }

        return new Promise<T>((resolve, reject) => {
            const timer = setTimeout(() => {
                this._pendingCalls.delete(id)
                reject(new RestError(408, `RPC call '${type}' timed out after ${DEFAULT_TIMEOUT_MS}ms`))
            }, DEFAULT_TIMEOUT_MS)

            this._pendingCalls.set(id, {
                resolve: resolve as (value: unknown) => void,
                reject,
                timer,
            })

            if (this._ws && this._ws.readyState === WebSocket.OPEN) {
                this._ws.send(JSON.stringify(message))
            } else {
                this._pendingCalls.delete(id)
                clearTimeout(timer)
                reject(new RestError(503, 'WebSocket not connected'))
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
        }
    }

    /** Close all open WebSocket connections and reject pending calls. */
    closeAll(): void {
        // Reject pending calls
        for (const [id, pending] of this._pendingCalls) {
            clearTimeout(pending.timer)
            pending.reject(new RestError(503, 'Client closed'))
        }
        this._pendingCalls.clear()
        this._closeWs()
        this._wsCallbacks.clear()
    }
}
