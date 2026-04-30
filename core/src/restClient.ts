/** Shape of event data parsed from JSON. Callers can narrow via generics. */
export interface SseEvent {
    type: string
    [key: string]: unknown
}

/** Error thrown by RestClient when an HTTP request fails. */
export class RestError extends Error {
    /** HTTP status code (e.g. 401, 500). */
    readonly status: number
    /** Raw response body text. */
    readonly body: string

    constructor(status: number, body: string) {
        super(`HTTP ${status}: ${body}`)
        this.name = 'RestError'
        this.status = status
        this.body = body
    }
}

/** Default request timeout in milliseconds (30 seconds). */
const DEFAULT_TIMEOUT_MS = 30_000

/** HTTP status codes that are safe to retry (transient server errors). */
const RETRYABLE_STATUS = new Set([502, 503, 504])

/** Maximum number of automatic retries for transient failures. */
const MAX_RETRIES = 2

/** Base delay in ms between retries (doubles each attempt). */
const RETRY_BASE_DELAY_MS = 500

/** Maximum reconnect delay in ms. */
const MAX_RECONNECT_DELAY_MS = 30_000

/** Initial reconnect delay in ms. */
const INITIAL_RECONNECT_DELAY_MS = 500

export class RestClient {
    constructor(private baseUrl: string, private token?: string) {}

    getBaseUrl(): string { return this.baseUrl; }
    getToken(): string | undefined { return this.token; }

    setToken(token: string) {
        this.token = token
    }

    private headers(): Record<string, string> {
        const h: Record<string, string> = { 'Content-Type': 'application/json' }
        if (this.token) h['Authorization'] = `Bearer ${this.token}`
        return h
    }

    private async fetchWithRetry(url: string, init: RequestInit): Promise<Response> {
        let lastError: unknown
        for (let attempt = 0; attempt <= MAX_RETRIES; attempt++) {
            const controller = new AbortController()
            const timeoutId = setTimeout(() => controller.abort(), DEFAULT_TIMEOUT_MS)
            try {
                const res = await fetch(url, { ...init, signal: controller.signal })
                clearTimeout(timeoutId)
                if (res.ok || !RETRYABLE_STATUS.has(res.status) || attempt === MAX_RETRIES) {
                    return res
                }
                lastError = new RestError(res.status, await res.text())
            } catch (err: unknown) {
                clearTimeout(timeoutId)
                // Don't retry non-transient fetch errors (e.g. AbortError from timeout)
                if (attempt === MAX_RETRIES) throw err
                lastError = err
            }
            await new Promise(r => setTimeout(r, RETRY_BASE_DELAY_MS * Math.pow(2, attempt)))
        }
        throw lastError
    }

    private async request<T>(method: string, path: string, body?: unknown): Promise<T> {
        const res = await this.fetchWithRetry(`${this.baseUrl}${path}`, {
            method,
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined,
        })
        if (!res.ok) throw new RestError(res.status, await res.text())
        const text = await res.text()
        if (!text) return undefined as T
        return JSON.parse(text) as T
    }

    async get<T>(path: string): Promise<T> {
        return this.request<T>('GET', path)
    }

    async post<T>(path: string, body?: unknown): Promise<T> {
        return this.request<T>('POST', path, body)
    }

    async put<T>(path: string, body?: unknown): Promise<T> {
        return this.request<T>('PUT', path, body)
    }

    async patch<T>(path: string, body?: unknown): Promise<T> {
        return this.request<T>('PATCH', path, body)
    }

    async delete<T>(path: string, body?: unknown): Promise<T> {
        return this.request<T>('DELETE', path, body)
    }

    // ── WebSocket-based event subscription ──────────────────────────────────

    private _ws: WebSocket | null = null
    private _wsCallbacks = new Set<(data: unknown) => void>()
    private _wsReady: Promise<void> | null = null
    private _wsReadyResolve: (() => void) | null = null
    private _wsReconnectTimer: ReturnType<typeof setTimeout> | null = null
    private _wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    private _wsClosed = false   // true when closeAll() is called explicitly
    private _wsPingTimer: ReturnType<typeof setInterval> | null = null

    private _getWsUrl(): string {
        // Convert http(s):// to ws(s)://
        const wsBase = this.baseUrl
            .replace(/^http:\/\//, 'ws://')
            .replace(/^https:\/\//, 'wss://')
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const path = '/api/v1/ws/events'
        return tokenParam ? `${wsBase}${path}?${tokenParam}` : `${wsBase}${path}`
    }

    private _ensureWs(): void {
        if (this._ws && (this._ws.readyState === WebSocket.OPEN || this._ws.readyState === WebSocket.CONNECTING)) {
            return
        }

        this._wsClosed = false

        // Create readiness promise
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
            // Start application-level ping to detect stale connections
            this._startPing()
        }

        ws.onmessage = (event) => {
            let parsed: Record<string, unknown>
            try { parsed = JSON.parse(event.data) } catch (e) {
                console.error('Error parsing WebSocket data:', e)
                return
            }
            // Ignore pong messages from the server
            if (parsed.type === 'pong') return
            for (const cb of this._wsCallbacks) cb(parsed)
        }

        ws.onerror = (e) => {
            console.error('WebSocket error:', e)
        }

        ws.onclose = () => {
            this._stopPing()
            this._ws = null
            if (!this._wsClosed && this._wsCallbacks.size > 0) {
                // Auto-reconnect with exponential backoff
                this._scheduleReconnect()
            }
        }
    }

    private _startPing(): void {
        this._stopPing()
        // Send application-level ping every 30s to detect dead connections
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
            if (!this._wsClosed && this._wsCallbacks.size > 0) {
                this._ensureWs()
            }
        }, delay)
    }

    subscribe<T = SseEvent>(path: string, callback: (data: T) => void): () => void {
        this._wsCallbacks.add(callback as (data: unknown) => void)
        this._ensureWs()

        return () => {
            this._wsCallbacks.delete(callback as (data: unknown) => void)
            if (this._wsCallbacks.size === 0) {
                this._closeWs()
            }
        }
    }

    /** Wait until the WebSocket connection is established. */
    async waitForSubscription(path: string): Promise<void> {
        if (this._wsReady) await this._wsReady
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

    /** Close all open WebSocket connections */
    closeAll(): void {
        this._closeWs()
        this._wsCallbacks.clear()
    }
}
