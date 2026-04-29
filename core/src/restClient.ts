type EventSourceInitCompat = {
    withCredentials?: boolean
    fetch?: typeof globalThis.fetch
}

// EventSource may come from DOM lib or a polyfill.
// The `eventsource` polyfill supports a custom `fetch` implementation,
// which we use to preserve Node's native web-stream-based fetch even if
// callers later overwrite `global.fetch` with `node-fetch`.
declare var EventSource: {
    new(url: string, init?: EventSourceInitCompat): {
        onmessage: ((event: { data: string }) => void) | null
        onerror: ((event: Event) => void) | null
        close(): void
    }
}

const nativeFetch =
    typeof globalThis.fetch === 'function' ? globalThis.fetch.bind(globalThis) : undefined

/** Shape of SSE event data parsed from JSON. Callers can narrow via generics. */
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
        return res.json() as Promise<T>
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

    // Shared EventSource instances keyed by full URL to avoid exhausting
    // the browser's per-origin HTTP/1.1 connection limit (6 in Chrome).
    private _eventSources = new Map<string, {
        es: InstanceType<typeof EventSource>,
        callbacks: Set<(data: unknown) => void>,
        ready: Promise<void>
    }>()

    subscribe<T = SseEvent>(path: string, callback: (data: T) => void): () => void {
        // Single /events endpoint — all event types multiplexed, server filters per-user.
        const targetPath = '/api/v1/events'
        const url = `${this.baseUrl}${targetPath}`
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const separator = url.includes('?') ? '&' : '?'
        const fullUrl = tokenParam ? `${url}${separator}${tokenParam}` : url

        let entry = this._eventSources.get(fullUrl)
        if (!entry) {
            const es = nativeFetch
                ? new EventSource(fullUrl, { fetch: nativeFetch })
                : new EventSource(fullUrl)
            // Track when the SSE connection is established so callers can
            // await readiness instead of relying on an arbitrary delay.
            const ready = new Promise<void>((resolve) => {
                const orig = es.onmessage
                // Resolve on first successful message (connection is live).
                const onFirstEvent = (event: { data: string }) => {
                    resolve()
                    // Delegate to the real handler for this first event too.
                    if (es.onmessage && es.onmessage !== onFirstEvent) {
                        es.onmessage(event)
                    }
                }
                es.onmessage = onFirstEvent
                // Also resolve after a short timeout as fallback — the server
                // may not send an event immediately, but the connection is open.
                setTimeout(resolve, 200)
            })
            entry = { es, callbacks: new Set(), ready }
            const ref = entry          // stable reference for closure
            // Replace the temporary onmessage with the real dispatcher.
            // If the first-event handler already fired, this just overwrites it.
            // If not, the first-event handler will call through to this one.
            es.onmessage = (event) => {
                let parsed: Record<string, unknown>
                try { parsed = JSON.parse(event.data) } catch (e) {
                    console.error('Error parsing SSE data:', e)
                    return
                }
                for (const cb of ref.callbacks) cb(parsed)
            }
            es.onerror = (e) => { console.error('SSE error:', e) }
            this._eventSources.set(fullUrl, entry)
        }

        entry.callbacks.add(callback as (data: unknown) => void)

        return () => {
            const e = this._eventSources.get(fullUrl)
            if (!e) return
            e.callbacks.delete(callback as (data: unknown) => void)
            if (e.callbacks.size === 0) {
                e.es.close()
                this._eventSources.delete(fullUrl)
            }
        }
    }

    /** Wait until the SSE connection for the given path is established. */
    async waitForSubscription(path: string): Promise<void> {
        const targetPath = '/api/v1/events'
        const url = `${this.baseUrl}${targetPath}`
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const separator = url.includes('?') ? '&' : '?'
        const fullUrl = tokenParam ? `${url}${separator}${tokenParam}` : url
        const entry = this._eventSources.get(fullUrl)
        if (entry) await entry.ready
    }

    /** Close all open EventSource connections */
    closeAll(): void {
        for (const [url, entry] of this._eventSources) {
            entry.es.close()
            entry.callbacks.clear()
        }
        this._eventSources.clear()
    }
}
