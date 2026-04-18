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

    async get<T>(path: string): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, { headers: this.headers() })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    async post<T>(path: string, body?: unknown): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, {
            method: 'POST',
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined
        })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    async put<T>(path: string, body?: unknown): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, {
            method: 'PUT',
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined
        })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    async patch<T>(path: string, body?: unknown): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, {
            method: 'PATCH',
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined
        })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    async delete<T>(path: string, body?: unknown): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, {
            method: 'DELETE',
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined
        })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    // Shared EventSource instances keyed by full URL to avoid exhausting
    // the browser's per-origin HTTP/1.1 connection limit (6 in Chrome).
    private _eventSources = new Map<string, {
        es: InstanceType<typeof EventSource>,
        callbacks: Set<(data: unknown) => void>
    }>()

    subscribe<T = SseEvent>(path: string, callback: (data: T) => void): () => void {
        // Route broad event feeds through the unified /events endpoint to avoid
        // exhausting the browser's per-origin SSE connection limit, but keep
        // dedicated subscription-specific streams on their own path because
        // they are not mirrored into the unified feed.
        const useDirectPath = path.startsWith('/api/v1/events/query-subscription/')
        const targetPath = useDirectPath ? path : '/api/v1/events/unified'
        const url = `${this.baseUrl}${targetPath}`
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const separator = url.includes('?') ? '&' : '?'
        const fullUrl = tokenParam ? `${url}${separator}${tokenParam}` : url

        let entry = this._eventSources.get(fullUrl)
        if (!entry) {
            const es = nativeFetch
                ? new EventSource(fullUrl, { fetch: nativeFetch })
                : new EventSource(fullUrl)
            entry = { es, callbacks: new Set() }
            const ref = entry          // stable reference for closure
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

    /** Close all open EventSource connections */
    closeAll(): void {
        for (const [url, entry] of this._eventSources) {
            entry.es.close()
            entry.callbacks.clear()
        }
        this._eventSources.clear()
    }
}
