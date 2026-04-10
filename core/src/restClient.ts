// EventSource may come from DOM lib or a polyfill
declare var EventSource: {
    new(url: string): {
        onmessage: ((event: { data: string }) => void) | null
        onerror: ((event: any) => void) | null
        close(): void
    }
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

    async post<T>(path: string, body?: any): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, {
            method: 'POST',
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined
        })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    async put<T>(path: string, body?: any): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, {
            method: 'PUT',
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined
        })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    async patch<T>(path: string, body?: any): Promise<T> {
        const res = await fetch(`${this.baseUrl}${path}`, {
            method: 'PATCH',
            headers: this.headers(),
            body: body !== undefined ? JSON.stringify(body) : undefined
        })
        if (!res.ok) throw new Error(await res.text())
        return res.json() as Promise<T>
    }

    async delete<T>(path: string, body?: any): Promise<T> {
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
        callbacks: Set<(data: any) => void>
    }>()

    subscribe(path: string, callback: (data: any) => void): () => void {
        // Route all subscriptions through the unified /events endpoint
        const unifiedPath = '/api/v1/events'
        const url = `${this.baseUrl}${unifiedPath}`
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const separator = url.includes('?') ? '&' : '?'
        const fullUrl = tokenParam ? `${url}${separator}${tokenParam}` : url

        let entry = this._eventSources.get(fullUrl)
        if (!entry) {
            const es = new EventSource(fullUrl)
            entry = { es, callbacks: new Set() }
            const ref = entry          // stable reference for closure
            es.onmessage = (event) => {
                let parsed: any
                try { parsed = JSON.parse(event.data) } catch (e) {
                    console.error('Error parsing SSE data:', e)
                    return
                }
                for (const cb of ref.callbacks) cb(parsed)
            }
            es.onerror = (e) => { console.error('SSE error:', e) }
            this._eventSources.set(fullUrl, entry)
        }

        entry.callbacks.add(callback)

        return () => {
            const e = this._eventSources.get(fullUrl)
            if (!e) return
            e.callbacks.delete(callback)
            if (e.callbacks.size === 0) {
                e.es.close()
                this._eventSources.delete(fullUrl)
            }
        }
    }
}
