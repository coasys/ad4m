// EventSource may come from DOM lib or a polyfill
import type { QueryLanguage } from './devtools/types';

declare var EventSource: {
    new(url: string): {
        onmessage: ((event: { data: string }) => void) | null
        onerror: ((event: Event) => void) | null
        close(): void
    }
}

/** Shape of SSE event data parsed from JSON. Callers can narrow via generics. */
export interface SseEvent {
    type: string
    [key: string]: unknown
}

export class RestClient {
    constructor(private baseUrl: string, private token?: string) {}

    getBaseUrl(): string { return this.baseUrl; }
    getToken(): string | undefined { return this.token; }
    getActiveEventStreams(): number { return this._eventSources.size; }

    setToken(token: string) {
        this.token = token
    }

    private headers(): Record<string, string> {
        const h: Record<string, string> = { 'Content-Type': 'application/json' }
        if (this.token) h['Authorization'] = `Bearer ${this.token}`
        return h
    }

    private devtools(): any {
        if (typeof globalThis === 'undefined') return undefined
        return (globalThis as any).__AD4M_DEVTOOLS__
    }

    private normalizeHeaders(headers: any): Record<string, string> {
        if (!headers) return {}
        if (typeof headers.forEach === 'function') {
            const normalized: Record<string, string> = {}
            headers.forEach((value: string, key: string) => {
                normalized[key] = value
            })
            return normalized
        }
        return { ...(headers as Record<string, string>) }
    }

    private extractStringField(body: unknown, field: string): string | undefined {
        if (!body || typeof body !== 'object' || Array.isArray(body)) return undefined
        const value = (body as Record<string, unknown>)[field]
        return typeof value === 'string' ? value : undefined
    }

    private inferQueryLanguage(path: string): QueryLanguage | undefined {
        if (path.includes('/query/prolog') || path.includes('/subscribe-infer')) return 'prolog'
        if (path.includes('/subscribe-query') || /\/query(?:$|\?)/.test(path)) return 'sparql'
        return undefined
    }

    private async parseResponseBody(response: any): Promise<any> {
        const text = await response.text()
        if (!text) return null
        try {
            return JSON.parse(text)
        } catch {
            return text
        }
    }

    private async request<T>(method: string, path: string, body?: unknown): Promise<T> {
        const url = `${this.baseUrl}${path}`
        const headers = this.headers()
        const devtools = this.devtools()
        const queryLanguage = this.inferQueryLanguage(path)
        const sparqlQuery = queryLanguage === 'sparql'
            ? this.extractStringField(body, 'query')
            : undefined

        let operationId: number | undefined
        try {
            operationId = devtools?.logOperation?.({
                type: 'request',
                transport: 'rest',
                operationName: `${method} ${path}`,
                method,
                path,
                url,
                queryLanguage,
                requestBody: body,
                requestHeaders: headers,
                sparqlQuery,
                startTime: Date.now(),
            })
        } catch {}

        let response: any
        try {
            response = await fetch(url, {
                method,
                headers,
                body: body !== undefined ? JSON.stringify(body) : undefined,
            })

            const parsed = await this.parseResponseBody(response)
            const completeOptions = {
                statusCode: response.status,
                responseHeaders: this.normalizeHeaders(response.headers),
            }

            if (!response.ok) {
                const message = (typeof parsed === 'string' && parsed) || response.statusText || `HTTP ${response.status}`
                const error = new Error(message)
                ;(error as any).name = 'HttpError'
                ;(error as any).status = response.status
                ;(error as any).statusCode = response.status
                try {
                    if (operationId !== undefined) {
                        devtools?.completeOperation?.(operationId, parsed, [error], completeOptions)
                    }
                } catch {}
                throw error
            }

            try {
                if (operationId !== undefined) {
                    devtools?.completeOperation?.(operationId, parsed, undefined, completeOptions)
                }
            } catch {}

            return parsed as T
        } catch (error) {
            try {
                if (operationId !== undefined && !response) {
                    devtools?.completeOperation?.(operationId, null, [error])
                }
            } catch {}
            throw error
        }
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
        callbacks: Set<(data: unknown) => void>
    }>()

    subscribe<T = SseEvent>(path: string, callback: (data: T) => void): () => void {
        // Route all subscriptions through the unified /events endpoint
        const unifiedPath = '/api/v1/events/unified'
        const url = `${this.baseUrl}${unifiedPath}`
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const separator = url.includes('?') ? '&' : '?'
        const fullUrl = tokenParam ? `${url}${separator}${tokenParam}` : url

        let entry = this._eventSources.get(fullUrl)
        if (!entry) {
            const es = new EventSource(fullUrl)
            entry = { es, callbacks: new Set() }
            const ref = entry
            es.onmessage = (event) => {
                let parsed: Record<string, unknown>
                try {
                    parsed = JSON.parse(event.data)
                } catch (e) {
                    console.error('Error parsing SSE data:', e)
                    return
                }
                try {
                    this.devtools()?.recordEventStreamMessage?.()
                } catch {}
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
        for (const [, entry] of this._eventSources) {
            entry.es.close()
            entry.callbacks.clear()
        }
        this._eventSources.clear()
    }
}
