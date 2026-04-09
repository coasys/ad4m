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

    subscribe(path: string, callback: (data: any) => void): () => void {
        const url = `${this.baseUrl}${path}`
        const tokenParam = this.token ? `token=${encodeURIComponent(this.token)}` : ''
        const separator = url.includes('?') ? '&' : '?'
        const fullUrl = tokenParam ? `${url}${separator}${tokenParam}` : url
        const eventSource = new EventSource(fullUrl)
        eventSource.onmessage = (event) => {
            try {
                callback(JSON.parse(event.data))
            } catch (e) {
                console.error('Error parsing SSE data:', e)
            }
        }
        eventSource.onerror = (e) => {
            console.error('SSE error:', e)
        }
        return () => eventSource.close()
    }
}
