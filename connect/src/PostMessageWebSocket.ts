/**
 * A WebSocket-compatible class that proxies all communication through
 * window.parent via postMessage instead of opening a real WebSocket.
 *
 * Used in embedded mode when the host/parent application sends AD4M_CONFIG
 * with proxy: true. The host opens a real WebSocket to the AD4M daemon and
 * forwards raw frames bidirectionally via the AD4M_PROXY_WS_* protocol.
 *
 * The constructor URL is intentionally ignored — the connection target is
 * determined by the host, not the embedded app.
 */
export class PostMessageWebSocket {
    static readonly CONNECTING = 0
    static readonly OPEN = 1
    static readonly CLOSING = 2
    static readonly CLOSED = 3

    readyState: number = 0 // CONNECTING

    onopen:    ((e: Event) => void) | null = null
    onmessage: ((e: MessageEvent) => void) | null = null
    onerror:   ((e: Event) => void) | null = null
    onclose:   ((e: CloseEvent) => void) | null = null

    private readonly _messageHandler: (e: MessageEvent) => void
    private _connectTimeout: ReturnType<typeof setTimeout> | null = null
    private readonly _targetOrigin: string

    static readonly CONNECT_TIMEOUT_MS = 30_000

    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    constructor(_url: string, targetOrigin: string) {
        this._targetOrigin = targetOrigin
        this._messageHandler = (e: MessageEvent) => {
            if (e.source !== window.parent) return

            const msg = e.data
            if (!msg || typeof msg.type !== 'string') return

            if (msg.type === 'AD4M_PROXY_WS_OPEN') {
                this._clearConnectTimeout()
                this.readyState = 1
                this.onopen?.(new Event('open'))
                return
            }
            if (msg.type === 'AD4M_PROXY_WS_MESSAGE') {
                this.onmessage?.(new MessageEvent('message', { data: msg.data }))
                return
            }
            if (msg.type === 'AD4M_PROXY_WS_ERROR') {
                this._clearConnectTimeout()
                this.onerror?.(new Event('error'))
                return
            }
            if (msg.type === 'AD4M_PROXY_WS_CLOSED') {
                this._clearConnectTimeout()
                this.readyState = 3
                this.onclose?.(
                    new CloseEvent('close', {
                        code: typeof msg.code === 'number' ? msg.code : 1000,
                        reason: typeof msg.reason === 'string' ? msg.reason : '',
                        wasClean: true,
                    })
                )
                window.removeEventListener('message', this._messageHandler)
            }
        }

        window.addEventListener('message', this._messageHandler)
        window.parent.postMessage({ type: 'AD4M_PROXY_WS_CONNECT' }, this._targetOrigin)

        this._connectTimeout = setTimeout(() => {
            this._connectTimeout = null
            window.removeEventListener('message', this._messageHandler)
            this.readyState = 3
            this.onerror?.(new Event('error'))
            this.onclose?.(new CloseEvent('close', { code: 1006, reason: 'Connection timeout', wasClean: false }))
        }, PostMessageWebSocket.CONNECT_TIMEOUT_MS)
    }

    private _clearConnectTimeout(): void {
        if (this._connectTimeout !== null) {
            clearTimeout(this._connectTimeout)
            this._connectTimeout = null
        }
    }

    send(data: string): void {
        window.parent.postMessage({ type: 'AD4M_PROXY_WS_SEND', data }, this._targetOrigin)
    }

    close(code?: number, reason?: string): void {
        this.readyState = 2 // CLOSING
        window.parent.postMessage({ type: 'AD4M_PROXY_WS_CLOSE', code, reason }, this._targetOrigin)
        window.removeEventListener('message', this._messageHandler)
    }
}
