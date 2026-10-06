import { callSafely } from './notifyListeners'
import { LONG_METHODS, READ_METHODS } from './generated/api/RpcMethods'
import type { RpcMethod, RpcMethods } from './generated/api/RpcMethods'
import type { EventMap, EventName } from './generated/api/Events'
import { EVENT_SCOPE_FIELDS } from './generated/api/Events'

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
    /** Typed error detail, e.g. a service method error's `{ name, … }`. */
    readonly data?: unknown

    constructor(status: number, body: string, data?: unknown) {
        super(`RPC error ${status}: ${body}`)
        this.name = "RpcError"
        this.status = status
        this.body = body
        this.data = data
    }
}

/** Options accepted by [`ApiClient.call`]. */
export interface CallOptions {
    /**
     * Aborting sends `request.cancel` and rejects the call with an
     * `AbortError`. The executor drops the reply; it cannot always stop the work.
     */
    signal?: AbortSignal
    /** Timeout in ms, from the call to the reply. Defaults to 30 s, or
     *  {@link LONG_TIMEOUT_MS} for a method the executor marks long. */
    timeoutMs?: number
}

/** Narrows an {@link ApiClient.on} handler to one perspective's events. */
export interface EventFilter {
    perspective?: string
}

interface Registration {
    handler: (event: never) => void
    /** The scope value wanted: a perspective UUID for core events. */
    perspective?: string
}

/** Per-method flags of a call outside the executor's own table (a service method). */
export interface MethodFlags {
    /** Idempotent: resent once after a reconnect. */
    read?: boolean
    /** May run for minutes: uses {@link LONG_TIMEOUT_MS}. */
    long?: boolean
}

const DEFAULT_TIMEOUT_MS = 30_000

/** Default timeout for calls that can run for minutes: LLM work, Holochain, publishing. */
export const LONG_TIMEOUT_MS = 20 * 60 * 1000

const INITIAL_RECONNECT_DELAY_MS = 500
const MAX_RECONNECT_DELAY_MS = 30_000

/** A failed `events.watch` is sent again after this delay, at most
 *  MAX_WATCH_RETRIES times in a row. */
const WATCH_RETRY_DELAY_MS = 1_000
const MAX_WATCH_RETRIES = 5

let _idCounter = 0
function nextId(): string {
    return String(++_idCounter)
}


/** Sockets that may close before a call goes out before the call fails with
 *  503. A call that never went out never reached the executor, so waiting
 *  for the next socket is safe for writes too. This covers an executor that
 *  is still binding its port (about 1.5 s with the reconnect backoff). */
const MAX_CONNECT_ATTEMPTS = 3

interface PendingCall {
    message: string
    /** True once the message went out on the current socket. */
    sent: boolean
    /** Sockets that closed before this call went out. */
    failedConnects: number
    /** True while a retryable read has its one retry left. */
    retry: boolean
    /** An `events.watch`: it serves the subscribers, so it does not keep the socket open. */
    watch: boolean
    resolve: (value: unknown) => void
    reject: (reason: unknown) => void
}

const closedError = () => new RpcError(503, 'WebSocket connection closed')

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

    // ── WebSocket ───────────────────────────────────────────────────────────

    /** The connecting or open socket; null otherwise. */
    private _ws: WebSocket | null = null
    /** Settles when `_ws` opens (resolve) or closes first (reject). */
    private _wsOpen: Promise<void> | null = null
    /** `on()` handlers by event type; a type with none has no entry. */
    private _handlers = new Map<string, Set<Registration>>()
    /** Service event type → the payload field `events.watch` narrows on. */
    private _scopeFields = new Map<string, string>()
    private _reconnectCallbacks = new Set<() => void>()
    private _hasConnectedOnce = false
    private _pendingCalls = new Map<string, PendingCall>()
    private _wsReconnectTimer: ReturnType<typeof setTimeout> | null = null
    private _wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    private _wsPingTimer: ReturnType<typeof setInterval> | null = null
    // The executor sends a socket only the events it asked for (`events.watch`).
    // What the executor has for the open socket, as sent; a new socket has `{}`.
    private _watching: string | null = '{}'
    private _watchScheduled = false
    private _watchDone: Promise<void> = Promise.resolve()
    // The last `events.watch` sent, settled once the executor replied.
    private _watchSent: Promise<void> = Promise.resolve()
    private _watchRetries = 0
    private _watchRetryTimer: ReturnType<typeof setTimeout> | null = null

    private _getWsUrl(): string {
        const wsBase = this.baseUrl
            .replace(/^http:\/\//, 'ws://')
            .replace(/^https:\/\//, 'wss://')
        const tokenParam = this.token ? `?token=${encodeURIComponent(this.token)}` : ''
        return `${wsBase}/api/v1/ws${tokenParam}`
    }

    private _ensureWs(): void {
        if (this._ws) return
        const WsImpl = this._webSocketImpl ?? globalThis.WebSocket
        const ws = new WsImpl(this._getWsUrl())
        this._ws = ws
        this._wsOpen = new Promise<void>((resolve, reject) => {
            ws.onopen = () => {
                resolve()
                this._onOpen(ws)
            }
            ws.onclose = () => {
                reject(closedError())
                this._onClose(ws)
            }
        })
        // Only watchApplied() awaits this; a close before open is not an error otherwise.
        this._wsOpen.catch(() => {})

        ws.onmessage = (event) => {
            let parsed: Record<string, unknown>
            try { parsed = JSON.parse(event.data) } catch (e) {
                console.error('Error parsing WebSocket data:', e)
                return
            }
            // Events carry a `type`; RPC responses never do.
            if (parsed.type === undefined) {
                const pending = this._pendingCalls.get(parsed.id as string)
                if (!pending) return // late reply to a timed-out or aborted call, or a cancel ack
                const error = parsed.error as { code?: number; message?: string; data?: unknown } | undefined
                if (error) pending.reject(new RpcError(error.code ?? 500, error.message ?? 'Unknown error', error.data))
                else pending.resolve(parsed.result)
                return
            }
            if (parsed.type === 'pong') return
            // Most core events narrow on `perspectiveUuid`; the others, and
            // service events, on the field their table names.
            const type = parsed.type as string
            const perspective = parsed[this._scopeFields.get(type) ?? EVENT_SCOPE_FIELDS[type as EventName] ?? 'perspectiveUuid']
            const regs = this._handlers.get(parsed.type as string)
            // Like DOM events: a handler added during dispatch waits for the
            // next event, and a handler removed during dispatch gets no more.
            for (const reg of [...regs ?? []]) {
                if (!regs!.has(reg)) continue
                if (reg.perspective !== undefined && reg.perspective !== perspective) continue
                callSafely(reg.handler as (event: unknown) => void, `Error in '${parsed.type}' handler:`, parsed)
            }
        }

        ws.onerror = (e) => {
            console.error('WebSocket error:', e)
        }
    }

    private _onOpen(ws: WebSocket): void {
        // A socket closed by _closeWs() must not take the calls queued for its successor.
        if (this._ws !== ws) return
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        this._startPing()
        // The watch goes out before the queued calls, so the events they cause reach listeners.
        this._watching = '{}'
        this._watchRetries = 0
        this._flushWatch()
        for (const pending of this._pendingCalls.values()) {
            if (!pending.sent) {
                ws.send(pending.message)
                pending.sent = true
            }
        }
        if (this._hasConnectedOnce) {
            for (const cb of this._reconnectCallbacks) {
                try { cb() } catch (e) {
                    console.error('Error in reconnect callback:', e)
                }
            }
        }
        this._hasConnectedOnce = true
    }

    private _onClose(ws: WebSocket): void {
        // A socket closed by _closeWs() can report after a new one exists.
        if (this._ws !== ws) return
        this._stopPing()
        this._ws = null
        this._wsOpen = null
        for (const pending of this._pendingCalls.values()) {
            if (!pending.sent && ++pending.failedConnects < MAX_CONNECT_ATTEMPTS) continue
            if (pending.sent && pending.retry) {
                pending.sent = false
                pending.retry = false
            } else {
                pending.reject(closedError())
            }
        }
        if (this._handlers.size > 0 || this._pendingCalls.size > 0) this._scheduleReconnect()
    }

    private _startPing(): void {
        this._stopPing()
        this._wsPingTimer = setInterval(() => {
            if (this._ws?.readyState === 1 /* OPEN */) {
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
            this._ensureWs()
        }, delay)
    }

    // ── RPC call method ─────────────────────────────────────────────────────

    /**
     * Call an executor method over the WebSocket, connecting first if needed.
     * Params and result are typed by the executor's method table
     * (`generated/api/RpcMethods.ts`).
     * @param options - `signal` to cancel, `timeoutMs` to override the
     *   default timeout. The timeout covers connecting and the reply.
     */
    call<M extends RpcMethod>(method: M, params: RpcMethods[M]['params'], options?: CallOptions): Promise<RpcMethods[M]['result']> {
        // A listener added before this call needs its `events.watch` on the
        // wire first. The executor applies a watch as it reads it, so the
        // event this call causes reaches the listener.
        this._flushWatch()
        return this._request(method, params, options) as Promise<RpcMethods[M]['result']>
    }

    /**
     * Call a method that is not in the executor's table: a service method
     * `<hash>.<method>`. Typed wrappers (`ServiceClient`) sit on top.
     */
    callMethod(method: string, params: unknown, flags: MethodFlags, options?: CallOptions): Promise<unknown> {
        this._flushWatch()
        return this._request(method, params, options, flags)
    }

    private _request(type: string, params?: unknown, options?: CallOptions, flags?: MethodFlags): Promise<unknown> {
        const signal = options?.signal
        if (signal?.aborted) {
            return Promise.reject(new DOMException('Aborted', 'AbortError'))
        }
        const long = flags?.long ?? (LONG_METHODS as ReadonlySet<string>).has(type)
        const timeoutMs = options?.timeoutMs ?? (long ? LONG_TIMEOUT_MS : DEFAULT_TIMEOUT_MS)
        const id = nextId()
        // Params go under "params" so they cannot clash with "id" and "type".
        const message = JSON.stringify({ id, type, params: params || {} })

        return new Promise<unknown>((resolve, reject) => {
            const settle = (fn: () => void) => {
                if (!this._pendingCalls.delete(id)) return
                clearTimeout(timer)
                signal?.removeEventListener('abort', onAbort)
                fn()
            }
            const timer = setTimeout(() => {
                settle(() => reject(new RpcError(408, `RPC call '${type}' timed out after ${timeoutMs}ms`)))
                // A connect that never completes (a black-holed port) would hold every later
                // call. Drop it once no call waits on it, so the next call dials again.
                if (this._ws?.readyState === 0 /* CONNECTING */ && this._pendingCalls.size === 0) {
                    this._closeWs()
                    if (this._handlers.size > 0) this._ensureWs()
                }
            }, timeoutMs)
            const onAbort = () => {
                if (pending.sent && this._ws?.readyState === 1 /* OPEN */) {
                    this._ws.send(JSON.stringify({ id: nextId(), type: 'request.cancel', params: { targetId: id } }))
                }
                settle(() => reject(new DOMException('Aborted', 'AbortError')))
            }
            const pending: PendingCall = {
                message,
                sent: false,
                failedConnects: 0,
                retry: flags?.read ?? (READ_METHODS as ReadonlySet<string>).has(type),
                watch: type === 'events.watch',
                resolve: (value) => settle(() => resolve(value)),
                reject: (reason) => settle(() => reject(reason)),
            }
            this._pendingCalls.set(id, pending)
            signal?.addEventListener('abort', onAbort, { once: true })

            try {
                this._ensureWs()
            } catch (e) {
                pending.reject(e)
                return
            }
            if (this._ws!.readyState === 1 /* OPEN */) {
                this._ws!.send(message)
                pending.sent = true
            }
        })
    }

    // ── Event subscriptions ─────────────────────────────────────────────────

    /**
     * Call `handler` with every `type` event the executor pushes, or only those
     * about `filter.perspective`. The client asks the executor for the events
     * its handlers need (`events.watch`). Returns a function that removes this
     * handler. Like `addEventListener`, registering the same handler for the
     * same type and perspective again changes nothing.
     */
    on<K extends EventName>(type: K, handler: (event: EventMap[K]) => void, filter?: EventFilter): () => void {
        return this._on(type, handler as (event: never) => void, filter?.perspective)
    }

    /**
     * Listen to an event outside the executor's own table: a service event
     * `<hash>.<event>`. `scopeField` is the payload field the interface
     * narrows on; `scope` keeps only events whose field has that value.
     */
    onEvent(type: string, handler: (event: never) => void, scopeField?: string, scope?: string): () => void {
        if (scopeField) this._scopeFields.set(type, scopeField)
        return this._on(type, handler, scope)
    }

    private _on(type: string, handler: (event: never) => void, perspective?: string): () => void {
        let regs = this._handlers.get(type)
        if (!regs) this._handlers.set(type, regs = new Set())
        let reg = [...regs].find(r => r.handler === handler && r.perspective === perspective)
        if (!reg) {
            reg = { handler: handler as (event: never) => void, perspective }
            regs.add(reg)
            this._scheduleWatch()
        }
        this._ensureWs()

        return () => {
            const regs = this._handlers.get(type)
            if (!regs?.delete(reg)) return
            if (regs.size === 0) this._handlers.delete(type)
            this._scheduleWatch()
            if (this._handlers.size === 0 && [...this._pendingCalls.values()].every(p => p.watch)) {
                for (const pending of this._pendingCalls.values()) pending.reject(closedError())
                this._closeWs()
            }
        }
    }

    /**
     * Resolves once the socket is open and the executor has applied this
     * client's current `events.watch`. After it, every event the registered
     * handlers need reaches them. Only events other peers cause (signals)
     * need this: a call already sends the watch ahead of itself.
     */
    async watchApplied(): Promise<void> {
        this._ensureWs()
        if (this._ws!.readyState !== 1 /* OPEN */) await this._wsOpen
        await this._watchDone
        await this._watchSent
    }

    /** Event type → perspectives wanted (`null`: all), from every handler. */
    watchedEvents(): Partial<Record<EventName, string[] | null>> {
        const events: Partial<Record<EventName, string[] | null>> = {}
        for (const type of [...this._handlers.keys()].sort() as EventName[]) {
            const perspectives = new Set<string>()
            let all = false
            for (const reg of this._handlers.get(type)!) {
                if (reg.perspective === undefined) all = true
                else perspectives.add(reg.perspective)
            }
            events[type] = all ? null : [...perspectives].sort()
        }
        return events
    }

    /** Flush the watch once per microtask, after a subscriber came or went. */
    private _scheduleWatch(): void {
        if (this._watchScheduled) return
        this._watchScheduled = true
        this._watchDone = new Promise<void>(resolve => queueMicrotask(() => {
            this._watchScheduled = false
            this._flushWatch()
            resolve()
        }))
    }

    /** Send `events.watch` now if the socket is open and the executor lacks
     *  the current interest; a socket still connecting gets it on open.
     *  `_watchSent` settles with its reply. */
    private _flushWatch(): void {
        // With no handlers left this sends `{}`, so the executor stops sending events.
        if (this._ws?.readyState !== 1 /* OPEN */) return
        const events = this.watchedEvents()
        const key = JSON.stringify(events)
        if (key === this._watching) return
        this._watching = key
        this._watchSent = this._request('events.watch', events).then(() => { this._watchRetries = 0 }, (e) => {
            if (this._watching !== key) return
            this._watching = null
            // A closed socket (503) gets the watch again when the next one opens.
            if (e instanceof RpcError && e.status === 503) return
            console.error('events.watch failed:', e)
            if (!this._watchRetryTimer && this._watchRetries++ < MAX_WATCH_RETRIES) {
                this._watchRetryTimer = setTimeout(() => {
                    this._watchRetryTimer = null
                    this._scheduleWatch()
                }, WATCH_RETRY_DELAY_MS)
            }
        })
    }

    private _closeWs(): void {
        this._stopPing()
        if (this._watchRetryTimer) {
            clearTimeout(this._watchRetryTimer)
            this._watchRetryTimer = null
        }
        if (this._wsReconnectTimer) {
            clearTimeout(this._wsReconnectTimer)
            this._wsReconnectTimer = null
        }
        const ws = this._ws
        this._ws = null
        this._wsOpen = null
        ws?.close()
    }

    /** Register a callback that fires after a successful WebSocket reconnect.
     *  Does NOT fire on the initial connection — only on reconnects.
     *  Returns an unsubscribe function. */
    onReconnect(callback: () => void): () => void {
        this._reconnectCallbacks.add(callback)
        return () => { this._reconnectCallbacks.delete(callback) }
    }

    /** Close the WebSocket and reject pending calls. */
    closeAll(): void {
        for (const pending of this._pendingCalls.values()) {
            pending.reject(new RpcError(503, 'Client closed'))
        }
        this._closeWs()
        this._handlers.clear()
        this._reconnectCallbacks.clear()
        // A reused client must not fire onReconnect on its next first open.
        this._hasConnectedOnce = false
    }
}
