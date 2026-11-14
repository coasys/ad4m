import { callSafely } from './notifyListeners'
import { LONG_METHODS, OPENS_METHODS, READ_METHODS } from './generated/api/RpcMethods'
import type { RpcMethod, RpcMethods } from './generated/api/RpcMethods'
import type { EventMap, EventName } from './generated/api/Events'

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

/** Options accepted by [`ApiClient.call`]. */
export interface CallOptions {
    /**
     * Aborting sends `request.cancel` and rejects the call with an
     * `AbortError`. The executor drops the reply; it cannot always stop the work.
     */
    signal?: AbortSignal
    /** Timeout in ms, from the call to the reply. Defaults to 30 s, or
     *  {@link LONG_TIMEOUT_MS} for a method the executor marks long. A call
     *  that opens something on the executor (`OPENS_METHODS`, a live query) is
     *  cancelled there when it times out, as when `signal` aborts it; any other
     *  call that times out still runs to its end. */
    timeoutMs?: number
    /**
     * Why the call is made, sent as `meta.reason` (at most 280 characters are kept). A bot's
     * write or join needs one; its owner reads it in the bot's activity log. Overrides the
     * client's {@link ApiClient.setReason}.
     */
    reason?: string
}

/** Narrows an {@link ApiClient.on} handler to one perspective's events. */
export interface EventFilter {
    perspective?: string
}

interface Registration {
    handler: (event: never) => void
    perspective?: string
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

/** An open socket sends a ping this often, and must deliver some message
 *  (the pong will do) within an interval of the first ping it left
 *  unanswered. After a network switch a socket can stay OPEN for minutes with
 *  no close event, since no FIN or RST ever arrives; this notices it within
 *  two intervals. */
const PING_INTERVAL_MS = 10_000

/** The slowest link a socket is given time for. The executor reads a ping
 *  only after the frames sent before it, so a ping left unanswered counts as
 *  silence only once everything the socket sent before that ping could have
 *  crossed this link and PONG_RETURN_MS more have passed, if that takes longer
 *  than PING_INTERVAL_MS. Messages that come in meanwhile do not shorten this:
 *  they say nothing about how far an upload has got. */
const SLOWEST_LINK_BYTES_PER_MS = 16 // 16 kB/s

/** How long a pong gets to come back after its ping could have been read
 *  behind a large write. Shorter than PING_INTERVAL_MS, so the few ms a small
 *  frame adds before a ping on an idle socket never cost a ping interval. */
const PONG_RETURN_MS = 5_000

/** How long `checkConnection` waits for the open socket to say anything
 *  after its ping. If a large write sent before it may still be on the way,
 *  the wait lasts until that write could have crossed, as for the periodic
 *  ping, and half of this more for the pong. On a working path the pong is
 *  one round trip away. */
const CHECK_CONNECTION_DEADLINE_MS = 2_000

/** The bytes `text` takes as UTF-8, as a WebSocket text frame sends it. */
function utf8Length(text: string): number {
    let bytes = text.length
    for (let i = 0; i < text.length; i++) {
        const unit = text.charCodeAt(i)
        // A surrogate is half of a 4-byte pair: 2 units, 2 extra bytes.
        if (unit >= 0x80) bytes += unit < 0x800 || (unit >= 0xd800 && unit <= 0xdfff) ? 1 : 2
    }
    return bytes
}

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
    /** True once the message went out on a socket. */
    sent: boolean
    /** True once the message went out at all: unlike `sent`, it stays set while a
     *  retryable read is armed to go out again, so the retry may go out only
     *  under the principal the first send ran as (a tokenless one included). */
    wentOut: boolean
    /** The socket the message went out on — a superseded one keeps answering it. */
    ws?: WebSocket
    /** The token of the socket the message went out on: the principal it executes
     *  as. A retry may go out again only under that same principal. */
    token?: string
    /** Sockets that closed before this call went out. */
    failedConnects: number
    /** True while a retryable read has its one retry left. */
    retry: boolean
    /** An `events.watch`: it serves the subscribers, so it does not keep the socket open. */
    watch: boolean
    /** Opens state on the executor bound to the socket it goes out on (a live
     *  query's subscription): a reply from a socket `setToken` replaced answers
     *  with a handle that dies when that socket does. */
    bound: boolean
    resolve: (value: unknown) => void
    reject: (reason: unknown) => void
}

/** Calls whose reply is bound to the socket they went out on: they open a live
 *  query's subscription, which the executor ends with that connection. */
const SOCKET_BOUND_METHODS: ReadonlySet<string> = new Set([
    'perspective.subscribeQuery',
    'perspective.subscribeSparql',
    'perspective.modelSubscribe',
])

const closedError = (reason?: string) => new RpcError(503, reason || 'WebSocket connection closed')

/** Close code for "try again later": the executor's wallet is locked, so it cannot check the
 *  token yet. The token may be good: reconnect as for any close, and pass the reason on. */
const CLOSE_TRY_AGAIN_LATER = 1013

/** Close code the executor uses when the socket's token opens no session, expired or was revoked. */
const CLOSE_AUTH_FAILED = 1008

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

    /** The reason sent with every call that names none (`meta.reason`); see {@link CallOptions.reason}. */
    private _reason: string | undefined

    /**
     * Sends `reason` as `meta.reason` with every call from now on, until set again;
     * `undefined` stops. A bot's harness sets it before a write made through the SDK's
     * clients, whose methods take no call options.
     */
    setReason(reason: string | undefined): void { this._reason = reason }

    /** Use `token` from now on: a renewed key session's, say. A socket opened
     *  with a different token is replaced — the session it carries is that
     *  token's principal's, so an anonymous or old-grant socket would stay what
     *  it is. Calls that went out on it settle there (a reply resolves by id
     *  from any socket, so nothing in flight is dropped or repeated) and it
     *  closes once they do; a call not sent yet goes out on the new socket, and
     *  subscriptions are watched again there — the watch goes out before the
     *  queued calls on open, so an event a call of this client causes still
     *  reaches its handlers. Missed are the events emitted between this call
     *  and the applied watch, the same gap a reconnect has: a superseded
     *  socket's events are not dispatched, since past the new watch an event
     *  would reach both sockets and deliver twice. A client whose old token the
     *  executor refused connects again for its event handlers. */
    setToken(token: string): void {
        const changed = token !== this.token
        this.token = token
        // An explicit new principal ends an intentional close's generation: a
        // wait it parked follows the socket this opens, as a dropped one's would.
        this._closed = false
        if (this._authFailed !== null) {
            this._authFailed = null
            // The refused token's watch outcome is stale: a rejected `events.watch`
            // (its 401) must not answer a later `watchApplied()` on the new socket.
            this._watchSent = Promise.resolve()
            if (this._handlers.size > 0) this._ensureWs()
            return
        }
        if (!changed) return
        const ws = this._ws
        if (!ws || this._wsToken === token) return
        // The socket's session is the token it dialed with; swap it for one on
        // the new token. In-flight calls finish on it — a reply resolves by id
        // from any socket — so nothing in flight is dropped or repeated. A
        // watch it sent is re-sent on the new socket, so it keeps nothing open.
        this._stopPing()
        this._superseded.add(ws)
        this._wasSuperseded.add(ws)
        this._ws = null
        this._wsOpen = null
        this._wsToken = undefined
        for (const pending of this._pendingCalls.values()) {
            if (pending.ws === ws && pending.watch) {
                pending.ws = undefined
                pending.resolve(undefined)
            }
        }
        if (this._handlers.size > 0 || this._pendingCalls.size > 0) this._ensureWs()
        // The socket replacing one dialled for a network change is on the new
        // network too: a repeat of that change must not drop it while it connects.
        if (this._networkDial === ws) this._networkDial = this._ws
        // Nothing went out on it: let it go now.
        if (![...this._pendingCalls.values()].some(p => p.ws === ws)) {
            this._superseded.delete(ws)
            ws.close()
        }
    }

    /** Perform an HTTP request, using an injected fetch implementation if provided. */
    doFetch(url: string, init: RequestInit): Promise<Response> {
        return this._fetchImpl ? this._fetchImpl(url, init) : fetch(url, init)
    }

    // ── WebSocket ───────────────────────────────────────────────────────────

    /** The connecting or open socket; null otherwise. */
    private _ws: WebSocket | null = null
    /** The token `_ws` dialed with: a close code is that token's verdict, not the current one's. */
    private _wsToken?: string
    /** Set by `_closeWs` — an intentional close — and cleared by the next dial:
     *  while it stands, `watchApplied()` answers 499 rather than open a socket
     *  for a wait the close ended. */
    private _closed = false
    /** Sockets `setToken` replaced, kept open while calls they carry are in flight. */
    private _superseded = new Set<WebSocket>()
    /** Sockets `setToken` ever replaced, remembered past their close (`_superseded`
     *  forgets one then), so `watchApplied()` can tell a replaced socket's
     *  open-rejection — its own verdict, never the wait's — from a live socket's. */
    private _wasSuperseded = new WeakSet<WebSocket>()
    /** Settles when `_ws` opens (resolve) or closes first (reject). */
    private _wsOpen: Promise<void> | null = null
    /** `on()` handlers by event type; a type with none has no entry. */
    private _handlers = new Map<string, Set<Registration>>()
    private _reconnectCallbacks = new Set<() => void>()
    private _authFailedCallbacks = new Set<(reason: string) => void>()
    /** Set once the executor refused the token: the reason. The client then
     *  does not connect again until `setToken` gives it another token. */
    private _authFailed: string | null = null
    private _hasConnectedOnce = false
    private _pendingCalls = new Map<string, PendingCall>()
    private _wsReconnectTimer: ReturnType<typeof setTimeout> | null = null
    private _wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
    private _wsPingTimer: ReturnType<typeof setInterval> | null = null
    /** When the open socket was first pinged since it last delivered a message; null while none is unanswered. */
    private _wsPingedAt: number | null = null
    /** When that ping could have reached the executor, which reads it only after what the socket sent before it. */
    private _wsPingArrivesBy = 0
    /** When everything the open socket sent so far could have crossed a SLOWEST_LINK_BYTES_PER_MS link.
     *  A message from the socket does not move it back: hearing from the executor says nothing
     *  about how much of a large upload has reached it. */
    private _wsSentBy = 0
    /** A `checkConnection` probe waiting for the open socket to say anything; null when none. */
    private _wsProbeTimer: ReturnType<typeof setTimeout> | null = null
    // The executor sends a socket only the events it asked for (`events.watch`).
    // What the executor has for the open socket, as sent; a new socket has `{}`.
    private _watching: string | null = '{}'
    private _watchScheduled = false
    private _watchDone: Promise<void> = Promise.resolve()
    // The last `events.watch` sent, settled once the executor replied — or, while
    // a failed send waits out its retry delay, once the retried watch's reply is in.
    private _watchSent: Promise<void> = Promise.resolve()
    private _watchRetries = 0
    private _watchRetryTimer: ReturnType<typeof setTimeout> | null = null
    /** Settles with the watch the pending retry's flush sends (its own retries
     *  chain off it), or resolves when that flush sends none: a dropped socket's
     *  watch goes out on the next open instead. A failed `events.watch` waits on
     *  it so `watchApplied()` answers only an applied watch. */
    private _watchRetryDone: Promise<void> | null = null
    /** Rejects `_watchRetryDone` when the retry it waits on is gone for good (the
     *  client closed the socket on purpose). A socket `setToken` replaced keeps
     *  the wait: the armed retry settles it with the new socket's watch. */
    private _watchRetryReject: ((reason: unknown) => void) | null = null
    private _networkCallbacks = new Set<() => void>()
    /** Removes the network-change listeners; null while not listening. */
    private _unlistenNetwork: (() => void) | null = null
    /** A network change waits for its socket to open, then tells `onNetworkChange`. */
    private _networkChanged = false
    /** The socket dialled for the last network change. */
    private _networkDial: WebSocket | null = null

    private _getWsUrl(): string {
        const wsBase = this.baseUrl
            .replace(/^http:\/\//, 'ws://')
            .replace(/^https:\/\//, 'wss://')
        const tokenParam = this.token ? `?token=${encodeURIComponent(this.token)}` : ''
        return `${wsBase}/api/v1/ws${tokenParam}`
    }

    private _ensureWs(): void {
        if (this._ws) return
        if (this._authFailed !== null) throw new RpcError(401, this._authFailed)
        // A dial ends the intentional close's generation: only an explicit open
        // reaches here — a dropped socket's reconnect timer died with it.
        this._closed = false
        this._listenForNetworkChanges()
        // A dial ends any wait for one: a call or subscriber that dials during the backoff
        // leaves no timer behind to swallow the retry after this socket's own drop.
        this._cancelReconnect()
        const WsImpl = this._webSocketImpl ?? globalThis.WebSocket
        const ws = new WsImpl(this._getWsUrl())
        this._ws = ws
        this._wsToken = this.token
        this._wsOpen = new Promise<void>((resolve, reject) => {
            ws.onopen = () => {
                resolve()
                this._onOpen(ws)
            }
            ws.onclose = (event?: CloseEvent) => {
                // Classify the close for watchApplied() as for every other waiting call.
                reject(event?.code === CLOSE_AUTH_FAILED
                    ? new RpcError(401, event.reason || 'token expired or revoked')
                    : closedError(event?.code === CLOSE_TRY_AGAIN_LATER ? event.reason : undefined))
                if (this._superseded.delete(ws)) {
                    // A socket `setToken` replaced: its close says something about the
                    // token it dialed with, never about the current one. The calls it
                    // carried get the answer a dropped socket's do.
                    this._releaseCalls(ws, event?.code === CLOSE_TRY_AGAIN_LATER ? event.reason : undefined)
                    return
                }
                // A refusal is the verdict of the token this socket dialed with: after
                // `setToken` it counts nothing against the current token, and the next
                // socket dials with that.
                if (event?.code === CLOSE_AUTH_FAILED && this._ws === ws && this._wsToken === this.token) {
                    this._failAuth(event.reason || 'Unauthorized access: the token expired or was revoked')
                } else {
                    this._onClose(ws, event?.code === CLOSE_TRY_AGAIN_LATER ? event.reason : undefined)
                }
            }
        })
        // Only watchApplied() awaits this; a close before open is not an error otherwise.
        this._wsOpen.catch(() => {})

        ws.onmessage = (event) => {
            if (this._ws === ws) {
                this._wsPingedAt = null
                this._endProbe()
            }
            let parsed: Record<string, unknown>
            try { parsed = JSON.parse(event.data) } catch (e) {
                console.error('Error parsing WebSocket data:', e)
                return
            }
            // Events carry a `type`; RPC responses never do.
            if (parsed.type === undefined) {
                const pending = this._pendingCalls.get(parsed.id as string)
                if (!pending) return // late reply to a timed-out or aborted call, or a cancel ack
                const error = parsed.error as { code?: number; message?: string } | undefined
                if (!error) {
                    // A socket-bound call's reply on a socket `setToken` replaced names
                    // state that dies when this socket does: refuse it, so the caller's
                    // open path answers on the current socket instead.
                    if (pending.bound && this._superseded.has(ws)) {
                        pending.reject(closedError())
                    } else {
                        pending.resolve(parsed.result)
                    }
                    return
                }
                const rpcError = new RpcError(error.code ?? 500, error.message ?? 'Unknown error')
                // Read the socket's standing before the settle lets a drained superseded one go.
                const superseded = this._superseded.has(ws)
                pending.reject(rpcError)
                // The executor answers 401 only when the token stopped working, then closes the
                // socket. On a superseded socket the verdict is the token it dialed with, never
                // the current one: the new token stands or falls by its own socket.
                if (rpcError.status === 401 && !superseded) this._failAuth(rpcError.body)
                return
            }
            // A dropped socket's events are not this client's any more: its successor watches
            // again. A superseded one's still arrive while it finishes its calls and go
            // undelivered on purpose: an event emitted after the successor's `events.watch`
            // landed reaches both sockets, so taking the old one's too would deliver it twice.
            if (parsed.type === 'pong' || this._ws !== ws) return
            const perspective = parsed.perspectiveUuid
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
            // A socket this client already let go (closed, or dropped as silent) is no news:
            // Node reports an `error` when the server ends the connection without answering
            // the close frame, as the executor does after the last listener goes.
            if (this._ws !== ws) return
            console.error('WebSocket error:', e)
        }
    }

    private _onOpen(ws: WebSocket): void {
        // A socket closed by _closeWs() must not take the calls queued for its successor.
        if (this._ws !== ws) return
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        this._startPing(ws)
        // The watch goes out before the queued calls, so the events they cause reach listeners.
        this._watching = '{}'
        this._watchRetries = 0
        this._flushWatch()
        this._flushCalls(ws)
        if (this._hasConnectedOnce) {
            for (const cb of this._reconnectCallbacks) {
                try { cb() } catch (e) {
                    console.error('Error in reconnect callback:', e)
                }
            }
        }
        this._hasConnectedOnce = true
        if (this._networkChanged) {
            this._networkChanged = false
            this._notifyNetworkChange()
        }
    }

    /**
     * Hear of network changes where the platform says so: `online` on the
     * global object (browsers, Deno) redials at once, and `change` on
     * `navigator.connection` (Chromium) runs `checkConnection`. Elsewhere,
     * e.g. Node, nothing is heard and the ping deadline notices a dead
     * socket on its own.
     */
    private _listenForNetworkChanges(): void {
        if (this._unlistenNetwork) return
        const handler = () => this._onNetworkChange()
        const removers: (() => void)[] = []
        const target = globalThis as unknown as Partial<EventTarget>
        if (typeof target.addEventListener === 'function') {
            target.addEventListener('online', handler)
            removers.push(() => target.removeEventListener?.('online', handler))
        }
        const connection = (globalThis as { navigator?: { connection?: Partial<EventTarget> } }).navigator?.connection
        if (typeof connection?.addEventListener === 'function') {
            // `change` also fires when Chromium's downlink or rtt estimate
            // moves, as a busy call makes it do: probe rather than drop.
            const probe = () => this.checkConnection()
            connection.addEventListener('change', probe)
            removers.push(() => connection.removeEventListener?.('change', probe))
        }
        this._unlistenNetwork = () => { for (const remove of removers) remove() }
    }

    /**
     * The network changed: the open socket most likely runs over a path that
     * is gone, and nothing would say so before the ping deadline. Drop it as
     * a silent socket is dropped and dial the next one now, without backoff.
     * `onNetworkChange` callbacks fire once that socket opens.
     */
    private _onNetworkChange(): void {
        if (this._authFailed !== null) return
        const ws = this._ws
        // A socket dialled for an earlier change is on the new network already.
        if (ws && ws === this._networkDial && ws.readyState === 0 /* CONNECTING */) return
        if (ws) {
            console.info('Network changed; reconnecting the WebSocket')
            this._onClose(ws)
            ws.close()
        }
        // Sockets `setToken` replaced ran over the same gone path: their calls get
        // the answer a dropped socket's do now (a retryable read goes out on the
        // next socket, a write gets a 503), not after a close that can take minutes.
        for (const old of [...this._superseded]) {
            this._superseded.delete(old)
            this._releaseCalls(old)
            old.close()
        }
        this._cancelReconnect()
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        if (this._handlers.size === 0 && this._pendingCalls.size === 0) {
            this._notifyNetworkChange()
            return
        }
        this._networkChanged = true
        this._ensureWs()
        this._networkDial = this._ws
    }

    private _notifyNetworkChange(): void {
        for (const cb of [...this._networkCallbacks]) callSafely(cb, 'Error in network-change callback:')
    }

    /** `reason` is set when the executor asked to try again later; calls that fail carry it. */
    private _onClose(ws: WebSocket, reason?: string): void {
        // A socket closed by _closeWs() can report after a new one exists.
        if (this._ws !== ws) return
        this._stopPing()
        this._ws = null
        this._wsOpen = null
        this._wsToken = undefined
        for (const pending of this._pendingCalls.values()) {
            if (!pending.sent && ++pending.failedConnects < MAX_CONNECT_ATTEMPTS) continue
            if (pending.sent && pending.ws !== ws) continue // a superseded socket's close decides
            pending.ws = undefined
            // A read gets its one retry only under the principal it went out as:
            // answered under a different token it would serve the new principal's data.
            if (pending.sent && pending.retry && pending.token === this.token) {
                pending.sent = false
                pending.retry = false
            } else {
                pending.reject(closedError(reason))
            }
        }
        if (this._handlers.size > 0 || this._pendingCalls.size > 0) this._scheduleReconnect()
    }

    /** The close of a socket `setToken` replaced: the calls it carried get the
     *  answer a dropped socket's would (a retryable read goes out on the next
     *  socket, a write gets an honest 503 — never a second send). */
    private _releaseCalls(ws: WebSocket, reason?: string): void {
        for (const pending of this._pendingCalls.values()) {
            if (pending.ws !== ws) continue
            pending.ws = undefined
            // A read gets its one retry only under the principal it went out as:
            // answered under a different token it would serve the new principal's data.
            if (pending.retry && pending.token === this.token) {
                pending.sent = false
                pending.retry = false
            } else {
                pending.reject(closedError(reason))
            }
        }
        // A read the closing socket carried goes out on its successor now, while it
        // is open: `_onOpen` is the only other place unsent calls are flushed, and
        // this socket's successor opened already.
        if (this._ws?.readyState === 1 /* OPEN */) this._flushCalls(this._ws)
    }

    /** Every call not yet sent goes out on `ws` (the open socket). */
    private _flushCalls(ws: WebSocket): void {
        for (const pending of this._pendingCalls.values()) {
            if (!pending.sent) {
                // A call that went out once does not go out again under another
                // principal — a tokenless one included: its answer would be the
                // new principal's. A call that never went out still may.
                if (pending.wentOut && pending.token !== this.token) {
                    pending.reject(closedError())
                    continue
                }
                this._send(ws, pending.message)
                pending.sent = true
                pending.wentOut = true
                pending.ws = ws
                pending.token = this._wsToken
            }
        }
    }

    /** The executor refused the token: fail the refused socket's calls and the
     *  calls that never went out with 401, stop reconnecting and tell the
     *  `onAuthFailed` callbacks, once. A call in flight on a superseded socket
     *  keeps its own verdict — it settles there, and that socket closes when
     *  they drain, as for any other close. */
    private _failAuth(reason: string): void {
        if (this._authFailed !== null) return
        this._authFailed = reason
        const error = new RpcError(401, reason)
        const refused = this._ws
        for (const pending of this._pendingCalls.values()) {
            // A call that went out on a socket `setToken` replaced answers on that
            // socket: the refusal is the verdict of the refused socket's token, not
            // of the token the call's socket dialed with.
            if (pending.ws !== undefined && pending.ws !== refused) continue
            pending.reject(error)
        }
        this._closeWs()
        for (const cb of [...this._authFailedCallbacks]) callSafely(cb, 'Error in auth-failed callback:', reason)
    }

    private _startPing(ws: WebSocket): void {
        this._stopPing()
        this._wsPingedAt = null
        this._wsSentBy = 0
        this._wsPingTimer = setInterval(() => {
            if (this._ws !== ws) return
            // A probe has its own, shorter deadline and decides for this ping.
            if (this._wsProbeTimer) return
            const ping = JSON.stringify({ type: 'ping' })
            if (this._wsPingedAt === null) {
                this._wsPingedAt = performance.now()
                this._send(ws, ping)
                this._wsPingArrivesBy = this._wsSentBy
                return
            }
            if (performance.now() >= Math.max(this._wsPingedAt + PING_INTERVAL_MS, this._wsPingArrivesBy + PONG_RETURN_MS)) {
                this._dropSilentSocket(ws)
                return
            }
            this._send(ws, ping)
        }, PING_INTERVAL_MS)
    }

    /** Send on `ws`, counting how long what the open socket sent could take to cross the slowest link. */
    private _send(ws: WebSocket, text: string): void {
        if (this._ws === ws) this._wsSentBy = Math.max(this._wsSentBy, performance.now()) + utf8Length(text) / SLOWEST_LINK_BYTES_PER_MS
        ws.send(text)
    }

    /** The socket answered nothing for a whole ping interval: treat it as
     *  closed now. Its close event can take minutes on a dead connection,
     *  since the close frame gets no answer either; `_onClose` ignores it then. */
    private _dropSilentSocket(ws: WebSocket): void {
        console.warn(`WebSocket silent for ${performance.now() - (this._wsPingedAt ?? performance.now())} ms after a ping; reconnecting`)
        this._onClose(ws)
        ws.close()
    }

    private _stopPing(): void {
        if (this._wsPingTimer) {
            clearInterval(this._wsPingTimer)
            this._wsPingTimer = null
        }
        this._endProbe()
    }

    private _endProbe(): void {
        if (this._wsProbeTimer) {
            clearTimeout(this._wsProbeTimer)
            this._wsProbeTimer = null
        }
    }

    private _scheduleReconnect(): void {
        if (this._wsReconnectTimer || this._authFailed !== null) return
        const delay = this._wsReconnectDelay
        this._wsReconnectDelay = Math.min(this._wsReconnectDelay * 2, MAX_RECONNECT_DELAY_MS)
        this._wsReconnectTimer = setTimeout(() => {
            this._wsReconnectTimer = null
            this._listenForWake(false)
            this._ensureWs()
        }, delay)
        this._listenForWake(true)
    }

    /** End the wait for the next dial, if one is pending, without dialling. */
    private _cancelReconnect(): void {
        if (this._wsReconnectTimer) {
            clearTimeout(this._wsReconnectTimer)
            this._wsReconnectTimer = null
        }
        this._listenForWake(false)
    }

    /**
     * Dial now if the client is waiting out its reconnect backoff (up to 30 s)
     * instead of letting the wait run out. For the moments the host knows the
     * executor may be reachable again: an executor it restarted is up, the
     * device woke. In a browser the client does this itself on `online` and when
     * the page becomes visible. Does nothing while a socket is open or
     * connecting, after `closeAll()` with nothing to reconnect, or once the
     * executor refused the token.
     */
    reconnectNow(): void {
        if (!this._wsReconnectTimer || this._ws || this._authFailed !== null) return
        this._cancelReconnect()
        // The executor is likely back, so a failure now starts the backoff over.
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        this._ensureWs()
    }

    private _wakeListening = false
    /** `online` always; `visibilitychange` only when the page became visible. */
    private readonly _onWake = (event: Event): void => {
        if (event.type === 'visibilitychange' && typeof document !== 'undefined' && document.visibilityState !== 'visible') return
        this.reconnectNow()
    }

    /** Listen for `online` and `visibilitychange` exactly while a reconnect
     *  timer is pending. Outside a browser (no `window`) it registers nothing. */
    private _listenForWake(on: boolean): void {
        if (on === this._wakeListening) return
        const win = typeof window !== 'undefined' ? window : undefined
        if (typeof win?.addEventListener !== 'function') return
        const doc = typeof document !== 'undefined' && typeof document.addEventListener === 'function' ? document : undefined
        this._wakeListening = on
        if (on) {
            win.addEventListener('online', this._onWake)
            doc?.addEventListener('visibilitychange', this._onWake)
        } else {
            win.removeEventListener('online', this._onWake)
            doc?.removeEventListener('visibilitychange', this._onWake)
        }
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

    private _request(type: string, params?: unknown, options?: CallOptions): Promise<unknown> {
        const signal = options?.signal
        if (signal?.aborted) {
            return Promise.reject(new DOMException('Aborted', 'AbortError'))
        }
        const timeoutMs = options?.timeoutMs ?? ((LONG_METHODS as ReadonlySet<string>).has(type) ? LONG_TIMEOUT_MS : DEFAULT_TIMEOUT_MS)
        const id = nextId()
        // Params go under "params" so they cannot clash with "id" and "type".
        const reason = options?.reason ?? this._reason
        const message = JSON.stringify(
            reason === undefined ? { id, type, params: params || {} } : { id, type, params: params || {}, meta: { reason } }
        )

        return new Promise<unknown>((resolve, reject) => {
            const settle = (fn: () => void) => {
                if (!this._pendingCalls.delete(id)) return
                clearTimeout(timer)
                signal?.removeEventListener('abort', onAbort)
                // The last call a superseded socket carried: let it go.
                const ws = pending.ws
                pending.ws = undefined
                if (ws && this._superseded.has(ws)
                    && ![...this._pendingCalls.values()].some(p => p.ws === ws)) {
                    this._superseded.delete(ws)
                    ws.close()
                }
                fn()
            }
            // Tell the executor the call is no longer wanted, so it stops the work. A late
            // reply finds no pending call and is dropped.
            // The cancel goes to the socket the call went out on: a superseded one
            // still carries it, and the executor knows the call only there.
            const cancel = () => {
                const target = pending.ws ?? this._ws
                if (pending.sent && target?.readyState === 1 /* OPEN */) {
                    this._send(target, JSON.stringify({ id: nextId(), type: 'request.cancel', params: { targetId: id } }))
                }
            }
            const timer = setTimeout(() => {
                // Only a call that opens something is cancelled on timeout: a write that
                // timed out still runs to its end on the executor, as it always has.
                if ((OPENS_METHODS as ReadonlySet<string>).has(type)) cancel()
                settle(() => reject(new RpcError(408, `RPC call '${type}' timed out after ${timeoutMs}ms`)))
                // A connect that never completes (a black-holed port) would hold every later
                // call. Drop it once no call waits on it, so the next call dials again.
                if (this._ws?.readyState === 0 /* CONNECTING */ && this._pendingCalls.size === 0) {
                    this._closeWs()
                    if (this._handlers.size > 0) this._ensureWs()
                }
            }, timeoutMs)
            const onAbort = () => {
                cancel()
                settle(() => reject(new DOMException('Aborted', 'AbortError')))
            }
            const pending: PendingCall = {
                message,
                sent: false,
                wentOut: false,
                failedConnects: 0,
                retry: (READ_METHODS as ReadonlySet<string>).has(type),
                watch: type === 'events.watch',
                bound: SOCKET_BOUND_METHODS.has(type),
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
                this._send(this._ws!, message)
                pending.sent = true
                pending.wentOut = true
                pending.ws = this._ws!
                pending.token = this._wsToken
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
        const perspective = filter?.perspective
        let regs = this._handlers.get(type)
        if (!regs) this._handlers.set(type, regs = new Set())
        let reg = [...regs].find(r => r.handler === handler && r.perspective === perspective)
        if (!reg) {
            reg = { handler: handler as (event: never) => void, perspective }
            regs.add(reg)
            this._scheduleWatch()
        }
        // A client whose token was refused keeps the handler but opens no socket.
        if (this._authFailed === null) this._ensureWs()

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
        for (;;) {
            // An intentional close ends the wait wherever it stands — waiting on
            // the open, on the watch reply or on a retry — rather than dial again.
            // A refused token is terminal too, with its own verdict: the 401
            // `_ensureWs` and the socket's close carry.
            if (this._closed && this._authFailed === null) throw new RpcError(499, 'Client closed')
            this._ensureWs()
            const ws = this._ws!
            const open = this._wsOpen!
            if (ws.readyState !== 1 /* OPEN */) {
                try {
                    await open
                } catch (e) {
                    if (this._closed && this._authFailed === null) throw new RpcError(499, 'Client closed')
                    // A rejection of a socket `setToken` replaced before it opened —
                    // its close's verdict is that replaced socket's, not the wait's:
                    // loop onto the current socket. Every other rejection propagates,
                    // a still-current socket's close or a refusal among them.
                    if (this._wasSuperseded.has(ws)) continue
                    throw e
                }
            }
            await this._watchDone
            await this._watchSent
            // The socket these settled on may not be the current one any more —
            // `setToken` replaced it, or it dropped and a successor opened: what a
            // waiter needs applied is the current socket's watch, so it waits on
            // that one too. Its failure (a refused token, a close) propagates.
            if (this._closed && this._authFailed === null) throw new RpcError(499, 'Client closed')
            if (this._ws === ws) return
        }
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
        const sent = this._request('events.watch', events).then(() => { this._watchRetries = 0 }, (e) => {
            if (this._watching !== key) return
            this._watching = null
            // A refused token (401) gets no next socket: watchApplied() must not resolve.
            if (e instanceof RpcError && e.status === 401) throw e
            // A closed socket (503) gets the watch again when the next one opens.
            if (e instanceof RpcError && e.status === 503) return
            console.error('events.watch failed:', e)
            if (this._watchRetries++ >= MAX_WATCH_RETRIES) throw e
            return this._retryWatch()
        })
        // Only watchApplied() awaits this; a refused watch is not an unhandled rejection otherwise.
        sent.catch(() => {})
        this._watchSent = sent
    }

    /** The outcome of the watch a scheduled retry sends, awaited by the failed
     *  send instead of resolving: a waiter's `_watchSent` stays unsettled across
     *  the retry delay. Every failure while a retry is pending waits on the same
     *  one. */
    private _retryWatch(): Promise<void> {
        if (this._watchRetryDone) return this._watchRetryDone
        this._watchRetryDone = new Promise<void>((resolve, reject) => {
            this._watchRetryReject = reject
            this._watchRetryTimer = setTimeout(() => {
                this._watchRetryTimer = null
                this._watchRetryDone = null
                this._watchRetryReject = null
                this._scheduleWatch()
                this._watchDone.then(() => {
                    // The flush the timer queued sent a watch: its `_watchSent` is the
                    // outcome. It sent none when the socket is gone — the next open's
                    // watch answers instead, which watchApplied() reaches through its loop.
                    if (this._watching === null) resolve()
                    else this._watchSent.then(resolve, reject)
                })
            }, WATCH_RETRY_DELAY_MS)
        })
        return this._watchRetryDone
    }

    private _closeWs(): void {
        this._closed = true
        this._stopPing()
        if (this._watchRetryTimer) {
            clearTimeout(this._watchRetryTimer)
            this._watchRetryTimer = null
        }
        // An intentional close fails a pending watch retry: its waiter propagates
        // the refusal rather than settling against a socket it would then open
        // with no handlers left to serve.
        this._watchRetryReject?.(new RpcError(499, 'Client closed'))
        this._watchRetryDone = null
        this._watchRetryReject = null
        this._cancelReconnect()
        const ws = this._ws
        this._ws = null
        this._wsOpen = null
        this._wsToken = undefined
        ws?.close()
        // A superseded socket still carrying calls drains as designed; the rest
        // can go now.
        for (const old of this._superseded) {
            if (![...this._pendingCalls.values()].some(p => p.ws === old)) {
                this._superseded.delete(old)
                old.close()
            }
        }
    }

    /** Register a callback that fires after a successful WebSocket reconnect.
     *  Does NOT fire on the initial connection — only on reconnects.
     *  Returns an unsubscribe function. */
    onReconnect(callback: () => void): () => void {
        this._reconnectCallbacks.add(callback)
        return () => { this._reconnectCallbacks.delete(callback) }
    }

    /** Register a callback that fires after the network changed (`online`,
     *  or a socket that failed `checkConnection`), once the socket dialled for the
     *  new network is open and its event watch sent. Calls made from it go
     *  out on that socket. Returns an unsubscribe function. */
    onNetworkChange(callback: () => void): () => void {
        this._networkCallbacks.add(callback)
        this._listenForNetworkChanges()
        return () => { this._networkCallbacks.delete(callback) }
    }

    /**
     * Check the open socket now rather than at the next ping: send a ping,
     * and if nothing at all arrives on that socket within `deadlineMs`,
     * treat it as a network change (drop it, dial the next one at once,
     * `onNetworkChange` callbacks fire when it opens). Any message keeps it.
     * For a caller that has seen its network path break, e.g. ICE going
     * disconnected, where the platform fired no `online` event. Without an
     * open socket, or while a check runs, this does nothing.
     */
    checkConnection(deadlineMs = CHECK_CONNECTION_DEADLINE_MS): void {
        const ws = this._ws
        if (!ws || ws.readyState !== 1 /* OPEN */ || this._wsProbeTimer) return
        const first = this._wsPingedAt === null
        if (first) this._wsPingedAt = performance.now()
        this._send(ws, JSON.stringify({ type: 'ping' }))
        if (first) this._wsPingArrivesBy = this._wsSentBy
        // Any message from the socket ends the probe (`onmessage` → `_endProbe`).
        // The ping reaches the executor only after what was sent before it,
        // and its pong gets half the deadline to come back after that.
        const wait = Math.max(deadlineMs, this._wsSentBy - performance.now() + deadlineMs / 2)
        this._wsProbeTimer = setTimeout(() => {
            this._wsProbeTimer = null
            if (this._ws !== ws) return
            console.warn(`WebSocket silent for ${wait} ms after a check; reconnecting`)
            this._onNetworkChange()
        }, wait)
    }

    /** Register a callback that fires once when the executor refuses this
     *  client's token: a 401 reply, or a socket closed with code 1008. Every
     *  call then fails with a 401 `RpcError` and the client does not reconnect
     *  until `setToken` gives it another token. Returns an unsubscribe function. */
    onAuthFailed(callback: (reason: string) => void): () => void {
        this._authFailedCallbacks.add(callback)
        return () => { this._authFailedCallbacks.delete(callback) }
    }

    /** Close the WebSocket and reject pending calls. */
    closeAll(): void {
        for (const pending of this._pendingCalls.values()) {
            pending.reject(new RpcError(503, 'Client closed'))
        }
        this._closeWs()
        this._handlers.clear()
        this._reconnectCallbacks.clear()
        this._authFailedCallbacks.clear()
        this._networkCallbacks.clear()
        this._unlistenNetwork?.()
        this._unlistenNetwork = null
        this._networkChanged = false
        this._networkDial = null
        // A reused client must not fire onReconnect on its next first open.
        this._hasConnectedOnce = false
    }
}
