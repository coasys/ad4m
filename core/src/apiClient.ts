import { callSafely } from './notifyListeners'
import { Mirror } from './mirror/Mirror'
import { agentStatusEntry } from './agent/statusEntry'
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
    /** Error code (maps to HTTP status semantics: 400, 401, 403, 404, 422, 500). */
    readonly status: number
    /** Raw error message from server. */
    readonly body: string
    /**
     * Structured detail the executor attached (`error.data`), when it did: e.g. the
     * `InstanceError` of a refused `perspective.instance*` write.
     */
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
     *  {@link LONG_TIMEOUT_MS} for a method the executor marks long. A call
     *  that opens something on the executor (`OPENS_METHODS`, a live query) is
     *  cancelled there when it times out, as when `signal` aborts it; any other
     *  call that times out still runs to its end. */
    timeoutMs?: number
    /** `true` for a request that cannot wait the normal socket check. On a
     *  socket nothing arrived on since it went out, it probes after
     *  URGENT_PENDING_PROBE_MS rather than PENDING_PROBE_MS, and that probe
     *  waits URGENT_CHECK_CONNECTION_DEADLINE_MS rather than
     *  CHECK_CONNECTION_DEADLINE_MS for any frame — with the same drain
     *  allowance for a large send ahead of the probe's ping. A probe already
     *  running gets its verdict moved earlier to this wait, never later.
     *  It shortens only this request's wait: any frame still proves the
     *  socket, and a request without it keeps the 4 s / 8 s timing. */
    urgent?: boolean
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
 *  ping, and half of this more for the pong. A busy socket's pong can wait
 *  seconds behind live-query results on the executor's side, so this is
 *  generous: a probe that drops a healthy socket costs every live query
 *  and can cost the call. */
const CHECK_CONNECTION_DEADLINE_MS = 8_000

/** A socket that delivered a message this recently is alive: `checkConnection` sends no probe. */
const CHECK_CONNECTION_HEARD_MS = 5_000

/** A request with no reply this long on a socket nothing arrived on since
 *  the request was sent makes the socket itself suspect: `checkConnection`
 *  probes it. A request on a socket that keeps delivering is just slow, and
 *  times out as an RPC failure — the frames that did arrive prove it. */
const PENDING_PROBE_MS = 4_000

/** The PENDING_PROBE_MS wait for a request that cannot last it
 *  (`CallOptions.urgent`): media lost to a dead socket is restarted inside
 *  a window the normal wait cannot meet. It shortens only that request's
 *  wait — the heard-skip and the one-probe rule are unchanged. */
const URGENT_PENDING_PROBE_MS = 1_000

/** The CHECK_CONNECTION_DEADLINE_MS verdict for a probe an urgent request
 *  started, with the same drain allowance: a large send ahead of the
 *  probe's ping still gets its crossing time plus half of this for the
 *  pong. A probe already running gets its verdict moved earlier to the
 *  same wait — this from the ping it waits on — and never later. Shorter
 *  only because the urgent request cannot wait longer — a busy socket's
 *  pong still gets the deadline above. */
const URGENT_CHECK_CONNECTION_DEADLINE_MS = 3_000

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
    /** True once the message went out on the current socket. */
    sent: boolean
    /** When the message last went out on the current socket (`performance.now`). */
    sentAt: number
    /** Sockets that closed before this call went out. */
    failedConnects: number
    /** True while a retryable read has its one retry left. */
    retry: boolean
    /** An `events.watch`: it serves the subscribers, so it does not keep the socket open. */
    watch: boolean
    /** A request that cannot wait the normal probe timing (`CallOptions.urgent`). */
    urgent: boolean
    resolve: (value: unknown) => void
    reject: (reason: unknown) => void
}

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

    /**
     * The values this client read from its executor, one entry per data point. A reconnect
     * or a `lagged` marker (the executor dropped events for this socket) marks every entry
     * stale. Two clients share nothing.
     */
    readonly mirror = new Mirror(() => {
        // The path is part of the executor's address, as in the socket URL: two executors on one host differ by it.
        try { const url = new URL(this.baseUrl); return url.origin + url.pathname.replace(/\/+$/, '') } catch { return this.baseUrl.replace(/\/+$/, '') }
    }, (name, handler) => this._addHandler(name, handler), () => agentStatusEntry(this).getSnapshot().value?.did || null)

    getBaseUrl(): string { return this.baseUrl }
    getToken(): string | undefined { return this.token }

    /** Perform an HTTP request, using an injected fetch implementation if provided. */
    doFetch(url: string, init: RequestInit): Promise<Response> {
        return this._fetchImpl ? this._fetchImpl(url, init) : fetch(url, init)
    }

    // ── WebSocket ───────────────────────────────────────────────────────────

    /** The connecting or open socket; null otherwise. */
    private _ws: WebSocket | null = null
    /** The socket that opened and has not been lost since: the one that keeps the mirror fresh. */
    private _liveWs: WebSocket | null = null
    /** `on()` handlers by event type; a type with none has no entry. */
    private _handlers = new Map<string, Set<Registration>>()
    private _reconnectCallbacks = new Set<() => void>()
    private _authFailedCallbacks = new Set<(reason: string) => void>()
    /** Set once the executor refused the token: the reason. The client then
     *  never connects again; a new token needs a new client. */
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
    /** When the running probe gives up (`performance.now` scale), as `_armProbe` armed it. */
    private _wsProbeAt = 0
    /** The wait for the next request's PENDING_PROBE_MS silence; null when no sent request needs one. */
    private _wsPendingProbeTimer: ReturnType<typeof setTimeout> | null = null
    /** What `_wsPendingProbeTimer` is armed for. */
    private _wsPendingProbeAt = 0
    /** When the open socket last delivered a message. */
    private _wsHeardAt = -Infinity
    // The executor sends a socket only the events it asked for (`events.watch`).
    // What the executor has for the open socket, as sent; a new socket has `{}`.
    private _watching: string | null = '{}'
    private _watchScheduled = false
    private _watchRetries = 0
    private _watchRetryTimer: ReturnType<typeof setTimeout> | null = null
    private _networkCallbacks = new Set<() => void>()
    /** Removes the network-change listeners; null while not listening. */
    private _unlistenNetwork: (() => void) | null = null
    /** A network change waits for an open socket to tell `onNetworkChange` on. */
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
        // Before the socket is reused: a refusal's first notifications run while the refused socket is still open.
        if (this._authFailed !== null) throw new RpcError(401, this._authFailed)
        if (this._ws) return
        this._listenForNetworkChanges()
        // A dial ends any wait for one: a call or subscriber that dials during the backoff
        // leaves no timer behind to swallow the retry after this socket's own drop.
        this._cancelReconnect()
        const WsImpl = this._webSocketImpl ?? globalThis.WebSocket
        const ws = new WsImpl(this._getWsUrl())
        this._ws = ws
        ws.onopen = () => this._onOpen(ws)
        ws.onclose = (event?: CloseEvent) => {
            if (event?.code === CLOSE_AUTH_FAILED && this._ws === ws) {
                this._failAuth(event.reason || 'Unauthorized access: the token expired or was revoked')
            } else {
                this._onClose(ws, event?.code === CLOSE_TRY_AGAIN_LATER ? event.reason : undefined)
            }
        }

        ws.onmessage = (event) => {
            if (this._ws === ws) {
                this._wsPingedAt = null
                this._wsHeardAt = performance.now()
                this._endProbe()
                this._armPendingProbe()
                // A frame also proves the socket a path change was judging:
                // the change's callbacks can go out on it now.
                if (this._networkChanged) {
                    this._networkChanged = false
                    this._notifyNetworkChange()
                }
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
                const error = parsed.error as { code?: number; message?: string; data?: unknown } | undefined
                if (!error) {
                    pending.resolve(parsed.result)
                    return
                }
                const rpcError = new RpcError(error.code ?? 500, error.message ?? 'Unknown error', error.data)
                pending.reject(rpcError)
                // The executor answers 401 only when the token stopped working, then closes the socket.
                if (rpcError.status === 401) this._failAuth(rpcError.body)
                return
            }
            // A dropped socket's events are not this client's any more: its successor watches again.
            if (parsed.type === 'pong' || this._ws !== ws) return
            // The executor sends this on a socket that dropped events: pushes may be lost.
            if (parsed.type === 'query-subscription-update' && parsed.lagged === true) this.mirror.markAllStale()
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
        this._liveWs = ws
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        this._startPing(ws)
        // The watch goes out before the queued calls, so the events they cause reach listeners.
        this._watching = '{}'
        this._watchRetries = 0
        this._flushWatch()
        for (const pending of this._pendingCalls.values()) {
            if (!pending.sent) {
                this._send(ws, pending.message)
                pending.sent = true
                pending.sentAt = performance.now()
            }
        }
        this._armPendingProbe()
        // Whatever went stale without a socket (a loss, a refusal, a close) loads now, once.
        this.mirror.reloadWatched()
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
     * global object (browsers, Deno) redials at once and tells
     * `onNetworkChange`, and a `change` on `navigator.connection` (Chromium)
     * whose connection type or online state moved marks a change and runs
     * `checkConnection` on the socket. Elsewhere, e.g. Node, nothing is
     * heard and the ping deadline notices a dead socket on its own.
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
        const navigator = (globalThis as { navigator?: { connection?: Partial<EventTarget> & { type?: string }; onLine?: boolean } }).navigator
        const connection = navigator?.connection
        if (typeof connection?.addEventListener === 'function') {
            // `change` also fires when Chromium's downlink or rtt estimate
            // moves, as a busy call makes it do. Only a new connection type or
            // a new online state says the path may have changed.
            const path = () => `${connection.type}|${navigator?.onLine}`
            let lastPath = path()
            const hasType = connection.type !== undefined
            const onPathChange = () => {
                const now = path()
                if (now !== lastPath) {
                    lastPath = now
                    this._onConnectionChange()
                    return
                }
                // The platform reports no `type` (desktop Chromium): the
                // event can still mean the path moved underneath — a
                // Wi-Fi-to-Ethernet switch and a moved estimate look alike
                // in the key. Let the socket judge it: a real change kills
                // it and the probe's drop redials; an estimate's keeps it
                // and marks nothing.
                if (!hasType) this.checkConnection()
            }
            connection.addEventListener('change', onPathChange)
            removers.push(() => connection.removeEventListener?.('change', onPathChange))
        }
        this._unlistenNetwork = () => { for (const remove of removers) remove() }
    }

    /**
     * `online` says the network changed: the open socket most likely runs
     * over a path that is gone, and nothing would say so before the ping
     * deadline. Drop it as a silent socket is dropped and dial the next one
     * now, without backoff. `onNetworkChange` callbacks fire once that
     * socket opens.
     */
    private _onNetworkChange(): void {
        if (this._authFailed !== null) return
        const ws = this._ws
        // A socket dialled for an earlier change is on the new network already.
        if (ws && ws === this._networkDial && ws.readyState === 0 /* CONNECTING */) return
        if (ws) console.info('Network changed; reconnecting the WebSocket')
        this._redialNow()
        if (this._handlers.size === 0 && this._pendingCalls.size === 0) {
            this._notifyNetworkChange()
            return
        }
        this._networkChanged = true
        this._networkDial = this._ws
    }

    /**
     * The local path changed (`navigator.connection` reported a new `type`
     * or `onLine`): media must restart, so `onNetworkChange` fires even if
     * the socket survives. The socket itself is judged by socket evidence —
     * `checkConnection` — and a probe that gives up only redials
     * (`_redialNow`), which raises no second notification: the mark here is
     * this change's one.
     *
     * The callbacks still run once a socket is open, so calls they make go
     * out on it rather than dying on a doomed one: a socket just heard from
     * needs no probe, so they run on it at once; with a probe in flight the
     * verdict decides — a frame notifies from `onmessage`, a drop notifies
     * on the next socket's open; a dial in flight or due notifies on open.
     */
    private _onConnectionChange(): void {
        if (this._authFailed !== null) return
        this._networkChanged = true
        const ws = this._ws
        if (ws?.readyState === 1 /* OPEN */) {
            this.checkConnection()
            // A probe, new or already running, decides the socket now.
            if (this._wsProbeTimer) return
            // checkConnection saw no need to probe (a frame just arrived):
            // the socket is open and proven, so the callbacks go out on it.
            this._networkChanged = false
            this._notifyNetworkChange()
            return
        }
        // No open socket to judge: a dial in flight or the backoff's one
        // notifies when it opens; cut that wait short, as `online` does —
        // the new path may reach the executor sooner.
        if (this._handlers.size === 0 && this._pendingCalls.size === 0) {
            this._networkChanged = false
            this._notifyNetworkChange()
            return
        }
        this._cancelReconnect()
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        if (!ws) this._ensureWs()
        this._networkDial = this._ws
    }

    /**
     * Drop the open socket, if there is one, and dial the next one at once,
     * without the reconnect backoff. A probe that gave up on a silent
     * socket takes this path too — socket evidence, not a network change —
     * so it marks no `_networkChanged` and fires no `onNetworkChange`
     * callback; `onReconnect` on the new socket is the only signal it
     * sends.
     */
    private _redialNow(): void {
        const ws = this._ws
        if (ws) {
            this._onClose(ws)
            ws.close()
        }
        this._cancelReconnect()
        this._wsReconnectDelay = INITIAL_RECONNECT_DELAY_MS
        if (this._handlers.size > 0 || this._pendingCalls.size > 0) this._ensureWs()
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
        // Events from now until the next socket are lost: what the mirror holds may be old.
        if (this._liveWs === ws) {
            this._liveWs = null
            // No socket to read from: the watched values reload when the next one opens.
            const resets = this.mirror.resets
            this.mirror.markAllStale(false)
            // A subscriber told of the loss closed the client: that close ends this operation, no reconnect follows.
            if (resets !== this.mirror.resets) return
        }
        for (const pending of this._pendingCalls.values()) {
            if (!pending.sent && ++pending.failedConnects < MAX_CONNECT_ATTEMPTS) continue
            if (pending.sent && pending.retry) {
                pending.sent = false
                pending.retry = false
            } else {
                pending.reject(closedError(reason))
            }
        }
        if (this._handlers.size > 0 || this._pendingCalls.size > 0) this._scheduleReconnect()
    }

    /** The executor refused the token: fail every call with 401, stop
     *  reconnecting and tell the `onAuthFailed` callbacks, once. */
    private _failAuth(reason: string): void {
        if (this._authFailed !== null) return
        this._authFailed = reason
        const error = new RpcError(401, reason)
        for (const pending of this._pendingCalls.values()) pending.reject(error)
        // No reconnect will mark the mirror stale: a read must reach the refusal, not a stored value.
        this.mirror.refuse()
        this._closeWs()
        for (const cb of [...this._authFailedCallbacks]) callSafely(cb, 'Error in auth-failed callback:', reason)
    }

    private _startPing(ws: WebSocket): void {
        this._stopPing()
        this._wsPingedAt = null
        this._wsHeardAt = -Infinity
        this._wsSentBy = 0
        this._wsPingTimer = setInterval(() => {
            if (this._ws !== ws) return
            // A probe decides for this ping.
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
        this._endPendingProbe()
    }

    private _endProbe(): void {
        if (this._wsProbeTimer) {
            clearTimeout(this._wsProbeTimer)
            this._wsProbeTimer = null
            this._wsProbeAt = 0
        }
    }

    private _endPendingProbe(): void {
        if (this._wsPendingProbeTimer) {
            clearTimeout(this._wsPendingProbeTimer)
            this._wsPendingProbeTimer = null
        }
        this._wsPendingProbeAt = 0
    }

    /**
     * A request sent on the open socket that nothing has answered — no reply,
     * and no other frame since it went out — asks the socket itself after
     * PENDING_PROBE_MS, or URGENT_PENDING_PROBE_MS for one sent urgent. The
     * check is one wake for the earliest such request; it fires into
     * `checkConnection`, so a running probe covers a normal request, an
     * urgent one moves that probe's verdict earlier to its own wait, and
     * the CHECK_CONNECTION_HEARD_MS silence rule applies. A request the
     * socket outlives (resent after a reconnect) is armed again when it
     * goes out.
     */
    private _armPendingProbe(): void {
        const now = performance.now()
        let due = Infinity
        for (const pending of this._pendingCalls.values()) {
            // A frame at or after a request's send proves the socket for it.
            if (!pending.sent || this._wsHeardAt >= pending.sentAt) continue
            // The heard-skip is built into the wait rather than retried.
            const at = Math.max(
                pending.sentAt + (pending.urgent ? URGENT_PENDING_PROBE_MS : PENDING_PROBE_MS),
                this._wsHeardAt + CHECK_CONNECTION_HEARD_MS,
            )
            // A request already due got its probe (or is covered by a running one).
            if (at > now && at < due) due = at
        }
        if (due === Infinity) {
            this._endPendingProbe()
            return
        }
        if (this._wsPendingProbeTimer && this._wsPendingProbeAt <= due) return
        this._endPendingProbe()
        this._wsPendingProbeAt = due
        this._wsPendingProbeTimer = setTimeout(() => {
            this._wsPendingProbeTimer = null
            this._wsPendingProbeAt = 0
            const now = performance.now()
            // One probe covers every pending call; an urgent request's own
            // wait falling due asks for the urgent deadline.
            let probe = false, urgent = false
            for (const pending of this._pendingCalls.values()) {
                if (!pending.sent || this._wsHeardAt >= pending.sentAt) continue
                if (now - pending.sentAt >= (pending.urgent ? URGENT_PENDING_PROBE_MS : PENDING_PROBE_MS)) {
                    probe = true
                    if (pending.urgent) { urgent = true; break }
                }
            }
            if (probe) this.checkConnection(urgent)
            this._armPendingProbe()
        }, due - now)
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
     *   `urgent` shortens the socket check an unanswered request starts.
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
        const message = JSON.stringify({ id, type, params: params || {} })

        return new Promise<unknown>((resolve, reject) => {
            const settle = (fn: () => void) => {
                if (!this._pendingCalls.delete(id)) return
                clearTimeout(timer)
                signal?.removeEventListener('abort', onAbort)
                fn()
            }
            // Tell the executor the call is no longer wanted, so it stops the work. A late
            // reply finds no pending call and is dropped.
            const cancel = () => {
                if (pending.sent && this._ws?.readyState === 1 /* OPEN */) {
                    this._send(this._ws, JSON.stringify({ id: nextId(), type: 'request.cancel', params: { targetId: id } }))
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
                sentAt: 0,
                failedConnects: 0,
                retry: (READ_METHODS as ReadonlySet<string>).has(type),
                watch: type === 'events.watch',
                urgent: options?.urgent === true,
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
                pending.sentAt = performance.now()
                this._armPendingProbe()
            }
        })
    }

    // ── Event subscriptions ─────────────────────────────────────────────────

    /**
     * Call `handler` with every `type` event the executor pushes, or only those
     * about `filter.perspective`. The client asks the executor for the events
     * its handlers need (`events.watch`). The watch goes out ahead of any call
     * this client sends after `on()`, on the same socket, and the executor
     * applies it before it reads the next frame: an event a later call of this
     * client causes reaches the handler. An event another client causes (a
     * peer's signal) may come before the executor applied the watch, and is
     * not sent; the caller needs its own barrier for those. Returns a function
     * that removes this handler. Like `addEventListener`, registering the same
     * handler for the same type and perspective again changes nothing.
     */
    on<K extends EventName>(type: K, handler: (event: EventMap[K]) => void, filter?: EventFilter): () => void {
        const remove = this._addHandler(type, handler, filter)
        // A client whose token was refused keeps the handler but opens no socket.
        if (this._authFailed === null) this._ensureWs()
        return remove
    }

    /** {@link on} without dialling: the mirror's handlers must be registered again after `closeAll()` with no socket left open. */
    private _addHandler<K extends EventName>(type: K, handler: (event: EventMap[K]) => void, filter?: EventFilter): () => void {
        const perspective = filter?.perspective
        let regs = this._handlers.get(type)
        if (!regs) this._handlers.set(type, regs = new Set())
        let reg = [...regs].find(r => r.handler === handler && r.perspective === perspective)
        if (!reg) {
            reg = { handler: handler as (event: never) => void, perspective }
            regs.add(reg)
            this._scheduleWatch()
        }

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
        queueMicrotask(() => {
            this._watchScheduled = false
            this._flushWatch()
        })
    }

    /** Send `events.watch` now if the socket is open and the executor lacks
     *  the current interest; a socket still connecting gets it on open. */
    private _flushWatch(): void {
        // With no handlers left this sends `{}`, so the executor stops sending events.
        if (this._ws?.readyState !== 1 /* OPEN */) return
        const events = this.watchedEvents()
        const key = JSON.stringify(events)
        if (key === this._watching) return
        this._watching = key
        this._request('events.watch', events).then(() => { this._watchRetries = 0 }, (e) => {
            if (this._watching !== key) return
            this._watching = null
            // A refused token (401) gets no next socket; a closed socket (503) gets the
            // watch again when the next one opens.
            if (e instanceof RpcError && (e.status === 401 || e.status === 503)) return
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
        this._cancelReconnect()
        const ws = this._ws
        this._ws = null
        ws?.close()
    }

    /** Register a callback that fires after a successful WebSocket reconnect.
     *  Does NOT fire on the initial connection — only on reconnects.
     *  Returns an unsubscribe function. */
    onReconnect(callback: () => void): () => void {
        this._reconnectCallbacks.add(callback)
        return () => { this._reconnectCallbacks.delete(callback) }
    }

    /** Register a callback that fires after the network changed — `online`
     *  on the global object, or a `navigator.connection` `change` whose
     *  connection type or online state moved — once a socket is open and its
     *  event watch sent. Calls made from it go out on that socket. A socket
     *  dropped for answering a probe with silence is not a network change:
     *  `onReconnect` fires for it, this does not. Returns an unsubscribe
     *  function. */
    onNetworkChange(callback: () => void): () => void {
        this._networkCallbacks.add(callback)
        this._listenForNetworkChanges()
        return () => { this._networkCallbacks.delete(callback) }
    }

    /**
     * Check the open socket now rather than at the next ping: send a ping,
     * and if nothing at all arrives on that socket within
     * CHECK_CONNECTION_DEADLINE_MS — URGENT_CHECK_CONNECTION_DEADLINE_MS
     * for `urgent` — drop it and dial the next one at once — socket
     * evidence, not a network change, so the redial fires `onReconnect`
     * but no `onNetworkChange`. Any message keeps it. The client runs it
     * itself when the network path may have changed
     * (`navigator.connection`, where the change itself is what notifies),
     * and when a request has had no reply for PENDING_PROBE_MS on a socket
     * nothing arrived on since it was sent — an urgent request's own wait
     * is URGENT_PENDING_PROBE_MS and its probe asks for the urgent
     * deadline. An urgent request that falls due while a probe already
     * runs moves that probe's verdict earlier — the urgent deadline from
     * the ping it waits on — never later. Without an open socket, or when
     * the socket delivered a message in the last CHECK_CONNECTION_HEARD_MS,
     * this does nothing.
     */
    checkConnection(urgent = false): void {
        const ws = this._ws
        if (!ws || ws.readyState !== 1 /* OPEN */) return
        if (this._wsProbeTimer) {
            if (urgent) this._hastenProbe(ws)
            return
        }
        if (performance.now() - this._wsHeardAt < CHECK_CONNECTION_HEARD_MS) return
        this._probe(ws, urgent ? URGENT_CHECK_CONNECTION_DEADLINE_MS : CHECK_CONNECTION_DEADLINE_MS)
    }

    private _probe(ws: WebSocket, deadline: number): void {
        const first = this._wsPingedAt === null
        if (first) this._wsPingedAt = performance.now()
        this._send(ws, JSON.stringify({ type: 'ping' }))
        if (first) this._wsPingArrivesBy = this._wsSentBy
        // Any message from the socket ends the probe (`onmessage` → `_endProbe`).
        // The ping reaches the executor only after what was sent before it,
        // and its pong gets half the deadline to come back after that.
        const wait = Math.max(
            deadline,
            this._wsSentBy - performance.now() + deadline / 2,
        )
        this._armProbe(ws, performance.now() + wait)
    }

    /**
     * A request that cannot wait the running probe's verdict moves that
     * verdict earlier: the urgent deadline from the ping the probe waits
     * on, with that ping's own drain allowance — and never later than the
     * probe's own. It stays one probe: no new ping goes out, and any frame
     * still ends it.
     */
    private _hastenProbe(ws: WebSocket): void {
        if (this._wsPingedAt === null) return
        const at = Math.max(
            this._wsPingedAt + URGENT_CHECK_CONNECTION_DEADLINE_MS,
            this._wsPingArrivesBy + URGENT_CHECK_CONNECTION_DEADLINE_MS / 2,
        )
        if (at >= this._wsProbeAt) return
        if (this._wsProbeTimer) clearTimeout(this._wsProbeTimer)
        this._armProbe(ws, at)
    }

    /**
     * Arm the probe's give-up for `at` (`performance.now` scale). A probe
     * that gives up only redials: socket evidence, not a network change.
     */
    private _armProbe(ws: WebSocket, at: number): void {
        this._wsProbeAt = at
        this._wsProbeTimer = setTimeout(() => {
            this._wsProbeTimer = null
            if (this._ws !== ws) return
            console.warn(`WebSocket silent for ${performance.now() - (this._wsPingedAt ?? performance.now())} ms after a check; reconnecting`)
            // Socket evidence, not a network change: only redial.
            this._redialNow()
        }, Math.max(0, at - performance.now()))
    }

    /** Register a callback that fires once when the executor refuses this
     *  client's token: a 401 reply, or a socket closed with code 1008. Every
     *  call then fails with a 401 `RpcError` and the client does not reconnect;
     *  build a new client to use another token. Returns an unsubscribe function. */
    onAuthFailed(callback: (reason: string) => void): () => void {
        this._authFailedCallbacks.add(callback)
        return () => { this._authFailedCallbacks.delete(callback) }
    }

    /** Close the WebSocket and reject pending calls. */
    closeAll(): void {
        this._handlers.clear()
        // Before the pending calls are rejected: the reads and disposals this starts fail with them.
        this.mirror.reset()
        for (const pending of this._pendingCalls.values()) {
            pending.reject(new RpcError(503, 'Client closed'))
        }
        this._liveWs = null
        this._closeWs()
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
