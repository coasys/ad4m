import { ApiClient, RpcError } from "./apiClient"

/**
 * Unit tests for ApiClient AbortSignal support.
 *
 * Uses a fake WebSocket implementation injected via the constructor so
 * we can drive open / send / message / close transitions deterministically
 * without spinning up a real server.
 */

type AnyMsg = Record<string, unknown>

class FakeWebSocket {
    static OPEN = 1
    static CONNECTING = 0
    static CLOSING = 2
    static CLOSED = 3

    readyState = FakeWebSocket.CONNECTING
    sent: string[] = []
    onopen: ((ev?: unknown) => void) | null = null
    onmessage: ((ev: { data: string }) => void) | null = null
    onerror: ((ev: unknown) => void) | null = null
    onclose: ((ev?: unknown) => void) | null = null

    static last: FakeWebSocket | null = null

    constructor(public url: string) {
        FakeWebSocket.last = this
        // Defer "open" so callers can attach handlers first.
        setTimeout(() => {
            this.readyState = FakeWebSocket.OPEN
            this.onopen?.()
        }, 0)
    }

    send(data: string) {
        this.sent.push(data)
    }

    close() {
        this.readyState = FakeWebSocket.CLOSED
        this.onclose?.()
    }

    // Test helper — simulate a server-sent message.
    serverPush(msg: AnyMsg) {
        this.onmessage?.({ data: JSON.stringify(msg) })
    }
}

function lastSent(ws: FakeWebSocket): AnyMsg | undefined {
    if (ws.sent.length === 0) return undefined
    return JSON.parse(ws.sent[ws.sent.length - 1]) as AnyMsg
}

function flushMicrotasks() {
    return new Promise((r) => setTimeout(r, 0))
}

describe('ApiClient AbortSignal support', () => {
    it('rejects immediately with AbortError if signal is already aborted', async () => {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        const controller = new AbortController()
        controller.abort()

        let caught: unknown
        try {
            await client.call('agent.get', {}, { signal: controller.signal })
        } catch (e) {
            caught = e
        }
        expect(caught).toBeInstanceOf(DOMException)
        expect((caught as DOMException).name).toBe('AbortError')
        // No socket should have been opened — the fast-path bailed before _ready.
        expect(FakeWebSocket.last).toBeNull()

        client.closeAll()
    })

    it('sends request.cancel and rejects with AbortError when signal fires mid-flight', async () => {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        const controller = new AbortController()
        const promise = client.call('perspective.querySparql', { uuid: 'u', engine: 'sparql', query: 'SELECT * WHERE { ?s ?p ?o }' }, { signal: controller.signal })

        // Wait for socket open + send.
        await flushMicrotasks()
        const ws = FakeWebSocket.last
        expect(ws).not.toBeNull()
        // First send should be the actual RPC request.
        const sentReq = JSON.parse(ws!.sent[0]) as AnyMsg
        expect(sentReq.type).toBe('perspective.querySparql')
        const origId = sentReq.id as string

        // Now abort.
        controller.abort()

        // Last sent message should be request.cancel pointing at orig id.
        const cancelMsg = lastSent(ws!)
        expect(cancelMsg).toBeDefined()
        expect(cancelMsg!.type).toBe('request.cancel')
        const params = cancelMsg!.params as { targetId: string }
        expect(params.targetId).toBe(origId)

        // The promise should now reject with AbortError.
        let caught: unknown
        try { await promise } catch (e) { caught = e }
        expect(caught).toBeInstanceOf(DOMException)
        expect((caught as DOMException).name).toBe('AbortError')

        client.closeAll()
    })

    it('late server reply after abort does not resolve or reject the original promise twice', async () => {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        const events: AnyMsg[] = []
        client.subscribe((m) => events.push(m as AnyMsg))

        const controller = new AbortController()
        const promise = client.call('perspective.querySparql', { uuid: 'u', engine: 'sparql', query: 'q' }, { signal: controller.signal })

        await flushMicrotasks()
        const ws = FakeWebSocket.last!
        const sentReq = JSON.parse(ws.sent[0]) as AnyMsg
        const origId = sentReq.id as string

        controller.abort()
        let caught: unknown
        try { await promise } catch (e) { caught = e }
        expect((caught as DOMException).name).toBe('AbortError')

        // Now the server (oblivious to cancellation) ships a late reply.
        // It must NOT trigger a double-settle (no unhandled rejection from
        // the test, no resolve thrown — the promise is already settled).
        // Because we delete the entry from _pendingCalls on abort, the
        // late reply with the same id will be routed to subscribers
        // instead.
        ws.serverPush({ id: origId, result: '[]' })

        // Subscribers should have received the late reply (it's routed
        // because the id is no longer in _pendingCalls).
        const lateReply = events.find((e) => e.id === origId)
        expect(lateReply).toBeDefined()
        expect(lateReply!.result).toBe('[]')

        client.closeAll()
    })

    it('successful resolution removes the abort listener (no leak)', async () => {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        const controller = new AbortController()
        let listenerCount = 0
        const orig = controller.signal.addEventListener.bind(controller.signal)
        controller.signal.addEventListener = ((type: string, listener: any, opts?: any) => {
            listenerCount++
            return orig(type as any, listener, opts)
        }) as typeof controller.signal.addEventListener

        const promise = client.call('agent.get', {}, { signal: controller.signal })
        await flushMicrotasks()
        const ws = FakeWebSocket.last!
        const sentReq = JSON.parse(ws.sent[0]) as AnyMsg
        const origId = sentReq.id as string

        // Resolve the call.
        ws.serverPush({ id: origId, result: { did: 'did:test' } })
        const result = await promise
        expect((result as { did: string }).did).toBe('did:test')

        // After resolution, firing the controller should not trigger any
        // abort handler (the listener was removed in cleanup()). The
        // simplest detection: confirm the abort doesn't cause a stray
        // request.cancel message on the wire — i.e. no new send beyond
        // the original.
        const sendCountBefore = ws.sent.length
        controller.abort()
        // Give microtasks a chance.
        await flushMicrotasks()
        expect(ws.sent.length).toBe(sendCountBefore)
        // Two listeners were registered in total: one in _readyOrAbort
        // (connection phase) and one in call() (in-flight phase). Both
        // were removed via cleanup — the "no extra send" check above
        // confirms no stale handler remains.
        expect(listenerCount).toBe(2)

        client.closeAll()
    })

    it('passes through normal call signatures (no options) without regression', async () => {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        const promise = client.call('agent.get', { foo: 'bar' })
        await flushMicrotasks()
        const ws = FakeWebSocket.last!
        const sentReq = JSON.parse(ws.sent[0]) as AnyMsg
        expect(sentReq.type).toBe('agent.get')
        expect(sentReq.params).toEqual({ foo: 'bar' })
        ws.serverPush({ id: sentReq.id, result: { did: 'd' } })
        const result = await promise
        expect((result as { did: string }).did).toBe('d')

        client.closeAll()
    })

    beforeEach(() => {
        FakeWebSocket.last = null
    })
})

/**
 * A WebSocket whose connect outcome is scripted per attempt, in order:
 * - 'refuse': fires error then close without ever opening. This is what
 *   the platform WebSocket does on ECONNREFUSED, e.g. when a client races
 *   the executor between its "API server starting" log line and the bind.
 * - 'open': opens normally.
 * - 'hang': stays CONNECTING forever (a black-holed SYN).
 * Once the script runs out, every further attempt refuses.
 */
type ConnectOutcome = 'refuse' | 'open' | 'hang'

class ScriptedWebSocket {
    static script: ConnectOutcome[] = []
    static instances: ScriptedWebSocket[] = []

    readyState = 0 /* CONNECTING */
    sent: string[] = []
    onopen: ((ev?: unknown) => void) | null = null
    onmessage: ((ev: { data: string }) => void) | null = null
    onerror: ((ev: unknown) => void) | null = null
    onclose: ((ev?: unknown) => void) | null = null

    constructor(public url: string) {
        ScriptedWebSocket.instances.push(this)
        const outcome = ScriptedWebSocket.script.shift() ?? 'refuse'
        if (outcome === 'hang') return
        setTimeout(() => {
            if (this.readyState !== 0) return // closed by the client meanwhile
            if (outcome === 'open') {
                this.readyState = 1
                this.onopen?.()
            } else {
                this.readyState = 3
                this.onerror?.({ type: 'error' })
                this.onclose?.()
            }
        }, 0)
    }

    send(data: string) {
        this.sent.push(data)
    }

    close() {
        if (this.readyState === 3) return
        this.readyState = 3
        this.onclose?.()
    }

    serverPush(msg: AnyMsg) {
        this.onmessage?.({ data: JSON.stringify(msg) })
    }
}

/** Track a promise's outcome without awaiting it, so a hang is observable. */
function track<T>(p: Promise<T>) {
    const state: { status: 'pending' | 'resolved' | 'rejected'; value?: T; error?: unknown } = { status: 'pending' }
    p.then(
        (value) => { state.status = 'resolved'; state.value = value },
        (error) => { state.status = 'rejected'; state.error = error },
    )
    return state
}

describe('ApiClient first-connect failures', () => {
    let errorSpy: jest.SpyInstance

    beforeEach(() => {
        jest.useFakeTimers()
        ScriptedWebSocket.script = []
        ScriptedWebSocket.instances = []
        // The client logs every WebSocket error event; keep the output clean.
        errorSpy = jest.spyOn(console, 'error').mockImplementation(() => {})
    })

    afterEach(() => {
        jest.useRealTimers()
        errorSpy.mockRestore()
    })

    function makeClient() {
        return new ApiClient(
            'http://127.0.0.1:1234',
            'secret-token',
            ScriptedWebSocket as unknown as new (url: string) => WebSocket,
        )
    }

    it('a call made while the first connect is refused is sent once a retry connects', async () => {
        // The CI flake (jobs 32532, 32154): the first connect is refused, and
        // the executor is listening a moment later.
        ScriptedWebSocket.script = ['refuse', 'open']
        const client = makeClient()
        const call = track(client.call<{ did: string }>('agent.generate', { passphrase: 'p' }))

        await jest.advanceTimersByTimeAsync(0)
        expect(ScriptedWebSocket.instances).toHaveLength(1)
        expect(call.status).toBe('pending')

        // First retry happens after the initial reconnect delay (500 ms).
        await jest.advanceTimersByTimeAsync(500)
        await jest.advanceTimersByTimeAsync(10) // the retry socket's open event
        expect(ScriptedWebSocket.instances).toHaveLength(2)
        const ws = ScriptedWebSocket.instances[1]
        expect(ws.sent).toHaveLength(1)
        const req = JSON.parse(ws.sent[0]) as AnyMsg
        expect(req.type).toBe('agent.generate')

        ws.serverPush({ id: req.id, result: { did: 'did:key:z' } })
        await jest.advanceTimersByTimeAsync(0)
        expect(call.status).toBe('resolved')
        expect(call.value).toEqual({ did: 'did:key:z' })

        client.closeAll()
    })

    it('a call rejects with a clear error when no connect succeeds within the connect budget', async () => {
        ScriptedWebSocket.script = [] // every attempt refuses
        const client = makeClient()
        const call = track(client.call('agent.status'))

        await jest.advanceTimersByTimeAsync(29_000)
        expect(call.status).toBe('pending')
        expect(ScriptedWebSocket.instances.length).toBeGreaterThan(1) // it retried

        await jest.advanceTimersByTimeAsync(1_000)
        expect(call.status).toBe('rejected')
        const err = call.error as RpcError
        expect(err).toBeInstanceOf(RpcError)
        expect(err.status).toBe(503)
        expect(err.body).toMatch(/could not connect to ws:\/\/127\.0\.0\.1:1234\/api\/v1\/ws within 30000ms/)
        // The token is a credential; it must not leak into error messages.
        expect(err.message).not.toContain('secret-token')

        // No retry is left running after the rejection.
        const attempts = ScriptedWebSocket.instances.length
        await jest.advanceTimersByTimeAsync(120_000)
        expect(ScriptedWebSocket.instances.length).toBe(attempts)

        client.closeAll()
    })

    it('a call rejects when the socket never leaves CONNECTING', async () => {
        ScriptedWebSocket.script = ['hang']
        const client = makeClient()
        const call = track(client.call('agent.status'))

        await jest.advanceTimersByTimeAsync(30_000)
        expect(call.status).toBe('rejected')
        expect((call.error as RpcError).status).toBe(503)
        // The stuck socket is closed rather than leaked.
        expect(ScriptedWebSocket.instances[0].readyState).toBe(3)

        client.closeAll()
    })

    it('the client recovers on the next call after a connect-budget rejection', async () => {
        ScriptedWebSocket.script = []
        const client = makeClient()
        const first = track(client.call('agent.status'))
        await jest.advanceTimersByTimeAsync(30_000)
        expect(first.status).toBe('rejected')

        ScriptedWebSocket.script = ['open']
        const second = track(client.call<string>('agent.status'))
        await jest.advanceTimersByTimeAsync(0)
        const ws = ScriptedWebSocket.instances[ScriptedWebSocket.instances.length - 1]
        const req = JSON.parse(ws.sent[0]) as AnyMsg
        ws.serverPush({ id: req.id, result: 'ok' })
        await jest.advanceTimersByTimeAsync(0)
        expect(second.status).toBe('resolved')

        client.closeAll()
    })

    it('closeAll() rejects a call that is still waiting for the first connect', async () => {
        ScriptedWebSocket.script = ['hang']
        const client = makeClient()
        const call = track(client.call('agent.status'))
        await jest.advanceTimersByTimeAsync(0)
        expect(call.status).toBe('pending')

        client.closeAll()
        await jest.advanceTimersByTimeAsync(0)
        expect(call.status).toBe('rejected')
        expect((call.error as RpcError).body).toBe('Client closed')
    })

    it('unsubscribing while a call waits for the connect does not close the socket under it', async () => {
        ScriptedWebSocket.script = ['refuse', 'open']
        const client = makeClient()
        const unsubscribe = client.subscribe(() => {})
        const call = track(client.call<string>('agent.status'))
        await jest.advanceTimersByTimeAsync(0)

        // The waiting call is not in _pendingCalls yet; it still counts.
        unsubscribe()
        await jest.advanceTimersByTimeAsync(510)
        expect(call.status).toBe('pending')
        const ws = ScriptedWebSocket.instances[ScriptedWebSocket.instances.length - 1]
        expect(ws.sent).toHaveLength(1)
        const req = JSON.parse(ws.sent[0]) as AnyMsg
        ws.serverPush({ id: req.id, result: 'ok' })
        await jest.advanceTimersByTimeAsync(0)
        expect(call.status).toBe('resolved')

        client.closeAll()
    })

    it('subscribers keep reconnecting after a connect-budget rejection', async () => {
        ScriptedWebSocket.script = []
        const client = makeClient()
        client.subscribe(() => {})
        await jest.advanceTimersByTimeAsync(30_000)
        const attempts = ScriptedWebSocket.instances.length

        // An event subscriber has no call to fail. It must not be stranded
        // with a dead socket; it keeps retrying at the capped backoff.
        await jest.advanceTimersByTimeAsync(60_000)
        expect(ScriptedWebSocket.instances.length).toBeGreaterThan(attempts)

        client.closeAll()
    })
})
