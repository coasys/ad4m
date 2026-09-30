import { ApiClient } from "./apiClient"

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

describe('ApiClient cancel-ack ids (L6)', () => {
    async function flushAll() {
        for (let i = 0; i < 20; i++) await Promise.resolve()
    }

    async function openClient() {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        client.connect()
        jest.advanceTimersByTime(0)
        await flushAll()
        return { client, ws: FakeWebSocket.last! }
    }

    async function cancelCalls(client: ApiClient, count: number) {
        const controllers = Array.from({ length: count }, () => new AbortController())
        const calls = controllers.map((c) => client.call('perspective.querySparql', {}, { signal: c.signal }).catch(() => {}))
        await flushAll()
        controllers.forEach((c) => c.abort())
        await Promise.all(calls)
    }

    beforeEach(() => jest.useFakeTimers())
    afterEach(() => jest.useRealTimers())

    it('10,000 cancelled calls start no timer', async () => {
        const { client, ws } = await openClient()
        const baseline = jest.getTimerCount() // the ping interval

        await cancelCalls(client, 10_000)

        expect(ws.sent.filter((m) => JSON.parse(m).type === 'request.cancel')).toHaveLength(10_000)
        expect(jest.getTimerCount()).toBe(baseline)
        client.closeAll()
    })

    it('swallows the ack of a cancel and forgets its id', async () => {
        const { client, ws } = await openClient()
        const events: AnyMsg[] = []
        client.subscribe((m) => events.push(m as AnyMsg))

        await cancelCalls(client, 1)
        const cancel = lastSent(ws)!
        expect(cancel.type).toBe('request.cancel')
        ws.serverPush({ id: cancel.id, result: true })

        expect(events).toHaveLength(0)
        expect((client as any)._ignoredResponseIds.size).toBe(0)
        client.closeAll()
    })

    it('forgets unacked cancel ids when the socket closes', async () => {
        const { client, ws } = await openClient()
        await cancelCalls(client, 3)
        expect((client as any)._ignoredResponseIds.size).toBe(3)
        ws.close()
        expect((client as any)._ignoredResponseIds.size).toBe(0)
        client.closeAll()
    })
})

describe('ApiClient event dispatch', () => {
    beforeEach(() => {
        FakeWebSocket.last = null
    })

    it('keeps delivering an event to later subscribers when one subscriber throws', async () => {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        const errorSpy = jest.spyOn(console, 'error').mockImplementation(() => {})
        const received: string[] = []
        client.subscribe(() => { received.push('first') })
        client.subscribe(() => { throw new Error('boom') })
        client.subscribe(() => { received.push('third') })
        await flushMicrotasks()

        FakeWebSocket.last!.serverPush({ type: 'perspective-added', perspective: { uuid: 'u' } })

        expect(received).toEqual(['first', 'third'])
        expect(errorSpy).toHaveBeenCalled()
        errorSpy.mockRestore()
        client.closeAll()
    })

    it('logs a rejection from an async subscriber instead of leaving it unhandled', async () => {
        const client = new ApiClient(
            'http://localhost:1234',
            undefined,
            FakeWebSocket as unknown as new (url: string) => WebSocket,
        )
        const errorSpy = jest.spyOn(console, 'error').mockImplementation(() => {})
        const unhandled = jest.fn()
        process.on('unhandledRejection', unhandled)
        const received: string[] = []
        client.subscribe(async () => { throw new Error('async boom') })
        client.subscribe(() => { received.push('second') })
        await flushMicrotasks()

        FakeWebSocket.last!.serverPush({ type: 'perspective-added', perspective: { uuid: 'u' } })
        await new Promise((r) => setTimeout(r, 0))

        expect(received).toEqual(['second'])
        expect(errorSpy).toHaveBeenCalledWith('Error in WebSocket event callback:', expect.objectContaining({ message: 'async boom' }))
        expect(unhandled).not.toHaveBeenCalled()
        process.off('unhandledRejection', unhandled)
        errorSpy.mockRestore()
        client.closeAll()
    })
})

describe('ApiClient with a socket that reports its close late', () => {
    it('a late close of the previous socket does not fail calls sent on the next one', async () => {
        const sockets: FakeWebSocket[] = []
        class LateCloseSocket extends FakeWebSocket {
            constructor(url: string) { super(url); sockets.push(this) }
            // Browsers and `ws` fire onclose some time after close().
            close() {
                this.readyState = FakeWebSocket.CLOSING
                setTimeout(() => { this.readyState = FakeWebSocket.CLOSED; this.onclose?.() }, 5)
            }
        }
        const client = new ApiClient('http://localhost:1234', undefined, LateCloseSocket as unknown as new (url: string) => WebSocket)
        const release = client.subscribe(() => {})
        await new Promise((r) => setTimeout(r, 0))

        release() // the last subscriber leaves: the socket closes, and reports it later
        const call = client.call('agent.get', {})
        await new Promise((r) => setTimeout(r, 1)) // the call is sent on a new socket
        const sent = JSON.parse(sockets[1].sent[0])
        await new Promise((r) => setTimeout(r, 10)) // the old socket's onclose fires
        sockets[1].serverPush({ id: sent.id, result: { did: 'did:test' } })

        await expect(call).resolves.toEqual({ did: 'did:test' })
        client.closeAll()
    })
})
