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
