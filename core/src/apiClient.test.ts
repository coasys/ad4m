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
        // No socket should have been opened.
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

    it('drops a late server reply to an aborted call', async () => {
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
        const origId = (JSON.parse(ws.sent[0]) as AnyMsg).id as string

        controller.abort()
        await expect(promise).rejects.toMatchObject({ name: 'AbortError' })

        // Neither the late reply nor the cancel ack reaches subscribers.
        ws.serverPush({ id: origId, result: '[]' })
        ws.serverPush({ id: 'cancel-ack', result: { cancelled: true } })
        expect(events).toEqual([])

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
        expect(listenerCount).toBe(1)

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

describe('ApiClient connect-phase failures', () => {
    // A socket that never opens on its own; tests drive close explicitly.
    class NeverOpenWebSocket {
        static instances: NeverOpenWebSocket[] = []
        readyState = 0
        onopen: ((ev?: unknown) => void) | null = null
        onmessage: ((ev: { data: string }) => void) | null = null
        onerror: ((ev: unknown) => void) | null = null
        onclose: ((ev?: unknown) => void) | null = null
        constructor(public url: string) { NeverOpenWebSocket.instances.push(this) }
        send(_data: string) {}
        close() { this.readyState = 3; this.onclose?.() }
        /** Simulate the server refusing the connection. */
        refuse() { this.readyState = 3; this.onclose?.() }
    }

    beforeEach(() => { NeverOpenWebSocket.instances = [] })

    function makeClient() {
        return new ApiClient(
            'http://localhost:1234',
            undefined,
            NeverOpenWebSocket as unknown as new (url: string) => WebSocket,
        )
    }

    it('rejects with 503 when the socket closes before it opens', async () => {
        const client = makeClient()
        const promise = client.call('agent.get', {}, { timeoutMs: 1_000 })
        await flushMicrotasks()
        NeverOpenWebSocket.instances[0].refuse()

        await expect(promise).rejects.toMatchObject({ name: 'RpcError', status: 503 })
        client.closeAll()
    })

    it('rejects with 408 when the socket never opens within the timeout', async () => {
        const client = makeClient()
        const started = Date.now()
        await expect(client.call('agent.get', {}, { timeoutMs: 50 }))
            .rejects.toMatchObject({ name: 'RpcError', status: 408 })
        expect(Date.now() - started).toBeLessThan(1_000)
        client.closeAll()
    })

    it('rejects callers waiting to connect when the client is closed', async () => {
        const client = makeClient()
        const promise = client.call('agent.get', {}, { timeoutMs: 1_000 })
        await flushMicrotasks()
        client.closeAll()

        await expect(promise).rejects.toMatchObject({ name: 'RpcError', status: 503 })
    })
})

describe('ApiClient with a socket that closes asynchronously', () => {
    // Browsers and `ws` fire onclose some time after close().
    class AsyncCloseWebSocket {
        static instances: AsyncCloseWebSocket[] = []
        readyState = 0
        sent: { id: string }[] = []
        onopen: ((ev?: unknown) => void) | null = null
        onmessage: ((ev: { data: string }) => void) | null = null
        onerror: ((ev: unknown) => void) | null = null
        onclose: ((ev?: unknown) => void) | null = null
        constructor(public url: string) { AsyncCloseWebSocket.instances.push(this) }
        send(data: string) { this.sent.push(JSON.parse(data)) }
        close() { this.readyState = 2; setTimeout(() => { this.readyState = 3; this.onclose?.() }, 5) }
        open() { this.readyState = 1; this.onopen?.() }
    }
    const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms))

    beforeEach(() => { AsyncCloseWebSocket.instances = [] })

    function makeClient() {
        return new ApiClient(
            'http://localhost:1234',
            undefined,
            AsyncCloseWebSocket as unknown as new (url: string) => WebSocket,
        )
    }

    it('a late close of the previous socket does not fail the next connection', async () => {
        const client = makeClient()
        const unsubscribe = client.subscribe(() => {})
        AsyncCloseWebSocket.instances[0].open()
        unsubscribe()
        client.subscribe(() => {})
        const ready = client.waitForSubscription()
        await sleep(10)
        AsyncCloseWebSocket.instances[1].open()

        await expect(ready).resolves.toBeUndefined()
        client.closeAll()
    })

    it('keeps the socket for a call still connecting when the last subscriber leaves', async () => {
        const client = makeClient()
        const unsubscribe = client.subscribe(() => {})
        const promise = client.call<number>('agent.get', {}, { timeoutMs: 1_000 })
        await flushMicrotasks()
        unsubscribe()
        const ws = AsyncCloseWebSocket.instances[0]
        ws.open()
        await flushMicrotasks()
        ws.onmessage?.({ data: JSON.stringify({ id: ws.sent[0].id, result: 1 }) })

        await expect(promise).resolves.toBe(1)
        client.closeAll()
    })
})
