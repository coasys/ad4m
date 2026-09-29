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

describe('ApiClient retries idempotent reads once after a reconnect', () => {
    class DropWebSocket {
        static instances: DropWebSocket[] = []
        readyState = 0
        sent: AnyMsg[] = []
        onopen: ((ev?: unknown) => void) | null = null
        onmessage: ((ev: { data: string }) => void) | null = null
        onerror: ((ev: unknown) => void) | null = null
        onclose: ((ev?: unknown) => void) | null = null
        constructor(public url: string) { DropWebSocket.instances.push(this) }
        send(data: string) { this.sent.push(JSON.parse(data) as AnyMsg) }
        close() { this.drop() }
        open() { this.readyState = 1; this.onopen?.() }
        drop() { this.readyState = 3; this.onclose?.() }
        reply(msg: AnyMsg) { this.onmessage?.({ data: JSON.stringify(msg) }) }
    }
    const socket = (i: number) => DropWebSocket.instances[i]

    let client: ApiClient
    beforeEach(() => {
        jest.useFakeTimers()
        DropWebSocket.instances = []
        client = new ApiClient('http://localhost:1234', undefined, DropWebSocket as unknown as new (url: string) => WebSocket)
    })
    afterEach(() => {
        client.closeAll()
        jest.useRealTimers()
    })

    /** A call whose rejection is handled until the test awaits it. */
    function call(type: string, params?: Record<string, unknown>) {
        const promise = client.call(type, params)
        promise.catch(() => {})
        return promise
    }

    /** Drop the open socket and let the reconnect timer open the next one. */
    function reconnect(from: number) {
        socket(from).drop()
        jest.advanceTimersByTime(500)
        socket(from + 1).open()
    }

    it('resends a read that was in flight and resolves it', async () => {
        const read = call('perspective.all')
        socket(0).open()
        reconnect(0)

        const resent = socket(1).sent[0]
        expect(resent).toEqual(socket(0).sent[0])
        socket(1).reply({ id: resent.id, result: ['p'] })
        await expect(read).resolves.toEqual(['p'])
    })

    it('rejects a write that was in flight with 503', async () => {
        const write = call('perspective.addLink', { uuid: 'u' })
        socket(0).open()
        socket(0).drop()

        await expect(write).rejects.toMatchObject({ name: 'RpcError', status: 503 })
        expect(DropWebSocket.instances).toHaveLength(1)
    })

    it('rejects a read that drops again after its retry', async () => {
        const read = call('agent.get')
        socket(0).open()
        reconnect(0)
        socket(1).drop()

        await expect(read).rejects.toMatchObject({ name: 'RpcError', status: 503 })
    })
})

describe('long calls', () => {
    const { AIClient } = require('./ai/AIClient')
    const { AgentClient } = require('./agent/AgentClient')
    const { LanguageClient } = require('./language/LanguageClient')
    const { NeighbourhoodClient } = require('./neighbourhood/NeighbourhoodClient')
    const { PerspectiveClient } = require('./perspectives/PerspectiveClient')
    const { RuntimeClient } = require('./runtime/RuntimeClient')
    const { LONG_TIMEOUT_MS } = require('./apiClient')

    class OpenWebSocket {
        static last: OpenWebSocket
        readyState = 0
        sent: AnyMsg[] = []
        onopen: ((ev?: unknown) => void) | null = null
        onmessage: ((ev: { data: string }) => void) | null = null
        onerror: ((ev: unknown) => void) | null = null
        onclose: ((ev?: unknown) => void) | null = null
        constructor(public url: string) {
            OpenWebSocket.last = this
            queueMicrotask(() => { this.readyState = 1; this.onopen?.() })
        }
        send(data: string) { this.sent.push(JSON.parse(data) as AnyMsg) }
        close() { this.readyState = 3; this.onclose?.() }
    }

    const url = 'http://localhost:1234'
    let api: ApiClient
    beforeEach(() => {
        jest.useFakeTimers()
        api = new ApiClient(url, undefined, OpenWebSocket as unknown as new (url: string) => WebSocket)
    })
    afterEach(() => {
        api.closeAll()
        jest.useRealTimers()
    })

    const calls: [string, (o?: object) => Promise<unknown>][] = [
        ['ai.prompt', (o) => new AIClient(url, undefined, false, api).prompt('t', 'p', o)],
        ['ai.embed', (o) => new AIClient(url, undefined, false, api).embed('m', 'x', o)],
        ['ai.addModel', (o) => new AIClient(url, undefined, false, api).addModel({ name: 'm', modelType: 'LLM' }, o)],
        ['agent.generate', (o) => new AgentClient(url, undefined, false, api).generate('pw', o)],
        ['agent.unlock', (o) => new AgentClient(url, undefined, false, api).unlock('pw', true, o)],
        ['language.publish', (o) => new LanguageClient(url, undefined, api).publish('/p', { name: 'l' }, o)],
        ['language.applyTemplate', (o) => new LanguageClient(url, undefined, api).applyTemplateAndPublish('h', '{}', o)],
        ['neighbourhood.publish', (o) => new NeighbourhoodClient(url, undefined, api).publishFromPerspective('u', 'l', { links: [] }, o)],
        ['neighbourhood.join', (o) => new NeighbourhoodClient(url, undefined, api).joinFromUrl('n://x', o)],
        ['runtime.restartHolochain', (o) => new RuntimeClient(url, undefined, false, api).restartHolochain(o)],
        ['perspective.runInterpretation', (o) => new PerspectiveClient(url, undefined, false, api).runInterpretation('u', [], 'b', undefined, undefined, undefined, undefined, o)],
        ['perspective.runInterpretationWithHarness', (o) => new PerspectiveClient(url, undefined, false, api).runInterpretationWithHarness('u', [], 'b', 1, undefined, undefined, undefined, undefined, undefined, o)],
    ]

    it.each(calls)('%s waits LONG_TIMEOUT_MS by default', async (type, run) => {
        let outcome: unknown = 'pending'
        run().then(() => { outcome = 'resolved' }, (e) => { outcome = e })
        await jest.advanceTimersByTimeAsync(1)
        expect(OpenWebSocket.last.sent.map((m) => m.type)).toContain(type)

        await jest.advanceTimersByTimeAsync(31_000)
        expect(outcome).toBe('pending')
        await jest.advanceTimersByTimeAsync(LONG_TIMEOUT_MS)
        expect(outcome).toMatchObject({ name: 'RpcError', status: 408 })
    })

    it.each(calls)('%s takes timeoutMs and signal', async (_type, run) => {
        const timedOut = run({ timeoutMs: 100 })
        timedOut.catch(() => {})
        await jest.advanceTimersByTimeAsync(100)
        await expect(timedOut).rejects.toMatchObject({ status: 408 })

        const controller = new AbortController()
        const aborted = run({ signal: controller.signal })
        aborted.catch(() => {})
        await jest.advanceTimersByTimeAsync(1)
        const sent = OpenWebSocket.last.sent.at(-1)!
        controller.abort()
        await expect(aborted).rejects.toMatchObject({ name: 'AbortError' })
        expect(OpenWebSocket.last.sent.at(-1)).toMatchObject({ type: 'request.cancel', params: { targetId: sent.id } })
    })
})
