import { ApiClient, LONG_TIMEOUT_MS } from "./apiClient"
import { AIClient } from "./ai/AIClient"
import { AgentClient } from "./agent/AgentClient"
import { LanguageClient } from "./language/LanguageClient"
import { NeighbourhoodClient } from "./neighbourhood/NeighbourhoodClient"
import { PerspectiveClient } from "./perspectives/PerspectiveClient"
import { RuntimeClient } from "./runtime/RuntimeClient"

type AnyMsg = Record<string, any>

/** A socket the tests drive by hand: open(), drop(), reply(). */
class TestSocket {
    static instances: TestSocket[] = []
    /** Open on the next microtask, like a reachable server. */
    static autoOpen = false
    /** Fire onclose some time after close(), like browsers and `ws`. */
    static asyncClose = false

    readyState = 0
    sent: AnyMsg[] = []
    onopen: (() => void) | null = null
    onmessage: ((ev: { data: string }) => void) | null = null
    onerror: ((ev: unknown) => void) | null = null
    onclose: (() => void) | null = null

    constructor(public url: string) {
        TestSocket.instances.push(this)
        if (TestSocket.autoOpen) queueMicrotask(() => this.open())
    }
    send(data: string) { this.sent.push(JSON.parse(data)) }
    close() {
        if (TestSocket.asyncClose) {
            this.readyState = 2
            setTimeout(() => this.drop(), 5)
        } else {
            this.drop()
        }
    }
    open() { this.readyState = 1; this.onopen?.() }
    drop() { this.readyState = 3; this.onclose?.() }
    reply(msg: AnyMsg) { this.onmessage?.({ data: JSON.stringify(msg) }) }
}

const url = 'http://localhost:1234'
const socket = (i: number) => TestSocket.instances[i]
const flush = () => new Promise((r) => setTimeout(r, 0))
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms))

let client: ApiClient
beforeEach(() => {
    TestSocket.instances = []
    TestSocket.autoOpen = true
    TestSocket.asyncClose = false
    client = new ApiClient(url, undefined, TestSocket as unknown as new (url: string) => WebSocket)
})
afterEach(() => client.closeAll())

/** A call whose rejection counts as handled until the test awaits it. */
function call<T = unknown>(type: string, params?: Record<string, unknown>, options?: object): Promise<T> {
    const promise = client.call<T>(type, params, options)
    promise.catch(() => {})
    return promise
}

describe('ApiClient calls', () => {
    it('sends the call and resolves with the reply', async () => {
        const promise = call('agent.get', { foo: 'bar' })
        await flush()
        const req = socket(0).sent[0]
        expect(req).toMatchObject({ type: 'agent.get', params: { foo: 'bar' } })
        socket(0).reply({ id: req.id, result: { did: 'd' } })
        await expect(promise).resolves.toEqual({ did: 'd' })
    })

    it('rejects with AbortError before opening a socket if the signal is already aborted', async () => {
        const controller = new AbortController()
        controller.abort()
        await expect(call('agent.get', {}, { signal: controller.signal })).rejects.toMatchObject({ name: 'AbortError' })
        expect(TestSocket.instances).toHaveLength(0)
    })

    it('sends request.cancel and rejects with AbortError when the signal fires in flight', async () => {
        const controller = new AbortController()
        const promise = call('perspective.querySparql', { uuid: 'u' }, { signal: controller.signal })
        await flush()
        const req = socket(0).sent[0]

        controller.abort()
        expect(socket(0).sent[1]).toMatchObject({ type: 'request.cancel', params: { targetId: req.id } })
        await expect(promise).rejects.toMatchObject({ name: 'AbortError' })
    })

    it('drops a late reply to an aborted call and the cancel ack', async () => {
        const events: AnyMsg[] = []
        client.subscribe((m) => events.push(m as AnyMsg))
        const controller = new AbortController()
        const promise = call('perspective.querySparql', { uuid: 'u' }, { signal: controller.signal })
        await flush()
        controller.abort()
        await expect(promise).rejects.toMatchObject({ name: 'AbortError' })

        socket(0).reply({ id: socket(0).sent[0].id, result: '[]' })
        socket(0).reply({ id: socket(0).sent[1].id, result: { cancelled: true } })
        expect(events).toEqual([])
    })

    it('removes the abort listener once the call settles', async () => {
        const controller = new AbortController()
        const remove = jest.spyOn(controller.signal, 'removeEventListener')
        const promise = call('agent.get', {}, { signal: controller.signal })
        await flush()
        socket(0).reply({ id: socket(0).sent[0].id, result: 1 })
        await promise

        expect(remove).toHaveBeenCalledTimes(1)
        controller.abort()
        expect(socket(0).sent).toHaveLength(1)
    })
})

describe('ApiClient connect-phase failures', () => {
    beforeEach(() => { TestSocket.autoOpen = false })

    it('rejects with 503 when the socket closes before it opens', async () => {
        const promise = call('agent.get', {}, { timeoutMs: 1_000 })
        socket(0).drop()
        await expect(promise).rejects.toMatchObject({ name: 'RpcError', status: 503 })
    })

    it('rejects with 408 when the socket never opens within the timeout', async () => {
        const started = Date.now()
        await expect(call('agent.get', {}, { timeoutMs: 50 })).rejects.toMatchObject({ name: 'RpcError', status: 408 })
        expect(Date.now() - started).toBeLessThan(1_000)
    })

    it('rejects callers waiting to connect when the client is closed', async () => {
        const promise = call('agent.get', {}, { timeoutMs: 1_000 })
        client.closeAll()
        await expect(promise).rejects.toMatchObject({ name: 'RpcError', status: 503 })
    })

    it('a late close of the previous socket does not fail the next connection', async () => {
        TestSocket.asyncClose = true
        const unsubscribe = client.subscribe(() => {})
        socket(0).open()
        unsubscribe()
        client.subscribe(() => {})
        const ready = client.waitForSubscription()
        await sleep(10)
        socket(1).open()
        await expect(ready).resolves.toBeUndefined()
    })

    it('keeps the socket for a call still connecting when the last subscriber leaves', async () => {
        TestSocket.asyncClose = true
        const unsubscribe = client.subscribe(() => {})
        const promise = call<number>('agent.get', {}, { timeoutMs: 1_000 })
        unsubscribe()
        socket(0).open()
        socket(0).reply({ id: socket(0).sent[0].id, result: 1 })
        await expect(promise).resolves.toBe(1)
    })
})

describe('ApiClient retries idempotent reads once after a reconnect', () => {
    beforeEach(() => {
        TestSocket.autoOpen = false
        jest.useFakeTimers()
    })
    afterEach(() => jest.useRealTimers())

    /** Drop socket `i` and let the reconnect timer open the next one. */
    function reconnect(i: number) {
        socket(i).drop()
        jest.advanceTimersByTime(500)
        socket(i + 1).open()
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
        expect(TestSocket.instances).toHaveLength(1)
    })

    it('rejects a read that drops again after its retry', async () => {
        const read = call('agent.get')
        socket(0).open()
        reconnect(0)
        socket(1).drop()
        await expect(read).rejects.toMatchObject({ name: 'RpcError', status: 503 })
    })
})

describe('ApiClient.onReconnect', () => {
    beforeEach(() => { TestSocket.autoOpen = false })

    it('fires on reconnect only, not after closeAll or once unsubscribed', () => {
        const reconnected = jest.fn()
        const unsubscribe = client.onReconnect(reconnected)
        client.waitForSubscription()
        socket(0).open()
        expect(reconnected).not.toHaveBeenCalled()

        socket(0).drop()
        client.waitForSubscription()
        socket(1).open()
        expect(reconnected).toHaveBeenCalledTimes(1)

        unsubscribe()
        socket(1).drop()
        client.waitForSubscription()
        socket(2).open()
        expect(reconnected).toHaveBeenCalledTimes(1)

        // A reused client's first open after closeAll() is not a reconnect.
        client.closeAll()
        client.onReconnect(reconnected)
        client.waitForSubscription()
        socket(3).open()
        expect(reconnected).toHaveBeenCalledTimes(1)
    })
})

describe('long calls', () => {
    beforeEach(() => jest.useFakeTimers())
    afterEach(() => jest.useRealTimers())

    const calls: [string, (o?: object) => Promise<unknown>][] = [
        ['ai.prompt', (o) => new AIClient(url, undefined, false, client).prompt('t', 'p', o)],
        ['ai.embed', (o) => new AIClient(url, undefined, false, client).embed('m', 'x', o)],
        ['ai.addModel', (o) => new AIClient(url, undefined, false, client).addModel({ name: 'm', modelType: 'LLM' } as any, o)],
        ['agent.generate', (o) => new AgentClient(url, undefined, false, client).generate('pw', o)],
        ['agent.unlock', (o) => new AgentClient(url, undefined, false, client).unlock('pw', true, o)],
        ['language.publish', (o) => new LanguageClient(url, undefined, client).publish('/p', { name: 'l' } as any, o)],
        ['language.applyTemplate', (o) => new LanguageClient(url, undefined, client).applyTemplateAndPublish('h', '{}', o)],
        ['neighbourhood.publish', (o) => new NeighbourhoodClient(url, undefined, client).publishFromPerspective('u', 'l', { links: [] } as any, o)],
        ['neighbourhood.join', (o) => new NeighbourhoodClient(url, undefined, client).joinFromUrl('n://x', o)],
        ['runtime.restartHolochain', (o) => new RuntimeClient(url, undefined, false, client).restartHolochain(o)],
        ['perspective.runInterpretation', (o) => new PerspectiveClient(url, undefined, false, client).runInterpretation('u', [], 'b', undefined, undefined, undefined, undefined, o)],
        ['perspective.runInterpretationWithHarness', (o) => new PerspectiveClient(url, undefined, false, client).runInterpretationWithHarness('u', [], 'b', 1, undefined, undefined, undefined, undefined, undefined, o)],
    ]
    const lastSent = () => TestSocket.instances[TestSocket.instances.length - 1].sent.at(-1)!

    it.each(calls)('%s waits LONG_TIMEOUT_MS by default', async (type, run) => {
        let outcome: unknown = 'pending'
        run().then(() => { outcome = 'resolved' }, (e) => { outcome = e })
        await jest.advanceTimersByTimeAsync(1)
        expect(lastSent().type).toBe(type)

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
        const sent = lastSent()
        controller.abort()
        await expect(aborted).rejects.toMatchObject({ name: 'AbortError' })
        expect(lastSent()).toMatchObject({ type: 'request.cancel', params: { targetId: sent.id } })
    })
})
