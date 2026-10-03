import { ApiClient, LONG_TIMEOUT_MS, RpcError } from "./apiClient"
import { AIClient } from "./ai/AIClient"
import { AgentClient } from "./agent/AgentClient"
import { LanguageClient } from "./language/LanguageClient"
import { NeighbourhoodClient } from "./neighbourhood/NeighbourhoodClient"
import { PerspectiveClient } from "./perspectives/PerspectiveClient"
import { PerspectiveHandle } from "./perspectives/PerspectiveHandle"
import { PerspectiveProxy } from "./perspectives/PerspectiveProxy"
import { RuntimeClient } from "./runtime/RuntimeClient"
import { AiInference_1_0_0 } from "./generated/services/ai.inference"
import { AiModels_1_0_0 } from "./generated/services/ai.models"
import { HolochainConductor_1_0_0 } from "./generated/services/holochain.conductor"

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
/** Starts a connection the way an event subscriber does. */
const open = () => { client.on('agent-updated', () => {}) }

let client: ApiClient
beforeEach(() => {
    TestSocket.instances = []
    TestSocket.autoOpen = true
    TestSocket.asyncClose = false
    client = new ApiClient(url, undefined, TestSocket as unknown as new (url: string) => WebSocket)
})
afterEach(() => client.closeAll())

/** Transport tests send arbitrary method names, outside the typed table. */
type UntypedCall = (type: string, params?: unknown, options?: object) => Promise<unknown>

/** A call whose rejection counts as handled until the test awaits it. */
function call<T = unknown>(type: string, params?: Record<string, unknown>, options?: object): Promise<T> {
    const promise = (client.call as UntypedCall)(type, params ?? {}, options) as Promise<T>
    promise.catch(() => {})
    return promise
}

/** Reads how `promise` settled so far: 'pending', 'resolved' or the rejection. */
function outcomeOf(promise: Promise<unknown>): () => unknown {
    let outcome: unknown = 'pending'
    promise.then(() => { outcome = 'resolved' }, (e) => { outcome = e })
    return () => outcome
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
        const events: unknown[] = []
        client.on('agent-updated', (e) => { events.push(e) })
        const controller = new AbortController()
        const promise = call('perspective.querySparql', { uuid: 'u' }, { signal: controller.signal })
        await flush()
        controller.abort()
        await expect(promise).rejects.toMatchObject({ name: 'AbortError' })

        socket(0).reply({ id: socket(0).sent[0].id, result: '[]' })
        socket(0).reply({ id: socket(0).sent[1].id, result: { cancelled: true } })
        expect(events).toEqual([])
    })

    it('rejects with an RpcError carrying the code and message of an error reply', async () => {
        const promise = call('perspective.get', { uuid: 'u' })
        await flush()
        socket(0).reply({ id: socket(0).sent[0].id, error: { code: 404, message: 'perspective not found' } })
        const error = await promise.catch((e) => e)
        expect(error).toBeInstanceOf(RpcError)
        expect(error).toMatchObject({ status: 404, body: 'perspective not found' })
    })

    it('rejects with status 500 when an error reply has no code', async () => {
        const promise = call('agent.get')
        await flush()
        socket(0).reply({ id: socket(0).sent[0].id, error: { message: 'boom' } })
        await expect(promise).rejects.toMatchObject({ name: 'RpcError', status: 500, body: 'boom' })
    })

    it('sends no request.cancel when the signal fires after the call settled', async () => {
        const controller = new AbortController()
        const promise = call('agent.get', {}, { signal: controller.signal })
        await flush()
        socket(0).reply({ id: socket(0).sent[0].id, result: 1 })
        await promise

        controller.abort()
        expect(socket(0).sent).toHaveLength(1)
    })
})

describe('ApiClient connect-phase failures', () => {
    beforeEach(() => { TestSocket.autoOpen = false })

    it('rejects with 503 before its timeout when every socket closes before the call goes out', async () => {
        jest.useFakeTimers()
        try {
            const timeoutMs = 10_000
            const outcome = outcomeOf(call('agent.get', {}, { timeoutMs }))
            let dropped = 0
            for (let elapsed = 0; elapsed < timeoutMs - 100 && outcome() === 'pending'; elapsed += 100) {
                while (dropped < TestSocket.instances.length) socket(dropped++).drop()
                await jest.advanceTimersByTimeAsync(100)
            }
            expect(outcome()).toMatchObject({ name: 'RpcError', status: 503 })
        } finally {
            jest.useRealTimers()
        }
    })

    it('sends a call made while the first connect is refused once a later socket opens', async () => {
        // An executor that logs "API server starting" before it binds refuses the first connect.
        jest.useFakeTimers()
        try {
            const promise = call<string>('agent.generate', { passphrase: 'p' }, { timeoutMs: 10_000 })
            socket(0).drop()
            await jest.advanceTimersByTimeAsync(500)
            socket(1).open()
            expect(socket(1).sent[0]).toMatchObject({ type: 'agent.generate' })
            socket(1).reply({ id: socket(1).sent[0].id, result: 'ok' })
            await expect(promise).resolves.toBe('ok')
        } finally {
            jest.useRealTimers()
        }
    })

    it("a replaced socket's late onclose does not fail the call on its successor", async () => {
        TestSocket.asyncClose = true
        open()
        socket(0).open()
        client.closeAll() // socket 0's onclose arrives 5 ms later
        const promise = call<string>('x')
        await sleep(10)
        socket(1).open()
        socket(1).reply({ id: socket(1).sent[0].id, result: 'ok' })
        await expect(promise).resolves.toBe('ok')
    })

    it('leaves no timers after closeAll', async () => {
        jest.useFakeTimers()
        try {
            const promise = call('agent.status')
            socket(0).open()
            socket(0).reply({ id: socket(0).sent[0].id, result: 1 })
            await promise
            client.closeAll()
            expect(jest.getTimerCount()).toBe(0)
        } finally {
            jest.useRealTimers()
        }
    })

    it('rejects with 408 when the socket never opens within the timeout', async () => {
        const started = Date.now()
        await expect(call('agent.get', {}, { timeoutMs: 50 })).rejects.toMatchObject({ name: 'RpcError', status: 408 })
        expect(Date.now() - started).toBeLessThan(1_000)
    })

    it('closes a socket stuck connecting when a call times out, so the next call dials again', async () => {
        await expect(call('agent.get', {}, { timeoutMs: 20 })).rejects.toMatchObject({ status: 408 })
        expect(socket(0).readyState).toBe(3)

        const next = call<number>('agent.get')
        expect(TestSocket.instances).toHaveLength(2)
        socket(1).open()
        socket(1).reply({ id: socket(1).sent[0].id, result: 1 })
        await expect(next).resolves.toBe(1)
    })

    it('keeps a socket stuck connecting while another call still waits on it', async () => {
        const first = call('agent.get', {}, { timeoutMs: 20 })
        const second = call<number>('agent.get', {}, { timeoutMs: 1_000 })
        await expect(first).rejects.toMatchObject({ status: 408 })
        expect(socket(0).readyState).toBe(0)
        socket(0).open()
        socket(0).reply({ id: socket(0).sent[0].id, result: 1 })
        await expect(second).resolves.toBe(1)
    })

    it('redials for subscribers when a stuck connect is dropped', async () => {
        open()
        await expect(call('agent.get', {}, { timeoutMs: 20 })).rejects.toMatchObject({ status: 408 })
        expect(socket(0).readyState).toBe(3)
        expect(TestSocket.instances).toHaveLength(2)
        socket(1).open()
        const promise = call('agent.get')
        socket(1).reply({ id: socket(1).sent.at(-1)!.id, result: 1 })
        await expect(promise).resolves.toBe(1)
    })

    it('rejects callers waiting to connect when the client is closed', async () => {
        const promise = call('agent.get', {}, { timeoutMs: 1_000 })
        client.closeAll()
        await expect(promise).rejects.toMatchObject({ name: 'RpcError', status: 503 })
    })

    it('keeps the socket for a call still connecting when the last subscriber leaves', async () => {
        TestSocket.asyncClose = true
        const unsubscribe = client.on('agent-updated', () => {})
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

    /** Drop the open socket, if any, and open socket `i`. */
    function connect(i: number) {
        if (socket(i - 1)?.readyState === 1) socket(i - 1).drop()
        open()
        socket(i).open()
    }

    it('fires on a reconnect but not on the first connect', () => {
        const reconnected = jest.fn()
        client.onReconnect(reconnected)
        connect(0)
        expect(reconnected).not.toHaveBeenCalled()
        connect(1)
        expect(reconnected).toHaveBeenCalledTimes(1)
    })

    it('does not fire once unsubscribed', () => {
        const reconnected = jest.fn()
        const unsubscribe = client.onReconnect(reconnected)
        connect(0)
        unsubscribe()
        connect(1)
        expect(reconnected).not.toHaveBeenCalled()
    })

    it('does not fire on the first connect of a client reused after closeAll', () => {
        const reconnected = jest.fn()
        connect(0)
        client.closeAll()
        client.onReconnect(reconnected)
        connect(1)
        expect(reconnected).not.toHaveBeenCalled()
    })
})

describe('long calls', () => {
    beforeEach(() => jest.useFakeTimers())
    afterEach(() => jest.useRealTimers())

    const calls: [string, (o?: object) => Promise<unknown>][] = [
        [`${AiInference_1_0_0.hash}.prompt`, (o) => new AIClient(url, undefined, client).prompt('t', 'p', o)],
        [`${AiInference_1_0_0.hash}.embed`, (o) => new AIClient(url, undefined, client).embed('m', 'x', o)],
        [`${AiModels_1_0_0.hash}.addModel`, (o) => new AIClient(url, undefined, client).addModel({ name: 'm', modelType: 'LLM' } as any, o)],
        ['agent.generate', (o) => new AgentClient(url, undefined, client).generate('pw', o)],
        ['agent.unlock', (o) => new AgentClient(url, undefined, client).unlock('pw', true, o)],
        ['language.publish', (o) => new LanguageClient(url, undefined, client).publish('/p', { name: 'l' } as any, o)],
        ['language.applyTemplate', (o) => new LanguageClient(url, undefined, client).applyTemplateAndPublish('h', '{}', o)],
        ['neighbourhood.publish', (o) => new NeighbourhoodClient(url, undefined, client).publishFromPerspective('u', 'l', { links: [] } as any, o)],
        ['neighbourhood.join', (o) => new NeighbourhoodClient(url, undefined, client).joinFromUrl('n://x', o)],
        [`${HolochainConductor_1_0_0.hash}.restart`, (o) => new RuntimeClient(url, undefined, client).restartHolochain(o)],
        ['perspective.runInterpretation', (o) => new PerspectiveClient(url, undefined, client).runInterpretation('u', [], 'b', undefined, undefined, undefined, undefined, o)],
        ['perspective.runInterpretationWithHarness', (o) => new PerspectiveClient(url, undefined, client).runInterpretationWithHarness('u', [], 'b', 1, undefined, undefined, undefined, undefined, undefined, o)],
    ]
    const proxy = () => new PerspectiveProxy(
        new PerspectiveHandle('u', 'p'),
        new PerspectiveClient(url, undefined, client),
    )
    const proxyCalls: typeof calls = [
        ['perspective.runInterpretation', (o) => proxy().runInterpretation([], 'b', undefined, o)],
        ['perspective.runInterpretationWithHarness', (o) => proxy().runInterpretationWithHarness([], 'b', 1, undefined, undefined, undefined, undefined, o)],
    ]
    const lastSent = () => TestSocket.instances[TestSocket.instances.length - 1].sent.at(-1)!

    describe.each([['client', calls], ['PerspectiveProxy', proxyCalls]])('%s', (_via, table) => {
        it.each(table)('%s waits LONG_TIMEOUT_MS by default', async (type, run) => {
            const outcome = outcomeOf(run())
            await jest.advanceTimersByTimeAsync(1)
            expect(lastSent().type).toBe(type)

            await jest.advanceTimersByTimeAsync(31_000)
            expect(outcome()).toBe('pending')
            await jest.advanceTimersByTimeAsync(LONG_TIMEOUT_MS)
            expect(outcome()).toMatchObject({ name: 'RpcError', status: 408 })
        })

        it.each(table)('%s takes timeoutMs', async (_type, run) => {
            const timedOut = outcomeOf(run({ timeoutMs: 100 }))
            await jest.advanceTimersByTimeAsync(100)
            expect(timedOut()).toMatchObject({ status: 408 })
        })
    })
})

/** A `link-added` event exactly as the executor sends it. */
function linkAdded(perspectiveUuid: string, target = 'literal://x') {
    return {
        type: 'link-added',
        perspectiveUuid,
        owner: 'did:test:owner',
        link: {
            author: 'did:test:alice',
            timestamp: '2026-01-01T00:00:00Z',
            data: { source: 'ad4m://self', predicate: 'ad4m://has', target },
            proof: { key: 'key', signature: 'sig', valid: true, invalid: false },
            status: 'SHARED',
        },
    }
}

/** An `agent-updated` event exactly as the executor sends it. */
const agentUpdated = { type: 'agent-updated', agent: { did: 'did:test:alice', directMessageLanguage: null, perspective: null } }

describe('ApiClient.on', () => {
    it('delivers an event, as sent, to the handlers of its type only', async () => {
        const added: unknown[] = []
        const updated: unknown[] = []
        client.on('link-added', (e) => { added.push(e) })
        client.on('agent-updated', (e) => { updated.push(e) })
        await flush()

        socket(0).reply(linkAdded('p1'))

        expect(added).toEqual([linkAdded('p1')])
        expect(updated).toEqual([])
    })

    it('delivers a scoped event only to handlers of its perspective or of every perspective', async () => {
        const p1: string[] = []
        const p2: string[] = []
        const all: string[] = []
        client.on('link-added', (e) => { p1.push(e.link.data.target) }, { perspective: 'p1' })
        client.on('link-added', (e) => { p2.push(e.link.data.target) }, { perspective: 'p2' })
        client.on('link-added', (e) => { all.push(e.link.data.target) })
        await flush()

        socket(0).reply(linkAdded('p1', 'literal://a'))
        socket(0).reply(linkAdded('p2', 'literal://b'))

        expect(p1).toEqual(['literal://a'])
        expect(p2).toEqual(['literal://b'])
        expect(all).toEqual(['literal://a', 'literal://b'])
    })

    it('stops delivery once unsubscribed and keeps the other handlers', async () => {
        const first = jest.fn()
        const second = jest.fn()
        const off = client.on('link-added', first)
        client.on('link-added', second)
        await flush()

        off()
        socket(0).reply(linkAdded('p1'))

        expect(first).not.toHaveBeenCalled()
        expect(second).toHaveBeenCalledTimes(1)
    })

    it('skips a handler removed during dispatch and delivers to one added during dispatch from the next event', async () => {
        const received: string[] = []
        let offSecond = () => {}
        client.on('link-added', () => {
            received.push('first')
            offSecond()
            client.on('link-added', () => { received.push('added') })
        })
        offSecond = client.on('link-added', () => { received.push('second') })
        await flush()

        socket(0).reply(linkAdded('p1'))
        expect(received).toEqual(['first'])

        socket(0).reply(linkAdded('p1'))
        expect(received).toEqual(['first', 'first', 'added'])
    })

    it('removes a type from watchedEvents() with its last unsubscribe', () => {
        const offAll = client.on('link-added', () => {})
        const offP1 = client.on('link-added', () => {}, { perspective: 'p1' })
        const offP2 = client.on('link-added', () => {}, { perspective: 'p2' })
        client.on('agent-updated', () => {})
        expect(client.watchedEvents()).toEqual({ 'agent-updated': null, 'link-added': null })

        offAll()
        expect(client.watchedEvents()).toEqual({ 'agent-updated': null, 'link-added': ['p1', 'p2'] })
        offP1()
        expect(client.watchedEvents()).toEqual({ 'agent-updated': null, 'link-added': ['p2'] })
        offP2()
        expect(client.watchedEvents()).toEqual({ 'agent-updated': null })
    })

    it('sends the executor the watched events on open', async () => {
        TestSocket.autoOpen = false
        client.on('link-added', () => {}, { perspective: 'p1' })
        client.on('agent-updated', () => {})
        await flush()
        socket(0).open()

        expect(socket(0).sent).toEqual([
            expect.objectContaining({ type: 'events.watch', params: { 'agent-updated': null, 'link-added': ['p1'] } }),
        ])
    })

    it('clears the watch when the last handler goes while a call keeps the socket open', async () => {
        TestSocket.autoOpen = false
        const off = client.on('link-added', () => {})
        await flush()
        socket(0).open()
        const pending = call('agent.get', {})
        off()
        await flush()

        const watches = socket(0).sent.filter(m => m.type === 'events.watch').map(m => m.params)
        expect(watches).toEqual([{ 'link-added': null }, {}])
        expect(socket(0).readyState).toBe(1)
        socket(0).reply({ id: socket(0).sent.find(m => m.type === 'agent.get')!.id, result: null })
        await pending
    })

    it('ignores a second registration of the same handler for the same type and perspective', async () => {
        const handler = jest.fn()
        const off = client.on('link-added', handler, { perspective: 'p1' })
        const offAgain = client.on('link-added', handler, { perspective: 'p1' })
        await flush()

        socket(0).reply(linkAdded('p1'))
        expect(handler).toHaveBeenCalledTimes(1)

        // Both returned functions release the one registration.
        offAgain()
        expect(client.watchedEvents()).toEqual({})
        off()
        socket(0).reply(linkAdded('p1'))
        expect(handler).toHaveBeenCalledTimes(1)
    })

    it('treats one handler under two perspectives as two registrations', async () => {
        const handler = jest.fn()
        client.on('link-added', handler, { perspective: 'p1' })
        const offP2 = client.on('link-added', handler, { perspective: 'p2' })
        await flush()

        offP2()
        socket(0).reply(linkAdded('p1'))
        socket(0).reply(linkAdded('p2'))
        expect(handler).toHaveBeenCalledTimes(1)
        expect(client.watchedEvents()).toEqual({ 'link-added': ['p1'] })
    })

    it('keeps delivering an event to later handlers when one handler throws', async () => {
        const errorSpy = jest.spyOn(console, 'error').mockImplementation(() => {})
        const received: string[] = []
        client.on('agent-updated', () => { received.push('first') })
        client.on('agent-updated', () => { throw new Error('boom') })
        client.on('agent-updated', () => { received.push('third') })
        await flush()

        socket(0).reply(agentUpdated)

        expect(received).toEqual(['first', 'third'])
        expect(errorSpy).toHaveBeenCalledWith("Error in 'agent-updated' handler:", expect.objectContaining({ message: 'boom' }))
        errorSpy.mockRestore()
    })

    it('logs a rejection from an async handler instead of leaving it unhandled', async () => {
        const errorSpy = jest.spyOn(console, 'error').mockImplementation(() => {})
        const unhandled = jest.fn()
        process.on('unhandledRejection', unhandled)
        const received: string[] = []
        client.on('agent-updated', async () => { throw new Error('async boom') })
        client.on('agent-updated', () => { received.push('second') })
        await flush()

        socket(0).reply(agentUpdated)
        await flush()

        expect(received).toEqual(['second'])
        expect(errorSpy).toHaveBeenCalledWith("Error in 'agent-updated' handler:", expect.objectContaining({ message: 'async boom' }))
        expect(unhandled).not.toHaveBeenCalled()
        process.off('unhandledRejection', unhandled)
        errorSpy.mockRestore()
    })

    it('opens no socket until the first handler, and closes it after the last unsubscribe', () => {
        expect(TestSocket.instances).toHaveLength(0)
        const off = client.on('agent-updated', () => {})
        expect(TestSocket.instances).toHaveLength(1)

        off()
        expect(socket(0).readyState).toBe(3)
    })
})

describe('PerspectiveProxy.on', () => {
    const proxyFor = (uuid: string) => new PerspectiveProxy(
        new PerspectiveHandle(uuid, uuid),
        new PerspectiveClient(url, undefined, client),
    )

    it("delivers only the proxy's perspective's events", async () => {
        const received: string[] = []
        proxyFor('p1').on('link-added', ({ link }) => { received.push(link.data.target) })
        await flush()

        socket(0).reply(linkAdded('p2', 'literal://other'))
        socket(0).reply(linkAdded('p1', 'literal://mine'))

        expect(received).toEqual(['literal://mine'])
        expect(client.watchedEvents()).toEqual({ 'link-added': ['p1'] })
    })

    it('releases every handler it registered on dispose() and leaves other proxies\' handlers', async () => {
        const mine = jest.fn()
        const other = jest.fn()
        const proxy = proxyFor('p1')
        const sibling = proxyFor('p1')
        proxy.on('link-added', mine)
        proxy.on('link-removed', mine)
        proxy.on('sync-state-change', mine)
        sibling.on('link-added', other)
        await flush()

        proxy.dispose()
        expect(client.watchedEvents()).toEqual({ 'link-added': ['p1'] })

        socket(0).reply(linkAdded('p1'))
        expect(mine).not.toHaveBeenCalled()
        expect(other).toHaveBeenCalledTimes(1)
    })

    it("a proxy's dispose() leaves another proxy's registration of the same function", async () => {
        const shared = jest.fn()
        const proxy = proxyFor('p1')
        const sibling = proxyFor('p1')
        proxy.on('link-added', shared)
        sibling.on('link-added', shared)
        await flush()

        proxy.dispose()
        socket(0).reply(linkAdded('p1'))
        expect(shared).toHaveBeenCalledTimes(1)
    })

    it('does not release a handler twice when its function runs before dispose()', async () => {
        const proxy = proxyFor('p1')
        const handler = jest.fn()
        const off = proxy.on('link-added', handler)
        off()
        // The client now holds a new registration of the same handler; dispose() must not drop it.
        client.on('link-added', handler, { perspective: 'p1' })
        proxy.dispose()
        await flush()

        socket(0).reply(linkAdded('p1'))
        expect(handler).toHaveBeenCalledTimes(1)
    })
})
