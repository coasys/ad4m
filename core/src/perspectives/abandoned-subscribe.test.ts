/**
 * A subscribe the client gave up on must not leave a live query on the executor.
 *
 * On staging, the OpenClaw waker's mention subscription took longer than its
 * handshake budget. Each attempt was given up on, the executor finished it
 * anyway and kept the subscription, and the client dropped the late reply that
 * named it: nobody could dispose it until the socket closed. A re-attempt every
 * 45 s added one more live model query over 76k links, until the executor sat
 * at 400 % CPU.
 *
 * These tests run the real ApiClient, PerspectiveClient, QuerySubscriptionProxy
 * and WakerSubscriptionManager against a fake executor socket that keeps
 * subscriptions as the executor does: registered when the subscribe call
 * arrives, dropped again by a `request.cancel` that arrives while the call is
 * in flight, kept once the call has answered, ended by `perspective.disposeQuery`.
 */
import { ApiClient } from '../apiClient'
import { PerspectiveClient } from './PerspectiveClient'
import { QuerySubscriptionProxy } from './PerspectiveProxy'
import { WakerSubscriptionManager } from './WakerSubscriptionManager'

type AnyMsg = Record<string, any>

const OPENS = new Set(['perspective.modelSubscribe', 'perspective.subscribeQuery'])

/** What the fake executor holds: the live subscriptions it would re-check on every change. */
const executor = {
    /** Live subscriptions by id. */
    live: new Set<string>(),
    /** How long a subscribe call computes its first result. */
    firstResultMs: 0,
    /** How long a reply takes to reach the client once sent. */
    transitMs: 0,
    /** Every `perspective.modelSubscribe` / `subscribeQuery` call received. */
    subscribes: 0,
    reset() {
        this.live = new Set()
        this.firstResultMs = 0
        this.transitMs = 0
        this.subscribes = 0
    },
}

let nextSub = 0

/** A socket to the fake executor. Client messages are handled when sent. */
class ExecutorSocket {
    readyState = 0
    onopen: (() => void) | null = null
    onmessage: ((ev: { data: string }) => void) | null = null
    onerror: ((ev: unknown) => void) | null = null
    onclose: ((ev: { code: number; reason: string }) => void) | null = null
    /** Subscribe calls computing their first result, by request id: the subscription each registered. */
    #inflight = new Map<string, { subscriptionId: string; timer: ReturnType<typeof setTimeout> }>()

    constructor(public url: string) {
        queueMicrotask(() => { this.readyState = 1; this.onopen?.() })
    }

    send(data: string) {
        const msg = JSON.parse(data)
        if (msg.type === 'ping') return this.#reply({ type: 'pong' })
        if (msg.type === 'request.cancel') {
            const call = this.#inflight.get(msg.params.targetId)
            if (call) {
                // The executor drops the call's future: `UnregisterOnDrop` takes the subscription out.
                clearTimeout(call.timer)
                this.#inflight.delete(msg.params.targetId)
                executor.live.delete(call.subscriptionId)
                this.#reply({ id: msg.params.targetId, error: { code: 499, message: 'Request cancelled by client' } })
            }
            return this.#reply({ id: msg.id, result: { cancelled: !!call, targetId: msg.params.targetId } })
        }
        if (OPENS.has(msg.type)) {
            executor.subscribes++
            // `open_subscription` registers before it computes the first result.
            const subscriptionId = `sub-${++nextSub}`
            executor.live.add(subscriptionId)
            const timer = setTimeout(() => {
                this.#inflight.delete(msg.id)
                this.#reply({ id: msg.id, result: { subscriptionId, result: { instances: [], totalCount: 0 }, revision: 0 } })
            }, executor.firstResultMs)
            this.#inflight.set(msg.id, { subscriptionId, timer })
            return
        }
        if (msg.type === 'perspective.disposeQuery') {
            const had = executor.live.delete(msg.params.subscriptionId)
            return this.#reply({ id: msg.id, result: had })
        }
        // events.watch and anything else: a plain ack.
        this.#reply({ id: msg.id, result: true })
    }

    close() { this.readyState = 3; this.onclose?.({ code: 1000, reason: '' }) }

    #reply(msg: AnyMsg) {
        const deliver = () => { if (this.readyState === 1) this.onmessage?.({ data: JSON.stringify(msg) }) }
        if (executor.transitMs > 0 && msg.type !== 'pong') setTimeout(deliver, executor.transitMs)
        else queueMicrotask(deliver)
    }
}

const ME = 'did:key:zAgent'

/** A mention subscription as `get_mention_waker_config` returns it: two model queries. */
const mention = {
    id: 'mention-9995ba16',
    type: 'mention' as const,
    perspective: '9995ba16-d14f-43c1-b2c3-83b9474633a0',
    channel: '',
    className: 'ad4m://TextContent',
    query: JSON.stringify({ where: { OR: [{ text: { contains: 'Data', author: { not: ME } } }] }, links: ['text'] }),
    terms: ['Data', ME],
    did: ME,
    mentionClass: 'ad4m://Mentioning',
    mentionQuery: JSON.stringify({ where: { mentions: { eq: ME, author: { not: ME } } }, links: ['mentions'] }),
}

const quiet = { info: () => {}, warn: () => {}, error: () => {}, debug: () => {} }

let api: ApiClient
let perspectives: PerspectiveClient

function waker(options: { subscribeTimeoutMs?: number } = {}) {
    return new WakerSubscriptionManager({
        perspectiveClient: perspectives,
        logger: quiet,
        onWake: () => {},
        QuerySubscriptionProxy,
        // Re-attempts are driven by hand below.
        retryPendingMs: 0,
        ...options,
    })
}

/** Run `promise` while advancing the fake clock by `ms`; returns how it settled. */
async function settleWithin(promise: Promise<unknown>, ms: number): Promise<unknown> {
    let outcome: unknown = 'pending'
    promise.then(() => { outcome = 'resolved' }, (e) => { outcome = e })
    await jest.advanceTimersByTimeAsync(ms)
    return outcome
}

beforeEach(() => {
    // Microtasks stay real: replies and the client's own promise chains run between timers.
    jest.useFakeTimers({ doNotFake: ['queueMicrotask', 'nextTick'] })
    executor.reset()
    api = new ApiClient('http://localhost:1234', undefined, ExecutorSocket as unknown as new (url: string) => WebSocket)
    perspectives = new PerspectiveClient('http://localhost:1234', undefined, api)
})

afterEach(() => {
    api.closeAll()
    jest.useRealTimers()
})

describe('a subscribe the client gave up on', () => {
    it('leaves no live query on the executor when its first result arrives after the handshake timeout, and a re-attempt does not stack another', async () => {
        const manager = waker()
        // The first result is ready just before the 15 s handshake budget ends, and the reply
        // is still on the wire when the client gives up: the cancel finds the call answered.
        executor.firstResultMs = 14_990
        executor.transitMs = 50

        const first = await settleWithin(manager.subscribe(mention), 16_000)
        expect(String(first)).toMatch(/handshake timed out after 15000ms/)
        expect(executor.subscribes).toBe(2) // the text query and the Mentioning query
        expect([...executor.live]).toEqual([])

        // The re-attempt the waker makes 30 s later fails the same way.
        await settleWithin(manager.retryPending(), 16_000)
        expect(manager.getPending().map((s) => s.id)).toEqual([mention.id])
        expect(executor.subscribes).toBe(4)
        expect([...executor.live]).toEqual([])

        // Once the executor answers in time, exactly the two queries of that attempt are live.
        executor.firstResultMs = 100
        executor.transitMs = 0
        await settleWithin(manager.retryPending(), 1_000)
        expect(manager.getActive().map((s) => s.id)).toEqual([mention.id])
        expect(executor.live.size).toBe(2)

        manager.disposeAll()
        await jest.advanceTimersByTimeAsync(10)
        expect([...executor.live]).toEqual([])
    })

    it('leaves no live query on the executor when the call itself times out', async () => {
        // No handshake budget of the waker's own: the RPC call's 30 s timeout is what gives up,
        // while the executor is still computing the first result.
        const manager = waker({ subscribeTimeoutMs: 0 })
        executor.firstResultMs = 45_000

        const first = await settleWithin(manager.subscribe(mention), 31_000)
        expect(String(first)).toMatch(/timed out after 30000ms/)
        // The executor got a cancel and dropped the call's subscription; nothing comes back later.
        await jest.advanceTimersByTimeAsync(20_000)
        expect([...executor.live]).toEqual([])
        manager.disposeAll()
    })

    it('disposes the subscription a late reply names, for a live query opened directly', async () => {
        executor.firstResultMs = 1_000
        executor.transitMs = 50
        const controller = new AbortController()
        const call = perspectives.modelSubscribe('p', 'ad4m://TextContent', '{}', { signal: controller.signal })
        call.catch(() => {})
        // Abort after the executor answered, before the answer arrives.
        await jest.advanceTimersByTimeAsync(1_020)
        controller.abort()
        await expect(call).rejects.toMatchObject({ name: 'AbortError' })
        expect(executor.live.size).toBe(1)
        await jest.advanceTimersByTimeAsync(200)
        expect([...executor.live]).toEqual([])
    })
})
