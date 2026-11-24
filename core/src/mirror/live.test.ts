import { Mirror } from './Mirror'
import { PerspectiveClient } from '../perspectives/PerspectiveClient'
import { DEFAULT_KEEP_ALIVE_MS, type QueryLagged, type QueryUpdate } from '../perspectives/LiveQuery'

/**
 * A `PerspectiveClient` over a stub `ApiClient` with a real mirror. `calls(type)` counts the
 * requests the executor would have got.
 */
function setup() {
    const mirror = new Mirror(() => '', () => {}, () => null)
    const listeners = new Set<(u: QueryUpdate | QueryLagged) => void>()
    const reconnects = new Set<() => void>()
    const call = jest.fn(async (type: string, params: any): Promise<any> => {
        if (type === 'perspective.subscribe') {
            return {
                subscriptionId: params.subscriptionId,
                revision: 0,
                result: params.sparql ? [{ s: 'live' }] : { instances: [{ id: 'a' }], totalCount: 1 },
            }
        }
        if (type === 'perspective.resyncSubscription') return { revision: 5, result: { instances: [{ id: 'a' }], totalCount: 1 } }
        if (type === 'perspective.disposeQuery') return true
        if (type === 'perspective.modelQuery') return JSON.stringify({ instances: [{ id: 'one-shot' }], totalCount: 1 })
        if (type === 'perspective.querySparql') return JSON.stringify([{ s: 'one-shot' }])
        if (type === 'perspective.addLinks') return []
        throw new Error(`unexpected ${type}`)
    })
    const api = {
        mirror,
        call,
        on: (_type: string, cb: any) => { listeners.add(cb); return () => listeners.delete(cb) },
        onReconnect: (cb: () => void) => { reconnects.add(cb); return () => reconnects.delete(cb) },
    }
    const client = new PerspectiveClient('http://localhost:0', undefined, api as any)
    return {
        client,
        mirror,
        call,
        calls: (type: string) => call.mock.calls.filter(c => c[0] === type).length,
        /** The id the first subscribe call minted: the one an update names. */
        subscriptionId: () => call.mock.calls.find(c => c[0] === 'perspective.subscribe')![1].subscriptionId as string,
        push: (update: Partial<QueryUpdate> | QueryLagged) => listeners.forEach(cb => cb(update as any)),
        reconnect: () => reconnects.forEach(cb => cb()),
        socketListeners: () => listeners.size,
    }
}

const model = { className: 'Recipe', queryJson: '{"limit":10}' }
const project = (raw: any) => raw.instances.map((i: any) => i.id) as string[]
const flush = async () => { for (let i = 0; i < 10; i++) await Promise.resolve() }

describe('live entries of the mirror', () => {
    beforeEach(() => jest.useFakeTimers())
    afterEach(() => jest.useRealTimers())

    it('two handles with one key are one executor subscription', async () => {
        const { client, calls, socketListeners } = setup()
        const a = client.liveQuery('p1', model, project)
        const b = client.liveQuery('p1', { ...model }, project)
        expect(await a.first).toEqual(['a'])
        expect(await b.first).toEqual(['a'])
        expect(calls('perspective.subscribe')).toBe(1)
        // one listener on the socket, not one per handle
        expect(socketListeners()).toBe(1)
    })

    it('a different query, perspective, kind of query or client is a separate subscription', async () => {
        const one = setup()
        const other = setup()
        await Promise.all([
            one.client.liveQuery('p1', model, project).first,
            one.client.liveQuery('p1', { ...model, queryJson: '{"limit":11}' }, project).first,
            one.client.liveQuery('p2', model, project).first,
            one.client.liveQuery('p1', { sparql: 'SELECT ?s WHERE { ?s ?p ?o }' }, (rows) => rows).first,
            other.client.liveQuery('p1', model, project).first,
        ])
        expect(one.calls('perspective.subscribe')).toBe(4)
        expect(other.calls('perspective.subscribe')).toBe(1)
    })

    it('disposing one handle keeps the other updating and silences only the disposed one', async () => {
        const { client, push, subscriptionId, calls } = setup()
        const a = client.liveQuery('p1', model, project)
        const b = client.liveQuery('p1', model, project)
        await a.first
        const heardA = jest.fn()
        const heardB = jest.fn()
        a.subscribe(heardA)
        b.subscribe(heardB)
        heardA.mockClear()
        heardB.mockClear()

        a.dispose()
        push({ subscriptionId: subscriptionId(), revision: 1, ids: ['a', 'z'], upsert: [{ id: 'z' }], totalCount: 2 })

        expect(heardA).not.toHaveBeenCalled()
        expect(heardB).toHaveBeenCalledTimes(1)
        expect(b.getSnapshot().data).toEqual(['a', 'z'])
        await jest.advanceTimersByTimeAsync(DEFAULT_KEEP_ALIVE_MS * 2)
        expect(calls('perspective.disposeQuery')).toBe(0)
    })

    it('a handle disposed from its own first subscribe callback gets no later update', async () => {
        const { client, push, subscriptionId } = setup()
        const a = client.liveQuery('p1', model, project)
        const b = client.liveQuery('p1', { ...model }, project)
        await a.first
        const seen: unknown[] = []
        // The subscription is ready: the callback runs inside subscribe(), before it returns its remover.
        a.subscribe((snap) => { seen.push(snap.data); a.dispose() })
        expect(seen).toHaveLength(1)
        push({ subscriptionId: subscriptionId(), perspectiveUuid: 'p1', revision: 1, result: { instances: [{ id: 'z' }], totalCount: 1 } } as any)
        await flush()
        expect(seen).toHaveLength(1)
        expect(await b.first).toEqual(['a'])
    })

    it('a handle disposed before the first result rejects its own `first` only', async () => {
        const { client } = setup()
        const a = client.liveQuery('p1', model, project)
        const b = client.liveQuery('p1', model, project)
        a.dispose()
        await expect(a.first).rejects.toThrow('disposed before its first result')
        await expect(b.first).resolves.toEqual(['a'])
    })

    it('the last handle released ends the executor subscription after keepAliveMs, not before', async () => {
        const { client, calls } = setup()
        const a = client.liveQuery('p1', model, project)
        const b = client.liveQuery('p1', model, project)
        await a.first
        a.dispose()
        b.dispose()

        await jest.advanceTimersByTimeAsync(DEFAULT_KEEP_ALIVE_MS - 1)
        expect(calls('perspective.disposeQuery')).toBe(0)

        await jest.advanceTimersByTimeAsync(1)
        expect(calls('perspective.disposeQuery')).toBe(1)
    })

    it('keepAliveMs of the handle released last sets the grace', async () => {
        const { client, calls } = setup()
        const a = client.liveQuery('p1', model, project, { keepAliveMs: 500 })
        await a.first
        a.dispose()
        await jest.advanceTimersByTimeAsync(499)
        expect(calls('perspective.disposeQuery')).toBe(0)
        await jest.advanceTimersByTimeAsync(1)
        expect(calls('perspective.disposeQuery')).toBe(1)
    })

    it('a new handle within the grace opens nothing and answers at once; the old grace is cancelled', async () => {
        const { client, calls } = setup()
        const first = client.liveQuery('p1', model, project)
        await first.first
        first.dispose()
        await jest.advanceTimersByTimeAsync(DEFAULT_KEEP_ALIVE_MS - 1000)

        const again = client.liveQuery('p1', model, project)
        // no request, and the snapshot is there before anything is awaited
        expect(again.getSnapshot()).toMatchObject({ status: 'ready', data: ['a'] })
        const heard = jest.fn()
        again.subscribe(heard)
        expect(heard).toHaveBeenCalledTimes(1)
        expect(await again.first).toEqual(['a'])
        expect(calls('perspective.subscribe')).toBe(1)

        await jest.advanceTimersByTimeAsync(DEFAULT_KEEP_ALIVE_MS * 2)
        expect(calls('perspective.disposeQuery')).toBe(0)
        again.dispose()
        await jest.advanceTimersByTimeAsync(DEFAULT_KEEP_ALIVE_MS)
        expect(calls('perspective.disposeQuery')).toBe(1)
    })

    it('a handle after the grace opens a new subscription', async () => {
        const { client, calls } = setup()
        const first = client.liveQuery('p1', model, project)
        await first.first
        first.dispose()
        await jest.advanceTimersByTimeAsync(DEFAULT_KEEP_ALIVE_MS)
        const again = client.liveQuery('p1', model, project)
        expect(again.getSnapshot().status).toBe('loading')
        await again.first
        expect(calls('perspective.subscribe')).toBe(2)
    })

    it('a failed first open is not shared: the next handle opens again', async () => {
        const { client, call, calls } = setup()
        call.mockImplementationOnce(async () => { throw new Error('refused') })
        const failed = client.liveQuery('p1', model, project)
        await expect(failed.first).rejects.toThrow('refused')
        const next = client.liveQuery('p1', model, project)
        await expect(next.first).resolves.toEqual(['a'])
        expect(calls('perspective.subscribe')).toBe(2)
    })

    it('a lag marker resyncs the shared subscription once, not once per handle', async () => {
        const { client, push, calls } = setup()
        const handles = [1, 2, 3].map(() => client.liveQuery('p1', model, project))
        await handles[0].first
        push({ lagged: true })
        await flush()
        expect(calls('perspective.resyncSubscription')).toBe(1)
    })

    it('a reconnect re-opens the shared subscription once, not once per handle', async () => {
        const { client, reconnect, calls } = setup()
        const handles = [1, 2, 3].map(() => client.liveQuery('p1', model, project))
        await handles[0].first
        reconnect()
        await flush()
        expect(calls('perspective.subscribe')).toBe(2)
    })

    it('a view opened again costs no request while the subscription is held', async () => {
        const { client, call } = setup()
        const view = client.liveQuery('p1', model, project)
        await view.first
        const opening = call.mock.calls.length
        view.dispose()
        const reopened = client.liveQuery('p1', model, project)
        await reopened.first
        expect({ opening, reopened: call.mock.calls.length - opening }).toEqual({ opening: 1, reopened: 0 })
    })

    it('a handle opened after an update has the current result as `first`', async () => {
        const { client, push, subscriptionId } = setup()
        const a = client.liveQuery('p1', model, project)
        expect(await a.first).toEqual(['a'])
        push({ subscriptionId: subscriptionId(), revision: 1, ids: ['a', 'b'], upsert: [{ id: 'b' }], totalCount: 2 })
        const b = client.liveQuery('p1', model, project)
        expect(b.getSnapshot().data).toEqual(['a', 'b'])
        expect(await b.first).toEqual(['a', 'b'])
    })

    it('a handle remounted within the grace has the current result as `first`', async () => {
        const { client, push, subscriptionId } = setup()
        const a = client.liveQuery('p1', model, project)
        await a.first
        a.dispose()
        push({ subscriptionId: subscriptionId(), revision: 1, ids: ['a', 'b'], upsert: [{ id: 'b' }], totalCount: 2 })
        await jest.advanceTimersByTimeAsync(1000)
        expect(await client.liveQuery('p1', model, project).first).toEqual(['a', 'b'])
    })

    it('handles with one key and different projections each get their own shape, from one subscription', async () => {
        const { client, push, subscriptionId, calls } = setup()
        const ids = client.liveQuery('p1', model, project)
        const count = client.liveQuery('p1', model, (raw: any) => raw.instances.length as number)
        expect(await ids.first).toEqual(['a'])
        expect(await count.first).toBe(1)
        expect(count.getSnapshot().data).toBe(1)
        // the snapshot is the same object until something changes
        expect(ids.getSnapshot()).toBe(ids.getSnapshot())
        const heard = jest.fn()
        count.subscribe(heard)
        heard.mockClear()
        push({ subscriptionId: subscriptionId(), revision: 1, ids: ['a', 'b'], upsert: [{ id: 'b' }], totalCount: 2 })
        expect(ids.getSnapshot().data).toEqual(['a', 'b'])
        expect(count.getSnapshot().data).toBe(2)
        expect(heard).toHaveBeenCalledTimes(1)
        expect(heard.mock.calls[0][0].data).toBe(2)
        expect(calls('perspective.subscribe')).toBe(1)
    })

    it('a projection that throws on the first result fails only its own view', async () => {
        const { client, calls } = setup()
        const broken = client.liveQuery('p1', model, () => { throw new Error('cannot project') })
        const working = client.liveQuery('p1', model, project)
        await expect(broken.first).rejects.toThrow('cannot project')
        expect(broken.getSnapshot()).toMatchObject({ status: 'error', reopening: false })
        expect(await working.first).toEqual(['a'])
        expect(working.getSnapshot()).toMatchObject({ status: 'ready', data: ['a'] })
        expect(calls('perspective.subscribe')).toBe(1)
    })

    it('a projection that throws on a later result keeps the view\'s old data', async () => {
        const { client, push, subscriptionId } = setup()
        const logged = jest.spyOn(console, 'error').mockImplementation(() => {})
        try {
            const strict = client.liveQuery('p1', model, (raw: any) => {
                if (raw.instances.some((i: any) => i.id === 'bad')) throw new Error('cannot project')
                return project(raw)
            })
            const lenient = client.liveQuery('p1', model, project)
            expect(await strict.first).toEqual(['a'])
            await lenient.first
            push({ subscriptionId: subscriptionId(), revision: 1, ids: ['a', 'bad'], upsert: [{ id: 'bad' }], totalCount: 2 })
            expect(strict.getSnapshot()).toMatchObject({ status: 'ready', data: ['a'] })
            expect(logged).toHaveBeenCalledWith('Live query result could not be read:', expect.any(Error))
            expect(lenient.getSnapshot().data).toEqual(['a', 'bad'])
        } finally {
            logged.mockRestore()
        }
    })

    describe('one-shot reads', () => {
        it('a model query with the key of a ready live query answers from it, with no request', async () => {
            const { client, calls } = setup()
            const live = client.liveQuery('p1', model, project)
            await live.first
            const answer = await client.modelQuery('p1', model.className, model.queryJson)
            expect(answer).toEqual({ instances: [{ id: 'a' }], totalCount: 1 })
            expect(calls('perspective.modelQuery')).toBe(0)
            // a copy: changing it does not change the live query
            answer.instances.push({ id: 'x' })
            expect(live.getSnapshot().data).toEqual(['a'])
        })

        it('a SPARQL query with the key of a ready live query answers from it, with no request', async () => {
            const { client, calls } = setup()
            const text = 'SELECT ?s WHERE { ?s ?p ?o }'
            await client.liveQuery('p1', { sparql: text }, (rows) => rows).first
            expect(await client.querySparql('p1', text)).toEqual([{ s: 'live' }])
            expect(await client.querySparql('p1', text, { timeoutMs: 1000 })).toEqual([{ s: 'live' }])
            expect(calls('perspective.querySparql')).toBe(0)
        })

        it('a call whose signal is already aborted does not take the live answer', async () => {
            const { client, calls } = setup()
            const text = 'SELECT ?s WHERE { ?s ?p ?o }'
            await client.liveQuery('p1', { sparql: text }, (rows) => rows).first
            await client.liveQuery('p1', model, project).first
            const signal = AbortSignal.abort()
            await client.querySparql('p1', text, { signal })
            await client.modelQuery('p1', model.className, model.queryJson, { signal })
            expect(calls('perspective.querySparql')).toBe(1)
            expect(calls('perspective.modelQuery')).toBe(1)
        })

        it('asks the executor while the live query is not ready, for another query, perspective or cursor', async () => {
            const { client, calls } = setup()
            const live = client.liveQuery('p1', model, project)
            // still loading
            await client.modelQuery('p1', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(1)
            await live.first
            await client.modelQuery('p1', model.className, '{"limit":11}')
            await client.modelQuery('p1', model.className, '{"limit":10,"after":"c"}')
            await client.modelQuery('p2', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(4)
        })

        it('asks the executor after an own write, until an update changes the result', async () => {
            const { client, calls, push, subscriptionId } = setup()
            await client.liveQuery('p1', model, project).first
            await client.addLinks('p1', [])
            await client.modelQuery('p1', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(1)

            push({ subscriptionId: subscriptionId(), revision: 1, ids: ['a', 'b'], upsert: [{ id: 'b' }], totalCount: 2 })
            const answer = await client.modelQuery('p1', model.className, model.queryJson)
            expect(answer.instances.map((i: any) => i.id)).toEqual(['a', 'b'])
            expect(calls('perspective.modelQuery')).toBe(1)
        })

        it('asks the executor once the mirror is marked stale, until the resync delivers a result', async () => {
            const { client, mirror, calls, reconnect } = setup()
            const live = client.liveQuery('p1', model, project)
            await live.first
            mirror.markAllStale()
            await client.modelQuery('p1', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(1)
            // the live snapshot stays for display
            expect(live.getSnapshot()).toMatchObject({ status: 'ready', data: ['a'] })
            reconnect()
            await flush()
            await client.modelQuery('p1', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(1)
        })

        it('a write to another perspective changes nothing', async () => {
            const { client, calls } = setup()
            await client.liveQuery('p1', model, project).first
            await client.addLinks('p2', [])
            await client.modelQuery('p1', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(0)
        })

        it('answers during the grace after the last handle, and asks again once it is over', async () => {
            const { client, calls } = setup()
            const live = client.liveQuery('p1', model, project)
            await live.first
            live.dispose()
            await client.modelQuery('p1', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(0)
            await jest.advanceTimersByTimeAsync(DEFAULT_KEEP_ALIVE_MS)
            await client.modelQuery('p1', model.className, model.queryJson)
            expect(calls('perspective.modelQuery')).toBe(1)
        })
    })
})

describe('live results around a loss, a resync and a write', () => {
    beforeEach(() => jest.useFakeTimers())
    afterEach(() => jest.useRealTimers())
    const text = 'SELECT ?s WHERE { ?s ?p ?o }'

    it('a subscriber that reads from the notice of the loss gets a request, not the old live result', async () => {
        const { client, mirror, calls } = setup()
        await client.liveQuery('p1', { sparql: text }, (rows) => rows).first
        const entry = mirror.entry<string>({ method: 'agent.status', load: () => Promise.resolve('s') })
        await entry.read()
        entry.subscribe((snapshot) => {
            if (snapshot.state === 'stale') client.querySparql('p1', text).catch(() => {})
        })
        mirror.markAllStale(false)
        expect(calls('perspective.querySparql')).toBe(1)
    })

    it('a reader called while the resync result and its buffered updates are applied gets a request', async () => {
        const { client, calls, push, subscriptionId } = setup()
        const live = client.liveQuery('p1', model, project)
        await live.first
        const asked: number[] = []
        live.subscribe(() => {
            client.modelQuery('p1', model.className, model.queryJson).catch(() => {})
            asked.push(calls('perspective.modelQuery'))
        })
        asked.length = 0
        push({ lagged: true })
        push({ subscriptionId: subscriptionId(), revision: 6, ids: ['a', 'b'], upsert: [{ id: 'b' }], totalCount: 2 })
        await flush()
        // every notice of the replay, the first one included, asked the executor
        expect(asked.length).toBeGreaterThan(0)
        expect(asked[0]).toBe(1)
    })

    it('a write during a resync keeps the result closed past the updates buffered before it', async () => {
        const { client, call, calls, push, subscriptionId } = setup()
        const live = client.liveQuery('p1', model, project)
        await live.first
        const impl = call.getMockImplementation()!
        let resolveResync!: (v: any) => void
        call.mockImplementation((type: string, params: any) => type === 'perspective.resyncSubscription'
            ? new Promise((resolve) => { resolveResync = resolve })
            : impl(type, params))
        push({ lagged: true })
        // an update from before the write
        push({ subscriptionId: subscriptionId(), revision: 6, ids: ['a', 'b'], upsert: [{ id: 'b' }], totalCount: 2 })
        await client.addLinks('p1', [])
        resolveResync({ revision: 5, result: { instances: [{ id: 'a' }], totalCount: 1 } })
        await flush()
        expect(live.getSnapshot().data).toEqual(['a', 'b'])
        await client.modelQuery('p1', model.className, model.queryJson)
        expect(calls('perspective.modelQuery')).toBe(1)
        // the update after the write opens it
        push({ subscriptionId: subscriptionId(), revision: 7, ids: ['a', 'b', 'c'], upsert: [{ id: 'c' }], totalCount: 3 })
        const answer = await client.modelQuery('p1', model.className, model.queryJson)
        expect(calls('perspective.modelQuery')).toBe(1)
        expect(answer.instances.map((i: any) => i.id)).toEqual(['a', 'b', 'c'])
    })
})
