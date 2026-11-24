import { Mirror } from './Mirror'

/** A load the test answers by hand. */
function loader<T>() {
    const asks: { resolve: (v: T) => void; reject: (e: unknown) => void }[] = []
    const load = jest.fn(() => new Promise<T>((resolve, reject) => { asks.push({ resolve, reject }) }))
    return { load, asks }
}

function setup<T>() {
    const mirror = new Mirror()
    const { load, asks } = loader<T>()
    const entry = mirror.entry<T>({ scope: 'agent', kind: 'pushed', method: 'agent.status', params: {}, load })
    return { mirror, entry, load, asks }
}

describe('Mirror entry transitions', () => {
    it('unknown -> loading -> fresh: a read sends one request and keeps the answer', async () => {
        const { entry, load, asks } = setup<string>()
        expect(entry.getSnapshot().state).toBe('unknown')
        const read = entry.read()
        expect(entry.getSnapshot().state).toBe('loading')
        asks[0].resolve('a')
        expect(await read).toBe('a')
        expect(entry.getSnapshot()).toMatchObject({ state: 'fresh', value: 'a', initial: true })
        expect(await entry.read()).toBe('a')
        expect(load).toHaveBeenCalledTimes(1)
    })

    it('concurrent reads share one request', async () => {
        const { entry, load, asks } = setup<string>()
        const reads = [entry.read(), entry.read(), entry.read()]
        expect(load).toHaveBeenCalledTimes(1)
        asks[0].resolve('a')
        expect(await Promise.all(reads)).toEqual(['a', 'a', 'a'])
    })

    it('loading -> stale: an answer sent before the invalidation is returned once and not kept', async () => {
        const { entry, load, asks } = setup<string>()
        const read = entry.read()
        entry.invalidate()
        expect(entry.getSnapshot().state).toBe('stale')
        asks[0].resolve('old')
        expect(await read).toBe('old')
        expect(entry.getSnapshot()).toMatchObject({ state: 'stale', value: undefined })
        const next = entry.read()
        expect(load).toHaveBeenCalledTimes(2)
        asks[1].resolve('new')
        expect(await next).toBe('new')
    })

    it('fresh -> fresh: a value push replaces the value and notifies', async () => {
        const { entry, asks } = setup<{ n: number }>()
        const read = entry.read()
        asks[0].resolve({ n: 1 })
        await read
        const seen: unknown[] = []
        entry.subscribe((s) => seen.push(s.value))
        entry.push({ n: 2 })
        expect(seen).toEqual([{ n: 2 }])
        expect(await entry.read()).toEqual({ n: 2 })
        expect(entry.getSnapshot().initial).toBe(false)
    })

    it('fresh -> fresh: a push of an equal value does not notify', async () => {
        const { entry, asks } = setup<{ a: number; b: number }>()
        const read = entry.read()
        asks[0].resolve({ a: 1, b: 2 })
        await read
        const listener = jest.fn()
        entry.subscribe(listener)
        const before = entry.getSnapshot()
        entry.push({ b: 2, a: 1 })
        expect(listener).not.toHaveBeenCalled()
        expect(entry.getSnapshot()).toBe(before)
    })

    it('a push while loading makes it fresh, and the earlier answer is not kept over it', async () => {
        const { entry, asks } = setup<string>()
        const read = entry.read()
        entry.push('pushed')
        asks[0].resolve('answer')
        expect(await read).toBe('answer')
        expect(entry.getSnapshot()).toMatchObject({ state: 'fresh', value: 'pushed' })
    })

    it('fresh -> stale: an invalidation push marks it stale; the next read sends one request', async () => {
        const { entry, load, asks } = setup<string>()
        const first = entry.read()
        asks[0].resolve('a')
        await first
        entry.invalidate()
        expect(entry.getSnapshot()).toMatchObject({ state: 'stale', value: 'a' })
        expect(load).toHaveBeenCalledTimes(1)
        const next = entry.read()
        expect(load).toHaveBeenCalledTimes(2)
        asks[1].resolve('b')
        expect(await next).toBe('b')
    })

    it('markAllStale: an entry without subscribers waits for a read; one with subscribers refetches', async () => {
        const mirror = new Mirror()
        const make = (method: string) => {
            const { load, asks } = loader<string>()
            return { entry: mirror.entry<string>({ scope: 'executor', kind: 'fixed', method, params: {}, load }), load, asks }
        }
        const quiet = make('a')
        const watched = make('b')
        for (const e of [quiet, watched]) {
            const read = e.entry.read()
            e.asks[0].resolve('v')
            await read
        }
        watched.entry.subscribe(() => {})
        mirror.markAllStale()
        expect(quiet.entry.getSnapshot().state).toBe('stale')
        expect(quiet.load).toHaveBeenCalledTimes(1)
        expect(watched.load).toHaveBeenCalledTimes(2)
        expect(watched.entry.getSnapshot().state).toBe('loading')
    })

    it('a subscriber to an unknown entry makes it load', async () => {
        const { entry, load, asks } = setup<string>()
        const seen: string[] = []
        entry.subscribe((s) => seen.push(s.state))
        expect(load).toHaveBeenCalledTimes(1)
        asks[0].resolve('a')
        await entry.read()
        expect(seen).toEqual(['loading', 'fresh'])
    })

    it('loading -> error: a failure is not cached and the next read retries', async () => {
        const { entry, load, asks } = setup<string>()
        const failed = entry.read()
        asks[0].reject(new Error('down'))
        await expect(failed).rejects.toThrow('down')
        expect(entry.getSnapshot()).toMatchObject({ state: 'error', value: undefined })
        const retry = entry.read()
        expect(load).toHaveBeenCalledTimes(2)
        asks[1].resolve('up')
        expect(await retry).toBe('up')
        expect(entry.getSnapshot()).toMatchObject({ state: 'fresh', error: undefined })
    })

    it('one key is one entry; another scope or params is another', () => {
        const mirror = new Mirror()
        const spec = { kind: 'pushed' as const, method: 'm', load: async () => 1 }
        const a = mirror.entry({ ...spec, scope: { perspective: 'p1' }, params: { x: 1, y: 2 } })
        expect(mirror.entry({ ...spec, scope: { perspective: 'p1' }, params: { y: 2, x: 1 } })).toBe(a)
        expect(mirror.entry({ ...spec, scope: { perspective: 'p2' }, params: { x: 1, y: 2 } })).not.toBe(a)
        expect(mirror.entry({ ...spec, scope: { perspective: 'p1' }, params: { x: 2, y: 2 } })).not.toBe(a)
    })
})
