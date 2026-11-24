import { Mirror } from './Mirror'

/** A load the test answers by hand. */
function loader<T>() {
    const asks: { resolve: (v: T) => void; reject: (e: unknown) => void }[] = []
    const load = jest.fn(() => new Promise<T>((resolve, reject) => { asks.push({ resolve, reject }) }))
    return { load, asks }
}

function setup<T>() {
    const mirror = new Mirror(() => '', () => {}, () => null)
    const { load, asks } = loader<T>()
    const entry = mirror.entry<T>({ method: 'agent.status', load })
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
        expect(entry.getSnapshot()).toMatchObject({ state: 'fresh', value: 'a' })
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
        const mirror = new Mirror(() => '', () => {}, () => null)
        const make = (method: string) => {
            const { load, asks } = loader<string>()
            return { entry: mirror.entry<string>({ method, load }), load, asks }
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

    it('one method is one entry; another method is another', () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const a = mirror.entry({ method: 'm', load: async () => 1 })
        expect(mirror.entry({ method: 'm', load: async () => 2 })).toBe(a)
        expect(mirror.entry({ method: 'n', load: async () => 1 })).not.toBe(a)
    })
})

describe('Mirror entry events', () => {
    it('registers each event of an entry spec once, whatever the number of reads, and hands the handler the entry', () => {
        const handlers: Record<string, (event: any) => void> = {}
        const listen = jest.fn((name: string, handler: (event: any) => void) => { handlers[name] = handler })
        const mirror = new Mirror(() => '', listen as any, () => null)
        const spec = (): any => ({
            method: 'agent.status', load: async () => 'loaded',
            on: { 'agent-status-changed': (event: { agent: string }, entry: any) => entry.push(event.agent) },
        })
        const entry = mirror.entry<string>(spec())
        mirror.entry<string>(spec())
        mirror.entry<string>(spec())
        expect(listen).toHaveBeenCalledTimes(1)

        handlers['agent-status-changed']({ agent: 'pushed' })
        expect(entry.getSnapshot()).toMatchObject({ state: 'fresh', value: 'pushed' })
    })
})

describe('Mirror.oneShot', () => {
    const spec = (mirror: Mirror, request: () => Promise<string>, params: unknown = { q: 1 }) =>
        mirror.oneShot({ perspective: 'p', method: 'perspective.querySparql', params, request })

    it('identical concurrent calls share one request; nothing is kept after it settles', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const { load, asks } = loader<string>()
        const a = spec(mirror, load)
        const b = spec(mirror, load, { q: 1 })
        expect(load).toHaveBeenCalledTimes(1)
        asks[0].resolve('r')
        expect(await Promise.all([a, b])).toEqual(['r', 'r'])
        const c = spec(mirror, load)
        expect(load).toHaveBeenCalledTimes(2)
        asks[1].resolve('r2')
        expect(await c).toBe('r2')
    })

    it('different params do not share', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const { load, asks } = loader<string>()
        const a = spec(mirror, load, { q: 1 })
        const b = spec(mirror, load, { q: 2 })
        expect(load).toHaveBeenCalledTimes(2)
        asks[0].resolve('a'); asks[1].resolve('b')
        expect(await Promise.all([a, b])).toEqual(['a', 'b'])
    })

    it('a call after a settled write does not join a request sent before it', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const { load, asks } = loader<string>()
        const before = spec(mirror, load)
        mirror.wrote('p')
        const after = spec(mirror, load)
        expect(load).toHaveBeenCalledTimes(2)
        const again = spec(mirror, load)
        expect(load).toHaveBeenCalledTimes(2)
        asks[0].resolve('old'); asks[1].resolve('new')
        expect(await before).toBe('old')
        expect(await Promise.all([after, again])).toEqual(['new', 'new'])
    })
})

describe('Mirror entry review round', () => {
    it('a subscriber that reads from its loading callback joins the read in flight', async () => {
        const { entry, load, asks } = setup<string>()
        let joined: Promise<string> | undefined
        let reading = false
        entry.subscribe((snapshot) => { if (snapshot.state === 'loading' && !reading) { reading = true; joined = entry.read() } })
        expect(load).toHaveBeenCalledTimes(1)
        asks[0].resolve('a')
        expect(await joined).toBe('a')
        expect(entry.getSnapshot()).toMatchObject({ state: 'fresh', value: 'a' })
    })

    it('a loader that throws before it returns a promise fails the read, and the next read retries', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        let attempts = 0
        const entry = mirror.entry<string>({ method: 'agent.status', load: () => {
            if (++attempts === 1) throw new Error('sync boom')
            return Promise.resolve('ok')
        } })
        const unsubscribe = entry.subscribe(() => {})
        expect(typeof unsubscribe).toBe('function')
        await new Promise((r) => setTimeout(r, 0))
        expect(entry.getSnapshot()).toMatchObject({ state: 'error' })
        await expect(entry.read()).resolves.toBe('ok')
        expect(entry.getSnapshot().state).toBe('fresh')
    })

    it('a retry shows no error while it loads, and keeps the last value', async () => {
        const { entry, asks } = setup<string>()
        const first = entry.read()
        asks[0].resolve('a')
        await first
        entry.invalidate()
        const failing = entry.read()
        asks[1].reject(new Error('down'))
        await expect(failing).rejects.toThrow('down')
        expect(entry.getSnapshot()).toMatchObject({ state: 'error', value: 'a' })
        const retry = entry.read()
        expect(entry.getSnapshot()).toMatchObject({ state: 'loading', value: 'a', error: undefined })
        asks[2].resolve('b')
        await retry
    })

    it('a watched entry whose read failed while the socket was down reads once when the socket opens', async () => {
        const { mirror, entry, load, asks } = setup<string>()
        const seen: string[] = []
        entry.subscribe((snapshot) => seen.push(snapshot.state))
        asks[0].reject(new Error('no socket'))
        await Promise.resolve(); await Promise.resolve()
        expect(entry.getSnapshot().state).toBe('error')
        mirror.reloadWatched()
        expect(load).toHaveBeenCalledTimes(2)
        mirror.reloadWatched()
        expect(load).toHaveBeenCalledTimes(2)
        asks[1].resolve('a')
        await Promise.resolve(); await Promise.resolve()
        expect(entry.getSnapshot()).toMatchObject({ state: 'fresh', value: 'a' })
    })

    it('a reset made by a subscriber while the socket-open reload runs stops the reads that follow', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const a = loader<string>()
        const b = loader<string>()
        const keyed = jest.fn(async (keys: string[]) => keys.map((k) => `v:${k}`))
        const entryA = mirror.entry<string>({ method: 'agent.status', load: a.load })
        const entryB = mirror.entry<string>({ method: 'agent.get', load: b.load })
        const cache = mirror.cache<string>({ method: 'agent.byDIDs', options: () => ({ loadMany: keyed }) })
        let armed = false
        entryA.subscribe((snapshot) => { if (armed && snapshot.state === 'loading') mirror.reset() })
        entryB.subscribe(() => {})
        const off = cache.live('k').subscribe(() => {})
        await new Promise((r) => setTimeout(r, 0))
        keyed.mockClear()
        a.asks[0].reject(new Error('no socket'))
        b.asks[0].reject(new Error('no socket'))
        await new Promise((r) => setTimeout(r, 0))
        expect(entryA.getSnapshot().state).toBe('error')
        cache.markStale(false)
        armed = true
        mirror.reloadWatched()
        await new Promise((r) => setTimeout(r, 0))
        expect(a.load).toHaveBeenCalledTimes(2)
        expect(b.load).toHaveBeenCalledTimes(1)
        expect(keyed).not.toHaveBeenCalled()
        off()
    })

    it('an unwatched entry that failed is not read when a socket opens', async () => {
        const { mirror, entry, load, asks } = setup<string>()
        const read = entry.read()
        asks[0].reject(new Error('no socket'))
        await expect(read).rejects.toThrow('no socket')
        mirror.reloadWatched()
        expect(load).toHaveBeenCalledTimes(1)
    })

    it('a one-shot made right after reset() sends its own request', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const { load, asks } = loader<string>()
        const spec = () => mirror.oneShot({ perspective: 'p', method: 'perspective.querySparql', params: {}, request: load })
        const before = spec()
        before.catch(() => {})
        mirror.reset()
        asks[0].reject(new Error('Client closed'))
        const after = spec()
        expect(load).toHaveBeenCalledTimes(2)
        asks[1].resolve('fresh')
        expect(await after).toBe('fresh')
        await expect(before).rejects.toThrow('Client closed')
    })
})

describe('Mirror.refuse', () => {
    const refused = () => Object.assign(new Error('refused'), { status: 401 })

    it('a cache created after the refusal is refused too: its persistent tier is not read', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        mirror.refuse()
        const stored = { value: 'from store', ts: Date.now() }
        const persistent = { get: jest.fn(async () => stored), put: jest.fn(async () => {}), delete: jest.fn(async () => {}), clear: jest.fn(async () => {}) }
        const load = jest.fn(async () => { throw refused() })
        const cache = mirror.cache<string>({ method: 'agent.byDIDs', options: () => ({ loadMany: async () => [], load, persistent }) })
        await expect(cache.get('k')).rejects.toMatchObject({ status: 401 })
        expect(persistent.get).not.toHaveBeenCalled()
    })

    it('no value reads as fresh while the refusal notifies: a subscriber reading another entry or a cache key reaches the source', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const a = mirror.entry<string>({ method: 'a', load: async () => 'a' })
        const bLoad = jest.fn(async () => { if (bLoad.mock.calls.length > 1) throw refused(); return 'b' })
        const b = mirror.entry<string>({ method: 'b', load: bLoad })
        const cLoad = jest.fn(async () => { if (cLoad.mock.calls.length > 1) throw refused(); return 'c' })
        const cache = mirror.cache<string>({ method: 'c', options: () => ({ loadMany: async () => [], load: cLoad }) })
        await a.read()
        await b.read()
        await cache.get('k')
        b.subscribe(() => {})
        const reads: Promise<unknown>[] = []
        a.subscribe((s) => {
            if (s.state === 'stale') reads.push(b.read(), cache.get('k'))
        })
        mirror.refuse()
        expect(reads).toHaveLength(2)
        await expect(reads[0]).rejects.toMatchObject({ status: 401 })
        await expect(reads[1]).rejects.toMatchObject({ status: 401 })
    })

    it('a cache key read by a subscriber told earlier still tells its own subscribers on the refusal', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const a = mirror.entry<string>({ method: 'a', load: async () => 'a' })
        const cache = mirror.cache<string>({ method: 'c', options: () => ({ loadMany: async () => [], load: async () => 'c' }) })
        await a.read()
        await cache.get('k')
        const view = cache.live('k')
        const heard: string[] = []
        view.subscribe((s) => heard.push(s.state))
        heard.length = 0
        a.subscribe(() => view.getSnapshot())
        mirror.refuse()
        expect(heard).toEqual(['stale'])
    })

    it('a socket loss marks every entry stale before any subscriber is told', async () => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const load = jest.fn(async () => 'x')
        const a = mirror.entry<string>({ method: 'a', load })
        const b = mirror.entry<string>({ method: 'b', load })
        await a.read()
        await b.read()
        const seen: string[] = []
        a.subscribe(() => seen.push(b.getSnapshot().state))
        mirror.markAllStale(false)
        expect(seen).toEqual(['stale'])
    })
})

describe('Mirror: a reset or refusal during a drain', () => {
    // A and B are watched and fresh; the subscriber of A, told first, resets the mirror.
    async function twoWatched(act: (mirror: Mirror) => void) {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const bLoad = jest.fn(async () => 'b')
        const aLoad = jest.fn(async () => 'a')
        const a = mirror.entry<string>({ method: 'a', load: aLoad })
        const b = mirror.entry<string>({ method: 'b', load: bLoad })
        const cLoad = jest.fn(async (key: string) => key)
        const cache = mirror.cache<string>({ method: 'c', options: () => ({ loadMany: async (keys) => keys, load: cLoad }) })
        await a.read()
        await b.read()
        await cache.get('k')
        const heard: string[] = []
        b.subscribe((s) => heard.push(s.state))
        cache.live('k').subscribe((s) => heard.push(`k:${s.state}`))
        let acted = false
        a.subscribe((s) => { if (s.state === 'stale' && !acted) { acted = true; act(mirror) } })
        aLoad.mockClear()
        bLoad.mockClear()
        cLoad.mockClear()
        mirror.markAllStale()
        await Promise.resolve()
        return { aLoad, bLoad, cLoad, heard, b, cache }
    }

    it('a reset by a subscriber of the drain stops the reads the drain had queued, and the other subscribers still hear of the loss', async () => {
        const { aLoad, bLoad, cLoad, heard, b, cache } = await twoWatched((mirror) => mirror.reset())
        // The entry whose subscriber reset the mirror does not read either.
        expect(aLoad).not.toHaveBeenCalled()
        expect(bLoad).not.toHaveBeenCalled()
        expect(cLoad).not.toHaveBeenCalled()
        // Each subscriber hears once; the order between a value and a cache key is not part of the rule.
        expect([...heard].sort()).toEqual(['k:stale', 'stale'])
        expect(b.getSnapshot().state).toBe('stale')
        expect(cache.live('k').getSnapshot().state).toBe('stale')
    })

    it('a refusal by a subscriber of the drain stops the reads the drain had queued', async () => {
        const { aLoad, bLoad, cLoad } = await twoWatched((mirror) => mirror.refuse())
        expect(aLoad).not.toHaveBeenCalled()
        expect(bLoad).not.toHaveBeenCalled()
        expect(cLoad).not.toHaveBeenCalled()
    })

    it.each([
        ['reset', (mirror: Mirror) => mirror.reset()],
        ['refusal', (mirror: Mirror) => mirror.refuse()],
    ])('a %s by the subscriber of one watched cache key stops the read of the next', async (_, act) => {
        const mirror = new Mirror(() => '', () => {}, () => null)
        const loadMany = jest.fn(async (keys: string[]) => keys)
        const cache = mirror.cache<string>({ method: 'c', options: () => ({ loadMany }) })
        await cache.getMany(['k1', 'k2'])
        let acted = false
        cache.live('k1').subscribe((s) => { if (s.state === 'loading' && !acted) { acted = true; act(mirror) } })
        cache.live('k2').subscribe(() => {})
        loadMany.mockClear()
        mirror.markAllStale()
        await new Promise((r) => setTimeout(r, 0))
        expect(acted).toBe(true)
        expect(loadMany.mock.calls.flat(2)).not.toContain('k2')
    })

    it('with no reset in between, the drain still reads each watched value once', async () => {
        const { aLoad, bLoad, cLoad } = await twoWatched(() => {})
        expect(aLoad).toHaveBeenCalledTimes(1)
        expect(bLoad).toHaveBeenCalledTimes(1)
        expect(cLoad).toHaveBeenCalledTimes(1)
    })
})
