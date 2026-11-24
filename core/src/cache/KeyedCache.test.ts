import { KeyedCache, StoredEntry } from './KeyedCache';
import { PersistentCache } from './PersistentCache';

async function until(condition: () => boolean): Promise<void> {
    const deadline = Date.now() + 2000;
    while (!condition()) {
        if (Date.now() > deadline) throw new Error('condition not met within 2s');
        await new Promise(r => setTimeout(r, 1));
    }
}

function deferred<T>() {
    let resolve!: (v: T) => void;
    const promise = new Promise<T>(r => { resolve = r; });
    return { promise, resolve };
}

class MapStore<V> implements PersistentCache<StoredEntry<V>> {
    entries = new Map<string, StoredEntry<V>>();
    puts: string[] = [];
    async get(key: string) { return this.entries.get(key); }
    async put(key: string, value: StoredEntry<V>) { this.puts.push(key); this.entries.set(key, value); }
    async delete(key: string) { this.entries.delete(key); }
    async clear() { this.entries.clear(); }
}

describe('KeyedCache', () => {
    test('get() and getMany() misses of one tick share one loadMany; a later read is a hit', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys.map(k => k.toUpperCase()));
        const cache = new KeyedCache<string>({ loadMany });
        const [a, many] = await Promise.all([cache.get('a'), cache.getMany(['b', 'a'])]);
        expect(a).toBe('A');
        expect(many).toEqual(['B', 'A']);
        expect(loadMany.mock.calls).toEqual([[['a', 'b']]]);
        expect(await cache.get('b')).toBe('B');
        expect(loadMany).toHaveBeenCalledTimes(1);
    });

    test('with load, a get() miss is read alone and settles without waiting for a slower key', async () => {
        const slow = deferred<string>();
        const load = jest.fn((key: string) => key === 'slow' ? slow.promise : Promise.resolve(key.toUpperCase()));
        const loadMany = jest.fn(async (keys: string[]) => keys.map(k => k.toUpperCase()));
        const cache = new KeyedCache<string>({ load, loadMany });
        const slowRead = cache.get('slow');
        expect(await cache.get('fast')).toBe('FAST');
        // A getMany() shares the get() in flight, and only its other misses go to loadMany.
        const many = cache.getMany(['fast', 'slow', 'b']);
        slow.resolve('SLOW');
        expect(await slowRead).toBe('SLOW');
        expect(await many).toEqual(['FAST', 'SLOW', 'B']);
        expect(load.mock.calls).toEqual([['slow'], ['fast']]);
        expect(loadMany.mock.calls).toEqual([[['b']]]);
    });

    test('with load, a get() miss is served from the persistent store before load', async () => {
        const persistent = new MapStore<string>();
        persistent.entries.set('a', { value: 'stored', ts: Date.now() });
        const load = jest.fn(async (key: string) => key);
        const cache = new KeyedCache<string>({ load, loadMany: async keys => keys, persistent });
        expect(await cache.get('a')).toBe('stored');
        expect(load).not.toHaveBeenCalled();
        expect(persistent.puts).toEqual([]);
    });

    test('with load, a failed get() rejects that read only, and the next get() retries', async () => {
        const load = jest.fn().mockRejectedValueOnce(new Error('boom')).mockResolvedValue('ok');
        const cache = new KeyedCache<string>({ load, loadMany: async keys => keys });
        await expect(cache.get('a')).rejects.toThrow('boom');
        expect(await cache.get('a')).toBe('ok');
        expect(load).toHaveBeenCalledTimes(2);
    });

    test('maxBatch splits one tick of misses into calls of at most that size', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys);
        const cache = new KeyedCache<string>({ loadMany, maxBatch: 2 });
        expect(await cache.getMany(['a', 'b', 'c', 'd', 'e'])).toEqual(['a', 'b', 'c', 'd', 'e']);
        expect(loadMany.mock.calls.map(([keys]) => keys)).toEqual([['a', 'b'], ['c', 'd'], ['e']]);
    });

    test('null (not found) is returned but not kept', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys.map(() => null));
        const cache = new KeyedCache<string>({ loadMany });
        expect(await cache.get('x')).toBeNull();
        expect(await cache.get('x')).toBeNull();
        expect(loadMany).toHaveBeenCalledTimes(2);
    });

    test('a subscribed key whose read answers null is fresh with a null value; get() still asks again', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys.map(() => null));
        const cache = new KeyedCache<string>({ loadMany });
        const live = cache.live('x');
        expect(live.getSnapshot()).toMatchObject({ state: 'unknown', value: undefined });
        const seen: string[] = [];
        const off = live.subscribe(s => seen.push(`${s.state}:${s.value}`));
        await until(() => live.getSnapshot().state === 'fresh');
        expect(live.getSnapshot()).toMatchObject({ state: 'fresh', value: null });
        expect(seen).toEqual(['loading:undefined', 'fresh:null']);
        expect(loadMany).toHaveBeenCalledTimes(1);
        expect(await live.read()).toBeNull();
        expect(loadMany).toHaveBeenCalledTimes(2);
        // A set() replaces the null, a markStale() makes it load again.
        cache.set('x', 'there');
        expect(live.getSnapshot()).toMatchObject({ state: 'fresh', value: 'there' });
        off();
    });

    test('a not-found key keeps nothing once it is no longer watched', async () => {
        const persistent = new MapStore<string>();
        const cache = new KeyedCache<string>({
            loadMany: async keys => keys.map(() => null),
            persistent,
            persistentStale: true,
            limits: () => ({ entries: 3, bytes: 1_000 }),
        });
        for (let i = 0; i < 50; i++) {
            const live = cache.live(`k${i}`);
            const off = live.subscribe(() => {});
            await until(() => live.getSnapshot().state === 'fresh');
            expect(live.getSnapshot().value).toBeNull();
            off();
        }
        for (let i = 0; i < 50; i++) await cache.get(`g${i}`);
        // Nothing is held for them: each key is back to unknown.
        for (let i = 0; i < 50; i++) {
            expect(cache.live(`k${i}`).getSnapshot().state).toBe('unknown');
            expect(cache.live(`g${i}`).getSnapshot().state).toBe('unknown');
        }
    });

    test('a failed key keeps its error while it is watched; the last unsubscribe drops it', async () => {
        const cache = new KeyedCache<string>({ loadMany: async keys => keys.map(() => new Error('boom')) });
        const live = cache.live('k');
        const off = live.subscribe(() => {});
        await until(() => live.getSnapshot().state === 'error');
        off();
        expect(live.getSnapshot()).toMatchObject({ state: 'unknown', error: undefined });
    });

    test('a failed refresh of a key with a value keeps its error only while watched', async () => {
        let fail = false;
        const cache = new KeyedCache<string>({ loadMany: async keys => keys.map(() => (fail ? new Error('boom') : 'v')) });
        const live = cache.live('k');
        const off = live.subscribe(() => {});
        await until(() => live.getSnapshot().state === 'fresh');
        fail = true;
        cache.markStale();
        await until(() => live.getSnapshot().state === 'error');
        off();
        expect(live.getSnapshot()).toMatchObject({ state: 'stale', value: 'v', error: undefined });
    });

    test('markStale(false) sends no read for a watched key; reloadWatched() reads it once', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys.map(() => 'v'));
        const cache = new KeyedCache<string>({ loadMany });
        const live = cache.live('k');
        const off = live.subscribe(() => {});
        await until(() => live.getSnapshot().state === 'fresh');
        cache.markStale(false);
        await new Promise(r => setTimeout(r, 20));
        expect(loadMany).toHaveBeenCalledTimes(1);
        expect(live.getSnapshot().state).toBe('stale');
        cache.reloadWatched();
        cache.reloadWatched();
        await until(() => live.getSnapshot().state === 'fresh');
        expect(loadMany).toHaveBeenCalledTimes(2);
        off();
    });

    test('a failed read of an unwatched key keeps no error', async () => {
        const cache = new KeyedCache<string>({ loadMany: async keys => keys.map(() => new Error('boom')) });
        for (let i = 0; i < 20; i++) await expect(cache.get(`k${i}`)).rejects.toThrow('boom');
        for (let i = 0; i < 20; i++) expect(cache.live(`k${i}`).getSnapshot()).toMatchObject({ state: 'unknown', error: undefined });
    });

    test('a refresh that finds nothing drops the value held, and the stored one', async () => {
        const persistent = new MapStore<string>();
        let answer: string | null = 'there';
        const loadMany = jest.fn(async (keys: string[]) => keys.map(() => answer));
        // Age 0: a reload reads the source, not the stored value.
        const cache = new KeyedCache<string>({ loadMany, persistent, persistentMaxAgeMs: () => 0 });
        const live = cache.live('k');
        const off = live.subscribe(() => {});
        await until(() => live.getSnapshot().state === 'fresh');
        expect(live.getSnapshot().value).toBe('there');
        expect(persistent.entries.has('k')).toBe(true);

        answer = null;
        cache.markStale();
        await until(() => loadMany.mock.calls.length === 2 && live.getSnapshot().state === 'fresh');
        expect(live.getSnapshot()).toMatchObject({ state: 'fresh', value: null });
        expect(persistent.entries.has('k')).toBe(false);
        off();
        expect(await cache.get('k')).toBeNull();
    });

    test('a refresh that finds nothing clears the error of an earlier failure', async () => {
        const answers: (string | null | Error)[] = [new Error('boom'), null];
        const cache = new KeyedCache<string>({ loadMany: async keys => keys.map(() => answers.shift()!) });
        const live = cache.live('k');
        const off = live.subscribe(() => {});
        await until(() => live.getSnapshot().state === 'error');
        await live.read().catch(() => {});
        await until(() => live.getSnapshot().state === 'fresh');
        expect(live.getSnapshot()).toMatchObject({ value: null, error: undefined });
        off();
    });

    test('a set() while a read is in flight wins over the read', async () => {
        const reply = deferred<string[]>();
        const cache = new KeyedCache<string>({ loadMany: () => reply.promise });
        const reading = cache.get('a');
        await Promise.resolve();
        cache.set('a', 'fresh');
        reply.resolve(['stale']);
        expect(await reading).toBe('stale');
        expect(await cache.get('a')).toBe('fresh');
    });

    test('a delete() while a read is in flight keeps the read out of the cache', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys);
        const cache = new KeyedCache<string>({ loadMany });
        const reading = cache.get('a');
        cache.delete('a');
        await reading;
        await cache.get('a');
        expect(loadMany).toHaveBeenCalledTimes(2);
    });

    test('a clear() while a read is in flight keeps the read out of the cache', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys);
        const cache = new KeyedCache<string>({ loadMany });
        const reading = cache.get('a');
        cache.clear();
        await reading;
        await cache.get('a');
        expect(loadMany).toHaveBeenCalledTimes(2);
    });

    test('maxAgeMs expires a value per key', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys);
        const cache = new KeyedCache<string>({ loadMany, maxAgeMs: k => (k === 'keep' ? Infinity : 0) });
        await cache.getMany(['keep', 'drop']);
        await cache.getMany(['keep', 'drop']);
        expect(loadMany.mock.calls.map(([keys]) => keys)).toEqual([['keep', 'drop'], ['drop']]);
    });

    test('limits evict the oldest values first, by count and by JSON size', async () => {
        let limits = { entries: 2, bytes: Infinity };
        const loadMany = jest.fn(async (keys: string[]) => keys);
        const cache = new KeyedCache<string>({ loadMany, limits: () => limits });
        await cache.getMany(['a', 'b']);
        await cache.get('c'); // evicts a
        await cache.getMany(['b', 'c']);
        expect(loadMany).toHaveBeenCalledTimes(2);
        await cache.get('a');
        expect(loadMany).toHaveBeenCalledTimes(3);

        limits = { entries: 100, bytes: 6 }; // each value is 3 bytes of JSON ("x")
        cache.clear();
        await cache.getMany(['x', 'y']);
        await cache.get('z'); // evicts x
        await cache.get('x');
        expect(loadMany.mock.calls.slice(3).map(([keys]) => keys)).toEqual([['x', 'y'], ['z'], ['x']]);
    });

    test('a watched key is not evicted: its view keeps its value, and the oldest unwatched key goes', async () => {
        const loadMany = jest.fn(async (keys: string[]) => keys);
        const cache = new KeyedCache<string>({ loadMany, limits: () => ({ entries: 2, bytes: Infinity }) });
        const a = cache.live('a');
        const off = a.subscribe(() => {});
        await until(() => a.getSnapshot().state === 'fresh');
        await cache.get('b');
        await cache.get('c'); // over the limit: b goes, not the watched a
        expect(a.getSnapshot()).toMatchObject({ state: 'fresh', value: 'a' });
        await cache.get('a');
        await cache.get('b');
        expect(loadMany.mock.calls.map(([keys]) => keys)).toEqual([['a'], ['b'], ['c'], ['b']]);
        off();
    });

    test('the persistent tier serves fresh entries without a load and is not rewritten by them', async () => {
        const store = new MapStore<string>();
        store.entries.set('fresh', { value: 'stored', ts: Date.now() });
        store.entries.set('stale', { value: 'old', ts: Date.now() - 10_000 });
        store.entries.set('legacy', { agent: 'old format' } as any);
        const loadMany = jest.fn(async (keys: string[]) => keys.map(k => `loaded:${k}`));
        const cache = new KeyedCache<string>({ loadMany, persistent: store, persistentMaxAgeMs: () => 5_000 });

        expect(await cache.getMany(['fresh', 'stale', 'legacy'])).toEqual(['stored', 'loaded:stale', 'loaded:legacy']);
        expect(loadMany.mock.calls).toEqual([[['stale', 'legacy']]]);
        expect(store.puts.sort()).toEqual(['legacy', 'stale']);
        expect(store.entries.get('stale')!.value).toBe('loaded:stale');
    });

    test('clear() keeps the persistent tier unless asked', async () => {
        const store = new MapStore<string>();
        const cache = new KeyedCache<string>({ loadMany: async keys => keys, persistent: store });
        await cache.get('a');
        await new Promise(r => setTimeout(r, 0));
        cache.clear();
        expect(store.entries.has('a')).toBe(true);
        cache.clear(true);
        expect(store.entries.has('a')).toBe(false);
    });
});

describe('KeyedCache review round', () => {
    test('the limits apply again when a key loses its last subscriber', async () => {
        const cache = new KeyedCache<string>({
            loadMany: async keys => keys.map(k => k.toUpperCase()),
            limits: () => ({ entries: 2, bytes: 1_000 }),
        });
        const offs = ['a', 'b', 'c'].map(k => cache.live(k).subscribe(() => {}));
        await until(() => ['a', 'b', 'c'].every(k => cache.live(k).getSnapshot().state === 'fresh'));
        // Watched keys are never evicted: three held for a limit of two.
        offs.forEach(off => off());
        const held = ['a', 'b', 'c'].filter(k => cache.live(k).getSnapshot().state === 'fresh');
        expect(held).toEqual(['b', 'c']);
    });

    test('a value restored from the persistent tier keeps its stored age', async () => {
        const store = new MapStore<string>();
        const now = Date.now();
        store.entries.set('k', { value: 'stored', ts: now - 299_000 });
        const loadMany = jest.fn(async (keys: string[]) => keys.map(k => `loaded:${k}`));
        const cache = new KeyedCache<string>({
            loadMany, persistent: store,
            maxAgeMs: () => 300_000, persistentMaxAgeMs: () => 300_000,
        });
        expect(await cache.get('k')).toBe('stored');
        const spy = jest.spyOn(Date, 'now').mockReturnValue(now + 2_000);
        try {
            expect(await cache.get('k')).toBe('loaded:k');
        } finally {
            spy.mockRestore();
        }
    });

    test('markStale() on an unwatched miss waiting for the persistent tier releases its once-per-session marker', async () => {
        const gate = deferred<void>();
        const store = new MapStore<string>();
        const gets: string[] = [];
        const get = store.get.bind(store);
        store.get = async (key: string) => { gets.push(key); await gate.promise; return get(key); };
        const cache = new KeyedCache<string>({
            loadMany: async keys => keys.map(k => `loaded:${k}`),
            persistent: store, persistentStale: true,
        });
        const first = cache.get('a');
        await until(() => gets.length === 1);
        cache.markStale();
        gate.resolve();
        await first;
        await cache.get('a');
        expect(gets).toEqual(['a', 'a']);
    });

    test('a retry of a failed watched key shows no error while it loads', async () => {
        let fail = true;
        const load = jest.fn(async (key: string) => { if (fail) throw new Error('down'); return key; });
        const cache = new KeyedCache<string>({ loadMany: async keys => keys, load });
        const live = cache.live('k');
        const off = live.subscribe(() => {});
        await until(() => live.getSnapshot().state === 'error');
        fail = false;
        const retry = cache.get('k');
        expect(live.getSnapshot()).toMatchObject({ state: 'loading', error: undefined });
        await retry;
        off();
    });

    test('refuse() makes every value stale and blocks the persistent tier, so the next read reaches the source', async () => {
        const store = new MapStore<string>();
        store.entries.set('stored', { value: 'from store', ts: Date.now() });
        const load = jest.fn(async (key: string) => `loaded:${key}`);
        const cache = new KeyedCache<string>({ loadMany: async keys => keys, load, persistent: store });
        expect(await cache.get('stored')).toBe('from store');
        await cache.get('other');
        cache.refuse();
        expect(load).toHaveBeenCalledTimes(1);
        expect(await cache.get('other')).toBe('loaded:other');
        cache.delete('stored');
        store.entries.set('stored', { value: 'from store', ts: Date.now() });
        expect(await cache.get('stored')).toBe('loaded:stored');
    });
});
