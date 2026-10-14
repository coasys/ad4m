import { KeyedCache, StoredEntry } from './KeyedCache';
import { PersistentCache } from './PersistentCache';

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

    test('get() uses loadOne when given; getMany() still uses loadMany', async () => {
        const loadOne = jest.fn(async (k: string) => `one:${k}`);
        const loadMany = jest.fn(async (keys: string[]) => keys.map(k => `many:${k}`));
        const cache = new KeyedCache<string>({ loadOne, loadMany });
        expect(await cache.get('a')).toBe('one:a');
        expect(await cache.getMany(['a', 'b'])).toEqual(['one:a', 'many:b']);
        expect(loadMany.mock.calls).toEqual([[['b']]]);
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
