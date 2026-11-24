import type { PersistentCache } from "./PersistentCache";
import { Entry, type Kind, type Live } from "../mirror/Entry";

/** What a {@link KeyedCache} writes to its persistent store. */
export interface StoredEntry<V> {
    value: V;
    ts: number;
}

export interface KeyedCacheOptions<V> {
    /**
     * Reads many keys in one call: one entry per key, in order, `null` when
     * not found, an `Error` when that key's read failed (it rejects that key's
     * readers only).
     */
    loadMany(keys: string[]): Promise<Array<V | null | Error>>;
    /**
     * Reads one key (`null` when not found; rejects when the read failed).
     * When set, a miss of {@link KeyedCache.get} is read alone with it, so it
     * waits for no other key; only {@link KeyedCache.getMany} uses `loadMany`.
     */
    load?(key: string): Promise<V | null>;
    /** Most keys per `loadMany` call. Default: no limit. */
    maxBatch?: number;
    /** Second tier, read on a memory miss (IndexedDB in browsers). */
    persistent?: PersistentCache<StoredEntry<V>>;
    /**
     * A value read from the persistent store is `stale`: the read returns it, and the key
     * is read once from the source in the background (once per key and cache). Default:
     * a stored value is as fresh as one just read.
     */
    persistentStale?: boolean;
    /** How long a value stays fresh in memory, per key. Default: until evicted. */
    maxAgeMs?(key: string): number;
    /** How long a stored value is used. Default: forever. */
    persistentMaxAgeMs?(): number;
    /** Memory limits; the oldest values go first. Default: none. */
    limits?(): { entries: number; bytes: number };
}

interface Loaded<V> {
    value: V | null;
    /** Came from the persistent store, so it is not written back. */
    stored: boolean;
}

/**
 * Keyed read cache shared by profile and expression lookups; the {@link Mirror}
 * owns one per RPC kind. Each key is an {@link Entry} (the mirror's states, keep rule
 * and views); this class adds what is specific to a keyed store: concurrent reads of one
 * key share one request, misses of one tick go out in one `loadMany` call (except a `get()`
 * miss when the options have `load`), `maxAgeMs`, memory limits and the persistent tier.
 * A `null` (not found) is returned but not kept, and neither is a failure.
 */
export class KeyedCache<V> {
    #opts: KeyedCacheOptions<V>;
    #kind: Kind;
    #entries = new Map<string, Entry<V | null>>();
    /** Keys that hold a value, oldest first, with the size of its JSON. */
    #sizes = new Map<string, number>();
    #bytes = 0;
    #batch: { keys: string[]; done: Promise<Map<string, Loaded<V | Error>>> } | null = null;
    /** Keys whose stored value was already used this session (see `persistentStale`). */
    #storedUsed = new Set<string>();
    /** Keys whose value just came from the persistent store, so it is not written back. */
    #fromStore = new Set<string>();

    constructor(opts: KeyedCacheOptions<V>, kind: 'immutable' | 'declared' = 'immutable') {
        this.#opts = opts;
        this.#kind = kind;
    }

    get(key: string): Promise<V | null> {
        return this.#read(key, this.#loader(key));
    }

    getMany(keys: string[]): Promise<Array<V | null>> {
        return Promise.all(keys.map(key => this.#read(key, () => this.#enqueue(key))));
    }

    /** Stores a value known to be current; a read in flight for the key is not kept. */
    set(key: string, value: V): void {
        this.#fromStore.delete(key);
        this.#entry(key).push(value);
    }

    /** Forgets a key in memory and in the persistent store. */
    delete(key: string): void {
        this.#drop(key);
        this.#opts.persistent?.delete(key); // fire-and-forget
    }

    /** Forgets every key in memory, and in the persistent store when `persistent` is set. */
    clear(persistent = false): void {
        for (const key of [...this.#entries.keys()]) this.#drop(key);
        if (persistent) this.#opts.persistent?.clear(); // fire-and-forget
    }

    /**
     * The executor may have changed anything: every held value is stale (still shown, read
     * again by the next read), and a read in flight is not kept. Keys with subscribers
     * are read again at once, the misses of this tick in one call.
     */
    markStale(): void {
        for (const entry of [...this.#entries.values()]) entry.invalidate();
    }

    /** A view on one key: `getSnapshot`, `subscribe`, `read`. A subscriber to a key that is not fresh makes it load. */
    live(key: string): Live<V | null> {
        return {
            getSnapshot: () => this.#entry(key).getSnapshot(),
            subscribe: (fn) => this.#entry(key).subscribe(fn),
            read: () => this.get(key),
        };
    }

    #entry(key: string): Entry<V | null> {
        let entry = this.#entries.get(key);
        if (!entry) {
            entry = new Entry<V | null>(this.#kind, this.#loader(key), {
                keep: value => value !== null,
                maxAgeMs: () => this.#opts.maxAgeMs?.(key) ?? Infinity,
                kept: value => this.#kept(key, value as V),
            });
            this.#entries.set(key, entry);
        }
        return entry;
    }

    /** How the entry of `key` reads itself: alone with `load`, else with the tick's batch. */
    #loader(key: string): () => Promise<V | null> {
        const load = this.#opts.load;
        return load ? () => this.#loadOne(key, load) : () => this.#enqueue(key);
    }

    #read(key: string, load: () => Promise<V | null>): Promise<V | null> {
        const reading = this.#entry(key).read(load);
        const prune = () => this.#prune(key);
        reading.then(prune, () => {});
        return reading;
    }

    /** An entry with nothing to show and no subscriber is not worth keeping. */
    #prune(key: string): void {
        const entry = this.#entries.get(key);
        if (entry && !entry.hasListeners && entry.getSnapshot().state === 'unknown') this.#entries.delete(key);
    }

    /** Forgets the value of a key and its place in the memory limits. */
    #drop(key: string): void {
        this.#forget(key);
        this.#fromStore.delete(key);
        this.#entries.get(key)?.reset();
        this.#prune(key);
    }

    #forget(key: string): void {
        this.#bytes -= this.#sizes.get(key) ?? 0;
        this.#sizes.delete(key);
    }

    /** The entry took a value: write it to the persistent store (unless it came from there) and apply the limits. */
    #kept(key: string, value: V): void {
        if (!this.#fromStore.delete(key)) this.#opts.persistent?.put(key, { value, ts: Date.now() }); // fire-and-forget
        const limits = this.#opts.limits?.();
        const size = limits ? JSON.stringify(value).length : 0;
        this.#forget(key);
        if (limits) {
            for (const oldest of this.#sizes.keys()) {
                if (this.#sizes.size < limits.entries && this.#bytes + size <= limits.bytes) break;
                this.#drop(oldest);
            }
        }
        this.#sizes.set(key, size);
        this.#bytes += size;
    }

    /**
     * What a read answers with. A value from the persistent store is not written back; when
     * `persistentStale` it is shown stale and the key is read once from the source.
     */
    #take(key: string, { value, stored }: Loaded<V>): V | null {
        const entry = this.#entries.get(key);
        // A set(), delete(), clear() or markStale() since the read started wins over its result.
        if (!entry || entry.getSnapshot().state !== 'loading') return value;
        if (stored && value !== null) {
            this.#fromStore.add(key);
            if (this.#opts.persistentStale) {
                entry.seed(value);
                entry.read().catch(() => {});
            }
        } else {
            this.#fromStore.delete(key);
        }
        return value;
    }

    #enqueue(key: string): Promise<V | null> {
        if (!this.#batch) {
            const keys: string[] = [];
            const done = Promise.resolve().then(() => {
                this.#batch = null;
                return this.#loadBatch(keys);
            });
            this.#batch = { keys, done };
        }
        this.#batch.keys.push(key);
        return this.#batch.done.then(loaded => {
            const entry = loaded.get(key)!;
            if (entry.value instanceof Error) throw entry.value;
            return this.#take(key, entry as Loaded<V>);
        });
    }

    async #loadOne(key: string, load: (key: string) => Promise<V | null>): Promise<V | null> {
        return this.#take(key, (await this.#stored(key)) ?? { value: await load(key), stored: false });
    }

    async #loadBatch(keys: string[]): Promise<Map<string, Loaded<V | Error>>> {
        const loaded = new Map<string, Loaded<V | Error>>();
        const stored = await Promise.all(keys.map(key => this.#stored(key)));
        const misses = keys.filter((key, i) => {
            if (stored[i]) loaded.set(key, stored[i]!);
            return !stored[i];
        });
        const size = this.#opts.maxBatch ?? Infinity;
        const chunks: string[][] = [];
        for (let i = 0; i < misses.length; i += size) chunks.push(misses.slice(i, i + size));
        await Promise.all(chunks.map(async chunk => {
            const values = await this.#opts.loadMany(chunk);
            chunk.forEach((key, i) => loaded.set(key, { value: values[i] ?? null, stored: false }));
        }));
        return loaded;
    }

    async #stored(key: string): Promise<Loaded<V> | null> {
        if (this.#opts.persistentStale) {
            if (this.#storedUsed.has(key)) return null;
            this.#storedUsed.add(key);
        }
        const entry = await this.#opts.persistent?.get(key);
        // An entry without `value` was written in an older format: a miss.
        if (entry?.value == null || !(Date.now() - (entry.ts ?? 0) < (this.#opts.persistentMaxAgeMs?.() ?? Infinity))) return null;
        return { value: entry.value, stored: true };
    }
}
