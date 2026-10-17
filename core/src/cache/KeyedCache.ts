import type { PersistentCache } from "./PersistentCache";

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
    /** Most keys per `loadMany` call. Default: no limit. */
    maxBatch?: number;
    /** Second tier, read on a memory miss (IndexedDB in browsers). */
    persistent?: PersistentCache<StoredEntry<V>>;
    /** How long a value stays fresh in memory, per key. Default: until evicted. */
    maxAgeMs?(key: string): number;
    /** How long a stored value stays fresh. Default: forever. */
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
 * Keyed read cache shared by profile and expression lookups. Concurrent reads
 * of one key share one request; misses of one tick go out in one `loadMany`
 * call. A `null` (not found) is returned but not kept.
 */
export class KeyedCache<V> {
    #opts: KeyedCacheOptions<V>;
    #values = new Map<string, { value: V; ts: number; size: number }>();
    #bytes = 0;
    #pending = new Map<string, Promise<V | null>>();
    #batch: { keys: string[]; done: Promise<Map<string, Loaded<V | Error>>> } | null = null;

    constructor(opts: KeyedCacheOptions<V>) {
        this.#opts = opts;
    }

    get(key: string): Promise<V | null> {
        return this.#read(key);
    }

    getMany(keys: string[]): Promise<Array<V | null>> {
        return Promise.all(keys.map(key => this.#read(key)));
    }

    /** Stores a value known to be current; a read in flight for the key is not kept. */
    set(key: string, value: V): void {
        this.#pending.delete(key);
        this.#remember(key, value, true);
    }

    /** Forgets a key in memory and in the persistent store. */
    delete(key: string): void {
        this.#pending.delete(key);
        this.#forget(key);
        this.#opts.persistent?.delete(key); // fire-and-forget
    }

    /** Forgets every key in memory, and in the persistent store when `persistent` is set. */
    clear(persistent = false): void {
        this.#pending.clear();
        this.#values.clear();
        this.#bytes = 0;
        if (persistent) this.#opts.persistent?.clear(); // fire-and-forget
    }

    #read(key: string): Promise<V | null> {
        const hit = this.#values.get(key);
        if (hit && Date.now() - hit.ts < (this.#opts.maxAgeMs?.(key) ?? Infinity)) return Promise.resolve(hit.value);
        const inFlight = this.#pending.get(key);
        if (inFlight) return inFlight;

        const loading = this.#enqueue(key);
        const promise: Promise<V | null> = loading.then(
            ({ value, stored }) => {
                // A set(), delete() or clear() since the read started wins over its result.
                if (this.#pending.get(key) === promise) {
                    this.#pending.delete(key);
                    if (value !== null) this.#remember(key, value, !stored);
                }
                return value;
            },
            (error) => {
                if (this.#pending.get(key) === promise) this.#pending.delete(key);
                throw error;
            },
        );
        this.#pending.set(key, promise);
        return promise;
    }

    #enqueue(key: string): Promise<Loaded<V>> {
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
            return entry as Loaded<V>;
        });
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
        const entry = await this.#opts.persistent?.get(key);
        // An entry without `value` was written in an older format: a miss.
        if (entry?.value == null || !(Date.now() - (entry.ts ?? 0) < (this.#opts.persistentMaxAgeMs?.() ?? Infinity))) return null;
        return { value: entry.value, stored: true };
    }

    #remember(key: string, value: V, persist: boolean): void {
        const limits = this.#opts.limits?.();
        const size = limits ? JSON.stringify(value).length : 0;
        this.#forget(key);
        if (limits) {
            for (const oldest of this.#values.keys()) {
                if (this.#values.size < limits.entries && this.#bytes + size <= limits.bytes) break;
                this.#forget(oldest);
            }
        }
        const ts = Date.now();
        this.#values.set(key, { value, ts, size });
        this.#bytes += size;
        if (persist) this.#opts.persistent?.put(key, { value, ts }); // fire-and-forget
    }

    #forget(key: string): void {
        const entry = this.#values.get(key);
        if (!entry) return;
        this.#bytes -= entry.size;
        this.#values.delete(key);
    }
}
