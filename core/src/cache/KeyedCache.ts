import type { PersistentCache } from "./PersistentCache";
import { canonical, type Live, type Snapshot, type State } from "../mirror/Entry";

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

interface Held<V> {
    value: V;
    ts: number;
    size: number;
    /** A reconnect, a lag marker or the persistent store made it stale: shown, but read again. */
    stale: boolean;
    /** No earlier value of the key was held. */
    initial: boolean;
}

/**
 * Keyed read cache shared by profile and expression lookups; the {@link Mirror}
 * owns one per RPC kind. Concurrent reads of one key share one request; misses of
 * one tick go out in one `loadMany` call, except a `get()` miss when the options
 * have `load`. A `null` (not found) is returned but not kept, and neither is a
 * failure. Each key has a {@link Live} view (`live`) with the mirror's states.
 */
export class KeyedCache<V> {
    #opts: KeyedCacheOptions<V>;
    #values = new Map<string, Held<V>>();
    #bytes = 0;
    #pending = new Map<string, Promise<V | null>>();
    #batch: { keys: string[]; done: Promise<Map<string, Loaded<V | Error>>> } | null = null;
    #errors = new Map<string, unknown>();
    #listeners = new Map<string, Set<(snapshot: Snapshot<V | null>) => void>>();
    #snapshots = new Map<string, Snapshot<V | null>>();
    /** Keys whose stored value was already used this session (see `persistentStale`). */
    #storedUsed = new Set<string>();

    constructor(opts: KeyedCacheOptions<V>) {
        this.#opts = opts;
    }

    get(key: string): Promise<V | null> {
        const load = this.#opts.load;
        return this.#read(key, load ? k => this.#loadOne(k, load) : k => this.#enqueue(k));
    }

    getMany(keys: string[]): Promise<Array<V | null>> {
        return Promise.all(keys.map(key => this.#read(key, k => this.#enqueue(k))));
    }

    /** Stores a value known to be current; a read in flight for the key is not kept. */
    set(key: string, value: V): void {
        this.#pending.delete(key);
        this.#errors.delete(key);
        this.#remember(key, value, true);
    }

    /** Forgets a key in memory and in the persistent store. */
    delete(key: string): void {
        this.#pending.delete(key);
        this.#errors.delete(key);
        this.#forget(key);
        this.#opts.persistent?.delete(key); // fire-and-forget
        this.#notify(key);
    }

    /** Forgets every key in memory, and in the persistent store when `persistent` is set. */
    clear(persistent = false): void {
        const keys = new Set([...this.#values.keys(), ...this.#pending.keys(), ...this.#errors.keys()]);
        this.#pending.clear();
        this.#errors.clear();
        this.#values.clear();
        this.#bytes = 0;
        if (persistent) this.#opts.persistent?.clear(); // fire-and-forget
        keys.forEach(key => this.#notify(key));
    }

    /**
     * The executor may have changed anything: every held value is stale (still shown, read
     * again by the next read), and a read in flight is not kept. Keys with subscribers
     * are read again at once, the misses of this tick in one call.
     */
    markStale(): void {
        const keys = new Set([...this.#values.keys(), ...this.#pending.keys()]);
        this.#pending.clear();
        for (const held of this.#values.values()) held.stale = true;
        keys.forEach(key => this.#notify(key));
        for (const key of keys) if (this.#listeners.get(key)?.size) this.#refetch(key);
    }

    /** A view on one key: `getSnapshot`, `subscribe`, `read`. A subscriber to a key that is not fresh makes it load. */
    live(key: string): Live<V | null> {
        return {
            getSnapshot: () => this.#snapshot(key),
            subscribe: (fn) => {
                let set = this.#listeners.get(key);
                if (!set) this.#listeners.set(key, (set = new Set()));
                set.add(fn);
                const state = this.#snapshot(key).state;
                if (state !== 'fresh' && state !== 'loading') this.#refetch(key);
                return () => {
                    set!.delete(fn);
                    if (set!.size === 0) this.#listeners.delete(key);
                };
            },
            read: () => this.get(key),
        };
    }

    #isFresh(key: string, held: Held<V> | undefined): held is Held<V> {
        return !!held && !held.stale && Date.now() - held.ts < (this.#opts.maxAgeMs?.(key) ?? Infinity);
    }

    #snapshot(key: string): Snapshot<V | null> {
        let snapshot = this.#snapshots.get(key);
        if (!snapshot) {
            const held = this.#values.get(key);
            const state: State = this.#pending.has(key) ? 'loading'
                : held ? (this.#isFresh(key, held) ? 'fresh' : 'stale')
                : this.#errors.has(key) ? 'error' : 'unknown';
            snapshot = { state, value: held?.value ?? undefined, error: this.#errors.get(key), initial: !!held?.initial };
            this.#snapshots.set(key, snapshot);
        }
        return snapshot;
    }

    #notify(key: string): void {
        this.#snapshots.delete(key);
        const listeners = this.#listeners.get(key);
        if (!listeners?.size) return;
        const snapshot = this.#snapshot(key);
        for (const fn of [...listeners]) {
            try { fn(snapshot); } catch (e) { console.error('Error in mirror subscriber:', e); }
        }
    }

    #refetch(key: string): void {
        this.get(key).catch(() => {});
    }

    #read(key: string, fetch: (key: string) => Promise<Loaded<V>>): Promise<V | null> {
        const hit = this.#values.get(key);
        if (this.#isFresh(key, hit)) return Promise.resolve(hit.value);
        const inFlight = this.#pending.get(key);
        if (inFlight) return inFlight;

        const loading = fetch(key);
        const promise: Promise<V | null> = loading.then(
            ({ value, stored }) => {
                // A set(), delete(), clear() or markStale() since the read started wins over its result.
                if (this.#pending.get(key) === promise) {
                    this.#pending.delete(key);
                    this.#errors.delete(key);
                    if (value !== null) {
                        const stale = stored && !!this.#opts.persistentStale;
                        this.#remember(key, value, !stored, stale);
                        if (stale) this.#refetch(key);
                    } else {
                        this.#notify(key);
                    }
                }
                return value;
            },
            (error) => {
                if (this.#pending.get(key) === promise) {
                    this.#pending.delete(key);
                    this.#errors.set(key, error);
                    this.#notify(key);
                }
                throw error;
            },
        );
        this.#pending.set(key, promise);
        this.#notify(key);
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

    async #loadOne(key: string, load: (key: string) => Promise<V | null>): Promise<Loaded<V>> {
        return (await this.#stored(key)) ?? { value: await load(key), stored: false };
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

    #remember(key: string, value: V, persist: boolean, stale = false): void {
        const limits = this.#opts.limits?.();
        const size = limits ? JSON.stringify(value).length : 0;
        const before = this.#values.get(key);
        const same = !!before && !before.stale && !stale && this.#listeners.has(key) && canonical(before.value) === canonical(value);
        this.#forget(key);
        if (limits) {
            for (const oldest of this.#values.keys()) {
                if (this.#values.size < limits.entries && this.#bytes + size <= limits.bytes) break;
                this.#forget(oldest);
                this.#notify(oldest);
            }
        }
        const ts = Date.now();
        // An equal value stays the same object: subscribers compare snapshots by identity.
        this.#values.set(key, { value: same ? before!.value : value, ts, size, stale, initial: !before });
        this.#bytes += size;
        if (persist) this.#opts.persistent?.put(key, { value, ts }); // fire-and-forget
        if (!same) this.#notify(key);
    }

    #forget(key: string): void {
        const entry = this.#values.get(key);
        if (!entry) return;
        this.#bytes -= entry.size;
        this.#values.delete(key);
    }
}
