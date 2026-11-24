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

/** A value the cache holds; only found values are held. */
interface Slot<V> {
    value: V;
    ts: number;
    /** Shown, but read again by the next read. */
    stale: boolean;
    /** Length of the value's JSON, when the cache has memory limits. */
    size: number;
}

/** The snapshot of a key nothing is known of. */
const UNKNOWN: Snapshot<unknown> = { state: 'unknown', value: undefined, error: undefined };

/** A read in flight; `valid` goes false when something changed the key since it went out. */
interface Flight<V> {
    valid: boolean;
    promise: Promise<V | null>;
}

/**
 * Keyed read cache shared by profile and expression lookups; the {@link Mirror} owns one
 * per RPC. It holds the rules of a keyed store, around plain values:
 * - concurrent reads of one key share one request, and the misses of one tick go out in
 *   one `loadMany` call (except a `get()` miss when the options have `load`);
 * - `maxAgeMs`: a value older than that is stale, nobody is told, the next read loads;
 * - `limits`: the oldest values go first;
 * - the persistent tier (`persistent`, `persistentMaxAgeMs`, `persistentStale`);
 * - a `null` (not found) is returned but not kept for {@link get} (the next read asks again), and
 *   neither is a failure; a key that has subscribers shows the `null` as `fresh`;
 * - an answer is kept only when nothing changed the key since its request went out
 *   (`set`, `delete`, `clear`, `markStale`): then it reaches its readers once and is dropped.
 * {@link live} gives a view on one key with the mirror's states, derived from what the cache
 * holds; a key keeps a snapshot, an error or a not-found marker only while something is
 * held or watched for it. A watched key is never evicted.
 */
export class KeyedCache<V> {
    #opts: KeyedCacheOptions<V>;
    /** Held values, oldest first. */
    #slots = new Map<string, Slot<V>>();
    #bytes = 0;
    #flights = new Map<string, Flight<V>>();
    #errors = new Map<string, unknown>();
    #listeners = new Map<string, Set<(snapshot: Snapshot<V | null>) => void>>();
    /** Snapshots handed out, kept for identity; only for keys with a slot, a read, an error or a listener. */
    #snapshots = new Map<string, Snapshot<V | null>>();
    /** Keys with a listener whose last read answered `null`: their snapshot is `fresh` with a `null` value. */
    #missing = new Set<string>();
    #batch: { keys: string[]; done: Promise<Map<string, Loaded<V | Error>>> } | null = null;
    /**
     * Keys whose stored value was already used this session (see `persistentStale`). Only
     * keys with a slot or a read in flight: a key that leaves the slots (not found, failed,
     * evicted, deleted) leaves this set.
     */
    #storedUsed = new Set<string>();

    constructor(opts: KeyedCacheOptions<V>) {
        this.#opts = opts;
    }

    get(key: string): Promise<V | null> {
        return this.#read(key, this.#loader(key));
    }

    getMany(keys: string[]): Promise<Array<V | null>> {
        return Promise.all(keys.map(key => this.#read(key, () => this.#enqueue(key))));
    }

    /** Stores a value known to be current; a read in flight for the key is not kept. */
    set(key: string, value: V): void {
        this.#cancel(key);
        this.#keep(key, value, false, false);
    }

    /** Forgets a key in memory and in the persistent store. */
    delete(key: string): void {
        this.#drop(key);
        this.#storedUsed.delete(key);
        this.#opts.persistent?.delete(key); // fire-and-forget
    }

    /** Forgets every key in memory, and in the persistent store when `persistent` is set. */
    clear(persistent = false): void {
        for (const key of new Set([...this.#slots.keys(), ...this.#flights.keys(), ...this.#errors.keys(), ...this.#missing])) this.#drop(key);
        this.#storedUsed.clear();
        if (persistent) this.#opts.persistent?.clear(); // fire-and-forget
    }

    /**
     * The executor may have changed anything: every held value is stale (still shown, read
     * again by the next read), and a read in flight is not kept. Keys with subscribers
     * are read again at once, the misses of this tick in one call.
     */
    markStale(): void {
        const again: string[] = [];
        const keys = new Set([...this.#slots.keys(), ...this.#flights.keys(), ...this.#errors.keys(), ...this.#missing]);
        for (const key of keys) {
            this.#cancel(key);
            this.#errors.delete(key);
            this.#missing.delete(key);
            const slot = this.#slots.get(key);
            if (slot) slot.stale = true;
            if (this.#listeners.get(key)?.size) again.push(key);
            else this.#notify(key);
        }
        for (const key of again) this.#read(key, this.#loader(key)).catch(() => {});
    }

    /** A view on one key: `getSnapshot`, `subscribe`, `read`. A subscriber to a key that is not fresh makes it load. */
    live(key: string): Live<V | null> {
        return {
            getSnapshot: () => this.#snapshot(key),
            subscribe: (fn) => {
                let listeners = this.#listeners.get(key);
                if (!listeners) this.#listeners.set(key, listeners = new Set());
                listeners.add(fn);
                const { state } = this.#snapshot(key);
                if (state !== 'fresh' && state !== 'loading') this.#read(key, this.#loader(key)).catch(() => {});
                return () => {
                    listeners!.delete(fn);
                    if (listeners!.size === 0 && this.#listeners.get(key) === listeners) this.#listeners.delete(key);
                    this.#prune(key);
                };
            },
            read: () => this.get(key),
        };
    }

    /** How a key reads itself: alone with `load`, else with the tick's batch. */
    #loader(key: string): () => Promise<Loaded<V>> {
        const load = this.#opts.load;
        return load ? () => this.#loadAlone(key, load) : () => this.#enqueue(key);
    }

    /** The value held, if it is fresh: not stale and not older than `maxAgeMs`. */
    #fresh(key: string): Slot<V> | undefined {
        const slot = this.#slots.get(key);
        if (!slot || slot.stale) return undefined;
        return Date.now() - slot.ts < (this.#opts.maxAgeMs?.(key) ?? Infinity) ? slot : undefined;
    }

    #read(key: string, load: () => Promise<Loaded<V>>): Promise<V | null> {
        const fresh = this.#fresh(key);
        if (fresh) return Promise.resolve(fresh.value);
        const held = this.#flights.get(key);
        if (held) return held.promise;
        const flight = { valid: true } as Flight<V>;
        const finish = (): boolean => {
            if (!flight.valid) return false;
            if (this.#flights.get(key) === flight) this.#flights.delete(key);
            return true;
        };
        flight.promise = load().then(
            (loaded) => {
                if (finish()) {
                    if (loaded.value === null) {
                        // Found before, missing now: the value held and the stored one are gone.
                        this.#forget(key);
                        this.#errors.delete(key);
                        this.#storedUsed.delete(key);
                        this.#opts.persistent?.delete(key); // fire-and-forget
                        if (this.#listeners.get(key)?.size) this.#missing.add(key);
                        this.#notify(key);
                    } else {
                        const stored = loaded.stored;
                        const stale = stored && !!this.#opts.persistentStale;
                        this.#keep(key, loaded.value, stored, stale);
                        // The stored value is shown now, the source read once in the background.
                        if (stale) this.#read(key, this.#loader(key)).catch(() => {});
                    }
                    this.#prune(key);
                }
                return loaded.value;
            },
            (error) => {
                if (finish()) {
                    // An error is kept only while a view watches the key.
                    if (this.#listeners.get(key)?.size) this.#errors.set(key, error);
                    else this.#errors.delete(key);
                    if (!this.#slots.has(key)) this.#storedUsed.delete(key);
                    this.#notify(key);
                    this.#prune(key);
                }
                throw error;
            },
        );
        this.#flights.set(key, flight);
        this.#notify(key);
        return flight.promise;
    }

    /** Whatever was read for `key` so far is no longer to be kept. */
    #cancel(key: string): void {
        const flight = this.#flights.get(key);
        if (flight) flight.valid = false;
        this.#flights.delete(key);
    }

    /** Forgets the value of a key and its place in the memory limits. Subscribers hear of it and do not load. */
    #drop(key: string): void {
        this.#cancel(key);
        this.#errors.delete(key);
        this.#missing.delete(key);
        this.#forget(key);
        this.#notify(key);
        this.#prune(key);
    }

    /** Drops what the maps hold for a key that has no value, no read and no subscriber. */
    #prune(key: string): void {
        if (this.#slots.has(key) || this.#flights.has(key) || this.#listeners.get(key)?.size) return;
        this.#errors.delete(key);
        this.#missing.delete(key);
        this.#snapshots.delete(key);
    }

    #forget(key: string): void {
        this.#bytes -= this.#slots.get(key)?.size ?? 0;
        this.#slots.delete(key);
    }

    /**
     * Holds a value: writes it to the persistent store (unless it came from there), applies
     * the limits and tells the key's subscribers. An equal value stays the same object
     * while someone subscribes: they compare snapshots by identity.
     */
    #keep(key: string, value: V, stored: boolean, stale: boolean): void {
        const limits = this.#opts.limits?.();
        const listening = (this.#listeners.get(key)?.size ?? 0) > 0;
        const text = limits || listening ? canonical(value) : undefined;
        const prev = this.#slots.get(key);
        const same = listening && !!prev && text === canonical(prev.value);
        this.#errors.delete(key);
        this.#missing.delete(key);
        if (!stored) this.#opts.persistent?.put(key, { value, ts: Date.now() }); // fire-and-forget
        const size = limits ? text!.length : 0;
        this.#forget(key);
        if (limits) {
            // A watched key is not evicted: its view would go blank with nothing to load it
            // again. While views hold them, watched keys may take the cache past its limits.
            for (const oldest of [...this.#slots.keys()]) {
                if (this.#slots.size < limits.entries && this.#bytes + size <= limits.bytes) break;
                if (this.#listeners.get(oldest)?.size) continue;
                this.#evict(oldest);
            }
        }
        this.#slots.set(key, {
            value: same ? prev!.value : value,
            ts: Date.now(),
            stale,
            size,
        });
        this.#bytes += size;
        this.#notify(key);
    }

    /** The memory limits took a value out; the persistent store keeps it. */
    #evict(key: string): void {
        this.#cancel(key);
        this.#forget(key);
        this.#storedUsed.delete(key);
        this.#notify(key);
        this.#prune(key);
    }

    #snapshot(key: string): Snapshot<V | null> {
        const slot = this.#slots.get(key);
        const missing = !slot && this.#missing.has(key);
        const state: State = this.#flights.has(key) ? 'loading'
            : this.#errors.has(key) ? 'error'
            : slot ? (this.#fresh(key) ? 'fresh' : 'stale')
            : missing ? 'fresh'
            : 'unknown';
        const prev = this.#snapshots.get(key);
        if (state === 'unknown') {
            // Nothing to show: every such key has the same snapshot, so none is stored.
            this.#snapshots.delete(key);
            return UNKNOWN as Snapshot<V | null>;
        }
        const next: Snapshot<V | null> = { state, value: slot ? slot.value : missing ? null : undefined, error: this.#errors.get(key) };
        if (prev && prev.state === next.state && prev.value === next.value && prev.error === next.error) return prev;
        this.#snapshots.set(key, next);
        return next;
    }

    /** Tells the key's subscribers when its snapshot changed. */
    #notify(key: string): void {
        const listeners = this.#listeners.get(key);
        if (!listeners?.size) return;
        const prev = this.#snapshots.get(key) ?? UNKNOWN;
        const next = this.#snapshot(key);
        if (next === prev) return;
        for (const fn of [...listeners]) {
            try { fn(next); } catch (e) { console.error('Error in mirror subscriber:', e); }
        }
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

    async #loadAlone(key: string, load: (key: string) => Promise<V | null>): Promise<Loaded<V>> {
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
}
