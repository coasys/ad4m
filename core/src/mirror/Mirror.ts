import { canonical, Entry } from './Entry'
import type { EventMap, EventName } from '../generated/api/Events'
import { KeyedCache, type KeyedCacheOptions } from '../cache/KeyedCache'
import type { LiveQuery, LiveSubscription } from '../perspectives/LiveQuery'
import { SharedLive } from './live'
import { createPersistentCache, NullCache, type PersistentCache } from '../cache/PersistentCache'

export type { Live, Snapshot, State, Unsubscribe } from './Entry'

/** What names one entry, and how it loads. */
export interface EntrySpec<T> {
    /** The RPC the entry reads, e.g. `agent.status`: one entry per method, the key. */
    method: string
    load: () => Promise<T>
    /**
     * The events that keep the entry fresh. The mirror registers each once, when it creates
     * the entry; the handler gets the entry to push into or invalidate.
     */
    on?: { [K in EventName]?: (event: EventMap[K], entry: Entry<T>) => void }
}

/**
 * The values one `ApiClient` read from its executor, one {@link Entry} per data point
 * (keyed by RPC name), plus the live queries and one-shot reads of its perspectives (keyed by
 * perspective uuid, RPC name and canonical params). Two clients share nothing: each owns a mirror.
 */
export class Mirror {
    #entries = new Map<string, Entry<unknown>>()
    #caches = new Map<string, { staleOnReconnect: boolean; cache: KeyedCache<any> }>()
    #lives = new Map<string, SharedLive<any>>()
    #flights = new Map<string, Promise<any>>()
    /** Per perspective, how many writes of this client have settled (see {@link wrote}). */
    #writes = new Map<string, number>()
    #origin: () => string
    #listen: <K extends EventName>(name: K, handler: (event: EventMap[K]) => void) => unknown
    #actingDid: () => string | null

    /**
     * `origin` names the executor: persisted values are keyed by it, the acting DID and the key.
     * `listen` registers an executor event handler (`ApiClient.on`), for the `on` of an entry
     * or cache spec. `actingDid` is the DID this client acts as, as far as the client already
     * knows it (it never sends a request for it): `null` while unknown, and then nothing is
     * read from or written to the persistent tier.
     */
    constructor(
        origin: () => string,
        listen: <K extends EventName>(name: K, handler: (event: EventMap[K]) => void) => unknown,
        actingDid: () => string | null,
    ) {
        this.#origin = origin
        this.#listen = listen
        this.#actingDid = actingDid
    }

    /**
     * The {@link KeyedCache} for one RPC: the store of its keyed values, owned by this
     * mirror, so two clients share nothing. The first call's spec stays; its `on` handlers
     * are registered once, with the cache to `set`, `delete` or `markStale`.
     * A reconnect or a `lagged` marker makes the cache stale (see {@link markAllStale}),
     * unless `staleOnReconnect` is `false`: for values no event ever corrects, which only
     * their own age expires.
     */
    cache<V>(spec: {
        method: string
        staleOnReconnect?: boolean
        options: () => KeyedCacheOptions<V>
        on?: { [K in EventName]?: (event: EventMap[K], cache: KeyedCache<V>) => void }
    }): KeyedCache<V> {
        let held = this.#caches.get(spec.method)
        if (!held) {
            const created = new KeyedCache<V>(spec.options())
            held = { staleOnReconnect: spec.staleOnReconnect !== false, cache: created }
            this.#caches.set(spec.method, held)
            for (const [name, handler] of Object.entries(spec.on ?? {}) as [EventName, (event: any, cache: KeyedCache<V>) => void][]) {
                this.#listen(name, (event) => handler(event, created))
            }
        }
        return held.cache as KeyedCache<V>
    }

    /**
     * A persistent store (IndexedDB in browsers) whose keys are the executor's origin, the
     * acting DID and the key: a second user of one browser profile reads none of the first's
     * values, and `clear()` removes only this client's own. Without an IndexedDB, or while
     * the DID is unknown, it keeps nothing and reads nothing.
     */
    persistent<T>(dbName: string, storeName: string): PersistentCache<T> {
        const inner = createPersistentCache<T>(dbName, storeName)
        if (inner instanceof NullCache) return inner
        const prefix = () => {
            const did = this.#actingDid()
            return did ? `${this.#origin()}|${did}|` : null
        }
        const scoped = (key: string) => { const p = prefix(); return p && p + key }
        return {
            get: async (key) => { const k = scoped(key); return k ? inner.get(k) : undefined },
            put: async (key, value) => { const k = scoped(key); if (k) await inner.put(k, value) },
            delete: async (key) => { const k = scoped(key); if (k) await inner.delete(k) },
            clear: async () => { const p = prefix(); if (p) await inner.clear(p) },
        }
    }

    /** The entry for this RPC; the first call's `load` stays. */
    entry<T>(spec: EntrySpec<T>): Entry<T> {
        const key = spec.method
        let entry = this.#entries.get(key)
        if (!entry) {
            const created = new Entry<T>(spec.load)
            entry = created as Entry<unknown>
            this.#entries.set(key, entry)
            for (const [name, handler] of Object.entries(spec.on ?? {}) as [EventName, (event: any, entry: Entry<T>) => void][]) {
                this.#listen(name, (event) => handler(event, created))
            }
        }
        return entry as Entry<T>
    }

    /**
     * A handle on the `live` entry for this key. The first call opens the subscription with
     * `open`; every later call with the same key, while it is open, shares it: one executor
     * subscription, a handle of its own each. The subscription resyncs itself on `lagged` and
     * after a reconnect, so the mirror does not mark it stale. After the last handle is
     * disposed it stays open `keepAliveMs` (default 30 s), so a handle opened again answers at once.
     */
    live<T>(spec: { perspective: string; method: string; params: unknown; keepAliveMs?: number; open: () => LiveSubscription<T> }): LiveQuery<T> {
        const key = keyOf(spec)
        let shared = this.#lives.get(key)
        if (!shared?.usable) {
            const created: SharedLive<T> = new SharedLive(spec.open(), spec.perspective, () => {
                if (this.#lives.get(key) === created) this.#lives.delete(key)
            })
            shared = created
            this.#lives.set(key, shared)
        }
        return (shared as SharedLive<T>).view(spec.keepAliveMs)
    }

    /**
     * The raw result of the open `live` entry for this key while it is ready and live, so a
     * one-shot read of the same question needs no request; `undefined` otherwise. A copy.
     */
    liveResult(spec: { perspective: string; method: string; params: unknown }): { result: any } | undefined {
        const held = this.#lives.get(keyOf(spec))?.answer()
        return held && { result: structuredClone(held.result) }
    }

    /**
     * A write of this client to the perspective settled. Its `live` entries answer no
     * one-shot read until an update changes their result: the executor sends it after the
     * write's reply, so what they hold may not include the write yet. The cost: a write that
     * does not change a query's result sends every later one-shot of that query to the
     * executor, until an update changes the result. It also starts a new generation for
     * {@link oneShot}: a call made after the write does not join a request that went out before it.
     */
    wrote(perspective: string): void {
        this.#writes.set(perspective, (this.#writes.get(perspective) ?? 0) + 1)
        for (const shared of this.#lives.values()) if (shared.perspective === perspective) shared.wrote()
    }

    /**
     * The socket reconnected or the executor said it dropped events: whatever was pushed
     * meanwhile is lost, so every entry is stale, and those with subscribers load again.
     * A cache with `staleOnReconnect: false` stays.
     */
    markAllStale(): void {
        for (const entry of this.#entries.values()) entry.invalidate()
        for (const { staleOnReconnect, cache } of this.#caches.values()) if (staleOnReconnect) cache.markStale()
    }

    /**
     * One request for identical one-shot reads: while one is in flight, a call with the same
     * perspective, RPC name and params (and no write of this client to it settled since it
     * went out) gets its promise. The caller reads its own copy of the reply. Nothing is kept
     * after it settles.
     */
    oneShot<T>(spec: { perspective: string; method: string; params: unknown; request: () => Promise<T> }): Promise<T> {
        const key = `${keyOf(spec)}|${this.#writes.get(spec.perspective) ?? 0}`
        let reply = this.#flights.get(key)
        if (!reply) {
            reply = spec.request().finally(() => this.#flights.delete(key))
            this.#flights.set(key, reply)
        }
        return reply
    }
}

/** Perspective uuid, RPC name and canonical params: the key of a live query or one-shot read. */
function keyOf(spec: { perspective: string; method: string; params: unknown }): string {
    return `${JSON.stringify(spec.perspective)}|${spec.method}|${canonical(spec.params)}`
}
