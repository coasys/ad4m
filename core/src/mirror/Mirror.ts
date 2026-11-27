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
    /** Every open live subscription per key, the newest last: one a write displaced stays here until it ends, so a reset closes it too. */
    #lives = new Map<string, SharedLive[]>()
    #flights = new Map<string, Promise<any>>()
    /** The executor refused the client's token (see {@link refuse}); no later cache escapes it. */
    #refused = false
    /** Counts the resets and refusals: a drain that sees it change knows the mirror was reset under it. */
    #resets = 0
    /** Per perspective, how many writes of this client have settled (see {@link wrote}). */
    #writes = new Map<string, number>()
    #origin: () => string
    #listen: <K extends EventName>(name: K, handler: (event: EventMap[K]) => void) => unknown
    #actingDid: () => string | null
    /** Every event handler an entry or cache registered, to register again after {@link reset}. */
    #registrations: [EventName, (event: any) => void][] = []

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
            // A client whose token was refused before this cache existed gets a refused one.
            if (this.#refused) created.refuse()
            held = { staleOnReconnect: spec.staleOnReconnect !== false, cache: created }
            this.#caches.set(spec.method, held)
            for (const [name, handler] of Object.entries(spec.on ?? {}) as [EventName, (event: any, cache: KeyedCache<V>) => void][]) {
                this.#register(name, (event) => handler(event, created))
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
                this.#register(name, (event) => handler(event, created))
            }
        }
        return entry as Entry<T>
    }

    #register(name: EventName, handler: (event: any) => void): void {
        this.#registrations.push([name, handler])
        this.#listen(name, handler)
    }

    /**
     * A handle on the `live` entry for this key. The first call opens the subscription with
     * `open`, which holds the executor's raw result; every later call with the same key, while
     * it is open, shares it: one executor subscription, a handle of its own each, and each
     * handle shapes the raw result with its own `project`. The subscription resyncs itself on
     * `lagged` and after a reconnect. After the last handle is disposed it stays open
     * `keepAliveMs` (default 30 s), so a handle opened again answers at once.
     */
    live<T>(spec: { perspective: string; method: string; params: unknown; keepAliveMs?: number; open: () => LiveSubscription; project: (raw: any) => T }): LiveQuery<T> {
        const key = keyOf(spec)
        const open = this.#lives.get(key) ?? []
        let shared = open[open.length - 1]
        // A subscription a write of this client waits on is not joined: its result may be the one from before the write.
        if (!shared?.joinable) {
            if (shared?.idle) { shared.close(); open.pop() }
            const created: SharedLive = new SharedLive(spec.open(), spec.perspective, () => {
                const list = this.#lives.get(key)
                if (list?.includes(created)) list.splice(list.indexOf(created), 1)
                if (list && !list.length) this.#lives.delete(key)
            })
            shared = created
            open.push(created)
            this.#lives.set(key, open)
        }
        return shared.view(spec.project, spec.keepAliveMs)
    }

    /**
     * The raw result of the open `live` entry for this key while it is ready and live, so a
     * one-shot read of the same question needs no request; `undefined` otherwise. A copy.
     */
    liveResult(spec: { perspective: string; method: string; params: unknown }): { result: any } | undefined {
        const held = this.#lives.get(keyOf(spec))?.at(-1)?.answer()
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
        for (const shared of this.#lives.values()) for (const one of shared) if (one.perspective === perspective) one.wrote()
    }

    /**
     * The socket was lost or reconnected, or the executor said it dropped events: whatever
     * was pushed meanwhile is lost, so every entry is stale and its read in flight is dropped.
     * With `refetch`, those with subscribers load again; without it (no socket) nothing is
     * sent. A cache with `staleOnReconnect: false` stays. The live results answer no one-shot
     * read until their resync delivers a result; their snapshots keep the last value.
     * Two phases: every value is marked first, then the subscribers are told (see `#drain`).
     */
    markAllStale(refetch = true): void {
        // The live results first: a subscriber told of the loss may read at once.
        for (const open of this.#lives.values()) for (const shared of open) shared.stale()
        // Two phases: every value is marked before any subscriber is told, or one told first
        // could read another that is still fresh.
        const tell: ((current: () => boolean) => void)[] = []
        for (const entry of this.#entries.values()) entry.invalidate(refetch, tell)
        for (const { staleOnReconnect, cache } of this.#caches.values()) if (staleOnReconnect) cache.markStale(refetch, tell)
        this.#drain(tell)
    }

    /**
     * Tells the subscribers of what was marked, one callback after the other. A subscriber may
     * reset the mirror or refuse its token meanwhile (`resets` changes): each callback asks right
     * before its read, so no read starts after it, the read of the entry whose subscriber did it
     * included, as a reset or a refusal sends no request. Every subscriber still hears.
     */
    #drain(tell: ((current: () => boolean) => void)[]): void {
        const resets = this.#resets
        const current = () => resets === this.#resets
        for (const run of tell) run(current)
    }

    /** How many times the mirror was reset or refused: a caller that notifies subscribers can tell whether one of them reset it. */
    get resets(): number {
        return this.#resets
    }

    /**
     * The executor refused the client's token, and no reconnect comes: every value is stale
     * and sends no request, the expression cache included (a refusal ends its reuse), and no
     * persistent tier is read, so every later read reaches the refusal. Snapshots keep the
     * last value for display. It sets `#refused` (a cache created later is refused from the
     * start) and counts as a reset: a subscriber told of it that resets or refuses again
     * stops the reads the drain queued, and every subscriber still hears.
     */
    refuse(): void {
        this.#refused = true
        this.#resets++
        // As `markAllStale`: every value is marked before any subscriber is told. The caches
        // staying fresh through a reconnect (the expressions) are refused too.
        const tell: ((current: () => boolean) => void)[] = []
        for (const open of this.#lives.values()) for (const shared of open) shared.stale()
        for (const entry of this.#entries.values()) entry.invalidate(false, tell)
        for (const { cache } of this.#caches.values()) cache.refuse(tell)
        this.#drain(tell)
    }

    /** A socket opened: every stale or failed value with a subscriber loads once. Fresh values and reads in flight stay. A reset made by a subscriber meanwhile stops the reads that follow. */
    reloadWatched(): void {
        const resets = this.#resets
        const current = () => resets === this.#resets
        for (const entry of this.#entries.values()) if (current()) entry.reloadWatched()
        for (const { staleOnReconnect, cache } of this.#caches.values()) if (staleOnReconnect && current()) cache.reloadWatched(current)
    }

    /**
     * The client closed its socket and handlers and may be used again: every value is stale
     * (except the keyed caches with `staleOnReconnect: false`, the expressions, which only their age expires),
     * the pooled live subscriptions end (their executor side died with the socket), and the
     * events the entries and caches listen to are registered again. It counts as a reset and
     * sends no request: `markAllStale(false)` also drops every read in flight. A subscriber told of
     * it that resets again stops the reads the drain queued.
     */
    reset(): void {
        for (const open of this.#lives.values()) for (const shared of open) shared.close()
        this.#lives.clear()
        // The calls in flight are about to be rejected: a one-shot made now sends its own request.
        this.#flights.clear()
        this.#resets++
        this.markAllStale(false)
        for (const [name, handler] of this.#registrations) this.#listen(name, handler)
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
            const flight: Promise<T> = spec.request().finally(() => {
                if (this.#flights.get(key) === flight) this.#flights.delete(key)
            })
            reply = flight
            this.#flights.set(key, flight)
        }
        return reply
    }
}

/** Perspective uuid, RPC name and canonical params: the key of a live query or one-shot read. */
function keyOf(spec: { perspective: string; method: string; params: unknown }): string {
    return `${JSON.stringify(spec.perspective)}|${spec.method}|${canonical(spec.params)}`
}
