import { canonical, Entry, type Kind, type Scope } from './Entry'
import type { EventMap, EventName } from '../generated/api/Events'
import { KeyedCache, type KeyedCacheOptions } from '../cache/KeyedCache'
import type { LiveQuery, LiveSubscription } from '../perspectives/LiveQuery'
import { SharedLive } from './live'
import { createPersistentCache, NullCache, type PersistentCache } from '../cache/PersistentCache'

export type { Kind, Live, Scope, Snapshot, State, Unsubscribe } from './Entry'

/** What names one entry, and how it loads. */
export interface EntrySpec<T> {
    scope: Scope
    kind: Kind
    /** The RPC the entry reads, e.g. `agent.status`. */
    method: string
    params: unknown
    load: () => Promise<T>
    /**
     * The events that keep the entry fresh. The mirror registers each once, when it creates
     * the entry; the handler gets the entry to push into or invalidate.
     */
    on?: { [K in EventName]?: (event: EventMap[K], entry: Entry<T>) => void }
}

/**
 * The values one `ApiClient` read from its executor, one {@link Entry} per data point,
 * keyed by scope, RPC name and canonical params. Two clients share nothing: each owns a mirror.
 */
export class Mirror {
    #entries = new Map<string, Entry<unknown>>()
    #caches = new Map<string, { kind: Kind; cache: KeyedCache<any> }>()
    #lives = new Map<string, SharedLive<any>>()
    #origin: () => string
    #listen: <K extends EventName>(name: K, handler: (event: EventMap[K]) => void) => unknown

    /**
     * The DID this client acts as, for the persistent tier's keys; `null` while unknown, and
     * then nothing is read from or written to that tier. `AgentClient` sets it.
     */
    actingDid: () => Promise<string | null> = async () => null

    /**
     * `origin` names the executor: persisted values are keyed by it, the acting DID and the key.
     * `listen` registers an executor event handler (`ApiClient.on`), for the `on` of an entry spec.
     */
    constructor(
        origin: () => string = () => '',
        listen: <K extends EventName>(name: K, handler: (event: EventMap[K]) => void) => unknown = () => {},
    ) {
        this.#origin = origin
        this.#listen = listen
    }

    /**
     * The {@link KeyedCache} for one RPC kind: the store of its `immutable` or `declared`
     * values, owned by this mirror, so two clients share nothing. The first call's options stay.
     */
    cache<V>(spec: { method: string; kind: 'immutable' | 'declared'; options: () => KeyedCacheOptions<V> }): KeyedCache<V> {
        let held = this.#caches.get(spec.method)
        if (!held) {
            held = { kind: spec.kind, cache: new KeyedCache<V>(spec.options(), spec.kind) }
            this.#caches.set(spec.method, held)
        }
        return held.cache as KeyedCache<V>
    }

    /**
     * A persistent store (IndexedDB in browsers) whose keys are the executor's origin, the
     * acting DID and the key: a second user of one browser profile reads none of the first's
     * values. Without an IndexedDB, or while the DID is unknown, it keeps nothing.
     */
    persistent<T>(dbName: string, storeName: string): PersistentCache<T> {
        const inner = createPersistentCache<T>(dbName, storeName)
        if (inner instanceof NullCache) return inner
        const scoped = async (key: string) => {
            const did = await this.actingDid().catch(() => null)
            return did ? `${this.#origin()}|${did}|${key}` : null
        }
        return {
            get: async (key) => { const k = await scoped(key); return k ? inner.get(k) : undefined },
            put: async (key, value) => { const k = await scoped(key); if (k) await inner.put(k, value) },
            delete: async (key) => { const k = await scoped(key); if (k) await inner.delete(k) },
            clear: () => inner.clear(),
        }
    }

    /** The entry for this key; the first call's `load` stays. */
    entry<T>(spec: EntrySpec<T>): Entry<T> {
        const key = keyOf(spec)
        let entry = this.#entries.get(key)
        if (!entry) {
            const created = new Entry<T>(spec.kind, spec.load)
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
    live<T>(spec: { scope: Scope; method: string; params: unknown; keepAliveMs?: number; open: () => LiveSubscription<T> }): LiveQuery<T> {
        const key = keyOf(spec)
        let shared = this.#lives.get(key)
        if (!shared?.usable) {
            const created: SharedLive<T> = new SharedLive(spec.open(), scopeOf(spec.scope), () => {
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
    liveResult(spec: { scope: Scope; method: string; params: unknown }): { result: any } | undefined {
        const held = this.#lives.get(keyOf(spec))?.answer()
        return held && { result: structuredClone(held.result) }
    }

    /**
     * A write of this client to `scope` settled. The `live` entries of that scope answer no
     * one-shot read until an update changes their result: the executor sends it after the
     * write's reply, so what they hold may not include the write yet.
     */
    wrote(scope: Scope): void {
        const name = scopeOf(scope)
        for (const shared of this.#lives.values()) if (shared.scope === name) shared.wrote()
    }

    /**
     * The socket reconnected or the executor said it dropped events: whatever was pushed
     * meanwhile is lost, so every entry is stale, and those with subscribers load again.
     * `immutable` values never change, so they stay.
     */
    markAllStale(): void {
        for (const entry of this.#entries.values()) entry.invalidate()
        for (const { kind, cache } of this.#caches.values()) if (kind !== 'immutable') cache.markStale()
    }
}

/** Scope, RPC name and canonical params: the key of an entry. */
function keyOf(spec: { scope: Scope; method: string; params: unknown }): string {
    return `${scopeOf(spec.scope)}|${spec.method}|${canonical(spec.params)}`
}

function scopeOf(scope: Scope): string {
    return typeof scope === 'string' ? scope : JSON.stringify(scope)
}
