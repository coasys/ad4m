import { canonical, Entry, type Kind, type Scope } from './Entry'
import { KeyedCache, type KeyedCacheOptions } from '../cache/KeyedCache'
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
}

/**
 * The values one `ApiClient` read from its executor, one {@link Entry} per data point,
 * keyed by scope, RPC name and canonical params. Two clients share nothing: each owns a mirror.
 */
export class Mirror {
    #entries = new Map<string, Entry<unknown>>()
    #caches = new Map<string, { kind: Kind; cache: KeyedCache<any> }>()
    #origin: () => string

    /**
     * The DID this client acts as, for the persistent tier's keys; `null` while unknown, and
     * then nothing is read from or written to that tier. `AgentClient` sets it.
     */
    actingDid: () => Promise<string | null> = async () => null

    /** `origin` names the executor: persisted values are keyed by it, the acting DID and the key. */
    constructor(origin: () => string = () => '') {
        this.#origin = origin
    }

    /**
     * The {@link KeyedCache} for one RPC kind: the store of its `immutable` or `declared`
     * values, owned by this mirror, so two clients share nothing. The first call's options stay.
     */
    cache<V>(spec: { method: string; kind: 'immutable' | 'declared'; options: () => KeyedCacheOptions<V> }): KeyedCache<V> {
        let held = this.#caches.get(spec.method)
        if (!held) {
            held = { kind: spec.kind, cache: new KeyedCache<V>(spec.options()) }
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
        const scope = typeof spec.scope === 'string' ? spec.scope : JSON.stringify(spec.scope)
        const key = `${scope}|${spec.method}|${canonical(spec.params)}`
        let entry = this.#entries.get(key)
        if (!entry) {
            entry = new Entry<unknown>(spec.kind, spec.load)
            this.#entries.set(key, entry)
        }
        return entry as Entry<T>
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
