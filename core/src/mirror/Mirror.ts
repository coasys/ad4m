import { canonical, Entry, type Kind, type Scope } from './Entry'

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
     */
    markAllStale(): void {
        for (const entry of this.#entries.values()) entry.invalidate()
    }
}
