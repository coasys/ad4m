import type { PerspectiveClient } from "./PerspectiveClient";

/** Reply to `perspective.subscribe`. */
export interface Subscribed {
    subscriptionId: string
    result: any
    revision: number
}

/**
 * A `query-subscription-update` event: the change from the previous revision.
 * Model results carry `ids` (the new order), `upsert` (new or changed
 * instances) and, when the query set `count: true`, `totalCount`; query results carry `added` / `removed` rows;
 * a result that could not be diffed comes whole as `result`.
 */
export interface QueryUpdate {
    subscriptionId: string
    revision: number
    ids?: string[]
    upsert?: any[]
    totalCount?: number
    added?: any[]
    removed?: any[]
    result?: any
}

/** The executor dropped updates, maybe for any live query on this socket:
 *  every live query resyncs. */
export interface QueryLagged {
    lagged: true
}

/** What a live query tells its owner about re-opening its subscription after a
 *  reconnect or a failed resync: `reopening` after each failed attempt, with
 *  the delay before the next one, and `open` once a re-open succeeds after
 *  one failed. Until then the query is not live, even though its last result
 *  is still readable. */
export type LiveQueryStatus =
    | { state: 'reopening', error: unknown, retryInMs: number }
    | { state: 'open' }

/** Delay before the first retry of a failed re-open; it doubles per failure. */
export const REOPEN_BASE_DELAY_MS = 1000
/** Longest delay between two re-open attempts. */
export const REOPEN_MAX_DELAY_MS = 60_000

/** The delay before re-open attempt `failures + 1`: between half and all of
 *  `REOPEN_BASE_DELAY_MS * 2^(failures - 1)`, capped at
 *  `REOPEN_MAX_DELAY_MS`. The random half spreads the clients of a restarted
 *  executor apart. */
export function reopenDelay(failures: number, random: () => number = Math.random): number {
    const ceiling = Math.min(REOPEN_BASE_DELAY_MS * 2 ** Math.max(0, failures - 1), REOPEN_MAX_DELAY_MS)
    return ceiling / 2 + random() * ceiling / 2
}

/** Apply one update to the last result. Query rows are a multiset: added rows are appended.
 *  Throws when the update names an instance that neither the last result nor
 *  the update holds: the last result was wrong, and the caller must resync. */
export function applyUpdate(result: any, update: QueryUpdate): any {
    if ('result' in update) return update.result
    if (update.ids) {
        const byId = new Map<string, any>(result.instances.map((i: any) => [i.id, i]))
        for (const instance of update.upsert!) byId.set(instance.id, instance)
        const instances = update.ids.map(id => {
            if (!byId.has(id)) throw new Error(`Live query update names unknown instance ${id}`)
            return byId.get(id)
        })
        return { ...result, instances, totalCount: update.totalCount }
    }
    const toRemove = new Map<string, number>()
    for (const row of update.removed!) {
        const key = JSON.stringify(row)
        toRemove.set(key, (toRemove.get(key) ?? 0) + 1)
    }
    const kept = (result as any[]).filter(row => {
        const key = JSON.stringify(row)
        const n = toRemove.get(key)
        if (!n) return true
        toRemove.set(key, n - 1)
        return false
    })
    return [...kept, ...update.added!]
}

/** What a live query holds at one moment; see {@link LiveQuery.getSnapshot}. */
export interface Snapshot<T> {
    /** `loading` until the first result, `ready` after it, `error` when the
     *  first open failed (the handle is then disposed). */
    status: 'loading' | 'ready' | 'error'
    /** The latest result; set while `status` is `ready`. */
    data?: T
    /** Why the first open failed; set while `status` is `error`. */
    error?: Error
    /** The executor's revision of `data`. */
    revision?: number
    /** A re-open failed and the query is retrying: `data` is then not live. */
    reopening: boolean
    /** `true` only for the value the handle opened on. A change after it, also
     *  the first result of a re-open that differs from the last one, is `false`. */
    initial: boolean
}

export type Unsubscribe = () => void

/** The rows of a SPARQL live query, as the executor serialises them. */
export type SparqlRows = any[]

/** Options of a live query. */
export interface LiveOptions {
    /** Timeout in ms of the call that opens the query (default 30 s). */
    timeoutMs?: number
}

/** The same JSON with object keys sorted, so equal results compare equal
 *  whatever order the executor wrote their keys in. */
function stableKey(value: unknown): string {
    return JSON.stringify(value, (_, v) => {
        if (v === null || typeof v !== 'object' || Array.isArray(v)) return v
        return Object.fromEntries(Object.keys(v).sort().map(k => [k, v[k]]))
    })
}

/**
 * One live query on the executor, opened by `Model.query()` or
 * `perspective.liveSparql()`.
 *
 * It holds the last result, applies each update to it, calls
 * `resyncSubscription` when a revision is missing, and opens a new
 * subscription after a reconnect (the executor ends a socket's subscriptions
 * when it closes). A re-open that fails (the executor is still loading the
 * perspective after a restart) is retried with backoff until it succeeds or
 * the query is disposed; `onStatus` listeners hear about it (#1595).
 *
 * The snapshot changes only when the result, the status or `reopening` does:
 * a resync, a re-open or an update that leaves the result as it was emits
 * nothing, as long as the status and `reopening` are unchanged too. Call `dispose()` when done.
 */
export class LiveQuery<T> {
    /** The first result; rejects when the first open fails or the handle is
     *  disposed before it arrives. */
    readonly first: Promise<T>
    #client: PerspectiveClient
    #uuid: string
    #open: (signal: AbortSignal) => Promise<Subscribed>
    #project: (raw: any) => T
    #id?: string
    #revision = 0
    #result: any
    // The stable form of `#result`, to tell a change from a repeat.
    #key?: string
    #snapshot: Snapshot<T> = { status: 'loading', reopening: false, initial: false }
    #disposed = false
    #generation = 0
    // Updates that arrive while a subscribe or resync reply is outstanding.
    #buffer: (QueryUpdate | QueryLagged)[] | null = null
    #unlisten: () => void
    #unreconnect: () => void
    // Aborts the subscribe call still in flight, for `dispose()`.
    #opening?: AbortController
    #retryTimer?: ReturnType<typeof setTimeout>
    #reopening = false
    #listeners = new Set<(snapshot: Snapshot<T>) => void>()
    #statusListeners = new Set<(status: LiveQueryStatus) => void>()
    #resolveFirst!: (data: T) => void
    #rejectFirst!: (error: Error) => void

    /** `open` subscribes on the executor; aborting its signal cancels the call.
     *  `project` builds the data from a raw result. The query opens at once. */
    constructor(client: PerspectiveClient, uuid: string, open: (signal: AbortSignal) => Promise<Subscribed>, project: (raw: any) => T) {
        this.#client = client
        this.#uuid = uuid
        this.#open = open
        this.#project = project
        this.first = new Promise<T>((resolve, reject) => {
            this.#resolveFirst = resolve
            this.#rejectFirst = reject
        })
        // `first` rejects for whoever reads it; one that does not must not get
        // an unhandled rejection from it.
        this.first.catch(() => {})
        this.#unlisten = client.onQueryUpdate(update => this.#receive(update))
        this.#unreconnect = client.onReconnect(() => this.#reopen())
        const generation = this.#generation + 1
        this.#start().catch(error => {
            // A newer start (a reconnect) owns the query now.
            if (this.#disposed || generation !== this.#generation) return
            this.#snapshot = { status: 'error', error, reopening: false, initial: false }
            this.#rejectFirst(error)
            this.#notify()
            this.dispose()
        })
    }

    // `getSnapshot` and `subscribe` are bound, so they pass as they are to
    // `useSyncExternalStore(live.subscribe, live.getSnapshot)`.

    /** The current snapshot: the same object until the result, the status or
     *  `reopening` changes. */
    readonly getSnapshot = (): Snapshot<T> => this.#snapshot

    /** Call `fn` with the current snapshot at once when the query is no longer
     *  loading, then with each change. Returns a remover. A disposed handle
     *  calls nobody and keeps nothing. */
    readonly subscribe = (fn: (snapshot: Snapshot<T>) => void): Unsubscribe => {
        if (this.#disposed) return () => {}
        this.#listeners.add(fn)
        if (this.#snapshot.status !== 'loading') this.#call(fn)
        return () => { this.#listeners.delete(fn) }
    }

    /** Call `listener` on each failed re-open and when a re-open succeeds
     *  after one failed. Returns a remover. */
    onStatus(listener: (status: LiveQueryStatus) => void): Unsubscribe {
        this.#statusListeners.add(listener)
        return () => { this.#statusListeners.delete(listener) }
    }

    /** End the subscription on the executor and drop every listener. A first
     *  open still in flight is cancelled, so the executor stops computing a
     *  result nobody waits for (#1595), and `first` rejects. Calling it again does nothing. */
    dispose(): void {
        if (this.#disposed) return
        this.#disposed = true
        this.#generation++
        this.#clearRetry()
        this.#statusListeners.clear()
        this.#listeners.clear()
        this.#unlisten()
        this.#unreconnect()
        this.#opening?.abort()
        if (this.#snapshot.status === 'loading') this.#rejectFirst(new Error('Live query disposed before its first result'))
        if (this.#id) this.#client.disposeQuerySubscription(this.#uuid, this.#id).catch(() => {})
    }

    /** Open the subscription (again). A start that a newer one overtakes
     *  resolves without a result. */
    async #start(): Promise<void> {
        this.#clearRetry()
        const generation = ++this.#generation
        this.#buffer = []
        const opening = new AbortController()
        this.#opening = opening
        let subscribed: Subscribed
        try {
            subscribed = await this.#open(opening.signal)
        } catch (e) {
            if (generation === this.#generation) this.#buffer = null
            throw e
        } finally {
            if (this.#opening === opening) this.#opening = undefined
        }
        if (this.#disposed || generation !== this.#generation) {
            this.#client.disposeQuerySubscription(this.#uuid, subscribed.subscriptionId).catch(() => {})
            return
        }
        this.#id = subscribed.subscriptionId
        const recovered = this.#reopening
        this.#reopening = false
        this.#replace(subscribed.revision, subscribed.result)
        if (recovered) this.#emit({ state: 'open' })
    }

    /** Open the subscription again, and keep trying with backoff
     *  (`reopenDelay`) while each attempt fails, until one succeeds, a newer
     *  start takes over, or the query is disposed. One failed attempt used
     *  to leave the query dead for good while its owner took it as live: a
     *  client that reconnects to a restarted executor before the perspective
     *  is loaded gets "Perspective not found" (#1595). */
    #reopen(failures = 0) {
        if (this.#disposed) return
        const reopened = this.#start()
        const generation = this.#generation
        reopened.catch(error => {
            if (this.#disposed || generation !== this.#generation) return
            const retryInMs = reopenDelay(failures + 1)
            console.error(`Error re-opening live query (attempt ${failures + 1}), retrying in ${Math.round(retryInMs)}ms:`, error)
            this.#reopening = true
            this.#showReopening()
            this.#emit({ state: 'reopening', error, retryInMs })
            const timer: any = setTimeout(() => {
                this.#retryTimer = undefined
                this.#reopen(failures + 1)
            }, retryInMs)
            // Node: a pending retry must not keep the process alive.
            if (typeof timer?.unref === 'function') timer.unref()
            this.#retryTimer = timer
        })
    }

    #clearRetry() {
        if (this.#retryTimer === undefined) return
        clearTimeout(this.#retryTimer)
        this.#retryTimer = undefined
    }

    #emit(status: LiveQueryStatus) {
        for (const listener of [...this.#statusListeners]) {
            try {
                listener(status)
            } catch (e) {
                console.error('Error in live query status listener:', e)
            }
        }
    }

    #call(fn: (snapshot: Snapshot<T>) => void) {
        try {
            fn(this.#snapshot)
        } catch (e) {
            console.error('Error in live query listener:', e)
        }
    }

    #notify() {
        // A listener may remove another, or dispose the handle, while this runs.
        for (const fn of [...this.#listeners]) {
            if (this.#disposed) return
            if (this.#listeners.has(fn)) this.#call(fn)
        }
    }

    /** Make `raw` the current result. Nothing changes, and nobody hears of it,
     *  when it equals the last one and `reopening` is as it was. */
    #show(raw: any) {
        const key = stableKey(raw)
        const prev = this.#snapshot
        const same = prev.status === 'ready' && key === this.#key
        if (same && prev.reopening === this.#reopening) return
        const data = same ? prev.data! : this.#project(raw)
        this.#key = key
        this.#snapshot = {
            status: 'ready',
            data,
            revision: same ? prev.revision : this.#revision,
            reopening: this.#reopening,
            initial: same ? prev.initial : prev.status === 'loading',
        }
        if (prev.status === 'loading') this.#resolveFirst(data)
        this.#notify()
    }

    /** Show that a re-open failed: the snapshot keeps its data. */
    #showReopening() {
        if (this.#snapshot.status === 'loading' || this.#snapshot.reopening) return
        this.#snapshot = { ...this.#snapshot, reopening: true }
        this.#notify()
    }

    /** Show `raw`. Before the first result a failure to project it fails the
     *  open; after it, the result is skipped. */
    #showOrSkip(raw: any) {
        try {
            this.#show(raw)
        } catch (e) {
            if (this.#snapshot.status === 'loading') throw e
            console.error('Live query result could not be read:', e)
            // Keep the old data, but let the snapshot agree with the status.
            if (this.#snapshot.reopening !== this.#reopening) {
                this.#snapshot = { ...this.#snapshot, reopening: this.#reopening }
                this.#notify()
            }
        }
    }

    /** Take `result` at `revision`, then apply the updates buffered meanwhile. */
    #replace(revision: number, result: any) {
        if (this.#disposed) return
        this.#revision = revision
        this.#result = result
        const buffered = this.#buffer ?? []
        this.#buffer = null
        this.#showOrSkip(result)
        // A listener may have disposed the handle: nothing more is applied.
        for (const update of buffered) this.#receive(update)
    }

    #receive(update: QueryUpdate | QueryLagged) {
        if (this.#disposed) return
        if (this.#buffer) {
            this.#buffer.push(update)
            return
        }
        if ('lagged' in update) {
            if (this.#id) this.#resync()
            return
        }
        if (update.subscriptionId !== this.#id || update.revision <= this.#revision) return
        if (update.revision !== this.#revision + 1) {
            this.#resync()
            return
        }
        let result: any
        try {
            result = applyUpdate(this.#result, update)
        } catch {
            this.#resync()
            return
        }
        this.#revision = update.revision
        this.#result = result
        this.#showOrSkip(result)
    }

    #resync() {
        if (this.#disposed) return
        const generation = this.#generation
        const id = this.#id!
        this.#buffer = []
        this.#client.resyncSubscription(this.#uuid, id).then(
            ({ revision, result }) => {
                if (!this.#disposed && generation === this.#generation) this.#replace(revision, result)
            },
            () => {
                // Replace the subscription with a new one.
                if (!this.#disposed && generation === this.#generation) {
                    this.#client.disposeQuerySubscription(this.#uuid, id).catch(() => {})
                    this.#reopen()
                }
            },
        )
    }
}
