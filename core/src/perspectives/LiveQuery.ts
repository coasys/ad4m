import type { PerspectiveClient } from "./PerspectiveClient";

/** Reply to `perspective.subscribeQuery` / `perspective.modelSubscribe`. */
export interface Subscribed {
    subscriptionId: string
    result: any
    revision: number
}

/**
 * A `query-subscription-update` event: the change from the previous revision.
 * Model results carry `ids` (the new order), `upsert` (new or changed
 * instances) and `totalCount`; query results carry `added` / `removed` rows;
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

/**
 * One live query on the executor. Holds the last result, applies each update
 * to it, calls `resyncSubscription` when a revision is missing, and opens a
 * new subscription after a reconnect (the executor ends a socket's
 * subscriptions when it closes). A re-open that fails (the executor is
 * still loading the perspective after a restart) is retried with backoff
 * until it succeeds or the query is disposed; `onStatus` listeners hear
 * about it (#1595). `onResult` gets every result after the first; `start()`
 * returns the first.
 */
export class LiveQuery {
    #client: PerspectiveClient
    #uuid: string
    #open: (signal: AbortSignal) => Promise<Subscribed>
    #onResult: (result: any) => void
    #id?: string
    #revision = 0
    #result: any
    #started = false
    #disposed = false
    #generation = 0
    #latest?: Promise<any>
    // Updates that arrive while a subscribe or resync reply is outstanding.
    #buffer: (QueryUpdate | QueryLagged)[] | null = null
    #unlisten: () => void
    #unreconnect: () => void
    // Aborts the subscribe call still in flight, for `dispose({ cancelOpening })`.
    #opening?: AbortController
    #retryTimer?: ReturnType<typeof setTimeout>
    #reopening = false
    #statusListeners = new Set<(status: LiveQueryStatus) => void>()

    /** `open` subscribes on the executor; aborting its signal cancels the call. */
    constructor(client: PerspectiveClient, uuid: string, open: (signal: AbortSignal) => Promise<Subscribed>, onResult: (result: any) => void) {
        this.#client = client
        this.#uuid = uuid
        this.#open = open
        this.#onResult = onResult
        this.#unlisten = client.onQueryUpdate(update => this.#receive(update))
        this.#unreconnect = client.onReconnect(() => this.#reopen())
    }

    get id(): string | undefined { return this.#id }
    get result(): any { return this.#result }
    /** Whether a re-open failed and the query is waiting to retry it. */
    get reopening(): boolean { return this.#reopening }

    /** Call `listener` on each failed re-open and when a re-open succeeds
     *  after one failed. Returns a remover. */
    onStatus(listener: (status: LiveQueryStatus) => void): () => void {
        this.#statusListeners.add(listener)
        return () => this.#statusListeners.delete(listener)
    }

    /** Open the subscription (again) and return its initial result. A start
     *  that a newer one overtakes resolves with the newer one's result. */
    start(): Promise<any> {
        this.#clearRetry()
        this.#latest = this.#start(++this.#generation)
        return this.#latest
    }

    async #start(generation: number): Promise<any> {
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
            // Disposed while opening (e.g. a newer subscribe() on the same
            // builder): the caller still gets the snapshot that was read.
            return this.#disposed ? (this.#result ?? subscribed.result) : this.#latest
        }
        this.#id = subscribed.subscriptionId
        this.#replace(subscribed.revision, subscribed.result)
        return this.#result
    }

    /** End the subscription. With `cancelOpening`, a subscribe call still in
     *  flight is cancelled too, so the executor stops computing a first result
     *  nobody waits for (#1595); the pending `start()` then rejects instead of
     *  resolving with that result. */
    dispose(options: { cancelOpening?: boolean } = {}) {
        this.#disposed = true
        this.#generation++
        this.#clearRetry()
        this.#statusListeners.clear()
        this.#unlisten()
        this.#unreconnect()
        if (options.cancelOpening) this.#opening?.abort()
        if (this.#id) this.#client.disposeQuerySubscription(this.#uuid, this.#id).catch(() => {})
    }

    /** Open the subscription again, and keep trying with backoff
     *  (`reopenDelay`) while each attempt fails, until one succeeds, a newer
     *  `start()` takes over, or the query is disposed. One failed attempt used
     *  to leave the query dead for good while its owner took it as live: a
     *  client that reconnects to a restarted executor before the perspective
     *  is loaded gets "Perspective not found" (#1595). */
    #reopen(failures = 0) {
        if (this.#disposed) return
        const reopened = this.start()
        const generation = this.#generation
        reopened.then(
            () => {
                if (this.#disposed || generation !== this.#generation || !this.#reopening) return
                this.#reopening = false
                this.#emit({ state: 'open' })
            },
            error => {
                if (this.#disposed || generation !== this.#generation) return
                const retryInMs = reopenDelay(failures + 1)
                console.error(`Error re-opening live query (attempt ${failures + 1}), retrying in ${Math.round(retryInMs)}ms:`, error)
                this.#reopening = true
                this.#emit({ state: 'reopening', error, retryInMs })
                const timer: any = setTimeout(() => {
                    this.#retryTimer = undefined
                    this.#reopen(failures + 1)
                }, retryInMs)
                // Node: a pending retry must not keep the process alive.
                if (typeof timer?.unref === 'function') timer.unref()
                this.#retryTimer = timer
            },
        )
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

    /** Take `result` at `revision`, then apply the updates buffered meanwhile. */
    #replace(revision: number, result: any) {
        this.#revision = revision
        this.#result = result
        const buffered = this.#buffer ?? []
        this.#buffer = null
        if (this.#started) this.#onResult(result)
        this.#started = true
        for (const update of buffered) this.#receive(update)
    }

    #receive(update: QueryUpdate | QueryLagged) {
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
        this.#onResult(this.#result)
    }

    #resync() {
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
