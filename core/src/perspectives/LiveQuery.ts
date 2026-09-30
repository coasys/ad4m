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
 * subscriptions when it closes). `onResult` gets every result after the
 * first; `start()` returns the first.
 */
export class LiveQuery {
    #client: PerspectiveClient
    #uuid: string
    #open: () => Promise<Subscribed>
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

    constructor(client: PerspectiveClient, uuid: string, open: () => Promise<Subscribed>, onResult: (result: any) => void) {
        this.#client = client
        this.#uuid = uuid
        this.#open = open
        this.#onResult = onResult
        this.#unlisten = client.onQueryUpdate(update => this.#receive(update))
        this.#unreconnect = client.onReconnect(() => {
            this.start().catch(e => console.error('Error re-opening live query after reconnect:', e))
        })
    }

    get id(): string | undefined { return this.#id }
    get result(): any { return this.#result }

    /** Open the subscription (again) and return its initial result. A start
     *  that a newer one overtakes resolves with the newer one's result. */
    start(): Promise<any> {
        this.#latest = this.#start(++this.#generation)
        return this.#latest
    }

    async #start(generation: number): Promise<any> {
        this.#buffer = []
        let subscribed: Subscribed
        try {
            subscribed = await this.#open()
        } catch (e) {
            if (generation === this.#generation) this.#buffer = null
            throw e
        }
        if (this.#disposed || generation !== this.#generation) {
            this.#client.disposeQuerySubscription(this.#uuid, subscribed.subscriptionId).catch(() => {})
            return this.#disposed ? this.#result : this.#latest
        }
        this.#id = subscribed.subscriptionId
        this.#replace(subscribed.revision, subscribed.result)
        return this.#result
    }

    dispose() {
        this.#disposed = true
        this.#generation++
        this.#unlisten()
        this.#unreconnect()
        if (this.#id) this.#client.disposeQuerySubscription(this.#uuid, this.#id).catch(() => {})
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
                    this.start().catch(e => console.error('Error re-opening live query:', e))
                }
            },
        )
    }
}
