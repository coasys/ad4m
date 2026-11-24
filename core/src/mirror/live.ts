import { DEFAULT_KEEP_ALIVE_MS, type LiveQuery, type LiveQueryStatus, type LiveSubscription, type Snapshot, type Unsubscribe } from '../perspectives/LiveQuery'

/** What a `live` entry of the mirror is: one subscription, and the handles held on it. */
export class SharedLive<T> {
    readonly subscription: LiveSubscription<T>
    readonly scope: string
    #views = 0
    // `changes` when a write of this client settled: until the result changes, it may not hold the write.
    #barrier?: number
    #timer?: ReturnType<typeof setTimeout>
    #end: () => void

    /** `end` runs once the grace after the last handle is over: the mirror forgets the entry. */
    constructor(subscription: LiveSubscription<T>, scope: string, end: () => void) {
        this.subscription = subscription
        this.scope = scope
        this.#end = end
    }

    /**
     * The raw result a one-shot read may take, if the subscription is ready and live and
     * no write of this client settled without an update after it: the executor sends the
     * update some time after the write's reply, so until the result changes it may be old.
     */
    answer(): { result: any } | undefined {
        const held = this.subscription.liveResult()
        return held && held.changes !== this.#barrier ? held : undefined
    }

    /**
     * A write of this client settled: the result is trusted again once an update arrives.
     * A result still on its way (first open, resync, re-open) may have been read before
     * the write, so it does not count.
     */
    wrote(): void {
        this.#barrier = this.subscription.changes + (this.subscription.liveResult() ? 0 : 1)
    }

    /** A subscription whose first open failed disposed itself: nobody can use it again. */
    get usable(): boolean {
        return this.subscription.getSnapshot().status !== 'error'
    }

    /** A new handle on the subscription; it cancels a grace running. */
    view(keepAliveMs: number = DEFAULT_KEEP_ALIVE_MS): LiveQuery<T> {
        clearTimeout(this.#timer)
        this.#timer = undefined
        this.#views++
        return new LiveView(this.subscription, () => this.#release(keepAliveMs))
    }

    #release(keepAliveMs: number): void {
        if (--this.#views > 0) return
        this.#timer = setTimeout(() => {
            this.#timer = undefined
            this.subscription.dispose()
            this.#end()
        }, keepAliveMs)
        // Node: a pending grace must not keep the process alive.
        const timer: any = this.#timer
        if (typeof timer?.unref === 'function') timer.unref()
    }
}

/**
 * One caller's handle on a shared subscription: its own listeners and its own `first`;
 * `dispose()` releases this handle only.
 */
class LiveView<T> implements LiveQuery<T> {
    readonly first: Promise<T>
    #subscription: LiveSubscription<T>
    #release: () => void
    #disposed = false
    #removers = new Set<Unsubscribe>()
    #rejectFirst!: (error: Error) => void

    constructor(subscription: LiveSubscription<T>, release: () => void) {
        this.#subscription = subscription
        this.#release = release
        this.first = new Promise<T>((resolve, reject) => {
            this.#rejectFirst = reject
            subscription.first.then(resolve, reject)
        })
        // `first` rejects for whoever reads it; one that does not must not get an unhandled rejection.
        this.first.catch(() => {})
    }

    readonly getSnapshot = (): Snapshot<T> => this.#subscription.getSnapshot()

    readonly subscribe = (fn: (snapshot: Snapshot<T>) => void): Unsubscribe => {
        if (this.#disposed) return () => {}
        return this.#hold(this.#subscription.subscribe(fn))
    }

    onStatus(listener: (status: LiveQueryStatus) => void): Unsubscribe {
        if (this.#disposed) return () => {}
        return this.#hold(this.#subscription.onStatus(listener))
    }

    dispose(): void {
        if (this.#disposed) return
        this.#disposed = true
        for (const remove of [...this.#removers]) remove()
        this.#removers.clear()
        this.#rejectFirst(new Error('Live query disposed before its first result'))
        this.#release()
    }

    #hold(remove: Unsubscribe): Unsubscribe {
        this.#removers.add(remove)
        return () => {
            this.#removers.delete(remove)
            remove()
        }
    }
}
