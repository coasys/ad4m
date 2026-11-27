import { DEFAULT_KEEP_ALIVE_MS, type LiveQuery, type LiveQueryStatus, type LiveSubscription, type Snapshot } from '../perspectives/LiveQuery'
import type { Unsubscribe } from './Entry'

/**
 * What a `live` entry of the mirror is: one subscription holding the executor's raw result,
 * and the handles held on it. Each handle shapes the raw result with its own projection.
 */
export class SharedLive {
    readonly subscription: LiveSubscription
    readonly perspective: string
    #views = 0
    // A result may be old until `changes` passes this: a write of this client settled, or the socket
    // was lost. Until then it answers no one-shot read.
    #barrier?: number
    // The `changes` a write of this client waits for: a handle may join only past it. The socket's loss
    // moves `#barrier` but not this, since a lost socket sends no request that a joining handle could wait on.
    #written?: number
    #timer?: ReturnType<typeof setTimeout>
    #ended = false
    #end: () => void

    /** `end` runs once the grace after the last handle is over: the mirror forgets the entry. */
    constructor(subscription: LiveSubscription, perspective: string, end: () => void) {
        this.subscription = subscription
        this.perspective = perspective
        this.#end = end
    }

    /**
     * The raw result a one-shot read may take, if the subscription is ready and live and
     * no write of this client settled without an update after it: the executor sends the
     * update some time after the write's reply, so until the result changes it may be old.
     */
    answer(): { result: any } | undefined {
        const held = this.subscription.liveResult()
        return held && (this.#barrier === undefined || held.changes > this.#barrier) ? held : undefined
    }

    /**
     * A write of this client settled: the result is trusted again once an update arrives.
     * A result still on its way (first open, resync, re-open) and the updates buffered behind it
     * came before the write, so they do not count.
     */
    wrote(): void {
        this.#barrier = this.#written = this.subscription.changes + this.subscription.pending
    }

    /**
     * Whether a handle may join this subscription: it is usable and no write of this client is
     * waiting for its update. A handle that joined before it would take the result computed
     * before the write as its first one.
     */
    get joinable(): boolean {
        return this.usable && (this.#written === undefined || this.subscription.changes > this.#written)
    }

    /** No handle holds the subscription. */
    get idle(): boolean {
        return this.#views === 0
    }

    /**
     * The socket that keeps the result fresh was lost, or the executor dropped updates: the
     * result may be old. It stays in the snapshots, but answers no one-shot read until the
     * resync (the next result) arrives.
     */
    stale(): void {
        // A write's barrier is not lowered: the result after the loss may still be one computed before the write.
        this.#barrier = Math.max(this.#barrier ?? 0, this.subscription.changes)
    }

    /** A subscription whose first open failed disposed itself: nobody can use it again. */
    get usable(): boolean {
        return this.subscription.getSnapshot().status !== 'error'
    }

    /** A new handle on the subscription; it cancels a grace running. */
    view<T>(project: (raw: any) => T, keepAliveMs: number = DEFAULT_KEEP_ALIVE_MS): LiveQuery<T> {
        clearTimeout(this.#timer)
        this.#timer = undefined
        this.#views++
        return new LiveView(this.subscription, project, () => this.#release(keepAliveMs))
    }

    /** The client closed: end the subscription now, whoever still holds a handle. */
    close(): void {
        clearTimeout(this.#timer)
        this.#timer = undefined
        this.#ended = true
        this.subscription.dispose()
    }

    #release(keepAliveMs: number): void {
        if (--this.#views > 0 || this.#ended) return
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
 * One caller's handle on a shared subscription: its own projection of the raw result, its
 * own listeners and its own `first`; `dispose()` releases this handle only.
 */
class LiveView<T> implements LiveQuery<T> {
    readonly first: Promise<T>
    #subscription: LiveSubscription
    #project: (raw: any) => T
    #release: () => void
    #disposed = false
    #removers = new Set<Unsubscribe>()
    #rejectFirst!: (error: Error) => void
    // The subscription's snapshot this handle's last one was made from, so the same one stays the same object.
    #last?: { from: Snapshot<any>; to: Snapshot<T> }
    // The raw result this handle opened on when the subscription already had one; absent when
    // the subscription was still loading (its own first result is then the opening one).
    #openedOn?: { data: any }

    constructor(subscription: LiveSubscription, project: (raw: any) => T, release: () => void) {
        this.#subscription = subscription
        this.#project = project
        this.#release = release
        // A handle that joins a subscription already holding a result opens on that result.
        const held = subscription.getSnapshot()
        if (held.status === 'ready') this.#openedOn = { data: held.data }
        this.first = new Promise<T>((resolve, reject) => {
            this.#rejectFirst = reject
            // The result the subscription has once it is ready, not the one it opened with.
            subscription.first.then(() => {
                const current = this.getSnapshot()
                if (current.status === 'ready') resolve(current.data as T)
                else reject(current.error)
            }, reject)
        })
        // `first` rejects for whoever reads it; one that does not must not get an unhandled rejection.
        this.first.catch(() => {})
    }

    readonly getSnapshot = (): Snapshot<T> => {
        const from = this.#subscription.getSnapshot()
        const prev = this.#last
        if (prev?.from === from) return prev.to
        let to: Snapshot<T>
        if (from.status !== 'ready') {
            to = from as Snapshot<T>
        } else {
            // `initial` is about this handle: the result it opened on, not the subscription's first.
            const initial = this.#openedOn ? from.data === this.#openedOn.data : from.initial
            try {
                // A change of `reopening` alone leaves the raw result, and so the projected data, as it was.
                const data = prev?.to.status === 'ready' && prev.from.data === from.data ? prev.to.data as T : this.#project(from.data)
                to = { ...from, data, initial }
            } catch (e) {
                if (prev?.to.status !== 'ready') {
                    to = { status: 'error', error: e as Error, reopening: false, initial: false }
                } else {
                    console.error('Live query result could not be read:', e)
                    to = { ...prev.to, reopening: from.reopening, initial }
                }
            }
        }
        this.#last = { from, to }
        return to
    }

    readonly subscribe = (fn: (snapshot: Snapshot<T>) => void): Unsubscribe => {
        if (this.#disposed) return () => {}
        return this.#hold(this.#subscription.subscribe(() => fn(this.getSnapshot())))
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
        // A callback that ran inside `subscribe` may have disposed the handle before it got its remover.
        if (this.#disposed) {
            remove()
            return () => {}
        }
        this.#removers.add(remove)
        return () => {
            this.#removers.delete(remove)
            remove()
        }
    }
}
