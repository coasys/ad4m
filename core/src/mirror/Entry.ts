export type State = 'unknown' | 'loading' | 'fresh' | 'stale' | 'error'

export type Unsubscribe = () => void

/**
 * What an entry holds at one moment. A new object for every change, so a UI store can
 * compare snapshots by identity.
 */
export interface Snapshot<T> {
    state: State
    /** The last value the entry had; kept while `stale`, `loading` again or `error`. */
    value: T | undefined
    /** Why the last read failed; set while `state` is `error`. */
    error: unknown
}

/** A typed view on one entry. */
export interface Live<T> {
    getSnapshot(): Snapshot<T>
    /** Calls `fn` on every change. A subscriber to an entry that is not fresh makes it load. */
    subscribe(fn: (snapshot: Snapshot<T>) => void): Unsubscribe
    /** The value; a fresh entry answers locally, anything else sends one request that concurrent reads share. */
    read(): Promise<T>
}

/** JSON with object keys in sorted order: equal values give equal text. */
export function canonical(value: unknown): string {
    return JSON.stringify(value, (_key, v) =>
        v && typeof v === 'object' && !Array.isArray(v)
            ? Object.fromEntries(Object.keys(v).sort().map((k) => [k, (v as Record<string, unknown>)[k]]))
            : v) ?? 'undefined'
}

/**
 * One data point the executor owns. The transitions follow the mirror's state diagram:
 * a read of an `unknown`, `stale` or `error` entry sends one request (`loading`); the answer
 * makes it `fresh`; a pushed value replaces it; an invalidation, a reconnect or a lag marker
 * makes it `stale`. An answer is kept only when nothing changed the entry since its request
 * went out, so one sent before a reconnect reaches its readers once and is dropped.
 * A failure is never kept. The rules of a keyed store (age, size limits, a persistent tier)
 * are not here: see `KeyedCache`.
 */
export class Entry<T> implements Live<T> {
    #load: () => Promise<T>
    #state: State = 'unknown'
    #value: T | undefined
    #error: unknown
    #hasValue = false
    /** Counts what changed the entry from outside a read: a push, an invalidation. */
    #version = 0
    #inflight: Promise<T> | null = null
    #listeners = new Set<(snapshot: Snapshot<T>) => void>()
    #snapshot: Snapshot<T>

    constructor(load: () => Promise<T>) {
        this.#load = load
        this.#snapshot = this.#makeSnapshot()
    }

    getSnapshot(): Snapshot<T> {
        return this.#snapshot
    }

    /** Changes whenever a push or an invalidation makes an answer on its way old: a write's reply applies only if it is still what it was when the write went out. */
    get version(): number {
        return this.#version
    }

    subscribe(fn: (snapshot: Snapshot<T>) => void): Unsubscribe {
        this.#listeners.add(fn)
        if (this.#state !== 'fresh' && this.#state !== 'loading') this.#refetch()
        return () => { this.#listeners.delete(fn) }
    }

    read(): Promise<T> {
        if (this.#state === 'fresh') return Promise.resolve(this.#value as T)
        if (this.#inflight) return this.#inflight
        const version = this.#version
        // A loader that throws before it returns a promise fails the read like a rejected one.
        let loading: Promise<T>
        try { loading = this.#load() } catch (e) { loading = Promise.reject(e) }
        const request = loading.then(
            (value) => {
                if (version === this.#version) this.#accept(value)
                return value
            },
            (error) => {
                if (version === this.#version) {
                    this.#error = error
                    this.#set('error')
                }
                throw error
            },
        )
        // The flight is in place before `loading` is published: a subscriber that reads then joins it.
        this.#inflight = request
        const done = () => { if (this.#inflight === request) this.#inflight = null }
        request.then(done, done)
        this.#error = undefined
        this.#set('loading')
        return request
    }

    /** A pushed value replaces the entry's; subscribers hear of it unless it equals the one held. */
    push(value: T): void {
        this.#version++
        this.#inflight = null
        this.#accept(value)
    }

    /**
     * The executor changed the value without giving it, or the connection lost events: stale.
     * With subscribers it reads again at once, unless `refetch` is false (no socket: see
     * {@link reloadWatched}). With `deferred`, the entry is stale at once but its subscribers are
     * told, and the read starts, only when the caller runs what was pushed there: a caller that
     * invalidates many entries marks them all before any subscriber can read one. The caller
     * of `deferred` says whether the read still goes out (`refetch` is a wish: a reset or a
     * refusal made while the callbacks ran takes it back).
     */
    invalidate(refetch = true, deferred?: ((refetch: boolean) => void)[]): void {
        this.#version++
        this.#inflight = null
        if (this.#state === 'unknown' || this.#state === 'stale') return
        this.#state = 'stale'
        const snapshot = this.#snapshot = this.#makeSnapshot()
        const tell = (go: boolean) => {
            // A subscriber told before us may have read or pushed: its snapshot is the newer one.
            if (this.#snapshot !== snapshot) return
            this.#tell()
            if (refetch && go && this.#listeners.size > 0 && this.#state === 'stale') this.#refetch()
        }
        if (deferred) deferred.push(tell)
        else tell(true)
    }

    /** A socket opened: an entry with subscribers that is not fresh (stale, or failed while there was no socket) reads once; a fresh or loading one does not. */
    reloadWatched(): void {
        if (this.#listeners.size > 0 && this.#state !== 'fresh' && this.#state !== 'loading') this.#refetch()
    }

    #accept(value: T): void {
        // An equal value stays the same object: subscribers compare snapshots by identity.
        const same = this.#listeners.size > 0 && this.#hasValue && canonical(value) === canonical(this.#value)
        if (!same) {
            this.#value = value
            this.#hasValue = true
        }
        this.#error = undefined
        if (!same || this.#state !== 'fresh') this.#set('fresh')
    }

    #refetch(): void {
        this.read().catch(() => {})
    }

    #makeSnapshot(): Snapshot<T> {
        return { state: this.#state, value: this.#value, error: this.#state === 'error' ? this.#error : undefined }
    }

    #set(state: State): void {
        this.#state = state
        this.#snapshot = this.#makeSnapshot()
        this.#tell()
    }

    #tell(): void {
        for (const fn of [...this.#listeners]) {
            try { fn(this.#snapshot) } catch (e) { console.error('Error in mirror subscriber:', e) }
        }
    }
}
