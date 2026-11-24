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

    subscribe(fn: (snapshot: Snapshot<T>) => void): Unsubscribe {
        this.#listeners.add(fn)
        if (this.#state !== 'fresh' && this.#state !== 'loading') this.#refetch()
        return () => { this.#listeners.delete(fn) }
    }

    read(): Promise<T> {
        if (this.#state === 'fresh') return Promise.resolve(this.#value as T)
        if (this.#inflight) return this.#inflight
        const version = this.#version
        this.#set('loading')
        const request = this.#load().then(
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
        this.#inflight = request
        const done = () => { if (this.#inflight === request) this.#inflight = null }
        request.then(done, done)
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
     * {@link reloadWatched}).
     */
    invalidate(refetch = true): void {
        this.#version++
        this.#inflight = null
        if (this.#state === 'unknown' || this.#state === 'stale') return
        this.#set('stale')
        if (refetch && this.#listeners.size > 0) this.#refetch()
    }

    /** A socket opened: a stale entry with subscribers reads once; a fresh or loading one does not. */
    reloadWatched(): void {
        if (this.#state === 'stale' && this.#listeners.size > 0) this.#refetch()
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
        return { state: this.#state, value: this.#value, error: this.#error }
    }

    #set(state: State): void {
        this.#state = state
        this.#snapshot = this.#makeSnapshot()
        for (const fn of [...this.#listeners]) {
            try { fn(this.#snapshot) } catch (e) { console.error('Error in mirror subscriber:', e) }
        }
    }
}
