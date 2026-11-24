/** Where a value belongs: the executor, the calling agent, one perspective or one neighbourhood. */
export type Scope = 'executor' | 'agent' | { perspective: string } | { neighbourhood: string }

/** What keeps an entry fresh. */
export type Kind = 'pushed' | 'live' | 'fixed' | 'immutable' | 'declared'

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
    /** True while `value` is the first one the entry ever had. */
    initial: boolean
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

/** What a cache of entries adds to the plain rules of an {@link Entry}. */
export interface EntryOptions<T> {
    /** `false`: the answer reaches its readers but is not kept (a value that is not found). Default: kept. */
    keep?(value: T): boolean
    /** How long a value stays fresh. Default: until the executor says it changed. */
    maxAgeMs?(): number
    /** The entry took `value` (an answer, a push or a seed): the cache's own bookkeeping. */
    kept?(value: T): void
}

/**
 * One data point the executor owns. The transitions follow the mirror's state diagram:
 * a read of an `unknown`, `stale` or `error` entry sends one request (`loading`); the answer
 * makes it `fresh`; a pushed value replaces it; an invalidation, a reconnect or a lag marker
 * makes it `stale`. An answer is kept only when nothing changed the entry since its request
 * went out, so one sent before a reconnect reaches its readers once and is dropped.
 */
export class Entry<T> implements Live<T> {
    readonly kind: Kind
    #load: () => Promise<T>
    #opts: EntryOptions<T>
    #state: State = 'unknown'
    #value: T | undefined
    #error: unknown
    #values = 0
    #ts = 0
    /** Counts what changed the entry from outside a read: a push, an invalidation. */
    #version = 0
    #inflight: Promise<T> | null = null
    #listeners = new Set<(snapshot: Snapshot<T>) => void>()
    #snapshot: Snapshot<T>

    constructor(kind: Kind, load: () => Promise<T>, opts: EntryOptions<T> = {}) {
        this.kind = kind
        this.#load = load
        this.#opts = opts
        this.#snapshot = this.#makeSnapshot()
    }

    get hasListeners(): boolean {
        return this.#listeners.size > 0
    }

    getSnapshot(): Snapshot<T> {
        this.#expire()
        return this.#snapshot
    }

    subscribe(fn: (snapshot: Snapshot<T>) => void): Unsubscribe {
        this.#listeners.add(fn)
        this.#expire()
        if (this.#state !== 'fresh' && this.#state !== 'loading') this.#refetch()
        return () => { this.#listeners.delete(fn) }
    }

    /** `load` replaces the entry's own request function for this read only (a batched read). */
    read(load: () => Promise<T> = this.#load): Promise<T> {
        this.#expire()
        if (this.#state === 'fresh') return Promise.resolve(this.#value as T)
        if (this.#inflight) return this.#inflight
        const version = this.#version
        this.#set('loading')
        const request = load().then(
            (value) => {
                if (version === this.#version) {
                    if (this.#opts.keep && !this.#opts.keep(value)) this.#set(this.#values > 0 ? 'stale' : 'unknown')
                    else this.#accept(value)
                }
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

    /** A value that may be old (from a persistent store): shown, read again by the next read. */
    seed(value: T): void {
        this.#version++
        this.#inflight = null
        this.#value = value
        this.#values++
        this.#ts = Date.now()
        this.#error = undefined
        this.#opts.kept?.(value)
        this.#set('stale')
    }

    /** The executor changed the value without giving it, or the connection lost events: stale. */
    invalidate(): void {
        this.#version++
        this.#inflight = null
        if (this.#state === 'unknown' || this.#state === 'stale') return
        this.#set('stale')
        if (this.#listeners.size > 0) this.#refetch()
    }

    /** Forgets the value: `unknown`; a read in flight is not kept. Subscribers hear of it and do not load. */
    reset(): void {
        this.#version++
        this.#inflight = null
        this.#value = undefined
        this.#values = 0
        this.#error = undefined
        this.#set('unknown')
    }

    #accept(value: T): void {
        const same = this.#listeners.size > 0 && this.#values > 0 && canonical(value) === canonical(this.#value)
        // An equal value stays the same object: subscribers compare snapshots by identity.
        if (!same) {
            this.#value = value
            this.#values++
        }
        this.#ts = Date.now()
        this.#error = undefined
        this.#opts.kept?.(same ? this.#value as T : value)
        if (!same || this.#state !== 'fresh') this.#set('fresh')
    }

    /** A value older than `maxAgeMs` is stale; no one is told, the next read loads. */
    #expire(): void {
        if (this.#state === 'fresh' && Date.now() - this.#ts >= (this.#opts.maxAgeMs?.() ?? Infinity)) {
            this.#state = 'stale'
            this.#snapshot = this.#makeSnapshot()
        }
    }

    #refetch(): void {
        this.read().catch(() => {})
    }

    #makeSnapshot(): Snapshot<T> {
        return { state: this.#state, value: this.#value, error: this.#error, initial: this.#values === 1 }
    }

    #set(state: State): void {
        this.#state = state
        this.#snapshot = this.#makeSnapshot()
        for (const fn of [...this.#listeners]) {
            try { fn(this.#snapshot) } catch (e) { console.error('Error in mirror subscriber:', e) }
        }
    }
}
