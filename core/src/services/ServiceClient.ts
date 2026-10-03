import type { ApiClient, CallOptions } from '../apiClient'

/** Method table of one interface version: method → params, result, and a
 *  streaming method's chunk (its stream event's payload). */
export type ServiceMethodTable = Record<string, { params: unknown; result: unknown; chunk?: unknown }>
/** Event table of one interface version: event → payload. */
export type ServiceEventTable = Record<string, unknown>

/**
 * One interface version, as `ad4m service-gen` emits it. `M` and `E` carry
 * the method and event types; the fields drive the wire protocol.
 */
export interface ServiceDefinition<M extends ServiceMethodTable = ServiceMethodTable, E extends ServiceEventTable = ServiceEventTable> {
    /** Interface version hash: the wire prefix `<hash>.<method>`. */
    hash: string
    /** The module ID, `service://<genesis hash>` (the hash fixes the author). */
    moduleId: string
    name: string
    version: string
    /** Idempotent methods: resent once after a reconnect. */
    read: ReadonlySet<string>
    /** Methods that may run for minutes. */
    long: ReadonlySet<string>
    /** Streaming method → the event its chunks arrive as. */
    streams: Partial<Record<keyof M & string, keyof E & string>>
    /** Event → the payload field `events.watch` narrows on. */
    scopes: Partial<Record<keyof E & string, string>>
    /** Type carrier only; never set. */
    readonly __types?: { methods: M; events: E }
}

/** Options of {@link ServiceClient}. */
export interface ServiceClientOptions {
    /** Call this implementation hash instead of letting the executor pick one. */
    implementation?: string
}

/** Narrows {@link ServiceClient.on} to events whose scope field has this value. */
export interface ServiceEventFilter {
    scope?: string
}

/** How long a finished stream waits for its end marker. */
const STREAM_END_GRACE_MS = 10_000

function streamId(): string {
    const c = (globalThis as { crypto?: { randomUUID?: () => string } }).crypto
    return c?.randomUUID?.() ?? `s-${Date.now().toString(36)}-${Math.random().toString(36).slice(2)}`
}

/**
 * A typed client for one service interface version. Methods go out as `<hash>.<method>`; events arrive as
 * `<hash>.<event>` through the client's `events.watch`.
 */
export class ServiceClient<M extends ServiceMethodTable, E extends ServiceEventTable> {
    readonly #api: ApiClient
    readonly #def: ServiceDefinition<M, E>
    readonly #target: string

    constructor(api: ApiClient, def: ServiceDefinition<M, E>, options?: ServiceClientOptions) {
        this.#api = api
        this.#def = def
        this.#target = options?.implementation ?? def.hash
    }

    get definition(): ServiceDefinition<M, E> {
        return this.#def
    }

    /** Call `method`. Rejects with an `RpcError`; a declared method error carries `data.name`. */
    call<K extends keyof M & string>(method: K, params: M[K]['params'], options?: CallOptions): Promise<M[K]['result']> {
        return this.#api.callMethod(
            `${this.#target}.${method}`,
            params,
            { read: this.#def.read.has(method), long: this.#def.long.has(method) },
            options,
        ) as Promise<M[K]['result']>
    }

    /** Call `handler` with each `event`, or only those in `filter.scope`. Returns the unsubscribe. */
    on<K extends keyof E & string>(event: K, handler: (event: E[K]) => void, filter?: ServiceEventFilter): () => void {
        return this.#api.onEvent(
            `${this.#def.hash}.${event}`,
            handler as (event: never) => void,
            this.#def.scopes[event],
            filter?.scope,
        )
    }

    /**
     * Call a streaming method: watch its chunk event under a fresh `streamId`
     * first, so no chunk is missed, then call. Resolves with the final result.
     */
    async stream<K extends keyof M & string>(
        method: K,
        params: Omit<M[K]['params'] & object, 'streamId'>,
        onChunk: (chunk: M[K]['chunk']) => void,
        options?: CallOptions,
    ): Promise<M[K]['result']> {
        const event = this.#def.streams[method]
        if (!event) throw new Error(`${this.#def.name}.${method} does not stream`)
        const id = streamId()
        const off = this.on(event, onChunk as (e: E[typeof event]) => void, { scope: id })
        // The reply can overtake the last chunks; `service-stream-end` travels
        // behind them, so the stream ends when it arrives.
        let ended: () => void = () => {}
        const end = new Promise<void>(resolve => { ended = resolve })
        const offEnd = this.#api.on('service-stream-end', () => ended(), { perspective: id })
        let timer: ReturnType<typeof setTimeout> | undefined
        try {
            const result = await this.call(method, { ...params, streamId: id } as M[K]['params'], options)
            // A reconnect can lose the end marker; do not wait forever for it.
            await Promise.race([end, new Promise<void>(resolve => { timer = setTimeout(resolve, STREAM_END_GRACE_MS) })])
            return result
        } finally {
            clearTimeout(timer)
            off()
            offEnd()
        }
    }
}
