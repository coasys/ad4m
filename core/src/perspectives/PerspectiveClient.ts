import { ApiClient, RpcError } from "../apiClient";
import { ExpressionRendered } from "../expression/Expression";
import { ExpressionClient } from "../expression/ExpressionClient";
import { Link, LinkExpressionInput, LinkExpression, LinkMutations, LinkExpressionMutations } from "../links/Links";
import { NeighbourhoodClient } from "../neighbourhood/NeighbourhoodClient";
import { NeighbourhoodProxy } from "../neighbourhood/NeighbourhoodProxy";
import { LinkQuery } from "./LinkQuery";
import { Perspective } from "./Perspective";
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle";
import { LinkStatus, PerspectiveProxy } from './PerspectiveProxy';
import { AIClient } from "../ai/AIClient";
import { AllInstancesResult } from "../model/types";
import type { TranscriptTurn } from "../generated/api";
import type { AddAutoProcessorConfig, AutoProcessorEvent, AutoProcessorNeighbourhoodStateEvent, InterpretationOverlayInfo, RawScope, RunInterpretationObserveOptions } from "./AutoProcessor";

export type PerspectiveHandleCallback = (perspective: PerspectiveHandle) => null
export type UuidCallback = (uuid: string) => null
export type LinkCallback = (link: LinkExpression) => null
export type SyncStateChangeCallback = (state: PerspectiveState) => null

function normalizeQueryResult(raw: unknown, errorContext: string): AllInstancesResult {
    let finalResult: unknown = raw

    if (typeof finalResult === 'string') {
        try {
            finalResult = JSON.parse(finalResult)
        } catch (e) {
            console.error(errorContext, e)
        }
    }

    return finalResult as AllInstancesResult
}

export class PerspectiveClient {
    #apiClient: ApiClient
    #perspectiveAddedCallbacks: PerspectiveHandleCallback[]
    #perspectiveUpdatedCallbacks: PerspectiveHandleCallback[]
    #perspectiveRemovedCallbacks: UuidCallback[]
    #perspectiveSyncStateChangeCallbacks: SyncStateChangeCallback[]
    #expressionClient?: ExpressionClient
    #neighbourhoodClient?: NeighbourhoodClient
    #aiClient?: AIClient
    #unsubscribers: (() => void)[]
    #linkUnsubscribers: Map<string, (() => void)[]>
    #querySubscriptionUnsubscribers: Map<string, () => void>

    constructor(baseUrl: string, token?: string, subscribe: boolean = true, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
        this.#perspectiveAddedCallbacks = []
        this.#perspectiveUpdatedCallbacks = []
        this.#perspectiveRemovedCallbacks = []
        this.#perspectiveSyncStateChangeCallbacks = []
        this.#unsubscribers = []
        this.#linkUnsubscribers = new Map()
        this.#querySubscriptionUnsubscribers = new Map()

        if(subscribe) {
            this.subscribePerspectiveAdded()
            this.subscribePerspectiveUpdated()
            this.subscribePerspectiveRemoved()
        }
    }

    setExpressionClient(client: ExpressionClient) {
        this.#expressionClient = client
    }

    setNeighbourhoodClient(client: NeighbourhoodClient) {
        this.#neighbourhoodClient = client
    }

    setAIClient(client: AIClient) {
        this.#aiClient = client
    }

    get aiClient(): AIClient {
        return this.#aiClient!
    }

    async all(): Promise<PerspectiveProxy[]> {
        const perspectives = await this.#apiClient.call<PerspectiveHandle[]>('perspective.all')
        return perspectives.map(handle => new PerspectiveProxy(handle, this))
    }

    async byUUID(uuid: string): Promise<PerspectiveProxy|null> {
        try {
            const perspective = await this.#apiClient.call<PerspectiveHandle>('perspective.get', { uuid })
            if(!perspective) return null
            return new PerspectiveProxy(perspective, this)
        } catch(e) {
            if (e instanceof RpcError && e.status === 404) return null
            throw e
        }
    }

    async snapshotByUUID(uuid: string): Promise<Perspective|null> {
        return this.#apiClient.call<Perspective|null>('perspective.snapshot', { uuid })
    }

    async publishSnapshotByUUID(uuid: string): Promise<string|null> {
        return this.#apiClient.call<string|null>('perspective.publishSnapshot', { uuid })
    }

    async queryLinks(uuid: string, query: LinkQuery): Promise<LinkExpression[]> {
        const params: Record<string, unknown> = { uuid }
        if (query.source) params.source = query.source
        if (query.predicate) params.predicate = query.predicate
        if (query.target) params.target = query.target
        if (query.fromDate) params.fromDate = query.fromDate instanceof Date ? query.fromDate.toISOString() : String(query.fromDate)
        if (query.untilDate) params.untilDate = query.untilDate instanceof Date ? query.untilDate.toISOString() : String(query.untilDate)
        if (query.limit !== undefined) params.limit = query.limit
        return this.#apiClient.call<LinkExpression[]>('perspective.queryLinks', params)
    }

    async queryProlog(uuid: string, query: string): Promise<unknown> {
        const result = await this.#apiClient.call<string>('perspective.queryProlog', { uuid, query })
        return JSON.parse(result)
    }

    async querySparql<T = any>(uuid: string, query: string): Promise<T> {
        const result = await this.#apiClient.call<string>('perspective.querySparql', { uuid, engine: 'sparql', query })
        return JSON.parse(result) as T
    }

    async subscribeQuery(uuid: string, query: string): Promise<{ subscriptionId: string, result: AllInstancesResult }> {
        const response = await this.#apiClient.call<{ subscriptionId: string, result: unknown }>(
            'perspective.subscribeQuery', { uuid, query }
        )
        const { subscriptionId, result } = response
        const parsed = normalizeQueryResult(result, 'Error parsing subscribeQuery result:')
        return { subscriptionId, result: parsed }
    }

    async perspectiveKeepAliveQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.keepAliveQuery', { uuid, subscriptionId }
        )
    }

    async perspectiveDisposeQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.disposeQuery', { uuid, subscriptionId }
        )
    }

    subscribeToQueryUpdates(subscriptionId: string, onData: (result: AllInstancesResult) => void): () => void {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                const event = data as Record<string, unknown>
                if (event.type !== 'query-subscription-update') return

                const eventSubscriptionId = event.subscriptionId || event.subscription_id
                if (eventSubscriptionId !== subscriptionId) return

                const parsed = normalizeQueryResult(event.result, 'Error parsing query subscription:')
                onData(parsed)
            }
        )
        this.#querySubscriptionUnsubscribers.set(subscriptionId, unsub)
        return unsub
    }

    async keepAliveQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.keepAliveQuery', { uuid, subscriptionId }
        )
    }

    async disposeQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        const unsub = this.#querySubscriptionUnsubscribers.get(subscriptionId)
        if (unsub) {
            unsub()
            this.#querySubscriptionUnsubscribers.delete(subscriptionId)
        }
        return this.#apiClient.call<boolean>(
            'perspective.disposeQuery', { uuid, subscriptionId }
        )
    }

    async modelQuery(uuid: string, className: string, queryJson: string): Promise<any> {
        const resultJson = await this.#apiClient.call<string>(
            'perspective.modelQuery', { uuid, class_name: className, query_json: queryJson }
        )
        return JSON.parse(resultJson)
    }

    async subjectClassesOf(uuid: string, uris: string[]): Promise<Record<string, string[]>> {
        return await this.#apiClient.call<Record<string, string[]>>(
            'perspective.subjectClassesOf', { uuid, uris }
        )
    }

    async evaluateGetters(
        uuid: string,
        className: string,
        instanceIds: string[],
        propertyNames?: string[],
    ): Promise<Record<string, Record<string, any>>> {
        const resultJson = await this.#apiClient.call<string>(
            'perspective.evaluateGetters', {
                uuid,
                class_name: className,
                instance_ids: instanceIds,
                ...(propertyNames && { property_names: propertyNames }),
            }
        )
        return JSON.parse(resultJson)
    }

    async modelSubscribe(uuid: string, className: string, queryJson: string): Promise<{ subscriptionId: string, result: any }> {
        const response = await this.#apiClient.call<{ subscription_id: string, result: string }>(
            'perspective.modelSubscribe', { uuid, class_name: className, query_json: queryJson }
        )
        return {
            subscriptionId: response.subscription_id,
            result: JSON.parse(response.result)
        }
    }

    async add(name: string): Promise<PerspectiveProxy> {
        const handle = await this.#apiClient.call<PerspectiveHandle>('perspective.create', { name })
        return new PerspectiveProxy(handle, this)
    }

    async update(uuid: string, name: string): Promise<PerspectiveProxy> {
        const handle = await this.#apiClient.call<PerspectiveHandle>('perspective.update', { uuid, name })
        return new PerspectiveProxy(handle, this)
    }

    async remove(uuid: string): Promise<{perspectiveRemove: boolean}> {
        const result = await this.#apiClient.call<boolean>('perspective.remove', { uuid })
        return { perspectiveRemove: result }
    }

    async addLink(uuid: string, link: Link, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return this.#apiClient.call<LinkExpression>(
            'perspective.addLink', { uuid, link, status, batchId }
        )
    }

    async addLinks(uuid: string, links: Link[], status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression[]> {
        return this.#apiClient.call<LinkExpression[]>(
            'perspective.addLinks', { uuid, links, status, batchId }
        )
    }

    async removeLinks(uuid: string, links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        return this.#apiClient.call<LinkExpression[]>(
            'perspective.removeLinks', { uuid, links, batchId }
        )
    }

    async linkMutations(uuid: string, mutations: LinkMutations, status?: LinkStatus): Promise<LinkExpressionMutations> {
        return this.#apiClient.call<LinkExpressionMutations>(
            'perspective.linkMutations', { uuid, mutations, status }
        )
    }

    /**
     * Run generic LLM interpretation over a transcript into this perspective's own
     * SHACL subject classes. The target shapes are resolved server-side from the
     * perspective's registered subject classes, so you pass only the transcript.
     * Returns the freshly minted instances (their base URIs + the links written).
     *
     * The server-side call prompts an LLM (up to `INTERPRETATION_MAX_ATTEMPTS`
     * retries on parse failure), so it can legitimately take minutes on slower
     * or CPU-only models. We raise the default 30s RPC timeout here to 20 min
     * (matches the CI `--timeout 1200000` for the interpretation tests).
     *
     * `existingScope` and `mintScope` match the AutoProcessor semantics:
     * `existingScope` constrains the dedup lookup to instances under a
     * subtree; `mintScope` links every FRESHLY-created base as a child of
     * `mintScope.id` via its predicate (upserts of pre-existing instances
     * are NOT re-parented — same rule as the watcher).
     */
    async runInterpretation(
        uuid: string,
        transcript: TranscriptTurn[],
        basePrefix: string,
        classes?: string[],
        existingScope?: RawScope,
        mintScope?: RawScope,
        observe?: RunInterpretationObserveOptions,
    ): Promise<string[]> {
        const RUN_INTERPRETATION_TIMEOUT_MS = 20 * 60 * 1000
        return this.#apiClient.call<string[]>(
            'perspective.runInterpretation',
            {
                uuid, transcript, basePrefix, classes, existingScope, mintScope,
                // Spread rather than always-present, so a client talking to a pre-#903
                // executor sends exactly the params it sent before. `serde` would ignore
                // the extra keys anyway; keeping the wire identical means a bug report
                // from an older node cannot be about these.
                ...(observe ? {
                    observationId: observe.observationId,
                    emitDebugEvents: observe.emitDebugEvents ?? false,
                } : {}),
            },
            RUN_INTERPRETATION_TIMEOUT_MS,
        )
    }

    /**
     * Tool-calling counterpart to {@link runInterpretation}. The LLM sees a
     * live per-class tool surface (`{Class}_query`, `{Class}_propose_create`,
     * `{Class}_propose_link_child`, …) and drives the extraction via tool
     * calls; buffered proposals drain through the same overlay gate the
     * single-shot path uses.
     *
     * `maxToolCalls` bounds the loop and MUST be > 0 — zero would collapse
     * the harness to a no-op final-answer step; use {@link runInterpretation}
     * for the classic single-shot path.
     *
     * Same 20-minute RPC timeout as the single-shot path — an LLM loop
     * that calls several tools can legitimately take longer than one plain
     * generation.
     */
    async runInterpretationWithHarness(
        uuid: string,
        transcript: TranscriptTurn[],
        basePrefix: string,
        maxToolCalls: number,
        classes?: string[],
        modelOverride?: string,
        existingScope?: RawScope,
        // Optional live-debug event surface — same shape/semantics as the
        // single-shot `runInterpretation`. `observationId` names the
        // `processor_id` + `batch_key` on emitted `ToolCall` / `ToolResult`
        // events so a subscribed UI can correlate them to this pass.
        // `emitDebugEvents` is a dead-letter without an observationId
        // (nothing to key against); the server gates on both.
        observationId?: string,
        emitDebugEvents?: boolean,
    ): Promise<string[]> {
        const RUN_INTERPRETATION_TIMEOUT_MS = 20 * 60 * 1000
        return this.#apiClient.call<string[]>(
            'perspective.runInterpretationWithHarness',
            {
                uuid,
                transcript,
                basePrefix,
                maxToolCalls,
                classes,
                modelOverride,
                existingScope,
                observationId,
                emitDebugEvents,
            },
            RUN_INTERPRETATION_TIMEOUT_MS,
        )
    }

    /**
     * Register a neighbourhood auto-processor on this perspective. The executor's
     * watch loop then runs interpretation automatically over new source items
     * (like Flux per channel), coordinating which peer processes each batch via
     * the shared-graph ProcessingClaim, and emits step signals on the events
     * WebSocket (subscribe via {@link addAutoProcessorEventListener}).
     * Returns the processor id.
     */
    async addAutoProcessor(uuid: string, config: AddAutoProcessorConfig): Promise<string> {
        return this.#apiClient.call<string>(
            'perspective.addAutoProcessor', { uuid, ...config },
        )
    }

    /** Delete an auto-processor's config. `false` when there was none to delete. */
    async removeAutoProcessor(uuid: string, processorId: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.removeAutoProcessor', { uuid, processorId },
        )
    }

    /** Pending interpretation overlays (LLM suggestions awaiting human accept/reject). */
    async interpretationOverlays(uuid: string): Promise<InterpretationOverlayInfo[]> {
        return this.#apiClient.call<InterpretationOverlayInfo[]>(
            'perspective.interpretationOverlays', { uuid },
        )
    }

    /** Accept an overlay's suggestion(s): the LLM value becomes the real value and
     *  the overlay is deleted. Omit `property` to accept the whole base. */
    async acceptInterpretation(uuid: string, base: string, property?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.acceptInterpretation', { uuid, base, property },
        )
    }

    /** Reject an overlay's suggestion(s). Omit `property` to reject the whole base
     *  (a rejected `create` deletes the suggested instance). */
    async rejectInterpretation(uuid: string, base: string, property?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.rejectInterpretation', { uuid, base, property },
        )
    }

    /**
     * Subscribe to auto-processor step signals. `cb` fires for every
     * `auto-processor-event` on `uuid` (BatchReady → Claimed/BackedOff/… →
     * Processed), letting a UI show progress and await the next batch.
     *
     * Registered on the per-uuid `#linkUnsubscribers` map so
     * `PerspectiveProxy.dispose()` → `removeAllListeners(uuid)` cleans the
     * subscription up; using the global `#unsubscribers` array would leak
     * callbacks across repeated view lifecycles (CodeRabbit #881 review).
     */
    async addAutoProcessorEventListener(uuid: String, cb: (event: AutoProcessorEvent) => void): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'auto-processor-event' && data.perspectiveUuid === uuid) {
                    cb(data as unknown as AutoProcessorEvent)
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await this.#apiClient.waitForSubscription()
    }

    /**
     * Subscribe to neighbourhood-state events on a perspective. `cb` fires
     * when THIS executor claims / finishes / abandons a batch for any
     * processor on `uuid` — perspective-scoped observability so a UI can
     * render "someone is auto-processing this" without receiving the batch
     * payload or LLM I/O. Registered on the per-uuid `#linkUnsubscribers`
     * map so `PerspectiveProxy.dispose()` sweeps it up.
     */
    async addAutoProcessorNeighbourhoodStateListener(
        uuid: String,
        cb: (event: AutoProcessorNeighbourhoodStateEvent) => void,
    ): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'auto-processor-neighbourhood-state' && data.perspectiveUuid === uuid) {
                    cb(data as unknown as AutoProcessorNeighbourhoodStateEvent)
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await this.#apiClient.waitForSubscription()
    }

    async addLinkExpression(uuid: string, link: LinkExpression, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return this.#apiClient.call<LinkExpression>(
            'perspective.addLinkExpression', { uuid, link, status, batchId }
        )
    }

    async updateLink(uuid: string, oldLink: LinkExpressionInput, newLink: Link, batchId?: string): Promise<LinkExpression> {
        return this.#apiClient.call<LinkExpression>(
            'perspective.updateLink', { uuid, oldLink, newLink, batchId }
        )
    }

    async removeLink(uuid: string, link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        delete link.__typename
        delete link.data.__typename
        delete link.proof.__typename
        delete link.status
        return this.#apiClient.call<boolean>(
            'perspective.removeLink', { uuid, link, batchId }
        )
    }

    async addSdna(uuid: string, name: string, sdnaCode: string | undefined, sdnaType: "subject_class" | "flow" | "custom", shaclJson?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.addSdna', { uuid, name, sdnaCode: sdnaCode || "", sdnaType, shaclJson }
        )
    }

    async addSdnaBatch(uuid: string, entries: { name: string; sdnaCode?: string; sdnaType: "subject_class" | "flow" | "custom"; shaclJson?: string }[]): Promise<boolean[]> {
        return this.#apiClient.call<boolean[]>(
            'perspective.addSdna', { uuid, entries: entries.map(e => ({ ...e, sdnaCode: e.sdnaCode || "" })) }
        )
    }

    async executeCommands(uuid: string, commands: string, expression: string, parameters: string, batchId?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.executeCommands', { uuid, commands, expression, parameters, batchId }
        )
    }

    async createSubject(uuid: string, subjectClass: string, expressionAddress: string, initialValues?: string, batchId?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.createSubject', { uuid, subjectClass, expressionAddress, initialValues, batchId }
        )
    }

    async getSubjectData(uuid: string, subjectClass: string, expressionAddress: string): Promise<string> {
        return this.#apiClient.call<string>(
            'perspective.getSubjectData', { uuid, subjectClass, expressionAddress }
        )
    }

    // ExpressionClient functions, needed for Subjects:
    async getExpression(expressionURI: string): Promise<ExpressionRendered> {
        return await this.#expressionClient!.get(expressionURI)
    }

    async createExpression(content: unknown, languageAddress: string): Promise<string> {
        return await this.#expressionClient!.create(content, languageAddress)
    }

    // Subscriptions:
    addPerspectiveAddedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveAddedCallbacks.push(cb)
    }

    subscribePerspectiveAdded() {
        const unsub = this.#apiClient.subscribe((data) => {
            if (data.type === 'perspective-added') {
                this.#perspectiveAddedCallbacks.forEach(cb => cb(data.perspective as PerspectiveHandle))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addPerspectiveUpdatedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveUpdatedCallbacks.push(cb)
    }

    subscribePerspectiveUpdated() {
        const unsub = this.#apiClient.subscribe((data) => {
            if (data.type === 'perspective-updated') {
                this.#perspectiveUpdatedCallbacks.forEach(cb => cb(data.perspective as PerspectiveHandle))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addPerspectiveSyncedListener(cb: SyncStateChangeCallback) {
        this.#perspectiveSyncStateChangeCallbacks.push(cb)
    }

    async addPerspectiveSyncStateChangeListener(uuid: String, cb: SyncStateChangeCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'sync-state-change' && data.uuid === uuid) {
                    cb.forEach(c => c(data.state as PerspectiveState))
                }
            }
        )
        this.#unsubscribers.push(unsub)
        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    addPerspectiveRemovedListener(cb: UuidCallback) {
        this.#perspectiveRemovedCallbacks.push(cb)
    }

    subscribePerspectiveRemoved() {
        const unsub = this.#apiClient.subscribe((data) => {
            if (data.type === 'perspective-removed') {
                this.#perspectiveRemovedCallbacks.forEach(cb => cb(data.uuid as string))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    async addPerspectiveLinkAddedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'link-added' && data.perspectiveUuid === uuid) {
                    cb.forEach(c => c(data.link as LinkExpression))
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await this.#apiClient.waitForSubscription()
    }

    async addPerspectiveLinkRemovedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'link-removed' && data.perspectiveUuid === uuid) {
                    const link = data.link as LinkExpression & { status?: unknown }
                    if (!link.status) {
                        delete link.status
                    }
                    cb.forEach(c => c(link))
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await this.#apiClient.waitForSubscription()
    }

    async addPerspectiveLinkUpdatedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'link-updated' && data.perspectiveUuid === uuid) {
                    const newLink = data.newLink as LinkExpression & { status?: unknown }
                    const oldLink = data.oldLink as LinkExpression & { status?: unknown }
                    if (!newLink.status) {
                        delete newLink.status
                    }
                    if (!oldLink.status) {
                        delete oldLink.status
                    }
                    cb.forEach(c => c(data as unknown as LinkExpression))
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await this.#apiClient.waitForSubscription()
    }

    /** Unsubscribe all link/sync-state listeners registered for the given perspective UUID.
     *  Called by PerspectiveProxy.dispose() to prevent subscription leaks. */
    removeAllListeners(uuid: string): void {
        const unsubs = this.#linkUnsubscribers.get(uuid)
        if (unsubs) {
            for (const fn of unsubs) {
                try { fn() } catch {}
            }
            this.#linkUnsubscribers.delete(uuid)
        }
    }

    getNeighbourhoodProxy(uuid: string): NeighbourhoodProxy {
        return new NeighbourhoodProxy(this.#neighbourhoodClient!, uuid)
    }

    async createBatch(uuid: string): Promise<string> {
        return this.#apiClient.call<string>('perspective.createBatch', { uuid })
    }

    async commitBatch(uuid: string, batchId: string): Promise<LinkExpressionMutations> {
        return this.#apiClient.call<LinkExpressionMutations>(
            'perspective.commitBatch', { uuid, batchId }
        )
    }
}
