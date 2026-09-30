import {ApiClient, CallOptions, EventFilter, RpcError } from '../apiClient';
import type { EventMap, EventName } from '../generated/api/Events';
import { ExpressionRendered } from "../expression/Expression";
import { ExpressionClient } from "../expression/ExpressionClient";
import {
    Link, LinkExpressionInput, LinkExpression, LinkMutations, LinkExpressionMutations,
    linkExpressionInputToWire, linkExpressionToWire, linkMutationsToWire,
} from "../links/Links";
import { NeighbourhoodClient } from "../neighbourhood/NeighbourhoodClient";
import { NeighbourhoodProxy } from "../neighbourhood/NeighbourhoodProxy";
import { LinkQuery } from "./LinkQuery";
import { Perspective } from "./Perspective";
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle";
import { LinkStatus, PerspectiveProxy } from './PerspectiveProxy';
import { AIClient } from "../ai/AIClient";
import type { QueryLagged, QueryUpdate, Subscribed } from "./LiveQuery";
import type { TranscriptTurn } from "../generated/api";
import type { PerspectiveQueryLinksParams } from "../generated/api/PerspectiveQueryLinksParams";
import type { JsonValue } from "../generated/api/serde_json/JsonValue";
import type { AddAutoProcessorConfig, InterpretationOverlayInfo, RawScope, RunInterpretationObserveOptions } from "./AutoProcessor";
// FlowInstance.ts owns the flow-proposal result types so they sit next to the
// `proposeTransition()` API they describe. `import type` keeps this out of the
// runtime module graph (FlowInstance → PerspectiveProxy → PerspectiveClient
// would otherwise be a cycle).
import type {
    FlowFireOutcome, FlowMintedReceipt, FlowOutputRef, FlowProposeResult,
    FlowReceiptVerdict, FlowValidOutput,
} from "./FlowInstance";


export class PerspectiveClient {
    #apiClient: ApiClient
    #expressionClient?: ExpressionClient
    #neighbourhoodClient?: NeighbourhoodClient
    #aiClient?: AIClient

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
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
        const perspectives = await this.#apiClient.call('perspective.all', {})
        return perspectives.map(handle => new PerspectiveProxy(PerspectiveHandle.fromWire(handle), this))
    }

    async byUUID(uuid: string): Promise<PerspectiveProxy|null> {
        try {
            const perspective = await this.#apiClient.call('perspective.get', { uuid })
            if(!perspective) return null
            return new PerspectiveProxy(PerspectiveHandle.fromWire(perspective), this)
        } catch(e) {
            if (e instanceof RpcError && e.status === 404) return null
            throw e
        }
    }

    async snapshotByUUID(uuid: string): Promise<Perspective|null> {
        const snapshot = await this.#apiClient.call('perspective.snapshot', { uuid })
        return snapshot ? new Perspective(snapshot.links.map(LinkExpression.fromWire)) : null
    }

    async publishSnapshotByUUID(uuid: string): Promise<string|null> {
        return this.#apiClient.call('perspective.publishSnapshot', { uuid })
    }

    async queryLinks(uuid: string, query: LinkQuery, options?: CallOptions): Promise<LinkExpression[]> {
        const params: PerspectiveQueryLinksParams = { uuid }
        if (query.source) params.source = query.source
        if (query.predicate) params.predicate = query.predicate
        if (query.target) params.target = query.target
        if (query.fromDate) params.fromDate = query.fromDate instanceof Date ? query.fromDate.toISOString() : String(query.fromDate)
        if (query.untilDate) params.untilDate = query.untilDate instanceof Date ? query.untilDate.toISOString() : String(query.untilDate)
        if (query.limit !== undefined) params.limit = query.limit
        const links = await this.#apiClient.call('perspective.queryLinks', params, options)
        return links.map(LinkExpression.fromWire)
    }

    async queryProlog(uuid: string, query: string, options?: CallOptions): Promise<unknown> {
        const result = await this.#apiClient.call('perspective.queryProlog', { uuid, query }, options)
        return JSON.parse(result)
    }

    async querySparql<T = any>(uuid: string, query: string, options?: CallOptions): Promise<T> {
        const result = await this.#apiClient.call('perspective.querySparql', { uuid, engine: 'sparql', query }, options)
        return JSON.parse(result) as T
    }

    /** Open a live SPARQL/Prolog query. Updates arrive through {@link onQueryUpdate}. */
    async subscribeQuery(uuid: string, query: string): Promise<Subscribed> {
        return this.#apiClient.call('perspective.subscribeQuery', { uuid, query })
    }

    /** Every `query-subscription-update` event on this client's socket. */
    onQueryUpdate(cb: (update: QueryUpdate | QueryLagged) => void): () => void {
        return this.#apiClient.on('query-subscription-update', cb)
    }

    /** The current result and revision of a live query, after a revision gap. */
    async resyncSubscription(uuid: string, subscriptionId: string): Promise<{ revision: number, result: any }> {
        return this.#apiClient.call('perspective.resyncSubscription', { uuid, subscriptionId })
    }

    /** Ends the subscription on the executor. The caller releases its local listener
     *  with the function {@link onQueryUpdate} returned. */
    async disposeQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#apiClient.call(
            'perspective.disposeQuery', { uuid, subscriptionId }
        )
    }

    async modelQuery(uuid: string, className: string, queryJson: string, options?: CallOptions): Promise<any> {
        const resultJson = await this.#apiClient.call(
            'perspective.modelQuery', { uuid, class_name: className, query_json: queryJson }, options
        )
        return JSON.parse(resultJson)
    }

    async subjectClassesOf(uuid: string, uris: string[]): Promise<Record<string, string[]>> {
        return await this.#apiClient.call(
            'perspective.subjectClassesOf', { uuid, uris }
        )
    }

    async evaluateGetters(
        uuid: string,
        className: string,
        instanceIds: string[],
        propertyNames?: string[],
    ): Promise<Record<string, Record<string, any>>> {
        const resultJson = await this.#apiClient.call(
            'perspective.evaluateGetters', {
                uuid,
                class_name: className,
                instance_ids: instanceIds,
                ...(propertyNames && { property_names: propertyNames }),
            }
        )
        return JSON.parse(resultJson)
    }

    /** Open a live model query. Updates arrive through {@link onQueryUpdate}. */
    async modelSubscribe(uuid: string, className: string, queryJson: string): Promise<Subscribed> {
        return this.#apiClient.call(
            'perspective.modelSubscribe', { uuid, class_name: className, query_json: queryJson }
        )
    }

    async add(name: string): Promise<PerspectiveProxy> {
        const handle = await this.#apiClient.call('perspective.create', { name })
        return new PerspectiveProxy(PerspectiveHandle.fromWire(handle), this)
    }

    async update(uuid: string, name: string): Promise<PerspectiveProxy> {
        const handle = await this.#apiClient.call('perspective.update', { uuid, name })
        return new PerspectiveProxy(PerspectiveHandle.fromWire(handle), this)
    }

    async remove(uuid: string): Promise<{perspectiveRemove: boolean}> {
        const result = await this.#apiClient.call('perspective.remove', { uuid })
        return { perspectiveRemove: result }
    }

    async addLink(uuid: string, link: Link, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        const added = await this.#apiClient.call(
            'perspective.addLink', { uuid, link, status, batchId }
        )
        return LinkExpression.fromWire(added)
    }

    async addLinks(uuid: string, links: Link[], status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression[]> {
        const added = await this.#apiClient.call(
            'perspective.addLinks', { uuid, links, status, batchId }
        )
        return added.map(LinkExpression.fromWire)
    }

    async removeLinks(uuid: string, links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        const removed = await this.#apiClient.call(
            'perspective.removeLinks', { uuid, links: links.map(linkExpressionInputToWire), batchId }
        )
        return removed.map(LinkExpression.fromWire)
    }

    async linkMutations(uuid: string, mutations: LinkMutations, status?: LinkStatus): Promise<LinkExpressionMutations> {
        const diff = await this.#apiClient.call(
            'perspective.linkMutations', { uuid, mutations: linkMutationsToWire(mutations), status }
        )
        return LinkExpressionMutations.fromWire(diff)
    }

    /**
     * Run generic LLM interpretation over a transcript into this perspective's own
     * SHACL subject classes. The target shapes are resolved server-side from the
     * perspective's registered subject classes, so you pass only the transcript.
     * Returns the freshly minted instances (their base URIs + the links written).
     *
     * The server-side call prompts an LLM (up to `INTERPRETATION_MAX_ATTEMPTS`
     * retries on parse failure), so it can legitimately take minutes on slower
     * or CPU-only models, so it defaults to {@link LONG_TIMEOUT_MS}.
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
        options?: CallOptions,
    ): Promise<string[]> {
        return this.#apiClient.call(
            'perspective.runInterpretation',
            {
                uuid, transcript, basePrefix, classes, existingScope, mintScope,
                observationId: observe?.observationId,
                emitDebugEvents: observe?.emitDebugEvents,
            },
            options,
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
     * Defaults to {@link LONG_TIMEOUT_MS}, like the single-shot path.
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
        options?: CallOptions,
    ): Promise<string[]> {
        return this.#apiClient.call(
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
            options,
        )
    }

    /**
     * Register a neighbourhood auto-processor on this perspective. The executor's
     * watch loop then runs interpretation automatically over new source items
     * (like Flux per channel), coordinating which peer processes each batch via
     * the shared-graph ProcessingClaim, and emits step signals on the events
     * WebSocket (subscribe with `on('auto-processor-event', …, { perspective })`).
     * Returns the processor id.
     */
    async addAutoProcessor(uuid: string, config: AddAutoProcessorConfig): Promise<string> {
        return this.#apiClient.call(
            'perspective.addAutoProcessor', { ...config, uuid },
        )
    }

    /** Delete an auto-processor's config. `false` when there was none to delete. */
    async removeAutoProcessor(uuid: string, processorId: string): Promise<boolean> {
        return this.#apiClient.call(
            'perspective.removeAutoProcessor', { uuid, processorId },
        )
    }

    /** Pending interpretation overlays (LLM suggestions awaiting human accept/reject). */
    async interpretationOverlays(uuid: string): Promise<InterpretationOverlayInfo[]> {
        const overlays = await this.#apiClient.call(
            'perspective.interpretationOverlays', { uuid },
        )
        return overlays.map(({ base, kind, run, inferred }) => ({
            base, run, inferred, kind: kind === 'create' ? 'create' : 'update',
        }))
    }

    /** Accept an overlay's suggestion(s): the LLM value becomes the real value and
     *  the overlay is deleted. Omit `property` to accept the whole base. */
    async acceptInterpretation(uuid: string, base: string, property?: string): Promise<boolean> {
        return this.#apiClient.call(
            'perspective.acceptInterpretation', { uuid, base, property },
        )
    }

    /** Reject an overlay's suggestion(s). Omit `property` to reject the whole base
     *  (a rejected `create` deletes the suggested instance). */
    async rejectInterpretation(uuid: string, base: string, property?: string): Promise<boolean> {
        return this.#apiClient.call(
            'perspective.rejectInterpretation', { uuid, base, property },
        )
    }

    async proposeFlowTransition(
        uuid: string,
        instanceUri: string,
        toState: string,
        rationale?: string,
        outputs?: FlowOutputRef[],
    ): Promise<FlowProposeResult> {
        return this.#apiClient.call(
            'perspective.proposeFlowTransition', { uuid, instanceUri, toState, rationale, outputs },
        )
    }

    async acceptFlowProposal(uuid: string, proposalUri: string): Promise<FlowFireOutcome[]> {
        return this.#apiClient.call(
            'perspective.acceptFlowProposal', { uuid, proposalUri },
        )
    }

    /**
     * Withdraw this agent's own links from a proposal. Resolves to how many
     * were retracted — one for a withdrawn vote, more when retracting a
     * proposal this agent opened.
     */
    async rejectFlowProposal(uuid: string, proposalUri: string): Promise<number> {
        const result = await this.#apiClient.call(
            'perspective.rejectFlowProposal', { uuid, proposalUri },
        )
        return result.retractedLinks
    }

    /** Re-decide a flow receipt under this perspective's own flow catalogue. */
    async verifyFlowReceipt(uuid: string, receipt: JsonValue): Promise<FlowReceiptVerdict> {
        return this.#apiClient.call(
            'perspective.verifyFlowReceipt', { uuid, receipt },
        )
    }

    /** The instances that are, as they stand, valid outputs of `flow`
     *  (optionally: of runs settled into terminal state `state`). */
    async flowValidOutputs(uuid: string, flow: string, state?: string): Promise<FlowValidOutput[]> {
        return this.#apiClient.call(
            'perspective.flowValidOutputs', { uuid, flow, state },
        )
    }

    /** Mint and store the receipt for a completed flow run. Fails while the
     *  run has not settled into a terminal state, and when an output's
     *  content no longer matches what the quorum committed to. */
    async mintFlowReceipt(uuid: string, instanceUri: string): Promise<FlowMintedReceipt> {
        return this.#apiClient.call(
            'perspective.mintFlowReceipt', { uuid, instanceUri },
        )
    }

    async addLinkExpression(uuid: string, link: LinkExpression, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        const added = await this.#apiClient.call(
            'perspective.addLinkExpression', { uuid, link: linkExpressionToWire(link), status, batchId }
        )
        return LinkExpression.fromWire(added)
    }

    async updateLink(uuid: string, oldLink: LinkExpressionInput, newLink: Link, batchId?: string): Promise<LinkExpression> {
        const updated = await this.#apiClient.call(
            'perspective.updateLink', { uuid, oldLink: linkExpressionInputToWire(oldLink), newLink, batchId }
        )
        return LinkExpression.fromWire(updated)
    }

    async removeLink(uuid: string, link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        const { status, ...wire } = linkExpressionInputToWire(link)
        return this.#apiClient.call(
            'perspective.removeLink', { uuid, link: wire, batchId }
        )
    }

    async addSdna(uuid: string, name: string, sdnaCode: string | undefined, sdnaType: "subject_class" | "flow" | "custom", shaclJson?: string): Promise<boolean> {
        const result = await this.#apiClient.call(
            'perspective.addSdna', { uuid, name, sdnaCode: sdnaCode || "", sdnaType, shaclJson }
        )
        return typeof result === 'boolean' ? result : result.every(Boolean)
    }

    async addSdnaBatch(uuid: string, entries: { name: string; sdnaCode?: string; sdnaType: "subject_class" | "flow" | "custom"; shaclJson?: string }[]): Promise<boolean[]> {
        const result = await this.#apiClient.call(
            'perspective.addSdna', { uuid, entries: entries.map(e => ({ ...e, sdnaCode: e.sdnaCode || "" })) }
        )
        return typeof result === 'boolean' ? [result] : result
    }

    async executeCommands(uuid: string, commands: string, expression: string, parameters: string, batchId?: string): Promise<boolean> {
        return this.#apiClient.call(
            'perspective.executeCommands', { uuid, commands, expression, parameters, batchId }
        )
    }

    async createSubject(uuid: string, subjectClass: string, expressionAddress: string, initialValues?: string, batchId?: string): Promise<boolean> {
        return this.#apiClient.call(
            'perspective.createSubject', { uuid, subjectClass, expressionAddress, initialValues, batchId }
        )
    }

    async getSubjectData(uuid: string, subjectClass: string, expressionAddress: string): Promise<string> {
        return this.#apiClient.call(
            'perspective.getSubjectData', { uuid, subjectClass, expressionAddress }
        )
    }

    // ── SHACL resolution (server-side) ─────────────────────────────────────────
    // These methods delegate shape resolution to the executor, which reads links
    // from its local store in-process.  Each call replaces the multi-round-trip
    // `queryLinks` sequences the old PerspectiveProxy methods performed.

    /** List the names of every SHACL shape stored in a perspective (one RPC call). */
    async getShaclNames(uuid: string): Promise<string[]> {
        return this.#apiClient.call('perspective.getShaclNames', { uuid })
    }

    /**
     * Resolve a shape's `sh:targetClass` by name (one RPC call).
     *
     * Returns `undefined` when the shape (or its `sh://targetClass` edge) is
     * absent — unified with `PerspectiveProxy.getShaclTargetClass` so callers
     * see a single "not found" representation across both layers. The
     * executor wire format is still `null` (see `perspective.getShaclTargetClass`
     * in `perspectives_ws.rs`); this method maps that at the boundary rather
     * than pushing the null one layer further up. PR #935 review r3897752023.
     */
    async getShaclTargetClass(uuid: string, name: string): Promise<string | undefined> {
        const result = await this.#apiClient.call(
            'perspective.getShaclTargetClass',
            { uuid, name },
        )
        return result ?? undefined
    }

    /**
     * Retrieve a single SHACL shape's link triples by name (one RPC call).
     * Returns `{shapeUri, links}` for reconstruction via `SHACLShape.fromLinks()`,
     * or `null` if no shape with that name exists.
     */
    async getShacl(uuid: string, name: string): Promise<{ shapeUri: string; links: Array<{source: string; predicate: string; target: string}> } | null> {
        return this.#apiClient.call(
            'perspective.getShacl', { uuid, name }
        )
    }

    /**
     * Retrieve all SHACL shapes in one call.  Returns an array of
     * `{name, shapeUri, links}` — one entry per shape (one RPC call).
     */
    async getAllShacl(uuid: string): Promise<Array<{ name: string; shapeUri: string; links: Array<{source: string; predicate: string; target: string}> }>> {
        return this.#apiClient.call(
            'perspective.getAllShacl', { uuid }
        )
    }


    // ExpressionClient functions, needed for Subjects:
    async getExpression(expressionURI: string): Promise<ExpressionRendered> {
        return await this.#expressionClient!.get(expressionURI)
    }

    async createExpression(content: unknown, languageAddress: string): Promise<string> {
        return await this.#expressionClient!.create(content, languageAddress)
    }

    /** The client's event bus; see {@link ApiClient.on}. */
    on<K extends EventName>(type: K, handler: (event: EventMap[K]) => void, filter?: EventFilter): () => void {
        return this.#apiClient.on(type, handler, filter)
    }

    getNeighbourhoodProxy(uuid: string): NeighbourhoodProxy {
        return new NeighbourhoodProxy(this.#neighbourhoodClient!, uuid)
    }

    async createBatch(uuid: string): Promise<string> {
        return this.#apiClient.call('perspective.createBatch', { uuid })
    }

    async commitBatch(uuid: string, batchId: string): Promise<LinkExpressionMutations> {
        const diff = await this.#apiClient.call(
            'perspective.commitBatch', { uuid, batchId }
        )
        return LinkExpressionMutations.fromWire(diff)
    }

    /** Register a callback that fires after a successful WebSocket reconnect.
     *  Passes through to ApiClient.onReconnect(). Returns an unsubscribe function. */
    onReconnect(callback: () => void): () => void {
        return this.#apiClient.onReconnect(callback)
    }
}
