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
import { PerspectiveHandle } from "./PerspectiveHandle";
import { forgetPerspective, keepPerspective, perspectiveList } from "./perspectiveList";
import type { Live } from "../mirror/Mirror";
import { LinkStatus, PerspectiveProxy } from './PerspectiveProxy';
import { AIClient } from "../ai/AIClient";
import { LiveSubscription, type LiveOptions, type LiveQuery, type QueryLagged, type QueryUpdate, type Subscribed } from "./LiveQuery";
import type { TranscriptTurn } from "../generated/api";
import type { AgentMessage } from "../generated/api/AgentMessage";
import type { AgentReply } from "../generated/api/AgentReply";
import type { PerspectiveQueryLinksParams } from "../generated/api/PerspectiveQueryLinksParams";
import type { JsonValue } from "../generated/api/serde_json/JsonValue";
import type { FlowFiredEntry } from "../generated/api/FlowFiredEntry";
import type {
    CollectionItemAdded, CollectionItemRemoved, InstanceAddToCollection, InstanceCreate,
    InstanceCreated, InstanceRemove, InstanceRemoveFromCollection, InstanceRemoved,
    InstanceUpdate, InstanceUpdated, RegisterResult, ShapeJson, ShapeSummary,
} from "../generated/api";
import type { AddAutoProcessorConfig, InterpretationOverlayInfo, RawScope, RunInterpretationObserveOptions } from "./AutoProcessor";
// FlowInstance.ts owns the flow-proposal result types so they sit next to the
// `proposeTransition()` API they describe. `import type` keeps this out of the
// runtime module graph (FlowInstance → PerspectiveProxy → PerspectiveClient
// would otherwise be a cycle).
import type {
    FlowFireOutcome, FlowMintedReceipt, FlowOutputRef, FlowProposeResult,
    FlowReceiptVerdict, FlowStateEntry, FlowValidOutput,
} from "./FlowInstance";


/** The wire form of a model query's classes: `class_name` for one, `class_names` for a union. */
function classParams(className: string | string[]): { class_name: string } | { class_names: string[] } {
    return Array.isArray(className) ? { class_names: className } : { class_name: className }
}

type LiveQueryText = { sparql: string } | { className: string | string[], queryJson: string }

/** What a live query sends to open, besides the perspective and the subscription id: its key's params. */
function subscribeWire(query: LiveQueryText) {
    return 'sparql' in query
        ? { sparql: query.sparql }
        : { model: { ...(Array.isArray(query.className) ? { classNames: query.className } : { className: query.className }), queryJson: query.queryJson } }
}

/** A new live query's id (a UUID v4). The client mints it, so it can end the
 *  subscription without the subscribe call's reply. `getRandomValues`, not
 *  `randomUUID`: a page served over plain HTTP has only the former. */
function mintSubscriptionId(): string {
    const b = globalThis.crypto.getRandomValues(new Uint8Array(16))
    b[6] = (b[6] & 0x0f) | 0x40
    b[8] = (b[8] & 0x3f) | 0x80
    const hex = Array.from(b, x => x.toString(16).padStart(2, '0')).join('')
    return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`
}

export class PerspectiveClient {
    #apiClient: ApiClient
    #expressionClient?: ExpressionClient
    #neighbourhoodClient?: NeighbourhoodClient
    #neighbourhoodProxies = new Map<string, NeighbourhoodProxy>()
    #stopWatchingRemoved?: () => void
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

    /**
     * Every perspective of the agent. One list is kept per client and followed: the
     * executor's `perspective-added`, `-updated` and `-removed` events and the handles that
     * {@link add}, {@link update}, {@link remove} and `neighbourhood.joinFromUrl` return
     * change it, so a read of a fresh list is local. Each call returns new proxies.
     */
    async all(): Promise<PerspectiveProxy[]> {
        const handles = await perspectiveList(this.#apiClient).read()
        return handles.map(handle => new PerspectiveProxy(handle, this))
    }

    /** Views on the client's mirror entries: subscribe for changes, or read. */
    readonly live = {
        /** The agent's perspectives as handles; {@link all} reads it. */
        all: (): Live<PerspectiveHandle[]> => perspectiveList(this.#apiClient),
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

    /**
     * Concurrent identical calls (same perspective and query, no `options`)
     * share one RPC, through the mirror's one-shot layer (`Mirror.oneShot`). Each caller
     * parses its own copy of the reply, and a call made after the shared one settled makes
     * a new RPC. So does a call made after a write through this client settled: the shared
     * RPC may have been answered before the write (see `#wrote`).
     */
    async querySparql<T = any>(uuid: string, query: string, options?: CallOptions): Promise<T> {
        const live = this.#liveAnswer(uuid, { sparql: query })
        if (live) return live.result as T
        const params = { uuid, engine: 'sparql' as const, query }
        if (options) return JSON.parse(await this.#apiClient.call('perspective.querySparql', params, options)) as T
        return JSON.parse(await this.#apiClient.mirror.oneShot({
            perspective: uuid,
            method: 'perspective.querySparql',
            params,
            request: () => this.#apiClient.call('perspective.querySparql', params),
        })) as T
    }

    /**
     * Await a write to `uuid`, then tell the mirror (`Mirror.wrote`) a write settled, also
     * when it failed: it may still have changed the perspective. One-shot calls already
     * waiting keep sharing their RPC; an identical call started from now on makes its own,
     * and the live queries of the perspective answer no one-shot until an update arrives.
     */
    async #wrote<T>(uuid: string, write: Promise<T>): Promise<T> {
        try {
            return await write
        } finally {
            this.#apiClient.mirror.wrote(uuid)
        }
    }

    /**
     * A live query handle on the client's mirror. Handles with one perspective, query and
     * parameters share one executor subscription (see {@link LiveQuery}); `open` runs only
     * for the first. `project` builds a handle's data from a raw result.
     */
    liveQuery<T>(uuid: string, query: LiveQueryText, project: (raw: any) => T, options: LiveOptions = {}): LiveQuery<T> {
        return this.#apiClient.mirror.live<T>({
            perspective: uuid,
            method: 'perspective.subscribe',
            params: subscribeWire(query),
            keepAliveMs: options.keepAliveMs,
            open: () => new LiveSubscription<T>(
                this, uuid,
                signal => this.subscribe(uuid, query, { signal, timeoutMs: options.timeoutMs }),
                project,
            ),
        })
    }

    /**
     * The result of an open live query with this text, while it is ready and live: what a
     * one-shot read of the same question answers, so it needs no request. The one-shot's
     * params are the live query's: a one-shot with a cursor (`after`) differs in its
     * `queryJson`, so it is no match.
     */
    #liveAnswer(uuid: string, query: LiveQueryText): { result: any } | undefined {
        return this.#apiClient.mirror.liveResult({
            perspective: uuid, method: 'perspective.subscribe', params: subscribeWire(query),
        })
    }

    /** Open a live query: a SPARQL query, or a model query over one class or
     *  several (#1238). Updates arrive through {@link onQueryUpdate}.
     *  `options.signal` cancels the call, and the executor stops computing the first result. */
    async subscribe(
        uuid: string,
        query: LiveQueryText,
        options?: CallOptions,
    ): Promise<Subscribed> {
        const subscriptionId = mintSubscriptionId()
        const wire = subscribeWire(query)
        return this.#released(uuid, subscriptionId, this.#apiClient.call(
            'perspective.subscribe', { uuid, subscriptionId, ...wire }, options,
        ))
    }

    /** A subscribe call that failed (timed out, aborted, refused) leaves no live
     *  query on the executor that nobody holds. The client minted its id, so it
     *  disposes it without the reply. A timeout or an abort sends `request.cancel`
     *  first, and the executor handles a cancel before the calls after it: the
     *  cancel either stops the call before it registers, or the dispose finds the
     *  subscription. The dispose goes out before the call's error is thrown, and
     *  is not awaited; one that fails changes nothing. */
    async #released(uuid: string, subscriptionId: string, call: Promise<Subscribed>): Promise<Subscribed> {
        try {
            return await call
        } catch (e) {
            this.disposeQuerySubscription(uuid, subscriptionId).catch(() => {})
            throw e
        }
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

    /** `className` may name several classes: one query over their union (#1238). */
    async modelQuery(uuid: string, className: string | string[], queryJson: string, options?: CallOptions): Promise<any> {
        const live = this.#liveAnswer(uuid, { className, queryJson })
        if (live) return live.result
        const params = { uuid, ...classParams(className), query_json: queryJson }
        if (options) return JSON.parse(await this.#apiClient.call('perspective.modelQuery', params, options))
        return JSON.parse(await this.#apiClient.mirror.oneShot({
            perspective: uuid,
            method: 'perspective.modelQuery',
            params,
            request: () => this.#apiClient.call('perspective.modelQuery', params),
        }))
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

    async add(name: string): Promise<PerspectiveProxy> {
        const handle = PerspectiveHandle.fromWire(await this.#apiClient.call('perspective.create', { name }))
        keepPerspective(this.#apiClient, handle)
        return new PerspectiveProxy(handle, this)
    }

    async update(uuid: string, name: string): Promise<PerspectiveProxy> {
        const handle = PerspectiveHandle.fromWire(await this.#apiClient.call('perspective.update', { uuid, name }))
        keepPerspective(this.#apiClient, handle)
        return new PerspectiveProxy(handle, this)
    }

    async remove(uuid: string): Promise<{perspectiveRemove: boolean}> {
        const result = await this.#apiClient.call('perspective.remove', { uuid })
        if (result) forgetPerspective(this.#apiClient, uuid)
        this.#neighbourhoodProxies.delete(uuid)
        return { perspectiveRemove: result }
    }

    async addLink(uuid: string, link: Link, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        const added = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.addLink', { uuid, link, status, batchId }
        ))
        return LinkExpression.fromWire(added)
    }

    async addLinks(uuid: string, links: Link[], status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression[]> {
        const added = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.addLinks', { uuid, links, status, batchId }
        ))
        return added.map(LinkExpression.fromWire)
    }

    async removeLinks(uuid: string, links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        const removed = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.removeLinks', { uuid, links: links.map(linkExpressionInputToWire), batchId }
        ))
        return removed.map(LinkExpression.fromWire)
    }

    async linkMutations(uuid: string, mutations: LinkMutations, status?: LinkStatus): Promise<LinkExpressionMutations> {
        const diff = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.linkMutations', { uuid, mutations: linkMutationsToWire(mutations), status }
        ))
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
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.runInterpretation',
            {
                uuid, transcript, basePrefix, classes, existingScope, mintScope,
                observationId: observe?.observationId,
                emitDebugEvents: observe?.emitDebugEvents,
            },
            options,
        ))
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
        return this.#wrote(uuid, this.#apiClient.call(
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
        ))
    }

    /**
     * A conversation with a model in this perspective, using the executor's own tools: the tools
     * that read this perspective, and only the writing tools named in `writeTools`. Every tool a
     * session offers acts on this perspective alone, so tools about other perspectives, the agent
     * or the node (listing or publishing perspectives, the profile) are not available. Runs as the
     * caller, with the caller's capabilities, and answers with the model's final reply.
     *
     * Defaults to {@link LONG_TIMEOUT_MS}: an agent may read documentation and make many tool
     * calls before it answers.
     */
    async runAgent(
        uuid: string,
        messages: AgentMessage[],
        settings?: {
            /** What the person is looking at, as a hint for the model (`{ recordId }`). */
            context?: Record<string, unknown>,
            /** Tools that write which the model may call. Absent: the session only reads. */
            writeTools?: string[],
            /** A model id; the node's default language model when absent. */
            model?: string,
            /** How many tool calls before the model must answer. Default 20. */
            maxToolCalls?: number,
        },
        options?: CallOptions,
    ): Promise<AgentReply> {
        const reply = this.#apiClient.call('perspective.runAgent', { uuid, messages, ...settings }, options)
        // Without write tools the session only reads.
        return settings?.writeTools?.length ? this.#wrote(uuid, reply) : reply
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
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.addAutoProcessor', { ...config, uuid },
        ))
    }

    /** Delete an auto-processor's config. `false` when there was none to delete. */
    async removeAutoProcessor(uuid: string, processorId: string): Promise<boolean> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.removeAutoProcessor', { uuid, processorId },
        ))
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
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.acceptInterpretation', { uuid, base, property },
        ))
    }

    /** Reject an overlay's suggestion(s). Omit `property` to reject the whole base
     *  (a rejected `create` deletes the suggested instance). */
    async rejectInterpretation(uuid: string, base: string, property?: string): Promise<boolean> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.rejectInterpretation', { uuid, base, property },
        ))
    }

    async proposeFlowTransition(
        uuid: string,
        instanceUri: string,
        toState: string,
        rationale?: string,
        outputs?: FlowOutputRef[],
    ): Promise<FlowProposeResult> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.proposeFlowTransition', { uuid, instanceUri, toState, rationale, outputs },
        ))
    }

    async acceptFlowProposal(uuid: string, proposalUri: string): Promise<FlowFireOutcome[]> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.acceptFlowProposal', { uuid, proposalUri },
        ))
    }

    /**
     * Withdraw this agent's own links from a proposal. Resolves to how many
     * were retracted — one for a withdrawn vote, more when retracting a
     * proposal this agent opened.
     */
    async rejectFlowProposal(uuid: string, proposalUri: string): Promise<number> {
        const result = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.rejectFlowProposal', { uuid, proposalUri },
        ))
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

    /** The calling agent's recorded state of each flow instance it has
     *  derived, optionally narrowed to `instances`. */
    async flowStates(uuid: string, instances?: string[]): Promise<FlowStateEntry[]> {
        return this.#apiClient.call(
            'perspective.flowStates', { uuid, instances },
        )
    }

    /** For each of `instances`, in order, the proposals the calling agent's
     *  replica recorded as fired, sorted. One call covers up to 200
     *  instances (more is refused with a 400); split a longer list into
     *  batches of 200. */
    async flowFired(uuid: string, instances: string[]): Promise<FlowFiredEntry[]> {
        return this.#apiClient.call(
            'perspective.flowFired', { uuid, instances },
        )
    }

    /** Mint and store the receipt for a completed flow run. Fails while the
     *  run has not settled into a terminal state, and when an output's
     *  content no longer matches what the quorum committed to. */
    async mintFlowReceipt(uuid: string, instanceUri: string): Promise<FlowMintedReceipt> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.mintFlowReceipt', { uuid, instanceUri },
        ))
    }

    async addLinkExpression(uuid: string, link: LinkExpression, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        const added = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.addLinkExpression', { uuid, link: linkExpressionToWire(link), status, batchId }
        ))
        return LinkExpression.fromWire(added)
    }

    async updateLink(uuid: string, oldLink: LinkExpressionInput, newLink: Link, batchId?: string): Promise<LinkExpression> {
        const updated = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.updateLink', { uuid, oldLink: linkExpressionInputToWire(oldLink), newLink, batchId }
        ))
        return LinkExpression.fromWire(updated)
    }

    async removeLink(uuid: string, link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        const { status, ...wire } = linkExpressionInputToWire(link)
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.removeLink', { uuid, link: wire, batchId }
        ))
    }

    /**
     * Register model classes, each as the class of its target class IRI. A
     * class whose stored shape already is the given one is left alone; the
     * others are rewritten in one diff. Answers once the executor reads the
     * new shapes.
     */
    async registerShapes(uuid: string, shapes: ShapeJson[]): Promise<RegisterResult> {
        return this.#wrote(uuid, this.#apiClient.call('perspective.registerShapes', { uuid, shapes }))
    }

    /** The registered classes (only those in `names` when given), sorted by name. */
    async shapes(uuid: string, names?: string[]): Promise<ShapeSummary[]> {
        return this.#apiClient.call('perspective.shapes', { uuid, names })
    }

    async executeCommands(uuid: string, commands: string, expression: string, parameters: string, batchId?: string): Promise<boolean> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.executeCommands', { uuid, commands, expression, parameters, batchId }
        ))
    }

    async createSubject(uuid: string, subjectClass: string, expressionAddress: string, initialValues?: string, batchId?: string): Promise<boolean> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.createSubject', { uuid, subjectClass, expressionAddress, initialValues, batchId }
        ))
    }

    /** Through an abstract class (#1435), the executor writes to the record's own classes. */
    async updateSubject(uuid: string, subjectClass: string, expressionAddress: string, values: string, batchId?: string): Promise<boolean> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.updateSubject', { uuid, subjectClass, expressionAddress, values, batchId }
        ))
    }

    async removeSubject(uuid: string, subjectClass: string, expressionAddress: string, batchId?: string): Promise<boolean> {
        return this.#wrote(uuid, this.#apiClient.call(
            'perspective.removeSubject', { uuid, subjectClass, expressionAddress, batchId }
        ))
    }

    /**
     * Validated writes (`perspective.instance*`): the executor checks each against the class's
     * SHACL shape — each property's type, which are required, which take a single value, `sh:in`
     * — before writing anything. A refused write rejects with an `RpcError` whose `data` is an
     * `InstanceError` and stages nothing for that call; a shape violation is status 422 with
     * `data.kind === 'validation'` and `data.errors` naming each property.
     *
     * With `batchId` the write is staged into that open batch and lands on its commit; the
     * link reads a write does (collection membership, the remove cascade) see the batch's own
     * staged work, but an instance created in the same batch is not found — instance existence
     * reads committed state only, so a create followed by an update, remove or
     * `instanceAddToCollection` of the new instance in one batch fails `notFound`; put a new
     * instance's initial collection items on the create itself. A failure that is not a
     * refusal — a store error mid-write, `failed` / `partiallyRemoved` — can leave part of the
     * write staged in the batch: discard it ({@link discardBatch}) instead of committing
     * it to keep the calls that succeeded.
     */
    async instanceCreate(uuid: string, body: InstanceCreate): Promise<InstanceCreated> {
        return this.#wrote(uuid, this.#apiClient.call('perspective.instanceCreate', { uuid, ...body }))
    }

    /** Sets the given single-valued properties; the others stay. See {@link instanceCreate}. */
    async instanceUpdate(uuid: string, body: InstanceUpdate): Promise<InstanceUpdated> {
        return this.#wrote(uuid, this.#apiClient.call('perspective.instanceUpdate', { uuid, ...body }))
    }

    /** Removes the instance and every link to and from it. See {@link instanceCreate}. */
    async instanceRemove(uuid: string, body: InstanceRemove): Promise<InstanceRemoved> {
        return this.#wrote(uuid, this.#apiClient.call('perspective.instanceRemove', { uuid, ...body }))
    }

    /** Adds an item to a collection (a no-op when it is a member). See {@link instanceCreate}. */
    async instanceAddToCollection(uuid: string, body: InstanceAddToCollection): Promise<CollectionItemAdded> {
        return this.#wrote(uuid, this.#apiClient.call('perspective.instanceAddToCollection', { uuid, ...body }))
    }

    /** Removes an item's membership link from a collection. See {@link instanceCreate}. */
    async instanceRemoveFromCollection(uuid: string, body: InstanceRemoveFromCollection): Promise<CollectionItemRemoved> {
        return this.#wrote(uuid, this.#apiClient.call('perspective.instanceRemoveFromCollection', { uuid, ...body }))
    }

    async getSubjectData(uuid: string, subjectClass: string, expressionAddress: string): Promise<string> {
        return this.#apiClient.call(
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

    /** The client's event bus; see {@link ApiClient.on}. */
    on<K extends EventName>(type: K, handler: (event: EventMap[K]) => void, filter?: EventFilter): () => void {
        return this.#apiClient.on(type, handler, filter)
    }

    /** The perspective's neighbourhood proxy: one per uuid, so its sessions and handlers are shared. */
    getNeighbourhoodProxy(uuid: string): NeighbourhoodProxy {
        // Registered once, with the first proxy: a perspective removed by another client of
        // this executor takes its proxy with it, not only one removed through `remove()`.
        this.#stopWatchingRemoved ??= this.#apiClient.on('perspective-removed', ({ perspectiveUuid }) => {
            this.#neighbourhoodProxies.delete(perspectiveUuid)
        })
        let proxy = this.#neighbourhoodProxies.get(uuid)
        if (!proxy) {
            proxy = new NeighbourhoodProxy(this.#neighbourhoodClient!, uuid, this.#apiClient)
            this.#neighbourhoodProxies.set(uuid, proxy)
        }
        return proxy
    }

    async createBatch(uuid: string): Promise<string> {
        return this.#apiClient.call('perspective.createBatch', { uuid })
    }

    async commitBatch(uuid: string, batchId: string): Promise<LinkExpressionMutations> {
        const diff = await this.#wrote(uuid, this.#apiClient.call(
            'perspective.commitBatch', { uuid, batchId }
        ))
        return LinkExpressionMutations.fromWire(diff)
    }

    /**
     * Drops an open batch without committing it — the recovery for a failed batched call:
     * a failed call does not roll back the calls before it and may have staged part of its
     * own work, so discard rather than commit to keep the successful calls. Resolves `true`
     * when the batch was open and is now dropped.
     */
    async discardBatch(uuid: string, batchId: string): Promise<boolean> {
        return this.#apiClient.call('perspective.discardBatch', { uuid, batchId })
    }

    /** Register a callback that fires after a successful WebSocket reconnect.
     *  Passes through to ApiClient.onReconnect(). Returns an unsubscribe function. */
    onReconnect(callback: () => void): () => void {
        return this.#apiClient.onReconnect(callback)
    }
}
