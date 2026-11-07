import { PerspectiveClient } from "./PerspectiveClient";
import type { EventMap, ScopedEventName } from "../generated/api/Events";
import type { FlowFiredEntry } from "../generated/api/FlowFiredEntry";
import type {
    FlowFireOutcome, FlowMintedReceipt, FlowOutputRef, FlowProposeResult,
    FlowReceiptVerdict, FlowStateEntry, FlowValidOutput,
} from "./FlowInstance";
import { CallOptions } from "../apiClient";
import { Link, LinkExpression, LinkExpressionInput, LinkExpressionMutations, LinkMutations } from "../links/Links";
import { LinkQuery } from "./LinkQuery";
import { PerspectiveHandle, PerspectiveState } from './PerspectiveHandle'
import { Perspective } from "./Perspective";
import { Literal } from "../Literal";
import { ExpressionRendered } from "../expression/Expression";
import { NeighbourhoodProxy } from "../neighbourhood/NeighbourhoodProxy";
import { NeighbourhoodExpression } from "../neighbourhood/Neighbourhood";
import { AIClient } from "../ai/AIClient";

import { getPropertiesMetadata, getRelationsMetadata } from "../model/decorators";
import { AllInstancesResult } from "../model/types";
import { LiveQuery, LiveQueryStatus, Subscribed } from "./LiveQuery";
import type { TranscriptTurn } from "../generated/api";
import type { AgentMessage } from "../generated/api/AgentMessage";
import type { AgentReply } from "../generated/api/AgentReply";
import type { JsonValue } from "../generated/api/serde_json/JsonValue";
import type { RegisterResult } from "../generated/api/RegisterResult";

import { SHACLShape } from "../shacl/SHACLShape";
import { SHACLFlow } from "../shacl/SHACLFlow";
import { Ad4mModel } from "../model/Ad4mModel";
import type { AddAutoProcessorConfig, InterpretationOverlayInfo, RawScope, RunInterpretationObserveOptions } from "./AutoProcessor";

type QueryCallback = (result: AllInstancesResult) => void;

/** A class registered in a perspective, as {@link PerspectiveProxy.shapes} reads it. */
export interface RegisteredShape {
    /** The class name: the local name of its target class. */
    name: string;
    shape: SHACLShape;
    /** SHA-256 (hex) of the shape's canonical JSON. */
    hash: string;
}

/** Extract namespace prefix from a URI (everything up to and including the last / or #) */
function extractNamespaceFromUri(uri: string): string {
    const hashIdx = uri.lastIndexOf('#');
    if (hashIdx >= 0) return uri.substring(0, hashIdx + 1);
    const slashIdx = uri.lastIndexOf('/');
    if (slashIdx >= 0) return uri.substring(0, slashIdx + 1);
    const colonIdx = uri.lastIndexOf(':');
    if (colonIdx >= 0) return uri.substring(0, colonIdx + 1);
    return uri;
}


/** A live SPARQL or model query with its latest result.
 *
 * `subscribe()` opens the query and resolves once the first result is in;
 * `onResult()` callbacks then get every later result. The executor sends
 * only changes; `LiveQuery` applies them, recovers from a missed update and
 * re-opens the query after a reconnect. Call `dispose()` when done.
 *
 * ```typescript
 * const subscription = await perspective.subscribeQuery("SELECT ?s WHERE { ?s ?p ?o }");
 * const remove = subscription.onResult(result => console.log(result));
 * subscription.dispose();
 * ```
 */
export class QuerySubscriptionProxy {
    #live: LiveQuery;
    #callbacks = new Set<QueryCallback>();
    #initialized: Promise<boolean>;
    #initResolve?: (value: boolean) => void;
    #initReject?: (reason?: any) => void;

    /** With `className`, `query` is a model query (JSON) over that class or
     *  union of classes, opened with `modelSubscribe`; results are
     *  `{ instances, totalCount }`. Without it, `query` is SPARQL. */
    constructor(uuid: string, query: string, client: PerspectiveClient, className?: string | string[]) {
        const open = className === undefined
            ? (signal: AbortSignal) => client.subscribeQuery(uuid, query, { signal })
            : (signal: AbortSignal) => client.modelSubscribe(uuid, className, query, { signal });
        this.#live = new LiveQuery(client, uuid, open, result => this.#deliver(result));
        this.#initialized = new Promise<boolean>((resolve, reject) => {
            this.#initResolve = resolve;
            this.#initReject = reject;
        });
        // `subscribe()` throws the same error, so a caller who never reads
        // `initialized` must not get an unhandled rejection from it.
        this.#initialized.catch(() => {});
    }

    /** Open the query; resolves with the first result delivered. */
    async subscribe() {
        try {
            this.#deliver(await this.#live.start());
        } catch (error) {
            this.#initReject?.(error);
            this.#initResolve = this.#initReject = undefined;
            throw error;
        }
    }

    /** The executor's id for this subscription. */
    get id(): string {
        return this.#live.id;
    }

    /** Resolves once the first result has arrived. */
    get initialized(): Promise<boolean> {
        return this.#initialized;
    }

    /** The latest result. */
    get result(): AllInstancesResult {
        return this.#live.result;
    }

    /** Call `callback` with every later result. Returns a remover. */
    onResult(callback: QueryCallback): () => void {
        this.#callbacks.add(callback);
        return () => this.#callbacks.delete(callback);
    }

    /** Whether the query lost its subscription (a reconnect, a failed
     *  resync), could not open it again yet and is retrying: `result` is then
     *  not live. */
    get reopening(): boolean {
        return this.#live.reopening;
    }

    /** Call `listener` on each failed re-open of the subscription and when a
     *  re-open succeeds after one failed (see {@link LiveQueryStatus}).
     *  Returns a remover. */
    onStatus(listener: (status: LiveQueryStatus) => void): () => void {
        return this.#live.onStatus(listener);
    }

    #deliver(result: AllInstancesResult) {
        this.#initResolve?.(true);
        this.#initResolve = this.#initReject = undefined;
        for (const callback of this.#callbacks) {
            try {
                callback(result);
            } catch (e) {
                console.error('Error in query subscription callback:', e);
            }
        }
    }

    /** End the subscription on the executor and drop all callbacks. With
     *  `cancelOpening`, a `subscribe()` still waiting on the executor is
     *  cancelled too, and rejects. */
    dispose(options: { cancelOpening?: boolean } = {}) {
        this.#live.dispose(options);
        this.#callbacks.clear();
    }
}

/** Link status argument. The executor accepts either case and returns links
 *  with the upper-case form (`LinkExpression.status`). */
export type LinkStatus = "shared" | "local" | "SHARED" | "LOCAL"
interface Parameter {
    name: string
    value: string
}

/**
 * PerspectiveProxy provides a high-level interface for working with AD4M Perspectives - agent-centric semantic graphs
 * that store and organize links between expressions.
 * 
 * A Perspective is fundamentally a collection of links (subject-predicate-object triples) that represent an agent's view
 * of their digital world. Through PerspectiveProxy, you can:
 * - Add, remove, and query links
 * - Work with Social DNA (subject classes and flows)
 * - Subscribe to real-time updates
 * - Share perspectives as Neighbourhoods
 * - Execute Prolog queries for complex graph patterns
 * 
 * 
 * @example
 * ```typescript
 * // Create and work with links
 * const perspective = await ad4m.perspective.add("My Space");
 * await perspective.add({
 *   source: "did:key:alice",
 *   predicate: "knows",
 *   target: "did:key:bob"
 * });
 * 
 * // Query links
 * const friends = await perspective.get({
 *   source: "did:key:alice",
 *   predicate: "knows"
 * });
 * 
 * // Use Social DNA
 * await Todo.register(perspective);
 * const todo = await perspective.createSubject("Todo", "expression://123");
 * 
 * // Subscribe to changes
 * perspective.on("link-added", ({ link }) => {
 *   console.log("New link added:", link);
 * });
 * ```
 */

export class PerspectiveProxy {
    /** Unique identifier of this perspective */
    uuid: string;

    /** Human-readable name of this perspective */
    name: string;

    /** If this perspective is shared as a Neighbourhood, this is its URL */
    sharedUrl: string|null;

    /** If this perspective is shared, this contains the Neighbourhood metadata */
    neighbourhood: NeighbourhoodExpression|null;

    /** Current sync state if this perspective is shared */
    state: PerspectiveState|null;

    /** List of owners of this perspective */
    owners?: string[]

    #handle: PerspectiveHandle
    #client: PerspectiveClient

    /** @internal Exposed for ModelQueryBuilder subscription management */
    get client(): PerspectiveClient { return this.#client; }

    /** Releases of the handlers `on()` registered and has not released yet. */
    #releases = new Set<() => void>()
    /** The `interpretationOverlays()` RPC currently in flight, shared by concurrent callers. */
    #overlaysInFlight: Promise<InterpretationOverlayInfo[]> | null = null

    /**
     * Creates a new PerspectiveProxy instance.
     * Note: Don't create this directly, use ad4m.perspective.add() instead.
     */
    constructor(handle: PerspectiveHandle, ad4m: PerspectiveClient) {
        this.#handle = handle
        this.#client = ad4m
        this.uuid = this.#handle.uuid;
        this.name = this.#handle.name;
        this.owners = this.#handle.owners;
        this.sharedUrl = this.#handle.sharedUrl;
        this.neighbourhood = this.#handle.neighbourhood;
        this.state = this.#handle.state;
    }

    /** Update the proxy's internal handle and public fields in-place.
     *  Keeps the same object reference so all holders see the update. */
    updateHandle(handle: PerspectiveHandle): void {
        if (handle.uuid !== this.#handle.uuid) {
            throw new Error(
                `PerspectiveProxy.updateHandle: UUID mismatch — proxy is bound to "${this.#handle.uuid}" but received handle with UUID "${handle.uuid}". ` +
                `This would silently diverge from listeners registered under the original UUID.`
            );
        }
        this.#handle = handle;
        this.uuid = handle.uuid;
        this.name = handle.name;
        this.owners = handle.owners;
        this.sharedUrl = handle.sharedUrl;
        this.neighbourhood = handle.neighbourhood;
        this.state = handle.state;
    }

    /**
     * Executes a set of actions on an expression with optional parameters.
     * Used internally by Social DNA flows and subject class operations.
     * 
     * Actions are specified as an array of commands that modify links in the perspective.
     * Each action is an object with the following format:
     * ```typescript
     * {
     *   action: "addLink" | "removeLink" | "setSingleTarget" | "collectionSetter",
     *   source: string,    // Usually "this" to reference the current expression
     *   predicate: string, // The predicate URI
     *   target: string     // The target value or "value" for parameters
     * }
     * ```
     * 
     * Available commands:
     * - `addLink`: Creates a new link
     * - `removeLink`: Removes an existing link
     * - `setSingleTarget`: Removes all existing links with the same source/predicate and adds a new one
     * - `collectionSetter`: Special command for setting relation properties
     * 
     * When used with parameters, the special value "value" in the target field will be 
     * replaced with the actual parameter value.
     * 
     * @example
     * ```typescript
     * // Add a state link and remove an old one
     * await perspective.executeAction([
     *   {
     *     action: "addLink",
     *     source: "this",
     *     predicate: "todo://state", 
     *     target: "todo://doing"
     *   },
     *   {
     *     action: "removeLink",
     *     source: "this",
     *     predicate: "todo://state",
     *     target: "todo://ready"
     *   }
     * ], "expression://123");
     * 
     * // Set a property using a parameter
     * await perspective.executeAction([
     *   {
     *     action: "setSingleTarget",
     *     source: "this",
     *     predicate: "todo://title",
     *     target: "value"
     *   }
     * ], "expression://123", [
     *   { name: "title", value: "New Title" }
     * ]);
     * ```
     * 
     * @param actions - Array of action objects to execute
     * @param expression - Target expression address (replaces "this" in actions)
     * @param parameters - Optional parameters that replace "value" in actions
     * @param batchId - Optional batch ID to group this operation with others
     */
    async executeAction(actions, expression, parameters: Parameter[], batchId?: string) {
        return this.#client.executeCommands(this.#handle.uuid, JSON.stringify(actions), expression, JSON.stringify(parameters), batchId)
    }

    /**
     * Retrieves links from the perspective that match the given query.
     * 
     * @param query - Query parameters to filter links
     * @returns Array of matching LinkExpressions
     * 
     * @example
     * ```typescript
     * // Get all links where Alice knows someone
     * const links = await perspective.get({
     *   source: "did:key:alice",
     *   predicate: "knows"
     * });
     * 
     * // Get all comments on a post
     * const comments = await perspective.get({
     *   source: "post://123",
     *   predicate: "comment"
     * });
     * ```
     */
    async get(query: LinkQuery, options?: CallOptions): Promise<LinkExpression[]> {
        return await this.#client.queryLinks(this.#handle.uuid, query, options)
    }

    /**
     * Runs generic LLM interpretation over a conversation transcript, turning it
     * into typed instances of this perspective's own SHACL subject classes
     * (steered by each class's `@InterpretationHint`). The target shapes are
     * resolved automatically from the perspective's registered classes, so you
     * pass only the transcript. New instances are written as links into this
     * perspective; the returned placements list each instance's base URI and
     * the links written for it.
     *
     * @param transcript ordered `{ speaker, text }` turns
     * @param basePrefix URI namespace for new instance identities, e.g. `soa://ext/`
     * @param classes local names of the subject classes to extract into; omit for all
     * @param options scopes, progress reporting, and `signal` / `timeoutMs`
     *   (default {@link LONG_TIMEOUT_MS})
     */
    async runInterpretation(
        transcript: TranscriptTurn[],
        basePrefix: string,
        classes?: string[],
        options?: CallOptions & {
            existingScope?: RawScope,
            mintScope?: RawScope,
            /** Report progress while the pass runs — see {@link RunInterpretationObserveOptions}. */
            observe?: RunInterpretationObserveOptions,
        },
    ): Promise<string[]> {
        // Grouped into an options bag rather than continuing the client's positional list.
        // The scopes were already unreachable from here for that reason, and a fourth, fifth and
        // sixth positional parameter would have made `undefined, undefined, { … }` the normal way
        // to ask for the only one of them most callers want.
        const { existingScope, mintScope, observe, ...callOptions } = options ?? {}
        return await this.#client.runInterpretation(
            this.#handle.uuid,
            transcript,
            basePrefix,
            classes,
            existingScope,
            mintScope,
            observe,
            callOptions,
        )
    }

    /**
     * Harness-dispatched interpretation pass over this perspective (design v3
     * §6 — the tool-calling counterpart to {@link runInterpretation}). The LLM
     * sees a live per-class tool surface (`{Class}_query`, `{Class}_get`,
     * `{Class}_propose_create`, `{Class}_propose_link_child`, …) and drives
     * the extraction via tool calls; buffered proposals drain through the
     * same overlay gate the single-shot path uses.
     *
     * @param transcript ordered `{ speaker, text }` turns
     * @param basePrefix URI namespace new instance identities are minted under
     * @param maxToolCalls upper bound on tool calls the harness will make in
     *   one pass (must be > 0 — use {@link runInterpretation} for the
     *   classic single-shot path)
     * @param classes local names of the subject classes to extract into; omit for all
     * @param modelOverride optional model override; omit for the default LLM
     * @param options `signal` / `timeoutMs` (default {@link LONG_TIMEOUT_MS})
     */
    async runInterpretationWithHarness(
        transcript: TranscriptTurn[],
        basePrefix: string,
        maxToolCalls: number,
        classes?: string[],
        modelOverride?: string,
        // Optional live-debug observability. When both `observationId` and
        // `emitDebugEvents` are supplied, every dispatched tool call fires
        // `ToolCall` + `ToolResult` events on the `auto-processor-event`
        // topic keyed by `observationId`. Subscribe with
        // `on('auto-processor-event')` to render the harness loop
        // live in a UI. Absent = fast headless path (no telemetry cost).
        observationId?: string,
        emitDebugEvents?: boolean,
        options?: CallOptions,
    ): Promise<string[]> {
        return await this.#client.runInterpretationWithHarness(
            this.#handle.uuid,
            transcript,
            basePrefix,
            maxToolCalls,
            classes,
            modelOverride,
            undefined,
            observationId,
            emitDebugEvents,
            options,
        )
    }

    /**
     * Ask a model something about this perspective, or ask it to change something, with the
     * executor's own tools — including the tools apps install for their records. It reads freely;
     * it writes only with the tools named in `settings.writeTools`, as you. Answers with the
     * model's final reply.
     *
     * @param messages the conversation so far, the person's request last
     * @param settings `context` (what the person is looking at), `writeTools`, `model`, `maxToolCalls`
     * @param options `signal` / `timeoutMs` (default {@link LONG_TIMEOUT_MS})
     */
    async runAgent(
        messages: AgentMessage[],
        settings?: Parameters<PerspectiveClient['runAgent']>[2],
        options?: CallOptions,
    ): Promise<AgentReply> {
        return await this.#client.runAgent(this.#handle.uuid, messages, settings, options)
    }

    /**
     * Register a neighbourhood auto-processor on this perspective. The executor
     * then runs interpretation automatically over new source items (like Flux
     * per channel), coordinating which peer processes each batch. Returns the
     * processor id. Subscribe to progress via `on('auto-processor-event')`.
     */
    async addAutoProcessor(config: AddAutoProcessorConfig): Promise<string> {
        return await this.#client.addAutoProcessor(this.#handle.uuid, config)
    }

    /**
     * Stop an auto-processor by deleting its config.
     *
     * The registration is data — the watch loop reads the processor set back out of the
     * perspective's graph on every tick — so deleting the config is what stops it, and there is
     * nothing else to unregister. The config is `Shared`, so this stops the processor for the
     * neighbourhood rather than only for this peer.
     *
     * Resolves `true` when there was a processor to remove and `false` when there was not; a
     * processor another peer has already removed is not an error.
     *
     * The processor's `InterpretationRun` nodes stay: they are the record of what it did and the
     * processed-turn cursor, so a processor later registered under the same id resumes where this
     * one left off rather than re-reading every turn.
     */
    async removeAutoProcessor(processorId: string): Promise<boolean> {
        return await this.#client.removeAutoProcessor(this.#handle.uuid, processorId)
    }

    /**
     * Pending interpretation overlays on this perspective — LLM suggestions the
     * §4 divergence gate staged rather than applied, awaiting human accept/reject.
     *
     * Concurrent callers share one in-flight RPC. Nothing is kept after it
     * settles, so every call made after that fetches from the executor again.
     * {@link acceptInterpretation} and {@link rejectInterpretation} detach the
     * in-flight RPC, so a read issued after they resolve never joins a read
     * that started before the write.
     * Each caller gets its own copy of the array.
     */
    async interpretationOverlays(): Promise<InterpretationOverlayInfo[]> {
        if (this.#overlaysInFlight) {
            return [...(await this.#overlaysInFlight)]
        }
        const pending = this.#client.interpretationOverlays(this.#handle.uuid)
        this.#overlaysInFlight = pending
        try {
            return [...(await pending)]
        } finally {
            if (this.#overlaysInFlight === pending) {
                this.#overlaysInFlight = null
            }
        }
    }

    /**
     * Accept an interpretation overlay's suggestion(s): the LLM's staged value
     * becomes the real, human-owned value and the overlay is deleted. Pass
     * `property` to accept a single predicate; omit it for the whole base.
     */
    async acceptInterpretation(base: string, property?: string): Promise<boolean> {
        try {
            return await this.#client.acceptInterpretation(this.#handle.uuid, base, property)
        } finally {
            // A read already in flight may predate this write; later callers must not join it.
            this.#overlaysInFlight = null
        }
    }

    /**
     * Reject an interpretation overlay's suggestion(s). Omit `property` to reject
     * the whole base — a rejected `create` deletes the suggested instance, a
     * rejected `update` drops the overlay and keeps the real value.
     */
    async rejectInterpretation(base: string, property?: string): Promise<boolean> {
        try {
            return await this.#client.rejectInterpretation(this.#handle.uuid, base, property)
        } finally {
            // A read already in flight may predate this write; later callers must not join it.
            this.#overlaysInFlight = null
        }
    }

    /**
     * `outputs` names the instances a run produces, as `{ className, id }`
     * pairs, for a transition into a terminal state. The proposal signs a
     * hash over their content, and a receipt for the run can only speak for
     * exactly these instances, as they stood at completion. Naming outputs
     * for a non-terminal state is refused.
     */
    async proposeFlowTransition(instanceUri: string, toState: string, rationale?: string, outputs?: FlowOutputRef[]): Promise<FlowProposeResult> {
        return await this.#client.proposeFlowTransition(this.#handle.uuid, instanceUri, toState, rationale, outputs)
    }

    async acceptFlowProposal(proposalUri: string): Promise<FlowFireOutcome[]> {
        return await this.#client.acceptFlowProposal(this.#handle.uuid, proposalUri)
    }

    /** Withdraw our own links from a proposal; resolves to how many went. */
    async rejectFlowProposal(proposalUri: string): Promise<number> {
        return await this.#client.rejectFlowProposal(this.#handle.uuid, proposalUri)
    }

    /**
     * Re-decide a flow receipt under this perspective's own flow catalogue.
     * The verdict is three-way — see {@link FlowReceiptVerdict}: branch on
     * `outcome`, never on a boolean you derive from it.
     */
    async verifyFlowReceipt(receipt: JsonValue): Promise<FlowReceiptVerdict> {
        return await this.#client.verifyFlowReceipt(this.#handle.uuid, receipt)
    }

    /**
     * Which instances are, as they stand, valid outputs of `flow`?
     *
     * Backed by receipt verification executor-side (see
     * {@link FlowValidOutput}); an instance without a verifying receipt, or
     * edited since its run completed, is not listed. The same predicate is
     * available as a model-query filter:
     * `where: { producedByFlow: { flow, state? } }`.
     *
     * Rejects — never resolves to `[]` — when the flow is not on this
     * perspective, or when it carries more receipt candidates than the
     * executor's per-flow budget (256): "could not read every receipt" is not
     * "no valid outputs". The filter rejects the same way.
     */
    async flowValidOutputs(flow: string, state?: string): Promise<FlowValidOutput[]> {
        return await this.#client.flowValidOutputs(this.#handle.uuid, flow, state)
    }

    /**
     * The state each flow instance is in, as **this agent's** executor last
     * derived it — one entry per instance it has derived, optionally narrowed
     * to `instances`. An instance missing from the answer has not been
     * derived here yet; it stands at its flow's first state.
     *
     * The state is not stored on the graph: the engine derives it from the
     * signed proposals and keeps what it derived per agent, outside the
     * perspective. So no other agent — a co-owner on the same executor included —
     * can read or write your entries, and a link query never sees them.
     * `FlowInstance.findAll` / `start` call this for you.
     */
    async flowStates(instances?: string[]): Promise<FlowStateEntry[]> {
        return await this.#client.flowStates(this.#handle.uuid, instances)
    }

    /**
     * For each of `instances`, in order, the proposals whose votes settled an
     * edge, as this agent's executor recorded them — the flow's history,
     * sorted by URI (`fired: []` when none). One call covers up to 200
     * instances (the executor refuses more with a 400); split a longer list
     * into batches of 200.
     * Replaces the `resolvedAs` property proposals used to carry.
     */
    async flowFired(instances: string[]): Promise<FlowFiredEntry[]> {
        return await this.#client.flowFired(this.#handle.uuid, instances)
    }

    /**
     * Called whenever a flow instance on this perspective changes state on
     * this agent's executor, or an edge settles: `{ instance, from, to,
     * firedProposals, caughtUp }`. Only this agent's sockets receive it.
     *
     * When the executor first derives an instance that already has history
     * (it synced in, or the executor was upgraded) it is called once with
     * `caughtUp: true`, `from: null` and no `firedProposals`: the settles
     * before that are history, but `to` is the state to show for a run that
     * read as not derived (`FlowInstance.currentStateName === undefined`).
     *
     * Returns a function that removes the handler; {@link dispose} removes
     * it too.
     */
    onFlowStateChanged(handler: (event: EventMap["flow-state-changed"]) => void): () => void {
        return this.on("flow-state-changed", handler)
    }

    /** Mint and store the receipt for a completed flow run. */
    async mintFlowReceipt(instanceUri: string): Promise<FlowMintedReceipt> {
        return await this.#client.mintFlowReceipt(this.#handle.uuid, instanceUri)
    }

    /**
     * Executes a Prolog query against the perspective's knowledge base.
     * This is a powerful way to find complex patterns in the graph.
     * 
     * @param query - Prolog query string
     * @returns Query results or false if no results
     * 
     * @example
     * ```typescript
     * // Find friends of friends
     * const results = await perspective.infer(`
     *   triple(A, "knows", B),
     *   triple(B, "knows", C),
     *   A \= C
     * `);
     * 
     * // Find all active todos
     * const todos = await perspective.infer(`
     *   instance(Todo, "Todo"),
     *   property_getter("Todo", Todo, "state", "active")
     * `);
     * ```
     */
    async infer(query: string, options?: CallOptions): Promise<any> {
        return await this.#client.queryProlog(this.#handle.uuid, query, options)
    }

    /**
     * Executes a SPARQL query against the perspective's link cache.
     * This allows powerful SQL-like queries on the link data stored in SPARQL.
     *
     * **Security Note:** Only read-only queries (SELECT, RETURN, etc.) are permitted.
     * Mutating operations (DELETE, UPDATE, INSERT, CREATE, DROP, DEFINE, etc.) are
     * blocked for security reasons. Use the perspective's add/remove methods to modify links.
     *
     * **Cancellation:** Pass `{ signal }` from an `AbortController` to abort
     * an in-flight query.  The executor receives `request.cancel` over the
     * WebSocket and short-circuits the JSON reply.  Caveat: Oxigraph itself
     * cannot be interrupted mid-evaluation, so an already-running scan will
     * keep its blocking thread busy until it completes; what's saved is the
     * serialise + network + client-deserialise tax on the result.  Aborted
     * calls reject with `DOMException('Aborted', 'AbortError')` — match
     * `fetch()` for handling. A call with `options` makes its own RPC.
     *
     * **Sharing:** concurrent identical calls without `options` share one RPC;
     * each caller gets its own result object. Nothing is cached once the
     * call settles: use a live query for repeated reads of the same data.
     *
     * @param query - SPARQL query string (read-only operations only)
     * @param options - Optional call options (e.g. AbortSignal for cancellation)
     * @returns Query results as parsed JSON
     */
    async querySparql<T = any>(query: string, options?: CallOptions): Promise<T> {
        return this.#client.querySparql<T>(this.#handle.uuid, query, options);
    }

    /**
     * Execute a model query — the executor-side replacement for SPARQL query building + JS hydration.
     *
     * The executor resolves the model's shape from the perspective's SHACL
     * triples (written by `registerShapes`).  Callers no longer ship shape
     * metadata with the query; the perspective is the source of truth.
     *
     * @param className - The model class name (e.g. "Recipe"), or several: one
     *   query over their union, whose rows carry `__subjectClass` and
     *   `__subjectClasses` (#1238)
     * @param queryJson - Structured query as JSON string
     * @returns Object with `instances` array, and `totalCount` when the query sets `count: true`
     */
    async modelQuery(className: string | string[], queryJson: string, options?: CallOptions): Promise<{ instances: any[], totalCount?: number }> {
        return await this.#client.modelQuery(this.#handle.uuid, className, queryJson, options);
    }

    /** Resolve each URI to the names of every subject class it is an instance of.
     *
     * The counterpart of {@link isSubjectInstance}, which asks the same question
     * one class at a time. Without this, finding the class of an arbitrary URI
     * meant looping over every registered class — a round trip each — and doing
     * it again for every URI.
     *
     * Class membership in AD4M is structural — a URI belongs to a class when it
     * carries that class's flags and required properties — so membership is **not
     * exclusive**: an instance conforms to its parent classes, and to any
     * unrelated class whose required set happens to be a subset of what it
     * carries. Every match is returned.
     *
     * The list is ordered **most specific first**, meaning by the number of
     * triples the class requires — a subclass requires everything its parent does
     * and more — with ties broken alphabetically so that every peer answers
     * identically. A caller that can only act on one class should take
     * `classes[0]`, but that head is only a heuristic: it is arbitrary between two
     * unrelated classes requiring the same number of triples, which is exactly
     * the case the full list exists to expose.
     *
     * A URI is **absent from the result** when no *registered* class matched it.
     * That covers two situations, and nothing here separates them: the URI may
     * not be a subject instance at all, or it may be an instance of a class this
     * perspective has not registered. Absence is used rather than an empty list
     * because an empty list would claim the stronger thing — that the URI belongs
     * to no class — which this cannot know.
     *
     * @param uris The expression URIs to classify.
     */
    async subjectClassesOf(uris: string[]): Promise<Record<string, string[]>> {
        if (uris.length === 0) return {};
        return await this.#client.subjectClassesOf(this.#handle.uuid, uris);
    }

    /**
     * Evaluate property getters for a batch of instances in a single RPC call.
     * Returns a map of `{ instanceId: { prop: value, ... } }`.
     *
     * Use this instead of `Ad4mModel.evaluateGetters()` for lazy-loading
     * getter-backed properties on visible items (e.g. `replyingTo` on messages).
     *
     * @param className - The model class name (e.g. "Message")
     * @param instanceIds - Array of instance base expression URIs
     * @param propertyNames - Optional subset of property names to evaluate
     * @returns Map of instance ID → evaluated property values
     */
    async evaluateGetters(
        className: string,
        instanceIds: string[],
        propertyNames?: string[],
    ): Promise<Record<string, Record<string, any>>> {
        return await this.#client.evaluateGetters(
            this.#handle.uuid, className, instanceIds, propertyNames,
        );
    }

    /**
     * Open a live model query on the executor. Later changes arrive as
     * updates; `LiveQuery` (or `ModelQueryBuilder.subscribe()`) applies them.
     * The subscription ends with `disposeQuerySubscription()` or when the
     * socket closes.
     *
     * @param className - The model class name, or several (same as modelQuery)
     * @param queryJson - JSON-serialized query parameters (same as modelQuery)
     * @returns `subscriptionId`, initial `result` and `revision` (0)
     */
    async modelSubscribe(className: string | string[], queryJson: string): Promise<Subscribed> {
        return await this.#client.modelSubscribe(this.#handle.uuid, className, queryJson);
    }

    /**
     * Adds a new link to the perspective.
     * 
     * @param link - The link to add
     * @param status - Whether the link should be shared in a Neighbourhood
     * @param batchId - Optional batch ID to group this operation with others
     * @returns The created LinkExpression
     * 
     * @example
     * ```typescript
     * // Add a simple relationship
     * await perspective.add({
     *   source: "did:key:alice",
     *   predicate: "follows",
     *   target: "did:key:bob"
     * });
     * 
     * // Add a local-only link
     * await perspective.add({
     *   source: "note://123",
     *   predicate: "tag",
     *   target: "private"
     * }, "local");
     * ```
     */
    async add(link: Link, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return this.#client.addLink(this.#handle.uuid, link, status, batchId)
    }

    /**
     * Adds multiple links to the perspective in a single operation.
     * More efficient than adding links one by one.
     * 
     * @param links - Array of links to add
     * @param status - Whether the links should be shared
     * @param batchId - Optional batch ID to group this operation with others
     * @returns Array of created LinkExpressions
     */
    async addLinks(links: Link[], status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression[]> {
        return this.#client.addLinks(this.#handle.uuid, links, status, batchId)
    }

    /**
     * Removes multiple links from the perspective.
     * 
     * @param links - Array of links to remove
     * @param batchId - Optional batch ID to group this operation with others
     * @returns Array of removed LinkExpressions
     */
    async removeLinks(links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        return this.#client.removeLinks(this.#handle.uuid, links, batchId)
    }

    /**
     * Applies a set of link mutations (adds and removes) in a single operation.
     * Useful for atomic updates to the perspective.
     * 
     * @param mutations - Object containing links to add and remove
     * @param status - Whether new links should be shared
     * @returns Object containing results of the mutations
     */
    async linkMutations(mutations: LinkMutations, status: LinkStatus = 'shared'): Promise<LinkExpressionMutations> {
        return this.#client.linkMutations(this.#handle.uuid, mutations, status)
    }

        /**
     * Adds a pre-signed LinkExpression to the perspective.
     *
     * The executor checks the signature against `link.author` and stores the
     * verdict as `proof.valid`. If the same link (author, source, predicate,
     * target and timestamp) is already stored with a signature that verifies,
     * its stored proof is kept. The expression passed here is still returned
     * and, when Shared, committed to the neighbourhood as given.
     * 
     * @param link - The signed LinkExpression to add
     * @param status - Whether the link should be shared
     * @param batchId - Optional batch ID to group this operation with others
     * @returns The added LinkExpression
     */
    async addLinkExpression(link: LinkExpression, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return await this.#client.addLinkExpression(this.#handle.uuid, link, status, batchId)
    }

        /**
     * Updates an existing link with new data.
     * 
     * @param oldLink - The existing link to update
     * @param newLink - The new link data
     * @param batchId - Optional batch ID to group this operation with others
     */
    async update(oldLink: LinkExpressionInput, newLink: Link, batchId?: string): Promise<LinkExpression> {
        return this.#client.updateLink(this.#handle.uuid, oldLink, newLink, batchId)
    }

    /**
     * Removes a link from the perspective.
     *
     * Accepts either a full `LinkExpressionInput` (as returned by `add`) or a bare
     * `Link` (source/predicate/target only).  When a bare Link is passed the method
     * resolves it to the first stored `LinkExpression` whose source, predicate, and
     * target match, then removes that expression.  A bare Link without a predicate
     * (or with the constructor's `""` stand-in) matches only stored links that
     * themselves have no predicate.  If no match is found an error is thrown
     * naming the link so the caller knows what was expected.
     *
     * @param link - The link to remove (LinkExpressionInput or bare Link)
     * @param batchId - Optional batch ID to group this operation with others
     */
    async remove(link: LinkExpressionInput | Link, batchId?: string): Promise<boolean> {
        let resolvedLink: LinkExpressionInput;
        if (!('data' in link) || (link as any).data === undefined) {
            // bare Link — resolve to stored expression
            const bare = link as Link;
            // The Link constructor coerces an absent predicate to "" and the
            // store reports predicate-less links as predicate null — so ""
            // and undefined both mean "no predicate" here, and neither may
            // widen the query: dropping the filter would resolve (and
            // remove!) an arbitrary source→target link under a *different*
            // predicate. LinkQuery cannot express "predicate is absent", so
            // in that case we query on source/target only and require the
            // absence ourselves.
            const predicateGiven = bare.predicate !== undefined && bare.predicate !== '';
            const candidates = await this.get(new LinkQuery({
                source: bare.source || undefined,
                predicate: predicateGiven ? bare.predicate : undefined,
                target: bare.target || undefined,
            }));
            const matches = predicateGiven
                ? candidates
                : candidates.filter(m => !m.data.predicate);
            if (matches.length === 0) {
                throw new Error(
                    `PerspectiveProxy.remove: no stored LinkExpression matches Link { source: "${bare.source}", predicate: "${bare.predicate}", target: "${bare.target}" }`
                );
            }
            resolvedLink = matches[0] as unknown as LinkExpressionInput;
        } else {
            resolvedLink = link as LinkExpressionInput;
        }
        return this.#client.removeLink(this.#handle.uuid, resolvedLink, batchId)
    }

    /** Creates a new batch for grouping operations */
    async createBatch(): Promise<string> {
        return await this.#client.createBatch(this.#handle.uuid)
    }

    /** Commits a batch of operations */
    async commitBatch(batchId: string): Promise<LinkExpressionMutations> {
        return this.#client.commitBatch(this.#handle.uuid, batchId)
    }
    /**
     * Retrieves and renders an Expression referenced in this perspective.
     * 
     * @param expressionURI - URI of the Expression to retrieve
     * @returns The rendered Expression
     */
    async getExpression(expressionURI: string): Promise<ExpressionRendered> {
        return await this.#client.getExpression(expressionURI)
    }

    /**
     * Creates a new Expression in the specified Language.
     * 
     * @param content - Content for the new Expression
     * @param languageAddress - Address of the Language to use
     * @returns URI of the created Expression
     */
    async createExpression(content: any, languageAddress: string): Promise<string> {
        return await this.#client.createExpression(content, languageAddress)
    }

    /**
     * Call `handler` with every `type` event about this perspective, until the
     * returned function or {@link dispose} releases it.
     *
     * @example
     * ```typescript
     * perspective.on("link-added", ({ link }) => console.log("New link:", link));
     * perspective.on("link-updated", ({ oldLink, newLink }) => console.log(oldLink, "->", newLink));
     * perspective.on("sync-state-change", ({ state }) => console.log("Sync state:", state));
     * ```
     */
    on<K extends ScopedEventName>(type: K, handler: (event: EventMap[K]) => void): () => void {
        // A registration of this proxy's own, so dispose() cannot release another proxy's
        // registration of the same function (on() treats equal handlers as one).
        const release = this.#client.on(type, (event: EventMap[K]) => handler(event), { perspective: this.#handle.uuid })
        const releaseOnce = () => {
            if (this.#releases.delete(releaseOnce)) release()
        }
        this.#releases.add(releaseOnce)
        return releaseOnce
    }

    /** Removes every handler this proxy registered. Other proxies for the same
     *  perspective keep theirs. */
    dispose(): void {
        for (const release of [...this.#releases]) release()
    }

    /**
     * Creates a snapshot of the current perspective state.
     * Useful for backup or sharing.
     *
     * @returns Perspective object containing all links
     */
    async snapshot(): Promise<Perspective> {
        return this.#client.snapshotByUUID(this.#handle.uuid)
    }

    /**
     * Loads a perspective snapshot, replacing current content.
     * 
     * @param snapshot - Perspective snapshot to load
     */
    async loadSnapshot(snapshot: Perspective) {
        //Clean the input data from __typename
        const cleanedSnapshot = JSON.parse(JSON.stringify(snapshot));
        delete cleanedSnapshot.__typename;
        cleanedSnapshot.links.forEach(link => {
           delete link.data.__typename;
        });

        for(const link of cleanedSnapshot.links) {
            await this.addLinkExpression(link)
        }
    }

    /**
     * Gets a single target value matching a query.
     * Useful when you expect only one result.
     * 
     * @param query - Query to find the target
     * @returns Target value or void if not found
     * 
     * @example
     * ```typescript
     * // Get a user's name
     * const name = await perspective.getSingleTarget({
     *   source: "did:key:alice",
     *   predicate: "name"
     * });
     * ```
     */
    async getSingleTarget(query: LinkQuery): Promise<string|void> {
        delete query.target
        const foundLinks = await this.get(query)
        if(foundLinks.length)
            return foundLinks[0].data.target
        else
            return null
    }

    /**
     * Sets a single target value, removing any existing targets.
     * 
     * @param link - Link defining the new target
     * @param status - Whether the link should be shared
     * 
     * @example
     * ```typescript
     * // Set a user's status
     * await perspective.setSingleTarget({
     *   source: "did:key:alice",
     *   predicate: "status",
     *   target: "online"
     * });
     * ```
     */
    async setSingleTarget(link: Link, status: LinkStatus = 'shared') {
        const query = new LinkQuery({source: link.source, predicate: link.predicate})
        const removals = await this.get(query)
        await this.linkMutations({additions: [link], removals}, status)
    }

    /** Returns all the Social DNA flows defined in this perspective */
    async sdnaFlows(): Promise<string[]> {
        // Query for all flow registration links
        const flowLinks = await this.get(new LinkQuery({
            source: "ad4m://self",
            predicate: "ad4m://has_flow"
        }));
        return flowLinks.map(l => {
            try {
                return Literal.fromUrl(l.data.target).get() as string;
            } catch {
                return l.data.target;
            }
        });
    }

    /**
     * Returns all Social DNA flows that can be started from the given expression.
     *
     * Post-`flowable` retirement: a flow is a candidate iff its `inputTypes`
     * declaration is compatible with the expression. Matching rules:
     * - Empty `inputTypes` or one containing the `"any"` wildcard → always
     *   matches (same behaviour as the legacy `flowable === "any"` case).
     * - Otherwise, at least one of the expression's registered subject-class
     *   URIs (resolved via {@link subjectClassesOf}) must appear in
     *   `inputTypes`. This is the concrete-type match the design doc's §5
     *   spawn engine calls for; without it, a flow declaring
     *   `inputTypes: ["Task"]` was previously *never* returned by
     *   `availableFlows("some-task-uri")` — the entire point of typed
     *   flows was silently unreachable (James PR #929 J#3).
     *
     * The expression is classified against the perspective's registered
     * classes only. An expression whose class is not registered on this
     * perspective matches no typed flow (absence, not falsehood — same
     * discipline as {@link subjectClassesOf}); typed flows requiring
     * unknown classes are still returned to their untyped-caller peers.
     */
    async availableFlows(exprAddr: string): Promise<string[]> {
        const allFlowNames = await this.sdnaFlows();
        if (allFlowNames.length === 0) return [];
        const classesOf = await this.subjectClassesOf([exprAddr]);
        const exprClasses = classesOf[exprAddr] ?? [];
        const available: string[] = [];
        for (const name of allFlowNames) {
            const flow = await this.getFlow(name);
            if (!flow) continue;
            if (flow.inputTypes.length === 0 || flow.inputTypes.includes("any")) {
                available.push(name);
                continue;
            }
            if (exprClasses.some(cls => flow.inputTypes.includes(cls))) {
                available.push(name);
            }
        }
        return available;
    }

    /**
     * Register model classes in this perspective, each as the class of its
     * target class IRI, in one call:
     *
     * ```typescript
     * await perspective.registerShapes([Recipe.generateSHACL().shape]);
     * ```
     *
     * A class whose stored shape already is the given one is left alone
     * (`unchanged`): registering on every start writes nothing. The others
     * are rewritten in one diff (`written`), which keeps links other apps
     * put on the class. Answers once the executor reads the new shapes.
     *
     * Rejects, writing nothing, when two shapes name one class or share a
     * shape URI, or when another class holds a shape URI given here.
     */
    async registerShapes(shapes: SHACLShape[]): Promise<RegisterResult> {
        return this.#client.registerShapes(this.#handle.uuid, shapes.map(shape => shape.toJSON()));
    }

    /**
     * The classes registered in this perspective, sorted by name (only those
     * named in `names` when given): each with its shape and the SHA-256 of
     * the shape's canonical JSON, which changes exactly when the shape does.
     */
    async shapes(names?: string[]): Promise<RegisteredShape[]> {
        const summaries = await this.#client.shapes(this.#handle.uuid, names);
        return summaries.map(({ name, shape, hash }) => ({ name, shape: SHACLShape.fromJSON(shape), hash }));
    }

    /**
     * **Recommended way to add Flow definitions.**
     * 
     * Store a SHACL Flow (state machine) in this Perspective using the type-safe `SHACLFlow` class.
     * The flow is serialized as RDF triples (links) for native AD4M storage and querying.
     * 
     * @param name - Flow name (e.g., 'TODO', 'Approval')
     * @param flow - SHACLFlow instance defining the state machine
     * 
     * @example
     * ```typescript
     * import { SHACLFlow } from '@coasys/ad4m';
     *
     * const deliveryFlow = new SHACLFlow('Delivery', 'delivery://');
     * deliveryFlow.inputTypes = ['Task'];
     * deliveryFlow.interpretationHint =
     *   'Advance a Task through delivery: Identified → Scoped → InProgress → Review → Done.';
     *
     * deliveryFlow.addState({ name: 'Identified', value: 0 });
     * deliveryFlow.addState({ name: 'Scoped',     value: 1 });
     * deliveryFlow.addState({ name: 'InProgress', value: 2 });
     * deliveryFlow.addState({ name: 'Review',     value: 3 });
     * deliveryFlow.addState({ name: 'Done',       value: 4 });
     *
     * deliveryFlow.addTransition({ actionName: 'Scope',  fromState: 'Identified', toState: 'Scoped',     actions: [] });
     * deliveryFlow.addTransition({ actionName: 'Start',  fromState: 'Scoped',     toState: 'InProgress', actions: [] });
     * deliveryFlow.addTransition({ actionName: 'Submit', fromState: 'InProgress', toState: 'Review',     actions: [] });
     * deliveryFlow.addTransition({ actionName: 'Accept', fromState: 'Review',     toState: 'Done',       actions: [] });
     *
     * // Optional: n distinct DIDs must co-sign a proposal before it fires.
     * deliveryFlow.consensusRule = { n: 2 };
     *
     * await perspective.addFlow('Delivery', deliveryFlow);
     * ```
     *
     * Rejects, storing nothing, when a `consensusRule` carries a key the
     * executor does not know or a key written twice (#1144): the error names
     * the state and the key.
     */
    async addFlow(name: string, flow: SHACLFlow): Promise<void> {
        const flowNameLiteral = Literal.from(name).toUrl();
        const wanted = [
            ...flow.toLinks(),
            { source: "ad4m://self", predicate: "ad4m://has_flow", target: flowNameLiteral },
            { source: flowNameLiteral, predicate: "ad4m://flow_uri", target: flow.flowUri },
        ];

        // Re-adding a flow replaces its definition. Adding alone would leave
        // a changed consensus rule beside the old one, and a state holding two
        // rules refuses every move into it. What is stored is found through
        // the flow's own links, since state and transition URIs may use an
        // older scheme. Only definition predicates are taken, from the flow's
        // URI and from each state and transition URI: all of them can carry
        // links the definition does not own.
        const flowLevel = new Set(SHACLFlow.FLOW_LEVEL_PREDICATES);
        const childLevel = new Set(SHACLFlow.STATE_AND_TRANSITION_PREDICATES);
        const ownLinks = (await this.get(new LinkQuery({ source: flow.flowUri })))
            .filter(l => flowLevel.has(l.data.predicate));
        const children = ownLinks
            .filter(l => l.data.predicate === "ad4m://hasState" || l.data.predicate === "ad4m://hasTransition")
            .map(l => l.data.target);
        const childLinks = (await Promise.all(children.map(uri => this.get(new LinkQuery({ source: uri })))))
            .flat()
            .filter(l => childLevel.has(l.data.predicate));
        // A name points at one flow: getFlow reads the first flow_uri link,
        // and FlowInstance.findAll drops records of any other flow URI. So
        // every flow_uri link of this name is taken, and one left from an
        // earlier namespace is removed rather than kept beside the new one.
        const registration = [
            ...await this.get(new LinkQuery({ source: "ad4m://self", predicate: "ad4m://has_flow", target: flowNameLiteral })),
            ...await this.get(new LinkQuery({ source: flowNameLiteral, predicate: "ad4m://flow_uri" })),
        ];
        const stored = [...ownLinks, ...childLinks, ...registration];

        // Only the difference is written, so an unchanged definition writes
        // nothing and a changed one touches only what changed.
        const key = (l: { source: string; predicate?: string; target: string }) =>
            JSON.stringify([l.source, l.predicate ?? "", l.target]);
        const wantedKeys = new Set(wanted.map(key));
        const storedKeys = new Set(stored.map(l => key(l.data)));
        const additions = wanted
            .filter((l, i, all) => !storedKeys.has(key(l)) && all.findIndex(o => key(o) === key(l)) === i)
            .map(l => new Link(l));
        const removals = stored.filter(l => !wantedKeys.has(key(l.data)));

        if (additions.length === 0 && removals.length === 0) return;
        await this.linkMutations({ additions, removals });
    }

    /**
     * Retrieve a Flow definition by name from this Perspective
     * 
     * @param name - Flow name to retrieve
     * @returns The SHACLFlow or null if not found
     */
    async getFlow(name: string): Promise<SHACLFlow | null> {
        const flowNameLiteral = Literal.from(name).toUrl();

        // Find flow URI from name mapping
        const flowUriLinks = await this.get(new LinkQuery({
            source: flowNameLiteral,
            predicate: "ad4m://flow_uri"
        }));

        if (flowUriLinks.length === 0) {
            return null;
        }

        const flowUri = flowUriLinks[0].data.target;

        // Fetch flow-level links (hasState, hasTransition, startAction, inputTypes/outputTypes/…)
        const flowLevelLinks = await this.get(new LinkQuery({ source: flowUri }));

        // Collect state and transition URIs, then fetch their child links
        const childUris = flowLevelLinks
            .filter(l => l.data.predicate === "ad4m://hasState"
                      || l.data.predicate === "ad4m://hasTransition")
            .map(l => l.data.target);
        const childResults = await Promise.all(
            childUris.map(uri => this.get(new LinkQuery({ source: uri })))
        );

        // Flatten into the {source, predicate, target} shape fromLinks expects
        const allExprs = [...flowLevelLinks, ...childResults.flat()];
        const flowLinks = allExprs.map(l => ({
            source: l.data.source,
            predicate: l.data.predicate,
            target: l.data.target
        }));

        return SHACLFlow.fromLinks(flowLinks, flowUri);
    }

    /** The class name of `subjectClass`: the string itself, the `@Model`
     *  name of a decorated class or instance, else the first registered class
     *  that has the template object's properties; `null` when none does. */
    async #classNameOf(subjectClass: unknown): Promise<string | null> {
        if (typeof subjectClass === "string") return subjectClass;
        const obj = subjectClass as any;
        return obj.className || obj.constructor?.className || obj.constructor?.prototype?.className
            || await this.findClassByProperties(obj);
    }

    async #className(subjectClass: unknown): Promise<string> {
        const className = await this.#classNameOf(subjectClass);
        if (!className) {
            throw new Error("Could not resolve subject class name from object. Use a decorated class or pass a className string.");
        }
        return className;
    }

    /**
     * Creates a new subject instance of the given subject class
     * 
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class.
     * @param exprAddr The address of the expression to be turned into a subject instance
     * @param initialValues Optional initial values for properties. If provided, these will be
     * merged with constructor actions for better performance. A collection property accepts
     * an array value and stores one entry per element; scalar properties store arrays/objects
     * as a single JSON value.
     * @param batchId Optional batch ID for grouping operations. If provided, returns the expression address
     * instead of the subject proxy since the subject won't exist until the batch is committed.
     * @returns A proxy object for the created subject, or just the expression address if in batch mode
     */
    async createSubject<T, B extends string | undefined = undefined>(
        subjectClass: T, 
        exprAddr: string,
        initialValues?: Record<string, any>,
        batchId?: B
    ): Promise<B extends undefined ? T : string> {
        await this.#client.createSubject(
            this.#handle.uuid,
            JSON.stringify({
                className: await this.#className(subjectClass),
                initialValues
            }),
            exprAddr,
            initialValues ? JSON.stringify(initialValues) : undefined,
            batchId
        );

        // Skip subject proxy creation when in batch mode since the subject won't exist until batch is committed
        if (batchId) {
            return exprAddr as B extends undefined ? T : string;
        }

        // Return the expression address directly — callers should use Ad4mModel or
        // getSubjectData() to interact with the created instance.
        return exprAddr as unknown as B extends undefined ? T : string;
    }

    /** Sets properties of an existing instance in the executor. Through an
     *  abstract class (#1435), each value is written by every class of the
     *  record that the abstract class reads, under its own name. */
    async updateSubject(className: string, exprAddr: string, values: Record<string, any>, batchId?: string) {
        await this.#client.updateSubject(this.#handle.uuid, className, exprAddr, JSON.stringify(values), batchId);
    }

    async getSubjectData<T>(subjectClass: T, exprAddr: string): Promise<T> {
        const className = await this.#className(subjectClass);
        return JSON.parse(await this.#client.getSubjectData(this.#handle.uuid, JSON.stringify({className}), exprAddr))
    }

    /** Removes a subject instance by running its (SDNA defined) destructor,
     * which means removing links around the given expression address
     *
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
     * @param exprAddr The address of the expression to be turned into a subject instance
     * @param batchId Optional batch ID for grouping operations. If provided, the removal will be part of the batch
     * and won't be executed until the batch is committed.
     */
    async removeSubject<T>(subjectClass: T, exprAddr: string, batchId?: string) {
        // A built-in abstract class (`TextContent`, #1446) is never stored,
        // so it has no marker: the executor dispatches it by its URI.
        const ctor = (subjectClass as any)?.constructor;
        if (typeof subjectClass === "object" && ctor && Object.prototype.hasOwnProperty.call(ctor, 'builtinUri')) {
            await this.#client.removeSubject(this.#handle.uuid, ctor.builtinUri, exprAddr, batchId);
            return;
        }
        // The executor runs the class's destructor: for an abstract class
        // (#1435), the destructors of the record's classes it reads.
        const className = await this.#className(subjectClass);
        await this.#client.removeSubject(this.#handle.uuid, className, exprAddr, batchId);
    }

    /** Checks if the given expression is a subject instance of the given subject class
     * @param expression The expression to be checked
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
    */
    async isSubjectInstance<T>(expression: string, subjectClass: T): Promise<boolean> {
        const className = await this.#classNameOf(subjectClass);
        const requiredTriples = className ? await this.#requiredTriples(className) : null;
        if (!requiredTriples) {
            console.warn(`isSubjectInstance: no registered class ${className}`);
            return false;
        }

        // If no required triples, any expression with links is an instance
        if (requiredTriples.length === 0) {
            const links = await this.get(new LinkQuery({ source: expression }));
            return links.length > 0;
        }

        // Build a single SPARQL ASK query with all required triple patterns
        const patterns = requiredTriples.map((t, i) => {
            const target = t.target ? `<${t.target}>` : `?t${i}`;
            return `<${expression}> <${t.predicate}> ${target} .`;
        }).join('\n    ');

        const result = await this.querySparql(`ASK WHERE {\n    ${patterns}\n}`);
        return result === true;
    }


    /**
     * The triples every instance of `className` has: those its constructor
     * adds, with the target only where it is fixed (not a value, not a
     * writable property). `null` when no such class is registered.
     */
    async #requiredTriples(className: string): Promise<Array<{predicate: string, target?: string}> | null> {
        const [registered] = await this.shapes([className]);
        if (!registered) return null;
        const { shape } = registered;
        const writablePredicates = new Set(shape.properties.filter(p => p.writable).map(p => p.path));
        return (shape.constructor_actions ?? [])
            .filter(action => action.predicate)
            .map(action => action.target && action.target !== 'value' && !writablePredicates.has(action.predicate!)
                ? { predicate: action.predicate!, target: action.target }
                : { predicate: action.predicate! });
    }

    /** The bases that have every one of `requiredTriples`; with none, every
     *  source of a link. */
    async #findInstances(requiredTriples: Array<{predicate: string, target?: string}>): Promise<string[]> {
        if (requiredTriples.length === 0) {
            const allLinks = await this.get(new LinkQuery({}));
            return [...new Set(allLinks.map(l => l.data.source))];
        }

        // Build a single SPARQL SELECT with all required triple patterns joined
        const patterns = requiredTriples.map((t, i) => {
            const target = t.target ? `<${t.target}>` : `?t${i}`;
            return `?base <${t.predicate}> ${target} .`;
        }).join('\n    ');

        const results = await this.querySparql(
            `SELECT DISTINCT ?base WHERE {\n    ${patterns}\n}`
        );
        if (!Array.isArray(results)) return [];
        return results.map((row: any) => row.base);
    }

    /** Returns all subject instances of the given subject class.
     *  @param subjectClass The name of the subject class, a `@Model` class (its
     * instances come back loaded), or an object with the properties of the
     * subject class (the first registered class that has them is used).
     */
    async getAllSubjectInstances<T>(subjectClass: T): Promise<T[]> {
        const className = await this.#classNameOf(subjectClass)
        const requiredTriples = className ? await this.#requiredTriples(className) : null
        if (!requiredTriples) return []

        const instances: T[] = []
        for (const base of await this.#findInstances(requiredTriples)) {
            try {
                if (typeof subjectClass === "function") {
                    const instance = new (subjectClass as any)(this, base);
                    await instance.get();
                    instances.push(instance);
                } else {
                    const data = JSON.parse(await this.#client.getSubjectData(this.#handle.uuid, JSON.stringify({ className }), base));
                    instances.push({ id: base, ...data } as unknown as T);
                }
            } catch (e) {
                // A base the class's triples match but that does not load is skipped.
            }
        }
        return instances
    }


    /**
     * Find a subject class by matching an object's properties/relations against the registered shapes.
     * @returns The matching class name, or null if no match found.
     */
    private async findClassByProperties(obj: object): Promise<string | null> {
        // Extract properties and relations from the object
        let properties: string[] = [];
        let relations: string[] = [];
        const proto = Object.getPrototypeOf(obj);
        const ctor = proto?.constructor;

        const registryProps = ctor ? getPropertiesMetadata(ctor) : {};
        if (Object.keys(registryProps).length > 0) {
            properties = Object.keys(registryProps);
        } else {
            properties = Object.keys(obj).filter(key => !Array.isArray((obj as any)[key]));
        }

        const registryRelations = ctor
            ? Object.fromEntries(
                Object.entries(getRelationsMetadata(ctor)).filter(([, r]) => r.kind === 'hasMany' || r.kind === 'belongsToMany')
              )
            : {};
        if (Object.keys(registryRelations).length > 0) {
            relations = Object.keys(registryRelations).filter(key => key !== 'isSubjectInstance');
        } else {
            relations = Object.keys(obj).filter(key => Array.isArray((obj as any)[key]) && key !== 'isSubjectInstance');
        }

        if (properties.length === 0 && relations.length === 0) {
            return null;
        }

        // A class matches when it has every property name, and every relation
        // name as a collection: names as the shapes declare them.
        for (const { name, shape } of await this.shapes()) {
            const names = new Set(shape.properties.map(p => p.name));
            const collections = new Set(shape.properties
                .filter(p => p.collection ?? (p.relationKind === 'hasMany' || p.relationKind === 'belongsToMany'))
                .map(p => p.name));
            if (properties.every(p => names.has(p)) && relations.every(r => collections.has(r))) {
                return name;
            }
        }
        return null;
    }

    getNeighbourhoodProxy(): NeighbourhoodProxy {
        return this.#client.getNeighbourhoodProxy(this.#handle.uuid)
    }

    /**
     * Returns a proxy object for working with AI capabilities.
     * 
     * @returns AIClient instance
     * 
     * @example
     * ```typescript
     * // Use AI to analyze perspective content
     * const summary = await perspective.ai.summarize();
     * 
     * // Generate new content
     * const suggestion = await perspective.ai.suggest("next action");
     * ```
     */
    get ai(): AIClient {
        return this.#client.aiClient
    }

    /** Live SPARQL query: resolves once the first result is in.
     *  Call `dispose()` on the returned proxy when done. The executor
     *  refuses a Prolog query (400): Prolog live queries are not supported. */
    async subscribeQuery(query: string): Promise<QuerySubscriptionProxy> {
        const subscription = new QuerySubscriptionProxy(this.uuid, query, this.#client);
        try {
            await subscription.subscribe();
        } catch (e) {
            // Nobody gets the proxy, so nobody can dispose it: left as is, it
            // would re-open on every reconnect and retry for good (#1595).
            subscription.dispose({ cancelOpening: true });
            throw e;
        }
        return subscription;
    }

}