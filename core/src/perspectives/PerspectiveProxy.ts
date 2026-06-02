import { LinkCallback, PerspectiveClient, SyncStateChangeCallback } from "./PerspectiveClient";
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
import { getCachedResult, setCachedResult, invalidatePerspectiveCache } from "../model/query-cache";
import { AllInstancesResult } from "../model/types";

import { SHACLShape } from "../shacl/SHACLShape";
import { SHACLFlow, LinkPattern } from "../shacl/SHACLFlow";

type QueryCallback = (result: AllInstancesResult) => void;

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


// Generic unsubscribe interface for event subscriptions
interface Unsubscribable {
    unsubscribe(): void;
}

/** Proxy object for a subscribed Prolog query that provides real-time updates
 * 
 * This class handles:
 * - Keeping the subscription alive by sending periodic keepalive signals
 * - Managing callbacks for result updates
 * - Subscribing to query updates via WebSocket subscriptions
 * - Maintaining the latest query result
 * - Ensuring subscription is fully initialized before allowing access
 * - Cleaning up resources when disposed
 * 
 * The subscription will remain active as long as keepalive signals are sent.
 * Make sure to call dispose() when you're done with the subscription to clean up
 * resources, stop keepalive signals, and notify the backend to remove the subscription.
 * 
 * The subscription goes through an initialization process where it waits for the first
 * result to come through the subscription channel. You can await the `initialized` 
 * promise to ensure the subscription is ready. The initialization will timeout after
 * 30 seconds if no result is received.
 * 
 * Example usage:
 * ```typescript
 * const subscription = await perspective.subscribeInfer("my_query(X)");
 * // At this point the subscription is already initialized since subscribeInfer waits
 * 
 * // Set up callback for future updates
 * const removeCallback = subscription.onResult(result => {
 *     console.log("New result:", result);
 * });
 * 
 * // Later: clean up subscription and notify backend
 * subscription.dispose();
 * ```
 */
export class QuerySubscriptionProxy {
    #uuid: string;
    #subscriptionId: string;
    #client: PerspectiveClient;
    #callbacks: Set<QueryCallback>;
    #keepaliveTimer: number;
    #unsubscribe?: () => void;
    #latestResult: AllInstancesResult|null;
    #disposed: boolean = false;
    #initialized: Promise<boolean>;
    #initResolve?: (value: boolean) => void;
    #initReject?: (reason?: any) => void;
    #initTimeoutId?: NodeJS.Timeout;
    #query: string;

    /** Creates a new query subscription
     * @param uuid - The UUID of the perspective
     * @param query - The Prolog query to subscribe to
     * @param client - The PerspectiveClient instance to use for communication
     */
    constructor(uuid: string, query: string, client: PerspectiveClient) {
        this.#uuid = uuid;
        this.#query = query;
        this.#client = client;
        this.#callbacks = new Set();
        this.#latestResult = null;
        
        // Create the promise once and store its resolve/reject
        this.#initialized = new Promise<boolean>((resolve, reject) => {
            this.#initResolve = resolve;
            this.#initReject = reject;
        });
    }

    async subscribe() {
        // Clean up previous subscription attempt if retrying
        if (this.#unsubscribe) {
            this.#unsubscribe();
            this.#unsubscribe = undefined;
        }
        
        // Clear any existing timeout
        if (this.#initTimeoutId) {
            clearTimeout(this.#initTimeoutId);
            this.#initTimeoutId = undefined;
        }

        // Clear any existing keepalive timer to prevent accumulation
        if (this.#keepaliveTimer) {
            clearTimeout(this.#keepaliveTimer);
            this.#keepaliveTimer = undefined;
        }

        try {
            // Initialize the query subscription
            let initialResult;
            initialResult = await this.#client.subscribeQuery(this.#uuid, this.#query);
            this.#subscriptionId = initialResult.subscriptionId;

            // Process the initial result immediately for fast UX.
            // The subscribeQuery() RPC call already returns the initial result,
            // so treat that as successful initialization instead of waiting for a
            // follow-up WebSocket update that may never arrive until the query changes.
            if (initialResult.result !== undefined) {
                this.#latestResult = initialResult.result;
                this.#notifyCallbacks(initialResult.result);

                if (this.#initResolve) {
                    this.#initResolve(true);
                    this.#initResolve = undefined;
                    this.#initReject = undefined;
                }
            } else {
                console.warn('⚠️ No initial result returned from subscribeQuery!');

                // Only keep the initialization timeout when the backend did not
                // provide an initial result up front.
                this.#initTimeoutId = setTimeout(() => {
                    console.error('Subscription initialization timed out after 30 seconds. Resubscribing...');
                    // Recursively retry subscription, catching any errors
                    this.subscribe().catch(error => {
                        console.error('Error during subscription retry after timeout:', error);
                    });
                }, 30000);
            }
            
            // Subscribe to query updates
            this.#unsubscribe = this.#client.subscribeToQueryUpdates(
                this.#subscriptionId,
                (updateResult) => {
                    // Clear timeout on first message
                    if (this.#initTimeoutId) {
                        clearTimeout(this.#initTimeoutId);
                        this.#initTimeoutId = undefined;
                    }
                    
                    // Resolve the initialization promise (only resolves once)
                    if (this.#initResolve) {
                        this.#initResolve(true);
                        this.#initResolve = undefined;  // Prevent double-resolve
                        this.#initReject = undefined;
                    }

                    this.#latestResult = updateResult;
                    this.#notifyCallbacks(updateResult);
                }
            );
        } catch (error) {
            console.error('Error setting up subscription:', error);
            
            // Reject the promise if this is the first attempt
            if (this.#initReject) {
                this.#initReject(error);
                this.#initResolve = undefined;
                this.#initReject = undefined;
            }
            
            throw error; // Re-throw so caller knows it failed
        }

        // Start keepalive loop using platform-agnostic setTimeout
        const keepaliveLoop = async () => {
            if (this.#disposed) return;
            
            try {
                await this.#client.keepAliveQuery(this.#uuid, this.#subscriptionId);
            } catch (e) {
                console.error('Error in keepalive:', e);
                // try to reinitialize the subscription
                console.log('Reinitializing subscription for query:', this.#query);
                try {
                    await this.subscribe();
                    console.log('Subscription reinitialized');
                } catch (resubscribeError) {
                    console.error('Error during resubscription from keepalive:', resubscribeError);
                    // Don't schedule another keepalive on resubscribe failure
                    return;
                }
            }

            // Schedule next keepalive if not disposed
            if (!this.#disposed) {
                this.#keepaliveTimer = setTimeout(keepaliveLoop, 30000) as unknown as number;
            }
        };

        // Start the first keepalive loop
        this.#keepaliveTimer = setTimeout(keepaliveLoop, 30000) as unknown as number;
    }

    /** Get the subscription ID for this query subscription
     * 
     * This is a unique identifier assigned when the subscription was created.
     * It can be used to reference this specific subscription, for example when
     * sending keepalive signals.
     * 
     * @returns The subscription ID string
     */
    get id(): string {
        return this.#subscriptionId;
    }

/** Promise that resolves when the subscription has received its first result
 * through the subscription channel. This ensures the subscription is fully
 * set up before allowing access to results or updates.
 * 
 * If no result is received within 30 seconds, the subscription will automatically
 * retry. The promise will remain pending until a subscription message successfully
 * arrives, or until a fatal error occurs during subscription setup.
 * 
 * Note: You typically don't need to await this directly since the subscription
 * creation methods (like subscribeInfer) already wait for initialization.
 */
    get initialized(): Promise<boolean> {
        return this.#initialized;
    }

    /** Get the latest query result
     * 
     * This returns the most recent result from the query, which could be either:
     * - The initial result from when the subscription was created
     * - The latest update received through the subscription
     * 
     * @returns The latest query result as a string (usually a JSON array of bindings)
     */
    get result(): AllInstancesResult {
        return this.#latestResult;
    }

    /** Add a callback that will be called whenever new results arrive
     * 
     * The callback will be called immediately with the current result,
     * and then again each time the query results change.
     * 
     * @param callback - Function that takes a result string and processes it
     * @returns A function that can be called to remove this callback
     * 
     * Example:
     * ```typescript
     * const removeCallback = subscription.onResult(result => {
     *     const bindings = JSON.parse(result);
     *     console.log("New bindings:", bindings);
     * });
     * 
     * // Later: stop receiving updates
     * removeCallback();
     * ```
     */
    onResult(callback: QueryCallback): () => void {
        this.#callbacks.add(callback);
        return () => this.#callbacks.delete(callback);
    }

    /** Internal method to notify all callbacks of a new result */
    #notifyCallbacks(result: AllInstancesResult) {
        for (const callback of this.#callbacks) {
            try {
                callback(result);
            } catch (e) {
                console.error('Error in query subscription callback:', e);
            }
        }
    }

    /** Clean up the subscription and stop keepalive signals
     * 
     * This method:
     * 1. Stops the keepalive timer
     * 2. Unsubscribes from subscription updates
     * 3. Clears all registered callbacks
     * 4. Cleans up any pending initialization timeout
     * 
     * After calling this method, the subscription is no longer active and
     * will not receive any more updates. The instance should be discarded.
     */
    dispose() {
        this.#disposed = true;
        clearTimeout(this.#keepaliveTimer);
        if (this.#unsubscribe) {
            this.#unsubscribe();
        }
        this.#callbacks.clear();
        if (this.#initTimeoutId) {
            clearTimeout(this.#initTimeoutId);
            this.#initTimeoutId = undefined;
        }

        // Tell the backend to dispose of the subscription
        if (this.#subscriptionId) {
            this.#client.disposeQuerySubscription(this.#uuid, this.#subscriptionId)
                .catch(e => console.error('Error disposing query subscription:', e));
        }
    }
}

type PerspectiveListenerTypes = "link-added" | "link-removed" | "link-updated"

export type LinkStatus = "shared" | "local"
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
 * await perspective.addSdna(todoClass, "subject_class");
 * const todo = await perspective.createSubject("Todo", "expression://123");
 * 
 * // Subscribe to changes
 * perspective.addListener("link-added", (link) => {
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

    #perspectiveLinkAddedCallbacks: LinkCallback[]
    #perspectiveLinkRemovedCallbacks: LinkCallback[]
    #perspectiveLinkUpdatedCallbacks: LinkCallback[]
    #perspectiveSyncStateChangeCallbacks: SyncStateChangeCallback[]
    #ensuredSubjectClasses = new Set<string>()

    /**
     * Creates a new PerspectiveProxy instance.
     * Note: Don't create this directly, use ad4m.perspective.add() instead.
     */
    constructor(handle: PerspectiveHandle, ad4m: PerspectiveClient) {
        this.#perspectiveLinkAddedCallbacks = []
        this.#perspectiveLinkRemovedCallbacks = []
        this.#perspectiveLinkUpdatedCallbacks = []
        this.#perspectiveSyncStateChangeCallbacks = []
        this.#handle = handle
        this.#client = ad4m
        this.uuid = this.#handle.uuid;
        this.name = this.#handle.name;
        this.owners = this.#handle.owners;
        this.sharedUrl = this.#handle.sharedUrl;
        this.neighbourhood = this.#handle.neighbourhood;
        this.state = this.#handle.state;
        this.#client.addPerspectiveLinkAddedListener(this.#handle.uuid, this.#perspectiveLinkAddedCallbacks)
        this.#client.addPerspectiveLinkRemovedListener(this.#handle.uuid, this.#perspectiveLinkRemovedCallbacks)
        this.#client.addPerspectiveLinkUpdatedListener(this.#handle.uuid, this.#perspectiveLinkUpdatedCallbacks)
        this.#client.addPerspectiveSyncStateChangeListener(this.#handle.uuid, this.#perspectiveSyncStateChangeCallbacks)
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
     * Escapes special regex characters in a string to prevent ReDoS attacks
     * and regex injection when building dynamic regular expressions.
     * 
     * @param str - The string to escape
     * @returns The escaped string safe for use in RegExp constructor
     * 
     * @private
     */
    private escapeRegExp(str: string): string {
        return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
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
        const result = await this.#client.executeCommands(this.#handle.uuid, JSON.stringify(actions), expression, JSON.stringify(parameters), batchId)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result
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
    async get(query: LinkQuery): Promise<LinkExpression[]> {
        return await this.#client.queryLinks(this.#handle.uuid, query)
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
    async infer(query: string): Promise<any> {
        return await this.#client.queryProlog(this.#handle.uuid, query)
    }

    /**
     * Executes a SPARQL query against the perspective's link cache.
     * This allows powerful SQL-like queries on the link data stored in SPARQL.
     * 
     * **Security Note:** Only read-only queries (SELECT, RETURN, etc.) are permitted.
     * Mutating operations (DELETE, UPDATE, INSERT, CREATE, DROP, DEFINE, etc.) are
     * blocked for security reasons. Use the perspective's add/remove methods to modify links.
     * 
     * @param query - SPARQL query string (read-only operations only)
     * @returns Query results as parsed JSON
     * 
     * Executes a SPARQL query against the perspective's RDF (Oxigraph) store.
     *
     * @param query - SPARQL query string
     * @returns Query results as parsed JSON
     */
    async querySparql<T = any>(query: string): Promise<T> {
        const cached = getCachedResult(this.#handle.uuid, query);
        if (cached !== undefined) return cached as T;
        const result = await this.#client.querySparql(this.#handle.uuid, query);
        setCachedResult(this.#handle.uuid, query, result);
        return result as T;
    }

    /**
     * Execute a model query — the executor-side replacement for SPARQL query building + JS hydration.
     *
     * The executor resolves the model's shape from the perspective's SHACL
     * triples (written by `addSdna`).  Callers no longer ship shape
     * metadata with the query; the perspective is the source of truth.
     *
     * @param className - The model class name (e.g. "Recipe")
     * @param queryJson - Structured query as JSON string
     * @returns Object with `instances` array and `totalCount`
     */
    async modelQuery(className: string, queryJson: string): Promise<{ instances: any[], totalCount: number }> {
        return await this.#client.modelQuery(this.#handle.uuid, className, queryJson);
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
     * Subscribe to model query changes. Builds trigger SPARQL from the model shape
     * internally in Rust, registers a subscription, runs the initial query, and
     * pushes updated results when relevant links change.
     *
     * The subscription reuses the same GraphQL subscription channel as subscribeQuery().
     * Use keepAliveQuery() / disposeQuerySubscription() with the returned subscriptionId.
     *
     * @param className - The model class name
     * @param queryJson - JSON-serialized query parameters (same as modelQuery)
     * @returns Object with `subscriptionId` and initial `result`
     */
    async modelSubscribe(className: string, queryJson: string): Promise<{ subscriptionId: string, result: any }> {
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
        const result = await this.#client.addLink(this.#handle.uuid, link, status, batchId)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result;
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
        const result = await this.#client.addLinks(this.#handle.uuid, links, status, batchId)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result;
    }

    /**
     * Removes multiple links from the perspective.
     * 
     * @param links - Array of links to remove
     * @param batchId - Optional batch ID to group this operation with others
     * @returns Array of removed LinkExpressions
     */
    async removeLinks(links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        const result = await this.#client.removeLinks(this.#handle.uuid, links, batchId)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result;
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
        const result = await this.#client.linkMutations(this.#handle.uuid, mutations, status)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result;
    }

        /**
     * Adds a pre-signed LinkExpression to the perspective.
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
        const result = await this.#client.updateLink(this.#handle.uuid, oldLink, newLink, batchId)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result;
    }

    /**
     * Removes a link from the perspective.
     * 
     * @param link - The link to remove
     * @param batchId - Optional batch ID to group this operation with others
     */
    async remove(link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        const result = await this.#client.removeLink(this.#handle.uuid, link, batchId)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result;
    }

    /** Creates a new batch for grouping operations */
    async createBatch(): Promise<string> {
        return await this.#client.createBatch(this.#handle.uuid)
    }

    /** Commits a batch of operations */
    async commitBatch(batchId: string): Promise<LinkExpressionMutations> {
        const result = await this.#client.commitBatch(this.#handle.uuid, batchId)
        invalidatePerspectiveCache(this.#handle.uuid);
        return result;
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
     * Subscribes to link changes in the perspective.
     * 
     * @param type - Type of change to listen for
     * @param cb - Callback function
     * 
     * @example
     * ```typescript
     * // Listen for new links
     * perspective.addListener("link-added", (link) => {
     *   console.log("New link:", link);
     * });
     * 
     * // Listen for removed links
     * perspective.addListener("link-removed", (link) => {
     *   console.log("Link removed:", link);
     * });
     * ```
     */
    async addListener(type: PerspectiveListenerTypes, cb: LinkCallback) {
        if (type === 'link-added') {
            this.#perspectiveLinkAddedCallbacks.push(cb);
        } else if (type === 'link-removed') {
            this.#perspectiveLinkRemovedCallbacks.push(cb);
        } else if (type === 'link-updated') {
            this.#perspectiveLinkUpdatedCallbacks.push(cb);
        }
    }

    /**
     * Subscribes to sync state changes if this perspective is shared.
     * 
     * @param cb - Callback function
     * 
     * @example
     * ```typescript
     * perspective.addSyncStateChangeListener((state) => {
     *   console.log("Sync state:", state);
     * });
     * ```
     */
    async addSyncStateChangeListener(cb: SyncStateChangeCallback) {
        this.#perspectiveSyncStateChangeCallbacks.push(cb)
    }

    /**
     * Unsubscribes from link changes.
     * 
     * @param type - Type of change to stop listening for
     * @param cb - The callback function to remove
     */
    async removeListener(type: PerspectiveListenerTypes, cb: LinkCallback) {
        if (type === 'link-added') {
            const index = this.#perspectiveLinkAddedCallbacks.indexOf(cb);
            if (index >= 0) this.#perspectiveLinkAddedCallbacks.splice(index, 1);
        } else if (type === 'link-removed') {
            const index = this.#perspectiveLinkRemovedCallbacks.indexOf(cb);
            if (index >= 0) this.#perspectiveLinkRemovedCallbacks.splice(index, 1);
        } else if (type === 'link-updated') {
            const index = this.#perspectiveLinkUpdatedCallbacks.indexOf(cb);
            if (index >= 0) this.#perspectiveLinkUpdatedCallbacks.splice(index, 1);
        }
    }

    /** Clean up all subscriptions registered by this proxy.
     *  Call this when the proxy is no longer needed to prevent subscription leaks.
     *  After calling dispose(), the proxy should not be used. */
    dispose(): void {
        this.#client.removeAllListeners(this.#handle.uuid)
        this.#perspectiveLinkAddedCallbacks.length = 0
        this.#perspectiveLinkRemovedCallbacks.length = 0
        this.#perspectiveLinkUpdatedCallbacks.length = 0
        this.#perspectiveSyncStateChangeCallbacks.length = 0
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
        const foundLinks = await this.get(query)
        const removals = [];
        for(const l of foundLinks){
            delete l.__typename
            delete l.data.__typename
            delete l.proof.__typename
            removals.push(l);
        }
        const additions = [link];

        await this.linkMutations({additions, removals}, status)
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

    /** Returns all Social DNA flows that can be started from the given expression */
    async availableFlows(exprAddr: string): Promise<string[]> {
        const allFlowNames = await this.sdnaFlows();
        const available: string[] = [];
        for (const name of allFlowNames) {
            const flow = await this.getFlow(name);
            if (!flow) continue;
            if (flow.flowable === "any") {
                available.push(name);
            } else {
                // Check if the expression matches the flowable link pattern
                const pattern = flow.flowable as LinkPattern;
                const source = pattern.source || exprAddr;
                const links = await this.get(new LinkQuery({
                    source,
                    predicate: pattern.predicate,
                    target: pattern.target
                }));
                if (links.length > 0) {
                    available.push(name);
                }
            }
        }
        return available;
    }

    /**  Starts the Social DNA flow @param flowName on the expression @param exprAddr */
    async startFlow(flowName: string, exprAddr: string) {
        const flow = await this.getFlow(flowName);
        if (!flow) throw `Flow "${flowName}" not found`;
        if (flow.startAction.length === 0) throw `Flow "${flowName}" has no start action`;
        await this.executeAction(flow.startAction, exprAddr, undefined)
    }

    /** Returns all expressions in the given state of given Social DNA flow */
    async expressionsInFlowState(flowName: string, flowState: number): Promise<string[]> {
        const flow = await this.getFlow(flowName);
        if (!flow) return [];
        // Find the state with the matching value
        const state = flow.states.find(s => s.value === flowState);
        if (!state) return [];
        // Query for expressions matching this state's check pattern
        const pattern = state.stateCheck;
        const links = await this.get(new LinkQuery({
            predicate: pattern.predicate,
            target: pattern.target
        }));
        // Return the sources (expression addresses) - use source if pattern has no explicit source
        return links.map(l => pattern.source ? l.data.target : l.data.source);
    }

    /** Returns the given expression's flow state with regard to given Social DNA flow */
    async flowState(flowName: string, exprAddr: string): Promise<number> {
        const flow = await this.getFlow(flowName);
        if (!flow) throw `Flow "${flowName}" not found`;
        // Check each state to find which one the expression is in
        for (const state of flow.states) {
            const pattern = state.stateCheck;
            const source = pattern.source || exprAddr;
            const links = await this.get(new LinkQuery({
                source,
                predicate: pattern.predicate,
                target: pattern.target
            }));
            if (links.length > 0) return state.value;
        }
        throw `Expression "${exprAddr}" is not in any state of flow "${flowName}"`;
    }

    /** Returns available action names, with regard to Social DNA flow and expression's flow state */
    async flowActions(flowName: string, exprAddr: string): Promise<string[]> {
        const flow = await this.getFlow(flowName);
        if (!flow) return [];
        // Determine current state
        let currentStateName: string | null = null;
        for (const state of flow.states) {
            const pattern = state.stateCheck;
            const source = pattern.source || exprAddr;
            const links = await this.get(new LinkQuery({
                source,
                predicate: pattern.predicate,
                target: pattern.target
            }));
            if (links.length > 0) {
                currentStateName = state.name;
                break;
            }
        }
        if (!currentStateName) return [];
        // Return transitions available from current state
        return flow.transitions
            .filter(t => t.fromState === currentStateName)
            .map(t => t.actionName);
    }

    /** Runs given Social DNA flow action */
    async runFlowAction(flowName: string, exprAddr: string, actionName: string) {
        const flow = await this.getFlow(flowName);
        if (!flow) throw `Flow "${flowName}" not found`;
        const transition = flow.transitions.find(t => t.actionName === actionName);
        if (!transition) throw `Action "${actionName}" not found in flow "${flowName}"`;
        await this.executeAction(transition.actions, exprAddr, undefined)
    }

    /** Returns the perspective's Social DNA code
     * This will return all SDNA code elements in an array.
     */
    async getSdna(): Promise<string[]> {
        // First, find all the name literals that are linked from ad4m://self with SDNA predicates
        const sdnaPredicates = [
            "ad4m://has_subject_class",
            "ad4m://has_flow",
            "ad4m://has_custom_sdna"
        ];
        
        const allSdnaCode: string[] = [];
        
        for (const predicate of sdnaPredicates) {
            // Find name literals linked from ad4m://self with this predicate
            const nameLinks = await this.get(new LinkQuery({
                source: "ad4m://self",
                predicate: predicate
            }));
            
            // For each name literal found, get the actual SDNA code
            for (const nameLink of nameLinks) {
                const nameLiteral = nameLink.data.target;
                
                // Now find the SDNA code linked from this name with predicate "ad4m://sdna"
                const sdnaLinks = await this.get(new LinkQuery({
                    source: nameLiteral,
                    predicate: "ad4m://sdna"
                }));
                
                // Extract the SDNA code from each link
                for (const sdnaLink of sdnaLinks) {
                    const code = Literal.fromUrl(sdnaLink.data.target).get();
                    if (typeof code === 'string') {
                        allSdnaCode.push(code);
                    }
                }
            }
        }
        
        return allSdnaCode;
    }

    /** Returns the Social DNA code for a specific class
     * This will return the SDNA code for the specified class, or null if not found.
     */
    async getSdnaForClass(className: string): Promise<string | null> {
        // First, find the name literal for this class that is linked from ad4m://self
        const nameLiteral = Literal.from(className);
        
        const links = await this.get(new LinkQuery({
            source: "ad4m://self",
            target: nameLiteral.toUrl(),
            predicate: "ad4m://has_subject_class"
        }));
        
        if (links.length === 0) {
            return null;
        }
        
        // Now find the SDNA code linked from this name with predicate "ad4m://sdna"
        const sdnaLinks = await this.get(new LinkQuery({
            source: nameLiteral.toUrl(),
            predicate: "ad4m://sdna"
        }));
        
        if (sdnaLinks.length === 0) {
            return null;
        }
        
        // Extract the SDNA code from the first link
        const code = Literal.fromUrl(sdnaLinks[0].data.target).get();
        return typeof code === 'string' ? code : null;
    }

    /** 
     * Adds Social DNA code to the perspective.
     * 
     * **Recommended:** Use {@link addShacl} instead, which accepts the `SHACLShape` type directly.
     * This method is primarily for the RPC API layer and legacy Prolog code.
     * 
     * @param name - Unique name for this SDNA definition
     * @param sdnaCode - Prolog SDNA code (legacy, can be empty string if shaclJson provided)
     * @param sdnaType - Type of SDNA: "subject_class", "flow", or "custom"
     * @param shaclJson - SHACL JSON string (use addShacl() for type-safe alternative)
     * 
     * @example
     * // Recommended: Use addShacl() with SHACLShape type
     * const shape = new SHACLShape('recipe://Recipe');
     * shape.addProperty({ name: 'title', path: 'recipe://title', datatype: 'xsd:string' });
     * await perspective.addShacl('Recipe', shape);
     * 
     * // Legacy: Prolog code is auto-converted to SHACL
     * await perspective.addSdna('Recipe', prologCode, 'subject_class');
     */
    async addSdna(name: string, sdnaCode: string, sdnaType: "subject_class" | "flow" | "custom", shaclJson?: string) {
        return this.#client.addSdna(this.#handle.uuid, name, sdnaCode, sdnaType, shaclJson)
    }

    /**
     * Batch variant of addSdna — registers multiple SDNA entries in a single RPC call.
     * Acquires the Rust-side mutex once for the entire batch.
     */
    async addSdnaAll(entries: { name: string; sdnaCode?: string; sdnaType: "subject_class" | "flow" | "custom"; shaclJson?: string }[]): Promise<boolean[]> {
        return this.#client.addSdnaBatch(this.#handle.uuid, entries)
    }

    /**
     * Batch-registers multiple model classes as SHACL subject classes in a single RPC call.
     * Skips classes already registered on this perspective instance.
     */
    async ensureSubjectClasses(jsClasses: any[]): Promise<void> {
        const entries: { name: string; sdnaCode?: string; sdnaType: "subject_class" | "flow" | "custom"; shaclJson?: string }[] = [];
        for (const jsClass of jsClasses) {
            const className = jsClass.className || jsClass.prototype?.className || jsClass.name;
            if (this.#ensuredSubjectClasses.has(className)) continue;

            if (!jsClass.generateSHACL) {
                throw new Error(`Class ${jsClass.name} must have generateSHACL(). Use @Model decorator.`);
            }

            const { shape } = jsClass.generateSHACL();
            entries.push({
                name: className,
                sdnaType: 'subject_class',
                shaclJson: JSON.stringify(shape.toJSON()),
            });
        }
        if (entries.length === 0) return;

        await this.addSdnaAll(entries);
        for (const entry of entries) {
            this.#ensuredSubjectClasses.add(entry.name);
        }
    }

    /**
     * Adds a subject class to the perspective.
     * Alias for addSdna() with sdnaType='subject_class'.
     */
    async addSubjectClass(name: string, shaclJson: string) {
        return this.addSdna(name, '', 'subject_class', shaclJson);
    }

    /**
     * **Recommended way to add SDNA schemas.**
     * 
     * Store a SHACL shape in this Perspective using the type-safe `SHACLShape` class.
     * The shape is serialized as RDF triples (links) for native AD4M storage and querying.
     * 
     * @param name - Unique name for this schema (e.g., 'Recipe', 'Task')
     * @param shape - SHACLShape instance defining the schema
     * 
     * @example
     * import { SHACLShape } from '@coasys/ad4m';
     * 
     * const shape = new SHACLShape('recipe://Recipe');
     * shape.addProperty({ 
     *   name: 'title', 
     *   path: 'recipe://title', 
     *   datatype: 'xsd:string',
     *   minCount: 1 
     * });
     * shape.addProperty({
     *   name: 'ingredients',
     *   path: 'recipe://has_ingredient',
     *   // No maxCount = relation
     * });
     * 
     * await perspective.addShacl('Recipe', shape);
     */
    async addShacl(name: string, shape: SHACLShape): Promise<void> {
        // Serialize shape to links
        const shapeLinks = shape.toLinks();
        
        // Create name -> shape mapping links
        const nameMapping = Literal.fromUrl(`literal:string:shacl://${name}`);
        const allLinks: Link[] = [
            ...shapeLinks.map(l => new Link({
                source: l.source,
                predicate: l.predicate,
                target: l.target
            })),
            new Link({
                source: "ad4m://self",
                predicate: "ad4m://has_shacl",
                target: nameMapping.toUrl()
            }),
            new Link({
                source: nameMapping.toUrl(),
                predicate: "ad4m://shacl_shape_uri",
                target: shape.nodeShapeUri
            })
        ];
        
        // Batch add all links at once
        await this.addLinks(allLinks);
    }
    
    /**
     * Retrieve a SHACL shape by name from this Perspective
     */
    async getShacl(name: string): Promise<SHACLShape | null> {
        // Find the shape URI from the name mapping
        const nameMapping = Literal.fromUrl(`literal:string:shacl://${name}`);
        const shapeUriLinks = await this.get(new LinkQuery({
            source: nameMapping.toUrl(),
            predicate: "ad4m://shacl_shape_uri"
        }));
        
        if (shapeUriLinks.length === 0) {
            return null;
        }
        
        const shapeUri = shapeUriLinks[0].data.target;
        
        // First get property shape URIs so we can query everything in one go
        const propertyLinks = await this.get(new LinkQuery({
            source: shapeUri,
            predicate: "sh://property"
        }));
        
        // Fetch all links from the shape and its property shapes
        const sourceUris = [shapeUri, ...propertyLinks.map(l => l.data.target)];
        const allLinks: Array<{source: string, predicate: string, target: string}> = [];
        for (const uri of sourceUris) {
            const links = await this.get(new LinkQuery({ source: uri }));
            for (const l of links) {
                allLinks.push({
                    source: l.data.source,
                    predicate: l.data.predicate,
                    target: l.data.target
                });
            }
        }
        
        const shapeLinks = allLinks;
        
        return SHACLShape.fromLinks(shapeLinks, shapeUri);
    }
    
    /**
     * Get all SHACL shapes stored in this Perspective
     */
    async getAllShacl(): Promise<Array<{name: string, shape: SHACLShape}>> {
        const nameLinks = await this.get(new LinkQuery({
            source: "ad4m://self",
            predicate: "ad4m://has_shacl"
        }));
        
        const shapes = [];
        for (const nameLink of nameLinks) {
            const nameUrl = nameLink.data.target;
            const name = Literal.fromUrl(nameUrl).get() as string;
            const shapeName = name.replace('shacl://', '');
            
            const shape = await this.getShacl(shapeName);
            if (shape) {
                shapes.push({ name: shapeName, shape });
            }
        }
        
        return shapes;
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
     * const todoFlow = new SHACLFlow('TODO', 'todo://');
     * todoFlow.flowable = 'any';
     * 
     * // Define states
     * todoFlow.addState({ name: 'ready', value: 0, stateCheck: { predicate: 'todo://state', target: 'todo://ready' }});
     * todoFlow.addState({ name: 'done', value: 1, stateCheck: { predicate: 'todo://state', target: 'todo://done' }});
     * 
     * // Define start action
     * todoFlow.startAction = [{ action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }];
     * 
     * // Define transitions
     * todoFlow.addTransition({
     *   actionName: 'Complete',
     *   fromState: 'ready',
     *   toState: 'done',
     *   actions: [
     *     { action: 'addLink', source: 'this', predicate: 'todo://state', target: 'todo://done' },
     *     { action: 'removeLink', source: 'this', predicate: 'todo://state', target: 'todo://ready' }
     *   ]
     * });
     * 
     * await perspective.addFlow('TODO', todoFlow);
     * ```
     */
    async addFlow(name: string, flow: SHACLFlow): Promise<void> {
        // Serialize flow to links
        const flowLinks = flow.toLinks();
        
        // Create registration and mapping links
        const flowNameLiteral = Literal.from(name).toUrl();
        const allLinks: Link[] = [
            ...flowLinks.map(l => new Link({
                source: l.source,
                predicate: l.predicate,
                target: l.target
            })),
            new Link({
                source: "ad4m://self",
                predicate: "ad4m://has_flow",
                target: flowNameLiteral
            }),
            new Link({
                source: flowNameLiteral,
                predicate: "ad4m://flow_uri",
                target: flow.flowUri
            })
        ];
        
        // Batch add all links at once
        await this.addLinks(allLinks);
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
        // Compute alternate prefix for state/transition URIs
        const alternatePrefix = flowUri.endsWith('Flow') 
            ? flowUri.slice(0, -4) + '.'
            : flowUri + '.';
        
        // Fetch flow links using SPARQL to match flowUri source OR alternatePrefix sources
        const sparqlQuery = `SELECT ?s ?p ?o WHERE {
            ?s ?p ?o .
            FILTER(?s = <${flowUri}> || STRSTARTS(STR(?s), "${alternatePrefix}"))
        }`;
        const sparqlResult = await this.querySparql(sparqlQuery);
        
        const flowLinks = (sparqlResult || []).map((r: any) => ({
            source: r.s,
            predicate: r.p,
            target: r.o
        }));
        
        return SHACLFlow.fromLinks(flowLinks, flowUri);
    }

    /** Returns all the Subject classes defined in this perspectives SDNA 
     * 
     * Uses SHACL-based lookup (Prolog-free implementation).
     */
    async subjectClasses(): Promise<string[]> {
        try {
            // Query SHACL class links directly — no need for a separate RPC endpoint
            const classLinks = await this.get(new LinkQuery({
                predicate: "rdf://type",
                target: "ad4m://SubjectClass"
            }));
            const classNames = classLinks
                .map(l => {
                    const source = l.data.source;
                    // Extract class name from URI like "recipe://Recipe" or "flux://Channel"
                    const parts = source.split("://");
                    const lastPart = parts[parts.length - 1];
                    return lastPart.split('/').pop() || '';
                })
                .filter(name => name.length > 0);
            // Deduplicate
            return [...new Set(classNames)];
        } catch (e) {
            console.warn('subjectClasses: SHACL lookup failed:', e);
            return [];
        }
    }

    async stringOrTemplateObjectToSubjectClassName<T>(subjectClass: T): Promise<string> {
        if(typeof subjectClass === "string")
            return subjectClass
        else {
            let subjectClasses = await this.subjectClassesByTemplate(subjectClass as object)
            if(subjectClasses[0]) {
                return subjectClasses[0]
            } else {
                //@ts-ignore
                return subjectClass.className
            }
        }
    }

    /**
     * Creates a new subject instance of the given subject class
     * 
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class.
     * @param exprAddr The address of the expression to be turned into a subject instance
     * @param initialValues Optional initial values for properties. If provided, these will be
     * merged with constructor actions for better performance.
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
        let className: string;

        if(typeof subjectClass === "string") {
            className = subjectClass;
            await this.#client.createSubject(
                this.#handle.uuid, 
                JSON.stringify({
                    className,
                    initialValues
                }), 
                exprAddr,
                initialValues ? JSON.stringify(initialValues) : undefined,
                batchId
            );
        } else {
            const obj = subjectClass as any;
            const resolvedClassName = obj.className || obj.constructor?.className || obj.constructor?.prototype?.className
                || await this.findClassByProperties(obj);
            if (!resolvedClassName) {
                throw new Error("Could not resolve subject class name from object. Use a decorated class or pass a className string.");
            }
            await this.#client.createSubject(
                this.#handle.uuid, 
                JSON.stringify({
                    className: resolvedClassName,
                    initialValues
                }), 
                exprAddr,
                initialValues ? JSON.stringify(initialValues) : undefined,
                batchId
            );
        }

        // Skip subject proxy creation when in batch mode since the subject won't exist until batch is committed
        if (batchId) {
            return exprAddr as B extends undefined ? T : string;
        }

        // Return the expression address directly — callers should use Ad4mModel or
        // getSubjectData() to interact with the created instance.
        return exprAddr as unknown as B extends undefined ? T : string;
    }

    async getSubjectData<T>(subjectClass: T, exprAddr: string): Promise<T> {
        if (typeof subjectClass === "string") {
            return JSON.parse(await this.#client.getSubjectData(this.#handle.uuid, JSON.stringify({className: subjectClass}), exprAddr))
        }
        const obj = subjectClass as any;
        const resolvedClassName = obj.className || obj.constructor?.className || obj.constructor?.prototype?.className
            || await this.findClassByProperties(obj);
        if (!resolvedClassName) {
            throw new Error("Could not resolve subject class name from object. Use a decorated class or pass a className string.");
        }
        return JSON.parse(await this.#client.getSubjectData(this.#handle.uuid, JSON.stringify({className: resolvedClassName}), exprAddr))
    }

    /**
     * Gets actions from SHACL links for a given predicate (e.g., ad4m://constructor, ad4m://destructor).
     * Returns the parsed action array if found, or null if not found.
     */
    private async getActionsFromSHACL(className: string, predicate: string): Promise<any[] | null> {
        // Use regex to match exact class name followed by "Shape" at end of URI
        // This prevents "RecipeShape" from matching "MyRecipeShape"
        const escaped = this.escapeRegExp(className);
        const shapePattern = new RegExp(`[/:#]${escaped}Shape$`);
        const links = await this.get(new LinkQuery({ predicate }));

        for (const link of links) {
            if (shapePattern.test(link.data.source)) {
                // Parse actions from literal:string:{json}
                const prefix = "literal:string:";
                if (link.data.target.startsWith(prefix)) {
                    const jsonStr = link.data.target.slice(prefix.length);
                    // Decode URL-encoded JSON if needed, with fallback for raw % characters
                    let decoded = jsonStr;
                    try { decoded = decodeURIComponent(jsonStr); } catch {}
                    try {
                        return JSON.parse(decoded);
                    } catch (e) {
                        console.warn(`Failed to parse SHACL actions JSON for ${className}:`, e);
                        return null;
                    }
                }
            }
        }

        return null;
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
        let className = await this.stringOrTemplateObjectToSubjectClassName(subjectClass)

        // Get destructor actions from SHACL links (Prolog-free)
        let actions = await this.getActionsFromSHACL(className, "ad4m://destructor");

        if (!actions) {
            throw `No destructor found for subject class: ${className}. Make sure the class was registered with SHACL.`;
        }

        await this.executeAction(actions, exprAddr, undefined, batchId)
    }

    /** Checks if the given expression is a subject instance of the given subject class
     * @param expression The expression to be checked
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
    */
    async isSubjectInstance<T>(expression: string, subjectClass: T): Promise<boolean> {
        let className = await this.stringOrTemplateObjectToSubjectClassName(subjectClass)

        // Get metadata from SHACL links
        const metadata = await this.getSubjectClassMetadata(className);
        if (!metadata) {
            console.warn(`isSubjectInstance: No SHACL metadata found for class ${className}`);
            return false;
        }

        // If no required triples, any expression with links is an instance
        if (metadata.requiredTriples.length === 0) {
            const links = await this.get(new LinkQuery({ source: expression }));
            return links.length > 0;
        }

        // Build a single SPARQL ASK query with all required triple patterns
        const patterns = metadata.requiredTriples.map((t, i) => {
            const target = t.target ? `<${t.target}>` : `?t${i}`;
            return `<${expression}> <${t.predicate}> ${target} .`;
        }).join('\n    ');

        const result = await this.querySparql(`ASK WHERE {\n    ${patterns}\n}`);
        return result === true;
    }


    /**
     * Gets subject class metadata from SHACL links using SHACLShape.fromLinks().
     * Retrieves the SHACL shape and extracts metadata for instance queries.
     */
    async getSubjectClassMetadata(className: string): Promise<{
        requiredPredicates: string[],
        requiredTriples: Array<{predicate: string, target?: string}>,
        properties: Map<string, { predicate: string, resolveLanguage?: string }>,
        relations: Map<string, { predicate: string, instanceFilter?: string, condition?: string }>
    } | null> {
        try {
            // Use getShacl() to retrieve the shape via SHACLShape.fromLinks()
            const shape = await this.getShacl(className);
            if (!shape) {
                console.warn(`No SHACL metadata found for ${className}`);
                return null;
            }

            const requiredPredicates: string[] = [];
            const requiredTriples: Array<{predicate: string, target?: string}> = [];
            const properties = new Map<string, { predicate: string, resolveLanguage?: string }>();
            const relations = new Map<string, { predicate: string, instanceFilter?: string }>();

            // Build property/relation maps and track writable predicates from shape properties
            const writablePredicates = new Set<string>();
            for (const prop of shape.properties) {
                if (!prop.path || !prop.name) continue;

                if (prop.writable) {
                    writablePredicates.add(prop.path);
                }

                const isRelation = prop.adder && prop.adder.length > 0;
                if (isRelation) {
                    relations.set(prop.name, { predicate: prop.path });
                } else {
                    properties.set(prop.name, {
                        predicate: prop.path,
                        resolveLanguage: prop.resolveLanguage
                    });
                }
            }

            // Extract required predicates/triples from constructor actions
            if (shape.constructor_actions) {
                for (const action of shape.constructor_actions) {
                    if (action.predicate) {
                        requiredPredicates.push(action.predicate);
                        const isWritableProperty = writablePredicates.has(action.predicate);
                        if (action.target && action.target !== 'value' && !isWritableProperty) {
                            requiredTriples.push({ predicate: action.predicate, target: action.target });
                        } else {
                            requiredTriples.push({ predicate: action.predicate });
                        }
                    }
                }
            }

            return { requiredPredicates, requiredTriples, properties, relations };
        } catch (e) {
            console.error(`Error getting SHACL metadata for ${className}:`, e);
            return null;
        }
    }

    /**
     * Generates a SPARQL query to find instances based on class metadata.
     */
    /**
     * Finds all instances matching subject class metadata by querying links.
     * Returns base URIs of matching instances.
     */
    private async findInstancesByMetadata(metadata: {
        requiredPredicates: string[],
        requiredTriples: Array<{predicate: string, target?: string}>,
        properties: Map<string, { predicate: string, resolveLanguage?: string }>,
        relations: Map<string, { predicate: string, instanceFilter?: string, condition?: string }>
    }): Promise<Array<{base: string}>> {
        if (metadata.requiredTriples.length === 0) {
            // No required triples - return all unique sources that have any link
            const allLinks = await this.get(new LinkQuery({}));
            const sources = new Set(allLinks.map(l => l.data.source));
            return Array.from(sources).map(s => ({ base: s }));
        }

        // Build a single SPARQL SELECT with all required triple patterns joined
        const patterns = metadata.requiredTriples.map((t, i) => {
            const target = t.target ? `<${t.target}>` : `?t${i}`;
            return `?base <${t.predicate}> ${target} .`;
        }).join('\n    ');

        const results = await this.querySparql(
            `SELECT DISTINCT ?base WHERE {\n    ${patterns}\n}`
        );
        if (!Array.isArray(results)) return [];
        return results.map((row: any) => ({ base: row.base }));
    }

    /** Returns all subject instances of the given subject class as proxy objects.
     *  @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, all subject classes
     * that match the given properties will be used.
     */
    async getAllSubjectInstances<T>(subjectClass: T): Promise<T[]> {
        let classes = []
        let isClassConstructor = typeof subjectClass === "function"
        if(typeof subjectClass === "string") {
            classes = [subjectClass]
        } else if (isClassConstructor) {
            // It's an Ad4mModel class constructor
            //@ts-ignore
            classes = [subjectClass.name]
        } else {
            classes = await this.subjectClassesByTemplate(subjectClass as object)
        }

        let instances = []
        for(let className of classes) {
            //console.log(`getAllSubjectInstances: Processing class ${className}`);
            // Query SDNA for metadata, then query SPARQL for instances
            const metadata = await this.getSubjectClassMetadata(className);
            //console.log(`getAllSubjectInstances: Got metadata for ${className}:`, metadata);
            if (metadata) {
                const results = await this.findInstancesByMetadata(metadata);
                
               // console.log(`getAllSubjectInstances: SPARQL returned ${results?.length || 0} results`);

                for (const result of results || []) {
                    //console.log(`getAllSubjectInstances: Creating subject for base ${result.base}`);
                    try {
                        let instance;
                        if (isClassConstructor) {
                            // Create an instance of the actual Ad4mModel class
                            //@ts-ignore
                            instance = new subjectClass(this, result.base);
                            // Load the instance data from links
                            await instance.get();
                        } else {
                            // Return plain data for string-based lookups
                            const data = await this.getSubjectData(className, result.base);
                            instance = { id: result.base, ...data };
                        }
                        instances.push(instance as unknown as T);
                        //console.log(`getAllSubjectInstances: Successfully created subject for ${result.base}`);
                    } catch (e) {
                        //console.warn(`Failed to create subject for ${result.base}:`, e);
                    }
                }
            } else {
                //console.warn(`getAllSubjectInstances: No metadata found for ${className}`);
            }
        }
        //console.log(`getAllSubjectInstances: Returning ${instances.length} instances`);
        return instances
    }


    /**
     * Find a subject class by matching an object's properties/relations against SHACL shapes.
     * Queries SHACL links client-side to find a class whose properties contain all required ones.
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

        // Single SPARQL query to get all class shapes with their properties and collections
        const results = await this.querySparql(`
            SELECT ?shape ?predicate ?target WHERE {
                { ?shape <rdf://type> <ad4m://SubjectClass> . BIND(<rdf://type> AS ?predicate) BIND(<ad4m://SubjectClass> AS ?target) }
                UNION
                { ?shape <sh://property> ?target . BIND(<sh://property> AS ?predicate) }
                UNION
                { ?shape <sh://collection> ?target . BIND(<sh://collection> AS ?predicate) }
            }
        `);
        if (!Array.isArray(results) || results.length === 0) return null;

        // Build a map of className -> { properties, relations }
        const classShapes: Map<string, { shapeUri: string, properties: string[], relations: string[] }> = new Map();

        // First pass: find all subject classes
        for (const r of results) {
            if (r.predicate === 'rdf://type' && r.target === 'ad4m://SubjectClass') {
                const source = r.shape;
                const sepIdx = source.indexOf('://');
                if (sepIdx < 0) continue;
                const className = source.substring(sepIdx + 3).split('/').pop();
                if (!className) continue;
                classShapes.set(className, { shapeUri: source, properties: [], relations: [] });
            }
        }

        // Second pass: collect properties and relations for each class
        for (const r of results) {
            if (r.predicate === 'sh://property' || r.predicate === 'sh://collection') {
                // Match shape source to class (e.g., "recipe://RecipeShape" -> "Recipe")
                for (const [className, shape] of classShapes) {
                    if (r.shape.endsWith(`${className}Shape`)) {
                        const dotIdx = r.target.lastIndexOf('.');
                        if (dotIdx < 0) continue;
                        const name = r.target.substring(dotIdx + 1);
                        if (r.predicate === 'sh://property') {
                            shape.properties.push(name);
                        } else {
                            shape.relations.push(name);
                        }
                    }
                }
            }
        }

        // Find a class that has all required properties and relations
        for (const [className, shape] of classShapes) {
            const hasAllProps = properties.every(p => shape.properties.includes(p));
            const hasAllRels = relations.every(c => shape.relations.includes(c));
            if (hasAllProps && hasAllRels) {
                return className;
            }
        }

        return null;
    }

    /** Returns all subject classes that match the given template object.
     * This function looks at the properties of the template object and
     * its setters and relations to create a Prolog query that finds
     * all subject classes that would be converted to a proxy object
     * with exactly the same properties and relations.
     *
     * Since there could be multiple subject classes that match the given
     * criteria, this function returns a list of class names.
     *
     * @param obj The template object
     */
    async subjectClassesByTemplate(obj: object): Promise<string[]> {
        // className lookup first: the @Model decorator sets a precise className on the prototype,
        // so this is always more specific than property-set matching.
        // Property matching alone has a superset-ambiguity problem: if DerivedModel adds
        // properties to BaseModel, then DerivedModel's shape is a superset of BaseModel's
        // property list, so findClassByProperties could wrongly return DerivedModel for a
        // BaseModel instance (causing the derived SDNA constructor to fire and inject extra
        // links into what should be a plain base instance).
        try {
            // @ts-ignore - className is added dynamically by decorators
            const className = obj.className || obj.constructor?.className || obj.constructor?.prototype?.className;
            if (className) {
                const existingClasses = await this.subjectClasses();
                if (existingClasses.includes(className)) {
                    return [className];
                }
            }
        } catch (e) {
            console.warn('subjectClassesByTemplate: className lookup failed:', e);
        }

        // Fall back to SHACL-based property matching for undecorated / dynamic objects
        try {
            const match = await this.findClassByProperties(obj);
            if (match) {
                return [match];
            }
        } catch (e) {
            console.warn('subjectClassesByTemplate: property matching failed:', e);
        }

        return [];
    }

    /** Takes a JS class (its constructor) and assumes that it was decorated by
     * the @subjectClass etc. decorators. It then tests if there is a subject class
     * already present in the perspective's SDNA that matches the given class.
     * If there is no such class, it gets the JS class's SDNA by calling its
     * static generateSDNA() function and adds it to the perspective's SDNA.
     */
    async ensureSDNASubjectClass(jsClass: any): Promise<void> {
        return this.ensureSubjectClass(jsClass);
    }

    /** Takes a JS class (its constructor) and ensures the SHACL subject class exists. */
    async ensureSubjectClass(jsClass: any): Promise<void> {
        // Get the class name from the JS class
        const className = jsClass.className || jsClass.prototype?.className || jsClass.name;
        
        // Skip if already registered for this perspective instance
        if (this.#ensuredSubjectClasses.has(className)) return;

        // Note: Duplicate checking is handled on the Rust side in add_sdna
        
        // Generate SHACL SDNA (Prolog-free)
        if (!jsClass.generateSHACL) {
            throw new Error(`Class ${jsClass.name} must have generateSHACL(). Use @Model decorator.`);
        }

        // Get SHACL shape (W3C standard + AD4M action definitions)
        const { shape } = jsClass.generateSHACL();

        // Serialize SHACL shape to JSON for Rust backend using SHACLShape.toJSON()
        const shaclJson = JSON.stringify(shape.toJSON());

        // Pass SHACL JSON to backend (Prolog-free)
        // Backend stores SHACL links directly
        await this.addSdna(className, '', 'subject_class', shaclJson);
        
        // Mark as registered for this perspective instance
        this.#ensuredSubjectClasses.add(className);
    }

    /** Clear the per-instance ensured subject class cache.
     * Call this after wiping a perspective's links so that
     * subsequent register() calls re-add SHACL definitions. */
    clearEnsuredSubjectClasses(): void {
        this.#ensuredSubjectClasses.clear();
    }

    /**
     * Returns a list of all class names that have been registered as SHACL
     * subject classes in this perspective (via `ensureSDNASubjectClass` or `addSdna`).
     *
     * @returns Array of class name strings (e.g. `["Channel", "Message", "Task"]`)
     */
    async listRegisteredClasses(): Promise<string[]> {
        const results = await this.querySparql(
            `SELECT DISTINCT ?class WHERE { ?class <rdf://type> <ad4m://SubjectClass> . }`
        );
        if (!Array.isArray(results)) return [];
        return results.map((row: any) => {
            const uri: string = row.class || '';
            // Extract class name from URI like "recipe://Recipe" or "flux://Channel"
            const hashIdx = uri.lastIndexOf('#');
            if (hashIdx >= 0) return uri.substring(hashIdx + 1);
            const slashIdx = uri.lastIndexOf('/');
            if (slashIdx >= 0) return uri.substring(slashIdx + 1);
            return uri;
        }).filter(Boolean);
    }

    /**
     * Returns the SHACL shape metadata for a registered class, including
     * property names, predicates, datatypes, and cardinality constraints.
     *
     * @param className - The name of the registered class
     * @returns Shape metadata or `null` if the class is not registered
     */
    async getClassShape(className: string): Promise<{
        className: string;
        shapeUri: string;
        properties: Array<{
            name: string;
            predicate: string;
            datatype?: string;
            required: boolean;
            collection: boolean;
            writable: boolean;
            options?: Array<{ value: string; label?: string }>;
        }>;
    } | null> {
        // Find the shape URI for this class
        const safeName = className.replace(/['"\\]/g, '');
        const shapeResults = await this.querySparql(
            `SELECT ?shapeUri ?targetClass WHERE {
                ?targetClass <rdf://type> <ad4m://SubjectClass> .
                ?targetClass <ad4m://shape> ?shapeUri .
                FILTER(STRENDS(STR(?targetClass), "/${safeName}") || STRENDS(STR(?targetClass), "#${safeName}"))
            } LIMIT 1`
        );
        if (!Array.isArray(shapeResults) || shapeResults.length === 0) return null;

        const shapeUri = shapeResults[0].shapeUri;
        const classUri = shapeResults[0].targetClass;

        // Query all properties for this shape, including sh:in
        const propResults = await this.querySparql(
            `SELECT ?propShape ?path ?datatype ?minCount ?maxCount ?writable ?propType ?shIn WHERE {
                <${shapeUri}> <sh://property> ?propShape .
                ?propShape <sh://path> ?path .
                ?propShape <rdf://type> ?propType .
                OPTIONAL { ?propShape <sh://datatype> ?datatype }
                OPTIONAL { ?propShape <sh://minCount> ?minCount }
                OPTIONAL { ?propShape <sh://maxCount> ?maxCount }
                OPTIONAL { ?propShape <ad4m://writable> ?writable }
                OPTIONAL { ?propShape <sh://in> ?shIn }
            }`
        );

        const properties = (Array.isArray(propResults) ? propResults : []).map((row: any) => {
            // Extract property name from shape URI like "recipe://Recipe.name"
            const propUri: string = row.propShape || '';
            const dotIdx = propUri.lastIndexOf('.');
            const name = dotIdx >= 0 ? propUri.substring(dotIdx + 1) : propUri;

            const minCount = parseInt(row.minCount || '0', 10);
            const isCollection = row.propType === 'ad4m://CollectionShape';
            const writable = row.writable === 'true' || row.writable === 'literal:true';

            // Parse sh:in values if present
            let options: Array<{ value: string; label?: string }> | undefined;
            if (row.shIn) {
                try {
                    let raw = row.shIn;
                    // Strip literal: prefix if present, and URI-decode for robustness
                    if (raw.startsWith('literal:string:')) {
                        raw = decodeURIComponent(raw.substring('literal:string:'.length));
                    }
                    options = JSON.parse(raw);
                } catch { /* ignore parse errors */ }
            }

            return {
                name,
                predicate: row.path || '',
                datatype: row.datatype || undefined,
                required: minCount > 0,
                collection: isCollection,
                writable,
                ...(options && options.length > 0 ? { options } : {}),
            };
        });

        return { className: safeName, shapeUri, properties };
    }

    /**
     * Detects which registered subject classes a given instance conforms to,
     * based on its stored triples matching the class shapes' required properties.
     *
     * @param baseExpression - The instance's base expression / subject URI
     * @returns Array of class names the instance matches
     */
    async getInstanceClasses(baseExpression: string): Promise<string[]> {
        const safeId = baseExpression.replace(/['"\\]/g, '');
        // Get all registered classes and their required conformance properties
        const results = await this.querySparql(
            `SELECT DISTINCT ?class WHERE {
                ?class <rdf://type> <ad4m://SubjectClass> .
                ?class <ad4m://shape> ?shape .
                ?shape <sh://property> ?propShape .
                ?propShape <sh://minCount> ?mc .
                FILTER(?mc >= 1)
                ?propShape <sh://path> ?path .
                <${safeId}> ?path ?_val .
            }`
        );
        if (!Array.isArray(results)) return [];
        return results.map((row: any) => {
            const uri: string = row.class || '';
            const hashIdx = uri.lastIndexOf('#');
            if (hashIdx >= 0) return uri.substring(hashIdx + 1);
            const slashIdx = uri.lastIndexOf('/');
            if (slashIdx >= 0) return uri.substring(slashIdx + 1);
            return uri;
        }).filter(Boolean);
    }

    /**
     * Returns all named options (sh:in values) for a registered class, grouped by property.
     *
     * @param className - The name of the registered class
     * @returns Record mapping property name → array of { value, label } options
     */
    async getNamedOptions(className: string): Promise<Record<string, Array<{ value: string; label?: string }>>> {
        const shape = await this.getClassShape(className);
        if (!shape) return {};
        const result: Record<string, Array<{ value: string; label?: string }>> = {};
        for (const prop of shape.properties) {
            if (prop.options && prop.options.length > 0) {
                result[prop.name] = prop.options;
            }
        }
        return result;
    }

    /**
     * Adds a named option (sh:in value) to a property of a registered class.
     * This appends to the existing sh:in list stored on the property shape.
     *
     * @param className - The registered class name
     * @param propertyName - The property to add the option to
     * @param value - The RDF value for the option
     * @param label - Optional human-readable label
     */
    async addNamedOption(className: string, propertyName: string, value: string, label?: string): Promise<void> {
        const shape = await this.getClassShape(className);
        if (!shape) throw new Error(`Class "${className}" not found`);

        const prop = shape.properties.find(p => p.name === propertyName);
        if (!prop) throw new Error(`Property "${propertyName}" not found on class "${className}"`);

        // Build the property shape URI
        const ns = extractNamespaceFromUri(shape.shapeUri);
        const propShapeId = `${ns}${className}.${propertyName}`;

        // Get existing options
        const existing = prop.options || [];
        // Don't add duplicates
        if (existing.some(o => o.value === value)) return;

        const updated = [...existing, { value, ...(label ? { label } : {}) }];

        // Remove old sh:in link if exists by querying for it
        const oldLinks = await this.get(new LinkQuery({ source: propShapeId, predicate: "sh://in" }));
        for (const oldLink of oldLinks) {
            await this.remove(oldLink);
        }

        // Add new sh:in link
        await this.add(new Link({
            source: propShapeId,
            predicate: "sh://in",
            target: `literal:string:${JSON.stringify(updated)}`
        }));
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

    /**
     * Creates a subscription for a Prolog query that updates in real-time.
     * 
     * This method:
     * 1. Creates the subscription on the Rust side
     * 2. Sets up the subscription callback
     * 3. Waits for the initial result to come through the subscription channel
     * 4. Returns a fully initialized QuerySubscriptionProxy
     * 
     * The returned subscription is guaranteed to be ready to receive updates,
     * as this method waits for the initialization process to complete.
     * 
     * The subscription will be automatically cleaned up on both frontend and backend
     * when dispose() is called. Make sure to call dispose() when you're done to
     * prevent memory leaks and ensure proper cleanup of resources.
     * 
     * @param query - Prolog query string
     * @returns Initialized QuerySubscriptionProxy instance
     * 
     * @example
     * ```typescript
     * // Subscribe to active todos
     * const subscription = await perspective.subscribeInfer(`
     *   instance(Todo, "Todo"),
     *   property_getter("Todo", Todo, "state", "active")
     * `);
     * 
     * // Subscription is already initialized here
     * console.log("Initial result:", subscription.result);
     * 
     * // Set up callback for future updates
     * subscription.onResult((todos) => {
     *   console.log("Active todos:", todos);
     * });
     * 
     * // Clean up subscription when done
     * subscription.dispose();
     * ```
     */
    async subscribeInfer(query: string): Promise<QuerySubscriptionProxy> {
        const subscriptionProxy = new QuerySubscriptionProxy(
            this.uuid,
            query,
            this.#client
        );

        // Start the subscription on the Rust side first to get the real subscription ID
        await subscriptionProxy.subscribe();

        // Wait for the initial result
        await subscriptionProxy.initialized;

        return subscriptionProxy;
    }

    /**
     * Creates a subscription for a SPARQL query that updates in real-time.
     * 
     * This method:
     * 1. Creates the subscription on the Rust side
     * 2. Sets up the subscription callback
     * 3. Waits for the initial result to come through the subscription channel
     * 4. Returns a fully initialized QuerySubscriptionProxy
     * 
     * The returned subscription is guaranteed to be ready to receive updates,
     * as this method waits for the initialization process to complete.
     * 
     * The subscription will be automatically cleaned up on both frontend and backend
     * when dispose() is called. Make sure to call dispose() when you're done to
     * prevent memory leaks and ensure proper cleanup of resources.
     * 
    /** Subscribe to a query with live updates via the SPARQL subscription endpoint.
     * @param query - Query string
     * @returns Initialized QuerySubscriptionProxy instance
     */
    async subscribeQuery(query: string): Promise<QuerySubscriptionProxy> {
        const subscriptionProxy = new QuerySubscriptionProxy(
            this.uuid,
            query,
            this.#client
        );

        // Start the subscription on the Rust side first to get the real subscription ID
        await subscriptionProxy.subscribe();

        // Wait for the initial result
        await subscriptionProxy.initialized;

        return subscriptionProxy;
    }

}