import { LinkCallback, PerspectiveClient, SyncStateChangeCallback } from "./PerspectiveClient";
import { Link, LinkExpression, LinkExpressionInput, LinkExpressionMutations, LinkMutations } from "../links/Links";
import { LinkQuery } from "./LinkQuery";
import { PerspectiveHandle, PerspectiveState } from './PerspectiveHandle'
import { Perspective } from "./Perspective";
import { Literal } from "../Literal";
import { Subject } from "../model/Subject";
import { ExpressionRendered } from "../expression/Expression";
import { collectionAdderToName, collectionRemoverToName, collectionSetterToName } from "../model/util";
import { NeighbourhoodProxy } from "../neighbourhood/NeighbourhoodProxy";
import { NeighbourhoodExpression } from "../neighbourhood/Neighbourhood";
import { AIClient } from "../ai/AIClient";
import { PERSPECTIVE_QUERY_SUBSCRIPTION } from "./PerspectiveResolver";
import { gql } from "@apollo/client/core";
import { AllInstancesResult } from "../model/Ad4mModel";
import { escapeSurrealString } from "../utils";

type QueryCallback = (result: AllInstancesResult) => void;

// Generic subscription interface that matches Apollo's Subscription
interface Unsubscribable {
    unsubscribe(): void;
}

/** Proxy object for a subscribed Prolog query that provides real-time updates
 * 
 * This class handles:
 * - Keeping the subscription alive by sending periodic keepalive signals
 * - Managing callbacks for result updates
 * - Subscribing to query updates via GraphQL subscriptions
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
    isSurrealDB: boolean = false;

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
            if (this.isSurrealDB) {
                initialResult = await this.#client.perspectiveSubscribeSurrealQuery(this.#uuid, this.#query);
            } else {
                initialResult = await this.#client.subscribeQuery(this.#uuid, this.#query);
            }
            this.#subscriptionId = initialResult.subscriptionId;

            // Process the initial result immediately for fast UX
            if (initialResult.result) {
                this.#latestResult = initialResult.result;
                this.#notifyCallbacks(initialResult.result);
            } else {
                console.warn('⚠️ No initial result returned from subscribeQuery!');
            }

            // Set up timeout for retry
            this.#initTimeoutId = setTimeout(() => {
                console.error('Subscription initialization timed out after 30 seconds. Resubscribing...');
                // Recursively retry subscription, catching any errors
                this.subscribe().catch(error => {
                    console.error('Error during subscription retry after timeout:', error);
                });
            }, 30000);
            
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

                    // Skip duplicate init messages
                    if (updateResult.isInit && this.#latestResult) return;

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
                if (this.isSurrealDB) {
                    await this.#client.perspectiveKeepAliveSurrealQuery(this.#uuid, this.#subscriptionId);
                } else {
                    await this.#client.keepAliveQuery(this.#uuid, this.#subscriptionId);
                }
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
     * 2. Unsubscribes from GraphQL subscription updates
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
            if (this.isSurrealDB) {
                this.#client.perspectiveDisposeSurrealQuerySubscription(this.#uuid, this.#subscriptionId)
                    .catch(e => console.error('Error disposing surreal query subscription:', e));
            } else {
                this.#client.disposeQuerySubscription(this.#uuid, this.#subscriptionId)
                    .catch(e => console.error('Error disposing query subscription:', e));
            }
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
    #perspectiveLinkAddedCallbacks: LinkCallback[]
    #perspectiveLinkRemovedCallbacks: LinkCallback[]
    #perspectiveLinkUpdatedCallbacks: LinkCallback[]
    #perspectiveSyncStateChangeCallbacks: SyncStateChangeCallback[]

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
     * - `collectionSetter`: Special command for setting collection properties
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
        return await this.#client.executeCommands(this.#handle.uuid, JSON.stringify(actions), expression, JSON.stringify(parameters), batchId)
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
     * Executes a SurrealQL query against the perspective's link cache.
     * This allows powerful SQL-like queries on the link data stored in SurrealDB.
     * 
     * **Security Note:** Only read-only queries (SELECT, RETURN, etc.) are permitted.
     * Mutating operations (DELETE, UPDATE, INSERT, CREATE, DROP, DEFINE, etc.) are
     * blocked for security reasons. Use the perspective's add/remove methods to modify links.
     * 
     * @param query - SurrealQL query string (read-only operations only)
     * @returns Query results as parsed JSON
     * 
     * @example
     * ```typescript
     * // Get all links
     * const links = await perspective.querySurrealDB('SELECT * FROM link');
     * 
     * // Filter links by predicate
     * const follows = await perspective.querySurrealDB(
     *   "SELECT * FROM link WHERE predicate = 'follows'"
     * );
     * 
     * // Complex aggregation query
     * const stats = await perspective.querySurrealDB(
     *   "SELECT predicate, count() as total FROM link GROUP BY predicate"
     * );
     * ```
     */
    async querySurrealDB(query: string): Promise<any> {
        return await this.#client.querySurrealDB(this.#handle.uuid, query)
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
        return await this.#client.addLink(this.#handle.uuid, link, status, batchId)
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
        return await this.#client.addLinks(this.#handle.uuid, links, status, batchId)
    }

    /**
     * Removes multiple links from the perspective.
     * 
     * @param links - Array of links to remove
     * @param batchId - Optional batch ID to group this operation with others
     * @returns Array of removed LinkExpressions
     */
    async removeLinks(links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        return await this.#client.removeLinks(this.#handle.uuid, links, batchId)
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
        return await this.#client.linkMutations(this.#handle.uuid, mutations, status)
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
        return await this.#client.updateLink(this.#handle.uuid, oldLink, newLink, batchId)
    }

    /**
     * Removes a link from the perspective.
     * 
     * @param link - The link to remove
     * @param batchId - Optional batch ID to group this operation with others
     */
    async remove(link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        return await this.#client.removeLink(this.#handle.uuid, link, batchId)
    }

    /** Creates a new batch for grouping operations */
    async createBatch(): Promise<string> {
        return await this.#client.createBatch(this.#handle.uuid)
    }

    /** Commits a batch of operations */
    async commitBatch(batchId: string): Promise<LinkExpressionMutations> {
        return await this.#client.commitBatch(this.#handle.uuid, batchId)

        
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

            this.#perspectiveLinkAddedCallbacks.splice(index, 1);
        } else if (type === 'link-removed') {
            const index = this.#perspectiveLinkRemovedCallbacks.indexOf(cb);

            this.#perspectiveLinkRemovedCallbacks.splice(index, 1);
        } else if (type === 'link-updated') {
            const index = this.#perspectiveLinkUpdatedCallbacks.indexOf(cb);

            this.#perspectiveLinkUpdatedCallbacks.splice(index, 1);
        }
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
        const allFlows = await this.infer("register_sdna_flow(X, _)")
        return allFlows.map(x => x.X)
    }

    /** Returns all Social DNA flows that can be started from the given expression */
    async availableFlows(exprAddr: string): Promise<string[]> {
        const availableFlows = await this.infer(`flowable("${exprAddr}", F), register_sdna_flow(X, F)`)
        return availableFlows.map(x => x.X)
    }

    /**  Starts the Social DNA flow @param flowName on the expression @param exprAddr */
    async startFlow(flowName: string, exprAddr: string) {
        let startAction = await this.infer(`start_action(Action, F), register_sdna_flow("${flowName}", F)`)
        // should always return one solution...
        startAction = eval(startAction[0].Action)
        await this.executeAction(startAction, exprAddr, undefined)
    }

    /** Returns all expressions in the given state of given Social DNA flow */
    async expressionsInFlowState(flowName: string, flowState: number): Promise<string[]> {
        let expressions = await this.infer(`register_sdna_flow("${flowName}", F), flow_state(X, ${flowState}, F)`)
        return expressions.map(r => r.X)
    }

    /** Returns the given expression's flow state with regard to given Social DNA flow */
    async flowState(flowName: string, exprAddr: string): Promise<number> {
        let state = await this.infer(`register_sdna_flow("${flowName}", F), flow_state("${exprAddr}", X, F)`)
        return state[0].X
    }

    /** Returns available action names, with regard to Social DNA flow and expression's flow state */
    async flowActions(flowName: string, exprAddr: string): Promise<string[]> {
        let actionNames = await this.infer(`register_sdna_flow("${flowName}", Flow), flow_state("${exprAddr}", State, Flow), action(State, Name, _, _)`)
        return actionNames.map(r => r.Name)
    }

    /** Runs given Social DNA flow action */
    async runFlowAction(flowName: string, exprAddr: string, actionName: string) {
        let action = await this.infer(`register_sdna_flow("${flowName}", Flow), flow_state("${exprAddr}", State, Flow), action(State, "${actionName}", _, Action)`)
        // should find only one
        action = eval(action[0].Action)
        await this.executeAction(action, exprAddr, undefined)
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

    /** Adds the given Social DNA code to the perspective's SDNA code */
    async addSdna(name: string, sdnaCode: string, sdnaType: "subject_class" | "flow" | "custom") {
        return this.#client.addSdna(this.#handle.uuid, name, sdnaCode, sdnaType)
    }

    /** Returns all the Subject classes defined in this perspectives SDNA */
    async subjectClasses(): Promise<string[]> {
        try {
            return (await this.infer("subject_class(X, _)")).map(x => x.X)
        }catch(e) {
            return []
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
            let query = this.buildQueryFromTemplate(subjectClass as object);
            await this.#client.createSubject(
                this.#handle.uuid, 
                JSON.stringify({
                    query,
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

        return this.getSubjectProxy(exprAddr, subjectClass) as Promise<B extends undefined ? T : string>;
    }

    async getSubjectData<T>(subjectClass: T, exprAddr: string): Promise<T> {
        if (typeof subjectClass === "string") {
            return JSON.parse(await this.#client.getSubjectData(this.#handle.uuid, JSON.stringify({className: subjectClass}), exprAddr))
        }
        let query = this.buildQueryFromTemplate(subjectClass as object)
        return JSON.parse(await this.#client.getSubjectData(this.#handle.uuid, JSON.stringify({query}), exprAddr))
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
        let result = await this.infer(`subject_class("${className}", C), destructor(C, Actions)`)
        if(!result.length) {
            throw "No destructor found for given subject class: " + className
        }

        let actions = result.map(x => eval(x.Actions))
        await this.executeAction(actions[0], exprAddr, undefined, batchId)
    }

    /** Checks if the given expression is a subject instance of the given subject class
     * @param expression The expression to be checked
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
    */
    async isSubjectInstance<T>(expression: string, subjectClass: T): Promise<boolean> {
        let className = await this.stringOrTemplateObjectToSubjectClassName(subjectClass)

        // Get metadata from SDNA using Prolog metaprogramming
        const metadata = await this.getSubjectClassMetadataFromSDNA(className);
        if (!metadata) {
            // Fallback to Prolog check if SDNA metadata isn't available
            // This handles cases where classes exist in Prolog but not in SDNA
            try {
                const escapedClassName = className.replace(/"/g, '\\"');
                const escapedExpression = expression.replace(/"/g, '\\"');
                const result = await this.infer(`subject_class("${escapedClassName}", C), instance(C, "${escapedExpression}")`);
                return result && result.length > 0;
            } catch (e) {
                console.warn(`Failed to check instance via Prolog for class ${className}:`, e);
                return false;
            }
        }

        // If no required triples, any expression with links is an instance
        if (metadata.requiredTriples.length === 0) {
            const escapedExpression = escapeSurrealString(expression);
            const checkQuery = `SELECT count() AS count FROM link WHERE in.uri = '${escapedExpression}'`;
            const result = await this.querySurrealDB(checkQuery);
            const count = result[0]?.count ?? 0;
            const countValue = typeof count === 'object' && count?.Int !== undefined ? count.Int : count;
            return countValue > 0;
        }

        // Check if the expression has all required triples (predicate + optional exact target)
        for (const triple of metadata.requiredTriples) {
            const escapedExpression = escapeSurrealString(expression);
            const escapedPredicate = escapeSurrealString(triple.predicate);
            let checkQuery: string;
            if (triple.target) {
                // Flag: must match both predicate AND exact target value
                const escapedTarget = escapeSurrealString(triple.target);
                checkQuery = `SELECT count() AS count FROM link WHERE in.uri = '${escapedExpression}' AND predicate = '${escapedPredicate}' AND out.uri = '${escapedTarget}'`;
            } else {
                // Property: just check predicate exists
                checkQuery = `SELECT count() AS count FROM link WHERE in.uri = '${escapedExpression}' AND predicate = '${escapedPredicate}'`;
            }
            const result = await this.querySurrealDB(checkQuery);

            if (!result || result.length === 0) {
                return false;
            }

            const count = result[0]?.count ?? 0;
            // Handle potential object response like {Int: 0}
            const countValue = typeof count === 'object' && count?.Int !== undefined ? count.Int : count;

            if (countValue === 0) {
                return false;
            }
        }

        return true;
    }


    /** For an existing subject instance (existing in the perspective's links)
     * this function returns a proxy object that can be used to access the subject's
     * properties and methods.
     *
     * @param base URI of the subject's root expression
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
     */
    async getSubjectProxy<T>(base: string, subjectClass: T): Promise<T> {
        if(!await this.isSubjectInstance(base, subjectClass)) {
            throw `Expression ${base} is not a subject instance of given class: ${JSON.stringify(subjectClass)}`
        }
        let className = await this.stringOrTemplateObjectToSubjectClassName(subjectClass)
        let subject = new Subject(this, base, className)
        await subject.init()
        return subject as unknown as T
    }

    /**
     * Extracts subject class metadata from SDNA by parsing the Prolog text.
     * Parses the instance rule to extract required predicates.
     * Returns required predicates that define what makes something an instance,
     * plus a map of property/collection names to their predicates.
     */
    async getSubjectClassMetadataFromSDNA(className: string): Promise<{
        requiredPredicates: string[],
        requiredTriples: Array<{predicate: string, target?: string}>,
        properties: Map<string, { predicate: string, resolveLanguage?: string }>,
        collections: Map<string, { predicate: string, instanceFilter?: string }>
    } | null> {
        try {
            // Get SDNA code from perspective - it's stored as a link
            // Use canonical Literal.from() to construct the source URL
            const sdnaLinks = await this.get(new LinkQuery({
                source: Literal.from(className).toUrl(),
                predicate: "ad4m://sdna"
            }));

            //console.log(`getSubjectClassMetadataFromSDNA: sdnaLinks for ${className}:`, sdnaLinks);

            if (!sdnaLinks || sdnaLinks.length === 0) {
                console.warn(`No SDNA found for class ${className}`);
                return null;
            }

            if (!sdnaLinks[0].data.target) {
                console.error(`SDNA link for ${className} has no target:`, sdnaLinks[0]);
                return null;
            }

            // Extract SDNA code from the literal
            const sdnaCode = Literal.fromUrl(sdnaLinks[0].data.target).get();
            //console.log("sdnaCode for", className, ":", sdnaCode.substring(0, 200));

            // Store required triples as {predicate, target?}
            // target is only set for flags (exact matches), otherwise undefined
            const requiredTriples: Array<{predicate: string, target?: string}> = [];

            // Parse the instance rule from the SDNA code
            // Format: instance(c, Base) :- triple(Base, "pred1", _), triple(Base, "pred2", "exact_value").
            // Use a more robust pattern that handles complex rule bodies
            // Match from "instance(" to the closing "." using non-greedy matching
            const instanceRulePattern = /instance\([^)]+\)\s*:-\s*([^.]+)\./g;
            let instanceRuleMatch;
            let foundInstanceRule = false;

            while ((instanceRuleMatch = instanceRulePattern.exec(sdnaCode)) !== null) {
                foundInstanceRule = true;
                const ruleBody = instanceRuleMatch[1];

                // Extract all triple(Base, "predicate", Target) patterns
                // Match both: triple(Base, "pred", _) and triple(Base, "pred", "value")
                const tripleRegex = /triple\([^,]+,\s*"([^"]+)",\s*(?:"([^"]+)"|_)\)/g;
                let match;

                while ((match = tripleRegex.exec(ruleBody)) !== null) {
                    const predicate = match[1];
                    const target = match[2]; // undefined if matched "_"
                    requiredTriples.push({ predicate, target });
                }
            }

            if (!foundInstanceRule) {
                console.warn(`No instance rule found in SDNA for ${className}`);
            }

            // For backward compatibility, also maintain requiredPredicates array
            const requiredPredicates = requiredTriples.map(t => t.predicate);

            // Extract property metadata
            const properties = new Map<string, { predicate: string, resolveLanguage?: string }>();
            const propertyResults = await this.infer(`subject_class("${className}", C), property(C, P)`);
            //console.log("propertyResults", propertyResults);

            if (propertyResults) {
                for (const result of propertyResults) {
                    const propName = result.P;
                    let predicate: string | null = null;

                    // Try to extract predicate from property_setter first
                    const setterResults = await this.infer(`subject_class("${className}", C), property_setter(C, "${propName}", Setter)`);
                    if (setterResults && setterResults.length > 0) {
                        const setterString = setterResults[0].Setter;
                        const predicateMatch = setterString.match(/predicate:\s*"([^"]+)"|predicate:\s*([^,}\]]+)/);
                        if (predicateMatch) {
                            predicate = predicateMatch[1] || predicateMatch[2];
                        }
                    }

                    // If no setter, try to extract from SDNA property_getter Prolog code
                    if (!predicate) {
                        // Parse the SDNA code for property_getter definition
                        // Escape propName to prevent regex injection and ReDoS attacks
                        const escapedPropName = this.escapeRegExp(propName);
                        const getterMatch = sdnaCode.match(new RegExp(`property_getter\\([^,]+,\\s*[^,]+,\\s*"${escapedPropName}"[^)]*\\)\\s*:-\\s*triple\\([^,]+,\\s*"([^"]+)"`));
                        if (getterMatch) {
                            predicate = getterMatch[1];
                        }
                    }

                    if (predicate) {
                        // Check if property has resolveLanguage
                        const resolveResults = await this.infer(`subject_class("${className}", C), property_resolve_language(C, "${propName}", Lang)`);
                        const resolveLanguage = resolveResults && resolveResults.length > 0 ? resolveResults[0].Lang : undefined;

                        properties.set(propName, { predicate, resolveLanguage });
                    }
                }
            }
            //console.log("properties", properties);

            // Extract collection metadata
            const collections = new Map<string, { predicate: string, instanceFilter?: string }>();
            const collectionResults = await this.infer(`subject_class("${className}", C), collection(C, Coll)`);
            //console.log("collectionResults", collectionResults);
            if (collectionResults) {
                for (const result of collectionResults) {
                    const collName = result.Coll;
                    let predicate: string | null = null;
                    let instanceFilter: string | undefined = undefined;

                    // Try to extract predicate from collection_adder first
                    const adderResults = await this.infer(`subject_class("${className}", C), collection_adder(C, "${collName}", Adder)`);
                    if (adderResults && adderResults.length > 0) {
                        const adderString = adderResults[0].Adder;
                        const predicateMatch = adderString.match(/predicate:\s*"([^"]+)"|predicate:\s*([^,}\]]+)/);
                        if (predicateMatch) {
                            predicate = predicateMatch[1] || predicateMatch[2];
                        }
                    }

                    // Parse collection_getter from SDNA to extract predicate and instanceFilter
                    // Format 1 (findall): collection_getter(c, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
                    // Format 2 (setof): collection_getter(c, Base, "messages", List) :- setof(Target, (triple(Base, "flux://entry_type", Target), ...), List).
                    // Use a line-based match to avoid capturing multiple collections
                    // Escape collName to prevent regex injection and ReDoS attacks
                    const escapedCollName = this.escapeRegExp(collName);
                    const getterLinePattern = new RegExp(`collection_getter\\([^,]+,\\s*[^,]+,\\s*"${escapedCollName}"[^)]*\\)\\s*:-[^.]+\\.`);
                    const getterLineMatch = sdnaCode.match(getterLinePattern);

                    if (getterLineMatch) {
                        const getterLine = getterLineMatch[0];
                        // Extract the body between setof/findall and the final ).
                        // Pattern: findall(Var, Body, List) or setof(Var, (Body), List)
                        const bodyPattern = /(?:setof|findall)\([^,]+,\s*(.+),\s*\w+\)\./;
                        const bodyMatch = getterLine.match(bodyPattern);

                        if (bodyMatch) {
                            let getterBody = bodyMatch[1];
                            // Remove outer parentheses if present (setof case)
                            if (getterBody.startsWith('(') && getterBody.endsWith(')')) {
                                getterBody = getterBody.substring(1, getterBody.length - 1);
                            }

                            // Extract predicate from triple(Base, "predicate", Target)
                            if (!predicate) {
                                const tripleMatch = getterBody.match(/triple\([^,]+,\s*"([^"]+)"/);
                                if (tripleMatch) {
                                    predicate = tripleMatch[1];
                                }
                            }

                            // Check for instance filter: subject_class("ClassName", OtherClass)
                            const instanceMatch = getterBody.match(/subject_class\("([^"]+)"/);
                            if (instanceMatch) {
                                instanceFilter = instanceMatch[1];
                            }
                        }
                    }

                    if (predicate) {
                        collections.set(collName, { predicate, instanceFilter });
                    }
                }
            }
            //console.log("collections", collections);
            return { requiredPredicates, requiredTriples, properties, collections };
        } catch (e) {
            console.error(`Error getting metadata for ${className}:`, e);
            return null;
        }
    }

    /**
     * Generates a SurrealDB query to find instances based on class metadata.
     */
    private generateSurrealInstanceQuery(metadata: {
        requiredPredicates: string[],
        requiredTriples: Array<{predicate: string, target?: string}>,
        properties: Map<string, { predicate: string, resolveLanguage?: string }>,
        collections: Map<string, { predicate: string }>
    }): string {
        if (metadata.requiredTriples.length === 0) {
            // No required triples - any node with links is an instance
            return `SELECT DISTINCT uri AS base FROM node WHERE count(->link) > 0`;
        }

        // Generate WHERE conditions for each required triple (predicate + optional exact target)
        const whereConditions = metadata.requiredTriples.map(triple => {
            const escapedPredicate = escapeSurrealString(triple.predicate);
            if (triple.target) {
                // Flag: must match both predicate AND exact target value
                const escapedTarget = escapeSurrealString(triple.target);
                return `count(->link[WHERE predicate = '${escapedPredicate}' AND out.uri = '${escapedTarget}']) > 0`;
            } else {
                // Property: just check predicate exists
                return `count(->link[WHERE predicate = '${escapedPredicate}']) > 0`;
            }
        }).join(' AND ');

        return `SELECT uri AS base FROM node WHERE ${whereConditions}`;
    }

    /**
     * Gets a property value using SurrealDB when Prolog fails.
     * This is used as a fallback in SdnaOnly mode where link data isn't in Prolog.
     */
    async getPropertyValueViaSurreal(baseExpression: string, className: string, propertyName: string): Promise<any> {
        const metadata = await this.getSubjectClassMetadataFromSDNA(className);
        if (!metadata) {
            return undefined;
        }

        const propMeta = metadata.properties.get(propertyName);
        if (!propMeta) {
            return undefined;
        }

        const escapedBaseExpression = escapeSurrealString(baseExpression);
        const escapedPredicate = escapeSurrealString(propMeta.predicate);
        const query = `SELECT out.uri AS value FROM link WHERE in.uri = '${escapedBaseExpression}' AND predicate = '${escapedPredicate}' LIMIT 1`;
        const result = await this.querySurrealDB(query);

        if (!result || result.length === 0) {
            return undefined;
        }

        const value = result[0].value;

        // Handle expression resolution if needed
        if (propMeta.resolveLanguage && value) {
            try {
                const expression = await this.getExpression(value);
                try {
                    return JSON.parse(expression.data);
                } catch (e) {
                    return expression.data;
                }
            } catch (err) {
                return value;
            }
        }

        return value;
    }

    /**
     * Gets collection values using SurrealDB when Prolog fails.
     * This is used as a fallback in SdnaOnly mode where link data isn't in Prolog.
     */
    async getCollectionValuesViaSurreal(baseExpression: string, className: string, collectionName: string): Promise<any[]> {
        const metadata = await this.getSubjectClassMetadataFromSDNA(className);
        if (!metadata) {
            return [];
        }

        const collMeta = metadata.collections.get(collectionName);
        if (!collMeta) {
            return [];
        }

        const escapedBaseExpression = escapeSurrealString(baseExpression);
        const escapedPredicate = escapeSurrealString(collMeta.predicate);
        const query = `SELECT out.uri AS value, timestamp FROM link WHERE in.uri = '${escapedBaseExpression}' AND predicate = '${escapedPredicate}' ORDER BY timestamp ASC`;
        const result = await this.querySurrealDB(query);

        if (!result || result.length === 0) {
            return [];
        }

        let values = result.map(r => r.value).filter(v => v !== "" && v !== '');

        // Apply instance filter if present - batch-check all values at once
        if (collMeta.instanceFilter) {
            try {
                const filterMetadata = await this.getSubjectClassMetadataFromSDNA(collMeta.instanceFilter);
                if (!filterMetadata) {
                    // Fallback to sequential checks if metadata isn't available
                    return this.filterInstancesSequential(values, collMeta.instanceFilter);
                }

                return await this.batchCheckSubjectInstances(values, filterMetadata);
            } catch (err) {
                // Fallback to sequential checks on error
                return this.filterInstancesSequential(values, collMeta.instanceFilter);
            }
        }

        return values;
    }

    /**
     * Batch-checks multiple expressions against subject class metadata using a single or limited SurrealDB queries.
     * This avoids N+1 query problems by checking all values at once.
     */
    async batchCheckSubjectInstances(
        expressions: string[],
        metadata: {
            requiredPredicates: string[],
            requiredTriples: Array<{predicate: string, target?: string}>,
            properties: Map<string, { predicate: string, resolveLanguage?: string }>,
            collections: Map<string, { predicate: string, instanceFilter?: string }>
        }
    ): Promise<string[]> {
        if (expressions.length === 0) {
            return [];
        }

        // If no required triples, check which expressions have any links
        if (metadata.requiredTriples.length === 0) {
            const escapedExpressions = expressions.map(e => `'${escapeSurrealString(e)}'`).join(', ');
            const checkQuery = `SELECT in.uri AS uri FROM link WHERE in.uri IN [${escapedExpressions}] GROUP BY in.uri HAVING count() > 0`;
            const result = await this.querySurrealDB(checkQuery);
            return result.map(r => r.uri);
        }

        // For each required triple, build a query that finds matching expressions
        const validExpressionSets: Set<string>[] = [];
        
        for (const triple of metadata.requiredTriples) {
            const escapedExpressions = expressions.map(e => `'${escapeSurrealString(e)}'`).join(', ');
            const escapedPredicate = escapeSurrealString(triple.predicate);
            
            let checkQuery: string;
            if (triple.target) {
                // Flag: must match both predicate AND exact target value
                const escapedTarget = escapeSurrealString(triple.target);
                // Note: Removed GROUP BY because it was causing SurrealDB to only return one result
                checkQuery = `SELECT in.uri AS uri FROM link WHERE in.uri IN [${escapedExpressions}] AND predicate = '${escapedPredicate}' AND out.uri = '${escapedTarget}'`;
            } else {
                // Property: just check predicate exists
                // Note: Removed GROUP BY because it was causing SurrealDB to only return one result
                checkQuery = `SELECT in.uri AS uri FROM link WHERE in.uri IN [${escapedExpressions}] AND predicate = '${escapedPredicate}'`;
            }
            
            const result = await this.querySurrealDB(checkQuery);
            validExpressionSets.push(new Set(result.map(r => r.uri)));
        }

        // Find intersection: expressions that passed ALL required triple checks
        if (validExpressionSets.length === 0) {
            return expressions;
        }

        const firstSet = validExpressionSets[0];
        const validExpressions = expressions.filter(expr => {
            return validExpressionSets.every(set => set.has(expr));
        });

        return validExpressions;
    }

    /**
     * Fallback sequential instance checking when batch checking isn't available.
     */
    private async filterInstancesSequential(values: string[], instanceFilter: string): Promise<string[]> {
        const filteredValues = [];
        for (const value of values) {
            try {
                const isInstance = await this.isSubjectInstance(value, instanceFilter);
                if (isInstance) {
                    filteredValues.push(value);
                }
            } catch (err) {
                // Skip values that fail instance check
                continue;
            }
        }
        return filteredValues;
    }

    /** Returns all subject instances of the given subject class as proxy objects.
     *  @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, all subject classes
     * that match the given properties will be used.
     */
    async getAllSubjectInstances<T>(subjectClass: T): Promise<T[]> {
        let classes = []
        if(typeof subjectClass === "string") {
            classes = [subjectClass]
        } else {
            classes = await this.subjectClassesByTemplate(subjectClass as object)
        }

        let instances = []
        for(let className of classes) {
            //console.log(`getAllSubjectInstances: Processing class ${className}`);
            // Query SDNA for metadata, then query SurrealDB for instances
            const metadata = await this.getSubjectClassMetadataFromSDNA(className);
            //console.log(`getAllSubjectInstances: Got metadata for ${className}:`, metadata);
            if (metadata) {
                const surrealQuery = this.generateSurrealInstanceQuery(metadata);
                const results = await this.querySurrealDB(surrealQuery);
               // console.log(`getAllSubjectInstances: SurrealDB returned ${results?.length || 0} results`);

                for (const result of results || []) {
                    //console.log(`getAllSubjectInstances: Creating subject for base ${result.base}`);
                    try {
                        let subject = new Subject(this, result.base, className);
                        await subject.init();
                        instances.push(subject as unknown as T);
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

    /** Returns all subject proxies of the given subject class as proxy objects.
     *  @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, all subject classes
     * that match the given properties will be used.
     */
    async getAllSubjectProxies<T>(subjectClass: T): Promise<T[]> {
        let classes = []
        if(typeof subjectClass === "string") {
            classes = [subjectClass]
        } else {
            classes = await this.subjectClassesByTemplate(subjectClass as object)
        }

        let instances = []
        for(let className of classes) {
            // Query SDNA for metadata, then query SurrealDB for instances
            const metadata = await this.getSubjectClassMetadataFromSDNA(className);
            if (metadata) {
                const surrealQuery = this.generateSurrealInstanceQuery(metadata);
                const results = await this.querySurrealDB(surrealQuery);

                for (const result of results || []) {
                    try {
                        let subject = new Subject(this, result.base, className);
                        await subject.init();
                        instances.push(subject as unknown as T);
                    } catch (e) {
                        // Skip subjects that fail to initialize
                    }
                }
            }
        }
        return instances
    }


    private buildQueryFromTemplate(obj: object): string {
        let result
        // We need to avoid strict mode for the following intropsective code
        (function(obj) {
            // Collect all string properties of the object in a list
            let properties = []

            // Collect all collections of the object in a list
            let collections = []

            // Collect all string properties of the object in a list
            if(Object.getPrototypeOf(obj).__properties) {
                Object.keys(Object.getPrototypeOf(obj).__properties).forEach(p => properties.push(p))
            } else {
                properties.push(...Object.keys(obj).filter(key => !Array.isArray(obj[key])))
            }

            // Collect all collections of the object in a list
            if (Object.getPrototypeOf(obj).__collections) {
                Object.keys(Object.getPrototypeOf(obj).__collections).filter(key => key !== 'isSubjectInstance').forEach(c => {
                    if (!collections.includes(c)) {
                        collections.push(c);
                    }
                });
            } else {
                collections.push(...Object.keys(obj).filter(key => Array.isArray(obj[key])).filter(key => key !== 'isSubjectInstance'))
            }

            // Collect all set functions of the object in a list
            let setFunctions = Object.getOwnPropertyNames(obj).filter(key => (typeof obj[key] === "function") && key.startsWith("set") && !key.startsWith("setCollection"))
            // Add all set functions of the object's prototype to that list
            setFunctions = setFunctions.concat(Object.getOwnPropertyNames(Object.getPrototypeOf(obj)).filter(key => {
                const descriptor = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(obj), key);
                return descriptor && typeof descriptor.value === "function" && key.startsWith("set") && !key.startsWith("setCollection");
            }));

            // Collect all add functions of the object in a list
            let addFunctions = Object.getOwnPropertyNames(obj).filter(key => (Object.prototype.hasOwnProperty.call(obj, key) && typeof obj[key] === "function") && key.startsWith("add"))
            // Add all add functions of the object's prototype to that list
            addFunctions = addFunctions.concat(Object.getOwnPropertyNames(Object.getPrototypeOf(obj)).filter(key => {
                const descriptor = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(obj), key);
                return descriptor && typeof descriptor.value === "function" && key.startsWith("add");
            }));

            // Collect all remove functions of the object in a list
            let removeFunctions = Object.getOwnPropertyNames(obj).filter(key => (Object.prototype.hasOwnProperty.call(obj, key) && typeof obj[key] === "function") && key.startsWith("remove"))
            // Add all remove functions of the object's prototype to that list
            removeFunctions = removeFunctions.concat(Object.getOwnPropertyNames(Object.getPrototypeOf(obj)).filter(key => {
                const descriptor = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(obj), key);
                return descriptor && typeof descriptor.value === "function" && key.startsWith("remove");
            }));

            // Collect all add functions of the object in a list
            let setCollectionFunctions = Object.getOwnPropertyNames(obj).filter(key => (Object.prototype.hasOwnProperty.call(obj, key) && typeof obj[key] === "function") && key.startsWith("setCollection"))
            // Add all add functions of the object's prototype to that list
            setCollectionFunctions = setCollectionFunctions.concat(Object.getOwnPropertyNames(Object.getPrototypeOf(obj)).filter(key => {
                const descriptor = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(obj), key);
                return descriptor && typeof descriptor.value === "function" && key.startsWith("setCollection");
            }));
            // Construct query to find all subject classes that have the given properties and collections
            let query = `subject_class(Class, C)`

            for(let property of properties) {
                query += `, property(C, "${property}")`
            }
            for(let collection of collections) {
                query += `, collection(C, "${collection}")`
            }

            for(let setFunction of setFunctions) {
                // e.g.  "setState" -> "state"
                let property = setFunction.substring(3)
                property = property.charAt(0).toLowerCase() + property.slice(1)
                query += `, property_setter(C, "${property}", _)`
            }
            for(let addFunction of addFunctions) {
                query += `, collection_adder(C, "${collectionAdderToName(addFunction)}", _)`
            }

            for(let removeFunction of removeFunctions) {
                query += `, collection_remover(C, "${collectionRemoverToName(removeFunction)}", _)`
            }

            for(let setCollectionFunction of setCollectionFunctions) {
                query += `, collection_setter(C, "${collectionSetterToName(setCollectionFunction)}", _)`
            }

            query += "."
            result = query
        }(obj))
        return result
    }

    /** Returns all subject classes that match the given template object.
     * This function looks at the properties of the template object and
     * its setters and collections to create a Prolog query that finds
     * all subject classes that would be converted to a proxy object
     * with exactly the same properties and collections.
     *
     * Since there could be multiple subject classes that match the given
     * criteria, this function returns a list of class names.
     *
     * @param obj The template object
     */
    async subjectClassesByTemplate(obj: object): Promise<string[]> {
        const query = this.buildQueryFromTemplate(obj);
        let result = await this.infer(query)
        if(!result) {
            return []
        } else {
            return result.map(x => x.Class)
        }
    }

    /** Takes a JS class (its constructor) and assumes that it was decorated by
     * the @subjectClass etc. decorators. It then tests if there is a subject class
     * already present in the perspective's SDNA that matches the given class.
     * If there is no such class, it gets the JS class's SDNA by calling its
     * static generateSDNA() function and adds it to the perspective's SDNA.
     */
    async ensureSDNASubjectClass(jsClass: any): Promise<void> {
        const subjectClass = await this.subjectClassesByTemplate(new jsClass)
        if(subjectClass.length > 0) {
            return
        }

        const { name, sdna } = jsClass.generateSDNA();

        await this.addSdna(name, sdna, 'subject_class');
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
     * Creates a subscription for a SurrealQL query that updates in real-time.
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
     * @param query - SurrealQL query string
     * @returns Initialized QuerySubscriptionProxy instance
     */
    async subscribeSurrealDB(query: string): Promise<QuerySubscriptionProxy> {
        const subscriptionProxy = new QuerySubscriptionProxy(
            this.uuid,
            query,
            this.#client
        );
        subscriptionProxy.isSurrealDB = true;

        // Start the subscription on the Rust side first to get the real subscription ID
        await subscriptionProxy.subscribe();

        // Wait for the initial result
        await subscriptionProxy.initialized;

        return subscriptionProxy;
    }

}