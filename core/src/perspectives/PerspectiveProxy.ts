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
 * 
 * The subscription will remain active as long as keepalive signals are sent.
 * Make sure to call dispose() when you're done with the subscription to clean up
 * resources and stop keepalive signals.
 * 
 * Example usage:
 * ```typescript
 * const subscription = await perspective.subscribeInfer("my_query(X)");
 * console.log("Initial result:", subscription.result);
 * 
 * // Set up callback for updates
 * const removeCallback = subscription.onResult(result => {
 *     console.log("New result:", result);
 * });
 * 
 * // Later: clean up
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
    #latestResult: AllInstancesResult;
    #disposed: boolean = false;

    /** Creates a new query subscription
     * @param uuid - The UUID of the perspective
     * @param subscriptionId - The ID returned by the subscription mutation
     * @param initialResult - The initial query result
     * @param client - The PerspectiveClient instance to use for communication
     */
    constructor(uuid: string, subscriptionId: string, initialResult: AllInstancesResult, client: PerspectiveClient) {
        this.#uuid = uuid;
        this.#subscriptionId = subscriptionId;
        this.#client = client;
        this.#callbacks = new Set();
        this.#latestResult = initialResult;

        // Call all callbacks with initial result
        this.#notifyCallbacks(initialResult);

        // Subscribe to query updates
        this.#unsubscribe = this.#client.subscribeToQueryUpdates(
            this.#subscriptionId,
            (result) => {
                this.#latestResult = result;
                this.#notifyCallbacks(result);
            }
        );

        // Start keepalive loop using platform-agnostic setTimeout
        const keepaliveLoop = async () => {
            if (this.#disposed) return;
            
            try {
                await this.#client.keepAliveQuery(this.#uuid, this.#subscriptionId);
            } catch (e) {
                console.error('Error in keepalive:', e);
            }

            // Schedule next keepalive if not disposed
            if (!this.#disposed) {
                this.#keepaliveTimer = setTimeout(keepaliveLoop, 30000) as unknown as number;
            }
        };

        // Start the first keepalive loop
        this.#keepaliveTimer = setTimeout(keepaliveLoop, 30000) as unknown as number;
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
    }
}

type PerspectiveListenerTypes = "link-added" | "link-removed" | "link-updated"

export type LinkStatus = "shared" | "local"
interface Parameter {
    name: string
    value: string
}

/** Perspective UI proxy object
 *
 * Convenience object for UIs to interact with a perspective.
 * It is created by some of the methods in the PerspectiveClient class and includes
 * a reference to the PerspectiveClient object that created it.
 */
export class PerspectiveProxy {
    /** Unique ID of the perspective */
    uuid: string;

    /** Given name of the perspective */
    name: string;

    /** If the perspective is shared as a Neighbourhood, this is the Neighbourhood URL */
    sharedUrl: string|null;

    /** If the perspective is shared as a Neighbourhood, this is the Neighbourhood Expression */
    neighbourhood: NeighbourhoodExpression|null;

    /** Returns the state of the perspective **/
    state: PerspectiveState|null;

    #handle: PerspectiveHandle
    #client: PerspectiveClient
    #perspectiveLinkAddedCallbacks: LinkCallback[]
    #perspectiveLinkRemovedCallbacks: LinkCallback[]
    #perspectiveLinkUpdatedCallbacks: LinkCallback[]
    #perspectiveSyncStateChangeCallbacks: SyncStateChangeCallback[]

    constructor(handle: PerspectiveHandle, ad4m: PerspectiveClient) {
        this.#perspectiveLinkAddedCallbacks = []
        this.#perspectiveLinkRemovedCallbacks = []
        this.#perspectiveLinkUpdatedCallbacks = []
        this.#perspectiveSyncStateChangeCallbacks = []
        this.#handle = handle
        this.#client = ad4m
        this.uuid = this.#handle.uuid;
        this.name = this.#handle.name;
        this.sharedUrl = this.#handle.sharedUrl;
        this.neighbourhood = this.#handle.neighbourhood;
        this.state = this.#handle.state;
        this.#client.addPerspectiveLinkAddedListener(this.#handle.uuid, this.#perspectiveLinkAddedCallbacks)
        this.#client.addPerspectiveLinkRemovedListener(this.#handle.uuid, this.#perspectiveLinkRemovedCallbacks)
        this.#client.addPerspectiveLinkUpdatedListener(this.#handle.uuid, this.#perspectiveLinkUpdatedCallbacks)
        this.#client.addPerspectiveSyncStateChangeListener(this.#handle.uuid, this.#perspectiveSyncStateChangeCallbacks)
    }

    async executeAction(actions, expression, parameters: Parameter[]) {
        return await this.#client.executeCommands(this.#handle.uuid, JSON.stringify(actions), expression, JSON.stringify(parameters))
    }

    /** Returns all the links of this perspective that matches the LinkQuery */
    async get(query: LinkQuery): Promise<LinkExpression[]> {
        return await this.#client.queryLinks(this.#handle.uuid, query)
    }

    /** Runs a Prolog query on the perspective's Prolog engine */
    async infer(query: string): Promise<any> {
        return await this.#client.queryProlog(this.#handle.uuid, query)
    }

    /** Adds a link to this perspective */
    async add(link: Link, status: LinkStatus = 'shared'): Promise<LinkExpression> {
        return await this.#client.addLink(this.#handle.uuid, link, status)
    }

    /** Adds multiple links to this perspective **/
    async addLinks(links: Link[], status: LinkStatus = 'shared'): Promise<LinkExpression[]> {
        return await this.#client.addLinks(this.#handle.uuid, links, status)
    }

    /** Removes multiple links from this perspective **/
    async removeLinks(links: LinkExpressionInput[]): Promise<LinkExpression[]> {
        return await this.#client.removeLinks(this.#handle.uuid, links)
    }

    /** Adds and removes multiple links from this perspective **/
    async linkMutations(mutations: LinkMutations, status: LinkStatus = 'shared'): Promise<LinkExpressionMutations> {
        return await this.#client.linkMutations(this.#handle.uuid, mutations, status)
    }

    /** Adds a linkExpression to this perspective */
    async addLinkExpression(link: LinkExpression, status: LinkStatus = 'shared'): Promise<LinkExpression> {
        return await this.#client.addLinkExpression(this.#handle.uuid, link, status)
    }

    async update(oldLink: LinkExpressionInput, newLink: Link) {
        return await this.#client.updateLink(this.#handle.uuid, oldLink, newLink)
    }

    async remove(link: LinkExpressionInput) {
        return await this.#client.removeLink(this.#handle.uuid, link)
    }

    async getExpression(expressionURI: string): Promise<ExpressionRendered> {
        return await this.#client.getExpression(expressionURI)
    }

    async createExpression(content: any, languageAddress: string): Promise<string> {
        return await this.#client.createExpression(content, languageAddress)
    }

    /** Adds a link listener
     * @param type Can be 'link-added' or 'link-removed'
     * @param cb Callback function that is called when a link is added to the perspective
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

    /** Adds a sync state listener
     * @param cb Callback function that is called when the sync state of the perspective changes
     * @returns A function that can be called to remove the listener
     */
    async addSyncStateChangeListener(cb: SyncStateChangeCallback) {
        this.#perspectiveSyncStateChangeCallbacks.push(cb)
    }

    /** Removes a previously added link listener
     * @param type Can be 'link-added' or 'link-removed'
     * @param cb Callback function that is called when a link is added to the perspective
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


    /** Create and return a snapshot of this perspective
     * A snapshot is a rendered Perspectie object that contains all the links of the perspective.
     */
    async snapshot(): Promise<Perspective> {
        return this.#client.snapshotByUUID(this.#handle.uuid)
    }

    /** Take and load all the links from the given snapshot */
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

    /** Convenience function to get the target of the first link that matches the given query
     * This makes sense when the query is expected to return only one link
     * and the target of that link is what you are looking for.
     *
     * Works best together with @member setSingelTarget()
     */
    async getSingleTarget(query: LinkQuery): Promise<string|void> {
        delete query.target
        const foundLinks = await this.get(query)
        if(foundLinks.length)
            return foundLinks[0].data.target
        else
            return null
    }

    /** Convenience function to ensure there is only one link with given source and predicate
     * This function will remove all links with the same source and predicate as the given link,
     * and then add the given link.
     * This ensures there is only one target for the given source and predicate.
     *
     * Works best together with @member getSingleTarget()
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
        let links = await this.get(new LinkQuery({
            predicate: "ad4m://sdna"
        }))

        return links.map(link => link.data.target).map(t => Literal.fromUrl(t).get())
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

    /** Creates a new subject instance by running its (SDNA defined) constructor,
     * which means adding links around the given expression address so that it
     * conforms to the given subject class.
     *
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
     * @param exprAddr The address of the expression to be turned into a subject instance
     */
    async createSubject<T>(subjectClass: T, exprAddr: string): Promise<T> {
        let className: string;

        if(typeof subjectClass === "string") {
            className = subjectClass

            await this.#client.createSubject(this.#handle.uuid, JSON.stringify({className}), exprAddr);
        } else {
            let query = this.buildQueryFromTemplate(subjectClass as object)
            await this.#client.createSubject(this.#handle.uuid, JSON.stringify({query}), exprAddr);
        }

        return this.getSubjectProxy(exprAddr, subjectClass)
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
     */
    async removeSubject<T>(subjectClass: T, exprAddr: string) {
        let className = await this.stringOrTemplateObjectToSubjectClassName(subjectClass)
        let result = await this.infer(`subject_class("${className}", C), destructor(C, Actions)`)
        if(!result.length) {
            throw "No constructor found for given subject class: " + className
        }

        let actions = result.map(x => eval(x.Actions))
        await this.executeAction(actions[0], exprAddr, undefined)
    }

    /** Checks if the given expression is a subject instance of the given subject class
     * @param expression The expression to be checked
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
    */
    async isSubjectInstance<T>(expression: string, subjectClass: T): Promise<boolean> {
        let className = await this.stringOrTemplateObjectToSubjectClassName(subjectClass)
        let isInstance = false;
        const maxAttempts = 5;
        let attempts = 0;

        while (attempts < maxAttempts && !isInstance) {
            isInstance = await this.infer(`subject_class("${className}", C), instance(C, "${expression}")`);
            attempts++;
          }

        return isInstance
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
            let instanceBaseExpressions = await this.infer(`subject_class("${className}", C), instance(C, X)`)
            let newInstances = await Promise.all(instanceBaseExpressions.map(async x => await this.getSubjectProxy(x.X, className) as unknown as T))
            instances = instances.concat(newInstances)
        }
        return instances
    }

    /** Returns all subject proxies of the given subject class.
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
            instances = await this.infer(`subject_class("${className}", C), instance(C, X)`)
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
                Object.keys(Object.getPrototypeOf(obj).__collections).filter(key => key !== 'isSubjectInstance').forEach(c => !collections.includes(c) ?? collections.push(c))
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

    get ai(): AIClient {
        return this.#client.aiClient
    }

    /** Subscribe to a Prolog query and get updates when the results change.
     * Returns a QuerySubscriptionProxy that handles keepalive and allows registering callbacks.
     * The initial and subsequent results can be accessed via the result property.
     * 
     * Make sure to call dispose() on the subscription when you're done with it
     */
    async subscribeInfer(query: string): Promise<QuerySubscriptionProxy> {
        const result = await this.#client.subscribeQuery(this.uuid, query);
        return new QuerySubscriptionProxy(
            this.uuid,
            result.subscriptionId,
            result.result,
            this.#client
        );
    }

}