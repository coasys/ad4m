import { LinkCallback, PerspectiveClient } from "./PerspectiveClient";
import { Link, LinkExpression, LinkExpressionInput } from "../links/Links";
import { LinkQuery } from "./LinkQuery";
import { Neighbourhood } from "../neighbourhood/Neighbourhood";
import { PerspectiveHandle } from './PerspectiveHandle'
import { Perspective } from "./Perspective";

type PerspectiveListenerTypes = "link-added" | "link-removed"

/** Perspective UI proxy object
 * 
 * Convenience object for UIs to interact with a perspective.
 * It is created by some of the methods in the PerspectiveClient class and includes
 * a reference to the PerspectiveClient object that created it.
 */
export class PerspectiveProxy {
    #handle: PerspectiveHandle
    #client: PerspectiveClient
    #perspectiveLinkAddedCallbacks: LinkCallback[]
    #perspectiveLinkRemovedCallbacks: LinkCallback[]
    #executeAction

    constructor(handle: PerspectiveHandle, ad4m: PerspectiveClient) {
        this.#perspectiveLinkAddedCallbacks = []
        this.#perspectiveLinkRemovedCallbacks = []
        this.#handle = handle
        this.#client = ad4m
        this.#client.addPerspectiveLinkAddedListener(this.#handle.uuid, this.#perspectiveLinkAddedCallbacks)
        this.#client.addPerspectiveLinkRemovedListener(this.#handle.uuid, this.#perspectiveLinkRemovedCallbacks)

        this.#executeAction = async (actions, expression) => {
            console.log("execute:", actions)
    
            const replaceThis = (input: string|undefined) => {
                if(input)
                    return input.replace('this', expression)
                else
                    return undefined
            }
    
            for(let command of actions) {
                switch(command.action) {
                    case 'addLink':
                        await this.add(new Link({
                            source: replaceThis(command.source),
                            predicate: replaceThis(command.predicate),
                            target: replaceThis(command.target)
                        }))
                        break;
                    case 'removeLink':
                        const linkExpressions = await this.get(new LinkQuery({
                            source: replaceThis(command.source), 
                            predicate: replaceThis(command.predicate), 
                            target: replaceThis(command.target)}))
                        for (const linkExpression of linkExpressions) {
                            await this.remove(linkExpression)
                        }
                        break;
                }
            }
        }
    }

    /** Unique ID of the perspective */
    get uuid(): string {
        return this.#handle.uuid
    }

    /** Given name of the perspective */
    get name(): string {
        return this.#handle.name
    }

    /** If the perspective is shared as a Neighbourhood, this is the Neighbourhood URL */
    get sharedUrl(): string|void {
        return this.#handle.sharedUrl
    }

    /** If the perspective is shared as a Neighbourhood, this is the Neighbourhood Expression */
    get neighbourhood(): Neighbourhood|void {
        return this.#handle.neighbourhood
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
    async add(link: Link): Promise<LinkExpression> {
        return await this.#client.addLink(this.#handle.uuid, link)
    }

    async update(oldLink: LinkExpression, newLink: Link) {
        return await this.#client.updateLink(this.#handle.uuid, oldLink, newLink)
    }

    async remove(link: LinkExpression) {
        return await this.#client.removeLink(this.#handle.uuid, link)
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
        }
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
    async setSingleTarget(link: Link) {
        const query = new LinkQuery({source: link.source, predicate: link.predicate})
        const foundLinks = await this.get(query)
        for(const l of foundLinks){
            delete l.__typename
            delete l.data.__typename
            delete l.proof.__typename
            await this.remove(l)
        }
            
        await this.add(link)
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
        await this.#executeAction(startAction, exprAddr)
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
        await this.#executeAction(action, exprAddr)
    }

}