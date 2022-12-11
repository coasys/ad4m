import { LinkCallback, PerspectiveClient } from "./PerspectiveClient";
import { Link, LinkExpression, LinkExpressionInput, LinkExpressionMutations, LinkInput, LinkMutations } from "../links/Links";
import { LinkQuery } from "./LinkQuery";
import { Neighbourhood } from "../neighbourhood/Neighbourhood";
import { PerspectiveHandle } from './PerspectiveHandle'
import { Perspective } from "./Perspective";
import { Literal } from "../Literal";
import { Subject } from "../subject/Subject";

type PerspectiveListenerTypes = "link-added" | "link-removed"

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
    #handle: PerspectiveHandle
    #client: PerspectiveClient
    #perspectiveLinkAddedCallbacks: LinkCallback[]
    #perspectiveLinkRemovedCallbacks: LinkCallback[]

    constructor(handle: PerspectiveHandle, ad4m: PerspectiveClient) {
        this.#perspectiveLinkAddedCallbacks = []
        this.#perspectiveLinkRemovedCallbacks = []
        this.#handle = handle
        this.#client = ad4m
        this.#client.addPerspectiveLinkAddedListener(this.#handle.uuid, this.#perspectiveLinkAddedCallbacks)
        this.#client.addPerspectiveLinkRemovedListener(this.#handle.uuid, this.#perspectiveLinkRemovedCallbacks)
    }

    async executeAction(actions, expression, parameters: Parameter[]) {
        const replaceThis = (input: string|undefined) => {
            if(input)
                return input.replace('this', expression)
            else
                return undefined
        }

        const replaceParameters = (input: string|undefined) => {
            if(parameters) {
                let output = input
                for(const parameter of parameters) {
                    output = output.replace(parameter.name, parameter.value)
                }
                return output
            } else
                return input
        }


        for(let command of actions) {
            let source = replaceThis(replaceParameters(command.source))
            let predicate = replaceThis(replaceParameters(command.predicate))
            let target = replaceThis(replaceParameters(command.target))
            switch(command.action) {
                case 'addLink':
                    await this.add(new Link({source, predicate, target}))
                    break;
                case 'removeLink':
                    const linkExpressions = await this.get(new LinkQuery({source, predicate, target}))
                    for (const linkExpression of linkExpressions) {
                        await this.remove(linkExpression)
                    }
                    break;
                case 'setSingleTarget':
                    await this.setSingleTarget(new Link({source, predicate, target}))
                    break;
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

    /** Adds multiple links to this perspective **/
    async addLinks(links: Link[]): Promise<LinkExpression[]> {
        return await this.#client.addLinks(this.#handle.uuid, links)
    }

    /** Removes multiple links from this perspective **/
    async removeLinks(links: LinkExpressionInput[]): Promise<LinkExpression[]> {
        return await this.#client.removeLinks(this.#handle.uuid, links)
    }

    /** Adds and removes multiple links from this perspective **/
    async linkMutations(mutations: LinkMutations): Promise<LinkExpressionMutations> {
        return await this.#client.linkMutations(this.#handle.uuid, mutations)
    }

    /** Adds a linkExpression to this perspective */
    async addLinkExpression(link: LinkExpressionInput): Promise<LinkExpression> {
        return await this.#client.addLinkExpression(this.#handle.uuid, link)
    }

    async update(oldLink: LinkExpressionInput, newLink: Link) {
        return await this.#client.updateLink(this.#handle.uuid, oldLink, newLink)
    }

    async remove(link: LinkExpressionInput) {
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

    /** Set the perspective's Social DNA code to the given string. 
     * This will replace all previous SDNA code elements with the new one.
     */
    async setSdna(sdnaCode: string) {
        await this.setSingleTarget(new Link({
            source: "ad4m://self",
            predicate: "ad4m://has_zome",
            target: Literal.from(sdnaCode).toUrl()
        }))
    }

    /** Returns the perspective's Social DNA code 
     * This will return all SDNA code elements in an array.
     */
    async getSdna(): Promise<string[]> {
        let links = await this.get(new LinkQuery({
            source: "ad4m://self",
            predicate: "ad4m://has_zome"
        }))

        return links.map(link => link.data.target).map(t => Literal.fromUrl(t).get())
    }

    /** Returns all the Subject classes defined in this perspectives SDNA */
    async subjectClasses(): Promise<string[]> {
        try {
            return (await this.infer("subject_class(X, _)")).map(x => x.X)
        }catch(e) {
            return []
        }
    }

    async stringOrTemplateObjectToSubjectClass<T>(subjectClass: T): Promise<string> {
        if(typeof subjectClass === "string")
            return subjectClass
        else { 
            let subjectClasses = await this.subjectClassesByTemplate(subjectClass as object)
            return subjectClasses[0]
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
        let className = await this.stringOrTemplateObjectToSubjectClass(subjectClass)
        let result = await this.infer(`subject_class("${className}", C), constructor(C, Actions)`)
        if(!result.length) {
            throw "No constructor found for given subject class"
        }

        let actions = result.map(x => eval(x.Actions))
        await this.executeAction(actions[0], exprAddr, undefined)
        return this.getSubjectProxy(exprAddr, subjectClass)
    }

    /** Checks if the given expression is a subject instance of the given subject class 
     * @param expression The expression to be checked
     * @param subjectClass Either a string with the name of the subject class, or an object
     * with the properties of the subject class. In the latter case, the first subject class
     * that matches the given properties will be used.
    */
    async isSubjectInstance<T>(expression: string, subjectClass: T): Promise<boolean> {
        let className = await this.stringOrTemplateObjectToSubjectClass(subjectClass)
        return await this.infer(`subject_class("${className}", c), instance(c, "${expression}")`)
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
            throw `Expression ${base} is not a subject instance of given class: ${subjectClass}`
        }
        let className = await this.stringOrTemplateObjectToSubjectClass(subjectClass)   
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
            let instanceBaseExpressions = await this.infer(`subject_class("${className}", c), instance(c, X)`)
            let newInstances = await Promise.all(instanceBaseExpressions.map(async x => await this.getSubjectProxy(x.X, className) as unknown as T))
            instances = instances.concat(newInstances)
        }
        return instances
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
        // Collect all string properties of the object in a list
        let properties = Object.keys(obj).filter(key => typeof obj[key] === "string")

        // Collect all collections of the object in a list
        let collections = Object.keys(obj).filter(key => Array.isArray(obj[key]))

        // Collect all set functions of the object in a list
        let setFunctions = Object.getOwnPropertyNames(obj).filter(key => (typeof obj[key] === "function") && key.startsWith("set"))
        // Add all set functions of the object's prototype to that list
        setFunctions = setFunctions.concat(Object.getOwnPropertyNames(Object.getPrototypeOf(obj)).filter(key => (typeof obj[key] === "function") && key.startsWith("set")))

        // Collect all add functions of the object in a list
        let addFunctions = Object.getOwnPropertyNames(obj).filter(key => (typeof obj[key] === "function") && key.startsWith("add"))
        // Add all add functions of the object's prototype to that list
        addFunctions = addFunctions.concat(Object.getOwnPropertyNames(Object.getPrototypeOf(obj)).filter(key => (typeof obj[key] === "function") && key.startsWith("add")))

        // Construct query to find all subject classes that have the given properties and collections
        let query = `subject_class(Class, c)`

        for(let property of properties) {
            query += `, property(c, "${property}")`
        }
        for(let collection of collections) {
            query += `, collection(c, "${collection}")`
        }

        for(let setFunction of setFunctions) {
            // e.g.  "setState" -> "state"
            let property = setFunction.substring(3)
            property = property.charAt(0).toLowerCase() + property.slice(1)
            query += `, property_setter(c, "${property}", _)`
        }
        for(let addFunction of addFunctions) {
            // e.g. "addComment" -> "comments"
            let property = addFunction.substring(3)
            property = property.charAt(0).toLowerCase() + property.slice(1) + "s"
            query += `, collection_adder(c, "${property}", _)`
        }

        query += "."
        //console.log(query)
        let result = await this.infer(query)

        if(!result) {
            return []
        } else {
            return result.map(x => x.Class)
        }
    }
}