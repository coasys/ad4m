import { ApolloClient, gql } from "@apollo/client/core";
import { ExpressionRendered } from "../expression/Expression";
import { ExpressionClient } from "../expression/ExpressionClient";
import { Link, LinkExpressionInput, LinkExpression, LinkInput, LinkMutations, LinkExpressionMutations } from "../links/Links";
import { NeighbourhoodClient } from "../neighbourhood/NeighbourhoodClient";
import { NeighbourhoodProxy } from "../neighbourhood/NeighbourhoodProxy";
import unwrapApolloResult from "../unwrapApolloResult";
import { LinkQuery } from "./LinkQuery";
import { Perspective } from "./Perspective";
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle";
import { LinkStatus, PerspectiveProxy } from './PerspectiveProxy';
import { AIClient } from "../ai/AIClient";
import { AllInstancesResult } from "../model/Ad4mModel";

const LINK_EXPRESSION_FIELDS = `
author
timestamp
status
data { source, predicate, target }
proof { valid, invalid, signature, key }
`

const PERSPECTIVE_HANDLE_FIELDS = `
uuid
name
sharedUrl
state
owners
neighbourhood {
    data {
        linkLanguage
        meta {
            links
                {
                    author
                    timestamp
                    data { source, predicate, target }
                    proof { valid, invalid, signature, key }
                }
        }
    }
    author
}
`

export type PerspectiveHandleCallback = (perspective: PerspectiveHandle) => null
export type UuidCallback = (uuid: string) => null
export type LinkCallback = (link: LinkExpression) => null
export type SyncStateChangeCallback = (state: PerspectiveState) => null

export class PerspectiveClient {
    #apolloClient: ApolloClient<any>
    #perspectiveAddedCallbacks: PerspectiveHandleCallback[]
    #perspectiveUpdatedCallbacks: PerspectiveHandleCallback[]
    #perspectiveRemovedCallbacks: UuidCallback[]
    #perspectiveSyncStateChangeCallbacks: SyncStateChangeCallback[]
    #expressionClient?: ExpressionClient
    #neighbourhoodClient?: NeighbourhoodClient
    #aiClient?: AIClient

    constructor(client: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = client
        this.#perspectiveAddedCallbacks = []
        this.#perspectiveUpdatedCallbacks = []
        this.#perspectiveRemovedCallbacks = []
        this.#perspectiveSyncStateChangeCallbacks = []

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
        return this.#aiClient
    }

    async all(): Promise<PerspectiveProxy[]> {
        const { perspectives } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query perspectives {
                perspectives {
                    ${PERSPECTIVE_HANDLE_FIELDS}
                }
            }`
        }))
        return perspectives.map(handle => new PerspectiveProxy(handle, this))
    }

    async byUUID(uuid: string): Promise<PerspectiveProxy|null> {
        const { perspective } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query perspective($uuid: String!) {
                perspective(uuid: $uuid) {
                    ${PERSPECTIVE_HANDLE_FIELDS}
                }
            }`,
            variables: { uuid }
        }))
        if(!perspective) return null
        return new PerspectiveProxy(perspective, this)
    }

    async snapshotByUUID(uuid: string): Promise<Perspective|null> {
        const { perspectiveSnapshot } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query perspectiveSnapshot($uuid: String!) {
                perspectiveSnapshot(uuid: $uuid) {
                    links { ${LINK_EXPRESSION_FIELDS} }
                }
            }`,
            variables: { uuid }
        }))
        return perspectiveSnapshot
    }
    async publishSnapshotByUUID(uuid: string): Promise<string|null> {
        const { perspectivePublishSnapshot } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectivePublishSnapshot($uuid: String!) {
                perspectivePublishSnapshot(uuid: $uuid)
            }`,
            variables: { uuid }
        }))
        return perspectivePublishSnapshot
    }

    async queryLinks(uuid: string, query: LinkQuery): Promise<LinkExpression[]> {
        const { perspectiveQueryLinks } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query perspectiveQueryLinks($uuid: String!, $query: LinkQuery!) {
                perspectiveQueryLinks(query: $query, uuid: $uuid) {
                    ${LINK_EXPRESSION_FIELDS}
                }
            }`,
            variables: { uuid, query }
        }))
        return perspectiveQueryLinks
    }

    async queryProlog(uuid: string, query: string): Promise<any> {
        const { perspectiveQueryProlog } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query perspectiveQueryProlog($uuid: String!, $query: String!) {
                perspectiveQueryProlog(uuid: $uuid, query: $query)
            }`,
            variables: { uuid, query }
        }))

        return JSON.parse(perspectiveQueryProlog)
    }

    /**
     * Executes a read-only SurrealQL query against a perspective's link cache.
     * 
     * Security: Only SELECT, RETURN, and other read-only queries are permitted.
     * Mutating operations (DELETE, UPDATE, INSERT, etc.) are blocked.
     * 
     * Note: GraphQL field name is "perspectiveQuerySurrealDb" (lowercase "b" in "Db")
     * as generated from Rust method "perspective_query_surreal_db"
     */
    async querySurrealDB(uuid: string, query: string): Promise<any> {
        const { perspectiveQuerySurrealDb } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query perspectiveQuerySurrealDb($uuid: String!, $query: String!) {
                perspectiveQuerySurrealDb(uuid: $uuid, query: $query)
            }`,
            variables: { uuid, query }
        }))

        return JSON.parse(perspectiveQuerySurrealDb)
    }

    async subscribeQuery(uuid: string, query: string): Promise<{ subscriptionId: string, result: AllInstancesResult, isInit?: boolean }> {
        const { perspectiveSubscribeQuery } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveSubscribeQuery($uuid: String!, $query: String!) {
                perspectiveSubscribeQuery(uuid: $uuid, query: $query) {
                    subscriptionId
                    result
                }
            }`,
            variables: { uuid, query }
        }))
        const { subscriptionId, result } = perspectiveSubscribeQuery
        let finalResult = result;
        let isInit = false;
        if(finalResult.startsWith("#init#")) {
            finalResult = finalResult.substring(6)
            isInit = true;
        }
        try {
            finalResult = JSON.parse(finalResult)
        } catch (e) {
            console.error('Error parsing perspectiveSubscribeQuery result:', e)
        }
        return { subscriptionId, result: finalResult, isInit }
    }

    async perspectiveSubscribeSurrealQuery(uuid: string, query: string): Promise<{ subscriptionId: string, result: AllInstancesResult, isInit?: boolean }> {
        const { perspectiveSubscribeSurrealQuery } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveSubscribeSurrealQuery($uuid: String!, $query: String!) {
                perspectiveSubscribeSurrealQuery(uuid: $uuid, query: $query) {
                    subscriptionId
                    result
                }
            }`,
            variables: { uuid, query }
        }))
        const { subscriptionId, result } = perspectiveSubscribeSurrealQuery
        let finalResult = result;
        let isInit = false;
        if(finalResult.startsWith("#init#")) {
            finalResult = finalResult.substring(6)
            isInit = true;
        }
        try {
            finalResult = JSON.parse(finalResult)
        } catch (e) {
            console.error('Error parsing perspectiveSubscribeSurrealQuery result:', e)
        }
        return { subscriptionId, result: finalResult, isInit }
    }

    async perspectiveKeepAliveSurrealQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        const { perspectiveKeepAliveSurrealQuery } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveKeepAliveSurrealQuery($uuid: String!, $subscriptionId: String!) {
                perspectiveKeepAliveSurrealQuery(uuid: $uuid, subscriptionId: $subscriptionId)
            }`,
            variables: { uuid, subscriptionId }
        }))

        return perspectiveKeepAliveSurrealQuery
    }

    async perspectiveDisposeSurrealQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        const { perspectiveDisposeSurrealQuerySubscription } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveDisposeSurrealQuerySubscription($uuid: String!, $subscriptionId: String!) {
                perspectiveDisposeSurrealQuerySubscription(uuid: $uuid, subscriptionId: $subscriptionId)
            }`,
            variables: { uuid, subscriptionId }
        }))

        return perspectiveDisposeSurrealQuerySubscription
    }

    subscribeToQueryUpdates(subscriptionId: string, onData: (result: AllInstancesResult) => void): () => void {
        const subscription = this.#apolloClient.subscribe({
            query: gql`
                subscription perspectiveQuerySubscription($subscriptionId: String!) {
                    perspectiveQuerySubscription(subscriptionId: $subscriptionId)
                }
            `,
            variables: {
                subscriptionId
            }
        }).subscribe({
            next: (result) => {
                if (result.data && result.data.perspectiveQuerySubscription) {
                    let finalResult = result.data.perspectiveQuerySubscription;
                    let isInit = false;
                    if(finalResult.startsWith("#init#")) {
                        finalResult = finalResult.substring(6)
                        isInit = true;
                    }
                    try {
                        finalResult = JSON.parse(finalResult)
                        if(isInit && typeof finalResult === 'object') {
                            finalResult.isInit = true;
                        }
                    } catch (e) {
                        console.error('Error parsing perspectiveQuerySubscription:', e)
                    }
                    onData(finalResult);
                }
            },
            error: (e) => console.error('Error in query subscription:', e)
        });

        return () => subscription.unsubscribe();
    }

    async keepAliveQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        const { perspectiveKeepAliveQuery } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveKeepAliveQuery($uuid: String!, $subscriptionId: String!) {
                perspectiveKeepAliveQuery(uuid: $uuid, subscriptionId: $subscriptionId)
            }`,
            variables: { uuid, subscriptionId }
        }))

        return perspectiveKeepAliveQuery
    }

    async disposeQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        const { perspectiveDisposeQuerySubscription } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveDisposeQuerySubscription($uuid: String!, $subscriptionId: String!) {
                perspectiveDisposeQuerySubscription(uuid: $uuid, subscriptionId: $subscriptionId)
            }`,
            variables: { uuid, subscriptionId }
        }))

        return perspectiveDisposeQuerySubscription
    }

    async add(name: string): Promise<PerspectiveProxy> {
        const { perspectiveAdd } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveAdd($name: String!) {
                perspectiveAdd(name: $name) {
                    ${PERSPECTIVE_HANDLE_FIELDS}
                }
            }`,
            variables: { name }
        }))
        return new PerspectiveProxy(perspectiveAdd, this)
    }

    async update(uuid: string, name: string): Promise<PerspectiveProxy> {
        const { perspectiveUpdate } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveUpdate($uuid: String!, $name: String!) {
                perspectiveUpdate(uuid: $uuid, name: $name) {
                    ${PERSPECTIVE_HANDLE_FIELDS}
                }
            }`,
            variables: { uuid, name }
        }))
        return new PerspectiveProxy(perspectiveUpdate, this)
    }

    async remove(uuid: string): Promise<{perspectiveRemove: boolean}> {
        return unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveRemove($uuid: String!) {
                perspectiveRemove(uuid: $uuid)
            }`,
            variables: { uuid }
        }))
    }

    async addLink(uuid: string, link: Link, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        const { perspectiveAddLink } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveAddLink($uuid: String!, $link: LinkInput!, $status: String!, $batchId: String) {
                perspectiveAddLink(uuid: $uuid, link: $link, status: $status, batchId: $batchId) {
                    ${LINK_EXPRESSION_FIELDS}
                }
            }`,
            variables: { uuid, link, status, batchId }
        }))
        return perspectiveAddLink
    }

    async addLinks(uuid: string, links: Link[], status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression[]> {
        const { perspectiveAddLinks } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveAddLinks($uuid: String!, $links: [LinkInput!]!, $status: String!, $batchId: String) {
                perspectiveAddLinks(uuid: $uuid, links: $links, status: $status, batchId: $batchId) {
                    ${LINK_EXPRESSION_FIELDS}
                }
            }`,
            variables: { uuid, links, status, batchId }
        }))
        return perspectiveAddLinks
    }

    async removeLinks(uuid: string, links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        const { perspectiveRemoveLinks } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveRemoveLinks($uuid: String!, $links: [LinkExpressionInput!]!, $batchId: String) {
                perspectiveRemoveLinks(uuid: $uuid, links: $links, batchId: $batchId) {
                    ${LINK_EXPRESSION_FIELDS}
                }
            }`,
            variables: { uuid, links, batchId }
        }))
        return perspectiveRemoveLinks
    }

    async linkMutations(uuid: string, mutations: LinkMutations, status?: LinkStatus): Promise<LinkExpressionMutations> {
        const { perspectiveLinkMutations } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveLinkMutations($uuid: String!, $mutations: LinkMutations!, $status: String){
                perspectiveLinkMutations(mutations: $mutations, uuid: $uuid, status: $status) {
                    additions {
                        ${LINK_EXPRESSION_FIELDS}
                    }
                    removals {
                        ${LINK_EXPRESSION_FIELDS}
                    }
                }
            }`,
            variables: { uuid, mutations, status }
        }))
        return perspectiveLinkMutations
    }

    async addLinkExpression(uuid: string, link: LinkExpression, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        const { perspectiveAddLinkExpression } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveAddLinkExpression($uuid: String!, $link: LinkExpressionInput!, $status: String!, $batchId: String) {
                perspectiveAddLinkExpression(uuid: $uuid, link: $link, status: $status, batchId: $batchId) {
                    ${LINK_EXPRESSION_FIELDS}
                }
            }`,
            variables: { uuid, link, status, batchId }
        }))
        return perspectiveAddLinkExpression
    }

    async updateLink(uuid: string, oldLink: LinkExpressionInput, newLink: Link, batchId?: string): Promise<LinkExpression> {
        const { perspectiveUpdateLink } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveUpdateLink($uuid: String!, $oldLink: LinkExpressionInput!, $newLink: LinkInput!, $batchId: String) {
                perspectiveUpdateLink(uuid: $uuid, oldLink: $oldLink, newLink: $newLink, batchId: $batchId) {
                    ${LINK_EXPRESSION_FIELDS}
                }
            }`,
            variables: { uuid, oldLink, newLink, batchId }
        }))
        return perspectiveUpdateLink
    }

    async removeLink(uuid: string, link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        delete link.__typename
        delete link.data.__typename
        delete link.proof.__typename
        delete link.status
        const { perspectiveRemoveLink } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveRemoveLink($link: LinkExpressionInput!, $uuid: String!, $batchId: String) {
                perspectiveRemoveLink(link: $link, uuid: $uuid, batchId: $batchId)
            }`,
            variables: { uuid, link, batchId }
        }))
        return perspectiveRemoveLink
    }

    /**
     * Adds Social DNA code to a perspective.
     * 
     * **Note:** For new code, prefer passing `shaclJson` directly. The `sdnaCode` parameter
     * accepts legacy Prolog SDNA which is automatically converted to SHACL links on the backend.
     * 
     * @param sdnaCode - Prolog SDNA code (legacy - can be empty string if shaclJson provided)
     * @param shaclJson - SHACL JSON representation (recommended for new code)
     */
    async addSdna(uuid: string,  name: string, sdnaCode: string, sdnaType: "subject_class" | "flow" | "custom", shaclJson?: string): Promise<boolean> {
        return unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveAddSdna($uuid: String!, $name: String!, $sdnaCode: String!, $sdnaType: String!, $shaclJson: String) {
                perspectiveAddSdna(uuid: $uuid, name: $name, sdnaCode: $sdnaCode, sdnaType: $sdnaType, shaclJson: $shaclJson)
            }`,            variables: { uuid, name, sdnaCode, sdnaType, shaclJson }
        })).perspectiveAddSdna
    }

    async executeCommands(uuid: string, commands: string, expression: string, parameters: string, batchId?: string): Promise<boolean> {
        return unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveExecuteCommands($uuid: String!, $commands: String!, $expression: String!, $parameters: String, $batchId: String) {
                perspectiveExecuteCommands(uuid: $uuid, commands: $commands, expression: $expression, parameters: $parameters, batchId: $batchId)
            }`,
            variables: { uuid, commands, expression, parameters, batchId }
        })).perspectiveExecuteCommands
    }

    async createSubject(uuid: string, subjectClass: string, expressionAddress: string, initialValues?: string, batchId?: string): Promise<boolean> {
        return unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveCreateSubject($uuid: String!, $subjectClass: String!, $expressionAddress: String!, $initialValues: String, $batchId: String) {
                perspectiveCreateSubject(uuid: $uuid, subjectClass: $subjectClass, expressionAddress: $expressionAddress, initialValues: $initialValues, batchId: $batchId)
            }`,
            variables: { uuid, subjectClass, expressionAddress, initialValues, batchId }
        })).perspectiveCreateSubject
    }

    async getSubjectData(uuid: string, subjectClass: string, expressionAddress: string): Promise<string> {
        return unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveGetSubjectData($uuid: String!, $subjectClass: String!, $expressionAddress: String!) {
                perspectiveGetSubjectData(uuid: $uuid, subjectClass: $subjectClass, expressionAddress: $expressionAddress)
            }`,
            variables: { uuid, subjectClass, expressionAddress }
        })).perspectiveGetSubjectData
    }

    // ExpressionClient functions, needed for Subjects:
    async getExpression(expressionURI: string): Promise<ExpressionRendered> {
        return await this.#expressionClient.get(expressionURI)
    }

    async createExpression(content: any, languageAddress: string): Promise<string> {
        return await this.#expressionClient.create(content, languageAddress)
    }

    // Subscriptions:
    addPerspectiveAddedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveAddedCallbacks.push(cb)
    }

    subscribePerspectiveAdded() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                perspectiveAdded { ${PERSPECTIVE_HANDLE_FIELDS} }
            }
        `}).subscribe({
            next: result => {
                this.#perspectiveAddedCallbacks.forEach(cb => {
                    cb(result.data.perspectiveAdded)
                })
            },
            error: (e) => console.error(e)
        })
    }

    addPerspectiveUpdatedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveUpdatedCallbacks.push(cb)
    }

    subscribePerspectiveUpdated() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                perspectiveUpdated { ${PERSPECTIVE_HANDLE_FIELDS} }
            }
        `}).subscribe({
            next: result => {
                this.#perspectiveUpdatedCallbacks.forEach(cb => {
                    cb(result.data.perspectiveUpdated)
                })
            },
            error: (e) => console.error(e)
        })
    }

    addPerspectiveSyncedListener(cb: SyncStateChangeCallback) {
        this.#perspectiveSyncStateChangeCallbacks.push(cb)
    }

    async addPerspectiveSyncStateChangeListener(uuid: String, cb: SyncStateChangeCallback[]): Promise<void> {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                perspectiveSyncStateChange(uuid: "${uuid}")
            }
        `}).subscribe({
            next: result => {
                cb.forEach(c => {
                    c(result.data.perspectiveSyncStateChange)
                })
            },
            error: (e) => console.error(e)
        })

        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    addPerspectiveRemovedListener(cb: UuidCallback) {
        this.#perspectiveRemovedCallbacks.push(cb)
    }

    subscribePerspectiveRemoved() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                perspectiveRemoved
            }
        `}).subscribe({
            next: result => {
                this.#perspectiveRemovedCallbacks.forEach(cb => {
                    cb(result.data.perspectiveRemoved)
                })
            },
            error: (e) => console.error(e)
        })
    }

    async addPerspectiveLinkAddedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                perspectiveLinkAdded(uuid: "${uuid}") { ${LINK_EXPRESSION_FIELDS} }
            }
        `}).subscribe({
            next: result => {
                cb.forEach(c => {
                    c(result.data.perspectiveLinkAdded)
                })
            },
            error: (e) => console.error(e)
        })

        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    async addPerspectiveLinkRemovedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                perspectiveLinkRemoved(uuid: "${uuid}") { ${LINK_EXPRESSION_FIELDS} }
            }
        `}).subscribe({
            next: result => {
                cb.forEach(c => {
                    if (!result.data.perspectiveLinkRemoved.status) {
                        delete result.data.perspectiveLinkRemoved.status
                    }
                    c(result.data.perspectiveLinkRemoved)
                })
            },
            error: (e) => console.error(e)
        })

        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    async addPerspectiveLinkUpdatedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                perspectiveLinkUpdated(uuid: "${uuid}") {
                    oldLink {
                        ${LINK_EXPRESSION_FIELDS}
                    }
                    newLink {
                        ${LINK_EXPRESSION_FIELDS}
                    }
                }
            }
        `}).subscribe({
            next: result => {
                cb.forEach(c => {
                    if (!result.data.perspectiveLinkUpdated.newLink.status) {
                        delete result.data.perspectiveLinkUpdated.newLink.status
                    }
                    if (!result.data.perspectiveLinkUpdated.oldLink.status) {
                        delete result.data.perspectiveLinkUpdated.oldLink.status
                    }
                    c(result.data.perspectiveLinkUpdated)
                })
            },
            error: (e) => console.error(e)
        })

        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    getNeighbourhoodProxy(uuid: string): NeighbourhoodProxy {
        return new NeighbourhoodProxy(this.#neighbourhoodClient, uuid)
    }

    async createBatch(uuid: string): Promise<string> {
        const { perspectiveCreateBatch } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveCreateBatch($uuid: String!) {
                perspectiveCreateBatch(uuid: $uuid)
            }`,
            variables: { uuid }
        }))
        return perspectiveCreateBatch
    }

    async commitBatch(uuid: string, batchId: string): Promise<LinkExpressionMutations> {
        const { perspectiveCommitBatch } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation perspectiveCommitBatch($uuid: String!, $batchId: String!) {
                perspectiveCommitBatch(uuid: $uuid, batchId: $batchId) {
                    additions {
                        ${LINK_EXPRESSION_FIELDS}
                    }
                    removals {
                        ${LINK_EXPRESSION_FIELDS}
                    }
                }
            }`,
            variables: { uuid, batchId }
        }))
        return perspectiveCommitBatch
    }
}