import { RestClient } from "../restClient";
import { ExpressionRendered } from "../expression/Expression";
import { ExpressionClient } from "../expression/ExpressionClient";
import { Link, LinkExpressionInput, LinkExpression, LinkInput, LinkMutations, LinkExpressionMutations } from "../links/Links";
import { NeighbourhoodClient } from "../neighbourhood/NeighbourhoodClient";
import { NeighbourhoodProxy } from "../neighbourhood/NeighbourhoodProxy";
import { LinkQuery } from "./LinkQuery";
import { Perspective } from "./Perspective";
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle";
import { LinkStatus, PerspectiveProxy } from './PerspectiveProxy';
import { AIClient } from "../ai/AIClient";
import { AllInstancesResult } from "../model/types";

export type PerspectiveHandleCallback = (perspective: PerspectiveHandle) => null
export type UuidCallback = (uuid: string) => null
export type LinkCallback = (link: LinkExpression) => null
export type SyncStateChangeCallback = (state: PerspectiveState) => null

export class PerspectiveClient {
    #restClient: RestClient
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

    constructor(baseUrl: string, token?: string, subscribe: boolean = true) {
        this.#restClient = new RestClient(baseUrl, token)
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
        const perspectives = await this.#restClient.get<PerspectiveHandle[]>('/api/v1/perspectives')
        return perspectives.map(handle => new PerspectiveProxy(handle, this))
    }

    async byUUID(uuid: string): Promise<PerspectiveProxy|null> {
        try {
            const perspective = await this.#restClient.get<PerspectiveHandle>(`/api/v1/perspectives/${encodeURIComponent(uuid)}`)
            if(!perspective) return null
            return new PerspectiveProxy(perspective, this)
        } catch(e) {
            return null
        }
    }

    async snapshotByUUID(uuid: string): Promise<Perspective|null> {
        return this.#restClient.get<Perspective|null>(`/api/v1/perspectives/${encodeURIComponent(uuid)}/snapshot`)
    }

    async publishSnapshotByUUID(uuid: string): Promise<string|null> {
        return this.#restClient.post<string|null>(`/api/v1/perspectives/${encodeURIComponent(uuid)}/publish-snapshot`)
    }

    async queryLinks(uuid: string, query: LinkQuery): Promise<LinkExpression[]> {
        const params = new URLSearchParams()
        if (query.source) params.set('source', query.source)
        if (query.predicate) params.set('predicate', query.predicate)
        if (query.target) params.set('target', query.target)
        if (query.fromDate) params.set('fromDate', query.fromDate instanceof Date ? query.fromDate.toISOString() : String(query.fromDate))
        if (query.untilDate) params.set('untilDate', query.untilDate instanceof Date ? query.untilDate.toISOString() : String(query.untilDate))
        if (query.limit !== undefined) params.set('limit', String(query.limit))
        return this.#restClient.get<LinkExpression[]>(`/api/v1/perspectives/${encodeURIComponent(uuid)}/links?${params.toString()}`)
    }

    async queryProlog(uuid: string, query: string): Promise<any> {
        const result = await this.#restClient.post<string>(`/api/v1/perspectives/${encodeURIComponent(uuid)}/query/prolog`, { query })
        return JSON.parse(result)
    }

    async querySparql(uuid: string, query: string): Promise<any> {
        const result = await this.#restClient.post<string>(`/api/v1/perspectives/${encodeURIComponent(uuid)}/query/sparql`, { query })
        return JSON.parse(result)
    }

    async querySurrealDB(uuid: string, query: string): Promise<any> {
        const result = await this.#restClient.post<string>(`/api/v1/perspectives/${encodeURIComponent(uuid)}/query/surreal`, { query })
        return JSON.parse(result)
    }

    async subscribeQuery(uuid: string, query: string): Promise<{ subscriptionId: string, result: AllInstancesResult, isInit?: boolean }> {
        const response = await this.#restClient.post<{ subscriptionId: string, result: string }>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/subscribe-query`, { query }
        )
        const { subscriptionId, result } = response
        let finalResult: any = result
        let isInit = false
        if (typeof finalResult === 'string' && finalResult.startsWith("#init#")) {
            finalResult = finalResult.substring(6)
            isInit = true
        }
        try {
            finalResult = JSON.parse(finalResult)
        } catch (e) {
            console.error('Error parsing subscribeQuery result:', e)
        }
        return { subscriptionId, result: finalResult, isInit }
    }

    async perspectiveSubscribeSurrealQuery(uuid: string, query: string): Promise<{ subscriptionId: string, result: AllInstancesResult, isInit?: boolean }> {
        const response = await this.#restClient.post<{ subscriptionId: string, result: string }>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/subscribe-surreal-query`, { query }
        )
        const { subscriptionId, result } = response
        let finalResult: any = result
        let isInit = false
        if (typeof finalResult === 'string' && finalResult.startsWith("#init#")) {
            finalResult = finalResult.substring(6)
            isInit = true
        }
        try {
            finalResult = JSON.parse(finalResult)
        } catch (e) {
            console.error('Error parsing perspectiveSubscribeSurrealQuery result:', e)
        }
        return { subscriptionId, result: finalResult, isInit }
    }

    async perspectiveKeepAliveSurrealQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#restClient.post<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/keep-alive-surreal-query`,
            { subscriptionId }
        )
    }

    async perspectiveDisposeSurrealQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#restClient.post<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/dispose-surreal-query-subscription`,
            { subscriptionId }
        )
    }

    subscribeToQueryUpdates(subscriptionId: string, onData: (result: AllInstancesResult) => void): () => void {
        const unsub = this.#restClient.subscribe(
            `/api/v1/events/query-subscription/${encodeURIComponent(subscriptionId)}`,
            (data) => {
                let finalResult = data.result || data
                let isInit = false
                if (typeof finalResult === 'string' && finalResult.startsWith("#init#")) {
                    finalResult = finalResult.substring(6)
                    isInit = true
                }
                try {
                    finalResult = JSON.parse(finalResult)
                    if (isInit && typeof finalResult === 'object') {
                        finalResult.isInit = true
                    }
                } catch (e) {
                    console.error('Error parsing query subscription:', e)
                }
                onData(finalResult)
            }
        )
        this.#querySubscriptionUnsubscribers.set(subscriptionId, unsub)
        return unsub
    }

    async keepAliveQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#restClient.post<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/keep-alive-query`,
            { subscriptionId }
        )
    }

    async disposeQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        const unsub = this.#querySubscriptionUnsubscribers.get(subscriptionId)
        if (unsub) {
            unsub()
            this.#querySubscriptionUnsubscribers.delete(subscriptionId)
        }
        return this.#restClient.post<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/dispose-query-subscription`,
            { subscriptionId }
        )
    }

    async add(name: string): Promise<PerspectiveProxy> {
        const handle = await this.#restClient.post<PerspectiveHandle>('/api/v1/perspectives', { name })
        return new PerspectiveProxy(handle, this)
    }

    async update(uuid: string, name: string): Promise<PerspectiveProxy> {
        const handle = await this.#restClient.put<PerspectiveHandle>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}`, { name }
        )
        return new PerspectiveProxy(handle, this)
    }

    async remove(uuid: string): Promise<{perspectiveRemove: boolean}> {
        const result = await this.#restClient.delete<boolean>(`/api/v1/perspectives/${encodeURIComponent(uuid)}`)
        return { perspectiveRemove: result }
    }

    async addLink(uuid: string, link: Link, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return this.#restClient.post<LinkExpression>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/links`,
            { link, status, batchId }
        )
    }

    async addLinks(uuid: string, links: Link[], status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression[]> {
        return this.#restClient.post<LinkExpression[]>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/links/bulk`,
            { links, status, batchId }
        )
    }

    async removeLinks(uuid: string, links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        return this.#restClient.post<LinkExpression[]>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/links/remove-bulk`,
            { links, batchId }
        )
    }

    async linkMutations(uuid: string, mutations: LinkMutations, status?: LinkStatus): Promise<LinkExpressionMutations> {
        return this.#restClient.post<LinkExpressionMutations>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/links/mutations`,
            { mutations, status }
        )
    }

    async addLinkExpression(uuid: string, link: LinkExpression, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return this.#restClient.post<LinkExpression>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/links/expression`,
            { link, status, batchId }
        )
    }

    async updateLink(uuid: string, oldLink: LinkExpressionInput, newLink: Link, batchId?: string): Promise<LinkExpression> {
        return this.#restClient.put<LinkExpression>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/links`,
            { oldLink, newLink, batchId }
        )
    }

    async removeLink(uuid: string, link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        delete link.__typename
        delete link.data.__typename
        delete link.proof.__typename
        delete link.status
        return this.#restClient.delete<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/links`,
            { link, batchId }
        )
    }

    async addSdna(uuid: string, name: string, sdnaCode: string | undefined, sdnaType: "subject_class" | "flow" | "custom", shaclJson?: string): Promise<boolean> {
        return this.#restClient.post<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/sdna`,
            { name, sdnaCode: sdnaCode || "", sdnaType, shaclJson }
        )
    }

    async executeCommands(uuid: string, commands: string, expression: string, parameters: string, batchId?: string): Promise<boolean> {
        return this.#restClient.post<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/execute-commands`,
            { commands, expression, parameters, batchId }
        )
    }

    async createSubject(uuid: string, subjectClass: string, expressionAddress: string, initialValues?: string, batchId?: string): Promise<boolean> {
        return this.#restClient.post<boolean>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/create-subject`,
            { subjectClass, expressionAddress, initialValues, batchId }
        )
    }

    async getSubjectData(uuid: string, subjectClass: string, expressionAddress: string): Promise<string> {
        return this.#restClient.post<string>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/get-subject-data`,
            { subjectClass, expressionAddress }
        )
    }

    // ExpressionClient functions, needed for Subjects:
    async getExpression(expressionURI: string): Promise<ExpressionRendered> {
        return await this.#expressionClient!.get(expressionURI)
    }

    async createExpression(content: any, languageAddress: string): Promise<string> {
        return await this.#expressionClient!.create(content, languageAddress)
    }

    // Subscriptions:
    addPerspectiveAddedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveAddedCallbacks.push(cb)
    }

    subscribePerspectiveAdded() {
        const unsub = this.#restClient.subscribe('/api/v1/events/perspectives', (data) => {
            if (data.type === 'perspective-added') {
                this.#perspectiveAddedCallbacks.forEach(cb => cb(data.perspective))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addPerspectiveUpdatedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveUpdatedCallbacks.push(cb)
    }

    subscribePerspectiveUpdated() {
        const unsub = this.#restClient.subscribe('/api/v1/events/perspectives', (data) => {
            if (data.type === 'perspective-updated') {
                this.#perspectiveUpdatedCallbacks.forEach(cb => cb(data.perspective))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addPerspectiveSyncedListener(cb: SyncStateChangeCallback) {
        this.#perspectiveSyncStateChangeCallbacks.push(cb)
    }

    async addPerspectiveSyncStateChangeListener(uuid: String, cb: SyncStateChangeCallback[]): Promise<void> {
        const unsub = this.#restClient.subscribe(
            `/api/v1/events/neighbourhood/${encodeURIComponent(uuid as string)}`,
            (data) => {
                if (data.type === 'sync-state-change') {
                    cb.forEach(c => c(data.state))
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
        const unsub = this.#restClient.subscribe('/api/v1/events/perspectives', (data) => {
            if (data.type === 'perspective-removed') {
                this.#perspectiveRemovedCallbacks.forEach(cb => cb(data.uuid))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    async addPerspectiveLinkAddedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#restClient.subscribe(
            `/api/v1/events/links/${encodeURIComponent(uuid as string)}`,
            (data) => {
                if (data.type === 'link-added') {
                    cb.forEach(c => c(data.link))
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    async addPerspectiveLinkRemovedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#restClient.subscribe(
            `/api/v1/events/links/${encodeURIComponent(uuid as string)}`,
            (data) => {
                if (data.type === 'link-removed') {
                    const link = data.link
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
        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    async addPerspectiveLinkUpdatedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#restClient.subscribe(
            `/api/v1/events/links/${encodeURIComponent(uuid as string)}`,
            (data) => {
                if (data.type === 'link-updated') {
                    if (!data.newLink.status) {
                        delete data.newLink.status
                    }
                    if (!data.oldLink.status) {
                        delete data.oldLink.status
                    }
                    cb.forEach(c => c(data))
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await new Promise<void>(resolve => setTimeout(resolve, 500))
    }

    getNeighbourhoodProxy(uuid: string): NeighbourhoodProxy {
        return new NeighbourhoodProxy(this.#neighbourhoodClient!, uuid)
    }

    async createBatch(uuid: string): Promise<string> {
        return this.#restClient.post<string>(`/api/v1/perspectives/${encodeURIComponent(uuid)}/batch`)
    }

    async commitBatch(uuid: string, batchId: string): Promise<LinkExpressionMutations> {
        return this.#restClient.post<LinkExpressionMutations>(
            `/api/v1/perspectives/${encodeURIComponent(uuid)}/batch/commit`,
            { batchId }
        )
    }
}
