import { ApiClient, RpcError } from "../apiClient";
import { ExpressionRendered } from "../expression/Expression";
import { ExpressionClient } from "../expression/ExpressionClient";
import { Link, LinkExpressionInput, LinkExpression, LinkMutations, LinkExpressionMutations } from "../links/Links";
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

function normalizeQueryResult(raw: unknown, errorContext: string): AllInstancesResult {
    let finalResult: unknown = raw

    if (typeof finalResult === 'string') {
        try {
            finalResult = JSON.parse(finalResult)
        } catch (e) {
            console.error(errorContext, e)
        }
    }

    return finalResult as AllInstancesResult
}

export class PerspectiveClient {
    #apiClient: ApiClient
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

    constructor(baseUrl: string, token?: string, subscribe: boolean = true, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
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
        const perspectives = await this.#apiClient.call<PerspectiveHandle[]>('perspective.all')
        return perspectives.map(handle => new PerspectiveProxy(handle, this))
    }

    async byUUID(uuid: string): Promise<PerspectiveProxy|null> {
        try {
            const perspective = await this.#apiClient.call<PerspectiveHandle>('perspective.get', { uuid })
            if(!perspective) return null
            return new PerspectiveProxy(perspective, this)
        } catch(e) {
            if (e instanceof RpcError && e.status === 404) return null
            throw e
        }
    }

    async snapshotByUUID(uuid: string): Promise<Perspective|null> {
        return this.#apiClient.call<Perspective|null>('perspective.snapshot', { uuid })
    }

    async publishSnapshotByUUID(uuid: string): Promise<string|null> {
        return this.#apiClient.call<string|null>('perspective.publishSnapshot', { uuid })
    }

    async queryLinks(uuid: string, query: LinkQuery): Promise<LinkExpression[]> {
        const params: Record<string, unknown> = { uuid }
        if (query.source) params.source = query.source
        if (query.predicate) params.predicate = query.predicate
        if (query.target) params.target = query.target
        if (query.fromDate) params.fromDate = query.fromDate instanceof Date ? query.fromDate.toISOString() : String(query.fromDate)
        if (query.untilDate) params.untilDate = query.untilDate instanceof Date ? query.untilDate.toISOString() : String(query.untilDate)
        if (query.limit !== undefined) params.limit = query.limit
        return this.#apiClient.call<LinkExpression[]>('perspective.queryLinks', params)
    }

    async queryProlog(uuid: string, query: string): Promise<unknown> {
        const result = await this.#apiClient.call<string>('perspective.queryProlog', { uuid, query })
        return JSON.parse(result)
    }

    async querySparql(uuid: string, query: string): Promise<unknown> {
        const result = await this.#apiClient.call<string>('perspective.querySparql', { uuid, engine: 'sparql', query })
        return JSON.parse(result)
    }

    async subscribeQuery(uuid: string, query: string): Promise<{ subscriptionId: string, result: AllInstancesResult }> {
        const response = await this.#apiClient.call<{ subscriptionId: string, result: unknown }>(
            'perspective.subscribeQuery', { uuid, query }
        )
        const { subscriptionId, result } = response
        const parsed = normalizeQueryResult(result, 'Error parsing subscribeQuery result:')
        return { subscriptionId, result: parsed }
    }

    async perspectiveKeepAliveQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.keepAliveQuery', { uuid, subscriptionId }
        )
    }

    async perspectiveDisposeQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.disposeQuery', { uuid, subscriptionId }
        )
    }

    subscribeToQueryUpdates(subscriptionId: string, onData: (result: AllInstancesResult) => void): () => void {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                const event = data as Record<string, unknown>
                if (event.type !== 'query-subscription-update') return

                const eventSubscriptionId = event.subscriptionId || event.subscription_id
                if (eventSubscriptionId !== subscriptionId) return

                const parsed = normalizeQueryResult(event.result, 'Error parsing query subscription:')
                onData(parsed)
            }
        )
        this.#querySubscriptionUnsubscribers.set(subscriptionId, unsub)
        return unsub
    }

    async keepAliveQuery(uuid: string, subscriptionId: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.keepAliveQuery', { uuid, subscriptionId }
        )
    }

    async disposeQuerySubscription(uuid: string, subscriptionId: string): Promise<boolean> {
        const unsub = this.#querySubscriptionUnsubscribers.get(subscriptionId)
        if (unsub) {
            unsub()
            this.#querySubscriptionUnsubscribers.delete(subscriptionId)
        }
        return this.#apiClient.call<boolean>(
            'perspective.disposeQuery', { uuid, subscriptionId }
        )
    }

    async modelQuery(uuid: string, className: string, queryJson: string, shapeJson?: string): Promise<any> {
        const resultJson = await this.#apiClient.call<string>(
            'perspective.modelQuery', { uuid, class_name: className, query_json: queryJson, shape_json: shapeJson }
        )
        return JSON.parse(resultJson)
    }

    async evaluateGetters(
        uuid: string,
        className: string,
        instanceIds: string[],
        shapeJson: string,
        propertyNames?: string[],
    ): Promise<Record<string, Record<string, any>>> {
        const resultJson = await this.#apiClient.call<string>(
            'perspective.evaluateGetters', {
                uuid,
                class_name: className,
                instance_ids: instanceIds,
                shape_json: shapeJson,
                ...(propertyNames && { property_names: propertyNames }),
            }
        )
        return JSON.parse(resultJson)
    }

    async modelSubscribe(uuid: string, className: string, queryJson: string, shapeJson?: string): Promise<{ subscriptionId: string, result: any }> {
        const response = await this.#apiClient.call<{ subscription_id: string, result: string }>(
            'perspective.modelSubscribe', { uuid, class_name: className, query_json: queryJson, shape_json: shapeJson }
        )
        return {
            subscriptionId: response.subscription_id,
            result: JSON.parse(response.result)
        }
    }

    async add(name: string): Promise<PerspectiveProxy> {
        const handle = await this.#apiClient.call<PerspectiveHandle>('perspective.create', { name })
        return new PerspectiveProxy(handle, this)
    }

    async update(uuid: string, name: string): Promise<PerspectiveProxy> {
        const handle = await this.#apiClient.call<PerspectiveHandle>('perspective.update', { uuid, name })
        return new PerspectiveProxy(handle, this)
    }

    async remove(uuid: string): Promise<{perspectiveRemove: boolean}> {
        const result = await this.#apiClient.call<boolean>('perspective.remove', { uuid })
        return { perspectiveRemove: result }
    }

    async addLink(uuid: string, link: Link, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return this.#apiClient.call<LinkExpression>(
            'perspective.addLink', { uuid, link, status, batchId }
        )
    }

    async addLinks(uuid: string, links: Link[], status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression[]> {
        return this.#apiClient.call<LinkExpression[]>(
            'perspective.addLinks', { uuid, links, status, batchId }
        )
    }

    async removeLinks(uuid: string, links: LinkExpressionInput[], batchId?: string): Promise<LinkExpression[]> {
        return this.#apiClient.call<LinkExpression[]>(
            'perspective.removeLinks', { uuid, links, batchId }
        )
    }

    async linkMutations(uuid: string, mutations: LinkMutations, status?: LinkStatus): Promise<LinkExpressionMutations> {
        return this.#apiClient.call<LinkExpressionMutations>(
            'perspective.linkMutations', { uuid, mutations, status }
        )
    }

    async addLinkExpression(uuid: string, link: LinkExpression, status: LinkStatus = 'shared', batchId?: string): Promise<LinkExpression> {
        return this.#apiClient.call<LinkExpression>(
            'perspective.addLinkExpression', { uuid, link, status, batchId }
        )
    }

    async updateLink(uuid: string, oldLink: LinkExpressionInput, newLink: Link, batchId?: string): Promise<LinkExpression> {
        return this.#apiClient.call<LinkExpression>(
            'perspective.updateLink', { uuid, oldLink, newLink, batchId }
        )
    }

    async removeLink(uuid: string, link: LinkExpressionInput, batchId?: string): Promise<boolean> {
        delete link.__typename
        delete link.data.__typename
        delete link.proof.__typename
        delete link.status
        return this.#apiClient.call<boolean>(
            'perspective.removeLink', { uuid, link, batchId }
        )
    }

    async addSdna(uuid: string, name: string, sdnaCode: string | undefined, sdnaType: "subject_class" | "flow" | "custom", shaclJson?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.addSdna', { uuid, name, sdnaCode: sdnaCode || "", sdnaType, shaclJson }
        )
    }

    async addSdnaBatch(uuid: string, entries: { name: string; sdnaCode?: string; sdnaType: "subject_class" | "flow" | "custom"; shaclJson?: string }[]): Promise<boolean[]> {
        return this.#apiClient.call<boolean[]>(
            'perspective.addSdna', { uuid, entries: entries.map(e => ({ ...e, sdnaCode: e.sdnaCode || "" })) }
        )
    }

    async executeCommands(uuid: string, commands: string, expression: string, parameters: string, batchId?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.executeCommands', { uuid, commands, expression, parameters, batchId }
        )
    }

    async createSubject(uuid: string, subjectClass: string, expressionAddress: string, initialValues?: string, batchId?: string): Promise<boolean> {
        return this.#apiClient.call<boolean>(
            'perspective.createSubject', { uuid, subjectClass, expressionAddress, initialValues, batchId }
        )
    }

    async getSubjectData(uuid: string, subjectClass: string, expressionAddress: string): Promise<string> {
        return this.#apiClient.call<string>(
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

    // Subscriptions:
    addPerspectiveAddedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveAddedCallbacks.push(cb)
    }

    subscribePerspectiveAdded() {
        const unsub = this.#apiClient.subscribe((data) => {
            if (data.type === 'perspective-added') {
                this.#perspectiveAddedCallbacks.forEach(cb => cb(data.perspective as PerspectiveHandle))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addPerspectiveUpdatedListener(cb: PerspectiveHandleCallback) {
        this.#perspectiveUpdatedCallbacks.push(cb)
    }

    subscribePerspectiveUpdated() {
        const unsub = this.#apiClient.subscribe((data) => {
            if (data.type === 'perspective-updated') {
                this.#perspectiveUpdatedCallbacks.forEach(cb => cb(data.perspective as PerspectiveHandle))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addPerspectiveSyncedListener(cb: SyncStateChangeCallback) {
        this.#perspectiveSyncStateChangeCallbacks.push(cb)
    }

    async addPerspectiveSyncStateChangeListener(uuid: String, cb: SyncStateChangeCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'sync-state-change' && data.uuid === uuid) {
                    cb.forEach(c => c(data.state as PerspectiveState))
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
        const unsub = this.#apiClient.subscribe((data) => {
            if (data.type === 'perspective-removed') {
                this.#perspectiveRemovedCallbacks.forEach(cb => cb(data.uuid as string))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    async addPerspectiveLinkAddedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'link-added' && data.perspectiveUuid === uuid) {
                    cb.forEach(c => c(data.link as LinkExpression))
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await this.#apiClient.waitForSubscription()
    }

    async addPerspectiveLinkRemovedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'link-removed' && data.perspectiveUuid === uuid) {
                    const link = data.link as LinkExpression & { status?: unknown }
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
        await this.#apiClient.waitForSubscription()
    }

    async addPerspectiveLinkUpdatedListener(uuid: String, cb: LinkCallback[]): Promise<void> {
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'link-updated' && data.perspectiveUuid === uuid) {
                    const newLink = data.newLink as LinkExpression & { status?: unknown }
                    const oldLink = data.oldLink as LinkExpression & { status?: unknown }
                    if (!newLink.status) {
                        delete newLink.status
                    }
                    if (!oldLink.status) {
                        delete oldLink.status
                    }
                    cb.forEach(c => c(data as unknown as LinkExpression))
                }
            }
        )
        let existing = this.#linkUnsubscribers.get(uuid as string) || []
        existing.push(unsub)
        this.#linkUnsubscribers.set(uuid as string, existing)
        await this.#apiClient.waitForSubscription()
    }

    getNeighbourhoodProxy(uuid: string): NeighbourhoodProxy {
        return new NeighbourhoodProxy(this.#neighbourhoodClient!, uuid)
    }

    async createBatch(uuid: string): Promise<string> {
        return this.#apiClient.call<string>('perspective.createBatch', { uuid })
    }

    async commitBatch(uuid: string, batchId: string): Promise<LinkExpressionMutations> {
        return this.#apiClient.call<LinkExpressionMutations>(
            'perspective.commitBatch', { uuid, batchId }
        )
    }
}
