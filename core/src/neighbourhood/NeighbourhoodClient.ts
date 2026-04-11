import { RestClient } from "../restClient"
import { Address } from "../Address"
import { DID } from "../DID"
import { OnlineAgent, TelepresenceSignalCallback } from "../language/Language"
import { Perspective, PerspectiveUnsignedInput } from "../perspectives/Perspective"
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle"
import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type { JoinNeighbourhoodRequest, PublishNeighbourhoodRequest } from "../generated/rest"

export class NeighbourhoodClient {
    #restClient: RestClient
    #signalHandlers: Map<string, TelepresenceSignalCallback[]> = new Map()
    #signalUnsubscribers: Map<string, () => void> = new Map()

    constructor(baseUrl: string, token?: string, sharedRestClient?: RestClient) {
        this.#restClient = sharedRestClient || new RestClient(baseUrl, token)
    }

    async publishFromPerspective(
        perspectiveUUID: string,
        linkLanguage: Address,
        meta: Perspective
    ): Promise<string> {
        return this.#restClient.post<string>('/api/v1/neighbourhoods/publish', {
            perspectiveUUID, linkLanguage, meta
        })
    }

    async joinFromUrl(url: string): Promise<PerspectiveHandle> {
        return this.#restClient.post<PerspectiveHandle>('/api/v1/neighbourhoods/join', { url })
    }

    async otherAgents(perspectiveUUID: string): Promise<DID[]> {
        return this.#restClient.get<DID[]>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/other-agents`)
    }

    async hasTelepresenceAdapter(perspectiveUUID: string): Promise<boolean> {
        return this.#restClient.get<boolean>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/has-telepresence`)
    }

    async onlineAgents(perspectiveUUID: string): Promise<OnlineAgent[]> {
        return this.#restClient.get<OnlineAgent[]>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/online-agents`)
    }

    async setOnlineStatus(perspectiveUUID: string, status: Perspective): Promise<boolean> {
        return this.#restClient.put<boolean>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/online-status`, { status })
    }

    async setOnlineStatusU(perspectiveUUID: string, status: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#restClient.put<boolean>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/online-status`, { status, signed: false })
    }

    async sendSignal(perspectiveUUID: string, remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        return this.#restClient.post<boolean>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/signal`, {
            remoteAgentDid, payload
        })
    }

    async sendSignalU(perspectiveUUID: string, remoteAgentDid: string, payload: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#restClient.post<boolean>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/signal`, {
            remoteAgentDid, payload, signed: false
        })
    }

    async sendBroadcast(perspectiveUUID: string, payload: Perspective, loopback: boolean = false): Promise<boolean> {
        return this.#restClient.post<boolean>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/broadcast`, {
            payload, loopback
        })
    }

    async sendBroadcastU(perspectiveUUID: string, payload: PerspectiveUnsignedInput, loopback: boolean = false): Promise<boolean> {
        return this.#restClient.post<boolean>(`/api/v1/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/broadcast`, {
            payload, loopback, signed: false
        })
    }

    dispatchSignal(perspectiveUUID: string, signal: unknown) {
        const handlers = this.#signalHandlers.get(perspectiveUUID)
        if (handlers) {
            for (const handler of handlers) {
                try {
                    handler(signal)
                } catch(e) {
                    console.error("Error in signal handler:", e)
                }
            }
        }
    }

    async subscribeToSignals(perspectiveUUID: string): Promise<void> {
        const unsub = this.#restClient.subscribe(
            `/api/v1/events/neighbourhoods/${encodeURIComponent(perspectiveUUID)}/signals`,
            (data) => {
                if (data.type === 'signal') {
                    this.dispatchSignal(perspectiveUUID, data.signal)
                }
            }
        )
        this.#signalUnsubscribers.set(perspectiveUUID, unsub)
    }

    async addSignalHandler(perspectiveUUID: string, handler: TelepresenceSignalCallback): Promise<void> {
        let handlersForPerspective = this.#signalHandlers.get(perspectiveUUID)
        if (!handlersForPerspective) {
            handlersForPerspective = []
            this.#signalHandlers.set(perspectiveUUID, handlersForPerspective)
            handlersForPerspective.push(handler)
            await this.subscribeToSignals(perspectiveUUID)
        } else {
            handlersForPerspective.push(handler)
        }
    }

    removeSignalHandler(perspectiveUUID: string, handler: TelepresenceSignalCallback): void {
        const handlersForPerspective = this.#signalHandlers.get(perspectiveUUID)
        if (handlersForPerspective) {
            const index = handlersForPerspective.indexOf(handler)
            if (index > -1) {
                handlersForPerspective.splice(index, 1)
            }
        }
    }
}
