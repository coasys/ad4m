import {ApiClient, CallOptions } from '../apiClient'
import { Address } from "../Address"
import { DID } from "../DID"
import { OnlineAgent, TelepresenceSignalCallback } from "../language/Language"
import { Perspective, PerspectiveExpression, PerspectiveUnsignedInput } from "../perspectives/Perspective"
import { perspectiveExpressionFromWire, perspectiveToWire } from "../expression/perspectiveWire"
import type { PerspectiveExpression as WirePerspectiveExpression } from "../generated/api/PerspectiveExpression"
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle"
import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type { JoinNeighbourhoodRequest, PublishNeighbourhoodRequest } from "../generated/api"

export class NeighbourhoodClient {
    #apiClient: ApiClient
    #signalHandlers: Map<string, TelepresenceSignalCallback[]> = new Map()
    #signalUnsubscribers: Map<string, () => void> = new Map()

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
    }

    async publishFromPerspective(
        perspectiveUUID: string,
        linkLanguage: Address,
        meta: Perspective,
        options?: CallOptions,
    ): Promise<string> {
        return this.#apiClient.call('neighbourhood.publish', {
            perspectiveUuid: perspectiveUUID, linkLanguage, meta: perspectiveToWire(meta)
        }, options)
    }

    async joinFromUrl(url: string, options?: CallOptions): Promise<PerspectiveHandle> {
        return PerspectiveHandle.fromWire(await this.#apiClient.call('neighbourhood.join', { url }, options))
    }

    async otherAgents(perspectiveUUID: string): Promise<DID[]> {
        return this.#apiClient.call('neighbourhood.otherAgents', { uuid: perspectiveUUID })
    }

    async hasTelepresenceAdapter(perspectiveUUID: string): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.hasTelepresence', { uuid: perspectiveUUID })
    }

    async onlineAgents(perspectiveUUID: string): Promise<OnlineAgent[]> {
        const agents = await this.#apiClient.call('neighbourhood.onlineAgents', { uuid: perspectiveUUID })
        return agents.map(({ did, status }) => ({ did, status: perspectiveExpressionFromWire(status) }))
    }

    async setOnlineStatus(perspectiveUUID: string, status: Perspective): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.setOnlineStatus', { uuid: perspectiveUUID, status: perspectiveToWire(status) })
    }

    async setOnlineStatusU(perspectiveUUID: string, status: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.setOnlineStatus', { uuid: perspectiveUUID, status, signed: false })
    }

    async sendSignal(perspectiveUUID: string, remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.sendSignal', {
            uuid: perspectiveUUID, remoteAgentDid, payload: perspectiveToWire(payload)
        })
    }

    async sendSignalU(perspectiveUUID: string, remoteAgentDid: string, payload: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.sendSignal', {
            uuid: perspectiveUUID, remoteAgentDid, payload, signed: false
        })
    }

    async sendBroadcast(perspectiveUUID: string, payload: Perspective, loopback: boolean = false): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.sendBroadcast', {
            uuid: perspectiveUUID, payload: perspectiveToWire(payload), loopback
        })
    }

    async sendBroadcastU(perspectiveUUID: string, payload: PerspectiveUnsignedInput, loopback: boolean = false): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.sendBroadcast', {
            uuid: perspectiveUUID, payload, loopback, signed: false
        })
    }

    dispatchSignal(perspectiveUUID: string, signal: PerspectiveExpression) {
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
        const unsub = this.#apiClient.subscribe(
            (data) => {
                if (data.type === 'signal' && (data.perspective as { uuid?: string } | undefined)?.uuid === perspectiveUUID) {
                    // The `signal` event carries a PerspectiveExpression (events_ws.rs).
                    this.dispatchSignal(perspectiveUUID, perspectiveExpressionFromWire(data.signal as WirePerspectiveExpression))
                }
            },
            { types: ['signal'], perspective: perspectiveUUID },
        )
        this.#signalUnsubscribers.set(perspectiveUUID, unsub)
        await this.#apiClient.waitForSubscription()
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
            if (handlersForPerspective.length === 0) {
                this.#signalHandlers.delete(perspectiveUUID)
                const unsub = this.#signalUnsubscribers.get(perspectiveUUID)
                if (unsub) {
                    unsub()
                    this.#signalUnsubscribers.delete(perspectiveUUID)
                }
            }
        }
    }
}
