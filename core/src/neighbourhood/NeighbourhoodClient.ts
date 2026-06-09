import { ApiClient } from "../apiClient"
import { Address } from "../Address"
import { DID } from "../DID"
import { OnlineAgent, TelepresenceSignalCallback } from "../language/Language"
import { Perspective, PerspectiveUnsignedInput } from "../perspectives/Perspective"
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle"
import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type { JoinNeighbourhoodRequest, PublishNeighbourhoodRequest } from "../generated/api"
import type {
    CallSessionInfo,
    SfuConfig,
    SfuQualityPreference,
    SfuRoomInfo,
} from "./SfuTypes"

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
        meta: Perspective
    ): Promise<string> {
        return this.#apiClient.call<string>('neighbourhood.publish', {
            perspectiveUUID, linkLanguage, meta
        })
    }

    async joinFromUrl(url: string): Promise<PerspectiveHandle> {
        return this.#apiClient.call<PerspectiveHandle>('neighbourhood.join', { url })
    }

    async otherAgents(perspectiveUUID: string): Promise<DID[]> {
        return this.#apiClient.call<DID[]>('neighbourhood.otherAgents', { uuid: perspectiveUUID })
    }

    async hasTelepresenceAdapter(perspectiveUUID: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('neighbourhood.hasTelepresence', { uuid: perspectiveUUID })
    }

    async onlineAgents(perspectiveUUID: string): Promise<OnlineAgent[]> {
        return this.#apiClient.call<OnlineAgent[]>('neighbourhood.onlineAgents', { uuid: perspectiveUUID })
    }

    async setOnlineStatus(perspectiveUUID: string, status: Perspective): Promise<boolean> {
        return this.#apiClient.call<boolean>('neighbourhood.setOnlineStatus', { uuid: perspectiveUUID, status })
    }

    async setOnlineStatusU(perspectiveUUID: string, status: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#apiClient.call<boolean>('neighbourhood.setOnlineStatus', { uuid: perspectiveUUID, status, signed: false })
    }

    async sendSignal(perspectiveUUID: string, remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        return this.#apiClient.call<boolean>('neighbourhood.sendSignal', {
            uuid: perspectiveUUID, remoteAgentDid, payload
        })
    }

    async sendSignalU(perspectiveUUID: string, remoteAgentDid: string, payload: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#apiClient.call<boolean>('neighbourhood.sendSignal', {
            uuid: perspectiveUUID, remoteAgentDid, payload, signed: false
        })
    }

    async sendBroadcast(perspectiveUUID: string, payload: Perspective, loopback: boolean = false): Promise<boolean> {
        return this.#apiClient.call<boolean>('neighbourhood.sendBroadcast', {
            uuid: perspectiveUUID, payload, loopback
        })
    }

    async sendBroadcastU(perspectiveUUID: string, payload: PerspectiveUnsignedInput, loopback: boolean = false): Promise<boolean> {
        return this.#apiClient.call<boolean>('neighbourhood.sendBroadcast', {
            uuid: perspectiveUUID, payload, loopback, signed: false
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
        const unsub = this.#apiClient.subscribe(
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

    // ── SFU (Selective Forwarding Unit) ─────────────────────────────────
    //
    // These wrap the `sfu.*` WS RPC handlers in
    // `rust-executor/src/api/sfu_ws.rs`.  The transport is the same
    // shared `ApiClient`; the only twist is that `callJoin` returns a
    // structured `CallSessionInfo` (SDP answer + optional cascade
    // redirect + stream mapping).

    async sfuStartRoom(neighbourhoodUrl: string, roomName: string): Promise<SfuRoomInfo> {
        return this.#apiClient.call<SfuRoomInfo>("sfu.startRoom", { neighbourhoodUrl, roomName })
    }

    async sfuStopRoom(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.stopRoom", { neighbourhoodUrl, roomName })
    }

    async sfuListRooms(): Promise<SfuRoomInfo[]> {
        return this.#apiClient.call<SfuRoomInfo[]>("sfu.listRooms", {})
    }

    async sfuCallJoin(
        neighbourhoodUrl: string,
        roomName: string,
        sdpOffer: string,
    ): Promise<CallSessionInfo> {
        return this.#apiClient.call<CallSessionInfo>("sfu.callJoin", {
            neighbourhoodUrl,
            roomName,
            sdpOffer,
        })
    }

    async sfuCallLeave(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.callLeave", { neighbourhoodUrl, roomName })
    }

    async sfuCallSetQualityPreference(
        neighbourhoodUrl: string,
        roomName: string,
        preference: SfuQualityPreference,
    ): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.callSetQualityPreference", {
            neighbourhoodUrl,
            roomName,
            preference,
        })
    }

    async sfuCallAnswerServerOffer(
        neighbourhoodUrl: string,
        roomName: string,
        sdpAnswer: string,
    ): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.callAnswerServerOffer", {
            neighbourhoodUrl,
            roomName,
            sdpAnswer,
        })
    }

    async sfuGetConfig(neighbourhoodUrl: string): Promise<SfuConfig> {
        return this.#apiClient.call<SfuConfig>("sfu.getConfig", { neighbourhoodUrl })
    }

    async sfuSetConfig(neighbourhoodUrl: string, config: SfuConfig): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.setConfig", { neighbourhoodUrl, config })
    }

    async sfuPeerForNeighbourhood(neighbourhoodUrl: string): Promise<string | null> {
        return this.#apiClient.call<string | null>("sfu.sfuPeerForNeighbourhood", {
            neighbourhoodUrl,
        })
    }

    async sfuPeersForNeighbourhood(neighbourhoodUrl: string): Promise<string[]> {
        return this.#apiClient.call<string[]>("sfu.sfuPeersForNeighbourhood", { neighbourhoodUrl })
    }
}
