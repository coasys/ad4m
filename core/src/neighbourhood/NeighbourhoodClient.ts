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
    SfuDataMessage,
    SfuQualityPreference,
    SfuRoomInfo,
    TrackMapEntry,
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

    async startRoom(neighbourhoodUrl: string, roomName: string): Promise<SfuRoomInfo> {
        return this.#apiClient.call<SfuRoomInfo>("sfu.startRoom", { neighbourhoodUrl, roomName })
    }

    async stopRoom(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.stopRoom", { neighbourhoodUrl, roomName })
    }

    async listRooms(): Promise<SfuRoomInfo[]> {
        return this.#apiClient.call<SfuRoomInfo[]>("sfu.listRooms", {})
    }

    async callJoin(
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

    async callLeave(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.callLeave", { neighbourhoodUrl, roomName })
    }

    async callSetQualityPreference(
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

    async callAnswerServerOffer(
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

    async getConfig(neighbourhoodUrl: string): Promise<SfuConfig> {
        return this.#apiClient.call<SfuConfig>("sfu.getConfig", { neighbourhoodUrl })
    }

    async setConfig(neighbourhoodUrl: string, config: SfuConfig): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.setConfig", { neighbourhoodUrl, config })
    }

    async peerForNeighbourhood(neighbourhoodUrl: string): Promise<string | null> {
        return this.#apiClient.call<string | null>("sfu.sfuPeerForNeighbourhood", {
            neighbourhoodUrl,
        })
    }

    async peersForNeighbourhood(neighbourhoodUrl: string): Promise<string[]> {
        return this.#apiClient.call<string[]>("sfu.sfuPeersForNeighbourhood", { neighbourhoodUrl })
    }

    // ── Trickle ICE ───────────────────────────────────────────────────

    /**
     * Add a remote ICE candidate to an existing SFU call.  Enables
     * trickle ICE: the client sends its SDP offer immediately after
     * `setLocalDescription` and then calls this method for each
     * candidate as it arrives, rather than waiting for gathering to
     * complete (which can take up to 8 seconds on restrictive
     * networks).
     */
    async addIceCandidate(
        neighbourhoodUrl: string,
        roomName: string,
        candidate: string,
    ): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.addIceCandidate", {
            neighbourhoodUrl,
            roomName,
            candidate,
        })
    }

    // ── Data channel relay ────────────────────────────────────────────

    /**
     * Send data through the SFU to all other participants in the room.
     * The server relays it to their matching data channel and
     * publishes it on the `sfu-data` events_ws topic.
     */
    async sendData(
        neighbourhoodUrl: string,
        roomName: string,
        channelLabel: string,
        data: string,
        binary: boolean = false,
    ): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.sendData", {
            neighbourhoodUrl,
            roomName,
            channelLabel,
            data,
            binary,
        })
    }

    /**
     * Subscribe to SFU data channel messages.  Returns an unsubscribe
     * function.  Messages arrive for every participant in the room;
     * filter by `senderDid` if needed.
     */
    subscribeDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void {
        return this.#apiClient.subscribe((data: any) => {
            if (data?.type !== "sfu-data") return
            callback(data as SfuDataMessage)
        })
    }

    /**
     * Subscribe to server-pushed SFU SDP renegotiation offers.  The
     * server publishes `sfu-call-renegotiation-offer` events on the
     * events_ws every time the relay's outbound track set changes for
     * `targetDid`.  Callers apply the offer to their `RTCPeerConnection`,
     * generate an answer, and post it via `callAnswerServerOffer`.
     *
     * The events_ws fanout already filters per-DID; this subscription
     * additionally double-filters on `targetDid` for safety.  Returns
     * an unsubscribe function.
     */
    subscribeCallRenegotiationOffer(
        targetDid: string,
        callback: (payload: {
            targetDid: string
            neighbourhoodUrl: string
            roomName: string
            sdpOffer: string
            trackMapping?: TrackMapEntry[]
        }) => void,
    ): () => void {
        return this.#apiClient.subscribe((data: any) => {
            if (data?.type !== "sfu-call-renegotiation-offer") return
            const payload = data as {
                type: string
                targetDid: string
                neighbourhoodUrl: string
                roomName: string
                sdpOffer: string
                trackMapping?: TrackMapEntry[]
            }
            if (payload.targetDid !== targetDid) return
            callback({
                targetDid: payload.targetDid,
                neighbourhoodUrl: payload.neighbourhoodUrl,
                roomName: payload.roomName,
                sdpOffer: payload.sdpOffer,
                trackMapping: payload.trackMapping,
            })
        })
    }

    /**
     * Subscribe to cascade rebalance migration events for `targetDid`.
     * The server publishes `sfu-migrate` events on the events_ws when
     * the cascade rebalancer decides a participant should move to a
     * less-loaded node.  Returns an unsubscribe function.
     */
    subscribeMigrateEvent(
        targetDid: string,
        callback: (payload: {
            targetDid: string
            neighbourhoodUrl: string
            roomName: string
            migrateToDid: string
        }) => void,
    ): () => void {
        return this.#apiClient.subscribe((data: any) => {
            if (data?.type !== "sfu-migrate") return
            const payload = data as {
                type: string
                targetDid: string
                neighbourhoodUrl: string
                roomName: string
                migrateToDid: string
            }
            if (payload.targetDid !== targetDid) return
            callback({
                targetDid: payload.targetDid,
                neighbourhoodUrl: payload.neighbourhoodUrl,
                roomName: payload.roomName,
                migrateToDid: payload.migrateToDid,
            })
        })
    }

    // ── SFU diagnostic / test-harness endpoints ────────────────────────

    /**
     * Read-only: how many SFU↔SFU pipe transports are fully established,
     * plus the list of pipes.  Useful for diagnostics and wind-tunnel
     * assertions.
     */
    async cascadeStatus(): Promise<{
        establishedCount: number
        pipes: { roomId: string; remoteDid: string }[]
    }> {
        return this.#apiClient.call("sfu.cascadeStatus", {})
    }

    /**
     * Read-only: per-participant quality preferences the SFU event loop
     * currently holds.  Returns `[{participantId, preference}, ...]`.
     */
    async qualityPreferences(): Promise<
        { participantId: string; preference: string }[]
    > {
        return this.#apiClient.call("sfu.qualityPreferences", {})
    }

    /**
     * Register a DID as a neighbourhood member on this executor.
     * In production the neighbourhood join flow handles this
     * automatically; this RPC exists for test harnesses and bridge
     * deployments.
     */
    async ensureMembership(
        neighbourhoodUrl: string,
        did: string,
    ): Promise<boolean> {
        return this.#apiClient.call<boolean>("sfu.ensureMembership", {
            neighbourhoodUrl,
            did,
        })
    }
}
