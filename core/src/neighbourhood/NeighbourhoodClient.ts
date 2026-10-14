import {ApiClient, CallOptions } from '../apiClient'
import { Address } from "../Address"
import { DID } from "../DID"
import { OnlineAgent, TelepresenceSignalCallback } from "../language/Language"
import { Perspective, PerspectiveExpression, PerspectiveUnsignedInput } from "../perspectives/Perspective"
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle"
import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type {
    CallSessionInfo,
    SfuCallRenegotiationOffer,
    SfuCascadeStatus,
    SfuConfig,
    SfuDataMessage,
    SfuMigrateEvent,
    SfuParticipantQualityPreference,
    SfuQualityPreference,
    SfuRoomInfo,
    SfuRoomRoster,
    SfuStatus,
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
        meta: Perspective,
        options?: CallOptions,
    ): Promise<string> {
        return this.#apiClient.call('neighbourhood.publish', {
            perspectiveUuid: perspectiveUUID, linkLanguage, meta: Perspective.toWire(meta)
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
        return agents.map(({ did, status }) => ({ did, status: PerspectiveExpression.fromWire(status) }))
    }

    /** The caller's own online status, as it last set it on this executor, or `null`. */
    async ownOnlineStatus(perspectiveUUID: string): Promise<PerspectiveExpression | null> {
        const status = await this.#apiClient.call('neighbourhood.ownOnlineStatus', { uuid: perspectiveUUID })
        return status ? PerspectiveExpression.fromWire(status) : null
    }

    async setOnlineStatus(perspectiveUUID: string, status: Perspective): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.setOnlineStatus', { uuid: perspectiveUUID, status: Perspective.toWire(status) })
    }

    async setOnlineStatusU(perspectiveUUID: string, status: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.setOnlineStatus', { uuid: perspectiveUUID, status, signed: false })
    }

    async sendSignal(perspectiveUUID: string, remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.sendSignal', {
            uuid: perspectiveUUID, remoteAgentDid, payload: Perspective.toWire(payload)
        })
    }

    async sendSignalU(perspectiveUUID: string, remoteAgentDid: string, payload: PerspectiveUnsignedInput): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.sendSignal', {
            uuid: perspectiveUUID, remoteAgentDid, payload, signed: false
        })
    }

    async sendBroadcast(perspectiveUUID: string, payload: Perspective, loopback: boolean = false): Promise<boolean> {
        return this.#apiClient.call('neighbourhood.sendBroadcast', {
            uuid: perspectiveUUID, payload: Perspective.toWire(payload), loopback
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
        const unsub = this.#apiClient.on(
            'signal',
            (event) => this.dispatchSignal(perspectiveUUID, PerspectiveExpression.fromWire(event.signal)),
            { perspective: perspectiveUUID },
        )
        this.#signalUnsubscribers.set(perspectiveUUID, unsub)
        await this.#apiClient.watchApplied()
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
        return this.#apiClient.call("sfu.startRoom", { neighbourhoodUrl, roomName })
    }

    async sfuStopRoom(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return this.#apiClient.call("sfu.stopRoom", { neighbourhoodUrl, roomName })
    }

    async sfuListRooms(): Promise<SfuRoomInfo[]> {
        return this.#apiClient.call("sfu.listRooms", {})
    }

    async sfuCallJoin(
        neighbourhoodUrl: string,
        roomName: string,
        sdpOffer: string,
    ): Promise<CallSessionInfo> {
        return this.#apiClient.call("sfu.callJoin", {
            neighbourhoodUrl,
            roomName,
            sdpOffer,
        })
    }

    async sfuCallLeave(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return this.#apiClient.call("sfu.callLeave", { neighbourhoodUrl, roomName })
    }

    async sfuCallSetQualityPreference(
        neighbourhoodUrl: string,
        roomName: string,
        preference: SfuQualityPreference,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.callSetQualityPreference", {
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
        return this.#apiClient.call("sfu.callAnswerServerOffer", {
            neighbourhoodUrl,
            roomName,
            sdpAnswer,
        })
    }

    async sfuGetConfig(neighbourhoodUrl: string): Promise<SfuConfig> {
        return this.#apiClient.call("sfu.getConfig", { neighbourhoodUrl })
    }

    async sfuSetConfig(neighbourhoodUrl: string, config: SfuConfig): Promise<boolean> {
        return this.#apiClient.call("sfu.setConfig", { neighbourhoodUrl, config })
    }

    async sfuPeerForNeighbourhood(neighbourhoodUrl: string): Promise<string | null> {
        return this.#apiClient.call("sfu.sfuPeerForNeighbourhood", { neighbourhoodUrl })
    }

    async sfuPeersForNeighbourhood(neighbourhoodUrl: string): Promise<string[]> {
        return this.#apiClient.call("sfu.sfuPeersForNeighbourhood", { neighbourhoodUrl })
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
    async sfuAddIceCandidate(
        neighbourhoodUrl: string,
        roomName: string,
        candidate: string,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.addIceCandidate", {
            neighbourhoodUrl,
            roomName,
            candidate,
        })
    }

    // ── Data channel relay ────────────────────────────────────────────

    /**
     * Send data through the SFU to all other participants in the room.
     * The server relays it to their matching data channel and
     * publishes it as an `sfu-data` event.
     */
    async sfuSendData(
        neighbourhoodUrl: string,
        roomName: string,
        channelLabel: string,
        data: string,
        binary: boolean = false,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.sendData", {
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
    subscribeSfuDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void {
        // A fresh handler per call: `on` merges a repeated handler, and
        // each subscription must own its unsubscribe.
        return this.#apiClient.on("sfu-data", (message) => callback(message))
    }

    /** Subscribe to the rosters of the call rooms this agent is in: who is in each, whenever that changes. */
    subscribeSfuRoomRoster(
        callback: (roster: SfuRoomRoster) => void,
    ): () => void {
        return this.#apiClient.on("sfu-room-roster", (roster) => callback(roster))
    }

    /**
     * Subscribe to server-pushed SFU SDP renegotiation offers.  The
     * server publishes an `sfu-call-renegotiation-offer` event every
     * time the relay's outbound track set changes for `targetDid`.
     * Callers apply the offer to their `RTCPeerConnection`, generate an
     * answer, and post it via `sfuCallAnswerServerOffer`.
     *
     * The executor already sends each socket only its own DID's offers;
     * this subscription additionally filters on `targetDid` for safety.
     * Returns an unsubscribe function.
     */
    subscribeSfuCallRenegotiationOffer(
        targetDid: string,
        callback: (payload: SfuCallRenegotiationOffer) => void,
    ): () => void {
        return this.#apiClient.on("sfu-call-renegotiation-offer", (event) => {
            if (event.targetDid !== targetDid) return
            callback({
                targetDid: event.targetDid,
                neighbourhoodUrl: event.neighbourhoodUrl,
                roomName: event.roomName,
                sdpOffer: event.sdpOffer,
                trackMapping: event.trackMapping,
            })
        })
    }

    /**
     * Subscribe to cascade rebalance migration events for `targetDid`.
     * The server publishes an `sfu-migrate` event when the cascade
     * rebalancer decides a participant should move to a less-loaded
     * node.  Returns an unsubscribe function.
     */
    subscribeSfuMigrateEvent(
        targetDid: string,
        callback: (payload: SfuMigrateEvent) => void,
    ): () => void {
        return this.#apiClient.on("sfu-migrate", (event) => {
            if (event.targetDid !== targetDid) return
            callback({
                targetDid: event.targetDid,
                neighbourhoodUrl: event.neighbourhoodUrl,
                roomName: event.roomName,
                migrateToDid: event.migrateToDid,
            })
        })
    }

    // ── SFU topology ───────────────────────────────────────────────────

    /**
     * Whether every member of the neighbourhood is hosted on this
     * executor — the members its SFU can serve. A call's "auto" topology
     * uses the SFU only then: anyone else could not join the same room.
     *
     * @param perspectiveId  The neighbourhood's perspective handle UUID.
     */
    async sfuServesAllMembers(perspectiveId: string): Promise<boolean> {
        return this.#apiClient.call("sfu.servesAllMembers", { uuid: perspectiveId })
    }

    // ── SFU diagnostic / test-harness endpoints ────────────────────────

    /** The calling agent's own DID — the user's, for a user token. */
    async callerDid(): Promise<string | null> {
        return (await this.#apiClient.call("agent.status", {})).did
    }

    /**
     * Read-only: SFU service status including public reachability.
     * Returns whether this executor can relay media to remote
     * participants (public), sits behind NAT (nat), or could not
     * determine its reachability (unknown).
     */
    async sfuStatus(): Promise<SfuStatus> {
        return this.#apiClient.call("sfu.status", {})
    }

    /**
     * Read-only: how many SFU↔SFU pipe transports are fully established,
     * plus the list of pipes.  Useful for diagnostics and wind-tunnel
     * assertions.
     */
    async sfuCascadeStatus(): Promise<SfuCascadeStatus> {
        return this.#apiClient.call("sfu.cascadeStatus", {})
    }

    /**
     * Read-only: per-participant quality preferences the SFU event loop
     * currently holds.  Returns `[{participantId, preference}, ...]`.
     */
    async sfuQualityPreferences(): Promise<SfuParticipantQualityPreference[]> {
        return this.#apiClient.call("sfu.qualityPreferences", {})
    }

    /**
     * Register a DID as a neighbourhood member on this executor.
     * In production the neighbourhood join flow handles this
     * automatically; this RPC exists for test harnesses and bridge
     * deployments.
     */
    async sfuEnsureMembership(
        neighbourhoodUrl: string,
        did: string,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.ensureMembership", {
            neighbourhoodUrl,
            did,
        })
    }
}
