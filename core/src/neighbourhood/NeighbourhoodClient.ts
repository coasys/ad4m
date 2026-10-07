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

    // ── SFU discovery ──────────────────────────────────────────────────

    /**
     * Discover SFU-capable nodes in a neighbourhood by scanning online
     * agents' presence links for the `ad4m://sfu/available` predicate.
     *
     * Returns an array of `{ did, bindAddress }` for each agent whose
     * executor advertises a publicly reachable SFU.  Empty when no
     * agents with public SFU capability appear in the neighbourhood.
     *
     * @param perspectiveId  The neighbourhood's perspective handle UUID.
     */
    async availableSfuNodes(
        perspectiveId: string,
    ): Promise<{ did: string; bindAddress: string }[]> {
        const SFU_PREDICATE = "ad4m://sfu/available"
        const agents = await this.onlineAgents(perspectiveId)
        const nodes: { did: string; bindAddress: string }[] = []
        for (const agent of agents) {
            // The status is a signed perspective: its links sit under `data`.
            for (const link of agent.status?.data?.links ?? []) {
                const l = link.data
                if (l?.predicate === SFU_PREDICATE && l.target) {
                    nodes.push({ did: agent.did, bindAddress: l.target })
                }
            }
        }
        return nodes
    }

    // ── SFU diagnostic / test-harness endpoints ────────────────────────

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
