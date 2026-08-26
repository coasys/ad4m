import { DID } from "../DID";
import { OnlineAgent } from "../language/Language";
import { Perspective, PerspectiveExpression, PerspectiveUnsignedInput } from "../perspectives/Perspective";
import { NeighbourhoodClient } from "./NeighbourhoodClient";
import type {
    CallSessionInfo,
    SfuConfig,
    SfuDataMessage,
    SfuQualityPreference,
    SfuRoomInfo,
    TrackMapEntry,
} from "./SfuTypes";

export class NeighbourhoodProxy {
    #client: NeighbourhoodClient
    #pID: string

    constructor(client: NeighbourhoodClient, pID: string) {
        this.#client = client
        this.#pID = pID
    }

    async otherAgents(): Promise<DID[]> {
        return await this.#client.otherAgents(this.#pID)
    }

    async hasTelepresenceAdapter(): Promise<boolean> {
        return await this.#client.hasTelepresenceAdapter(this.#pID)
    }

    async onlineAgents(): Promise<OnlineAgent[]> {
        return await this.#client.onlineAgents(this.#pID)
    }

    async setOnlineStatus(status: Perspective): Promise<boolean> {
        return await this.#client.setOnlineStatus(this.#pID, status)
    }

    async setOnlineStatusU(status: PerspectiveUnsignedInput): Promise<boolean> {
        return await this.#client.setOnlineStatusU(this.#pID, status)
    }

    async sendSignal(remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        return await this.#client.sendSignal(this.#pID, remoteAgentDid, payload)
    }

    async sendSignalU(remoteAgentDid: string, payload: PerspectiveUnsignedInput): Promise<boolean> {
        return await this.#client.sendSignalU(this.#pID, remoteAgentDid, payload)
    }

    async sendBroadcast(payload: Perspective, loopback: boolean = false): Promise<boolean> {
        return await this.#client.sendBroadcast(this.#pID, payload, loopback)
    }

    async sendBroadcastU(payload: PerspectiveUnsignedInput, loopback: boolean = false): Promise<boolean> {
        return await this.#client.sendBroadcastU(this.#pID, payload, loopback)
    }

    async addSignalHandler(handler: (payload: PerspectiveExpression) => void): Promise<void> {
        await this.#client.addSignalHandler(this.#pID, handler)
    }

    removeSignalHandler(handler: (payload: PerspectiveExpression) => void) {
        this.#client.removeSignalHandler(this.#pID, handler)
    }

    // ── SFU (Selective Forwarding Unit) ─────────────────────────────────
    //
    // Thin pass-through to `NeighbourhoodClient.sfu*`.  Each method takes
    // the neighbourhood URL explicitly — the proxy is per-perspective,
    // not per-neighbourhood, and the URL is owned by the consumer (Flux's
    // call context, the WE UI, etc).

    async sfuConfig(neighbourhoodUrl: string): Promise<SfuConfig> {
        return await this.#client.sfuGetConfig(neighbourhoodUrl)
    }

    async setSfuConfig(neighbourhoodUrl: string, config: SfuConfig): Promise<boolean> {
        return await this.#client.sfuSetConfig(neighbourhoodUrl, config)
    }

    async sfuPeer(neighbourhoodUrl: string): Promise<string | null> {
        return await this.#client.sfuPeerForNeighbourhood(neighbourhoodUrl)
    }

    async sfuPeers(neighbourhoodUrl: string): Promise<string[]> {
        return await this.#client.sfuPeersForNeighbourhood(neighbourhoodUrl)
    }

    async sfuStartRoom(neighbourhoodUrl: string, roomName: string): Promise<SfuRoomInfo> {
        return await this.#client.sfuStartRoom(neighbourhoodUrl, roomName)
    }

    async sfuStopRoom(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return await this.#client.sfuStopRoom(neighbourhoodUrl, roomName)
    }

    async sfuListRooms(): Promise<SfuRoomInfo[]> {
        return await this.#client.sfuListRooms()
    }

    async callJoin(
        neighbourhoodUrl: string,
        roomName: string,
        sdpOffer: string,
    ): Promise<CallSessionInfo> {
        return await this.#client.sfuCallJoin(neighbourhoodUrl, roomName, sdpOffer)
    }

    async callLeave(neighbourhoodUrl: string, roomName: string): Promise<boolean> {
        return await this.#client.sfuCallLeave(neighbourhoodUrl, roomName)
    }

    async callSetQualityPreference(
        neighbourhoodUrl: string,
        roomName: string,
        preference: SfuQualityPreference,
    ): Promise<boolean> {
        return await this.#client.sfuCallSetQualityPreference(
            neighbourhoodUrl,
            roomName,
            preference,
        )
    }

    async callAnswerServerOffer(
        neighbourhoodUrl: string,
        roomName: string,
        sdpAnswer: string,
    ): Promise<boolean> {
        return await this.#client.sfuCallAnswerServerOffer(neighbourhoodUrl, roomName, sdpAnswer)
    }

    /**
     * Subscribe to server-pushed renegotiation offers for `targetDid`.
     * Returns an unsubscribe function.  See
     * `NeighbourhoodClient.subscribeSfuCallRenegotiationOffer` for the
     * underlying mechanism.
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
        return this.#client.subscribeSfuCallRenegotiationOffer(targetDid, callback)
    }

    /**
     * Subscribe to cascade rebalance migration events for `targetDid`.
     * Returns an unsubscribe function.
     */
    subscribeMigrateEvent(
        targetDid: string,
        callback: (event: {
            targetDid: string
            neighbourhoodUrl: string
            roomName: string
            migrateToDid: string
        }) => void,
    ): () => void {
        return this.#client.subscribeSfuMigrateEvent(targetDid, callback)
    }

    // ── SFU trickle ICE ─────────────────────────────────────────────────

    async addIceCandidate(
        neighbourhoodUrl: string,
        roomName: string,
        candidate: string,
    ): Promise<boolean> {
        return await this.#client.sfuAddIceCandidate(neighbourhoodUrl, roomName, candidate)
    }

    // ── SFU data channel relay ────────────────────────────────────────

    async sendData(
        neighbourhoodUrl: string,
        roomName: string,
        channelLabel: string,
        data: string,
        binary: boolean = false,
    ): Promise<boolean> {
        return await this.#client.sfuSendData(
            neighbourhoodUrl,
            roomName,
            channelLabel,
            data,
            binary,
        )
    }

    subscribeDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void {
        return this.#client.subscribeSfuDataChannel(callback)
    }

    // ── SFU diagnostic / test-harness ──────────────────────────────────

    async sfuCascadeStatus(): Promise<{
        establishedCount: number
        pipes: { roomId: string; remoteDid: string }[]
    }> {
        return await this.#client.sfuCascadeStatus()
    }

    async sfuQualityPreferences(): Promise<
        { participantId: string; preference: string }[]
    > {
        return await this.#client.sfuQualityPreferences()
    }

    async sfuEnsureMembership(
        neighbourhoodUrl: string,
        did: string,
    ): Promise<boolean> {
        return await this.#client.sfuEnsureMembership(neighbourhoodUrl, did)
    }
}
