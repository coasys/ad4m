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
import { createSession, type Session, type SessionCreateOptions } from "./Session";

export class NeighbourhoodProxy {
    #client: NeighbourhoodClient
    #pID: string
    #agentDid: string
    #sessions: Session[] = []

    constructor(client: NeighbourhoodClient, pID: string, agentDid: string = "") {
        this.#client = client
        this.#pID = pID
        this.#agentDid = agentDid
    }

    // ── Telepresence ────────────────────────────────────────────────────
    //
    // Link-language ephemeral messaging: presence, signalling, broadcast.
    // Routes through the neighbourhood's link language.

    /** List DIDs of agents who have joined this neighbourhood. */
    async otherAgents(): Promise<DID[]> {
        return await this.#client.otherAgents(this.#pID)
    }

    /** Check whether the neighbourhood's link language supports telepresence. */
    async hasTelepresenceAdapter(): Promise<boolean> {
        return await this.#client.hasTelepresenceAdapter(this.#pID)
    }

    /** List agents currently online in this neighbourhood. */
    async onlineAgents(): Promise<OnlineAgent[]> {
        return await this.#client.onlineAgents(this.#pID)
    }

    /** Set this agent's online status with a signed perspective payload. */
    async setOnlineStatus(status: Perspective): Promise<boolean> {
        return await this.#client.setOnlineStatus(this.#pID, status)
    }

    /** Set this agent's online status with an unsigned perspective payload. */
    async setOnlineStatusU(status: PerspectiveUnsignedInput): Promise<boolean> {
        return await this.#client.setOnlineStatusU(this.#pID, status)
    }

    /** Send a signed signal to a specific remote agent via the link language. */
    async sendSignal(remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        return await this.#client.sendSignal(this.#pID, remoteAgentDid, payload)
    }

    /** Send an unsigned signal to a specific remote agent via the link language. */
    async sendSignalU(remoteAgentDid: string, payload: PerspectiveUnsignedInput): Promise<boolean> {
        return await this.#client.sendSignalU(this.#pID, remoteAgentDid, payload)
    }

    /** Broadcast a signed signal to all agents in the neighbourhood. */
    async sendBroadcast(payload: Perspective, loopback: boolean = false): Promise<boolean> {
        return await this.#client.sendBroadcast(this.#pID, payload, loopback)
    }

    /** Broadcast an unsigned signal to all agents in the neighbourhood. */
    async sendBroadcastU(payload: PerspectiveUnsignedInput, loopback: boolean = false): Promise<boolean> {
        return await this.#client.sendBroadcastU(this.#pID, payload, loopback)
    }

    /** Register a handler for incoming telepresence signals. */
    async addSignalHandler(handler: (payload: PerspectiveExpression) => void): Promise<void> {
        await this.#client.addSignalHandler(this.#pID, handler)
    }

    /** Remove a previously registered signal handler. */
    removeSignalHandler(handler: (payload: PerspectiveExpression) => void) {
        this.#client.removeSignalHandler(this.#pID, handler)
    }

    // ── Sessions ────────────────────────────────────────────────────────
    //
    // WebRTC media sessions wrapping mesh and SFU topologies.
    // The session surface handles topology resolution, SDP negotiation,
    // cascade redirects, and failover internally.

    /** Active sessions in this neighbourhood. Destroyed sessions remove themselves. */
    get sessions(): ReadonlyArray<Session> {
        return this.#sessions
    }

    /** Create a WebRTC media session. Call session.join(localStream) to connect. */
    createSession(roomName: string, options?: SessionCreateOptions): Session {
        const session = createSession({
            api: this.#client,
            roomId: roomName,
            agentDid: this.#agentDid,
            neighbourhoodUrl: options?.neighbourhoodUrl ?? "",
            topology: options?.topology ?? "auto",
        })
        this.#sessions.push(session)
        session.on("state-changed", (state: string) => {
            if (state === "closed") {
                const idx = this.#sessions.indexOf(session)
                if (idx !== -1) this.#sessions.splice(idx, 1)
            }
        })
        return session
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
