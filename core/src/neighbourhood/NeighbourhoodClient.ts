import {ApiClient, CallOptions } from '../apiClient'
import { Address } from "../Address"
import { DID } from "../DID"
import { OnlineAgent, TelepresenceSignalCallback } from "../language/Language"
import { Perspective, PerspectiveExpression, PerspectiveUnsignedInput } from "../perspectives/Perspective"
import { PerspectiveHandle } from "../perspectives/PerspectiveHandle"
import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type { CreatedInvite } from "../generated/api/CreatedInvite"
import type { CreateNeighbourhoodParams } from "../generated/api/CreateNeighbourhoodParams"
import type { CreateNeighbourhoodResult } from "../generated/api/CreateNeighbourhoodResult"
import type { LinkLanguageChoice } from "../generated/api/LinkLanguageChoice"
import type { InviteInfo } from "../generated/api/InviteInfo"
import type { JoinMode } from "../generated/api/JoinMode"
import type { MemberInfo } from "../generated/api/MemberInfo"
import type { NewMemberRole } from "../generated/api/NewMemberRole"
import type { PersonCount } from "../generated/api/PersonCount"
import type { PersonCountQuery } from "../generated/api/PersonCountQuery"
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
    SfuSessionReplaced,
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

    /**
     * Makes the perspective a space as this session's person, on a link server: the executor
     * makes the space's link language (its creator is the person), writes the space's rules
     * and the person's `admin` link, and publishes it. The link language is fixed from then on:
     * a second `create` of the same perspective rejects (`already-created`). A session without
     * a person rejects with `needs-identity`; `{ kind: "holochain" }` with `coming-soon`.
     */
    async create(
        params: CreateNeighbourhoodParams,
        options?: CallOptions,
    ): Promise<CreateNeighbourhoodResult> {
        return this.#apiClient.call('neighbourhood.create', params, options)
    }

    /** The link language the space syncs through, fixed when it was made; `null` for a
     *  perspective that is not a space. */
    async linkLanguageChoice(perspectiveUUID: string): Promise<LinkLanguageChoice | null> {
        return this.#apiClient.call('neighbourhood.linkLanguageChoice', { perspectiveUuid: perspectiveUUID })
    }

    /**
     * Joins the neighbourhood at `url`. In a space with membership rules the executor writes
     * this person's own `member` link (with `invite`, an `invite://<id>` from a
     * `coasys://join` link, in an invite-only space); a refused join rejects, removes the
     * perspective and pushes `neighbourhood-join-refused` with the reason.
     */
    async joinFromUrl(url: string, options?: CallOptions & { invite?: string }): Promise<PerspectiveHandle> {
        const { invite, ...callOptions } = options ?? {}
        const params = invite ? { url, invite } : { url }
        return PerspectiveHandle.fromWire(await this.#apiClient.call('neighbourhood.join', params, callOptions))
    }

    /** The current members of the space: people, admins and their bots. */
    async members(perspectiveUUID: string): Promise<MemberInfo[]> {
        return this.#apiClient.call('neighbourhood.members', { uuid: perspectiveUUID })
    }

    /** Adds `person` as a member or admin; refused unless the rules let this person add. */
    async addMember(perspectiveUUID: string, person: DID, role: NewMemberRole = "member"): Promise<void> {
        await this.#apiClient.call('neighbourhood.addMember', { uuid: perspectiveUUID, person, role })
    }

    /** Removes `person` (admins only): they cannot join again on their own. */
    async removeMember(perspectiveUUID: string, person: DID): Promise<void> {
        await this.#apiClient.call('neighbourhood.removeMember', { uuid: perspectiveUUID, person })
    }

    /** Leaves the space; a person who left may join again. */
    async leave(perspectiveUUID: string): Promise<void> {
        await this.#apiClient.call('neighbourhood.leave', { uuid: perspectiveUUID })
    }

    /** Makes an invite (admins only), with no expiry unless `expiresAt` is given. */
    async createInvite(perspectiveUUID: string, expiresAt?: Date): Promise<CreatedInvite> {
        const params = expiresAt
            ? { uuid: perspectiveUUID, expiresAt: expiresAt.toISOString() }
            : { uuid: perspectiveUUID }
        return this.#apiClient.call('neighbourhood.createInvite', params)
    }

    /** The space's invites, with who used each. */
    async invites(perspectiveUUID: string): Promise<InviteInfo[]> {
        return this.#apiClient.call('neighbourhood.invites', { uuid: perspectiveUUID })
    }

    /** Sets whether anyone may join (`open`) or only with an invite (admins only). */
    async setJoinMode(perspectiveUUID: string, join: JoinMode): Promise<void> {
        await this.#apiClient.call('neighbourhood.setJoinMode', { uuid: perspectiveUUID, join })
    }

    /** Adds `botDid`, a bot key of this person's, as their bot in the space. */
    async addBot(perspectiveUUID: string, botDid: DID): Promise<void> {
        await this.#apiClient.call('neighbourhood.addBot', { uuid: perspectiveUUID, botDid })
    }

    /** Removes a bot: its owner or an admin. */
    async removeBot(perspectiveUUID: string, botDid: DID): Promise<void> {
        await this.#apiClient.call('neighbourhood.removeBot', { uuid: perspectiveUUID, botDid })
    }

    /**
     * Counts the links `source -[predicate]-> *` of a space once per person (a vote, a
     * reaction): each person's latest link counts for its target. Bots, keys without a log,
     * non-members and links whose author could not be checked are left out, by reason, in
     * `skipped`. With `at` (ISO 8601) the count is as of that time.
     */
    async countByPerson(q: PersonCountQuery): Promise<PersonCount> {
        return this.#apiClient.call('neighbourhood.countByPerson', q)
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

    /**
     * Join a call from `deviceId`. The executor replaces this agent's session
     * from the same device (another tab) and keeps those from other devices.
     * The result's `participantId` names the session in every later call.
     * `sessionKey` is the caller's random key for the session: a rebuild
     * sends it again; a key a later join from this device replaced is
     * refused with `RpcError` status 409, and that session must stop.
     */
    async sfuCallJoin(
        neighbourhoodUrl: string,
        roomName: string,
        sdpOffer: string,
        deviceId: string,
        sessionKey: string,
    ): Promise<CallSessionInfo> {
        return this.#apiClient.call("sfu.callJoin", {
            neighbourhoodUrl,
            roomName,
            sdpOffer,
            deviceId,
            sessionKey,
        })
    }

    /** Leave the session `participantId`; `false` when it is not this agent's or is gone already. */
    async sfuCallLeave(neighbourhoodUrl: string, roomName: string, participantId: string): Promise<boolean> {
        return this.#apiClient.call("sfu.callLeave", { neighbourhoodUrl, roomName, participantId })
    }

    async sfuCallSetQualityPreference(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
        preference: SfuQualityPreference,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.callSetQualityPreference", {
            neighbourhoodUrl,
            roomName,
            participantId,
            preference,
        })
    }

    async sfuCallAnswerServerOffer(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
        sdpAnswer: string,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.callAnswerServerOffer", {
            neighbourhoodUrl,
            roomName,
            participantId,
            sdpAnswer,
        })
    }

    /** Ask the SFU to restart ICE on the session `participantId` (this
     *  agent's, as its join returned it; any other is refused). The offer
     *  arrives as an `sfu-call-renegotiation-offer` event for that session;
     *  answer it with {@link sfuCallAnswerServerOffer}. */
    async sfuCallRestartIce(neighbourhoodUrl: string, roomName: string, participantId: string): Promise<boolean> {
        return this.#apiClient.call("sfu.callRestartIce", { neighbourhoodUrl, roomName, participantId })
    }

    /** Fires after a network change, once the client's socket is back on
     *  the new network. See {@link ApiClient.onNetworkChange}. */
    onNetworkChange(callback: () => void): () => void {
        return this.#apiClient.onNetworkChange(callback)
    }

    /** Check the client's socket now: one that says nothing within a
     *  deadline is treated as a network change. See {@link ApiClient.checkConnection}. */
    checkConnection(): void {
        this.#apiClient.checkConnection()
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
        participantId: string,
        candidate: string,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.addIceCandidate", {
            neighbourhoodUrl,
            roomName,
            participantId,
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
        participantId: string,
        channelLabel: string,
        data: string,
        binary: boolean = false,
    ): Promise<boolean> {
        return this.#apiClient.call("sfu.sendData", {
            neighbourhoodUrl,
            roomName,
            participantId,
            channelLabel,
            data,
            binary,
        })
    }

    /**
     * Subscribe to SFU data channel messages.  Returns an unsubscribe
     * function.  Messages arrive for every participant in the room;
     * filter by `senderDid` or `senderParticipantId` if needed. One sent by
     * a session of this agent arrives here when the agent is in the call on
     * another device too: it carries the sending session's participant id.
     */
    subscribeSfuDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void {
        // A fresh handler per call: `on` merges a repeated handler, and
        // each subscription must own its unsubscribe.
        return this.#apiClient.on("sfu-data", (message) => callback(message))
    }

    /** Subscribe to the rosters of the call rooms this agent is in: who is in each, by session, whenever that changes. */
    subscribeSfuRoomRoster(
        callback: (roster: SfuRoomRoster) => void,
    ): () => void {
        return this.#apiClient.on("sfu-room-roster", (roster) => callback(roster))
    }

    /**
     * Subscribe to the executor's notice that a session of this agent was
     * replaced: a later join from the same device took its call over. The
     * notice names the replaced session's participant id; the session that
     * holds it stops and does not rejoin.
     */
    subscribeSfuSessionReplaced(
        callback: (event: SfuSessionReplaced) => void,
    ): () => void {
        return this.#apiClient.on("sfu-session-replaced", (event) => callback(event))
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
     * An agent in the call on two devices gets the offers of both: each
     * names the session it is for (`participantId`).
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
                participantId: event.participantId,
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
