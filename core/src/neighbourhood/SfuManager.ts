/**
 * SFU (Selective Forwarding Unit) client manager.
 *
 * Framework-agnostic WebRTC client for the AD4M executor's embedded
 * SFU.  Handles topology resolution, SDP negotiation, server-pushed
 * renegotiation, simulcast quality preferences, and reconnecting after
 * a failed connection.
 *
 * The client reaches one SFU: its own executor's. Multi-node calls work
 * through cascade pipes between executors, so the client never follows
 * a redirect or a rebalance to another node — it has no way to reach one.
 *
 * Applications (Flux, WE, or any other) wrap this class with their
 * own reactive store and UI layer.  The manager itself only touches
 * Web APIs (RTCPeerConnection, MediaStream) and the AD4M
 * neighbourhood RPC surface.
 */

import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type {
    CallSessionInfo,
    SfuCallRenegotiationOffer,
    SfuConfig,
    SfuDataMessage,
    SfuQualityPreference,
    SfuRoomRoster,
} from "./SfuTypes"

// ── Public types ──────────────────────────────────────────────────

/** Minimal interface for the neighbourhood client methods the SFU
 *  manager calls.  NeighbourhoodClient satisfies this directly —
 *  pass the client through without an adapter. */
export interface SfuNeighbourhoodApi {
    sfuCallJoin(
        neighbourhoodUrl: string,
        roomName: string,
        sdpOffer: string,
    ): Promise<CallSessionInfo>
    sfuCallLeave(
        neighbourhoodUrl: string,
        roomName: string,
    ): Promise<boolean>
    sfuCallSetQualityPreference(
        neighbourhoodUrl: string,
        roomName: string,
        preference: SfuQualityPreference,
    ): Promise<boolean>
    sfuCallAnswerServerOffer(
        neighbourhoodUrl: string,
        roomName: string,
        sdpAnswer: string,
    ): Promise<boolean>
    subscribeSfuCallRenegotiationOffer(
        targetDid: string,
        callback: (event: SfuCallRenegotiationOffer) => void,
    ): () => void
    sfuAddIceCandidate(
        neighbourhoodUrl: string,
        roomName: string,
        candidate: string,
    ): Promise<boolean>
    sfuSendData(
        neighbourhoodUrl: string,
        roomName: string,
        channelLabel: string,
        data: string,
        binary?: boolean,
    ): Promise<boolean>
    subscribeSfuDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void
    subscribeSfuRoomRoster(
        callback: (roster: SfuRoomRoster) => void,
    ): () => void
}

export type SfuTopology = "sfu" | "mesh" | "cascaded"

export interface SfuCallState {
    topology: SfuTopology
    roomId: string
    participantId: string | null
    sfuPeerDid: string | null
    participants: Map<string, SfuParticipantState>
    localStream: MediaStream | null
    peerConnection: RTCPeerConnection | null
}

export interface SfuParticipantState {
    did: string
    stream: MediaStream
    hasAudio: boolean
    hasVideo: boolean
    isActiveSpeaker: boolean
}

export type SfuEvent =
    | "topology-changed"
    | "participant-joined"
    | "participant-left"
    | "active-speaker"
    | "stream-added"
    | "stream-removed"
    | "error"

export type SfuEventCallback = (...args: any[]) => void

/** ICE server configuration for SFU peer connections. */
export interface SfuIceConfig {
    stun?: string[]
    turn?: { urls: string; username: string; credential: string }[]
}

// ── Constants ─────────────────────────────────────────────────────

const DEFAULT_ICE_SERVERS: RTCIceServer[] = [
    { urls: "stun:stun.l.google.com:19302" },
]

// ── Topology resolution ───────────────────────────────────────────

/**
 * Determine the optimal call topology from the SFU config and the
 * current participant count.  Returns the topology, the DID of the
 * SFU peer to connect to (if any), and the full config.
 */
export async function resolveTopology(
    neighbourhood: NeighbourhoodProxy,
    neighbourhoodUrl: string,
    participantCount: number,
): Promise<{ topology: SfuTopology; sfuPeer: string | null; config: SfuConfig }> {
    const config = await neighbourhood.sfuConfig(neighbourhoodUrl)

    if (config.mode === "cascaded") {
        const sfuPeers: string[] = await neighbourhood.sfuPeers(neighbourhoodUrl)
        if (sfuPeers.length > 1) {
            return { topology: "cascaded", sfuPeer: sfuPeers[0], config }
        }
        if (sfuPeers.length === 1) {
            return { topology: "sfu", sfuPeer: sfuPeers[0], config }
        }
        return { topology: "mesh", sfuPeer: null, config }
    }

    if (config.mode === "mesh") {
        return { topology: "mesh", sfuPeer: null, config }
    }

    const sfuPeer = await neighbourhood.sfuPeer(neighbourhoodUrl)

    if (!sfuPeer) {
        if (participantCount <= config.maxMeshParticipants) {
            return { topology: "mesh", sfuPeer: null, config }
        }
        console.warn(
            `SFU peer unavailable and ${participantCount} participants ` +
            `exceeds mesh limit (${config.maxMeshParticipants}). ` +
            `Attempting mesh anyway.`,
        )
        return { topology: "mesh", sfuPeer: null, config }
    }

    if (participantCount <= config.maxMeshParticipants) {
        return { topology: "mesh", sfuPeer, config }
    }

    return { topology: "sfu", sfuPeer, config }
}

// ── SFU Manager ───────────────────────────────────────────────────

export class SfuManager {
    private neighbourhood: SfuNeighbourhoodApi
    private neighbourhoodUrl: string
    private roomId: string
    private agentDid: string
    private state: SfuCallState
    private callbacks: Map<SfuEvent, SfuEventCallback[]> = new Map()
    private iceServers: RTCIceServer[]
    /** Reconnects since ICE last reached connected — not since a join: a
     *  join can succeed while the media path never comes up. */
    private reconnectAttempts: number = 0
    private static readonly MAX_RECONNECTS = 3
    /** How long one rebuild keeps retrying a join the executor cannot take yet. */
    private static readonly REJOIN_WINDOW_MS = 120_000
    /** How long ICE may stay disconnected before a rebuild: the executor's `ICE_DISCONNECT_GRACE`. */
    private static readonly DISCONNECT_GRACE_MS = 10_000
    /** A rebuild is running: a second failure signal joins it rather than starting another. */
    private reconnecting = false
    /** Bumped by `leave()`, so a reconnect in flight stops before rejoining. */
    private generation: number = 0
    /** Which participant each received media section carries, from the server's mappings. */
    private midToParticipant: Map<string, string> = new Map()
    /** DIDs the roster removed and has not listed since. */
    private departed: Set<string> = new Set()
    /** The sender of each kind of local media: declared at join, so a later camera or screen needs no renegotiation. */
    private senders: Map<"audio" | "video", RTCRtpSender> = new Map()
    /** Tracks `replaceTrack` set since the user's join, for a rebuilt connection to send. */
    private replacedTracks: Map<"audio" | "video", MediaStreamTrack | null> = new Map()
    /** The last quality preference set, for a rebuilt connection to restore. */
    private qualityPreference: SfuQualityPreference | null = null
    /** Server offers waiting to be applied, in arrival order, one at a time. */
    private offerQueue: Promise<void> = Promise.resolve()
    private renegotiationUnsubscribe: (() => void) | null = null
    private rosterUnsubscribe: (() => void) | null = null
    private dataChannelUnsubscribe: (() => void) | null = null
    private disconnectedTimer: ReturnType<typeof setTimeout> | null = null

    constructor(
        neighbourhood: SfuNeighbourhoodApi,
        roomId: string,
        agentDid: string,
        neighbourhoodUrl?: string,
        iceConfig?: SfuIceConfig,
        sfuConfig?: SfuConfig,
    ) {
        this.neighbourhood = neighbourhood
        this.neighbourhoodUrl = neighbourhoodUrl || ""
        this.roomId = roomId
        this.agentDid = agentDid
        this.state = {
            topology: "sfu",
            roomId,
            participantId: null,
            sfuPeerDid: null,
            participants: new Map(),
            localStream: null,
            peerConnection: null,
        }

        // Resolution order for ICE servers:
        //   1. sfuConfig.iceServers — authoritative when set
        //   2. iceConfig parameter — legacy override for tests
        //   3. DEFAULT_ICE_SERVERS — public STUN fallback
        let servers: RTCIceServer[] = []
        if (sfuConfig?.iceServers && sfuConfig.iceServers.length > 0) {
            servers = sfuConfig.iceServers.map((s) => ({
                urls: s.urls,
                username: s.username,
                credential: s.credential,
            }))
        } else if (iceConfig) {
            if (iceConfig.stun) {
                for (const url of iceConfig.stun) {
                    servers.push({ urls: url })
                }
            }
            if (iceConfig.turn) {
                for (const t of iceConfig.turn) {
                    servers.push({
                        urls: t.urls,
                        username: t.username,
                        credential: t.credential,
                    })
                }
            }
        }
        this.iceServers = servers.length > 0 ? servers : DEFAULT_ICE_SERVERS
    }

    /**
     * Rebuild the call after its connection failed: leave, then join again
     * on this executor. Leaving first matters: the executor still holds this
     * DID's old session until it notices the connection died.
     *
     * Two budgets. A rebuild whose join works but whose connection fails
     * again counts once against `MAX_RECONNECTS`, reset when ICE connects.
     * Within one rebuild, a join that fails because the executor cannot be
     * reached — a restart, an outage — is retried with backoff for up to
     * `REJOIN_WINDOW_MS`, so the call comes back on its own once it can.
     */
    private async reconnect(): Promise<void> {
        const stream = this.state.localStream
        if (!stream || this.reconnecting) return
        const generation = this.generation
        if (++this.reconnectAttempts > SfuManager.MAX_RECONNECTS) {
            this.emit("error", new Error("SFU reconnect attempts exhausted"))
            return
        }
        console.info(
            `SFU: connection failed, reconnecting (${this.reconnectAttempts}/${SfuManager.MAX_RECONNECTS})`,
        )
        this.reconnecting = true
        try {
            const deadline = Date.now() + SfuManager.REJOIN_WINDOW_MS
            for (let delay = 1000; ; delay = Math.min(delay * 2, 10_000)) {
                this.releaseServerEvents()
                if (this.state.peerConnection) {
                    this.state.peerConnection.close()
                    this.state.peerConnection = null
                }
                try {
                    await this.neighbourhood.sfuCallLeave(this.neighbourhoodUrl, this.roomId)
                } catch {
                    /* best-effort: the executor frees a dead session on its own */
                }
                if (generation !== this.generation) return // left meanwhile
                this.resetParticipants()
                try {
                    await this.join(stream)
                    if (this.qualityPreference) await this.setQualityPreference(this.qualityPreference)
                    return
                } catch (e) {
                    if (generation !== this.generation) return
                    if (Date.now() + delay > deadline) {
                        console.error("SFU reconnect failed:", e)
                        this.emit("error", e)
                        return
                    }
                    console.warn(`SFU: rejoin failed, retrying in ${delay} ms:`, e)
                    await new Promise((resolve) => setTimeout(resolve, delay))
                    if (generation !== this.generation) return
                }
            }
        } finally {
            this.reconnecting = false
        }
    }

    on(event: SfuEvent, callback: SfuEventCallback): void {
        if (!this.callbacks.has(event)) {
            this.callbacks.set(event, [])
        }
        this.callbacks.get(event)!.push(callback)
    }

    off(event: SfuEvent, callback?: SfuEventCallback): void {
        if (!callback) {
            this.callbacks.delete(event)
            return
        }
        const cbs = this.callbacks.get(event)
        if (cbs) {
            const idx = cbs.indexOf(callback)
            if (idx !== -1) cbs.splice(idx, 1)
            if (cbs.length === 0) this.callbacks.delete(event)
        }
    }

    private emit(event: SfuEvent, ...args: any[]): void {
        const cbs = this.callbacks.get(event)
        if (cbs) {
            for (const cb of cbs) {
                try {
                    cb(...args)
                } catch (e) {
                    console.error(
                        `SFU event handler error (${event}):`,
                        e,
                    )
                }
            }
        }
    }

    /** Join the SFU call with simulcast support. */
    async join(localStream: MediaStream): Promise<void> {
        this.state.localStream = localStream

        const pc = new RTCPeerConnection({ iceServers: this.iceServers })
        this.state.peerConnection = pc

        // ICE state monitoring for reconnecting.
        // "disconnected" is often transient (network blip, route change):
        // the connection gets the same grace the server gives it before
        // dropping the peer, and a rebuild only follows once the server
        // would have dropped it anyway.  "failed" reconnects at once.
        pc.oniceconnectionstatechange = () => {
            if (this.state.peerConnection !== pc) return // superseded
            if (pc.iceConnectionState === "failed") {
                if (this.disconnectedTimer) {
                    clearTimeout(this.disconnectedTimer)
                    this.disconnectedTimer = null
                }
                this.reconnect()
            } else if (pc.iceConnectionState === "disconnected") {
                if (!this.disconnectedTimer) {
                    this.disconnectedTimer = setTimeout(() => {
                        this.disconnectedTimer = null
                        // Only reconnect if this pc is still the active one
                        // and still disconnected.
                        if (
                            this.state.peerConnection === pc &&
                            pc.iceConnectionState === "disconnected"
                        ) {
                            this.reconnect()
                        }
                    }, SfuManager.DISCONNECT_GRACE_MS)
                }
            } else {
                // Any other state (connected, completed, checking) cancels
                // the pending disconnected timer; a working path ends a run
                // of reconnects.
                if (
                    pc.iceConnectionState === "connected" ||
                    pc.iceConnectionState === "completed"
                ) {
                    this.reconnectAttempts = 0
                }
                if (this.disconnectedTimer) {
                    clearTimeout(this.disconnectedTimer)
                    this.disconnectedTimer = null
                }
            }
        }

        // Both kinds of local media, send-only, declared now whether or not a
        // track exists yet: the SFU only ever receives on these sections, and a
        // camera or screen started later replaces an empty track with no
        // renegotiation. Video goes out as three simulcast layers.
        this.senders.clear()
        for (const kind of ["audio", "video"] as const) {
            const track = this.replacedTracks.has(kind)
                ? this.replacedTracks.get(kind)
                : localStream.getTracks().find((t) => t.kind === kind)
            const transceiver = pc.addTransceiver(track ?? kind, {
                direction: "sendonly",
                streams: [localStream],
                ...(kind === "video"
                    ? {
                          sendEncodings: [
                              { rid: "high", maxBitrate: 1500000 },
                              { rid: "medium", maxBitrate: 500000, scaleResolutionDownBy: 2 },
                              { rid: "low", maxBitrate: 150000, scaleResolutionDownBy: 4 },
                          ],
                      }
                    : {}),
            })
            this.senders.set(kind, transceiver.sender)
        }

        // Incoming media belongs to the participant the server mapped its
        // section to. A track on a section with no mapping is not someone's
        // media, so it is left alone rather than attributed by guesswork.
        pc.ontrack = (event: RTCTrackEvent) => {
            if (this.state.peerConnection !== pc) return
            const mid = event.transceiver?.mid
            const did = mid ? this.midToParticipant.get(mid) : undefined
            // Someone the roster said left stays gone: a late offer can still name them.
            if (!did || did === this.agentDid || this.departed.has(did)) return
            const participant = this.ensureParticipant(did)
            participant.stream.addTrack(event.track)
            this.refreshKinds(participant)
            this.emit("stream-added", participant.stream, event.track)
            event.track.addEventListener("ended", () => {
                participant.stream.removeTrack(event.track)
                this.refreshKinds(participant)
                this.emit("stream-removed", participant.stream, event.track)
            })
        }

        // ── Trickle ICE ──────────────────────────────────────────────
        //
        // Buffer candidates gathered before callJoin returns (the
        // server needs the participant registered first).  After
        // callJoin completes, flush the buffer and trickle live.

        const pendingCandidates: string[] = []
        let joinComplete = false

        pc.onicecandidate = (event) => {
            if (!event.candidate) return
            if (joinComplete) {
                this.neighbourhood.sfuAddIceCandidate(
                    this.neighbourhoodUrl,
                    this.roomId,
                    event.candidate.candidate,
                ).catch((err) => {
                    console.error(
                        "SFU: failed to trickle ICE candidate:",
                        err,
                    )
                })
            } else {
                pendingCandidates.push(event.candidate.candidate)
            }
        }

        // Create offer and send immediately — no ICE gathering wait
        const offer = await pc.createOffer()
        await pc.setLocalDescription(offer)

        const sdpOffer = JSON.stringify(pc.localDescription)
        // Server offers can arrive before the join's own answer is applied;
        // they wait for it, then apply in order.
        let markJoined!: () => void
        const joined = new Promise<void>((resolve) => { markJoined = resolve })
        this.subscribeServerEvents(pc, joined)
        let session: CallSessionInfo
        try {
            session = await this.neighbourhood.sfuCallJoin(
                this.neighbourhoodUrl,
                this.roomId,
                sdpOffer,
            )
        } catch (err) {
            // Settled either way: offers queued behind this join reach
            // `applyServerOffer`, which drops them for the abandoned connection.
            markJoined()
            this.abandon(pc)
            throw err
        }
        this.state.participantId = session.participantId

        try {
            const answer = JSON.parse(session.sdpAnswer)
            await pc.setRemoteDescription(new RTCSessionDescription(answer))
        } catch (err) {
            markJoined()
            this.abandon(pc)
            throw err
        }
        markJoined()

        // Who was in the room when this join landed (`participantId:did`).
        // The roster event keeps it current from here.
        this.applyRoster(session.streamMapping.map((entry) => entry.slice(entry.indexOf(":") + 1)), false)

        // Flush buffered trickle ICE candidates
        joinComplete = true
        for (const candidate of pendingCandidates) {
            this.neighbourhood.sfuAddIceCandidate(
                this.neighbourhoodUrl,
                this.roomId,
                candidate,
            ).catch((err) => {
                console.error(
                    "SFU: failed to trickle buffered ICE candidate:",
                    err,
                )
            })
        }
    }

    /** Drop the server-event subscriptions of the current join. */
    private releaseServerEvents(): void {
        this.renegotiationUnsubscribe?.()
        this.renegotiationUnsubscribe = null
        this.rosterUnsubscribe?.()
        this.rosterUnsubscribe = null
    }

    /** Undo a join that failed part way: its subscription and its connection. */
    private abandon(pc: RTCPeerConnection): void {
        this.releaseServerEvents()
        pc.close()
        if (this.state.peerConnection === pc) this.state.peerConnection = null
    }

    /**
     * Subscribe to the server's events for this join: renegotiation offers
     * and the room roster. `join` calls this before `sfuCallJoin`: the
     * executor sends a socket only the events it watches, and a call
     * carries the pending watch ahead of itself, so no event the join
     * causes is lost. Offers wait for `joined` and then apply one at a
     * time, in arrival order; one that arrives for a connection already
     * replaced is dropped.
     */
    private subscribeServerEvents(pc: RTCPeerConnection, joined: Promise<void>): void {
        // Join may run more than once (a reconnect). The prior
        // subscriptions go after the new ones exist, so the event socket
        // never drops to zero listeners in between.
        const previous = [this.renegotiationUnsubscribe, this.rosterUnsubscribe]
        const ours = (e: { neighbourhoodUrl: string; roomName: string }) =>
            e.neighbourhoodUrl === this.neighbourhoodUrl && e.roomName === this.roomId

        this.renegotiationUnsubscribe = this.neighbourhood.subscribeSfuCallRenegotiationOffer(
            this.agentDid,
            (event) => {
                if (!ours(event)) return
                this.offerQueue = this.offerQueue
                    .then(() => joined)
                    .then(() => this.applyServerOffer(pc, event))
            },
        )
        this.rosterUnsubscribe = this.neighbourhood.subscribeSfuRoomRoster((roster) => {
            if (!ours(roster) || this.state.peerConnection !== pc) return
            void joined.then(() => {
                if (this.state.peerConnection === pc) this.applyRoster(roster.participants, true)
            })
        })

        for (const unsubscribe of previous) unsubscribe?.()
    }

    /** Apply one server offer to `pc` and answer it, unless `pc` is no longer the call's. */
    private async applyServerOffer(pc: RTCPeerConnection, event: SfuCallRenegotiationOffer): Promise<void> {
        if (this.state.peerConnection !== pc) return
        // The mapping first: the tracks this offer adds arrive while it is applied.
        for (const entry of event.trackMapping ?? []) {
            this.midToParticipant.set(entry.mid, entry.agentDid)
        }
        try {
            await pc.setRemoteDescription(new RTCSessionDescription(JSON.parse(event.sdpOffer)))
            await pc.setLocalDescription(await pc.createAnswer())
            await this.neighbourhood.sfuCallAnswerServerOffer(
                this.neighbourhoodUrl,
                event.roomName,
                JSON.stringify(pc.localDescription),
            )
        } catch (err) {
            if (this.state.peerConnection !== pc) return
            // A renegotiation that fails leaves this connection out of step
            // with the server — or the server has already dropped it. Either
            // way the call is rebuilt; `reconnect` reports an error only if
            // that fails too.
            console.warn("SFU: renegotiation failed, rebuilding the connection:", err)
            void this.reconnect()
        }
    }

    /**
     * Make the participants match `dids`, the room's roster: everyone new
     * joins, and — when `complete`, as the roster event is — everyone
     * missing leaves. The join's own roster can lag the room, so it only adds.
     */
    private applyRoster(dids: string[], complete: boolean): void {
        const present = new Set(dids.filter((did) => did !== this.agentDid))
        for (const did of present) {
            this.departed.delete(did)
            this.ensureParticipant(did)
        }
        if (!complete) return
        for (const [did, participant] of [...this.state.participants]) {
            if (present.has(did)) continue
            this.state.participants.delete(did)
            this.departed.add(did)
            for (const [mid, owner] of this.midToParticipant) {
                if (owner === did) this.midToParticipant.delete(mid)
            }
            this.emit("participant-left", participant)
        }
    }

    /** The participant for `did`, joining them to the call if they are new. */
    private ensureParticipant(did: string): SfuParticipantState {
        let participant = this.state.participants.get(did)
        if (!participant) {
            participant = {
                did,
                stream: new MediaStream(),
                hasAudio: false,
                hasVideo: false,
                isActiveSpeaker: false,
            }
            this.state.participants.set(did, participant)
            this.emit("participant-joined", participant)
        }
        return participant
    }

    private refreshKinds(participant: SfuParticipantState): void {
        participant.hasAudio = participant.stream.getAudioTracks().length > 0
        participant.hasVideo = participant.stream.getVideoTracks().length > 0
    }

    async leave(): Promise<void> {
        this.generation++
        this.releaseServerEvents()
        if (this.disconnectedTimer) {
            clearTimeout(this.disconnectedTimer)
            this.disconnectedTimer = null
        }
        this.senders.clear()
        this.replacedTracks.clear()
        this.qualityPreference = null
        if (this.dataChannelUnsubscribe) {
            try {
                this.dataChannelUnsubscribe()
            } catch {
                /* swallow */
            }
            this.dataChannelUnsubscribe = null
        }
        if (this.state.peerConnection) {
            this.state.peerConnection.close()
            this.state.peerConnection = null
        }
        try {
            await this.neighbourhood.sfuCallLeave(
                this.neighbourhoodUrl,
                this.roomId,
            )
        } catch (e) {
            console.error("Error leaving SFU call:", e)
        }
        this.resetParticipants()
    }

    /** Forget the call's participants, telling listeners each one left. */
    private resetParticipants(): void {
        for (const [, participant] of this.state.participants) {
            this.emit("participant-left", participant)
        }
        this.state.participants.clear()
        this.state.participantId = null
        this.midToParticipant.clear()
        this.departed.clear()
    }

    async setQualityPreference(
        preference: SfuQualityPreference,
    ): Promise<void> {
        this.qualityPreference = preference
        if (!this.state.participantId) {
            console.warn(
                "Cannot set quality preference: not connected to SFU",
            )
            return
        }
        try {
            await this.neighbourhood.sfuCallSetQualityPreference(
                this.neighbourhoodUrl,
                this.roomId,
                preference,
            )
        } catch (e) {
            console.error("Failed to set quality preference:", e)
            this.emit("error", e)
        }
    }

    getState(): Readonly<SfuCallState> {
        return this.state
    }

    getParticipants(): SfuParticipantState[] {
        return Array.from(this.state.participants.values())
    }

    // ── Track replacement ──────────────────────────────────────────

    /**
     * Replace the outbound track of a given kind on the SFU peer connection.
     *
     * Both kinds were declared at join, so this is always
     * `RTCRtpSender.replaceTrack` — no renegotiation. Pass `null` to stop
     * sending that kind.
     */
    async replaceTrack(kind: "audio" | "video", track: MediaStreamTrack | null): Promise<void> {
        this.replacedTracks.set(kind, track)
        // Between connections (a rebuild) the next join sends it.
        if (this.state.peerConnection) await this.senders.get(kind)?.replaceTrack(track)
    }

    // ── Data channel relay ──────────────────────────────────────────

    /**
     * Send data to all other participants in the room via the SFU.
     * Text payloads pass as-is; binary payloads should arrive
     * base64-encoded with `binary: true`.
     */
    async sendData(
        channelLabel: string,
        data: string,
        binary: boolean = false,
    ): Promise<void> {
        if (!this.state.participantId) {
            console.warn("Cannot send data: not connected to SFU")
            return
        }
        try {
            await this.neighbourhood.sfuSendData(
                this.neighbourhoodUrl,
                this.roomId,
                channelLabel,
                data,
                binary,
            )
        } catch (e) {
            console.error("SFU: sendData failed:", e)
            this.emit("error", e)
        }
    }

    /**
     * Subscribe to data channel messages from other participants.
     * Returns an unsubscribe function.  Only one subscription per
     * manager instance — a second call replaces the first.
     */
    subscribeDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void {
        // Tear down any existing subscription
        if (this.dataChannelUnsubscribe) {
            try {
                this.dataChannelUnsubscribe()
            } catch {
                /* swallow */
            }
        }
        this.dataChannelUnsubscribe =
            this.neighbourhood.subscribeSfuDataChannel(callback)
        return () => {
            if (this.dataChannelUnsubscribe) {
                try {
                    this.dataChannelUnsubscribe()
                } catch {
                    /* swallow */
                }
                this.dataChannelUnsubscribe = null
            }
        }
    }

    async destroy(): Promise<void> {
        try {
            await this.leave()
        } catch (e) {
            console.error("Error during SFU destroy:", e)
        }
        this.callbacks.clear()
        this.midToParticipant.clear()
    }
}
