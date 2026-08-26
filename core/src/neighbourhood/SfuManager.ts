/**
 * SFU (Selective Forwarding Unit) client manager.
 *
 * Framework-agnostic WebRTC client for the AD4M executor's embedded
 * SFU.  Handles topology resolution, SDP negotiation, cascade
 * redirect following, server-pushed renegotiation, simulcast quality
 * preferences, and cascade failover.
 *
 * Applications (Flux, WE, or any other) wrap this class with their
 * own reactive store and UI layer.  The manager itself only touches
 * Web APIs (RTCPeerConnection, MediaStream) and the AD4M
 * neighbourhood RPC surface.
 */

import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type {
    CallSessionInfo,
    SfuConfig,
    SfuQualityPreference,
    TrackMapEntry,
} from "./SfuTypes"

// ── Public types ──────────────────────────────────────────────────

/** Minimal interface for the neighbourhood proxy methods the SFU
 *  manager calls.  Applications can pass a `NeighbourhoodProxy`
 *  directly or provide a lightweight adapter. */
export interface SfuNeighbourhoodApi {
    callJoin(
        neighbourhoodUrl: string,
        roomName: string,
        sdpOffer: string,
    ): Promise<CallSessionInfo>
    callLeave(
        neighbourhoodUrl: string,
        roomName: string,
    ): Promise<boolean>
    callSetQualityPreference(
        neighbourhoodUrl: string,
        roomName: string,
        preference: SfuQualityPreference,
    ): Promise<boolean>
    callAnswerServerOffer(
        neighbourhoodUrl: string,
        roomName: string,
        sdpAnswer: string,
    ): Promise<boolean>
    subscribeCallRenegotiationOffer(
        targetDid: string,
        callback: (event: {
            targetDid: string
            neighbourhoodUrl: string
            roomName: string
            sdpOffer: string
            trackMapping?: TrackMapEntry[]
        }) => void,
    ): () => void
    subscribeMigrateEvent(
        targetDid: string,
        callback: (event: {
            targetDid: string
            neighbourhoodUrl: string
            roomName: string
            migrateToDid: string
        }) => void,
    ): () => void
}

export type SfuTopology = "sfu" | "mesh" | "cascaded"

export interface SfuNodeState {
    did: string
    participantCount: number
    capacityHint: number
}

export interface SfuCallState {
    topology: SfuTopology
    roomId: string
    participantId: string | null
    sfuPeerDid: string | null
    participants: Map<string, SfuParticipantState>
    localStream: MediaStream | null
    peerConnection: RTCPeerConnection | null
    cascadeNodes: SfuNodeState[]
    connectedNodeDid: string | null
    /** DIDs of participants already in the room at join time */
    knownParticipantDids: string[]
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

const ICE_GATHERING_TIMEOUT_MS = 8000

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
    private streamToParticipant: Map<string, string> = new Map()
    private failoverAttempts: number = 0
    private midToParticipant: Map<string, string> = new Map()
    private trackDidIndex: number = 0
    private renegotiationUnsubscribe: (() => void) | null = null
    private migrateUnsubscribe: (() => void) | null = null

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
            cascadeNodes: [],
            connectedNodeDid: null,
            knownParticipantDids: [],
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

    /** Select the least-loaded SFU node from the cascade. */
    private selectNode(nodes: SfuNodeState[]): SfuNodeState | null {
        if (nodes.length === 0) return null
        return nodes.reduce((best, n) =>
            n.participantCount < best.participantCount ? n : best,
        )
    }

    /** Handle SFU node disconnection in cascaded mode. */
    private async handleCascadeFailover(): Promise<void> {
        if (this.state.topology !== "cascaded") return

        this.failoverAttempts++
        const maxAttempts = Math.max(this.state.cascadeNodes.length, 3)
        if (this.failoverAttempts >= maxAttempts) {
            console.error("Cascade failover exhausted — no healthy nodes")
            this.emit("error", new Error("Cascade failover exhausted"))
            return
        }

        const availableNodes = this.state.cascadeNodes.filter(
            (n) => n.did !== this.state.connectedNodeDid,
        )
        const nextNode = this.selectNode(availableNodes)
        if (!nextNode) {
            console.warn(
                "No cascade nodes available for failover, falling back to mesh",
            )
            this.emit("topology-changed", "mesh")
            return
        }

        console.info(
            `SFU cascade failover: reconnecting to node ${nextNode.did}`,
        )
        this.state.connectedNodeDid = nextNode.did
        this.state.sfuPeerDid = nextNode.did

        if (this.state.localStream) {
            try {
                if (this.state.peerConnection) {
                    this.state.peerConnection.close()
                    this.state.peerConnection = null
                }
                await this.join(this.state.localStream)
            } catch (e) {
                console.error("Cascade failover failed:", e)
                this.emit("error", e)
            }
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

        // ICE state monitoring for cascade failover
        pc.oniceconnectionstatechange = () => {
            if (
                pc.iceConnectionState === "failed" ||
                pc.iceConnectionState === "disconnected"
            ) {
                this.handleCascadeFailover()
            }
        }

        // Add local tracks — video with simulcast encodings
        for (const track of localStream.getTracks()) {
            if (track.kind === "video") {
                pc.addTransceiver(track, {
                    direction: "sendrecv",
                    sendEncodings: [
                        { rid: "high", maxBitrate: 1500000 },
                        {
                            rid: "medium",
                            maxBitrate: 500000,
                            scaleResolutionDownBy: 2,
                        },
                        {
                            rid: "low",
                            maxBitrate: 150000,
                            scaleResolutionDownBy: 4,
                        },
                    ],
                })
            } else {
                pc.addTrack(track, localStream)
            }
        }

        // Handle incoming tracks from SFU
        pc.ontrack = (event: RTCTrackEvent) => {
            const stream = event.streams[0]
            if (!stream) return

            const existing = Array.from(
                this.state.participants.values(),
            ).find((p) => p.stream.id === stream.id)

            // Resolve participant DID via three paths:
            // 1. Mid-based lookup from server track_mapping
            // 2. Stream-id cache from a prior ontrack
            // 3. Arrival-order index into knownParticipantDids
            let participantDid: string | undefined
            const mid = event.transceiver?.mid
            if (mid) {
                participantDid = this.midToParticipant.get(mid)
                if (participantDid)
                    this.streamToParticipant.set(stream.id, participantDid)
            }
            if (!participantDid)
                participantDid = this.streamToParticipant.get(stream.id)
            if (
                !participantDid &&
                this.state.knownParticipantDids.length > 0 &&
                this.trackDidIndex <
                    this.state.knownParticipantDids.length
            ) {
                participantDid =
                    this.state.knownParticipantDids[this.trackDidIndex]
                this.streamToParticipant.set(stream.id, participantDid)
                this.trackDidIndex++
            }
            if (!participantDid) participantDid = stream.id

            if (existing) {
                existing.hasAudio =
                    existing.hasAudio || event.track.kind === "audio"
                existing.hasVideo =
                    existing.hasVideo || event.track.kind === "video"
            } else {
                const participant: SfuParticipantState = {
                    did: participantDid,
                    stream,
                    hasAudio: event.track.kind === "audio",
                    hasVideo: event.track.kind === "video",
                    isActiveSpeaker: false,
                }
                this.state.participants.set(stream.id, participant)
                this.emit("participant-joined", participant)
            }

            this.emit("stream-added", stream, event.track)
            event.track.onended = () =>
                this.emit("stream-removed", stream, event.track)
        }

        // Create and send offer
        const offer = await pc.createOffer()
        await pc.setLocalDescription(offer)

        // Wait for ICE gathering with timeout
        await new Promise<void>((resolve) => {
            if (pc.iceGatheringState === "complete") return resolve()
            const timeout = setTimeout(() => {
                pc.onicegatheringstatechange = null
                console.warn(
                    "SFU: ICE gathering timed out, proceeding with partial candidates",
                )
                resolve()
            }, ICE_GATHERING_TIMEOUT_MS)
            pc.onicegatheringstatechange = () => {
                if (pc.iceGatheringState === "complete") {
                    clearTimeout(timeout)
                    pc.onicegatheringstatechange = null
                    resolve()
                }
            }
        })

        const sdpOffer = JSON.stringify(pc.localDescription)
        const session: CallSessionInfo =
            await this.neighbourhood.callJoin(
                this.neighbourhoodUrl,
                this.roomId,
                sdpOffer,
            )
        this.state.participantId = session.participantId

        // Handle cascade redirect
        if (session.redirectTo) {
            console.info(
                `SFU redirect: reconnecting to node ${session.redirectTo}`,
            )
            this.state.connectedNodeDid = session.redirectTo
            this.state.sfuPeerDid = session.redirectTo
            pc.close()
            this.state.peerConnection = null
            return await this.join(localStream)
        }

        if (
            session.streamMapping &&
            session.streamMapping.length > 0
        ) {
            this.state.knownParticipantDids = session.streamMapping
            this.trackDidIndex = 0
        }

        const answer = JSON.parse(session.sdpAnswer)
        await pc.setRemoteDescription(new RTCSessionDescription(answer))
        this.failoverAttempts = 0

        // Subscribe to server-initiated renegotiation offers
        this.renegotiationUnsubscribe =
            this.neighbourhood.subscribeCallRenegotiationOffer(
                this.agentDid,
                async (event) => {
                    if (event.neighbourhoodUrl !== this.neighbourhoodUrl)
                        return
                    if (event.roomName !== this.roomId) return

                    if (event.trackMapping) {
                        for (const entry of event.trackMapping) {
                            this.midToParticipant.set(
                                entry.mid,
                                entry.agentDid,
                            )
                        }
                    }

                    const currentPc = this.state.peerConnection
                    if (!currentPc) {
                        console.warn(
                            "SFU: no peer connection for renegotiation",
                        )
                        return
                    }
                    try {
                        const offerSdp = JSON.parse(event.sdpOffer)
                        await currentPc.setRemoteDescription(
                            new RTCSessionDescription(offerSdp),
                        )
                        const renegAnswer = await currentPc.createAnswer()
                        await currentPc.setLocalDescription(renegAnswer)
                        const answerJson = JSON.stringify(
                            currentPc.localDescription,
                        )
                        await this.neighbourhood.callAnswerServerOffer(
                            this.neighbourhoodUrl,
                            event.roomName,
                            answerJson,
                        )
                    } catch (err) {
                        console.error("SFU: renegotiation failed:", err)
                        this.emit("error", err)
                    }
                },
            )

        // Subscribe to cascade rebalance migration events.
        // The server tells this participant to leave the current
        // (overloaded) node and rejoin on a less-loaded peer.
        // Same flow as cascade failover: leave → set target → rejoin.
        this.migrateUnsubscribe =
            this.neighbourhood.subscribeMigrateEvent(
                this.agentDid,
                async (event) => {
                    if (event.neighbourhoodUrl !== this.neighbourhoodUrl)
                        return
                    if (event.roomName !== this.roomId) return

                    console.info(
                        `SFU rebalance: migrating to node ${event.migrateToDid}`,
                    )

                    const stream = this.state.localStream
                    if (!stream) return

                    // Clean up current connection without calling
                    // the full leave() — we still hold the local
                    // stream for the rejoin.
                    if (this.renegotiationUnsubscribe) {
                        try {
                            this.renegotiationUnsubscribe()
                        } catch {
                            /* swallow */
                        }
                        this.renegotiationUnsubscribe = null
                    }
                    if (this.state.peerConnection) {
                        this.state.peerConnection.close()
                        this.state.peerConnection = null
                    }
                    try {
                        await this.neighbourhood.callLeave(
                            this.neighbourhoodUrl,
                            this.roomId,
                        )
                    } catch {
                        /* best-effort — the server already expects
                         * us to leave */
                    }

                    // Clear participant state for the rejoin
                    for (const [, participant] of this.state.participants) {
                        this.emit("participant-left", participant)
                    }
                    this.state.participants.clear()
                    this.state.participantId = null
                    this.midToParticipant.clear()
                    this.streamToParticipant.clear()
                    this.trackDidIndex = 0
                    this.state.knownParticipantDids = []

                    // Point at the target node and rejoin
                    this.state.connectedNodeDid = event.migrateToDid
                    this.state.sfuPeerDid = event.migrateToDid
                    try {
                        await this.join(stream)
                    } catch (err) {
                        console.error(
                            "SFU rebalance: rejoin failed:",
                            err,
                        )
                        this.emit("error", err)
                    }
                },
            )
    }

    async leave(): Promise<void> {
        if (this.renegotiationUnsubscribe) {
            try {
                this.renegotiationUnsubscribe()
            } catch {
                /* swallow */
            }
            this.renegotiationUnsubscribe = null
        }
        if (this.migrateUnsubscribe) {
            try {
                this.migrateUnsubscribe()
            } catch {
                /* swallow */
            }
            this.migrateUnsubscribe = null
        }
        if (this.state.peerConnection) {
            this.state.peerConnection.close()
            this.state.peerConnection = null
        }
        try {
            await this.neighbourhood.callLeave(
                this.neighbourhoodUrl,
                this.roomId,
            )
        } catch (e) {
            console.error("Error leaving SFU call:", e)
        }
        for (const [, participant] of this.state.participants) {
            this.emit("participant-left", participant)
        }
        this.state.participants.clear()
        this.state.participantId = null
        this.midToParticipant.clear()
        this.streamToParticipant.clear()
        this.trackDidIndex = 0
        this.state.knownParticipantDids = []
    }

    async setQualityPreference(
        preference: SfuQualityPreference,
    ): Promise<void> {
        if (!this.state.participantId) {
            console.warn(
                "Cannot set quality preference: not connected to SFU",
            )
            return
        }
        try {
            await this.neighbourhood.callSetQualityPreference(
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

    async destroy(): Promise<void> {
        try {
            await this.leave()
        } catch (e) {
            console.error("Error during SFU destroy:", e)
        }
        this.callbacks.clear()
        this.streamToParticipant.clear()
        this.midToParticipant.clear()
    }
}
