/**
 * Peer-to-peer WebRTC mesh transport for the AD4M SDK Session interface.
 *
 * Adapted from WE Framework's `mesh.ts` and `protocol.ts` — the
 * WebRTC negotiation logic that runs entirely client-side, with
 * signalling routed through AD4M's telepresence layer.
 *
 * ## Mesh, not SFU
 *
 * Every participant connects to every other, so N participants means
 * N−1 connections each and each peer uploads its video N−1 times.
 * That ceiling caps at roughly four to six peers before upstream
 * bandwidth becomes the limit.  The SFU path (`SfuManager`) handles
 * larger rooms; this path handles small groups with no server.
 *
 * ## Perfect negotiation
 *
 * Both peers see each other join at slightly different moments, both
 * add tracks, and both fire `negotiationneeded` — so offers collide.
 * The mesh implements the standard *perfect negotiation* pattern:
 * each pair has a **polite** peer and an **impolite** one, decided by
 * comparing DIDs, which needs no agreement round-trip.  On collision
 * the impolite peer ignores the incoming offer and the polite peer
 * rolls back its own.  Exactly one offer survives.
 *
 * ## Signalling via telepresence
 *
 * The mesh publishes SDP and ICE messages through the neighbourhood's
 * telepresence channel (`sendBroadcastU` / `addSignalHandler`).
 * See `createTelepresenceChannel` for the bridge.
 */

import type { PerspectiveExpression, PerspectiveUnsignedInput } from "../perspectives/Perspective"
import type { SfuDataMessage } from "./SfuTypes"

// ── Protocol ────────────────────────────────────────────────────────

export const CALL_PROTOCOL_VERSION = 1

export type CallBody =
    | { kind: "description"; description: RTCSessionDescriptionInit }
    | { kind: "ice"; candidate: RTCIceCandidateInit }
    | { kind: "data"; label: string; data: string; binary: boolean }

export interface CallEnvelope {
    v: number
    call: string
}

export type CallMessage = CallEnvelope & CallBody

/**
 * Narrow an untrusted payload off the transport.
 *
 * Everything arriving here was published by another agent, so it
 * gets shape-checked before use.  The sender id comes from the
 * transport rather than the payload — a self-reported `from` would
 * lack authentication.
 */
export function parseCallMessage(payload: unknown): CallMessage | null {
    if (typeof payload !== "object" || payload === null) return null
    const msg = payload as Partial<CallMessage & { to?: string }>

    if (msg.v !== CALL_PROTOCOL_VERSION) return null
    if (typeof msg.call !== "string" || !msg.call) return null

    if (msg.kind === "description") {
        const description = (msg as { description?: unknown }).description
        if (typeof description !== "object" || description === null) return null
        const { type, sdp } = description as RTCSessionDescriptionInit
        if (type !== "offer" && type !== "answer" && type !== "pranswer" && type !== "rollback") return null
        if (sdp !== undefined && typeof sdp !== "string") return null
        return { v: msg.v, call: msg.call, kind: "description", description: { type, sdp } }
    }

    if (msg.kind === "ice") {
        const candidate = (msg as { candidate?: unknown }).candidate
        if (typeof candidate !== "object" || candidate === null) return null
        return { v: msg.v, call: msg.call, kind: "ice", candidate: candidate as RTCIceCandidateInit }
    }

    if (msg.kind === "data") {
        const { label, data, binary } = msg as { label?: unknown; data?: unknown; binary?: unknown }
        if (typeof label !== "string" || typeof data !== "string") return null
        return { v: msg.v, call: msg.call, kind: "data", label, data, binary: !!binary }
    }

    return null
}

// ── Signalling channel ──────────────────────────────────────────────

const RTC_PREDICATE = "ad4m://session/rtc"
const TARGET_ALL = "*"

/** Structurally an ephemeral channel, restated so this module carries
 *  no import-time dependency on any particular transport. */
export interface SignallingChannel {
    publish(payload: unknown, to?: { agentId?: string }): void
    onMessage(cb: (from: string, payload: unknown) => void): () => void
}

/** Minimal telepresence surface the channel bridge needs. */
export interface TelepresenceProxy {
    sendBroadcastU(status: PerspectiveUnsignedInput): Promise<boolean>
    addSignalHandler(handler: (payload: PerspectiveExpression) => void): Promise<void>
    removeSignalHandler(handler: (payload: PerspectiveExpression) => void): void
}

/**
 * Bridge NeighbourhoodProxy telepresence to the SignallingChannel
 * the mesh consumes.
 *
 * ## Why unicast runs emulated over broadcast
 *
 * AD4M exposes real directed send (`sendSignalU`), but it remains
 * broken in current executor builds — the WE framework documents
 * the same limitation in `ad4mEphemeralAdapter.ts`.  This adapter
 * addresses over broadcast: the recipient DID goes in the link's
 * `target` and receivers drop anything not addressed to them.
 * That provides **addressing, not privacy** — every peer still
 * receives the payload.
 *
 * When `sendSignalU` gets fixed, route `publish(..., { agentId })`
 * through it, flip the addressing to native unicast, and delete the
 * broadcast-with-filter path.  No consumer changes needed.
 */
export function createTelepresenceChannel(
    proxy: TelepresenceProxy,
    selfDid: string,
): SignallingChannel {
    return {
        publish(payload, to) {
            /**
             * Addressed twice, on purpose.
             *
             * `to` on the publish lets a backend with **native** unicast
             * send to one peer.  The recipient also appears in the link
             * target because emulated unicast fans out and filters on
             * receipt — and on a fanout-only transport it would not
             * filter at all, so a third peer would apply an offer meant
             * for someone else.  The duplicated field costs a few bytes
             * and makes the mesh correct on every transport tier.
             */
            proxy.sendBroadcastU({
                links: [{
                    source: JSON.stringify(payload),
                    predicate: RTC_PREDICATE,
                    target: to?.agentId ?? TARGET_ALL,
                }],
            }).catch((err) => {
                console.error("mesh: telepresence publish failed:", err)
            })
        },

        onMessage(cb) {
            const handler = (signal: PerspectiveExpression) => {
                const link = signal?.data?.links?.[0]
                if (!link?.author) return

                const { source, predicate, target } = link.data ?? {}
                if (typeof predicate !== "string" || predicate !== RTC_PREDICATE) return

                // Emulated unicast: drop messages addressed to someone else
                if (target && target !== TARGET_ALL && target !== selfDid) return
                // Drop our own echo
                if (link.author === selfDid) return

                let parsed: unknown
                try {
                    parsed = JSON.parse(source as string)
                } catch {
                    return
                }

                cb(link.author as string, parsed)
            }

            proxy.addSignalHandler(handler).catch((err) => {
                console.warn("mesh: signal handler registration failed — inbound traffic will get missed:", err)
            })

            return () => proxy.removeSignalHandler(handler)
        },
    }
}

// ── Mesh manager ────────────────────────────────────────────────────

export type MeshEvent =
    | "participant-joined"
    | "participant-left"
    | "stream-added"
    | "stream-removed"
    | "error"

export type MeshEventCallback = (...args: any[]) => void

export interface MeshManagerOptions {
    channel: SignallingChannel
    callId: string
    selfId: string
    createPeerConnection?: () => RTCPeerConnection
}

const DEFAULT_ICE_SERVERS: RTCIceServer[] = [
    { urls: ["stun:stun.l.google.com:19302"] },
]

interface PeerSlot {
    pc: RTCPeerConnection
    /** Decided by DID comparison — symmetric, so the two peers always
     *  disagree, which provides the point. */
    polite: boolean
    makingOffer: boolean
    ignoreOffer: boolean
    stream: MediaStream
    /**
     * The sender carrying each kind, remembered rather than looked up.
     *
     * `getSenders().find((s) => s.track?.kind === kind)` cannot find a
     * sender whose track has value `null`, and a sender's track *holds*
     * null for the whole time this agent sends nothing of that kind —
     * which results from `replaceTrack(null)` when a screen share stops
     * with no camera to fall back to.
     *
     * The consequence was not a missing frame, it was a permanent one.
     * Sharing again found no sender, took the `addTrack` branch, and
     * gave the peer a *second* video track; their `<video>` renders
     * the first one in the stream, which holds the dead one.  Their
     * view froze on the last shared frame and never recovered.
     */
    senders: Map<"audio" | "video", RTCRtpSender>
}

const MAX_PENDING_PEERS = 16
const MAX_PENDING_PER_PEER = 8

export class MeshManager {
    private channel: SignallingChannel
    private callId: string
    private selfId: string
    private createPeerConnection: () => RTCPeerConnection
    private callbacks: Map<MeshEvent, MeshEventCallback[]> = new Map()
    private slots: Map<string, PeerSlot> = new Map()
    private states: Map<string, RTCPeerConnectionState> = new Map()
    private outbound: Map<"audio" | "video", MediaStreamTrack | null> = new Map()
    private outboundStream: MediaStream | null = null
    private closed = false
    private channelUnsubscribe: (() => void) | null = null
    private pending: Map<string, CallMessage[]> = new Map()
    private dataListeners: Set<(msg: SfuDataMessage) => void> = new Set()

    constructor(options: MeshManagerOptions) {
        this.channel = options.channel
        this.callId = options.callId
        this.selfId = options.selfId
        this.createPeerConnection = options.createPeerConnection
            ?? (() => new RTCPeerConnection({ iceServers: DEFAULT_ICE_SERVERS }))
    }

    // ── Events ──────────────────────────────────────────────────────

    on(event: MeshEvent, callback: MeshEventCallback): void {
        if (!this.callbacks.has(event)) this.callbacks.set(event, [])
        this.callbacks.get(event)!.push(callback)
    }

    off(event: MeshEvent, callback?: MeshEventCallback): void {
        if (!callback) { this.callbacks.delete(event); return }
        const cbs = this.callbacks.get(event)
        if (cbs) {
            const idx = cbs.indexOf(callback)
            if (idx !== -1) cbs.splice(idx, 1)
            if (cbs.length === 0) this.callbacks.delete(event)
        }
    }

    private emit(event: MeshEvent, ...args: any[]): void {
        const cbs = this.callbacks.get(event)
        if (cbs) for (const cb of cbs) {
            try { cb(...args) } catch (e) {
                console.error(`mesh event handler error (${event}):`, e)
            }
        }
    }

    private fail(context: string, error: unknown): void {
        this.emit("error", new Error(`${context}: ${error}`))
    }

    // ── Lifecycle ───────────────────────────────────────────────────

    async join(localStream: MediaStream): Promise<void> {
        this.outboundStream = new MediaStream()

        for (const track of localStream.getAudioTracks()) {
            this.outbound.set("audio", track)
            this.outboundStream.addTrack(track)
        }
        for (const track of localStream.getVideoTracks()) {
            this.outbound.set("video", track)
            this.outboundStream.addTrack(track)
        }

        // Subscribe to signalling messages
        this.channelUnsubscribe = this.channel.onMessage((from, payload) => {
            if (this.closed || from === this.selfId) return

            const message = parseCallMessage(payload)
            if (!message || message.call !== this.callId) return

            // Check unicast addressing in payload.
            // Broadcast messages carry TARGET_ALL ("*") — accept those too.
            const to = (payload as { to?: unknown }).to
            if (typeof to === "string" && to !== TARGET_ALL && to !== this.selfId) return

            // Only negotiate with peers the roster placed in the call.
            // Without this, anyone on the channel could open a connection
            // by sending an offer.
            const slot = this.slots.get(from)
            if (!slot) {
                this.hold(from, message)
                return
            }

            if (message.kind === "data") {
                this.handleData(from, message as CallMessage & { kind: "data"; label: string; data: string; binary: boolean })
                return
            }

            void this.handleSignalling(from, slot, message)
        })
    }

    async leave(): Promise<void> {
        this.closed = true
        if (this.channelUnsubscribe) {
            this.channelUnsubscribe()
            this.channelUnsubscribe = null
        }
        this.pending.clear()
        for (const peerId of [...this.slots.keys()]) this.disconnect(peerId)
        this.outbound.clear()
        this.outboundStream = null
    }

    async destroy(): Promise<void> {
        await this.leave()
        this.callbacks.clear()
        this.dataListeners.clear()
    }

    // ── Roster management ───────────────────────────────────────────

    /**
     * Reconcile connections against who presence says belongs in the
     * call.  Excludes this agent.
     *
     * Membership comes from presence; this class only negotiates.
     * A peer that appears gets a connection, a peer that disappears
     * has its connection torn down.  There exists no join or leave
     * message, because a peer that crashes never sends one.
     */
    setRoster(peerIds: string[]): void {
        if (this.closed) return
        const wanted = new Set(peerIds.filter((id) => id !== this.selfId))

        for (const peerId of this.slots.keys()) {
            if (!wanted.has(peerId)) this.disconnect(peerId)
        }
        for (const peerId of wanted) {
            if (!this.slots.has(peerId)) this.release(peerId, this.connect(peerId))
        }
        // Anything held for an agent the roster does not list will not
        // get wanted.
        for (const peerId of this.pending.keys()) {
            if (!wanted.has(peerId)) this.pending.delete(peerId)
        }
    }

    // ── Participants ────────────────────────────────────────────────

    getParticipants(): { did: string; stream: MediaStream; hasAudio: boolean; hasVideo: boolean; isActiveSpeaker: boolean }[] {
        const result: { did: string; stream: MediaStream; hasAudio: boolean; hasVideo: boolean; isActiveSpeaker: boolean }[] = []
        for (const [peerId, slot] of this.slots) {
            result.push({
                did: peerId,
                stream: slot.stream,
                hasAudio: slot.stream.getAudioTracks().length > 0,
                hasVideo: slot.stream.getVideoTracks().length > 0,
                isActiveSpeaker: false,
            })
        }
        return result
    }

    // ── Track replacement ────────────────────────────────────────────

    /**
     * Replace the outbound track of a given kind on every peer connection.
     *
     * Uses `RTCRtpSender.replaceTrack` — no renegotiation required.
     * Pass `null` to stop sending that kind.
     *
     * Also updates the internal `outbound` map and `outboundStream` so
     * state stays consistent whether called from Session or directly.
     * `setOutboundTrack` delegates here.
     */
    async replaceTrack(kind: "audio" | "video", track: MediaStreamTrack | null): Promise<void> {
        const previous = this.outbound.get(kind)
        this.outbound.set(kind, track)
        if (this.outboundStream) {
            if (previous) this.outboundStream.removeTrack(previous)
            if (track) this.outboundStream.addTrack(track)
        }
        if (this.closed) return

        await Promise.all(
            [...this.slots.entries()].map(async ([peerId, slot]) => {
                try {
                    const sender = slot.senders.get(kind)
                    if (sender) {
                        await sender.replaceTrack(track)
                    } else if (track) {
                        slot.senders.set(kind, slot.pc.addTrack(track, this.outboundStream!))
                    }
                } catch (error) {
                    this.emit("error", new Error(`replacing ${kind} track for ${peerId}: ${error}`))
                }
            }),
        )
    }

    // ── Data channel ────────────────────────────────────────────────

    /**
     * Send data to all other participants via the signalling channel.
     * The mesh has no server-side relay — data rides the same
     * telepresence broadcast the SDP/ICE messages use.
     */
    sendData(label: string, data: string, binary: boolean = false): void {
        this.send(TARGET_ALL, {
            kind: "data",
            label,
            data,
            binary,
        })
    }

    subscribeDataChannel(callback: (msg: SfuDataMessage) => void): () => void {
        this.dataListeners.add(callback)
        return () => this.dataListeners.delete(callback)
    }

    private handleData(from: string, message: { label: string; data: string; binary: boolean }): void {
        const msg: SfuDataMessage = {
            senderDid: from,
            neighbourhoodUrl: "",
            roomName: this.callId,
            channelLabel: message.label,
            binary: message.binary,
            data: message.data,
        }
        for (const cb of this.dataListeners) {
            try { cb(msg) } catch (e) {
                console.error("mesh: data listener error:", e)
            }
        }
    }

    // ── Outbound tracks ─────────────────────────────────────────────

    /**
     * Set what this agent sends on a track kind.  `null` stops sending
     * that kind.
     *
     * Legacy entry point — delegates to `replaceTrack`, which handles
     * both the outbound state bookkeeping and the per-slot sender
     * replacement.  Kept for WE's mesh-path compatibility.
     */
    async setOutboundTrack(kind: "audio" | "video", track: MediaStreamTrack | null): Promise<void> {
        await this.replaceTrack(kind, track)
    }

    // ── Peer connection management ──────────────────────────────────

    private send(to: string, message: CallBody): void {
        this.channel.publish(
            { v: CALL_PROTOCOL_VERSION, call: this.callId, to, ...message },
            to !== TARGET_ALL ? { agentId: to } : undefined,
        )
    }

    private connect(peerId: string): PeerSlot {
        const existing = this.slots.get(peerId)
        if (existing) return existing

        const pc = this.createPeerConnection()
        const slot: PeerSlot = {
            pc,
            // Comparing DIDs gives each pair exactly one polite side
            // without a round trip.
            polite: this.selfId > peerId,
            makingOffer: false,
            ignoreOffer: false,
            stream: new MediaStream(),
            senders: new Map(),
        }
        this.slots.set(peerId, slot)

        pc.onnegotiationneeded = async () => {
            try {
                slot.makingOffer = true
                // No argument: the browser picks offer or answer from the
                // signaling state, which makes the rolled-back case recover
                // on its own.
                await pc.setLocalDescription()
                if (pc.localDescription) {
                    this.send(peerId, { kind: "description", description: pc.localDescription })
                }
            } catch (error) {
                this.fail(`negotiating with ${peerId}`, error)
            } finally {
                slot.makingOffer = false
            }
        }

        pc.onicecandidate = ({ candidate }) => {
            if (candidate) {
                this.send(peerId, { kind: "ice", candidate: candidate.toJSON() })
            }
        }

        pc.ontrack = ({ track }) => {
            slot.stream.addTrack(track)
            // A replaced track (camera → screen) arrives as a new track
            // and the old one ends.  Without this the tile would
            // accumulate dead tracks and keep rendering the first one.
            track.addEventListener("ended", () => {
                slot.stream.removeTrack(track)
                this.emit("stream-removed", slot.stream, track)
            })
            this.emit("stream-added", slot.stream, track)
        }

        pc.onconnectionstatechange = () => {
            this.states.set(peerId, pc.connectionState)
        }

        /*
         * Both m-lines, before there exists anything to put in them.
         *
         * A peer connection can only carry media of a kind it negotiated
         * an m-section for, and `addTrack` creates one.  So an agent
         * sending no video negotiated **no video m-line at all** — and
         * since the topology gets agreed between the pair, that left the
         * *other* peer's camera with nowhere to arrive.  Block your
         * camera and their video never appeared, however healthy the
         * connection was; start a screen share and theirs would suddenly
         * turn up, because adding a video track of your own finally
         * created the m-line their video had been waiting for.
         *
         * Declaring both up front makes the topology a constant rather
         * than a consequence.  Every connection has one audio and one
         * video section from the moment it exists, so what either side
         * happens to send becomes a question about tracks — answered by
         * `replaceTrack` — instead of a question about SDP.  That also
         * means muting, unmuting, and swapping camera for screen never
         * renegotiate at all.
         */
        for (const kind of ["audio", "video"] as const) {
            try {
                slot.senders.set(
                    kind,
                    pc.addTransceiver(kind, {
                        direction: "sendrecv",
                        streams: [this.outboundStream!],
                    }).sender,
                )
            } catch (error) {
                this.fail(`preparing ${kind} for ${peerId}`, error)
            }
        }

        // Attach whatever already gets sent.  A peer joining mid-call
        // must receive our media without waiting for us to toggle
        // something — and this now counts as an attachment, not a
        // topology change.
        for (const [kind, track] of this.outbound) {
            if (!track) continue
            const sender = slot.senders.get(kind)
            if (!sender) continue
            void sender.replaceTrack(track).catch((error) =>
                this.fail(`sending ${kind} to ${peerId}`, error),
            )
        }

        this.emit("participant-joined", {
            did: peerId,
            stream: slot.stream,
            hasAudio: false,
            hasVideo: false,
            isActiveSpeaker: false,
        })

        return slot
    }

    private disconnect(peerId: string): void {
        const slot = this.slots.get(peerId)
        if (!slot) return
        slot.pc.onnegotiationneeded = null
        slot.pc.onicecandidate = null
        slot.pc.ontrack = null
        slot.pc.onconnectionstatechange = null
        try { slot.pc.close() } catch (error) {
            this.fail(`closing connection to ${peerId}`, error)
        }
        this.emit("participant-left", {
            did: peerId,
            stream: slot.stream,
            hasAudio: false,
            hasVideo: false,
            isActiveSpeaker: false,
        })
        this.slots.delete(peerId)
        this.states.delete(peerId)
    }

    // ── Pending message buffer ──────────────────────────────────────

    /**
     * Signalling that arrived before the roster had caught up, kept
     * until it does.
     *
     * Dropping it was normally self-healing: both peers add tracks,
     * so whoever's offer was discarded fires `negotiationneeded` again
     * a moment later.  **A peer who denied the microphone has no
     * outbound tracks, so it never fires at all.**  They joined,
     * appeared on everyone's roster, and connected to nobody in either
     * direction — showing "Connecting…" forever, since
     * `connectionState` never reaches `failed` and the honest error
     * badge never appears.
     *
     * Bounded on both axes, because this buffers messages from agents
     * the roster has not vouched for and an unbounded one becomes a
     * memory target for anybody on the channel.  Overflow drops the
     * oldest: a stale offer carries less value than the one behind it.
     */
    private hold(peerId: string, message: CallMessage): void {
        if (!this.pending.has(peerId) && this.pending.size >= MAX_PENDING_PEERS) return
        const queue = this.pending.get(peerId) ?? []
        queue.push(message)
        if (queue.length > MAX_PENDING_PER_PEER) queue.shift()
        this.pending.set(peerId, queue)
    }

    /** Replay what this peer sent while we were still learning they
     *  belong here. */
    private release(peerId: string, slot: PeerSlot): void {
        const queue = this.pending.get(peerId)
        if (!queue) return
        this.pending.delete(peerId)
        for (const message of queue) void this.handleSignalling(peerId, slot, message)
    }

    // ── Message handling ────────────────────────────────────────────

    private async handleSignalling(peerId: string, slot: PeerSlot, message: CallMessage): Promise<void> {
        try {
            if (message.kind === "description") {
                const { description } = message as CallMessage & { kind: "description"; description: RTCSessionDescriptionInit }
                const collision =
                    description.type === "offer" &&
                    (slot.makingOffer || slot.pc.signalingState !== "stable")

                // The impolite peer wins a collision by ignoring the
                // other's offer; the polite peer yields, and
                // `setRemoteDescription` performs the implicit rollback
                // that lets it accept.
                slot.ignoreOffer = !slot.polite && collision
                if (slot.ignoreOffer) return

                await slot.pc.setRemoteDescription(description)
                if (description.type === "offer") {
                    await slot.pc.setLocalDescription()
                    if (slot.pc.localDescription) {
                        this.send(peerId, { kind: "description", description: slot.pc.localDescription })
                    }
                }
                return
            }

            if (message.kind === "ice") {
                const { candidate } = message as CallMessage & { kind: "ice"; candidate: RTCIceCandidateInit }
                try {
                    await slot.pc.addIceCandidate(candidate)
                } catch (error) {
                    // Candidates for an offer we deliberately ignored
                    // will fail, and that outcome stays expected — the
                    // connection they belong to was never established.
                    // Anything else counts as real.
                    if (!slot.ignoreOffer) throw error
                }
            }
        } catch (error) {
            this.fail(`handling ${message.kind} from ${peerId}`, error)
        }
    }
}
