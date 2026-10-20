/**
 * Peer-to-peer WebRTC mesh transport for the AD4M SDK Session interface.
 *
 * Ported from WE Framework's call module (`mesh.ts` and `protocol.ts`), which
 * stays wire-compatible with it: a WE mesh peer and an SDK mesh peer in the
 * same call negotiate with each other.
 *
 * ## Mesh, not SFU
 *
 * Every participant connects to every other, so N participants means N−1
 * connections each and each peer uploads its video N−1 times. That caps at
 * roughly four to six peers before upstream bandwidth becomes the limit. The
 * SFU path (`SfuManager`) handles larger rooms; this path needs no server.
 *
 * ## Membership comes from the roster; this class only negotiates
 *
 * {@link MeshManager.setRoster} receives whoever presence says is in the call.
 * Connections are reconciled against it: appear → connect, disappear → tear
 * down. There is no join or leave message, because a peer that crashes never
 * sends one.
 *
 * ## Perfect negotiation
 *
 * Both peers see each other join at slightly different moments and both fire
 * `negotiationneeded`, so offers collide. Each pair has a **polite** peer and
 * an **impolite** one, decided by comparing DIDs, which needs no agreement
 * round-trip. On collision the impolite peer ignores the incoming offer and
 * the polite peer rolls back its own.
 *
 * ## The signalling channel is not reliable
 *
 * Perfect negotiation assumes a reliable ordered channel. Telepresence is
 * neither: the executor confirms it took a message and nothing confirms a peer
 * received it, sends are dispatched concurrently, and the first broadcast on a
 * freshly joined neighbourhood has been measured at eighteen seconds. Against
 * that, a lost offer deadlocks a pair, a candidate overtaking its description
 * gets discarded, and concurrent handling corrupts the state machine. Four
 * mechanisms fix that:
 *
 * 1. **Per-peer serialisation.** Signalling for one peer runs one message at a
 *    time ({@link MeshManager.enqueue}).
 * 2. **Candidate buffering.** Candidates arriving before a remote description
 *    wait for one.
 * 3. **A recovery ladder.** ICE restart → resend the description → rebuild the
 *    pair, escalating, on a sweep ({@link MeshManager.tick}).
 * 4. **A `reset` message**, so a rebuild is agreed rather than unilateral.
 *
 * ## Which connection a message is about
 *
 * A pair has many connections over its life — a rebuild, a reset, a peer that
 * reloads — and the two sides routinely disagree for a moment about which one
 * is current. Applied to the wrong connection, a description is refused by the
 * browser ("The order of m-lines in subsequent offer doesn't match order from
 * previous offer/answer"). So every connection names itself
 * ({@link PeerSlot.generation}) and every message says which connection sent it
 * and which it is for ({@link CallGenerations}). See {@link MeshManager.placeOf}.
 *
 * ## Who repairs
 *
 * Recovery uses the same tie-break as offers: the impolite peer acts first.
 * The polite peer runs the same ladder on a doubled deadline, which covers the
 * case where the impolite side has no slot for the pair at all.
 *
 * ## Signalling via telepresence
 *
 * SDP and ICE travel through the neighbourhood's telepresence channel
 * (`sendBroadcastU` / `addSignalHandler`). See {@link createTelepresenceChannel}.
 */

import type { PerspectiveExpression, PerspectiveUnsignedInput } from "../perspectives/Perspective"
import type { SfuDataMessage } from "./SfuTypes"

// ── Protocol ────────────────────────────────────────────────────────

/**
 * Checked on receipt; a mismatch is dropped. New message kinds and the
 * generation fields were added without a bump: an old peer ignores a kind it
 * does not know and never reads the generations, so old and new peers still
 * converge. A bump would make them drop every message from each other.
 */
export const CALL_PROTOCOL_VERSION = 1

/**
 * Which of the sender's connections a message comes from, and which of the
 * receiver's it is for. Both are optional; a message without them is read as
 * it was before generations existed.
 */
export interface CallGenerations {
    /** The sender's connection, an id it chose when it made the connection. */
    gen?: string
    /** The receiver's connection as the sender last heard it named. */
    peerGen?: string
}

export type CallBody =
    /**
     * `offer` numbers the offers of one connection: an offer carries its own,
     * an answer the one it answers. An answer to an offer this side has since
     * replaced is refused by the browser, so it is dropped instead. Optional:
     * a peer predating it sends none, and its answers apply as before.
     */
    | { kind: "description"; description: RTCSessionDescriptionInit; offer?: number }
    | { kind: "ice"; candidate: RTCIceCandidateInit }
    /** Throw this pair's connection away and build a new one. */
    | { kind: "reset" }
    /** Application data, relayed over signalling because the mesh has no server. SDK-only. */
    | { kind: "data"; label: string; data: string; binary: boolean }

export interface CallEnvelope extends CallGenerations {
    v: number
    call: string
}

export type CallMessage = CallEnvelope & CallBody

/**
 * Narrow an untrusted payload off the transport.
 *
 * Everything arriving here was published by another agent, so it gets
 * shape-checked before use. The sender id comes from the transport rather
 * than the payload — a self-reported `from` would lack authentication.
 */
export function parseCallMessage(payload: unknown): CallMessage | null {
    if (typeof payload !== "object" || payload === null) return null
    const msg = payload as Partial<CallMessage>

    if (msg.v !== CALL_PROTOCOL_VERSION) return null
    if (typeof msg.call !== "string" || !msg.call) return null

    // A malformed generation is dropped rather than failing the message,
    // which leaves it read as a message from a peer that predates them.
    const envelope: CallEnvelope = { v: msg.v, call: msg.call }
    if (typeof msg.gen === "string" && msg.gen) envelope.gen = msg.gen
    if (typeof msg.peerGen === "string" && msg.peerGen) envelope.peerGen = msg.peerGen

    if (msg.kind === "description") {
        const description = (msg as { description?: unknown }).description
        if (typeof description !== "object" || description === null) return null
        const { type, sdp } = description as RTCSessionDescriptionInit
        if (type !== "offer" && type !== "answer" && type !== "pranswer" && type !== "rollback") return null
        if (sdp !== undefined && typeof sdp !== "string") return null
        const offer = (msg as { offer?: unknown }).offer
        return {
            ...envelope,
            kind: "description",
            description: { type, sdp },
            ...(Number.isSafeInteger(offer) ? { offer: offer as number } : {}),
        }
    }

    if (msg.kind === "ice") {
        const candidate = (msg as { candidate?: unknown }).candidate
        if (typeof candidate !== "object" || candidate === null) return null
        return { ...envelope, kind: "ice", candidate: candidate as RTCIceCandidateInit }
    }

    if (msg.kind === "reset") return { ...envelope, kind: "reset" }

    if (msg.kind === "data") {
        const { label, data, binary } = msg as { label?: unknown; data?: unknown; binary?: unknown }
        if (typeof label !== "string" || typeof data !== "string") return null
        return { ...envelope, kind: "data", label, data, binary: !!binary }
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
    /**
     * One result per publish, uncorrelated with the message. Optional: a
     * transport that cannot tell what became of a send must not pretend.
     * Where present, the mesh uses it to tell "the message was lost" from
     * "the message has not gone yet" — see {@link MeshManager.outstanding}.
     */
    onPublishResult?(cb: (result: { ok: boolean; ms: number }) => void): () => void
    /**
     * Settles once messages published from now on reach `onMessage`.
     * Optional: a transport that delivers from the moment `onMessage`
     * returns has nothing to wait for. A peer told about this one before it
     * listens would offer into the void until a repair resends.
     */
    listening?(): Promise<void>
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
 * broken in current executor builds. This adapter addresses over
 * broadcast: the recipient DID goes in the link's `target` and
 * receivers drop anything not addressed to them. That provides
 * **addressing, not privacy** — every peer still receives the payload.
 */
export function createTelepresenceChannel(
    proxy: TelepresenceProxy,
    selfDid: string,
): SignallingChannel {
    const resultListeners = new Set<(result: { ok: boolean; ms: number }) => void>()
    let subscribed: Promise<void> = Promise.resolve()
    const report = (result: { ok: boolean; ms: number }) => {
        for (const cb of resultListeners) {
            try { cb(result) } catch (e) {
                console.error("mesh: publish result listener error:", e)
            }
        }
    }

    return {
        publish(payload, to) {
            const started = Date.now()
            // Every publish must report, or the mesh counts it outstanding forever
            // and never repairs again — so a synchronous throw becomes a rejection.
            let sent: Promise<boolean>
            try {
                sent = proxy.sendBroadcastU({
                    links: [{
                        source: JSON.stringify(payload),
                        predicate: RTC_PREDICATE,
                        target: to?.agentId ?? TARGET_ALL,
                    }],
                })
            } catch (err) {
                sent = Promise.reject(err)
            }
            sent.then(
                (ok) => report({ ok: ok !== false, ms: Date.now() - started }),
                (err) => {
                    console.error("mesh: telepresence publish failed:", err)
                    report({ ok: false, ms: Date.now() - started })
                },
            )
        },

        onMessage(cb) {
            const handler = (signal: PerspectiveExpression) => {
                const link = signal?.data?.links?.[0]
                if (!link?.author) return

                /*
                 * The sender is the link's author, so it has to be one: the
                 * mesh acts on it (offers, answers, `reset`, rebuilds), and an
                 * author is only a claim until a signature backs it. The
                 * executor verifies both signatures on receipt and records the
                 * result in `proof.valid`. Accept a link only when the signal's
                 * signature and the link's own both hold, and both are by the
                 * same agent — a valid link relayed inside another agent's
                 * signal speaks for its author, not for whoever relayed it.
                 */
                if (signal.proof?.valid !== true || link.proof?.valid !== true) return
                if (link.author !== signal.author) return

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

            subscribed = proxy.addSignalHandler(handler).catch((err) => {
                console.warn("mesh: signal handler registration failed — inbound traffic will get missed:", err)
            })

            return () => proxy.removeSignalHandler(handler)
        },

        listening: () => subscribed,

        onPublishResult(cb) {
            resultListeners.add(cb)
            return () => resultListeners.delete(cb)
        },
    }
}

// ── Mesh manager ────────────────────────────────────────────────────

export type MeshEvent =
    | "participant-joined"
    | "participant-left"
    /** A participant's stream object or the liveness of its tracks changed:
     *  a rebuilt connection, or a remote track muted or unmuted. */
    | "participant-updated"
    | "stream-added"
    | "stream-removed"
    /** `(did, RTCPeerConnectionState)` */
    | "peer-state"
    /** `(did, { rung, attempts })` — a repair was attempted on this pair. */
    | "peer-recovery"
    | "error"

export type MeshEventCallback = (...args: any[]) => void

/** Which rung of the recovery ladder an attempt was on. */
export type RecoveryRung = "ice-restart" | "resend" | "rebuild"

export interface MeshParticipant {
    did: string
    stream: MediaStream
    hasAudio: boolean
    hasVideo: boolean
    isActiveSpeaker: boolean
}

export interface MeshManagerOptions {
    channel: SignallingChannel
    callId: string
    selfId: string
    /** NAT traversal. Defaults to {@link DEFAULT_ICE_SERVERS} — public STUN, no TURN. */
    iceServers?: RTCIceServer[]
    /** Injected so tests can supply a fake. Receives the configuration the mesh decided on. */
    createPeerConnection?: (configuration: RTCConfiguration) => RTCPeerConnection
    /** Injectable clock, so the recovery ladder is testable without waiting out real deadlines. */
    now?: () => number
    /** How often the mesh looks at its own connections. `0` disables the timer;
     *  {@link MeshManager.tick} still works, which is how tests drive it. */
    sweepMs?: number
    /** Called after a network change (see `ApiClient.onNetworkChange`): every
     *  pair restarts ICE then, rather than waiting for each to notice. */
    onNetworkChange?: (callback: () => void) => () => void
    /** Called when a pair goes disconnected: checks the socket the signals
     *  go over now (see `ApiClient.checkConnection`). A dead one is redialled
     *  and `onNetworkChange` restarts every pair. */
    checkConnection?: () => void
}

/** Public STUN, and no TURN. Two servers, because ICE queries them in parallel. */
export const DEFAULT_ICE_SERVERS: RTCIceServer[] = [
    { urls: ["stun:stun.l.google.com:19302", "stun:stun1.l.google.com:19302"] },
]

/**
 * How long a pair may sit unconnected before the impolite side intervenes.
 * Only has to outlast a healthy negotiation: a send still in flight defers
 * repair on its own (see {@link MeshManager.outstanding}).
 */
export const HANDSHAKE_GRACE_MS = 10_000

/** How long each rung of the ladder is given before escalating to the next. */
export const RECOVERY_INTERVAL_MS = 8_000

/**
 * How long `disconnected` is tolerated before an ICE restart. Shorter than the
 * others: `disconnected` often recovers by itself within a second or two.
 */
export const DISCONNECT_GRACE_MS = 5_000

/**
 * How many times a pair is repaired before the mesh stops and lets `failed`
 * stand. A pair that failed a full ladder four times needs TURN or a network,
 * and neither is fixed by a fifth attempt. Reset by a successful connection.
 */
export const MAX_RECOVERY_ATTEMPTS = 4

/** How often the mesh looks at its own connections, when not driven by hand. */
export const SWEEP_INTERVAL_MS = 2_000

/**
 * The shortest gap between two repairs, whatever asks for them. A settled send
 * calls `tick` and a repair publishes, so without a floor the two chase each
 * other through every pair in the call. Below {@link SWEEP_INTERVAL_MS} so the
 * ordinary cadence is never throttled.
 */
export const REPAIR_SPACING_MS = 1_500

/** Candidates held for a remote description that has not arrived. */
const MAX_EARLY_CANDIDATES = 64

/** How many of a peer's replaced connections are remembered, so a late message from one is refused. */
const MAX_RETIRED_PER_PEER = 16

/** Signalling held from agents the roster has not listed yet. Bounded: it buffers for strangers. */
const MAX_PENDING_PEERS = 16
const MAX_PENDING_PER_PEER = 32

/** A name for a new connection. Unique enough among one peer's connections, which is all it is compared with. */
const newGeneration = () => Math.random().toString(36).slice(2, 10)

/** The `RTCStats` fields {@link MeshManager.transportOf} reads. */
interface RtcStatReport {
    type?: string
    id?: string
    state?: string
    localCandidateId?: string
    candidateType?: string
}

interface PeerSlot {
    pc: RTCPeerConnection
    /** This connection's name, sent on everything it says. */
    generation: string
    /** The peer's connection this one is talking to, once the peer has named it. */
    remoteGeneration: string | null
    /** Decided by DID comparison — symmetric, so the two peers always disagree. */
    polite: boolean
    makingOffer: boolean
    ignoreOffer: boolean
    stream: MediaStream
    /**
     * The sender carrying each kind, remembered rather than looked up:
     * `getSenders().find((s) => s.track?.kind === kind)` cannot find a sender
     * whose track is `null`, which `replaceTrack(null)` leaves behind when a
     * screen share stops. Sharing again then added a second video track and
     * the peer's view froze on the dead first one.
     */
    senders: Map<"audio" | "video", RTCRtpSender>
    /** The tail of this peer's signalling, so messages are applied one at a time. */
    work: Promise<void>
    /** A remote description has been applied, so candidates can be too. */
    remoteReady: boolean
    /** Candidates that cannot be applied yet, each with the peer connection that gathered it. */
    earlyCandidates: { gen?: string; candidate: RTCIceCandidateInit }[]
    /** The last description this side sent, so a lost one can be sent again rather than rebuilt. */
    lastSent: Extract<CallBody, { kind: "description" }> | null
    /** How many offers this connection has made: the last one's `offer` number. */
    offers: number
    /** The `offer` number of the peer's last applied offer, which this side's answer names. */
    remoteOffer: number | undefined
    /** The `sdp` already resent once. Resending is worth exactly one try per description. */
    lastResent: string | null
    /** Connected at least once: what broke is the transport, not a message. */
    everConnected: boolean
    /**
     * When this side sent an offer that has had no answer yet, or 0. Tracked
     * apart from the connection state because a pair renegotiates after it
     * connects: the answerer's media travels in its own offer, sent once the
     * pair is up. See {@link MeshManager.dueForRepair}.
     */
    offeredAt: number
    /** The pending offer was made while the pair was already connected: a renegotiation. */
    renegotiating: boolean
    /** An offer this side held back until the pair connects; see `onnegotiationneeded`. */
    offerDeferred: boolean
    /** When this slot was created or last rebuilt — where the handshake deadline counts from. */
    since: number
    /** When the last repair was attempted, so rungs are spaced rather than fired in a burst. */
    lastAttempt: number
    /** How many repairs this pair has had. Reset by a successful connection. */
    attempts: number
    /** When ICE first reported `disconnected`, or 0. */
    disconnectedAt: number
}

/** Whether a remote track currently carries media. A peer that leaves without
 *  saying so leaves its tracks `live` but `muted`. */
const isLive = (track: MediaStreamTrack) => track.readyState !== "ended" && !track.muted

export class MeshManager {
    private channel: SignallingChannel
    private callId: string
    private selfId: string
    private configuration: RTCConfiguration
    private createPeerConnection: (configuration: RTCConfiguration) => RTCPeerConnection
    private now: () => number
    private sweepMs: number
    private sweep: ReturnType<typeof setInterval> | null = null
    private callbacks: Map<MeshEvent, MeshEventCallback[]> = new Map()
    private slots: Map<string, PeerSlot> = new Map()
    private states: Map<string, RTCPeerConnectionState> = new Map()
    private outbound: Map<"audio" | "video", MediaStreamTrack | null> = new Map()
    /** One stream for all outbound tracks, so a peer receives mic and camera grouped. */
    private outboundStream: MediaStream | null = null
    private closed = false
    private channelUnsubscribe: (() => void) | null = null
    private unwatchPublish: (() => void) | null = null
    private pending: Map<string, CallMessage[]> = new Map()
    /** The peer connections each peer has replaced. Kept past a slot, so a late message cannot revive one. */
    private retired: Map<string, string[]> = new Map()
    private dataListeners: Set<(msg: SfuDataMessage) => void> = new Set()
    /**
     * Sends published and not yet reported on. With an unreliable transport,
     * "the peer never answered" and "the message has not left yet" look the
     * same, and repairing the second piles sends onto a stalled executor. Stays
     * zero on a transport without `onPublishResult`.
     */
    private outstanding = 0
    /** When a sweep last repaired something — see {@link REPAIR_SPACING_MS}. */
    private lastRepairAt = 0
    /**
     * Repairs of a connected pair's unanswered renegotiation, per peer, kept
     * across rebuilds. A slot's `attempts` resets when its connection connects,
     * which a rebuilt pair does before it renegotiates again — so a peer that
     * never answers a renegotiation would be resent to and rebuilt forever.
     * Cleared only when a renegotiation is answered.
     */
    private renegotiationRepairs: Map<string, number> = new Map()
    private onNetworkChange: MeshManagerOptions["onNetworkChange"]
    private unwatchNetwork: (() => void) | null = null
    private checkConnection: MeshManagerOptions["checkConnection"]

    constructor(options: MeshManagerOptions) {
        this.channel = options.channel
        this.callId = options.callId
        this.selfId = options.selfId
        this.configuration = {
            iceServers: options.iceServers?.length ? options.iceServers : DEFAULT_ICE_SERVERS,
            // Gather one candidate set before the handshake asks for it, so the
            // STUN round trip is spent while the user reads the permission prompt.
            iceCandidatePoolSize: 1,
        }
        this.createPeerConnection = options.createPeerConnection
            ?? ((configuration) => new RTCPeerConnection(configuration))
        this.now = options.now ?? (() => Date.now())
        this.sweepMs = options.sweepMs ?? SWEEP_INTERVAL_MS
        this.onNetworkChange = options.onNetworkChange
        this.checkConnection = options.checkConnection
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

    private participant(peerId: string, slot: PeerSlot): MeshParticipant {
        return {
            did: peerId,
            stream: slot.stream,
            hasAudio: slot.stream.getAudioTracks().some(isLive),
            hasVideo: slot.stream.getVideoTracks().some(isLive),
            isActiveSpeaker: false,
        }
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

        if (typeof this.channel.onPublishResult === "function") {
            this.unwatchPublish = this.channel.onPublishResult(() => {
                this.outstanding = Math.max(0, this.outstanding - 1)
                // A send whose outcome is known is a reason to look now. Deferred,
                // because a transport may report from inside `publish` and a repair
                // publishes — a direct call would re-enter the sweep mid-repair.
                queueMicrotask(() => this.tick())
            })
        }

        this.channelUnsubscribe = this.channel.onMessage((from, payload) => {
            if (this.closed || from === this.selfId) return

            const message = parseCallMessage(payload)
            if (!message || message.call !== this.callId) return

            // The recipient is carried in the payload too, so a fanout-only
            // transport cannot make a bystander negotiate. Data goes to all.
            const to = (payload as { to?: unknown }).to
            if (typeof to === "string" && to !== TARGET_ALL && to !== this.selfId) return

            // Only peers the roster placed in the call. Without this, anyone on
            // the channel could open a connection by sending an offer.
            const slot = this.slots.get(from)

            if (message.kind === "data") {
                if (slot) this.handleData(from, message)
                return
            }

            if (!slot) {
                this.hold(from, message)
                return
            }

            this.enqueue(from, slot, message)
        })
        // A session announces itself once `join` returns; peers offer at once.
        await this.channel.listening?.()

        /*
         * Spend the transport's first-send cost on something that does not
         * matter. The first broadcast on a freshly joined neighbourhood has
         * been measured at eighteen seconds and every one after it at tens of
         * milliseconds. Without this, the message paying that cost is the
         * first SDP offer. It parses to nothing on every peer — no `kind`.
         */
        this.publish({ v: CALL_PROTOCOL_VERSION, call: this.callId, warm: true })

        if (this.sweepMs > 0) this.sweep = setInterval(() => this.tick(), this.sweepMs)
        this.unwatchNetwork = this.onNetworkChange?.(() => this.restartAll()) ?? null
    }

    async leave(): Promise<void> {
        this.closed = true
        if (this.sweep) { clearInterval(this.sweep); this.sweep = null }
        this.unwatchNetwork?.()
        this.unwatchNetwork = null
        this.unwatchPublish?.()
        this.unwatchPublish = null
        if (this.channelUnsubscribe) {
            this.channelUnsubscribe()
            this.channelUnsubscribe = null
        }
        this.pending.clear()
        this.renegotiationRepairs.clear()
        for (const peerId of [...this.slots.keys()]) this.removePeer(peerId)
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
     * call. Excludes this agent.
     */
    setRoster(peerIds: string[]): void {
        if (this.closed) return
        const wanted = new Set(peerIds.filter((id) => id !== this.selfId))

        for (const peerId of [...this.slots.keys()]) {
            if (!wanted.has(peerId)) this.removePeer(peerId)
        }
        for (const peerId of wanted) {
            if (this.slots.has(peerId)) continue
            const slot = this.connect(peerId)
            this.emit("participant-joined", this.participant(peerId, slot))
            this.release(peerId, slot)
        }
        // Anything held for an agent the roster does not list will not get wanted.
        for (const peerId of [...this.pending.keys()]) {
            if (!wanted.has(peerId)) this.pending.delete(peerId)
        }
    }

    // ── Participants and diagnostics ────────────────────────────────

    getParticipants(): MeshParticipant[] {
        return [...this.slots].map(([peerId, slot]) => this.participant(peerId, slot))
    }

    peerStates(): Map<string, RTCPeerConnectionState> {
        return new Map(this.states)
    }

    /**
     * Throw one pair's connection away and negotiate it again, telling the
     * peer to do the same. The manual rung of the ladder: reachable from either
     * side regardless of politeness, and it resets the automatic budget.
     */
    reconnect(peerId: string): void {
        const slot = this.slots.get(peerId)
        if (this.closed || !slot) return
        slot.attempts = 0
        this.renegotiationRepairs.delete(peerId)
        this.rebuild(peerId, { announce: true, rung: "rebuild" })
    }

    /**
     * How this pair's media reaches the other end — `host` on a LAN, `srflx`
     * through NAT, `relay` through TURN — or `null` when nothing is connected.
     * Separates "this pair needs TURN" from "the handshake never finished".
     */
    async transportOf(peerId: string): Promise<string | null> {
        const slot = this.slots.get(peerId)
        if (!slot || typeof slot.pc.getStats !== "function") return null
        try {
            const reports: RtcStatReport[] = []
            ;(await slot.pc.getStats()).forEach((report: RtcStatReport) => reports.push(report))

            const winner = reports.find((report) => report.type === "candidate-pair" && report.state === "succeeded")
            const localId = winner?.localCandidateId
            if (!localId) return null

            const local = reports.find((report) => report.type === "local-candidate" && report.id === localId)
            return local?.candidateType ?? null
        } catch (error) {
            this.fail(`reading transport stats for ${peerId}`, error)
            return null
        }
    }

    // ── Track replacement ────────────────────────────────────────────

    /**
     * Replace the outbound track of a given kind on every peer connection.
     * Uses `RTCRtpSender.replaceTrack` on the sections declared up front, so
     * muting, unmuting and camera↔screen swaps never renegotiate. Pass `null`
     * to stop sending that kind.
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
                        // Only reachable if `addTransceiver` failed. This fires
                        // `negotiationneeded`, which perfect negotiation resolves.
                        slot.senders.set(kind, slot.pc.addTrack(track, this.outboundStream!))
                    }
                } catch (error) {
                    this.fail(`replacing ${kind} track for ${peerId}`, error)
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
        this.publish({ v: CALL_PROTOCOL_VERSION, call: this.callId, to: TARGET_ALL, kind: "data", label, data, binary })
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

    // ── Sending ─────────────────────────────────────────────────────

    /** Every publish goes through here, so {@link outstanding} counts exactly the reported ones. */
    private publish(payload: object, to?: { agentId?: string }): void {
        if (this.unwatchPublish) this.outstanding += 1
        this.channel.publish(payload, to)
    }

    /**
     * Addressed twice, on purpose: `to` on the publish lets a transport with
     * native unicast send to one peer, and `to` in the payload makes a
     * fanout-only transport filter on receipt.
     */
    private send(to: string, message: CallBody & CallGenerations): void {
        this.publish({ v: CALL_PROTOCOL_VERSION, call: this.callId, to, ...message }, { agentId: to })
    }

    /** Which connections a message from this slot is between. */
    private stamp(slot: PeerSlot): CallGenerations {
        return slot.remoteGeneration ? { gen: slot.generation, peerGen: slot.remoteGeneration } : { gen: slot.generation }
    }

    private sendFrom(peerId: string, slot: PeerSlot, message: CallBody): void {
        this.send(peerId, { ...message, ...this.stamp(slot) })
    }

    /** Send the connection's local description, numbered as {@link CallBody} describes. */
    private sendDescription(peerId: string, slot: PeerSlot): void {
        const { type, sdp } = slot.pc.localDescription!
        const offer = type === "offer" ? ++slot.offers : slot.remoteOffer
        slot.lastSent = { kind: "description", description: { type, sdp }, ...(offer !== undefined ? { offer } : {}) }
        this.sendFrom(peerId, slot, slot.lastSent)
    }

    private retire(peerId: string, generation: string | null | undefined): void {
        if (!generation) return
        const list = this.retired.get(peerId) ?? []
        if (list.includes(generation)) return
        list.push(generation)
        if (list.length > MAX_RETIRED_PER_PEER) list.shift()
        this.retired.set(peerId, list)
    }

    // ── Peer connection management ──────────────────────────────────

    private connect(peerId: string): PeerSlot {
        const existing = this.slots.get(peerId)
        if (existing) return existing

        const pc = this.createPeerConnection(this.configuration)
        const slot: PeerSlot = {
            pc,
            generation: newGeneration(),
            remoteGeneration: null,
            // Comparing DIDs gives each pair exactly one polite side
            // without a round trip.
            polite: this.selfId > peerId,
            makingOffer: false,
            ignoreOffer: false,
            stream: new MediaStream(),
            senders: new Map(),
            work: Promise.resolve(),
            remoteReady: false,
            earlyCandidates: [],
            lastSent: null,
            offers: 0,
            remoteOffer: undefined,
            lastResent: null,
            everConnected: false,
            offeredAt: 0,
            renegotiating: false,
            offerDeferred: false,
            since: this.now(),
            lastAttempt: 0,
            attempts: 0,
            disconnectedAt: 0,
        }
        this.slots.set(peerId, slot)

        const negotiate = async () => {
            /*
             * An answerer's first offer waits until the pair connects. Before
             * that this side cannot know its answer arrived; if it was lost,
             * the peer still has its offer out, reads this offer as glare and
             * negotiates afresh, and both sides take the same DTLS role
             * ("Failed to set SSL role"). Connecting proves the peer applied
             * the answer: DTLS needs the fingerprint it carries. Once
             * connected the roles are fixed, so later offers go at once.
             * Read from the connection: the browser can fire this while the
             * peer's offer is being applied, before any answer exists.
             */
            if (pc.remoteDescription?.type === "offer" && !slot.everConnected) {
                slot.offerDeferred = true
                return
            }
            slot.offerDeferred = false
            try {
                slot.makingOffer = true
                /*
                 * Always an offer, never "whatever the state calls for". The
                 * browser can fire this during the rollback that accepting the
                 * peer's offer starts; a description picked from the state when
                 * the call runs is then the answer, and the answering path
                 * makes an offer in its place. If the peer's offer got here
                 * first the browser refuses, and asks again once stable.
                 */
                await pc.setLocalDescription({ type: "offer" })
                slot.offeredAt = this.now()
                slot.renegotiating = pc.connectionState === "connected"
                this.sendDescription(peerId, slot)
            } catch (error) {
                if (pc.signalingState === "stable") this.fail(`negotiating with ${peerId}`, error)
            } finally {
                slot.makingOffer = false
            }
        }
        pc.onnegotiationneeded = negotiate

        pc.onicecandidate = ({ candidate }) => {
            if (candidate) this.sendFrom(peerId, slot, { kind: "ice", candidate: candidate.toJSON() })
        }

        pc.ontrack = ({ track }) => {
            slot.stream.addTrack(track)
            // Track events outlive the connection; a replaced one reports nothing.
            const current = () => this.slots.get(peerId) === slot
            const updated = () => {
                if (current()) this.emit("participant-updated", this.participant(peerId, slot))
            }
            // A replaced track (camera → screen) arrives as a new track and
            // the old one ends. Without this the tile would keep rendering it.
            track.addEventListener("ended", () => {
                slot.stream.removeTrack(track)
                if (!current()) return
                this.emit("stream-removed", slot.stream, track)
                updated()
            })
            /*
             * A remote track that stops receiving goes MUTED, not ended. When a
             * peer leaves, crashes or drops off the network the track stays
             * `live` for as long as the connection exists, and a `<video>`
             * goes on painting the last decoded frame. Both edges, because a
             * connection that recovers unmutes the same track.
             */
            track.addEventListener("mute", updated)
            track.addEventListener("unmute", updated)
            this.emit("stream-added", slot.stream, track)
        }

        pc.onconnectionstatechange = () => {
            const state = pc.connectionState
            this.states.set(peerId, state)
            if (state === "connected") {
                slot.everConnected = true
                // The budget bounds one failure, not a session: a call that
                // drops and recovers twice must not run out of repairs.
                slot.attempts = 0
                slot.disconnectedAt = 0
                // Keep only candidates from a peer connection this one has not met.
                slot.earlyCandidates = slot.earlyCandidates.filter((held) => held.gen && held.gen !== slot.remoteGeneration)
                if (slot.offerDeferred && pc.signalingState === "stable") void negotiate()
            }
            if (state === "disconnected" && !slot.disconnectedAt) {
                slot.disconnectedAt = this.now()
                this.checkConnection?.()
            }
            if (state === "connected" || state === "failed") slot.disconnectedAt = 0
            this.emit("peer-state", peerId, state)
            // `failed` is actionable at once — there is nothing in flight to wait for.
            if (state === "failed") this.tick()
        }

        /*
         * Both m-lines, before there is anything to put in them.
         *
         * A connection can only carry media of a kind it negotiated a section
         * for, and `addTrack` creates one. An agent sending no video negotiated
         * no video section, which left the *other* peer's camera with nowhere
         * to arrive. Declaring both up front makes every track change a
         * `replaceTrack` on an existing section.
         */
        for (const kind of ["audio", "video"] as const) {
            try {
                slot.senders.set(
                    kind,
                    pc.addTransceiver(kind, {
                        direction: "sendrecv",
                        streams: this.outboundStream ? [this.outboundStream] : [],
                    }).sender,
                )
            } catch (error) {
                this.fail(`preparing ${kind} for ${peerId}`, error)
            }
        }

        // Attach whatever already gets sent, so a peer joining mid-call
        // receives our media without waiting for a toggle.
        for (const [kind, track] of this.outbound) {
            if (!track) continue
            const sender = slot.senders.get(kind)
            if (!sender) continue
            void sender.replaceTrack(track).catch((error) =>
                this.fail(`sending ${kind} to ${peerId}`, error),
            )
        }

        return slot
    }

    /** Close one pair's connection, without telling anyone it happened. */
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
        this.slots.delete(peerId)
        this.states.delete(peerId)
        // A late message from the peer's connection must not be mistaken for a
        // new one when the peer returns.
        this.retire(peerId, slot.remoteGeneration)
        // Held signalling belongs to the connection going away.
        this.pending.delete(peerId)
    }

    /** The peer left the call. */
    private removePeer(peerId: string): void {
        const slot = this.slots.get(peerId)
        if (!slot) return
        this.disconnect(peerId)
        this.renegotiationRepairs.delete(peerId)
        this.emit("participant-left", this.participant(peerId, slot))
    }

    // ── Pending message buffer ──────────────────────────────────────

    /**
     * Signalling that arrived before the roster caught up, kept until it does.
     *
     * A peer who denied the microphone has no outbound tracks, so it never
     * fires `negotiationneeded` again after its offer is dropped. It joined and
     * connected to nobody. Bounded on both axes, because this buffers messages
     * from agents the roster has not vouched for. Overflow drops the oldest.
     */
    private hold(peerId: string, message: CallMessage): void {
        if (!this.pending.has(peerId) && this.pending.size >= MAX_PENDING_PEERS) return
        const queue = this.pending.get(peerId) ?? []
        queue.push(message)
        // An offer is followed by its candidates; dropping the oldest message
        // would drop the offer, which the pair cannot start without. Drop the
        // oldest message that is not a description or a reset instead.
        while (queue.length > MAX_PENDING_PER_PEER) {
            const spare = queue.findIndex((held) => held.kind !== "description" && held.kind !== "reset")
            queue.splice(spare === -1 ? 0 : spare, 1)
        }
        this.pending.set(peerId, queue)
    }

    /** Replay what this peer sent while we were still learning it belongs here. */
    private release(peerId: string, slot: PeerSlot): void {
        const queue = this.pending.get(peerId)
        if (!queue) return
        this.pending.delete(peerId)
        // Enqueued rather than started all at once, on a connection that is
        // also about to fire `negotiationneeded` for its new transceivers.
        for (const message of queue) this.enqueue(peerId, slot, message)
    }

    // ── Message handling ────────────────────────────────────────────

    /** Put one message on this peer's queue. The chain never rejects — `handle` reports its own failures. */
    private enqueue(peerId: string, slot: PeerSlot, message: CallMessage): void {
        slot.work = slot.work.then(async () => {
            if (this.closed) return
            const current = this.slots.get(peerId)
            if (!current) return
            // The pair was rebuilt while this waited. A message that names its
            // connections follows the pair to its replacement and gets judged
            // there; one that does not is about a connection that is gone.
            if (current !== slot) {
                if (message.gen) this.enqueue(peerId, current, message)
                return
            }
            await this.handle(peerId, slot, message)
        })
    }

    /**
     * Where a message stands against the connection this side holds for its sender.
     *
     * - `current` — from the peer connection this one talks to, or the first to
     *   name itself, or it names nothing (a peer predating generations).
     * - `stale` — for a connection of this side's that was replaced, or from
     *   one of the peer's that was. Nothing it says applies.
     * - `newer` — from a peer connection this side has not met: the peer has started over.
     */
    private placeOf(peerId: string, slot: PeerSlot, message: CallMessage): "current" | "stale" | "newer" {
        if (message.peerGen && message.peerGen !== slot.generation) return "stale"
        if (!message.gen) return "current"
        if (this.retired.get(peerId)?.includes(message.gen)) return "stale"
        if (!slot.remoteGeneration || slot.remoteGeneration === message.gen) return "current"
        return "newer"
    }

    private async handle(peerId: string, slot: PeerSlot, message: CallMessage): Promise<void> {
        try {
            const place = this.placeOf(peerId, slot, message)
            if (place === "stale") return
            if (place === "current" && message.gen && !slot.remoteGeneration) slot.remoteGeneration = message.gen

            if (message.kind === "reset") {
                // The peer is starting over. Rebuild to match and say nothing
                // back — two peers answering resets with resets is a loop.
                this.retire(peerId, message.gen)
                this.rebuild(peerId, { announce: false, rung: "rebuild" })
                return
            }

            if (place === "newer") {
                // A candidate can overtake the offer it belongs to, so it waits.
                if (message.kind === "ice") {
                    if (slot.earlyCandidates.length < MAX_EARLY_CANDIDATES) {
                        slot.earlyCandidates.push({ gen: message.gen, candidate: message.candidate })
                    }
                    return
                }
                /*
                 * The peer has a connection this side never heard from: a
                 * reload, a rebuild whose reset was lost, a reset this offer
                 * overtook. Applying its offer here is the "order of m-lines"
                 * refusal, so this side starts over too, and applies the offer
                 * at once — an offer of its own would collide with it. An
                 * answer from an unknown connection has nothing to apply to;
                 * the rebuilt side offers afresh.
                 */
                const next = this.rebuild(peerId, { announce: false, rung: "rebuild" })
                next.earlyCandidates = slot.earlyCandidates.filter((held) => held.gen === message.gen)
                if (message.kind === "description" && message.description.type === "offer") {
                    next.remoteGeneration = message.gen ?? null
                    next.work = this.handle(peerId, next, message)
                }
                return
            }

            if (message.kind === "description") {
                const { description } = message
                // An offer older than one already applied — overtaken on this
                // unordered transport — describes m-lines the connection has
                // moved past. The same number again is the resend rung's copy,
                // and is answered.
                if (
                    description.type === "offer" &&
                    message.offer !== undefined &&
                    slot.remoteOffer !== undefined &&
                    message.offer < slot.remoteOffer
                ) return
                const collision =
                    description.type === "offer" &&
                    (slot.makingOffer || slot.pc.signalingState !== "stable")

                // The impolite peer wins a collision by ignoring the other's
                // offer; the polite peer yields, and `setRemoteDescription`
                // performs the implicit rollback that lets it accept.
                slot.ignoreOffer = !slot.polite && collision
                if (slot.ignoreOffer) return
                // An answer with no offer out is a late duplicate — the
                // resend rung got one answer for each copy. Expected on this
                // transport, so not an error.
                if (description.type === "answer" && slot.pc.signalingState === "stable") return
                // An answer to an earlier offer of this connection — a late copy
                // from the resend rung — would be refused against the current one.
                if (description.type === "answer" && message.offer !== undefined && message.offer !== slot.offers) return

                await slot.pc.setRemoteDescription(description)
                if (description.type === "offer") slot.remoteOffer = message.offer
                // An answer settles this side's offer, and accepting the
                // peer's offer rolled it back. Either way none is pending.
                // An answered renegotiation is the evidence the connected-pair
                // repair waits for, so only that clears its budget.
                if (description.type === "answer" && slot.renegotiating) {
                    this.renegotiationRepairs.delete(peerId)
                    if (slot.pc.connectionState === "connected") slot.attempts = 0
                }
                slot.offeredAt = 0
                slot.renegotiating = false
                // Candidates become applicable exactly here. On an unordered
                // transport a candidate overtaking its offer is the normal case.
                slot.remoteReady = true
                await this.flushCandidates(peerId, slot)

                if (description.type === "offer") {
                    await slot.pc.setLocalDescription({ type: "answer" })
                    this.sendDescription(peerId, slot)
                }
                return
            }

            if (message.kind !== "ice") return

            if (!slot.remoteReady) {
                if (slot.earlyCandidates.length < MAX_EARLY_CANDIDATES) {
                    slot.earlyCandidates.push({ gen: message.gen, candidate: message.candidate })
                }
                return
            }

            try {
                await slot.pc.addIceCandidate(message.candidate)
            } catch (error) {
                // Candidates for an offer we deliberately ignored will fail,
                // and that is expected. Anything else is real.
                if (!slot.ignoreOffer) throw error
            }
        } catch (error) {
            this.fail(`handling ${message.kind} from ${peerId}`, error)
        }
    }

    private async flushCandidates(peerId: string, slot: PeerSlot): Promise<void> {
        if (!slot.earlyCandidates.length) return
        // Only this connection's. A candidate from a peer connection it has not met stays held.
        const mine = (held: { gen?: string }) => !held.gen || held.gen === slot.remoteGeneration
        const ready = slot.earlyCandidates.filter(mine).map((held) => held.candidate)
        slot.earlyCandidates = slot.earlyCandidates.filter((held) => !mine(held))
        for (const candidate of ready) {
            try {
                await slot.pc.addIceCandidate(candidate)
            } catch (error) {
                // One stale candidate must not stop the rest.
                this.fail(`applying a held candidate from ${peerId}`, error)
            }
        }
    }

    // ── Recovery ────────────────────────────────────────────────────

    /**
     * Throw this pair away and start again. Attempts carry over to the new
     * slot, or a pair that rebuilds every time would never reach
     * {@link MAX_RECOVERY_ATTEMPTS}.
     */
    private rebuild(peerId: string, opts: { announce: boolean; rung: RecoveryRung }): PeerSlot {
        const previous = this.slots.get(peerId)
        const attempts = (previous?.attempts ?? 0) + 1
        // Sent from the connection being thrown away, so the peer can tell a late reset from a new one.
        const farewell = previous ? this.stamp(previous) : {}
        this.disconnect(peerId)
        if (opts.announce) this.send(peerId, { kind: "reset", ...farewell })
        const slot = this.connect(peerId)
        slot.attempts = attempts
        slot.lastAttempt = this.now()
        // The participant's stream is a new object; consumers holding the old one render a dead stream.
        this.emit("participant-updated", this.participant(peerId, slot))
        this.emit("peer-recovery", peerId, { rung: opts.rung, attempts })
        return slot
    }

    /**
     * One pair, one repair, on whichever rung its state calls for, cheapest first:
     *
     * 1. **ICE restart.** Keeps the connection and DTLS session; only
     *    re-gathers the transport. For `failed` and a `disconnected` that did
     *    not come back.
     * 2. **Resend the description.** For a handshake that never completed,
     *    which on this transport means a message was dropped — either half.
     * 3. **Rebuild.** When there is nothing to resend, or the rungs above were tried.
     */
    private repair(peerId: string, slot: PeerSlot): void {
        slot.lastAttempt = this.now()
        const attempts = slot.attempts + 1
        slot.attempts = attempts

        const state = slot.pc.connectionState
        const canRestart = typeof slot.pc.restartIce === "function"

        if ((state === "failed" || state === "disconnected") && canRestart && slot.remoteReady) {
            try {
                slot.pc.restartIce()
                this.emit("peer-recovery", peerId, { rung: "ice-restart", attempts })
                return
            } catch (error) {
                this.fail(`restarting ice with ${peerId}`, error)
            }
        }

        const unanswered = slot.offeredAt > 0 && slot.pc.signalingState === "have-local-offer"
        if (state === "connected") {
            this.renegotiationRepairs.set(peerId, (this.renegotiationRepairs.get(peerId) ?? 0) + 1)
        }
        if ((!slot.everConnected || unanswered) && slot.lastSent && slot.lastSent.description.sdp !== slot.lastResent) {
            slot.lastResent = slot.lastSent.description.sdp ?? null
            this.sendFrom(peerId, slot, slot.lastSent)
            this.emit("peer-recovery", peerId, { rung: "resend", attempts })
            return
        }

        // Undo the increment `rebuild` is about to make again, so one repair counts once.
        slot.attempts = attempts - 1
        this.rebuild(peerId, { announce: true, rung: "rebuild" })
    }

    /**
     * The network changed: every pair's path is likely gone, though ICE has
     * noticed on none yet. Each negotiated pair takes the ladder's first rung
     * now, all at once — an ICE restart is one offer per pair, and waiting
     * for each to time out costs seconds of silence. Pairs still negotiating
     * have nothing to restart; the sweep keeps looking after them.
     */
    private restartAll(): void {
        if (this.closed) return
        const at = this.now()
        for (const [peerId, slot] of this.slots) {
            if (!slot.remoteReady || typeof slot.pc.restartIce !== "function") continue
            slot.lastAttempt = at
            slot.attempts += 1
            try {
                slot.pc.restartIce()
                this.emit("peer-recovery", peerId, { rung: "ice-restart", attempts: slot.attempts })
            } catch (error) {
                this.fail(`restarting ice with ${peerId}`, error)
            }
        }
        this.lastRepairAt = at
    }

    /** Whether this pair has been given long enough, and whether it is this side's turn to act. */
    private dueForRepair(peerId: string, slot: PeerSlot, at: number): boolean {
        const state = slot.pc.connectionState
        if (slot.attempts >= MAX_RECOVERY_ATTEMPTS) return false
        /*
         * Connected, with an offer of this side's never answered. A pair
         * renegotiates after it connects — the answerer's media travels in
         * its own offer — and on a reordering transport that offer can reach
         * an impolite peer before the answer it follows, which reads it as
         * glare and ignores it. Nothing else revisits a connected pair, so
         * the media in that direction would never arrive. Only this side can
         * see the offer is pending, so it acts without the polite wait.
         */
        if (state === "connected") {
            if (!slot.offeredAt || slot.pc.signalingState !== "have-local-offer") return false
            if ((this.renegotiationRepairs.get(peerId) ?? 0) >= MAX_RECOVERY_ATTEMPTS) return false
            return at - Math.max(slot.offeredAt, slot.lastAttempt) >= RECOVERY_INTERVAL_MS
        }

        // The impolite peer repairs first; the polite one waits twice as long,
        // which covers an impolite side whose roster never listed this peer.
        const patience = slot.polite ? 2 : 1

        if (state === "disconnected" && slot.disconnectedAt) {
            return at - slot.disconnectedAt >= DISCONNECT_GRACE_MS * patience
        }
        if (state === "failed") {
            return at - Math.max(slot.lastAttempt, slot.since) >= RECOVERY_INTERVAL_MS * patience
        }
        // 'new' | 'connecting' | 'closed' — a handshake that has not finished.
        const from = slot.lastAttempt || slot.since
        const deadline = slot.lastAttempt ? RECOVERY_INTERVAL_MS : HANDSHAKE_GRACE_MS
        return at - from >= deadline * patience
    }

    /**
     * Look at every connection and repair what is stuck. Driven by an internal
     * timer; public because tests drive it by hand.
     */
    tick(): void {
        if (this.closed) return
        // Nothing is repaired while a send is unaccounted for: an offer not
        // yet delivered and an offer lost look the same from here.
        if (this.outstanding > 0) return
        const at = this.now()
        if (this.lastRepairAt && at - this.lastRepairAt < REPAIR_SPACING_MS) return
        for (const [peerId, slot] of [...this.slots]) {
            if (!this.dueForRepair(peerId, slot, at)) continue
            this.lastRepairAt = at
            this.repair(peerId, slot)
            // One pair per sweep: a network that just came back has every pair
            // due at once, and a burst of offers is what made them fail.
            break
        }
    }
}
