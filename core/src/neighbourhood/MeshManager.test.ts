/**
 * MeshManager unit tests: two or more agents negotiating in one process.
 *
 * Ported from WE Framework's `packages/module-system/call/src/mesh.test.ts`,
 * which tests the same algorithm behind a function API. The mesh is the part
 * that cannot be checked by reading it: perfect negotiation is a protocol
 * between two peers whose bugs (glare, a lost candidate, a connection that
 * never tears down) only appear when both sides run.
 *
 * The `RTCPeerConnection` fake is deliberately thin: it models signalling
 * state and the callbacks the mesh drives, and nothing about media. What is
 * under test is the negotiation, not the browser. Nothing sleeps: a hand-moved
 * clock and `mesh.tick()` stand in for the sweep timer (`sweepMs: 0`).
 */

// ── Browser shims for Node/Jest ─────────────────────────────────────

/**
 * The mesh groups its outbound tracks into one stream and collects each
 * peer's inbound tracks into another, so it constructs `MediaStream` even
 * when no media exists. Node has no such global.
 */
class FakeMediaStream {
    #tracks: unknown[]
    constructor(tracks: unknown[] = []) {
        this.#tracks = [...tracks]
    }
    getTracks() {
        return [...this.#tracks]
    }
    getVideoTracks() {
        return this.#tracks.filter((track) => (track as { kind?: string }).kind === "video")
    }
    getAudioTracks() {
        return this.#tracks.filter((track) => (track as { kind?: string }).kind === "audio")
    }
    addTrack(track: unknown) {
        if (!this.#tracks.includes(track)) this.#tracks.push(track)
    }
    removeTrack(track: unknown) {
        const i = this.#tracks.indexOf(track)
        if (i !== -1) this.#tracks.splice(i, 1)
    }
}
;(globalThis as { MediaStream?: unknown }).MediaStream = FakeMediaStream

/** A track that can go quiet, come back and end, as a remote track does. */
class FakeTrack {
    readyState: MediaStreamTrackState = "live"
    muted = false
    private listeners = new Map<string, Set<() => void>>()

    constructor(readonly kind: "audio" | "video") {}

    addEventListener(name: string, cb: () => void) {
        if (!this.listeners.has(name)) this.listeners.set(name, new Set())
        this.listeners.get(name)!.add(cb)
    }

    removeEventListener(name: string, cb: () => void) {
        this.listeners.get(name)?.delete(cb)
    }

    listening(name: string) {
        return (this.listeners.get(name)?.size ?? 0) > 0
    }

    /** Change the state the event describes, then fire it. */
    fire(name: "mute" | "unmute" | "ended") {
        if (name === "mute") this.muted = true
        if (name === "unmute") this.muted = false
        if (name === "ended") this.readyState = "ended"
        for (const cb of [...(this.listeners.get(name) ?? [])]) cb()
    }
}

const track = (kind: "audio" | "video") => new FakeTrack(kind) as unknown as MediaStreamTrack
const stream = (...tracks: MediaStreamTrack[]) => new FakeMediaStream(tracks) as unknown as MediaStream

import {
    MeshManager,
    parseCallMessage,
    createTelepresenceChannel,
    CALL_PROTOCOL_VERSION,
    DEFAULT_ICE_SERVERS,
    DISCONNECT_GRACE_MS,
    HANDSHAKE_GRACE_MS,
    MAX_RECOVERY_ATTEMPTS,
    RECOVERY_INTERVAL_MS,
    REPAIR_SPACING_MS,
    type CallMessage,
    type MeshParticipant,
    type RecoveryRung,
    type SignallingChannel,
    type TelepresenceProxy,
} from "./MeshManager"
import type { PerspectiveExpression } from "../perspectives/Perspective"

// ── A fake RTCPeerConnection ────────────────────────────────────────

let nextConnection = 0

class FakePeerConnection {
    /** Which connection this is, written into every description it makes — see `setRemoteDescription`. */
    readonly id = `pc${(nextConnection += 1)}`
    /** The connection the remote descriptions applied so far came from. */
    private remoteOrigin: string | null = null

    signalingState: RTCSignalingState = "stable"
    connectionState: RTCPeerConnectionState = "new"
    localDescription: RTCSessionDescriptionInit | null = null
    remoteDescription: RTCSessionDescriptionInit | null = null

    onnegotiationneeded: (() => void) | null = null
    onicecandidate: ((e: { candidate: { toJSON(): RTCIceCandidateInit } | null }) => void) | null = null
    ontrack: ((e: { track: unknown }) => void) | null = null
    onconnectionstatechange: (() => void) | null = null

    senders: { track: MediaStreamTrack | null; replaceTrack(t: MediaStreamTrack | null): Promise<void> }[] = []
    addedTracks: unknown[] = []
    /** The m-sections this connection has negotiated. */
    transceivers: { kind: string; direction: string }[] = []
    candidates: RTCIceCandidateInit[] = []
    closed = false

    /**
     * Fire `negotiationneeded` once per task, as a browser does: declaring an
     * audio and a video section together produces one negotiation.
     */
    private negotiationNeeded = false
    private negotiationQueued = false
    private queueNegotiation() {
        this.negotiationNeeded = true
        if (this.negotiationQueued) return
        this.negotiationQueued = true
        queueMicrotask(() => {
            this.negotiationQueued = false
            // Cleared by `setLocalDescription`: a peer that has just answered
            // an offer describing these sections does not then offer them again.
            if (!this.negotiationNeeded) return
            // And never mid-handshake: the spec aborts the event unless stable.
            if (this.signalingState !== "stable") return
            this.onnegotiationneeded?.()
        })
    }

    private makeSender(track: MediaStreamTrack | null) {
        const sender = {
            track,
            async replaceTrack(next: MediaStreamTrack | null) {
                sender.track = next
            },
        }
        this.senders.push(sender)
        return sender
    }

    addTransceiver(kind: string, init?: { direction?: string; streams?: MediaStream[] }) {
        this.transceivers.push({ kind, direction: init?.direction ?? "sendrecv" })
        this.queueNegotiation()
        return { sender: this.makeSender(null) }
    }

    addTrack(track: MediaStreamTrack, _stream?: MediaStream) {
        this.addedTracks.push(track)
        this.queueNegotiation()
        return this.makeSender(track)
    }

    getSenders() {
        return this.senders
    }

    private offersMade = 0
    private remoteOfferSeq = 0

    /**
     * An offer is `offer-sdp@pc1#2` (pc1's second offer); an answer names the
     * offer it answers: `answer-sdp@pc2>pc1#2`.
     */
    private describe(type: "offer" | "answer") {
        return type === "offer"
            ? `offer-sdp@${this.id}#${(this.offersMade += 1)}`
            : `answer-sdp@${this.id}>${this.remoteDescription?.sdp?.split("@")[1] ?? ""}`
    }

    async createOffer(): Promise<RTCSessionDescriptionInit> {
        return { type: "offer", sdp: this.describe("offer") }
    }

    async createAnswer(): Promise<RTCSessionDescriptionInit> {
        if (this.signalingState !== "have-remote-offer") {
            throw new Error(`InvalidStateError: cannot create an answer in state ${this.signalingState}`)
        }
        return { type: "answer", sdp: this.describe("answer") }
    }

    async setLocalDescription(description?: RTCSessionDescriptionInit) {
        // As Chromium does: a description passed in is applied as written, so
        // a type with no `sdp` is an empty description. `@roamhq/wrtc` accepts
        // it, which is how the end-to-end scenarios missed it.
        if (description && !description.sdp) {
            throw new Error(
                "OperationError: Failed to execute 'setLocalDescription' on 'RTCPeerConnection': Failed to parse SessionDescription.  Expect line: v=",
            )
        }
        this.negotiationNeeded = false
        // With no argument the browser picks offer or answer from the signaling state.
        const type = description?.type ?? (this.signalingState === "have-remote-offer" ? "answer" : "offer")
        // As a browser does: an offer only from stable, an answer only to an offer.
        if ((type === "offer") !== (this.signalingState !== "have-remote-offer")) {
            throw new Error(`InvalidStateError: cannot set a local ${type} in state ${this.signalingState}`)
        }
        const sdp = description?.sdp ?? this.describe(type as "offer" | "answer")
        this.localDescription = { type: type as RTCSdpType, sdp }
        this.signalingState = type === "offer" ? "have-local-offer" : "stable"
    }

    /**
     * Refuses a description from a different connection than the one it has
     * been talking to — the browser's "The order of m-lines in subsequent
     * offer doesn't match order from previous offer/answer" — and an answer
     * to another offer than the one out — "The order of m-lines in answer
     * doesn't match order in offer". Modelled by origin rather than by
     * m-lines; a hand-written description with no origin is accepted.
     */
    async setRemoteDescription(description: RTCSessionDescriptionInit) {
        const [, tail] = description.sdp?.split("@") ?? []
        const origin = tail?.split(/[#>]/)[0] ?? null
        if (origin && this.remoteOrigin && origin !== this.remoteOrigin) {
            throw new Error(
                "InvalidAccessError: Failed to set remote offer sdp: The order of m-lines in subsequent offer doesn't match order from previous offer/answer.",
            )
        }
        // An offer older than one applied describes m-lines the connection has moved past.
        const offerSeq = Number(tail?.split(">")[0].split("#")[1])
        if (description.type === "offer" && offerSeq < this.remoteOfferSeq) {
            throw new Error(
                "InvalidAccessError: Failed to set remote offer sdp: The order of m-lines in subsequent offer doesn't match order from previous offer/answer.",
            )
        }
        if (description.type === "offer" && offerSeq) this.remoteOfferSeq = offerSeq
        const answers = tail?.split(">")[1]
        if (description.type === "answer" && answers && answers !== this.localDescription?.sdp?.split("@")[1]) {
            throw new Error(
                "InvalidAccessError: Failed to set remote answer sdp: The order of m-lines in answer doesn't match order in offer. Rejecting answer.",
            )
        }
        if (origin) this.remoteOrigin = origin
        this.remoteDescription = description
        this.signalingState = description.type === "offer" ? "have-remote-offer" : "stable"
    }

    async addIceCandidate(candidate: RTCIceCandidateInit) {
        if (this.signalingState === "stable" && !this.remoteDescription) {
            throw new Error("no remote description")
        }
        this.candidates.push(candidate)
    }

    /** How many ICE restarts this connection was asked for — the cheapest rung of the ladder. */
    restartIceCalls = 0
    restartIce() {
        this.restartIceCalls += 1
        // A real restart re-gathers and renegotiates.
        this.queueNegotiation()
    }

    /** What `transportOf` reads. Empty unless a test says otherwise. */
    stats: Record<string, unknown>[] = []
    async getStats() {
        return new Map(this.stats.map((report) => [report.id as string, report]))
    }

    close() {
        this.closed = true
    }

    /** Test helper: pretend ICE produced a candidate. */
    emitCandidate(candidate: RTCIceCandidateInit) {
        this.onicecandidate?.({ candidate: { toJSON: () => candidate } })
    }

    /** Test helper: drive the connection state the recovery ladder reads. */
    setConnectionState(state: RTCPeerConnectionState) {
        this.connectionState = state
        this.onconnectionstatechange?.()
    }
}

// ── In-memory signalling ────────────────────────────────────────────

type Listener = (from: string, payload: unknown) => void
type PublishResult = { ok: boolean; ms: number }

interface ChannelOptions {
    /**
     * Swallow an outgoing message instead of delivering it — a lossy
     * transport, which is what telepresence is. Read on every publish, so a
     * test can lose the opening handshake and then let the repair through.
     * A dropped message is still reported `ok`: the executor accepted it and
     * no peer received it, which is the failure being modelled.
     */
    drop?: (payload: unknown) => boolean
    /** Hold publish results until `results.release()`, so sends stay outstanding. */
    holdResults?: boolean
}

/**
 * Every subscriber hears every publish but its own, and the sender id comes
 * from the transport, never from the payload. Delivery is synchronous; a
 * test that wants a message late or out of order drops it and hands it to
 * {@link InMemoryBus.deliver} itself.
 */
class InMemoryBus {
    private subscribers = new Map<string, Set<Listener>>()

    channel(selfId: string, options: ChannelOptions = {}) {
        const watchers = new Set<(result: PublishResult) => void>()
        const sent: unknown[] = []
        const report = (result: PublishResult) => {
            for (const cb of [...watchers]) cb(result)
        }
        const results = {
            holding: !!options.holdResults,
            held: 0,
            /** Report every held publish, oldest first. */
            release(result: PublishResult = { ok: true, ms: 18_000 }) {
                const n = results.held
                results.held = 0
                for (let i = 0; i < n; i += 1) report(result)
            },
        }

        const channel: SignallingChannel = {
            publish: (payload) => {
                sent.push(payload)
                if (!options.drop?.(payload)) this.deliver(selfId, payload)
                if (results.holding) results.held += 1
                else report({ ok: true, ms: 0 })
            },
            onMessage: (cb) => {
                if (!this.subscribers.has(selfId)) this.subscribers.set(selfId, new Set())
                this.subscribers.get(selfId)!.add(cb)
                return () => { this.subscribers.get(selfId)?.delete(cb) }
            },
            onPublishResult: (cb) => {
                watchers.add(cb)
                return () => { watchers.delete(cb) }
            },
        }

        return { channel, sent, results }
    }

    /** Hand a message to one subscriber, whoever it claims to be from — even itself. */
    deliverTo(to: string, from: string, payload: unknown) {
        for (const cb of [...(this.subscribers.get(to) ?? [])]) cb(from, payload)
    }

    /** Deliver as though `from` had published it: to every subscriber but `from`. */
    deliver(from: string, payload: unknown) {
        for (const [id, listeners] of this.subscribers) {
            if (id === from) continue
            for (const cb of [...listeners]) cb(from, payload)
        }
    }
}

// ── Helpers ─────────────────────────────────────────────────────────

/**
 * A clock the tests move by hand. The recovery ladder is defined in seconds;
 * `advance` moves the clock and `mesh.tick()` is the sweep.
 */
function makeClock() {
    let at = 1_000
    return { now: () => at, advance: (ms: number) => (at += ms) }
}

interface MeshOptions extends ChannelOptions {
    iceServers?: RTCIceServer[]
    now?: () => number
    /** What `join` is handed. An empty stream unless a test says otherwise. */
    localStream?: MediaStream
}

/** Every mesh a test made, so each is left afterwards and no timer or listener outlives its test. */
const meshes: MeshManager[] = []

async function makeMesh(bus: InMemoryBus, selfId: string, callId: string, options: MeshOptions = {}) {
    const { channel, sent, results } = bus.channel(selfId, options)

    const connections: FakePeerConnection[] = []
    const configs: RTCConfiguration[] = []
    const errors: Error[] = []
    const recoveries: { peerId: string; rung: RecoveryRung; attempts: number }[] = []
    const joins: string[] = []
    const leaves: string[] = []
    const updates: MeshParticipant[] = []
    const states: [string, RTCPeerConnectionState][] = []

    const mesh = new MeshManager({
        channel,
        callId,
        selfId,
        iceServers: options.iceServers,
        now: options.now,
        // No timer: every test drives `tick()` itself, so a sweep never fires
        // between an act and its assertion. The interval has its own test.
        sweepMs: 0,
        createPeerConnection: (configuration) => {
            configs.push(configuration)
            const pc = new FakePeerConnection()
            connections.push(pc)
            return pc as unknown as RTCPeerConnection
        },
    })
    meshes.push(mesh)

    mesh.on("error", (error: Error) => errors.push(error))
    mesh.on("peer-recovery", (peerId: string, attempt: { rung: RecoveryRung; attempts: number }) =>
        recoveries.push({ peerId, ...attempt }))
    mesh.on("participant-joined", (p: MeshParticipant) => joins.push(p.did))
    mesh.on("participant-left", (p: MeshParticipant) => leaves.push(p.did))
    mesh.on("participant-updated", (p: MeshParticipant) => updates.push(p))
    mesh.on("peer-state", (peerId: string, state: RTCPeerConnectionState) => states.push([peerId, state]))

    await mesh.join(options.localStream ?? stream())

    /** The connection currently serving a pair — the last one built, after any rebuilds. */
    const live = () => connections[connections.length - 1]

    return { mesh, connections, configs, errors, recoveries, joins, leaves, updates, states, sent, results, live }
}

/** Every message kind the mesh published, in order. The warm-up message has none. */
const kinds = (sent: unknown[]) => sent.map((payload) => (payload as { kind?: string }).kind)

/** Let queued microtasks and promise chains settle. */
const settle = () => new Promise((resolve) => setTimeout(resolve, 0))

const settleAll = async () => {
    for (let n = 0; n < 6; n += 1) await settle()
}

afterEach(async () => {
    for (const mesh of meshes.splice(0)) await mesh.destroy()
})

const CALL_ID = "call:rec-abc"

// ── Protocol ────────────────────────────────────────────────────────

describe("parseCallMessage", () => {
    const good: CallMessage = {
        v: CALL_PROTOCOL_VERSION,
        call: "room:x",
        kind: "description",
        description: { type: "offer", sdp: "v=0" },
    }

    it("accepts a well-formed description", () => {
        expect(parseCallMessage(good)).toEqual(good)
    })

    it("accepts an offer with no sdp", () => {
        const noSdp = { ...good, description: { type: "offer" } }
        expect(parseCallMessage(noSdp)).toEqual({
            ...good,
            description: { type: "offer", sdp: undefined },
        })
    })

    it("accepts an answer", () => {
        const answer = { ...good, description: { type: "answer", sdp: "a=0" } }
        expect(parseCallMessage(answer)).toEqual(answer)
    })

    it("accepts an ICE candidate", () => {
        const ice = {
            v: CALL_PROTOCOL_VERSION,
            call: "room:x",
            kind: "ice" as const,
            candidate: { candidate: "a=candidate:1 ..." },
        }
        expect(parseCallMessage(ice)).toEqual(ice)
    })

    it("accepts a data message", () => {
        const data = {
            v: CALL_PROTOCOL_VERSION,
            call: "room:x",
            kind: "data" as const,
            label: "chat",
            data: "hello",
            binary: false,
        }
        expect(parseCallMessage(data)).toEqual(data)
    })

    it("coerces missing binary flag to false", () => {
        const raw = {
            v: CALL_PROTOCOL_VERSION,
            call: "room:x",
            kind: "data",
            label: "chat",
            data: "hello",
        }
        const parsed = parseCallMessage(raw)
        expect(parsed).not.toBeNull()
        expect((parsed as { binary: boolean }).binary).toBe(false)
    })

    it.each([
        ["mismatched version", { ...good, v: 99 }],
        ["missing call id", { ...good, call: undefined }],
        ["empty call id", { ...good, call: "" }],
        ["unknown kind", { ...good, kind: "chat" }],
        ["non-object description", { ...good, description: "offer" }],
        ["invalid sdp type", { ...good, description: { type: "nonsense", sdp: "x" } }],
        ["null payload", null],
        ["string payload", "offer"],
        ["number payload", 42],
        ["null description", { ...good, description: null }],
        ["non-string data label", { v: CALL_PROTOCOL_VERSION, call: "x", kind: "data", label: 123, data: "y" }],
        ["non-string data payload", { v: CALL_PROTOCOL_VERSION, call: "x", kind: "data", label: "x", data: 123 }],
        ["null ice candidate", { v: CALL_PROTOCOL_VERSION, call: "x", kind: "ice", candidate: null }],
    ])("rejects %s", (_label, payload) => {
        expect(parseCallMessage(payload)).toBeNull()
    })

    it("carries which connections a message is between, and only as strings", () => {
        expect(parseCallMessage({ ...good, gen: "g1", peerGen: "g2" })).toEqual({ ...good, gen: "g1", peerGen: "g2" })
        // A malformed generation is dropped rather than failing the message:
        // without one, it is read the way a peer predating generations is read.
        expect(parseCallMessage({ ...good, gen: 7, peerGen: { x: 1 } })).toEqual(good)
        expect(parseCallMessage({ v: CALL_PROTOCOL_VERSION, call: "room:x", kind: "reset", gen: "g1" })).toEqual({
            v: CALL_PROTOCOL_VERSION,
            call: "room:x",
            kind: "reset",
            gen: "g1",
        })
    })

    it("stays at protocol version 1, the version WE speaks", () => {
        // A bump would make SDK and WE peers in one call drop every message
        // from each other. New kinds and fields go in without one.
        expect(CALL_PROTOCOL_VERSION).toBe(1)
    })

    it("reads WE's current wire format, generations and reset included", () => {
        // Exactly what WE's mesh publishes: `to` in the payload, both
        // generations, and the `reset` kind. `to` is addressing and is not
        // part of the parsed message.
        const call = "space:rec-abc"
        const offer = {
            v: 1, call, to: "did:alice", kind: "description",
            description: { type: "offer", sdp: "v=0\r\n" }, gen: "k3j2a9x1", peerGen: "p0q9r8s7",
        }
        const candidate = {
            v: 1, call, to: "did:alice", kind: "ice",
            candidate: { candidate: "candidate:1 1 udp 2122260223 10.0.0.2 5000 typ host", sdpMid: "0", sdpMLineIndex: 0 },
            gen: "k3j2a9x1",
        }
        const reset = { v: 1, call, to: "did:alice", kind: "reset", gen: "k3j2a9x1", peerGen: "p0q9r8s7" }
        const warm = { v: 1, call, warm: true }

        expect(parseCallMessage(offer)).toEqual({
            v: 1, call, kind: "description",
            description: { type: "offer", sdp: "v=0\r\n" }, gen: "k3j2a9x1", peerGen: "p0q9r8s7",
        })
        expect(parseCallMessage(candidate)).toEqual({
            v: 1, call, kind: "ice", candidate: candidate.candidate, gen: "k3j2a9x1",
        })
        expect(parseCallMessage(reset)).toEqual({ v: 1, call, kind: "reset", gen: "k3j2a9x1", peerGen: "p0q9r8s7" })
        // The warm-up message has no kind and parses to nothing, on both sides.
        expect(parseCallMessage(warm)).toBeNull()
    })
})

// ── Negotiation and roster ──────────────────────────────────────────

describe("call mesh", () => {
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    it("connects two peers, with exactly one surviving offer", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        // Presence says both are in the call. Neither sent a join message.
        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        expect(alice.connections).toHaveLength(1)
        expect(bob.connections).toHaveLength(1)

        // Negotiation begins with the connection, not with the first track.
        await settle()
        await settle()

        expect(alice.connections[0].signalingState).toBe("stable")
        expect(bob.connections[0].signalingState).toBe("stable")
        const answered = [alice, bob].filter((peer) => peer.connections[0].localDescription?.type === "answer")
        expect(answered).toHaveLength(1)

        // And a track needs no further handshake — it attaches to a section that already exists.
        await alice.mesh.replaceTrack("audio", track("audio"))
        await settle()

        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.errors).toEqual([])
        expect(bob.errors).toEqual([])
    })

    it("publishes one warm-up message on join, which no peer acts on", async () => {
        // The first broadcast on a fresh neighbourhood is the slow one; it
        // should not be the first SDP offer.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        expect(alice.sent).toEqual([{ v: CALL_PROTOCOL_VERSION, call: CALL_ID, warm: true }])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        expect(bob.errors).toEqual([])
        expect(bob.connections[0].remoteDescription).toBeNull()
    })

    it("negotiates a video section even when this agent has no camera", async () => {
        // A connection carries only the kinds it negotiated a section for, so
        // an agent sending no video left the other agent's camera nowhere to land.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        await alice.mesh.replaceTrack("audio", track("audio"))
        await settle()

        expect(alice.connections[0].transceivers.map((t) => t.kind).sort()).toEqual(["audio", "video"])
        expect(alice.connections[0].transceivers.every((t) => t.direction === "sendrecv")).toBe(true)
    })

    it("survives glare: both peers offering at once still converges", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        await Promise.all([
            alice.mesh.replaceTrack("audio", track("audio")),
            bob.mesh.replaceTrack("audio", track("audio")),
        ])
        await settle()
        await settle()
        await settle()

        expect(alice.connections[0].signalingState).toBe("stable")
        expect(bob.connections[0].signalingState).toBe("stable")
        expect(alice.errors).toEqual([])
        expect(bob.errors).toEqual([])
    })

    it("determines the polite side by DID comparison (higher DID = polite)", async () => {
        // 'did:bob' > 'did:alice', so Bob is polite and yields; Alice's offer wins.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settleAll()

        expect(bob.connections[0].localDescription?.type).toBe("answer")
        expect(alice.connections[0].localDescription?.type).toBe("offer")
    })

    it("tears down a connection when presence drops the peer from the roster", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        await makeMesh(bus, "did:bob", CALL_ID)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        expect(alice.connections[0].closed).toBe(false)

        // Bob's laptop closed. Presence evicts him; no leave message is ever sent.
        alice.mesh.setRoster(["did:alice"])
        expect(alice.connections[0].closed).toBe(true)
        expect(alice.mesh.getParticipants()).toEqual([])
        expect(alice.mesh.peerStates().size).toBe(0)
    })

    it("emits participant-joined from setRoster and participant-left when the roster drops the peer", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(alice.joins).toEqual(["did:bob"])

        // Listing an existing peer again is not a second join.
        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(alice.joins).toEqual(["did:bob"])

        alice.mesh.setRoster(["did:alice"])
        expect(alice.leaves).toEqual(["did:bob"])
    })

    it("filters self from the roster", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)

        alice.mesh.setRoster(["did:alice"])
        expect(alice.connections).toHaveLength(0)
        expect(alice.joins).toEqual([])
    })

    it("returns participants from getParticipants()", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)

        expect(alice.mesh.getParticipants()).toEqual([])

        alice.mesh.setRoster(["did:alice", "did:bob", "did:carol"])
        const participants = alice.mesh.getParticipants()
        expect(participants).toHaveLength(2)
        expect(participants.map((p) => p.did).sort()).toEqual(["did:bob", "did:carol"])
        expect(participants.every((p) => !p.hasAudio && !p.hasVideo)).toBe(true)
    })

    it("ignores signalling from an agent the roster does not include", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice"])

        // A stranger offers out of the blue. Without the roster check this would open a connection.
        bus.deliver("did:mallory", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            to: "did:alice",
            kind: "description",
            description: { type: "offer", sdp: "x" },
        })
        await settle()

        expect(alice.connections).toHaveLength(0)
        expect(alice.errors).toEqual([])
    })

    it("ignores traffic for a different call in the same space", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const before = alice.connections[0].remoteDescription
        bus.deliver("did:bob", {
            v: CALL_PROTOCOL_VERSION,
            call: "call:rec-other",
            to: "did:alice",
            kind: "description",
            description: { type: "offer", sdp: "wrong-call" },
        })
        await settle()

        expect(alice.connections[0].remoteDescription).toBe(before)
    })

    it("ignores signalling addressed to another agent", async () => {
        // A fanout-only transport delivers everything to everyone; the `to`
        // in the payload stops a bystander negotiating.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        bus.deliver("did:bob", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            to: "did:carol",
            kind: "description",
            description: { type: "offer", sdp: "for-carol" },
        })
        await settle()

        expect(alice.connections[0].remoteDescription).toBeNull()
        expect(alice.errors).toEqual([])
    })

    it("drops its own echo", async () => {
        // A transport that echoes a publish back to its sender must not make
        // the mesh negotiate with itself.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const echo = alice.sent.find((payload) => (payload as { kind?: string }).kind === "description")
        expect(echo).toBeDefined()
        // Handed back to Alice as from herself, bypassing the bus's own self filter.
        // Readdressed to Alice, so only the sender check stands between it and her.
        bus.deliverTo("did:alice", "did:alice", { ...(echo as object), to: "did:alice" })

        await settle()
        expect(alice.connections).toHaveLength(1)
        expect(alice.connections[0].remoteDescription).toBeNull()
        expect(alice.errors).toEqual([])
    })

    it("re-emits when an inbound track stops receiving, and again when it comes back", async () => {
        // A remote track does not END when its sender goes away — it goes
        // `muted`. Only `ended` was wired up, so a peer's tile went on
        // painting its last frame. Both edges, so a recovered connection's
        // tile comes back without waiting for some unrelated event.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const remote = new FakeTrack("video")
        const added: MediaStreamTrack[] = []
        alice.mesh.on("stream-added", (_stream: MediaStream, t: MediaStreamTrack) => added.push(t))

        alice.connections[0].ontrack?.({ track: remote })
        expect(added).toEqual([remote])
        expect(remote.listening("mute")).toBe(true)
        expect(remote.listening("unmute")).toBe(true)
        expect(alice.mesh.getParticipants()[0].hasVideo).toBe(true)
        const afterArrival = alice.updates.length

        remote.fire("mute")
        expect(alice.updates).toHaveLength(afterArrival + 1)
        expect(alice.updates[alice.updates.length - 1].hasVideo).toBe(false)

        remote.fire("unmute")
        expect(alice.updates).toHaveLength(afterArrival + 2)
        expect(alice.updates[alice.updates.length - 1].hasVideo).toBe(true)

        // The track stays in the stream through both: muting is not removal.
        expect(alice.mesh.getParticipants()[0].stream.getVideoTracks()).toEqual([remote])
    })

    it("removes an inbound track that ends, and says so", async () => {
        // Camera → screen arrives as a new track and the old one ends.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const camera = new FakeTrack("video")
        const removed: unknown[] = []
        alice.mesh.on("stream-removed", (_stream: MediaStream, t: unknown) => removed.push(t))
        alice.connections[0].ontrack?.({ track: camera })

        camera.fire("ended")
        expect(removed).toEqual([camera])
        expect(alice.mesh.getParticipants()[0].stream.getVideoTracks()).toEqual([])
        expect(alice.updates[alice.updates.length - 1].hasVideo).toBe(false)
    })

    it("does not report a muted track for a connection that has been replaced", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const remote = new FakeTrack("video")
        alice.connections[0].ontrack?.({ track: remote })
        alice.mesh.reconnect("did:bob")
        const before = alice.updates.length

        remote.fire("mute")
        expect(alice.updates).toHaveLength(before)
    })

    it("does not report an ended track for a connection that has been replaced", async () => {
        // Closing a peer connection ends its remote tracks; the rebuilt pair's
        // listeners must not hear about a stream nobody holds any more.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const remote = new FakeTrack("video")
        const removed: unknown[] = []
        alice.mesh.on("stream-removed", (_stream: MediaStream, t: unknown) => removed.push(t))
        alice.connections[0].ontrack?.({ track: remote })
        alice.mesh.reconnect("did:bob")
        const before = alice.updates.length

        remote.fire("ended")
        expect(removed).toEqual([])
        expect(alice.updates).toHaveLength(before)
    })

    it("replaces rather than re-adds when an outbound track changes", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const camera = track("video")
        const screen = track("video")

        await alice.mesh.replaceTrack("video", camera)
        await alice.mesh.replaceTrack("video", screen)

        // One sender, swapped, and no topology change at all.
        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.connections[0].getSenders().filter((s) => s.track)).toHaveLength(1)
        expect(alice.connections[0].getSenders().find((s) => s.track)?.track).toBe(screen)
    })

    it("reuses the sender after the outbound track is cleared, rather than adding a second one", async () => {
        // The frozen-screen bug: a sender whose track is null cannot be found
        // by kind, so sharing again added a second video track and the peer's
        // view stayed on the dead first one.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const screen = track("video")
        const screenAgain = track("video")

        await alice.mesh.replaceTrack("video", screen)
        await alice.mesh.replaceTrack("video", null)
        await alice.mesh.replaceTrack("video", screenAgain)

        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.connections[0].getSenders().filter((s) => s.track)).toHaveLength(1)
        expect(alice.connections[0].getSenders().find((s) => s.track)?.track).toBe(screenAgain)
    })

    it("stops sending when the outbound track is cleared", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        await alice.mesh.replaceTrack("video", track("video"))
        await alice.mesh.replaceTrack("video", null)

        expect(alice.connections[0].getSenders().every((sender) => sender.track === null)).toBe(true)
    })

    it("sends a peer joining mid-call the media already being sent", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice"])
        const mic = track("audio")
        await alice.mesh.replaceTrack("audio", mic)

        // Bob arrives after Alice's mic was already on.
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        // Attached to the section created with the connection, rather than added as a new one.
        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.connections[0].getSenders().find((sender) => sender.track)?.track).toBe(mic)
    })

    it("attaches the tracks handed to join, one per kind", async () => {
        const mic = track("audio")
        const camera = track("video")
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { localStream: stream(mic, camera) })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.connections[0].getSenders().map((sender) => sender.track)).toEqual([mic, camera])
    })

    it("stops negotiating once left", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        await alice.mesh.leave()
        expect(alice.connections[0].closed).toBe(true)

        bus.deliver("did:bob", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            to: "did:alice",
            kind: "description",
            description: { type: "offer", sdp: "late" },
        })
        await settle()

        expect(alice.connections).toHaveLength(1)
        expect(alice.errors).toEqual([])
    })

    it("ignores setRoster after leave()", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.leave()

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(alice.connections).toHaveLength(0)
    })

    it("destroy() clears all state", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        let eventFired = false
        alice.mesh.on("error", () => { eventFired = true })

        await alice.mesh.destroy()
        expect(alice.connections[0].closed).toBe(true)
        expect(alice.mesh.getParticipants()).toEqual([])

        bus.deliver("did:bob", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            kind: "description",
            description: { type: "offer", sdp: "late" },
        })
        await settle()
        expect(eventFired).toBe(false)
    })

    it("forwards ICE candidates between peers", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settleAll()

        alice.connections[0].emitCandidate({ candidate: "candidate:1" })
        await settle()

        expect(bob.connections[0].candidates).toEqual([{ candidate: "candidate:1" }])
        expect(bob.errors).toEqual([])
    })

    it("handles three peers with correct connection count", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)
        const carol = await makeMesh(bus, "did:carol", CALL_ID)

        const roster = ["did:alice", "did:bob", "did:carol"]
        alice.mesh.setRoster(roster)
        bob.mesh.setRoster(roster)
        carol.mesh.setRoster(roster)
        await settleAll()

        // Each peer connects to the two others, and every pair settles.
        for (const peer of [alice, bob, carol]) {
            expect(peer.connections).toHaveLength(2)
            expect(peer.connections.every((pc) => pc.signalingState === "stable")).toBe(true)
            expect(peer.errors).toEqual([])
        }
        expect(alice.mesh.getParticipants()).toHaveLength(2)
    })

    it("reports each pair's connection state", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        alice.live().setConnectionState("connecting")
        alice.live().setConnectionState("connected")

        expect(alice.states).toEqual([["did:bob", "connecting"], ["did:bob", "connected"]])
        expect(alice.mesh.peerStates().get("did:bob")).toBe("connected")
    })
})

// ── Events ──────────────────────────────────────────────────────────

describe("MeshManager events", () => {
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    it("on() registers and off() removes callbacks", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)

        let calls = 0
        const cb = () => calls++
        alice.mesh.on("participant-joined", cb)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(calls).toBe(1)

        alice.mesh.off("participant-joined", cb)
        alice.mesh.setRoster(["did:alice", "did:bob", "did:carol"])
        expect(calls).toBe(1)
    })

    it("off() with no callback removes all listeners for that event", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.off("participant-joined")

        let calls = 0
        alice.mesh.on("participant-joined", () => calls++)
        alice.mesh.on("participant-joined", () => calls++)

        alice.mesh.off("participant-joined")
        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(calls).toBe(0)
    })

    it("event handler errors do not break other handlers", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)

        let secondCalled = false
        const consoleError = jest.spyOn(console, "error").mockImplementation()

        alice.mesh.on("participant-joined", () => { throw new Error("boom") })
        alice.mesh.on("participant-joined", () => { secondCalled = true })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(secondCalled).toBe(true)
        expect(consoleError).toHaveBeenCalled()

        consoleError.mockRestore()
    })
})

// ── Data ────────────────────────────────────────────────────────────

describe("data over signalling", () => {
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    async function pair(callId = CALL_ID) {
        const alice = await makeMesh(bus, "did:alice", callId)
        const bob = await makeMesh(bus, "did:bob", callId)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        return { alice, bob }
    }

    it("sends data via the signalling channel", async () => {
        const { alice, bob } = await pair()

        const received: { label: string; data: string; binary: boolean; sender: string }[] = []
        bob.mesh.subscribeDataChannel((msg) => {
            received.push({ label: msg.channelLabel, data: msg.data, binary: msg.binary, sender: msg.senderDid })
        })

        alice.mesh.sendData("chat", "hello world", false)
        await settle()

        expect(received).toEqual([{ label: "chat", data: "hello world", binary: false, sender: "did:alice" }])
        // Data goes to everyone, not to one peer.
        expect(alice.sent[alice.sent.length - 1]).toMatchObject({ kind: "data", to: "*" })
    })

    it("sends binary data with binary flag", async () => {
        const { alice, bob } = await pair()

        const received: { binary: boolean }[] = []
        bob.mesh.subscribeDataChannel((msg) => received.push({ binary: msg.binary }))

        alice.mesh.sendData("file", "base64payload", true)
        await settle()

        expect(received).toEqual([{ binary: true }])
    })

    it("unsubscribes data listener on returned function call", async () => {
        const { alice, bob } = await pair()

        let count = 0
        const unsub = bob.mesh.subscribeDataChannel(() => count++)

        alice.mesh.sendData("x", "1")
        await settle()
        expect(count).toBe(1)

        unsub()
        alice.mesh.sendData("x", "2")
        await settle()
        expect(count).toBe(1)
    })

    it("populates roomName on data messages from callId", async () => {
        const { alice, bob } = await pair("my-room")

        let roomName = ""
        bob.mesh.subscribeDataChannel((msg) => { roomName = msg.roomName })

        alice.mesh.sendData("x", "y")
        await settle()

        expect(roomName).toBe("my-room")
    })

    it("drops data from an agent the roster does not list, rather than holding it", async () => {
        // Data is not signalling: there is nothing to replay it into later,
        // and holding it would buffer strangers' traffic.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)
        const received: string[] = []
        bob.mesh.subscribeDataChannel((msg) => received.push(msg.data))

        alice.mesh.sendData("chat", "before the roster")
        await settle()
        expect(received).toEqual([])

        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        expect(received).toEqual([])

        alice.mesh.sendData("chat", "after")
        await settle()
        expect(received).toEqual(["after"])
    })
})

// ── An unordered transport ──────────────────────────────────────────

describe("an unordered transport", () => {
    /**
     * Telepresence dispatches sends concurrently and confirms only that the
     * executor took a message, so a candidate overtaking the description it
     * belongs to is the ordinary case, not a race.
     */
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    const offerFrom = (from: string) => ({
        v: CALL_PROTOCOL_VERSION,
        call: CALL_ID,
        to: "did:alice",
        kind: "description" as const,
        description: { type: "offer" as const, sdp: `${from}-offer` },
    })

    const candidateFrom = (candidate: string) => ({
        v: CALL_PROTOCOL_VERSION,
        call: CALL_ID,
        to: "did:alice",
        kind: "ice" as const,
        candidate: { candidate },
    })

    it("holds candidates that arrive before the description, and applies them after", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:aaron"])
        await settle()

        bus.deliver("did:aaron", candidateFrom("a"))
        bus.deliver("did:aaron", candidateFrom("b"))
        bus.deliver("did:aaron", candidateFrom("c"))
        await settle()

        expect(alice.live().candidates).toEqual([])
        expect(alice.errors).toEqual([])

        bus.deliver("did:aaron", offerFrom("did:aaron"))
        await settle()

        // All three, in the order they were sent.
        expect(alice.live().candidates.map((c) => c.candidate)).toEqual(["a", "b", "c"])
        expect(alice.errors).toEqual([])
    })

    it("does not hold candidates without bound, for a peer whose description never comes", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:aaron"])
        await settle()

        for (let n = 0; n < 500; n += 1) bus.deliver("did:aaron", candidateFrom(`c${n}`))
        await settle()
        bus.deliver("did:aaron", offerFrom("did:aaron"))
        await settle()

        // Bounded, and it kept the earliest — host candidates come first.
        expect(alice.live().candidates.length).toBeLessThanOrEqual(64)
        expect(alice.live().candidates[0]?.candidate).toBe("c0")
    })

    it("applies signalling one message at a time", async () => {
        // Two descriptions in one tick used to run concurrently and read the
        // collision state at a moment that was already stale.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:aaron"])
        await settle()

        bus.deliver("did:aaron", offerFrom("first"))
        bus.deliver("did:aaron", {
            ...offerFrom("second"),
            description: { type: "offer" as const, sdp: "second-offer" },
        })
        await settle()
        await settle()

        expect(alice.live().remoteDescription?.sdp).toBe("second-offer")
        expect(alice.errors).toEqual([])
    })
})

// ── Recovery ────────────────────────────────────────────────────────

describe("recovering a pair that did not connect", () => {
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    it("resends a description that was lost, rather than rebuilding the connection", async () => {
        // Alice is impolite, so on a collision she ignores Bob's offer and
        // expects her own to win — and hers is the one the transport drops.
        const clock = makeClock()
        let losing = true
        const alice = await makeMesh(bus, "did:alice", CALL_ID, {
            now: clock.now,
            drop: (payload) => losing && (payload as { kind?: string }).kind === "description",
        })
        const bob = await makeMesh(bus, "did:bob", CALL_ID, { now: clock.now })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        expect(alice.live().signalingState).toBe("have-local-offer")
        expect(alice.live().remoteDescription).toBeNull()

        losing = false
        clock.advance(HANDSHAKE_GRACE_MS + 1)
        alice.mesh.tick()
        await settle()
        await settle()
        await settle()

        expect(alice.recoveries.map((r) => r.rung)).toEqual(["resend"])
        expect(alice.recoveries[0]).toEqual({ peerId: "did:bob", rung: "resend", attempts: 1 })
        expect(alice.connections).toHaveLength(1)
        expect(bob.live().remoteDescription?.type).toBe("offer")
        expect(alice.live().signalingState).toBe("stable")
        expect(alice.errors).toEqual([])
        expect(bob.errors).toEqual([])
    })

    it("resends the answer when that is the half that was lost", async () => {
        // The answerer believes it is done while the offerer waits: a rung
        // gated on "no remote description" would skip the cheap fix.
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        // Bob answers and his answer never arrives. He is polite: doubled deadline.
        const bob = await makeMesh(bus, "did:bob", CALL_ID, {
            now: clock.now,
            drop: (payload) => (payload as { kind?: string }).kind === "description",
        })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        expect(bob.live().remoteDescription?.type).toBe("offer")
        expect(bob.live().localDescription?.type).toBe("answer")
        expect(alice.live().remoteDescription).toBeNull()

        clock.advance(HANDSHAKE_GRACE_MS * 2 + 1)
        bob.mesh.tick()
        await settle()

        expect(bob.recoveries.map((r) => r.rung)).toEqual(["resend"])
        expect(bob.connections).toHaveLength(1)
        const resent = bob.sent.filter((payload) => (payload as { kind?: string }).kind === "description")
        // Bob's own opening offer (ignored by impolite Alice), his answer, and the same answer again.
        const [answer, again] = resent.slice(-2).map((payload) => (payload as { description: RTCSessionDescriptionInit }).description)
        expect(answer.type).toBe("answer")
        expect(again).toEqual(answer)
    })

    it("holds the answerer's own offer until the pair connects", async () => {
        // Bob answers Alice and the answer is lost. An offer from Bob now would
        // reach Alice with hers still out: glare, a fresh negotiation, and both
        // sides taking the same DTLS role.
        const bobLoses = { answer: true }
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID, {
            drop: (payload) =>
                bobLoses.answer && (payload as { description?: { type?: string } }).description?.type === "answer",
        })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        // As in a browser: Bob's own sections are not in Alice's offer, so his
        // answer leaves them to negotiate, and `negotiationneeded` fires while
        // the answer is still being set.
        const pc = bob.live() as unknown as {
            setLocalDescription(d?: RTCSessionDescriptionInit): Promise<void>
            localDescription: RTCSessionDescriptionInit | null
            queueNegotiation(): void
        }
        const setLocal = pc.setLocalDescription.bind(pc)
        pc.setLocalDescription = async (d) => {
            await setLocal(d)
            if (pc.localDescription?.type === "answer") pc.queueNegotiation()
        }
        await settleAll()
        const lastFromBob = () =>
            bob.sent
                .map((p) => (p as { description?: { type?: string } }).description?.type)
                .filter(Boolean)
                .slice(-1)[0]
        expect(bob.live().localDescription?.type).toBe("answer")
        expect(lastFromBob()).toBe("answer")
        expect(alice.live().signalingState).toBe("have-local-offer")

        // Connected: Alice must have applied the answer, so the offer goes.
        bob.live().setConnectionState("connected")
        await settleAll()
        expect(lastFromBob()).toBe("offer")
        expect(bob.errors).toEqual([])
    })

    it("resends a description once, then escalates rather than repeating it", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, {
            now: clock.now,
            drop: (payload) => (payload as { kind?: string }).kind === "description",
        })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()

        clock.advance(HANDSHAKE_GRACE_MS + 1)
        alice.mesh.tick()
        await settle()
        clock.advance(RECOVERY_INTERVAL_MS + 1)
        alice.mesh.tick()
        await settle()
        await settle()

        expect(alice.recoveries.map((r) => r.rung)).toEqual(["resend", "rebuild"])
    })

    it("restarts ICE when a connected pair fails, before throwing anything away", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        const bob = await makeMesh(bus, "did:bob", CALL_ID, { now: clock.now })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        alice.live().setConnectionState("connected")
        alice.live().setConnectionState("failed")
        await settle()

        clock.advance(RECOVERY_INTERVAL_MS + 1)
        alice.mesh.tick()
        await settle()

        // The connection, its transceivers and its DTLS session all survive.
        expect(alice.live().restartIceCalls).toBe(1)
        expect(alice.recoveries.map((r) => r.rung)).toEqual(["ice-restart"])
        expect(alice.connections).toHaveLength(1)
    })

    it("gives a disconnected pair a shorter grace, since it often comes back by itself", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        const bob = await makeMesh(bus, "did:bob", CALL_ID, { now: clock.now })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        alice.live().setConnectionState("connected")
        alice.live().setConnectionState("disconnected")

        clock.advance(DISCONNECT_GRACE_MS - 1)
        alice.mesh.tick()
        expect(alice.live().restartIceCalls).toBe(0)

        clock.advance(2)
        alice.mesh.tick()
        await settle()
        expect(alice.live().restartIceCalls).toBe(1)
    })

    it("does nothing for a disconnected pair that comes back by itself", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        // A negotiated pair, so no offer of Alice's is left pending.
        const bob = await makeMesh(bus, "did:bob", CALL_ID, { now: clock.now })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settleAll()
        expect(alice.live().signalingState).toBe("stable")

        alice.live().setConnectionState("connected")
        alice.live().setConnectionState("disconnected")
        clock.advance(DISCONNECT_GRACE_MS - 1)
        alice.live().setConnectionState("connected")

        clock.advance(DISCONNECT_GRACE_MS * 4)
        alice.mesh.tick()
        await settle()
        expect(alice.recoveries).toEqual([])
    })

    it("rebuilds the pair, and says so, once the cheaper rungs have not worked", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        const bob = await makeMesh(bus, "did:bob", CALL_ID, { now: clock.now })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        // Connected, so nothing to resend — and the host has no `restartIce`.
        alice.live().setConnectionState("connected")
        alice.live().setConnectionState("failed")
        ;(alice.live() as { restartIce?: unknown }).restartIce = undefined

        const streamBefore = alice.mesh.getParticipants()[0].stream
        clock.advance(RECOVERY_INTERVAL_MS + 1)
        alice.mesh.tick()
        await settle()
        await settle()

        expect(alice.recoveries.map((r) => r.rung)).toEqual(["rebuild"])
        expect(alice.connections).toHaveLength(2)
        expect(alice.connections[0].closed).toBe(true)
        // The peer is told, or it would hold a connection to a session that is gone.
        expect(kinds(alice.sent)).toContain("reset")
        expect(bob.connections).toHaveLength(2)
        // A consumer holding the old stream object would render a dead stream.
        const streamAfter = alice.mesh.getParticipants()[0].stream
        expect(streamAfter).not.toBe(streamBefore)
        expect(alice.updates[alice.updates.length - 1].stream).toBe(streamAfter)
    })

    it("answers a reset without sending one back", async () => {
        // Two peers answering resets with resets is a loop with no floor.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        bus.deliver("did:bob", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            to: "did:alice",
            kind: "reset",
        })
        await settle()
        await settle()

        expect(alice.connections).toHaveLength(2)
        expect(alice.connections[0].closed).toBe(true)
        expect(kinds(alice.sent)).not.toContain("reset")
        expect(alice.recoveries.map((r) => r.rung)).toEqual(["rebuild"])
    })

    it("stops repairing after a few attempts, so a spinner is not a promise", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        for (let n = 0; n < MAX_RECOVERY_ATTEMPTS + 3; n += 1) {
            clock.advance(HANDSHAKE_GRACE_MS + RECOVERY_INTERVAL_MS + 1)
            alice.mesh.tick()
            await settle()
        }

        expect(alice.recoveries.length).toBe(MAX_RECOVERY_ATTEMPTS)
        expect(alice.recoveries.map((r) => r.attempts)).toEqual(
            Array.from({ length: MAX_RECOVERY_ATTEMPTS }, (_, i) => i + 1),
        )
    })

    it("lets the impolite peer repair first, and the polite one wait", async () => {
        const clock = makeClock()
        // Alice is impolite for this pair; Bob is polite. Neither ever hears from the other.
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now, drop: () => true })
        const bob = await makeMesh(bus, "did:bob", CALL_ID, { now: clock.now, drop: () => true })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()

        clock.advance(HANDSHAKE_GRACE_MS + 1)
        alice.mesh.tick()
        bob.mesh.tick()
        await settle()

        expect(alice.recoveries).toHaveLength(1)
        expect(bob.recoveries).toHaveLength(0)

        // The polite side covers an impolite side with no slot for this pair at all.
        clock.advance(HANDSHAKE_GRACE_MS + 1)
        bob.mesh.tick()
        await settle()
        expect(bob.recoveries).toHaveLength(1)
    })

    it("waits while a send is still outstanding, rather than piling onto a stalled transport", async () => {
        // An offer not yet delivered and an offer lost look the same; the
        // first broadcast on a fresh neighbourhood has taken eighteen seconds.
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now, holdResults: true })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        const duringHandshake = alice.sent.length
        expect(kinds(alice.sent)).toContain("description")
        expect(alice.results.held).toBe(duringHandshake)

        clock.advance(HANDSHAKE_GRACE_MS * 4)
        alice.mesh.tick()
        await settle()

        expect(alice.sent).toHaveLength(duringHandshake)
        expect(alice.recoveries).toEqual([])

        // The transport reports, so now the silence means something.
        alice.results.release()
        await settle()
        alice.mesh.tick()
        await settle()

        expect(alice.sent.length).toBeGreaterThan(duringHandshake)
        expect(alice.recoveries.map((r) => r.rung)).toEqual(["resend"])
    })

    it("counts a failed publish as accounted for, so a rejected send does not hold repair forever", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now, holdResults: true })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        clock.advance(HANDSHAKE_GRACE_MS + 1)
        alice.results.release({ ok: false, ms: 30_000 })
        await settle()

        expect(alice.recoveries.map((r) => r.rung)).toEqual(["resend"])
    })

    it("repairs on a transport that reports nothing, as though every send landed", async () => {
        // A count that went up and never came down would hold the ladder
        // closed for the life of the call.
        const clock = makeClock()
        let published = 0
        const channel: SignallingChannel = {
            publish: () => { published += 1 },
            onMessage: () => () => {},
        }
        const mesh = new MeshManager({
            channel,
            callId: CALL_ID,
            selfId: "did:alice",
            now: clock.now,
            sweepMs: 0,
            createPeerConnection: () => new FakePeerConnection() as unknown as RTCPeerConnection,
        })
        meshes.push(mesh)
        const rungs: RecoveryRung[] = []
        mesh.on("peer-recovery", (_peerId: string, attempt: { rung: RecoveryRung }) => rungs.push(attempt.rung))
        await mesh.join(stream())

        mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        const before = published

        clock.advance(HANDSHAKE_GRACE_MS + 1)
        mesh.tick()
        await settle()

        expect(rungs).toEqual(["resend"])
        expect(published).toBe(before + 1)
    })

    it("forgets a pair's failures once it connects", async () => {
        // The budget bounds one failure, not a session.
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        clock.advance(HANDSHAKE_GRACE_MS + 1)
        alice.mesh.tick()
        await settle()
        expect(alice.recoveries).toHaveLength(1)

        alice.live().setConnectionState("connected")
        alice.live().setConnectionState("failed")

        for (let n = 0; n < MAX_RECOVERY_ATTEMPTS; n += 1) {
            clock.advance(RECOVERY_INTERVAL_MS + 1)
            alice.mesh.tick()
            await settle()
        }

        expect(alice.recoveries).toHaveLength(1 + MAX_RECOVERY_ATTEMPTS)
    })

    it("reconnects one peer on request, whatever the connection claims to be", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        // Deliberately healthy: the person watching judges better than `connectionState`.
        alice.live().setConnectionState("connected")
        alice.mesh.reconnect("did:bob")
        await settle()
        await settle()

        expect(alice.connections).toHaveLength(2)
        expect(alice.connections[0].closed).toBe(true)
        expect(kinds(alice.sent)).toContain("reset")
        expect(bob.connections).toHaveLength(2)
        expect(alice.recoveries).toEqual([{ peerId: "did:bob", rung: "rebuild", attempts: 1 }])
    })

    it("resets the automatic budget when reconnecting by hand", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        for (let n = 0; n < MAX_RECOVERY_ATTEMPTS + 1; n += 1) {
            clock.advance(HANDSHAKE_GRACE_MS + RECOVERY_INTERVAL_MS + 1)
            alice.mesh.tick()
            await settle()
        }
        expect(alice.recoveries).toHaveLength(MAX_RECOVERY_ATTEMPTS)

        alice.mesh.reconnect("did:bob")
        await settle()
        clock.advance(HANDSHAKE_GRACE_MS + RECOVERY_INTERVAL_MS + 1)
        alice.mesh.tick()
        await settle()

        expect(alice.recoveries.slice(MAX_RECOVERY_ATTEMPTS).map((r) => r.attempts)).toEqual([1, 2])
    })

    it("does nothing when asked to reconnect someone who is not in the call", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        alice.mesh.reconnect("did:nobody")
        await settle()

        expect(alice.connections).toHaveLength(1)
        expect(kinds(alice.sent)).not.toContain("reset")
    })

    it("repairs one pair per sweep", async () => {
        // A network that just came back has every pair due at once, and a
        // burst of offers is what made them fail.
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        alice.mesh.setRoster(["did:alice", "did:bob", "did:carol", "did:dave"])
        await settle()

        clock.advance(HANDSHAKE_GRACE_MS + 1)
        alice.mesh.tick()
        await settle()

        expect(alice.recoveries).toHaveLength(1)
    })

    it("spaces repairs by REPAIR_SPACING_MS, whatever asks for them", async () => {
        // A settled send calls `tick`, and a repair publishes: without a
        // floor the two chase each other through every pair in the call.
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        alice.mesh.setRoster(["did:alice", "did:bob", "did:carol"])
        await settle()

        clock.advance(HANDSHAKE_GRACE_MS + 1)
        alice.mesh.tick()
        await settle()
        expect(alice.recoveries).toHaveLength(1)

        alice.mesh.tick()
        clock.advance(REPAIR_SPACING_MS - 1)
        alice.mesh.tick()
        await settle()
        expect(alice.recoveries).toHaveLength(1)

        clock.advance(1)
        alice.mesh.tick()
        await settle()
        expect(alice.recoveries.map((r) => r.peerId).sort()).toEqual(["did:bob", "did:carol"])
    })

    it("acts on a failed connection at once, without waiting for the sweep", async () => {
        const clock = makeClock()
        const alice = await makeMesh(bus, "did:alice", CALL_ID, { now: clock.now })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        clock.advance(RECOVERY_INTERVAL_MS + 1)
        alice.live().setConnectionState("failed")
        await settle()

        expect(alice.recoveries).toHaveLength(1)
    })
})

describe("a connected pair whose renegotiation went unanswered", () => {
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    /** Alice and Bob, negotiated and connected, with Bob's descriptions and Alice's answers droppable from here on. */
    async function connectedPair() {
        const clock = makeClock()
        const bobLoses = { description: false }
        const aliceLoses = { answer: false }
        const alice = await makeMesh(bus, "did:alice", CALL_ID, {
            now: clock.now,
            drop: (payload) =>
                aliceLoses.answer &&
                (payload as { description?: { type?: string } }).description?.type === "answer",
        })
        const bob = await makeMesh(bus, "did:bob", CALL_ID, {
            now: clock.now,
            drop: (payload) => bobLoses.description && (payload as { kind?: string }).kind === "description",
        })
        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        for (let i = 0; i < 4; i += 1) await settle()
        alice.live().setConnectionState("connected")
        bob.live().setConnectionState("connected")
        expect(alice.live().signalingState).toBe("stable")
        expect(bob.live().signalingState).toBe("stable")
        return { clock, alice, bob, bobLoses, aliceLoses }
    }

    /** The descriptions a mesh has published, newest last. */
    const descriptions = (sent: unknown[]) =>
        sent.filter((payload) => (payload as { kind?: string }).kind === "description")

    it("repairs a renegotiation reordered behind the answer it follows", async () => {
        // The case T20 found, pinned without timing: Bob answers Alice's
        // offer, then offers his own media. His offer reaches Alice first,
        // while hers is still out; Alice is impolite, so she reads it as glare
        // and ignores it. Then the answer lands and the pair looks settled.
        const { clock, alice, bob, bobLoses } = await connectedPair()
        bobLoses.description = true

        alice.live().restartIce()
        await settleAll()
        expect(bob.live().signalingState).toBe("stable")
        bob.live().restartIce()
        await settleAll()
        const [answer, offer] = descriptions(bob.sent).slice(-2)
        expect((answer as { description: { type: string } }).description.type).toBe("answer")
        expect((offer as { description: { type: string } }).description.type).toBe("offer")

        bus.deliverTo("did:alice", "did:bob", offer)
        await settleAll()
        bus.deliverTo("did:alice", "did:bob", answer)
        await settleAll()
        expect(alice.live().signalingState).toBe("stable")
        expect(bob.live().signalingState).toBe("have-local-offer")

        bobLoses.description = false
        clock.advance(RECOVERY_INTERVAL_MS)
        bob.mesh.tick()
        await settleAll()

        expect(bob.recoveries.map((r) => r.rung)).toEqual(["resend"])
        expect(bob.live().signalingState).toBe("stable")
        expect(alice.live().remoteDescription?.type).toBe("offer")
        expect(alice.errors).toEqual([])
        expect(bob.errors).toEqual([])
    })

    it("drops a late duplicate answer instead of reporting an error", async () => {
        // Slow rather than lost: the resend fires, the peer answers both
        // copies, and the second answer arrives with nothing left to settle.
        const { clock, alice, bob, aliceLoses } = await connectedPair()

        aliceLoses.answer = true
        bob.live().restartIce()
        await settleAll()
        const late = descriptions(alice.sent).slice(-1)[0]
        expect(bob.live().signalingState).toBe("have-local-offer")

        aliceLoses.answer = false
        clock.advance(RECOVERY_INTERVAL_MS)
        bob.mesh.tick()
        await settleAll()
        expect(bob.live().signalingState).toBe("stable")

        bus.deliverTo("did:bob", "did:alice", late)
        await settleAll()

        expect(bob.errors).toEqual([])
        expect(bob.live().signalingState).toBe("stable")
    })

    it("drops an answer to an offer the connection has since replaced", async () => {
        // A late copy of the answer to Bob's last offer reaches him while his
        // next offer is out. Applied, the browser refuses it ("The order of
        // m-lines in answer doesn't match order in offer").
        const { clock, alice, bob, aliceLoses } = await connectedPair()
        bob.live().restartIce()
        await settleAll()
        const stale = descriptions(alice.sent).slice(-1)[0] as { description: { type: string } }
        expect(stale.description.type).toBe("answer")

        aliceLoses.answer = true
        bob.live().restartIce()
        await settleAll()
        expect(bob.live().signalingState).toBe("have-local-offer")
        bus.deliverTo("did:bob", "did:alice", stale)
        await settleAll()
        expect(bob.errors).toEqual([])
        expect(bob.live().signalingState).toBe("have-local-offer")

        // The answer to the current offer still settles it.
        aliceLoses.answer = false
        clock.advance(RECOVERY_INTERVAL_MS)
        bob.mesh.tick()
        await settleAll()
        expect(bob.live().signalingState).toBe("stable")
        expect(bob.errors).toEqual([])
    })

    it("answers the peer's offer when negotiation is asked for while it is applied", async () => {
        // The browser can fire `negotiationneeded` during the rollback that
        // accepting the peer's offer starts. A description picked from the
        // state when the handler runs is then the answer, and the answering
        // path, finding the connection stable, makes an offer in its place.
        const { alice, bob } = await connectedPair()
        const pc = bob.live() as unknown as {
            setRemoteDescription(description: RTCSessionDescriptionInit): Promise<void>
            onnegotiationneeded: (() => void) | null
        }
        const setRemote = pc.setRemoteDescription.bind(pc)
        pc.setRemoteDescription = async (description) => {
            await setRemote(description)
            if (description.type === "offer") pc.onnegotiationneeded?.()
        }
        const before = descriptions(bob.sent).length

        alice.live().restartIce()
        await settleAll()

        const sent = descriptions(bob.sent).slice(before) as { description: { type: string } }[]
        expect(sent.map((message) => message.description.type)).toEqual(["answer"])
        expect(alice.live().signalingState).toBe("stable")
        expect(bob.live().signalingState).toBe("stable")
        expect(bob.errors).toEqual([])
    })

    it("ignores an offer overtaken by a later one", async () => {
        // Alice's earlier offer arrives after Bob applied her next one.
        const { alice, bob } = await connectedPair()
        alice.live().restartIce()
        await settleAll()
        const overtaken = descriptions(alice.sent).slice(-1)[0] as { description: { type: string } }
        expect(overtaken.description.type).toBe("offer")
        alice.live().restartIce()
        await settleAll()
        expect(bob.live().signalingState).toBe("stable")

        bus.deliverTo("did:bob", "did:alice", overtaken)
        await settleAll()
        expect(bob.errors).toEqual([])
        expect(bob.live().signalingState).toBe("stable")
    })

    it("stops repairing a peer that never answers a renegotiation, across rebuilds", async () => {
        // Each rebuilt connection connects before it renegotiates, which resets
        // a slot's own budget. The renegotiation budget is the peer's, so the
        // resend → rebuild → connect → unanswered cycle still runs out.
        const { clock, bob, bobLoses } = await connectedPair()
        bobLoses.description = true

        for (let round = 0; round < MAX_RECOVERY_ATTEMPTS + 2; round += 1) {
            bob.live().setConnectionState("connected")
            bob.live().restartIce()
            await settleAll()
            clock.advance(RECOVERY_INTERVAL_MS)
            bob.mesh.tick()
            await settleAll()
        }
        const spent = bob.recoveries.length
        expect(spent).toBeGreaterThan(0)

        bob.live().setConnectionState("connected")
        bob.live().restartIce()
        await settleAll()
        for (let i = 0; i < 4; i += 1) {
            clock.advance(RECOVERY_INTERVAL_MS)
            bob.mesh.tick()
            await settleAll()
        }

        expect(bob.recoveries).toHaveLength(spent)
        expect(bob.recoveries.filter((r) => r.rung !== "ice-restart").length).toBeLessThanOrEqual(MAX_RECOVERY_ATTEMPTS)
    })

    it("gives a peer its renegotiation budget back once one is answered", async () => {
        const { clock, bob, bobLoses } = await connectedPair()

        bobLoses.description = true
        bob.live().restartIce()
        await settleAll()
        clock.advance(RECOVERY_INTERVAL_MS)
        bob.mesh.tick()
        await settleAll()
        expect(bob.recoveries.map((r) => r.rung)).toEqual(["resend"])

        // The next renegotiation is answered, and the one after that is lost.
        bobLoses.description = false
        clock.advance(RECOVERY_INTERVAL_MS)
        bob.mesh.tick()
        await settleAll()
        expect(bob.live().signalingState).toBe("stable")

        for (let round = 0; round < MAX_RECOVERY_ATTEMPTS + 1; round += 1) {
            bobLoses.description = true
            bob.live().setConnectionState("connected")
            bob.live().restartIce()
            await settleAll()
            bobLoses.description = false
            clock.advance(RECOVERY_INTERVAL_MS)
            bob.mesh.tick()
            await settleAll()
            expect(bob.live().signalingState).toBe("stable")
        }
    })

    it("resends an offer that never got its answer, though the pair reads connected", async () => {
        // The answerer's media travels in an offer of its own, sent once the
        // pair is up. Reordered behind the answer it follows, an impolite peer
        // reads it as glare and ignores it; lost, it never arrives. Either way
        // the pair reads `connected` with one direction silent. Found by T20.
        const { clock, alice, bob, bobLoses } = await connectedPair()

        bobLoses.description = true
        bob.live().restartIce()
        await settle()
        await settle()
        expect(bob.live().signalingState).toBe("have-local-offer")
        expect(alice.live().signalingState).toBe("stable")

        bobLoses.description = false
        clock.advance(RECOVERY_INTERVAL_MS - 1)
        bob.mesh.tick()
        expect(bob.recoveries).toEqual([])

        clock.advance(1)
        bob.mesh.tick()
        for (let i = 0; i < 3; i += 1) await settle()

        expect(bob.recoveries.map((r) => r.rung)).toEqual(["resend"])
        expect(bob.connections).toHaveLength(1)
        expect(bob.live().signalingState).toBe("stable")
        expect(alice.errors).toEqual([])
        expect(bob.errors).toEqual([])
    })

    it("leaves a connected pair alone while nothing it offered is pending", async () => {
        const { clock, alice, bob } = await connectedPair()

        clock.advance(RECOVERY_INTERVAL_MS * 4)
        alice.mesh.tick()
        bob.mesh.tick()
        await settle()

        expect(alice.recoveries).toEqual([])
        expect(bob.recoveries).toEqual([])
    })

    it("rebuilds the pair when the resent offer goes unanswered too", async () => {
        const { clock, bob, bobLoses } = await connectedPair()

        bobLoses.description = true
        bob.live().restartIce()
        await settle()
        await settle()

        clock.advance(RECOVERY_INTERVAL_MS)
        bob.mesh.tick()
        await settle()
        clock.advance(RECOVERY_INTERVAL_MS)
        bob.mesh.tick()
        await settle()

        expect(bob.recoveries.map((r) => r.rung)).toEqual(["resend", "rebuild"])
        expect(kinds(bob.sent)).toContain("reset")
    })
})

// ── Diagnostics and configuration ───────────────────────────────────

describe("what a call is told about its own connections", () => {
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    it("uses the ICE servers it is given, and its own when given none", async () => {
        const mine = [{ urls: "turn:relay.example.org:3478", username: "u", credential: "p" }]
        const configured = await makeMesh(bus, "did:alice", CALL_ID, { iceServers: mine })
        configured.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        expect(configured.configs[0].iceServers).toEqual(mine)

        const bare = await makeMesh(bus, "did:zoe", CALL_ID)
        bare.mesh.setRoster(["did:zoe", "did:bob"])
        await settle()
        expect(bare.configs[0].iceServers?.[0]).toMatchObject({
            urls: expect.arrayContaining(["stun:stun.l.google.com:19302"]),
        })
        expect(bare.configs[0].iceServers).toEqual(DEFAULT_ICE_SERVERS)

        // An empty list is "none given", not "no servers at all".
        const empty = await makeMesh(bus, "did:yan", CALL_ID, { iceServers: [] })
        empty.mesh.setRoster(["did:yan", "did:bob"])
        expect(empty.configs[0].iceServers).toEqual(DEFAULT_ICE_SERVERS)
    })

    it("pre-gathers one candidate set", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(alice.configs[0].iceCandidatePoolSize).toBe(1)
    })

    it("says how the media is actually reaching the other end", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        alice.live().stats = [
            { id: "pair-1", type: "candidate-pair", state: "failed", localCandidateId: "cand-1" },
            { id: "pair-2", type: "candidate-pair", state: "succeeded", localCandidateId: "cand-2" },
            { id: "cand-1", type: "local-candidate", candidateType: "host" },
            { id: "cand-2", type: "local-candidate", candidateType: "relay" },
        ]

        expect(await alice.mesh.transportOf("did:bob")).toBe("relay")
        expect(await alice.mesh.transportOf("did:nobody")).toBeNull()
    })

    it("answers nothing rather than guessing when no pair has succeeded", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        alice.live().stats = [{ id: "pair-1", type: "candidate-pair", state: "in-progress", localCandidateId: "cand-1" }]
        expect(await alice.mesh.transportOf("did:bob")).toBeNull()
    })

    it("reports a stats failure as an error and answers nothing", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        alice.live().getStats = async () => { throw new Error("stats unavailable") }
        expect(await alice.mesh.transportOf("did:bob")).toBeNull()
        expect(alice.errors).toHaveLength(1)
    })

    it("sweeps on its own timer when it is given one", async () => {
        // Every other test drives `tick()` by hand, which would leave the line
        // that makes this run in production untested.
        const { channel } = bus.channel("did:alice")
        const clock = makeClock()
        const recoveries: RecoveryRung[] = []
        const mesh = new MeshManager({
            channel,
            callId: CALL_ID,
            selfId: "did:alice",
            now: clock.now,
            sweepMs: 1,
            createPeerConnection: () => new FakePeerConnection() as unknown as RTCPeerConnection,
        })
        meshes.push(mesh)
        mesh.on("peer-recovery", (_peerId: string, attempt: { rung: RecoveryRung }) => recoveries.push(attempt.rung))
        await mesh.join(stream())

        mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        clock.advance(HANDSHAKE_GRACE_MS + 1)
        await new Promise((resolve) => setTimeout(resolve, 10))

        expect(recoveries.length).toBeGreaterThan(0)
        await mesh.leave()

        // And the timer goes with the mesh.
        const after = recoveries.length
        clock.advance(HANDSHAKE_GRACE_MS * 10)
        await new Promise((resolve) => setTimeout(resolve, 10))
        expect(recoveries).toHaveLength(after)
    })
})

// ── Signalling before the roster ────────────────────────────────────

describe("signalling that arrives before the roster", () => {
    /**
     * A peer who denied the microphone has no outbound tracks and never fires
     * `negotiationneeded` again after its offer is dropped, so dropping it
     * left that peer connected to nobody.
     */
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    it("keeps the held offer when its candidates overflow the buffer", async () => {
        // An offer is followed by many candidates; held for a peer the roster
        // has not listed yet, the overflow must not take the offer with it.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        for (let i = 0; i < 40; i += 1) {
            alice.live().emitCandidate({ candidate: `candidate:${i} 1 udp 1 10.0.0.${i} 9 typ host`, sdpMid: "0" })
        }

        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settleAll()

        expect(bob.live().remoteDescription?.type).toBe("offer")
        expect(bob.errors).toEqual([])
    })

    it("connects a peer whose roster arrived after the offer, with no tracks of their own", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await alice.mesh.replaceTrack("audio", track("audio"))
        await settle()
        await settle()

        expect(bob.connections).toHaveLength(0)

        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()

        // Held rather than dropped, and replayed the moment the roster vouches for Alice.
        expect(bob.connections).toHaveLength(1)
        expect(bob.connections[0].remoteDescription?.type).toBe("offer")
        expect(bob.errors).toEqual([])
        expect(alice.errors).toEqual([])
    })

    it("does not negotiate with an agent the roster never lists", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const stranger = await makeMesh(bus, "did:stranger", CALL_ID)

        stranger.mesh.setRoster(["did:stranger", "did:alice"])
        await stranger.mesh.replaceTrack("audio", track("audio"))
        await settle()
        await settle()

        // Holding a message is not a promise to negotiate.
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        expect(alice.connections).toHaveLength(1)
        expect(alice.connections[0].remoteDescription).toBeNull()

        // And what was held for the stranger is gone, not waiting: listed
        // later, after the stranger has stopped talking, nothing is replayed.
        await stranger.mesh.leave()
        alice.mesh.setRoster(["did:alice", "did:bob", "did:stranger"])
        await settle()
        expect(alice.connections[1].remoteDescription).toBeNull()
    })

    it("bounds what it holds for an agent nobody has vouched for", async () => {
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)

        bob.mesh.setRoster(["did:alice", "did:bob"])
        for (let n = 0; n < 40; n += 1) {
            await bob.mesh.replaceTrack("audio", track("audio"))
            await bob.mesh.replaceTrack("audio", null)
            bob.live().emitCandidate({ candidate: `c${n}` })
        }
        await settle()

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()

        // It replayed something rather than nothing, and did not replay everything.
        expect(alice.connections[0].remoteDescription).not.toBeNull()
        expect(alice.connections[0].candidates.length).toBeLessThan(40)
    })
})

// ── Generations ─────────────────────────────────────────────────────

describe("a description from a different connection", () => {
    /**
     * A message from one of the peer's connections used to be applied to
     * whichever connection this side held, and the browser refuses that:
     * "The order of m-lines in subsequent offer doesn't match order from
     * previous offer/answer". Each case is a way the two sides come to hold
     * different generations of the pair.
     */
    let bus: InMemoryBus

    beforeEach(() => {
        bus = new InMemoryBus()
    })

    async function connected(aliceOptions: MeshOptions = {}) {
        const alice = await makeMesh(bus, "did:alice", CALL_ID, aliceOptions)
        const bob = await makeMesh(bus, "did:bob", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settleAll()
        expect(alice.live().signalingState).toBe("stable")
        expect(bob.live().signalingState).toBe("stable")
        return { alice, bob }
    }

    it("names both connections on everything it sends", async () => {
        const { alice, bob } = await connected()
        const answer = bob.sent.find((payload) =>
            (payload as { description?: { type?: string } }).description?.type === "answer") as
            { gen?: string; peerGen?: string }
        const offer = alice.sent.find((payload) =>
            (payload as { description?: { type?: string } }).description?.type === "offer") as
            { gen?: string; peerGen?: string }

        expect(typeof offer.gen).toBe("string")
        // Bob's answer names Alice's connection as the one it is for.
        expect(answer.peerGen).toBe(offer.gen)
        expect(typeof answer.gen).toBe("string")
    })

    it("follows a peer that reloaded while it was still on the roster", async () => {
        // A reload starts a new connection and offers from it with no reset;
        // presence has not noticed, so this side's connection survives.
        const { alice, bob } = await connected()
        await bob.mesh.leave()

        const bobAgain = await makeMesh(bus, "did:bob", CALL_ID)
        bobAgain.mesh.setRoster(["did:alice", "did:bob"])
        await settleAll()

        expect(alice.errors).toEqual([])
        expect(bobAgain.errors).toEqual([])
        // Alice started over to match, rather than applying the offer to a connection it was not for.
        expect(alice.connections).toHaveLength(2)
        expect(alice.connections[0].closed).toBe(true)
        expect(alice.live().signalingState).toBe("stable")
        expect(bobAgain.live().signalingState).toBe("stable")
        expect(bobAgain.live().remoteDescription?.type).toBe("answer")
    })

    it("is not undone by the reset that the new offer overtook", async () => {
        // A rebuild sends `reset` and then offers from the new connection; on
        // an unordered transport the offer can arrive first.
        let holding = false
        const held: unknown[] = []
        const { alice, bob } = await connected({
            drop: (payload) => {
                if (!holding || (payload as { kind?: string }).kind !== "reset") return false
                held.push(payload)
                return true
            },
        })

        holding = true
        alice.mesh.reconnect("did:bob")
        await settleAll()

        expect(held).toHaveLength(1)
        expect(bob.errors).toEqual([])
        expect(bob.connections).toHaveLength(2)

        // Now the reset turns up.
        bus.deliver("did:alice", held[0])
        await settleAll()

        expect(bob.connections).toHaveLength(2)
        expect(bob.live().closed).toBe(false)
        expect(bob.live().signalingState).toBe("stable")
        expect(alice.live().signalingState).toBe("stable")
        expect(bob.errors).toEqual([])
        expect(alice.errors).toEqual([])
    })

    it("ignores an offer from a connection the peer has since replaced", async () => {
        const { alice, bob } = await connected()
        const staleOffer = bob.sent.find((payload) => (payload as { kind?: string }).kind === "description")
        await bob.mesh.leave()

        const bobAgain = await makeMesh(bus, "did:bob", CALL_ID)
        bobAgain.mesh.setRoster(["did:alice", "did:bob"])
        await settleAll()
        const before = alice.connections.length

        // The first Bob's message, delayed until after Alice has moved on to the second.
        bus.deliver("did:bob", staleOffer)
        await settleAll()

        expect(alice.connections).toHaveLength(before)
        expect(alice.live().signalingState).toBe("stable")
        expect(alice.errors).toEqual([])
    })

    it("ignores signalling addressed to a connection of its own that it has replaced", async () => {
        const { alice, bob } = await connected()
        const toOldAlice = bob.sent.filter((payload) => (payload as { kind?: string }).kind === "description")

        // Alice rebuilds; Bob follows. Anything Bob said to the first Alice is now about nothing.
        alice.mesh.reconnect("did:bob")
        await settleAll()
        const before = { alice: alice.connections.length, bob: bob.connections.length }

        for (const payload of toOldAlice) bus.deliver("did:bob", payload)
        await settleAll()

        expect(alice.connections).toHaveLength(before.alice)
        expect(bob.connections).toHaveLength(before.bob)
        expect(alice.errors).toEqual([])
    })

    it("still answers a peer that does not say which connection it is", async () => {
        // A peer predating generations sends none; mixed versions still connect.
        const alice = await makeMesh(bus, "did:alice", CALL_ID)
        alice.mesh.setRoster(["did:alice", "did:aaron"])
        await settle()

        bus.deliver("did:aaron", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            to: "did:alice",
            kind: "description",
            description: { type: "offer", sdp: "offer-sdp@legacy" },
        })
        await settleAll()

        expect(alice.live().remoteDescription?.sdp).toBe("offer-sdp@legacy")
        expect(alice.errors).toEqual([])
    })
})

// ── Telepresence channel ────────────────────────────────────────────

describe("createTelepresenceChannel", () => {
    const PREDICATE = "ad4m://session/rtc"

    function fakeProxy() {
        const handlers = new Set<(payload: PerspectiveExpression) => void>()
        const sends: { resolve(ok: boolean): void; reject(error: unknown): void; links: unknown[] }[] = []
        const proxy: TelepresenceProxy = {
            sendBroadcastU: (status) =>
                new Promise<boolean>((resolve, reject) => {
                    sends.push({ resolve, reject, links: status.links })
                }),
            addSignalHandler: async (handler) => { handlers.add(handler) },
            removeSignalHandler: (handler) => { handlers.delete(handler) },
        }
        /** A signal as the executor delivers it, with both proofs checked and recorded in `valid`. */
        const signal = (
            author: string,
            data: { source?: unknown; predicate?: unknown; target?: unknown },
            proofs: { signer?: string; signalValid?: boolean | null; linkValid?: boolean } = {},
        ) => {
            const expression = {
                author: proofs.signer ?? author,
                // `null`: a proof the executor never evaluated.
                proof: proofs.signalValid === null ? {} : { valid: proofs.signalValid ?? true },
                data: { links: [{ author, data, proof: { valid: proofs.linkValid ?? true } }] },
            } as unknown as PerspectiveExpression
            for (const handler of [...handlers]) handler(expression)
        }
        return { proxy, sends, handlers, signal }
    }

    it("accepts only a link whose author signed it, inside a signal that author signed", async () => {
        // The mesh acts on the sender id — offers, answers, resets, rebuilds —
        // so an author claim the executor could not verify must never reach it.
        const { proxy, signal } = fakeProxy()
        const channel = createTelepresenceChannel(proxy, "did:alice")
        const heard: [string, unknown][] = []
        channel.onMessage((from, payload) => heard.push([from, payload]))
        await settle()
        const message = (n: number) => ({ source: JSON.stringify({ n }), predicate: PREDICATE, target: "*" })

        signal("did:bob", message(1), { linkValid: false })
        signal("did:bob", message(2), { signalValid: false })
        signal("did:bob", message(3), { signer: "did:mallory" })
        signal("did:bob", message(4), { signalValid: null })
        signal("did:bob", message(5))

        expect(heard).toEqual([["did:bob", { n: 5 }]])
    })

    it("lets a forged reset change nothing on the mesh", async () => {
        // End to end through the adapter: a reset whose link claims Bob but
        // whose proof does not hold must not rebuild Alice's pair with Bob.
        const { proxy, signal } = fakeProxy()
        const alice = new MeshManager({
            channel: createTelepresenceChannel(proxy, "did:alice"),
            callId: CALL_ID,
            selfId: "did:alice",
            createPeerConnection: () => new FakePeerConnection() as unknown as RTCPeerConnection,
            sweepMs: 0,
        })
        meshes.push(alice)
        await alice.join(stream())
        alice.setRoster(["did:alice", "did:bob"])
        await settle()
        const before = alice.getParticipants()[0]?.stream
        const recoveries: unknown[] = []
        alice.on("peer-recovery", (...args: unknown[]) => recoveries.push(args))

        const reset = { source: JSON.stringify({ v: CALL_PROTOCOL_VERSION, call: CALL_ID, to: "did:alice", kind: "reset" }), predicate: PREDICATE, target: "did:alice" }
        signal("did:bob", reset, { linkValid: false })
        signal("did:bob", reset, { signer: "did:mallory" })
        await settleAll()

        expect(recoveries).toEqual([])
        expect(alice.getParticipants()[0]?.stream).toBe(before)

        signal("did:bob", reset)
        await settleAll()
        expect(recoveries).toHaveLength(1)
    })

    it("publishes over broadcast, addressed in the link target", () => {
        const { proxy, sends } = fakeProxy()
        const channel = createTelepresenceChannel(proxy, "did:alice")

        channel.publish({ hello: 1 }, { agentId: "did:bob" })
        channel.publish({ hello: 2 })

        expect(sends.map((send) => send.links)).toEqual([
            [{ source: JSON.stringify({ hello: 1 }), predicate: PREDICATE, target: "did:bob" }],
            [{ source: JSON.stringify({ hello: 2 }), predicate: PREDICATE, target: "*" }],
        ])
    })

    it("delivers what is for this agent, with the author as the sender", async () => {
        const { proxy, signal, handlers } = fakeProxy()
        const channel = createTelepresenceChannel(proxy, "did:alice")
        const heard: [string, unknown][] = []
        const unsubscribe = channel.onMessage((from, payload) => heard.push([from, payload]))
        await settle()

        signal("did:bob", { source: JSON.stringify({ n: 1 }), predicate: PREDICATE, target: "did:alice" })
        signal("did:bob", { source: JSON.stringify({ n: 2 }), predicate: PREDICATE, target: "*" })
        signal("did:bob", { source: JSON.stringify({ n: 3 }), predicate: PREDICATE, target: "did:carol" })
        signal("did:bob", { source: JSON.stringify({ n: 4 }), predicate: "other://predicate", target: "*" })
        signal("did:alice", { source: JSON.stringify({ n: 5 }), predicate: PREDICATE, target: "*" })
        signal("did:bob", { source: "{not json", predicate: PREDICATE, target: "*" })

        expect(heard).toEqual([["did:bob", { n: 1 }], ["did:bob", { n: 2 }]])

        unsubscribe()
        expect(handlers.size).toBe(0)
    })

    it("reports one result per publish, failed when the send is refused or rejected", async () => {
        const { proxy, sends } = fakeProxy()
        const channel = createTelepresenceChannel(proxy, "did:alice")
        const results: { ok: boolean; ms: number }[] = []
        const unwatch = channel.onPublishResult!((result) => results.push(result))
        const consoleError = jest.spyOn(console, "error").mockImplementation()

        channel.publish({ n: 1 })
        channel.publish({ n: 2 })
        channel.publish({ n: 3 })
        await settle()
        // Nothing is reported while the sends are outstanding.
        expect(results).toEqual([])

        sends[1].resolve(false)
        sends[0].resolve(true)
        sends[2].reject(new Error("executor gone"))
        await settle()

        expect(results.map((result) => result.ok)).toEqual([false, true, false])
        expect(results.every((result) => typeof result.ms === "number" && result.ms >= 0)).toBe(true)
        expect(consoleError).toHaveBeenCalled()

        unwatch()
        channel.publish({ n: 4 })
        sends[3].resolve(true)
        await settle()
        expect(results).toHaveLength(3)

        consoleError.mockRestore()
    })

    it("reports a failed result when the send throws synchronously", async () => {
        // An unreported publish stays outstanding and holds the mesh's repairs off for good.
        const proxy: TelepresenceProxy = {
            sendBroadcastU: () => { throw new Error("not connected") },
            addSignalHandler: async () => {},
            removeSignalHandler: () => {},
        }
        const channel = createTelepresenceChannel(proxy, "did:alice")
        const results: { ok: boolean; ms: number }[] = []
        channel.onPublishResult!((result) => results.push(result))
        const consoleError = jest.spyOn(console, "error").mockImplementation()

        expect(() => channel.publish({ n: 1 })).not.toThrow()
        await settle()

        expect(results.map((result) => result.ok)).toEqual([false])
        consoleError.mockRestore()
    })

    it("lets the mesh defer repair until telepresence has reported", async () => {
        const { proxy, sends } = fakeProxy()
        const clock = makeClock()
        const mesh = new MeshManager({
            channel: createTelepresenceChannel(proxy, "did:alice"),
            callId: CALL_ID,
            selfId: "did:alice",
            now: clock.now,
            sweepMs: 0,
            createPeerConnection: () => new FakePeerConnection() as unknown as RTCPeerConnection,
        })
        meshes.push(mesh)
        const rungs: RecoveryRung[] = []
        mesh.on("peer-recovery", (_peerId: string, attempt: { rung: RecoveryRung }) => rungs.push(attempt.rung))
        await mesh.join(stream())

        mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        clock.advance(HANDSHAKE_GRACE_MS + 1)
        mesh.tick()
        expect(rungs).toEqual([])

        for (const send of [...sends]) send.resolve(true)
        await settle()
        expect(rungs).toEqual(["resend"])
    })
})
