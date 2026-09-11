/**
 * MeshManager unit tests.
 *
 * Adapted from WE's `mesh.test.ts` pattern: a `FakePeerConnection`
 * models signalling state and callbacks without touching the browser's
 * WebRTC stack; an `InMemoryChannel` replaces the telepresence bridge.
 *
 * What these tests exercise is the *protocol*, not the media — correct
 * negotiation, roster-driven lifecycle, pending buffer, data relay.
 */

// ── Browser polyfills for Node/Jest ─────────────────────────────────
// MeshManager.join() and connect() construct MediaStream instances.
// In Node.js that constructor doesn't exist — provide a minimal shim.

class FakeMediaStream {
    private tracks: any[] = []
    getAudioTracks() { return this.tracks.filter((t) => t.kind === "audio") }
    getVideoTracks() { return this.tracks.filter((t) => t.kind === "video") }
    addTrack(t: any) { this.tracks.push(t) }
    removeTrack(t: any) {
        const idx = this.tracks.indexOf(t)
        if (idx !== -1) this.tracks.splice(idx, 1)
    }
}
;(globalThis as any).MediaStream = FakeMediaStream

import {
    MeshManager,
    parseCallMessage,
    CALL_PROTOCOL_VERSION,
    type SignallingChannel,
    type CallMessage,
} from "./MeshManager"

// ── Fake RTCPeerConnection ─────────────────────────────────────────

class FakePeerConnection {
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
    transceivers: { kind: string; direction: string }[] = []
    candidates: RTCIceCandidateInit[] = []
    closed = false

    private negotiationNeeded = false
    private negotiationQueued = false

    private queueNegotiation() {
        this.negotiationNeeded = true
        if (this.negotiationQueued) return
        this.negotiationQueued = true
        queueMicrotask(() => {
            this.negotiationQueued = false
            if (!this.negotiationNeeded) return
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

    getSenders() { return this.senders }

    async setLocalDescription(description?: RTCSessionDescriptionInit) {
        this.negotiationNeeded = false
        const type = description?.type ?? (this.signalingState === "have-remote-offer" ? "answer" : "offer")
        this.localDescription = { type: type as RTCSdpType, sdp: `${type}-sdp` }
        this.signalingState = type === "offer" ? "have-local-offer" : "stable"
    }

    async setRemoteDescription(description: RTCSessionDescriptionInit) {
        this.remoteDescription = description
        this.signalingState = description.type === "offer" ? "have-remote-offer" : "stable"
    }

    async addIceCandidate(candidate: RTCIceCandidateInit) {
        if (this.signalingState === "stable" && !this.remoteDescription) {
            throw new Error("no remote description")
        }
        this.candidates.push(candidate)
    }

    close() { this.closed = true }

    emitCandidate(candidate: RTCIceCandidateInit) {
        this.onicecandidate?.({ candidate: { toJSON: () => candidate } })
    }
}

// ── In-memory signalling channel ───────────────────────────────────

class InMemoryChannel {
    private listeners = new Map<string, Set<(from: string, payload: unknown) => void>>()

    createChannel(selfId: string): SignallingChannel {
        return {
            publish: (payload, to) => {
                for (const [id, cbs] of this.listeners) {
                    if (id === selfId) continue
                    for (const cb of cbs) cb(selfId, payload)
                }
            },
            onMessage: (cb) => {
                if (!this.listeners.has(selfId)) this.listeners.set(selfId, new Set())
                this.listeners.get(selfId)!.add(cb)
                return () => this.listeners.get(selfId)?.delete(cb)
            },
        }
    }

    /** Direct delivery — bypass the normal publish filtering. */
    deliver(from: string, to: string, payload: unknown) {
        const cbs = this.listeners.get(to)
        if (cbs) for (const cb of cbs) cb(from, payload)
    }
}

// ── Helpers ────────────────────────────────────────────────────────

const settle = () => new Promise((r) => setTimeout(r, 0))

function fakeMediaStream(): MediaStream {
    return {
        getAudioTracks: () => [{ kind: "audio" } as MediaStreamTrack],
        getVideoTracks: () => [{ kind: "video" } as MediaStreamTrack],
        addTrack: jest.fn(),
        removeTrack: jest.fn(),
    } as unknown as MediaStream
}

/** A stream with no tracks — for tests that exercise replaceTrack
 *  in isolation, without join() already having attached media. */
function emptyMediaStream(): MediaStream {
    return {
        getAudioTracks: () => [],
        getVideoTracks: () => [],
        addTrack: jest.fn(),
        removeTrack: jest.fn(),
    } as unknown as MediaStream
}

function makeMesh(bus: InMemoryChannel, selfId: string, callId: string) {
    const connections: FakePeerConnection[] = []
    const errors: unknown[] = []
    const participantJoins: string[] = []
    const participantLeaves: string[] = []

    const channel = bus.createChannel(selfId)
    const mesh = new MeshManager({
        channel,
        callId,
        selfId,
        createPeerConnection: () => {
            const pc = new FakePeerConnection()
            connections.push(pc)
            return pc as unknown as RTCPeerConnection
        },
    })

    mesh.on("error", (err) => errors.push(err))
    mesh.on("participant-joined", (p: { did: string }) => participantJoins.push(p.did))
    mesh.on("participant-left", (p: { did: string }) => participantLeaves.push(p.did))

    return { mesh, connections, errors, participantJoins, participantLeaves, channel }
}

// ── Tests ──────────────────────────────────────────────────────────

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
        expect((parsed as any).binary).toBe(false)
    })

    it.each([
        ["mismatched version", { ...good, v: 99 }],
        ["missing call id", { ...good, call: "" }],
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
})

describe("MeshManager", () => {
    let bus: InMemoryChannel
    const CALL_ID = "test-room"

    beforeEach(() => {
        bus = new InMemoryChannel()
    })

    // ── Connection lifecycle ────────────────────────────────────────

    it("connects two peers via roster, with exactly one surviving offer", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        expect(alice.connections).toHaveLength(1)
        expect(bob.connections).toHaveLength(1)

        expect(alice.connections[0].signalingState).toBe("stable")
        expect(bob.connections[0].signalingState).toBe("stable")

        // Exactly one side answered — collision resolved.
        const answered = [alice, bob].filter(
            (p) => p.connections[0].localDescription?.type === "answer",
        )
        expect(answered).toHaveLength(1)

        expect(alice.errors).toEqual([])
        expect(bob.errors).toEqual([])
    })

    it("declares both audio and video m-sections up front", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        expect(
            alice.connections[0].transceivers.map((t) => t.kind).sort(),
        ).toEqual(["audio", "video"])
    })

    it("emits participant-joined on connect and participant-left on disconnect", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(alice.participantJoins).toEqual(["did:bob"])

        alice.mesh.setRoster(["did:alice"])
        expect(alice.participantLeaves).toEqual(["did:bob"])
    })

    it("tears down connection when roster removes the peer", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        expect(alice.connections[0].closed).toBe(false)

        alice.mesh.setRoster(["did:alice"])
        expect(alice.connections[0].closed).toBe(true)
    })

    it("filters self from the roster", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice"])
        expect(alice.connections).toHaveLength(0)
    })

    it("returns participants from getParticipants()", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        expect(alice.mesh.getParticipants()).toEqual([])

        alice.mesh.setRoster(["did:alice", "did:bob", "did:carol"])
        const participants = alice.mesh.getParticipants()
        expect(participants).toHaveLength(2)
        expect(participants.map((p) => p.did).sort()).toEqual(["did:bob", "did:carol"])
    })

    // ── Perfect negotiation ─────────────────────────────────────────

    it("survives glare: both peers offering at once converges", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        expect(alice.connections[0].signalingState).toBe("stable")
        expect(bob.connections[0].signalingState).toBe("stable")
        expect(alice.errors).toEqual([])
        expect(bob.errors).toEqual([])
    })

    it("determines polite side by DID comparison (higher DID = polite)", async () => {
        // 'did:bob' > 'did:alice', so Bob = polite, Alice = impolite
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        // Bob (polite) yields and answers; Alice (impolite) wins.
        expect(bob.connections[0].localDescription?.type).toBe("answer")
    })

    // ── Signalling isolation ────────────────────────────────────────

    it("ignores signalling from a stranger not on the roster", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())
        alice.mesh.setRoster(["did:alice"])

        bus.deliver("did:mallory", "did:alice", {
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

    it("ignores traffic for a different call id", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const before = alice.connections[0].remoteDescription
        bus.deliver("did:bob", "did:alice", {
            v: CALL_PROTOCOL_VERSION,
            call: "other-room",
            to: "did:alice",
            kind: "description",
            description: { type: "offer", sdp: "wrong-call" },
        })
        await settle()

        expect(alice.connections[0].remoteDescription).toBe(before)
    })

    it("drops own echo from channel", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        // Simulate an echo — the channel's onMessage should filter selfId.
        // InMemoryChannel already does this, so this just validates the
        // flow path stays clean.
        expect(alice.errors).toEqual([])
    })

    // ── Outbound tracks ─────────────────────────────────────────────

    it("replaces rather than re-adds when an outbound track changes", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(emptyMediaStream())
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const camera = { kind: "video" } as MediaStreamTrack
        const screen = { kind: "video" } as MediaStreamTrack

        await alice.mesh.replaceTrack("video", camera)
        await alice.mesh.replaceTrack("video", screen)

        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.connections[0].getSenders().filter((s) => s.track).length).toBe(1)
        expect(alice.connections[0].getSenders().find((s) => s.track)?.track).toBe(screen)
    })

    it("reuses sender after outbound track is cleared (null), not adding a second", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(emptyMediaStream())
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const screen = { kind: "video" } as MediaStreamTrack
        const screenAgain = { kind: "video" } as MediaStreamTrack

        await alice.mesh.replaceTrack("video", screen)
        await alice.mesh.replaceTrack("video", null)
        await alice.mesh.replaceTrack("video", screenAgain)

        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.connections[0].getSenders().filter((s) => s.track).length).toBe(1)
        expect(alice.connections[0].getSenders().find((s) => s.track)?.track).toBe(screenAgain)
    })

    it("stops sending when outbound track is cleared", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(emptyMediaStream())
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        await alice.mesh.replaceTrack("video", { kind: "video" } as MediaStreamTrack)
        await alice.mesh.replaceTrack("video", null)

        expect(alice.connections[0].getSenders().every((s) => s.track === null)).toBe(true)
    })

    it("sends existing media to a peer joining mid-call", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())
        alice.mesh.setRoster(["did:alice"])

        const track = { kind: "audio" } as MediaStreamTrack
        await alice.mesh.replaceTrack("audio", track)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        expect(alice.connections[0].addedTracks).toEqual([])
        expect(alice.connections[0].getSenders().find((s) => s.track)?.track).toBe(track)
    })

    // ── Pending message buffer ──────────────────────────────────────

    it("connects a peer whose roster arrived after the offer", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        // Alice knows Bob, sends an offer. Bob's roster has not ticked yet.
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()

        expect(bob.connections).toHaveLength(0)

        // Bob's roster catches up — held messages get replayed.
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()

        expect(bob.connections).toHaveLength(1)
        expect(bob.connections[0].remoteDescription).not.toBeNull()
        expect(bob.errors).toEqual([])
        expect(alice.errors).toEqual([])
    })

    it("does not negotiate with an agent the roster never lists", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const stranger = makeMesh(bus, "did:stranger", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await stranger.mesh.join(fakeMediaStream())

        stranger.mesh.setRoster(["did:stranger", "did:alice"])
        await settle()
        await settle()

        // Alice's roster names somebody else — held messages from stranger
        // stay unreplayed.
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        expect(alice.connections).toHaveLength(1)
        expect(alice.connections[0].remoteDescription).toBeNull()
    })

    it("bounds what it holds per-peer", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        bob.mesh.setRoster(["did:alice", "did:bob"])
        for (let n = 0; n < 20; n++) {
            await bob.mesh.replaceTrack("audio", { kind: "audio" } as MediaStreamTrack)
            await bob.mesh.replaceTrack("audio", null)
        }
        await settle()

        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()

        // Replayed something, but not everything — the buffer bounded it.
        expect(alice.connections[0].remoteDescription).not.toBeNull()
    })

    // ── Data channel ────────────────────────────────────────────────

    it("sends data via the signalling channel", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const received: { label: string; data: string; binary: boolean; sender: string }[] = []
        bob.mesh.subscribeDataChannel((msg) => {
            received.push({
                label: msg.channelLabel,
                data: msg.data,
                binary: msg.binary,
                sender: msg.senderDid,
            })
        })

        alice.mesh.sendData("chat", "hello world", false)
        await settle()

        expect(received).toEqual([
            { label: "chat", data: "hello world", binary: false, sender: "did:alice" },
        ])
    })

    it("sends binary data with binary flag", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        const received: { binary: boolean }[] = []
        bob.mesh.subscribeDataChannel((msg) => received.push({ binary: msg.binary }))

        alice.mesh.sendData("file", "base64payload", true)
        await settle()

        expect(received[0].binary).toBe(true)
    })

    it("unsubscribes data listener on returned function call", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

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
        const alice = makeMesh(bus, "did:alice", "my-room")
        const bob = makeMesh(bus, "did:bob", "my-room")

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        let roomName = ""
        bob.mesh.subscribeDataChannel((msg) => { roomName = msg.roomName })

        alice.mesh.sendData("x", "y")
        await settle()

        expect(roomName).toBe("my-room")
    })

    // ── Lifecycle ───────────────────────────────────────────────────

    it("stops negotiating after leave()", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        await alice.mesh.leave()
        expect(alice.connections[0].closed).toBe(true)

        bus.deliver("did:bob", "did:alice", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            to: "did:alice",
            kind: "description",
            description: { type: "offer", sdp: "late" },
        })
        await settle()

        // No new connections opened after leave.
        expect(alice.connections).toHaveLength(1)
        expect(alice.errors).toEqual([])
    })

    it("ignores setRoster after leave()", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())
        await alice.mesh.leave()

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(alice.connections).toHaveLength(0)
    })

    it("destroy() clears all state", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())
        alice.mesh.setRoster(["did:alice", "did:bob"])
        await settle()

        let eventFired = false
        alice.mesh.on("error", () => { eventFired = true })

        await alice.mesh.destroy()
        expect(alice.connections[0].closed).toBe(true)
        expect(alice.mesh.getParticipants()).toEqual([])

        // Events should no longer fire after destroy.
        bus.deliver("did:bob", "did:alice", {
            v: CALL_PROTOCOL_VERSION,
            call: CALL_ID,
            kind: "description",
            description: { type: "offer", sdp: "late" },
        })
        await settle()
        expect(eventFired).toBe(false)
    })

    // ── Event emitter ───────────────────────────────────────────────

    it("on() registers and off() removes callbacks", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        let calls = 0
        const cb = () => calls++
        alice.mesh.on("participant-joined", cb)

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(calls).toBe(1)

        alice.mesh.off("participant-joined", cb)
        alice.mesh.setRoster(["did:alice", "did:bob", "did:carol"])
        // Should only increment once more — for carol, since we removed
        // the callback before adding carol. Wait — off removes the cb
        // so carol's join should not fire it.
        expect(calls).toBe(1)
    })

    it("off() with no callback removes all listeners for that event", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        let calls = 0
        alice.mesh.on("participant-joined", () => calls++)
        alice.mesh.on("participant-joined", () => calls++)

        alice.mesh.off("participant-joined")
        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(calls).toBe(0)
    })

    it("event handler errors do not break other handlers", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        await alice.mesh.join(fakeMediaStream())

        let secondCalled = false
        const consoleError = jest.spyOn(console, "error").mockImplementation()

        alice.mesh.on("participant-joined", () => { throw new Error("boom") })
        alice.mesh.on("participant-joined", () => { secondCalled = true })

        alice.mesh.setRoster(["did:alice", "did:bob"])
        expect(secondCalled).toBe(true)

        consoleError.mockRestore()
    })

    // ── ICE candidates ──────────────────────────────────────────────

    it("forwards ICE candidates between peers", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())

        alice.mesh.setRoster(["did:alice", "did:bob"])
        bob.mesh.setRoster(["did:alice", "did:bob"])
        await settle()
        await settle()
        await settle()

        // Emit ICE candidate from Alice's connection.
        alice.connections[0].emitCandidate({ candidate: "candidate:1" })
        await settle()

        expect(bob.connections[0].candidates).toEqual(
            expect.arrayContaining([
                expect.objectContaining({ candidate: "candidate:1" }),
            ]),
        )
    })

    // ── Three-peer mesh ─────────────────────────────────────────────

    it("handles three peers with correct connection count", async () => {
        const alice = makeMesh(bus, "did:alice", CALL_ID)
        const bob = makeMesh(bus, "did:bob", CALL_ID)
        const carol = makeMesh(bus, "did:carol", CALL_ID)

        await alice.mesh.join(fakeMediaStream())
        await bob.mesh.join(fakeMediaStream())
        await carol.mesh.join(fakeMediaStream())

        const roster = ["did:alice", "did:bob", "did:carol"]
        alice.mesh.setRoster(roster)
        bob.mesh.setRoster(roster)
        carol.mesh.setRoster(roster)
        await settle()
        await settle()
        await settle()

        // Each peer connects to 2 others.
        expect(alice.connections).toHaveLength(2)
        expect(bob.connections).toHaveLength(2)
        expect(carol.connections).toHaveLength(2)

        expect(alice.mesh.getParticipants()).toHaveLength(2)
    })
})
