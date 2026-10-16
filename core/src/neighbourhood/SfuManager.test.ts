/**
 * SfuManager unit tests: the client's contract with the executor's SFU.
 *
 * Server events: the executor sends a socket only the events it watches,
 * and the SDK sends a pending watch ahead of the next call.  So the manager
 * must subscribe before `sfuCallJoin`, or an event the join triggers can be
 * lost.  Offers that arrive before the join's answer is applied wait for
 * it, then apply one at a time in arrival order.
 *
 * Membership comes from the room roster.  A received track belongs to the
 * participant the server mapped its media section to, and to no one when
 * its section has no mapping.
 */

class FakeTrack {
    private listeners: (() => void)[] = []
    constructor(readonly kind: "audio" | "video") {}
    addEventListener(type: string, listener: () => void) {
        if (type === "ended") this.listeners.push(listener)
    }
    end() {
        for (const listener of this.listeners) listener()
    }
}

class FakeMediaStream {
    private tracks: FakeTrack[]
    constructor(tracks: FakeTrack[] = []) {
        this.tracks = [...tracks]
    }
    getTracks() { return [...this.tracks] }
    getAudioTracks() { return this.tracks.filter((t) => t.kind === "audio") }
    getVideoTracks() { return this.tracks.filter((t) => t.kind === "video") }
    addTrack(track: FakeTrack) {
        if (!this.tracks.includes(track)) this.tracks.push(track)
    }
    removeTrack(track: FakeTrack) {
        this.tracks = this.tracks.filter((t) => t !== track)
    }
}

interface FakeTransceiver {
    trackOrKind: FakeTrack | string
    init: RTCRtpTransceiverInit
    sender: { replaceTrack: jest.Mock<Promise<void>, [FakeTrack | null]> }
}

class FakePeerConnection {
    static instances: FakePeerConnection[] = []
    localDescription: RTCSessionDescriptionInit | null = null
    remoteDescriptions: RTCSessionDescriptionInit[] = []
    transceivers: FakeTransceiver[] = []
    offersCreated = 0
    oniceconnectionstatechange: (() => void) | null = null
    onicecandidate: ((e: { candidate: null }) => void) | null = null
    ontrack: ((e: unknown) => void) | null = null
    closed = false

    constructor(readonly configuration?: RTCConfiguration) {
        FakePeerConnection.instances.push(this)
    }
    addTransceiver(trackOrKind: FakeTrack | string, init: RTCRtpTransceiverInit) {
        const transceiver: FakeTransceiver = {
            trackOrKind,
            init,
            sender: { replaceTrack: jest.fn(async (_track: FakeTrack | null) => {}) },
        }
        this.transceivers.push(transceiver)
        return transceiver
    }
    async createOffer() {
        this.offersCreated++
        return { type: "offer", sdp: "local-offer" }
    }
    async createAnswer() { return { type: "answer", sdp: "local-answer" } }
    async setLocalDescription(d: RTCSessionDescriptionInit) { this.localDescription = d }
    async setRemoteDescription(d: RTCSessionDescriptionInit) { this.remoteDescriptions.push(d) }
    close() { this.closed = true }

    /** A remote track arriving on the media section `mid`. */
    receive(track: FakeTrack, mid: string | null) {
        this.ontrack?.({ track, transceiver: { mid } })
    }
    /** The SDPs of the remote descriptions applied so far. */
    remoteSdps() {
        return this.remoteDescriptions.map((d) => d.sdp)
    }
}
;(globalThis as any).RTCPeerConnection = FakePeerConnection
;(globalThis as any).MediaStream = FakeMediaStream
;(globalThis as any).RTCSessionDescription = class {
    constructor(init: RTCSessionDescriptionInit) { Object.assign(this, init) }
}

import { SfuManager, type SfuNeighbourhoodApi, type SfuParticipantState } from "./SfuManager"
import type {
    CallSessionInfo,
    SfuCallRenegotiationOffer,
    SfuConfig,
    SfuRoomRoster,
    TrackMapEntry,
} from "./SfuTypes"

const URL = "neighbourhood://n"
const ROOM = "room"
const ME = "did:me"
const ALICE = "did:key:alice"
const BOB = "did:key:bob"

function fakeStream(...tracks: FakeTrack[]): MediaStream {
    return new FakeMediaStream(tracks) as unknown as MediaStream
}

function session(overrides: Partial<CallSessionInfo> = {}): CallSessionInfo {
    return {
        roomName: ROOM,
        neighbourhoodUrl: URL,
        participantId: "p1",
        sdpAnswer: JSON.stringify({ type: "answer", sdp: "server-answer" }),
        streamMapping: [],
        ...overrides,
    }
}

function offer(sdp: string, trackMapping?: TrackMapEntry[]): SfuCallRenegotiationOffer {
    return {
        targetDid: ME,
        neighbourhoodUrl: URL,
        roomName: ROOM,
        sdpOffer: JSON.stringify({ type: "offer", sdp }),
        trackMapping,
    }
}

function mapped(mid: string, agentDid: string, mediaKind: "audio" | "video"): TrackMapEntry {
    return { mid, agentDid, mediaKind }
}

function roster(participants: string[], overrides: Partial<SfuRoomRoster> = {}): SfuRoomRoster {
    return { neighbourhoodUrl: URL, roomName: ROOM, participants, ...overrides }
}

function deferred<T>() {
    let resolve!: (value: T) => void
    const promise = new Promise<T>((r) => { resolve = r })
    return { promise, resolve }
}

/** A fake API that records the order of subscribe and RPC calls. */
function fakeApi() {
    const log: string[] = []
    const offerHandlers = new Set<(e: SfuCallRenegotiationOffer) => void>()
    const rosterHandlers = new Set<(r: SfuRoomRoster) => void>()
    const api = {
        log,
        offerHandlers,
        rosterHandlers,
        emitOffer(event: SfuCallRenegotiationOffer) {
            for (const handler of offerHandlers) handler(event)
        },
        emitRoster(event: SfuRoomRoster) {
            for (const handler of rosterHandlers) handler(event)
        },
        sfuCallJoin: jest.fn(async () => {
            log.push("sfuCallJoin")
            return session()
        }),
        sfuCallLeave: jest.fn(async () => {
            log.push("sfuCallLeave")
            return true
        }),
        sfuCallSetQualityPreference: jest.fn(async () => true),
        sfuCallAnswerServerOffer: jest.fn(async () => {
            log.push("sfuCallAnswerServerOffer")
            return true
        }),
        subscribeSfuCallRenegotiationOffer: jest.fn(
            (_did: string, handler: (e: SfuCallRenegotiationOffer) => void) => {
                log.push("subscribe-offer")
                offerHandlers.add(handler)
                return () => {
                    log.push("unsubscribe-offer")
                    offerHandlers.delete(handler)
                }
            },
        ),
        subscribeSfuRoomRoster: jest.fn((handler: (r: SfuRoomRoster) => void) => {
            log.push("subscribe-roster")
            rosterHandlers.add(handler)
            return () => {
                log.push("unsubscribe-roster")
                rosterHandlers.delete(handler)
            }
        }),
        sfuAddIceCandidate: jest.fn(async () => true),
        sfuSendData: jest.fn(async () => true),
        subscribeSfuDataChannel: jest.fn(() => () => {}),
    }
    return api satisfies SfuNeighbourhoodApi & Record<string, unknown>
}

/** A manager with its membership and media events recorded. */
function recordedManager(api: ReturnType<typeof fakeApi>) {
    const manager = new SfuManager(api, ROOM, ME, URL)
    const joined: string[] = []
    const left: string[] = []
    const added: [MediaStream, FakeTrack][] = []
    const removed: [MediaStream, FakeTrack][] = []
    manager.on("participant-joined", (p: SfuParticipantState) => joined.push(p.did))
    manager.on("participant-left", (p: SfuParticipantState) => left.push(p.did))
    manager.on("stream-added", (s: MediaStream, t: FakeTrack) => added.push([s, t]))
    manager.on("stream-removed", (s: MediaStream, t: FakeTrack) => removed.push([s, t]))
    const participants = () => manager.getParticipants().map((p) => p.did)
    return { manager, joined, left, added, removed, participants }
}

// setImmediate, which the fake-timer tests leave real, so a flush works under both.
const flush = () => new Promise((resolve) => setImmediate(resolve))
const currentPc = () => FakePeerConnection.instances[FakePeerConnection.instances.length - 1]

/** Drive the manager's current connection to an ICE state. */
async function iceState(state: string) {
    const pc = currentPc() as any
    pc.iceConnectionState = state
    pc.oniceconnectionstatechange()
    await flush()
}

beforeEach(() => {
    FakePeerConnection.instances = []
})

// ── Server events ─────────────────────────────────────────────────

test("the app's ICE servers reach the connection as given, ahead of the config's", async () => {
    const relay = {
        urls: ["stun:relay.example.com:3478", "turn:relay.example.com:3478"],
        username: "user",
        credential: "pass",
    }
    const config = { iceServers: [{ urls: ["stun:config.example.com:3478"] }] } as unknown as SfuConfig
    await new SfuManager(fakeApi(), ROOM, ME, URL, { servers: [relay] }, config).join(fakeStream())
    expect(FakePeerConnection.instances[0].configuration).toEqual({ iceServers: [relay] })
})

test("subscribes to offers and the roster before sfuCallJoin", async () => {
    const api = fakeApi()
    await new SfuManager(api, ROOM, ME, URL).join(fakeStream())
    expect(api.log.slice(0, 3)).toEqual(["subscribe-offer", "subscribe-roster", "sfuCallJoin"])
})

test("offers that arrive before the join completes apply after its answer, in arrival order", async () => {
    const api = fakeApi()
    const join = deferred<CallSessionInfo>()
    api.sfuCallJoin.mockImplementation(() => join.promise)
    const manager = new SfuManager(api, ROOM, ME, URL)
    const joining = manager.join(fakeStream())
    await flush()

    api.emitOffer(offer("early-1"))
    api.emitOffer(offer("early-2"))
    await flush()
    const pc = currentPc()
    expect(pc.remoteSdps()).toEqual([])
    expect(api.sfuCallAnswerServerOffer).not.toHaveBeenCalled()

    join.resolve(session())
    await joining
    await flush()
    expect(pc.remoteSdps()).toEqual(["server-answer", "early-1", "early-2"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledWith(
        URL,
        ROOM,
        JSON.stringify({ type: "answer", sdp: "local-answer" }),
    )
})

test("a second offer waits until the first one is answered", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const answered = deferred<boolean>()
    api.sfuCallAnswerServerOffer.mockImplementationOnce(() => answered.promise)

    api.emitOffer(offer("first"))
    api.emitOffer(offer("second"))
    await flush()
    const pc = currentPc()
    expect(pc.remoteSdps()).toEqual(["server-answer", "first"])

    answered.resolve(true)
    await flush()
    expect(pc.remoteSdps()).toEqual(["server-answer", "first", "second"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
})

test("an offer queued for a connection that has since been replaced is dropped", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const old = currentPc()
    const answered = deferred<boolean>()
    api.sfuCallAnswerServerOffer.mockImplementationOnce(() => answered.promise)
    api.emitOffer(offer("first"))
    api.emitOffer(offer("stale"))
    await flush()

    await iceState("failed")
    const replacement = currentPc()
    expect(replacement).not.toBe(old)
    answered.resolve(true)
    await flush()

    expect(old.remoteSdps()).toEqual(["server-answer", "first"])
    expect(replacement.remoteSdps()).toEqual(["server-answer"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(1)
})

test("a failed join drops its subscriptions", async () => {
    const api = fakeApi()
    api.sfuCallJoin.mockRejectedValue(new Error("Not a member of this neighbourhood"))
    await expect(new SfuManager(api, ROOM, ME, URL).join(fakeStream())).rejects.toThrow("Not a member")
    expect(api.offerHandlers.size).toBe(0)
    expect(api.rosterHandlers.size).toBe(0)
    expect(api.log).toEqual(expect.arrayContaining(["unsubscribe-offer", "unsubscribe-roster"]))
})

test("a rejoin subscribes again before it releases the old subscriptions", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.log.length = 0
    await manager.join(fakeStream())
    expect(api.log).toEqual([
        "subscribe-offer",
        "subscribe-roster",
        "unsubscribe-offer",
        "unsubscribe-roster",
        "sfuCallJoin",
    ])
    expect(api.offerHandlers.size).toBe(1)
    expect(api.rosterHandlers.size).toBe(1)
})

test("leave releases both subscriptions and ignores a roster delivered after it", async () => {
    const api = fakeApi()
    const { manager, joined, participants } = recordedManager(api)
    await manager.join(fakeStream())
    const lateRoster = [...api.rosterHandlers]

    await manager.leave()
    expect(api.offerHandlers.size).toBe(0)
    expect(api.rosterHandlers.size).toBe(0)

    // An event already in flight when the subscription went away.
    for (const handler of lateRoster) handler(roster([ALICE]))
    await flush()
    expect(joined).toEqual([])
    expect(participants()).toEqual([])
})

// ── Membership ────────────────────────────────────────────────────

test("a roster names participants before any media arrives, never the agent itself", async () => {
    const api = fakeApi()
    const { manager, joined, participants } = recordedManager(api)
    await manager.join(fakeStream())

    api.emitRoster(roster([ALICE, ME]))
    await flush()

    expect(joined).toEqual([ALICE])
    expect(participants()).toEqual([ALICE])
    expect(manager.getParticipants()[0].stream.getTracks()).toEqual([])
})

test("a participant missing from a later roster leaves, and their media sections stop attributing", async () => {
    const api = fakeApi()
    const { manager, joined, left, added, participants } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitOffer(offer("alice", [mapped("0", ALICE, "audio")]))
    api.emitRoster(roster([ALICE, BOB]))
    await flush()

    api.emitRoster(roster([BOB]))
    await flush()
    expect(left).toEqual([ALICE])
    expect(participants()).toEqual([BOB])

    currentPc().receive(new FakeTrack("audio"), "0")
    expect(joined).toEqual([ALICE, BOB])
    expect(added).toEqual([])
    expect(participants()).toEqual([BOB])
})

test("a roster that arrives before the join completes applies after it", async () => {
    const api = fakeApi()
    const join = deferred<CallSessionInfo>()
    api.sfuCallJoin.mockImplementation(() => join.promise)
    const { manager, joined } = recordedManager(api)
    const joining = manager.join(fakeStream())
    await flush()

    api.emitRoster(roster([ALICE]))
    await flush()
    expect(joined).toEqual([])

    join.resolve(session())
    await joining
    await flush()
    expect(joined).toEqual([ALICE])
})

test("a roster for another room is ignored", async () => {
    const api = fakeApi()
    const { manager, joined } = recordedManager(api)
    await manager.join(fakeStream())

    api.emitRoster(roster([ALICE], { roomName: "other" }))
    api.emitRoster(roster([BOB], { neighbourhoodUrl: "neighbourhood://other" }))
    await flush()

    expect(joined).toEqual([])
})

test("the join's stream mapping seeds the participants", async () => {
    const api = fakeApi()
    api.sfuCallJoin.mockResolvedValueOnce(session({ streamMapping: [`p2:${ALICE}`, `p3:${BOB}`] }))
    const { manager, joined, participants } = recordedManager(api)
    await manager.join(fakeStream())

    expect(joined).toEqual([ALICE, BOB])
    expect(participants()).toEqual([ALICE, BOB])
})

// ── Received media ────────────────────────────────────────────────

test("a track on an unmapped media section is ignored", async () => {
    const api = fakeApi()
    const { manager, joined, added, participants } = recordedManager(api)
    await manager.join(fakeStream())

    currentPc().receive(new FakeTrack("audio"), "7")
    currentPc().receive(new FakeTrack("video"), null)

    expect(joined).toEqual([])
    expect(added).toEqual([])
    expect(participants()).toEqual([])
})

test("two tracks of one participant land in one participant and one stream", async () => {
    const api = fakeApi()
    const { manager, joined, added } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitOffer(offer("alice", [mapped("0", ALICE, "audio"), mapped("1", ALICE, "video")]))
    await flush()

    const audio = new FakeTrack("audio")
    const video = new FakeTrack("video")
    currentPc().receive(audio, "0")
    currentPc().receive(video, "1")

    expect(joined).toEqual([ALICE])
    const [alice] = manager.getParticipants()
    expect(alice.stream.getTracks()).toEqual([audio, video])
    expect(alice.hasAudio).toBe(true)
    expect(alice.hasVideo).toBe(true)
    expect(added).toEqual([[alice.stream, audio], [alice.stream, video]])
})

test("a track that ends leaves its participant's stream", async () => {
    const api = fakeApi()
    const { manager, removed } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitOffer(offer("alice", [mapped("0", ALICE, "audio"), mapped("1", ALICE, "video")]))
    await flush()
    const audio = new FakeTrack("audio")
    const video = new FakeTrack("video")
    currentPc().receive(audio, "0")
    currentPc().receive(video, "1")

    video.end()

    const [alice] = manager.getParticipants()
    expect(alice.stream.getTracks()).toEqual([audio])
    expect(removed).toEqual([[alice.stream, video]])
})

// ── Local media ───────────────────────────────────────────────────

test("join declares audio and video send-only, video as three simulcast layers", async () => {
    const api = fakeApi()
    const mic = new FakeTrack("audio")
    const stream = fakeStream(mic)
    await new SfuManager(api, ROOM, ME, URL).join(stream)

    // No video track yet: the section is declared by kind.
    expect(currentPc().transceivers.map((t) => [t.trackOrKind, t.init])).toEqual([
        [mic, { direction: "sendonly", streams: [stream] }],
        [
            "video",
            {
                direction: "sendonly",
                streams: [stream],
                sendEncodings: [
                    expect.objectContaining({ rid: "high" }),
                    expect.objectContaining({ rid: "medium" }),
                    expect.objectContaining({ rid: "low" }),
                ],
            },
        ],
    ])
})

test("replaceTrack uses the sender declared at join, even for a kind the join had no track of", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream(new FakeTrack("audio")))
    const pc = currentPc()
    const [audioSender, videoSender] = pc.transceivers.map((t) => t.sender)

    const camera = new FakeTrack("video")
    await manager.replaceTrack("video", camera as unknown as MediaStreamTrack)
    await manager.replaceTrack("audio", null)

    expect(videoSender.replaceTrack.mock.calls).toEqual([[camera]])
    expect(audioSender.replaceTrack.mock.calls).toEqual([[null]])
    // No new section and no renegotiation.
    expect(pc.transceivers).toHaveLength(2)
    expect(pc.offersCreated).toBe(1)
})

// ── Reconnecting ──────────────────────────────────────────────────

test("a failed connection leaves, then joins this executor again", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.log.length = 0

    const failed = currentPc()
    await iceState("failed")

    expect(failed.closed).toBe(true)
    expect(api.log).toEqual([
        "unsubscribe-offer",
        "unsubscribe-roster",
        "sfuCallLeave",
        "subscribe-offer",
        "subscribe-roster",
        "sfuCallJoin",
    ])
    expect(api.sfuCallJoin).toHaveBeenLastCalledWith(URL, ROOM, expect.any(String))
    expect(FakePeerConnection.instances).toHaveLength(2)
})

test("a rejoin the executor cannot take yet is retried, and succeeds once it can", async () => {
    // A restart or an outage: the executor refuses for a while, then is back.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        const errors: unknown[] = []
        manager.on("error", (e) => errors.push(e))
        await manager.join(fakeStream())
        api.sfuCallJoin.mockRejectedValueOnce(new Error("unreachable")).mockRejectedValueOnce(new Error("unreachable"))

        await iceState("failed")
        for (let i = 0; i < 4; i++) {
            await jest.advanceTimersByTimeAsync(5_000)
        }

        expect(api.sfuCallJoin).toHaveBeenCalledTimes(4)
        expect(errors).toEqual([])
        expect(currentPc().closed).toBe(false)
    } finally {
        jest.useRealTimers()
    }
})

test("a rejoin that keeps failing reports the error after the window, its connections closed", async () => {
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        const errors: unknown[] = []
        manager.on("error", (e) => errors.push(e))
        await manager.join(fakeStream())
        api.sfuCallJoin.mockRejectedValue(new Error("Room is full"))

        await iceState("failed")
        await jest.advanceTimersByTimeAsync(130_000)

        expect(errors).toEqual([new Error("Room is full")])
        expect(FakePeerConnection.instances.every((pc) => pc.closed)).toBe(true)
    } finally {
        jest.useRealTimers()
    }
})

test("joins that succeed while ICE keeps failing stop after three reconnects", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    const errors: unknown[] = []
    manager.on("error", (e) => errors.push(e))
    await manager.join(fakeStream())

    for (let i = 0; i < 4; i++) await iceState("failed")

    expect(api.sfuCallLeave).toHaveBeenCalledTimes(3)
    expect(errors).toEqual([new Error("SFU reconnect attempts exhausted")])
})

test("a connection that comes up resets the reconnect count", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    const errors: unknown[] = []
    manager.on("error", (e) => errors.push(e))
    await manager.join(fakeStream())

    for (let i = 0; i < 6; i++) {
        await iceState("failed")
        await iceState("connected")
    }

    expect(api.sfuCallLeave).toHaveBeenCalledTimes(6)
    expect(errors).toEqual([])
})

test("a rebuilt connection sends the tracks replaceTrack set and restores the quality preference", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    const screen = new FakeTrack("video")
    await manager.join(fakeStream(new FakeTrack("audio"), new FakeTrack("video")))
    await manager.replaceTrack("video", screen as unknown as MediaStreamTrack)
    await manager.replaceTrack("audio", null)
    await manager.setQualityPreference("low")

    await iceState("failed")

    expect(currentPc().transceivers.map((t) => t.trackOrKind)).toEqual(["audio", screen])
    expect(api.sfuCallSetQualityPreference.mock.calls).toEqual([
        [URL, ROOM, "low"],
        [URL, ROOM, "low"],
    ])
})

test("leave forgets replaced tracks, so the next join sends its own stream", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    await manager.replaceTrack("video", new FakeTrack("video") as unknown as MediaStreamTrack)
    await manager.leave()

    const camera = new FakeTrack("video")
    await manager.join(fakeStream(camera))

    expect(currentPc().transceivers.map((t) => t.trackOrKind)).toEqual(["audio", camera])
})

test("leaving during a reconnect stops it before it joins again", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    let releaseLeave!: () => void
    api.sfuCallLeave.mockImplementationOnce(
        () => new Promise((resolve) => { releaseLeave = () => resolve(true) }),
    )

    await iceState("failed")
    await manager.leave()
    releaseLeave()
    await flush()

    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
})
