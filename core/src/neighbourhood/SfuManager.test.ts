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
 *
 * A participant is a session, not an agent: the same agent on another device
 * is someone else in the call. Every call about this session names the
 * participant id its join returned, and a manager that never joined sends
 * nothing.
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

import { defaultDeviceId, SfuManager, type SfuNeighbourhoodApi, type SfuParticipantState } from "./SfuManager"
import type {
    CallSessionInfo,
    SfuCallRenegotiationOffer,
    SfuConfig,
    SfuDataMessage,
    SfuRoomRoster,
    SfuSessionReplaced,
    TrackMapEntry,
} from "./SfuTypes"

const URL = "neighbourhood://n"
const ROOM = "room"
const ME = "did:me"
const ALICE = "did:key:alice"
const BOB = "did:key:bob"
/** The participant id the executor gives this manager's join. */
const MINE = "P1"
const DEVICE = "device-1"

/** The session of `did` in the room: this manager's own for ME, one per other agent otherwise. */
function pid(did: string): string {
    return did === ME ? MINE : `P-${did}`
}

function fakeStream(...tracks: FakeTrack[]): MediaStream {
    return new FakeMediaStream(tracks) as unknown as MediaStream
}

function session(overrides: Partial<CallSessionInfo> = {}): CallSessionInfo {
    return {
        roomName: ROOM,
        neighbourhoodUrl: URL,
        participantId: MINE,
        sdpAnswer: JSON.stringify({ type: "answer", sdp: "server-answer" }),
        streamMapping: [],
        ...overrides,
    }
}

function offer(sdp: string, trackMapping?: TrackMapEntry[], participantId = MINE): SfuCallRenegotiationOffer {
    return {
        targetDid: ME,
        participantId,
        neighbourhoodUrl: URL,
        roomName: ROOM,
        sdpOffer: JSON.stringify({ type: "offer", sdp }),
        trackMapping,
    }
}

function mapped(mid: string, agentDid: string, mediaKind: "audio" | "video", participantId = pid(agentDid)): TrackMapEntry {
    return { mid, participantId, agentDid, mediaKind }
}

/**
 * The room's roster as the executor publishes it: one session per DID (`pid`),
 * or the sessions given as `[participantId, did]`. This manager's session
 * (`MINE`) is in it too, unless `withoutMe`.
 */
function roster(
    participants: (string | [string, string])[],
    overrides: Partial<SfuRoomRoster> = {},
    { withoutMe = false } = {},
): SfuRoomRoster {
    const sessions = participants.map((p) =>
        typeof p === "string" ? { participantId: pid(p), agentDid: p } : { participantId: p[0], agentDid: p[1] },
    )
    const listed =
        withoutMe || sessions.some((s) => s.participantId === MINE)
            ? sessions
            : [...sessions, { participantId: MINE, agentDid: ME }]
    return { neighbourhoodUrl: URL, roomName: ROOM, participants: listed, ...overrides }
}

function replaced(participantId: string, overrides: Partial<SfuSessionReplaced> = {}): SfuSessionReplaced {
    return { targetDid: ME, neighbourhoodUrl: URL, roomName: ROOM, participantId, ...overrides }
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
    const replacedHandlers = new Set<(r: SfuSessionReplaced) => void>()
    const api = {
        log,
        offerHandlers,
        rosterHandlers,
        replacedHandlers,
        emitReplaced(event: SfuSessionReplaced) {
            for (const handler of replacedHandlers) handler(event)
        },
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
        subscribeSfuSessionReplaced: jest.fn((handler: (r: SfuSessionReplaced) => void) => {
            replacedHandlers.add(handler)
            return () => {
                replacedHandlers.delete(handler)
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
    const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
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
        MINE,
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
    expect(api.replacedHandlers.size).toBe(0)
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
    expect(api.sfuCallJoin).toHaveBeenLastCalledWith(URL, ROOM, expect.any(String), expect.any(String))
    expect(FakePeerConnection.instances).toHaveLength(2)
})

test("a roster that leaves this session out rebuilds the call after the grace", async () => {
    // The server dropped this session; ICE never said so (E6, CI job 53158).
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        const dropped = currentPc()
        api.log.length = 0

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(2_999)
        expect(api.log).toEqual([])

        await jest.advanceTimersByTimeAsync(1)
        expect(dropped.closed).toBe(true)
        expect(api.log).toEqual([
            "unsubscribe-offer",
            "unsubscribe-roster",
            "sfuCallLeave",
            "subscribe-offer",
            "subscribe-roster",
            "sfuCallJoin",
        ])
    } finally {
        jest.useRealTimers()
    }
})

test("a roster that lists this session again within the grace cancels the rebuild", async () => {
    // A roster published just before this join landed can arrive after it.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        api.log.length = 0

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(1_000)
        api.emitRoster(roster([ALICE, ME]))
        await flush()
        await jest.advanceTimersByTimeAsync(10_000)

        expect(api.log).toEqual([])
        expect(currentPc().closed).toBe(false)
    } finally {
        jest.useRealTimers()
    }
})

test("leaving stops a pending rebuild for an unlisted session", async () => {
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()

        await manager.leave()
        await jest.advanceTimersByTimeAsync(10_000)

        expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    } finally {
        jest.useRealTimers()
    }
})

test("after a cancelled grace, a later roster without this session starts a new one", async () => {
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        api.emitRoster(roster([ALICE, ME]))
        await flush()
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    } finally {
        jest.useRealTimers()
    }
})

test("a second roster without this session keeps the first grace, and one listing it cancels both", async () => {
    // Others leaving while this session is unlisted publish more rosters without it.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE, BOB], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(1_000)
        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(1_000)
        api.emitRoster(roster([ALICE, ME]))
        await flush()
        await jest.advanceTimersByTimeAsync(10_000)

        expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
        expect(currentPc().closed).toBe(false)
    } finally {
        jest.useRealTimers()
    }
})

test("a rebuild for ICE ends the old grace, and the new connection gets its own", async () => {
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(1_000)
        await iceState("failed")
        expect(FakePeerConnection.instances).toHaveLength(2)

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(2_999)
        expect(FakePeerConnection.instances).toHaveLength(2)
        await jest.advanceTimersByTimeAsync(1)
        expect(FakePeerConnection.instances).toHaveLength(3)
    } finally {
        jest.useRealTimers()
    }
})

test("a roster that lists another device of this agent but not this session rebuilds after the grace", async () => {
    // The DID stays listed through the other device: only the participant id tells.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
        await manager.join(fakeStream())
        const dropped = currentPc()
        api.log.length = 0

        api.emitRoster(roster([ALICE, ["P2", ME]], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(2_999)
        expect(api.log).toEqual([])
        expect(manager.getParticipants().map((p) => p.participantId).sort()).toEqual(["P2", pid(ALICE)].sort())

        await jest.advanceTimersByTimeAsync(1)
        expect(dropped.closed).toBe(true)
        expect(api.sfuCallLeave).toHaveBeenCalledWith(URL, ROOM, MINE)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    } finally {
        jest.useRealTimers()
    }
})

for (const order of ["roster first", "notice first"] as const) {
    test(`a replaced session left out of the roster never rebuilds (${order})`, async () => {
        // A later tab on this device took the call: the executor sends this
        // agent the notice and a roster listing the new session, not this one.
        // Rejoining would replace the newer tab in turn.
        jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
        try {
            const api = fakeApi()
            const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
            await manager.join(fakeStream())
            const pc = currentPc()
            const newerTab = roster([ALICE, ["P2", ME]], {}, { withoutMe: true })

            if (order === "roster first") {
                api.emitRoster(newerTab)
                await flush()
                await jest.advanceTimersByTimeAsync(1_000)
                api.emitReplaced(replaced(MINE))
            } else {
                api.emitReplaced(replaced(MINE))
                api.emitRoster(newerTab)
            }
            await flush()
            await jest.advanceTimersByTimeAsync(10_000)

            expect(pc.closed).toBe(true)
            expect(manager.getState().participantId).toBeNull()
            expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
            expect(api.sfuCallLeave).not.toHaveBeenCalled()
            expect(FakePeerConnection.instances).toHaveLength(1)
        } finally {
            jest.useRealTimers()
        }
    })
}

test("the roster of a reconnect's own leave does not rebuild the rejoined call", async () => {
    // It arrives between the reconnect's leave and its join, while the manager
    // has no participant id; the rejoin's roster lists the new session.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        const join = deferred<CallSessionInfo>()
        api.sfuCallJoin.mockImplementationOnce(() => join.promise)
        await iceState("failed")
        expect(manager.getState().participantId).toBeNull()

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        join.resolve(session())
        await flush()
        api.emitRoster(roster([ALICE]))
        await flush()
        await jest.advanceTimersByTimeAsync(10_000)

        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
        expect(FakePeerConnection.instances).toHaveLength(2)
        expect(manager.getState().participantId).toBe(MINE)
    } finally {
        jest.useRealTimers()
    }
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
        [URL, ROOM, MINE, "low"],
        [URL, ROOM, MINE, "low"],
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

test("replaced while a reconnect waits between leave and join: it stops, and never joins", async () => {
    // Another tab joined just before this reconnect's leave landed: the
    // executor replaced the old session, so the leave finds nothing.
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
    const replacedEvents: unknown[] = []
    manager.on("replaced", (event: unknown) => replacedEvents.push(event))
    await manager.join(fakeStream())
    const leaving = deferred<boolean>()
    api.sfuCallLeave.mockImplementationOnce(() => leaving.promise)

    await iceState("failed")
    api.emitReplaced(replaced(MINE))
    leaving.resolve(false)
    await flush()

    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    expect(replacedEvents).toEqual([replaced(MINE)])
    expect(manager.getState().peerConnection).toBeNull()
    expect(api.replacedHandlers.size).toBe(0)
})

test("replaced while a reconnect's join is out: the session that join got is left", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
    const replacedEvents: unknown[] = []
    manager.on("replaced", (event: unknown) => replacedEvents.push(event))
    await manager.join(fakeStream())
    api.sfuCallLeave.mockResolvedValueOnce(false)
    const rejoining = deferred<CallSessionInfo>()
    api.sfuCallJoin.mockImplementationOnce(() => rejoining.promise)

    await iceState("failed")
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    api.emitReplaced(replaced(MINE))
    rejoining.resolve(session({ participantId: "P2" }))
    await flush()

    expect(replacedEvents).toEqual([replaced(MINE)])
    expect(api.sfuCallLeave.mock.calls).toEqual([
        [URL, ROOM, MINE],
        [URL, ROOM, "P2"],
    ])
    expect(manager.getState().participantId).toBeNull()
    expect(manager.getState().peerConnection).toBeNull()
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
})

test("a reconnect whose leave never landed is not stopped by its own rejoin replacing the old session", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
    const replacedEvents: unknown[] = []
    manager.on("replaced", (event: unknown) => replacedEvents.push(event))
    await manager.join(fakeStream())
    api.sfuCallLeave.mockRejectedValueOnce(new Error("socket closed"))
    const rejoining = deferred<CallSessionInfo>()
    api.sfuCallJoin.mockImplementationOnce(() => rejoining.promise)

    await iceState("failed")
    // The executor still held the old session: the rejoin, from this device, replaces it.
    api.emitReplaced(replaced(MINE))
    rejoining.resolve(session({ participantId: "P2" }))
    await flush()

    expect(replacedEvents).toEqual([])
    expect(manager.getState().participantId).toBe("P2")
    expect(currentPc().closed).toBe(false)
})

// ── One session per device ────────────────────────────────────────

test("the join sends the device id; every later call names the participant id the join returned", async () => {
    const api = fakeApi()
    const { manager } = recordedManager(api)
    const joining = manager.join(fakeStream())
    // A candidate gathered before the join returns waits for its participant id.
    ;(currentPc() as any).onicecandidate({ candidate: { candidate: "early" } })
    await joining
    ;(currentPc() as any).onicecandidate({ candidate: { candidate: "late" } })
    api.emitOffer(offer("next"))
    await flush()
    await manager.setQualityPreference("low")
    await manager.sendData("chat", "hi")
    await manager.leave()

    expect(api.sfuCallJoin).toHaveBeenCalledWith(URL, ROOM, expect.any(String), DEVICE)
    expect(api.sfuAddIceCandidate.mock.calls).toEqual([
        [URL, ROOM, MINE, "early"],
        [URL, ROOM, MINE, "late"],
    ])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledWith(URL, ROOM, MINE, expect.any(String))
    expect(api.sfuCallSetQualityPreference).toHaveBeenCalledWith(URL, ROOM, MINE, "low")
    expect(api.sfuSendData).toHaveBeenCalledWith(URL, ROOM, MINE, "chat", "hi", false)
    expect(api.sfuCallLeave.mock.calls).toEqual([[URL, ROOM, MINE]])
})

test("a manager that never joined, or whose join was refused, sends no leave", async () => {
    const api = fakeApi()
    await new SfuManager(api, ROOM, ME, URL).destroy()

    api.sfuCallJoin.mockRejectedValue(new Error("Room is full"))
    const refused = new SfuManager(api, ROOM, ME, URL)
    await expect(refused.join(fakeStream())).rejects.toThrow("Room is full")
    await refused.leave()
    await refused.destroy()

    expect(api.sfuCallLeave).not.toHaveBeenCalled()
    expect(refused.getState().participantId).toBeNull()
})

test("a join whose answer cannot be applied leaves the session it made, once", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    const original = FakePeerConnection.prototype.setRemoteDescription
    FakePeerConnection.prototype.setRemoteDescription = async () => { throw new Error("bad answer") }
    try {
        await expect(manager.join(fakeStream())).rejects.toThrow("bad answer")
    } finally {
        FakePeerConnection.prototype.setRemoteDescription = original
    }
    await manager.destroy()

    expect(api.sfuCallLeave.mock.calls).toEqual([[URL, ROOM, MINE]])
})

test("replaced: the session ends without a leave or a rejoin, and says so", async () => {
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const { manager, left } = recordedManager(api)
        const replacedEvents: unknown[] = []
        manager.on("replaced", (event: unknown) => replacedEvents.push(event))
        await manager.join(fakeStream())
        api.emitRoster(roster([ALICE]))
        await flush()
        const pc = currentPc()

        api.emitReplaced(replaced(MINE))
        await flush()

        expect(replacedEvents).toEqual([replaced(MINE)])
        expect(pc.closed).toBe(true)
        expect(left).toEqual([ALICE])
        expect(manager.getState().participantId).toBeNull()
        expect(manager.getState().peerConnection).toBeNull()
        expect(api.offerHandlers.size + api.rosterHandlers.size + api.replacedHandlers.size).toBe(0)

        // Nothing brings it back: not its dead connection, not time.
        ;(pc as any).iceConnectionState = "failed"
        ;(pc as any).oniceconnectionstatechange()
        await jest.advanceTimersByTimeAsync(180_000)
        await manager.destroy()
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        expect(FakePeerConnection.instances).toHaveLength(1)
    } finally {
        jest.useRealTimers()
    }
})

test("a replaced event for another session, or another call, is ignored", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    const replacedEvents: unknown[] = []
    manager.on("replaced", (event: unknown) => replacedEvents.push(event))
    await manager.join(fakeStream())

    api.emitReplaced(replaced("P9"))
    api.emitReplaced(replaced(MINE, { roomName: "other" }))
    api.emitReplaced(replaced(MINE, { neighbourhoodUrl: "neighbourhood://other" }))
    await flush()

    expect(replacedEvents).toEqual([])
    expect(currentPc().closed).toBe(false)
    expect(manager.getState().participantId).toBe(MINE)
})

test("an agent in the call from two devices is two participants, each with its own media", async () => {
    const api = fakeApi()
    const { manager, joined, participants } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitOffer(offer("alice", [mapped("0", ALICE, "audio", "P7"), mapped("1", ALICE, "audio", "P8")]))
    api.emitRoster(roster([["P7", ALICE], ["P8", ALICE], ME]))
    await flush()
    const laptop = new FakeTrack("audio")
    const phone = new FakeTrack("audio")
    currentPc().receive(laptop, "0")
    currentPc().receive(phone, "1")

    expect(joined).toEqual([ALICE, ALICE])
    expect(participants()).toEqual([ALICE, ALICE])
    const [first, second] = manager.getParticipants()
    expect([first.participantId, second.participantId]).toEqual(["P7", "P8"])
    expect(first.stream.getTracks()).toEqual([laptop])
    expect(second.stream.getTracks()).toEqual([phone])
})

test("this agent's other device is a participant; this session never is", async () => {
    const api = fakeApi()
    const { manager, joined, added } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitOffer(offer("mine", [mapped("0", ME, "audio", MINE), mapped("1", ME, "audio", "P5")]))
    api.emitRoster(roster([[MINE, ME], ["P5", ME], ALICE]))
    await flush()
    currentPc().receive(new FakeTrack("audio"), "0")
    const otherDevice = new FakeTrack("audio")
    currentPc().receive(otherDevice, "1")

    expect(joined).toEqual([ME, ALICE])
    expect(manager.getParticipants().map((p) => p.participantId)).toEqual(["P5", pid(ALICE)])
    expect(added.map(([, track]) => track)).toEqual([otherDevice])
})

test("an offer for another session of this agent is not applied", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())

    api.emitOffer(offer("for-my-phone", [], "P5"))
    await flush()

    expect(currentPc().remoteSdps()).toEqual(["server-answer"])
    expect(api.sfuCallAnswerServerOffer).not.toHaveBeenCalled()
})

test("an agent hanging up on one device stays in the call on the other", async () => {
    const api = fakeApi()
    const { manager, left } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitRoster(roster([["P7", ALICE], ["P8", ALICE]]))
    await flush()

    api.emitRoster(roster([["P8", ALICE]]))
    await flush()

    expect(left).toEqual([ALICE])
    expect(manager.getParticipants().map((p) => p.participantId)).toEqual(["P8"])
})

test("data this session sent never comes back to it; data from this agent's other device does", async () => {
    const api = fakeApi()
    let deliver!: (message: SfuDataMessage) => void
    api.subscribeSfuDataChannel.mockImplementation(((handler: (message: SfuDataMessage) => void) => {
        deliver = handler
        return () => {}
    }) as any)
    const manager = new SfuManager(api, ROOM, ME, URL)
    const received: string[] = []
    manager.subscribeDataChannel((message) => received.push(message.data))
    await manager.join(fakeStream())

    const message = (senderParticipantId: string, data: string): SfuDataMessage => ({
        senderDid: ME, senderParticipantId, neighbourhoodUrl: URL, roomName: ROOM, channelLabel: "chat", binary: false, data,
    })
    deliver(message(MINE, "echo"))
    deliver(message("P5", "from my phone"))

    expect(received).toEqual(["from my phone"])
})

// ── Device id ─────────────────────────────────────────────────────

describe("the default device id", () => {
    const g = globalThis as any
    afterEach(() => { delete g.localStorage })

    function fakeLocalStorage() {
        const items = new Map<string, string>()
        return {
            items,
            getItem: (key: string) => items.get(key) ?? null,
            setItem: (key: string, value: string) => { items.set(key, value) },
            removeItem: (key: string) => { items.delete(key) },
        }
    }

    test("in a browser, every manager of one profile shares one, kept in localStorage", async () => {
        g.localStorage = fakeLocalStorage()
        const api = fakeApi()
        await new SfuManager(api, ROOM, ME, URL).join(fakeStream())
        await new SfuManager(api, "other-room", ME, URL).join(fakeStream())

        const [first, second] = api.sfuCallJoin.mock.calls.map((call: unknown[]) => call[3])
        expect(typeof first).toBe("string")
        expect(first).not.toBe("")
        expect(second).toBe(first)
        expect([...g.localStorage.items.values()]).toEqual([first])
        expect(defaultDeviceId()).toBe(first)
    })

    test("another browser profile, with its own storage, is another device", () => {
        g.localStorage = fakeLocalStorage()
        const one = defaultDeviceId()
        g.localStorage = fakeLocalStorage()
        expect(defaultDeviceId()).not.toBe(one)
    })

    test("outside a browser, one per process", () => {
        expect(g.localStorage).toBeUndefined()
        expect(defaultDeviceId()).toBe(defaultDeviceId())
    })

    test("an explicit device id overrides it", async () => {
        g.localStorage = fakeLocalStorage()
        const api = fakeApi()
        await new SfuManager(api, ROOM, ME, URL, undefined, undefined, "electron-main").join(fakeStream())
        expect(api.sfuCallJoin).toHaveBeenCalledWith(URL, ROOM, expect.any(String), "electron-main")
        expect(g.localStorage.items.size).toBe(0)
    })
})
