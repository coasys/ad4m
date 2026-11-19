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

import { RpcError } from "../apiClient"
import { defaultDeviceId, SfuManager, type SfuNeighbourhoodApi, type SfuParticipantState } from "./SfuManager"
import type {
    CallSessionInfo,
    SfuCallRenegotiationOffer,
    SfuConfig,
    SfuDataMessage,
    SfuRoomInfo,
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

/** The room as `sfu.listRooms` returns it, with the sessions given as `[participantId, did]`. */
function room(...sessions: [string, string][]): SfuRoomInfo {
    return {
        neighbourhoodUrl: URL,
        roomName: ROOM,
        participantCount: sessions.length,
        participants: sessions.map(([participantId, agentDid]) => ({
            participantId,
            agentDid,
            hasAudio: false,
            hasVideo: false,
            isActiveSpeaker: false,
        })),
        createdAtMs: 0,
    }
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
    const networkHandlers = new Set<() => void>()
    const reconnectHandlers = new Set<() => void>()
    const api = {
        log,
        offerHandlers,
        rosterHandlers,
        replacedHandlers,
        emitReplaced(event: SfuSessionReplaced) {
            for (const handler of replacedHandlers) handler(event)
        },
        networkHandlers,
        emitNetworkChange() {
            for (const handler of networkHandlers) handler()
        },
        reconnectHandlers,
        /** The executor socket opened again after a drop. */
        emitReconnect() {
            for (const handler of reconnectHandlers) handler()
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
        sfuCallRestartIce: jest.fn(async (_url: string, _room: string, _participantId: string) => {
            log.push("sfuCallRestartIce")
            return true
        }),
        onNetworkChange: jest.fn((handler: () => void) => {
            networkHandlers.add(handler)
            return () => { networkHandlers.delete(handler) }
        }),
        checkConnection: jest.fn(() => {}),
        onReconnect: jest.fn((handler: () => void) => {
            reconnectHandlers.add(handler)
            return () => { reconnectHandlers.delete(handler) }
        }),
        sfuCallResume: jest.fn(async (_url: string, _room: string, _participantId: string) => {
            log.push("sfuCallResume")
            return true
        }),
        // The room read before an unlisted rebuild: by default the executor lists nobody.
        sfuListRooms: jest.fn(async (): Promise<SfuRoomInfo[]> => []),
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

test("offers that arrive before the join completes apply after its answer, the newest only", async () => {
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
    expect(pc.remoteSdps()).toEqual(["server-answer", "early-2"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(1)
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
    await flush()
    api.emitOffer(offer("second"))
    await flush()
    const pc = currentPc()
    expect(pc.remoteSdps()).toEqual(["server-answer", "first"])

    answered.resolve(true)
    await flush()
    expect(pc.remoteSdps()).toEqual(["server-answer", "first", "second"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
})

test("an offer the executor sends again after it was answered is not answered twice", async () => {
    // A second restart request while the restart's offer waits gets that
    // offer again. The executor takes one answer per offer: a second answer
    // would land on its next offer and leave it with stale ICE credentials.
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())

    api.emitOffer(offer("restart"))
    await flush()
    api.emitOffer(offer("restart"))
    await flush()
    api.emitOffer(offer("next"))
    await flush()

    expect(currentPc().remoteSdps()).toEqual(["server-answer", "restart", "next"])
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
    await flush()
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
    expect(api.sfuCallJoin).toHaveBeenLastCalledWith(URL, ROOM, expect.any(String), expect.any(String), expect.any(String))
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

test("a rebuild's join refused as replaced stops the call, says so, and never joins again", async () => {
    // The replaced notice was missed (the socket was down): only the refusal
    // tells this tab that another tab of the device took the call.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
        const replacedEvents: unknown[] = []
        const errors: unknown[] = []
        manager.on("replaced", (event: unknown) => replacedEvents.push(event))
        manager.on("error", (e: unknown) => errors.push(e))
        await manager.join(fakeStream())
        api.sfuCallLeave.mockResolvedValueOnce(false)
        api.sfuCallJoin.mockRejectedValueOnce(new RpcError(409, "This call session was replaced"))

        await iceState("failed")
        await flush()
        await jest.advanceTimersByTimeAsync(180_000)

        expect(replacedEvents).toEqual([replaced(MINE)])
        expect(errors).toEqual([])
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
        expect(manager.getState().participantId).toBeNull()
        expect(manager.getState().peerConnection).toBeNull()
        expect(api.offerHandlers.size + api.rosterHandlers.size + api.replacedHandlers.size).toBe(0)
    } finally {
        jest.useRealTimers()
    }
})

test("a rebuild joins with the session key of its call; a new join takes a new key", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
    await manager.join(fakeStream())
    await iceState("failed")
    await manager.leave()
    await manager.join(fakeStream())

    const keys = (api.sfuCallJoin.mock.calls as unknown as string[][]).map((call) => call[4])
    expect(keys).toHaveLength(3)
    expect(keys[0]).toEqual(expect.any(String))
    expect(keys[1]).toBe(keys[0])
    expect(keys[2]).not.toBe(keys[0])
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

    expect(api.sfuCallJoin).toHaveBeenCalledWith(URL, ROOM, expect.any(String), DEVICE, expect.any(String))
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

test("replaced while the join applies its answer: the join resolves, and leaves nothing", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
    const replacedEvents: unknown[] = []
    manager.on("replaced", (event: unknown) => replacedEvents.push(event))
    const original = FakePeerConnection.prototype.setRemoteDescription
    // The notice lands while the answer is applied: stop() closes the connection under it.
    FakePeerConnection.prototype.setRemoteDescription = async () => {
        api.emitReplaced(replaced(MINE))
        throw new Error("InvalidStateError: the connection is closed")
    }
    try {
        await expect(manager.join(fakeStream())).resolves.toBeUndefined()
    } finally {
        FakePeerConnection.prototype.setRemoteDescription = original
    }
    await flush()

    expect(replacedEvents).toEqual([replaced(MINE)])
    expect(api.sfuCallLeave).not.toHaveBeenCalled()
    expect(manager.getState().participantId).toBeNull()
    expect(manager.getState().peerConnection).toBeNull()
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
        expect(api.sfuCallJoin).toHaveBeenCalledWith(URL, ROOM, expect.any(String), "electron-main", expect.any(String))
        expect(g.localStorage.items.size).toBe(0)
    })
})

// ── Recovering on the same connection ───────────────────────────────────────

const fakeTimers = () => jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })

test("ICE disconnected asks the executor for an ICE restart at once and answers the offer it sends", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")
        api.log.length = 0

        await iceState("disconnected")
        expect(api.sfuCallRestartIce).toHaveBeenCalledWith(URL, ROOM, MINE)

        // The restart arrives as a server offer, on the same connection.
        api.emitOffer(offer("restart-offer"))
        await flush()
        expect(currentPc().remoteSdps()).toContain("restart-offer")
        await iceState("checking")
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(30_000)

        expect(api.log).toEqual(["sfuCallRestartIce", "sfuCallAnswerServerOffer"])
        expect(FakePeerConnection.instances).toHaveLength(1)
    } finally {
        jest.useRealTimers()
    }
})

test("ICE disconnected also checks the executor socket once, with the restart", async () => {
    // A network switch the platform announces nothing for: the restart may
    // go out on a dead socket, and only the check finds that out quickly.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")
        api.log.length = 0

        await iceState("disconnected")
        expect(api.checkConnection).toHaveBeenCalledTimes(1)
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(1)

        // Connected again: no further check.
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.checkConnection).toHaveBeenCalledTimes(1)
    } finally {
        jest.useRealTimers()
    }
})

test("a lost restart offer stays open until the executor can resend it", async () => {
    // ICE drops, the offer is lost, and signaling returns 2 s later. The
    // executor resends that offer at 10 s. The restart stays open through
    // that resend, then long enough for ICE to finish.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")
        api.log.length = 0

        await iceState("disconnected")
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(1)
        await jest.advanceTimersByTimeAsync(2_000)
        api.emitNetworkChange()
        // ICE is still down 2 s after the socket is back: ask again.
        await jest.advanceTimersByTimeAsync(2_000)
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(2)

        // t = 7 s. A 5 s window started at t = 2 s would rebuild here.
        await jest.advanceTimersByTimeAsync(3_000)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        // t = 10 s. The executor's first resend is due. Still no rebuild.
        await jest.advanceTimersByTimeAsync(3_000)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()

        // The original peer must apply the resent offer and post its answer
        // before ICE recovery. `connected` clears the restart timer on its own.
        const peer = currentPc()
        api.emitOffer(offer("restart-offer"))
        await flush()
        expect(peer.remoteSdps()).toContain("restart-offer")
        expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(1)
        expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledWith(
            URL,
            ROOM,
            MINE,
            JSON.stringify({ type: "answer", sdp: "local-answer" }),
        )

        await iceState("connected")
        await jest.advanceTimersByTimeAsync(30_000)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        expect(FakePeerConnection.instances).toHaveLength(1)
        expect(manager.getState().peerConnection).toBe(FakePeerConnection.instances[0])
    } finally {
        jest.useRealTimers()
    }
})

test("a restart that has not brought the connection back after 15 s falls back to a rebuild", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")

        await iceState("disconnected")
        await jest.advanceTimersByTimeAsync(14_900)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        await jest.advanceTimersByTimeAsync(200)
        expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    } finally {
        jest.useRealTimers()
    }
})

test("a refused restart also waits for the timeout before rebuilding", async () => {
    // The executor no longer knows this session: a blip that heals keeps the call.
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuCallRestartIce.mockRejectedValue(new Error("Session P1 is not in this call"))
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        await iceState("disconnected")
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()

        await iceState("disconnected")
        await jest.advanceTimersByTimeAsync(14_900)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        await jest.advanceTimersByTimeAsync(200)
        expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
    } finally {
        jest.useRealTimers()
    }
})

test("a network change asks again during a restart, and its timeout starts over", async () => {
    // ICE noticed first, and its restart went out over the old network:
    // the network change is the one that can reach the executor.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        await iceState("disconnected")
        await jest.advanceTimersByTimeAsync(3_000)
        api.emitNetworkChange()
        await jest.advanceTimersByTimeAsync(2_000)
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(2)

        await jest.advanceTimersByTimeAsync(4_000)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("ICE going disconnected during a restart does not ask again", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitNetworkChange()
        await iceState("disconnected")
        await iceState("checking")
        await iceState("disconnected")
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(1)

        // Once that one settled, a new disconnect restarts again.
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(5_000)
        await iceState("disconnected")
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(2)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("a roster that lists this agent during a restart lets the restart finish in place", async () => {
    // The server keeps the session while ICE restarts: rosters still list it.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")

        await iceState("disconnected")
        api.emitRoster(roster([ALICE, ME]))
        await flush()
        api.emitOffer(offer("restart-offer"))
        await flush()
        await jest.advanceTimersByTimeAsync(4_000)
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(30_000)

        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        expect(FakePeerConnection.instances).toHaveLength(1)
        expect(currentPc().closed).toBe(false)
    } finally {
        jest.useRealTimers()
    }
})

test("a roster that leaves this agent out during a restart rebuilds after its grace, once", async () => {
    // The server dropped the session: the restart cannot heal it, so the
    // roster's grace (3 s) ends it before the restart's timeout (15 s).
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")

        await iceState("disconnected")
        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(2_999)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        await jest.advanceTimersByTimeAsync(1)
        expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)

        // The rebuild ended the restart: its timeout does not rebuild again.
        await jest.advanceTimersByTimeAsync(30_000)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
        expect(FakePeerConnection.instances).toHaveLength(2)
    } finally {
        jest.useRealTimers()
    }
})

test("ICE failed still rebuilds at once, ending a restart in flight", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        await iceState("disconnected")
        await iceState("failed")
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
        // The restart's timeout does not rebuild the new connection again.
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    } finally {
        jest.useRealTimers()
    }
})

test("leave stops listening for network changes and drops a restart in flight", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        expect(api.networkHandlers.size).toBe(1)
        await iceState("disconnected")
        await manager.leave()

        expect(api.networkHandlers.size).toBe(0)
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    } finally {
        jest.useRealTimers()
    }
})

test("a rebuild listens for network changes once, not once per join", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    await iceState("failed")
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    expect(api.networkHandlers.size).toBe(1)
})

test("a restart names the session of the current join: after a rebuild, the new one", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuCallJoin
            .mockResolvedValueOnce(session())
            .mockResolvedValueOnce(session({ participantId: "P2" }))
        const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
        await manager.join(fakeStream())
        await iceState("disconnected")
        await iceState("failed")
        expect(manager.getState().participantId).toBe("P2")

        await iceState("connected")
        await iceState("disconnected")

        expect(api.sfuCallRestartIce.mock.calls).toEqual([
            [URL, ROOM, MINE],
            [URL, ROOM, "P2"],
        ])
    } finally {
        jest.useRealTimers()
    }
})

test("no restart goes out before the join has its session", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const joining = deferred<CallSessionInfo>()
        api.sfuCallJoin.mockImplementationOnce(() => joining.promise)
        const manager = new SfuManager(api, ROOM, ME, URL)
        const joined = manager.join(fakeStream())
        await flush()

        api.emitNetworkChange()
        await iceState("disconnected")
        expect(api.sfuCallRestartIce).not.toHaveBeenCalled()
        // No restart in flight, so no timeout rebuilds the call either.
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()

        joining.resolve(session())
        await joined
        await iceState("disconnected")
        expect(api.sfuCallRestartIce.mock.calls).toEqual([[URL, ROOM, MINE]])
    } finally {
        jest.useRealTimers()
    }
})

test("a replaced manager sends no restart, and its restart in flight rebuilds nothing", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL, undefined, undefined, DEVICE)
        await manager.join(fakeStream())
        const pc = currentPc()
        await iceState("disconnected")
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(1)

        api.emitReplaced(replaced(MINE))
        await flush()
        // The old connection's late state changes and a network change reach nothing.
        ;(pc as any).iceConnectionState = "disconnected"
        ;(pc as any).oniceconnectionstatechange()
        api.emitNetworkChange()
        await jest.advanceTimersByTimeAsync(30_000)

        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(1)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("a manager that left sends no restart for its old connection", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const pc = currentPc()
    await manager.leave()

    ;(pc as any).iceConnectionState = "disconnected"
    ;(pc as any).oniceconnectionstatechange()
    api.emitNetworkChange()
    await flush()

    expect(api.sfuCallRestartIce).not.toHaveBeenCalled()
})

// ── A socket fault on the answer keeps the call (C1) ───────────────────────

test.each([
    ["503 (the socket closed while the answer was out)", new RpcError(503, "WebSocket connection closed")],
    ["408 (the answer timed out)", new RpcError(408, "RPC call 'sfu.callAnswerServerOffer' timed out after 30000ms")],
])("a %s keeps the connection and the participant id, and the answer goes again once the socket is back", async (_name, fault) => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    await iceState("connected")
    const pc = currentPc()
    api.sfuCallAnswerServerOffer.mockRejectedValueOnce(fault)

    api.emitOffer(offer("renegotiate"))
    await flush()
    expect(pc.closed).toBe(false)
    expect(manager.getState().participantId).toBe(MINE)
    expect(manager.getState().peerConnection).toBe(pc)
    expect(api.sfuCallLeave).not.toHaveBeenCalled()
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)

    api.emitReconnect()
    await flush()
    const answer = JSON.stringify({ type: "answer", sdp: "local-answer" })
    expect(api.sfuCallAnswerServerOffer.mock.calls).toEqual([
        [URL, ROOM, MINE, answer],
        [URL, ROOM, MINE, answer],
    ])
    // Applied once: the second send is the same answer, not a second negotiation.
    expect(pc.remoteSdps()).toEqual(["server-answer", "renegotiate"])

    // Answered now: the executor's late resend of that offer gets nothing.
    api.emitOffer(offer("renegotiate"))
    api.emitReconnect()
    await flush()
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
    expect(FakePeerConnection.instances).toHaveLength(1)
})

test("an offer the executor resends while its answer is unsent gets that answer, applied once", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const pc = currentPc()
    api.sfuCallAnswerServerOffer.mockRejectedValueOnce(new RpcError(503, "WebSocket connection closed"))

    api.emitOffer(offer("renegotiate"))
    await flush()
    api.emitOffer(offer("renegotiate"))
    await flush()

    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
    expect(pc.remoteSdps()).toEqual(["server-answer", "renegotiate"])
    expect(api.sfuCallLeave).not.toHaveBeenCalled()
})

test("an executor that refuses the answer does not make the client rebuild", async () => {
    // Whether the server still has the session is for the roster and ICE to say.
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.sfuCallAnswerServerOffer.mockRejectedValueOnce(new RpcError(500, "SFU answer failed"))

    api.emitOffer(offer("renegotiate"))
    await flush()

    expect(currentPc().closed).toBe(false)
    expect(api.sfuCallLeave).not.toHaveBeenCalled()
    expect(manager.getState().participantId).toBe(MINE)
})

test("an offer the connection cannot apply still rebuilds the call", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const pc = currentPc()
    jest.spyOn(pc, "setRemoteDescription").mockRejectedValueOnce(new Error("InvalidSessionDescription"))

    api.emitOffer(offer("broken"))
    await flush()

    expect(pc.closed).toBe(true)
    expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    expect(api.sfuCallAnswerServerOffer).not.toHaveBeenCalled()
})

// ── Resume after a socket reconnect (C6) ────────────────────────────────────

test("after the socket reconnects the manager asks the executor to resume its session", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.log.length = 0

    api.emitReconnect()
    await flush()

    expect(api.sfuCallResume.mock.calls).toEqual([[URL, ROOM, MINE]])
    expect(api.log).toEqual(["sfuCallResume"])
    expect(api.sfuCallLeave).not.toHaveBeenCalled()
})

test("an answer the socket fault kept goes out before the resume", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.sfuCallAnswerServerOffer.mockRejectedValueOnce(new RpcError(503, "WebSocket connection closed"))
    api.emitOffer(offer("renegotiate"))
    await flush()
    api.log.length = 0

    api.emitReconnect()
    await flush()

    expect(api.log).toEqual(["sfuCallAnswerServerOffer", "sfuCallResume"])
})

test("an executor without sfu.callResume: the manager waits for its resend, keeps the call, and asks again next time", async () => {
    const api = fakeApi()
    api.sfuCallResume.mockRejectedValue(new RpcError(404, "Unknown type: sfu.callResume"))
    const manager = new SfuManager(api, ROOM, ME, URL)
    const errors: unknown[] = []
    manager.on("error", (e) => errors.push(e))
    await manager.join(fakeStream())

    api.emitReconnect()
    await flush()
    // The executor resends its pending offer on its own (10 s, 30 s).
    api.emitOffer(offer("resent"))
    await flush()
    api.emitReconnect()
    await flush()

    expect(currentPc().remoteSdps()).toEqual(["server-answer", "resent"])
    expect(api.sfuCallResume).toHaveBeenCalledTimes(2)
    expect(api.sfuCallLeave).not.toHaveBeenCalled()
    expect(errors).toEqual([])
})

test("an api without onReconnect or sfuCallResume still joins and keeps its call", async () => {
    const api = fakeApi() as Partial<ReturnType<typeof fakeApi>>
    delete api.onReconnect
    delete api.sfuCallResume
    const manager = new SfuManager(api as SfuNeighbourhoodApi, ROOM, ME, URL)
    await manager.join(fakeStream())
    expect(manager.getState().participantId).toBe(MINE)
})

test("no resume goes out without a session, and none after leave", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    api.emitReconnect()
    await manager.join(fakeStream())
    await manager.leave()
    api.emitReconnect()
    await flush()
    expect(api.sfuCallResume).not.toHaveBeenCalled()
    expect(api.reconnectHandlers.size).toBe(0)
})

// ── ICE restart only when ICE is down after the socket is back (C3) ────────

test("a network change while ICE stays connected restarts nothing", async () => {
    // A late pong dropped a healthy socket: media never stopped.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")

        api.emitNetworkChange()
        api.emitReconnect()
        await jest.advanceTimersByTimeAsync(30_000)

        expect(api.sfuCallRestartIce).not.toHaveBeenCalled()
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        expect(FakePeerConnection.instances).toHaveLength(1)
    } finally {
        jest.useRealTimers()
    }
})

test.each(["network change", "plain reconnect"])(
    "after a %s, ICE that is still not connected 2 s later is restarted",
    async (kind) => {
        fakeTimers()
        try {
            const api = fakeApi()
            const manager = new SfuManager(api, ROOM, ME, URL)
            await manager.join(fakeStream())
            await iceState("connected")
            ;(currentPc() as any).iceConnectionState = "checking"

            if (kind === "network change") api.emitNetworkChange()
            else api.emitReconnect()
            await jest.advanceTimersByTimeAsync(1_999)
            expect(api.sfuCallRestartIce).not.toHaveBeenCalled()
            await jest.advanceTimersByTimeAsync(1)
            expect(api.sfuCallRestartIce.mock.calls).toEqual([[URL, ROOM, MINE]])
        } finally {
            jest.useRealTimers()
        }
    },
)

test("ICE that comes back within the 2 s after the socket does is not restarted", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        ;(currentPc() as any).iceConnectionState = "checking"

        api.emitNetworkChange()
        await jest.advanceTimersByTimeAsync(1_000)
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(30_000)

        expect(api.sfuCallRestartIce).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("leave drops a pending check of ICE after the socket", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        api.emitNetworkChange()
        await manager.leave()
        await jest.advanceTimersByTimeAsync(5_000)
        expect(api.sfuCallRestartIce).not.toHaveBeenCalled()
        expect(jest.getTimerCount()).toBe(0)
    } finally {
        jest.useRealTimers()
    }
})

// ── Rebuild budget per time window (C4) ─────────────────────────────────────

test("at most three rebuilds in two minutes, even when ICE connects in between; then it stops and says so once", async () => {
    // A flapping socket used to rebuild for ever: the count reset on every connect.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        const errors: unknown[] = []
        manager.on("error", (e) => errors.push(e))
        await manager.join(fakeStream())

        for (let i = 0; i < 5; i++) {
            await iceState("failed")
            await iceState("connected")
            await jest.advanceTimersByTimeAsync(10_000)
        }

        expect(api.sfuCallLeave).toHaveBeenCalledTimes(3)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(4)
        expect(errors).toEqual([new Error("SFU reconnect attempts exhausted")])
    } finally {
        jest.useRealTimers()
    }
})

test("rebuilds further apart than the window are not limited", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        const errors: unknown[] = []
        manager.on("error", (e) => errors.push(e))
        await manager.join(fakeStream())

        for (let i = 0; i < 6; i++) {
            await iceState("failed")
            await iceState("connected")
            await jest.advanceTimersByTimeAsync(45_000)
        }

        expect(api.sfuCallLeave).toHaveBeenCalledTimes(6)
        expect(errors).toEqual([])
    } finally {
        jest.useRealTimers()
    }
})

test("a new join by the user after the budget ran out starts with a fresh one", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    for (let i = 0; i < 4; i++) await iceState("failed")
    expect(api.sfuCallLeave).toHaveBeenCalledTimes(3)

    await manager.leave()
    await manager.join(fakeStream())
    await iceState("failed")
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(6)
})

// ── Ask the executor before an unlisted rebuild (C5) ────────────────────────

test("a roster that leaves this session out does not rebuild while the executor still lists it", async () => {
    // One roster event can be stale or out of order; the room read is current.
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuListRooms.mockResolvedValue([room([MINE, ME], [pid(ALICE), ALICE])])
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(10_000)

        expect(api.sfuListRooms).toHaveBeenCalledTimes(1)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        expect(currentPc().closed).toBe(false)
    } finally {
        jest.useRealTimers()
    }
})

test("an unlisted session is rebuilt once the executor's room read confirms it is gone", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuListRooms.mockResolvedValue([room([pid(ALICE), ALICE])])
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)

        expect(api.sfuListRooms).toHaveBeenCalledTimes(1)
        expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
        expect(api.sfuListRooms.mock.invocationCallOrder[0]).toBeLessThan(api.sfuCallLeave.mock.invocationCallOrder[0])
    } finally {
        jest.useRealTimers()
    }
})

test("a room read that fails does not rebuild the call", async () => {
    // The socket is down: the server's state cannot be read, so nothing is torn down for it.
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuListRooms.mockRejectedValue(new RpcError(503, "WebSocket connection closed"))
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(10_000)

        expect(api.sfuCallLeave).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("a roster that lists this session while the room read is out cancels the rebuild", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const read = deferred<SfuRoomInfo[]>()
        api.sfuListRooms.mockImplementation(() => read.promise)
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)
        api.emitRoster(roster([ALICE, ME]))
        await flush()
        read.resolve([])
        await jest.advanceTimersByTimeAsync(1_000)

        expect(api.sfuCallLeave).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("a room read the executor refuses (not a socket fault) goes by the roster and rebuilds", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuListRooms.mockRejectedValue(new RpcError(403, "Capability check failed"))
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)

        expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
    } finally {
        jest.useRealTimers()
    }
})

// ── Offer queue: newest only, each step time-boxed (C7) ─────────────────────

test("of several offers waiting, only the newest is applied; the track mappings of all are kept", async () => {
    const api = fakeApi()
    const join = deferred<CallSessionInfo>()
    api.sfuCallJoin.mockImplementation(() => join.promise)
    const { manager, participants } = recordedManager(api)
    const joining = manager.join(fakeStream())
    await flush()

    api.emitOffer(offer("early-1", [mapped("1", ALICE, "audio")]))
    api.emitOffer(offer("early-2", [mapped("2", BOB, "audio")]))
    api.emitOffer(offer("early-3"))
    join.resolve(session())
    await joining
    await flush()

    const pc = currentPc()
    expect(pc.remoteSdps()).toEqual(["server-answer", "early-3"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(1)
    pc.receive(new FakeTrack("audio"), "1")
    pc.receive(new FakeTrack("audio"), "2")
    expect(participants()).toEqual([ALICE, BOB])
})

test("an offer step that hangs is given up after 10 s, and the next offer is applied", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        const pc = currentPc()
        api.sfuCallAnswerServerOffer.mockImplementationOnce(() => new Promise(() => {}))

        api.emitOffer(offer("first"))
        await flush()
        await jest.advanceTimersByTimeAsync(1_000)
        api.emitOffer(offer("second"))
        await jest.advanceTimersByTimeAsync(8_999)
        expect(pc.remoteSdps()).toEqual(["server-answer", "first"])

        await jest.advanceTimersByTimeAsync(1)
        expect(pc.remoteSdps()).toEqual(["server-answer", "first", "second"])
        expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("an answer given up on that fails after a newer offer was answered is not kept for a resend", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        let timeOut!: (e: unknown) => void
        api.sfuCallAnswerServerOffer.mockImplementationOnce(() => new Promise((_, reject) => { timeOut = reject }))

        api.emitOffer(offer("first"))
        await flush()
        api.emitOffer(offer("second"))
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
        api.sfuCallAnswerServerOffer.mockClear()

        // The first answer's call times out only now.
        timeOut(new RpcError(408, "RPC call 'sfu.callAnswerServerOffer' timed out after 30000ms"))
        await flush()
        api.emitReconnect()
        await flush()
        expect(api.sfuCallAnswerServerOffer).not.toHaveBeenCalled()
        expect(api.sfuCallResume).toHaveBeenCalledTimes(1)
    } finally {
        jest.useRealTimers()
    }
})

// ── A rebuild keeps the roster (C8) ─────────────────────────────────────────

test("a rebuild keeps the participants: nobody leaves, their media is cleared until the new connection brings it", async () => {
    const api = fakeApi()
    const { manager, joined, left, removed, participants } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitRoster(roster([ALICE, BOB]))
    await flush()
    api.emitOffer(offer("tracks", [mapped("1", ALICE, "audio")]))
    await flush()
    const aliceTrack = new FakeTrack("audio")
    currentPc().receive(aliceTrack, "1")
    const alice = manager.getParticipants().find((p) => p.did === ALICE)!
    expect(alice.hasAudio).toBe(true)

    await iceState("failed")

    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    expect(left).toEqual([])
    expect(joined).toEqual([ALICE, BOB])
    expect(participants()).toEqual([ALICE, BOB])
    // The same participant, and the same stream an app has attached, without the old track.
    expect(manager.getParticipants().find((p) => p.did === ALICE)).toBe(alice)
    expect(alice.stream.getTracks()).toEqual([])
    expect(alice.hasAudio).toBe(false)
    expect(removed).toEqual([[alice.stream, aliceTrack]])

    // The new connection's media lands in that stream; the room's roster still decides who left.
    api.emitOffer(offer("tracks-again", [mapped("1", ALICE, "audio")]))
    await flush()
    const again = new FakeTrack("audio")
    currentPc().receive(again, "1")
    expect(alice.stream.getTracks()).toEqual([again])
    api.emitRoster(roster([ALICE]))
    await flush()
    expect(left).toEqual([BOB])
})
