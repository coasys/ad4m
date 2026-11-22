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
    SfuAnswerResult,
    SfuCallRenegotiationOffer,
    SfuConfig,
    SfuDataMessage,
    SfuResumeResult,
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

/** A server offer; its `offerId` names it, so a resend of the same offer carries the same id. */
function offer(sdp: string, trackMapping?: TrackMapEntry[], participantId = MINE): SfuCallRenegotiationOffer {
    return {
        targetDid: ME,
        participantId,
        offerId: `offer-${sdp}`,
        neighbourhoodUrl: URL,
        roomName: ROOM,
        sdpOffer: JSON.stringify({ type: "offer", sdp }),
        trackMapping,
    }
}

function mapped(mid: string, agentDid: string, mediaKind: "audio" | "video", participantId = pid(agentDid)): TrackMapEntry {
    return { mid, participantId, agentDid, mediaKind }
}

/** The executor numbers every roster publish of a room; successive helpers get newer numbers. */
let nextRevision = 1

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
    return { neighbourhoodUrl: URL, roomName: ROOM, revision: nextRevision++, participants: listed, ...overrides }
}

/** `sfu.callResume` saying this session is still listed, its room's roster attached. */
function resumeListed(overrides: Partial<SfuResumeResult> = {}): SfuResumeResult {
    return {
        listed: true,
        roster: { revision: nextRevision++, participants: [{ participantId: MINE, agentDid: ME }] },
        ...overrides,
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
        sfuCallAnswerServerOffer: jest.fn(async (): Promise<SfuAnswerResult> => {
            log.push("sfuCallAnswerServerOffer")
            return { accepted: true }
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
        sfuCallResume: jest.fn(async (): Promise<SfuResumeResult> => {
            log.push("sfuCallResume")
            return resumeListed()
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
        offer("early-2").offerId,
        JSON.stringify({ type: "answer", sdp: "local-answer" }),
    )
})

test("a second offer waits until the first one is answered", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const answered = deferred<SfuAnswerResult>()
    api.sfuCallAnswerServerOffer.mockImplementationOnce(() => answered.promise)

    api.emitOffer(offer("first"))
    await flush()
    api.emitOffer(offer("second"))
    await flush()
    const pc = currentPc()
    expect(pc.remoteSdps()).toEqual(["server-answer", "first"])

    answered.resolve({ accepted: true })
    await flush()
    expect(pc.remoteSdps()).toEqual(["server-answer", "first", "second"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
})

test("a resume reply's offer waits behind an offer's SDP step still in flight", async () => {
    // The unlisted-roster timer asked for the resume outside the queue, so
    // its offer could be applied beside another offer's SDP step.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        const pc = currentPc()

        const applying = deferred<void>()
        const original = FakePeerConnection.prototype.setRemoteDescription
        FakePeerConnection.prototype.setRemoteDescription = async function (this: FakePeerConnection, d) {
            if (d.sdp === "event-offer") await applying.promise
            this.remoteDescriptions.push(d)
        }
        try {
            api.emitOffer(offer("event-offer"))
            await flush()

            // The grace for the unlisted roster ends; the resume brings an offer.
            api.sfuCallResume.mockResolvedValue({ listed: true, offer: offer("resume-offer") })
            api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
            await flush()
            await jest.advanceTimersByTimeAsync(3_000)
            await flush()
            await flush()

            expect(pc.remoteSdps()).toEqual(["server-answer"])

            applying.resolve()
            await flush()
            await flush()
            expect(pc.remoteSdps()).toEqual(["server-answer", "event-offer", "resume-offer"])
        } finally {
            FakePeerConnection.prototype.setRemoteDescription = original
        }
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
})

test("a slow SDP step on a socket-reconnect resume holds the queue past the step timeout", async () => {
    // socketBack boxed the whole resume step: past the limit the queue went
    // on while its SDP work still ran, beside the next offer's.
    jest.useFakeTimers({ doNotFake: ["queueMicrotask", "nextTick", "setImmediate"] })
    try {
        const api = fakeApi()
        api.sfuCallResume.mockResolvedValue({ listed: true, offer: offer("resume-offer") })
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        const pc = currentPc()
        // Media is fine: the socket check finds nothing to restart.
        ;(pc as any).iceConnectionState = "connected"

        const applying = deferred<void>()
        const original = FakePeerConnection.prototype.setRemoteDescription
        FakePeerConnection.prototype.setRemoteDescription = async function (this: FakePeerConnection, d) {
            if (d.sdp === "resume-offer") await applying.promise
            this.remoteDescriptions.push(d)
        }
        try {
            api.emitReconnect()
            await flush()
            await flush()

            // Long past the step timeout, with the resume's offer still applying.
            await jest.advanceTimersByTimeAsync(30_000)
            api.emitOffer(offer("next-offer"))
            await flush()
            await flush()
            expect(pc.remoteSdps()).toEqual(["server-answer"])

            applying.resolve()
            await flush()
            await flush()
            expect(pc.remoteSdps()).toEqual(["server-answer", "resume-offer", "next-offer"])
        } finally {
            FakePeerConnection.prototype.setRemoteDescription = original
        }
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
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
    const answered = deferred<SfuAnswerResult>()
    api.sfuCallAnswerServerOffer.mockImplementationOnce(() => answered.promise)
    api.emitOffer(offer("first"))
    await flush()
    api.emitOffer(offer("stale"))
    await flush()

    await iceState("failed")
    const replacement = currentPc()
    expect(replacement).not.toBe(old)
    answered.resolve({ accepted: true })
    await flush()

    expect(old.remoteSdps()).toEqual(["server-answer", "first"])
    expect(replacement.remoteSdps()).toEqual(["server-answer"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(1)
})

test("an answer refused for a replaced connection does not rebuild the new call", async () => {
    // The reply landed after its connection was gone: unknown-session is
    // the executor talking about the old session, not the new one.
    const api = fakeApi()
    api.sfuCallJoin.mockResolvedValueOnce(session()).mockResolvedValueOnce(session({ participantId: "P2" }))
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const old = currentPc()
    const answered = deferred<SfuAnswerResult>()
    api.sfuCallAnswerServerOffer.mockImplementationOnce(() => answered.promise)
    api.emitOffer(offer("first"))
    await flush()

    await iceState("failed")
    await flush()
    expect(currentPc()).not.toBe(old)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)

    answered.resolve({ accepted: false, reason: "unknown-session" })
    await flush()
    await flush()

    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    expect(FakePeerConnection.instances).toHaveLength(2)
    await manager.leave()
})

test("an answer accepted for a replaced connection does not mark its offer answered", async () => {
    // answeredOfferId is the new session's to write; the old reply keeping
    // it would drop that session's offer of the same id unanswered.
    const api = fakeApi()
    api.sfuCallJoin.mockResolvedValueOnce(session()).mockResolvedValueOnce(session({ participantId: "P2" }))
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    const answered = deferred<SfuAnswerResult>()
    api.sfuCallAnswerServerOffer.mockImplementationOnce(() => answered.promise)
    api.emitOffer(offer("first"))
    await flush()

    await iceState("failed")
    await flush()
    const replacement = currentPc()

    answered.resolve({ accepted: true })
    await flush()
    // The executor's resend carries the offer's id again, to the new session.
    api.emitOffer(offer("first", undefined, "P2"))
    await flush()
    await flush()

    expect(replacement.remoteSdps()).toEqual(["server-answer", "first"])
    await manager.leave()
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

test("a roster older than one already applied is dropped", async () => {
    const api = fakeApi()
    const { manager, left, participants } = recordedManager(api)
    await manager.join(fakeStream())
    const fresh = roster([ALICE, BOB])
    api.emitRoster(fresh)
    await flush()
    expect(participants()).toEqual([ALICE, BOB])

    api.emitRoster(roster([BOB], { revision: fresh.revision - 1 }))
    await flush()
    expect(left).toEqual([])
    expect(participants()).toEqual([ALICE, BOB])

    api.emitRoster(roster([BOB]))
    await flush()
    expect(left).toEqual([ALICE])
})

test("a resume's roster older than an applied roster event is dropped, its offer still applies", async () => {
    const api = fakeApi()
    const { manager, left, participants } = recordedManager(api)
    await manager.join(fakeStream())
    const fresh = roster([ALICE])
    api.emitRoster(fresh)
    await flush()
    api.sfuCallResume.mockResolvedValueOnce(
        resumeListed({ roster: { revision: fresh.revision - 1, participants: [{ participantId: MINE, agentDid: ME }] }, offer: offer("R") }),
    )

    api.emitReconnect()
    await flush()
    await flush()

    expect(left).toEqual([])
    expect(participants()).toEqual([ALICE])
    expect(currentPc().remoteSdps()).toEqual(["server-answer", "R"])
    await manager.leave()
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
        // The executor's answer to the resume: the session is gone.
        api.sfuCallResume.mockImplementation(async () => {
            api.log.push("sfuCallResume")
            return { listed: false }
        })
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
            "sfuCallResume",
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

        api.sfuCallResume.mockResolvedValue({ listed: false })
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

        api.sfuCallResume.mockResolvedValue({ listed: false })
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
        api.sfuCallResume.mockResolvedValue({ listed: false })
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

    expect(api.sfuCallLeave).toHaveBeenCalledTimes(4) // 3 rebuilds, then the end leaves the last session
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
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledWith(URL, ROOM, MINE, offer("next").offerId, expect.any(String))
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

test("ICE disconnected restarts ICE in place and does not check the executor socket", async () => {
    // Judging the socket is the socket layer's: ICE `disconnected` is media
    // evidence, and media evidence answers with the restart alone.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")
        api.log.length = 0

        await iceState("disconnected")
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(1)
        expect(api.checkConnection).not.toHaveBeenCalled()

        // Connected again: still no check.
        await iceState("connected")
        await jest.advanceTimersByTimeAsync(10_000)
        expect(api.checkConnection).not.toHaveBeenCalled()
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
            offer("restart-offer").offerId,
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
        api.sfuCallResume.mockResolvedValue({ listed: false })
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

    // The socket is back: the answer it kept goes out again, and the
    // manager asks the executor for what it missed (`sfu.callResume`).
    api.emitReconnect()
    await flush()
    await flush()
    const answer = JSON.stringify({ type: "answer", sdp: "local-answer" })
    expect(api.sfuCallAnswerServerOffer.mock.calls).toEqual([
        [URL, ROOM, MINE, "offer-renegotiate", answer],
        [URL, ROOM, MINE, "offer-renegotiate", answer],
    ])
    expect(api.sfuCallResume).toHaveBeenCalledWith(URL, ROOM, MINE)
    // Applied once: the second send is the same answer, not a second negotiation.
    expect(pc.remoteSdps()).toEqual(["server-answer", "renegotiate"])

    // Answered now: the executor's late resend of that offer gets nothing.
    api.emitOffer(offer("renegotiate"))
    api.emitReconnect()
    await flush()
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(2)
    expect(FakePeerConnection.instances).toHaveLength(1)
    await manager.leave()
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

test("an executor that fails the answer's RPC does not make the client rebuild", async () => {
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

test.each(["stale", "no-pending"] as const)("an answer refused as %s changes nothing: a newer offer is coming or was settled", async (reason) => {
    // The server names why it did not take the answer; both mean this offer
    // is settled elsewhere, not that the session is gone.
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.sfuCallAnswerServerOffer.mockResolvedValueOnce({ accepted: false, reason })

    api.emitOffer(offer("renegotiate"))
    await flush()

    expect(currentPc().closed).toBe(false)
    expect(api.sfuCallLeave).not.toHaveBeenCalled()
    expect(manager.getState().participantId).toBe(MINE)
})

test("an answer refused as unknown-session rebuilds the call", async () => {
    // The event loop no longer holds this session: the call cannot go on with it.
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.sfuCallAnswerServerOffer.mockResolvedValueOnce({ accepted: false, reason: "unknown-session" })

    api.emitOffer(offer("renegotiate"))
    await flush()

    expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
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
    await manager.leave()
})

test("after the socket is back a restart offer whose event was lost is answered through the resume's reply", async () => {
    // X's answer was lost with the socket; the server now waits on a restart
    // offer R whose event never arrived. The answer kept for X is refused by
    // its offer id, and R is applied through the same path the event uses.
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    distinctAnswers(currentPc())
    api.sfuCallAnswerServerOffer.mockRejectedValueOnce(new RpcError(503, "WebSocket connection closed"))
    api.emitOffer(offer("X"))
    await flush()
    api.sfuCallAnswerServerOffer.mockResolvedValueOnce({ accepted: false, reason: "stale" })
    api.sfuCallResume.mockResolvedValueOnce({ listed: true, offer: offer("R") })

    api.emitReconnect()
    await flush()
    await flush()

    // X's first send never arrived; the kept one goes again and is refused
    // `stale`, and R gets its own answer.
    expect(answersSent(api)).toEqual([answerTo("X"), answerTo("X"), answerTo("R")])
    expect(currentPc().remoteSdps()).toEqual(["server-answer", "X", "R"])
    await manager.leave()
})

test("the offer a resume returns gets the answer the socket kept for it, without being applied again", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    distinctAnswers(currentPc())
    api.sfuCallAnswerServerOffer.mockRejectedValueOnce(new RpcError(503, "WebSocket connection closed"))
    api.emitOffer(offer("X"))
    await flush()
    api.sfuCallResume.mockResolvedValueOnce({ listed: true, offer: offer("X") })

    api.emitReconnect()
    await flush()
    await flush()

    expect(answersSent(api)).toEqual([answerTo("X"), answerTo("X")])
    expect(currentPc().remoteSdps()).toEqual(["server-answer", "X"])
    await manager.leave()
})

test("the roster a resume returns applies like a roster event", async () => {
    const api = fakeApi()
    const { manager, joined } = recordedManager(api)
    await manager.join(fakeStream())
    api.sfuCallResume.mockResolvedValueOnce(
        resumeListed({ roster: { revision: nextRevision++, participants: [{ participantId: MINE, agentDid: ME }, { participantId: pid(ALICE), agentDid: ALICE }] } }),
    )

    api.emitReconnect()
    await flush()
    await flush()

    expect(joined).toEqual([ALICE])
    await manager.leave()
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

        expect(api.sfuCallLeave).toHaveBeenCalledTimes(4) // 3 rebuilds, then the end leaves the last session
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
    expect(api.sfuCallLeave).toHaveBeenCalledTimes(4) // 3 rebuilds, then the end leaves the last session

    await manager.leave()
    await manager.join(fakeStream())
    await iceState("failed")
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(6)
})

// ── Ask the executor before an unlisted rebuild (C5) ────────────────────────

test("a roster that leaves this session out does not rebuild while the executor still lists it", async () => {
    // One roster event can be stale or out of order; the resume's reply is current.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(10_000)

        expect(api.sfuCallResume).toHaveBeenCalledTimes(1)
        expect(api.sfuCallResume).toHaveBeenCalledWith(URL, ROOM, MINE)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
        expect(currentPc().closed).toBe(false)
    } finally {
        jest.useRealTimers()
    }
})

test("an unlisted session is rebuilt once the resume's reply confirms it is gone", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuCallResume.mockResolvedValue({ listed: false })
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)

        expect(api.sfuCallResume).toHaveBeenCalledTimes(1)
        expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
        expect(api.sfuCallResume.mock.invocationCallOrder[0]).toBeLessThan(api.sfuCallLeave.mock.invocationCallOrder[0])
    } finally {
        jest.useRealTimers()
    }
})

test("a resume the socket fails does not rebuild the call", async () => {
    // The socket is down: the server's state cannot be read, so nothing is torn down for it.
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuCallResume.mockRejectedValue(new RpcError(503, "WebSocket connection closed"))
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

test("a roster that lists this session while the resume is out cancels the rebuild", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const resuming = deferred<SfuResumeResult>()
        api.sfuCallResume.mockImplementation(() => resuming.promise)
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)
        api.emitRoster(roster([ALICE, ME]))
        await flush()
        resuming.resolve(resumeListed())
        await jest.advanceTimersByTimeAsync(1_000)

        expect(api.sfuCallLeave).not.toHaveBeenCalled()
    } finally {
        jest.useRealTimers()
    }
})

test("a resume refused with 403 ends the call: the session is another agent's", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        api.sfuCallResume.mockRejectedValue(new RpcError(403, "Session P1 belongs to another agent"))
        const { manager } = recordedManager(api)
        const ended: unknown[] = []
        manager.on("call-ended", (event: unknown) => ended.push(event))
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)

        // Ended, not rebuilt: the session is left, no new join goes out.
        expect(ended).toHaveLength(1)
        expect((ended[0] as { reason: string }).reason).toBe("refused")
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
        expect(manager.getState().participantId).toBeNull()
        expect(manager.getState().peerConnection).toBeNull()
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

test("an answer given up on that fails after a newer offer was answered goes out once and is refused, without a rebuild", async () => {
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

        // The first answer's call times out only now, so the manager keeps it
        // for the next socket; the server refuses it by its offer id.
        api.sfuCallAnswerServerOffer.mockResolvedValueOnce({ accepted: false, reason: "stale" })
        timeOut(new RpcError(408, "RPC call 'sfu.callAnswerServerOffer' timed out after 30000ms"))
        await flush()
        api.emitReconnect()
        await flush()
        expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledTimes(1)
        expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledWith(URL, ROOM, MINE, offer("first").offerId, expect.any(String))
        expect(api.sfuCallResume).toHaveBeenCalledTimes(1)
        expect(api.sfuCallLeave).not.toHaveBeenCalled()
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

test("a call left from a stream-removed handler during a rebuild is not joined again", async () => {
    // `clearMedia` emits stream-removed synchronously, after the rebuild's
    // generation check; an app may hang up there.
    const api = fakeApi()
    const { manager, removed } = recordedManager(api)
    await manager.join(fakeStream())
    api.emitRoster(roster([ALICE]))
    await flush()
    api.emitOffer(offer("tracks", [mapped("1", ALICE, "audio")]))
    await flush()
    currentPc().receive(new FakeTrack("audio"), "1")
    let left: Promise<void> | undefined
    manager.on("stream-removed", () => { left ??= manager.leave() })

    await iceState("failed")
    await left
    await flush()

    expect(removed).toHaveLength(1)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    expect(manager.getState().participantId).toBeNull()
    expect(manager.getState().peerConnection).toBeNull()
})

// ── Round 2: an answer is sent only for the offer it answers ────────────────

/** Answers that name the offer they answer, so a test can tell them apart. */
function distinctAnswers(pc: FakePeerConnection) {
    jest.spyOn(pc, "createAnswer").mockImplementation(async () => ({
        type: "answer",
        sdp: `answer-to-${pc.remoteSdps().at(-1)}`,
    }))
}
const answerTo = (sdp: string) => JSON.stringify({ type: "answer", sdp: `answer-to-${sdp}` })
const answersSent = (api: ReturnType<typeof fakeApi>) => (api.sfuCallAnswerServerOffer.mock.calls as unknown as string[][]).map((c) => c[4])

test("an answer the socket kept is dropped when a newer offer arrives; after the socket is back only the newer one goes out", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        distinctAnswers(currentPc())
        // A's answer is lost with the socket; B's answer is still out when the socket is back.
        let failB!: (e: unknown) => void
        api.sfuCallAnswerServerOffer
            .mockRejectedValueOnce(new RpcError(503, "WebSocket connection closed"))
            .mockImplementationOnce(() => new Promise((_, reject) => { failB = reject }))

        api.emitOffer(offer("A"))
        await flush()
        api.emitOffer(offer("B"))
        await jest.advanceTimersByTimeAsync(10_000)
        api.emitReconnect()
        await jest.advanceTimersByTimeAsync(0)
        expect(answersSent(api)).toEqual([answerTo("A"), answerTo("B")])

        // B's answer was lost too: the next socket gets B's, never A's.
        failB(new RpcError(503, "WebSocket connection closed"))
        await flush()
        // The executor never got B's answer: the resume's reply carries B again.
        api.sfuCallResume.mockResolvedValueOnce({ listed: true, offer: offer("B") })
        api.emitReconnect()
        await jest.advanceTimersByTimeAsync(0)
        expect(answersSent(api)).toEqual([answerTo("A"), answerTo("B"), answerTo("B")])
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
})

test("a same-mid ICE restart offer is never answered with the answer of the offer before it", async () => {
    // The server matches an answer to its pending offer by mids only: A's
    // answer would use up the restart's token.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        await iceState("connected")
        distinctAnswers(currentPc())
        api.sfuCallAnswerServerOffer
            .mockRejectedValueOnce(new RpcError(408, "RPC call 'sfu.callAnswerServerOffer' timed out after 30000ms"))
            .mockImplementationOnce(() => new Promise(() => {}))

        api.emitOffer(offer("renegotiate"))
        await flush()
        await iceState("disconnected")
        expect(api.sfuCallRestartIce).toHaveBeenCalledTimes(1)
        api.emitOffer(offer("restart"))
        await jest.advanceTimersByTimeAsync(10_000)
        api.emitReconnect()
        await jest.advanceTimersByTimeAsync(0)

        expect(answersSent(api)).toEqual([answerTo("renegotiate"), answerTo("restart")])
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
})

test("a stalled SDP step for one offer holds the next: a newer offer is applied after it, and the overtaken answer is refused", async () => {
    // The steps stay serial: A's step runs to its end before B's starts. A's
    // answer then goes out under A's offer id and the server refuses it
    // (`stale`): correlation is the id's, not the client's.
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        const pc = currentPc()
        distinctAnswers(pc)
        api.sfuCallAnswerServerOffer.mockResolvedValueOnce({ accepted: false, reason: "stale" })
        const setLocal = jest.spyOn(pc, "setLocalDescription")
        let releaseA!: () => void
        const realSetRemote = pc.setRemoteDescription.bind(pc)
        jest.spyOn(pc, "setRemoteDescription").mockImplementationOnce(
            (d: RTCSessionDescriptionInit) => new Promise<void>((resolve) => {
                releaseA = () => { void realSetRemote(d).then(resolve) }
            }),
        )

        api.emitOffer(offer("A"))
        await flush()
        await jest.advanceTimersByTimeAsync(1_000)
        api.emitOffer(offer("B"))
        await jest.advanceTimersByTimeAsync(20_000)
        // B's step has not touched the connection while A's is stalled.
        expect(setLocal).not.toHaveBeenCalled()
        releaseA()
        await jest.advanceTimersByTimeAsync(0)
        await flush()

        expect(answersSent(api)).toEqual([answerTo("A"), answerTo("B")])
        expect(setLocal.mock.calls.map(([d]) => d.sdp)).toEqual(["answer-to-A", "answer-to-B"])
        expect(pc.localDescription?.sdp).toBe("answer-to-B")
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
})

test("the answer RPC is still time-boxed: one that hangs lets the next offer go on after 10 s", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        const pc = currentPc()
        api.sfuCallAnswerServerOffer.mockImplementationOnce(() => new Promise(() => {}))
        api.emitOffer(offer("first"))
        await flush()
        api.emitOffer(offer("second"))
        await jest.advanceTimersByTimeAsync(9_999)
        expect(pc.remoteSdps()).toEqual(["server-answer", "first"])
        await jest.advanceTimersByTimeAsync(1)
        expect(pc.remoteSdps()).toEqual(["server-answer", "first", "second"])
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
})

test("an SDP step that never settles on a dropped connection does not hold the rebuilt connection's offers", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    jest.spyOn(currentPc(), "setRemoteDescription").mockImplementationOnce(() => new Promise<void>(() => {}))
    api.emitOffer(offer("stuck"))
    await flush()

    await iceState("failed")
    const rebuilt = currentPc()
    api.emitOffer(offer("after-rebuild"))
    await flush()

    expect(rebuilt.remoteSdps()).toEqual(["server-answer", "after-rebuild"])
    await manager.leave()
})

test("a call that gave up rebuilding ends: `call-ended` with reason gave-up, its connection closed and its session left", async () => {
    // Before, the manager kept a dead connection and only said `error`: the UI spun for ever.
    const api = fakeApi()
    const { manager, left } = recordedManager(api)
    const ended: unknown[] = []
    manager.on("call-ended", (event: unknown) => ended.push(event))
    await manager.join(fakeStream())
    api.emitRoster(roster([ALICE]))
    await flush()

    for (let i = 0; i < 4; i++) await iceState("failed")

    expect(ended).toEqual([{ reason: "gave-up", error: new Error("SFU reconnect attempts exhausted") }])
    expect(FakePeerConnection.instances.every((pc) => pc.closed)).toBe(true)
    expect(manager.getState().peerConnection).toBeNull()
    expect(manager.getState().participantId).toBeNull()
    // Three rebuilds left their sessions, and the end left the last one.
    expect(api.sfuCallLeave).toHaveBeenCalledTimes(4)
    expect(left).toEqual([ALICE])
    // Ended for good: no later signal rebuilds it.
    api.emitNetworkChange()
    api.emitReconnect()
    await flush()
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(4)
})

// ── Round 2: the resume's reply decides ─────────────────────────────────────

test("a resume that says the session is not listed rejoins the call", async () => {
    const api = fakeApi()
    api.sfuCallResume.mockResolvedValue({ listed: false })
    api.sfuCallJoin.mockResolvedValueOnce(session()).mockResolvedValueOnce(session({ participantId: "P2" }))
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    // The session is gone from the room: the last roster did not list it.
    api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
    await flush()

    api.emitReconnect()
    await flush()
    await flush()

    expect(api.sfuCallResume).toHaveBeenCalledWith(URL, ROOM, MINE)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    expect(manager.getState().participantId).toBe("P2")
    await manager.leave()
})

test("a resume that says the session is not listed rejoins though the last roster listed it", async () => {
    // The socket drop that took the session took the departure roster with
    // it, so the newest roster this client holds still lists it.
    const api = fakeApi()
    api.sfuCallResume.mockResolvedValue({ listed: false })
    api.sfuCallJoin.mockResolvedValueOnce(session()).mockResolvedValueOnce(session({ participantId: "P2" }))
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.emitRoster(roster([ALICE, ME]))
    await flush()

    api.emitReconnect()
    await flush()
    await flush()

    expect(api.sfuCallResume).toHaveBeenCalledWith(URL, ROOM, MINE)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
    expect(manager.getState().participantId).toBe("P2")
    await manager.leave()
})

test("a roster newer than the resume's ask that lists this session again cancels the listed:false rebuild", async () => {
    const api = fakeApi()
    const resuming = deferred<SfuResumeResult>()
    api.sfuCallResume.mockImplementation(() => resuming.promise)
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
    await flush()

    api.emitReconnect()
    await flush()
    // Newer than the ask, and listing again: the session is back.
    api.emitRoster(roster([ALICE, ME]))
    await flush()
    resuming.resolve({ listed: false })
    await flush()

    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    await manager.leave()
})

test("a resume that lists the session keeps the call", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())

    api.emitReconnect()
    await flush()
    await flush()

    expect(api.sfuCallResume).toHaveBeenCalledTimes(1)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    expect(manager.getState().participantId).toBe(MINE)
    await manager.leave()
})

test("a resume the socket fails (503) changes nothing: the next reconnect asks again", async () => {
    const api = fakeApi()
    api.sfuCallResume.mockRejectedValueOnce(new RpcError(503, "WebSocket connection closed"))
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())

    api.emitReconnect()
    await flush()
    await flush()
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)

    api.emitReconnect()
    await flush()
    await flush()
    expect(api.sfuCallResume).toHaveBeenCalledTimes(2)
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    await manager.leave()
})

test("a resume refused with 403 ends the call, not a rebuild: the session is another agent's", async () => {
    const api = fakeApi()
    api.sfuCallResume.mockRejectedValue(new RpcError(403, "Session P1 belongs to another agent"))
    const manager = new SfuManager(api, ROOM, ME, URL)
    const ended: unknown[] = []
    manager.on("call-ended", (event: unknown) => ended.push(event))
    await manager.join(fakeStream())

    api.emitReconnect()
    await flush()
    await flush()

    expect(ended).toHaveLength(1)
    expect((ended[0] as { reason: string }).reason).toBe("refused")
    expect(api.sfuCallJoin).toHaveBeenCalledTimes(1)
    expect(manager.getState().participantId).toBeNull()
})

// ── Round 2: resumes before a rebuild do not pile up ────────────────────────

test("a resume that answers after a roster listed this session again rebuilds nothing, and the next unlisted roster asks again", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const first = deferred<SfuResumeResult>()
        api.sfuCallResume.mockImplementationOnce(() => first.promise).mockResolvedValue({ listed: false })
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)
        api.emitRoster(roster([ALICE, ME]))
        await flush()
        first.resolve({ listed: false })
        await jest.advanceTimersByTimeAsync(0)
        // Listed meanwhile: the stale "not listed" does not rebuild.
        expect(api.sfuCallLeave).not.toHaveBeenCalled()

        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)
        expect(api.sfuCallResume).toHaveBeenCalledTimes(2)
        expect(api.sfuCallLeave).toHaveBeenCalledTimes(1)
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
})

test("a resume from before a rebuild is not used for the rebuilt session", async () => {
    fakeTimers()
    try {
        const api = fakeApi()
        const old = deferred<SfuResumeResult>()
        api.sfuCallResume.mockImplementationOnce(() => old.promise).mockResolvedValue(resumeListed())
        const manager = new SfuManager(api, ROOM, ME, URL)
        await manager.join(fakeStream())
        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)

        await iceState("failed")
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
        api.emitRoster(roster([ALICE], {}, { withoutMe: true }))
        await flush()
        await jest.advanceTimersByTimeAsync(3_000)
        old.resolve({ listed: false })
        await jest.advanceTimersByTimeAsync(0)

        expect(api.sfuCallResume).toHaveBeenCalledTimes(2)
        expect(api.sfuCallJoin).toHaveBeenCalledTimes(2)
        await manager.leave()
    } finally {
        jest.useRealTimers()
    }
})
