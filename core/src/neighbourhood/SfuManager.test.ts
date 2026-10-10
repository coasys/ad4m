/**
 * SfuManager unit tests: when the server-event subscriptions exist
 * relative to `sfuCallJoin`.
 *
 * The executor sends a socket only the events it watches, and the SDK
 * sends a pending watch ahead of the next call.  So the manager must
 * subscribe before `sfuCallJoin`, or an offer the join triggers can be
 * lost.  Offers that arrive before the join completes stay ignored, as
 * the peer connection cannot apply them yet.
 */

class FakePeerConnection {
    static instances: FakePeerConnection[] = []
    localDescription: RTCSessionDescriptionInit | null = null
    remoteDescriptions: RTCSessionDescriptionInit[] = []
    oniceconnectionstatechange: (() => void) | null = null
    onicecandidate: ((e: { candidate: null }) => void) | null = null
    ontrack: ((e: unknown) => void) | null = null
    closed = false

    constructor() {
        FakePeerConnection.instances.push(this)
    }
    addTransceiver() {}
    addTrack() {}
    async createOffer() { return { type: "offer", sdp: "local-offer" } }
    async createAnswer() { return { type: "answer", sdp: "local-answer" } }
    async setLocalDescription(d: RTCSessionDescriptionInit) { this.localDescription = d }
    async setRemoteDescription(d: RTCSessionDescriptionInit) { this.remoteDescriptions.push(d) }
    close() { this.closed = true }
}
;(globalThis as any).RTCPeerConnection = FakePeerConnection
;(globalThis as any).RTCSessionDescription = class {
    constructor(init: RTCSessionDescriptionInit) { Object.assign(this, init) }
}

import { SfuManager, type SfuNeighbourhoodApi } from "./SfuManager"
import type { CallSessionInfo, SfuCallRenegotiationOffer } from "./SfuTypes"

const URL = "neighbourhood://n"
const ROOM = "room"
const ME = "did:me"

function fakeStream(): MediaStream {
    return { getTracks: () => [] } as unknown as MediaStream
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

function offer(sdp: string): SfuCallRenegotiationOffer {
    return {
        targetDid: ME,
        neighbourhoodUrl: URL,
        roomName: ROOM,
        sdpOffer: JSON.stringify({ type: "offer", sdp }),
    }
}

/** A fake API that records the order of subscribe and RPC calls. */
function fakeApi() {
    const log: string[] = []
    const offerHandlers = new Set<(e: SfuCallRenegotiationOffer) => void>()
    const api = {
        log,
        offerHandlers,
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
        sfuAddIceCandidate: jest.fn(async () => true),
        sfuSendData: jest.fn(async () => true),
        subscribeSfuDataChannel: jest.fn(() => () => {}),
    }
    return api satisfies SfuNeighbourhoodApi & Record<string, unknown>
}

const flush = () => new Promise((resolve) => setTimeout(resolve, 0))

beforeEach(() => {
    FakePeerConnection.instances = []
})

test("subscribes to server events before sfuCallJoin", async () => {
    const api = fakeApi()
    await new SfuManager(api, ROOM, ME, URL).join(fakeStream())
    expect(api.log.slice(0, 2)).toEqual(["subscribe-offer", "sfuCallJoin"])
})

test("ignores offers before the join completes and answers them after", async () => {
    const api = fakeApi()
    let finishJoin!: (s: CallSessionInfo) => void
    api.sfuCallJoin.mockImplementation(() => new Promise((resolve) => { finishJoin = resolve }))
    const manager = new SfuManager(api, ROOM, ME, URL)
    const joined = manager.join(fakeStream())
    await flush()

    for (const handler of api.offerHandlers) handler(offer("early"))
    await flush()
    expect(api.sfuCallAnswerServerOffer).not.toHaveBeenCalled()

    finishJoin(session())
    await joined
    for (const handler of api.offerHandlers) handler(offer("late"))
    await flush()
    const pc = FakePeerConnection.instances[0]
    expect(pc.remoteDescriptions.map((d) => d.sdp)).toEqual(["server-answer", "late"])
    expect(api.sfuCallAnswerServerOffer).toHaveBeenCalledWith(
        URL,
        ROOM,
        JSON.stringify({ type: "answer", sdp: "local-answer" }),
    )
})

test("a failed join drops its subscriptions", async () => {
    const api = fakeApi()
    api.sfuCallJoin.mockRejectedValue(new Error("Not a member of this neighbourhood"))
    await expect(new SfuManager(api, ROOM, ME, URL).join(fakeStream())).rejects.toThrow("Not a member")
    expect(api.offerHandlers.size).toBe(0)
    expect(api.log).toContain("unsubscribe-offer")
})

test("a rejoin subscribes again before it releases the old subscriptions", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.log.length = 0
    await manager.join(fakeStream())
    expect(api.log).toEqual(["subscribe-offer", "unsubscribe-offer", "sfuCallJoin"])
    expect(api.offerHandlers.size).toBe(1)
})

test("a failed connection leaves, then joins this executor again", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    await manager.join(fakeStream())
    api.log.length = 0

    const failed = FakePeerConnection.instances[0] as any
    failed.iceConnectionState = "failed"
    failed.oniceconnectionstatechange()
    await flush()

    expect(failed.closed).toBe(true)
    expect(api.log).toEqual(["unsubscribe-offer", "sfuCallLeave", "subscribe-offer", "sfuCallJoin"])
    expect(api.sfuCallJoin).toHaveBeenLastCalledWith(URL, ROOM, expect.any(String))
    expect(FakePeerConnection.instances).toHaveLength(2)
})

/** Drive the manager's current connection to an ICE state. */
async function iceState(state: string) {
    const pc = FakePeerConnection.instances[FakePeerConnection.instances.length - 1] as any
    pc.iceConnectionState = state
    pc.oniceconnectionstatechange()
    await flush()
}

test("a rejoin that fails reports the error and closes its connection", async () => {
    const api = fakeApi()
    const manager = new SfuManager(api, ROOM, ME, URL)
    const errors: unknown[] = []
    manager.on("error", (e) => errors.push(e))
    await manager.join(fakeStream())
    api.sfuCallJoin.mockRejectedValue(new Error("Room is full"))

    await iceState("failed")

    expect(errors).toEqual([new Error("Room is full")])
    expect(FakePeerConnection.instances.every((pc) => pc.closed)).toBe(true)
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
