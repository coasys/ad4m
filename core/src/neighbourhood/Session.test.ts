/**
 * Session unit tests.
 *
 * Tests the `createSession` factory that wraps SfuManager (SFU path)
 * and MeshManager (mesh path) behind a unified interface.
 *
 * Both managers get mocked — their internal behaviour belongs to their
 * own test files.  What these tests exercise is the Session's topology
 * branching, state machine, event forwarding, roster polling, and
 * cleanup.
 */

// ── Module mocks — must be before imports ──────────────────────────

const mockSfuManagerInstance = {
    on: jest.fn(),
    join: jest.fn().mockResolvedValue(undefined),
    leave: jest.fn().mockResolvedValue(undefined),
    destroy: jest.fn().mockResolvedValue(undefined),
    getParticipants: jest.fn().mockReturnValue([]),
    setQualityPreference: jest.fn().mockResolvedValue(undefined),
    replaceTrack: jest.fn().mockResolvedValue(undefined),
    sendData: jest.fn().mockResolvedValue(undefined),
    subscribeDataChannel: jest.fn().mockReturnValue(() => {}),
}

jest.mock("./SfuManager", () => ({
    SfuManager: jest.fn().mockImplementation(() => mockSfuManagerInstance),
    defaultDeviceId: () => "this-device",
}))

const mockMeshManagerInstance = {
    on: jest.fn(),
    join: jest.fn().mockResolvedValue(undefined),
    leave: jest.fn().mockResolvedValue(undefined),
    destroy: jest.fn().mockResolvedValue(undefined),
    getParticipants: jest.fn().mockReturnValue([]),
    setRoster: jest.fn(),
    replaceTrack: jest.fn().mockResolvedValue(undefined),
    sendData: jest.fn(),
    subscribeDataChannel: jest.fn().mockReturnValue(() => {}),
}

jest.mock("./MeshManager", () => ({
    ...jest.requireActual("./MeshManager"),
    MeshManager: jest.fn().mockImplementation(() => mockMeshManagerInstance),
}))

// ── Imports ────────────────────────────────────────────────────────

import { createSession, type Session, type SessionState, type SessionEvent } from "./Session"
import { SfuManager } from "./SfuManager"
import { MeshManager, type SignallingChannel } from "./MeshManager"

// ── Helpers ────────────────────────────────────────────────────────

function fakeMediaStream(): MediaStream {
    return {
        getAudioTracks: () => [],
        getVideoTracks: () => [],
    } as unknown as MediaStream
}

function fakeApi() {
    return {
        sfuCallJoin: jest.fn(),
        sfuCallLeave: jest.fn(),
        sfuCallSetQualityPreference: jest.fn(),
        sfuCallAnswerServerOffer: jest.fn(),
        sfuCallRestartIce: jest.fn(),
        onNetworkChange: jest.fn().mockReturnValue(() => {}),
        checkConnection: jest.fn(),
        subscribeSfuCallRenegotiationOffer: jest.fn(),
        sfuAddIceCandidate: jest.fn(),
        sfuSendData: jest.fn(),
        subscribeSfuDataChannel: jest.fn(),
        subscribeSfuRoomRoster: jest.fn(),
        subscribeSfuSessionReplaced: jest.fn(),
    }
}

function fakeChannel(): SignallingChannel {
    return {
        publish: jest.fn(),
        onMessage: jest.fn().mockReturnValue(() => {}),
    }
}

function baseConfig(overrides: Record<string, unknown> = {}) {
    return {
        api: fakeApi(),
        roomId: "test-room",
        agentDid: "did:test",
        neighbourhoodUrl: "neighbourhood://test",
        topology: "auto" as const,
        ...overrides,
    }
}

/** The beat link a mesh device writes beside its presence link: its peer id, and `<room>#<beat>`. */
function beatOf(peer: string, room: string) {
    return { source: peer, predicate: "ad4m://session/in-call-beat", target: expect.stringMatching(new RegExp(`^${room}#\\d+$`)) }
}

function collectStates(session: Session): SessionState[] {
    const states: SessionState[] = []
    session.on("state-changed", (s: SessionState) => states.push(s))
    return states
}

// ── Tests ──────────────────────────────────────────────────────────

beforeEach(() => {
    jest.clearAllMocks()
    // Reset the mock return values to defaults
    mockSfuManagerInstance.getParticipants.mockReturnValue([])
    mockMeshManagerInstance.getParticipants.mockReturnValue([])
    mockSfuManagerInstance.join.mockResolvedValue(undefined)
    mockMeshManagerInstance.join.mockResolvedValue(undefined)
    mockSfuManagerInstance.leave.mockResolvedValue(undefined)
    mockMeshManagerInstance.leave.mockResolvedValue(undefined)
    mockSfuManagerInstance.destroy.mockResolvedValue(undefined)
    mockMeshManagerInstance.destroy.mockResolvedValue(undefined)
    mockSfuManagerInstance.subscribeDataChannel.mockReturnValue(() => {})
    mockMeshManagerInstance.subscribeDataChannel.mockReturnValue(() => {})
})

describe("topology resolution", () => {
    it("resolves 'sfu' to SFU path", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        expect(SfuManager).toHaveBeenCalled()
        expect(MeshManager).not.toHaveBeenCalled()
    })

    // Apps show which topology a call runs on from this event, mesh and SFU alike.
    it("emits topology-changed with 'sfu' once the SFU join completes", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        const events: unknown[] = []
        session.on("topology-changed", (topo: unknown) => events.push(topo))
        await session.join(fakeMediaStream())
        expect(events).toEqual(["sfu"])
    })

    it("resolves 'mesh' to mesh path", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        expect(MeshManager).toHaveBeenCalled()
        expect(SfuManager).not.toHaveBeenCalled()
    })

    it("resolves 'auto' with sfuConfig.mode='mesh' to mesh path", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            sfuConfig: { mode: "mesh", fallback: "mesh", maxMeshParticipants: 6, sfuPeers: [] },
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        expect(MeshManager).toHaveBeenCalled()
    })

    // "auto" must not split a call: the SFU path reaches only this executor's
    // SFU, so it is right only when every member is hosted here.
    const PUBLIC_SFU = { reachability: "public", isPublic: true, bindAddress: "1.2.3.4:0", detail: "" }

    it("resolves 'auto' to SFU when the local SFU is public and hosts every member", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            localSfuStatus: PUBLIC_SFU,
            sfuServesAllMembers: async () => true,
        }))
        await session.join(fakeMediaStream())
        expect(SfuManager).toHaveBeenCalled()
        expect(MeshManager).not.toHaveBeenCalled()
    })

    it("resolves 'auto' to mesh when a member is hosted elsewhere, even with a public SFU", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            localSfuStatus: PUBLIC_SFU,
            sfuServesAllMembers: async () => false,
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        expect(MeshManager).toHaveBeenCalled()
        expect(SfuManager).not.toHaveBeenCalled()
    })

    it("resolves 'auto' to mesh when the executor cannot say who it hosts", async () => {
        const consoleWarn = jest.spyOn(console, "warn").mockImplementation()
        const session = createSession(baseConfig({
            topology: "auto",
            localSfuStatus: PUBLIC_SFU,
            sfuServesAllMembers: async () => { throw new Error("rpc failed") },
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        expect(MeshManager).toHaveBeenCalled()
        expect(SfuManager).not.toHaveBeenCalled()
        consoleWarn.mockRestore()
    })

    it("resolves 'auto' to mesh when the local SFU is not public", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            localSfuStatus: { ...PUBLIC_SFU, reachability: "nat", isPublic: false },
            sfuServesAllMembers: async () => true,
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        expect(MeshManager).toHaveBeenCalled()
        expect(SfuManager).not.toHaveBeenCalled()
    })

    it("does not advertise its SFU in call presence", async () => {
        const setOnlineStatus = jest.fn().mockResolvedValue(true)
        const session = createSession(baseConfig({
            topology: "mesh",
            localSfuStatus: PUBLIC_SFU,
            channel: fakeChannel(),
            setOnlineStatus,
        }))
        await session.join(fakeMediaStream())
        const links = setOnlineStatus.mock.calls.flatMap((c: any[]) => c[0].links)
        expect(links.map((l: any) => l.predicate)).not.toContain("ad4m://sfu/available")
    })

    it("resolves 'auto' with sfuConfig.mode='designated' to SFU path", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            sfuConfig: { mode: "designated", fallback: "mesh", maxMeshParticipants: 6, sfuPeers: [] },
        }))
        await session.join(fakeMediaStream())
        expect(SfuManager).toHaveBeenCalled()
        expect(MeshManager).not.toHaveBeenCalled()
    })
})

describe("mesh path", () => {
    it("throws when mesh topology has no channel", async () => {
        const session = createSession(baseConfig({ topology: "mesh" }))
        await expect(session.join(fakeMediaStream())).rejects.toThrow(/signalling channel/)
    })

    it("creates MeshManager with channel, callId, and selfId", async () => {
        const channel = fakeChannel()
        const session = createSession(baseConfig({
            topology: "mesh",
            channel,
            roomId: "my-room",
            agentDid: "did:me",
        }))
        await session.join(fakeMediaStream())

        expect(MeshManager).toHaveBeenCalledWith({
            channel,
            callId: "my-room",
            // A mesh peer is the agent on this device.
            selfId: "did:me#this-device",
            onNetworkChange: expect.any(Function),
            checkConnection: expect.any(Function),
        })
    })

    it("calls mesh.join with the local stream", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        const stream = fakeMediaStream()
        await session.join(stream)
        expect(mockMeshManagerInstance.join).toHaveBeenCalledWith(stream)
    })

    it("emits topology-changed with 'mesh' on join", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        const events: [string, unknown][] = []
        session.on("topology-changed", (...args: unknown[]) => events.push(["topology-changed", args[0]]))
        await session.join(fakeMediaStream())
        expect(events).toEqual([["topology-changed", "mesh"]])
    })

    it("announces call-presence via setOnlineStatus", async () => {
        const setOnlineStatus = jest.fn().mockResolvedValue(true)
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            setOnlineStatus,
            roomId: "my-room",
            agentDid: "did:me",
        }))
        await session.join(fakeMediaStream())

        expect(setOnlineStatus).toHaveBeenCalledWith({
            links: [
                { source: "did:me#this-device", predicate: "ad4m://session/in-call", target: "my-room" },
                beatOf("did:me#this-device", "my-room"),
            ],
        })
    })

    // A call's presence is one link in the agent's status. The app's own links
    // must survive the call, and the presence must go when the call ends.
    describe("call presence in the agent's status", () => {
        const appLink = { source: "did:me", predicate: "app://mood", target: "literal:string:busy" }
        const inCall = { source: "did:me#this-device", predicate: "ad4m://session/in-call", target: "my-room" }
        const phoneInCall = { source: "did:me#phone", predicate: "ad4m://session/in-call", target: "my-room" }
        const beat = beatOf("did:me#this-device", "my-room")
        const statusWith = (...links: object[]) => ({ data: { links: links.map((data) => ({ data })) } })

        function presenceSession(ownOnlineStatus: jest.Mock) {
            const setOnlineStatus = jest.fn().mockResolvedValue(true)
            const session = createSession(baseConfig({
                topology: "mesh",
                channel: fakeChannel(),
                setOnlineStatus,
                ownOnlineStatus,
                roomId: "my-room",
                agentDid: "did:me",
            }))
            return { session, setOnlineStatus }
        }

        it("adds the presence link to the app's links on join", async () => {
            const { session, setOnlineStatus } = presenceSession(jest.fn().mockResolvedValue(statusWith(appLink)))
            await session.join(fakeMediaStream())
            expect(setOnlineStatus).toHaveBeenLastCalledWith({ links: [appLink, inCall, beat] })
        })

        it("takes the presence link out on leave and keeps the app's links", async () => {
            const own = jest.fn().mockResolvedValue(statusWith(appLink))
            const { session, setOnlineStatus } = presenceSession(own)
            await session.join(fakeMediaStream())
            own.mockResolvedValue(statusWith(appLink, inCall))
            await session.leave()
            expect(setOnlineStatus).toHaveBeenLastCalledWith({ links: [appLink] })
        })

        it("adds its link beside another device's, and leaving takes out only its own", async () => {
            const own = jest.fn().mockResolvedValue(statusWith(appLink, phoneInCall))
            const { session, setOnlineStatus } = presenceSession(own)
            await session.join(fakeMediaStream())
            expect(setOnlineStatus).toHaveBeenLastCalledWith({ links: [appLink, phoneInCall, inCall, beat] })
            own.mockResolvedValue(statusWith(appLink, phoneInCall, inCall))
            await session.leave()
            expect(setOnlineStatus).toHaveBeenLastCalledWith({ links: [appLink, phoneInCall] })
        })

        it("takes the presence link out on destroy", async () => {
            const own = jest.fn().mockResolvedValue(statusWith(appLink))
            const { session, setOnlineStatus } = presenceSession(own)
            await session.join(fakeMediaStream())
            own.mockResolvedValue(statusWith(appLink, inCall))
            await session.destroy()
            expect(setOnlineStatus).toHaveBeenLastCalledWith({ links: [appLink] })
        })

        it("restores the links it saw at join when the status cannot be read on leave", async () => {
            const consoleWarn = jest.spyOn(console, "warn").mockImplementation()
            const own = jest.fn().mockResolvedValue(statusWith(appLink))
            const { session, setOnlineStatus } = presenceSession(own)
            await session.join(fakeMediaStream())
            own.mockRejectedValue(new Error("rpc failed"))
            await session.leave()
            expect(setOnlineStatus).toHaveBeenLastCalledWith({ links: [appLink] })
            consoleWarn.mockRestore()
        })
    })

    it("starts roster polling on join and stops on leave", async () => {
        jest.useFakeTimers()

        const onlineAgents = jest.fn().mockResolvedValue([])
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            onlineAgents,
        }))
        await session.join(fakeMediaStream())

        // Immediate poll
        expect(onlineAgents).toHaveBeenCalledTimes(1)

        // Periodic poll
        jest.advanceTimersByTime(3_000)
        // Need to flush the async
        await Promise.resolve()
        expect(onlineAgents).toHaveBeenCalledTimes(2)

        await session.leave()

        // No more polls after leave
        jest.advanceTimersByTime(6_000)
        await Promise.resolve()
        expect(onlineAgents).toHaveBeenCalledTimes(2)

        jest.useRealTimers()
    })

    describe("the roster", () => {
        // A status is a signed perspective: its links sit under `data`.
        const statusOf = (did: string, ...links: { source: string; target: string }[]) => ({
            author: did,
            timestamp: "2026-01-01T00:00:00Z",
            data: {
                links: links.map(({ source, target }) => ({
                    author: did,
                    timestamp: "2026-01-01T00:00:00Z",
                    data: { source, predicate: "ad4m://session/in-call", target },
                })),
            },
        })
        const agent = (did: string, ...links: { source: string; target: string }[]) => ({ did, status: statusOf(did, ...links) })

        async function rosterOf(agents: unknown[], own: unknown = null) {
            const session = createSession(baseConfig({
                topology: "mesh",
                channel: fakeChannel(),
                onlineAgents: jest.fn().mockResolvedValue(agents),
                ownOnlineStatus: jest.fn().mockResolvedValue(own),
                roomId: "my-room",
                agentDid: "did:me",
            }))
            await session.join(fakeMediaStream())
            // Wait for the async pollRoster to settle
            await new Promise((r) => setTimeout(r, 10))
            return mockMeshManagerInstance.setRoster.mock.calls.at(-1)?.[0]
        }

        it("is each device in this room, of every agent", async () => {
            const roster = await rosterOf([
                agent("did:bob", { source: "did:bob#desk", target: "my-room" }, { source: "did:bob#phone", target: "my-room" }),
                agent("did:carol", { source: "did:carol#desk", target: "other-room" }),
            ])
            expect(roster).toEqual(["did:bob#desk", "did:bob#phone"])
        })

        it("holds this agent's other devices, read from its own status, never this one", async () => {
            const own = statusOf("did:me", { source: "did:me#this-device", target: "my-room" }, { source: "did:me#phone", target: "my-room" })
            // An executor may or may not list the caller; the roster is the same either way.
            expect(await rosterOf([], own)).toEqual(["did:me#phone"])
            expect(await rosterOf([{ did: "did:me", status: own }], own)).toEqual(["did:me#phone"])
        })

        it("takes no agent's word for another agent's device, nor a link that names no device", async () => {
            const roster = await rosterOf([
                agent("did:carol", { source: "did:bob#desk", target: "my-room" }, { source: "did:carol", target: "my-room" }),
            ])
            expect(roster).toEqual([])
        })
    })

    // Two tabs of one device are one mesh peer, so only one of them can hold the call.
    describe("joined again from this device", () => {
        const delivered = () => new Promise((r) => setTimeout(r, 20))

        function tab(overrides: Record<string, unknown> = {}) {
            const setOnlineStatus = jest.fn().mockResolvedValue(true)
            const session = createSession(baseConfig({
                topology: "mesh",
                channel: fakeChannel(),
                setOnlineStatus,
                // A room no other test's session is still in: tabs of one device hear each other per room.
                roomId: "tab-room",
                agentDid: "did:me",
                ...overrides,
            }))
            const replaced: unknown[] = []
            session.on("replaced", (event: unknown) => replaced.push(event))
            return { session, setOnlineStatus, replaced }
        }

        it("hands the call to the later tab: the earlier says replaced, goes idle and leaves the presence link", async () => {
            const a = tab()
            await a.session.join(fakeMediaStream())
            const announced = a.setOnlineStatus.mock.calls.length
            const b = tab()
            await b.session.join(fakeMediaStream())
            await delivered()

            expect(a.replaced).toEqual([{
                targetDid: "did:me", neighbourhoodUrl: "neighbourhood://test", roomName: "tab-room", participantId: "did:me#this-device",
            }])
            expect(a.session.getState()).toBe("idle")
            expect(mockMeshManagerInstance.leave).toHaveBeenCalledTimes(1)
            // The link is the device's, and the later tab holds the call on it.
            expect(a.setOnlineStatus).toHaveBeenCalledTimes(announced)
            expect(b.replaced).toEqual([])
            expect(b.session.getState()).toBe("active")

            await Promise.all([a.session.destroy(), b.session.destroy()])
        })

        it("keeps the presence link for the later tab when the earlier is replaced and then destroyed while it announces", async () => {
            let finishAnnounce: (ok: boolean) => void = () => {}
            // The announce waits until the test lets it finish; any later write settles at once.
            const aSetStatus = jest.fn(() => Promise.resolve(true))
                .mockImplementationOnce(() => new Promise<boolean>((resolve) => { finishAnnounce = resolve }))
            const a = tab({ setOnlineStatus: aSetStatus })
            const aJoin = a.session.join(fakeMediaStream()).catch((err: unknown) => err)
            await delivered()
            const b = tab()
            await b.session.join(fakeMediaStream())
            await delivered()
            expect(a.replaced).toHaveLength(1)

            await a.session.destroy()
            finishAnnounce(true)
            await aJoin

            // Only the announce: no withdraw of the link the later tab holds the call on.
            expect(aSetStatus).toHaveBeenCalledTimes(1)
            expect(b.session.getState()).toBe("active")
            await b.session.destroy()
        })

        it("leaves a call on another device alone", async () => {
            const a = tab()
            await a.session.join(fakeMediaStream())
            const b = tab({ deviceId: "phone" })
            await b.session.join(fakeMediaStream())
            await delivered()

            expect(a.replaced).toEqual([])
            expect(a.session.getState()).toBe("active")

            await Promise.all([a.session.destroy(), b.session.destroy()])
        })

        it("lets a tab that left be joined from again without replacing anyone", async () => {
            const a = tab()
            await a.session.join(fakeMediaStream())
            await a.session.leave()
            const b = tab()
            await b.session.join(fakeMediaStream())
            await delivered()

            expect(a.replaced).toEqual([])
            expect(b.session.getState()).toBe("active")

            await Promise.all([a.session.destroy(), b.session.destroy()])
        })
    })

    it("delegates participants to mesh manager (includes stream)", async () => {
        const bobStream = fakeMediaStream()
        mockMeshManagerInstance.getParticipants.mockReturnValue([
            { did: "did:bob#desk", stream: bobStream, hasAudio: true, hasVideo: false, isActiveSpeaker: false },
        ])
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())

        // The mesh keys its peers by device: the peer id is the participant id.
        expect(session.participants).toEqual([
            { participantId: "did:bob#desk", agentDid: "did:bob", stream: bobStream, hasAudio: true, hasVideo: false, isActiveSpeaker: false },
        ])
    })

    it("reports a mesh participant's events as the SFU path does: the device's session and its agent", async () => {
        const session = createSession(baseConfig({ topology: "mesh", channel: fakeChannel() }))
        await session.join(fakeMediaStream())
        const heard: [string, unknown][] = []
        for (const event of ["participant-joined", "participant-left", "participant-updated"] as const) {
            session.on(event, (p: unknown) => heard.push([event, p]))
        }
        const stream = fakeMediaStream()
        const bob = { did: "did:bob#desk", stream, hasAudio: true, hasVideo: true, isActiveSpeaker: false }
        for (const [, handler] of mockMeshManagerInstance.on.mock.calls) handler(bob)

        const reported = { participantId: "did:bob#desk", did: "did:bob", stream, hasAudio: true, hasVideo: true, isActiveSpeaker: false }
        expect(heard).toEqual([
            ["participant-joined", reported],
            ["participant-left", reported],
            ["participant-updated", reported],
        ])
    })

    it("delegates replaceTrack to mesh manager", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())

        const fakeTrack = { kind: "audio" } as MediaStreamTrack
        await session.replaceTrack("audio", fakeTrack)
        expect(mockMeshManagerInstance.replaceTrack).toHaveBeenCalledWith("audio", fakeTrack)
    })

    it("setQualityPreference silently returns on mesh", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())

        // Should not throw — mesh has no simulcast layers.
        await session.setQualityPreference("high")
    })

    it("delegates sendData to mesh manager", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())

        await session.sendData("chat", "hello", false)
        expect(mockMeshManagerInstance.sendData).toHaveBeenCalledWith("chat", "hello", false)
    })

    it("calls mesh.leave on leave()", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        await session.leave()
        expect(mockMeshManagerInstance.leave).toHaveBeenCalled()
    })

    it("calls mesh.destroy on destroy()", async () => {
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        await session.destroy()
        expect(mockMeshManagerInstance.leave).toHaveBeenCalled()
        expect(mockMeshManagerInstance.destroy).toHaveBeenCalled()
    })

    it("stays closed when destroyed while the mesh joins", async () => {
        jest.useFakeTimers()
        try {
            let listened!: () => void
            mockMeshManagerInstance.join.mockImplementationOnce(
                () => new Promise<void>((resolve) => { listened = resolve }),
            )
            const setOnlineStatus = jest.fn().mockResolvedValue(true)
            const onlineAgents = jest.fn().mockResolvedValue([])
            const session = createSession(baseConfig({
                topology: "mesh",
                channel: fakeChannel(),
                setOnlineStatus,
                onlineAgents,
            }))
            const states = collectStates(session)

            const joining = session.join(fakeMediaStream())
            const outcome = joining.then(() => null, (err: Error) => err)
            await Promise.resolve()
            await session.destroy()
            listened()

            expect((await outcome)?.message).toBe("Session destroyed")
            expect(session.getState()).toBe("closed")
            expect(states).not.toContain("active")
            expect(setOnlineStatus).not.toHaveBeenCalled()
            expect(onlineAgents).not.toHaveBeenCalled()
            expect(jest.getTimerCount()).toBe(0)
        } finally {
            jest.useRealTimers()
        }
    })

    it("withdraws its presence when destroyed while announcing it", async () => {
        let announced!: () => void
        const setOnlineStatus = jest.fn()
            .mockImplementationOnce(() => new Promise<boolean>((resolve) => { announced = () => resolve(true) }))
            .mockResolvedValue(true)
        const onlineAgents = jest.fn().mockResolvedValue([])
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            setOnlineStatus,
            onlineAgents,
            roomId: "my-room",
            agentDid: "did:me",
        }))

        const outcome = session.join(fakeMediaStream()).then(() => null, (err: Error) => err)
        while (setOnlineStatus.mock.calls.length === 0) await Promise.resolve()
        await session.destroy()
        announced()

        expect((await outcome)?.message).toBe("Session destroyed")
        expect(session.getState()).toBe("closed")
        expect(setOnlineStatus).toHaveBeenLastCalledWith({ links: [] })
        expect(onlineAgents).not.toHaveBeenCalled()
    })
})

describe("SFU path", () => {
    it("creates SfuManager with correct arguments", async () => {
        const session = createSession(baseConfig({
            topology: "sfu",
            roomId: "sfu-room",
            agentDid: "did:sfu-test",
            neighbourhoodUrl: "neighbourhood://sfu",
        }))
        await session.join(fakeMediaStream())
        expect(SfuManager).toHaveBeenCalledWith(
            expect.anything(), // api
            "sfu-room",
            "did:sfu-test",
            "neighbourhood://sfu",
            undefined, // iceConfig
            undefined, // sfuConfig
            "this-device", // deviceId: the SDK's default
        )
    })

    it("passes the app's device id to the SFU manager", async () => {
        const session = createSession(baseConfig({ topology: "sfu", deviceId: "electron-main" }))
        await session.join(fakeMediaStream())
        expect((SfuManager as unknown as jest.Mock).mock.calls[0][6]).toBe("electron-main")
    })

    it("lists each session of an agent as its own participant", async () => {
        const laptop = fakeMediaStream()
        const phone = fakeMediaStream()
        mockSfuManagerInstance.getParticipants.mockReturnValue([
            { participantId: "P2", did: "did:bob", stream: laptop, hasAudio: true, hasVideo: false, isActiveSpeaker: false },
            { participantId: "P3", did: "did:bob", stream: phone, hasAudio: true, hasVideo: false, isActiveSpeaker: false },
        ])
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())

        expect(session.participants.map((p) => [p.participantId, p.agentDid, p.stream])).toEqual([
            ["P2", "did:bob", laptop],
            ["P3", "did:bob", phone],
        ])
    })

    it("a replaced call says so and goes idle without leaving, and can be joined again", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        const dataUnsubscribe = jest.fn()
        mockSfuManagerInstance.subscribeDataChannel.mockReturnValue(dataUnsubscribe)
        await session.join(fakeMediaStream())
        const states = collectStates(session)
        const replacedEvents: unknown[] = []
        session.on("replaced", (event: unknown) => replacedEvents.push(event))

        const event = { targetDid: "did:test", neighbourhoodUrl: "neighbourhood://test", roomName: "test-room", participantId: "P1" }
        for (const [name, handler] of mockSfuManagerInstance.on.mock.calls) if (name === "replaced") handler(event)

        expect(replacedEvents).toEqual([event])
        expect(states).toEqual(["idle"])
        expect(dataUnsubscribe).toHaveBeenCalled()
        expect(mockSfuManagerInstance.leave).not.toHaveBeenCalled()
        expect(session.participants).toEqual([])

        await session.join(fakeMediaStream())
        expect(session.getState()).toBe("active")
        expect(SfuManager).toHaveBeenCalledTimes(2)
    })

    it("a replaced call is idle by the time its listeners hear it, so a listener may join again", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        let rejoin: Promise<unknown> = Promise.resolve()
        let stateHeard: string | null = null
        session.on("replaced", () => {
            stateHeard = session.getState()
            rejoin = session.join(fakeMediaStream()).then(() => "joined", (e: Error) => e.message)
        })

        const event = { targetDid: "did:test", neighbourhoodUrl: "neighbourhood://test", roomName: "test-room", participantId: "P1" }
        for (const [name, handler] of mockSfuManagerInstance.on.mock.calls) if (name === "replaced") handler(event)

        expect(await rejoin).toBe("joined")
        expect(stateHeard).toBe("idle")
        expect(session.getState()).toBe("active")
    })

    it("a replaced call's manager no longer reaches the session's listeners", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        const heard = jest.fn()
        session.on("error", heard)
        session.on("participant-left", heard)
        const handlers = [...mockSfuManagerInstance.on.mock.calls]
        const event = { targetDid: "did:test", neighbourhoodUrl: "neighbourhood://test", roomName: "test-room", participantId: "P1" }
        for (const [name, handler] of handlers) if (name === "replaced") handler(event)

        // A sendData or quality call made before the replacement rejects after it.
        for (const [name, handler] of handlers) if (name === "error") handler(new Error("late"))
        for (const [name, handler] of handlers) if (name === "participant-left") handler("did:other")
        expect(heard).not.toHaveBeenCalled()
    })

    it("a replaced call's late error does not reach the next call's listeners", async () => {
        // Each join gets its own manager, so the first one's handlers stay bound to it.
        const first = { ...mockSfuManagerInstance, on: jest.fn() }
        ;(SfuManager as unknown as jest.Mock).mockImplementationOnce(() => first)
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        const event = { targetDid: "did:test", neighbourhoodUrl: "neighbourhood://test", roomName: "test-room", participantId: "P1" }
        for (const [name, handler] of first.on.mock.calls) if (name === "replaced") handler(event)
        await session.join(fakeMediaStream())
        expect(session.getState()).toBe("active")

        const heard = jest.fn()
        session.on("error", heard)
        for (const [name, handler] of first.on.mock.calls) if (name === "error") handler(new Error("late"))
        expect(heard).not.toHaveBeenCalled()
        // The current manager's errors still arrive.
        for (const [name, handler] of mockSfuManagerInstance.on.mock.calls) if (name === "error") handler(new Error("now"))
        expect(heard).toHaveBeenCalledTimes(1)
    })

    it("a call replaced while it joins stays idle", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        const event = { targetDid: "did:test", neighbourhoodUrl: "neighbourhood://test", roomName: "test-room", participantId: "P1" }
        mockSfuManagerInstance.join.mockImplementationOnce(async () => {
            for (const [name, handler] of mockSfuManagerInstance.on.mock.calls) if (name === "replaced") handler(event)
        })
        const states = collectStates(session)

        await session.join(fakeMediaStream())

        expect(states).toEqual(["joining", "idle"])
        expect(session.getState()).toBe("idle")
    })

    it("a session destroyed while its topology resolves never joins", async () => {
        let serves!: (all: boolean) => void
        const session = createSession(baseConfig({
            localSfuStatus: { isPublic: true },
            sfuServesAllMembers: () => new Promise<boolean>((resolve) => { serves = resolve }),
        }))
        const joining = session.join(fakeMediaStream())
        await session.destroy()
        serves(true)

        await expect(joining).rejects.toThrow(/destroyed/)
        expect(SfuManager).not.toHaveBeenCalled()
        expect(session.getState()).toBe("closed")
    })

    it("a session destroyed while the SFU joins never turns active", async () => {
        let joined!: () => void
        mockSfuManagerInstance.join.mockImplementationOnce(
            () => new Promise<void>((resolve) => { joined = resolve }),
        )
        const session = createSession(baseConfig({ topology: "sfu" }))
        const states = collectStates(session)
        const outcome = session.join(fakeMediaStream()).then(() => null, (err: Error) => err)
        while (mockSfuManagerInstance.join.mock.calls.length === 0) await Promise.resolve()

        const destroying = session.destroy()
        joined()
        await destroying

        expect((await outcome)?.message).toBe("Session destroyed")
        expect(states).not.toContain("active")
        expect(session.getState()).toBe("closed")
    })

    it("calls sfu.join with the local stream", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        const stream = fakeMediaStream()
        await session.join(stream)
        expect(mockSfuManagerInstance.join).toHaveBeenCalledWith(stream)
    })

    it("delegates participants to SFU manager (includes stream)", async () => {
        const bobStream = fakeMediaStream()
        mockSfuManagerInstance.getParticipants.mockReturnValue([
            { participantId: "P2", did: "did:bob", stream: bobStream, hasAudio: true, hasVideo: true, isActiveSpeaker: true },
        ])
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())

        expect(session.participants).toEqual([
            { participantId: "P2", agentDid: "did:bob", stream: bobStream, hasAudio: true, hasVideo: true, isActiveSpeaker: true },
        ])
    })

    it("delegates replaceTrack to SFU manager", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())

        await session.replaceTrack("video", null)
        expect(mockSfuManagerInstance.replaceTrack).toHaveBeenCalledWith("video", null)
    })

    it("delegates setQualityPreference to SFU manager", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        await session.setQualityPreference("low")
        expect(mockSfuManagerInstance.setQualityPreference).toHaveBeenCalledWith("low")
    })

    it("delegates sendData to SFU manager", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        await session.sendData("chat", "hello", true)
        expect(mockSfuManagerInstance.sendData).toHaveBeenCalledWith("chat", "hello", true)
    })

    it("hands each ICE server to the SFU path as given", async () => {
        // A provider's entry lists STUN and TURN URLs under one credential;
        // splitting it left TURN URLs without one, which WebRTC refuses.
        const session = createSession(baseConfig({
            topology: "sfu",
            iceServers: [
                { urls: ["stun:stun.example.com:3478"] },
                {
                    urls: ["stun:relay.example.com:3478", "turn:relay.example.com:3478", "turns:relay.example.com:5349"],
                    username: "user",
                    credential: "pass",
                },
            ],
        }))
        await session.join(fakeMediaStream())

        const iceConfig = (SfuManager as unknown as jest.Mock).mock.calls[0][4]
        expect(iceConfig.servers).toEqual([
            { urls: ["stun:stun.example.com:3478"] },
            {
                urls: ["stun:relay.example.com:3478", "turn:relay.example.com:3478", "turns:relay.example.com:5349"],
                username: "user",
                credential: "pass",
            },
        ])
    })
})

describe("state machine", () => {
    it("starts in idle state", () => {
        const session = createSession(baseConfig())
        expect(session.getState()).toBe("idle")
    })

    it("transitions idle → joining → active on join", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        const states = collectStates(session)
        await session.join(fakeMediaStream())
        expect(states).toEqual(["joining", "active"])
        expect(session.getState()).toBe("active")
    })

    it("transitions active → leaving → idle on leave", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        const states = collectStates(session)
        await session.leave()
        expect(states).toEqual(["leaving", "idle"])
        expect(session.getState()).toBe("idle")
    })

    it("leave() does nothing when not active", async () => {
        const session = createSession(baseConfig())
        const states = collectStates(session)
        await session.leave()
        expect(states).toEqual([])
    })

    it("throws on join when destroyed", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.destroy()
        await expect(session.join(fakeMediaStream())).rejects.toThrow(/destroyed/)
    })

    it("throws on join when already active", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        await expect(session.join(fakeMediaStream())).rejects.toThrow(/already active/)
    })

    it("transitions to closed on destroy", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        const states = collectStates(session)
        await session.destroy()
        expect(states).toEqual(["closed"])
        expect(session.getState()).toBe("closed")
    })

    it("calls leave before destroy if active", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())
        const states = collectStates(session)
        await session.destroy()
        expect(states).toEqual(["leaving", "idle", "closed"])
    })
})

describe("event subscriptions", () => {
    it("on() registers and off() removes by callback", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        let calls = 0
        const cb = () => calls++
        session.on("state-changed", cb)
        await session.join(fakeMediaStream())
        expect(calls).toBe(2) // joining + active

        session.off("state-changed", cb)
        await session.leave()
        expect(calls).toBe(2) // no more increments
    })

    it("off() with no callback removes all listeners for that event", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        let a = 0, b = 0
        session.on("state-changed", () => a++)
        session.on("state-changed", () => b++)
        session.off("state-changed")
        await session.join(fakeMediaStream())
        expect(a).toBe(0)
        expect(b).toBe(0)
    })
})

describe("track subscriptions", () => {
    it("onTrack returns an unsubscribe function", () => {
        const session = createSession(baseConfig())
        let calls = 0
        const unsub = session.onTrack(() => calls++)
        expect(typeof unsub).toBe("function")
        unsub()
    })
})

describe("data subscriptions", () => {
    it("onData returns an unsubscribe function", () => {
        const session = createSession(baseConfig())
        let calls = 0
        const unsub = session.onData(() => calls++)
        expect(typeof unsub).toBe("function")
        unsub()
    })
})

describe("participants when inactive", () => {
    it("returns empty array when no manager exists", () => {
        const session = createSession(baseConfig())
        expect(session.participants).toEqual([])
    })
})

describe("sendData, replaceTrack, and setQualityPreference when inactive", () => {
    it("sendData throws when not active (SFU)", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await expect(session.sendData("x", "y")).rejects.toThrow(/not active/)
    })

    it("replaceTrack throws when not active", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await expect(session.replaceTrack("audio", null)).rejects.toThrow(/not active/)
    })

    it("setQualityPreference throws when not active (SFU)", async () => {
        const session = createSession(baseConfig({ topology: "sfu" }))
        await expect(session.setQualityPreference("high")).rejects.toThrow(/not active/)
    })
})

describe("mesh roster polling edge cases", () => {
    it("handles onlineAgents returning agents with no status", async () => {
        const onlineAgents = jest.fn().mockResolvedValue([
            { did: "did:bob", status: null },
            { did: "did:carol" },
        ])
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            onlineAgents,
        }))
        await session.join(fakeMediaStream())
        await new Promise((r) => setTimeout(r, 10))

        // Neither agent had call-presence links, so roster stays empty.
        expect(mockMeshManagerInstance.setRoster).toHaveBeenCalledWith([])
    })

    it("skips a roster tick while the previous poll is still running", async () => {
        jest.useFakeTimers()
        try {
            let answer: (agents: unknown[]) => void = () => {}
            const onlineAgents = jest.fn().mockImplementation(() => new Promise((r) => { answer = r }))
            const session = createSession(baseConfig({
                topology: "mesh",
                channel: fakeChannel(),
                onlineAgents,
            }))
            await session.join(fakeMediaStream())
            expect(onlineAgents).toHaveBeenCalledTimes(1)

            // The first poll is still waiting: three ticks start no second one.
            jest.advanceTimersByTime(9_000)
            await Promise.resolve()
            expect(onlineAgents).toHaveBeenCalledTimes(1)

            answer([])
            await Promise.resolve()
            await Promise.resolve()
            jest.advanceTimersByTime(3_000)
            await Promise.resolve()
            expect(onlineAgents).toHaveBeenCalledTimes(2)

            await session.leave()
        } finally {
            jest.useRealTimers()
        }
    })

    it("drops a poll result that arrives after leave", async () => {
        const consoleWarn = jest.spyOn(console, "warn").mockImplementation()
        let answer: (agents: unknown[]) => void = () => {}
        const onlineAgents = jest.fn().mockImplementation(() => new Promise((r) => { answer = r }))
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            onlineAgents,
        }))
        await session.join(fakeMediaStream())
        await session.leave()
        answer([])
        await new Promise((r) => setTimeout(r, 10))

        expect(mockMeshManagerInstance.setRoster).not.toHaveBeenCalled()
        expect(consoleWarn).not.toHaveBeenCalled()
        consoleWarn.mockRestore()
    })

    it("handles onlineAgents rejection gracefully", async () => {
        const consoleWarn = jest.spyOn(console, "warn").mockImplementation()
        const onlineAgents = jest.fn().mockRejectedValue(new Error("network error"))
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            onlineAgents,
        }))
        await session.join(fakeMediaStream())
        await new Promise((r) => setTimeout(r, 10))

        // Should not throw — just warn.
        expect(consoleWarn).toHaveBeenCalled()
        consoleWarn.mockRestore()
    })

    it("handles setOnlineStatus rejection gracefully", async () => {
        const consoleWarn = jest.spyOn(console, "warn").mockImplementation()
        const setOnlineStatus = jest.fn().mockRejectedValue(new Error("broadcast failed"))
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            setOnlineStatus,
        }))
        await session.join(fakeMediaStream())
        // Should not throw.
        expect(session.getState()).toBe("active")
        consoleWarn.mockRestore()
    })
})

// A device can die without leaving (a crash, a closed browser, a lost network).
// Its links stay in its agent's one status for as long as another device of
// the agent keeps that status, so each device renews a beat and readers drop a
// device whose beat has not changed for the stale bound (#1581's regression).
describe("a mesh device that dies without leaving", () => {
    const ROOM = "beat-room"
    /** The bounds the session works to (Session.ts): renewal, staleness, roster poll. */
    const RENEW_MS = 30_000
    const STALE_MS = 90_000
    const POLL_MS = 3_000
    const realSetTimeout = setTimeout

    type Link = { source: string; predicate: string; target: string }
    const presence = (peer: string): Link => ({ source: peer, predicate: "ad4m://session/in-call", target: ROOM })
    const beatLink = (peer: string, beat: number): Link => ({ source: peer, predicate: "ad4m://session/in-call-beat", target: `${ROOM}#${beat}` })
    const statusOf = (links: Link[]) => ({ data: { links: links.map((data) => ({ data })) } })

    /** Agents' statuses as an executor holds them; this session is `did:me`'s device `this-device`. */
    function world(overrides: Record<string, unknown> = {}) {
        const statuses = new Map<string, Link[]>()
        const setOnlineStatus = jest.fn(async ({ links }: { links: Link[] }) => { statuses.set("did:me", links); return true })
        const ownOnlineStatus = jest.fn(async () => (statuses.has("did:me") ? statusOf(statuses.get("did:me")!) : null))
        const onlineAgents = jest.fn(async () =>
            [...statuses].filter(([did]) => did !== "did:me").map(([did, links]) => ({ did, status: statusOf(links) })))
        /** A device of `did` writes its links into the agent's status, keeping everyone else's. */
        const write = (did: string, peer: string, ...links: Link[]) =>
            statuses.set(did, [...(statuses.get(did) ?? []).filter((l) => l.source !== peer), ...links])
        /** A live device: announces now, then a new beat every renewal interval. */
        const live = (did: string, peer: string) => {
            let beat = 1
            write(did, peer, presence(peer), beatLink(peer, beat))
            setInterval(() => write(did, peer, presence(peer), beatLink(peer, ++beat)), RENEW_MS)
        }
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            setOnlineStatus,
            ownOnlineStatus,
            onlineAgents,
            roomId: ROOM,
            agentDid: "did:me",
            ...overrides,
        }))
        /** The last roster the mesh got, sorted: a device's rewrite moves its links in the status. */
        const roster = (): string[] => [...(mockMeshManagerInstance.setRoster.mock.calls.at(-1)?.[0] ?? [])].sort()
        const own = (): Link[] => statuses.get("did:me") ?? []
        return { session, statuses, setOnlineStatus, write, live, roster, own }
    }

    beforeEach(() => { jest.useFakeTimers() })
    afterEach(() => {
        jest.clearAllTimers()
        jest.useRealTimers()
    })

    it("drops another agent's device whose beat stops once the stale bound passes, and keeps a renewing one", async () => {
        const w = world()
        w.live("did:bob", "did:bob#desk")
        w.write("did:bob", "did:bob#laptop", presence("did:bob#laptop"), beatLink("did:bob#laptop", 7))
        // A device on an SDK from before the beat writes none: it never goes stale.
        w.write("did:carol", "did:carol#old", presence("did:carol#old"))
        await w.session.join(fakeMediaStream())

        await jest.advanceTimersByTimeAsync(STALE_MS - POLL_MS)
        expect(w.roster()).toEqual(["did:bob#desk", "did:bob#laptop", "did:carol#old"])

        await jest.advanceTimersByTimeAsync(POLL_MS)
        expect(w.roster()).toEqual(["did:bob#desk", "did:carol#old"])

        // It comes back: a new beat puts it in again.
        w.write("did:bob", "did:bob#laptop", presence("did:bob#laptop"), beatLink("did:bob#laptop", 8))
        await jest.advanceTimersByTimeAsync(POLL_MS)
        expect(w.roster()).toEqual(["did:bob#desk", "did:bob#laptop", "did:carol#old"])
        await w.session.leave()
    })

    it("renews its beat every renewal interval, one status write each", async () => {
        const w = world()
        await w.session.join(fakeMediaStream())
        expect(w.setOnlineStatus).toHaveBeenCalledTimes(1)

        await jest.advanceTimersByTimeAsync(3 * RENEW_MS + POLL_MS)
        expect(w.setOnlineStatus).toHaveBeenCalledTimes(4)
        const beats = w.setOnlineStatus.mock.calls.map(([{ links }]) =>
            links.filter((l) => l.predicate === "ad4m://session/in-call-beat").map((l) => l.target))
        // Each write carries one beat of this device, and each a new one.
        expect(beats.every((b) => b.length === 1)).toBe(true)
        expect(new Set(beats.flat()).size).toBe(4)
        expect(w.own()).toEqual(expect.arrayContaining([presence("did:me#this-device"), beatOf("did:me#this-device", ROOM)]))
        await w.session.leave()
    })

    it("drops its agent's dead device from the roster and from the status, and keeps the live one's links", async () => {
        const appLink = { source: "did:me", predicate: "app://mood", target: "literal:string:busy" }
        const w = world()
        w.statuses.set("did:me", [appLink])
        w.live("did:me", "did:me#tablet")
        w.write("did:me", "did:me#phone", presence("did:me#phone"), beatLink("did:me#phone", 3))
        await w.session.join(fakeMediaStream())

        // Its own renewals keep both other devices while the phone's beat is not yet stale.
        await jest.advanceTimersByTimeAsync(2 * RENEW_MS + POLL_MS)
        expect(w.roster()).toEqual(["did:me#phone", "did:me#tablet"])
        expect(w.own()).toEqual(expect.arrayContaining([presence("did:me#phone"), presence("did:me#tablet")]))

        await jest.advanceTimersByTimeAsync(STALE_MS + POLL_MS - (2 * RENEW_MS + POLL_MS))
        expect(w.roster()).toEqual(["did:me#tablet"])
        // The renewal after the stale bound took the phone's links out, and kept everyone else's.
        const sources = w.own().map((l) => l.source)
        expect(sources).not.toContain("did:me#phone")
        expect(w.own()).toEqual(expect.arrayContaining([
            appLink, presence("did:me#tablet"), presence("did:me#this-device"), beatOf("did:me#this-device", ROOM),
        ]))
        expect(w.own().filter((l) => l.source === "did:me#tablet")).toHaveLength(2)
        await w.session.leave()
    })

    it("takes no agent's beat for another agent's device", async () => {
        const w = world()
        w.write("did:carol", "did:carol#desk", presence("did:carol#desk"), beatLink("did:carol#desk", 1))
        // Bob renews a beat in Carol's device's name, and announces a device of Carol's.
        let forged = 1
        const forge = () => w.write("did:bob", "did:carol#desk",
            presence("did:carol#desk"), beatLink("did:carol#desk", ++forged), presence("did:carol#phone"))
        forge()
        setInterval(forge, RENEW_MS)
        await w.session.join(fakeMediaStream())
        await jest.advanceTimersByTimeAsync(POLL_MS)
        expect(w.roster()).toEqual(["did:carol#desk"])

        await jest.advanceTimersByTimeAsync(STALE_MS)
        expect(w.roster()).toEqual([])
        await w.session.leave()
    })

    /** Join, see the first renewal, end the call with `end`, and count status writes for long after. */
    async function renewalsAfter(end: (w: ReturnType<typeof world>) => Promise<void>, overrides: Record<string, unknown> = {}) {
        const w = world(overrides)
        await w.session.join(fakeMediaStream())
        await jest.advanceTimersByTimeAsync(RENEW_MS + POLL_MS)
        expect(w.setOnlineStatus).toHaveBeenCalledTimes(2)
        await end(w)
        const writes = w.setOnlineStatus.mock.calls.length
        await jest.advanceTimersByTimeAsync(4 * RENEW_MS)
        return { w, after: w.setOnlineStatus.mock.calls.length - writes }
    }

    it("stops renewing on leave, and takes its beat out with its presence", async () => {
        const { w, after } = await renewalsAfter((w) => w.session.leave())
        expect(after).toBe(0)
        expect(w.own()).toEqual([])
    })

    it("stops renewing on destroy", async () => {
        const { w, after } = await renewalsAfter((w) => w.session.destroy())
        expect(after).toBe(0)
        expect(w.own()).toEqual([])
    })

    it("stops renewing when another tab of this device takes the call over", async () => {
        let later: Session | null = null
        const { w, after } = await renewalsAfter(async () => {
            later = createSession(baseConfig({ topology: "mesh", channel: fakeChannel(), roomId: ROOM, agentDid: "did:me" }))
            await later.join(fakeMediaStream())
            // BroadcastChannel delivers on the real event loop.
            await new Promise((r) => realSetTimeout(r, 20))
        })
        expect(w.session.getState()).toBe("idle")
        expect(after).toBe(0)
        await later!.destroy()
        await w.session.destroy()
    })
})
