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
        subscribeSfuCallRenegotiationOffer: jest.fn(),
        subscribeSfuMigrateEvent: jest.fn(),
        sfuAddIceCandidate: jest.fn(),
        sfuSendData: jest.fn(),
        subscribeSfuDataChannel: jest.fn(),
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

    it("resolves 'auto' without sfuConfig to SFU when local SFU is public", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            localSfuStatus: { reachability: "public", isPublic: true, bindAddress: "1.2.3.4:0", detail: "" },
        }))
        await session.join(fakeMediaStream())
        expect(SfuManager).toHaveBeenCalled()
    })

    it("resolves 'auto' to mesh when no SFU nodes found", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            availableSfuNodes: async () => [],
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())
        expect(MeshManager).toHaveBeenCalled()
        expect(SfuManager).not.toHaveBeenCalled()
    })

    it("resolves 'auto' to SFU when neighbourhood has available SFU nodes", async () => {
        const session = createSession(baseConfig({
            topology: "auto",
            availableSfuNodes: async () => [{ did: "did:sfu:node1", bindAddress: "5.6.7.8:9000" }],
        }))
        await session.join(fakeMediaStream())
        expect(SfuManager).toHaveBeenCalled()
        expect(MeshManager).not.toHaveBeenCalled()
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
            selfId: "did:me",
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
            links: [{ source: "did:me", predicate: "ad4m://session/in-call", target: "my-room" }],
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

    it("filters roster to agents in the same call room", async () => {
        const onlineAgents = jest.fn().mockResolvedValue([
            {
                did: "did:bob",
                status: {
                    links: [{
                        data: { predicate: "ad4m://session/in-call", target: "my-room" },
                    }],
                },
            },
            {
                did: "did:carol",
                status: {
                    links: [{
                        data: { predicate: "ad4m://session/in-call", target: "other-room" },
                    }],
                },
            },
            {
                did: "did:me",
                status: {
                    links: [{
                        data: { predicate: "ad4m://session/in-call", target: "my-room" },
                    }],
                },
            },
        ])

        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
            onlineAgents,
            roomId: "my-room",
            agentDid: "did:me",
        }))
        await session.join(fakeMediaStream())
        // Wait for the async pollRoster to settle
        await new Promise((r) => setTimeout(r, 10))

        expect(mockMeshManagerInstance.setRoster).toHaveBeenCalledWith(["did:bob"])
    })

    it("delegates participants to mesh manager (includes stream)", async () => {
        const bobStream = fakeMediaStream()
        mockMeshManagerInstance.getParticipants.mockReturnValue([
            { did: "did:bob", stream: bobStream, hasAudio: true, hasVideo: false, isActiveSpeaker: false },
        ])
        const session = createSession(baseConfig({
            topology: "mesh",
            channel: fakeChannel(),
        }))
        await session.join(fakeMediaStream())

        expect(session.participants).toEqual([
            { agentDid: "did:bob", stream: bobStream, hasAudio: true, hasVideo: false, isActiveSpeaker: false },
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
        )
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
            { did: "did:bob", stream: bobStream, hasAudio: true, hasVideo: true, isActiveSpeaker: true },
        ])
        const session = createSession(baseConfig({ topology: "sfu" }))
        await session.join(fakeMediaStream())

        expect(session.participants).toEqual([
            { agentDid: "did:bob", stream: bobStream, hasAudio: true, hasVideo: true, isActiveSpeaker: true },
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

    it("converts iceServers to SfuManager format", async () => {
        const session = createSession(baseConfig({
            topology: "sfu",
            iceServers: [
                { urls: ["stun:stun.example.com:3478"] },
                { urls: ["turn:turn.example.com:3478"], username: "user", credential: "pass" },
            ],
        }))
        await session.join(fakeMediaStream())

        const call = (SfuManager as jest.Mock).mock.calls[0]
        const iceConfig = call[4]
        expect(iceConfig.stun).toEqual(["stun:stun.example.com:3478"])
        expect(iceConfig.turn).toEqual([{
            urls: "turn:turn.example.com:3478",
            username: "user",
            credential: "pass",
        }])
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
