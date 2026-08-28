import { SfuManager, SfuNeighbourhoodApi } from "./SfuManager"
import type {
    SfuConfig,
    SfuDataMessage,
    SfuQualityPreference,
    SfuParticipantInfo,
    IceServer,
} from "./SfuTypes"

export type SessionTopology = "mesh" | "sfu" | "auto"

export type SessionState = "idle" | "joining" | "active" | "leaving" | "closed"

export type SessionEvent =
    | "topology-changed"
    | "participant-joined"
    | "participant-left"
    | "active-speaker"
    | "stream-added"
    | "stream-removed"
    | "state-changed"
    | "error"

export type SessionEventCallback = (...args: any[]) => void

/** Unified WebRTC media session wrapping both mesh and SFU topologies. */
export interface Session {
    /** Connect to the room with local media. Resolves when the session becomes active. */
    join(localStream: MediaStream): Promise<void>
    /** Disconnect from the room. The session returns to idle and can rejoin. */
    leave(): Promise<void>
    /** Current participants in the room (excluding self). */
    readonly participants: ReadonlyArray<SfuParticipantInfo>
    /** Subscribe to incoming remote media tracks. Returns an unsubscribe function. */
    onTrack(cb: (stream: MediaStream, track: MediaStreamTrack) => void): () => void
    /** Set the preferred simulcast quality layer. */
    setQualityPreference(pref: SfuQualityPreference): Promise<void>
    /** Send data to all other participants via the SFU relay. */
    sendData(label: string, data: string, binary?: boolean): Promise<void>
    /** Subscribe to data channel messages from other participants. Returns an unsubscribe function. */
    onData(cb: (message: SfuDataMessage) => void): () => void
    /** Current lifecycle state. */
    getState(): SessionState
    /** Subscribe to session events. */
    on(event: SessionEvent, cb: SessionEventCallback): void
    /** Unsubscribe from session events. Omit cb to remove all listeners for the event. */
    off(event: SessionEvent, cb?: SessionEventCallback): void
    /** Release all resources. Calls leave() if still active. */
    destroy(): Promise<void>
}

export interface SessionJoinOptions {
    topology?: SessionTopology
    neighbourhoodUrl?: string
}

export interface SessionCreateOptions {
    topology?: SessionTopology
    neighbourhoodUrl?: string
}

interface SessionImplConfig {
    api: SfuNeighbourhoodApi
    roomId: string
    agentDid: string
    neighbourhoodUrl: string
    topology: SessionTopology
    sfuConfig?: SfuConfig
    iceServers?: IceServer[]
}

export function createSession(config: SessionImplConfig): Session {
    const { api, roomId, agentDid, neighbourhoodUrl, topology, sfuConfig, iceServers } = config

    let state: SessionState = "idle"
    let manager: SfuManager | null = null
    const callbacks = new Map<SessionEvent, SessionEventCallback[]>()
    const trackListeners: ((stream: MediaStream, track: MediaStreamTrack) => void)[] = []
    const dataListeners: ((message: SfuDataMessage) => void)[] = []
    let dataUnsubscribe: (() => void) | null = null

    function emit(event: SessionEvent, ...args: any[]) {
        const cbs = callbacks.get(event)
        if (cbs) for (const cb of cbs) cb(...args)
    }

    function setState(next: SessionState) {
        state = next
        emit("state-changed", next)
    }

    function resolvedTopology(): "sfu" | "mesh" {
        if (topology === "sfu") return "sfu"
        if (topology === "mesh") return "mesh"
        // auto: use sfuConfig if available, default to sfu
        if (sfuConfig && sfuConfig.mode === "mesh") return "mesh"
        return "sfu"
    }

    const session: Session = {
        async join(localStream: MediaStream) {
            if (state === "closed") throw new Error("Session destroyed")
            if (state === "active" || state === "joining") throw new Error("Session already active")

            const topo = resolvedTopology()
            if (topo === "mesh") {
                throw new Error("Mesh transport not yet implemented — use topology 'sfu' or 'auto' with SFU-capable neighbourhood")
            }

            setState("joining")

            const iceConfig = iceServers && iceServers.length > 0
                ? { stun: iceServers.filter(s => s.urls.some(u => u.startsWith("stun:"))).flatMap(s => s.urls),
                    turn: iceServers.filter(s => s.urls.some(u => u.startsWith("turn:"))).map(s => ({ urls: s.urls.join(","), username: s.username || "", credential: s.credential || "" })) }
                : undefined

            manager = new SfuManager(api, roomId, agentDid, neighbourhoodUrl, iceConfig, sfuConfig)

            for (const event of ["topology-changed", "participant-joined", "participant-left", "active-speaker", "error"] as const) {
                manager.on(event, (...args: any[]) => emit(event, ...args))
            }
            manager.on("stream-added", (stream: MediaStream, track: MediaStreamTrack) => {
                emit("stream-added", stream, track)
                for (const cb of trackListeners) cb(stream, track)
            })
            manager.on("stream-removed", (stream: MediaStream, track: MediaStreamTrack) => {
                emit("stream-removed", stream, track)
            })

            dataUnsubscribe = manager.subscribeDataChannel((msg) => {
                for (const cb of dataListeners) cb(msg)
            })

            await manager.join(localStream)
            setState("active")
        },

        async leave() {
            if (state !== "active") return
            setState("leaving")
            if (dataUnsubscribe) { dataUnsubscribe(); dataUnsubscribe = null }
            if (manager) { await manager.leave(); manager = null }
            setState("idle")
        },

        get participants() {
            if (!manager) return []
            return manager.getParticipants().map(p => ({
                agentDid: p.did,
                hasAudio: p.hasAudio,
                hasVideo: p.hasVideo,
                isActiveSpeaker: p.isActiveSpeaker,
            }))
        },

        onTrack(cb) {
            trackListeners.push(cb)
            return () => {
                const idx = trackListeners.indexOf(cb)
                if (idx !== -1) trackListeners.splice(idx, 1)
            }
        },

        async setQualityPreference(pref) {
            if (!manager) throw new Error("Session not active")
            await manager.setQualityPreference(pref)
        },

        async sendData(label, data, binary = false) {
            if (!manager) throw new Error("Session not active")
            await manager.sendData(label, data, binary)
        },

        onData(cb) {
            dataListeners.push(cb)
            return () => {
                const idx = dataListeners.indexOf(cb)
                if (idx !== -1) dataListeners.splice(idx, 1)
            }
        },

        getState() { return state },

        on(event, cb) {
            if (!callbacks.has(event)) callbacks.set(event, [])
            callbacks.get(event)!.push(cb)
        },

        off(event, cb?) {
            if (!cb) { callbacks.delete(event); return }
            const cbs = callbacks.get(event)
            if (cbs) {
                const idx = cbs.indexOf(cb)
                if (idx !== -1) cbs.splice(idx, 1)
                if (cbs.length === 0) callbacks.delete(event)
            }
        },

        async destroy() {
            if (state === "active" || state === "joining") await session.leave()
            if (manager) { await manager.destroy(); manager = null }
            callbacks.clear()
            trackListeners.length = 0
            dataListeners.length = 0
            setState("closed")
        },
    }

    return session
}
