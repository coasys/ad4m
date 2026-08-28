import { SfuManager, SfuNeighbourhoodApi } from "./SfuManager"
import { MeshManager, type SignallingChannel } from "./MeshManager"
import type { OnlineAgent } from "../language/Language"
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
    /**
     * Replace the outbound track of a given kind on all peer connections.
     *
     * Uses `RTCRtpSender.replaceTrack` — no renegotiation, so camera↔screen
     * swaps and mute/unmute are instant and atomic across all peers.
     * Pass `null` to stop sending that kind without tearing down the transceiver.
     */
    replaceTrack(kind: "audio" | "video", track: MediaStreamTrack | null): Promise<void>
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
    /** Signalling channel for mesh topology — required when topology resolves to mesh. */
    channel?: SignallingChannel
    /** Publish call-presence via telepresence.  The session broadcasts
     *  membership so other agents' roster polls can discover it. */
    setOnlineStatus?: (status: { links: { source: string; predicate: string; target: string }[] }) => Promise<boolean>
    /** Poll for agents currently online in the neighbourhood.  The
     *  session reconciles the result against its mesh connections. */
    onlineAgents?: () => Promise<OnlineAgent[]>
}

/**
 * Call-presence predicate.  When the session joins, it publishes an
 * online-status link with this predicate and the room id as target.
 * Other agents polling `onlineAgents()` use this to discover the
 * roster without a join message.
 */
const CALL_PRESENCE_PREDICATE = "ad4m://session/in-call"

/** How often (ms) the mesh session polls `onlineAgents` for roster changes. */
const ROSTER_POLL_INTERVAL_MS = 3_000

export function createSession(config: SessionImplConfig): Session {
    const {
        api, roomId, agentDid, neighbourhoodUrl,
        topology, sfuConfig, iceServers,
        channel, setOnlineStatus, onlineAgents,
    } = config

    let state: SessionState = "idle"
    let sfuManager: SfuManager | null = null
    let meshManager: MeshManager | null = null
    let rosterInterval: ReturnType<typeof setInterval> | null = null
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

    // ── Mesh roster polling ─────────────────────────────────────────

    /**
     * Announce this agent's call membership via telepresence presence.
     * Other agents' roster polls will pick this up.
     */
    async function announceCallPresence(): Promise<void> {
        if (!setOnlineStatus) return
        try {
            await setOnlineStatus({
                links: [{ source: agentDid, predicate: CALL_PRESENCE_PREDICATE, target: roomId }],
            })
        } catch (err) {
            console.warn("session: call-presence announcement failed:", err)
        }
    }

    /**
     * Poll online agents and reconcile the mesh roster.
     *
     * Agents in the same call carry a link with predicate
     * `CALL_PRESENCE_PREDICATE` and target matching the room id.
     * The poll reads the full online-agents list, filters to those
     * links, and passes the resulting DID set to `mesh.setRoster()`.
     * That function opens connections to new arrivals and tears down
     * connections to departed peers.
     */
    async function pollRoster(): Promise<void> {
        if (!meshManager || !onlineAgents) return
        try {
            const agents = await onlineAgents()
            const inCall: string[] = []
            for (const agent of agents) {
                if (agent.did === agentDid) continue
                // Each agent's status carries a perspective with links.
                // Check for the call-presence link matching this room.
                const status = agent.status
                if (status?.links) {
                    for (const link of status.links) {
                        const l = link.data ?? link
                        if (l.predicate === CALL_PRESENCE_PREDICATE && l.target === roomId) {
                            inCall.push(agent.did)
                            break
                        }
                    }
                }
            }
            meshManager.setRoster(inCall)
        } catch (err) {
            console.warn("session: roster poll failed:", err)
        }
    }

    function startRosterPolling(): void {
        // Immediate first poll, then periodic
        void pollRoster()
        rosterInterval = setInterval(() => void pollRoster(), ROSTER_POLL_INTERVAL_MS)
    }

    function stopRosterPolling(): void {
        if (rosterInterval !== null) {
            clearInterval(rosterInterval)
            rosterInterval = null
        }
    }

    // ── Mesh wiring helpers ─────────────────────────────────────────

    function wireMeshEvents(mesh: MeshManager): void {
        for (const event of ["participant-joined", "participant-left", "error"] as const) {
            mesh.on(event, (...args: any[]) => emit(event, ...args))
        }
        mesh.on("stream-added", (stream: MediaStream, track: MediaStreamTrack) => {
            emit("stream-added", stream, track)
            for (const cb of trackListeners) cb(stream, track)
        })
        mesh.on("stream-removed", (stream: MediaStream, track: MediaStreamTrack) => {
            emit("stream-removed", stream, track)
        })
    }

    // ── SFU wiring helpers ──────────────────────────────────────────

    function wireSfuEvents(sfu: SfuManager): void {
        for (const event of ["topology-changed", "participant-joined", "participant-left", "active-speaker", "error"] as const) {
            sfu.on(event, (...args: any[]) => emit(event, ...args))
        }
        sfu.on("stream-added", (stream: MediaStream, track: MediaStreamTrack) => {
            emit("stream-added", stream, track)
            for (const cb of trackListeners) cb(stream, track)
        })
        sfu.on("stream-removed", (stream: MediaStream, track: MediaStreamTrack) => {
            emit("stream-removed", stream, track)
        })
    }

    // ── Session object ──────────────────────────────────────────────

    const session: Session = {
        async join(localStream: MediaStream) {
            if (state === "closed") throw new Error("Session destroyed")
            if (state === "active" || state === "joining") throw new Error("Session already active")

            const topo = resolvedTopology()
            setState("joining")

            if (topo === "mesh") {
                if (!channel) {
                    throw new Error(
                        "Mesh topology requires a signalling channel — " +
                        "pass a channel via SessionImplConfig or use topology 'sfu'",
                    )
                }

                const mesh = new MeshManager({
                    channel,
                    callId: roomId,
                    selfId: agentDid,
                })
                meshManager = mesh
                wireMeshEvents(mesh)

                dataUnsubscribe = mesh.subscribeDataChannel((msg) => {
                    for (const cb of dataListeners) cb(msg)
                })

                await mesh.join(localStream)

                // Announce this agent's presence in the call, then
                // start polling so the mesh reconciles connections
                // against whoever else the roster contains.
                await announceCallPresence()
                startRosterPolling()

                emit("topology-changed", "mesh")
                setState("active")
                return
            }

            // SFU path — unchanged
            const iceConfig = iceServers && iceServers.length > 0
                ? { stun: iceServers.filter(s => s.urls.some(u => u.startsWith("stun:"))).flatMap(s => s.urls),
                    turn: iceServers.filter(s => s.urls.some(u => u.startsWith("turn:"))).map(s => ({ urls: s.urls.join(","), username: s.username || "", credential: s.credential || "" })) }
                : undefined

            sfuManager = new SfuManager(api, roomId, agentDid, neighbourhoodUrl, iceConfig, sfuConfig)
            wireSfuEvents(sfuManager)

            dataUnsubscribe = sfuManager.subscribeDataChannel((msg) => {
                for (const cb of dataListeners) cb(msg)
            })

            await sfuManager.join(localStream)
            setState("active")
        },

        async leave() {
            if (state !== "active") return
            setState("leaving")
            stopRosterPolling()
            if (dataUnsubscribe) { dataUnsubscribe(); dataUnsubscribe = null }
            if (meshManager) { await meshManager.leave(); meshManager = null }
            if (sfuManager) { await sfuManager.leave(); sfuManager = null }
            setState("idle")
        },

        get participants() {
            if (meshManager) {
                return meshManager.getParticipants().map(p => ({
                    agentDid: p.did,
                    stream: p.stream,
                    hasAudio: p.hasAudio,
                    hasVideo: p.hasVideo,
                    isActiveSpeaker: p.isActiveSpeaker,
                }))
            }
            if (sfuManager) {
                return sfuManager.getParticipants().map(p => ({
                    agentDid: p.did,
                    stream: p.stream,
                    hasAudio: p.hasAudio,
                    hasVideo: p.hasVideo,
                    isActiveSpeaker: p.isActiveSpeaker,
                }))
            }
            return []
        },

        onTrack(cb) {
            trackListeners.push(cb)
            return () => {
                const idx = trackListeners.indexOf(cb)
                if (idx !== -1) trackListeners.splice(idx, 1)
            }
        },

        async replaceTrack(kind, track) {
            if (meshManager) { await meshManager.replaceTrack(kind, track); return }
            if (sfuManager) { await sfuManager.replaceTrack(kind, track); return }
            throw new Error("Session not active")
        },

        async setQualityPreference(pref) {
            // Mesh has no quality layers — simulcast only applies to SFU.
            // Silently accept so callers need no topology check.
            if (meshManager) return
            if (!sfuManager) throw new Error("Session not active")
            await sfuManager.setQualityPreference(pref)
        },

        async sendData(label, data, binary = false) {
            if (meshManager) {
                meshManager.sendData(label, data, binary)
                return
            }
            if (!sfuManager) throw new Error("Session not active")
            await sfuManager.sendData(label, data, binary)
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
            // Capture refs before leave() nulls them
            const mesh = meshManager
            const sfu = sfuManager
            if (state === "active" || state === "joining") await session.leave()
            stopRosterPolling()
            if (mesh) { await mesh.destroy() }
            if (sfu) { await sfu.destroy() }
            meshManager = null
            sfuManager = null
            // Emit closed BEFORE clearing listeners
            setState("closed")
            callbacks.clear()
            trackListeners.length = 0
            dataListeners.length = 0
        },
    }

    return session
}
