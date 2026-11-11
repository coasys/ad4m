import { defaultDeviceId, SfuManager, SfuNeighbourhoodApi } from "./SfuManager"
import { agentOfMeshPeer, MeshManager, meshPeerId, type MeshParticipant, type SignallingChannel } from "./MeshManager"
import type { OnlineAgent } from "../language/Language"
import type {
    SfuConfig,
    SfuDataMessage,
    SfuQualityPreference,
    SfuParticipantInfo,
    SfuStatus,
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
    /** `(participant)` — a participant's stream object or track liveness changed. Mesh only. */
    | "participant-updated"
    /** `(did, RTCPeerConnectionState)` — one pair's connection state. Mesh only. */
    | "peer-state"
    /** `(did, { rung, attempts })` — the mesh attempted a repair on this pair. */
    | "peer-recovery"
    | "state-changed"
    /**
     * `(SfuSessionReplaced)` — the call was joined again from this device (another tab), and
     * that join took it over: this session's call has ended, and it is idle again.
     */
    | "replaced"
    | "error"

export type SessionEventCallback = (...args: any[]) => void

/** Unified WebRTC media session wrapping both mesh and SFU topologies. */
export interface Session {
    /**
     * Connect to the room with local media. Resolves when the session becomes active, or
     * idle if another tab on this device replaced the call while it joined (`replaced`).
     */
    join(localStream: MediaStream): Promise<void>
    /** Disconnect from the room. The session returns to idle and can rejoin. */
    leave(): Promise<void>
    /** Current participants in the room (excluding this session; this agent's other devices included). */
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
    /** Each mesh peer's connection state. Empty on the SFU path, which has one connection. */
    peerStates(): Map<string, RTCPeerConnectionState>
    /** Rebuild one mesh peer's connection, telling the peer to do the same. No-op on the SFU path. */
    reconnect(did: string): void
    /** How a mesh peer's media travels — `host`, `srflx` or `relay` — or `null`. */
    transportOf(did: string): Promise<string | null>
    /** Release all resources. Calls leave() if still active. */
    destroy(): Promise<void>
}

export interface SessionCreateOptions {
    topology?: SessionTopology
    neighbourhoodUrl?: string
    /** The neighbourhood's call config. With `topology: "auto"`, its mode decides: any SFU mode
     *  runs through the SFU, `mesh` stays peer-to-peer. Passing one is the app's statement that
     *  every member reaches the same SFU. */
    sfuConfig?: SfuConfig
    /** NAT traversal for both paths, each server as given. Takes precedence over `sfuConfig.iceServers`. */
    iceServers?: IceServer[]
    /**
     * The device this session calls from: a join from the same device replaces this agent's
     * earlier session (another tab); another device joins beside it, as a participant of its own.
     * Unset, the browser profile's id (`localStorage`), or one per process outside a browser.
     */
    deviceId?: string
}

interface SessionImplConfig {
    api: SfuNeighbourhoodApi
    roomId: string
    agentDid: string
    neighbourhoodUrl: string
    topology: SessionTopology
    sfuConfig?: SfuConfig
    iceServers?: IceServer[]
    /** See `SessionCreateOptions.deviceId`. */
    deviceId?: string
    /** Signalling channel for mesh topology — required when topology resolves to mesh. It must
     *  speak as this session's peer, {@link meshPeerId} of `agentDid` and the device. */
    channel?: SignallingChannel
    /** Publish call-presence via telepresence.  The session broadcasts
     *  membership so other agents' roster polls can discover it. */
    setOnlineStatus?: (status: { links: StatusLink[] }) => Promise<boolean>
    /** This agent's current online status. The session adds its presence
     *  link to it and takes it out again, leaving the app's own links and
     *  the presence of this agent's other devices — which the mesh roster
     *  reads from here too. */
    ownOnlineStatus?: () => Promise<{ data?: { links?: { data?: Partial<StatusLink> }[] } } | null>
    /** Poll for agents currently online in the neighbourhood.  The
     *  session reconciles the result against its mesh connections. */
    onlineAgents?: () => Promise<OnlineAgent[]>
    /** Local executor's SFU status. "auto" needs `isPublic`: clients
     *  that are not on the executor's machine must reach its SFU. */
    localSfuStatus?: SfuStatus
    /** Whether every member of the neighbourhood is hosted on the local
     *  executor, so all of them reach its SFU. "auto" picks the SFU only
     *  when this resolves true. */
    sfuServesAllMembers?: () => Promise<boolean>
}

/**
 * Call-presence predicate.  When the session joins, it publishes an
 * online-status link with this predicate, its mesh peer id as source and
 * the room id as target.  Other agents polling `onlineAgents()` use this
 * to discover the roster without a join message; one agent's devices each
 * add a link of their own to the agent's one status.
 */
const CALL_PRESENCE_PREDICATE = "ad4m://session/in-call"

type StatusLink = { source: string; predicate?: string; target: string }

/** How often (ms) the mesh session polls `onlineAgents` for roster changes. */
const ROSTER_POLL_INTERVAL_MS = 3_000

/**
 * The mesh peer a call-presence link of `did` announces, or `null`. A link
 * counts only for a peer of the agent whose status carries it, so no agent
 * can place another in a call.
 */
function presencePeer(did: string, link: Partial<StatusLink> | undefined, roomId: string): string | null {
    if (link?.predicate !== CALL_PRESENCE_PREDICATE || link.target !== roomId) return null
    const source = link.source
    if (typeof source !== "string" || source === did || agentOfMeshPeer(source) !== did) return null
    return source
}

/**
 * Where a mesh session hears that this device joined the same call again,
 * from another tab. `BroadcastChannel` reaches every tab of one browser
 * profile — the same reach as the `localStorage` the device id lives in.
 */
function sameDeviceChannel(agentDid: string, device: string, roomId: string): BroadcastChannel | null {
    const Channel = (globalThis as { BroadcastChannel?: typeof BroadcastChannel }).BroadcastChannel
    if (typeof Channel !== "function") return null
    try {
        return new Channel(`ad4m-call:${agentDid}:${device}:${roomId}`)
    } catch {
        return null
    }
}

export function createSession(config: SessionImplConfig): Session {
    const {
        api, roomId, agentDid, neighbourhoodUrl,
        topology, sfuConfig, iceServers, deviceId,
        channel, setOnlineStatus, ownOnlineStatus, onlineAgents,
        localSfuStatus, sfuServesAllMembers,
    } = config
    const device = deviceId || defaultDeviceId()
    const selfPeer = meshPeerId(agentDid, device)

    let state: SessionState = "idle"
    let sfuManager: SfuManager | null = null
    let meshManager: MeshManager | null = null
    let rosterInterval: ReturnType<typeof setInterval> | null = null
    /** A roster poll is running. Each poll waits on one remote call per
     *  online agent, so it can outlast the interval; the next tick is then
     *  skipped instead of stacking a second poll on the first. */
    let polling = false
    const callbacks = new Map<SessionEvent, SessionEventCallback[]>()
    const trackListeners: ((stream: MediaStream, track: MediaStreamTrack) => void)[] = []
    const dataListeners: ((message: SfuDataMessage) => void)[] = []
    let dataUnsubscribe: (() => void) | null = null
    /** The mesh call's line to this device's other tabs; see {@link sameDeviceChannel}. */
    let sameDevice: BroadcastChannel | null = null

    function emit(event: SessionEvent, ...args: any[]) {
        const cbs = callbacks.get(event)
        if (cbs) for (const cb of cbs) cb(...args)
    }

    function setState(next: SessionState) {
        state = next
        emit("state-changed", next)
    }

    /** `destroy()` ran: a join still resolving must not go on to reach the executor. */
    /** Set when `destroy()` starts, before its awaits: a join that resumes
     *  while the session closes must not go on. */
    let destroying = false

    function destroyed(): boolean {
        return destroying || state === "closed"
    }

    /**
     * Resolve the effective topology.
     *
     * Explicit "sfu" or "mesh" returns immediately; so does "auto" with an
     * `sfuConfig`. Otherwise "auto" must not split the call: the SFU path
     * joins this executor's SFU only, and the mesh path never reaches it.
     * So it picks the SFU only when the executor says every member of the
     * neighbourhood is hosted on it and its SFU is public. Anything else,
     * including a failed check, is mesh.
     */
    async function resolveTopology(): Promise<"sfu" | "mesh"> {
        if (topology === "sfu") return "sfu"
        if (topology === "mesh") return "mesh"

        if (sfuConfig) {
            return sfuConfig.mode === "mesh" ? "mesh" : "sfu"
        }

        if (!localSfuStatus?.isPublic || !sfuServesAllMembers) return "mesh"
        try {
            return (await sfuServesAllMembers()) ? "sfu" : "mesh"
        } catch (err) {
            console.warn("session: SFU membership check failed, using mesh:", err)
            return "mesh"
        }
    }

    // ── Mesh roster polling ─────────────────────────────────────────

    /** The app's links in this agent's status when the call started — what
     *  leaving restores if the status cannot be read then. */
    let statusBeforeCall: StatusLink[] = []
    let announced = false

    /** This agent's status links, without this device's presence link for this call. */
    async function ownLinksWithoutCall(): Promise<StatusLink[]> {
        const status = ownOnlineStatus ? await ownOnlineStatus() : null
        const links: StatusLink[] = []
        for (const link of status?.data?.links ?? []) {
            const l = link.data
            if (!l?.source || l.target === undefined) continue
            if (presencePeer(agentDid, l, roomId) === selfPeer) continue
            links.push({ source: l.source, target: l.target, ...(l.predicate ? { predicate: l.predicate } : {}) })
        }
        return links
    }

    /**
     * Add this call's presence link to the agent's online status. Other
     * agents' roster polls pick it up.
     */
    async function announceCallPresence(): Promise<void> {
        if (!setOnlineStatus) return
        try {
            statusBeforeCall = await ownLinksWithoutCall().catch((err) => {
                console.warn("session: reading own status failed:", err)
                return []
            })
            const presence = { source: selfPeer, predicate: CALL_PRESENCE_PREDICATE, target: roomId }
            await setOnlineStatus({ links: [...statusBeforeCall, presence] })
            announced = true
        } catch (err) {
            console.warn("session: call-presence announcement failed:", err)
        }
    }

    /** Take this call's presence link out of the agent's status again. */
    async function withdrawCallPresence(): Promise<void> {
        if (!setOnlineStatus || !announced) return
        announced = false
        try {
            const links = await ownLinksWithoutCall().catch((err) => {
                console.warn("session: reading own status failed:", err)
                return statusBeforeCall
            })
            await setOnlineStatus({ links })
        } catch (err) {
            console.warn("session: withdrawing call presence failed:", err)
        }
    }

    /**
     * Poll online agents and reconcile the mesh roster.
     *
     * Each device in the call carries a link with predicate
     * `CALL_PRESENCE_PREDICATE`, its peer id as source and the room id as
     * target, in its agent's status. The poll reads other agents' statuses
     * from the online-agents list and this agent's own — its other devices —
     * from `ownOnlineStatus`, and passes the peers to `mesh.setRoster()`.
     * That function opens connections to new arrivals and tears down
     * connections to departed peers.
     */
    async function pollRoster(): Promise<void> {
        const mesh = meshManager
        if (!mesh || !onlineAgents || polling) return
        polling = true
        try {
            const agents = await onlineAgents()
            const own = ownOnlineStatus ? await ownOnlineStatus() : null
            // The session left while the poll was in flight.
            if (meshManager !== mesh) return
            const inCall = new Set<string>()
            const collect = (did: string, links: { data?: Partial<StatusLink> }[] | undefined) => {
                for (const link of links ?? []) {
                    const peer = presencePeer(did, link.data, roomId)
                    if (peer && peer !== selfPeer) inCall.add(peer)
                }
            }
            // Each agent's status is a signed perspective: its links sit
            // under `data`. Whether an executor lists the caller among the
            // online agents varies, so this agent's own status is read here
            // from where it was set, never from the list.
            for (const agent of agents) {
                if (agent.did !== agentDid) collect(agent.did, agent.status?.data?.links)
            }
            collect(agentDid, own?.data?.links)
            mesh.setRoster([...inCall])
        } catch (err) {
            console.warn("session: roster poll failed:", err)
        } finally {
            polling = false
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
        for (const event of ["peer-state", "peer-recovery", "error"] as const) {
            mesh.on(event, (...args: any[]) => emit(event, ...args))
        }
        // As the SFU path reports them: the session (here, the device) and the agent it belongs to.
        for (const event of ["participant-joined", "participant-left", "participant-updated"] as const) {
            mesh.on(event, (p: MeshParticipant) => emit(event, { ...p, participantId: p.did, did: agentOfMeshPeer(p.did) }))
        }
        mesh.on("stream-added", (stream: MediaStream, track: MediaStreamTrack) => {
            emit("stream-added", stream, track)
            for (const cb of trackListeners) cb(stream, track)
        })
        mesh.on("stream-removed", (stream: MediaStream, track: MediaStreamTrack) => {
            emit("stream-removed", stream, track)
        })
    }

    /**
     * Listen for this device joining the same mesh call from another tab,
     * and tell any tab already in it. Both tabs are one peer, so only one
     * can hold the call: the earlier lets go, without withdrawing the
     * presence link the two share, and says `replaced`.
     */
    function claimDevice(mesh: MeshManager): void {
        sameDevice = sameDeviceChannel(agentDid, device, roomId)
        if (!sameDevice) return
        const channel = sameDevice
        channel.onmessage = (event: MessageEvent) => {
            if (meshManager !== mesh || (event.data as { joined?: unknown })?.joined !== true) return
            releaseDevice()
            stopRosterPolling()
            if (dataUnsubscribe) { dataUnsubscribe(); dataUnsubscribe = null }
            meshManager = null
            announced = false
            void mesh.leave()
            // Idle before listeners hear it: one may join again at once.
            setState("idle")
            emit("replaced", { targetDid: agentDid, neighbourhoodUrl, roomName: roomId, participantId: selfPeer })
        }
        channel.postMessage({ joined: true })
    }

    function releaseDevice(): void {
        if (!sameDevice) return
        sameDevice.onmessage = null
        sameDevice.close()
        sameDevice = null
    }

    // ── SFU wiring helpers ──────────────────────────────────────────

    function wireSfuEvents(sfu: SfuManager): void {
        // A manager the session let go of (replaced, or left) may still settle a call it
        // made before: none of its events reach the session's listeners.
        const current = () => sfuManager === sfu
        for (const event of ["topology-changed", "participant-joined", "participant-left", "active-speaker", "error"] as const) {
            sfu.on(event, (...args: any[]) => { if (current()) emit(event, ...args) })
        }
        sfu.on("stream-added", (stream: MediaStream, track: MediaStreamTrack) => {
            if (!current()) return
            emit("stream-added", stream, track)
            for (const cb of trackListeners) cb(stream, track)
        })
        sfu.on("stream-removed", (stream: MediaStream, track: MediaStreamTrack) => {
            if (!current()) return
            emit("stream-removed", stream, track)
        })
        // Another tab on this device took the call over. The manager has stopped
        // and will not rejoin; the session is idle and may join again.
        sfu.on("replaced", (event: unknown) => {
            if (sfuManager !== sfu) return
            if (dataUnsubscribe) { dataUnsubscribe(); dataUnsubscribe = null }
            sfuManager = null
            // Idle before listeners hear it: one may join again at once.
            setState("idle")
            emit("replaced", event)
        })
    }

    // ── Session object ──────────────────────────────────────────────

    const session: Session = {
        async join(localStream: MediaStream) {
            if (state === "closed") throw new Error("Session destroyed")
            if (state === "active" || state === "joining") throw new Error("Session already active")

            setState("joining")
            const topo = await resolveTopology()
            if (destroyed()) throw new Error("Session destroyed")

            if (topo === "mesh") {
                if (!channel) {
                    throw new Error(
                        "Mesh topology requires a signalling channel — " +
                        "pass a channel via SessionImplConfig or use topology 'sfu'",
                    )
                }

                // Build effective ICE servers for the mesh path, using the
                // same precedence as the SFU path: explicit iceServers →
                // sfuConfig.iceServers → MeshManager default.
                const meshIce: RTCIceServer[] = []
                if (iceServers && iceServers.length > 0) {
                    for (const s of iceServers) {
                        meshIce.push({
                            urls: s.urls,
                            ...(s.username ? { username: s.username } : {}),
                            ...(s.credential ? { credential: s.credential } : {}),
                        })
                    }
                } else if (sfuConfig?.iceServers && sfuConfig.iceServers.length > 0) {
                    for (const s of sfuConfig.iceServers) {
                        meshIce.push({
                            urls: s.urls,
                            ...(s.username ? { username: s.username } : {}),
                            ...(s.credential ? { credential: s.credential } : {}),
                        })
                    }
                }

                const mesh = new MeshManager({
                    channel,
                    callId: roomId,
                    selfId: selfPeer,
                    ...(meshIce.length > 0 ? { iceServers: meshIce } : {}),
                    onNetworkChange: (cb: () => void) => api.onNetworkChange(cb),
                    checkConnection: () => api.checkConnection(),
                })
                meshManager = mesh
                wireMeshEvents(mesh)

                dataUnsubscribe = mesh.subscribeDataChannel((msg) => {
                    for (const cb of dataListeners) cb(msg)
                })

                claimDevice(mesh)
                await mesh.join(localStream)
                // Destroyed while the mesh joined: destroy() already let the
                // mesh go and found nothing to leave.
                if (destroyed()) throw new Error("Session destroyed")
                // Replaced while joining: the session is idle again.
                if (meshManager !== mesh) return

                // Announce this agent's presence in the call, then
                // start polling so the mesh reconciles connections
                // against whoever else the roster contains.
                await announceCallPresence()
                if (destroyed()) {
                    await withdrawCallPresence()
                    throw new Error("Session destroyed")
                }
                // Replaced while announcing: the link stays, for the tab that took over.
                if (meshManager !== mesh) return
                startRosterPolling()

                emit("topology-changed", "mesh")
                setState("active")
                return
            }

            // SFU path — this executor's SFU.
            const iceConfig = iceServers && iceServers.length > 0
                ? { servers: iceServers.map(s => ({ urls: s.urls, ...(s.username ? { username: s.username } : {}), ...(s.credential ? { credential: s.credential } : {}) })) }
                : undefined

            const manager = new SfuManager(api, roomId, agentDid, neighbourhoodUrl, iceConfig, sfuConfig, device)
            sfuManager = manager
            wireSfuEvents(manager)

            dataUnsubscribe = manager.subscribeDataChannel((msg) => {
                for (const cb of dataListeners) cb(msg)
            })

            await manager.join(localStream)
            if (destroyed()) throw new Error("Session destroyed")
            // Replaced while joining: the session is idle again.
            if (sfuManager !== manager) return
            emit("topology-changed", "sfu")
            setState("active")
        },

        async leave() {
            if (state !== "active") return
            setState("leaving")
            stopRosterPolling()
            releaseDevice()
            await withdrawCallPresence()
            if (dataUnsubscribe) { dataUnsubscribe(); dataUnsubscribe = null }
            if (meshManager) { await meshManager.leave(); meshManager = null }
            if (sfuManager) { await sfuManager.leave(); sfuManager = null }
            setState("idle")
        },

        get participants() {
            if (meshManager) {
                return meshManager.getParticipants().map(p => ({
                    participantId: p.did,
                    agentDid: agentOfMeshPeer(p.did),
                    stream: p.stream,
                    hasAudio: p.hasAudio,
                    hasVideo: p.hasVideo,
                    isActiveSpeaker: p.isActiveSpeaker,
                }))
            }
            if (sfuManager) {
                return sfuManager.getParticipants().map(p => ({
                    participantId: p.participantId,
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

        peerStates() {
            return meshManager ? meshManager.peerStates() : new Map()
        },

        reconnect(did) {
            meshManager?.reconnect(did)
        },

        async transportOf(did) {
            return meshManager ? meshManager.transportOf(did) : null
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
            destroying = true
            // Capture refs before leave() nulls them
            const mesh = meshManager
            const sfu = sfuManager
            if (state === "active" || state === "joining") await session.leave()
            stopRosterPolling()
            releaseDevice()
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
