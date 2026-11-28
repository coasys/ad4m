/**
 * SFU (Selective Forwarding Unit) client manager.
 *
 * Framework-agnostic WebRTC client for the AD4M executor's embedded
 * SFU.  Handles topology resolution, SDP negotiation, server-pushed
 * renegotiation, simulcast quality preferences, and reconnecting after
 * a failed connection.
 *
 * The client reaches one SFU: its own executor's. Multi-node calls work
 * through cascade pipes between executors, so the client never follows
 * a redirect or a rebalance to another node — it has no way to reach one.
 *
 * One session per device: a join names this device (`deviceId`), and the
 * executor gives it a participant id that every later call about the
 * session names. The same agent on another device is another participant;
 * a later join from this device — another tab — replaces this session, and
 * the manager stops (`replaced`) rather than taking the call back. Each
 * user join sends a random session key that the rebuilds of that call send
 * again; the executor refuses (409) a key another tab's join replaced, so a
 * tab that missed its `sfu-session-replaced` notice stops all the same.
 *
 * Applications (Flux, WE, or any other) wrap this class with their
 * own reactive store and UI layer.  The manager itself only touches
 * Web APIs (RTCPeerConnection, MediaStream) and the AD4M
 * neighbourhood RPC surface.
 */

import { RpcError } from "../apiClient"
import { NeighbourhoodProxy } from "./NeighbourhoodProxy"
import type {
    CallSessionInfo,
    SfuAnswerResult,
    SfuCallRenegotiationOffer,
    SfuConfig,
    SfuDataMessage,
    SfuQualityPreference,
    SfuResumeResult,
    SfuRoomRoster,
    SfuRosterParticipant,
    SfuSessionReplaced,
} from "./SfuTypes"

// ── Public types ──────────────────────────────────────────────────

/** Minimal interface for the neighbourhood client methods the SFU
 *  manager calls.  NeighbourhoodClient satisfies this directly —
 *  pass the client through without an adapter. */
export interface SfuNeighbourhoodApi {
    sfuCallJoin(
        neighbourhoodUrl: string,
        roomName: string,
        sdpOffer: string,
        deviceId: string,
        sessionKey: string,
    ): Promise<CallSessionInfo>
    sfuCallLeave(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
    ): Promise<boolean>
    sfuCallSetQualityPreference(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
        preference: SfuQualityPreference,
    ): Promise<boolean>
    sfuCallAnswerServerOffer(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
        offerId: string,
        sdpAnswer: string,
    ): Promise<SfuAnswerResult>
    subscribeSfuCallRenegotiationOffer(
        targetDid: string,
        callback: (event: SfuCallRenegotiationOffer) => void,
    ): () => void
    /** Ask the SFU to restart ICE on the session `participantId`; the offer
     *  arrives through `subscribeSfuCallRenegotiationOffer`. */
    sfuCallRestartIce(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
    ): Promise<boolean>
    /** Called after a network change, once calls reach the executor again. */
    onNetworkChange(callback: () => void): () => void
    /** Fires after the executor socket opened again after a drop (not on the
     *  first open). */
    onReconnect(callback: () => void): () => void
    /** This session's state on the executor (`sfu.callResume`): listed with
     *  the pending offer and the room's roster, or not listed. */
    sfuCallResume(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
    ): Promise<SfuResumeResult>
    sfuAddIceCandidate(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
        candidate: string,
    ): Promise<boolean>
    sfuSendData(
        neighbourhoodUrl: string,
        roomName: string,
        participantId: string,
        channelLabel: string,
        data: string,
        binary?: boolean,
    ): Promise<boolean>
    subscribeSfuDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void
    subscribeSfuRoomRoster(
        callback: (roster: SfuRoomRoster) => void,
    ): () => void
    subscribeSfuSessionReplaced(
        callback: (event: SfuSessionReplaced) => void,
    ): () => void
}

export type SfuTopology = "sfu" | "mesh" | "cascaded"

export interface SfuCallState {
    topology: SfuTopology
    roomId: string
    participantId: string | null
    sfuPeerDid: string | null
    participants: Map<string, SfuParticipantState>
    localStream: MediaStream | null
    peerConnection: RTCPeerConnection | null
}

/** One session in the call: an agent on two devices is two participants. */
export interface SfuParticipantState {
    participantId: string
    did: string
    stream: MediaStream
    hasAudio: boolean
    hasVideo: boolean
    isActiveSpeaker: boolean
}

export type SfuEvent =
    | "topology-changed"
    | "participant-joined"
    | "participant-left"
    | "active-speaker"
    | "stream-added"
    | "stream-removed"
    /** `(CallEnded)` — the call ended without the user leaving it, and will not come back
     *  without a new `join`. The manager has stopped. `reason` says why. */
    | "ended"
    | "error"

/**
 * Why a call ended without the user leaving it: the payload of `ended`, on
 * the managers and on the `Session`. One event for every end, so an app
 * needs one handler.
 */
export type CallEnded =
    /** A later join from this device (another tab) took the call over; the
     *  executor already dropped this session, and `session` is its notice. */
    | { reason: "replaced"; session: SfuSessionReplaced }
    /** Rebuilding could not keep the call up — MAX_REBUILDS within
     *  REBUILD_WINDOW_MS, or a rejoin window spent without a rejoin. */
    | { reason: "gave-up"; error: Error }
    /** The executor refused this session's resume (403) — the session is
     *  another agent's, or this agent is no longer a member of the
     *  neighbourhood; `error` carries the executor's message. */
    | { reason: "refused"; error: Error }

export type SfuEventCallback = (...args: any[]) => void

/** ICE server configuration for SFU peer connections. */
export interface SfuIceConfig {
    /** Used as given, before `sfuConfig.iceServers`. */
    servers?: RTCIceServer[]
}

// ── Constants ─────────────────────────────────────────────────────

/** The last answer a connection produced for a server offer. The executor
 *  holds its pending offer until an answer lands and resends it, so a
 *  resent offer — an event's, or one the resume's reply returns — gets
 *  this answer sent again, without the SDP steps a second time. */
interface CachedAnswer {
    pc: RTCPeerConnection
    offerId: string
    roomName: string
    participantId: string
    sdpAnswer: string
    /** The executor took the answer: resends of its offer get nothing more. */
    accepted: boolean
}

/** A failure of the signalling socket, not of the call: the socket closed
 *  while the call was out (503) or the reply did not come in time (408). */
function isSocketFault(err: unknown): boolean {
    return err instanceof RpcError && (err.status === 503 || err.status === 408)
}

const DEFAULT_ICE_SERVERS: RTCIceServer[] = [
    { urls: "stun:stun.l.google.com:19302" },
]

/**
 * Where a browser profile keeps its device id. The prefix versions the
 * stored value, as `connect`'s `{version}/ad4m-token` does; it is not the
 * SDK release, so an upgrade does not make every browser a new device.
 */
const DEVICE_ID_KEY = "v1/ad4m-sfu-device-id"

/** Outside a browser: one device id for the life of the process. */
let processDeviceId: string | null = null

function randomId(): string {
    const crypto = (globalThis as any).crypto
    if (crypto?.randomUUID) return crypto.randomUUID()
    return Array.from({ length: 4 }, () => Math.random().toString(36).slice(2, 10)).join("")
}

/**
 * This device's id for SFU calls, unless the app gives one. In a browser it
 * is kept in `localStorage`, which every tab of one profile shares, so the
 * tabs of one browser are one device; outside a browser (Node), the process
 * is the device.
 */
export function defaultDeviceId(): string {
    let storage: Storage | undefined
    try {
        storage = (globalThis as any).localStorage
    } catch {
        storage = undefined // storage denied (sandboxed frame, privacy settings)
    }
    if (storage) {
        try {
            let id = storage.getItem(DEVICE_ID_KEY)
            if (!id) {
                id = randomId()
                storage.setItem(DEVICE_ID_KEY, id)
            }
            return id
        } catch {
            /* unusable storage: fall back to the process */
        }
    }
    return (processDeviceId ??= randomId())
}

// ── Topology resolution ───────────────────────────────────────────

/**
 * Determine the optimal call topology from the SFU config and the
 * current participant count.  Returns the topology, the DID of the
 * SFU peer to connect to (if any), and the full config.
 */
export async function resolveTopology(
    neighbourhood: NeighbourhoodProxy,
    neighbourhoodUrl: string,
    participantCount: number,
): Promise<{ topology: SfuTopology; sfuPeer: string | null; config: SfuConfig }> {
    const config = await neighbourhood.sfuConfig(neighbourhoodUrl)

    if (config.mode === "cascaded") {
        const sfuPeers: string[] = await neighbourhood.sfuPeers(neighbourhoodUrl)
        if (sfuPeers.length > 1) {
            return { topology: "cascaded", sfuPeer: sfuPeers[0], config }
        }
        if (sfuPeers.length === 1) {
            return { topology: "sfu", sfuPeer: sfuPeers[0], config }
        }
        return { topology: "mesh", sfuPeer: null, config }
    }

    if (config.mode === "mesh") {
        return { topology: "mesh", sfuPeer: null, config }
    }

    const sfuPeer = await neighbourhood.sfuPeer(neighbourhoodUrl)

    if (!sfuPeer) {
        if (participantCount <= config.maxMeshParticipants) {
            return { topology: "mesh", sfuPeer: null, config }
        }
        console.warn(
            `SFU peer unavailable and ${participantCount} participants ` +
            `exceeds mesh limit (${config.maxMeshParticipants}). ` +
            `Attempting mesh anyway.`,
        )
        return { topology: "mesh", sfuPeer: null, config }
    }

    if (participantCount <= config.maxMeshParticipants) {
        return { topology: "mesh", sfuPeer, config }
    }

    return { topology: "sfu", sfuPeer, config }
}

// ── SFU Manager ───────────────────────────────────────────────────

/** What one read of a connection's stats says about its traffic: see `SfuManager.watchReceiving`. */
interface ReceiveSample {
    /** RTP packets received, over all media sections. */
    inbound: number
    /** RTP packets sent. */
    outbound: number
    /** RTP packets sent on video. */
    outboundVideo: number
    /** The RTCP reports on this client's media (`remote-inbound-rtp`): changes when a new one arrives. */
    reports: string
}

async function receiveSample(pc: RTCPeerConnection): Promise<ReceiveSample> {
    let inbound = 0
    let outbound = 0
    let outboundVideo = 0
    const reports: string[] = []
    ;(await pc.getStats()).forEach((stat: any) => {
        if (stat.type === "inbound-rtp") inbound += stat.packetsReceived ?? 0
        else if (stat.type === "outbound-rtp") {
            outbound += stat.packetsSent ?? 0
            if (stat.kind === "video") outboundVideo += stat.packetsSent ?? 0
        } else if (stat.type === "remote-inbound-rtp") {
            reports.push(
                [stat.id, stat.timestamp, stat.packetsLost, stat.jitter, stat.roundTripTimeMeasurements].join(":"),
            )
        }
    })
    return { inbound, outbound, outboundVideo, reports: reports.sort().join("|") }
}

export class SfuManager {
    private neighbourhood: SfuNeighbourhoodApi
    private neighbourhoodUrl: string
    private roomId: string
    private agentDid: string
    /** The device this manager calls from: a later join from it replaces this session. */
    private deviceId: string
    /** This call session's key: a rebuild joins with it again, a user's join
     *  makes a new one. The executor refuses a key a later join from this
     *  device replaced, so a tab that missed its notice cannot take the call back. */
    private sessionKey: string = randomId()
    private state: SfuCallState
    private callbacks: Map<SfuEvent, SfuEventCallback[]> = new Map()
    private iceServers: RTCIceServer[]
    /** When each rebuild of this call started, those within the last
     *  REBUILD_WINDOW_MS. ICE connecting does not clear it: a socket that
     *  flaps can rebuild a call whose ICE connects every time. */
    private rebuilds: number[] = []
    private static readonly MAX_REBUILDS = 3
    private static readonly REBUILD_WINDOW_MS = 120_000
    /** How long one rebuild keeps retrying a join the executor cannot take yet. */
    private static readonly REJOIN_WINDOW_MS = 120_000
    /** A rebuild is running: a second failure signal joins it rather than starting another. */
    private reconnecting = false
    /** Bumped by `leave()`, so a reconnect in flight stops before rejoining. */
    private generation: number = 0
    /** Which participant (session and agent) each received media section carries, from the server's mappings. */
    private midToParticipant: Map<string, { participantId: string; did: string }> = new Map()
    /** Participant ids the roster removed and has not listed since. */
    private departed: Set<string> = new Set()
    /** The sender of each kind of local media: declared at join, so a later camera or screen needs no renegotiation. */
    private senders: Map<"audio" | "video", RTCRtpSender> = new Map()
    /** Tracks `replaceTrack` set since the user's join, for a rebuilt connection to send. */
    private replacedTracks: Map<"audio" | "video", MediaStreamTrack | null> = new Map()
    /** The last quality preference set, for a rebuilt connection to restore. */
    private qualityPreference: SfuQualityPreference | null = null
    /** Server-offer work, one step at a time. Every offer — an event's or a
     *  resume reply's — runs its SDP steps to their end before the next
     *  offer's start; no RPC waits here — an answer's send only needs the
     *  queue to have produced it, and the executor matches each answer to
     *  its offer by id, so answers need no order between them. A resume's
     *  reply applies its roster and offer here, but the ask's own wait is
     *  off the queue: the restart offer it triggers on the executor must
     *  not wait for it. */
    private offerQueue: Promise<void> = Promise.resolve()
    /** The `offerId` of the server offer last applied to the connection.
     *  The resume ask snapshots it: the reply's offer applies only while
     *  this is still what it was when the ask went out. */
    private appliedOfferId: string | null = null
    /** The connection a `sfu.callResume` ask is out for — the ask sent and
     *  its reply not yet applied (or its box not yet spent). One at a time
     *  per connection: a trigger while it is set only marks `resumeAgain`,
     *  and every ask that ends runs that one follow-up. Otherwise each of
     *  the ask's triggers — the socket's return, ICE down during a
     *  restart, the chain a reply's offer starts — sent its own RPC, and
     *  each made the executor resend its pending offer. */
    private resumeInFlight: RTCPeerConnection | null = null
    /** A trigger while `resumeInFlight`'s ask was out wants one more ask once it ends. */
    private resumeAgain = false
    /** The last answer this connection produced for a server offer. The
     *  executor's pending offer is the one source of what is still owed:
     *  a socket fault on the send keeps nothing more, since the executor
     *  resends the offer, or the resume's reply returns it. */
    private lastAnswer: CachedAnswer | null = null
    private reconnectUnsubscribe: (() => void) | null = null
    private renegotiationUnsubscribe: (() => void) | null = null
    private rosterUnsubscribe: (() => void) | null = null
    /** For the manager's call, not one join: a reconnect between its leave and its join still hears a notice. */
    private replacedUnsubscribe: (() => void) | null = null
    /** Participant ids of this call a notice can still name: the current one, and any whose leave found it gone. */
    private heldIds: Set<string> = new Set()
    /** Settles when the latest join has its participant id, or failed. */
    private joinSettled: Promise<void> = Promise.resolve()
    private dataChannelUnsubscribe: (() => void) | null = null
    /** The connection an ICE restart is in flight on, until ICE says how it went: `connected`
     *  ends it, `failed` rebuilds the call. ICE going disconnected again joins it, a network
     *  change replaces it. No timer ends it: ICE reports the outcome, and a session the server
     *  dropped meanwhile is found by the roster and `sfu.callResume`. */
    private restarting: RTCPeerConnection | null = null
    private networkUnsubscribe: (() => void) | null = null
    /** The revision of the newest applied roster that listed this session.
     *  A resume's `listed: false` stands unless a roster newer than its ask
     *  listed the session meanwhile — a boolean could not order the two. */
    private listedAtRevision = 0
    /** The highest roster `revision` this join applied; older rosters and
     *  resume replies are dropped. Reset per join: the executor counts
     *  revisions across all its rooms, and a restarted executor counts
     *  from 1 again. */
    private rosterRevision = 0
    /** The receive-stall watchdog's poll, while a connection is the call's: see `watchReceiving`.
     *  The one timer of the call layer's media judgement: RTP statistics raise no event, so they
     *  are read at an interval. */
    private receiveWatch: ReturnType<typeof setInterval> | null = null
    private static readonly RECEIVE_CHECK_MS = 1_000
    /** How long a connection may send and receive no RTP before ICE is restarted. */
    private static readonly RECEIVE_STALL_MS = 5_000
    /** str0m's report intervals on the media it receives (`RtcpReportIntervals`, str0m 0.24). */
    private static readonly VIDEO_REPORT_MS = 1_000
    private static readonly AUDIO_REPORT_MS = 5_000
    /** The reports count as stopped only once more than this many in a row are missing. */
    private static readonly MISSED_REPORTS = 2
    /** The connection a receive stall was found on and restarted ICE for, until media or a report
     *  comes back. Found stalled again while set, the call is rebuilt: that rebuild is one of
     *  `rebuilds`, so a stall no rebuild cures ends with the rebuild budget. */
    private stalled: RTCPeerConnection | null = null

    constructor(
        neighbourhood: SfuNeighbourhoodApi,
        roomId: string,
        agentDid: string,
        neighbourhoodUrl?: string,
        iceConfig?: SfuIceConfig,
        sfuConfig?: SfuConfig,
        /** This device; unset, `defaultDeviceId()`. Apps outside a browser profile (Electron, tests) pass their own. */
        deviceId?: string,
    ) {
        this.neighbourhood = neighbourhood
        this.neighbourhoodUrl = neighbourhoodUrl || ""
        this.roomId = roomId
        this.agentDid = agentDid
        this.deviceId = deviceId || defaultDeviceId()
        this.state = {
            topology: "sfu",
            roomId,
            participantId: null,
            sfuPeerDid: null,
            participants: new Map(),
            localStream: null,
            peerConnection: null,
        }

        // Resolution order for ICE servers:
        //   1. iceConfig.servers — the app's (Session's `iceServers`)
        //   2. sfuConfig.iceServers
        //   3. DEFAULT_ICE_SERVERS — public STUN fallback
        let servers: RTCIceServer[] = []
        if (iceConfig?.servers && iceConfig.servers.length > 0) {
            servers = iceConfig.servers
        } else if (sfuConfig?.iceServers && sfuConfig.iceServers.length > 0) {
            servers = sfuConfig.iceServers.map((s) => ({
                urls: s.urls,
                username: s.username,
                credential: s.credential,
            }))
        }
        this.iceServers = servers.length > 0 ? servers : DEFAULT_ICE_SERVERS
    }

    /**
     * Rebuild the call after its connection failed: leave, then join again
     * on this executor. Leaving first frees the old session's slot at once;
     * the join would replace it anyway, as it comes from this device.
     * Another tab's join can still replace the old session before the leave
     * lands: that notice stops the rebuild wherever it is, and a session its
     * join already got is left. A tab that missed the notice learns it from
     * the rejoin: the executor refuses the replaced session key (409), and
     * the manager stops as the notice would have stopped it, without retrying.
     *
     * Two budgets. At most `MAX_REBUILDS` rebuilds start within
     * `REBUILD_WINDOW_MS`; one more ends the call: `error`, then
     * `ended` with reason `gave-up`, until the user joins again. Within one rebuild, a join that fails because
     * the executor cannot be reached — a restart, an outage — is retried with backoff for up to
     * `REJOIN_WINDOW_MS`, so the call comes back on its own once it can. A window spent without a
     * rejoin ends it the same way (`giveUp`).
     */
    private async reconnect(): Promise<void> {
        const stream = this.state.localStream
        if (!stream || this.reconnecting) return
        const generation = this.generation
        const now = performance.now()
        this.rebuilds = this.rebuilds.filter((at) => now - at < SfuManager.REBUILD_WINDOW_MS)
        if (this.rebuilds.length >= SfuManager.MAX_REBUILDS) {
            console.error(
                `SFU: ${this.rebuilds.length} rebuilds in ${SfuManager.REBUILD_WINDOW_MS / 1000} s; not rebuilding again`,
            )
            this.giveUp(new Error("SFU reconnect attempts exhausted"))
            return
        }
        this.rebuilds.push(now)
        console.info(
            `SFU: connection failed, reconnecting (${this.rebuilds.length}/${SfuManager.MAX_REBUILDS} in ${SfuManager.REBUILD_WINDOW_MS / 1000} s)`,
        )
        this.reconnecting = true
        this.endRestart()
        const previous = this.state.participantId
        try {
            const deadline = performance.now() + SfuManager.REJOIN_WINDOW_MS
            for (let delay = 1000; ; delay = Math.min(delay * 2, 10_000)) {
                this.releaseServerEvents()
                if (this.state.peerConnection) {
                    this.state.peerConnection.close()
                    this.state.peerConnection = null
                }
                await this.leaveSession()
                if (generation !== this.generation) return // left or replaced meanwhile
                // The people in the call have not changed: keep them, drop only
                // the old connection's media. The rejoin's roster says who left.
                this.clearMedia()
                // A stream-removed handler may have left the call.
                if (generation !== this.generation) return
                try {
                    await this.join(stream)
                    if (this.qualityPreference) await this.setQualityPreference(this.qualityPreference)
                    return
                } catch (e) {
                    if (generation !== this.generation) return
                    if (e instanceof RpcError && e.status === 409) {
                        this.stopReplaced({
                            targetDid: this.agentDid,
                            neighbourhoodUrl: this.neighbourhoodUrl,
                            roomName: this.roomId,
                            participantId: previous ?? "",
                        })
                        return
                    }
                    if (performance.now() + delay > deadline) {
                        console.error("SFU reconnect failed:", e)
                        this.giveUp(e instanceof Error ? e : new Error(String(e)))
                        return
                    }
                    // A timer, because no event says the executor can take the
                    // join now: it can fail with the socket open (the executor is
                    // still loading), so the socket's reconnect event does not cover it.
                    console.warn(`SFU: rejoin failed, retrying in ${delay} ms:`, e)
                    await new Promise((resolve) => setTimeout(resolve, delay))
                    if (generation !== this.generation) return
                }
            }
        } finally {
            this.reconnecting = false
        }
    }

    /**
     * Repair the connection in place: the SFU offers new ICE credentials
     * (`sfuCallRestartIce`), the offer is answered like any server offer,
     * and the browser gathers candidates on whatever network it has now.
     * DTLS, tracks and the room slot stay. ICE says how it went: `connected`
     * ends the restart, `failed` rebuilds the call; a session the server
     * dropped meanwhile is found by the roster and `sfu.callResume`. One
     * restart at a time: a call while one is in flight does nothing.
     *
     * The restart names this manager's session, the participant id its
     * current join returned: the executor restarts that session only, never
     * another device's of the same agent. A manager with no session — its
     * join not back yet, between a rebuild's leave and join, left, or
     * replaced by another tab — sends nothing.
     */
    private restartIce(pc: RTCPeerConnection): void {
        const participantId = this.state.participantId
        if (this.restarting || this.reconnecting || this.state.peerConnection !== pc || !participantId) return
        console.info("SFU: restarting ICE")
        this.restarting = pc
        this.sendRestart(pc, participantId)
    }

    /**
     * Ask the executor to restart ICE on `pc`'s session. Nothing is kept
     * when the request does not arrive: the next resume reply says whether
     * the executor holds this session's pending offer — none while the
     * restart is still in flight and ICE is still down asks for a fresh
     * one. A refusal (an executor that lost this session) leaves the
     * decision to ICE and the roster: a blip may still heal.
     */
    private sendRestart(pc: RTCPeerConnection, participantId: string): void {
        this.neighbourhood.sfuCallRestartIce(this.neighbourhoodUrl, this.roomId, participantId).catch((err) => {
            if (isSocketFault(err)) {
                console.warn("SFU: the ICE restart may not have reached the executor:", err)
                return
            }
            console.warn("SFU: ICE restart refused:", err)
        })
    }

    private endRestart(): void {
        this.restarting = null
    }

    on(event: SfuEvent, callback: SfuEventCallback): void {
        if (!this.callbacks.has(event)) {
            this.callbacks.set(event, [])
        }
        this.callbacks.get(event)!.push(callback)
    }

    off(event: SfuEvent, callback?: SfuEventCallback): void {
        if (!callback) {
            this.callbacks.delete(event)
            return
        }
        const cbs = this.callbacks.get(event)
        if (cbs) {
            const idx = cbs.indexOf(callback)
            if (idx !== -1) cbs.splice(idx, 1)
            if (cbs.length === 0) this.callbacks.delete(event)
        }
    }

    private emit(event: SfuEvent, ...args: any[]): void {
        const cbs = this.callbacks.get(event)
        if (cbs) {
            for (const cb of cbs) {
                try {
                    cb(...args)
                } catch (e) {
                    console.error(
                        `SFU event handler error (${event}):`,
                        e,
                    )
                }
            }
        }
    }

    /** Join the SFU call with simulcast support. */
    async join(localStream: MediaStream): Promise<void> {
        const generation = this.generation
        if (!this.reconnecting) {
            this.sessionKey = randomId()
            this.rebuilds = []
        }
        this.state.localStream = localStream

        const pc = new RTCPeerConnection({ iceServers: this.iceServers })
        this.state.peerConnection = pc

        // ICE state monitoring for recovery. "disconnected" (a route change,
        // a network switch, a blip) restarts ICE on this connection at once;
        // "failed" rebuilds the call at once.
        pc.oniceconnectionstatechange = () => {
            if (this.state.peerConnection !== pc) return // superseded
            if (pc.iceConnectionState === "failed") {
                this.endRestart()
                void this.reconnect()
            } else if (pc.iceConnectionState === "disconnected") {
                // The restart is the whole answer: whether the socket it
                // goes out on still answers is the socket layer's call. A
                // restart already in flight gets no blind second request —
                // one a network change sent while ICE still read
                // `connected` may never have reached the executor, and
                // only its pending offer says so: the resume's reply
                // applies it, or asks for the restart again.
                if (this.restarting) this.resume(pc)
                else this.restartIce(pc)
            } else if (
                pc.iceConnectionState === "connected" ||
                pc.iceConnectionState === "completed"
            ) {
                // A working path ends a restart.
                if (this.restarting === pc) this.endRestart()
            }
        }

        // A network change: the path ICE used is gone, though ICE may still
        // report `connected` — that is the old path's state, and it keeps
        // reporting it until consent checks fail, seconds after the path is
        // lost. Restart ICE at once on the socket the change opened,
        // whatever ICE reports. A restart in flight is replaced: its
        // request may have gone out over the old network. Listened to once
        // per call, not per rebuild.
        if (!this.networkUnsubscribe) {
            this.networkUnsubscribe = this.neighbourhood.onNetworkChange(() => {
                const current = this.state.peerConnection
                if (!current || this.reconnecting) return
                this.endRestart()
                this.restartIce(current)
            })
        }
        // The socket came back: ICE is checked, and the executor is asked
        // for what went by while it was down.
        if (!this.reconnectUnsubscribe) {
            this.reconnectUnsubscribe = this.neighbourhood.onReconnect(() => this.socketBack())
        }

        // Both kinds of local media, send-only, declared now whether or not a
        // track exists yet: the SFU only ever receives on these sections, and a
        // camera or screen started later replaces an empty track with no
        // renegotiation. Video goes out as three simulcast layers.
        this.senders.clear()
        for (const kind of ["audio", "video"] as const) {
            const track = this.replacedTracks.has(kind)
                ? this.replacedTracks.get(kind)
                : localStream.getTracks().find((t) => t.kind === kind)
            const transceiver = pc.addTransceiver(track ?? kind, {
                direction: "sendonly",
                streams: [localStream],
                ...(kind === "video"
                    ? {
                          sendEncodings: [
                              { rid: "high", maxBitrate: 1500000 },
                              { rid: "medium", maxBitrate: 500000, scaleResolutionDownBy: 2 },
                              { rid: "low", maxBitrate: 150000, scaleResolutionDownBy: 4 },
                          ],
                      }
                    : {}),
            })
            this.senders.set(kind, transceiver.sender)
        }

        // Incoming media belongs to the participant the server mapped its
        // section to. A track on a section with no mapping is not someone's
        // media, so it is left alone rather than attributed by guesswork.
        pc.ontrack = (event: RTCTrackEvent) => {
            if (this.state.peerConnection !== pc) return
            const mid = event.transceiver?.mid
            const owner = mid ? this.midToParticipant.get(mid) : undefined
            // Someone the roster said left stays gone: a late offer can still name them.
            if (!owner || owner.participantId === this.state.participantId || this.departed.has(owner.participantId)) return
            const participant = this.ensureParticipant(owner.participantId, owner.did)
            participant.stream.addTrack(event.track)
            this.refreshKinds(participant)
            this.emit("stream-added", participant.stream, event.track)
            event.track.addEventListener("ended", () => {
                // A rebuild may have taken the track out already, and told listeners.
                if (!participant.stream.getTracks().includes(event.track)) return
                participant.stream.removeTrack(event.track)
                this.refreshKinds(participant)
                this.emit("stream-removed", participant.stream, event.track)
            })
        }

        // ── Trickle ICE ──────────────────────────────────────────────
        //
        // Buffer candidates gathered before callJoin returns (the
        // server needs the participant registered first).  After
        // callJoin completes, flush the buffer and trickle live.

        const pendingCandidates: string[] = []
        let joinComplete = false

        pc.onicecandidate = (event) => {
            if (!event.candidate) return
            if (joinComplete) {
                if (this.state.peerConnection !== pc || !this.state.participantId) return
                this.neighbourhood.sfuAddIceCandidate(
                    this.neighbourhoodUrl,
                    this.roomId,
                    this.state.participantId,
                    event.candidate.candidate,
                ).catch((err) => {
                    console.error(
                        "SFU: failed to trickle ICE candidate:",
                        err,
                    )
                })
            } else {
                pendingCandidates.push(event.candidate.candidate)
            }
        }

        // Create offer and send immediately — no ICE gathering wait
        const offer = await pc.createOffer()
        await pc.setLocalDescription(offer)

        const sdpOffer = JSON.stringify(pc.localDescription)
        // Server offers can arrive before the join's own answer is applied;
        // they wait for it, then apply in order.
        let markJoined!: () => void
        const joined = new Promise<void>((resolve) => { markJoined = resolve })
        this.joinSettled = joined
        this.subscribeServerEvents(pc, joined)
        let session: CallSessionInfo
        try {
            session = await this.neighbourhood.sfuCallJoin(
                this.neighbourhoodUrl,
                this.roomId,
                sdpOffer,
                this.deviceId,
                this.sessionKey,
            )
        } catch (err) {
            // Settled either way: offers queued behind this join reach
            // `applyServerOffer`, which drops them for the abandoned connection.
            // The executor holds no session for it, so there is nothing to leave.
            markJoined()
            this.abandon(pc)
            throw err
        }
        if (generation !== this.generation) {
            // Left or replaced while the join was out: the session it got is no one's.
            markJoined()
            this.abandon(pc)
            await this.neighbourhood
                .sfuCallLeave(this.neighbourhoodUrl, this.roomId, session.participantId)
                .catch((e) => console.warn("SFU: leaving the call failed:", e))
            return
        }
        this.state.participantId = session.participantId
        this.heldIds.add(session.participantId)

        try {
            const answer = JSON.parse(session.sdpAnswer)
            await pc.setRemoteDescription(new RTCSessionDescription(answer))
        } catch (err) {
            markJoined()
            // Replaced or left while the answer applied: that already closed the
            // connection and settled the session, so the join just ends.
            if (generation !== this.generation) return
            // The executor holds a session this connection cannot use: leave it.
            this.abandon(pc)
            await this.leaveSession()
            throw err
        }
        markJoined()
        this.watchReceiving(pc)

        // Who was in the room when this join landed (`participantId:did`).
        // The roster event keeps it current from here.
        this.applyRoster(
            session.streamMapping.map((entry) => {
                const at = entry.indexOf(":")
                return { participantId: entry.slice(0, at), agentDid: entry.slice(at + 1) }
            }),
            false,
        )

        // Flush buffered trickle ICE candidates
        joinComplete = true
        for (const candidate of pendingCandidates) {
            this.neighbourhood.sfuAddIceCandidate(
                this.neighbourhoodUrl,
                this.roomId,
                session.participantId,
                candidate,
            ).catch((err) => {
                console.error(
                    "SFU: failed to trickle buffered ICE candidate:",
                    err,
                )
            })
        }
    }

    /** Drop the server-event subscriptions of the current join. */
    private releaseServerEvents(): void {
        this.renegotiationUnsubscribe?.()
        this.renegotiationUnsubscribe = null
        this.rosterUnsubscribe?.()
        this.rosterUnsubscribe = null
    }

    /**
     * The room's roster no longer lists this session (its participant id)
     * while `pc` is the call's: the server has dropped this session, and ICE
     * may not say so. In E6 (CI job 53158) one of six sessions never started
     * a rebuild after a 20 s outage the server had dropped it for. The check
     * is per session, not per agent: another device of this agent keeps the
     * DID listed. A roster that leaves this session out asks the executor for
     * this session's state (`resume`) at once, and the reply decides whether
     * the call is rebuilt: `listed: true` keeps it, and `listed: false`
     * rebuilds it unless a roster newer than the ask listed the session
     * (`listedAtRevision`), so a roster published just before this join
     * landed that arrives after it costs one ask, not a rebuild. A manager
     * with no session (between leave and join, or stopped because a later
     * tab replaced it) asks nothing: `stop` drops `pc`, so a replaced session
     * never rejoins and never takes the call back from the newer tab.
     */
    private noticeRoster(pc: RTCPeerConnection, sessions: SfuRosterParticipant[], revision: number): void {
        const participantId = this.state.participantId
        if (!participantId) return
        if (sessions.some((s) => s.participantId === participantId)) {
            this.listedAtRevision = revision
            return
        }
        this.resume(pc)
    }

    /**
     * The call layer's one media-health rule: restart ICE when `pc` sends
     * but receives nothing. After a NAT rebinding the SFU (str0m, the
     * controlled ICE agent) takes this client's media from its new address at
     * once, but sends only on a pair the client nominates. Firefox does not
     * nominate the new one, so the SFU sends this client nothing while
     * everyone else still gets its media; its own ICE checks are answered, so
     * the connection stays `connected` (sfu-soak5, 2026-11-19). A restart
     * makes the browser check and nominate again.
     *
     * ICE state events cover a path that reports its loss; this covers one
     * that does not. RTP statistics raise no event, so they are read at an
     * interval — this poll is the only timer that judges media. ICE is
     * restarted (`restartIce`: this connection, its tracks and its room slot
     * stay) when all of these hold:
     *  - `pc` is the call's and `connected`, its join is done, and no
     *    rebuild or ICE-state restart is running;
     *  - someone else sends: the room lists another participant this
     *    client holds a received track of;
     *  - inbound RTP over all media sections has not grown for 5 s, while
     *    this client sends now (outbound RTP grew since the read before);
     *  - and the RTCP reports on this client's own media have stopped: more
     *    than two report intervals with none, the interval being the
     *    shortest for what this client sends now (str0m: video 1 s, audio
     *    5 s). One lost report is not a dead path.
     *
     * The reports are how "nobody sends" is told apart from "nothing reaches
     * me". A muted track still sends silence or black frames, so RTP keeps
     * coming. Tracks turned off (`replaceTrack(null)`) send no RTP, but the
     * SFU still reports on the media it receives from this client, so the
     * path back is there and a restart could not help. A client that sends
     * nothing gets no reports either, so it cannot tell the two apart and the
     * watchdog leaves it alone.
     *
     * A stall restart is judged by what arrives, not by ICE, which said
     * `connected` all along: media or a report ends the stall. Found stalled
     * again with the restart's stall still open — a fresh window of no media
     * and no reports — the call is rebuilt, and that rebuild is one of
     * `rebuilds`: a stall no rebuild cures ends with the rebuild budget.
     *
     * The interval lives as long as `pc` is the call's: a leave, a
     * replacement, a rebuild or `pc` closing stops it.
     */
    private watchReceiving(pc: RTCPeerConnection): void {
        this.stopReceiveWatch()
        this.stalled = null
        // Since when inbound RTP and the reports have not changed.
        let rtp: { at: number; inbound: number } | null = null
        let reports: { at: number; key: string } | null = null
        let last: ReceiveSample | null = null
        let sampling = false
        const watch = setInterval(async () => {
            // A connection that is no longer the call's, or closed, is watched no more.
            if (this.state.peerConnection !== pc || pc.connectionState === "closed") {
                clearInterval(watch)
                if (this.receiveWatch === watch) this.receiveWatch = null
                return
            }
            if (sampling) return
            if (!this.mayWatchReceiving(pc)) {
                rtp = reports = last = null
                return
            }
            sampling = true
            let sample: ReceiveSample
            try {
                sample = await receiveSample(pc)
            } catch {
                return
            } finally {
                sampling = false
            }
            if (this.receiveWatch !== watch || !this.mayWatchReceiving(pc)) {
                rtp = reports = last = null
                return
            }
            const now = Date.now()
            const previous = last
            // Media or a report since the last read: the path back works.
            if (previous && (sample.inbound !== previous.inbound || sample.reports !== previous.reports)) {
                if (this.stalled === pc) {
                    console.info("SFU: receiving again after the ICE restart")
                    this.stalled = null
                    this.endRestart()
                }
            }
            last = sample
            if (!rtp || sample.inbound !== rtp.inbound) rtp = { at: now, inbound: sample.inbound }
            if (!reports || sample.reports !== reports.key) reports = { at: now, key: sample.reports }

            // An ICE-state restart is running (ICE is down): ICE decides.
            if (this.restarting === pc && this.stalled !== pc && !this.iceUp(pc)) {
                rtp = reports = null
                return
            }
            // What this client sends now, against the read before: not since inbound
            // last moved, or a camera turned off in an idle room keeps the video window.
            if (!previous || sample.outbound <= previous.outbound) return
            const reportMs =
                sample.outboundVideo > previous.outboundVideo ? SfuManager.VIDEO_REPORT_MS : SfuManager.AUDIO_REPORT_MS
            if (now - rtp.at < SfuManager.RECEIVE_STALL_MS || now - reports.at <= SfuManager.MISSED_REPORTS * reportMs) return
            rtp = reports = null
            if (this.stalled === pc) {
                console.info("SFU: the ICE restart brought no media back, rebuilding the call")
                this.stalled = null
                this.endRestart()
                void this.reconnect()
                return
            }
            console.info("SFU: receiving nothing while sending, restarting ICE")
            this.stalled = pc
            this.endRestart()
            this.restartIce(pc)
        }, SfuManager.RECEIVE_CHECK_MS)
        this.receiveWatch = watch
    }

    private iceUp(pc: RTCPeerConnection): boolean {
        return pc.iceConnectionState === "connected" || pc.iceConnectionState === "completed"
    }

    /** Whether the receive-stall watchdog reads `pc` now: see `watchReceiving`. */
    private mayWatchReceiving(pc: RTCPeerConnection): boolean {
        return (
            this.state.peerConnection === pc &&
            pc.connectionState === "connected" &&
            !!this.state.participantId &&
            !this.reconnecting &&
            [...this.state.participants.values()].some((p) => p.hasAudio || p.hasVideo)
        )
    }

    private stopReceiveWatch(): void {
        if (this.receiveWatch) {
            clearInterval(this.receiveWatch)
            this.receiveWatch = null
        }
    }

    /**
     * Leave this manager's session on the executor, if it has one, and forget
     * it. Only the participant id a join returned is ever left: a manager
     * that never joined, or whose join was refused, sends nothing, so it can
     * never end a session another tab of this agent is in.
     */
    private async leaveSession(): Promise<void> {
        const participantId = this.state.participantId
        if (!participantId) return
        this.state.participantId = null
        // A session the leave found gone may have been replaced, its notice
        // still on the way. One the leave ended, or that it never reached, a
        // notice names only if this manager's own rejoin replaced it.
        let found = true
        try {
            found = await this.neighbourhood.sfuCallLeave(this.neighbourhoodUrl, this.roomId, participantId)
        } catch (e) {
            /* best-effort: the executor frees a dead session on its own */
            console.warn("SFU: leaving the call failed:", e)
        }
        if (found) this.heldIds.delete(participantId)
    }

    /** Undo a join that failed part way: its subscriptions and its connection. */
    private abandon(pc: RTCPeerConnection): void {
        this.releaseServerEvents()
        if (this.heldIds.size === 0) {
            this.replacedUnsubscribe?.()
            this.replacedUnsubscribe = null
        }
        pc.close()
        if (this.state.peerConnection === pc) this.state.peerConnection = null
    }

    /**
     * Subscribe to the server's events for this join: renegotiation offers
     * and the room roster. `join` calls this before `sfuCallJoin`: the
     * executor sends a socket only the events it watches, and a call
     * carries the pending watch ahead of itself, so no event the join
     * causes is lost. Offers wait for `joined` and then apply one at a
     * time. Of the offers waiting when a step starts, only the newest is
     * applied (the executor has one pending offer at a time, so the others
     * are stale), with the track mappings of all of them, in arrival order.
     * One that arrives for a connection already replaced is dropped.
     */
    private subscribeServerEvents(pc: RTCPeerConnection, joined: Promise<void>): void {
        // Join may run more than once (a reconnect). The prior
        // subscriptions go after the new ones exist, so the event socket
        // never drops to zero listeners in between.
        const previous = [this.renegotiationUnsubscribe, this.rosterUnsubscribe]
        const ours = (e: { neighbourhoodUrl: string; roomName: string }) =>
            e.neighbourhoodUrl === this.neighbourhoodUrl && e.roomName === this.roomId

        // The executor sends this agent's offers and notices to each of its
        // sessions; only those naming this session's participant id are ours,
        // and that id is known once the join returns.
        const waiting: SfuCallRenegotiationOffer[] = []
        // A queue per connection: a step stuck on a dropped connection holds nothing of this one.
        this.offerQueue = Promise.resolve()
        this.listedAtRevision = 0
        this.rosterRevision = 0
        // Per-session offer state: offer ids name one session's offers,
        // so this session decides on its own ids afresh, and the old
        // session's last answer is not this one's to send.
        this.lastAnswer = null
        this.appliedOfferId = null
        // The old connection's resume ask and whatever it marked belong to
        // it: its late reply applies nothing and runs nothing here.
        this.resumeInFlight = null
        this.resumeAgain = false
        this.renegotiationUnsubscribe = this.neighbourhood.subscribeSfuCallRenegotiationOffer(
            this.agentDid,
            (event) => {
                if (!ours(event)) return
                waiting.push(event)
                this.offerQueue = this.offerQueue
                    .then(() => joined)
                    .then(() => {
                        const offers = waiting.splice(0).filter((e) => e.participantId === this.state.participantId)
                        if (offers.length > 0) return this.applyServerOffers(pc, offers)
                    })
                    .catch((err) => console.warn("SFU: applying a server offer failed:", err))
            },
        )
        this.rosterUnsubscribe = this.neighbourhood.subscribeSfuRoomRoster((roster) => {
            if (!ours(roster) || this.state.peerConnection !== pc) return
            if (roster.revision <= this.rosterRevision) return
            this.rosterRevision = roster.revision
            void joined.then(() => {
                if (this.state.peerConnection !== pc) return
                this.applyRoster(roster.participants, true)
                this.noticeRoster(pc, roster.participants, roster.revision)
            })
        })
        // A notice can name the id the join in flight is about to return.
        this.replacedUnsubscribe ??= this.neighbourhood.subscribeSfuSessionReplaced((event) => {
            if (!ours(event)) return
            const stopIfHeld = () => {
                if (this.heldIds.has(event.participantId)) this.stopReplaced(event)
            }
            stopIfHeld()
            void this.joinSettled.then(stopIfHeld)
        })

        for (const unsubscribe of previous) unsubscribe?.()
    }

    /** Apply the newest of `offers` (in arrival order) to `pc` and answer it, with the track mappings of all. */
    private async applyServerOffers(pc: RTCPeerConnection, offers: SfuCallRenegotiationOffer[]): Promise<void> {
        const event = offers[offers.length - 1]
        if (this.state.peerConnection !== pc) return
        // The executor's resend of the offer this connection last answered —
        // an event's resend or one the resume's reply returned: its answer
        // is the cached one, sent again without applying the SDP a second
        // time. Once the executor took it, its resends get nothing.
        const last = this.lastAnswer
        if (last && last.pc === pc && last.offerId === event.offerId) {
            if (last.accepted) return
            void this.sendAnswer(last)
            return
        }
        // The mapping first: the tracks this offer adds arrive while it is applied.
        for (const entry of offers.flatMap((offer) => offer.trackMapping ?? [])) {
            this.midToParticipant.set(entry.mid, { participantId: entry.participantId, did: entry.agentDid })
        }
        let sdpAnswer: string
        try {
            await pc.setRemoteDescription(new RTCSessionDescription(JSON.parse(event.sdpOffer)))
            if (this.state.peerConnection !== pc) return
            const answer = await pc.createAnswer()
            if (this.state.peerConnection !== pc) return
            await pc.setLocalDescription(answer)
            sdpAnswer = JSON.stringify(pc.localDescription)
        } catch (err) {
            if (this.state.peerConnection !== pc) return
            // An offer this connection cannot take leaves it out of step with
            // the server: rebuild. `reconnect` reports an error only if that fails too.
            console.warn("SFU: renegotiation failed, rebuilding the connection:", err)
            void this.reconnect()
            return
        }
        const answer: CachedAnswer = { pc, offerId: event.offerId, roomName: event.roomName, participantId: event.participantId, sdpAnswer, accepted: false }
        this.appliedOfferId = event.offerId
        this.lastAnswer = answer
        // The send is not waited on: the reply only marks the answer
        // accepted or acts on `unknown-session`, and the offer's id already
        // matches it to its offer — so the next offer's SDP need not wait.
        void this.sendAnswer(answer)
    }

    /**
     * Send an answer to the executor, under the `offerId` of the offer it
     * answers: the executor refuses one that names another offer, so a stale
     * answer cannot use up a newer same-mid offer. The send runs beside the
     * offer queue, not inside it — the reply is nothing a later offer's SDP
     * steps wait on. A refusal of `stale` or
     * `no-pending` is non-acceptance, not a failure — the offer was settled
     * or superseded, or the server's own SDP step refused it (a str0m
     * rejection, `ChangesOutOfOrder`) — and whether the connection needs
     * repair is media and session evidence's call, not a rebuild's here.
     * One of `unknown-session` rebuilds the call. A socket fault (503,
     * 408) loses the send and keeps nothing more: the executor holds its
     * offer and resends it — or the resume's reply returns it — and the
     * resent offer gets the cached answer. Anything else leaves the
     * verdict to ICE and the roster, which see whether the session is
     * really gone.
     */
    private async sendAnswer(answer: CachedAnswer): Promise<void> {
        if (this.state.peerConnection !== answer.pc || this.state.participantId !== answer.participantId) return
        try {
            const result = await this.neighbourhood.sfuCallAnswerServerOffer(
                this.neighbourhoodUrl,
                answer.roomName,
                answer.participantId,
                answer.offerId,
                answer.sdpAnswer,
            )
            // The reply names the connection and session it went out for;
            // one that lands after either changed is about a call that is
            // gone: its refusal must not rebuild the new one, its
            // acceptance must not mark this one's offer answered.
            if (this.state.peerConnection !== answer.pc || this.state.participantId !== answer.participantId) return
            if (result.accepted) {
                answer.accepted = true
            } else if (result.reason === "unknown-session") {
                console.warn("SFU: the executor no longer holds this session; rebuilding the call")
                void this.reconnect()
            }
            // `stale` or `no-pending`: not accepted — the offer was settled or
            // superseded, or the server's own SDP step refused it.
        } catch (err) {
            if (this.state.peerConnection !== answer.pc) return
            if (isSocketFault(err)) {
                console.warn(
                    "SFU: the answer did not reach the executor; the offer's resend gets the cached one:",
                    err,
                )
                return
            }
            console.warn("SFU: the executor refused the answer; keeping the connection:", err)
        }
    }

    /**
     * The executor socket opened again: `sfu.callResume` returns what went
     * by while the socket was down — an offer the executor still holds and
     * resends, a roster. Nothing is replayed: the executor's pending offer
     * is the one source of what is still owed. ICE is not checked here: the
     * path ICE uses is the one it had, so media that kept flowing through
     * the fault is left alone, and ICE reports the path's loss itself
     * (`disconnected` restarts, `failed` rebuilds), as does the receive-stall
     * watchdog for a path that reads `connected` and carries nothing. (A
     * network change is a different event: the path changed, so it restarts
     * ICE at once — see the `onNetworkChange` handler in `join`.)
     */
    private socketBack(): void {
        const pc = this.state.peerConnection
        if (!pc || this.reconnecting) return
        // Off the queue: the ask waits out a join still settling, and its
        // reply applies on the queue — so the restart offer it triggers on
        // the executor is answered while the reply is still out.
        this.resume(pc)
    }

    /**
     * Ask the executor for this session's state (`sfu.callResume`). The ask
     * is not a queue step — the restart offer it triggers on the executor
     * is answered while the reply is still out — but it waits out a join
     * still settling, and its reply's roster and pending offer apply on the
     * queue like the events that carry them. `listed: false` — the room or
     * the session gone — rebuilds the call, unless a roster newer than the
     * ask listed it again meanwhile. The reply names the connection and
     * session it was asked for: one that lands after either changed applies
     * nothing. The ask has no timer of its own: the socket layer fails a
     * request on a closed socket (503) or one that gets no reply (408), and
     * either keeps the call and waits for the next trigger.
     * One ask per connection at a time: while `pc`'s is out a trigger only
     * marks `resumeAgain`, and `resumeDone` — once the reply is applied —
     * sends the one follow-up the marks add up to, while `pc` is still this
     * call's.
     * A socket fault (503, 408) keeps the call: the next socket reconnect
     * asks again. A 403 ends it: the executor refused this session's resume —
     * it is another agent's, or this agent lost its membership — and a
     * refusal does not heal.
     */
    private resume(pc: RTCPeerConnection): void {
        const participantId = this.state.participantId
        if (!participantId || this.state.peerConnection !== pc) return
        if (this.resumeInFlight === pc) {
            this.resumeAgain = true
            return
        }
        this.resumeInFlight = pc
        const joined = this.joinSettled
        void joined.then(async () => {
            // The join the ask waited for may not be this call's any more.
            if (this.state.peerConnection !== pc || this.state.participantId !== participantId) {
                this.resumeDone(pc, participantId)
                return
            }
            const askedAtRevision = this.rosterRevision
            // What `askedAtRevision` is for the roster, this is for offers:
            // the reply's offer applies only while the offer last applied
            // is still the one the ask went out under — see `applyResume`.
            const askedAtOfferId = this.appliedOfferId
            let reply: { result: SfuResumeResult } | { error: unknown }
            try {
                reply = { result: await this.neighbourhood.sfuCallResume(this.neighbourhoodUrl, this.roomId, participantId) }
            } catch (error) {
                reply = { error }
            }
            this.offerQueue = this.offerQueue
                .then(() => this.applyResume(pc, participantId, askedAtRevision, askedAtOfferId, reply))
                .catch((err) => console.warn("SFU: applying the resume's reply failed:", err))
                .finally(() => this.resumeDone(pc, participantId))
        })
    }

    /**
     * `pc`'s resume ask ended — its reply applied, or the join it waited on
     * turned out to be another call's. The in-flight marker frees, and a
     * trigger that marked `resumeAgain` while the ask was out gets its one
     * follow-up, only while `pc` is still this call's connection and
     * session. An ask ending after its connection was replaced touches
     * nothing: the new connection's marker and marks are its own.
     */
    private resumeDone(pc: RTCPeerConnection, participantId: string): void {
        if (this.resumeInFlight !== pc) return
        this.resumeInFlight = null
        const again = this.resumeAgain
        this.resumeAgain = false
        if (!again || this.state.peerConnection !== pc || this.state.participantId !== participantId) return
        this.resume(pc)
    }

    /**
     * Apply a `sfu.callResume` reply on the offer queue: `resume` asked it,
     * `askedAtRevision` is the roster revision and `askedAtOfferId` the
     * last applied offer when it did. See `resume` for what each answer
     * means for the call.
     */
    private async applyResume(
        pc: RTCPeerConnection,
        participantId: string,
        askedAtRevision: number,
        askedAtOfferId: string | null,
        reply: { result: SfuResumeResult } | { error: unknown },
    ): Promise<void> {
        if ("error" in reply) {
            const err = reply.error
            if (this.state.peerConnection !== pc || this.state.participantId !== participantId) return
            if (isSocketFault(err)) {
                console.warn("SFU: resuming the session failed with the socket; the next reconnect asks again:", err)
                return
            }
            if (err instanceof RpcError && err.status === 403) {
                // The executor refused this session's resume — an ownership
                // mismatch only when its message says so; it refuses a
                // former member's own session the same way.
                console.error(
                    err.body.includes("another agent")
                        ? "SFU: the session is another agent's; the call ends:"
                        : "SFU: the executor refused this session's resume; the call ends:",
                    err,
                )
                this.endCall({ reason: "refused", error: err })
                return
            }
            console.warn("SFU: resuming the session failed; keeping the call:", err)
            return
        }
        const result = reply.result
        if (this.state.peerConnection !== pc || this.state.participantId !== participantId) return
        if (!result.listed) {
            // Only a roster applied since the request went out, and listing
            // this session, cancels the rebuild: the last one it knows of may
            // predate the socket drop that took the session and its departure
            // roster with it.
            if (this.listedAtRevision > askedAtRevision) return
            console.info("SFU: the executor no longer lists this session, rebuilding the call")
            void this.reconnect()
            return
        }
        if (result.roster && result.roster.revision > this.rosterRevision) {
            this.rosterRevision = result.roster.revision
            this.applyRoster(result.roster.participants, true)
            // The reply says the session is listed, whatever the roster it carries shows.
            this.listedAtRevision = Math.max(this.listedAtRevision, result.roster.revision)
        }
        // The ask's wait ran off the queue: an offer event could apply
        // while the reply was out, and `appliedOfferId` then moved to what
        // the reply carries or past it — the executor snapshotted its
        // pending offer when the ask reached it, so nothing it sent since
        // is older. Applying the reply's offer anyway would take the
        // connection back to the older SDP, and its answer comes back
        // `stale` while the newer offer's ICE credentials are lost. The
        // executor holds its current offer and resends it, so the drop
        // loses nothing; while `appliedOfferId` is still the ask's, the
        // reply's offer applies.
        const offer = result.offer?.participantId === participantId ? result.offer : undefined
        if (offer && this.appliedOfferId === askedAtOfferId) {
            await this.applyServerOffers(pc, [offer])
        }
        // While a restart is in flight and ICE is still down, the reply
        // decides what is owed next. No offer for this session means the
        // executor holds no pending offer for it — a restart request it
        // never saw is the usual cause, so a fresh one is asked for. An
        // offer may be the restart already, or only an ordinary
        // renegotiation the drop hid — the wire does not say — so once it
        // is answered the executor is asked again: its next reply carries
        // the next pending offer (answered the same way) or, carrying
        // none, asks for the restart. The chain ends when ICE leaves
        // `disconnected`/`failed`, or when the restart ends. ICE `checking`
        // says the restart's answer reached the executor — nothing is owed then.
        if (this.restarting === pc && (pc.iceConnectionState === "disconnected" || pc.iceConnectionState === "failed")) {
            if (offer) this.resume(pc)
            else this.sendRestart(pc, participantId)
        }
    }

    /**
     * Make the participants match `sessions`, the room's roster: everyone new
     * joins, and — when `complete`, as the roster event is — everyone
     * missing leaves. The join's own roster can lag the room, so it only adds.
     * This session is never a participant; this agent's other devices are.
     */
    private applyRoster(sessions: { participantId: string; agentDid: string }[], complete: boolean): void {
        const present = sessions.filter((s) => s.participantId !== this.state.participantId)
        for (const { participantId, agentDid } of present) {
            this.departed.delete(participantId)
            this.ensureParticipant(participantId, agentDid)
        }
        if (!complete) return
        const listed = new Set(present.map((s) => s.participantId))
        for (const [participantId, participant] of [...this.state.participants]) {
            if (listed.has(participantId)) continue
            this.state.participants.delete(participantId)
            this.departed.add(participantId)
            for (const [mid, owner] of this.midToParticipant) {
                if (owner.participantId === participantId) this.midToParticipant.delete(mid)
            }
            this.emit("participant-left", participant)
        }
    }

    /** The participant for session `participantId` of `did`, joining them to the call if they are new. */
    private ensureParticipant(participantId: string, did: string): SfuParticipantState {
        let participant = this.state.participants.get(participantId)
        if (!participant) {
            participant = {
                participantId,
                did,
                stream: new MediaStream(),
                hasAudio: false,
                hasVideo: false,
                isActiveSpeaker: false,
            }
            this.state.participants.set(participantId, participant)
            this.emit("participant-joined", participant)
        }
        return participant
    }

    /**
     * A later join from this device replaced a session of this call: the
     * executor has already dropped it. Stop as `leave` does, but never
     * rejoin — the other tab has the call now — and say so with `ended`,
     * reason `replaced`. A session this manager got since (a reconnect's) is
     * left.
     */
    private stopReplaced(event: SfuSessionReplaced): void {
        console.info("SFU: another tab on this device joined the call; this one stops")
        if (this.state.participantId === event.participantId) this.state.participantId = null
        this.stop()
        void this.leaveSession()
        this.resetParticipants()
        this.emit("ended", { reason: "replaced", session: event } satisfies CallEnded)
    }

    /**
     * A terminal give-up: the call cannot come back on its own. No rejoin
     * will run, so no roster will settle who is still here — the
     * participants are told they left first, then the failure is reported
     * and the call ended `gave-up`.
     */
    private giveUp(error: Error): void {
        this.resetParticipants()
        this.emit("error", error)
        this.endCall({ reason: "gave-up", error })
    }

    /**
     * The call ended without the user leaving: stop as `leave` does, so no
     * dead connection is left behind, and say why with `ended`. A new
     * `join` starts the call again with a fresh rebuild budget.
     */
    private endCall(event: CallEnded): void {
        console.error(`SFU: the call ended (${event.reason})`)
        this.stop()
        void this.leaveSession()
        this.resetParticipants()
        this.emit("ended", event)
    }

    /** End this manager's call locally: no reconnect in flight continues, nothing is sent. */
    private stop(): void {
        this.generation++
        this.releaseServerEvents()
        this.replacedUnsubscribe?.()
        this.replacedUnsubscribe = null
        this.heldIds.clear()
        this.endRestart()
        this.stalled = null
        this.networkUnsubscribe?.()
        this.networkUnsubscribe = null
        this.reconnectUnsubscribe?.()
        this.reconnectUnsubscribe = null
        this.stopReceiveWatch()
        this.lastAnswer = null
        this.senders.clear()
        this.replacedTracks.clear()
        this.qualityPreference = null
        if (this.dataChannelUnsubscribe) {
            try {
                this.dataChannelUnsubscribe()
            } catch {
                /* swallow */
            }
            this.dataChannelUnsubscribe = null
        }
        if (this.state.peerConnection) {
            this.state.peerConnection.close()
            this.state.peerConnection = null
        }
        this.state.localStream = null
    }

    private refreshKinds(participant: SfuParticipantState): void {
        participant.hasAudio = participant.stream.getAudioTracks().length > 0
        participant.hasVideo = participant.stream.getVideoTracks().length > 0
    }

    /** End the call: locally, and on the executor if this manager joined. */
    async leave(): Promise<void> {
        this.stop()
        await this.leaveSession()
        this.resetParticipants()
    }

    /** Take the old connection's tracks out of every participant's stream,
     *  keeping the participants and their streams for the new connection. */
    private clearMedia(): void {
        for (const participant of this.state.participants.values()) {
            const tracks = participant.stream.getTracks()
            for (const track of tracks) participant.stream.removeTrack(track)
            this.refreshKinds(participant)
            for (const track of tracks) this.emit("stream-removed", participant.stream, track)
        }
        this.midToParticipant.clear()
    }

    /** Forget the call's participants, telling listeners each one left. */
    private resetParticipants(): void {
        for (const [, participant] of this.state.participants) {
            this.emit("participant-left", participant)
        }
        this.state.participants.clear()
        this.midToParticipant.clear()
        this.departed.clear()
    }

    async setQualityPreference(
        preference: SfuQualityPreference,
    ): Promise<void> {
        this.qualityPreference = preference
        if (!this.state.participantId) {
            console.warn(
                "Cannot set quality preference: not connected to SFU",
            )
            return
        }
        try {
            await this.neighbourhood.sfuCallSetQualityPreference(
                this.neighbourhoodUrl,
                this.roomId,
                this.state.participantId,
                preference,
            )
        } catch (e) {
            console.error("Failed to set quality preference:", e)
            this.emit("error", e)
        }
    }

    getState(): Readonly<SfuCallState> {
        return this.state
    }

    getParticipants(): SfuParticipantState[] {
        return Array.from(this.state.participants.values())
    }

    // ── Track replacement ──────────────────────────────────────────

    /**
     * Replace the outbound track of a given kind on the SFU peer connection.
     *
     * Both kinds were declared at join, so this is always
     * `RTCRtpSender.replaceTrack` — no renegotiation. Pass `null` to stop
     * sending that kind.
     */
    async replaceTrack(kind: "audio" | "video", track: MediaStreamTrack | null): Promise<void> {
        this.replacedTracks.set(kind, track)
        // Between connections (a rebuild) the next join sends it.
        if (this.state.peerConnection) await this.senders.get(kind)?.replaceTrack(track)
    }

    // ── Data channel relay ──────────────────────────────────────────

    /**
     * Send data to all other participants in the room via the SFU.
     * Text payloads pass as-is; binary payloads should arrive
     * base64-encoded with `binary: true`.
     */
    async sendData(
        channelLabel: string,
        data: string,
        binary: boolean = false,
    ): Promise<void> {
        if (!this.state.participantId) {
            console.warn("Cannot send data: not connected to SFU")
            return
        }
        try {
            await this.neighbourhood.sfuSendData(
                this.neighbourhoodUrl,
                this.roomId,
                this.state.participantId,
                channelLabel,
                data,
                binary,
            )
        } catch (e) {
            console.error("SFU: sendData failed:", e)
            this.emit("error", e)
        }
    }

    /**
     * Subscribe to data channel messages from other participants — this
     * agent's other devices included, this session's own messages not.
     * Returns an unsubscribe function.  Only one subscription per
     * manager instance — a second call replaces the first.
     */
    subscribeDataChannel(
        callback: (message: SfuDataMessage) => void,
    ): () => void {
        // Tear down any existing subscription
        if (this.dataChannelUnsubscribe) {
            try {
                this.dataChannelUnsubscribe()
            } catch {
                /* swallow */
            }
        }
        this.dataChannelUnsubscribe = this.neighbourhood.subscribeSfuDataChannel((message) => {
            if (message.senderParticipantId && message.senderParticipantId === this.state.participantId) return
            callback(message)
        })
        return () => {
            if (this.dataChannelUnsubscribe) {
                try {
                    this.dataChannelUnsubscribe()
                } catch {
                    /* swallow */
                }
                this.dataChannelUnsubscribe = null
            }
        }
    }

    async destroy(): Promise<void> {
        try {
            await this.leave()
        } catch (e) {
            console.error("Error during SFU destroy:", e)
        }
        this.callbacks.clear()
        this.midToParticipant.clear()
    }
}
