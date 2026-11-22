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
    /** `(SfuSessionReplaced)` — a later join from this device took the call over; this manager stopped. */
    | "replaced"
    /** `(SfuCallEnded)` — the call ended on its own and will not come back without a new `join`:
     *  rebuilding gave up on it (`reason: "gave-up"`), or the executor refused this session's
     *  resume (`reason: "refused"`). The manager has stopped. */
    | "call-ended"
    | "error"

/** Why a call ended without the user leaving it. */
export interface SfuCallEnded {
    /** `gave-up`: rebuilding could not keep the call up — MAX_REBUILDS within
     *  REBUILD_WINDOW_MS, or a rejoin window spent without a rejoin.
     *  `refused`: the executor refused this session's resume (403) — the
     *  session is another agent's, or this agent is no longer a member of
     *  the neighbourhood; `error` carries the executor's message. */
    reason: "gave-up" | "refused"
    error: Error
}

export type SfuEventCallback = (...args: any[]) => void

/** ICE server configuration for SFU peer connections. */
export interface SfuIceConfig {
    /** Used as given, before `sfuConfig.iceServers`. */
    servers?: RTCIceServer[]
}

// ── Constants ─────────────────────────────────────────────────────

/** An answer made for a server offer, kept until the executor has it. */
interface UnsentAnswer {
    pc: RTCPeerConnection
    offerId: string
    roomName: string
    participantId: string
    sdpAnswer: string
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
    /** The executor's wait before the first resend of an unanswered offer.
     *  `OFFER_RETRY_BASE` in `rust-executor/src/sfu/server.rs`. */
    private static readonly OFFER_RESEND_BASE_MS = 10_000
    /** How long ICE has to come back after that resend arrives. */
    private static readonly RESTART_AFTER_RESEND_MS = 5_000
    /** How long an ICE restart has to bring the connection back before a rebuild.
     *  The window stays open through the first resend, then the previous 5 s
     *  for ICE. A restart that connects clears the timer at once. */
    private static readonly RESTART_TIMEOUT_MS =
        SfuManager.OFFER_RESEND_BASE_MS + SfuManager.RESTART_AFTER_RESEND_MS
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
     *  offer's start; only an RPC is boxed, an answer's at most
     *  OFFER_STEP_TIMEOUT_MS. A resume's reply applies its roster and offer
     *  here, but the ask's own wait is off the queue: the restart offer it
     *  triggers on the executor must not wait for it. */
    private offerQueue: Promise<void> = Promise.resolve()
    /** How long one answer RPC of `offerQueue` may hold the steps behind it. */
    private static readonly OFFER_STEP_TIMEOUT_MS = 10_000
    /** How long a `sfu.callResume` ask waits for its reply before dropping
     *  it. The RPC's own timeout is 30 s, but an ICE restart has only
     *  RESTART_TIMEOUT_MS to bring media back, and the last
     *  RESTART_AFTER_RESEND_MS of that is the resent offer's share once its
     *  answer is out. An ask made while a restart is in flight is bounded
     *  by the restart's remaining time, which is always within this. */
    private static readonly RESUME_STEP_TIMEOUT_MS =
        SfuManager.RESTART_TIMEOUT_MS - SfuManager.RESTART_AFTER_RESEND_MS
    /** The `offerId` of the last server offer the executor took an answer
     *  for. Its resends are not answered again. */
    private answeredOfferId: string | null = null
    /** The `offerId` of the server offer last applied to the connection,
     *  whether or not its answer reached the executor. Only its answer is
     *  kept on a socket fault: an older offer's call failing late must not
     *  displace the answer the executor is still waiting on. */
    private appliedOfferId: string | null = null
    /** An answer a socket fault kept from the executor (503, 408): the
     *  connection is fine, so it goes out again once the socket is back, or
     *  when the executor sends its offer again. */
    private unsentAnswer: UnsentAnswer | null = null
    /** An ICE restart request a socket fault (503, 408) kept from the
     *  executor: the restart it belongs to stays in flight, and the request
     *  goes out again from `socketBack`. Set only while the restart it
     *  belongs to is the current one — a network change that replaced it
     *  ends its wait, and its own late failure retains nothing. */
    private unsentRestart: { pc: RTCPeerConnection; participantId: string; timer: ReturnType<typeof setTimeout> } | null = null
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
    /** An ICE restart is in flight until this fires: ICE going disconnected
     *  again joins it, a network change replaces it. */
    private restartTimer: ReturnType<typeof setTimeout> | null = null
    /** The restart in flight's deadline — `restartTimer`'s fire time — so an
     *  ask it made (`sfu.callResume` after the socket's return) is bounded
     *  by what is left of the window, never a fresh one. */
    private restartDeadline: number | null = null
    private networkUnsubscribe: (() => void) | null = null
    /** Running while the room's roster leaves this session out: see `noticeRoster`. */
    private unlistedTimer: ReturnType<typeof setTimeout> | null = null
    /** The newest applied roster left this session out and no ask has cleared
     *  it since: a socket-faulted ask for it is asked again within the grace. */
    private unlisted = false
    /** The revision of the newest applied roster that listed this session.
     *  A resume's `listed: false` stands unless a roster newer than its ask
     *  listed the session meanwhile — a boolean could not order the two. */
    private listedAtRevision = 0
    /** The highest roster `revision` this join applied; older rosters and
     *  resume replies are dropped. Reset per join: a new room's revisions
     *  start at 1 again. */
    private rosterRevision = 0
    /** How long the roster may leave this session out before the call is rebuilt. */
    private static readonly UNLISTED_GRACE_MS = 3_000
    /** How long after a socket reconnect — with no network change — ICE has to be connected before it is restarted. */
    private static readonly ICE_CHECK_AFTER_SOCKET_MS = 2_000
    /** Running from a socket reconnect until ICE is looked at: see `checkIceSoon`. */
    private iceCheckTimer: ReturnType<typeof setTimeout> | null = null

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
     * `call-ended` with reason `gave-up`, until the user joins again. Within one rebuild, a join that fails because
     * the executor cannot be reached — a restart, an outage — is retried with backoff for up to
     * `REJOIN_WINDOW_MS`, so the call comes back on its own once it can. A window spent without a
     * rejoin ends it the same way (`giveUp`).
     */
    private async reconnect(): Promise<void> {
        const stream = this.state.localStream
        if (!stream || this.reconnecting) return
        const generation = this.generation
        const now = Date.now()
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
            const deadline = Date.now() + SfuManager.REJOIN_WINDOW_MS
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
                    if (Date.now() + delay > deadline) {
                        console.error("SFU reconnect failed:", e)
                        this.giveUp(e instanceof Error ? e : new Error(String(e)))
                        return
                    }
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
     * DTLS, tracks and the room slot stay. If the connection is not back
     * within `RESTART_TIMEOUT_MS`, the call is rebuilt. One restart at a
     * time: a call while one is in flight does nothing.
     *
     * The restart names this manager's session, the participant id its
     * current join returned: the executor restarts that session only, never
     * another device's of the same agent. A manager with no session — its
     * join not back yet, between a rebuild's leave and join, left, or
     * replaced by another tab — sends nothing.
     */
    private restartIce(pc: RTCPeerConnection): void {
        const participantId = this.state.participantId
        if (this.restartTimer || this.reconnecting || this.state.peerConnection !== pc || !participantId) return
        const generation = this.generation
        console.info("SFU: restarting ICE")
        const restart = setTimeout(() => {
            this.restartTimer = null
            this.restartDeadline = null
            if (generation !== this.generation || this.state.peerConnection !== pc) return
            if (pc.iceConnectionState === "connected" || pc.iceConnectionState === "completed") return
            void this.reconnect()
        }, SfuManager.RESTART_TIMEOUT_MS)
        this.restartTimer = restart
        this.restartDeadline = Date.now() + SfuManager.RESTART_TIMEOUT_MS
        this.sendRestart(pc, participantId, restart)
    }

    /**
     * Ask the executor to restart ICE on `pc`'s session. A socket fault
     * (503, 408) means the request may never have arrived — the socket the
     * network change closed rejected it — so it is kept for `socketBack`
     * to send again, but only while `restart` is still the current one:
     * a network change that replaced it already sent its own request, and
     * keeping the old one's would send a restart the restart in flight
     * never asked for. A refusal (an executor that lost this session)
     * leaves the decision to the restart's timeout: a blip may still heal.
     */
    private sendRestart(pc: RTCPeerConnection, participantId: string, restart: ReturnType<typeof setTimeout>): void {
        this.neighbourhood.sfuCallRestartIce(this.neighbourhoodUrl, this.roomId, participantId).catch((err) => {
            if (isSocketFault(err) && this.restartTimer === restart && this.state.peerConnection === pc) {
                console.warn(
                    "SFU: the ICE restart did not reach the executor; it goes out again once the socket is back:",
                    err,
                )
                this.unsentRestart = { pc, participantId, timer: restart }
                return
            }
            console.warn("SFU: ICE restart refused:", err)
        })
    }

    /**
     * The socket came back with no network change: the path ICE uses is the
     * one it had, so media that kept flowing through the fault is left
     * alone, and ICE is restarted only if it is still not connected
     * ICE_CHECK_AFTER_SOCKET_MS later. A restart already in flight is left
     * alone too: a request the dead socket kept went out again already
     * (`unsentRestart`), and a new one would only restart its gathering.
     * (A network change is a different call: the path changed, so it
     * restarts ICE at once — see the `onNetworkChange` handler in `join`.)
     */
    private checkIceSoon(): void {
        if (this.iceCheckTimer) clearTimeout(this.iceCheckTimer)
        this.iceCheckTimer = setTimeout(() => {
            this.iceCheckTimer = null
            const pc = this.state.peerConnection
            if (!pc || this.reconnecting || this.restartTimer) return
            if (pc.iceConnectionState === "connected" || pc.iceConnectionState === "completed") return
            this.restartIce(pc)
        }, SfuManager.ICE_CHECK_AFTER_SOCKET_MS)
    }

    private endRestart(): void {
        if (this.restartTimer) {
            clearTimeout(this.restartTimer)
            this.restartTimer = null
        }
        this.restartDeadline = null
        this.unsentRestart = null
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
                // goes out on still answers is the socket layer's call.
                this.restartIce(pc)
            } else if (
                pc.iceConnectionState === "connected" ||
                pc.iceConnectionState === "completed"
            ) {
                // A working path ends a restart.
                this.endRestart()
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
                if (this.iceCheckTimer) {
                    clearTimeout(this.iceCheckTimer)
                    this.iceCheckTimer = null
                }
                const current = this.state.peerConnection
                if (!current || this.reconnecting) return
                this.endRestart()
                this.restartIce(current)
            })
        }
        // The socket came back: what a fault kept from the executor goes out now.
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
        this.clearUnlistedTimer()
    }

    private clearUnlistedTimer(): void {
        if (this.unlistedTimer) {
            clearTimeout(this.unlistedTimer)
            this.unlistedTimer = null
        }
    }

    /**
     * The room's roster no longer lists this session (its participant id)
     * while `pc` is the call's: the server has dropped this session, and ICE
     * may not say so. In E6 (CI job 53158) one of six sessions never started
     * a rebuild after a 20 s outage the server had dropped it for. The check
     * is per session, not per agent: another device of this agent keeps the
     * DID listed. Once the roster has left this session out for
     * `UNLISTED_GRACE_MS`, ask the executor for this session's state
     * (`resume`): its reply decides whether the call is rebuilt; a roster
     * that lists it again in that time cancels, since one published just
     * before this join landed can arrive after it. A manager with no session
     * (between leave and join, or stopped because a later tab replaced it)
     * starts no rebuild: `stop` clears the timer and drops `pc`, so a
     * replaced session never rejoins and never takes the call back from the
     * newer tab.
     */
    private noticeRoster(pc: RTCPeerConnection, sessions: SfuRosterParticipant[], revision: number): void {
        const participantId = this.state.participantId
        if (!participantId || sessions.some((s) => s.participantId === participantId)) {
            if (participantId) this.listedAtRevision = revision
            this.unlisted = false
            this.clearUnlistedTimer()
            return
        }
        this.unlisted = true
        if (this.unlistedTimer) return
        this.unlistedTimer = setTimeout(() => {
            this.unlistedTimer = null
            if (this.state.peerConnection !== pc || this.state.participantId !== participantId || !this.unlisted) return
            this.resume(pc, true)
        }, SfuManager.UNLISTED_GRACE_MS)
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
        this.unlisted = false
        // Per-session offer state: the executor's offer counter restarts
        // with it, so this session decides on its own ids afresh, and an
        // answer kept for the old session's connection is not its to send.
        this.answeredOfferId = null
        this.appliedOfferId = null
        this.unsentAnswer = null
        this.unsentRestart = null
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

    /** Run one bounded RPC wait of the offer work; after `withinMs` whoever
     *  waits on it goes on without it. Never an SDP step: one given up on
     *  would go on changing the connection. */
    private timeBoxed(step: () => Promise<void>, withinMs = SfuManager.OFFER_STEP_TIMEOUT_MS): Promise<void> {
        let timer: ReturnType<typeof setTimeout> | undefined
        const timeout = new Promise<void>((resolve) => {
            timer = setTimeout(() => {
                console.warn(`SFU: an offer-queue step took over ${withinMs} ms; going on without it`)
                resolve()
            }, withinMs)
        })
        const run = step().catch((err) => console.warn("SFU: a server-offer step failed:", err))
        return Promise.race([run, timeout]).finally(() => clearTimeout(timer))
    }

    /** Apply the newest of `offers` (in arrival order) to `pc` and answer it, with the track mappings of all. */
    private async applyServerOffers(pc: RTCPeerConnection, offers: SfuCallRenegotiationOffer[]): Promise<void> {
        const event = offers[offers.length - 1]
        if (this.state.peerConnection !== pc || event.offerId === this.answeredOfferId) return
        // The executor sent again an offer whose answer a socket fault kept:
        // that answer is still the one, and the offer is applied already.
        const unsent = this.unsentAnswer
        if (unsent) {
            if (unsent.pc === pc && unsent.offerId === event.offerId) {
                return this.timeBoxed(() => this.sendAnswer(unsent))
            }
            // A kept answer names its offer by id; one for another offer is stale.
            this.unsentAnswer = null
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
        const answer = { pc, offerId: event.offerId, roomName: event.roomName, participantId: event.participantId, sdpAnswer }
        this.appliedOfferId = event.offerId
        await this.timeBoxed(() => this.sendAnswer(answer))
    }

    /**
     * Send an answer to the executor, under the `offerId` of the offer it
     * answers: the executor refuses one that names another offer, so a stale
     * answer cannot use up a newer same-mid offer. A refusal of `stale` or
     * `no-pending` is non-acceptance, not a failure — the offer was settled
     * or superseded, or the server's own SDP step refused it (a str0m
     * rejection, `ChangesOutOfOrder`) — and whether the connection needs
     * repair is media and session evidence's call, not a rebuild's here.
     * One of `unknown-session` rebuilds the call. A socket fault (503,
     * 408) keeps the answer for the next socket or the executor's resend of
     * the offer; anything else leaves the verdict to ICE and the roster,
     * which see whether the session is really gone.
     */
    private async sendAnswer(answer: UnsentAnswer): Promise<void> {
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
            // Accepted or refused, the answer reached the executor: nothing
            // to keep for the next socket.
            if (this.unsentAnswer === answer) this.unsentAnswer = null
            if (result.accepted) {
                this.answeredOfferId = answer.offerId
            } else if (result.reason === "unknown-session") {
                console.warn("SFU: the executor no longer holds this session; rebuilding the call")
                void this.reconnect()
            }
            // `stale` or `no-pending`: not accepted — the offer was settled or
            // superseded, or the server's own SDP step refused it.
        } catch (err) {
            if (this.state.peerConnection !== answer.pc) return
            if (isSocketFault(err)) {
                // The executor may never have seen this answer; keep it unless
                // a resend of the offer was already answered meanwhile — and
                // only while it is the newest applied offer's: an older
                // offer's call failing late must not displace the kept one.
                if (this.answeredOfferId !== answer.offerId && this.appliedOfferId === answer.offerId) {
                    console.warn("SFU: the answer did not reach the executor; sending it again once the socket is back:", err)
                    this.unsentAnswer = answer
                }
                return
            }
            console.warn("SFU: the executor refused the answer; keeping the connection:", err)
        }
    }

    /**
     * The executor socket opened again: what a socket fault kept goes out
     * now — the kept answer, which the executor refuses by its offer id if
     * it is stale, and a restart request it never saw — then
     * `sfu.callResume` returns what went by while the socket was down (an
     * offer, a roster). The kept restart is read inside the queue, so a
     * network change that opened this socket has already dropped it for
     * the immediate restart the change runs instead.
     */
    private socketBack(): void {
        const pc = this.state.peerConnection
        if (!pc || this.reconnecting) return
        this.checkIceSoon()
        // A join may still be settling — its SDP step and its stream mapping
        // are part of this call's state, so recovery waits them out on the
        // queue before it rechecks the peer.
        const joined = this.joinSettled
        this.offerQueue = this.offerQueue
            .then(() => joined)
            .then(async () => {
                if (this.state.peerConnection !== pc) return
                const unsent = this.unsentAnswer
                // The box is on the RPCs only: one given up on while its SDP
                // step still ran would free the queue to mutate `pc` beside it.
                if (unsent?.pc === pc) await this.timeBoxed(() => this.sendAnswer(unsent))
                const restart = this.unsentRestart
                if (restart?.pc === pc) {
                    this.unsentRestart = null
                    this.sendRestart(pc, restart.participantId, restart.timer)
                }
            })
            .catch((err) => console.warn("SFU: recovering after the socket's return failed:", err))
        // Off the queue: the ask waits out a join still settling, and its
        // reply applies on the queue — so the restart offer it triggers on
        // the executor is answered while the reply is still out.
        this.resume(pc)
    }

    /**
     * Ask the executor for this session's state (`sfu.callResume`). The ask
     * is not a queue step — the restart offer it triggers on the executor
     * is answered while the reply is still out, inside the restart's
     * window — but it waits out a join still settling, and its reply's
     * roster and pending offer apply on the queue like the events that
     * carry them. `listed: false` — the room or the session gone —
     * rebuilds the call, unless a roster newer than the ask listed it
     * again meanwhile. The reply's wait is bounded so a reply landing
     * after it is dropped — it must not list or unlist, roster, offer or
     * rebuild a call that already went on; while a restart is in flight
     * the bound is the restart's remaining time, never a fresh one from
     * the socket's return. Only the RPC is bounded: the SDP steps a reply
     * starts run to their end like any offer's.
     * A socket fault (503, 408) keeps the call: an ask a roster left this
     * session out for (`retryUnlisted`) is asked again within the grace
     * while the roster still omits it; the others wait for the next
     * reconnect. A 403 ends it: the executor refused this session's
     * resume — it is another agent's, or this agent lost its membership —
     * and a refusal does not heal.
     */
    private resume(pc: RTCPeerConnection, retryUnlisted = false): void {
        const participantId = this.state.participantId
        if (!participantId || this.state.peerConnection !== pc) return
        const joined = this.joinSettled
        void joined.then(async () => {
            // The join the ask waited for may not be this call's any more.
            if (this.state.peerConnection !== pc || this.state.participantId !== participantId) return
            const askedAtRevision = this.rosterRevision
            // What `askedAtRevision` is for the roster, this is for offers:
            // the reply's offer applies only while the offer last applied
            // is still the one the ask went out under — see `applyResume`.
            const askedAtOfferId = this.appliedOfferId
            // A restart in flight owns its remaining window: a socket that
            // came back late in it bounds the ask by what is left, or
            // drops it.
            const restartLeft = this.restartDeadline === null ? Infinity : this.restartDeadline - Date.now()
            if (restartLeft <= 0) return
            let reply: { result: SfuResumeResult } | { error: unknown } | null = null
            await this.timeBoxed(async () => {
                try {
                    reply = { result: await this.neighbourhood.sfuCallResume(this.neighbourhoodUrl, this.roomId, participantId) }
                } catch (error) {
                    reply = { error }
                }
            }, Math.min(SfuManager.RESUME_STEP_TIMEOUT_MS, restartLeft))
            // The box won: whatever the RPC answers now belongs to a call
            // that went on without it.
            if (reply === null) return
            this.offerQueue = this.offerQueue
                .then(() => this.applyResume(pc, participantId, askedAtRevision, askedAtOfferId, reply!, retryUnlisted))
                .catch((err) => console.warn("SFU: applying the resume's reply failed:", err))
        })
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
        retryUnlisted: boolean,
    ): Promise<void> {
        if ("error" in reply) {
            const err = reply.error
            if (this.state.peerConnection !== pc || this.state.participantId !== participantId) return
            if (isSocketFault(err)) {
                // An ask a roster left this session out for stays open until
                // one answers: the fault left the omission unresolved, so
                // while the roster still omits it the grace asks again
                // rather than waiting for a socket reconnect.
                if (retryUnlisted && this.unlisted && !this.unlistedTimer) {
                    this.unlistedTimer = setTimeout(() => {
                        this.unlistedTimer = null
                        if (this.state.peerConnection !== pc || this.state.participantId !== participantId || !this.unlisted) return
                        this.resume(pc, true)
                    }, SfuManager.UNLISTED_GRACE_MS)
                } else {
                    console.warn("SFU: resuming the session failed with the socket; the next reconnect asks again:", err)
                }
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
            this.noticeRoster(pc, result.roster.participants, result.roster.revision)
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
        // reply's offer applies — a kept answer for it replaying as ever.
        const offer = result.offer
        if (offer && offer.participantId === participantId && this.appliedOfferId === askedAtOfferId) {
            await this.applyServerOffers(pc, [offer])
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
     * rejoin — the other tab has the call now — and say so with `replaced`.
     * A session this manager got since (a reconnect's) is left.
     */
    private stopReplaced(event: SfuSessionReplaced): void {
        console.info("SFU: another tab on this device joined the call; this one stops")
        if (this.state.participantId === event.participantId) this.state.participantId = null
        this.stop()
        void this.leaveSession()
        this.resetParticipants()
        this.emit("replaced", event)
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
     * dead connection is left behind, and say why with `call-ended`. A new
     * `join` starts the call again with a fresh rebuild budget.
     */
    private endCall(event: SfuCallEnded): void {
        console.error(`SFU: the call ended (${event.reason})`)
        this.stop()
        void this.leaveSession()
        this.resetParticipants()
        this.emit("call-ended", event)
    }

    /** End this manager's call locally: no reconnect in flight continues, nothing is sent. */
    private stop(): void {
        this.generation++
        this.releaseServerEvents()
        this.replacedUnsubscribe?.()
        this.replacedUnsubscribe = null
        this.heldIds.clear()
        this.endRestart()
        this.networkUnsubscribe?.()
        this.networkUnsubscribe = null
        this.reconnectUnsubscribe?.()
        this.reconnectUnsubscribe = null
        if (this.iceCheckTimer) {
            clearTimeout(this.iceCheckTimer)
            this.iceCheckTimer = null
        }
        this.unsentAnswer = null
        this.unsentRestart = null
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
            for (const track of participant.stream.getTracks()) {
                participant.stream.removeTrack(track)
                this.emit("stream-removed", participant.stream, track)
            }
            this.refreshKinds(participant)
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
