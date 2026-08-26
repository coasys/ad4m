/**
 * Public types exposed by the executor SFU (Selective Forwarding Unit)
 * service.  Mirrors `rust-executor/src/sfu/types.rs` field-for-field;
 * keep the two in sync.
 */

/** Topology selection — `"mesh"` is the no-SFU full-mesh fallback. */
export type SfuMode = "mesh" | "designated" | "gateway" | "cascaded"

/**
 * Per-neighbourhood SFU configuration.  Stored on the Social DNA so it
 * travels with the perspective.
 */
export interface IceServer {
    /** STUN or TURN URLs. */
    urls: string[]
    /** TURN username (long-term credential). */
    username?: string
    /** TURN credential / password. */
    credential?: string
}

/**
 * Server-pushed SDP offer.  When a new peer joins a room the SFU
 * regenerates an offer per existing participant with the new outbound
 * tracks added.  Clients listen for the `sfu-call-renegotiation-offer`
 * event over the events WS, apply the SDP as a remote description,
 * generate an answer, and post the answer through
 * `sfu.callAnswerServerOffer`.
 */
/** Maps one outbound SDP m-line to the originating participant's DID. */
export interface TrackMapEntry {
    /** SDP mid (media-line identifier) in the offer. */
    mid: string
    /** DID of the participant whose media this m-line carries. */
    agentDid: string
    /** `"audio"` or `"video"`. */
    mediaKind: string
}

export interface SfuCallRenegotiationOffer {
    /** DID of the participant this offer is addressed to. */
    targetDid: string
    neighbourhoodUrl: string
    roomName: string
    /** JSON-encoded `RTCSessionDescriptionInit`. */
    sdpOffer: string
    /**
     * Mid-to-DID attribution for newly added outbound m-lines.
     * Use to correlate incoming tracks to participant DIDs
     * (arrival order alone cannot guarantee correct attribution).
     */
    trackMapping?: TrackMapEntry[]
}

export interface SfuConfig {
    /** `"mesh"` | `"designated"` | `"gateway"` | `"cascaded"` */
    mode: SfuMode
    /** DID of the designated SFU peer (only used when mode = "designated"). */
    designatedPeer?: string
    /** Fallback mode when SFU is unavailable. */
    fallback: SfuMode
    /** Maximum participants before mesh is degraded. */
    maxMeshParticipants: number
    /** DIDs of SFU peers in cascaded mode. */
    sfuPeers: string[]
    /** Max participants per SFU node in cascaded mode. */
    maxParticipantsPerNode?: number
    /**
     * DID of the preferred SFU node in cascaded mode.  The cascade
     * manager routes joins here first; overflow spills to other nodes
     * only when the preferred one reaches capacity.
     */
    preferredSfuDid?: string
    /**
     * ICE servers the SFU advertises to clients.  When set, clients
     * MUST use these instead of their hardcoded defaults so the host
     * application can rotate TURN credentials without a client
     * redeploy.  An empty array means "use the SDK defaults".
     */
    iceServers?: IceServer[]
}

/** Snapshot of an active SFU room. */
export interface SfuRoomInfo {
    neighbourhoodUrl: string
    roomName: string
    participantCount: number
    participants: SfuParticipantInfo[]
    createdAtMs: number
}

export interface SfuParticipantInfo {
    agentDid: string
    hasAudio: boolean
    hasVideo: boolean
    isActiveSpeaker: boolean
}

/**
 * Result of `callJoin` — SDP answer + optional cascade redirect +
 * stream mapping for the joining peer.
 */
export interface CallSessionInfo {
    roomName: string
    neighbourhoodUrl: string
    participantId: string
    sdpAnswer: string
    /**
     * When set, the joining peer should reconnect to this DID's SFU
     * node (cascaded mode load redirect).
     */
    redirectTo?: string
    /**
     * Roster of existing participants at join time, format:
     * `"participantId:did"` (local) or `"remote-did:did"` (cascade).
     * For track-to-DID attribution, use `trackMapping` on each
     * renegotiation offer — the SDP mids get assigned there.
     */
    streamMapping: string[]
}

/** Quality preference for selective forwarding (simulcast layer choice). */
export type SfuQualityPreference = "high" | "medium" | "low" | "auto"

/** Data channel message relayed through the SFU. */
export interface SfuDataMessage {
    /** DID of the participant who sent the data. */
    senderDid: string
    neighbourhoodUrl: string
    roomName: string
    /** Label of the data channel. */
    channelLabel: string
    /** True when the payload is binary (base64-encoded in `data`). */
    binary: boolean
    /** The payload — UTF-8 text or base64-encoded binary. */
    data: string
}
