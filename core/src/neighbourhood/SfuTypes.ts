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
    /** Stream-to-participant DID mapping, format: `"participantId:did"`. */
    streamMapping: string[]
}

/** Quality preference for selective forwarding (simulcast layer choice). */
export type SfuQualityPreference = "high" | "medium" | "low" | "auto"
