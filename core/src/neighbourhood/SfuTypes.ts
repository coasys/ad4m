/**
 * Public types exposed by the executor SFU (Selective Forwarding Unit)
 * service.  The wire types are generated from `rust-executor/src/sfu/types.rs`
 * and `rust-executor/src/api/sfu_ws.rs` (`pnpm run generate:api-types`);
 * this module re-exports them under the SDK's public names and adds the
 * browser-side types.
 */

import type { SfuConfig } from "../generated/api/SfuConfig"
import type { SfuQualityPreferenceParams } from "../generated/api/SfuQualityPreferenceParams"

export type { CallSessionInfo } from "../generated/api/CallSessionInfo"
export type { IceServer } from "../generated/api/IceServer"
export type { SfuCallRenegotiationOffer } from "../generated/api/SfuCallRenegotiationOffer"
export type { SfuCascadePipe } from "../generated/api/SfuCascadePipe"
export type { SfuCascadeStatus } from "../generated/api/SfuCascadeStatus"
export type { SfuConfig } from "../generated/api/SfuConfig"
export type { SfuDataMessage } from "../generated/api/SfuDataMessage"
export type { SfuMigrateEvent } from "../generated/api/SfuMigrateEvent"
export type { SfuParticipantQualityPreference } from "../generated/api/SfuParticipantQualityPreference"
export type { SfuRoomInfo } from "../generated/api/SfuRoomInfo"
export type { SfuRoomParticipantInfo } from "../generated/api/SfuRoomParticipantInfo"
export type { SfuStatus } from "../generated/api/SfuStatus"
export type { TrackMapEntry } from "../generated/api/TrackMapEntry"

/** Topology selection — `"mesh"` is the no-SFU full-mesh fallback. */
export type SfuMode = SfuConfig["mode"]

/** Quality preference for selective forwarding (simulcast layer choice). */
export type SfuQualityPreference = SfuQualityPreferenceParams["preference"]

/** Browser-side participant — carries a live MediaStream. */
export interface SfuParticipantInfo {
    agentDid: string
    stream: MediaStream
    hasAudio: boolean
    hasVideo: boolean
    isActiveSpeaker: boolean
}
