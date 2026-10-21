/** Messages between a scenario and its participant processes. */

import type { SfuQualityPreference } from "@coasys/ad4m";

export interface JoinOptions {
  port: number;
  token: string;
  neighbourhoodUrl: string;
  room: string;
  /** This participant's tone. */
  tone: number;
  /** Every participant's tone, so a receiver can name what it hears. */
  tones: number[];
  /** The camera's size, or null for no camera. */
  video: Size | null;
}

export interface Size {
  width: number;
  height: number;
}

export type Command =
  | { kind: "join"; options: JoinOptions }
  | { kind: "rejoin" }
  | { kind: "leave" }
  | { kind: "mute"; media: "audio" | "video" }
  | { kind: "unmute"; media: "audio" | "video" }
  | { kind: "shareScreen" }
  | { kind: "stopScreen" }
  | { kind: "quality"; preference: SfuQualityPreference }
  | { kind: "send"; label: string; data: string; binary: boolean }
  | { kind: "snapshot" }
  /** Destroy the session and join a new one on `topology` with the same local stream, as an app switching a live call does. */
  | { kind: "switch"; topology: "mesh" | "sfu" }
  | { kind: "close" };

/** What arrives from one remote participant. */
export interface Remote {
  audioFrames: number;
  /** Audio frames in the last second. */
  audioRecent: number;
  /** The tone this audio carries, or null for silence. */
  tone: number | null;
  videoFrames: number;
  videoRecent: number;
  width: number;
  height: number;
}

export interface Snapshot {
  joined: boolean;
  state: string;
  topology: string | null;
  /** DIDs the session's `participant-joined` / `participant-left` events say are in the call. */
  roster: string[];
  /** DIDs in `session.participants`. */
  participants: string[];
  left: string[];
  /** How many `participant-left` events the session has fired. */
  departures: number;
  /** Media per remote DID, as the SDK attributed it. */
  remotes: Record<string, Remote>;
  /**
   * Media per remote DID on the stream the session's events last handed over
   * for them, as an app's video element plays it. Unlike `remotes`, never
   * found by reading `session.participants` again: what an app shows without
   * re-rendering.
   */
  shown: Record<string, Remote>;
  data: { from: string; label: string; data: string; binary: boolean }[];
  errors: string[];
  peerStates: Record<string, string>;
  recoveries: number;
  /** The remote address (`ip:port`) of each connection's selected candidate pair: where media goes. */
  selectedRemotes: string[];
}
