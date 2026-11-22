/**
 * The no-sudo call-fault layer (slice 2a of #1891):
 *
 * - `TcpShaper` — the app's WebSocket behind a proxy: delay, rate, `stall`,
 *   `silence(ms)` (open but mute), `cut()` (socket dies while media flows).
 * - `UdpRelay` — packet loss and delay on media.
 * - `WsShaper` — WebSocket-framing proxy: drops one whole message matching a
 *   predicate, in either direction, without corrupting the stream.
 * - `seeded` — deterministic randomness for fault plans.
 * - `blocking-link-language.js` + `publishBlockingLanguage` — a commit that
 *   sleeps on `soak://slow` links.
 * - `networkChanged` — the `online`/`navigator.connection` events a host
 *   gives the SDK on a network switch, for wrtc participants and tabs.
 * - `RunChecks` — the per-row assertions: no `participant-left` for a peer
 *   still joined, media flowing again on every pair inside a bound, call
 *   rebuilds under a bound.
 */

export { TcpShaper, UdpRelay, seeded } from "./shaper.js";
export type { Shape, ShaperEvent } from "./shaper.js";
export { WsShaper, matchRpc, matchEvent } from "./wsproxy.js";
export type { DroppedMessage, WsDirection, WsMatch, WsProxyEvent } from "./wsproxy.js";
export { networkChanged, connectionEstimateChanged } from "./netchange.js";
export { publishBlockingLanguage, SLOW_PREDICATE } from "./language.js";
export { RunChecks } from "./checks.js";
export type { FaultWindow, RunCheckOptions } from "./checks.js";

/**
 * Rows that fail on `dev`: they gate slice 3 (the signalling/media-decision
 * rework), so they stay in the tree and are skipped with the reason here.
 * Remove an entry when its row passes.
 */
export const FAILS_ON_DEV: Record<string, string> = {
  // A dropped sfu.callAnswerServerOffer makes the session rebuild the whole
  // call (every observer sees participant-left for carol): on dev the answer
  // is a fatal RPC, not a message that can be lost and retried.
  e26: "a lost sfu.callAnswerServerOffer rebuilds carol's call on dev (participant-left fires); slice 3 makes the answer retryable",
};
