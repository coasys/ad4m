/**
 * The no-sudo call-fault layer (slice 2a of #1891):
 *
 * - `TcpShaper` — the app's WebSocket behind a proxy: delay, rate, `stall`,
 *   `silence()` (the open connections go mute, a new dial passes), `cut()`
 *   (socket dies while media flows), `blackout(ms)` (cut, and a dial waits
 *   until the link is back); `connections()` shows the client redial.
 * - `UdpRelay` — packet loss and delay on media.
 * - `WsShaper` — WebSocket-framing proxy: drops one whole message matching a
 *   predicate, in either direction, or the reply to one request, without
 *   corrupting the stream.
 * - `seeded` — deterministic randomness for fault plans.
 * - `blocking-link-language.js` + `publishBlockingLanguage` — a commit that
 *   sleeps on `soak://slow` links.
 * - `networkChanged` — the `online`/`navigator.connection` events a host
 *   gives the SDK on a network switch, for wrtc participants and tabs.
 * - `RunChecks` — the per-row assertions: no `participant-left` for a peer
 *   still joined, audio and video growing again on every pair inside a
 *   bound, departures under a bound. `reconnected` — the client's app socket
 *   came back as a new connection that answers an RPC.
 *
 * Rows that dev cannot pass yet are listed in `FAILS_ON_DEV` and skipped;
 * `E2E_KNOWN_FAILURES=1` (exactly `1`) runs them anyway, so a branch can show
 * they pass; one that passes stays a pass and warns to remove its entry:
 *
 *   E2E_KNOWN_FAILURES=1 E2E_ONLY=e27,e29,e30 pnpm run test-sfu-e2e
 */

export { TcpShaper, UdpRelay, seeded } from "./shaper.js";
export type { Shape, ShaperConnection, ShaperEvent } from "./shaper.js";
export { WsShaper, matchRpc, matchEvent } from "./wsproxy.js";
export type { DroppedMessage, ObservedReply, WsDirection, WsMatch, WsProxyEvent } from "./wsproxy.js";
export { networkChanged, connectionEstimateChanged } from "./netchange.js";
export { publishBlockingLanguage, SLOW_PREDICATE } from "./language.js";
export { RunChecks, reconnected } from "./checks.js";
export type { FaultWindow, RunCheckOptions } from "./checks.js";

/**
 * Rows that fail on `dev`: they gate slice 3 (the signalling/media-decision
 * rework), so they stay in the tree and are skipped with the reason here,
 * unless `E2E_KNOWN_FAILURES=1`. Remove an entry when its row passes.
 */
export const FAILS_ON_DEV: Record<string, string> = {
  e27: "a lost sfu.callAnswerServerOffer request leaves carol without the offer's media until the 30 s RPC timeout rebuilds her call",
  e29: "a lost reply to an applied sfu.callAnswerServerOffer times out after 30 s and rebuilds carol's call (participant-left for everyone)",
  e30: "dev sends no sfu.callResume, so no resume reply brings carol the roster and offer she missed while her socket was down",
};

/** The env var that runs the `FAILS_ON_DEV` rows instead of skipping them. */
export const KNOWN_FAILURES_ENV = "E2E_KNOWN_FAILURES";

/** A row's `unavailable`: the reason it is skipped on dev, or null when it runs. */
export function failsOnDev(id: string): string | null {
  const reason = FAILS_ON_DEV[id];
  if (!reason || process.env[KNOWN_FAILURES_ENV] === "1") return null;
  return `fails on dev: ${reason} (${KNOWN_FAILURES_ENV}=1 runs it)`;
}

/** A row calls this when every step passed: a listed row that passes warns that its entry can go. */
export function rowPassed(id: string): void {
  if (FAILS_ON_DEV[id]) console.warn(`[${id}] passed, but FAILS_ON_DEV lists it ("${FAILS_ON_DEV[id]}"): remove its entry`);
}
