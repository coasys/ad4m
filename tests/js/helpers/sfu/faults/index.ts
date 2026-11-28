/**
 * The no-sudo call-fault layer (slice 2a of #1891):
 *
 * - `WsShaper` (`wsproxy.ts`) — one proxy in front of a participant's
 *   executor socket. Socket faults: `cut()`, `silence()`, `blackout()` and
 *   `restore()`; `connections()` shows the client redial. Message faults:
 *   `intercept` drops or observes one whole WebSocket message (a request, an
 *   event, or the reply to a request: `matchRpc`, `matchEvent`, `replyTo`)
 *   without corrupting the stream.
 * - `RunChecks` (`checks.ts`) — the per-row assertions: no `participant-left`
 *   for a peer still joined, audio and video growing again on every pair
 *   inside a bound, departures under a bound. `reconnected` — the client's
 *   app socket came back as a new connection that answers an RPC.
 * - `timing.ts` — every timing the rows use, with the SDK or executor
 *   constant it follows.
 *
 * A row points its participant at the proxy with `world.routeThrough`.
 */

export { WsShaper, FrameParser, matchRpc, matchEvent, replyTo, parse } from "./wsproxy.js";
export type { Interception, InterceptedMessage, RpcMessage, ShaperConnection, WsDirection, WsMatch } from "./wsproxy.js";
export { RunChecks, reconnected } from "./checks.js";
export type { FaultWindow } from "./checks.js";
