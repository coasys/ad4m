/**
 * E26: one participant's app socket stays open but passes nothing. A
 * regression row: dev passes it. Needs no `sudo`: it covers the SDK's ping
 * deadline where E17 (every socket goes dark, the same path plus ICE) is
 * skipped for want of passwordless `tc`.
 *
 * `silence()` keeps carol's TCP connection up and holds every byte on it,
 * both ways, for good: the case a browser has no event for at all (the
 * socket reads open, nobody answers on it), and what a NAT that drops the
 * flow looks like to TCP. A new dial passes. The SDK must notice by itself
 * (its ping deadline, see `timing.ts`) and dial a new socket. See
 * `socket-fault.ts` for what the row asserts.
 */

import { REDIAL_SILENT_MS } from "../../faults/timing.js";
import { socketFaultRow } from "./socket-fault.js";

export const e26SocketSilent = socketFaultRow({
  id: "e26",
  name: "Open but silent app socket",
  description: "carol's executor WebSocket stays open but forwards nothing: she notices, redials over a new connection and repairs while media flows throughout",
  kind: "silent-socket",
  apply: (proxy) => proxy.silence(),
  doing: "goes silent",
  reconnectMs: REDIAL_SILENT_MS,
});
