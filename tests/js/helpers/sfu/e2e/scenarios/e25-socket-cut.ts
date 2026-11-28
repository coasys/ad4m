/**
 * E25: one participant's app socket is cut while the network, and so its
 * media, stays up. A regression row: dev passes it. Needs no `sudo`.
 *
 * `cut()` closes carol's socket both ways, as the kernel does when a route
 * or a NAT entry dies: the socket reports closed, nothing on the link is
 * wrong. The SDK must dial a new socket over the same link. See
 * `socket-fault.ts` for what the row asserts.
 */

import { REDIAL_MS } from "../../faults/timing.js";
import { socketFaultRow } from "./socket-fault.js";

export const e25SocketCut = socketFaultRow({
  id: "e25",
  name: "App socket cut while media stays up",
  description: "carol's executor WebSocket is cut mid-call with the link itself fine: she redials over a new connection, nobody leaves, media keeps flowing",
  kind: "socket-cut",
  apply: (proxy) => proxy.cut(),
  doing: "is cut",
  reconnectMs: REDIAL_MS,
});
