// The messages between the page (or Node host) and the Worker that runs the web executor.
// Each `cid` is one WorkerSocket, so one RPC connection in the core.
//
// host -> Worker:
//   {op: 'boot', config, logLevel}   first message: start the core
//   {op: 'connect', cid}             open connection `cid` (operator, no token)
//   {op: 'send', cid, text}          one client frame on it
//   {op: 'close', cid}               the socket closed: end the connection
//   {op: 'panic-for-test', message}  only for tests/web-executor/worker-death.test.mjs
// Worker -> host:
//   {op: 'ready'}                    the core booted
//   {op: 'frame', cid, text}         one frame the core sent on `cid` (reply, pong, event)
//   {op: 'closed', cid, code, reason} the core ended `cid`
//   {op: 'dead', reason}             the core cannot go on (a panic, a failed boot): every
//                                    socket is closed, and none opens any more

export type ToWorker =
  | { op: 'boot'; config: string; logLevel?: string }
  | { op: 'connect'; cid: number }
  | { op: 'send'; cid: number; text: string }
  | { op: 'close'; cid: number }
  | { op: 'panic-for-test'; message: string }

export type FromWorker =
  | { op: 'ready' }
  | { op: 'frame'; cid: number; text: string }
  | { op: 'closed'; cid: number; code: number; reason: string }
  | { op: 'dead'; reason: string }

/** The close code of every socket of a dead Worker (RFC 6455 "internal error"). */
export const CLOSE_DEAD = 1011

/** `reason` cut to the 123 bytes of a WebSocket close reason, on a character boundary. */
export function closeReason(reason: string): string {
  const bytes = new TextEncoder().encode(reason)
  if (bytes.length <= 123) return reason
  let end = 123
  while (end > 0 && (bytes[end] & 0xc0) === 0x80) end--
  return new TextDecoder().decode(bytes.slice(0, end))
}
