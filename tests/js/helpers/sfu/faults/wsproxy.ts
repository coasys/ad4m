/**
 * A WebSocket-aware proxy between one client and the executor's API port:
 * the no-sudo way to make socket and message faults (`tc` needs root).
 *
 * Socket faults, on the TCP connections the client holds through it:
 *  - `cut()` destroys them both ways: the client's WebSocket dies while its
 *    media (UDP, never proxied) keeps flowing;
 *  - `silence()` keeps them open and forwards nothing on them, in either
 *    direction, for good: the socket a browser reads as open that nobody
 *    answers on. A new dial passes;
 *  - `blackout()` cuts, and holds every new dial (no answer, as a SYN
 *    retransmits) until `restore()`, then connects it;
 *  - `connections()` lists every connection so far with the bytes that
 *    crossed it, so a row sees the client dial a new one.
 *
 * Message faults: a byte hole corrupts the WebSocket stream's framing for
 * everything after it, so the proxy parses the framing itself and `intercept`
 * eats or reports a whole *message* while every ping, pong and other message
 * is delivered. A rule has a direction, a `match` (see `matchRpc`,
 * `matchEvent`, `replyTo`), `once` or not, and an action: `drop` eats the
 * message, `observe` lets it through and reports it. Each message a rule hit
 * carries the time it crossed.
 *
 * Scope: plaintext `ws` only (no TLS). It neither negotiates away nor
 * decodes permessage-deflate: a compressed message would not match (the
 * executor offers no extension today). A half-close is not kept: either
 * side's end destroys both.
 */

import { connect, createServer, type AddressInfo, type Server, type Socket } from "node:net";

/** The client's WebSocket points at the proxy; `up` is client→executor. */
export type WsDirection = "up" | "down";

/**
 * Whether a message matches. It is asked about every message, in both
 * directions, so a match can learn from one (`replyTo` reads the request's
 * id); a rule acts only on a message going in its own direction.
 */
export type WsMatch = (text: string, direction: WsDirection) => boolean;

/** A message a rule hit. */
export interface InterceptedMessage {
  direction: WsDirection;
  opcode: number;
  /** Decoded payload; text decoded as UTF-8. */
  text: string;
  bytes: number;
  /** RFC 6455 frames the message was made of. */
  frames: number;
  /** ms epoch when it crossed the proxy (or was eaten). */
  at: number;
  /** True when a `drop` rule ate it. */
  dropped: boolean;
}

export interface Interception {
  /** Every message the rule hit so far. */
  seen: InterceptedMessage[];
  /** Resolves with the first one. */
  first: Promise<InterceptedMessage>;
  /** Remove the rule. */
  stop(): void;
}

export interface ShaperConnection {
  conn: number;
  open: boolean;
  /** Bytes forwarded client→executor and executor→client. */
  upBytes: number;
  downBytes: number;
}

/** A JSON-RPC message of the executor's socket: a request (`type` = method, `id`), an event (`type`), or a reply (`id`, `result` or `error`). */
export interface RpcMessage {
  type?: string;
  id?: unknown;
  result?: unknown;
  error?: unknown;
  [key: string]: unknown;
}

/** The message as an object, or null when it is not a JSON object (a binary frame, say). */
export function parse(text: string): RpcMessage | null {
  try {
    const message = JSON.parse(text);
    return message !== null && typeof message === "object" ? message : null;
  } catch {
    return null;
  }
}

/** A request for `method`. */
export const matchRpc =
  (method: string): WsMatch =>
  (text) => {
    const m = parse(text);
    return m !== null && m.type === method && m.id !== undefined;
  };

/** An event named `name`. */
export const matchEvent =
  (name: string): WsMatch =>
  (text) => parse(text)?.type === name;

/** The reply to the first `method` request the client sends from now on: no `type`, the request's id. */
export const replyTo = (method: string): WsMatch => {
  const request = matchRpc(method);
  let id: unknown;
  return (text, direction) => {
    if (direction === "up") {
      if (id === undefined && request(text, direction)) id = parse(text)!.id;
      return false;
    }
    const m = parse(text);
    return id !== undefined && m !== null && m.type === undefined && m.id === id;
  };
};

const OPCODE_CLOSE = 0x8;

const isControl = (opcode: number): boolean => opcode >= OPCODE_CLOSE;

export interface Frame {
  fin: boolean;
  opcode: number;
  /** The frame's wire bytes, mask included: forwarded as they arrived. */
  raw: Buffer;
  /** Decoded (unmasked) payload. */
  payload: Buffer;
}

/** Splits a byte stream into RFC 6455 frames. */
export class FrameParser {
  private buf = Buffer.alloc(0);
  feed(chunk: Buffer): Frame[] {
    this.buf = this.buf.length === 0 ? chunk : Buffer.concat([this.buf, chunk]);
    const frames: Frame[] = [];
    for (;;) {
      const frame = this.next();
      if (!frame) break;
      frames.push(frame);
    }
    return frames;
  }
  private next(): Frame | null {
    const b = this.buf;
    if (b.length < 2) return null;
    const fin = (b[0] & 0x80) !== 0;
    const opcode = b[0] & 0x0f;
    const masked = (b[1] & 0x80) !== 0;
    let len = b[1] & 0x7f;
    let off = 2;
    if (len === 126) {
      if (b.length < off + 2) return null;
      len = b.readUInt16BE(off);
      off += 2;
    } else if (len === 127) {
      if (b.length < off + 8) return null;
      len = Number(b.readBigUInt64BE(off));
      off += 8;
    }
    const maskOff = off;
    if (masked) off += 4;
    if (b.length < off + len) return null;
    const raw = b.subarray(0, off + len);
    const payload = Buffer.from(b.subarray(off, off + len));
    if (masked) {
      for (let i = 0; i < payload.length; i++) payload[i] ^= b[maskOff + (i % 4)];
    }
    this.buf = b.subarray(off + len);
    return { fin, opcode, raw, payload };
  }
}

interface PendingMessage {
  opcode: number;
  raws: Buffer[];
  payloads: Buffer[];
}

interface Rule {
  direction: WsDirection | "both";
  match: WsMatch;
  once: boolean;
  action: "drop" | "observe";
  hit: (message: InterceptedMessage) => void;
}

interface Live {
  info: ShaperConnection;
  silent: boolean;
  sever: () => void;
}

/** The most handshake bytes buffered before the proxy gives up upgrading. */
const HANDSHAKE_LIMIT = 64 * 1024;

/** A TCP proxy to `127.0.0.1:target` that parses the WebSocket framing on it and can fault the sockets and the messages. */
export class WsShaper {
  private server: Server;
  private connSeq = 0;
  private sockets = new Set<Socket>();
  private rules: Rule[] = [];
  private conns = new Map<number, Live>();
  private blackedOut = false;
  /** Dials waiting out a blackout, with the listener that forgets one that goes away. */
  private held = new Map<Socket, () => void>();
  private closed = false;
  port = 0;
  constructor(private target: number) {
    this.server = createServer((client) => {
      if (this.closed) return void client.destroy();
      if (this.blackedOut) return this.hold(client);
      this.attach(client);
    });
  }

  async listen(): Promise<number> {
    await new Promise<void>((resolve) => this.server.listen(0, "127.0.0.1", resolve));
    this.port = (this.server.address() as AddressInfo).port;
    return this.port;
  }

  /** Every connection so far, open or closed, oldest first. */
  connections(): ShaperConnection[] {
    return [...this.conns.values()].map((live) => ({ ...live.info }));
  }

  private open(): Live[] {
    return [...this.conns.values()].filter((live) => live.info.open);
  }

  /**
   * Destroy every open connection both ways: the client's WebSocket gets a
   * close while the link itself, and the UDP media beside it, is fine. The
   * listener stays up, so the next dial lands on a working connection.
   * Returns the ids of the connections cut.
   */
  cut(): number[] {
    const live = this.open();
    for (const l of live) l.sever();
    return live.map((l) => l.info.conn);
  }

  /**
   * The connections open now stay open and forward nothing, either way, for
   * the rest of their life: no FIN, no RST, while a new dial passes. Bytes
   * are never dropped mid-message but held back whole, which would corrupt
   * the stream otherwise. Returns the ids of the connections silenced.
   */
  silence(): number[] {
    const live = this.open();
    for (const l of live) l.silent = true;
    return live.map((l) => l.info.conn);
  }

  /**
   * The link goes away: every open connection is cut, and a dial meanwhile
   * gets no answer until `restore()`. Returns the ids of the connections cut.
   */
  blackout(): number[] {
    this.blackedOut = true;
    return this.cut();
  }

  /** The link is back: held dials connect, and new ones pass. */
  restore(): void {
    this.blackedOut = false;
    for (const [client, forget] of this.held) {
      client.off("error", forget);
      client.off("close", forget);
      this.held.delete(client);
      if (client.destroyed || this.closed) continue;
      this.attach(client);
      client.resume();
    }
  }

  /**
   * Arm a rule on messages going in `direction`. `drop` eats a match: the
   * receiver never sees it, the stream stays valid. `observe` lets it through
   * and reports it. `once` ends the rule at its first hit. The rule is armed
   * when this returns, so a message sent after it is seen.
   */
  intercept(opts: { direction: WsDirection | "both"; match: WsMatch; once: boolean; action: "drop" | "observe" }): Interception {
    const seen: InterceptedMessage[] = [];
    let resolveFirst!: (message: InterceptedMessage) => void;
    const first = new Promise<InterceptedMessage>((resolve) => (resolveFirst = resolve));
    const rule: Rule = {
      ...opts,
      hit: (message) => {
        seen.push(message);
        if (seen.length === 1) resolveFirst(message);
      },
    };
    this.rules.push(rule);
    return {
      seen,
      first,
      stop: () => {
        this.rules = this.rules.filter((r) => r !== rule);
      },
    };
  }

  private hold(client: Socket): void {
    client.pause();
    this.sockets.add(client);
    const forget = () => {
      this.held.delete(client);
      this.sockets.delete(client);
      client.destroy();
    };
    this.held.set(client, forget);
    client.on("error", forget);
    client.on("close", forget);
  }

  private attach(client: Socket): void {
    const conn = ++this.connSeq;
    const executor = connect(this.target, "127.0.0.1");
    client.setNoDelay(true);
    executor.setNoDelay(true);
    this.sockets.add(client).add(executor);

    const info: ShaperConnection = { conn, open: true, upBytes: 0, downBytes: 0 };
    const live: Live = {
      info,
      silent: false,
      sever: () => {
        client.destroy();
        executor.destroy();
      },
    };
    this.conns.set(conn, live);

    const parsers = { up: new FrameParser(), down: new FrameParser() };
    const pending = { up: null as PendingMessage | null, down: null as PendingMessage | null };
    const dst = { up: executor, down: client };
    /** `handshake` until the 101 head passes, then `frames` (`raw` if it never was an upgrade). */
    let mode: "handshake" | "frames" | "raw" = "handshake";
    /** Down bytes held while the HTTP upgrade head completes. */
    let head = Buffer.alloc(0);

    const forward = (dir: WsDirection, raws: Buffer[]): void => {
      if (live.silent) return;
      for (const raw of raws) {
        if (dst[dir].destroyed) continue;
        dst[dir].write(raw);
        if (dir === "up") info.upBytes += raw.length;
        else info.downBytes += raw.length;
      }
    };

    const completeMessage = (dir: WsDirection, msg: PendingMessage): void => {
      const text = Buffer.concat(msg.payloads).toString("utf8");
      const at = Date.now();
      let dropped = false;
      // Every rule's match sees every message (a match may learn from one); a hit acts only in the rule's direction.
      for (const rule of [...this.rules]) {
        if (!safeMatch(rule.match, text, dir)) continue;
        if (rule.direction !== "both" && rule.direction !== dir) continue;
        if (rule.once) this.rules = this.rules.filter((r) => r !== rule);
        if (rule.action === "drop") dropped = true;
        rule.hit({ direction: dir, opcode: msg.opcode, text, bytes: msg.raws.reduce((n, r) => n + r.length, 0), frames: msg.raws.length, at, dropped: rule.action === "drop" });
      }
      if (!dropped) forward(dir, msg.raws);
    };

    const feedFrames = (dir: WsDirection, buf: Buffer): void => {
      for (const frame of parsers[dir].feed(buf)) {
        if (isControl(frame.opcode)) {
          forward(dir, [frame.raw]);
          continue;
        }
        const msg = pending[dir] ?? { opcode: frame.opcode, raws: [], payloads: [] };
        pending[dir] = msg;
        msg.raws.push(frame.raw);
        msg.payloads.push(frame.payload);
        if (frame.fin) {
          pending[dir] = null;
          completeMessage(dir, msg);
        }
      }
    };

    client.on("data", (buf) => {
      if (mode !== "frames") {
        // The request head (and nothing else — a client sends no frames
        // before the 101 arrives): passed raw.
        forward("up", [buf]);
        return;
      }
      feedFrames("up", buf);
    });

    executor.on("data", (buf) => {
      if (mode === "frames") {
        feedFrames("down", buf);
        return;
      }
      if (mode === "raw") {
        forward("down", [buf]);
        return;
      }
      head = head.length === 0 ? buf : Buffer.concat([head, buf]);
      const end = head.indexOf("\r\n\r\n");
      if (end === -1) {
        if (head.length > HANDSHAKE_LIMIT) {
          // Not an upgrade after all: never look at frames.
          mode = "raw";
          forward("down", [head]);
          head = Buffer.alloc(0);
        }
        return;
      }
      const is101 = head.subarray(0, end).includes(" 101");
      forward("down", [head.subarray(0, end + 4)]);
      const rest = Buffer.from(head.subarray(end + 4));
      head = Buffer.alloc(0);
      mode = is101 ? "frames" : "raw";
      if (rest.length > 0) {
        if (is101) feedFrames("down", rest);
        else forward("down", [rest]);
      }
    });

    const gone = () => {
      info.open = false;
      client.destroy();
      executor.destroy();
      this.sockets.delete(client);
      this.sockets.delete(executor);
    };
    client.on("error", gone);
    executor.on("error", gone);
    client.on("close", gone);
    executor.on("close", gone);
  }

  close(): void {
    this.closed = true;
    this.server.close();
    for (const s of this.sockets) s.destroy();
  }
}

function safeMatch(match: WsMatch, text: string, direction: WsDirection): boolean {
  try {
    return match(text, direction);
  } catch {
    return false;
  }
}
