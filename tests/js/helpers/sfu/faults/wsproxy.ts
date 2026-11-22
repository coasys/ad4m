/**
 * A WebSocket-aware proxy between one client and the executor's API port.
 *
 * Message loss on the app's socket cannot be modelled by dropping TCP bytes:
 * a byte hole corrupts the WebSocket stream's framing for everything after
 * it. This proxy parses the framing itself, so a whole *message* can be
 * dropped while the stream stays valid — "the request never arrived" or "the
 * event never arrived", with every ping, pong and other message delivered.
 *
 * `dropOnce` arms one predicate; the first message matching it, in the armed
 * direction, is eaten and every later message passes. Used for the lost
 * `sfu.callAnswerServerOffer` request and the lost
 * `sfu-call-renegotiation-offer` event rows.
 */

import { connect, createServer, type AddressInfo, type Server, type Socket } from "node:net";

/** The client's WebSocket points at the proxy; `up` is client→executor. */
export type WsDirection = "up" | "down";

export type WsMatch = (text: string, opcode: number) => boolean;

export interface DroppedMessage {
  direction: WsDirection;
  opcode: number;
  /** Decoded payload; text decoded as UTF-8. */
  text: string;
  bytes: number;
  /** RFC 6455 frames the message was made of. */
  frames: number;
}

export interface WsProxyEvent {
  t: string;
  conn: number;
  what: "open" | "close" | "drop";
  direction?: WsDirection;
  text?: string;
  by?: "client" | "executor";
}

const OPCODE_CONTINUATION = 0x0;
const OPCODE_TEXT = 0x1;
const OPCODE_BINARY = 0x2;
const OPCODE_CLOSE = 0x8;
const OPCODE_PING = 0x9;
const OPCODE_PONG = 0xa;

const isControl = (opcode: number): boolean => opcode >= OPCODE_CLOSE;

interface Frame {
  fin: boolean;
  opcode: number;
  /** The frame's wire bytes, mask included: forwarded as they arrived. */
  raw: Buffer;
  /** Decoded (unmasked) payload. */
  payload: Buffer;
}

/** Splits a byte stream into RFC 6455 frames. */
class FrameParser {
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

interface DropRule {
  direction: WsDirection | "both";
  match: WsMatch;
  resolve: (dropped: DroppedMessage) => void;
}

/** Predicates for the messages a scenario drops. */
export const matchRpc =
  (method: string): WsMatch =>
  (text) => {
    try {
      const m = JSON.parse(text);
      return m != null && typeof m === "object" && m.type === method && m.id !== undefined;
    } catch {
      return false;
    }
  };

export const matchEvent =
  (name: string): WsMatch =>
  (text) => {
    try {
      const m = JSON.parse(text);
      return m != null && typeof m === "object" && m.type === name;
    } catch {
      return false;
    }
  };

/** The most handshake bytes buffered before the proxy gives up upgrading. */
const HANDSHAKE_LIMIT = 64 * 1024;

/** A TCP proxy to `127.0.0.1:target` that can drop whole WebSocket messages. */
export class WsShaper {
  private server: Server;
  private connSeq = 0;
  private sockets = new Set<Socket>();
  private rules: DropRule[] = [];
  port = 0;
  droppedLog: DroppedMessage[] = [];
  constructor(
    private target: number,
    private onEvent: (e: WsProxyEvent) => void = () => {},
  ) {
    this.server = createServer((client) => this.attach(client));
  }

  async listen(): Promise<number> {
    await new Promise<void>((resolve) => this.server.listen(0, "127.0.0.1", resolve));
    this.port = (this.server.address() as AddressInfo).port;
    return this.port;
  }

  /**
   * Arm a drop for the first message matching `match`, in `direction`
   * (`"up"` client→executor, `"down"` executor→client, default both).
   * `match` may be a substring, a RegExp, or `(text, opcode) => boolean`.
   * The returned promise resolves once the message was eaten; only that one
   * message is dropped.
   */
  dropOnce(opts: { direction?: WsDirection | "both"; match: string | RegExp | WsMatch }): { dropped: Promise<DroppedMessage> } {
    const match: WsMatch =
      typeof opts.match === "string"
        ? (text) => text.includes(opts.match as string)
        : opts.match instanceof RegExp
          ? (text) => (opts.match as RegExp).test(text)
          : opts.match;
    const dropped = new Promise<DroppedMessage>((resolve) => {
      this.rules.push({ direction: opts.direction ?? "both", match, resolve });
    });
    return { dropped };
  }

  private attach(client: Socket): void {
    const conn = ++this.connSeq;
    const executor = connect(this.target, "127.0.0.1");
    client.setNoDelay(true);
    executor.setNoDelay(true);
    this.sockets.add(client).add(executor);
    this.onEvent({ t: new Date().toISOString(), conn, what: "open" });

    const parsers = { up: new FrameParser(), down: new FrameParser() };
    const pending = { up: null as PendingMessage | null, down: null as PendingMessage | null };
    const dst = { up: executor, down: client };
    /** `handshake` until the 101 head passes, then `frames` (`raw` if it never was an upgrade). */
    let mode: "handshake" | "frames" | "raw" = "handshake";
    /** Down bytes held while the HTTP upgrade head completes. */
    let head = Buffer.alloc(0);

    const forward = (dir: WsDirection, raws: Buffer[]): void => {
      for (const raw of raws) if (!dst[dir].destroyed) dst[dir].write(raw);
    };

    const completeMessage = (dir: WsDirection, msg: PendingMessage): void => {
      const text = Buffer.concat(msg.payloads).toString("utf8");
      const idx = this.rules.findIndex((r) => (r.direction === "both" || r.direction === dir) && safeMatch(r.match, text, msg.opcode));
      if (idx === -1) {
        forward(dir, msg.raws);
        return;
      }
      const [rule] = this.rules.splice(idx, 1);
      const dropped: DroppedMessage = { direction: dir, opcode: msg.opcode, text, bytes: msg.raws.reduce((n, r) => n + r.length, 0), frames: msg.raws.length };
      this.droppedLog.push(dropped);
      this.onEvent({ t: new Date().toISOString(), conn, what: "drop", direction: dir, text: text.slice(0, 200) });
      rule.resolve(dropped);
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
      if (is101 && rest.length > 0) feedFrames("down", rest);
    });

    let by: "client" | "executor" | null = null;
    const drop = (who: "client" | "executor") => () => {
      if (!by) {
        by = who;
        this.onEvent({ t: new Date().toISOString(), conn, what: "close", by });
      }
      client.destroy();
      executor.destroy();
      this.sockets.delete(client);
      this.sockets.delete(executor);
    };
    client.on("error", drop("client"));
    executor.on("error", drop("executor"));
    client.on("close", drop("client"));
    executor.on("close", drop("executor"));
  }

  close(): void {
    this.server.close();
    for (const s of this.sockets) s.destroy();
  }
}

function safeMatch(match: WsMatch, text: string, opcode: number): boolean {
  try {
    return match(text, opcode);
  } catch {
    return false;
  }
}
