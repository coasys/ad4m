/**
 * Network trouble for one client without root: `tc netem` needs CAP_NET_ADMIN
 * (or a user namespace), which the test host does not give. In-process
 * stand-ins, ported from the rehearsal soak (`rehearsal/calls-1120`,
 * helpers/sfu/appload/shaper.ts):
 *
 *  - `TcpShaper`: a TCP proxy between one client and the executor's API port.
 *    Everything the client's Ad4mClient sends and receives (its one WebSocket)
 *    crosses it, with a fixed delay, a byte rate per direction, stalls,
 *    `cut()` and `silence()`. A stall or silence holds every byte for a while
 *    and then lets them go in order: what packet loss does to a TCP stream
 *    (a retransmission timeout), since a proxy above TCP cannot lose bytes.
 *    `cut()` destroys the proxied sockets both ways: the client's WebSocket
 *    dies while its media — UDP, not proxied — keeps flowing.
 *  - `UdpRelay`: a UDP relay between one client and the remote ends it calls,
 *    with loss and delay per packet: the client's media path.
 *
 * Both take their randomness from the caller, so a seed gives the same
 * trouble again.
 */

import { createSocket, type Socket as UdpSocket } from "node:dgram";
import { connect, createServer, type AddressInfo, type Server, type Socket } from "node:net";

export interface Shape {
  /** One-way delay in ms. */
  delayMs?: number;
  /** Bytes per second from the client to the executor; 0 or absent: no limit. */
  upBytesPerS?: number;
  /** Bytes per second from the executor to the client; 0 or absent: no limit. */
  downBytesPerS?: number;
}

/** Bytes a direction may hold before its source is paused, as a path's buffers fill. */
const HIGH_WATER = 256 * 1024;
const LOW_WATER = 64 * 1024;
/** The largest piece sent at once, so a rate limit paces evenly. */
const SLICE = 4 * 1024;

export interface ShaperEvent {
  t: string;
  conn: number;
  what: "open" | "close" | "cut";
  /** Who ended the connection first. */
  by?: "client" | "executor" | "shaper";
  upBytes?: number;
  downBytes?: number;
  seconds?: number;
}

class Direction {
  private queue: { buf: Buffer; at: number }[] = [];
  private queued = 0;
  private nextFree = 0;
  private timer: ReturnType<typeof setTimeout> | null = null;
  private ended = false;
  bytes = 0;
  constructor(
    private src: Socket,
    private dst: Socket,
    private shape: () => { delayMs: number; rate: number; stalledUntil: number },
  ) {
    src.on("data", (buf: Buffer) => {
      const at = Date.now() + this.shape().delayMs;
      for (let o = 0; o < buf.length; o += SLICE) this.queue.push({ buf: buf.subarray(o, Math.min(buf.length, o + SLICE)), at });
      this.queued += buf.length;
      if (this.queued > HIGH_WATER) src.pause();
      this.pump();
    });
    src.on("end", () => {
      this.ended = true;
      this.pump();
    });
  }
  private pump(): void {
    if (this.timer) return;
    for (;;) {
      const head = this.queue[0];
      if (!head) {
        if (this.ended && !this.dst.destroyed) this.dst.end();
        return;
      }
      const { rate, stalledUntil } = this.shape();
      const now = Date.now();
      const due = Math.max(head.at, this.nextFree, stalledUntil);
      if (due > now) {
        this.timer = setTimeout(() => {
          this.timer = null;
          this.pump();
        }, due - now);
        return;
      }
      this.queue.shift();
      this.queued -= head.buf.length;
      this.bytes += head.buf.length;
      if (!this.dst.destroyed) this.dst.write(head.buf);
      if (rate > 0) this.nextFree = Math.max(now, this.nextFree) + (head.buf.length / rate) * 1000;
      if (this.queued < LOW_WATER && this.src.isPaused()) this.src.resume();
    }
  }
  stop(): void {
    if (this.timer) clearTimeout(this.timer);
    this.timer = null;
    this.queue = [];
  }
}

/** A TCP proxy to `127.0.0.1:target` that shapes what crosses it. */
export class TcpShaper {
  private server: Server;
  private shapeNow: Shape = {};
  private heldUntil = 0;
  private sockets = new Set<Socket>();
  private connSeq = 0;
  private live = new Map<number, { client: Socket; executor: Socket; sever: () => void }>();
  port = 0;
  stalls = 0;
  silences = 0;
  cuts = 0;
  constructor(
    private target: number,
    private onEvent: (e: ShaperEvent) => void = () => {},
  ) {
    this.server = createServer((client) => {
      const conn = ++this.connSeq;
      const started = Date.now();
      const executor = connect(target, "127.0.0.1");
      client.setNoDelay(true);
      executor.setNoDelay(true);
      this.sockets.add(client).add(executor);
      this.onEvent({ t: new Date().toISOString(), conn, what: "open" });
      const hold = () => ({ delayMs: this.shapeNow.delayMs ?? 0, stalledUntil: this.heldUntil });
      const up = new Direction(client, executor, () => ({ ...hold(), rate: this.shapeNow.upBytesPerS ?? 0 }));
      const down = new Direction(executor, client, () => ({ ...hold(), rate: this.shapeNow.downBytesPerS ?? 0 }));
      let by: "client" | "executor" | "shaper" | null = null;
      const closed = (who: "client" | "executor" | "shaper") => () => {
        if (!by) {
          by = who;
          this.onEvent({ t: new Date().toISOString(), conn, what: "close", by, upBytes: up.bytes, downBytes: down.bytes, seconds: Math.round((Date.now() - started) / 1000) });
        }
      };
      client.on("end", closed("client"));
      executor.on("end", closed("executor"));
      const drop = (who: "client" | "executor" | "shaper") => () => {
        closed(who)();
        up.stop();
        down.stop();
        client.destroy();
        executor.destroy();
        this.sockets.delete(client);
        this.sockets.delete(executor);
        this.live.delete(conn);
      };
      client.on("error", drop("client"));
      executor.on("error", drop("executor"));
      client.on("close", drop("client"));
      executor.on("close", drop("executor"));
      this.live.set(conn, {
        client,
        executor,
        sever: () => {
          closed("shaper")();
          up.stop();
          down.stop();
          client.destroy();
          executor.destroy();
        },
      });
    });
  }
  async listen(): Promise<number> {
    await new Promise<void>((resolve) => this.server.listen(0, "127.0.0.1", resolve));
    this.port = (this.server.address() as AddressInfo).port;
    return this.port;
  }
  set(shape: Shape): void {
    this.shapeNow = shape;
  }
  /** Hold every byte, both ways, for `ms` from now. */
  stall(ms: number): void {
    this.stalls++;
    this.heldUntil = Math.max(this.heldUntil, Date.now() + ms);
  }
  /**
   * The link stays open but nothing crosses it for `ms`: the silent-socket
   * case (the OS keeps the socket; no FIN, no RST). Bytes are held and then
   * delivered — never dropped, which would corrupt the WebSocket stream — so
   * the socket's close handshake still completes after the interval.
   */
  silence(ms: number): void {
    this.silences++;
    this.heldUntil = Math.max(this.heldUntil, Date.now() + ms);
  }
  /**
   * Cut every live connection's TCP path both ways: the client's WebSocket
   * gets a close while the link itself — and the UDP media beside it — is
   * fine. The listener stays up, so the next dial lands on a working
   * connection. Returns the number of connections cut.
   */
  cut(): number {
    const entries = [...this.live.values()];
    this.cuts += entries.length;
    for (const { sever } of entries) sever();
    return entries.length;
  }
  close(): void {
    this.server.close();
    for (const s of this.sockets) s.destroy();
  }
}

/**
 * A UDP relay between one client and the remote ends it calls, with loss and
 * delay. One front socket per remote address, on the remote's own IP; the
 * client is told that socket's port in place of the remote's.
 */
export class UdpRelay {
  loss = 0;
  delayMs = 0;
  passed = 0;
  dropped = 0;
  private fronts = new Map<string, Promise<number>>();
  private sockets: UdpSocket[] = [];
  constructor(private random: () => number) {}
  portFor(target: string): Promise<number> {
    let port = this.fronts.get(target);
    if (!port) {
      port = this.open(target);
      this.fronts.set(target, port);
    }
    return port;
  }
  set(c: { loss?: number; delayMs?: number }): void {
    this.loss = c.loss ?? 0;
    this.delayMs = c.delayMs ?? 0;
  }
  private pass(send: () => void): void {
    if (this.random() * 100 < this.loss) {
      this.dropped++;
      return;
    }
    this.passed++;
    if (this.delayMs > 0) setTimeout(send, this.delayMs + this.random() * this.delayMs * 0.2);
    else send();
  }
  private async open(target: string): Promise<number> {
    const at = target.lastIndexOf(":");
    const host = target.slice(0, at);
    const port = Number(target.slice(at + 1));
    const front = createSocket("udp4");
    const back = createSocket("udp4");
    this.sockets.push(front, back);
    let client: { address: string; port: number } | null = null;
    back.on("message", (msg) => {
      const to = client;
      if (to) this.pass(() => front.send(msg, to.port, to.address));
    });
    await new Promise<void>((resolve) => back.bind(0, resolve));
    front.on("message", (msg, from) => {
      client = { address: from.address, port: from.port };
      this.pass(() => back.send(msg, port, host));
    });
    await new Promise<void>((resolve) => front.bind(0, host, resolve));
    return front.address().port;
  }
  close(): void {
    for (const s of this.sockets) {
      try {
        s.close();
      } catch {
        // already closed
      }
    }
  }
}

/** A small seeded generator (mulberry32): the same seed gives the same run plan. */
export function seeded(seed: number): () => number {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
