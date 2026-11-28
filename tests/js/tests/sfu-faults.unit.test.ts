/**
 * Unit tests of the SFU fault layer's wire parsing (helpers/sfu/faults): the
 * WebSocket frame parser, and a message made of several frames that a rule
 * eats whole. No executor. `pnpm run test-perf-unit`.
 */

import { expect } from "chai";
import { createServer, connect, type AddressInfo, type Socket } from "node:net";

import { Call, type Check } from "../helpers/sfu/e2e/call.js";
import { Hearing } from "../helpers/sfu/e2e/media.js";
import type { Snapshot } from "../helpers/sfu/e2e/protocol.js";
import { FrameParser, RunChecks, WsShaper, matchEvent, parse, replyTo, type FaultWindow } from "../helpers/sfu/faults/index.js";

/** One RFC 6455 frame. `ext` forces the length's wire form: 0 (7 bits), 2 (16 bits) or 8 (64 bits). */
function frame(payload: Buffer, { opcode = 0x1, fin = true, ext = 0, mask }: { opcode?: number; fin?: boolean; ext?: 0 | 2 | 8; mask?: Buffer } = {}): Buffer {
  const first = Buffer.from([(fin ? 0x80 : 0) | opcode]);
  const m = mask ? 0x80 : 0;
  let len: Buffer;
  if (ext === 0) len = Buffer.from([m | payload.length]);
  else if (ext === 2) {
    len = Buffer.alloc(3);
    len[0] = m | 126;
    len.writeUInt16BE(payload.length, 1);
  } else {
    len = Buffer.alloc(9);
    len[0] = m | 127;
    len.writeBigUInt64BE(BigInt(payload.length), 1);
  }
  const body = mask ? Buffer.from(payload.map((byte, i) => byte ^ mask[i % 4])) : payload;
  return Buffer.concat([first, len, ...(mask ? [mask] : []), body]);
}

const text = (s: string) => Buffer.from(s, "utf8");

describe("SFU fault layer: FrameParser", () => {
  it("reads 7-bit, 16-bit and 64-bit lengths", () => {
    const small = text("hi");
    const medium = Buffer.alloc(300, 0x61);
    const large = Buffer.alloc(70_000, 0x62);
    const frames = new FrameParser().feed(Buffer.concat([frame(small), frame(medium, { ext: 2 }), frame(large, { ext: 8 })]));
    expect(frames.map((f) => f.payload.length)).to.deep.equal([2, 300, 70_000]);
    expect(frames[1].payload.equals(medium)).to.equal(true);
    expect(frames[2].payload.equals(large)).to.equal(true);
  });

  it("reads a 64-bit length written for a short payload", () => {
    const [f] = new FrameParser().feed(frame(text("short"), { ext: 8 }));
    expect(f.payload.toString()).to.equal("short");
  });

  it("unmasks a client frame and keeps its wire bytes as they arrived", () => {
    const wire = frame(text("masked"), { mask: Buffer.from([1, 2, 3, 4]) });
    const [f] = new FrameParser().feed(wire);
    expect(f.payload.toString()).to.equal("masked");
    expect(f.raw.equals(wire)).to.equal(true);
  });

  it("assembles frames split at every byte", () => {
    const wire = Buffer.concat([frame(Buffer.alloc(300, 0x63), { ext: 2, mask: Buffer.from([9, 8, 7, 6]) }), frame(text("next"))]);
    const parser = new FrameParser();
    const frames = [];
    for (const byte of wire) frames.push(...parser.feed(Buffer.from([byte])));
    expect(frames.map((f) => f.payload.length)).to.deep.equal([300, 4]);
  });

  it("holds an incomplete frame until the rest comes", () => {
    const wire = frame(Buffer.alloc(300, 0x64), { ext: 2 });
    const parser = new FrameParser();
    expect(parser.feed(wire.subarray(0, 100))).to.have.length(0);
    expect(parser.feed(wire.subarray(100))).to.have.length(1);
  });

  it("reports fin and opcode of each fragment", () => {
    const frames = new FrameParser().feed(Buffer.concat([frame(text("a"), { fin: false }), frame(text("b"), { opcode: 0x0, fin: false }), frame(text("c"), { opcode: 0x0 })]));
    expect(frames.map((f) => [f.opcode, f.fin])).to.deep.equal([
      [0x1, false],
      [0x0, false],
      [0x0, true],
    ]);
  });
});

describe("SFU fault layer: WsShaper drops whole messages", () => {
  const HEAD = "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n\r\n";
  let cleanup: (() => void)[] = [];
  afterEach(() => {
    for (const fn of cleanup.splice(0)) fn();
  });

  /** An executor that answers any request head with a 101 and then `frames`; a client behind a proxy that collects what arrives. */
  async function setup(frames: Buffer[]) {
    const executor = createServer((socket) => {
      socket.once("data", () => socket.write(Buffer.concat([Buffer.from(HEAD), ...frames])));
    });
    await new Promise<void>((resolve) => executor.listen(0, "127.0.0.1", resolve));
    const proxy = new WsShaper((executor.address() as AddressInfo).port);
    await proxy.listen();
    cleanup.push(() => executor.close(), () => proxy.close());
    return { proxy };
  }

  /** Dial the proxy; resolves with everything received once `done(received)` holds. */
  function dial(port: number, done: (received: Buffer) => boolean): Promise<Buffer> {
    return new Promise((resolve, reject) => {
      const socket: Socket = connect(port, "127.0.0.1", () => socket.write("GET / HTTP/1.1\r\n\r\n"));
      cleanup.push(() => socket.destroy());
      let received = Buffer.alloc(0);
      socket.on("data", (chunk) => {
        received = Buffer.concat([received, chunk]);
        if (done(received)) resolve(received);
      });
      socket.on("error", reject);
    });
  }

  it("eats a message of three frames and delivers the ones around it", async () => {
    const before = frame(text(JSON.stringify({ type: "keep-before" })));
    const eaten = JSON.stringify({ type: "sfu-call-renegotiation-offer", sdp: "x".repeat(300) });
    const parts = [frame(text(eaten.slice(0, 100)), { fin: false }), frame(text(eaten.slice(100, 200)), { opcode: 0x0, fin: false }), frame(text(eaten.slice(200)), { opcode: 0x0, ext: 2 })];
    const ping = frame(Buffer.alloc(0), { opcode: 0x9 });
    const after = frame(text(JSON.stringify({ type: "keep-after" })), { ext: 2 });
    const { proxy } = await setup([before, ...parts.slice(0, 2), ping, parts[2], after]);
    const rule = proxy.intercept({ direction: "down", match: matchEvent("sfu-call-renegotiation-offer"), once: true, action: "drop" });
    const marker = after.subarray(after.length - 20);
    const received = await dial(proxy.port, (r) => r.includes(marker));
    const body = received.subarray(received.indexOf("\r\n\r\n") + 4);
    // What arrives is exactly the head's frames minus the eaten message: the stream stays valid.
    expect(body.equals(Buffer.concat([before, ping, after]))).to.equal(true);
    const hit = await rule.first;
    expect(hit.frames).to.equal(3);
    expect(hit.dropped).to.equal(true);
    expect(parse(hit.text)?.type).to.equal("sfu-call-renegotiation-offer");
    expect(hit.text).to.equal(eaten);
  });

  it("matches the reply to a request by its id, and eats only the first request's reply", async () => {
    const executor = createServer((socket) => {
      let head = true;
      const parser = new FrameParser();
      socket.on("data", (chunk) => {
        if (head) {
          head = false;
          socket.write(HEAD);
          return;
        }
        // Echo each request (a masked client frame) as an unmasked reply with its id.
        for (const f of parser.feed(chunk)) {
          const id = parse(f.payload.toString())!.id;
          socket.write(frame(text(JSON.stringify({ id, result: { accepted: true } }))));
        }
      });
    });
    await new Promise<void>((resolve) => executor.listen(0, "127.0.0.1", resolve));
    const proxy = new WsShaper((executor.address() as AddressInfo).port);
    await proxy.listen();
    cleanup.push(() => executor.close(), () => proxy.close());
    const rule = proxy.intercept({ direction: "down", match: replyTo("m"), once: true, action: "drop" });
    const request = (id: number) => frame(text(JSON.stringify({ type: "m", id })), { mask: Buffer.from([5, 6, 7, 8]) });
    const socket: Socket = connect(proxy.port, "127.0.0.1", () => socket.write("GET / HTTP/1.1\r\n\r\n"));
    cleanup.push(() => socket.destroy());
    let received = Buffer.alloc(0);
    socket.on("data", (chunk) => (received = Buffer.concat([received, chunk])));
    await new Promise<void>((resolve) => socket.once("data", () => resolve()));
    socket.write(Buffer.concat([request(1), request(2)]));
    const reply2 = frame(text(JSON.stringify({ id: 2, result: { accepted: true } })));
    await new Promise<void>((resolve) => {
      const check = () => (received.includes(reply2) ? resolve() : setTimeout(check, 10));
      check();
    });
    const reply1 = frame(text(JSON.stringify({ id: 1, result: { accepted: true } })));
    expect(received.includes(reply1)).to.equal(false);
    expect(parse((await rule.first).text)?.id).to.equal(1);
  });
});

describe("SFU fault layer: Call.eventually keeps its bound", () => {
  /** A Call whose snapshots take `ms` and always show a healthy call. No processes. */
  const slowCall = (ms: number): Call =>
    Object.assign(Object.create(Call.prototype) as Call, {
      snapshots: () => new Promise<Map<string, Snapshot>>((resolve) => setTimeout(() => resolve(new Map()), ms)),
    });
  const ok: Check = () => [];

  it("fails a success whose snapshots came back after the bound", async () => {
    const started = Date.now();
    const error = await slowCall(40).eventually(ok, 5, "late").then(() => null, (e: Error) => e);
    expect(error?.message).to.match(/^late: not true within 5 ms/);
    expect(Date.now() - started).to.be.below(40);
  });

  it("passes a success inside the bound", async () => {
    expect(await slowCall(5).eventually(ok, 1_000, "in time")).to.be.below(1_000);
  });

  it("a check still false at the bound fails with what the snapshots last showed", async () => {
    const error = await slowCall(1).eventually(() => ["u1 sees u2 at 320x240, expected 640x480"], 250, "video").then(() => null, (e: Error) => e);
    expect(error?.message).to.match(/^video: not true within 250 ms — u1 sees u2 at 320x240, expected 640x480/);
  });

  it("now looks once, with no bound", async () => {
    await slowCall(20).now(ok, "once");
    const error = await slowCall(1).now(() => ["u1 hears nothing from u2"], "once").then(() => null, (e: Error) => e);
    expect(error?.message).to.equal("once: not true — u1 hears nothing from u2");
  });
});

describe("SFU fault layer: RunChecks judges a joiner inside an uninterrupted window", () => {
  /** A snapshot of `me`: `deaf` lists the peers it gets no media from. */
  const snap = (deaf: string[] = []) => ({ left: [], departures: 0, deaf }) as unknown as Snapshot;
  const at = (t: number, call: Record<string, string[]>, from = t) => ({ from, t, snaps: new Map(Object.entries(call).map(([me, deaf]) => [me, snap(deaf)])) });

  /** A Call stand-in whose only media check is "me gets media from every other among". */
  function stubCall(present: string[]): Call {
    const hears = (among?: string[]): Check => (snaps) =>
      [...snaps].filter(([me]) => !among || among.includes(me)).flatMap(([me, s]) =>
        ((s as unknown as { deaf: string[] }).deaf).filter((o) => !among || among.includes(o)).map((o) => `${me} hears nothing from ${o}`));
    return {
      present: new Set(present),
      who: (name: string) => ({ did: `did:${name}` }),
      hearsEveryone: hears,
      seesEveryone: () => () => [],
      framesGrow: () => () => [],
    } as unknown as Call;
  }

  /** The violations of one window over `samples`, from start 1000 to end 5000, with a clean recovery after it. */
  function judge(samples: ReturnType<typeof at>[]): string[] {
    const checks = new RunChecks(stubCall(["alice", "carol", "dave"]));
    const internals = checks as unknown as { samples: unknown[]; windows: FaultWindow[]; violations(): string[] };
    const w = checks.fault("lost-answer", ["carol"], { uninterrupted: true });
    w.start = 1_000;
    w.endAt = 5_000;
    const all3 = { alice: [], carol: [], dave: [] };
    internals.samples = [at(0, { alice: [], carol: [] }), ...samples, at(6_000, all3), at(7_500, all3)];
    return internals.violations();
  }

  it("a joiner still connecting is not a violation", () => {
    expect(judge([at(1_500, { alice: [], carol: [] }), at(2_500, { alice: [], carol: [], dave: ["carol"] }), at(3_500, { alice: [], carol: [], dave: [] })])).to.deep.equal([]);
  });

  it("a stall on the joiner's pair after it carried media is a violation", () => {
    const violations = judge([
      at(1_500, { alice: [], carol: [] }),
      at(2_500, { alice: [], carol: [], dave: [] }),
      at(3_500, { alice: [], carol: ["dave"], dave: [] }),
    ]);
    expect(violations).to.have.length(1);
    expect(violations[0]).to.match(/^during lost-answer on carol \(t\+2500ms\): carol hears nothing from dave/);
  });

  it("a stall on one joiner pair counts although another pair of the joiner is still connecting", () => {
    const violations = judge([
      at(1_500, { alice: [], carol: [] }),
      at(2_500, { alice: [], carol: [], dave: ["alice"] }),
      at(3_500, { alice: [], carol: ["dave"], dave: ["alice"] }),
    ]);
    expect(violations).to.have.length(1);
    expect(violations[0]).to.match(/^during lost-answer on carol \(t\+2500ms\): carol hears nothing from dave/);
  });
});

describe("SFU fault layer: RunChecks baselines come from whole collections", () => {
  const snaps = () => new Map([["alice", { left: [], departures: 0 } as unknown as Snapshot]]);
  const sample = (from: number, t: number) => ({ from, t, snaps: snaps() });
  const stub = { present: new Set(["alice"]), who: () => ({ did: "did:alice" }), hearsEveryone: () => () => [], seesEveryone: () => () => [], framesGrow: () => () => [] } as unknown as Call;

  /** Recovery of a window that ended at 5000, over `samples`. */
  function recovered(samples: ReturnType<typeof sample>[]): unknown {
    const checks = new RunChecks(stub);
    const internals = checks as unknown as { samples: unknown[]; recovery(w: FaultWindow): { recovered: unknown } };
    const w = checks.fault("socket-cut", ["alice"]);
    w.endAt = 5_000;
    internals.samples = samples;
    return internals.recovery(w).recovered;
  }

  it("a collection that started before the window ended is no baseline", () => {
    // started at 4800, answered at 5200: part of it is from inside the window
    expect(recovered([sample(4_800, 5_200), sample(6_300, 6_400)])).to.equal(null);
    expect(recovered([sample(5_100, 5_200), sample(6_300, 6_400)])).to.not.equal(null);
  });

  it("a baseline must have come back GROWTH_MS before the collection starts", () => {
    // 5200 + 1000 = 6200 > 6150: one participant's two observations can be under GROWTH_MS apart
    expect(recovered([sample(5_100, 5_200), sample(6_150, 6_250)])).to.equal(null);
    expect(recovered([sample(5_100, 5_200), sample(6_200, 6_250)])).to.not.equal(null);
  });
});

describe("SFU fault layer: Hearing keeps a participant's frames when a track is replaced", () => {
  const track = (id: string, kind: string) => ({ id, kind });
  const heard = (agentDid: string, ...tracks: { id: string; kind: string }[]) => ({ agentDid, stream: { getTracks: () => tracks } }) as never;
  const receiver = (kind: string, frames: number) => ({ frames, recentFrames: 0, width: 0, height: 0, track: { kind }, dominantTone: () => null, stop() {} });

  it("totals never fall across a replaced track, by DID and by session", () => {
    const hearing = new Hearing();
    const receivers = (hearing as unknown as { receivers: Map<string, unknown> }).receivers;
    receivers.set("a1", receiver("audio", 40));
    receivers.set("v1", receiver("video", 25));
    const first = [heard("did:bob", track("a1", "audio"), track("v1", "video"))];
    const before = hearing.remotes(first, [])["did:bob"];
    expect([before.audioFrames, before.videoFrames]).to.deep.equal([40, 25]);

    // Both tracks are replaced; the new receivers start at zero and have counted 3 and 2 frames.
    receivers.set("a2", receiver("audio", 3));
    receivers.set("v2", receiver("video", 2));
    const second = [heard("did:bob", track("a2", "audio"), track("v2", "video"))];
    const after = hearing.remotes(second, [])["did:bob"];
    expect([after.audioFrames, after.videoFrames]).to.deep.equal([43, 27]);
    // The same frames are in the next call with another key too.
    const bySession = hearing.remotes(second, [], () => "p1").p1;
    expect([bySession.audioFrames, bySession.videoFrames]).to.deep.equal([43, 27]);
  });

  it("a participant that holds nothing now has no entry", () => {
    const hearing = new Hearing();
    (hearing as unknown as { receivers: Map<string, unknown> }).receivers.set("a1", receiver("audio", 9));
    hearing.remotes([heard("did:bob", track("a1", "audio"))], []);
    expect(hearing.remotes([], [])).to.deep.equal({});
  });
});
