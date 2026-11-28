/**
 * What a fault row asserts — plain helpers, not a monitor. Slice 2b builds
 * the shared Monitor; a row here opens `RunChecks` for one run, marks the
 * windows faults were applied in with `fault()`, and at the end gets the
 * violations back:
 *
 *  - no `participant-left` for a peer that is still joined: a socket or
 *    message fault is nobody leaving, so a still-joined peer's DID must
 *    never appear in a session's `left` set;
 *  - media grows again: after a fault's window ends, every
 *    observer→remote pair must pass `Call`'s `hearsEveryone` and
 *    `seesEveryone` and show its audio and its video frame counters grown
 *    over the last second, both measured after the window, within
 *    `recoverMs`. An `uninterrupted` window asks the same of every sample
 *    inside it;
 *  - departures stay under a bound: `departures` counts the
 *    `participant-left` events a session fired, so its growth over the run
 *    is how many peers visibly left (a rebuild fires one per roster member).
 *
 * A sample that failed is missing evidence, never a pass. `stop()` keeps
 * sampling until every ended window shows its recovery (or its bound has
 * passed), then judges. `abort()` stops at once without judging: a row calls
 * it in `finally`, so a failed step ends the sampling too.
 *
 * `reconnected` is the socket half: the client dialled a new connection
 * through the shaper and an RPC was answered over it.
 */

import type { Call } from "../e2e/call.js";
import type { Participant } from "../e2e/participant.js";
import type { Snapshot } from "../e2e/protocol.js";
import type { TcpShaper } from "./shaper.js";

export interface FaultWindow {
  /** What the fault was (`"socket-cut"`, `"silent-socket"`, …). */
  kind: string;
  /** Users the fault was applied to. */
  users: string[];
  /** ms epoch when the fault was applied. */
  start: number;
  /** ms epoch when its window ended; null until `end()` runs. */
  endAt: number | null;
  /** When true, every pair must carry media in every sample inside the window. */
  uninterrupted: boolean;
  /** Mark the fault's effect over (the cut healed, the silence elapsed). */
  end(): void;
}

export interface RunCheckOptions {
  /** Snapshot cadence. Default 500 ms. */
  sampleMs?: number;
  /** Media must flow on every pair again within this of a window's end. Default 20 s. */
  recoverMs?: number;
  /** Per observer, `participant-left` events beyond this count fail the run. Default 0. */
  maxDepartures?: number;
  /** The participants send video too, so its frames must grow as well as audio's. Default true. */
  video?: boolean;
}

interface Sample {
  t: number;
  /** null: the snapshot failed, so this moment has no evidence. */
  snaps: Map<string, Snapshot> | null;
  error?: string;
}

type GoodSample = Sample & { snaps: Map<string, Snapshot> };

/** Frame counters must grow over this span to count as media flowing now. */
const GROWTH_MS = 1_000;
/** A process that stood still this long sent and took in nothing meanwhile (as `Call`'s holds). */
const PAUSE_MS = 250;

export class RunChecks {
  private readonly sampleMs: number;
  private readonly recoverMs: number;
  private readonly maxDepartures: number;
  private readonly video: boolean;
  private samples: Sample[] = [];
  private windows: FaultWindow[] = [];
  private departuresAtStart = new Map<string, number>();
  private joined: string[] = [];
  private timer: ReturnType<typeof setTimeout> | null = null;
  private inflight: Promise<void> | null = null;
  private stopped = false;
  private result: Promise<string[]> | null = null;

  constructor(
    private call: Call,
    opts: RunCheckOptions = {},
  ) {
    this.sampleMs = opts.sampleMs ?? 500;
    this.recoverMs = opts.recoverMs ?? 20_000;
    this.maxDepartures = opts.maxDepartures ?? 0;
    this.video = opts.video ?? true;
  }

  /** Start sampling. Call once everyone is joined and media flows. */
  async start(): Promise<void> {
    this.joined = [...this.call.present];
    const snaps = await this.call.snapshots();
    for (const [name, snap] of snaps) this.departuresAtStart.set(name, snap.departures);
    this.samples.push({ t: Date.now(), snaps });
    const tick = () => {
      this.timer = null;
      if (this.stopped) return;
      this.inflight = this.sample().finally(() => {
        this.inflight = null;
        if (!this.stopped) this.timer = setTimeout(tick, this.sampleMs);
      });
    };
    this.timer = setTimeout(tick, this.sampleMs);
  }

  private async sample(): Promise<void> {
    const t = Date.now();
    try {
      const snaps = await this.call.snapshots();
      if (!this.stopped) this.samples.push({ t, snaps });
    } catch (error) {
      if (!this.stopped) this.samples.push({ t, snaps: null, error: error instanceof Error ? error.message : String(error) });
    }
  }

  /** Mark a fault being applied to `users`; `end()` it when its effect is over. */
  fault(kind: string, users: string[], opts: { uninterrupted?: boolean } = {}): FaultWindow {
    const window: FaultWindow = {
      kind,
      users,
      start: Date.now(),
      endAt: null,
      uninterrupted: opts.uninterrupted ?? false,
      end() {
        this.endAt ??= Date.now();
      },
    };
    this.windows.push(window);
    return window;
  }

  /**
   * Sample on until every ended window shows its recovery or its bound has
   * passed, then stop and return every violation found, earliest first. An
   * empty array means the call behaved. Idempotent: later calls return the
   * first call's answer.
   */
  stop(): Promise<string[]> {
    this.result ??= this.finish();
    return this.result;
  }

  /** Stop sampling now, judging nothing (a row that already failed). */
  async abort(): Promise<void> {
    this.result ??= this.halt().then(() => []);
    await this.result;
  }

  private async halt(): Promise<void> {
    this.stopped = true;
    if (this.timer) clearTimeout(this.timer);
    this.timer = null;
    await this.inflight;
  }

  private async finish(): Promise<string[]> {
    if (this.samples.length === 0) {
      await this.halt();
      return [];
    }
    const ended = this.windows.filter((w) => w.endAt !== null);
    const deadline = Math.max(0, ...ended.map((w) => w.endAt! + this.recoverMs));
    while (Date.now() < deadline && ended.some((w) => !this.recovery(w).recovered)) {
      await new Promise((r) => setTimeout(r, this.sampleMs));
    }
    await this.halt();
    const t = Date.now();
    try {
      this.samples.push({ t, snaps: await this.call.snapshots() });
    } catch (error) {
      this.samples.push({ t, snaps: null, error: error instanceof Error ? error.message : String(error) });
    }
    return this.violations();
  }

  private good(): GoodSample[] {
    return this.samples.filter((s): s is GoodSample => s.snaps !== null);
  }

  /** What is wrong with media at `s`, against the latest good sample at least GROWTH_MS older and not before `since`; null: no such sample yet. */
  private mediaProblems(good: GoodSample[], s: GoodSample, since: number): string[] | null {
    const base = [...good].reverse().find((b) => b.t <= s.t - GROWTH_MS && b.t >= since);
    if (!base) return null;
    const problems = [
      ...this.call.hearsEveryone(this.joined)(s.snaps, true),
      ...(this.video ? this.call.seesEveryone(undefined, this.joined)(s.snaps, true) : []),
    ];
    const kinds: ("audio" | "video")[] = this.video ? ["audio", "video"] : ["audio"];
    for (const me of this.joined) {
      for (const peer of this.joined) {
        if (peer === me) continue;
        if ([me, peer].some((n) => (s.snaps.get(n)?.pausedMs ?? 0) >= PAUSE_MS)) continue;
        const did = this.call.who(peer).did;
        const now = s.snaps.get(me)?.remotes[did];
        const then = base.snaps.get(me)?.remotes[did];
        for (const kind of kinds) {
          const key = kind === "audio" ? "audioFrames" : "videoFrames";
          if (!now || !then || now[key] <= then[key]) {
            problems.push(`${me}'s ${kind} frames from ${peer} did not grow over ${s.t - base.t}ms`);
          }
        }
      }
    }
    return problems;
  }

  /** The first sample after `w` ended with media fresh on every pair, measured after the end; else the last problems seen. */
  private recovery(w: FaultWindow): { recovered: GoodSample | null; problems: string[] } {
    const good = this.good();
    let problems = ["no sample after the window could judge media"];
    for (const s of good) {
      if (s.t < w.endAt!) continue;
      const found = this.mediaProblems(good, s, w.endAt!);
      if (found === null) continue;
      if (found.length === 0) return { recovered: s, problems: [] };
      problems = found;
    }
    return { recovered: null, problems };
  }

  private violations(): string[] {
    const violations: string[] = [];
    const t0 = this.samples[0].t;
    const didOf = new Map(this.joined.map((name) => [name, this.call.who(name).did]));
    const joinedDids = new Set(didOf.values());
    const nameOf = new Map([...didOf].map(([name, did]) => [did, name]));
    const good = this.good();

    // ── participant-left for a still-joined peer ──────────────────────
    const seenLeft = new Set<string>();
    for (const { t, snaps } of good) {
      for (const [me, snap] of snaps) {
        for (const did of snap.left) {
          if (!joinedDids.has(did)) continue;
          const key = `${me}|${did}`;
          if (seenLeft.has(key)) continue;
          seenLeft.add(key);
          violations.push(
            `${me} saw participant-left for ${nameOf.get(did) ?? did} at t+${t - t0}ms, ` +
              `while ${nameOf.get(did) ?? did} never left the call`,
          );
        }
      }
    }

    // ── departures ────────────────────────────────────────────────────
    const last = good[good.length - 1];
    for (const [me, snap] of last.snaps) {
      const leftCount = snap.departures - (this.departuresAtStart.get(me) ?? 0);
      if (leftCount > this.maxDepartures) {
        violations.push(`${me} recorded ${leftCount} participant-left event(s) over the run — bound is ${this.maxDepartures}`);
      }
    }

    // ── media during and after each fault window ──────────────────────
    for (const w of this.windows) {
      if (w.endAt === null) continue;
      const endAt = w.endAt;
      const what = `${w.kind} on ${w.users.join("+")}`;
      if (w.uninterrupted) {
        const missing = this.samples.find((s) => s.snaps === null && s.t >= w.start && s.t <= endAt);
        if (missing) violations.push(`no snapshot at t+${missing.t - w.start}ms into ${what} (${missing.error}): no evidence media flowed`);
        let judged = 0;
        for (const s of good.filter((s) => s.t >= w.start && s.t <= endAt)) {
          const problems = this.mediaProblems(good, s, 0);
          if (problems === null) continue;
          judged++;
          if (problems.length) {
            violations.push(`during ${what} (t+${s.t - w.start}ms): ${problems.join("; ")}`);
            break;
          }
        }
        if (judged === 0) violations.push(`no sample inside ${what} could judge media: no evidence it flowed`);
      }
      const { recovered, problems } = this.recovery(w);
      if (!recovered || recovered.t - endAt > this.recoverMs) {
        violations.push(
          `media did not flow on every pair within ${this.recoverMs}ms after ${what} ended` +
            (recovered ? ` (recovered at +${recovered.t - endAt}ms)` : `: ${problems.join("; ")}`),
        );
      }
    }
    return violations;
  }
}

/**
 * The client of `shaper` dialled a new connection after `before` (the ids
 * open when the fault hit), and an RPC `participant` made was answered over
 * it, within `boundMs`. An old connection may still be open (a silent one
 * waits out its close handshake); what counts is that the answer crossed the
 * new one. Returns the new connection's id.
 */
export async function reconnected(shaper: TcpShaper, participant: Participant, before: number[], boundMs: number): Promise<number> {
  const deadline = Date.now() + boundMs;
  const newest = Math.max(0, ...before);
  let last = "no new connection";
  while (Date.now() < deadline) {
    const fresh = shaper.connections().filter((c) => c.conn > newest && c.open);
    if (fresh.length) {
      const conn = fresh[fresh.length - 1];
      try {
        await withTimeout(participant.probe(), 5_000, "the probe RPC");
        const after = shaper.connections().find((c) => c.conn === conn.conn);
        if (after && after.open && after.downBytes > conn.downBytes && after.upBytes > conn.upBytes) return conn.conn;
        last = `the probe RPC was not answered over connection ${conn.conn}`;
      } catch (error) {
        last = error instanceof Error ? error.message : String(error);
      }
    }
    await new Promise((r) => setTimeout(r, 250));
  }
  throw new Error(`${participant.name}'s socket did not come back within ${boundMs}ms: ${last}`);
}

async function withTimeout<T>(promise: Promise<T>, ms: number, what: string): Promise<T> {
  let timer: ReturnType<typeof setTimeout>;
  const timed = new Promise<never>((_, reject) => {
    timer = setTimeout(() => reject(new Error(`${what} got no answer within ${ms}ms`)), ms);
  });
  return Promise.race([promise, timed]).finally(() => clearTimeout(timer));
}
