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
 *    observer→remote pair must carry packets (audio or video frames in the
 *    last second) within `recoverMs`;
 *  - call rebuilds stay under a bound: `departures` counts the
 *    `participant-left` events a session fired, so its growth over the run
 *    is how many peers visibly left — a rebuild is exactly that, one per
 *    observer.
 */

import type { Call } from "../e2e/call.js";
import type { Snapshot } from "../e2e/protocol.js";

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
  maxRebuilds?: number;
}

interface Sample {
  t: number;
  snaps: Map<string, Snapshot>;
}

/** Packets flowed to `snapshot` from `did` in the last second. */
function flowing(snapshot: Snapshot | undefined, did: string): boolean {
  const remote = snapshot?.remotes[did];
  return remote !== undefined && remote.audioRecent + remote.videoRecent > 0;
}

export class RunChecks {
  private readonly sampleMs: number;
  private readonly recoverMs: number;
  private readonly maxRebuilds: number;
  private samples: Sample[] = [];
  private windows: FaultWindow[] = [];
  private departuresAtStart = new Map<string, number>();
  private joined: string[] = [];
  private timer: ReturnType<typeof setTimeout> | null = null;
  private stopped = false;

  constructor(
    private call: Call,
    opts: RunCheckOptions = {},
  ) {
    this.sampleMs = opts.sampleMs ?? 500;
    this.recoverMs = opts.recoverMs ?? 20_000;
    this.maxRebuilds = opts.maxRebuilds ?? 0;
  }

  /** Start sampling. Call once everyone is joined and media flows. */
  async start(): Promise<void> {
    this.joined = [...this.call.present];
    const snaps = await this.call.snapshots();
    for (const [name, snap] of snaps) this.departuresAtStart.set(name, snap.departures);
    this.samples.push({ t: Date.now(), snaps });
    const tick = async () => {
      if (this.stopped) return;
      try {
        this.samples.push({ t: Date.now(), snaps: await this.call.snapshots() });
      } catch {
        // a participant mid-restart may refuse a snapshot; the next tick retries
      }
      this.timer = setTimeout(tick, this.sampleMs);
    };
    this.timer = setTimeout(tick, this.sampleMs);
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
        this.endAt = Date.now();
      },
    };
    this.windows.push(window);
    return window;
  }

  /**
   * Stop sampling and return every violation found, earliest first.
   * An empty array means the call behaved.
   */
  async stop(): Promise<string[]> {
    this.stopped = true;
    if (this.timer) clearTimeout(this.timer);
    this.timer = null;
    try {
      this.samples.push({ t: Date.now(), snaps: await this.call.snapshots() });
    } catch {
      // keep the samples we have
    }
    const violations: string[] = [];
    const didOf = new Map(this.joined.map((name) => [name, this.call.who(name).did]));
    const joinedDids = new Set(didOf.values());
    const nameOf = new Map([...didOf].map(([name, did]) => [did, name]));

    // ── participant-left for a still-joined peer ──────────────────────
    const seenLeft = new Set<string>();
    for (const { t, snaps } of this.samples) {
      for (const [me, snap] of snaps) {
        for (const did of snap.left) {
          if (!joinedDids.has(did)) continue;
          const key = `${me}|${did}`;
          if (seenLeft.has(key)) continue;
          seenLeft.add(key);
          violations.push(
            `${me} saw participant-left for ${nameOf.get(did) ?? did} at t+${t - this.samples[0].t}ms, ` +
              `while ${nameOf.get(did) ?? did} never left the call`,
          );
        }
      }
    }

    // ── call rebuilds ─────────────────────────────────────────────────
    const last = this.samples[this.samples.length - 1];
    for (const [me, snap] of last.snaps) {
      const leftCount = snap.departures - (this.departuresAtStart.get(me) ?? 0);
      if (leftCount > this.maxRebuilds) {
        violations.push(
          `${me} recorded ${leftCount} participant-left event(s) over the run; ` +
            `a still-joined peer rebuilt or rejoined — bound is ${this.maxRebuilds}`,
        );
      }
    }

    // ── media recovery after each fault window ────────────────────────
    for (const w of this.windows) {
      if (w.endAt === null) continue;
      const inside = this.samples.filter((s) => s.t >= w.start && s.t <= w.endAt!);
      const endSample = this.samples.find((s) => s.t >= w.endAt!) ?? last;
      for (const me of this.joined) {
        for (const peer of this.joined) {
          if (peer === me) continue;
          const did = didOf.get(peer)!;
          if (w.uninterrupted) {
            const silent = inside.find((s) => !flowing(s.snaps.get(me), did));
            if (silent) {
              violations.push(
                `${me} had no media packets from ${peer} during ${w.kind} on ${w.users.join("+")} ` +
                  `(t+${silent.t - w.start}ms into the fault)`,
              );
            }
          }
          const recovered = this.samples.find((s) => s.t >= endSample.t && flowing(s.snaps.get(me), did));
          if (!recovered || recovered.t - w.endAt > this.recoverMs) {
            violations.push(
              `${me} had no media packets from ${peer} within ${this.recoverMs}ms ` +
                `after ${w.kind} on ${w.users.join("+")} ended` +
                (recovered ? ` (recovered at +${recovered.t - w.endAt}ms)` : " (never recovered)"),
            );
          }
        }
      }
    }
    return violations;
  }
}
