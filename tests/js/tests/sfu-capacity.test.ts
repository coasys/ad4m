import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import type { Server } from "node:http";
import path from "node:path";
import {
  joinCall, launch, openTab, receiveProblems, report, servePage,
  type BrowserUser, type Launched,
} from "../helpers/sfu/browser/browsers.js";
import type { PageReport } from "../helpers/sfu/browser/page.js";
import { startWorld, type World } from "../helpers/sfu/e2e/world.js";

/**
 * SFU capacity: what the executor spends on calls of growing size, in real
 * Chromium tabs sending a fake camera and microphone through the SFU that runs
 * inside the executor.
 *
 * Per call size, over a steady window after every tab receives every other:
 *
 * - the executor's CPU (its process's CPU time over the window, in percent of
 *   one core);
 * - the UDP datagrams the SFU sends per second (RTP, RTCP and ICE), and the
 *   passes its media event loop makes per second (both read from `sfu.status`,
 *   twice, the time apart);
 * - the executor's CPU per thousand datagrams sent, which stays put when the
 *   host is faster or slower in the same way for the datagrams and the CPU.
 *
 * Every tab must stay on the SFU (`report.topology`) before and after the window:
 * the space falls back to mesh when the SFU fails, and that is not what is measured.
 *
 * The numbers go to `$PERF_RESULTS_DIR/sfu-capacity.json` for the nightly
 * history (`scripts/perf/AGENTS.md`), one series per call size. Without
 * `PERF_RESULTS_DIR` they are printed only. Run after
 * `pnpm run prepare-test-local`:
 *
 *   pnpm run test-main-sfu-capacity
 *
 * - `SFU_CAP_SIZES` (`3,8,16`): the call sizes, in participants.
 * - `SFU_CAP_STEADY_S` (30): the measured window.
 * - `SFU_CAP_VIDEO` (`320x240`): the fake camera's size.
 *
 * Every participant is a tab of its own browser context, at most
 * `TABS_PER_BROWSER` to one Chromium process: one headless Chromium starts at
 * most 16 fake microphones.
 */

const SIZES = (process.env.SFU_CAP_SIZES || "3,8,16").split(",").map((s) => Number(s.trim()));
const STEADY_MS = Number(process.env.SFU_CAP_STEADY_S || 30) * 1000;
const [VIDEO_W, VIDEO_H] = (process.env.SFU_CAP_VIDEO || "320x240").split("x").map(Number);
const CONVERGE_TIMEOUT_MS = 240_000;
/** One tab's join (getUserMedia, offer, answer). */
const JOIN_TIMEOUT_MS = 60_000;
const POLL_MS = 100;
/** On top of a test's own phases: the browser start, the tabs' setup and their teardown. */
const TEST_SLACK_MS = 300_000;
const TABS_PER_BROWSER = 8;
const MAX_USERS = Math.max(...SIZES);
/** Per call size, and for the hooks: every join of the largest call, the convergence, the window and the slack. */
const TEST_TIMEOUT_MS = MAX_USERS * JOIN_TIMEOUT_MS + CONVERGE_TIMEOUT_MS + STEADY_MS + TEST_SLACK_MS;
const CLK_TCK = Number(execFileSync("getconf", ["CLK_TCK"]).toString().trim());

/** User time plus system time of a process, in clock ticks, from `/proc/<pid>/stat`. */
function cpuTicks(pid: number): number {
  const text = readFileSync(`/proc/${pid}/stat`, "utf8");
  // Fields after the command name, which may hold spaces and brackets: utime is field 14, stime 15.
  const fields = text.slice(text.lastIndexOf(")") + 2).split(" ");
  return Number(fields[11]) + Number(fields[12]);
}

interface Measurement {
  participants: number;
  windowS: number;
  cpuPercent: number;
  datagramsPerS: number;
  loopIterationsPerS: number;
  cpuMsPerKDatagram: number;
}

interface PerfMetric {
  metric: string; value: number; unit: string; better: "lower" | "higher"; params: { participants: number };
}

/** What the nightly history records for one measurement. */
function perfMetrics(m: Measurement): PerfMetric[] {
  const params = { participants: m.participants };
  return [
    { metric: "sfu_capacity.cpu_percent", value: m.cpuPercent, unit: "percent", better: "lower", params },
    { metric: "sfu_capacity.datagrams_per_s", value: m.datagramsPerS, unit: "datagrams/s", better: "higher", params },
    { metric: "sfu_capacity.loop_iterations_per_s", value: m.loopIterationsPerS, unit: "iterations/s", better: "lower", params },
    { metric: "sfu_capacity.cpu_ms_per_kdatagram", value: m.cpuMsPerKDatagram, unit: "ms", better: "lower", params },
  ];
}

/** `work`, or an error naming `what` when it takes longer than `ms`: a hang reaches the test's cleanup. */
async function within<T>(work: Promise<T>, ms: number, what: string): Promise<T> {
  let timer: ReturnType<typeof setTimeout> | undefined;
  const late = new Promise<never>((_, reject) => {
    timer = setTimeout(() => reject(new Error(`${what} took longer than ${ms / 1000} s`)), ms);
  });
  try {
    return await Promise.race([work, late]);
  } finally {
    clearTimeout(timer);
  }
}

/** The tabs whose call is not on the SFU (the space falls back to mesh when the SFU fails). */
const offSfu = (reps: Record<string, PageReport>) =>
  Object.entries(reps).filter(([, r]) => r.topology !== "sfu").map(([name, r]) => `${name} is on ${r.topology}`);

describe("SFU capacity: the executor's cost of calls of growing size", function () {
  this.timeout(TEST_TIMEOUT_MS);

  let world: World | null = null;
  let server: Server | null = null;
  let pageUrl = "";
  let pid = 0;
  const browsers: Launched[] = [];
  const measurements: Measurement[] = [];
  const userNames = Array.from({ length: MAX_USERS }, (_, i) => `u${i + 1}`);

  before(async function () {
    ({ url: pageUrl, server } = await servePage());
    world = await startWorld({
      id: "capacity",
      users: userNames.map((name) => ({ name })),
      // A room holds maxMeshParticipants x 4: 24 here.
      neighbourhoods: [{ name: "sfu", owner: "u1", members: userNames, call: { mode: "designated", maxMeshParticipants: 6 } }],
    });
    pid = world.executorPid();
  });

  after(async function () {
    this.timeout(120_000);
    for (const { browser } of browsers) await browser?.close().catch(() => {});
    server?.close();
    await world?.stop().catch(() => {});
    const results = process.env.PERF_RESULTS_DIR;
    if (results && measurements.length > 0) {
      mkdirSync(results, { recursive: true });
      writeFileSync(path.join(results, "sfu-capacity.json"), JSON.stringify({ metrics: measurements.flatMap(perfMetrics) }, null, 1));
    }
  });

  /** The `index`th Chromium process, started on first use. */
  async function browserFor(index: number): Promise<Launched> {
    while (browsers.length <= index) {
      const launched = await launch("chromium");
      if (!launched.browser) throw new Error(launched.reason);
      browsers.push(launched);
    }
    return browsers[index];
  }

  const reports = async (users: BrowserUser[]) =>
    Object.fromEntries(await Promise.all(users.map(async (u) => [u.name, await report(u)] as const))) as Record<string, PageReport>;

  /** What each tab misses from the others in its call. */
  const problemsOf = (users: BrowserUser[], before: Record<string, PageReport>, now: Record<string, PageReport>) =>
    users.flatMap((u) => users.filter((o) => o !== u).flatMap((o) =>
      receiveProblems(before[u.name], now[u.name], o.did).map((p) => `${u.name} ${p} [${o.name}]`)));

  for (const size of SIZES) {
    it(`${size} participants, camera ${VIDEO_W}x${VIDEO_H}`, async function () {
      const users: BrowserUser[] = [];
      try {
        for (let i = 0; i < size; i++) {
          const { browser } = await browserFor(Math.floor(i / TABS_PER_BROWSER));
          const w = world!.users[userNames[i]];
          users.push(await openTab(browser!, "chromium", w.name, w.did, pageUrl));
        }
        // Everyone joins one right after the other, as people do at a meeting's start.
        const room = `capacity-${size}-${Date.now()}`;
        for (const user of users) {
          const w = world!.users[user.name];
          await within(joinCall(user, {
            executorUrl: `http://127.0.0.1:${w.port}`, token: w.token, neighbourhoodUrl: world!.url("sfu"),
            room, name: user.name, video: { width: VIDEO_W, height: VIDEO_H },
          }), JOIN_TIMEOUT_MS, `${user.name}'s join`);
        }

        // Until every tab receives every other, from fresh packets and frames.
        const deadline = Date.now() + CONVERGE_TIMEOUT_MS;
        // Counted from one baseline, so a poll as short as POLL_MS sees growth.
        const baseline = await reports(users);
        for (;;) {
          await new Promise((r) => setTimeout(r, POLL_MS));
          const now = await reports(users);
          const problems = problemsOf(users, baseline, now);
          if (problems.length === 0) break;
          if (Date.now() > deadline) {
            throw new Error(`not every tab received every other within ${CONVERGE_TIMEOUT_MS / 1000} s:\n  ${problems.slice(0, 10).join("\n  ")}`);
          }
        }

        // The window. The SFU's counters and the executor's CPU are read together, at both ends.
        const sfu = world!.users.u1.client.neighbourhood;
        const reportsStart = await reports(users);
        const offAtStart = offSfu(reportsStart);
        if (offAtStart.length > 0) throw new Error(`not on the SFU before the window:\n  ${offAtStart.slice(0, 10).join("\n  ")}`);
        const countersStart = await sfu.sfuStatus();
        const ticksStart = cpuTicks(pid);
        const t0 = performance.now();
        await new Promise((r) => setTimeout(r, STEADY_MS));
        const countersEnd = await sfu.sfuStatus();
        const ticksEnd = cpuTicks(pid);
        const windowS = (performance.now() - t0) / 1000;
        const reportsEnd = await reports(users);

        // A window in which a tab left the SFU or lost another is not a measurement of a call of this size.
        const offAtEnd = offSfu(reportsEnd);
        if (offAtEnd.length > 0) throw new Error(`not on the SFU after the window:\n  ${offAtEnd.slice(0, 10).join("\n  ")}`);
        const lost = problemsOf(users, reportsStart, reportsEnd);
        if (lost.length > 0) throw new Error(`a tab stopped receiving another in the window:\n  ${lost.slice(0, 10).join("\n  ")}`);

        const cpuSeconds = (ticksEnd - ticksStart) / CLK_TCK;
        const datagrams = countersEnd.datagramsSent - countersStart.datagramsSent;
        const iterations = countersEnd.loopIterations - countersStart.loopIterations;
        if (datagrams <= 0 || iterations <= 0) throw new Error(`the SFU counters did not move: ${datagrams} datagrams, ${iterations} iterations`);
        const m: Measurement = {
          participants: size,
          windowS,
          cpuPercent: (cpuSeconds / windowS) * 100,
          datagramsPerS: datagrams / windowS,
          loopIterationsPerS: iterations / windowS,
          cpuMsPerKDatagram: (cpuSeconds * 1000) / (datagrams / 1000),
        };
        measurements.push(m);
        console.log(
          `    ${size} participants: executor ${m.cpuPercent.toFixed(1)}% of a core, ${m.datagramsPerS.toFixed(0)} datagrams/s sent, ` +
            `${m.loopIterationsPerS.toFixed(0)} loop passes/s, ${m.cpuMsPerKDatagram.toFixed(2)} CPU ms per 1000 datagrams (window ${windowS.toFixed(1)} s)`,
        );
      } finally {
        for (const user of users) {
          // Hang up as an app does: a tab closed mid-call keeps its SFU slot until the server notices.
          await Promise.race([
            user.page.evaluate(() => (window as any).__call.leave()).catch(() => {}),
            new Promise((r) => setTimeout(r, 10_000)),
          ]);
          await user.context.close().catch(() => {});
        }
      }
    });
  }
});
