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
 * - the RTP packets the SFU forwards per second, and the passes its media event
 *   loop makes per second (both read from `sfu.status`, twice, the time apart);
 * - the executor's CPU per thousand forwarded packets, which stays put when the
 *   host is faster or slower in the same way for the packets and the CPU.
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
const TABS_PER_BROWSER = 8;
const MAX_USERS = Math.max(...SIZES);
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
  packetsPerS: number;
  loopIterationsPerS: number;
  cpuMsPerKPacket: number;
}

interface PerfMetric {
  metric: string; value: number; unit: string; better: "lower" | "higher"; params: { participants: number };
}

/** What the nightly history records for one measurement. */
function perfMetrics(m: Measurement): PerfMetric[] {
  const params = { participants: m.participants };
  return [
    { metric: "sfu_capacity.cpu_percent", value: m.cpuPercent, unit: "percent", better: "lower", params },
    { metric: "sfu_capacity.packets_per_s", value: m.packetsPerS, unit: "packets/s", better: "higher", params },
    { metric: "sfu_capacity.loop_iterations_per_s", value: m.loopIterationsPerS, unit: "iterations/s", better: "lower", params },
    { metric: "sfu_capacity.cpu_ms_per_kpacket", value: m.cpuMsPerKPacket, unit: "ms", better: "lower", params },
  ];
}

describe("SFU capacity: the executor's cost of calls of growing size", function () {
  this.timeout(0);

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
          await joinCall(user, {
            executorUrl: `http://127.0.0.1:${w.port}`, token: w.token, neighbourhoodUrl: world!.url("sfu"),
            room, name: user.name, video: { width: VIDEO_W, height: VIDEO_H },
          });
        }

        // Until every tab receives every other, from fresh packets and frames.
        const deadline = Date.now() + CONVERGE_TIMEOUT_MS;
        let before = await reports(users);
        for (;;) {
          await new Promise((r) => setTimeout(r, 2_000));
          const now = await reports(users);
          const problems = problemsOf(users, before, now);
          if (problems.length === 0) break;
          if (Date.now() > deadline) {
            throw new Error(`not every tab received every other within ${CONVERGE_TIMEOUT_MS / 1000} s:\n  ${problems.slice(0, 10).join("\n  ")}`);
          }
          before = now;
        }

        // The window. The SFU's counters and the executor's CPU are read together, at both ends.
        const sfu = world!.users.u1.client.neighbourhood;
        const reportsStart = await reports(users);
        const countersStart = await sfu.sfuStatus();
        const ticksStart = cpuTicks(pid);
        const t0 = performance.now();
        await new Promise((r) => setTimeout(r, STEADY_MS));
        const countersEnd = await sfu.sfuStatus();
        const ticksEnd = cpuTicks(pid);
        const windowS = (performance.now() - t0) / 1000;
        const reportsEnd = await reports(users);

        // A window in which a tab lost another is not a measurement of a call of this size.
        const lost = problemsOf(users, reportsStart, reportsEnd);
        if (lost.length > 0) throw new Error(`a tab stopped receiving another in the window:\n  ${lost.slice(0, 10).join("\n  ")}`);

        const cpuSeconds = (ticksEnd - ticksStart) / CLK_TCK;
        const packets = countersEnd.packetsForwarded - countersStart.packetsForwarded;
        const iterations = countersEnd.loopIterations - countersStart.loopIterations;
        if (packets <= 0 || iterations <= 0) throw new Error(`the SFU counters did not move: ${packets} packets, ${iterations} iterations`);
        const m: Measurement = {
          participants: size,
          windowS,
          cpuPercent: (cpuSeconds / windowS) * 100,
          packetsPerS: packets / windowS,
          loopIterationsPerS: iterations / windowS,
          cpuMsPerKPacket: (cpuSeconds * 1000) / (packets / 1000),
        };
        measurements.push(m);
        console.log(
          `    ${size} participants: executor ${m.cpuPercent.toFixed(1)}% of a core, ${m.packetsPerS.toFixed(0)} packets/s forwarded, ` +
            `${m.loopIterationsPerS.toFixed(0)} loop passes/s, ${m.cpuMsPerKPacket.toFixed(2)} CPU ms per 1000 packets (window ${windowS.toFixed(1)} s)`,
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
