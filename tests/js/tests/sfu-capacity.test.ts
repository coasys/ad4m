import { execFileSync } from "node:child_process";
import { mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import type { Server } from "node:http";
import { loadavg } from "node:os";
import path from "node:path";
import type { Browser } from "playwright-core";
import {
  joinCall, launch, outboundIp, receiveProblems, report, servePage,
  type BrowserKind, type BrowserUser, type Launched,
} from "../helpers/sfu/browser/browsers.js";
import { installCallHarness, type PageReport } from "../helpers/sfu/browser/page.js";
import { startWorld, type World } from "../helpers/sfu/e2e/world.js";

/**
 * Capacity measurement, not a pass/fail suite: calls of growing size in real
 * browsers on one executor, with the executor's CPU, memory and threads read
 * from /proc and each tab's WebRTC stats read over a steady window.
 * In no CI job. Run by hand, after `pnpm run prepare-test-local`:
 *
 *   SFU_CAP_RUNS=sfu:4:chromium,sfu:8:mixed,mesh:4:chromium \
 *   SFU_CAP_OUT=/path/to/results pnpm exec ts-mocha -p tsconfig.json --timeout 0 --exit tests/sfu-capacity.test.ts
 *
 * - `SFU_CAP_RUNS`: `topology:size:browsers` items; browsers is `chromium`,
 *   `firefox`, `webkit` or `mixed` (round robin over the three).
 * - `SFU_CAP_STEADY_S` (60): the measured window, after every tab receives every other.
 * - `SFU_CAP_VIDEO` (320x240): the fake camera's size.
 * - `SFU_CAP_OUT`: one JSON file per run is written there.
 *
 * Bytes are WebRTC's transport counters (the selected candidate pair) summed
 * over the tabs: a tab's bytes sent reach the SFU, its bytes received left the
 * SFU. On loopback nothing limits bandwidth; the figures are for extrapolation.
 */

type Topology = "sfu" | "mesh";
interface RunSpec { topology: Topology; size: number; browsers: BrowserKind | "mixed" }

const RUNS: RunSpec[] = (process.env.SFU_CAP_RUNS || "sfu:4:chromium").split(",").map((item) => {
  const [topology, size, browsers] = item.trim().split(":");
  return { topology: topology as Topology, size: Number(size), browsers: (browsers || "chromium") as RunSpec["browsers"] };
});
const STEADY_MS = Number(process.env.SFU_CAP_STEADY_S || 60) * 1000;
const [VIDEO_W, VIDEO_H] = (process.env.SFU_CAP_VIDEO || "320x240").split("x").map(Number);
const OUT = process.env.SFU_CAP_OUT || path.join(process.cwd(), "sfu-capacity-results");
const CONVERGE_TIMEOUT_MS = Number(process.env.SFU_CAP_CONVERGE_S || 240) * 1000;
const MAX_USERS = Math.max(...RUNS.map((r) => r.size));
const CLK_TCK = Number(execFileSync("getconf", ["CLK_TCK"]).toString().trim());
const KINDS: BrowserKind[] = ["chromium", "firefox", "webkit"];
const TABS_PER_BROWSER = 8;

/** Records every peer connection the page makes, for `__capStats`. Self-contained: Playwright serialises it. */
function installStats(): void {
  const w = window as any;
  const Native = w.RTCPeerConnection;
  const pcs: RTCPeerConnection[] = [];
  w.RTCPeerConnection = class extends Native {
    constructor(...args: any[]) {
      super(...args);
      pcs.push(this as unknown as RTCPeerConnection);
    }
  };
  const KEEP = [
    "id", "type", "kind", "rid", "ssrc", "bytesSent", "bytesReceived", "packetsSent", "packetsReceived", "packetsLost",
    "jitter", "framesDecoded", "framesDropped", "framesPerSecond", "frameWidth", "frameHeight", "framesEncoded",
    "freezeCount", "totalFreezesDuration", "pauseCount", "totalPausesDuration", "nackCount", "pliCount", "firCount",
    "concealedSamples", "totalSamplesReceived", "concealmentEvents", "roundTripTime", "currentRoundTripTime", "state",
    "nominated", "selected", "selectedCandidatePairId", "qualityLimitationReason", "fractionLost", "audioLevel",
    "retransmittedPacketsSent", "retransmittedBytesSent", "headerBytesReceived", "headerBytesSent",
  ];
  w.__capStats = async () => {
    const out: { state: string; stats: Record<string, unknown>[] }[] = [];
    for (const pc of pcs) {
      if (pc.connectionState === "closed") continue;
      const report = await pc.getStats();
      const stats: Record<string, unknown>[] = [];
      report.forEach((s: any) => {
        if (!["inbound-rtp", "outbound-rtp", "remote-inbound-rtp", "candidate-pair", "transport"].includes(s.type)) return;
        const kept: Record<string, unknown> = {};
        for (const key of KEEP) if (s[key] !== undefined) kept[key] = s[key];
        stats.push(kept);
      });
      out.push({ state: pc.connectionState, stats });
    }
    return out;
  };
}

type PcStats = { state: string; stats: Record<string, any>[] }[];

/** Sums over one tab's connections, from one `__capStats` read. */
function summarise(pcs: PcStats) {
  const s = {
    connections: pcs.length, connected: pcs.filter((p) => p.state === "connected").length,
    transportBytesSent: 0, transportBytesReceived: 0, pairRtts: [] as number[], remoteRtts: [] as number[],
    audioIn: { streams: 0, bytes: 0, packets: 0, lost: 0, concealed: 0, samples: 0, jitter: [] as number[] },
    videoIn: { streams: 0, bytes: 0, packets: 0, lost: 0, framesDecoded: 0, framesDropped: 0, freezeCount: 0, freezeS: 0, nack: 0, pli: 0, fps: [] as number[], widths: [] as number[] },
    audioOut: { bytes: 0, packets: 0 },
    videoOut: { bytes: 0, packets: 0, retransmittedBytes: 0, layers: [] as { rid: string | null; width: number; height: number; fps: number; limitation: string }[] },
  };
  for (const pc of pcs) {
    const byId = new Map(pc.stats.map((st) => [st.id, st]));
    const transport = pc.stats.find((st) => st.type === "transport" && st.selectedCandidatePairId);
    const pair = transport
      ? byId.get(transport.selectedCandidatePairId)
      : pc.stats.find((st) => st.type === "candidate-pair" && (st.selected || st.nominated) && st.state === "succeeded");
    if (pair) {
      s.transportBytesSent += pair.bytesSent ?? 0;
      s.transportBytesReceived += pair.bytesReceived ?? 0;
      if (typeof pair.currentRoundTripTime === "number") s.pairRtts.push(pair.currentRoundTripTime);
    }
    for (const st of pc.stats) {
      if (st.type === "remote-inbound-rtp" && typeof st.roundTripTime === "number") s.remoteRtts.push(st.roundTripTime);
      if (st.type === "inbound-rtp" && st.kind === "audio") {
        s.audioIn.streams++; s.audioIn.bytes += st.bytesReceived ?? 0; s.audioIn.packets += st.packetsReceived ?? 0;
        s.audioIn.lost += Math.max(0, st.packetsLost ?? 0); s.audioIn.concealed += st.concealedSamples ?? 0;
        s.audioIn.samples += st.totalSamplesReceived ?? 0; if (typeof st.jitter === "number") s.audioIn.jitter.push(st.jitter);
      }
      if (st.type === "inbound-rtp" && st.kind === "video") {
        const v = s.videoIn;
        v.streams++; v.bytes += st.bytesReceived ?? 0; v.packets += st.packetsReceived ?? 0; v.lost += Math.max(0, st.packetsLost ?? 0);
        v.framesDecoded += st.framesDecoded ?? 0; v.framesDropped += st.framesDropped ?? 0; v.freezeCount += st.freezeCount ?? 0;
        v.freezeS += st.totalFreezesDuration ?? 0; v.nack += st.nackCount ?? 0; v.pli += st.pliCount ?? 0;
        if (typeof st.framesPerSecond === "number") v.fps.push(st.framesPerSecond);
        if (typeof st.frameWidth === "number") v.widths.push(st.frameWidth);
      }
      if (st.type === "outbound-rtp" && st.kind === "audio") { s.audioOut.bytes += st.bytesSent ?? 0; s.audioOut.packets += st.packetsSent ?? 0; }
      if (st.type === "outbound-rtp" && st.kind === "video") {
        s.videoOut.bytes += st.bytesSent ?? 0; s.videoOut.packets += st.packetsSent ?? 0; s.videoOut.retransmittedBytes += st.retransmittedBytesSent ?? 0;
        s.videoOut.layers.push({ rid: st.rid ?? null, width: st.frameWidth ?? 0, height: st.frameHeight ?? 0, fps: st.framesPerSecond ?? 0, limitation: st.qualityLimitationReason ?? "" });
      }
    }
  }
  return s;
}
type Summary = ReturnType<typeof summarise>;

const mean = (xs: number[]) => (xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : null);

// ---- /proc readers ------------------------------------------------------

function cpuTicks(statPath: string): { comm: string; ticks: number; ppid: number } | null {
  try {
    const text = readFileSync(statPath, "utf8");
    const close = text.lastIndexOf(")");
    const comm = text.slice(text.indexOf("(") + 1, close);
    const fields = text.slice(close + 2).split(" ");
    // fields[0] is state (field 3); utime is field 14, stime 15; ppid field 4.
    return { comm, ticks: Number(fields[11]) + Number(fields[12]), ppid: Number(fields[1]) };
  } catch {
    return null;
  }
}

function threadTicks(pid: number): Map<string, { comm: string; ticks: number }> {
  const out = new Map<string, { comm: string; ticks: number }>();
  for (const tid of readdirSync(`/proc/${pid}/task`)) {
    const t = cpuTicks(`/proc/${pid}/task/${tid}/stat`);
    if (t) out.set(tid, { comm: t.comm, ticks: t.ticks });
  }
  return out;
}

function status(pid: number): { rssKb: number; threads: number } {
  const text = readFileSync(`/proc/${pid}/status`, "utf8");
  const field = (name: string) => Number(text.match(new RegExp(`^${name}:\\s+(\\d+)`, "m"))?.[1] ?? 0);
  return { rssKb: field("VmRSS"), threads: field("Threads") };
}

function systemTicks(): { total: number; idle: number } {
  const line = readFileSync("/proc/stat", "utf8").split("\n")[0].trim().split(/\s+/).slice(1).map(Number);
  return { total: line.reduce((a, b) => a + b, 0), idle: line[3] + line[4] };
}

/** CPU ticks of every descendant of `root`, leaving out `exclude`'s subtree: the browsers and their helpers. */
function descendantTicks(root: number, exclude: number): number {
  const parent = new Map<number, number>();
  const ticks = new Map<number, number>();
  for (const entry of readdirSync("/proc")) {
    if (!/^\d+$/.test(entry)) continue;
    const t = cpuTicks(`/proc/${entry}/stat`);
    if (!t) continue;
    parent.set(Number(entry), t.ppid);
    ticks.set(Number(entry), t.ticks);
  }
  let sum = 0;
  for (const [pid, value] of ticks) {
    let p: number | undefined = pid;
    let under = false;
    while (p && p > 1) {
      if (p === exclude) { under = false; break; }
      if (p === root) { under = pid !== root; break; }
      p = parent.get(p);
    }
    if (under) sum += value;
  }
  return sum;
}

function executorPid(port: number): number {
  const out = execFileSync("ss", ["-Htlnp", `sport = :${port}`]).toString();
  const pid = Number(out.match(/pid=(\d+)/)?.[1]);
  if (!pid) throw new Error(`no process listens on ${port}: ${out}`);
  const comm = readFileSync(`/proc/${pid}/comm`, "utf8").trim();
  if (!comm.startsWith("ad4m-executor")) throw new Error(`port ${port} belongs to ${comm} (${pid}), not the executor`);
  return pid;
}

// ---- the runs -------------------------------------------------------------

describe("SFU capacity: calls of growing size in real browsers", function () {
  this.timeout(0);

  let world: World | null = null;
  let server: Server | null = null;
  let pageUrl = "";
  let pid = 0;
  const browsers = new Map<BrowserKind, Launched>();
  const userNames = Array.from({ length: MAX_USERS }, (_, i) => `u${i + 1}`);

  before(async function () {
    mkdirSync(OUT, { recursive: true });
    const kinds = new Set<BrowserKind>(RUNS.flatMap((r) => (r.browsers === "mixed" ? KINDS : [r.browsers])));
    for (const kind of kinds) {
      const launched = await launch(kind);
      browsers.set(kind, launched);
      console.log(launched.browser ? `  ${kind} ${launched.browser.version()}` : `  ${launched.reason}`);
    }
    ({ url: pageUrl, server } = await servePage());
    const sfuIp = process.env.SFU_BROWSERS_SFU_ADDR || await outboundIp();
    if (!sfuIp) throw new Error("no IPv4 address beyond loopback for the SFU to bind");
    world = await startWorld({
      id: "capacity",
      executorEnv: { AD4M_SFU_BIND_ADDR: sfuIp },
      users: userNames.map((name) => ({ name })),
      neighbourhoods: [
        // A room holds maxMeshParticipants x 4: 24 here.
        { name: "sfu", owner: "u1", members: userNames, call: { mode: "designated", maxMeshParticipants: 6 } },
        { name: "mesh", owner: "u1", members: userNames, call: { mode: "mesh", maxMeshParticipants: Math.max(MAX_USERS, 6) } },
      ],
    });
    pid = executorPid(world.users.u1.port);
    console.log(`  executor pid ${pid}, SFU on ${sfuIp}`);
  });

  /**
   * The `index`th browser process of a kind. One headless Chromium starts at most
   * 16 fake microphones ("Could not start audio source" for the 17th), so tabs
   * are spread over processes, TABS_PER_BROWSER each.
   */
  const extra = new Map<string, Launched>();
  async function browserFor(kind: BrowserKind, index: number): Promise<Launched> {
    if (index === 0) return browsers.get(kind)!;
    const key = `${kind}-${index}`;
    if (!extra.has(key)) extra.set(key, await launch(kind));
    return extra.get(key)!;
  }

  after(async function () {
    for (const { browser } of [...browsers.values(), ...extra.values()]) await browser?.close().catch(() => {});
    server?.close();
    await world?.stop().catch(() => {});
  });

  async function openTab(browser: Browser, kind: BrowserKind, name: string, did: string): Promise<BrowserUser> {
    const context = await browser.newContext(kind === "firefox" ? {} : { permissions: ["camera", "microphone"] });
    await context.addInitScript(installCallHarness);
    await context.addInitScript(installStats);
    const page = await context.newPage();
    const consoleErrors: string[] = [];
    page.on("console", (m) => { if (m.type() === "error") consoleErrors.push(m.text().replace(/token=[^'"\s]+/g, "token=…")); });
    page.on("pageerror", (e) => consoleErrors.push(`pageerror: ${e.message}`));
    await page.goto(pageUrl);
    await page.waitForFunction(() => (window as any).ad4mReady === true);
    return { name, kind, did, context, page, consoleErrors };
  }

  const stats = async (users: BrowserUser[]) =>
    Object.fromEntries(await Promise.all(users.map(async (u) => [u.name, await u.page.evaluate(() => (window as any).__capStats())] as const))) as Record<string, PcStats>;
  const reports = async (users: BrowserUser[]) =>
    Object.fromEntries(await Promise.all(users.map(async (u) => [u.name, await report(u)] as const))) as Record<string, PageReport>;
  const problemsOf = (users: BrowserUser[], before: Record<string, PageReport>, now: Record<string, PageReport>) =>
    Object.fromEntries(users.map((u) => [u.name, users.filter((o) => o !== u).flatMap((o) =>
      receiveProblems(before[u.name], now[u.name], o.did).map((p) => `${p} [${o.name}]`))]));

  for (const spec of RUNS) {
    const label = `${spec.topology}-${spec.size}-${spec.browsers}`;
    it(label, async function () {
      const result: Record<string, any> = { label, ...spec, video: `${VIDEO_W}x${VIDEO_H}`, steadyMs: STEADY_MS, startedAt: new Date().toISOString(), loadavgAtStart: loadavg() };
      const file = path.join(OUT, `${label}-${Date.now()}.json`);
      const save = () => writeFileSync(file, JSON.stringify(result, null, 1));
      const users: BrowserUser[] = [];
      try {
        const kindFor = (i: number): BrowserKind => (spec.browsers === "mixed" ? KINDS[i % 3] : spec.browsers);
        const perKind = new Map<BrowserKind, number>();
        for (let i = 0; i < spec.size; i++) {
          const kind = kindFor(i);
          const n = perKind.get(kind) ?? 0;
          perKind.set(kind, n + 1);
          const launched = await browserFor(kind, Math.floor(n / TABS_PER_BROWSER));
          if (!launched.browser) throw new Error(launched.reason);
          const w = world!.users[userNames[i]];
          users.push(await openTab(launched.browser, kind, w.name, w.did));
        }
        const executorBefore = status(pid);
        result.executorIdle = executorBefore;

        // Join everyone, one right after the other, as people do at a meeting's start.
        const room = `cap-${label}-${Date.now()}`;
        const joinedAt: Record<string, number> = {};
        const t0 = Date.now();
        for (const user of users) {
          const w = world!.users[user.name];
          joinedAt[user.name] = Date.now() - t0;
          await joinCall(user, {
            executorUrl: `http://127.0.0.1:${w.port}`, token: w.token, neighbourhoodUrl: world!.url(spec.topology),
            room, name: user.name, video: { width: VIDEO_W, height: VIDEO_H },
          });
        }
        result.joinsIssuedMs = Date.now() - t0;

        // Until every tab receives every other: per tab, when it first did.
        const fullAt: Record<string, number> = {};
        const activeAt: Record<string, number> = {};
        let before = await reports(users);
        let lastProblems: Record<string, string[]> = {};
        for (;;) {
          await new Promise((r) => setTimeout(r, 2_000));
          const now = await reports(users);
          lastProblems = problemsOf(users, before, now);
          const at = Date.now() - t0;
          for (const user of users) {
            if (activeAt[user.name] === undefined && now[user.name].state === "active") activeAt[user.name] = at;
            if (fullAt[user.name] === undefined && lastProblems[user.name].length === 0) fullAt[user.name] = at;
          }
          before = now;
          if (users.every((u) => fullAt[u.name] !== undefined)) break;
          if (Date.now() - t0 > CONVERGE_TIMEOUT_MS) {
            result.convergeFailed = Object.fromEntries(Object.entries(lastProblems).filter(([, p]) => p.length).map(([n, p]) => [n, p.slice(0, 6).concat(p.length > 6 ? [`… ${p.length} problems`] : [])]));
            result.consoleErrors = Object.fromEntries(users.map((u) => [u.name, u.consoleErrors.slice(-5)]));
            // Where does a tab that never connected stand? Each step of the page's join, timed, on its own.
            result.stuckProbe = {};
            for (const user of users.filter((u) => Object.keys(before[u.name].connections).length === 0)) {
              const w = world!.users[user.name];
              result.stuckProbe[user.name] = await user.page.evaluate(async (a) => {
                const timed = async (f: () => Promise<unknown>) => {
                  const t = performance.now();
                  try {
                    const v = await Promise.race([f(), new Promise((_, no) => setTimeout(() => no(new Error("10 s timeout")), 10_000))]);
                    return { ok: true, ms: performance.now() - t, value: typeof v === "object" ? undefined : v };
                  } catch (e: any) {
                    return { ok: false, ms: performance.now() - t, error: String(e?.message ?? e) };
                  }
                };
                const client = new (window as any).ad4m.Ad4mClient(a.executorUrl, a.token);
                let perspective: any = null;
                return {
                  perspectives: await timed(async () => { perspective = (await client.perspective.all()).find((p: any) => p.sharedUrl === a.url); return !!perspective; }),
                  sfuConfig: await timed(() => perspective.getNeighbourhoodProxy().sfuConfig(a.url)),
                  getUserMedia: await timed(() => navigator.mediaDevices.getUserMedia({ audio: true, video: { width: 320, height: 240 } })),
                };
              }, { executorUrl: `http://127.0.0.1:${w.port}`, token: w.token, url: world!.url(spec.topology) });
            }
            console.log(`    stuck tabs: ${JSON.stringify(result.stuckProbe)}`);
            console.log(`    ${label}: not everyone received everyone within ${CONVERGE_TIMEOUT_MS / 1000} s; measuring anyway`);
            break;
          }
        }
        result.joinTimes = Object.fromEntries(users.map((u) => [u.name, { joinCalledMs: joinedAt[u.name], activeMs: activeAt[u.name] ?? null, receivesAllMs: fullAt[u.name] ?? null, receivesAllAfterOwnJoinMs: fullAt[u.name] !== undefined ? fullAt[u.name] - joinedAt[u.name] : null }]));
        result.convergedMs = Math.max(...users.map((u) => fullAt[u.name] ?? NaN));
        save();

        // Settle, then the steady window.
        await new Promise((r) => setTimeout(r, 10_000));
        result.loadavgAtWindow = loadavg();
        const reportsStart = await reports(users);
        const statsStart = await stats(users);
        const threadsStart = threadTicks(pid);
        const procStart = cpuTicks(`/proc/${pid}/stat`)!.ticks;
        const sysStart = systemTicks();
        const browsersStart = descendantTicks(process.pid, pid);
        const wStart = Date.now();
        const samples: { t: number; cpuPct: number; rssKb: number; threads: number }[] = [];
        let lastTicks = procStart;
        let lastT = wStart;
        while (Date.now() - wStart < STEADY_MS) {
          await new Promise((r) => setTimeout(r, 1_000));
          const ticks = cpuTicks(`/proc/${pid}/stat`)!.ticks;
          const t = Date.now();
          samples.push({ t: t - wStart, cpuPct: ((ticks - lastTicks) / CLK_TCK) / ((t - lastT) / 1000) * 100, ...status(pid) });
          lastTicks = ticks; lastT = t;
        }
        const wEnd = Date.now();
        const seconds = (wEnd - wStart) / 1000;
        const procEnd = cpuTicks(`/proc/${pid}/stat`)!.ticks;
        const sysEnd = systemTicks();
        const browsersEnd = descendantTicks(process.pid, pid);
        const threadsEnd = threadTicks(pid);
        const statsEnd = await stats(users);
        const reportsEnd = await reports(users);

        const threadCpu = [...threadsEnd].map(([tid, t]) => ({ tid, comm: t.comm, cpuPct: ((t.ticks - (threadsStart.get(tid)?.ticks ?? 0)) / CLK_TCK) / seconds * 100 }))
          .sort((a, b) => b.cpuPct - a.cpuPct);
        const sysTotal = sysEnd.total - sysStart.total;
        result.executor = {
          pid, windowS: seconds,
          cpuPct: ((procEnd - procStart) / CLK_TCK) / seconds * 100,
          cpuPctMax1s: Math.max(...samples.map((s) => s.cpuPct)),
          rssKbMean: mean(samples.map((s) => s.rssKb)), rssKbMax: Math.max(...samples.map((s) => s.rssKb)),
          threads: samples[samples.length - 1]?.threads,
          busiestThreads: threadCpu.slice(0, 6),
          threadsAbove1Pct: threadCpu.filter((t) => t.cpuPct > 1).length,
          samples,
        };
        result.host = {
          cpus: (await import("node:os")).cpus().length,
          systemBusyPct: ((sysTotal - (sysEnd.idle - sysStart.idle)) / sysTotal) * 100,
          browsersAndHarnessCpuPct: ((browsersEnd - browsersStart) / CLK_TCK) / seconds * 100,
        };

        const perUser: Record<string, any> = {};
        let inBytes = 0, outBytes = 0;
        for (const user of users) {
          const a = summarise(statsStart[user.name]);
          const b = summarise(statsEnd[user.name]);
          const sent = (b.transportBytesSent - a.transportBytesSent) / seconds;
          const received = (b.transportBytesReceived - a.transportBytesReceived) / seconds;
          inBytes += sent; outBytes += received;
          const audioPackets = b.audioIn.packets - a.audioIn.packets;
          const audioLost = b.audioIn.lost - a.audioIn.lost;
          const videoPackets = b.videoIn.packets - a.videoIn.packets;
          const videoLost = b.videoIn.lost - a.videoIn.lost;
          const samplesIn = b.audioIn.samples - a.audioIn.samples;
          perUser[user.name] = {
            kind: user.kind,
            connections: b.connections, connected: b.connected,
            sendBytesPerS: sent, receiveBytesPerS: received,
            audioStreamsIn: b.audioIn.streams, videoStreamsIn: b.videoIn.streams,
            audioLossPct: audioPackets + audioLost > 0 ? (audioLost / (audioPackets + audioLost)) * 100 : null,
            videoLossPct: videoPackets + videoLost > 0 ? (videoLost / (videoPackets + videoLost)) * 100 : null,
            audioConcealedPct: samplesIn > 0 ? ((b.audioIn.concealed - a.audioIn.concealed) / samplesIn) * 100 : null,
            audioJitterMsMean: (mean(b.audioIn.jitter) ?? 0) * 1000,
            framesDecodedPerS: (b.videoIn.framesDecoded - a.videoIn.framesDecoded) / seconds,
            framesDecodedPerSPerStream: b.videoIn.streams ? (b.videoIn.framesDecoded - a.videoIn.framesDecoded) / seconds / b.videoIn.streams : null,
            framesDroppedInWindow: b.videoIn.framesDropped - a.videoIn.framesDropped,
            freezesInWindow: b.videoIn.freezeCount - a.videoIn.freezeCount,
            freezeSecondsInWindow: b.videoIn.freezeS - a.videoIn.freezeS,
            nacksInWindow: b.videoIn.nack - a.videoIn.nack, plisInWindow: b.videoIn.pli - a.videoIn.pli,
            fpsMinIn: b.videoIn.fps.length ? Math.min(...b.videoIn.fps) : null,
            widthsIn: [...new Set(b.videoIn.widths)],
            pairRttMsMean: (mean(b.pairRtts) ?? NaN) * 1000, remoteRttMsMean: (mean(b.remoteRtts) ?? NaN) * 1000,
            sendAudioBytesPerS: (b.audioOut.bytes - a.audioOut.bytes) / seconds,
            sendVideoBytesPerS: (b.videoOut.bytes - a.videoOut.bytes) / seconds,
            sentLayers: b.videoOut.layers,
            receivesAllInWindow: problemsOf([user, ...users.filter((u) => u !== user)], reportsStart, reportsEnd)[user.name],
            sessionErrors: reportsEnd[user.name].sessionErrors, topology: reportsEnd[user.name].topology,
          };
        }
        result.traffic = {
          source: "WebRTC transport stats (selected candidate pair bytesSent/bytesReceived), summed over tabs; excludes UDP/IP headers",
          intoSfuBytesPerS: inBytes, outOfSfuBytesPerS: outBytes,
          perParticipantSendBytesPerS: inBytes / users.length, perParticipantReceiveBytesPerS: outBytes / users.length,
        };
        result.perUser = perUser;
        result.everyoneReceivesEveryone = Object.values(perUser).every((u: any) => u.receivesAllInWindow.length === 0);
        result.finishedAt = new Date().toISOString();
        save();
        console.log(`    ${label}: executor ${result.executor.cpuPct.toFixed(1)}% CPU (top thread ${result.executor.busiestThreads[0]?.comm} ${result.executor.busiestThreads[0]?.cpuPct.toFixed(1)}%), ` +
          `RSS ${(result.executor.rssKbMax / 1024).toFixed(0)} MB, ${result.executor.threads} threads, in ${(inBytes * 8 / 1e6).toFixed(2)} Mbit/s, out ${(outBytes * 8 / 1e6).toFixed(2)} Mbit/s, ` +
          `all receive all: ${result.everyoneReceivesEveryone}, converged ${result.convergedMs} ms -> ${file}`);
      } catch (error) {
        result.error = String(error instanceof Error ? error.stack : error);
        save();
        throw error;
      } finally {
        for (const user of users) {
          await Promise.race([
            user.page.evaluate(() => (window as any).__call.leave()).catch(() => {}),
            new Promise((r) => setTimeout(r, 10_000)),
          ]);
          await user.context.close().catch(() => {});
        }
        // Let the SFU drop what is left before the next run.
        await new Promise((r) => setTimeout(r, 15_000));
      }
    });
  }
});
