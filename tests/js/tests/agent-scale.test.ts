/**
 * Agent-scale wind tunnel: how the work of ONE executor grows with the number of agents it
 * holds (multi-user mode, no Holochain). For each agent count in AGENT_SCALE_COUNTS (default
 * 10,20,50,100) a fresh executor on a fresh data dir runs these stages:
 *
 *   1 onboard      createUser + loginUser for every user, one after the other
 *   2 sign-in      every user logs in again, all at once
 *   3 write        every user makes a perspective and writes AGENT_SCALE_LINKS links (batches of 20), all at once
 *   4 read         every user runs a SPARQL query over its perspective, all at once, 5 rounds
 *   5 events       every user listens for link-added, writes one link, and times its own event
 *   6 shared       every user joins one neighbourhood (local link language); the first writes a link,
 *                  time until every user sees it
 *   7 idle         IDLE seconds with no request
 *   8 restart      stop the executor, start it on the same data, time to ready and to each user's first query
 *
 * The executor's RSS, threads and open files are sampled every second from /proc.
 *
 * Writes $PERF_RESULTS_DIR/agent-scale.json (when set) for the nightly history, and always
 * prints each metric by agent count with its growth and flag (helpers/agent-scale.ts). It fails
 * only on a functional error: a user that cannot log in, an event that reaches another user, a
 * link that is lost. A metric that scales poorly is flagged, never failed here.
 *
 * Needs the local test languages: `pnpm run prepare-test-local`; run it with `pnpm run test:agent-scale`.
 */

import { ChildProcess } from "node:child_process";
import { randomUUID } from "node:crypto";
import { readdirSync, readFileSync, rmSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import * as chai from "chai";
import { Ad4mClient, Link, Perspective, PerspectiveProxy } from "@coasys/ad4m";
import {
  baseUrl, executorDataPath, gracefulShutdown, pollUntil, sleep, spawnExecutor, startExecutor,
} from "../utils/utils";
import { deregisterPorts, getFreePorts, registerPorts } from "../helpers/ports.js";
import { preparedLinkLanguage } from "../helpers/sfu/e2e/world.js";
import {
  cpuPercent, formatTable, growthRows, median, parseCounts, parseCpuTicks, parseStatus, percentile,
  writeResults, type MetricKind, type Readings,
} from "../helpers/agent-scale.js";

const expect = chai.expect;
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const BOOTSTRAP_SEED = path.join(__dirname, "..", "bootstrapSeed.json");
const ADMIN_CREDENTIAL = "agent-scale-admin";
const PASSWORD = "agent-scale-password";

const COUNTS = parseCounts(process.env.AGENT_SCALE_COUNTS);
const LINKS = Number(process.env.AGENT_SCALE_LINKS ?? 200);
const BATCH = 20;
const READ_ROUNDS = 5;
const IDLE_SECONDS = Number(process.env.AGENT_SCALE_IDLE_SECONDS ?? 30);
const CLOCK_TICKS_PER_SECOND = 100;
/** A wait for an event or a link to arrive; only the bound at which the test fails. */
const SIGNAL_TIMEOUT_MS = 120_000;

const PRED = "agent-scale://has";
const COUNT_QUERY = `SELECT (COUNT(*) AS ?n) WHERE { ?s <${PRED}> ?o }`;

const now = () => performance.now();

/**
 * Samples an executor's RSS, threads and open files from /proc every second. The executor
 * forbids core dumps and debugger attach (`forbid_core_dumps`, PR_SET_DUMPABLE 0), so the kernel
 * makes its /proc/<pid>/fd readable by root only: without root `fdsPeak` stays null and the
 * metric is not recorded, instead of recording 0.
 */
class ProcSampler {
  private pid = 0;
  private timer: NodeJS.Timeout | null = null;
  rssPeakMib = 0;
  threadsPeak = 0;
  fdsPeak: number | null = null;

  follow(pid: number): void {
    this.pid = pid;
    this.sample();
  }

  start(): void {
    this.timer = setInterval(() => this.sample(), 1000);
  }

  stop(): void {
    if (this.timer) clearInterval(this.timer);
    this.timer = null;
  }

  /** RSS in MiB right now. */
  rssMib(): number {
    return parseStatus(readFileSync(`/proc/${this.pid}/status`, "utf8")).rssMib;
  }

  cpuTicks(): number {
    return parseCpuTicks(readFileSync(`/proc/${this.pid}/stat`, "utf8"));
  }

  private sample(): void {
    if (!this.pid) return;
    try {
      const { rssMib, threads } = parseStatus(readFileSync(`/proc/${this.pid}/status`, "utf8"));
      this.rssPeakMib = Math.max(this.rssPeakMib, rssMib);
      this.threadsPeak = Math.max(this.threadsPeak, threads);
    } catch {
      // The process is gone (between a stop and a start): nothing to sample.
      return;
    }
    try {
      this.fdsPeak = Math.max(this.fdsPeak ?? 0, readdirSync(`/proc/${this.pid}/fd`).length);
    } catch {
      // Not readable without root (see above).
    }
  }
}

/** One gauge of the executor's /metrics (Prometheus text), read as the operator. */
async function gauge(port: number, name: string): Promise<number> {
  const response = await fetch(`${baseUrl(port)}/metrics`, { headers: { Authorization: `Bearer ${ADMIN_CREDENTIAL}` } });
  expect(response.status, "/metrics as the operator").to.equal(200);
  const line = (await response.text()).split("\n").find((l) => l.startsWith(`${name} `));
  expect(line, `${name} in /metrics`).to.not.equal(undefined);
  return Number(line!.split(" ").pop());
}

/** Time one async call in ms. */
async function timed<T>(fn: () => Promise<T>): Promise<{ ms: number; value: T }> {
  const t0 = now();
  const value = await fn();
  return { ms: now() - t0, value };
}

/** The count a COUNT query returns, whatever shape the row has. */
function countOf(result: unknown): number {
  const row = Array.isArray(result) ? result[0] : result;
  const raw = row && typeof row === "object" ? Object.values(row as object)[0] : row;
  const text = String(raw).replace(/^"?(-?\d+)"?.*$/s, "$1");
  return Number(text);
}

interface User {
  email: string;
  token: string;
  client: Ad4mClient;
  perspective?: PerspectiveProxy;
}

describe("Agent scale: one executor, many agents", function () {
  const readings: Readings = new Map();

  after(() => {
    // Print and write what was measured, also when a count failed half way.
    if (readings.size === 0) return;
    console.log(`\nAgent scale, by agents in one executor (${LINKS} links each):\n${formatTable(readings)}\n`);
    const flags = growthRows(readings).filter((r) => r.flag);
    console.log(flags.length === 0
      ? "No metric scales poorly."
      : `Scales poorly:\n${flags.map((r) => `  ${r.metric}: x${r.growth!.toFixed(1)} from ${r.from} to ${r.to} agents (${r.kind})`).join("\n")}`);
    if (process.env.PERF_RESULTS_DIR) console.log(`Wrote ${writeResults(process.env.PERF_RESULTS_DIR, readings, LINKS)}`);
  });

  for (const agents of COUNTS) {
    it(`holds ${agents} agents in one executor`, async function () {
      this.timeout(45 * 60_000);
      const metrics = new Map<string, { value: number; unit: string; kind: MetricKind }>();
      readings.set(agents, metrics);
      const put = (name: string, value: number, unit: string, kind: MetricKind) => metrics.set(name, { value, unit, kind });
      /** Median and p95 of one call's time (flat is good). */
      const putCalls = (name: string, ms: number[]) => {
        put(`${name}.median`, median(ms), "ms", "latency");
        put(`${name}.p95`, percentile(ms, 95), "ms", "latency");
      };

      const linkLanguage = preparedLinkLanguage();
      const ports = await getFreePorts(3);
      registerPorts(ports);
      const [port, hcAdmin, hcApp] = ports;
      const dataPath = path.join(__dirname, "..", "tst-tmp", "agents", `agent-scale-${agents}`);
      const sampler = new ProcSampler();
      const clients: Ad4mClient[] = [];
      const newClient = (token: string) => {
        const client = new Ad4mClient(baseUrl(port), token);
        clients.push(client);
        return client;
      };
      let executor: ChildProcess | null = null;

      try {
        executor = await startExecutor(
          dataPath, BOOTSTRAP_SEED, port, hcAdmin, hcApp,
          false, ADMIN_CREDENTIAL, undefined, undefined, undefined, false, undefined, undefined,
          false, // runHolochain
        );
        sampler.follow(executor.pid!);
        sampler.start();
        const admin = newClient(ADMIN_CREDENTIAL);
        await admin.agent.generate("passphrase");
        await admin.runtime.setMultiUserEnabled(true);
        const rssBefore = sampler.rssMib();

        // 1. Onboard, one after the other.
        const users: User[] = [];
        const createMs: number[] = [];
        const loginMs: number[] = [];
        for (let i = 0; i < agents; i++) {
          const email = `agent-scale-${agents}-${i}@example.com`;
          const created = await timed(() => admin.agent.createUser(email, PASSWORD));
          expect(created.value.success, `createUser ${email}: ${created.value.error}`).to.equal(true);
          await admin.runtime.setUserFreeAccess(email, true);
          const login = await timed(() => admin.agent.loginUser(email, PASSWORD));
          expect(login.value, `loginUser ${email}`).to.be.a("string").and.not.equal("");
          createMs.push(created.ms);
          loginMs.push(login.ms);
          users.push({ email, token: login.value, client: newClient(login.value) });
        }
        putCalls("onboard.create_ms", createMs);
        putCalls("onboard.login_ms", loginMs);
        // Each user's own DID, so the isolation checks below know whose they are.
        const dids = await Promise.all(users.map(async (u) => (await u.client.agent.status()).did!));
        expect(new Set(dids).size, "every user has its own DID").to.equal(agents);

        // 2. Every user signs in at once.
        const wallStart = now();
        const concurrent = await Promise.all(users.map((u) => timed(() => admin.agent.loginUser(u.email, PASSWORD))));
        put("onboard.concurrent_login_ms.wall", now() - wallStart, "ms", "total");
        // The slowest of N calls made at once waits for the others: it grows like the wall time.
        put("onboard.concurrent_login_ms.max", Math.max(...concurrent.map((c) => c.ms)), "ms", "total");
        for (const c of concurrent) expect(c.value, "concurrent loginUser").to.be.a("string").and.not.equal("");

        // 3. Every user writes, all at once.
        const batchMs: number[] = [];
        const writeStart = now();
        await Promise.all(users.map(async (u, i) => {
          u.perspective = await u.client.perspective.add(`agent-scale ${agents} #${i}`);
          for (let from = 0; from < LINKS; from += BATCH) {
            const links = Array.from({ length: Math.min(BATCH, LINKS - from) }, (_, k) => new Link({
              source: `agent-scale://user-${i}/item-${from + k}`,
              predicate: PRED,
              target: `literal://string:${from + k}`,
            }));
            batchMs.push((await timed(() => u.perspective!.addLinks(links))).ms);
          }
        }));
        put("write.total_ms", now() - writeStart, "ms", "total");
        putCalls("write.link_batch_ms", batchMs);

        // 4. Every user reads its own perspective, all at once, in rounds.
        const queryMs: number[] = [];
        for (let round = 0; round < READ_ROUNDS; round++) {
          const results = await Promise.all(users.map((u) => timed(() => u.perspective!.querySparql(COUNT_QUERY))));
          for (const [i, r] of results.entries()) {
            queryMs.push(r.ms);
            expect(countOf(r.value), `user ${i} sees every link it wrote (round ${round})`).to.equal(LINKS);
          }
        }
        putCalls("read.query_ms", queryMs);

        // 5. Events: every user times its own link-added, and none may see another user's.
        const ownSeen = users.map(() => new Map<string, number>());
        let leaked = 0;
        const seenAt = (i: number, marker: string) => ownSeen[i].get(marker);
        for (const [i, u] of users.entries()) {
          // Unfiltered: this client gets link-added of every perspective it may see, and only its own may arrive.
          u.client.on("link-added", (event) => {
            if (event.perspectiveUuid !== u.perspective!.uuid) leaked++;
            else ownSeen[i].set(String(event.link.data.target), now());
          });
        }
        const eventRound = async (marker: string): Promise<number[]> => {
          const sentAt = users.map(() => 0);
          await Promise.all(users.map(async (u, i) => {
            sentAt[i] = now();
            await u.perspective!.add(new Link({ source: `agent-scale://user-${i}/event`, predicate: PRED + "-event", target: `literal://string:${marker}` }));
          }));
          await pollUntil(() => users.every((_, i) => seenAt(i, `literal://string:${marker}`) !== undefined),
            { timeoutMs: SIGNAL_TIMEOUT_MS, intervalMs: 20, label: `every user's own link-added event (${marker})` });
          return users.map((_, i) => seenAt(i, `literal://string:${marker}`)! - sentAt[i]);
        };
        putCalls("events.own_ms", await eventRound("first"));
        // A sentinel: once every user got its second event, an event another user's first write
        // caused would have arrived too (one socket keeps the order of its events).
        await eventRound("second");
        put("events.leaked", leaked, "count", "count");
        expect(leaked, "link-added events of another user's perspective").to.equal(0);

        // 6. One neighbourhood, every user a member.
        const owner = users[0];
        const sharedName = `agent-scale shared ${agents}`;
        const shared = await owner.client.perspective.add(sharedName);
        const language = await owner.client.languages.applyTemplateAndPublish(linkLanguage, JSON.stringify({ uid: randomUUID(), name: sharedName }));
        const url = await owner.client.neighbourhood.publishFromPerspective(shared.uuid, language.address, new Perspective([]));
        const joinMs: number[] = [];
        const members: PerspectiveProxy[] = [shared];
        for (const u of users.slice(1)) {
          const joined = await timed(() => u.client.neighbourhood.joinFromUrl(url));
          joinMs.push(joined.ms);
          const proxy = await u.client.perspective.byUUID(joined.value.uuid);
          expect(proxy, `${u.email} has the joined perspective`).to.not.equal(null);
          members.push(proxy!);
        }
        if (joinMs.length > 0) putCalls("shared.join_ms", joinMs);
        const ready = ["LINK_LANGUAGE_INSTALLED_BUT_NOT_SYNCED", "SYNCED"];
        await pollUntil(async () => {
          for (const [i, u] of users.entries()) {
            const state = (await u.client.perspective.all()).find((p) => p.uuid === members[i].uuid)?.state;
            if (!ready.includes(state as string)) return false;
          }
          return true;
        }, { timeoutMs: SIGNAL_TIMEOUT_MS, intervalMs: 100, label: "the link language installed for every member" });
        const sharedSeen: number[] = new Array(agents).fill(0);
        for (const [i, m] of members.entries()) {
          m.on("link-added", ({ link }) => {
            if (link.data.target === "literal://string:shared-hello" && !sharedSeen[i]) sharedSeen[i] = now();
          });
        }
        // The listeners are registered on the executor before the first write: each client's
        // watch goes out ahead of its later calls, and the writer's call is the owner's.
        await Promise.all(users.map((u) => u.client.perspective.all()));
        const sharedSentAt = now();
        await members[0].add(new Link({ source: "agent-scale://shared", predicate: PRED + "-shared", target: "literal://string:shared-hello" }));
        await pollUntil(() => sharedSeen.every((t) => t > 0),
          { timeoutMs: SIGNAL_TIMEOUT_MS, intervalMs: 20, label: "every member sees the first user's link" });
        const sharedDelays = sharedSeen.map((t) => t - sharedSentAt);
        // The first of the other users to see it (the writer's own event is not a delivery).
        put("shared.first_see_ms", Math.min(...sharedDelays.slice(1)), "ms", "latency");
        put("shared.all_see_ms", Math.max(...sharedDelays), "ms", "total");

        // What the executor holds open for all these agents (its own gauges, not /proc).
        put("perspectives_open", await gauge(port, "ad4m_perspectives_open"), "count", "total");
        put("sparql_stores_open", await gauge(port, "ad4m_sparql_stores_open"), "count", "total");

        // 7. Idle: no request, only the executor's own background work.
        const ticksBefore = sampler.cpuTicks();
        await sleep(IDLE_SECONDS * 1000);
        put("idle.cpu_pct", cpuPercent(ticksBefore, sampler.cpuTicks(), IDLE_SECONDS, CLOCK_TICKS_PER_SECOND), "percent", "total");

        // 8. Restart on the same data: time to ready and to each user's first query.
        for (const client of clients.splice(0)) client.close();
        const stopStart = now();
        await gracefulShutdown(executor, `agent-scale ${agents} executor`, 60_000);
        put("restart.stop_ms", now() - stopStart, "ms", "total");
        const startStart = now();
        // The same arguments, without the init that would wipe the data.
        executor = await spawnExecutor(executor.spawnargs.slice(1), port, {});
        sampler.follow(executor.pid!);
        const adminAfter = newClient(ADMIN_CREDENTIAL);
        await adminAfter.agent.unlock("passphrase", true);
        put("restart.ready_ms", now() - startStart, "ms", "total");
        // `unlock` answers before the executor has loaded every user's perspectives. A user's first
        // query is the time from ready until its own links answer, retrying while its perspective is
        // not loaded yet; one that never loads fails the test at the timeout.
        const readyAt = now();
        const firstQuery = await Promise.all(users.map(async (u, i) => {
          const client = newClient(u.token);
          let answered = NaN;
          await pollUntil(async () => {
            const perspective = await client.perspective.byUUID(u.perspective!.uuid);
            if (perspective === null) return false;
            expect(countOf(await perspective.querySparql(COUNT_QUERY)), `user ${i}'s links after the restart`).to.equal(LINKS);
            answered = now();
            return true;
          }, { timeoutMs: SIGNAL_TIMEOUT_MS, intervalMs: 10, label: `user ${i}'s perspective after the restart` });
          return answered - readyAt;
        }));
        putCalls("restart.first_query_ms", firstQuery);

        sampler.stop();
        put("rss_peak_mib", sampler.rssPeakMib, "MiB", "total");
        // The memory each agent added to the executor's own, with no agent in it.
        put("rss_per_agent_mib", (sampler.rssPeakMib - rssBefore) / agents, "MiB", "latency");
        put("threads_peak", sampler.threadsPeak, "count", "total");
        if (sampler.fdsPeak !== null) put("fds_peak", sampler.fdsPeak, "count", "total");
        else console.log("fds_peak not measured: the executor's /proc/<pid>/fd is readable by root only");
      } finally {
        sampler.stop();
        for (const client of clients) client.close();
        await gracefulShutdown(executor, `agent-scale ${agents} executor`, 60_000);
        deregisterPorts(ports);
        // Keep the disk bounded: the next count starts on a data dir of its own.
        rmSync(executorDataPath(dataPath), { recursive: true, force: true });
        rmSync(dataPath, { recursive: true, force: true });
      }
    });
  }
});
