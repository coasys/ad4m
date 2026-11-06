/**
 * Processes the run starts, and what they hold: resident memory of a process tree and the
 * GPU memory its DRM clients (amdgpu, i915/xe) or CUDA contexts hold. Only processes the run
 * started are ever stopped.
 */

import { ChildProcess, execFile, execFileSync, spawn } from "node:child_process";
import { createWriteStream, existsSync, readdirSync, readFileSync } from "node:fs";
import { promisify } from "node:util";
import { getFreePort } from "../ports.js";

const execFileP = promisify(execFile);

/** Ports other services on shared machines own; the run never binds or probes them. */
export const RESERVED_PORTS = new Set(
  (process.env.AI_WT_RESERVED_PORTS ?? "12000,12500,12503,13000,3101,8180,5801,9091,9877,11434").split(",").map(Number),
);

export async function freePort(): Promise<number> {
  for (;;) {
    const p = await getFreePort();
    if (!RESERVED_PORTS.has(p)) return p;
  }
}

export function assertNotReserved(url: string): void {
  const port = Number(new URL(url).port);
  if (RESERVED_PORTS.has(port)) throw new Error(`${url} uses reserved port ${port} (AI_WT_RESERVED_PORTS)`);
}

/**
 * `fn` run at most once: every call, also a concurrent one, gets the same promise. The run's
 * cleanup goes through it, so a signal that arrives while `finally` cleans up waits for that
 * cleanup instead of starting a second one and exiting before the first has stopped everything.
 */
export function once<T>(fn: () => Promise<T>): () => Promise<T> {
  let p: Promise<T> | null = null;
  return () => (p ??= fn());
}

export interface Spawned {
  proc: ChildProcess;
  /** Resolves when the process has exited. */
  exited: Promise<void>;
  /**
   * Whether it still runs. A process a signal ended (an abort, a crash) has no exit code,
   * only a signal code, so `exitCode === null` alone takes it for running.
   */
  running(): boolean;
  stop(): Promise<void>;
}

/** Start `cmd`, its output appended to `logFile`. */
export function spawnLogged(cmd: string, args: string[], logFile: string, env: Record<string, string | undefined> = {}, cwd?: string): Spawned {
  const log = createWriteStream(logFile, { flags: "a" });
  log.write(`$ ${cmd} ${args.join(" ")}\n`);
  const proc = spawn(cmd, args, { env: { ...process.env, ...env }, stdio: ["ignore", "pipe", "pipe"], cwd });
  proc.stdout!.pipe(log, { end: false });
  proc.stderr!.pipe(log, { end: false });
  const exited = new Promise<void>((resolve) => proc.once("exit", () => resolve()));
  proc.once("error", (e) => log.write(`spawn error: ${e.message}\n`));
  const spawned: Spawned = {
    proc,
    exited,
    running: () => proc.exitCode === null && proc.signalCode === null,
    async stop() {
      live.delete(spawned);
      if (proc.exitCode !== null || proc.signalCode !== null) return;
      proc.kill("SIGTERM");
      const done = await Promise.race([exited.then(() => true), new Promise<boolean>((r) => setTimeout(() => r(false), 10_000))]);
      if (!done) {
        proc.kill("SIGKILL");
        await exited;
      }
    },
  };
  live.add(spawned);
  return spawned;
}

const live = new Set<Spawned>();

/** Stop every process `spawnLogged` started that is still running. */
export async function stopAllSpawned(): Promise<void> {
  await Promise.all([...live].map((s) => s.stop()));
}

export const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

/** Poll `url` until it answers with a status `ok` accepts; rejects at the deadline or when `alive()` turns false. */
export async function waitForHttp(
  url: string,
  opts: { timeoutMs: number; ok?: (status: number) => boolean; alive?: () => boolean; label?: string },
): Promise<void> {
  const ok = opts.ok ?? ((s) => s >= 200 && s < 300);
  const deadline = Date.now() + opts.timeoutMs;
  let last = "";
  while (Date.now() < deadline) {
    if (opts.alive && !opts.alive()) throw new Error(`${opts.label ?? url} exited before it answered (${last})`);
    try {
      const res = await fetch(url, { signal: AbortSignal.timeout(3000) });
      if (ok(res.status)) return;
      last = `HTTP ${res.status}`;
    } catch (e: any) {
      last = e?.cause?.code ?? e?.message ?? String(e);
    }
    await sleep(250);
  }
  throw new Error(`${opts.label ?? url} did not answer within ${opts.timeoutMs / 1000} s (${last})`);
}

/** `pid` and all its descendants (Linux /proc; elsewhere `pid` alone). */
export function processTree(pid: number): number[] {
  if (process.platform !== "linux") return [pid];
  const children = new Map<number, number[]>();
  for (const d of readdirSync("/proc")) {
    if (!/^\d+$/.test(d)) continue;
    try {
      const stat = readFileSync(`/proc/${d}/stat`, "utf8");
      const ppid = Number(stat.slice(stat.lastIndexOf(")") + 2).split(" ")[1]);
      children.set(ppid, [...(children.get(ppid) ?? []), Number(d)]);
    } catch {}
  }
  const out: number[] = [];
  const stack = [pid];
  while (stack.length) {
    const p = stack.pop()!;
    out.push(p);
    stack.push(...(children.get(p) ?? []));
  }
  return out;
}

function rssBytes(pid: number): number {
  if (process.platform === "linux") {
    try {
      const m = readFileSync(`/proc/${pid}/status`, "utf8").match(/^VmRSS:\s+(\d+)\s+kB/m);
      return m ? Number(m[1]) * 1024 : 0;
    } catch {
      return 0;
    }
  }
  try {
    return Number(execFileSync("ps", ["-o", "rss=", "-p", String(pid)], { encoding: "utf8" }).trim()) * 1024 || 0;
  } catch {
    return 0;
  }
}

/**
 * GPU memory one DRM client's fdinfo reports, over its vram, gtt, system and local regions.
 * From kernel 6.13 amdgpu prints `drm-resident-<region>` (drm_print_memory_stats) and then
 * the legacy `drm-memory-<region>` again, as an alias of it; so the legacy keys count only
 * where no `drm-resident-` key is printed (amdgpu up to 6.12).
 */
export function drmClientBytes(info: string): number {
  const sum = (prefix: "resident" | "memory"): number | null => {
    let bytes: number | null = null;
    for (const m of info.matchAll(new RegExp(`^drm-${prefix}-(?:vram|gtt|system|local):\\s+(\\d+)\\s*(KiB|MiB|kB)?`, "gm"))) {
      bytes = (bytes ?? 0) + Number(m[1]) * (m[2] === "MiB" ? 1024 * 1024 : m[2] ? 1024 : 1);
    }
    return bytes;
  };
  return sum("resident") ?? sum("memory") ?? 0;
}

/**
 * GPU memory the processes' DRM clients hold, from /proc/<pid>/fdinfo (`drmClientBytes`).
 * Each client id counts once.
 */
function drmBytes(pids: number[]): number {
  if (process.platform !== "linux") return 0;
  const clients = new Map<string, number>();
  for (const pid of pids) {
    let fds: string[] = [];
    try {
      fds = readdirSync(`/proc/${pid}/fdinfo`);
    } catch {
      continue;
    }
    for (const fd of fds) {
      let info = "";
      try {
        info = readFileSync(`/proc/${pid}/fdinfo/${fd}`, "utf8");
      } catch {
        continue;
      }
      const id = info.match(/^drm-client-id:\s+(\d+)/m)?.[1];
      if (!id) continue;
      const bytes = drmClientBytes(info);
      clients.set(`${pid}:${id}`, Math.max(clients.get(`${pid}:${id}`) ?? 0, bytes));
    }
  }
  // One client is often open through several fds of the same process; the pid:id key dedupes those.
  return [...clients.values()].reduce((a, b) => a + b, 0);
}

async function cudaBytes(pids: number[]): Promise<number> {
  try {
    const { stdout } = await execFileP("nvidia-smi", ["--query-compute-apps=pid,used_memory", "--format=csv,noheader,nounits"], { timeout: 3000 });
    const set = new Set(pids);
    return stdout
      .split("\n")
      .map((l) => l.split(",").map((s) => s.trim()))
      .filter(([p]) => set.has(Number(p)))
      .reduce((a, [, mib]) => a + Number(mib) * 1024 * 1024, 0);
  } catch {
    return 0;
  }
}

export interface MemoryPeak {
  peakRssBytes: number | null;
  peakGpuBytes: number | null;
}

/** Samples a set of processes every 250 ms until stopped, keeping the peaks. */
export class MemorySampler {
  #timer: NodeJS.Timeout | null = null;
  #rss = 0;
  #gpu = 0;
  #samples = 0;
  /** The sample being taken (a CUDA query can take a while), so `stop` can wait for it. */
  #active: Promise<void> | null = null;
  constructor(private roots: () => number[], private cuda = existsSync("/dev/nvidiactl")) {}

  async #sample(): Promise<void> {
    const pids = this.roots().flatMap((p) => processTree(p));
    if (!pids.length) return;
    this.#rss = Math.max(this.#rss, pids.reduce((a, p) => a + rssBytes(p), 0));
    const gpu = drmBytes(pids) + (this.cuda ? await cudaBytes(pids) : 0);
    this.#gpu = Math.max(this.#gpu, gpu);
    this.#samples++;
  }

  start(): this {
    const tick = () => {
      if (this.#active) return;
      this.#active = this.#sample()
        .catch(() => {})
        .finally(() => (this.#active = null));
    };
    tick();
    this.#timer = setInterval(tick, 250);
    return this;
  }

  /** Stops sampling once the sample in progress has finished, and answers the peaks. */
  async stop(): Promise<MemoryPeak> {
    if (this.#timer) clearInterval(this.#timer);
    this.#timer = null;
    if (this.#active) await this.#active;
    if (this.#samples === 0) return { peakRssBytes: null, peakGpuBytes: null };
    // 0 is a reading (nothing on the GPU) where GPU memory is readable; elsewhere there is none.
    const readable = process.platform === "linux" && (existsSync("/dev/dri") || this.cuda);
    return { peakRssBytes: this.#rss, peakGpuBytes: readable ? this.#gpu : null };
  }
}

/**
 * The docker CLI as this user can run it: `docker` directly, or through `sg docker -c`
 * when the session predates the user's docker group membership. Null without docker.
 */
let dockerPrefix: string[] | null | undefined;
export async function dockerCli(): Promise<string[] | null> {
  if (dockerPrefix !== undefined) return dockerPrefix;
  const tries: string[][] = [["docker"], ["sg", "docker", "-c", "docker"]];
  for (const t of tries) {
    try {
      await runDocker(t, ["version", "--format", "{{.Server.Version}}"]);
      return (dockerPrefix = t);
    } catch {}
  }
  return (dockerPrefix = null);
}

function shellQuote(s: string): string {
  return /^[\w@%+=:,./-]+$/.test(s) ? s : `'${s.replace(/'/g, `'\\''`)}'`;
}

async function runDocker(prefix: string[], args: string[], timeout = 120_000, withStderr = false): Promise<string> {
  const opts = { timeout, maxBuffer: 64 * 1024 * 1024 };
  const { stdout, stderr } =
    prefix[0] === "sg"
      ? await execFileP("sg", ["docker", "-c", ["docker", ...args].map(shellQuote).join(" ")], opts)
      : await execFileP(prefix[0], args, opts);
  return withStderr ? `${stdout}${stderr}` : stdout.trim();
}

export async function docker(args: string[], timeout?: number, withStderr = false): Promise<string> {
  const prefix = await dockerCli();
  if (!prefix) throw new Error("docker is not available");
  return runDocker(prefix, args, timeout, withStderr);
}
