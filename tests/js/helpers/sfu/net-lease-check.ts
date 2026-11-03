/**
 * Proves that a `leaseNet` impairment cannot outlive its run, on a real kernel:
 * the owner releases, is SIGKILLed with its whole process group (as a cancelled
 * CI job is), or hangs; two owners contend; the guard itself is SIGKILLed.
 * Each case checks the qdisc and whether a TCP connect to the impaired port
 * gets through.
 *
 * Runs only in a fresh network namespace with its own `lo` and lock file, never
 * against the host's: `tests/sfu-e2e.test.ts` starts it under `unshare -n`.
 * Exits non-zero on the first failed check.
 */

import { execSync, spawn, type ChildProcess } from "node:child_process";
import net from "node:net";
import { createInterface } from "node:readline";
import { fileURLToPath } from "node:url";
import { leaseNet } from "./net.js";

const SUDO = process.env.WT_NET_SUDO ?? "sudo -n";
const P1 = 41_001;
const P2 = 41_002;
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));
const show = (what: string) => execSync(`${SUDO} tc ${what} show dev lo`.trim()).toString().trim();
const impaired = () => show("qdisc").includes("qdisc prio ad4: root");

function check(ok: boolean, what: string): void {
  if (!ok) {
    console.log(`not ok - ${what}\n${show("qdisc")}`);
    process.exit(1);
  }
  console.log(`ok - ${what}`);
}

/** Whether a TCP connection to `port` on the loopback completes within 1.5 s. */
function reachable(port: number): Promise<boolean> {
  return new Promise((resolve) => {
    const socket = net.connect({ host: "127.0.0.1", port, timeout: 1_500 });
    socket.once("connect", () => { socket.destroy(); resolve(true); });
    socket.once("timeout", () => { socket.destroy(); resolve(false); });
    socket.once("error", () => resolve(false));
  });
}

/** Milliseconds until `done()`, or -1 after `timeoutMs`. */
async function until(done: () => boolean, timeoutMs: number): Promise<number> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    if (done()) return Date.now() - start;
    await sleep(50);
  }
  return -1;
}

/** An owner process in its own process group, as a CI step is; it prints `locked`, `impaired`, `released`. */
function owner(port: number, { ttlMs = 30_000, waitMs = 30_000 } = {}) {
  const proc: ChildProcess = spawn(process.execPath, [...process.execArgv, fileURLToPath(import.meta.url), "owner", String(port), String(ttlMs), String(waitMs)], {
    detached: true,
    stdio: ["pipe", "pipe", "inherit"],
  });
  const seen: string[] = [];
  const waiters: Array<() => void> = [];
  createInterface({ input: proc.stdout! }).on("line", (line) => {
    seen.push(line);
    waiters.splice(0).forEach((w) => w());
  });
  const exited = new Promise<number | null>((resolve) => proc.once("exit", (code) => resolve(code)));
  return {
    pid: proc.pid!,
    seen,
    exited,
    send: (line: string) => proc.stdin!.write(`${line}\n`),
    async wait(line: string, timeoutMs = 20_000): Promise<void> {
      const deadline = Date.now() + timeoutMs;
      while (!seen.includes(line)) {
        if (Date.now() > deadline) throw new Error(`owner on ${port} never printed "${line}" (saw ${seen.join(", ")})`);
        await new Promise<void>((r) => { waiters.push(r); setTimeout(r, 200); });
      }
    },
  };
}

async function runOwner([port, ttlMs, waitMs]: string[]): Promise<void> {
  const lease = await leaseNet({ ttlMs: Number(ttlMs), waitMs: Number(waitMs), onWait: () => console.log("waiting") })
    .catch(() => process.exit(4));
  console.log("locked");
  lease.impair([Number(port)], { lossPct: 100 });
  console.log("impaired");
  for await (const line of createInterface({ input: process.stdin })) {
    if (line === "release") {
      await lease.release();
      console.log("released");
      process.exit(0);
    }
  }
  // The driver has gone (it failed a check): go too, so no owner outlives the run.
  process.exit(1);
}

async function main(): Promise<void> {
  const links = execSync("ip -o link show").toString().trim().split("\n");
  if (links.length !== 1 || !links[0].includes(": lo:") || !process.env.WT_NET_LOCK) {
    console.log("refusing to run: needs a fresh network namespace (only `lo`) and WT_NET_LOCK");
    process.exit(2);
  }
  for (const port of [P1, P2]) await new Promise<void>((r) => net.createServer((s) => s.destroy()).listen(port, "127.0.0.1", r));
  check(!impaired() && (await reachable(P1)) && (await reachable(P2)), "fresh namespace: no impairment, both ports reachable");

  console.log("# normal end");
  const a = owner(P1);
  await a.wait("impaired");
  console.log(show("qdisc"));
  check(!(await reachable(P1)) && (await reachable(P2)), "impairment applied: the leased port is cut, another port is not");
  a.send("release");
  await a.wait("released");
  check(!impaired() && (await reachable(P1)), "release clears it");

  console.log("# owner SIGKILLed with its process group mid-outage");
  const b = owner(P1);
  await b.wait("impaired");
  process.kill(-b.pid, "SIGKILL");
  const killed = await until(() => !impaired(), 5_000);
  check(killed >= 0 && (await reachable(P1)), `cleared ${killed} ms after the SIGKILL`);

  console.log("# owner hangs (SIGSTOP), heartbeat window 2 s");
  const c = owner(P1, { ttlMs: 2_000 });
  await c.wait("impaired");
  process.kill(-c.pid, "SIGSTOP");
  const hung = await until(() => !impaired(), 10_000);
  check(hung >= 0 && (await reachable(P1)), `cleared ${hung} ms after the owner stopped`);
  process.kill(-c.pid, "SIGKILL");
  await c.exited;

  console.log("# two owners");
  const first = owner(P1);
  await first.wait("impaired");
  const second = owner(P2);
  await second.wait("waiting");
  await sleep(2_000);
  check(!second.seen.includes("locked") && !(await reachable(P1)) && (await reachable(P2)),
    "the second waits; the first's impairment is untouched");
  const impatient = owner(P2, { waitMs: 1_000 });
  check((await impatient.exited) === 4 && !(await reachable(P1)) && (await reachable(P2)),
    "an owner that cannot wait gives up and leaves the first's impairment alone");
  first.send("release");
  await second.wait("impaired");
  check((await reachable(P1)) && !(await reachable(P2)), "after the first releases, the second impairs only its own port");
  process.kill(-second.pid, "SIGKILL");
  check((await until(() => !impaired(), 5_000)) >= 0, "and the second's ends with it");

  console.log("# guard SIGKILLed: the impairment stays until the next lease sweeps it");
  const d = owner(P1);
  await d.wait("impaired");
  const guardPid = execSync(`pgrep -P ${d.pid} -f wt-net-guard`).toString().trim();
  process.kill(Number(guardPid), "SIGKILL");
  process.kill(-d.pid, "SIGKILL");
  await d.exited;
  check(impaired(), "a SIGKILLed guard leaves the impairment behind");
  const next = await leaseNet({ waitMs: 5_000 });
  check(!impaired() && (await reachable(P1)), "the next lease sweeps it before it starts");
  await next.release();
  check(!impaired(), "no impairment left");
  process.exit(0);
}

if (process.argv[2] === "owner") void runOwner(process.argv.slice(3));
else void main().catch((e) => { console.log(`not ok - ${(e as Error).message}`); process.exit(1); });
