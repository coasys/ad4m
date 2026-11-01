/**
 * Network shaping helpers for the wind tunnel.
 *
 * On Linux we drive `tc qdisc netem` against the loopback interface
 * (`lo`) to inject packet loss / delay / corruption.  These are
 * platform-specific; on macOS this module simply no-ops (the wind
 * tunnel's mesh + SFU scenarios then run unimpaired and the F-series
 * scenarios surface a `skipped: "tc not available"` outcome).
 *
 * Requires sudo without password — the call sites use `sudo -n` and
 * gracefully error out if it would prompt.
 */

import { execSync, spawn } from "node:child_process";
import { readdirSync, readFileSync, readlinkSync } from "node:fs";
import { createInterface } from "node:readline";
import { randomBytes } from "node:crypto";

const DEFAULT_IFACE = process.env.WT_NET_IFACE ?? "lo";
/** How tc gets root: `sudo -n` unless overridden (empty when already root, as in a test namespace). */
const SUDO = process.env.WT_NET_SUDO ?? "sudo -n";
/**
 * One impairment per host at a time. Not under /tmp: Marvin's CI runners
 * each get a private /tmp (systemd `PrivateTmp`), and they share one `lo`.
 */
const LOCK = process.env.WT_NET_LOCK ?? "/run/lock/ad4m-wt-net.lock";
/** Our root qdisc's handle: what we clear, so nothing else's qdisc is touched. */
const HANDLE = "ad4:";

const tc = (args: string) => execSync(`${SUDO} tc ${args}`.trim(), { stdio: "pipe" }).toString();

function tcAvailable(): boolean {
  if (process.platform !== "linux") return false;
  try {
    // Probe the actual command directly — /usr/sbin may not appear on
    // PATH, and sudoers may allow tc specifically without blanket
    // NOPASSWD (so `sudo -n true` would fail).
    tc("qdisc show dev lo");
    return true;
  } catch {
    return false;
  }
}

/**
 * Check whether tc + sudo are usable.  Scenarios use this to decide
 * whether to run end-to-end or report `skipped: tc unavailable`.
 */
export function netAvailable(): boolean {
  return tcAvailable();
}

export interface NetemOptions {
  /** Random packet loss as a percentage [0,100]. */
  lossPct?: number;
  /** One-way delay in milliseconds. */
  delayMs?: number;
  /** Random jitter on top of the delay, in milliseconds. */
  jitterMs?: number;
}

/** The right to impair `iface`; see `leaseNet`. */
export interface NetLease {
  /**
   * Apply netem only to traffic to or from `ports` (TCP and UDP). A prio
   * qdisc sends the matching packets to a fourth band carrying netem; the
   * priomap keeps everything else in the three plain bands, so other jobs on
   * the host are not touched. Replaces this lease's previous impairment.
   */
  impair(ports: (number | SocketPort)[], opts: NetemOptions): void;
  /** Add `ports` to the current impairment, without lifting it for the others meanwhile. */
  extend(ports: (number | SocketPort)[]): void;
  /** Remove the impairment; the lease stays held. */
  clear(): void;
  /** Remove the impairment and let the next run have the interface. */
  release(): Promise<void>;
}

/**
 * The guard, one detached process per lease. It takes the host lock, sweeps
 * any impairment of ours a dead guard left, and holds the lock while the owner
 * lives. It removes the impairment and exits when the owner releases, when the
 * owner's end of stdin closes (the owner exited or was killed, SIGKILL
 * included), when the owner's heartbeat stops for `ttl` seconds (hung or
 * stopped), or on any catchable signal. It runs in its own session (setsid),
 * so a kill of the owner's process group or session (a cancelled CI job) does
 * not reach it.
 */
const GUARD = `
lock=$1 iface=$2 ttl=$3 wait=$4 handle=$5
ours() { $WT_NET_SUDO tc qdisc show dev "$iface" 2>/dev/null | grep -q "^qdisc prio $handle root"; }
clear() { if ours; then $WT_NET_SUDO tc qdisc del dev "$iface" root >/dev/null 2>&1; fi; }
exec 9>>"$lock" || exit 3
if ! flock -n 9; then echo waiting; flock -w "$wait" 9 || { echo busy; exit 4; }; fi
trap 'clear; exit 0' HUP INT TERM PIPE
clear
echo locked
while IFS= read -r -t "$ttl" line; do [ "$line" = release ] && break; done
clear
`;

/**
 * Take the host's network-impairment lease for `iface`: waits up to `waitMs`
 * while another run holds it. The impairment cannot outlive the lease, and the
 * lease cannot outlive this process (see GUARD), however the run ends.
 */
export async function leaseNet(
  { iface = DEFAULT_IFACE, waitMs = 600_000, ttlMs = 30_000, onWait }:
    { iface?: string; waitMs?: number; ttlMs?: number; onWait?: () => void } = {},
): Promise<NetLease> {
  const guard = spawn("bash", ["-c", GUARD, "wt-net-guard", LOCK, iface, String(ttlMs / 1000), String(waitMs / 1000), HANDLE], {
    detached: true,
    stdio: ["pipe", "pipe", "ignore"],
    env: { ...process.env, WT_NET_SUDO: SUDO },
  });
  let ended = false;
  const exited = new Promise<void>((resolve) => guard.once("close", () => { ended = true; resolve(); }));
  guard.stdin.on("error", () => { /* the guard has gone: `ended` says so */ });
  await new Promise<void>((resolve, reject) => {
    const lines = createInterface({ input: guard.stdout });
    lines.on("line", (line) => {
      if (line === "waiting") onWait?.();
      if (line === "locked") resolve();
    });
    void exited.then(() => reject(new Error(`could not take the network lease on ${iface} (${LOCK}): `
      + `another run held it for ${waitMs / 1000} s, or the guard could not start`)));
  });
  const heartbeat = setInterval(() => guard.stdin.write("alive\n"), ttlMs / 3);
  void exited.then(() => clearInterval(heartbeat));
  const held = () => {
    if (ended) throw new Error(`the network lease on ${iface} has ended`);
  };
  /** Send traffic to or from `ports` through the netem band; a `SocketPort` matches its protocol too. */
  const filter = (ports: (number | SocketPort)[]) => {
    for (const entry of ports) {
      const { port, protocol } = typeof entry === "number" ? { port: entry, protocol: undefined } : entry;
      const match = protocol ? `match ip protocol ${protocol === "tcp" ? 6 : 17} 0xff ` : "";
      for (const field of ["sport", "dport"]) {
        tc(`filter add dev ${iface} parent ${HANDLE} protocol ip prio 1 u32 ${match}match ip ${field} ${port} 0xffff flowid ${HANDLE}4`);
      }
    }
  };
  const clear = () => {
    try {
      if (tc(`qdisc show dev ${iface}`).includes(`qdisc prio ${HANDLE} root`)) tc(`qdisc del dev ${iface} root`);
    } catch {
      /* nothing to delete */
    }
  };
  return {
    impair(ports, opts) {
      held();
      const netem: string[] = [];
      if (opts.lossPct != null) netem.push(`loss ${opts.lossPct}%`);
      if (opts.delayMs != null) netem.push(`delay ${opts.delayMs}ms${opts.jitterMs != null ? ` ${opts.jitterMs}ms` : ""}`);
      if (ports.length === 0 || netem.length === 0) throw new Error("nothing to impair");
      clear();
      try {
        tc(`qdisc add dev ${iface} root handle ${HANDLE} prio bands 4 priomap 1 2 2 2 1 2 0 0 1 1 1 1 1 1 1 1`);
        tc(`qdisc add dev ${iface} parent ${HANDLE}4 handle 40: netem ${netem.join(" ")}`);
        filter(ports);
      } catch (e) {
        clear();
        throw new Error(`could not impair ports ${describePorts(ports)} on ${iface}: ${(e as Error).message}`);
      }
    },
    extend(ports) {
      held();
      try {
        filter(ports);
      } catch (e) {
        clear();
        throw new Error(`could not add ports ${describePorts(ports)} to the impairment on ${iface}: ${(e as Error).message}`);
      }
    },
    clear,
    async release() {
      if (!ended) {
        guard.stdin.write("release\n");
        guard.stdin.end();
      }
      await exited;
    },
  };
}

/** `ports` for an error message: a number as it is, a socket as `protocol/port`. */
function describePorts(ports: (number | SocketPort)[]): string {
  return ports.map((p) => (typeof p === "number" ? String(p) : `${p.protocol}/${p.port}`)).join(", ");
}

/**
 * The local ports of every TCP and UDP socket process `pid` holds open: its
 * connections to the executor and the sockets its WebRTC stack gathered on.
 * Impairing these cuts that one process off, as its own network failing does,
 * and leaves every other process on the loopback alone. Linux only (/proc).
 */
export function socketPorts(pid: number): SocketPort[] {
  const inodes = new Set<string>();
  for (const fd of readdirSync(`/proc/${pid}/fd`)) {
    try {
      const target = readlinkSync(`/proc/${pid}/fd/${fd}`);
      if (target.startsWith("socket:[")) inodes.add(target.slice(8, -1));
    } catch {
      /* closed meanwhile */
    }
  }
  const ports = new Map<string, SocketPort>();
  for (const table of ["tcp", "tcp6", "udp", "udp6"]) {
    let lines: string[];
    try {
      lines = readFileSync(`/proc/${pid}/net/${table}`, "utf8").trim().split("\n").slice(1);
    } catch {
      continue;
    }
    const protocol = table.startsWith("tcp") ? "tcp" : "udp";
    for (const line of lines) {
      // sl local_address rem_address st tx:rx tr:when retrnsmt uid timeout inode
      const fields = line.trim().split(/\s+/);
      if (!inodes.has(fields[9])) continue;
      const port = parseInt(fields[1].split(":").pop()!, 16);
      if (port > 0) ports.set(`${protocol}/${port}`, { port, protocol });
    }
  }
  return [...ports.values()].sort((a, b) => a.port - b.port);
}

/**
 * One local socket's port and protocol. TCP and UDP number their ports
 * separately, so an impairment given the protocol cuts this socket's traffic
 * and not another process's socket that happens to have the same number.
 */
export interface SocketPort {
  port: number;
  protocol: "tcp" | "udp";
}

function iptablesAvailable(): boolean {
  if (process.platform !== "linux") return false;
  try {
    // Probe directly — /usr/sbin may not appear on PATH.
    execSync("sudo -n iptables -L -n", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

export function iptablesPartitionAvailable(): boolean {
  return iptablesAvailable();
}

/** A partition installed by {@link dropTcpPorts}. */
export interface TcpPartition {
  /** The iptables chain holding this partition's rules. */
  chain: string;
  ports: number[];
  /** Remove the chain and its rules; safe to call more than once. */
  clear(): void;
}

/**
 * Install an iptables chain that drops loopback TCP traffic to the
 * given ports.  Used by F4 to simulate a cluster partition without
 * going through any admin RPC.
 *
 * Each call makes its own chain (`WT_P_<pid>_<random>`) holding one rule
 * per port, so a concurrent run on the same host, with its own ports and
 * its own chain, is neither partitioned nor un-partitioned by this one.
 * The returned handle's `clear()` unhooks and deletes exactly that
 * chain; it also runs on process exit, SIGINT and SIGTERM. A SIGKILL
 * (a cancelled CI job, a step timeout) runs none of those, so each call
 * first deletes the chains of processes that are no longer alive: a DROP
 * left on a port the OS may hand to the next run lasts until the next F4,
 * not until reboot.  Returns null when iptables is unavailable or
 * installing failed (any part already installed is removed).
 */
export function dropTcpPorts(ports: number[]): TcpPartition | null {
  if (!iptablesAvailable()) return null;
  removeOrphanedPartitions();
  const chain = `WT_P_${process.pid}_${randomBytes(4).toString("hex")}`;
  let cleared = false;
  const onSignal = (signal: NodeJS.Signals) => {
    clear();
    // Re-raise when nobody else handles it, so the signal still ends the process.
    if (process.listenerCount(signal) === 0) process.kill(process.pid, signal);
  };
  const clear = () => {
    if (cleared) return;
    cleared = true;
    process.off("exit", clear);
    process.off("SIGINT", onSignal);
    process.off("SIGTERM", onSignal);
    removeChain(chain);
  };
  process.once("exit", clear);
  process.once("SIGINT", onSignal);
  process.once("SIGTERM", onSignal);
  try {
    execSync(`sudo -n iptables -N ${chain}`, { stdio: "pipe" });
    for (const port of ports) {
      execSync(
        `sudo -n iptables -A ${chain} -i lo -p tcp --dport ${port} -j DROP`,
        { stdio: "pipe" },
      );
    }
    execSync(`sudo -n iptables -I INPUT -j ${chain}`, { stdio: "pipe" });
    return { chain, ports: [...ports], clear };
  } catch {
    clear();
    return null;
  }
}

/** Unhook and delete one partition chain; each step tolerates a part already gone. */
function removeChain(chain: string): void {
  for (const command of [`-D INPUT -j ${chain}`, `-F ${chain}`, `-X ${chain}`]) {
    try {
      execSync(`sudo -n iptables ${command}`, { stdio: "pipe" });
    } catch {
      /* not attached, or already gone */
    }
  }
}

/** Delete every `WT_P_<pid>_*` chain whose process has exited. */
function removeOrphanedPartitions(): void {
  let rules: string;
  try {
    rules = execSync("sudo -n iptables -S", { stdio: "pipe" }).toString();
  } catch {
    return;
  }
  for (const [, chain, pid] of rules.matchAll(/^-N (WT_P_(\d+)_[0-9a-f]+)$/gm)) {
    if (!processAlive(Number(pid))) removeChain(chain);
  }
}

function processAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    // EPERM: alive, owned by another user.
    return (error as NodeJS.ErrnoException).code === "EPERM";
  }
}

/** Whether `sudo -n iptables` can change the nat table. */
export function natAvailable(): boolean {
  try {
    execSync("sudo -n iptables -t nat -L OUTPUT -n", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

/** The two rules of {@link reachOnlyThrough}: `iptables -t <table> <action> <rule>`. */
function onlyThroughRules(published: string, local: string, port: number): [string, string][] {
  const tag = `-m comment --comment ad4m-e2e-only-through-${port}`;
  return [
    ["nat", `OUTPUT -p udp -d ${published} --dport ${port} ${tag} -j DNAT --to-destination ${local}:${port}`],
    ["filter", `OUTPUT -p udp -d ${local} --dport ${port} -m conntrack ! --ctstate DNAT ${tag} -j DROP`],
  ];
}

/**
 * Make UDP `port` on `local` reachable only as `published:port`: traffic to
 * `published:port` is translated to `local:port`, and traffic sent to
 * `local:port` directly is dropped. Loopback, this one port only, so other
 * processes on the host are not touched. Returns false when the rules cannot
 * be installed (none are left behind).
 */
export function reachOnlyThrough(published: string, local: string, port: number): boolean {
  clearReachOnlyThrough(published, local, port);
  try {
    for (const [table, rule] of onlyThroughRules(published, local, port)) {
      execSync(`sudo -n iptables -t ${table} -A ${rule}`, { stdio: "pipe" });
    }
    return true;
  } catch {
    clearReachOnlyThrough(published, local, port);
    return false;
  }
}

/** Remove what {@link reachOnlyThrough} installed. */
export function clearReachOnlyThrough(published: string, local: string, port: number): void {
  for (const [table, rule] of onlyThroughRules(published, local, port)) {
    try {
      execSync(`sudo -n iptables -t ${table} -D ${rule}`, { stdio: "pipe" });
    } catch {
      /* not installed */
    }
  }
}
