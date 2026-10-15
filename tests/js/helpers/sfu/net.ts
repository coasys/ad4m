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
import { createInterface } from "node:readline";

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
  impair(ports: number[], opts: NetemOptions): void;
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
        for (const port of ports) {
          for (const field of ["sport", "dport"]) {
            tc(`filter add dev ${iface} parent ${HANDLE} protocol ip prio 1 u32 match ip ${field} ${port} 0xffff flowid ${HANDLE}4`);
          }
        }
      } catch (e) {
        clear();
        throw new Error(`could not impair ports ${ports.join(", ")} on ${iface}: ${(e as Error).message}`);
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

const PARTITION_CHAIN = "WT_PARTITION";

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

/**
 * Install (or reset) an iptables chain that drops loopback TCP
 * traffic to the given ports.  Used by F4 to simulate a cluster
 * partition without going through any admin RPC.
 */
export function dropTcpPorts(ports: number[]): boolean {
  if (!iptablesAvailable()) return false;
  // Wipe + recreate the chain so repeated calls are idempotent.
  try {
    execSync(`sudo -n iptables -D INPUT -j ${PARTITION_CHAIN}`, { stdio: "pipe" });
  } catch {
    /* not yet attached */
  }
  try {
    execSync(`sudo -n iptables -F ${PARTITION_CHAIN}`, { stdio: "pipe" });
  } catch {
    /* chain may not exist */
  }
  try {
    execSync(`sudo -n iptables -X ${PARTITION_CHAIN}`, { stdio: "pipe" });
  } catch {
    /* not present */
  }
  if (ports.length === 0) return true;
  try {
    execSync(`sudo -n iptables -N ${PARTITION_CHAIN}`, { stdio: "pipe" });
    for (const port of ports) {
      execSync(
        `sudo -n iptables -A ${PARTITION_CHAIN} -p tcp --dport ${port} -j DROP`,
        { stdio: "pipe" },
      );
    }
    execSync(`sudo -n iptables -I INPUT -j ${PARTITION_CHAIN}`, { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

/** Clear the partition chain installed by `dropTcpPorts`. */
export function clearPartition(): void {
  if (!iptablesAvailable()) return;
  try {
    execSync(`sudo -n iptables -D INPUT -j ${PARTITION_CHAIN}`, { stdio: "pipe" });
  } catch {
    /* not attached */
  }
  try {
    execSync(`sudo -n iptables -F ${PARTITION_CHAIN}`, { stdio: "pipe" });
  } catch {}
  try {
    execSync(`sudo -n iptables -X ${PARTITION_CHAIN}`, { stdio: "pipe" });
  } catch {}
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
