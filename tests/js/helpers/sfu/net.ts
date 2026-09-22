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

import { execSync } from "node:child_process";

const DEFAULT_IFACE = process.env.WT_NET_IFACE ?? "lo";

function tcAvailable(): boolean {
  if (process.platform !== "linux") return false;
  try {
    // Probe the actual command directly — /usr/sbin may not appear on
    // PATH, and sudoers may allow tc specifically without blanket
    // NOPASSWD (so `sudo -n true` would fail).
    execSync("sudo -n tc qdisc show dev lo", { stdio: "pipe" });
    return true;
  } catch {
    return false;
  }
}

/** Wipe any qdisc on the interface — idempotent, swallows errors. */
export function clearNet(iface: string = DEFAULT_IFACE): void {
  if (!tcAvailable()) return;
  try {
    execSync(`sudo -n tc qdisc del dev ${iface} root`, { stdio: "pipe" });
  } catch {
    /* nothing to delete */
  }
}

export interface NetemOptions {
  /** Random packet loss as a percentage [0,100]. */
  lossPct?: number;
  /** One-way delay in milliseconds. */
  delayMs?: number;
  /** Random jitter on top of the delay, in milliseconds. */
  jitterMs?: number;
}

/**
 * Apply a netem qdisc to the interface.  Replaces any existing root
 * qdisc.  Returns true on success.  Returns false when tc is
 * unavailable on this platform — callers should treat that as a skip.
 */
export function setNetem(opts: NetemOptions, iface: string = DEFAULT_IFACE): boolean {
  if (!tcAvailable()) return false;
  const parts: string[] = ["netem"];
  if (opts.lossPct != null) parts.push(`loss ${opts.lossPct}%`);
  if (opts.delayMs != null) {
    parts.push(`delay ${opts.delayMs}ms`);
    if (opts.jitterMs != null) parts[parts.length - 1] += ` ${opts.jitterMs}ms`;
  }
  if (parts.length === 1) return false;
  try {
    // Try replace first; fall back to add if no root qdisc exists yet.
    execSync(`sudo -n tc qdisc replace dev ${iface} root ${parts.join(" ")}`, {
      stdio: "pipe",
    });
    return true;
  } catch (e) {
    try {
      execSync(`sudo -n tc qdisc add dev ${iface} root ${parts.join(" ")}`, {
        stdio: "pipe",
      });
      return true;
    } catch {
      return false;
    }
  }
}

/**
 * Check whether tc + sudo are usable.  Scenarios use this to decide
 * whether to run end-to-end or report `skipped: tc unavailable`.
 */
export function netAvailable(): boolean {
  return tcAvailable();
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
