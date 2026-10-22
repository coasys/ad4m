/**
 * Multi-node cascade harness for T3 / T4 / M3 / F4 / S2 / S3.
 *
 * Each node is a real ad4m-executor process spawned with cascade
 * gossip enabled via CLI flags (`--sfu-local-did`,
 * `--sfu-cascade-listen`, `--sfu-cascade-peers`) — no admin RPCs
 * required.  The executor's built-in TCP gossip transport exchanges
 * announce / leave / pipe-offer signals between nodes and the
 * CascadeManager picks redirects based on the live cross-node view.
 *
 * Production deployments use the same `CascadeGossip` trait wired to
 * whatever signalling layer the host application owns (Holochain
 * neighbourhood signals for AD4M, libp2p for another runtime, etc.).
 * The wind tunnel TCP transport is one of several backends.
 */

import { existsSync, mkdirSync, mkdtempSync, rmSync, openSync, writeSync, closeSync } from "node:fs";
import { spawn, execSync, ChildProcess } from "node:child_process";
import { randomBytes } from "node:crypto";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { InstrumentedClient } from "./client.js";
import { ISOLATED_HOLOCHAIN_ARGS } from "./executor.js";
import { getFreePorts, registerPorts, deregisterPorts } from "../ports.js";

const ADMIN_TOKEN = process.env.AD4M_ADMIN_TOKEN ?? "test123";

export interface CascadeNode {
  id: string;
  /** WS RPC port (`/api/v1/ws`). */
  port: number;
  /** Cascade gossip listener port (TCP). */
  gossipPort: number;
  /** This node's own directory: holds `data/` and `executor.log`. */
  dir: string;
  /** The executor's `--app-data-path` (`<dir>/data`), removed at shutdown. */
  dataPath: string;
  /** Executor stdout + stderr (`<dir>/executor.log`), kept after shutdown. */
  logPath: string;
  process: ChildProcess;
  /** Admin client to this executor — used for user provisioning + room mgmt. */
  client: InstrumentedClient;
  /**
   * Cascade-cluster DID for this node:
   * `did:windtunnel:cascade:<runId>:node-<i>`.  The run id keeps two
   * clusters on one host from ever naming the same node.
   */
  did: string;
}

export interface CascadeClusterOptions {
  nodeCount: number;
  maxParticipantsPerNode: number;
  /** Absolute path to the ad4m-executor binary. */
  executorBin: string;
  /**
   * Directory the node directories are created in: the scenario's run
   * directory (`ScenarioContext.tmpDirBase`), which the harness keeps on
   * failure and removes on success.  Without it the cluster makes its own
   * `ad4m-sfu-test-cascade-*` directory under the OS temp dir and leaves
   * the logs in it.
   */
  workDir?: string;
}

export interface CascadeCluster {
  /** Short random id of this cluster; part of every node DID and directory name. */
  runId: string;
  nodes: CascadeNode[];
  shutdown(): Promise<void>;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

async function waitForHealth(port: number, timeoutMs = 60_000): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    try {
      const res = await fetch(`http://127.0.0.1:${port}/health`);
      if (res.ok) return;
    } catch {
      /* not ready */
    }
    await sleep(500);
  }
  throw new Error(`Executor on port ${port} did not become healthy in ${timeoutMs}ms`);
}

function initDataDir(bin: string, dataPath: string): void {
  mkdirSync(dataPath, { recursive: true });
  execSync(`"${bin}" init --data-path "${dataPath}"`, { stdio: "pipe" });
}

/**
 * SIGTERM the given executors, give them a few seconds (ad4m-executor's
 * holochain shutdown can take a while), then SIGKILL whatever is still
 * alive.  Only ever touches processes this cluster spawned: another
 * test job on the same host owns its own executors and ports.
 */
async function stopProcesses(procs: ChildProcess[]): Promise<void> {
  for (const p of procs) {
    try {
      p.kill("SIGTERM");
    } catch {}
  }
  const settleMs = 3000;
  const settleStart = Date.now();
  while (Date.now() - settleStart < settleMs) {
    const anyAlive = procs.some((p) => p.exitCode === null && p.signalCode === null);
    if (!anyAlive) break;
    await sleep(200);
  }
  for (const p of procs) {
    if (p.exitCode === null && p.signalCode === null) {
      try {
        p.kill("SIGKILL");
      } catch {}
    }
  }
  await sleep(500);
}

function removeDataDirs(dataPaths: string[]): void {
  for (const dataPath of dataPaths) {
    if (existsSync(dataPath)) {
      try {
        rmSync(dataPath, { recursive: true, force: true });
      } catch {}
    }
  }
}

export async function startCluster(opts: CascadeClusterOptions): Promise<CascadeCluster> {
  const bin = opts.executorBin;
  const nodes: CascadeNode[] = [];

  // Everything this cluster claims is its own: OS-assigned free ports
  // (registered so cleanup.js can find a leaked executor), fresh
  // directories, and DIDs carrying a per-run id.  Two runs on one host
  // (concurrent CI pipelines on a shared machine runner) then never
  // share a port, a data dir, a log file or a node DID, and nothing here
  // kills a process by port: shutdown only signals what it spawned.
  const runId = randomBytes(4).toString("hex");
  const workDir = opts.workDir ?? mkdtempSync(join(tmpdir(), "ad4m-sfu-test-cascade-"));
  const ports = await getFreePorts(opts.nodeCount * 2);
  registerPorts(ports);

  // Pre-compute node identities so each one can be passed the full
  // peer list at spawn time.
  const plannedNodes = Array.from({ length: opts.nodeCount }, (_, i) => {
    const dir = mkdtempSync(join(workDir, `cascade-${runId}-node-${i}-`));
    return {
      id: `node-${i}`,
      port: ports[i],
      gossipPort: ports[opts.nodeCount + i],
      dir,
      dataPath: join(dir, "data"),
      logPath: join(dir, "executor.log"),
      // The lexical order of DIDs within one cluster is still node-0 <
      // node-1 < … (same prefix), which T6's dialer tie-break relies on.
      did: `did:windtunnel:cascade:${runId}:node-${i}`,
    };
  });

  // Spawn every node first so the gossip mesh can form before any
  // node tries to do heavy init (agent.generate needs main-key
  // creation; user.login needs that main key).  Doing the
  // spawn → wait-health pass concurrently across nodes is fine since
  // each binds a different port.
  const spawnedProcs: ChildProcess[] = [];
  const spawnedClients: InstrumentedClient[] = [];
  try {
    for (let i = 0; i < plannedNodes.length; i++) {
      const planned = plannedNodes[i];
      initDataDir(bin, planned.dataPath);

      const peerEntries = plannedNodes
        .filter((_, j) => j !== i)
        .map((p) => `${p.did}=127.0.0.1:${p.gossipPort}`)
        .join(",");

      const args: string[] = [
        "run",
        "--app-data-path", planned.dataPath,
        "--port", String(planned.port),
        "--admin-credential", ADMIN_TOKEN,
        "--run-dapp-server", "false",
        ...ISOLATED_HOLOCHAIN_ARGS,
        "--enable-multi-user", "true",
        "--connect-holochain", "false",
        "--sfu-local-did", planned.did,
        "--sfu-max-participants-per-node", String(opts.maxParticipantsPerNode),
        "--sfu-cascade-listen", `127.0.0.1:${planned.gossipPort}`,
      ];
      if (peerEntries) {
        args.push("--sfu-cascade-peers", peerEntries);
      }

      // Capture executor stdout+stderr to the node's log file — when
      // agent.generate or user.create hangs on a cascade node, the
      // executor log is the only place we can see what state it's
      // wedged on.  The log lives in the node's directory under the
      // scenario's `ad4m-sfu-test-*` run dir, which CI collects on
      // failure.
      const proc = spawn(bin, args, {
        stdio: ["ignore", "pipe", "pipe"],
        env: { ...process.env, RUST_LOG: "info" },
      });
      spawnedProcs.push(proc);
      try {
        const fd = openSync(planned.logPath, "w");
        proc.stdout?.on("data", (d) => writeSync(fd, d));
        proc.stderr?.on("data", (d) => writeSync(fd, d));
        proc.on("close", () => {
          try {
            closeSync(fd);
          } catch {}
        });
      } catch (e) {
        console.warn(`[cascade] failed to open ${planned.logPath}:`, e);
      }
    }

    // Wait for every node's HTTP health endpoint in parallel.
    await Promise.all(plannedNodes.map((p) => waitForHealth(p.port)));

    // Connect admin clients in parallel.
    for (const planned of plannedNodes) {
      const c = new InstrumentedClient({ port: planned.port, adminToken: ADMIN_TOKEN });
      spawnedClients.push(c);
    }
    await Promise.all(spawnedClients.map((c) => c.connect()));

    // Run agent.generate on every node in PARALLEL — this is the
    // expensive bit (main-key creation + language load).  Running it
    // sequentially used to take 60+s × nodeCount on Josh; running in
    // parallel completes in roughly one node's worth of time because
    // the Holochain initialisation is mostly disk-bound, not contended.
    // Tolerate "already exists" so re-runs are idempotent.
    await Promise.all(
      spawnedClients.map(async (c, i) => {
        const planned = plannedNodes[i];
        const gen = await Promise.race([
          c.generateAgent("wind-tunnel-cascade"),
          sleep(180_000).then(() => ({ error: "agent.generate timeout (180s)" })),
        ]);
        if (gen && (gen as any).error && !/already/i.test((gen as any).error)) {
          throw new Error(
            `cascade: node ${planned.id} (${planned.did}) agent.generate failed: ${(gen as any).error} ` +
              `— log: ${planned.logPath}`,
          );
        }
      }),
    );
  } catch (e) {
    // A half-started cluster has no shutdown() to call: stop what we
    // spawned and give the ports back before surfacing the error.  The
    // logs stay for the failure report.
    for (const c of spawnedClients) {
      try {
        await c.disconnect();
      } catch {}
    }
    await stopProcesses(spawnedProcs);
    removeDataDirs(plannedNodes.map((p) => p.dataPath));
    deregisterPorts(ports);
    throw e;
  }

  for (let i = 0; i < plannedNodes.length; i++) {
    const planned = plannedNodes[i];
    nodes.push({
      id: planned.id,
      port: planned.port,
      gossipPort: planned.gossipPort,
      dir: planned.dir,
      dataPath: planned.dataPath,
      logPath: planned.logPath,
      process: spawnedProcs[i],
      client: spawnedClients[i],
      did: planned.did,
    });
  }

  return {
    runId,
    nodes,
    async shutdown(): Promise<void> {
      for (const n of nodes) {
        try {
          await n.client.disconnect();
        } catch {}
      }
      // Make sure every executor is gone before its data dir is removed
      // and its ports are handed back to the registry.
      await stopProcesses(nodes.map((n) => n.process));
      removeDataDirs(nodes.map((n) => n.dataPath));
      deregisterPorts(ports);
    },
  };
}
