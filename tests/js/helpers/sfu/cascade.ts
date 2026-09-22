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

import { existsSync, mkdirSync, rmSync, openSync, writeSync, closeSync } from "node:fs";
import { spawn, execSync, ChildProcess } from "node:child_process";

import { InstrumentedClient } from "./client.js";

const ADMIN_TOKEN = process.env.AD4M_ADMIN_TOKEN ?? "test123";

export interface CascadeNode {
  id: string;
  /** WS RPC port (`/api/v1/ws`). */
  port: number;
  /** Cascade gossip listener port (TCP). */
  gossipPort: number;
  dataPath: string;
  process: ChildProcess;
  /** Admin client to this executor — used for user provisioning + room mgmt. */
  client: InstrumentedClient;
  /** Cascade-cluster DID for this node. */
  did: string;
}

export interface CascadeClusterOptions {
  nodeCount: number;
  maxParticipantsPerNode: number;
  /** Absolute path to the ad4m-executor binary. */
  executorBin: string;
  /** WS RPC port base — node `i` listens on `wsBasePort + i`. */
  wsBasePort?: number;
  /** Cascade gossip port base — node `i` binds `gossipBasePort + i`. */
  gossipBasePort?: number;
}

export interface CascadeCluster {
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
  if (existsSync(dataPath)) {
    rmSync(dataPath, { recursive: true, force: true });
  }
  mkdirSync(dataPath, { recursive: true });
  execSync(`${bin} init --data-path ${dataPath}`, { stdio: "pipe" });
}

export async function startCluster(opts: CascadeClusterOptions): Promise<CascadeCluster> {
  const bin = opts.executorBin;
  const wsBasePort = opts.wsBasePort ?? 12000;
  const gossipBasePort = opts.gossipBasePort ?? 24000;
  const nodes: CascadeNode[] = [];

  // Defensive pre-cleanup: if a prior cascade run leaked, leftover
  // executors hold the WS port and waitForHealth ends up connecting
  // to the OLD executor whose data dir we just wiped — confusingly
  // surfacing as `user.create timed out`.  Force-kill any straggler
  // listening on the ports we're about to claim.
  for (let i = 0; i < opts.nodeCount; i++) {
    const wsPort = wsBasePort + i;
    const gossipPort = gossipBasePort + i;
    for (const port of [wsPort, gossipPort]) {
      try {
        execSync(`fuser -k -KILL ${port}/tcp 2>/dev/null || true`, { stdio: "pipe" });
      } catch {
        /* fuser exits non-zero if no process held the port — fine */
      }
    }
  }
  await sleep(500);

  // Pre-compute node identities so each one can be passed the full
  // peer list at spawn time.
  const plannedNodes = Array.from({ length: opts.nodeCount }, (_, i) => ({
    id: `node-${i}`,
    port: wsBasePort + i,
    gossipPort: gossipBasePort + i,
    dataPath: `/tmp/ad4m-cascade-node-${i}`,
    did: `did:windtunnel:cascade:node-${i}`,
  }));

  // Spawn every node first so the gossip mesh can form before any
  // node tries to do heavy init (agent.generate needs main-key
  // creation; user.login needs that main key).  Doing the
  // spawn → wait-health pass concurrently across nodes is fine since
  // each binds a different port.
  const spawnedProcs: ChildProcess[] = [];
  const spawnedClients: InstrumentedClient[] = [];
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
      "--hc-use-bootstrap", "false",
      "--hc-use-proxy", "false",
      "--enable-multi-user", "true",
      "--connect-holochain", "false",
      "--sfu-local-did", planned.did,
      "--sfu-max-participants-per-node", String(opts.maxParticipantsPerNode),
      "--sfu-cascade-listen", `127.0.0.1:${planned.gossipPort}`,
    ];
    if (peerEntries) {
      args.push("--sfu-cascade-peers", peerEntries);
    }

    // Capture executor stdout+stderr to per-node log files — when
    // agent.generate or user.create hangs on a cascade node, the
    // executor log is the only place we can see what state it's
    // wedged on.  Otherwise stdout/stderr just buffer into the
    // ChildProcess pipes and we never read them.
    const proc = spawn(bin, args, {
      stdio: ["ignore", "pipe", "pipe"],
      env: { ...process.env, RUST_LOG: "info" },
    });
    const logPath = `/tmp/ad4m-cascade-node-${i}.log`;
    try {
      const fd = openSync(logPath, "w");
      proc.stdout?.on("data", (d) => writeSync(fd, d));
      proc.stderr?.on("data", (d) => writeSync(fd, d));
      proc.on("close", () => {
        try {
          closeSync(fd);
        } catch {}
      });
    } catch (e) {
      console.warn(`[cascade] failed to open ${logPath}:`, e);
    }
    spawnedProcs.push(proc);
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
          `cascade: node ${planned.id} agent.generate failed: ${(gen as any).error}`,
        );
      }
    }),
  );

  for (let i = 0; i < plannedNodes.length; i++) {
    const planned = plannedNodes[i];
    nodes.push({
      id: planned.id,
      port: planned.port,
      gossipPort: planned.gossipPort,
      dataPath: planned.dataPath,
      process: spawnedProcs[i],
      client: spawnedClients[i],
      did: planned.did,
    });
  }

  return {
    nodes,
    async shutdown(): Promise<void> {
      for (const n of nodes) {
        try {
          await n.client.disconnect();
        } catch {}
        try {
          n.process.kill("SIGTERM");
        } catch {}
      }
      // Wait for SIGTERM to take effect — ad4m-executor's holochain
      // shutdown can take a few seconds.  Then force-kill anything
      // still alive so the next test run gets a clean port + lair-
      // keystore.  Without this, leftover executors hold ports 13000+
      // and the next startCluster() races against a partially-wiped
      // data dir, producing the very confusing "user.create timeout"
      // because the wind tunnel actually connects to the OLD executor.
      const settleMs = 3000;
      const settleStart = Date.now();
      while (Date.now() - settleStart < settleMs) {
        const anyAlive = nodes.some((n) => n.process.exitCode === null);
        if (!anyAlive) break;
        await sleep(200);
      }
      for (const n of nodes) {
        if (n.process.exitCode === null) {
          try {
            n.process.kill("SIGKILL");
          } catch {}
        }
      }
      await sleep(500);
      for (const n of nodes) {
        if (existsSync(n.dataPath)) {
          try {
            rmSync(n.dataPath, { recursive: true, force: true });
          } catch {}
        }
      }
    },
  };
}
