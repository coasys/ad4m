/**
 * Perspective & Neighbourhood scaling profiler for AD4M.
 *
 * Starts an AD4M executor with full bootstrap languages, then:
 *   Phase 1: Creates local perspectives (0, 1, 3, 10) — baseline
 *   Phase 2: Creates real neighbourhoods with perspective-diff-sync (0, 1, 3, 10) — full stack
 *
 * Usage:
 *   AD4M_EXECUTOR_PATH=/path/to/ad4m-executor npx tsx tests/js/profile-perspectives.ts
 */

import { ChildProcess, exec, execSync } from "node:child_process";
import { rmSync, existsSync, readFileSync } from "node:fs";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions/index.js";
import { ApolloClient, InMemoryCache } from "@apollo/client/core/index.js";
import Websocket from "ws";
import { createClient } from "graphql-ws";
import path from "path";
import { fileURLToPath } from "url";
import { dirname } from "path";
import { Ad4mClient, Link, LinkExpression, Perspective, ExpressionProof } from "@coasys/ad4m";
import { v4 as uuidv4 } from "uuid";
import { runHcLocalServices } from "./utils/utils.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const EXECUTOR_PATH =
  process.env.AD4M_EXECUTOR_PATH ||
  path.resolve(__dirname, "..", "..", "target", "release", "ad4m-executor");

const BOOTSTRAP_SEED_PATH = path.resolve(__dirname, "bootstrapSeed.json");
const DIFF_SYNC_HASH_PATH = path.resolve(__dirname, "scripts", "perspective-diff-sync-hash");
const DATA_PATH = "/tmp/ad4m-profile-test";
const GQL_PORT = 14000;
const HC_ADMIN_PORT = 14001;
const HC_APP_PORT = 14002;

// ── Helpers ──

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

function apolloClient(port: number, token?: string): ApolloClient<any> {
  const wsLink = new GraphQLWsLink(
    createClient({
      url: `ws://127.0.0.1:${port}/graphql`,
      webSocketImpl: Websocket,
      connectionParams: () => ({
        headers: { authorization: token || "" },
      }),
    })
  );
  return new ApolloClient({
    link: wsLink,
    cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
    defaultOptions: {
      watchQuery: { fetchPolicy: "no-cache" },
      query: { fetchPolicy: "no-cache" },
      mutate: { fetchPolicy: "no-cache" },
    },
  });
}

async function startExecutor(
  proxyUrl: string,
  bootstrapUrl: string,
  relayUrl?: string
): Promise<ChildProcess> {
  if (!existsSync(EXECUTOR_PATH)) {
    throw new Error(
      `ad4m-executor not found at ${EXECUTOR_PATH}.\n` +
        `Set AD4M_EXECUTOR_PATH or build: cargo build --release -p ad4m-executor`
    );
  }
  if (!existsSync(BOOTSTRAP_SEED_PATH)) {
    throw new Error(
      `bootstrapSeed.json not found at ${BOOTSTRAP_SEED_PATH}.\n` +
        `Run: pnpm run prepare-test`
    );
  }

  rmSync(DATA_PATH, { recursive: true, force: true });
  execSync(
    `${EXECUTOR_PATH} init --data-path ${DATA_PATH} --network-bootstrap-seed ${BOOTSTRAP_SEED_PATH}`,
    { cwd: process.cwd() }
  );

  const relayArg = relayUrl ? `--hc-relay-url ${relayUrl}` : "";

  const proc = exec(
    `${EXECUTOR_PATH} run ` +
      `--app-data-path ${DATA_PATH} ` +
      `--gql-port ${GQL_PORT} ` +
      `--hc-admin-port ${HC_ADMIN_PORT} ` +
      `--hc-app-port ${HC_APP_PORT} ` +
      `--hc-proxy-url ${proxyUrl} ` +
      `--hc-bootstrap-url ${bootstrapUrl} ` +
      `${relayArg} ` +
      `--hc-use-bootstrap true ` +
      `--hc-use-proxy true ` +
      `--hc-use-local-proxy true ` +
      `--hc-use-mdns true ` +
      `--language-language-only false ` +
      `--run-dapp-server false ` +
      `--admin-credential profile-token`,
    { maxBuffer: 100 * 1024 * 1024 }
  );

  const ready = new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(
      () => reject(new Error("Executor startup timeout (180s)")),
      180000
    );
    const check = (data: Buffer) => {
      if (
        data.toString().includes(`listening on http://127.0.0.1:${GQL_PORT}`)
      ) {
        clearTimeout(timeout);
        resolve();
      }
    };
    proc.stdout!.on("data", check);
    proc.stderr!.on("data", check);
  });

  // Log executor output for debugging
  proc.stdout!.on("data", (data: Buffer) => {
    const s = data.toString();
    if (s.includes("ERROR") || s.includes("WARN") || s.includes("listening")) {
      process.stderr.write(`[executor] ${s}`);
    }
  });
  proc.stderr!.on("data", (data: Buffer) => {
    process.stderr.write(`[executor-err] ${data.toString()}`);
  });

  await ready;
  console.log("Executor ready, waiting for languages to settle...");
  await sleep(10000); // Give bootstrap languages time to install
  return proc;
}

async function killProcess(proc: ChildProcess | null): Promise<void> {
  if (!proc) return;
  proc.stdout?.destroy();
  proc.stderr?.destroy();
  proc.stdin?.destroy();
  proc.kill("SIGTERM");
  await sleep(2000);
  if (!proc.killed) proc.kill("SIGKILL");
  if (proc.pid) {
    try { process.kill(-proc.pid, "SIGKILL"); } catch {}
    try { process.kill(proc.pid, "SIGKILL"); } catch {}
  }
  try { execSync("pkill -9 -f ad4m-executor 2>/dev/null || true", { stdio: "ignore" }); } catch {}
  try { execSync("pkill -9 -f holochain 2>/dev/null || true", { stdio: "ignore" }); } catch {}
  try { execSync("pkill -9 -f lair-keystore 2>/dev/null || true", { stdio: "ignore" }); } catch {}
  proc.unref();
  await sleep(500);
}

// ── Process tree measurement ──

function getAllDescendantPids(pid: number): string[] {
  const result = [pid.toString()];
  try {
    const children = execSync(`pgrep -P ${pid} 2>/dev/null || true`, {
      encoding: "utf-8",
    }).trim();
    if (children) {
      for (const childPid of children.split("\n").filter(Boolean)) {
        result.push(...getAllDescendantPids(parseInt(childPid, 10)));
      }
    }
  } catch {}
  return result;
}

interface Snapshot {
  label: string;
  count: number;
  rssKB: number;
  vszKB: number;
  cpuPercent: string;
  childProcesses: number;
}

interface SmapsRegion {
  name: string;
  rssKB: number;
  pssKB: number;
  sharedKB: number;
  privateKB: number;
}

function parseSmaps(pid: number): SmapsRegion[] {
  try {
    const raw = execSync(`cat /proc/${pid}/smaps 2>/dev/null || true`, {
      encoding: "utf-8",
      maxBuffer: 50 * 1024 * 1024,
    });
    if (!raw.trim()) return [];

    const regions: SmapsRegion[] = [];
    let current: SmapsRegion | null = null;

    for (const line of raw.split("\n")) {
      // Header line: address perms offset dev inode pathname
      const headerMatch = line.match(
        /^[0-9a-f]+-[0-9a-f]+\s+\S+\s+\S+\s+\S+\s+\d+\s*(.*)/
      );
      if (headerMatch) {
        if (current) regions.push(current);
        current = {
          name: headerMatch[1].trim() || "[anon]",
          rssKB: 0,
          pssKB: 0,
          sharedKB: 0,
          privateKB: 0,
        };
        continue;
      }
      if (!current) continue;
      const kv = line.match(/^(\w[\w_]*?):\s+(\d+)\s+kB/);
      if (kv) {
        const val = parseInt(kv[2], 10);
        switch (kv[1]) {
          case "Rss":
            current.rssKB = val;
            break;
          case "Pss":
            current.pssKB = val;
            break;
          case "Shared_Clean":
          case "Shared_Dirty":
            current.sharedKB += val;
            break;
          case "Private_Clean":
          case "Private_Dirty":
            current.privateKB += val;
            break;
        }
      }
    }
    if (current) regions.push(current);
    return regions;
  } catch {
    return [];
  }
}

function printSmapsSummary(pid: number): void {
  const regions = parseSmaps(pid);
  if (regions.length === 0) {
    console.log("  smaps: not available (Linux /proc required)");
    return;
  }

  // Aggregate by mapped file/category
  const buckets = new Map<string, { rssKB: number; pssKB: number; privateKB: number; count: number }>();

  for (const r of regions) {
    if (r.rssKB === 0) continue;

    let category: string;
    const name = r.name;

    if (name.includes("libholochain") || name.includes("holochain"))
      category = "holochain";
    else if (name.includes("libv8") || name.includes("v8_") || name.includes("libdeno") || name.includes("deno"))
      category = "v8/deno";
    else if (name.includes("surreal") || name.includes("rocksdb") || name.includes("librocksdb"))
      category = "surrealdb/rocksdb";
    else if (name.includes("lair") || name.includes("sodiumoxide") || name.includes("libsodium"))
      category = "lair/crypto";
    else if (name.includes("libssl") || name.includes("libcrypto"))
      category = "tls/openssl";
    else if (name.includes("libc-") || name.includes("libc.so") || name.includes("libm.so") || name.includes("libpthread") || name.includes("ld-linux") || name.includes("libdl") || name.includes("librt"))
      category = "libc/system";
    else if (name.includes("libwasmtime") || name.includes("wasmer") || name.includes("wasm"))
      category = "wasm-runtime";
    else if (name.includes("ad4m") || name.includes("executor"))
      category = "ad4m-executor";
    else if (name === "[heap]")
      category = "[heap]";
    else if (name === "[stack]" || name.startsWith("[stack:"))
      category = "[stack]";
    else if (name === "[anon]" || name === "")
      category = "[anonymous]";
    else if (name.startsWith("/usr/lib") || name.startsWith("/lib"))
      category = "system-libs";
    else
      category = "other";

    const b = buckets.get(category) || { rssKB: 0, pssKB: 0, privateKB: 0, count: 0 };
    b.rssKB += r.rssKB;
    b.pssKB += r.pssKB;
    b.privateKB += r.privateKB;
    b.count++;
    buckets.set(category, b);
  }

  // Sort by RSS descending
  const sorted = [...buckets.entries()].sort((a, b) => b[1].rssKB - a[1].rssKB);
  const totalRSS = sorted.reduce((s, [, v]) => s + v.rssKB, 0);

  console.log("\n  === MEMORY MAP (smaps) ===");
  console.log(
    "  " +
      "Category".padEnd(22) +
      "| RSS (MB)  | PSS (MB)  | Private MB | Regions"
  );
  console.log(
    "  " +
      "----------------------|-----------|-----------|------------|--------"
  );
  for (const [cat, v] of sorted) {
    const pct = ((v.rssKB / totalRSS) * 100).toFixed(0);
    console.log(
      "  " +
        cat.padEnd(22) +
        `| ${(v.rssKB / 1024).toFixed(1).padStart(7)}   ` +
        `| ${(v.pssKB / 1024).toFixed(1).padStart(7)}   ` +
        `| ${(v.privateKB / 1024).toFixed(1).padStart(8)}   ` +
        `| ${String(v.count).padStart(5)}  (${pct}%)`
    );
  }
  console.log(
    "  " +
      "TOTAL".padEnd(22) +
      `| ${(totalRSS / 1024).toFixed(1).padStart(7)}   |           |            |`
  );
}

function measureProcessTree(pid: number): {
  rssKB: number;
  vszKB: number;
  cpuPercent: string;
  childCount: number;
} {
  try {
    const pids = [...new Set(getAllDescendantPids(pid))];

    const raw = execSync(
      `ps -o pid=,rss=,vsz=,%cpu=,comm= -p ${pids.join(",")} 2>/dev/null || true`,
      { encoding: "utf-8" }
    ).trim();

    const lines = raw.split("\n").filter(Boolean);
    let totalRSS = 0;
    let totalVSZ = 0;
    let totalCPU = 0;
    let count = 0;
    const details: string[] = [];

    for (const line of lines) {
      const parts = line.trim().split(/\s+/);
      if (parts.length >= 5) {
        const rss = parseInt(parts[1], 10) || 0;
        const vsz = parseInt(parts[2], 10) || 0;
        const cpu = parseFloat(parts[3]) || 0;
        const comm = parts.slice(4).join(" ");
        totalRSS += rss;
        totalVSZ += vsz;
        totalCPU += cpu;
        count++;
        details.push(
          `    PID ${parts[0]}: ${(rss / 1024).toFixed(1)}MB RSS, ${cpu}% CPU — ${comm}`
        );
      }
    }

    if (details.length > 0) {
      console.log("  Process breakdown:");
      details.forEach((d) => console.log(d));
    }

    return {
      rssKB: totalRSS,
      vszKB: totalVSZ,
      cpuPercent: totalCPU.toFixed(1),
      childCount: Math.max(0, count - 1),
    };
  } catch {
    return { rssKB: 0, vszKB: 0, cpuPercent: "0", childCount: 0 };
  }
}

async function takeSnapshot(
  label: string,
  pid: number,
  count: number
): Promise<Snapshot> {
  await sleep(5000); // settle time

  const tree = measureProcessTree(pid);

  const snapshot: Snapshot = {
    label,
    count,
    rssKB: tree.rssKB,
    vszKB: tree.vszKB,
    cpuPercent: tree.cpuPercent,
    childProcesses: tree.childCount,
  };

  console.log(`\n=== ${label} ===`);
  console.log(`  Count: ${count}`);
  console.log(`  Total RSS: ${(snapshot.rssKB / 1024).toFixed(1)} MB`);
  console.log(`  Total VSZ: ${(snapshot.vszKB / 1024).toFixed(1)} MB`);
  console.log(`  Total CPU: ${snapshot.cpuPercent}%`);
  console.log(`  Child processes: ${snapshot.childProcesses}`);

  return snapshot;
}

function printTable(title: string, snapshots: Snapshot[], label: string) {
  console.log(`\n=== ${title} ===\n`);
  console.log(
    `${label.padEnd(14)}| RSS (MB)  | VSZ (MB)  | CPU %  | Processes`
  );
  console.log(
    "--------------|-----------|-----------|--------|----------"
  );
  for (const s of snapshots) {
    console.log(
      `${String(s.count).padStart(13)} | ${(s.rssKB / 1024)
        .toFixed(1)
        .padStart(9)} | ${(s.vszKB / 1024)
        .toFixed(1)
        .padStart(9)} | ${s.cpuPercent.padStart(6)} | ${String(
        s.childProcesses
      ).padStart(9)}`
    );
  }

  console.log(`\n=== DELTA PER ${label.trim().toUpperCase()} ===\n`);
  for (let i = 1; i < snapshots.length; i++) {
    const prev = snapshots[i - 1];
    const curr = snapshots[i];
    const added = curr.count - prev.count;
    if (added === 0) continue;
    const rssDelta = curr.rssKB - prev.rssKB;
    const procDelta = curr.childProcesses - prev.childProcesses;
    console.log(
      `${prev.count} → ${curr.count}: ` +
        `+${(rssDelta / 1024).toFixed(1)} MB RSS ` +
        `(${(rssDelta / 1024 / added).toFixed(1)} MB/each), ` +
        `+${procDelta} processes`
    );
  }
}

// ── Main ──

async function main() {
  console.log("=== AD4M Perspective & Neighbourhood Scaling Profiler ===\n");
  console.log(`Executor: ${EXECUTOR_PATH}`);
  console.log(`Bootstrap: ${BOOTSTRAP_SEED_PATH}`);
  console.log(`Data path: ${DATA_PATH}`);

  // Check for diff-sync hash
  let diffSyncHash: string | null = null;
  if (existsSync(DIFF_SYNC_HASH_PATH)) {
    diffSyncHash = readFileSync(DIFF_SYNC_HASH_PATH, "utf-8").trim();
    console.log(`Diff-sync hash: ${diffSyncHash}`);
  } else {
    console.log("WARNING: No perspective-diff-sync-hash found. Neighbourhood tests will be skipped.");
  }
  console.log();

  // Start local bootstrap services
  console.log("Starting local HC bootstrap services...");
  const services = await runHcLocalServices();
  console.log(`Bootstrap: ${services.bootstrapUrl}, Proxy: ${services.proxyUrl}, Relay: ${services.relayUrl}`);

  console.log("\nStarting executor...");
  const executorProcess = await startExecutor(
    services.proxyUrl!,
    services.bootstrapUrl!,
    services.relayUrl || undefined
  );
  const pid = executorProcess.pid!;
  console.log(`Executor PID: ${pid}`);

  const perspectiveSnapshots: Snapshot[] = [];
  const neighbourhoodSnapshots: Snapshot[] = [];

  try {
    const client = new Ad4mClient(apolloClient(GQL_PORT, "profile-token"));

    console.log("\nGenerating agent...");
    await client.agent.generate("profiletest123");
    await sleep(5000);

    // ── Phase 1: Local Perspectives ──
    console.log("\n\n========================================");
    console.log("  PHASE 1: LOCAL PERSPECTIVES (no link language)");
    console.log("========================================\n");

    perspectiveSnapshots.push(await takeSnapshot("Baseline (0 perspectives)", pid, 0));
    console.log("\n--- Baseline memory map ---");
    printSmapsSummary(pid);

    console.log("\n--- Creating 1 perspective ---");
    await client.perspective.add("local-test-1");
    perspectiveSnapshots.push(await takeSnapshot("After 1 perspective", pid, 1));

    console.log("\n--- Creating 2 more (total 3) ---");
    for (let i = 2; i <= 3; i++) {
      await client.perspective.add(`local-test-${i}`);
    }
    perspectiveSnapshots.push(await takeSnapshot("After 3 perspectives", pid, 3));

    console.log("\n--- Creating 7 more (total 10) ---");
    for (let i = 4; i <= 10; i++) {
      await client.perspective.add(`local-test-${i}`);
    }
    perspectiveSnapshots.push(await takeSnapshot("After 10 perspectives", pid, 10));

    // ── Phase 2: Real Neighbourhoods ──
    if (diffSyncHash) {
      console.log("\n\n========================================");
      console.log("  PHASE 2: REAL NEIGHBOURHOODS (perspective-diff-sync)");
      console.log("========================================\n");

      neighbourhoodSnapshots.push(await takeSnapshot("Baseline (0 neighbourhoods, 10 local perspectives)", pid, 0));

      async function createNeighbourhood(name: string): Promise<string> {
        const perspective = await client.perspective.add(name);

        // Clone the diff-sync language with unique params
        const linkLang = await client.languages.applyTemplateAndPublish(
          diffSyncHash!,
          JSON.stringify({ uid: uuidv4(), name })
        );
        console.log(`  Link language cloned: ${linkLang.address}`);

        // Publish as neighbourhood
        const url = await client.neighbourhood.publishFromPerspective(
          perspective.uuid,
          linkLang.address,
          new Perspective([])
        );
        console.log(`  Neighbourhood published: ${url}`);

        // Wait for sync state
        let tries = 0;
        while (tries < 30) {
          const p = await client.perspective.byUUID(perspective.uuid);
          if (p?.state === "Synced") break;
          await sleep(2000);
          tries++;
        }

        return url;
      }

      console.log("\n--- Creating 1 neighbourhood ---");
      await createNeighbourhood("neighbourhood-1");
      neighbourhoodSnapshots.push(await takeSnapshot("After 1 neighbourhood", pid, 1));

      console.log("\n--- Creating 2 more neighbourhoods (total 3) ---");
      for (let i = 2; i <= 3; i++) {
        await createNeighbourhood(`neighbourhood-${i}`);
      }
      neighbourhoodSnapshots.push(await takeSnapshot("After 3 neighbourhoods", pid, 3));

      console.log("\n--- Creating 7 more neighbourhoods (total 10) ---");
      for (let i = 4; i <= 10; i++) {
        await createNeighbourhood(`neighbourhood-${i}`);
      }
      neighbourhoodSnapshots.push(await takeSnapshot("After 10 neighbourhoods", pid, 10));
      console.log("\n--- Memory map after 10 neighbourhoods ---");
      printSmapsSummary(pid);
    }

    // ── Summary ──
    console.log("\n\n========================================");
    console.log("  RESULTS");
    console.log("========================================");

    printTable("LOCAL PERSPECTIVES (no link language)", perspectiveSnapshots, "Perspectives ");
    if (neighbourhoodSnapshots.length > 0) {
      printTable("REAL NEIGHBOURHOODS (perspective-diff-sync + Holochain DNA)", neighbourhoodSnapshots, "Neighbourhoods");
    }

  } finally {
    console.log("\nCleaning up...");
    await killProcess(executorProcess);
    try { services.process.kill("SIGKILL"); } catch {}
  }
}

main().catch((err) => {
  console.error("Profile failed:", err);
  process.exit(1);
});
