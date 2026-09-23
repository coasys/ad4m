#!/usr/bin/env -S npx tsx
/**
 * Multi-machine WE Electron verification via Playwright.
 *
 * Launches WE Electron on each configured machine, connects all
 * instances to a shared executor with SFU, and captures Playwright
 * screenshots of the app window from every machine.
 *
 * Options:
 *   --config <path>          Machine config JSON (default: multi-machine.config.json)
 *   --executor-path <path>   Path to ad4m-executor binary
 *   --duration <sec>         Settle time before final screenshot (default: 15)
 *   --port <port>            Executor port (default: 15200)
 *   --output-dir <dir>       Screenshot output directory (default: /tmp/sfu-electron-verify)
 *   --machines <ids>         Comma-separated machine IDs (default: all)
 *   --verbose                Show full output from machines
 */

import { execSync, spawn, ChildProcess } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import { InstrumentedClient } from "./client.js";

interface MachineConfig {
  id: string;
  sshHost: string | null;
  platform?: "linux" | "darwin";
  adRepoPath: string;
  weRepoPath: string;
  audioToneHz: number;
  shellPreamble?: string;
}

interface Args {
  configPath: string;
  executorPath: string;
  duration: number;
  port: number;
  outputDir: string;
  machineFilter: string | null;
  verbose: boolean;
}

const ADMIN_CRED = "test123";
const CONNECT_VERSION = "0.13.0-test-interpretation-2";

function log(msg: string): void {
  const ts = new Date().toISOString().slice(11, 23);
  console.log(`[${ts}] ${msg}`);
}

function parseArgs(): Args {
  const argv = process.argv.slice(2);
  const get = (flag: string, def: string): string => {
    const i = argv.indexOf(flag);
    return i >= 0 && i + 1 < argv.length ? argv[i + 1] : def;
  };
  const repoRoot = join(import.meta.dirname, "..", "..", "..", "..");
  return {
    configPath: get("--config", join(import.meta.dirname, "multi-machine.config.json")),
    executorPath: get("--executor-path", join(repoRoot, "target", "release", "ad4m-executor")),
    duration: Number(get("--duration", "15")),
    port: Number(get("--port", "15200")),
    outputDir: get("--output-dir", "/tmp/sfu-electron-verify"),
    machineFilter: get("--machines", "") || null,
    verbose: argv.includes("--verbose"),
  };
}

function loadMachines(configPath: string): MachineConfig[] {
  if (!existsSync(configPath)) {
    console.error(`Config not found: ${configPath}`);
    process.exit(1);
  }
  const raw = JSON.parse(readFileSync(configPath, "utf-8"));
  return Array.isArray(raw) ? raw : raw.machines;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

function detectExecutorHost(): string {
  try {
    const out = execSync("tailscale status --self --json 2>/dev/null", {
      timeout: 5000,
    }).toString().trim();
    const j = JSON.parse(out);
    if (j.Self?.DNSName) return j.Self.DNSName.replace(/\.$/, "");
  } catch {}
  return "127.0.0.1";
}

async function startExecutor(
  executorPath: string,
  port: number,
  dataDir: string,
  verbose: boolean,
): Promise<ChildProcess> {
  // Kill any stale executor on this port
  try { execSync(`fuser -k ${port}/tcp 2>/dev/null`, { timeout: 5000 }); } catch {}
  mkdirSync(dataDir, { recursive: true });
  if (!existsSync(join(dataDir, "mainnet_seed.seed"))) {
    log("Initialising executor data directory...");
    execSync(`"${executorPath}" init --data-path "${dataDir}"`, { timeout: 30_000 });
  }
  const proc = spawn(executorPath, [
    "run",
    "--port", port.toString(),
    "--admin-credential", ADMIN_CRED,
    "--app-data-path", dataDir,
    "--run-dapp-server", "false",
    "--connect-holochain", process.env.SHARED_SPACE === "true" ? "true" : "false",
    "--localhost", "false",
    "--enable-multi-user", "true",
  ], { stdio: ["ignore", "pipe", "pipe"] });
  if (verbose) {
    proc.stdout?.on("data", (d: Buffer) => process.stdout.write(`[executor] ${d}`));
    proc.stderr?.on("data", (d: Buffer) => process.stderr.write(`[executor] ${d}`));
  }
  const maxAttempts = process.env.SHARED_SPACE === "true" ? 180 : 60;
  for (let i = 0; i < maxAttempts; i++) {
    try {
      execSync(`curl -sf http://127.0.0.1:${port}/health >/dev/null 2>&1`, { timeout: 2000 });
      return proc;
    } catch {}
    await sleep(500);
  }
  proc.kill();
  throw new Error("Executor health check timed out");
}

async function provisionUser(
  admin: InstrumentedClient,
  email: string,
  password: string,
): Promise<{ token: string; did: string }> {
  const result = await admin.call<{ did: string; success: boolean; error?: string }>(
    "user.create", { email, password },
  );
  if (!result.success) throw new Error(`user.create failed for ${email}: ${result.error}`);

  const token = await admin.call<string>("user.login", { email, password, appName: "electron-verify" });
  if (typeof token !== "string" || token.length === 0) {
    throw new Error(`user.login returned empty token for ${email}`);
  }
  return { token, did: result.did };
}

function ensurePlaywrightOnRemote(machine: MachineConfig): void {
  if (!machine.sshHost) return;
  const preamble = machine.shellPreamble || "";
  log(`  Ensuring playwright-core on ${machine.id}...`);
  try {
    execSync(`ssh -T ${machine.sshHost} bash -l << 'SSHEOF'
${preamble}
mkdir -p /tmp/sfu-electron-verify
cd /tmp/sfu-electron-verify
if [ ! -d node_modules/playwright-core ]; then
  echo '{"private":true}' > package.json
  npm install playwright-core 2>&1 | tail -3
fi
echo "PW_OK"
SSHEOF`, { timeout: 60_000 });
  } catch (err: any) {
    log(`  Warning: playwright-core setup on ${machine.id}: ${err.message?.slice(0, 100)}`);
  }
}

function deployClient(machine: MachineConfig): void {
  if (!machine.sshHost) return;
  const clientPath = join(import.meta.dirname, "electron-client.cjs");
  log(`  Deploying client to ${machine.id}...`);
  execSync(`ssh ${machine.sshHost} 'mkdir -p /tmp/sfu-electron-verify' && scp "${clientPath}" ${machine.sshHost}:/tmp/sfu-electron-verify/electron-client.cjs`, {
    timeout: 10_000,
  });
}

interface MachineHandle {
  machine: MachineConfig;
  proc: ChildProcess;
  result: Promise<Record<string, unknown>>;
  waitForMarker(marker: string, timeoutMs: number): Promise<void>;
}

function spawnOnMachine(
  machine: MachineConfig,
  executorHost: string,
  port: number,
  userToken: string,
  duration: number,
  verbose: boolean,
  envOverrides: Record<string, string> = {},
): MachineHandle {
  const env: Record<string, string> = {
    MACHINE_ID: machine.id,
    EXECUTOR_HOST: executorHost,
    EXECUTOR_PORT: port.toString(),
    USER_TOKEN: userToken,
    CONNECT_VERSION,
    WE_REPO_PATH: machine.weRepoPath,
    OUTPUT_DIR: "/tmp/sfu-electron-verify",
    SETTLE_SECONDS: duration.toString(),
    ...envOverrides,
  };

  let proc: ChildProcess;

  if (!machine.sshHost) {
    const clientPath = join(import.meta.dirname, "electron-client.cjs");
    const xvfbRun = ["/usr/bin/xvfb-run", "/usr/local/bin/xvfb-run"]
      .find((p) => existsSync(p));
    const cmd = xvfbRun ? `"${xvfbRun}" -a node "${clientPath}"` : `node "${clientPath}"`;
    proc = spawn("bash", ["-c", cmd], {
      stdio: ["ignore", "pipe", "pipe"],
      env: { ...process.env, ...env, NODE_PATH: "/tmp/sfu-electron-verify/node_modules" },
    });
  } else {
    const preamble = machine.shellPreamble || "";
    const envExports = Object.entries(env)
      .map(([k, v]) => `export ${k}=${JSON.stringify(v)}`)
      .join("\n");
    const isMac = machine.id === "macbook";
    const wrapper = isMac ? "node" : "xvfb-run -a node";
    const cmd = `ssh -T -o ServerAliveInterval=5 -o ServerAliveCountMax=3 -R ${port}:localhost:${port} ${machine.sshHost} bash -l << 'SSHEOF'
${preamble}
${envExports}
export NODE_PATH="/tmp/sfu-electron-verify/node_modules"
cd /tmp/sfu-electron-verify
${wrapper} electron-client.cjs
SSHEOF`;
    proc = spawn("bash", ["-c", cmd], {
      stdio: ["ignore", "pipe", "pipe"],
    });
  }

  let stdout = "";
  let stderr = "";
  type MarkerCb = { marker: string; resolve: () => void };
  const markerListeners: MarkerCb[] = [];

  proc.stdout?.on("data", (d: Buffer) => {
    stdout += d.toString();
    if (verbose) process.stdout.write(`[${machine.id}:out] ${d}`);
  });
  proc.stderr?.on("data", (d: Buffer) => {
    const chunk = d.toString();
    stderr += chunk;
    if (verbose) process.stderr.write(`[${machine.id}:err] ${d}`);
    for (const listener of markerListeners) {
      if (stderr.includes(listener.marker)) listener.resolve();
    }
  });

  const result = new Promise<Record<string, unknown>>((resolve) => {
    proc.on("close", () => {
      const idx = stdout.indexOf("---RESULT---");
      if (idx >= 0) {
        try { resolve(JSON.parse(stdout.slice(idx + 13))); return; } catch {}
      }
      resolve({ machine: machine.id, passed: false, error: "no result marker", rawTail: stdout.slice(-500) });
    });
  });

  function waitForMarker(marker: string, timeoutMs: number): Promise<void> {
    if (stderr.includes(marker)) return Promise.resolve();
    return new Promise((resolve, reject) => {
      const entry: MarkerCb = { marker, resolve };
      markerListeners.push(entry);
      const timer = setTimeout(() => {
        const i = markerListeners.indexOf(entry);
        if (i >= 0) markerListeners.splice(i, 1);
        reject(new Error(`Timeout waiting for ${marker} from ${machine.id}`));
      }, timeoutMs);
      const origResolve = entry.resolve;
      entry.resolve = () => { clearTimeout(timer); origResolve(); };
    });
  }

  return { machine, proc, result, waitForMarker };
}

async function launchOnMachine(
  machine: MachineConfig,
  executorHost: string,
  port: number,
  userToken: string,
  duration: number,
  verbose: boolean,
): Promise<Record<string, unknown>> {
  return new Promise((resolve) => {
    const env: Record<string, string> = {
      MACHINE_ID: machine.id,
      EXECUTOR_HOST: executorHost,
      EXECUTOR_PORT: port.toString(),
      USER_TOKEN: userToken,
      CONNECT_VERSION,
      WE_REPO_PATH: machine.weRepoPath,
      OUTPUT_DIR: "/tmp/sfu-electron-verify",
      SETTLE_SECONDS: duration.toString(),
      ...(process.env.CREATE_SPACE && { CREATE_SPACE: process.env.CREATE_SPACE }),
      ...(process.env.SHARED_SPACE && { SHARED_SPACE: process.env.SHARED_SPACE }),
      ...(process.env.START_CALL && { START_CALL: process.env.START_CALL }),
      ...(process.env.JOIN_SPACE_URL && { JOIN_SPACE_URL: process.env.JOIN_SPACE_URL }),
    };

    let proc: ChildProcess;

    if (!machine.sshHost) {
      const clientPath = join(import.meta.dirname, "electron-client.cjs");
      const xvfbRun = ["/usr/bin/xvfb-run", "/usr/local/bin/xvfb-run"]
        .find((p) => existsSync(p));
      const cmd = xvfbRun ? `"${xvfbRun}" -a node "${clientPath}"` : `node "${clientPath}"`;

      proc = spawn("bash", ["-c", cmd], {
        timeout: 120_000,
        stdio: ["ignore", "pipe", "pipe"],
        env: {
          ...process.env,
          ...env,
          NODE_PATH: "/tmp/sfu-electron-verify/node_modules",
        },
      });
    } else {
      const preamble = machine.shellPreamble || "";
      const envExports = Object.entries(env)
        .map(([k, v]) => `export ${k}=${JSON.stringify(v)}`)
        .join("\n");
      const isMac = machine.platform === "darwin";
      const wrapper = isMac ? "node" : "xvfb-run -a node";

      const cmd = `ssh -T -o ServerAliveInterval=5 -o ServerAliveCountMax=3 -R ${port}:localhost:${port} ${machine.sshHost} bash -l << 'SSHEOF'
${preamble}
${envExports}
export NODE_PATH="/tmp/sfu-electron-verify/node_modules"
cd /tmp/sfu-electron-verify
${wrapper} electron-client.cjs
SSHEOF`;
      proc = spawn("bash", ["-c", cmd], {
        timeout: 120_000,
        stdio: ["ignore", "pipe", "pipe"],
      });
    }

    let stdout = "";
    proc.stdout?.on("data", (d: Buffer) => {
      stdout += d.toString();
      if (verbose) process.stdout.write(`[${machine.id}:out] ${d}`);
    });
    proc.stderr?.on("data", (d: Buffer) => {
      if (verbose) process.stderr.write(`[${machine.id}:err] ${d}`);
    });
    proc.on("close", () => {
      const marker = stdout.indexOf("---RESULT---");
      if (marker >= 0) {
        try {
          resolve(JSON.parse(stdout.slice(marker + 13)));
          return;
        } catch {}
      }
      resolve({
        machine: machine.id,
        passed: false,
        error: "no result marker",
        rawTail: stdout.slice(-500),
      });
    });
  });
}

function fetchScreenshots(
  machine: MachineConfig,
  screenshots: string[],
  localDir: string,
): string[] {
  if (!machine.sshHost || !screenshots?.length) return screenshots || [];
  const fetched: string[] = [];
  for (const remotePath of screenshots) {
    const filename = `${machine.id}-${remotePath.split("/").pop()}`;
    const localPath = join(localDir, filename);
    try {
      execSync(`scp ${machine.sshHost}:"${remotePath}" "${localPath}" 2>/dev/null`, {
        timeout: 10_000,
      });
      fetched.push(existsSync(localPath) ? localPath : `FETCH_FAILED:${remotePath}`);
    } catch {
      fetched.push(`FETCH_FAILED:${remotePath}`);
    }
  }
  return fetched;
}

async function main(): Promise<void> {
  const args = parseArgs();
  mkdirSync(args.outputDir, { recursive: true });

  const allMachines = loadMachines(args.configPath);
  const machines = args.machineFilter
    ? allMachines.filter((m) =>
        args.machineFilter!.split(",").map((s) => s.trim()).includes(m.id),
      )
    : allMachines;

  if (machines.length === 0) {
    console.error("No machines selected.");
    process.exit(1);
  }

  log(`Machines: ${machines.map((m) => m.id).join(", ")}`);
  log(`Settle time: ${args.duration}s`);

  // Prepare local temp dir with playwright-core
  const localTmpDir = "/tmp/sfu-electron-verify";
  mkdirSync(localTmpDir, { recursive: true });
  if (!existsSync(join(localTmpDir, "node_modules", "playwright-core"))) {
    log("Installing playwright-core locally...");
    execSync(`cd "${localTmpDir}" && echo '{"private":true}' > package.json && npm install playwright-core 2>&1`, {
      timeout: 60_000,
    });
  }

  // Prepare remotes
  log("Preparing remote machines...");
  for (const m of machines.filter((m) => m.sshHost)) {
    ensurePlaywrightOnRemote(m);
    deployClient(m);
  }

  // Start executor
  log("Starting executor...");
  const dataDir = join(args.outputDir, "executor-data");
  const executorProc = await startExecutor(args.executorPath, args.port, dataDir, args.verbose);
  log("Executor ready");

  // Generate agent (required before any user operations)
  const admin = new InstrumentedClient({ port: args.port, host: "127.0.0.1", adminToken: ADMIN_CRED });
  await admin.connect();
  try {
    await admin.call("agent.generate", { passphrase: "electron-verify" });
    log("Agent generated");
  } catch {
    log("Agent already generated");
  }

  // Provision users
  log("Provisioning users...");
  const users: Array<{ machine: MachineConfig; email: string; token: string; did: string }> = [];
  for (const m of machines) {
    const email = `${m.id}@test.com`;
    const { token, did } = await provisionUser(admin, email, "test123");
    users.push({ machine: m, email, token, did });
    log(`  ${m.id}: ${did.slice(0, 24)}...`);
  }

  const executorHost = detectExecutorHost();
  log(`Executor host for remotes: ${executorHost}`);

  let results: Record<string, unknown>[] = [];

  if (process.env.SHARED_SPACE === "true") {
    // --- Multi-machine call test (two-phase sequential) ---
    log("Running multi-machine call test...");
    const creator = users[0];
    const joiners = users.slice(1);

    // Phase 1: Creator makes shared space + starts call with long hold
    log(`Phase 1: ${creator.machine.id} creates shared space + starts call...`);
    const creatorHandle = spawnOnMachine(
      creator.machine, "127.0.0.1", args.port, creator.token, args.duration, args.verbose,
      { CREATE_SPACE: "true", SHARED_SPACE: "true", START_CALL: "true", HOLD_CALL_SECONDS: process.env.CREATOR_HOLD || "240" },
    );

    let phase1Ok = false;
    try {
      await creatorHandle.waitForMarker("---CALL-ACTIVE---", 120_000);
      phase1Ok = true;
    } catch (err: any) {
      log(`Phase 1 failed: ${err.message}`);
      creatorHandle.proc.kill();
      results = [await creatorHandle.result];
    }

    if (phase1Ok) {
      log(`Phase 1 complete: ${creator.machine.id} in call`);

      // Phase 2: Extract neighbourhood URL from executor
      log("Phase 2: Extracting neighbourhood URL...");
      const userClient = new InstrumentedClient({ port: args.port, host: "127.0.0.1", adminToken: creator.token });
      await userClient.connect();
      const perspectives = await userClient.call<any[]>("perspective.all");
      const nhPerspective = perspectives?.find((p: any) => p.sharedUrl);
      await userClient.disconnect();

      if (!nhPerspective?.sharedUrl) {
        log(`ERROR: No neighbourhood found. Perspectives: ${JSON.stringify(perspectives?.map((p: any) => ({ name: p.name, sharedUrl: p.sharedUrl })))}`);
        creatorHandle.proc.kill();
        results = [await creatorHandle.result];
      } else {
      log(`Neighbourhood URL: ${nhPerspective.sharedUrl.slice(0, 60)}...`);

      // Phase 3: Join neighbourhood for other users via API
      log("Phase 3: Joining neighbourhood for other users...");
      for (const u of joiners) {
        try {
          const uc = new InstrumentedClient({ port: args.port, host: "127.0.0.1", adminToken: u.token });
          await uc.connect();
          await uc.call("neighbourhood.join", { url: nhPerspective.sharedUrl });
          await uc.disconnect();
          log(`  ${u.machine.id}: joined neighbourhood`);
        } catch (err: any) {
          log(`  ${u.machine.id}: join failed — ${err.message}`);
        }
      }
      await sleep(15_000);

      // Phase 4: Launch joiners — navigate to existing space + join call
      log("Phase 4: Launching joiners...");
      const joinerHandles = joiners.map((u) =>
        spawnOnMachine(
          u.machine, "127.0.0.1", args.port, u.token, args.duration, args.verbose,
          { START_CALL: "true", HOLD_CALL_SECONDS: process.env.JOINER_HOLD || "120", SPACE_NAME: nhPerspective.name || "SFU Test" },
        ),
      );

      // Wait for all joiners to enter call (or timeout gracefully)
      log("Waiting for joiners to enter call...");
      const joinerReady = await Promise.allSettled(
        joinerHandles.map((h) => h.waitForMarker("---CALL-ACTIVE---", 120_000)),
      );
      for (let i = 0; i < joinerReady.length; i++) {
        const status = joinerReady[i].status === "fulfilled" ? "in call" : "timed out";
        log(`  ${joiners[i].machine.id}: ${status}`);
      }

      // Phase 5: Settle for media stabilization
      log(`Phase 5: Settling ${args.duration}s for media...`);
      await sleep(args.duration * 1000);

      // Phase 6: Wait for all processes to finish (hold periods expire)
      log("Phase 6: Waiting for hold periods to expire...");
      const allHandles = [creatorHandle, ...joinerHandles];
      results = await Promise.all(allHandles.map((h) => h.result));
    }
    } // phase1Ok
  } else {
    // --- Landing page test (parallel launch) ---
    log("Launching WE Electron on all machines...");
    const launchPromises = users.map((u) =>
      launchOnMachine(u.machine, "127.0.0.1", args.port, u.token, args.duration, args.verbose),
    );
    results = await Promise.all(launchPromises);
  }

  // Fetch screenshots from remotes
  log("Fetching screenshots...");
  for (const r of results) {
    const machine = machines.find((m) => m.id === r.machine);
    if (!machine) continue;
    if (r.screenshots && Array.isArray(r.screenshots)) {
      r.localScreenshots = fetchScreenshots(machine, r.screenshots as string[], args.outputDir);
    }
  }

  // Report
  console.log("\n" + "=".repeat(72));
  console.log("  WE ELECTRON MULTI-MACHINE VERIFICATION");
  console.log("=".repeat(72));
  for (const r of results) {
    const status = r.passed ? "\x1b[32m[PASS]\x1b[0m" : "\x1b[31m[FAIL]\x1b[0m";
    console.log(`\n  ${status} ${r.machine}`);
    if (r.error) console.log(`    error: ${r.error}`);
    if (r.pageUrl) console.log(`    url: ${r.pageUrl}`);
    if (r.pageTitle) console.log(`    title: ${r.pageTitle}`);
    const shots = (r.localScreenshots || r.screenshots || []) as string[];
    for (const s of shots) console.log(`    screenshot: ${s}`);
    if (r.durationMs) console.log(`    duration: ${Math.round(Number(r.durationMs) / 1000)}s`);
  }
  console.log("\n" + "=".repeat(72));
  const allPassed = results.every((r) => r.passed);
  console.log(`  VERDICT: ${allPassed ? "\x1b[32mALL PASSED\x1b[0m" : "\x1b[31mFAILURES DETECTED\x1b[0m"}`);
  console.log("=".repeat(72));

  writeFileSync(
    join(args.outputDir, "electron-report.json"),
    JSON.stringify({ timestamp: new Date().toISOString(), executorHost, results }, null, 2),
  );
  log(`Report: ${join(args.outputDir, "electron-report.json")}`);

  await admin.disconnect();
  executorProc.kill("SIGKILL");

  // Hard exit fallback — SSH tunnels can hold the process open
  setTimeout(() => process.exit(allPassed ? 0 : 1), 3000).unref();
  process.exit(allPassed ? 0 : 1);
}

main().catch((err) => {
  console.error("Fatal:", err);
  setTimeout(() => process.exit(1), 1000).unref();
  process.exit(1);
});
