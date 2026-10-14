#!/usr/bin/env -S npx tsx
/**
 * Multi-machine SFU verification orchestrator.
 *
 * Starts a single executor on the local machine (with SFU + multi-user),
 * provisions one user per remote machine, starts an SFU room, then
 * SSH-launches the client script on each machine.  Collects results,
 * SCP-fetches screenshots, and prints a summary report.
 *
 * Usage:
 *   npx tsx multi-machine-verify.ts [options]
 *
 * Options:
 *   --config <path>          Machine config JSON (default: multi-machine.config.json)
 *   --executor-path <path>   Path to ad4m-executor binary
 *   --duration <sec>         Media hold duration (default: 30)
 *   --port <port>            Executor port (default: 15200)
 *   --output-dir <dir>       Local results directory (default: /tmp/sfu-verify)
 *   --machines <ids>         Comma-separated machine IDs (default: all from config)
 *   --skip-install           Skip pnpm install on remotes
 *   --verbose                Show executor + client stdout
 *
 * Machine definitions live in a JSON config file (not committed).
 * Copy multi-machine.config.example.json and fill in your machines.
 */

import { ChildProcess, spawn, execSync } from "node:child_process";
import { mkdirSync, existsSync, writeFileSync, readFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { hostname } from "node:os";

import { InstrumentedClient } from "./client.js";
import { startExecutor, waitForHealth, stopExecutor, sleep } from "./executor.js";
import { provisionPeers, registerSfuMembers, disconnectPeers, PeerSession } from "./users.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// ---------------------------------------------------------------------------
// Machine definitions — loaded from a JSON config file at runtime
// ---------------------------------------------------------------------------
interface MachineConfig {
  id: string;
  sshHost: string | null; // null = run locally (no SSH)
  adRepoPath: string;
  audioToneHz: number;
  /** Extra shell preamble (e.g. nvm setup). */
  shellPreamble?: string;
}

const DEFAULT_CONFIG_PATH = join(__dirname, "multi-machine.config.json");

function loadMachines(configPath: string): MachineConfig[] {
  if (!existsSync(configPath)) {
    console.error(
      `Machine config not found: ${configPath}\n` +
      `Create it from multi-machine.config.example.json and fill in your machines.`,
    );
    process.exit(1);
  }
  const raw = JSON.parse(readFileSync(configPath, "utf-8"));
  const machines: MachineConfig[] = Array.isArray(raw) ? raw : raw.machines;
  if (!machines || machines.length === 0) {
    console.error("Machine config contains no machines.");
    process.exit(1);
  }
  for (const m of machines) {
    if (m.sshHost === null && !m.adRepoPath) {
      m.adRepoPath = join(__dirname, "..", "..", "..", "..");
    }
  }
  return machines;
}

// ---------------------------------------------------------------------------
// CLI argument parsing
// ---------------------------------------------------------------------------
interface OrchestratorArgs {
  configPath: string;
  executorPath: string;
  duration: number;
  port: number;
  outputDir: string;
  machineFilter: string | null;
  skipInstall: boolean;
  verbose: boolean;
}

function parseArgs(): OrchestratorArgs {
  const argv = process.argv.slice(2);
  const get = (flag: string, def: string): string => {
    const i = argv.indexOf(flag);
    return i >= 0 && i + 1 < argv.length ? argv[i + 1] : def;
  };
  const has = (flag: string): boolean => argv.includes(flag);

  const repoRoot = join(__dirname, "..", "..", "..", "..");
  const defaultExec = join(repoRoot, "target", "release", "ad4m-executor");

  const machinesRaw = get("--machines", "");

  return {
    configPath: get("--config", DEFAULT_CONFIG_PATH),
    executorPath: get("--executor-path", defaultExec),
    duration: Number(get("--duration", "30")),
    port: Number(get("--port", "15200")),
    outputDir: get("--output-dir", "/tmp/sfu-verify"),
    machineFilter: machinesRaw || null,
    skipInstall: has("--skip-install"),
    verbose: has("--verbose"),
  };
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
const ADMIN_TOKEN = "test123";
const NEIGHBOURHOOD_URL = "windtunnel://multi-machine";
const ROOM_NAME = "multi-machine-verify";

function log(msg: string): void {
  const ts = new Date().toISOString().slice(11, 23);
  console.error(`[${ts}] ${msg}`);
}

function repoRoot(): string {
  return join(__dirname, "..", "..", "..", "..");
}

/** Run a command on a remote machine via SSH, or locally. */
function sshExec(
  machine: MachineConfig,
  cmd: string,
  opts?: { timeout?: number },
): string {
  const timeout = opts?.timeout ?? 120_000;
  const fullCmd = machine.shellPreamble
    ? `${machine.shellPreamble} && ${cmd}`
    : cmd;

  if (!machine.sshHost) {
    return execSync(fullCmd, {
      stdio: "pipe",
      timeout,
      shell: "/bin/bash",
    }).toString();
  }

  // Pipe the command via heredoc to a remote bash login shell.
  // This avoids all quoting-level issues and ensures PATH is set up.
  return execSync(
    `ssh -T ${machine.sshHost} bash -l << 'SSHEOF'\n${fullCmd}\nSSHEOF`,
    { stdio: "pipe", timeout, shell: "/bin/bash" },
  ).toString();
}

/** Deploy the client script to a remote machine via SCP. */
function deployClientScript(machine: MachineConfig): void {
  if (!machine.sshHost) return;
  const localScript = join(__dirname, "multi-machine-client.ts");
  const remoteDir = `${machine.adRepoPath}/tests/js/helpers/sfu`;
  log(`[${machine.id}] Deploying client script...`);
  try {
    execSync(`ssh ${machine.sshHost} "mkdir -p ${remoteDir}"`, { timeout: 10_000 });
    execSync(`scp "${localScript}" ${machine.sshHost}:${remoteDir}/multi-machine-client.ts`, { timeout: 30_000 });
    log(`[${machine.id}] Client script deployed.`);
  } catch (err: any) {
    log(`[${machine.id}] WARNING: deploy failed: ${err.message?.slice(0, 200)}`);
  }
}

/** Install test deps on a machine. */
function installDeps(machine: MachineConfig): void {
  log(`[${machine.id}] Installing test dependencies...`);
  const cmd = `cd ${machine.adRepoPath}/tests/js && pnpm install --no-frozen-lockfile 2>&1 | tail -3`;
  try {
    const out = sshExec(machine, cmd, { timeout: 180_000 });
    log(`[${machine.id}] deps installed: ${out.trim().split("\n").pop()}`);
  } catch (err: any) {
    log(`[${machine.id}] WARNING: dep install failed: ${err.message?.slice(0, 200)}`);
  }
}

interface ClientProcess {
  machine: MachineConfig;
  session: PeerSession;
  proc: ChildProcess;
  stdout: string;
  stderr: string;
}

function launchClient(
  machine: MachineConfig,
  session: PeerSession,
  args: OrchestratorArgs,
  executorHost: string,
): ClientProcess {
  const clientScript = `${machine.adRepoPath}/tests/js/helpers/sfu/multi-machine-client.ts`;
  const remoteOutputDir = "/tmp/sfu-verify";

  const clientArgs = [
    "--host", executorHost,
    "--port", String(args.port),
    "--token", session.token,
    "--neighbourhood", NEIGHBOURHOOD_URL,
    "--room", ROOM_NAME,
    "--duration", String(args.duration),
    "--output-dir", remoteOutputDir,
    "--label", machine.id,
    "--tone-hz", String(machine.audioToneHz),
  ].join(" ");

  const runCmd = `cd ${machine.adRepoPath}/tests/js && tsx helpers/sfu/multi-machine-client.ts ${clientArgs}`;
  const fullCmd = machine.shellPreamble
    ? `${machine.shellPreamble} && ${runCmd}`
    : runCmd;

  let proc: ChildProcess;
  if (machine.sshHost) {
    // Wrap in a local bash heredoc so the remote gets a clean bash
    // login shell with .profile sourced (nvm/node on PATH).
    // Using ./node_modules/.bin/tsx avoids npx PATH resolution issues.
    const heredoc =
      `ssh -T ${machine.sshHost} bash -l << 'SSHEOF'\n${fullCmd}\nSSHEOF`;
    proc = spawn("/bin/bash", ["-c", heredoc], { stdio: "pipe" });
  } else {
    proc = spawn("/bin/bash", ["-c", fullCmd], { stdio: "pipe" });
  }

  const cp: ClientProcess = {
    machine,
    session,
    proc,
    stdout: "",
    stderr: "",
  };

  proc.stdout?.on("data", (d) => {
    cp.stdout += d.toString();
    if (args.verbose) process.stderr.write(`[${machine.id}:out] ${d}`);
  });
  proc.stderr?.on("data", (d) => {
    cp.stderr += d.toString();
    if (args.verbose) process.stderr.write(`[${machine.id}:err] ${d}`);
  });

  return cp;
}

function extractResult(cp: ClientProcess): Record<string, unknown> | null {
  const marker = "---RESULT---";
  const idx = cp.stdout.indexOf(marker);
  if (idx < 0) return null;
  const json = cp.stdout.slice(idx + marker.length).trim();
  try {
    return JSON.parse(json);
  } catch {
    return null;
  }
}

/** Convert a raw YUV file to PNG using ffmpeg. */
function yuvToPng(yuvPath: string, width: number, height: number): string | null {
  const pngPath = yuvPath.replace(/\.yuv$/, ".png");
  try {
    execSync(
      `ffmpeg -f rawvideo -pix_fmt yuv420p -s ${width}x${height} -i "${yuvPath}" -frames:v 1 -y "${pngPath}" 2>/dev/null`,
    );
    return existsSync(pngPath) ? pngPath : null;
  } catch {
    return null;
  }
}

/** Fetch screenshot from a remote machine via SCP. Tries PNG first, then YUV. */
function fetchScreenshot(
  machine: MachineConfig,
  localDir: string,
): string | null {
  const pngName = `${machine.id}-received.png`;
  const yuvName = `${machine.id}-received.yuv`;

  if (!machine.sshHost) {
    const localPng = join("/tmp/sfu-verify", pngName);
    if (existsSync(localPng)) return localPng;
    const localYuv = join("/tmp/sfu-verify", yuvName);
    if (existsSync(localYuv)) return yuvToPng(localYuv, 320, 240) ?? localYuv;
    return null;
  }

  const localPng = join(localDir, pngName);
  const localYuv = join(localDir, yuvName);

  try {
    execSync(`scp ${machine.sshHost}:/tmp/sfu-verify/${pngName} "${localPng}" 2>/dev/null`);
    if (existsSync(localPng)) return localPng;
  } catch {}

  try {
    execSync(`scp ${machine.sshHost}:/tmp/sfu-verify/${yuvName} "${localYuv}" 2>/dev/null`);
    if (existsSync(localYuv)) return yuvToPng(localYuv, 320, 240) ?? localYuv;
  } catch {}

  return null;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
async function main(): Promise<void> {
  const args = parseArgs();
  mkdirSync(args.outputDir, { recursive: true });

  const allMachines = loadMachines(args.configPath);
  const machines = args.machineFilter
    ? allMachines.filter((m) => args.machineFilter!.split(",").map((s) => s.trim()).includes(m.id))
    : allMachines;
  if (machines.length === 0) {
    console.error("No machines selected. Available:", allMachines.map((m) => m.id).join(", "));
    process.exit(1);
  }

  log(`Machines: ${machines.map((m) => m.id).join(", ")}`);
  log(`Executor: ${args.executorPath}`);
  log(`Duration: ${args.duration}s`);
  log(`Port: ${args.port}`);

  if (!existsSync(args.executorPath)) {
    console.error(`Executor binary not found: ${args.executorPath}`);
    process.exit(1);
  }

  // Step 1: Install deps on all machines (parallel)
  if (!args.skipInstall) {
    log("Installing test dependencies on all machines...");
    await Promise.all(
      machines.map((m) =>
        new Promise<void>((resolve) => {
          try {
            installDeps(m);
          } catch {}
          resolve();
        }),
      ),
    );
  }

  // Step 2: Start executor
  log("Starting executor...");
  const dataPath = join(args.outputDir, "executor-data");
  const proc = await startExecutor(args.executorPath, {
    branch: "multi-machine",
    port: args.port,
    dataPath,
    adminToken: ADMIN_TOKEN,
    adamRepoPath: repoRoot(),
    buildDir: "",
    extraArgs: ["--localhost", "false"],
  });
  log("Waiting for executor health...");
  await waitForHealth(args.port, 120_000, ADMIN_TOKEN);
  log("Executor ready.");

  const admin = new InstrumentedClient({ port: args.port, adminToken: ADMIN_TOKEN });
  await admin.connect();
  try {
    await admin.call("agent.generate", { passphrase: "multi-machine-verify" });
  } catch {}

  // Step 3: Provision users and SFU room
  log("Provisioning users...");
  const sessions = await provisionPeers({
    admin,
    port: args.port,
    count: machines.length,
    labelPrefix: "mm-peer",
  });

  log("Starting SFU room...");
  await admin.call("sfu.startRoom", {
    neighbourhoodUrl: NEIGHBOURHOOD_URL,
    roomName: ROOM_NAME,
  });

  log("Registering SFU members...");
  await registerSfuMembers({ admin, neighbourhoodUrl: NEIGHBOURHOOD_URL, sessions });

  // Detect the hostname remote machines can reach us on.
  // Prefer the Tailscale hostname; fall back to auto-detect.
  const executorHost = detectExecutorHost();
  log(`Executor host for remotes: ${executorHost}`);

  // Step 3.5: Deploy client script to remote machines
  log("Deploying client script to remote machines...");
  for (const m of machines) {
    deployClientScript(m);
  }

  // Step 4: Launch clients (parallel)
  log("Launching clients on all machines...");
  const clients: ClientProcess[] = machines.map((m, i) =>
    launchClient(m, sessions[i], args, executorHost),
  );

  // Wait for all clients to finish
  const results = await Promise.all(
    clients.map(
      (cp) =>
        new Promise<{ machine: string; result: Record<string, unknown> | null; exitCode: number | null }>(
          (resolve) => {
            const timeout = setTimeout(() => {
              cp.proc.kill("SIGTERM");
              resolve({
                machine: cp.machine.id,
                result: { error: "client timeout", passed: false },
                exitCode: null,
              });
            }, (args.duration + 60) * 1000);

            cp.proc.on("close", (code) => {
              clearTimeout(timeout);
              resolve({
                machine: cp.machine.id,
                result: extractResult(cp) ?? {
                  error: `no result (exit ${code}): ${cp.stderr.slice(-500)}`,
                  passed: false,
                },
                exitCode: code,
              });
            });
          },
        ),
    ),
  );

  // Step 5: Fetch screenshots
  log("Fetching screenshots...");
  for (const m of machines) {
    const path = fetchScreenshot(m, args.outputDir);
    if (path) log(`  ${m.id}: ${path}`);
    else log(`  ${m.id}: no screenshot`);
  }

  // Step 6: Clean up
  log("Stopping SFU room...");
  try {
    await admin.call("sfu.stopRoom", {
      neighbourhoodUrl: NEIGHBOURHOOD_URL,
      roomName: ROOM_NAME,
    });
  } catch {}
  await disconnectPeers(sessions);
  await admin.disconnect();
  stopExecutor(proc);
  await sleep(1000);

  // Step 7: Report
  console.log("\n" + "=".repeat(72));
  console.log("  MULTI-MACHINE SFU VERIFICATION REPORT");
  console.log("=".repeat(72));
  console.log(`  Machines:  ${machines.map((m) => m.id).join(", ")}`);
  console.log(`  Duration:  ${args.duration}s`);
  console.log(`  Executor:  ${executorHost}:${args.port}`);
  console.log();

  let allPassed = true;
  for (const r of results) {
    const res = r.result ?? {};
    const passed = res.passed === true;
    if (!passed) allPassed = false;

    const status = passed ? "PASS" : "FAIL";
    console.log(`  [${status}] ${r.machine}`);
    if (res.error) console.log(`         error: ${res.error}`);
    console.log(`         sent: ${fmtBytes(res.bytesSent as number)} | received: ${fmtBytes(res.bytesReceived as number)}`);
    console.log(`         rtt: ${res.roundTripMs ?? "n/a"}ms | loss: ${res.packetsLost ?? "n/a"} pkts`);
    console.log(`         candidate: ${res.localCandidateType ?? "?"} ↔ ${res.remoteCandidateType ?? "?"}`);
    console.log(`         renegotiations: ${res.renegotiations ?? 0} (failures: ${res.renegotiationFailures ?? 0})`);
    console.log(`         frames enc: ${res.framesEncoded ?? 0} | dec: ${res.framesDecoded ?? 0} | drop: ${res.framesDropped ?? 0}`);
    if (res.screenshot && typeof res.screenshot === "object") {
      const ss = res.screenshot as Record<string, unknown>;
      if (ss.error) console.log(`         screenshot: ${ss.error}`);
      else console.log(`         screenshot: ${ss.width}x${ss.height} → ${ss.path}`);
    }
    console.log();
  }

  console.log("=".repeat(72));
  console.log(`  VERDICT: ${allPassed ? "ALL PASSED" : "SOME FAILED"}`);
  console.log("=".repeat(72));

  // Write machine-readable results
  const reportPath = join(args.outputDir, "report.json");
  writeFileSync(
    reportPath,
    JSON.stringify(
      {
        timestamp: new Date().toISOString(),
        machines: machines.map((m) => m.id),
        duration: args.duration,
        allPassed,
        results: results.map((r) => ({ machine: r.machine, ...r.result })),
      },
      null,
      2,
    ),
  );
  log(`Report written to ${reportPath}`);

  process.exit(allPassed ? 0 : 1);
}

function detectExecutorHost(): string {
  // Try Tailscale hostname first
  try {
    const ts = execSync("tailscale status --self --json 2>/dev/null", {
      timeout: 5000,
    }).toString();
    const parsed = JSON.parse(ts);
    if (parsed.Self?.DNSName) {
      return parsed.Self.DNSName.replace(/\.$/, "");
    }
  } catch {}

  // Fall back to hostname
  const hn = hostname();
  if (hn && hn !== "localhost") return hn;

  // Last resort: detect outbound IP
  try {
    const ip = execSync(
      "python3 -c \"import socket; s=socket.socket(socket.AF_INET, socket.SOCK_DGRAM); s.connect(('8.8.8.8',80)); print(s.getsockname()[0])\" 2>/dev/null",
      { timeout: 5000 },
    )
      .toString()
      .trim();
    if (ip) return ip;
  } catch {}

  return "127.0.0.1";
}

function fmtBytes(n: unknown): string {
  if (typeof n !== "number" || n === 0) return "0 B";
  if (n < 1024) return `${n} B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)} KB`;
  return `${(n / (1024 * 1024)).toFixed(2)} MB`;
}

main().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
