import path from "path";
import fs from "fs";
import { spawn, execFileSync } from "child_process";

import type { PluginConfig } from "./types";

/**
 * Search for ad4m-executor binary in PATH and common locations.
 * Returns the absolute path if found, null otherwise.
 */
export function findExecutorBinary(): string | null {
  const name = "ad4m-executor";

  // 1. Check PATH entries
  const envPath = process.env.PATH ?? "";
  for (const dir of envPath.split(path.delimiter)) {
    if (!dir) continue;
    const candidate = path.join(dir, name);
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      return candidate;
    } catch {
      // not found or not executable
    }
  }

  // 2. Check common locations not always in PATH
  const home = process.env.HOME || process.env.USERPROFILE || "";
  const commonPaths = [
    "/usr/local/bin",
    "/usr/bin",
    "/opt/homebrew/bin",
    path.join(home, ".cargo", "bin"),
    path.join(home, ".local", "bin"),
    path.join(home, "bin"),
  ];

  for (const dir of commonPaths) {
    const candidate = path.join(dir, name);
    try {
      fs.accessSync(candidate, fs.constants.X_OK);
      return candidate;
    } catch {
      // not found
    }
  }

  return null;
}

// ---------------------------------------------------------------------------
// Executor Process Management (Managed Mode)
// ---------------------------------------------------------------------------

let executorProcess: ReturnType<typeof spawn> | null = null;
let executorLogStream: fs.WriteStream | null = null;

export function isExecutorRunning(
  endpoint: string,
  timeoutMs: number = 3000,
): Promise<boolean> {
  return new Promise((resolve) => {
    const controller = new AbortController();
    const timeout = setTimeout(() => {
      controller.abort();
      resolve(false);
    }, timeoutMs);

    fetch(endpoint, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 0,
        method: "tools/list",
        params: {},
      }),
      signal: controller.signal,
    })
      .then(() => {
        clearTimeout(timeout);
        resolve(true);
      })
      .catch(() => {
        clearTimeout(timeout);
        resolve(false);
      });
  });
}

/**
 * Result of ensureExecutorRunning:
 * - "already_running" — executor was found at the endpoint (not spawned by us)
 * - "spawned"         — we spawned a new executor process
 * - false             — failed to start / not available
 */
export type ExecutorStartResult = "already_running" | "spawned" | false;

export async function ensureExecutorRunning(
  adminCredential: string,
  logger: any,
  endpoint: string = "http://localhost:3001/mcp",
  wsEndpoint: string = "ws://localhost:12000/graphql",
  binaryPath?: string,
): Promise<ExecutorStartResult> {
  logger.info(`[ad4m] Checking if executor is running at ${endpoint}...`);

  // Check if already running
  if (await isExecutorRunning(endpoint, 3000)) {
    logger.info(`[ad4m] Executor is already running`);
    return "already_running";
  }

  const executorPath = binaryPath || "ad4m-executor";
  logger.info(`[ad4m] Executor not running, attempting to start...`);
  logger.info(`[ad4m] Using binary: ${executorPath}`);
  logger.info(`[ad4m] PATH: ${process.env.PATH ?? "(unset)"}`);

  // Check if executor needs initialization (first run).
  // ONLY run init when the data directory doesn't exist at all.
  // Running init on an existing directory can wipe agent keys via
  // its version-mismatch cleanup logic.
  const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
  const ad4mDir = path.join(home, ".ad4m");

  if (!fs.existsSync(ad4mDir)) {
    logger.info(
      `[ad4m] Data directory ${ad4mDir} not found — running init for first-time setup...`,
    );
    try {
      execFileSync(executorPath, ["init"], {
        stdio: ["ignore", "pipe", "pipe"],
        timeout: 30000,
      });
      logger.info(`[ad4m] Executor initialized successfully`);
    } catch (initErr: any) {
      logger.error(`[ad4m] Executor init failed: ${initErr.message}`);
      if (initErr.stderr) {
        logger.error(
          `[ad4m] init stderr: ${initErr.stderr.toString().trim()}`,
        );
      }
      return false;
    }
  }

  // Open log file for tee-ing stdout/stderr
  const logFilePath = path.join(ad4mDir, "ad4m.log");
  try {
    if (!fs.existsSync(ad4mDir)) {
      fs.mkdirSync(ad4mDir, { recursive: true });
    }
    executorLogStream = fs.createWriteStream(logFilePath, { flags: "a" });
    logger.info(`[ad4m] Logging executor output to ${logFilePath}`);
  } catch (logErr: any) {
    logger.warn(
      `[ad4m] Could not open log file ${logFilePath}: ${logErr.message}`,
    );
  }

  try {
    // Track whether spawn itself failed (ENOENT, permission error, etc.)
    let spawnFailed = false;
    let spawnError: string | null = null;

    // Start the executor as a child process
    executorProcess = spawn(
      executorPath,
      [
        "run",
        "--enable-mcp",
        "true",
        "--admin-credential",
        adminCredential,
        "--mcp-port",
        "3001",
      ],
      {
        stdio: ["ignore", "pipe", "pipe"],
        detached: false,
      },
    );

    executorProcess.stdout?.on("data", (data: Buffer) => {
      const line = data.toString().trim();
      logger.info(`[ad4m-executor] ${line}`);
      if (executorLogStream) {
        executorLogStream.write(
          `${new Date().toISOString()} [stdout] ${line}\n`,
        );
      }
    });

    executorProcess.stderr?.on("data", (data: Buffer) => {
      const line = data.toString().trim();
      logger.info(`[ad4m-executor] ${line}`);
      if (executorLogStream) {
        executorLogStream.write(
          `${new Date().toISOString()} [stderr] ${line}\n`,
        );
      }
    });

    executorProcess.on("error", (err: Error) => {
      spawnFailed = true;
      spawnError = err.message;
      logger.error(`[ad4m] Failed to start executor: ${err.message}`);
      logger.error(`[ad4m] PATH: ${process.env.PATH ?? "(unset)"}`);
      if (executorLogStream) {
        executorLogStream.end();
        executorLogStream = null;
      }
      executorProcess = null;
    });

    executorProcess.on("exit", (code: number | null) => {
      logger.info(`[ad4m] Executor exited with code ${code}`);
      if (code !== null && code !== 0) {
        spawnFailed = true;
        spawnError = `Executor exited with code ${code}`;
      }
      if (executorLogStream) {
        executorLogStream.end();
        executorLogStream = null;
      }
      executorProcess = null;
    });

    // Wait for executor to be ready (check spawn failure each iteration)
    logger.info(`[ad4m] Waiting for executor to start...`);
    for (let i = 0; i < 30; i++) {
      await new Promise((r) => setTimeout(r, 1000));

      // If spawn failed (ENOENT, permission, non-zero exit), stop waiting
      if (spawnFailed) {
        logger.error(
          `[ad4m] Executor process failed to start: ${
            spawnError ?? "unknown error"
          }`,
        );
        logger.error(
          `[ad4m] Make sure ad4m-executor is installed. Set ad4mBinaryPath in plugin config to the full path (e.g. /usr/local/bin/ad4m-executor) if it's not in PATH.`,
        );
        return false;
      }

      if (await isExecutorRunning(endpoint, 2000)) {
        logger.info(`[ad4m] Executor started successfully`);
        return "spawned";
      }
      logger.info(`[ad4m] Waiting... (${i + 1}/30)`);
    }

    logger.error(`[ad4m] Executor failed to start within 30 seconds`);
    return false;
  } catch (err: any) {
    logger.error(`[ad4m] Error starting executor: ${err.message}`);
    logger.error(
      `[ad4m] Make sure ad4m-executor is installed. Set ad4mBinaryPath in plugin config to the full path. PATH: ${
        process.env.PATH ?? "(unset)"
      }`,
    );
    return false;
  }
}

export function stopExecutor(logger?: any): void {
  if (executorProcess) {
    executorProcess.kill("SIGTERM");
    executorProcess = null;
    if (logger) logger.info(`[ad4m] Executor stopped`);
  }
  if (executorLogStream) {
    executorLogStream.end();
    executorLogStream = null;
  }
}


