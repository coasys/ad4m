import path from "path";
import fs from "fs";
import os from "os";
import https from "https";
import http from "http";
import { spawn, execFileSync } from "child_process";

import type { PluginConfig } from "./types";

/** Canonical home directory — used by both discovery and download. */
const PLUGIN_BIN_DIR = path.join(
  process.env.HOME || process.env.USERPROFILE || os.tmpdir(),
  ".ad4m-plugin",
  "bin",
);

/**
 * Search for ad4m-executor binary in PATH and common locations.
 * Returns the absolute path if found, null otherwise.
 */
export function findExecutorBinary(): string | null {
  const isWindows = process.platform === "win32";
  const candidateNames = isWindows
    ? ["ad4m-executor.exe", "ad4m-executor"]
    : ["ad4m-executor"];
  // Windows doesn't use the executable permission bit
  const accessFlag = isWindows ? fs.constants.F_OK : fs.constants.X_OK;

  // 1. Check PATH entries
  const envPath = process.env.PATH ?? "";
  for (const dir of envPath.split(path.delimiter)) {
    if (!dir) continue;
    for (const name of candidateNames) {
      const candidate = path.join(dir, name);
      try {
        fs.accessSync(candidate, accessFlag);
        return candidate;
      } catch {
        // not found or not executable
      }
    }
  }

  // 2. Check common locations not always in PATH
  const home = process.env.HOME || process.env.USERPROFILE || "";
  const commonPaths = [
    PLUGIN_BIN_DIR,
    "/usr/local/bin",
    "/usr/bin",
    "/opt/homebrew/bin",
    path.join(home, ".cargo", "bin"),
    path.join(home, ".local", "bin"),
    path.join(home, "bin"),
  ];

  for (const dir of commonPaths) {
    for (const name of candidateNames) {
      const candidate = path.join(dir, name);
      try {
        fs.accessSync(candidate, accessFlag);
        return candidate;
      } catch {
        // not found
      }
    }
  }

  return null;
}

// ---------------------------------------------------------------------------
// Auto-download of ad4m-executor and ad4m CLI binaries
// ---------------------------------------------------------------------------

// Hardcoded version for auto-download.
// TODO: Update this with each release or make it configurable via plugin config.
const EXECUTOR_VERSION = "0.12.0";

/** Directory where downloaded binaries are stored */
function getPluginBinDir(): string {
  return PLUGIN_BIN_DIR;
}

interface DownloadAsset {
  /** Display name for logging */
  label: string;
  /** GitHub release asset filename */
  assetName: string;
  /** Local filename to save as */
  localName: string;
}

/**
 * Get the list of assets to download for the current platform.
 * Returns null with a logged error if the platform is unsupported.
 */
function getDownloadAssets(logger: any): DownloadAsset[] | null {
  const platform = process.platform;
  const arch = process.arch;

  // Map Node.js platform/arch to asset naming convention.
  // Linux x64 binaries are available now; macOS and Windows binaries will be
  // published in a future release (Nico will handle the CI for those).
  // If the asset doesn't exist for the current release version, the download
  // will fail with a clear HTTP 404 / "binary not found" message.

  let osName: string;
  let archName: string;
  let ext = ""; // Windows binaries need .exe

  switch (platform) {
    case "linux":
      osName = "linux";
      break;
    case "darwin":
      osName = "macos";
      break;
    case "win32":
      osName = "windows";
      ext = ".exe";
      break;
    default:
      logger.error(
        `[ad4m] Auto-download is not supported on ${platform}/${arch}. ` +
          `Please install ad4m-executor manually.`,
      );
      return null;
  }

  switch (arch) {
    case "x64":
      archName = "x64";
      break;
    case "arm64":
      archName = "aarch64";
      break;
    default:
      logger.error(
        `[ad4m] Auto-download is not supported for architecture ${arch} on ${platform}. ` +
          `Please install ad4m-executor manually.`,
      );
      return null;
  }

  const exeSuffix = ext; // .exe on Windows, empty otherwise
  const localExeSuffix = platform === "win32" ? ".exe" : "";

  return [
    {
      label: "ad4m-executor",
      assetName: `ad4m-cli-executor-${osName}-${EXECUTOR_VERSION}-${archName}${exeSuffix}`,
      localName: `ad4m-executor${localExeSuffix}`,
    },
    {
      label: "ad4m CLI client",
      assetName: `ad4m-cli-client-${osName}-${EXECUTOR_VERSION}-${archName}${exeSuffix}`,
      localName: `ad4m${localExeSuffix}`,
    },
  ];
}

const MAX_REDIRECTS = 5;
const REQUEST_TIMEOUT_MS = 300_000; // 5 minutes — executor binaries are ~364MB

/**
 * Download a file from a URL, following redirects (GitHub releases -> S3).
 * Returns a promise that resolves when the file is fully written.
 */
function downloadFile(
  url: string,
  destPath: string,
  label: string,
  logger: any,
  redirectCount: number = 0,
): Promise<void> {
  return new Promise((resolve, reject) => {
    const proto = url.startsWith("https") ? https : http;

    const req = proto
      .get(url, { headers: { "User-Agent": "openclaw-ad4m-plugin" }, timeout: REQUEST_TIMEOUT_MS }, (res) => {
        // Follow redirects (301, 302, 307, 308)
        if (
          res.statusCode &&
          res.statusCode >= 300 &&
          res.statusCode < 400 &&
          res.headers.location
        ) {
          if (redirectCount >= MAX_REDIRECTS) {
            res.resume();
            reject(new Error(`Too many redirects (${MAX_REDIRECTS}) downloading ${label}`));
            return;
          }
          const redirectUrl = new URL(res.headers.location, url).toString();
          downloadFile(redirectUrl, destPath, label, logger, redirectCount + 1)
            .then(resolve)
            .catch(reject);
          res.resume(); // consume response to free memory
          return;
        }

        if (!res.statusCode || res.statusCode !== 200) {
          res.resume();
          reject(
            new Error(
              `Failed to download ${label}: HTTP ${res.statusCode ?? "unknown"}`,
            ),
          );
          return;
        }

        const totalBytes = parseInt(res.headers["content-length"] || "0", 10);
        let downloadedBytes = 0;
        let lastLogPercent = -10; // log every 10%

        const tempPath = destPath + ".part";
        const fileStream = fs.createWriteStream(tempPath);

        res.on("data", (chunk: Buffer) => {
          downloadedBytes += chunk.length;
          if (totalBytes > 0) {
            const percent = Math.floor((downloadedBytes / totalBytes) * 100);
            if (percent - lastLogPercent >= 10) {
              const mb = (downloadedBytes / 1024 / 1024).toFixed(1);
              const totalMb = (totalBytes / 1024 / 1024).toFixed(1);
              logger.info(
                `[ad4m] Downloading ${label}: ${mb}MB / ${totalMb}MB (${percent}%)`,
              );
              lastLogPercent = percent;
            }
          }
        });

        res.pipe(fileStream);

        fileStream.on("finish", () => {
          fileStream.close();
          const mb = (downloadedBytes / 1024 / 1024).toFixed(1);
          logger.info(`[ad4m] Downloaded ${label}: ${mb}MB`);
          try {
            fs.renameSync(tempPath, destPath);
          } catch (renameErr: any) {
            fs.unlink(tempPath, () => {});
            reject(new Error(`Failed to finalize ${label}: ${renameErr.message}`));
            return;
          }
          resolve();
        });

        fileStream.on("error", (err) => {
          fs.unlink(tempPath, () => {}); // clean up partial file
          reject(new Error(`Failed to write ${label}: ${err.message}`));
        });
      });

    req.on("timeout", () => {
      req.destroy();
      fs.unlink(destPath + ".part", () => {});
      reject(new Error(`Download of ${label} timed out after ${REQUEST_TIMEOUT_MS / 1000}s`));
    });

    req.on("error", (err) => {
      fs.unlink(destPath + ".part", () => {});
      reject(new Error(`Network error downloading ${label}: ${err.message}`));
    });
  });
}

/**
 * Download ad4m-executor and ad4m CLI client binaries for the current platform.
 * Binaries are saved to ~/.ad4m-plugin/bin/ and made executable.
 *
 * Returns true if the executor binary was successfully downloaded.
 */
export async function downloadExecutor(logger: any): Promise<boolean> {
  const assets = getDownloadAssets(logger);
  if (!assets) return false;

  const binDir = getPluginBinDir();

  // Create bin directory
  try {
    fs.mkdirSync(binDir, { recursive: true });
  } catch (err: any) {
    logger.error(
      `[ad4m] Failed to create directory ${binDir}: ${err.message}`,
    );
    return false;
  }

  const baseUrl = `https://github.com/coasys/ad4m/releases/download/v${EXECUTOR_VERSION}`;

  for (const asset of assets) {
    const url = `${baseUrl}/${asset.assetName}`;
    const destPath = path.join(binDir, asset.localName);

    logger.info(`[ad4m] Downloading ${asset.label} from ${url}`);

    try {
      await downloadFile(url, destPath, asset.label, logger);

      // TODO: Add SHA-256 checksum verification when release manifests are available
      // Basic integrity check: reject zero-byte files
      const fileSize = fs.statSync(destPath).size;
      if (fileSize === 0) {
        fs.unlinkSync(destPath);
        throw new Error(`Downloaded file is empty (0 bytes)`);
      }

      fs.chmodSync(destPath, 0o755);
      logger.info(`[ad4m] Installed ${asset.label} at ${destPath}`);
    } catch (err: any) {
      logger.error(`[ad4m] Failed to download ${asset.label}: ${err.message}`);
      // If executor download fails, return false. Client failure is non-fatal.
      if (asset.label === "ad4m-executor") return false;
    }
  }

  return true;
}

// ---------------------------------------------------------------------------
// Executor Process Management (Managed Mode)
// ---------------------------------------------------------------------------

let executorProcess: ReturnType<typeof spawn> | null = null;
let executorLogStream: fs.WriteStream | null = null;

/**
 * Check whether an executor is reachable.
 *
 * Tries the MCP endpoint first, then falls back to a lightweight GraphQL
 * query on the default HTTP port (12000).  This ensures we detect executors
 * launched via ad4m-launcher where MCP is typically disabled.
 *
 * Returns `"mcp"` or `"graphql"` to indicate which interface responded,
 * or `false` if neither is reachable.
 */
export async function isExecutorRunning(
  endpoint: string,
  timeoutMs: number = 3000,
  graphqlHttpUrl: string = "http://localhost:12000/graphql",
): Promise<"mcp" | "graphql" | false> {
  const probe = (url: string, body: string, validate?: (json: any) => boolean): Promise<boolean> =>
    new Promise((resolve) => {
      const controller = new AbortController();
      const timeout = setTimeout(() => {
        controller.abort();
        resolve(false);
      }, timeoutMs);

      fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body,
        signal: controller.signal,
      })
        .then(async (res) => {
          clearTimeout(timeout);
          if (!res.ok) return resolve(false);
          if (!validate) return resolve(true);
          try {
            const json = await res.json();
            resolve(validate(json));
          } catch {
            resolve(false);
          }
        })
        .catch(() => {
          clearTimeout(timeout);
          resolve(false);
        });
    });

  // Try MCP and GraphQL in parallel — return the first that succeeds
  const [mcp, gql] = await Promise.all([
    probe(
      endpoint,
      JSON.stringify({ jsonrpc: "2.0", id: 0, method: "tools/list", params: {} }),
    ),
    probe(
      graphqlHttpUrl,
      JSON.stringify({ query: "{ agentStatus { isInitialized } }" }),
      // Unauthenticated requests lack AGENT_READ_CAPABILITY so the query
      // returns errors — but any GraphQL-shaped response (data or errors key)
      // confirms an AD4M executor is listening.
      (json) => json != null && ("data" in json || "errors" in json),
    ),
  ]);

  if (mcp) return "mcp";
  if (gql) return "graphql";
  return false;
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
  rustLog?: string,
  logTarget: "file" | "openclaw" | "both" = "file",
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

    if (rustLog) {
      logger.info(`[ad4m] Setting RUST_LOG=${rustLog}`);
    }

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
        env: {
          ...process.env,
          ...(rustLog ? { RUST_LOG: rustLog } : {}),
        },
      },
    );

    const logToOpenclaw = logTarget === "openclaw" || logTarget === "both";
    const logToFile = logTarget === "file" || logTarget === "both";

    executorProcess.stdout?.on("data", (data: Buffer) => {
      const line = data.toString().trim();
      if (logToOpenclaw) logger.info(`[ad4m-executor] ${line}`);
      if (logToFile && executorLogStream) {
        executorLogStream.write(
          `${new Date().toISOString()} [stdout] ${line}\n`,
        );
      }
    });

    executorProcess.stderr?.on("data", (data: Buffer) => {
      const line = data.toString().trim();
      if (logToOpenclaw) logger.info(`[ad4m-executor] ${line}`);
      if (logToFile && executorLogStream) {
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


