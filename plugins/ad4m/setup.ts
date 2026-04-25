import fs from "fs";
import path from "path";
import { generateRandomPassphrase } from "./config";
import {
  findExecutorBinary,
  isExecutorRunning,
  ensureExecutorRunning,
  stopExecutor,
  downloadExecutor,
} from "./executor";
import { ensureAgentReady } from "./agent";
import {
  mcpInitialize,
  mcpCallTool,
  extractMcpResultData,
} from "./mcpClient";

const SEPARATOR = "══════════════════════════════════════════";

/**
 * First-run setup flow.
 *
 * Invoked via the `openclaw ad4m-setup` CLI command (registered through
 * `registerCli`).  Gathers information (binary path, executor state, agent),
 * performs initial setup where possible, and prints a config snippet for the
 * user to copy-paste into their openclaw.json.
 *
 * @param openclawConfig - The full OpenClaw config object (provides hooks.token)
 * @param logger         - Plugin logger
 * @param endpoint       - MCP endpoint URL
 * @param executorUrl    - REST URL
 */
export async function runSetup(
  openclawConfig: any,
  logger: any,
  endpoint: string = "http://localhost:3001/mcp",
  executorUrl: string = "http://localhost:12000",
): Promise<void> {
  logger.info("[ad4m-setup] Starting first-run setup...");

  // ── Step 1: Read wakeToken ──
  let wakeToken: string | undefined;
  try {
    wakeToken = openclawConfig?.hooks?.token;
    if (wakeToken) {
      logger.info("[ad4m-setup] Found wakeToken from OpenClaw hooks config");
    } else {
      logger.warn(
        "[ad4m-setup] No hooks token found. Enable hooks in OpenClaw first " +
          "(openclaw.json → hooks.enabled: true) to use the waker service.",
      );
    }
  } catch {
    logger.warn("[ad4m-setup] Could not read OpenClaw hooks config");
  }

  // ── Step 2: Find binary ──
  const binaryPath = findExecutorBinary();
  if (binaryPath) {
    logger.info(`[ad4m-setup] Found ad4m-executor at: ${binaryPath}`);
  } else {
    logger.warn(
      "[ad4m-setup] ad4m-executor not found in PATH or common locations.",
    );
  }

  // ── Step 3: Check if executor is already running ──
  const running = await isExecutorRunning(endpoint, 3000, executorUrl);

  // ── Branch routing ──

  if (running) {
    // Branch B: Executor already running
    await setupExternalMode(logger, endpoint, wakeToken, running, executorUrl);
  } else if (binaryPath) {
    // Branch A: No running executor, binary found
    await setupManagedMode(logger, binaryPath, endpoint, executorUrl, wakeToken);
  } else {
    // Branch C: No binary, no executor — try to download, then set up managed mode
    await setupWithDownload(logger, endpoint, executorUrl, wakeToken);
  }
}

// ---------------------------------------------------------------------------
// Branch A: Managed mode — start executor, generate agent, print config
// ---------------------------------------------------------------------------

async function setupManagedMode(
  logger: any,
  binaryPath: string,
  endpoint: string,
  executorUrl: string,
  wakeToken?: string,
): Promise<void> {
  logger.info("[ad4m-setup] Setting up managed mode...");

  // Check if an existing agent/keys already exist
  const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
  const ad4mDir = path.join(home, ".ad4m");
  const existingAgent = fs.existsSync(ad4mDir);

  if (existingAgent) {
    // Existing agent data found — we can't generate a new agent and we
    // don't know the passphrase.  Ask the user to provide it in config.
    logger.info(
      `[ad4m-setup] Found existing AD4M agent data at ${ad4mDir}.`,
    );
    logger.info(
      "[ad4m-setup] Please provide your existing agent passphrase in the config below.",
    );
    printConfigSnippet(logger, "managed", {
      ad4mBinaryPath: binaryPath,
      agentPassphrase: "<enter-your-existing-passphrase>",
      wakeToken,
    });
    return;
  }

  // No existing agent — proceed with fresh setup
  const adminCredential = generateRandomPassphrase(24);
  const agentPassphrase = generateRandomPassphrase(32);

  // Start executor
  const startResult = await ensureExecutorRunning(
    adminCredential,
    logger,
    endpoint,
    executorUrl,
    binaryPath,
  );

  if (!startResult) {
    logger.error(
      "[ad4m-setup] Failed to start executor. Cannot complete setup.",
    );
    printConfigSnippet(logger, "managed", {
      ad4mBinaryPath: binaryPath,
      agentPassphrase: "<run setup again after fixing executor>",
      wakeToken,
    });
    return;
  }

  if (startResult === "already_running") {
    // Unexpected — we checked above, but executor may have started between checks.
    // Fall through to external mode logic.
    logger.info(
      "[ad4m-setup] Executor started between checks — switching to external mode flow.",
    );
    stopExecutor(logger);
    await setupExternalMode(logger, endpoint, wakeToken);
    return;
  }

  // Executor spawned — generate agent
  const agentResult = await ensureAgentReady(
    executorUrl,
    adminCredential,
    logger,
    agentPassphrase,
  );

  // Stop executor — user will start it properly after adding config
  stopExecutor(logger);

  const effectivePassphrase = agentResult?.passphrase ?? agentPassphrase;

  if (agentResult) {
    logger.info(
      `[ad4m-setup] Agent ready. DID: ${agentResult.did}`,
    );
  } else {
    logger.warn("[ad4m-setup] Agent setup did not complete cleanly.");
  }

  printConfigSnippet(logger, "managed", {
    ad4mBinaryPath: binaryPath,
    agentPassphrase: effectivePassphrase,
    wakeToken,
  });
}

// ---------------------------------------------------------------------------
// Branch B: External mode — request capabilities, obtain JWT, print config
// ---------------------------------------------------------------------------

async function setupExternalMode(
  logger: any,
  endpoint: string,
  wakeToken?: string,
  detectedVia: "mcp" | "rest" = "mcp",
  executorUrl: string = "http://localhost:12000",
): Promise<void> {
  if (detectedVia === "rest") {
    // Executor found via REST (MCP is disabled / not available).
    // Use Ad4mClient REST API to request capabilities.
    logger.info(
      `[ad4m-setup] Found a running AD4M executor via REST at ${executorUrl}. ` +
        "MCP does not appear to be enabled.",
    );
    logger.info(
      "[ad4m-setup] Attempting capability request via Ad4mClient...",
    );

    try {
      await setupExternalModeViaRest(logger, executorUrl, wakeToken);
      return;
    } catch (e: any) {
      logger.error(
        `[ad4m-setup] REST auth flow failed: ${e.message}`,
      );
      if (e.stack) {
        logger.error(`[ad4m-setup] Stack: ${e.stack}`);
      }
      printConfigSnippet(logger, "external", {
        executorUrl,
        token: "<paste-your-jwt-here>",
        wakeToken,
      });
      return;
    }
  }

  // ── MCP path (original logic) ──
  logger.info(
    `[ad4m-setup] Found a running AD4M executor at ${endpoint}. ` +
      "Will attempt to request capabilities.",
  );
  logger.info(
    "[ad4m-setup] The executor will show a verification code. " +
      "Please confirm the request in your executor UI.",
  );

  try {
    // Initialize MCP session (no auth needed)
    const initResp = await mcpInitialize(endpoint);

    // Request capabilities
    const capResult = await mcpCallTool(
      endpoint,
      "request_capability",
      {
        app_name: "OpenClaw AD4M Plugin",
        app_desc: "OpenClaw agent plugin for AD4M neighbourhoods",
      },
      initResp.sessionId,
    );
    const capData = extractMcpResultData(capResult);

    if (capData?.request_id && capData?.code) {
      logger.info(
        `[ad4m-setup] Capability requested. Verification code: ${capData.code}`,
      );
      logger.info(
        "[ad4m-setup] Please confirm this code in your AD4M executor, " +
          "then the plugin will finalize authentication.",
      );

      // Attempt to generate JWT
      const jwtResult = await mcpCallTool(
        endpoint,
        "generate_jwt",
        { request_id: capData.request_id, code: capData.code },
        initResp.sessionId,
      );
      const jwtData = extractMcpResultData(jwtResult);

      if (jwtData?.token) {
        logger.info("[ad4m-setup] JWT obtained successfully!");
        printConfigSnippet(logger, "external", {
          mcpEndpoint: endpoint,
          token: jwtData.token,
          wakeToken,
        });
        return;
      }
    }

    // JWT auth failed
    logger.warn(
      "[ad4m-setup] Could not complete auth automatically. " +
        "Please obtain a JWT token manually from your executor.",
    );
    printConfigSnippet(logger, "external", {
      mcpEndpoint: endpoint,
      token: "<paste-your-jwt-here>",
      wakeToken,
    });
  } catch (e: any) {
    logger.error(`[ad4m-setup] Auth flow failed: ${e.message}`);
    printConfigSnippet(logger, "external", {
      mcpEndpoint: endpoint,
      token: "<paste-your-jwt-here>",
      wakeToken,
    });
  }
}

/**
 * Prompt the user for a line of input on stdin.
 */
function promptUser(question: string): Promise<string> {
  const readline = require("readline");
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  return new Promise((resolve) => {
    rl.question(question, (answer: string) => {
      rl.close();
      resolve(answer.trim());
    });
  });
}

/**
 * External-mode auth flow using Ad4mClient over REST.
 * Used when the executor is detected via REST (MCP disabled).
 *
 * Flow:
 * 1. requestCapability → returns requestId, launcher shows 6-digit code
 * 2. User enters the 6-digit code from the launcher UI
 * 3. generateJwt(requestId, code) → returns JWT
 */
async function setupExternalModeViaRest(
  logger: any,
  executorUrl: string,
  wakeToken?: string,
): Promise<void> {
  const { Ad4mClient } = require("@coasys/ad4m");

  logger.info(`[ad4m-setup] Creating REST client for ${executorUrl}...`);

  try {
    const client = new Ad4mClient(executorUrl, undefined, false);
    logger.info("[ad4m-setup] Ad4mClient created successfully");

    // Step 1: Request capability — triggers the verification popup in the launcher
    const authInfo = {
      appName: "OpenClaw AD4M Plugin",
      appDesc: "OpenClaw agent plugin for AD4M neighbourhoods",
      appDomain: "",
      capabilities: [
        { with: { domain: "*", pointers: ["*"] }, can: ["*"] },
      ],
    };

    logger.info("[ad4m-setup] Sending requestCapability...");
    logger.info(`[ad4m-setup] authInfo: ${JSON.stringify(authInfo)}`);
    const requestId = await client.agent.requestCapability(authInfo);

    logger.info(
      `[ad4m-setup] Capability requested successfully.`,
    );
    logger.info("");
    logger.info(
      "[ad4m-setup] ┌─────────────────────────────────────────────────────────┐",
    );
    logger.info(
      "[ad4m-setup] │  A capability request has been sent to your AD4M       │",
    );
    logger.info(
      "[ad4m-setup] │  launcher. Please:                                     │",
    );
    logger.info(
      "[ad4m-setup] │                                                        │",
    );
    logger.info(
      "[ad4m-setup] │  1. Open your AD4M launcher                            │",
    );
    logger.info(
      "[ad4m-setup] │  2. Approve the capability request from 'OpenClaw'     │",
    );
    logger.info(
      "[ad4m-setup] │  3. Note the 6-digit verification code shown           │",
    );
    logger.info(
      "[ad4m-setup] │  4. Come back here and enter that code below           │",
    );
    logger.info(
      "[ad4m-setup] └─────────────────────────────────────────────────────────┘",
    );
    logger.info("");

    // Step 2: Prompt the user for the 6-digit code shown in the launcher
    const code = await promptUser(
      "[ad4m-setup] Enter the 6-digit code from your AD4M launcher: ",
    );

    if (!code) {
      logger.warn("[ad4m-setup] No code entered. Setup cancelled.");
      printConfigSnippet(logger, "external", {
        executorUrl,
        token: "<paste-your-jwt-here>",
        wakeToken,
      });
      return;
    }

    // Step 3: Generate JWT using requestId + user-provided code
    logger.info(`[ad4m-setup] Sending generateJwt...`);
    const jwt = await client.agent.generateJwt(requestId, code);
    logger.info(`[ad4m-setup] generateJwt returned: ${jwt ? `token (${jwt.length} chars)` : "null/empty"}`);

    if (jwt) {
      logger.info("[ad4m-setup] JWT obtained successfully!");
      printConfigSnippet(logger, "external", {
        executorUrl,
        token: jwt,
        wakeToken,
      });
    } else {
      logger.warn(
        "[ad4m-setup] Could not obtain JWT. The code may have been incorrect. " +
          "Please try running setup again or obtain a JWT manually.",
      );
      printConfigSnippet(logger, "external", {
        executorUrl,
        token: "<paste-your-jwt-here>",
        wakeToken,
      });
    }
  } catch (e: any) {
    logger.error(`[ad4m-setup] REST auth flow failed: ${e.message}`);
    if (e.stack) logger.error(`[ad4m-setup] Stack: ${e.stack}`);
    printConfigSnippet(logger, "external", {
      executorUrl,
      token: "<paste-your-jwt-here>",
      wakeToken,
    });
  }
}

// ---------------------------------------------------------------------------
// Branch C: No executor, no binary — auto-download then managed mode
// ---------------------------------------------------------------------------

async function setupWithDownload(
  logger: any,
  endpoint: string,
  executorUrl: string,
  wakeToken?: string,
): Promise<void> {
  logger.info(
    "[ad4m-setup] No ad4m-executor binary found and no running executor detected.",
  );
  logger.info(
    "[ad4m-setup] Attempting to download ad4m-executor for your platform...",
  );

  const downloaded = await downloadExecutor(logger);

  if (!downloaded) {
    logger.error(
      "[ad4m-setup] Auto-download failed. Please install ad4m-executor manually:",
    );
    logger.info(
      "[ad4m-setup]   https://github.com/coasys/ad4m/releases",
    );
    printConfigSnippet(logger, "managed", {
      ad4mBinaryPath: "<path-to-ad4m-executor>",
      agentPassphrase: "<will-be-shown-after-setup>",
      wakeToken,
    });
    return;
  }

  // After download, find the binary in the plugin bin dir
  const binaryPath = findExecutorBinary();
  if (!binaryPath) {
    logger.error(
      "[ad4m-setup] Download succeeded but binary not found. This should not happen.",
    );
    return;
  }

  logger.info(`[ad4m-setup] Downloaded ad4m-executor to: ${binaryPath}`);

  // Continue with managed mode setup using the downloaded binary
  await setupManagedMode(logger, binaryPath, endpoint, executorUrl, wakeToken);
}

// ---------------------------------------------------------------------------
// Config snippet printer
// ---------------------------------------------------------------------------

function printConfigSnippet(
  logger: any,
  mode: "managed" | "external",
  values: Record<string, string | undefined>,
): void {
  const config: Record<string, any> = { mode };

  if (mode === "managed") {
    if (values.ad4mBinaryPath) config.ad4mBinaryPath = values.ad4mBinaryPath;
    if (values.agentPassphrase) config.agentPassphrase = values.agentPassphrase;
  } else {
    if (values.mcpEndpoint) config.mcpEndpoint = values.mcpEndpoint;
    if (values.executorUrl) config.executorUrl = values.executorUrl;
    if (values.token) config.token = values.token;
  }

  if (values.wakeToken) config.wakeToken = values.wakeToken;

  const title =
    mode === "managed"
      ? "AD4M Plugin — Managed Mode Setup"
      : "AD4M Plugin — External Mode Setup";

  logger.info(`[ad4m-setup] ${SEPARATOR}`);
  logger.info(`[ad4m-setup] ${title}`);
  logger.info(
    `[ad4m-setup] Add this to your openclaw.json under plugins.entries["ad4m"].config:`,
  );
  logger.info(`[ad4m-setup]`);

  const snippet = JSON.stringify(config, null, 2);
  for (const line of snippet.split("\n")) {
    logger.info(`[ad4m-setup] ${line}`);
  }

  logger.info(`[ad4m-setup]`);
  logger.info(`[ad4m-setup] one line for copy&paste: ${JSON.stringify(config)}`);
  logger.info(`[ad4m-setup]`);
  logger.info(
    `[ad4m-setup] After adding the config, restart OpenClaw to activate the plugin.`,
  );
  logger.info(`[ad4m-setup] ${SEPARATOR}`);
}
