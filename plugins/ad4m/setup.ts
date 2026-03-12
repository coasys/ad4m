import fs from "fs";
import path from "path";
import { generateRandomPassphrase } from "./config";
import {
  findExecutorBinary,
  isExecutorRunning,
  ensureExecutorRunning,
  stopExecutor,
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
 * Called from the ad4m-mcp service `start()` when `providedConfig.mode` is
 * not set (i.e., the user hasn't configured the plugin yet).
 *
 * Gathers information (binary path, executor state, agent), performs
 * initial setup where possible, and prints a config snippet for the user
 * to copy-paste into their openclaw.json.
 */
export async function runSetup(
  api: any,
  logger: any,
  endpoint: string = "http://localhost:3001/mcp",
  executorWsUrl: string = "ws://localhost:12000/graphql",
): Promise<void> {
  logger.info("[ad4m-setup] Starting first-run setup...");

  // ── Step 1: Read wakeToken ──
  let wakeToken: string | undefined;
  try {
    const globalConfig = api.config;
    wakeToken = (globalConfig as any)?.hooks?.token;
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
  const running = await isExecutorRunning(endpoint);

  // ── Branch routing ──

  if (running) {
    // Branch B: Executor already running
    await setupExternalMode(logger, endpoint, wakeToken);
  } else if (binaryPath) {
    // Branch A: No running executor, binary found
    await setupManagedMode(logger, binaryPath, endpoint, executorWsUrl, wakeToken);
  } else {
    // Branch C: No binary, no executor
    setupNoExecutor(logger, wakeToken);
  }
}

// ---------------------------------------------------------------------------
// Branch A: Managed mode — start executor, generate agent, print config
// ---------------------------------------------------------------------------

async function setupManagedMode(
  logger: any,
  binaryPath: string,
  endpoint: string,
  executorWsUrl: string,
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
    executorWsUrl,
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
    executorWsUrl,
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
): Promise<void> {
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

// ---------------------------------------------------------------------------
// Branch C: No executor, no binary
// ---------------------------------------------------------------------------

function setupNoExecutor(logger: any, wakeToken?: string): void {
  logger.info(
    "[ad4m-setup] No ad4m-executor binary found and no running executor detected.",
  );
  logger.info(
    "[ad4m-setup] To use this plugin, either:",
  );
  logger.info(
    "[ad4m-setup]   1. Install ad4m-executor and restart OpenClaw (managed mode)",
  );
  logger.info(
    "[ad4m-setup]   2. Start an executor manually and configure external mode",
  );
  logger.info(
    "[ad4m-setup]   3. Set ad4mBinaryPath in plugin config to the full path",
  );

  printConfigSnippet(logger, "managed", {
    ad4mBinaryPath: "<path-to-ad4m-executor>",
    agentPassphrase: "<will-be-shown-after-setup>",
    wakeToken,
  });
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
    `[ad4m-setup] Add this to your openclaw.json under plugins.entries["ad4m-openclaw-plugin"].config:`,
  );
  logger.info(`[ad4m-setup]`);

  const snippet = JSON.stringify(config, null, 2);
  for (const line of snippet.split("\n")) {
    logger.info(`[ad4m-setup] ${line}`);
  }

  logger.info(`[ad4m-setup]`);
  logger.info(
    `[ad4m-setup] After adding the config, restart OpenClaw to activate the plugin.`,
  );
  logger.info(`[ad4m-setup] ${SEPARATOR}`);
}
