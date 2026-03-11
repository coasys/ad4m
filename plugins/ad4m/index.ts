/**
 * AD4M OpenClaw Plugin
 *
 * Bridges AD4M's MCP server into OpenClaw by:
 * 1. Connecting to the AD4M executor's Streamable HTTP MCP endpoint
 * 2. Discovering all available tools (including dynamic SHACL-generated ones)
 * 3. Registering each tool with OpenClaw via api.registerTool()
 * 4. Periodically polling for new dynamic tools as perspectives sync SHACL schemas
 * 5. Embedded waker: subscribes to AD4M perspectives via GraphQL WS and
 *    wakes the agent via /hooks/wake when changes are detected
 */

// ---------------------------------------------------------------------------
// Imports
// ---------------------------------------------------------------------------

import path from "path";
import fs from "fs";
import { spawn, execFileSync, ChildProcess } from "child_process";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export interface McpResponse {
  jsonrpc: string;
  id: number;
  result?: any;
  error?: { code: number; message: string; data?: any };
}

export interface McpTool {
  name: string;
  description?: string;
  inputSchema?: Record<string, any>;
}

export interface PluginConfig {
  mode?: "managed" | "external";
  mcpEndpoint?: string;
  /** Auth token — JWT in external mode, admin credential in managed mode (internal). */
  token?: string;
  agentPassphrase?: string;
  ad4mBinaryPath?: string;
  toolRefreshIntervalMs?: number;
  wakerEnabled?: boolean;
  executorWsUrl?: string;
  wakeUrl?: string;
  wakeToken?: string;
  debounceMs?: number;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

export function generateRandomPassphrase(length: number = 32): string {
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  for (let i = 0; i < length; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result;
}

/**
 * Persist plugin config fields to the OpenClaw config file.
 * Uses api.runtime.config to load the full config, patch our plugin entry,
 * and write it back.
 */
export async function updatePluginConfig(
  api: any,
  patch: Partial<PluginConfig>,
  logger?: any,
): Promise<void> {
  try {
    const cfg = api.runtime.config.loadConfig();
    const pluginId = api.id;
    const entries = cfg.plugins?.entries ?? {};
    const existing = entries[pluginId] ?? {};
    const existingConfig = (existing.config ?? {}) as Record<string, unknown>;

    const next = {
      ...cfg,
      plugins: {
        ...cfg.plugins,
        entries: {
          ...entries,
          [pluginId]: {
            ...existing,
            config: { ...existingConfig, ...patch },
          },
        },
      },
    };

    await api.runtime.config.writeConfigFile(next);
    logger?.info?.(`[ad4m] Config persisted: ${Object.keys(patch).join(", ")}`);
  } catch (e: any) {
    logger?.error?.(`[ad4m] Failed to persist config: ${e.message}`);
  }
}

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
    logger.warn(`[ad4m] Could not open log file ${logFilePath}: ${logErr.message}`);
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
      if (executorLogStream) executorLogStream.write(`${new Date().toISOString()} [stdout] ${line}\n`);
    });

    executorProcess.stderr?.on("data", (data: Buffer) => {
      const line = data.toString().trim();
      logger.info(`[ad4m-executor] ${line}`);
      if (executorLogStream) executorLogStream.write(`${new Date().toISOString()} [stderr] ${line}\n`);
    });

    executorProcess.on("error", (err: Error) => {
      spawnFailed = true;
      spawnError = err.message;
      logger.error(`[ad4m] Failed to start executor: ${err.message}`);
      logger.error(`[ad4m] PATH: ${process.env.PATH ?? "(unset)"}`);
      if (executorLogStream) { executorLogStream.end(); executorLogStream = null; }
      executorProcess = null;
    });

    executorProcess.on("exit", (code: number | null) => {
      logger.info(`[ad4m] Executor exited with code ${code}`);
      if (code !== null && code !== 0) {
        spawnFailed = true;
        spawnError = `Executor exited with code ${code}`;
      }
      if (executorLogStream) { executorLogStream.end(); executorLogStream = null; }
      executorProcess = null;
    });

    // Wait for executor to be ready (check spawn failure each iteration)
    logger.info(`[ad4m] Waiting for executor to start...`);
    for (let i = 0; i < 30; i++) {
      await new Promise((r) => setTimeout(r, 1000));

      // If spawn failed (ENOENT, permission, non-zero exit), stop waiting
      if (spawnFailed) {
        logger.error(
          `[ad4m] Executor process failed to start: ${spawnError ?? "unknown error"}`,
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
      `[ad4m] Make sure ad4m-executor is installed. Set ad4mBinaryPath in plugin config to the full path. PATH: ${process.env.PATH ?? "(unset)"}`,
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

export interface WakerSubscription {
  id: string;
  type: "mention" | "channel-messages";
  perspective: string;
  channel: string;
  query: string;
  neighbourhood?: string;
}

// ---------------------------------------------------------------------------
// Agent management (generate / unlock)
// ---------------------------------------------------------------------------

/**
 * Ensure the AD4M agent is initialized and unlocked.
 *
 * Creates a temporary GraphQL WS connection to the executor, checks agent
 * status, and generates (first run) or unlocks (subsequent runs) as needed.
 *
 * Returns the agent DID on success, or null if agent management failed.
 */
export async function ensureAgentReady(
  executorWsUrl: string,
  adminCredential: string,
  logger: any,
  agentPassphrase?: string,
  /** @internal — pass a pre-built Ad4mClient for testing */
  _testClient?: any,
  /** OpenClaw plugin API — for persisting generated passphrase */
  _api?: any,
): Promise<string | null> {
  const MAX_CONNECT_ATTEMPTS = 10;
  const CONNECT_RETRY_DELAY_MS = 2000;

  let wsClient: any = null;
  let client: any = null;

  // Helper: create a fresh WS-backed Ad4mClient
  function createWsClient() {
    const { Ad4mClient } = require("@coasys/ad4m");
    const { ApolloClient, InMemoryCache } = require("@apollo/client/core");
    const { GraphQLWsLink } = require("@apollo/client/link/subscriptions");
    const { createClient } = require("graphql-ws");
    const WebSocket = require("ws");

    // Dispose previous client if any
    if (wsClient) {
      try { wsClient.dispose(); } catch {}
    }

    wsClient = createClient({
      url: executorWsUrl,
      webSocketImpl: WebSocket,
      connectionParams: adminCredential
        ? { headers: { authorization: adminCredential } }
        : {},
      retryAttempts: 0, // We handle retries ourselves in the outer loop
    });

    const wsLink = new GraphQLWsLink(wsClient);
    const apolloClient = new ApolloClient({
      link: wsLink,
      cache: new InMemoryCache(),
      defaultOptions: {
        watchQuery: { fetchPolicy: "no-cache" },
        query: { fetchPolicy: "no-cache" },
        mutate: { fetchPolicy: "no-cache" },
      },
    });

    client = new Ad4mClient(apolloClient);
  }

  try {
    if (_testClient) {
      client = _testClient;
    } else {
      logger.info(`[ad4m] Connecting to executor at ${executorWsUrl} for agent management...`);
    }

    // Retry loop: the GraphQL WS server may not be ready immediately after
    // the MCP endpoint comes up. We retry the initial status check.
    // When _testClient is provided (tests), skip retries — no real WS to wait for.
    const maxAttempts = _testClient ? 1 : MAX_CONNECT_ATTEMPTS;
    let agentStatus: any = null;
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      try {
        if (!_testClient) {
          createWsClient();
        }
        agentStatus = await client.agent.status();
        logger.info(
          `[ad4m] Agent status: initialized=${agentStatus.isInitialized}, unlocked=${agentStatus.isUnlocked}`,
        );
        break; // Connected successfully
      } catch (e: any) {
        if (attempt < maxAttempts) {
          logger.info(
            `[ad4m] WS not ready yet (${e.message}), retrying in ${CONNECT_RETRY_DELAY_MS}ms... (${attempt}/${maxAttempts})`,
          );
          await new Promise((r: any) => setTimeout(r, CONNECT_RETRY_DELAY_MS));
        } else {
          logger.error(`[ad4m] Failed to connect to executor after ${maxAttempts} attempt(s): ${e.message}`);
          return null;
        }
      }
    }

    if (!agentStatus) {
      return null;
    }

    if (!agentStatus.isInitialized) {
      // First run — generate new agent
      const passphrase = agentPassphrase || generateRandomPassphrase(32);
      logger.info(`[ad4m] Agent not initialized, generating new agent...`);
      try {
        await client.agent.generate(passphrase);
        if (_api) await updatePluginConfig(_api, { agentPassphrase: passphrase }, logger);
        // Re-fetch status to get the DID
        const newStatus = await client.agent.status();
        logger.info(
          `[ad4m] Agent generated successfully. DID: ${newStatus.did}`,
        );
        return newStatus.did;
      } catch (e: any) {
        logger.error(`[ad4m] Failed to generate agent: ${e.message}`);
        return null;
      }
    } else if (agentStatus.isUnlocked === false) {
      // Previously initialized but locked — try to unlock
      const passphrase = agentPassphrase;
      if (passphrase) {
        logger.info(`[ad4m] Agent is locked, attempting to unlock...`);
        try {
          await client.agent.unlock(passphrase);
          const newStatus = await client.agent.status();
          logger.info(`[ad4m] Agent unlocked successfully. DID: ${newStatus.did}`);
          return newStatus.did;
        } catch (e: any) {
          logger.error(`[ad4m] Failed to unlock agent: ${e.message}`);
          logger.warn(
            `[ad4m] You may need to provide the correct agentPassphrase in config or reconfigure.`,
          );
          return null;
        }
      } else {
        logger.error(
          `[ad4m] Agent is locked but no passphrase available. Set agentPassphrase in plugin config.`,
        );
        return null;
      }
    } else {
      // Already initialized and unlocked
      logger.info(`[ad4m] Agent is ready. DID: ${agentStatus.did}`);
      return agentStatus.did;
    }
  } catch (err: any) {
    logger.error(`[ad4m] Agent management failed: ${err.message}`);
    logger.error(
      `[ad4m] Make sure @coasys/ad4m and dependencies are installed (npm install in the plugin directory).`,
    );
    return null;
  } finally {
    // Clean up temporary WS connection
    if (wsClient) {
      try {
        wsClient.dispose();
      } catch {
        /* ignore */
      }
    }
  }
}

// ---------------------------------------------------------------------------
// MCP HTTP Client (Streamable HTTP with SSE support)
// ---------------------------------------------------------------------------

let requestIdCounter = 0;

/**
 * Parse an SSE text/event-stream body into the first JSON-RPC message.
 */
export async function parseSSEStream(response: Response): Promise<McpResponse> {
  const reader = response.body?.getReader();
  if (!reader) throw new Error("No response body");

  const decoder = new TextDecoder();
  let buffer = "";

  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      buffer += decoder.decode(value, { stream: true });
      const lines = buffer.split("\n");
      // Keep incomplete last line in buffer
      buffer = lines.pop() ?? "";

      for (const line of lines) {
        const trimmed = line.trim();
        if (trimmed.startsWith("data:")) {
          const payload = trimmed.substring(5).trim();
          if (payload.length > 0) {
            try {
              const parsed = JSON.parse(payload);
              if (parsed.jsonrpc) {
                reader.cancel();
                return parsed as McpResponse;
              }
            } catch {
              // Not valid JSON yet, continue
            }
          }
        }
      }
    }
  } finally {
    reader.releaseLock();
  }

  // Try remaining buffer
  for (const line of buffer.split("\n")) {
    const trimmed = line.trim();
    if (trimmed.startsWith("data:")) {
      const payload = trimmed.substring(5).trim();
      if (payload.length > 0) {
        try {
          return JSON.parse(payload) as McpResponse;
        } catch {
          /* skip */
        }
      }
    }
  }

  throw new Error("SSE stream ended without JSON-RPC data");
}

/**
 * Send a JSON-RPC request to the AD4M MCP server.
 */
export async function mcpRequest(
  endpoint: string,
  method: string,
  params: any = {},
  sessionId?: string,
  authToken?: string,
): Promise<McpResponse> {
  const id = ++requestIdCounter;
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    Accept: "application/json, text/event-stream",
  };
  if (sessionId) headers["Mcp-Session-Id"] = sessionId;
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const response = await fetch(endpoint, {
    method: "POST",
    headers,
    body: JSON.stringify({ jsonrpc: "2.0", id, method, params }),
  });

  if (!response.ok) {
    throw new Error(`MCP HTTP ${response.status}: ${response.statusText}`);
  }

  const ct = response.headers.get("content-type") ?? "";
  if (ct.includes("text/event-stream")) {
    return parseSSEStream(response);
  }
  return (await response.json()) as McpResponse;
}

/**
 * Send a JSON-RPC notification (no id, no response expected).
 */
export async function mcpNotify(
  endpoint: string,
  method: string,
  params: any = {},
  sessionId?: string,
  authToken?: string,
): Promise<void> {
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    Accept: "application/json, text/event-stream",
  };
  if (sessionId) headers["Mcp-Session-Id"] = sessionId;
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  await fetch(endpoint, {
    method: "POST",
    headers,
    body: JSON.stringify({ jsonrpc: "2.0", method, params }),
  });
}

/**
 * Initialize an MCP session: initialize + notifications/initialized handshake.
 */
export async function mcpInitialize(
  endpoint: string,
  authToken?: string,
): Promise<{ sessionId: string; serverInfo: any }> {
  const id = ++requestIdCounter;
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    Accept: "application/json, text/event-stream",
  };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const resp = await fetch(endpoint, {
    method: "POST",
    headers,
    body: JSON.stringify({
      jsonrpc: "2.0",
      id,
      method: "initialize",
      params: {
        protocolVersion: "2024-11-05",
        capabilities: { roots: { listChanged: false } },
        clientInfo: { name: "openclaw-ad4m-plugin", version: "0.1.0" },
      },
    }),
  });

  if (!resp.ok) {
    throw new Error(`MCP initialize HTTP ${resp.status}: ${resp.statusText}`);
  }

  const sessionId = resp.headers.get("mcp-session-id") ?? "";
  const ct = resp.headers.get("content-type") ?? "";
  let result: McpResponse;
  if (ct.includes("text/event-stream")) {
    result = await parseSSEStream(resp);
  } else {
    result = (await resp.json()) as McpResponse;
  }

  if (result.error) {
    throw new Error(`MCP initialize error: ${result.error.message}`);
  }

  // Complete handshake
  await mcpNotify(
    endpoint,
    "notifications/initialized",
    {},
    sessionId,
    authToken,
  );

  return { sessionId, serverInfo: result.result };
}

/**
 * Fetch tool list from the MCP server.
 */
export async function mcpListTools(
  endpoint: string,
  sessionId: string,
  authToken?: string,
): Promise<McpTool[]> {
  const resp = await mcpRequest(
    endpoint,
    "tools/list",
    {},
    sessionId,
    authToken,
  );
  return resp.result?.tools ?? [];
}

/**
 * Call an MCP tool and return the result.
 */
export async function mcpCallTool(
  endpoint: string,
  toolName: string,
  args: Record<string, any>,
  sessionId: string,
  authToken?: string,
): Promise<any> {
  const resp = await mcpRequest(
    endpoint,
    "tools/call",
    { name: toolName, arguments: args },
    sessionId,
    authToken,
  );

  if (resp.error) {
    return {
      content: [
        { type: "text", text: JSON.stringify({ error: resp.error.message }) },
      ],
    };
  }

  return resp.result;
}

/**
 * Extract text from an MCP tool result. Handles both { content: [{ text }] }
 * and raw string results, parsing JSON if possible.
 */
export function extractMcpResultData(result: any): any {
  let text = result;
  if (result?.content?.[0]?.text) {
    text = result.content[0].text;
  }
  if (typeof text === "string") {
    try {
      return JSON.parse(text);
    } catch {
      return text;
    }
  }
  return text;
}

// ---------------------------------------------------------------------------
// Waker helpers
// ---------------------------------------------------------------------------

export function buildWakeMessage(
  config: PluginConfig,
  sub: WakerSubscription,
  agentDid: string,
  parent: string,
): string {
  const event =
    sub.type === "mention"
      ? "You were @mentioned in an AD4M neighbourhood."
      : "New messages in an AD4M neighbourhood.";

  return [
    event,
    "Read the AD4M skill for instructions on how to handle this.",
    "",
    `MCP endpoint: ${config.mcpEndpoint ?? "http://localhost:3001/mcp"}`,
    `Agent DID: ${agentDid}`,
    `Perspective: ${sub.perspective}`,
    parent ? `Parent: ${parent}` : null,
    sub.neighbourhood ? `Neighbourhood: ${sub.neighbourhood}` : null,
    `Subscription: ${sub.id}`,
    `Event type: ${sub.type}`,
  ]
    .filter(Boolean)
    .join("\n");
}

export async function postWake(
  config: PluginConfig,
  sub: WakerSubscription,
  agentDid: string,
  logger: any,
  parentChannel?: string,
): Promise<void> {
  const effectiveChannel = parentChannel || sub.channel;
  const message = buildWakeMessage(config, sub, agentDid, effectiveChannel);
  const body = JSON.stringify({ text: message, mode: "now" });

  try {
    const resp = await fetch(config.wakeUrl!, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${config.wakeToken}`,
      },
      body,
      signal: AbortSignal.timeout(5000),
    });
    if (!resp.ok) {
      logger.error(`[ad4m-waker] wake POST failed: ${resp.status}`);
    } else {
      logger.info(`[ad4m-waker] wake POST sent successfully`);
    }
  } catch (e: any) {
    logger.error(`[ad4m-waker] wake POST error: ${e.message}`);
  }
}

// ---------------------------------------------------------------------------
// Plugin Export
// ---------------------------------------------------------------------------

export default async function ad4mPlugin(api: any) {
  const logger = api.logger;

  const providedConfig: PluginConfig = (api.pluginConfig as PluginConfig) ?? {};
  const mode = providedConfig.mode || "managed";

  // Determine endpoint - default to localhost for managed, use provided for external
  const endpoint = providedConfig.mcpEndpoint ?? "http://localhost:3001/mcp";

  // Resolve executorWsUrl once — used by both ensureAgentReady and waker service
  const executorWsUrl =
    providedConfig.executorWsUrl ?? "ws://localhost:12000/graphql";

  // Determine auth token based on mode
  let authToken: string = "";
  let pluginAgentDid: string = "";

  updatePluginConfig(api, { 
    ...providedConfig,
    mode: providedConfig.mode ?? "managed", 
    agentPassphrase: providedConfig.agentPassphrase ?? "PLEASE_FILL_IN_FOR_PRE_EXISTING_AD4M_KEYS",
    ad4mBinaryPath: providedConfig.ad4mBinaryPath ?? findExecutorBinary() ?? "PLEASE_POINT_TO_AD4M_EXECUTOR_BINARY"
  }, logger);

  if (mode === "external") {
    // External mode: use token from config (JWT from setup), or request new JWT
    authToken = providedConfig.token || "";
    if (!authToken) {
      // Try to obtain JWT via MCP capability request (auto-permits)
      logger.info("[ad4m] No token found, attempting JWT auth via MCP...");
      try {
        const initResp = await mcpInitialize(endpoint);
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
          const jwtResult = await mcpCallTool(
            endpoint,
            "generate_jwt",
            { request_id: capData.request_id, code: capData.code },
            initResp.sessionId,
          );
          const jwtData = extractMcpResultData(jwtResult);
          if (jwtData?.token) {
            authToken = jwtData.token;
            await updatePluginConfig(api, { token: jwtData.token }, logger);
            logger.info("[ad4m] JWT obtained and stored in config");
          }
        }
      } catch (e: any) {
        logger.warn(`[ad4m] Auto JWT auth failed: ${e.message}. Tools may not work until authenticated.`);
      }
    }
  } else {
    // Managed mode: generate ephemeral admin credential (not persisted)
    const adminCredential = generateRandomPassphrase(24);
    authToken = adminCredential;

    // Resolve executor binary path: config > discover > bare name
    let binaryPath = providedConfig.ad4mBinaryPath;
    if (!binaryPath) {
      logger.info(`[ad4m] Searching for ad4m-executor binary...`);
      const discovered = findExecutorBinary();
      if (discovered) {
        logger.info(`[ad4m] Found ad4m-executor at: ${discovered}`);
        binaryPath = discovered;
      } else {
        logger.warn(
          `[ad4m] ad4m-executor not found in PATH or common locations. Will try bare name.`,
        );
      }
    } else {
      logger.info(`[ad4m] Using executor binary: ${binaryPath}`);
    }

    // Generate passphrase if not already in config
    const agentPassphrase = providedConfig.agentPassphrase || generateRandomPassphrase(32);

    // Write managed config on first run (or whenever fields are missing)
    if (!providedConfig.mode || !providedConfig.ad4mBinaryPath || !providedConfig.agentPassphrase) {
      const configToWrite: Partial<PluginConfig> = {
        mode: "managed",
      };
      if (binaryPath) configToWrite.ad4mBinaryPath = binaryPath;
      if (!providedConfig.agentPassphrase) configToWrite.agentPassphrase = agentPassphrase;
      await updatePluginConfig(api, configToWrite, logger);
    }

    // Use the resolved passphrase for agent management below
    providedConfig.agentPassphrase = agentPassphrase;

    // Ensure executor is running (spawn if not)
    const executorStartResult = await ensureExecutorRunning(
      adminCredential,
      logger,
      endpoint,
      executorWsUrl,
      binaryPath,
    );
    if (!executorStartResult) {
      logger.error(
        `[ad4m] Failed to start executor in managed mode. Set ad4mBinaryPath in plugin config if ad4m-executor is not in PATH.`,
      );
    } else if (executorStartResult === "already_running") {
      // Executor was already running (e.g. from Launcher or previous install).
      // Our ephemeral credential won't match — obtain a JWT instead.
      // Agent should already be initialized; we only unlock, never generate.
      logger.info("[ad4m] Executor was already running — obtaining JWT for auth...");
      try {
        const initResp = await mcpInitialize(endpoint);
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
          const jwtResult = await mcpCallTool(
            endpoint,
            "generate_jwt",
            { request_id: capData.request_id, code: capData.code },
            initResp.sessionId,
          );
          const jwtData = extractMcpResultData(jwtResult);
          if (jwtData?.token) {
            authToken = jwtData.token;
            logger.info("[ad4m] JWT obtained for pre-existing executor");
          }
        }
      } catch (e: any) {
        logger.warn(`[ad4m] JWT auth failed for pre-existing executor: ${e.message}`);
        // Can't authenticate with a pre-existing executor — MCP tools
        // will attempt without auth; agent management is skipped.
        authToken = "";
      }

      if (authToken) {
        const agentDid = await ensureAgentReady(
          executorWsUrl,
          authToken,
          logger,
          providedConfig.agentPassphrase,
        );
        if (agentDid) {
          pluginAgentDid = agentDid;
        } else {
          logger.warn("[ad4m] Could not verify agent on pre-existing executor.");
        }
      }
    } else {
      // We spawned the executor with our admin credential — use it directly.
      // ensureAgentReady may generate the agent on first run.
      const agentDid = await ensureAgentReady(
        executorWsUrl,
        adminCredential,
        logger,
        providedConfig.agentPassphrase,
        undefined, // _testClient
        api,       // for persisting generated passphrase
      );
      if (agentDid) {
        pluginAgentDid = agentDid;
      } else {
        logger.error(
          `[ad4m] Agent management failed. MCP tools and waker will not work correctly.`,
        );
      }
    }
  }

  // Resolve wakeToken: plugin config override > OpenClaw global hooks config
  let resolvedWakeToken = providedConfig.wakeToken;
  if (!resolvedWakeToken) {
    try {
      const globalConfig = api.config;
      resolvedWakeToken = (globalConfig as any)?.hooks?.token;
      if (resolvedWakeToken) {
        logger.info("[ad4m] Resolved wakeToken from OpenClaw hooks config");
      }
    } catch {
      // api.config may not expose hooks token
    }
  }

  const config: PluginConfig = {
    ...providedConfig,
    mode,
    mcpEndpoint: endpoint,
    token: authToken || undefined,
    executorWsUrl,
    wakeUrl: providedConfig.wakeUrl ?? "http://localhost:18789/hooks/wake",
    wakeToken: resolvedWakeToken,
    debounceMs: providedConfig.debounceMs ?? 2000,
  };

  if (authToken) {
    logger.info(`[ad4m] Auth token ready (length: ${authToken.length})`);
  } else {
    logger.warn(`[ad4m] No auth token. Tools may not work until authenticated.`);
  }

  // -- Setup helper tool --

  api.registerTool({
    name: "ad4m_get_sample_config",
    description:
      "Get a sample OpenClaw plugin config for AD4M. Use this to see the required configuration format.",
    parameters: { type: "object", properties: {} },
    async execute() {
      const managedConfig = {
        mode: "managed",
      };
      const externalConfig = {
        mode: "external",
        mcpEndpoint: "http://their-executor:3001/mcp",
      };
      return {
        content: [
          {
            type: "text",
            text: `# Managed mode (default):
${JSON.stringify({ ad4m: managedConfig }, null, 2)}

# External mode (connect to existing executor):
${JSON.stringify({ ad4m: externalConfig }, null, 2)}

Notes:
- Managed: auto-starts executor, auto-generates credentials
- External: provide executor URL, uses executor's auth
- Credentials stored in ~/.ad4m-plugin/ for reuse
- If executor is not in PATH, set ad4mBinaryPath to the full path (e.g. /usr/local/bin/ad4m-executor)
- Find the binary: which ad4m-executor || find / -name ad4m-executor -type f 2>/dev/null`,
          },
        ],
      };
    },
  });

  // Debug: log config values
  logger.info(
    `[ad4m] Config received: ${JSON.stringify({
      mcpEndpoint: config.mcpEndpoint,
      token: config.token
        ? "*** (length: " + config.token.length + ")"
        : "UNDEFINED",
      wakeUrl: config.wakeUrl,
      wakeToken: config.wakeToken
        ? "*** (length: " + config.wakeToken.length + ")"
        : "UNDEFINED",
      wakerEnabled: config.wakerEnabled,
      executorWsUrl: config.executorWsUrl,
    })}`,
  );

  // -- MCP bridge state --
  let sessionId = "";
  let registeredTools = new Set<string>();
  let refreshTimer: ReturnType<typeof setInterval> | null = null;

  // -- Waker state --
  let wakerClient: any = null;
  const wakerProxies = new Map<string, any>(); // id -> QuerySubscriptionProxy
  const wakerDebounceTimers = new Map<string, ReturnType<typeof setTimeout>>();
  const wakerSubscriptions = new Map<string, WakerSubscription>();

  /**
   * (Re-)initialize the MCP session. Called on first connect and when the
   * session becomes invalid (e.g. executor restart, session expiry → 422).
   */
  async function ensureSession(): Promise<string> {
    if (sessionId) return sessionId;
    logger.info(`[ad4m] Initializing MCP session at ${endpoint}`);
    const init = await mcpInitialize(endpoint, authToken);
    sessionId = init.sessionId;
    logger.info(`[ad4m] MCP session established (id: ${sessionId})`);
    return sessionId;
  }

  /**
   * Drop the current session so the next ensureSession() re-initializes.
   */
  function invalidateSession(): void {
    sessionId = "";
  }

  /**
   * Call an MCP tool with automatic session recovery.
   * If the call fails with a 4xx (likely 422 = invalid session), re-initialize
   * the session once and retry.
   */
  async function callToolWithRetry(
    toolName: string,
    args: Record<string, any>,
  ): Promise<any> {
    await ensureSession();
    try {
      return await mcpCallTool(endpoint, toolName, args, sessionId, authToken);
    } catch (err: any) {
      if (err.message && /MCP HTTP 4\d\d/.test(err.message)) {
        logger.info(
          `[ad4m] Session error calling ${toolName}, re-initializing...`,
        );
        invalidateSession();
        await ensureSession();
        return await mcpCallTool(
          endpoint,
          toolName,
          args,
          sessionId,
          authToken,
        );
      }
      throw err;
    }
  }

  /**
   * Convert an MCP tool definition's inputSchema to OpenClaw parameters format.
   * Strips `$schema` (and `$id`) because AD4M's schemars 1.0 emits
   * draft 2020-12 which OpenClaw's validator doesn't recognise.
   */
  function toParameters(tool: McpTool): Record<string, any> {
    if (!tool.inputSchema) {
      return { type: "object", properties: {}, required: [] };
    }
    const { $schema, $id, ...rest } = tool.inputSchema as any;
    return rest;
  }

  /**
   * Register a single MCP tool with OpenClaw.
   */
  // Tools that create/join neighbourhoods — auto-subscribe to mentions after success.
  const NEIGHBOURHOOD_TOOLS = new Set([
    "neighbourhood_join_from_url",
    "neighbourhood_publish_from_perspective",
  ]);

  /**
   * Extract the perspective UUID from a successful neighbourhood tool result.
   * Returns null if the result doesn't look like a success or has no UUID.
   */
  function extractPerspectiveUuid(
    toolName: string,
    result: any,
  ): string | null {
    // The result may be wrapped in MCP content format
    let data = result;
    if (result?.content?.[0]?.text) {
      try {
        data = JSON.parse(result.content[0].text);
      } catch {
        return null;
      }
    }
    if (!data?.success) return null;

    // neighbourhood_join_from_url returns { perspective_uuid }
    // neighbourhood_publish_from_perspective returns { perspective_uuid }
    return data.perspective_uuid ?? null;
  }

  function registerMcpTool(tool: McpTool) {
    if (registeredTools.has(tool.name)) return;

    const isNeighbourhoodTool = NEIGHBOURHOOD_TOOLS.has(tool.name);

    api.registerTool({
      name: tool.name,
      description: tool.description ?? `AD4M MCP tool: ${tool.name}`,
      parameters: toParameters(tool),
      async execute(_id: string, params: Record<string, any>) {
        try {
          const result = await callToolWithRetry(tool.name, params);

          // Auto-subscribe to mentions when a neighbourhood is joined or published
          if (isNeighbourhoodTool) {
            const perspId = extractPerspectiveUuid(tool.name, result);
            if (perspId) {
              // Fire and forget — don't block the tool response
              autoSubscribeMentions(perspId);
            }
          }

          if (result?.content) return result;
          return { content: [{ type: "text", text: JSON.stringify(result) }] };
        } catch (err: any) {
          return { error: err.message };
        }
      },
    });

    registeredTools.add(tool.name);
  }

  /**
   * Fetch tools from MCP and register any new ones.
   * Automatically re-initializes the session on 4xx errors.
   */
  async function refreshTools(): Promise<number> {
    try {
      await ensureSession();
      try {
        const tools = await mcpListTools(endpoint, sessionId, authToken);
        let newCount = 0;
        for (const tool of tools) {
          if (!registeredTools.has(tool.name)) {
            registerMcpTool(tool);
            newCount++;
          }
        }
        if (newCount > 0) {
          logger.info(
            `[ad4m] Registered ${newCount} new tool(s), total: ${registeredTools.size}`,
          );
        }
        return newCount;
      } catch (err: any) {
        // Session error → re-initialize and retry once
        if (err.message && /MCP HTTP 4\d\d/.test(err.message)) {
          logger.info(
            `[ad4m] Session error during tool refresh, re-initializing...`,
          );
          invalidateSession();
          await ensureSession();
          const tools = await mcpListTools(endpoint, sessionId, authToken);
          let newCount = 0;
          for (const tool of tools) {
            if (!registeredTools.has(tool.name)) {
              registerMcpTool(tool);
              newCount++;
            }
          }
          if (newCount > 0) {
            logger.info(
              `[ad4m] Registered ${newCount} new tool(s), total: ${registeredTools.size}`,
            );
          }
          return newCount;
        }
        throw err;
      }
    } catch (err: any) {
      logger.warn(`[ad4m] Tool refresh failed: ${err.message}`);
      return 0;
    }
  }

  /**
   * Create a live waker subscription from a WakerSubscription config.
   * Requires the waker service to be running (wakerClient set).
   */
  async function createLiveSubscription(sub: WakerSubscription): Promise<void> {
    if (!wakerClient) {
      throw new Error(
        "Waker service not connected. Ensure ad4m-executor is running and wakerEnabled is true.",
      );
    }

    // Dispose existing subscription with same id if any
    disposeLiveSubscription(sub.id);

    const { QuerySubscriptionProxy } = require("@coasys/ad4m");
    const debounceMs = config.debounceMs ?? 2000;

    const proxy = new QuerySubscriptionProxy(
      sub.perspective,
      sub.query,
      wakerClient.perspective,
    );
    proxy.isSurrealDB = true;
    await proxy.subscribe();
    await proxy.initialized;

    let lastResultHash: string | null = null;

    proxy.onResult(async (result: any) => {
      const serialized = JSON.stringify(result);
      if (lastResultHash === serialized) return;
      lastResultHash = serialized;

      const count = Array.isArray(result) ? result.length : "?";
      logger.info(
        `[ad4m-waker] ${sub.id}: query result changed (${count} items)`,
      );

      // Determine the parent from the result
      let parentChannel = sub.channel;

      // For mention subscriptions, the query returns has_child links pointing to messages with mentions
      // The source of each has_child link is the parent (channel), target is the message
      if (
        !parentChannel &&
        sub.type === "mention" &&
        Array.isArray(result) &&
        result.length > 0
      ) {
        // Each result is a has_child link where the target is a message with a mention
        // The source is the parent (channel)
        const parentLink = result.find(
          (link: any) =>
            link && link.predicate === "ad4m://has_child" && link.source,
        );

        if (parentLink) {
          parentChannel = parentLink.source;
          logger.info(
            `[ad4m-waker] ${sub.id}: found parent ${parentChannel} from has_child link`,
          );
        }
      }

      const existing = wakerDebounceTimers.get(sub.id);
      if (existing) clearTimeout(existing);

      wakerDebounceTimers.set(
        sub.id,
        setTimeout(() => {
          postWake(config, sub, pluginAgentDid, logger, parentChannel);
          wakerDebounceTimers.delete(sub.id);
        }, debounceMs),
      );
    });

    wakerProxies.set(sub.id, proxy);
    wakerSubscriptions.set(sub.id, sub);

    logger.info(
      `[ad4m-waker] Subscription ${sub.id} active (type=${sub.type}, perspective=${sub.perspective})`,
    );
  }

  /**
   * Dispose a single live subscription.
   */
  function disposeLiveSubscription(id: string): void {
    const proxy = wakerProxies.get(id);
    if (proxy) {
      try {
        proxy.dispose();
      } catch {
        /* ignore */
      }
      wakerProxies.delete(id);
    }
    const timer = wakerDebounceTimers.get(id);
    if (timer) {
      clearTimeout(timer);
      wakerDebounceTimers.delete(id);
    }
    wakerSubscriptions.delete(id);
  }

  // =========================================================================
  // Tools
  // =========================================================================

  // -- Manual refresh tool --

  api.registerTool({
    name: "refresh_ad4m_tools",
    description:
      "Re-fetch the AD4M MCP tool list and register any new tools. " +
      "Call this after add_model, adding SHACL subject classes, or joining a neighbourhood " +
      "to immediately discover new dynamic tools without waiting for the next poll cycle.",
    parameters: { type: "object", properties: {}, required: [] },
    async execute() {
      const newCount = await refreshTools();
      const msg =
        newCount > 0
          ? `Discovered and registered ${newCount} new tool(s). Total tools: ${registeredTools.size}.`
          : `No new tools found. Total tools: ${registeredTools.size}.`;
      return { content: [{ type: "text", text: msg }] };
    },
  });

  // -- Waker subscription tools --

  /**
   * Subscribe to mentions for a perspective (neighbourhood).
   * Reusable by both the explicit tool and the auto-subscription hooks.
   * Returns a human-readable status string, or throws on failure.
   */
  async function subscribeToMentionsForPerspective(
    perspectiveId: string,
  ): Promise<string> {
    // Skip if already subscribed
    const existingId = `mention-${perspectiveId.substring(0, 8)}`;
    if (wakerSubscriptions.has(existingId)) {
      return `Already subscribed to mentions in perspective ${perspectiveId}.`;
    }

    const result = await callToolWithRetry("get_mention_waker_config", {
      perspective_id: perspectiveId,
    });
    const data = extractMcpResultData(result);

    if (data?.error) {
      throw new Error(data.error);
    }

    const subscription: WakerSubscription = {
      id: data.subscription?.id ?? existingId,
      type: "mention",
      perspective: perspectiveId,
      channel: "",
      query: data.query ?? data.subscription?.query,
      neighbourhood: data.neighbourhood,
    };

    if (!subscription.query) {
      throw new Error(
        "MCP tool did not return a query. Is the perspective accessible?",
      );
    }

    await createLiveSubscription(subscription);

    return (
      `Subscribed to mentions in perspective ${perspectiveId}. ` +
      `Subscription ID: ${subscription.id}. ` +
      `Watching for: ${(data.names ?? []).join(", ")} and DID ${data.did ?? "unknown"}.`
    );
  }

  /**
   * Auto-subscribe to mentions after a neighbourhood is joined or published.
   * Best-effort: logs errors but never throws.
   */
  async function autoSubscribeMentions(perspectiveId: string): Promise<void> {
    if (!wakerClient) {
      logger.info(
        `[ad4m-waker] Skipping auto-subscribe for ${perspectiveId} — waker not connected`,
      );
      return;
    }
    try {
      const msg = await subscribeToMentionsForPerspective(perspectiveId);
      logger.info(`[ad4m-waker] Auto-subscribed: ${msg}`);
    } catch (err: any) {
      logger.error(
        `[ad4m-waker] Auto-subscribe to mentions failed for ${perspectiveId}: ${err.message}`,
      );
    }
  }

  api.registerTool({
    name: "subscribe_to_mentions",
    description:
      "Subscribe to mentions of this agent in a neighbourhood. " +
      "Creates a live waker subscription that watches for messages mentioning " +
      "your name or DID and wakes you when detected. " +
      "Note: this is called automatically when you join or publish a neighbourhood.",
    parameters: {
      type: "object",
      properties: {
        perspective_id: {
          type: "string",
          description: "Local perspective UUID of the neighbourhood",
        },
      },
      required: ["perspective_id"],
    },
    async execute(_id: string, params: { perspective_id: string }) {
      try {
        const msg = await subscribeToMentionsForPerspective(
          params.perspective_id,
        );
        return { content: [{ type: "text", text: msg }] };
      } catch (err: any) {
        return { content: [{ type: "text", text: `Error: ${err.message}` }] };
      }
    },
  });

  api.registerTool({
    name: "unsubscribe_from_mentions",
    description: "Remove the mention subscription for a neighbourhood.",
    parameters: {
      type: "object",
      properties: {
        perspective_id: {
          type: "string",
          description: "Local perspective UUID",
        },
      },
      required: ["perspective_id"],
    },
    async execute(_id: string, params: { perspective_id: string }) {
      // Find subscription by perspective
      let found = false;
      for (const [id, sub] of wakerSubscriptions) {
        if (
          sub.perspective === params.perspective_id &&
          sub.type === "mention"
        ) {
          disposeLiveSubscription(id);
          found = true;
          break;
        }
      }
      return {
        content: [
          {
            type: "text",
            text: found
              ? `Unsubscribed from mentions in perspective ${params.perspective_id}.`
              : `No mention subscription found for perspective ${params.perspective_id}.`,
          },
        ],
      };
    },
  });

  api.registerTool({
    name: "subscribe_to_children",
    description:
      "Subscribe to new children (e.g., messages) under a specific parent (e.g., a channel). " +
      "Creates a live waker subscription that watches for new child links and wakes you when detected. " +
      "Call this to monitor a specific channel for all messages.",
    parameters: {
      type: "object",
      properties: {
        perspective_id: {
          type: "string",
          description: "Local perspective UUID",
        },
        expression_address: {
          type: "string",
          description: "Parent expression address (e.g., channel ID)",
        },
      },
      required: ["perspective_id", "expression_address"],
    },
    async execute(
      _id: string,
      params: { perspective_id: string; expression_address: string },
    ) {
      try {
        // Call the MCP tool to generate the waker query
        const result = await callToolWithRetry(
          "generate_waker_query",
          {
            perspective_id: params.perspective_id,
            class_name: "Message",
            parent_address: params.expression_address,
          },
        );
        const data = extractMcpResultData(result);

        if (data?.error) {
          return { content: [{ type: "text", text: `Error: ${data.error}` }] };
        }

        const subId =
          data.waker_config?.id ??
          data.subscription_id ??
          `children-${params.perspective_id.substring(0, 8)}`;

        const subscription: WakerSubscription = {
          id: subId,
          type: "channel-messages",
          perspective: params.perspective_id,
          channel: params.expression_address,
          query: data.surreal_query ?? data.waker_config?.query,
        };

        if (!subscription.query) {
          return {
            content: [
              { type: "text", text: "Error: MCP tool did not return a query." },
            ],
          };
        }

        await createLiveSubscription(subscription);

        return {
          content: [
            {
              type: "text",
              text:
                `Subscribed to children of ${params.expression_address} in perspective ${params.perspective_id}. ` +
                `Subscription ID: ${subscription.id}.`,
            },
          ],
        };
      } catch (err: any) {
        return { content: [{ type: "text", text: `Error: ${err.message}` }] };
      }
    },
  });

  api.registerTool({
    name: "unsubscribe_from_children",
    description:
      "Remove the child subscription for a specific parent in a perspective.",
    parameters: {
      type: "object",
      properties: {
        perspective_id: {
          type: "string",
          description: "Local perspective UUID",
        },
        expression_address: {
          type: "string",
          description: "Parent expression address (e.g., channel ID)",
        },
      },
      required: ["perspective_id", "expression_address"],
    },
    async execute(
      _id: string,
      params: { perspective_id: string; expression_address: string },
    ) {
      let found = false;
      for (const [id, sub] of wakerSubscriptions) {
        if (
          sub.perspective === params.perspective_id &&
          sub.channel === params.expression_address &&
          sub.type === "channel-messages"
        ) {
          disposeLiveSubscription(id);
          found = true;
          break;
        }
      }
      return {
        content: [
          {
            type: "text",
            text: found
              ? `Unsubscribed from children of ${params.expression_address} in perspective ${params.perspective_id}.`
              : `No child subscription found for ${params.expression_address} in perspective ${params.perspective_id}.`,
          },
        ],
      };
    },
  });

  api.registerTool({
    name: "list_waker_subscriptions",
    description: "List all active waker subscriptions.",
    parameters: { type: "object", properties: {}, required: [] },
    async execute() {
      const subs = Array.from(wakerSubscriptions.values());
      if (subs.length === 0) {
        return {
          content: [{ type: "text", text: "No active waker subscriptions." }],
        };
      }
      const summary = subs
        .map(
          (s) =>
            `- ${s.id} (${s.type}) perspective=${s.perspective}${s.channel ? ` channel=${s.channel}` : ""}`,
        )
        .join("\n");
      return {
        content: [
          {
            type: "text",
            text: `Active subscriptions (${subs.length}):\n${summary}`,
          },
        ],
      };
    },
  });

  // =========================================================================
  // Background Services
  // =========================================================================

  // -- MCP bridge service --

  api.registerService({
    id: "ad4m-mcp",
    async start() {
      logger.info(`[ad4m] Connecting to AD4M MCP at ${endpoint}`);

      try {
        await ensureSession();

        // Initial tool discovery
        await refreshTools();
        logger.info(
          `[ad4m] Registered ${registeredTools.size} initial tool(s)`,
        );

        // Start periodic polling for dynamic SHACL tools
        const refreshInterval = config.toolRefreshIntervalMs ?? 30000;
        refreshTimer = setInterval(() => {
          refreshTools();
        }, refreshInterval);
        logger.info(`[ad4m] Dynamic tool polling every ${refreshInterval}ms`);
      } catch (err: any) {
        logger.error(`[ad4m] Failed to connect to AD4M MCP: ${err.message}`);
        logger.error(
          `[ad4m] Make sure ad4m-executor is running with --enable-mcp true and the credential is correct.`,
        );
      }
    },
    stop() {
      if (refreshTimer) {
        clearInterval(refreshTimer);
        refreshTimer = null;
      }
      stopExecutor(logger);
      logger.info("[ad4m] AD4M MCP service stopped");
    },
  });

  // -- Waker service --

  api.registerService({
    id: "ad4m-waker",
    async start() {
      const wakerEnabled = config.wakerEnabled ?? true;
      if (!wakerEnabled) {
        logger.info("[ad4m-waker] Waker disabled via config");
        return;
      }

      if (!config.wakeToken) {
        logger.info(
          "[ad4m-waker] wakeToken not configured. Set wakeToken in plugin config to enable the waker. Skipping.",
        );
        return;
      }

      const executorWsUrl =
        config.executorWsUrl ?? "ws://localhost:12000/graphql";
      const token = authToken;

      try {
        // Dynamic imports to avoid load-time issues with @holochain/client transitive deps
        const { Ad4mClient } = require("@coasys/ad4m");
        const { ApolloClient, InMemoryCache } = require("@apollo/client/core");
        const { GraphQLWsLink } = require("@apollo/client/link/subscriptions");
        const { createClient } = require("graphql-ws");
        const WebSocket = require("ws");

        logger.info(`[ad4m-waker] Connecting to ${executorWsUrl}`);

        const wsClient = createClient({
          url: executorWsUrl,
          webSocketImpl: WebSocket,
          connectionParams: token ? { headers: { authorization: token } } : {},
          retryAttempts: Infinity,
          retryWait: async (retries: number) => {
            const delay = Math.min(1000 * Math.pow(2, retries), 30000);
            logger.info(
              `[ad4m-waker] reconnecting in ${delay}ms (attempt ${retries + 1})...`,
            );
            await new Promise((r: any) => setTimeout(r, delay));
          },
        });

        const wsLink = new GraphQLWsLink(wsClient);
        const apolloClient = new ApolloClient({
          link: wsLink,
          cache: new InMemoryCache(),
          defaultOptions: {
            watchQuery: { fetchPolicy: "no-cache" },
            query: { fetchPolicy: "no-cache" },
            mutate: { fetchPolicy: "no-cache" },
          },
        });

        wakerClient = new Ad4mClient(apolloClient);

        // Verify connection and get agent DID (agent should already be
        // initialized/unlocked by ensureAgentReady during plugin init)
        const status = await wakerClient.agent.status();
        if (!status.isInitialized || status.isUnlocked === false) {
          logger.error(
            `[ad4m-waker] Agent is not ready (initialized=${status.isInitialized}, unlocked=${status.isUnlocked}). Agent management should have run during plugin init.`,
          );
          wakerClient = null;
          return;
        }
        pluginAgentDid = status.did;
        logger.info(
          `[ad4m-waker] Connected — agent: ${status.did.substring(0, 40)}...`,
        );
      } catch (err: any) {
        logger.error(`[ad4m-waker] Failed to connect: ${err.message}`);
        logger.error(
          `[ad4m-waker] Make sure @coasys/ad4m and dependencies are installed (npm install in the plugin directory).`,
        );
        wakerClient = null;
      }
    },
    stop() {
      for (const [id] of wakerProxies) {
        disposeLiveSubscription(id);
      }
      wakerClient = null;
      logger.info("[ad4m-waker] Waker service stopped");
    },
  });
}
