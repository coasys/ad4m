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
import { spawn, ChildProcess } from "child_process";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

interface McpResponse {
  jsonrpc: string;
  id: number;
  result?: any;
  error?: { code: number; message: string; data?: any };
}

interface McpTool {
  name: string;
  description?: string;
  inputSchema?: Record<string, any>;
}

interface PluginConfig {
  mode?: "managed" | "external";
  mcpEndpoint?: string;
  adminCredential?: string;
  agentPassphrase?: string;
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

function generateRandomPassphrase(length: number = 32): string {
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  for (let i = 0; i < length; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result;
}

function getPluginDataPath(): string {
  // Use HOME/.ad4m-plugin as storage location
  const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
  return path.join(home, ".ad4m-plugin");
}

function ensurePluginDataDir(): string {
  const dataPath = getPluginDataPath();
  try {
    if (!fs.existsSync(dataPath)) {
      fs.mkdirSync(dataPath, { recursive: true });
    }
  } catch (e) {
    // Ignore - might not have fs access
  }
  return dataPath;
}

function getStoredPassphrase(): string | null {
  try {
    const dataPath = ensurePluginDataDir();
    const passphraseFile = path.join(dataPath, "agent.passphrase");
    if (fs.existsSync(passphraseFile)) {
      return fs.readFileSync(passphraseFile, "utf-8").trim();
    }
  } catch (e) {
    // Ignore - might not have fs access
  }
  return null;
}

function storePassphrase(passphrase: string): void {
  try {
    const dataPath = ensurePluginDataDir();
    const passphraseFile = path.join(dataPath, "agent.passphrase");
    fs.writeFileSync(passphraseFile, passphrase, { mode: 0o600 });
  } catch (e) {
    // Ignore - might not have fs access
  }
}

function getStoredAdminCredential(): string | null {
  try {
    const dataPath = ensurePluginDataDir();
    const credFile = path.join(dataPath, "admin.credential");
    if (fs.existsSync(credFile)) {
      return fs.readFileSync(credFile, "utf-8").trim();
    }
  } catch (e) {
    // Ignore
  }
  return null;
}

function storeAdminCredential(credential: string): void {
  try {
    const dataPath = ensurePluginDataDir();
    const credFile = path.join(dataPath, "admin.credential");
    fs.writeFileSync(credFile, credential, { mode: 0o600 });
  } catch (e) {
    // Ignore
  }
}

// ---------------------------------------------------------------------------
// Executor Process Management (Managed Mode)
// ---------------------------------------------------------------------------

let executorProcess: ReturnType<typeof spawn> | null = null;

function isExecutorRunning(
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

async function ensureExecutorRunning(
  adminCredential: string,
  logger: any,
  endpoint: string = "http://localhost:3001/mcp",
  wsEndpoint: string = "ws://localhost:12100/graphql",
): Promise<boolean> {
  logger.info(`[ad4m] Checking if executor is running at ${endpoint}...`);

  // Check if already running
  if (await isExecutorRunning(endpoint, 3000)) {
    logger.info(`[ad4m] Executor is already running`);
    return true;
  }

  logger.info(`[ad4m] Executor not running, attempting to start...`);

  // Try to find ad4m-executor in PATH
  const executorPath = "ad4m-executor";

  try {
    // Start the executor as a child process
    executorProcess = spawn(
      executorPath,
      [
        "run",
        "--enable-mcp",
        "true",
        "--admin-credential",
        adminCredential,
        "--port",
        "3001",
      ],
      {
        stdio: ["ignore", "pipe", "pipe"],
        detached: false,
      },
    );

    executorProcess.stdout?.on("data", (data: Buffer) => {
      logger.info(`[ad4m-executor] ${data.toString().trim()}`);
    });

    executorProcess.stderr?.on("data", (data: Buffer) => {
      logger.info(`[ad4m-executor] ${data.toString().trim()}`);
    });

    executorProcess.on("error", (err: Error) => {
      logger.error(`[ad4m] Failed to start executor: ${err.message}`);
      executorProcess = null;
    });

    executorProcess.on("exit", (code: number) => {
      logger.info(`[ad4m] Executor exited with code ${code}`);
      executorProcess = null;
    });

    // Wait for executor to be ready
    logger.info(`[ad4m] Waiting for executor to start...`);
    for (let i = 0; i < 30; i++) {
      await new Promise((r) => setTimeout(r, 1000));
      if (await isExecutorRunning(endpoint, 2000)) {
        logger.info(`[ad4m] Executor started successfully`);
        return true;
      }
      logger.info(`[ad4m] Waiting... (${i + 1}/30)`);
    }

    logger.error(`[ad4m] Executor failed to start within 30 seconds`);
    return false;
  } catch (err: any) {
    logger.error(`[ad4m] Error starting executor: ${err.message}`);
    logger.error(`[ad4m] Make sure ad4m-executor is installed and in PATH`);
    return false;
  }
}

function stopExecutor(): void {
  if (executorProcess) {
    executorProcess.kill("SIGTERM");
    executorProcess = null;
    logger.info(`[ad4m] Executor stopped`);
  }
}

interface WakerSubscription {
  id: string;
  type: "mention" | "channel-messages";
  perspective: string;
  channel: string;
  query: string;
  neighbourhood?: string;
}

// ---------------------------------------------------------------------------
// MCP HTTP Client (Streamable HTTP with SSE support)
// ---------------------------------------------------------------------------

let requestIdCounter = 0;

/**
 * Parse an SSE text/event-stream body into the first JSON-RPC message.
 */
async function parseSSEStream(response: Response): Promise<McpResponse> {
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
async function mcpRequest(
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
async function mcpNotify(
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
async function mcpInitialize(
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
async function mcpListTools(
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
async function mcpCallTool(
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
function extractMcpResultData(result: any): any {
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

function buildWakeMessage(
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
    `Auth credential: ${config.adminCredential}`,
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

async function postWake(
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
      logger.info(
        `[ad4m] Using adminCredential: *** (length: ${adminCredential.length})`,
      );
    }

    const authToken = adminCredential;

    // -- Setup helper tool --
  } catch (e: any) {
    logger.error(`[ad4m-waker] wake POST error: ${e.message}`);
  }
}

// ---------------------------------------------------------------------------
// Plugin Export
// ---------------------------------------------------------------------------

export default async function ad4mPlugin(api: any) {
  const providedConfig: PluginConfig = (api.pluginConfig as PluginConfig) ?? {};
  const mode = providedConfig.mode || "managed";

  // Determine endpoint - default to localhost for managed, use provided for external
  const endpoint = providedConfig.mcpEndpoint ?? "http://localhost:3001/mcp";

  // Determine auth token based on mode
  let adminCredential: string | undefined;
  let authToken: string;

  if (mode === "external") {
    // External mode: use provided credential as-is (may be empty - uses JWT flow)
    adminCredential = providedConfig.adminCredential;
    authToken = adminCredential || "";
  } else {
    // Managed mode: auto-generate and manage credentials AND start executor
    const storedPassphrase = getStoredPassphrase();
    const storedAdminCred = getStoredAdminCredential();

    // Determine effective adminCredential: provided > stored > generate new
    adminCredential = providedConfig.adminCredential;
    if (!adminCredential && storedAdminCred) {
      adminCredential = storedAdminCred;
    }
    if (!adminCredential) {
      adminCredential = generateRandomPassphrase(24);
      storeAdminCredential(adminCredential);
    }

    // Store the adminCredential we end up using
    if (!providedConfig.adminCredential && adminCredential) {
      storeAdminCredential(adminCredential);
    }
    authToken = adminCredential;

    // Ensure executor is running (spawn if not)
    const executorWsUrl =
      providedConfig.executorWsUrl ?? "ws://localhost:12100/graphql";
    const executorStarted = await ensureExecutorRunning(
      adminCredential,
      logger,
      endpoint,
      executorWsUrl,
    );
    if (!executorStarted) {
      logger.error(
        `[ad4m] Failed to start executor in managed mode. Please ensure ad4m-executor is installed.`,
      );
    }
  }

  const config: PluginConfig = {
    ...providedConfig,
    mode,
    mcpEndpoint: endpoint,
    adminCredential,
  };
  const logger = api.logger;

  // Check for required config
  if (!adminCredential) {
    logger.warn(
      `[ad4m] Warning: adminCredential not configured. MCP tools will not be available. Please configure adminCredential in plugin settings.`,
    );
  } else {
    logger.info(
      `[ad4m] Using adminCredential: *** (length: ${adminCredential.length})`,
    );
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
- Credentials stored in ~/.ad4m-plugin/ for reuse`,
          },
        ],
      };
    },
  });

  // Debug: log config values
  logger.info(
    `[ad4m] Config received: ${JSON.stringify({
      mcpEndpoint: config.mcpEndpoint,
      adminCredential: config.adminCredential
        ? "*** (length: " + config.adminCredential.length + ")"
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
  let wakerAgentDid = "";
  const wakerProxies = new Map<string, any>(); // id -> QuerySubscriptionProxy
  const wakerDebounceTimers = new Map<string, ReturnType<typeof setTimeout>>();
  const wakerSubscriptions = new Map<string, WakerSubscription>();

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
  function registerMcpTool(tool: McpTool) {
    if (registeredTools.has(tool.name)) return;

    api.registerTool({
      name: tool.name,
      description: tool.description ?? `AD4M MCP tool: ${tool.name}`,
      parameters: toParameters(tool),
      async execute(_id: string, params: Record<string, any>) {
        try {
          const result = await mcpCallTool(
            endpoint,
            tool.name,
            params,
            sessionId,
            authToken,
          );
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
   */
  async function refreshTools(): Promise<number> {
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
          postWake(config, sub, wakerAgentDid, logger, parentChannel);
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

  api.registerTool({
    name: "subscribe_to_mentions",
    description:
      "Subscribe to mentions of this agent in a neighbourhood. " +
      "Creates a live waker subscription that watches for messages mentioning " +
      "your name or DID and wakes you when detected. Call this once per neighbourhood you join.",
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
        // Call the MCP tool to generate the mention query
        const result = await mcpCallTool(
          endpoint,
          "get_mention_waker_config",
          { perspective_id: params.perspective_id },
          sessionId,
          authToken,
        );
        const data = extractMcpResultData(result);

        if (data?.error) {
          return { content: [{ type: "text", text: `Error: ${data.error}` }] };
        }

        const subscription: WakerSubscription = {
          id:
            data.subscription?.id ??
            `mention-${params.perspective_id.substring(0, 8)}`,
          type: "mention",
          perspective: params.perspective_id,
          channel: "",
          query: data.query ?? data.subscription?.query,
          neighbourhood: data.neighbourhood,
        };

        if (!subscription.query) {
          return {
            content: [
              {
                type: "text",
                text: "Error: MCP tool did not return a query. Is the perspective accessible?",
              },
            ],
          };
        }

        await createLiveSubscription(subscription);

        return {
          content: [
            {
              type: "text",
              text:
                `Subscribed to mentions in perspective ${params.perspective_id}. ` +
                `Subscription ID: ${subscription.id}. ` +
                `Watching for: ${(data.names ?? []).join(", ")} and DID ${data.did ?? "unknown"}.`,
            },
          ],
        };
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
        const result = await mcpCallTool(
          endpoint,
          "generate_waker_query",
          {
            perspective_id: params.perspective_id,
            class_name: "Message",
            parent_address: params.expression_address,
          },
          sessionId,
          authToken,
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
        const init = await mcpInitialize(endpoint, authToken);
        sessionId = init.sessionId;
        logger.info(`[ad4m] MCP session established (id: ${sessionId})`);

        // Initial tool discovery
        const tools = await mcpListTools(endpoint, sessionId, authToken);
        for (const tool of tools) {
          registerMcpTool(tool);
        }
        logger.info(
          `[ad4m] Registered ${registeredTools.size} initial tool(s)`,
        );

        // Start periodic polling for dynamic SHACL tools
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
      stopExecutor();
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

      if (!config.wakeUrl || !config.wakeToken) {
        logger.info(
          "[ad4m-waker] Waker not configured (wakeUrl and wakeToken required). Skipping.",
        );
        return;
      }

      const executorWsUrl =
        config.executorWsUrl ?? "ws://localhost:12100/graphql";
      const token = config.adminCredential;

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

        // Check agent status and manage if needed
        let agentStatus;
        try {
          agentStatus = await wakerClient.agent.status();
          logger.info(
            `[ad4m-waker] Agent status: initialized=${agentStatus.isInitialized}, unlocked=${agentStatus.isUnlocked}`,
          );
        } catch (e: any) {
          logger.error(`[ad4m-waker] Failed to get agent status: ${e.message}`);
          throw e;
        }

        // Agent management: generate or unlock as needed
        if (!agentStatus.isInitialized) {
          // First run - generate new agent
          const passphrase = generateRandomPassphrase(32);
          logger.info(
            `[ad4m-waker] Agent not initialized, generating new agent...`,
          );
          try {
            await wakerClient.agent.generate(passphrase);
            storePassphrase(passphrase);
            logger.info(
              `[ad4m-waker] Agent generated successfully. DID: ${agentStatus.did}`,
            );
          } catch (e: any) {
            logger.error(`[ad4m-waker] Failed to generate agent: ${e.message}`);
          }
        } else if (agentStatus.isLocked) {
          // Previously initialized but locked - try to unlock
          const passphrase = getStoredPassphrase();
          if (passphrase) {
            logger.info(
              `[ad4m-waker] Agent is locked, attempting to unlock...`,
            );
            try {
              await wakerClient.agent.unlock(passphrase);
              logger.info(`[ad4m-waker] Agent unlocked successfully.`);
            } catch (e: any) {
              logger.error(`[ad4m-waker] Failed to unlock agent: ${e.message}`);
              logger.warn(
                `[ad4m-waker] You may need to provide the correct passphrase or reconfigure.`,
              );
            }
          } else {
            logger.warn(
              `[ad4m-waker] Agent is locked but no passphrase stored. Please provide agentPassphrase in config or reinstall.`,
            );
          }
        } else {
          logger.info(`[ad4m-waker] Agent is ready.`);
        }

        // Verify connection + get agent DID
        const status = await wakerClient.agent.status();
        wakerAgentDid = status.did;
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
      wakerAgentDid = "";
      logger.info("[ad4m-waker] Waker service stopped");
    },
  });
}
