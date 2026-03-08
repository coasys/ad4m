/**
 * AD4M OpenClaw Plugin
 *
 * Bridges AD4M's MCP server into OpenClaw by:
 * 1. Connecting to the AD4M executor's Streamable HTTP MCP endpoint
 * 2. Discovering all available tools (including dynamic SHACL-generated ones)
 * 3. Registering each tool with OpenClaw via api.registerTool()
 * 4. Periodically polling for new dynamic tools as perspectives sync SHACL schemas
 */

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
  mcpEndpoint?: string;
  adminCredential: string;
  toolRefreshIntervalMs?: number;
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
  await mcpNotify(endpoint, "notifications/initialized", {}, sessionId, authToken);

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
  const resp = await mcpRequest(endpoint, "tools/list", {}, sessionId, authToken);
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
    return { content: [{ type: "text", text: JSON.stringify({ error: resp.error.message }) }] };
  }

  return resp.result;
}

// ---------------------------------------------------------------------------
// Plugin Export
// ---------------------------------------------------------------------------

export default function ad4mPlugin(api: any) {
  const config: PluginConfig = api.getConfig?.() ?? {};
  const endpoint = config.mcpEndpoint ?? "http://localhost:3001/mcp";
  const authToken = config.adminCredential;
  const refreshInterval = config.toolRefreshIntervalMs ?? 30000;
  const logger = api.logger;

  let sessionId = "";
  let registeredTools = new Set<string>();
  let refreshTimer: ReturnType<typeof setInterval> | null = null;

  /**
   * Convert an MCP tool definition's inputSchema to OpenClaw parameters format.
   * MCP tools use JSON Schema in inputSchema; OpenClaw accepts the same format.
   */
  function toParameters(tool: McpTool): Record<string, any> {
    if (tool.inputSchema) {
      return tool.inputSchema;
    }
    return { type: "object", properties: {}, required: [] };
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
          const result = await mcpCallTool(endpoint, tool.name, params, sessionId, authToken);
          // MCP result is in { content: [{ type, text }] } format
          // Extract the text content for OpenClaw
          if (result?.content && Array.isArray(result.content)) {
            const textParts = result.content
              .filter((c: any) => c.type === "text")
              .map((c: any) => c.text)
              .join("\n");
            if (textParts) {
              try {
                // Try to parse as JSON for structured results
                return JSON.parse(textParts);
              } catch {
                // Return as plain text string
                return textParts;
              }
            }
          }
          return result;
        } catch (err: any) {
          return { error: err.message };
        }
      },
    });

    registeredTools.add(tool.name);
  }

  /**
   * Fetch tools from MCP and register any new ones.
   * Returns the number of newly registered tools.
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
        logger.info(`[ad4m] Registered ${newCount} new tool(s), total: ${registeredTools.size}`);
      }
      return newCount;
    } catch (err: any) {
      logger.warn(`[ad4m] Tool refresh failed: ${err.message}`);
      return 0;
    }
  }

  // -- Manual refresh tool for agents to call after add_model / schema changes --

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

  // -- Background service: maintains MCP session and polls for dynamic tools --

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
        logger.info(`[ad4m] Registered ${registeredTools.size} initial tool(s)`);

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
      logger.info("[ad4m] AD4M MCP service stopped");
    },
  });
}
