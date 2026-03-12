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

import {
  McpResponse,
  McpTool,
  PluginConfig,
  WakerSubscription,
} from "./types";
import { generateRandomPassphrase, updatePluginConfig } from "./config";
import {
  ensureExecutorRunning,
  ExecutorStartResult,
  findExecutorBinary,
  isExecutorRunning,
  stopExecutor,
} from "./executor";
import { ensureAgentReady } from "./agent";
import {
  extractMcpResultData,
  mcpCallTool,
  mcpInitialize,
  mcpListTools,
} from "./mcpClient";
import { buildWakeMessage, postWake } from "./wakerHelpers";

// ---------------------------------------------------------------------------
// Types & helpers (re-exported from domain modules)
// ---------------------------------------------------------------------------

export type {
  McpResponse,
  McpTool,
  PluginConfig,
  WakerSubscription,
} from "./types";
export { generateRandomPassphrase, updatePluginConfig } from "./config";
export {
  findExecutorBinary,
  isExecutorRunning,
  ensureExecutorRunning,
  stopExecutor,
} from "./executor";
export type { ExecutorStartResult } from "./executor";
export { ensureAgentReady } from "./agent";
export {
  parseSSEStream,
  mcpRequest,
  mcpNotify,
  mcpInitialize,
  mcpListTools,
  mcpCallTool,
  extractMcpResultData,
} from "./mcpClient";
export { buildWakeMessage, postWake } from "./wakerHelpers";



// ---------------------------------------------------------------------------
// MCP HTTP Client (Streamable HTTP with SSE support)
// ---------------------------------------------------------------------------

let requestIdCounter = 0;



// ---------------------------------------------------------------------------
// Plugin Export
// ---------------------------------------------------------------------------

export default function ad4mPlugin(api: any) {
  const logger = api.logger;

  const providedConfig: PluginConfig = (api.pluginConfig as PluginConfig) ?? {};
  const mode = providedConfig.mode || "managed";

  // Determine endpoint - default to localhost for managed, use provided for external
  const endpoint = providedConfig.mcpEndpoint ?? "http://localhost:3001/mcp";

  // Resolve executorWsUrl once — used by both ensureAgentReady and waker service
  const executorWsUrl =
    providedConfig.executorWsUrl ?? "ws://localhost:12000/graphql";

  // Mutable state set during service start() — shared across tools and services
  let authToken: string = providedConfig.token || "";
  let pluginAgentDid: string = "";

  // Write initial defaults for missing fields (fire-and-forget).
  // Only adds fields not already in config — never overwrites existing values.
  const initDefaults: Partial<PluginConfig> = {};
  if (!providedConfig.mode) initDefaults.mode = "managed";
  if (!providedConfig.ad4mBinaryPath) {
    const found = findExecutorBinary();
    if (found) initDefaults.ad4mBinaryPath = found;
  }
  if (!providedConfig.agentPassphrase) {
    const generated = generateRandomPassphrase(32);
    initDefaults.agentPassphrase = generated;
    // Keep the in-memory view in sync so later logic can rely on it
    providedConfig.agentPassphrase = generated;
  }
  if (Object.keys(initDefaults).length > 0) {
    updatePluginConfig(api, initDefaults, logger);
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

  // Debug: log config values
  logger.info(
    `[ad4m] Config: ${JSON.stringify({
      mode: config.mode,
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
  const activeSubscriptions = new Map<string, WakerSubscription>();

  /**
   * (Re-)initialize the MCP session. Called on first connect and when the
   * session becomes invalid (e.g. executor restart, session expiry -> 422).
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
        // Session error -> re-initialize and retry once
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
    activeSubscriptions.set(sub.id, sub);
    persistSubscriptions();

    logger.info(
      `[ad4m-waker] Subscription ${sub.id} active (type=${sub.type}, perspective=${sub.perspective})`,
    );
  }

  /**
   * Persist the current subscription list to the OpenClaw config.
   */
  function persistSubscriptions(): void {
    const subs = Array.from(activeSubscriptions.values());
    updatePluginConfig(api, { wakerSubscriptions: subs }, logger);
  }

  /**
   * Dispose a single live subscription.
   * @param persist — if false, skip persisting to config (used during service stop
   *   so saved subscriptions survive for restore on next start).
   */
  function disposeLiveSubscription(id: string, persist = true): void {
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
    activeSubscriptions.delete(id);
    if (persist) persistSubscriptions();
  }

  /**
   * Obtain a JWT token from a running executor via MCP capability request.
   * Returns the JWT string or empty string on failure.
   */
  async function obtainJwtFromExecutor(): Promise<string> {
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
          return jwtData.token;
        }
      }
    } catch (e: any) {
      logger.warn(`[ad4m] JWT auth failed: ${e.message}`);
    }
    return "";
  }

  // =========================================================================
  // Tools
  // =========================================================================

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
    if (activeSubscriptions.has(existingId)) {
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
      for (const [id, sub] of activeSubscriptions) {
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
      for (const [id, sub] of activeSubscriptions) {
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
      const subs = Array.from(activeSubscriptions.values());
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

  // -- MCP bridge service (registered first — starts first) --
  // Handles executor startup, agent management, auth token acquisition,
  // MCP session init, and tool discovery.

  api.registerService({
    id: "ad4m-mcp",
    async start() {
      logger.info(`[ad4m] Starting MCP bridge service (mode=${mode})`);

      // ── Mode-specific initialization ──

      if (mode === "external") {
        // External mode: use token from config, or obtain JWT
        if (!authToken) {
          logger.info("[ad4m] No token found, attempting JWT auth via MCP...");
          const jwt = await obtainJwtFromExecutor();
          if (jwt) {
            authToken = jwt;
            await updatePluginConfig(api, { token: jwt }, logger);
            logger.info("[ad4m] JWT obtained and stored in config");
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

        // Write defaults for any missing config fields.
        // Only sets fields not already present — never overwrites existing values.
        const defaults: Partial<PluginConfig> = {};
        if (!providedConfig.mode) defaults.mode = "managed";
        if (!providedConfig.ad4mBinaryPath && binaryPath) defaults.ad4mBinaryPath = binaryPath;
        if (!providedConfig.agentPassphrase) defaults.agentPassphrase = agentPassphrase;
        if (Object.keys(defaults).length > 0) {
          await updatePluginConfig(api, defaults, logger);
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
          logger.info("[ad4m] Executor was already running — obtaining JWT for auth...");
          const jwt = await obtainJwtFromExecutor();
          if (jwt) {
            authToken = jwt;
            logger.info("[ad4m] JWT obtained for pre-existing executor");
          } else {
            logger.warn("[ad4m] JWT auth failed for pre-existing executor");
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

      // ── MCP session + tool discovery ──

      if (authToken) {
        logger.info(`[ad4m] Auth token ready (length: ${authToken.length})`);
      } else {
        logger.warn(`[ad4m] No auth token. Tools may not work until authenticated.`);
      }

      // Update config with resolved token
      config.token = authToken || undefined;

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

  // -- Waker service (registered second — starts after ad4m-mcp) --
  // By the time this starts, authToken and pluginAgentDid are set by
  // the mcp service above (OpenClaw starts services sequentially).

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

      if (!authToken) {
        logger.warn(
          "[ad4m-waker] No auth token available — cannot connect to executor. Skipping.",
        );
        return;
      }

      const wsUrl =
        config.executorWsUrl ?? "ws://localhost:12000/graphql";

      try {
        // Dynamic imports to avoid load-time issues with @holochain/client transitive deps
        const { Ad4mClient } = require("@coasys/ad4m");
        const { ApolloClient, InMemoryCache } = require("@apollo/client/core");
        const { GraphQLWsLink } = require("@apollo/client/link/subscriptions");
        const { createClient } = require("graphql-ws");
        const WebSocket = require("ws");

        logger.info(`[ad4m-waker] Connecting to ${wsUrl}`);

        const wsClient = createClient({
          url: wsUrl,
          webSocketImpl: WebSocket,
          connectionParams: authToken ? { headers: { authorization: authToken } } : {},
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
        // initialized/unlocked by ensureAgentReady during mcp service start)
        const status = await wakerClient.agent.status();
        if (!status.isInitialized || status.isUnlocked === false) {
          logger.error(
            `[ad4m-waker] Agent is not ready (initialized=${status.isInitialized}, unlocked=${status.isUnlocked}). Agent management should have run during mcp service start.`,
          );
          wakerClient = null;
          return;
        }
        pluginAgentDid = status.did;
        logger.info(
          `[ad4m-waker] Connected — agent: ${status.did.substring(0, 40)}...`,
        );

        // Restore persisted subscriptions from config
        const saved = config.wakerSubscriptions;
        if (saved && saved.length > 0) {
          logger.info(
            `[ad4m-waker] Restoring ${saved.length} persisted subscription(s)...`,
          );
          for (const sub of saved) {
            try {
              await createLiveSubscription(sub);
              logger.info(
                `[ad4m-waker] Restored: ${sub.id} (${sub.type}, perspective=${sub.perspective})`,
              );
            } catch (err: any) {
              logger.error(
                `[ad4m-waker] Failed to restore ${sub.id}: ${err.message}`,
              );
            }
          }
        }
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
        disposeLiveSubscription(id, false);
      }
      wakerClient = null;
      logger.info("[ad4m-waker] Waker service stopped");
    },
  });
}
