import type { McpResponse, McpTool } from "./types";

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
  timeoutMs: number = 30000,
): Promise<McpResponse> {
  const id = ++requestIdCounter;
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    Accept: "application/json, text/event-stream",
  };
  if (sessionId) headers["Mcp-Session-Id"] = sessionId;
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  // Hard timeout across the whole request — including the SSE body read.
  // The executor keeps the event-stream open, so parseSSEStream's reader.read()
  // would block forever if a response frame never arrives; the abort turns that
  // into a fast, surfaced error instead of a hang.
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const response = await fetch(endpoint, {
      method: "POST",
      headers,
      body: JSON.stringify({ jsonrpc: "2.0", id, method, params }),
      signal: controller.signal,
    });

    if (!response.ok) {
      // Release the unread body so the socket is not held open on error.
      try {
        await response.body?.cancel();
      } catch {
        /* already closed */
      }
      throw new Error(`MCP HTTP ${response.status}: ${response.statusText}`);
    }

    const ct = response.headers.get("content-type") ?? "";
    if (ct.includes("text/event-stream")) {
      return await parseSSEStream(response);
    }
    return (await response.json()) as McpResponse;
  } catch (e: any) {
    if (e?.name === "AbortError") {
      throw new Error(`MCP ${method} timed out after ${timeoutMs}ms`);
    }
    throw e;
  } finally {
    clearTimeout(timer);
  }
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
  timeoutMs: number = 10000,
): Promise<void> {
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    Accept: "application/json, text/event-stream",
  };
  if (sessionId) headers["Mcp-Session-Id"] = sessionId;
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const resp = await fetch(endpoint, {
      method: "POST",
      headers,
      body: JSON.stringify({ jsonrpc: "2.0", method, params }),
      signal: controller.signal,
    });
    // A notification expects no response body; release the connection so the
    // server's open event-stream cannot stall subsequent requests.
    try {
      await resp.body?.cancel();
    } catch {
      /* already closed */
    }
  } catch {
    /* notification delivery is best-effort */
  } finally {
    clearTimeout(timer);
  }
}

/**
 * Initialize an MCP session: initialize + notifications/initialized handshake.
 */
export async function mcpInitialize(
  endpoint: string,
  authToken?: string,
  timeoutMs: number = 30000,
): Promise<{ sessionId: string; serverInfo: any }> {
  const id = ++requestIdCounter;
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    Accept: "application/json, text/event-stream",
  };
  if (authToken) headers["Authorization"] = `Bearer ${authToken}`;

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  let sessionId = "";
  let result: McpResponse;
  try {
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
      signal: controller.signal,
    });

    if (!resp.ok) {
      // Release the unread body so the socket is not held open on error.
      try {
        await resp.body?.cancel();
      } catch {
        /* already closed */
      }
      throw new Error(`MCP initialize HTTP ${resp.status}: ${resp.statusText}`);
    }

    sessionId = resp.headers.get("mcp-session-id") ?? "";
    const ct = resp.headers.get("content-type") ?? "";
    if (ct.includes("text/event-stream")) {
      result = await parseSSEStream(resp);
    } else {
      result = (await resp.json()) as McpResponse;
    }
  } catch (e: any) {
    if (e?.name === "AbortError") {
      throw new Error(`MCP initialize timed out after ${timeoutMs}ms`);
    }
    throw e;
  } finally {
    clearTimeout(timer);
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

