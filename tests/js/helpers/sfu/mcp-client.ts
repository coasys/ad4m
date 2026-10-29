/**
 * Minimal MCP client for the AD4M executor's streamable-HTTP transport.
 *
 * Speaks JSON-RPC 2.0 over HTTP POST; the executor replies as `text/event-stream`
 * (SSE) and tracks the session via the `Mcp-Session-Id` header. Auth rides a
 * Bearer token (the admin credential or a user JWT).
 *
 * This replaces the `a1` raw-`fetch` stub — it negotiates a real session, parses
 * the SSE frames, and can list + call tools. Reused by A2/A3/A4.
 */

export interface McpTool {
  name: string;
  description?: string;
  inputSchema?: any;
}

export interface McpToolResult {
  content?: Array<{ type: string; text?: string; [k: string]: any }>;
  isError?: boolean;
  [k: string]: any;
}

export class McpClient {
  private sessionId: string | null = null;
  private nextId = 1;

  constructor(
    private baseUrl: string,
    private token?: string,
  ) {}

  /** Swap the auth token (e.g. after a user login returns a fresh JWT). */
  setToken(token: string): void {
    this.token = token;
  }

  private headers(): Record<string, string> {
    const h: Record<string, string> = {
      "Content-Type": "application/json",
      Accept: "application/json, text/event-stream",
    };
    if (this.token) h["Authorization"] = `Bearer ${this.token}`;
    if (this.sessionId) h["Mcp-Session-Id"] = this.sessionId;
    return h;
  }

  /** POST a JSON-RPC request and resolve its matching response. */
  private async rpc(method: string, params: any, timeoutMs = 20000): Promise<any> {
    const id = this.nextId++;
    const ctrl = new AbortController();
    const timer = setTimeout(() => ctrl.abort(), timeoutMs);
    try {
      const res = await fetch(`${this.baseUrl}/mcp`, {
        method: "POST",
        headers: this.headers(),
        body: JSON.stringify({ jsonrpc: "2.0", id, method, params }),
        signal: ctrl.signal,
      });
      const sid = res.headers.get("mcp-session-id");
      if (sid) this.sessionId = sid;
      const ctype = res.headers.get("content-type") || "";
      if (ctype.includes("text/event-stream")) {
        return await this.readSseForId(res, id, timeoutMs);
      }
      const json = await res.json();
      if (json.error) throw new Error(`MCP ${method} error: ${JSON.stringify(json.error)}`);
      return json.result;
    } finally {
      clearTimeout(timer);
    }
  }

  /** Read the SSE body until a `data:` frame carries our JSON-RPC id. */
  private async readSseForId(res: Response, id: number, timeoutMs: number): Promise<any> {
    if (!res.body) throw new Error("MCP: empty response body");
    const reader = (res.body as any).getReader();
    const dec = new TextDecoder();
    let buf = "";
    const deadline = Date.now() + timeoutMs;
    try {
      while (Date.now() < deadline) {
        const { value, done } = await reader.read();
        if (done) break;
        buf += dec.decode(value, { stream: true });
        let sep: number;
        while ((sep = buf.indexOf("\n\n")) >= 0) {
          const frame = buf.slice(0, sep);
          buf = buf.slice(sep + 2);
          for (const line of frame.split("\n")) {
            const m = line.match(/^data:\s?(.*)$/);
            if (!m || !m[1]) continue;
            let msg: any;
            try {
              msg = JSON.parse(m[1]);
            } catch {
              continue;
            }
            if (msg.id === id) {
              if (msg.error) throw new Error(`MCP error: ${JSON.stringify(msg.error)}`);
              return msg.result;
            }
          }
        }
      }
    } finally {
      try {
        await reader.cancel();
      } catch {
        /* stream already closed */
      }
    }
    throw new Error(`MCP: no response for id ${id} within ${timeoutMs}ms`);
  }

  /** Fire a JSON-RPC notification (no id, no response expected). */
  private async notify(method: string, params: any): Promise<void> {
    const ctrl = new AbortController();
    const timer = setTimeout(() => ctrl.abort(), 5000);
    try {
      await fetch(`${this.baseUrl}/mcp`, {
        method: "POST",
        headers: this.headers(),
        body: JSON.stringify({ jsonrpc: "2.0", method, params }),
        signal: ctrl.signal,
      }).catch(() => {});
    } finally {
      clearTimeout(timer);
    }
  }

  async initialize(clientName = "wind-tunnel", version = "0.1.0"): Promise<any> {
    const result = await this.rpc("initialize", {
      protocolVersion: "2024-11-05",
      capabilities: {},
      clientInfo: { name: clientName, version },
    });
    await this.notify("notifications/initialized", {});
    return result;
  }

  async listTools(): Promise<McpTool[]> {
    const r = await this.rpc("tools/list", {});
    return r?.tools ?? [];
  }

  async callTool(name: string, args: any = {}): Promise<McpToolResult> {
    return await this.rpc("tools/call", { name, arguments: args });
  }

  /** Convenience: call a tool and JSON-parse the first text content block. */
  async callToolJson(name: string, args: any = {}): Promise<any> {
    const r = await this.callTool(name, args);
    const text = r?.content?.find((c) => c.type === "text")?.text;
    if (text == null) return r;
    try {
      return JSON.parse(text);
    } catch {
      return text;
    }
  }

  get session(): string | null {
    return this.sessionId;
  }
}
