import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import path from "path";
import fs from "fs";
import os from "os";
import { EventEmitter } from "events";

// Mock child_process before any imports that use it
const { mockSpawn } = vi.hoisted(() => {
  return { mockSpawn: vi.fn() };
});
vi.mock("child_process", async (importOriginal) => {
  const actual = await importOriginal<typeof import("child_process")>();
  return { ...actual, spawn: mockSpawn };
});

import {
  generateRandomPassphrase,
  getPluginDataPath,
  ensurePluginDataDir,
  getStoredPassphrase,
  storePassphrase,
  getStoredAdminCredential,
  storeAdminCredential,
  isExecutorRunning,
  ensureExecutorRunning,
  stopExecutor,
  parseSSEStream,
  mcpRequest,
  mcpNotify,
  mcpInitialize,
  mcpListTools,
  mcpCallTool,
  extractMcpResultData,
  buildWakeMessage,
  postWake,
  type PluginConfig,
  type WakerSubscription,
  type McpTool,
  type McpResponse,
} from "./index";

import ad4mPlugin from "./index";

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Create a minimal ReadableStream from a string (for SSE testing). */
function stringToStream(text: string): ReadableStream<Uint8Array> {
  const encoder = new TextEncoder();
  return new ReadableStream({
    start(controller) {
      controller.enqueue(encoder.encode(text));
      controller.close();
    },
  });
}

/** Build a fake Response with a body stream. */
function fakeSSEResponse(sseText: string): Response {
  return new Response(stringToStream(sseText), {
    status: 200,
    headers: { "Content-Type": "text/event-stream" },
  });
}

/** Build a fake JSON Response. */
function fakeJsonResponse(
  data: any,
  headers?: Record<string, string>,
): Response {
  const h = new Headers({ "Content-Type": "application/json", ...headers });
  return new Response(JSON.stringify(data), { status: 200, headers: h });
}

function makeMockLogger() {
  return {
    info: vi.fn(),
    warn: vi.fn(),
    error: vi.fn(),
    debug: vi.fn(),
  };
}

// ---------------------------------------------------------------------------
// generateRandomPassphrase
// ---------------------------------------------------------------------------

describe("generateRandomPassphrase", () => {
  it("returns a string of the requested length", () => {
    const result = generateRandomPassphrase(16);
    expect(result).toHaveLength(16);
  });

  it("defaults to 32 characters", () => {
    const result = generateRandomPassphrase();
    expect(result).toHaveLength(32);
  });

  it("contains only alphanumeric characters", () => {
    const result = generateRandomPassphrase(100);
    expect(result).toMatch(/^[A-Za-z0-9]+$/);
  });

  it("produces different results on successive calls", () => {
    const a = generateRandomPassphrase(32);
    const b = generateRandomPassphrase(32);
    // Extremely unlikely to collide
    expect(a).not.toBe(b);
  });
});

// ---------------------------------------------------------------------------
// getPluginDataPath
// ---------------------------------------------------------------------------

describe("getPluginDataPath", () => {
  it("returns a path under HOME/.ad4m-plugin", () => {
    const result = getPluginDataPath();
    const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
    expect(result).toBe(path.join(home, ".ad4m-plugin"));
  });
});

// ---------------------------------------------------------------------------
// ensurePluginDataDir
// ---------------------------------------------------------------------------

describe("ensurePluginDataDir", () => {
  it("returns the data path and the directory exists", () => {
    const dataPath = ensurePluginDataDir();
    expect(dataPath).toBe(getPluginDataPath());
    expect(fs.existsSync(dataPath)).toBe(true);
  });
});

// ---------------------------------------------------------------------------
// storePassphrase / getStoredPassphrase
// ---------------------------------------------------------------------------

describe("storePassphrase / getStoredPassphrase", () => {
  const testPassphrase = "test-passphrase-" + Date.now();
  let originalPassphrase: string | null;

  beforeEach(() => {
    originalPassphrase = getStoredPassphrase();
  });

  afterEach(() => {
    // Restore original passphrase if any
    if (originalPassphrase) {
      storePassphrase(originalPassphrase);
    } else {
      // Remove test file
      try {
        const dataPath = getPluginDataPath();
        const file = path.join(dataPath, "agent.passphrase");
        if (fs.existsSync(file)) fs.unlinkSync(file);
      } catch {
        // ignore
      }
    }
  });

  it("stores and retrieves a passphrase", () => {
    storePassphrase(testPassphrase);
    const result = getStoredPassphrase();
    expect(result).toBe(testPassphrase);
  });

  it("returns null when no passphrase stored", () => {
    // Remove passphrase file
    try {
      const dataPath = getPluginDataPath();
      const file = path.join(dataPath, "agent.passphrase");
      if (fs.existsSync(file)) fs.unlinkSync(file);
    } catch {
      // ignore
    }
    const result = getStoredPassphrase();
    expect(result).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// storeAdminCredential / getStoredAdminCredential
// ---------------------------------------------------------------------------

describe("storeAdminCredential / getStoredAdminCredential", () => {
  const testCred = "test-credential-" + Date.now();
  let originalCred: string | null;

  beforeEach(() => {
    originalCred = getStoredAdminCredential();
  });

  afterEach(() => {
    if (originalCred) {
      storeAdminCredential(originalCred);
    } else {
      try {
        const dataPath = getPluginDataPath();
        const file = path.join(dataPath, "admin.credential");
        if (fs.existsSync(file)) fs.unlinkSync(file);
      } catch {
        // ignore
      }
    }
  });

  it("stores and retrieves an admin credential", () => {
    storeAdminCredential(testCred);
    const result = getStoredAdminCredential();
    expect(result).toBe(testCred);
  });

  it("returns null when no credential stored", () => {
    try {
      const dataPath = getPluginDataPath();
      const file = path.join(dataPath, "admin.credential");
      if (fs.existsSync(file)) fs.unlinkSync(file);
    } catch {
      // ignore
    }
    const result = getStoredAdminCredential();
    expect(result).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// isExecutorRunning
// ---------------------------------------------------------------------------

describe("isExecutorRunning", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("returns true when endpoint responds", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({ jsonrpc: "2.0", id: 0, result: {} }),
    );
    const result = await isExecutorRunning("http://localhost:3001/mcp", 1000);
    expect(result).toBe(true);
  });

  it("returns false when endpoint times out or errors", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));
    const result = await isExecutorRunning("http://localhost:3001/mcp", 500);
    expect(result).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// ensureExecutorRunning
// ---------------------------------------------------------------------------

/** Create a fake ChildProcess (EventEmitter with stdout/stderr). */
function createFakeChildProcess() {
  const proc = new EventEmitter() as EventEmitter & {
    stdout: EventEmitter;
    stderr: EventEmitter;
    kill: ReturnType<typeof vi.fn>;
    pid: number;
  };
  proc.stdout = new EventEmitter();
  proc.stderr = new EventEmitter();
  proc.kill = vi.fn();
  proc.pid = 12345;
  return proc;
}

describe("ensureExecutorRunning", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("returns true immediately when executor is already running", async () => {
    // Mock fetch to succeed (executor already running)
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({ jsonrpc: "2.0", id: 0, result: {} }),
    );

    const logger = makeMockLogger();
    const result = await ensureExecutorRunning("cred", logger);

    expect(result).toBe(true);
    const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(msgs.some((m: string) => m.includes("already running"))).toBe(true);
  });

  it("returns false and logs PATH when spawn emits error (ENOENT)", async () => {
    // First call: isExecutorRunning check → not running
    // Subsequent calls: still not running
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);

    const logger = makeMockLogger();
    const resultPromise = ensureExecutorRunning("cred", logger);

    // Simulate spawn ENOENT error after a short delay (before first poll completes)
    setTimeout(() => {
      fakeProc.emit("error", new Error("spawn ad4m-executor ENOENT"));
    }, 100);

    const result = await resultPromise;

    expect(result).toBe(false);

    // Should have logged PATH
    const allMessages = [
      ...logger.info.mock.calls.map((c: any[]) => c[0]),
      ...logger.error.mock.calls.map((c: any[]) => c[0]),
    ];
    expect(allMessages.some((m: string) => m.includes("PATH:"))).toBe(true);

    // Should have logged the ENOENT error
    expect(
      logger.error.mock.calls.some((c: any[]) =>
        c[0].includes("ENOENT"),
      ),
    ).toBe(true);

    // Should have logged "failed to start" (not timed out after 30s)
    expect(
      logger.error.mock.calls.some((c: any[]) =>
        c[0].includes("failed to start") || c[0].includes("Failed to start"),
      ),
    ).toBe(true);
  }, 10000);

  it("returns false when process exits with non-zero code", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);

    const logger = makeMockLogger();
    const resultPromise = ensureExecutorRunning("cred", logger);

    // Simulate non-zero exit after a short delay
    setTimeout(() => {
      fakeProc.emit("exit", 1);
    }, 100);

    const result = await resultPromise;

    expect(result).toBe(false);

    // Should have logged the exit code
    const allErrors = logger.error.mock.calls.map((c: any[]) => c[0]);
    expect(
      allErrors.some(
        (m: string) => m.includes("exited with code") || m.includes("failed to start"),
      ),
    ).toBe(true);
  }, 10000);

  it("returns true when executor becomes available during polling", async () => {
    let fetchCallCount = 0;
    vi.spyOn(globalThis, "fetch").mockImplementation(async () => {
      fetchCallCount++;
      // First 2 calls fail (initial check + first poll), third succeeds
      if (fetchCallCount <= 2) {
        throw new Error("ECONNREFUSED");
      }
      return fakeJsonResponse({ jsonrpc: "2.0", id: 0, result: {} });
    });

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);

    const logger = makeMockLogger();
    const result = await ensureExecutorRunning("cred", logger);

    expect(result).toBe(true);
    const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(msgs.some((m: string) => m.includes("started successfully"))).toBe(
      true,
    );
  }, 15000);

  it("logs PATH on startup attempt", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);

    const logger = makeMockLogger();
    const resultPromise = ensureExecutorRunning("cred", logger);

    // Trigger immediate failure to avoid waiting
    setTimeout(() => {
      fakeProc.emit("error", new Error("spawn ad4m-executor ENOENT"));
    }, 100);

    await resultPromise;

    // The initial "attempting to start" message should include PATH
    const infoMsgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(infoMsgs.some((m: string) => m.includes("PATH:"))).toBe(true);
  }, 10000);
});

// ---------------------------------------------------------------------------
// stopExecutor
// ---------------------------------------------------------------------------

describe("stopExecutor", () => {
  it("does nothing when no executor process is running", () => {
    // Should not throw
    expect(() => stopExecutor()).not.toThrow();
  });

  it("accepts an optional logger parameter", () => {
    const logger = makeMockLogger();
    expect(() => stopExecutor(logger)).not.toThrow();
  });
});

// ---------------------------------------------------------------------------
// parseSSEStream
// ---------------------------------------------------------------------------

describe("parseSSEStream", () => {
  it("parses a valid SSE stream with a JSON-RPC message", async () => {
    const ssePayload =
      'data: {"jsonrpc":"2.0","id":1,"result":{"tools":[]}}\n\n';
    const response = fakeSSEResponse(ssePayload);
    const parsed = await parseSSEStream(response);
    expect(parsed.jsonrpc).toBe("2.0");
    expect(parsed.id).toBe(1);
    expect(parsed.result).toEqual({ tools: [] });
  });

  it("ignores non-JSON data lines", async () => {
    const ssePayload =
      "data: [DONE]\n" + 'data: {"jsonrpc":"2.0","id":2,"result":"ok"}\n\n';
    const response = fakeSSEResponse(ssePayload);
    const parsed = await parseSSEStream(response);
    expect(parsed.id).toBe(2);
    expect(parsed.result).toBe("ok");
  });

  it("throws when stream ends without JSON-RPC data", async () => {
    const ssePayload = "event: ping\n\n";
    const response = fakeSSEResponse(ssePayload);
    await expect(parseSSEStream(response)).rejects.toThrow(
      "SSE stream ended without JSON-RPC data",
    );
  });
});

// ---------------------------------------------------------------------------
// mcpRequest
// ---------------------------------------------------------------------------

describe("mcpRequest", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("sends a JSON-RPC request and returns JSON response", async () => {
    const mockResult = { jsonrpc: "2.0", id: 1, result: { tools: [] } };
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse(mockResult),
    );

    const resp = await mcpRequest("http://localhost:3001/mcp", "tools/list");
    expect(resp.result).toEqual({ tools: [] });
    expect(fetch).toHaveBeenCalledOnce();
  });

  it("handles SSE responses", async () => {
    const ssePayload = 'data: {"jsonrpc":"2.0","id":1,"result":"sse-ok"}\n\n';
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeSSEResponse(ssePayload),
    );

    const resp = await mcpRequest("http://localhost:3001/mcp", "tools/list");
    expect(resp.result).toBe("sse-ok");
  });

  it("includes session and auth headers when provided", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({ jsonrpc: "2.0", id: 1, result: {} }),
    );

    await mcpRequest(
      "http://localhost:3001/mcp",
      "tools/list",
      {},
      "session-123",
      "my-token",
    );

    const call = vi.mocked(fetch).mock.calls[0];
    const opts = call[1] as RequestInit;
    const headers = opts.headers as Record<string, string>;
    expect(headers["Mcp-Session-Id"]).toBe("session-123");
    expect(headers["Authorization"]).toBe("Bearer my-token");
  });

  it("throws on non-OK HTTP status", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      new Response("Unauthorized", { status: 401, statusText: "Unauthorized" }),
    );
    await expect(
      mcpRequest("http://localhost:3001/mcp", "tools/list"),
    ).rejects.toThrow("MCP HTTP 401");
  });
});

// ---------------------------------------------------------------------------
// mcpNotify
// ---------------------------------------------------------------------------

describe("mcpNotify", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("sends a JSON-RPC notification (no id)", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      new Response(null, { status: 200 }),
    );

    await mcpNotify(
      "http://localhost:3001/mcp",
      "notifications/initialized",
      {},
      "session-123",
    );

    const call = vi.mocked(fetch).mock.calls[0];
    const body = JSON.parse(call[1]!.body as string);
    expect(body.jsonrpc).toBe("2.0");
    expect(body.method).toBe("notifications/initialized");
    expect(body.id).toBeUndefined();
  });
});

// ---------------------------------------------------------------------------
// mcpInitialize
// ---------------------------------------------------------------------------

describe("mcpInitialize", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("performs initialize handshake and returns sessionId", async () => {
    const initResult = {
      jsonrpc: "2.0",
      id: 1,
      result: { serverInfo: { name: "ad4m-mcp", version: "1.0" } },
    };

    let callCount = 0;
    vi.spyOn(globalThis, "fetch").mockImplementation(async (_url, opts) => {
      callCount++;
      if (callCount === 1) {
        // initialize request
        return new Response(JSON.stringify(initResult), {
          status: 200,
          headers: {
            "Content-Type": "application/json",
            "Mcp-Session-Id": "test-session-42",
          },
        });
      }
      // notifications/initialized
      return new Response(null, { status: 200 });
    });

    const { sessionId, serverInfo } = await mcpInitialize(
      "http://localhost:3001/mcp",
      "my-token",
    );
    expect(sessionId).toBe("test-session-42");
    expect(serverInfo).toEqual({
      serverInfo: { name: "ad4m-mcp", version: "1.0" },
    });
    expect(callCount).toBe(2);
  });

  it("throws on MCP error response", async () => {
    const errorResult = {
      jsonrpc: "2.0",
      id: 1,
      error: { code: -1, message: "Auth required" },
    };
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse(errorResult),
    );

    await expect(mcpInitialize("http://localhost:3001/mcp")).rejects.toThrow(
      "MCP initialize error: Auth required",
    );
  });
});

// ---------------------------------------------------------------------------
// mcpListTools
// ---------------------------------------------------------------------------

describe("mcpListTools", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("returns tools array from MCP response", async () => {
    const tools = [
      { name: "list_perspectives", description: "List perspectives" },
      { name: "add_model", description: "Add a model" },
    ];
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({ jsonrpc: "2.0", id: 1, result: { tools } }),
    );

    const result = await mcpListTools("http://localhost:3001/mcp", "session-1");
    expect(result).toHaveLength(2);
    expect(result[0].name).toBe("list_perspectives");
  });

  it("returns empty array when no tools in response", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({ jsonrpc: "2.0", id: 1, result: {} }),
    );

    const result = await mcpListTools("http://localhost:3001/mcp", "session-1");
    expect(result).toEqual([]);
  });
});

// ---------------------------------------------------------------------------
// mcpCallTool
// ---------------------------------------------------------------------------

describe("mcpCallTool", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("calls a tool and returns its result", async () => {
    const toolResult = {
      content: [{ type: "text", text: '{"perspectives":[]}' }],
    };
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({
        jsonrpc: "2.0",
        id: 1,
        result: toolResult,
      }),
    );

    const result = await mcpCallTool(
      "http://localhost:3001/mcp",
      "list_perspectives",
      {},
      "session-1",
    );
    expect(result).toEqual(toolResult);
  });

  it("wraps error responses in content format", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({
        jsonrpc: "2.0",
        id: 1,
        error: { code: -1, message: "Not found" },
      }),
    );

    const result = await mcpCallTool(
      "http://localhost:3001/mcp",
      "nonexistent_tool",
      {},
      "session-1",
    );
    expect(result.content[0].text).toContain("Not found");
  });
});

// ---------------------------------------------------------------------------
// extractMcpResultData
// ---------------------------------------------------------------------------

describe("extractMcpResultData", () => {
  it("extracts and parses JSON from content[0].text", () => {
    const result = {
      content: [{ type: "text", text: '{"foo":"bar"}' }],
    };
    expect(extractMcpResultData(result)).toEqual({ foo: "bar" });
  });

  it("returns raw text if not valid JSON", () => {
    const result = {
      content: [{ type: "text", text: "hello world" }],
    };
    expect(extractMcpResultData(result)).toBe("hello world");
  });

  it("returns a string directly if given one", () => {
    expect(extractMcpResultData("plain text")).toBe("plain text");
  });

  it("returns a JSON string parsed", () => {
    expect(extractMcpResultData('{"a":1}')).toEqual({ a: 1 });
  });

  it("returns the input as-is for non-string, non-content objects", () => {
    const input = { something: "else" };
    expect(extractMcpResultData(input)).toBe(input);
  });
});

// ---------------------------------------------------------------------------
// buildWakeMessage
// ---------------------------------------------------------------------------

describe("buildWakeMessage", () => {
  const config: PluginConfig = {
    mcpEndpoint: "http://localhost:3001/mcp",
    adminCredential: "test-cred",
  };

  const mentionSub: WakerSubscription = {
    id: "mention-abc",
    type: "mention",
    perspective: "uuid-123",
    channel: "",
    query: "SELECT * FROM ...",
    neighbourhood: "neighbourhood://Qm123",
  };

  const channelSub: WakerSubscription = {
    id: "children-abc",
    type: "channel-messages",
    perspective: "uuid-456",
    channel: "literal://string:channel-1",
    query: "SELECT * FROM ...",
  };

  it("builds a mention wake message", () => {
    const msg = buildWakeMessage(config, mentionSub, "did:key:z6Mk123", "");
    expect(msg).toContain("You were @mentioned in an AD4M neighbourhood.");
    expect(msg).toContain("MCP endpoint: http://localhost:3001/mcp");
    expect(msg).toContain("Agent DID: did:key:z6Mk123");
    expect(msg).toContain("Perspective: uuid-123");
    expect(msg).toContain("Subscription: mention-abc");
    expect(msg).toContain("Event type: mention");
    expect(msg).toContain("Neighbourhood: neighbourhood://Qm123");
  });

  it("builds a channel-messages wake message", () => {
    const msg = buildWakeMessage(
      config,
      channelSub,
      "did:key:z6Mk456",
      "literal://string:channel-1",
    );
    expect(msg).toContain("New messages in an AD4M neighbourhood.");
    expect(msg).toContain("Parent: literal://string:channel-1");
    expect(msg).toContain("Perspective: uuid-456");
    expect(msg).toContain("Event type: channel-messages");
  });

  it("omits Parent line when parent is empty", () => {
    const msg = buildWakeMessage(config, mentionSub, "did:key:z6Mk123", "");
    expect(msg).not.toContain("Parent:");
  });

  it("omits Neighbourhood line when not set", () => {
    const msg = buildWakeMessage(config, channelSub, "did:key:z6Mk456", "ch1");
    expect(msg).not.toContain("Neighbourhood:");
  });
});

// ---------------------------------------------------------------------------
// postWake
// ---------------------------------------------------------------------------

describe("postWake", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("POSTs wake message to the configured wakeUrl", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      new Response(null, { status: 200 }),
    );

    const config: PluginConfig = {
      mcpEndpoint: "http://localhost:3001/mcp",
      adminCredential: "cred",
      wakeUrl: "http://localhost:18789/hooks/wake",
      wakeToken: "wake-token-123",
    };

    const sub: WakerSubscription = {
      id: "test-sub",
      type: "mention",
      perspective: "uuid-1",
      channel: "",
      query: "SELECT ...",
    };

    const logger = makeMockLogger();
    await postWake(config, sub, "did:key:abc", logger);

    expect(fetch).toHaveBeenCalledOnce();
    const call = vi.mocked(fetch).mock.calls[0];
    expect(call[0]).toBe("http://localhost:18789/hooks/wake");

    const opts = call[1] as RequestInit;
    const headers = opts.headers as Record<string, string>;
    expect(headers["Authorization"]).toBe("Bearer wake-token-123");

    const body = JSON.parse(opts.body as string);
    expect(body.mode).toBe("now");
    expect(body.text).toContain("You were @mentioned");
  });

  it("logs error on fetch failure", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("Network error"));

    const config: PluginConfig = {
      wakeUrl: "http://localhost:18789/hooks/wake",
      wakeToken: "t",
      adminCredential: "c",
    };

    const sub: WakerSubscription = {
      id: "s",
      type: "mention",
      perspective: "p",
      channel: "",
      query: "q",
    };

    const logger = makeMockLogger();
    await postWake(config, sub, "did:key:x", logger);

    expect(logger.error).toHaveBeenCalledWith(
      expect.stringContaining("Network error"),
    );
  });

  it("logs error on non-OK response", async () => {
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      new Response("Forbidden", { status: 403 }),
    );

    const config: PluginConfig = {
      wakeUrl: "http://localhost:18789/hooks/wake",
      wakeToken: "t",
      adminCredential: "c",
    };

    const sub: WakerSubscription = {
      id: "s",
      type: "channel-messages",
      perspective: "p",
      channel: "ch",
      query: "q",
    };

    const logger = makeMockLogger();
    await postWake(config, sub, "did:key:x", logger);

    expect(logger.error).toHaveBeenCalledWith(
      expect.stringContaining("wake POST failed: 403"),
    );
  });
});

// ---------------------------------------------------------------------------
// ad4mPlugin (main export)
// ---------------------------------------------------------------------------

describe("ad4mPlugin", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("registers expected tools and services", async () => {
    const registeredTools: Array<{ name: string; [k: string]: any }> = [];
    const registeredServices: Array<{ id: string; [k: string]: any }> = [];

    // Mock fetch to fail gracefully (no real executor running)
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("No executor"));

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        adminCredential: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn((svc: any) => registeredServices.push(svc)),
    };

    await ad4mPlugin(mockApi);

    // Check that base tools are registered
    const toolNames = registeredTools.map((t) => t.name);
    expect(toolNames).toContain("ad4m_get_sample_config");
    expect(toolNames).toContain("refresh_ad4m_tools");
    expect(toolNames).toContain("subscribe_to_mentions");
    expect(toolNames).toContain("unsubscribe_from_mentions");
    expect(toolNames).toContain("subscribe_to_children");
    expect(toolNames).toContain("unsubscribe_from_children");
    expect(toolNames).toContain("list_waker_subscriptions");

    // Check services
    const serviceIds = registeredServices.map((s) => s.id);
    expect(serviceIds).toContain("ad4m-mcp");
    expect(serviceIds).toContain("ad4m-waker");
  });

  it("ad4m_get_sample_config tool returns config examples", async () => {
    const registeredTools: Array<{ name: string; execute: Function }> = [];

    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("No executor"));

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        adminCredential: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    const sampleConfigTool = registeredTools.find(
      (t) => t.name === "ad4m_get_sample_config",
    );
    expect(sampleConfigTool).toBeDefined();

    const result = await sampleConfigTool!.execute();
    expect(result.content[0].text).toContain("Managed mode");
    expect(result.content[0].text).toContain("External mode");
  });

  it("list_waker_subscriptions returns empty when no subscriptions", async () => {
    const registeredTools: Array<{ name: string; execute: Function }> = [];

    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("No executor"));

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        adminCredential: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    const listTool = registeredTools.find(
      (t) => t.name === "list_waker_subscriptions",
    );
    expect(listTool).toBeDefined();

    const result = await listTool!.execute();
    expect(result.content[0].text).toContain("No active waker subscriptions");
  });

  it("refresh_ad4m_tools returns count when MCP is unavailable", async () => {
    const registeredTools: Array<{ name: string; execute: Function }> = [];

    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("No executor"));

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        adminCredential: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    const refreshTool = registeredTools.find(
      (t) => t.name === "refresh_ad4m_tools",
    );
    expect(refreshTool).toBeDefined();

    const result = await refreshTool!.execute();
    // MCP unavailable, so "No new tools found"
    expect(result.content[0].text).toContain("No new tools found");
  });

  it("mcp-service start connects and registers MCP tools", async () => {
    const registeredTools: Array<{ name: string; execute: Function }> = [];
    const registeredServices: Array<{
      id: string;
      start: Function;
      stop: Function;
    }> = [];

    const initResponse = {
      jsonrpc: "2.0",
      id: 1,
      result: { serverInfo: { name: "ad4m" } },
    };
    const toolsResponse = {
      jsonrpc: "2.0",
      id: 2,
      result: {
        tools: [
          {
            name: "list_perspectives",
            description: "List all perspectives",
            inputSchema: {
              $schema: "https://json-schema.org/draft/2020-12/schema",
              type: "object",
              properties: {},
            },
          },
        ],
      },
    };

    let callCount = 0;
    vi.spyOn(globalThis, "fetch").mockImplementation(async (_url, opts) => {
      callCount++;
      const body = JSON.parse((opts as any).body as string);

      if (body.method === "initialize") {
        return new Response(JSON.stringify(initResponse), {
          status: 200,
          headers: {
            "Content-Type": "application/json",
            "Mcp-Session-Id": "sess-1",
          },
        });
      }
      if (body.method === "notifications/initialized") {
        return new Response(null, { status: 200 });
      }
      if (body.method === "tools/list") {
        return fakeJsonResponse(toolsResponse);
      }
      return fakeJsonResponse({ jsonrpc: "2.0", id: body.id, result: {} });
    });

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        adminCredential: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn((svc: any) => registeredServices.push(svc)),
    };

    await ad4mPlugin(mockApi);

    // Start the MCP service
    const mcpService = registeredServices.find((s) => s.id === "ad4m-mcp");
    expect(mcpService).toBeDefined();
    await mcpService!.start();

    // Should now have the MCP tool registered (list_perspectives)
    const toolNames = registeredTools.map((t) => t.name);
    expect(toolNames).toContain("list_perspectives");

    // The registered tool should strip $schema
    const lpTool = registeredTools.find((t) => t.name === "list_perspectives");
    expect(lpTool).toBeDefined();
    // Check that $schema was stripped by calling the tool
    expect((lpTool as any).parameters.$schema).toBeUndefined();

    // Stop the service
    mcpService!.stop();
  });

  it("managed mode uses stored or generated credentials", async () => {
    // Store a known credential
    const knownCred = "known-test-cred-" + Date.now();
    storeAdminCredential(knownCred);

    // In managed mode, isExecutorRunning will be called via ensureExecutorRunning
    // Mock fetch to simulate executor already running
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({ jsonrpc: "2.0", id: 0, result: {} }),
    );

    const mockApi = {
      pluginConfig: {
        mode: "managed",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn(),
      registerService: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    // Logger should show the credential was found (it logs the length)
    const infoMessages = mockApi.logger.info.mock.calls.map((c: any[]) => c[0]);
    const credMessage = infoMessages.find((m: string) =>
      m.includes("Using adminCredential"),
    );
    expect(credMessage).toBeDefined();
    expect(credMessage).toContain(`length: ${knownCred.length}`);

    // Clean up
    try {
      const dataPath = getPluginDataPath();
      fs.unlinkSync(path.join(dataPath, "admin.credential"));
    } catch {
      // ignore
    }
  });

  it("warns when adminCredential is not configured in external mode", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("No executor"));

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        // no adminCredential
      },
      logger: makeMockLogger(),
      registerTool: vi.fn(),
      registerService: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    const warnMessages = mockApi.logger.warn.mock.calls.map((c: any[]) => c[0]);
    const warningFound = warnMessages.some((m: string) =>
      m.includes("adminCredential not configured"),
    );
    expect(warningFound).toBe(true);
  });
});
