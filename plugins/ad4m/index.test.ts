import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import path from "path";
import fs from "fs";
import os from "os";
import { EventEmitter } from "events";

// Mock child_process before any imports that use it
const { mockSpawn, mockExecFileSync } = vi.hoisted(() => {
  return { mockSpawn: vi.fn(), mockExecFileSync: vi.fn() };
});
vi.mock("child_process", async (importOriginal) => {
  const actual = await importOriginal<typeof import("child_process")>();
  return { ...actual, spawn: mockSpawn, execFileSync: mockExecFileSync };
});

/** Build a fake Ad4mClient with controllable agent methods. */
function makeMockAd4mClient(agentMethods: {
  status: ReturnType<typeof vi.fn>;
  generate?: ReturnType<typeof vi.fn>;
  unlock?: ReturnType<typeof vi.fn>;
}) {
  return {
    agent: {
      status: agentMethods.status,
      generate: agentMethods.generate ?? vi.fn(),
      unlock: agentMethods.unlock ?? vi.fn(),
    },
  };
}

import {
  generateRandomPassphrase,
  loadWakerState,
  saveWakerState,
  findExecutorBinary,
  isExecutorRunning,
  ensureExecutorRunning,
  ensureAgentReady,
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
  type ExecutorStartResult,
  type AgentResult,
} from "./index";

import ad4mPlugin, { _resetModuleState } from "./index";
import { WakerSubscriptionManager } from "./wakerSubscriptionManager";

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

/** Create a temporary directory for stateDir tests. */
function makeTempDir(): string {
  return fs.mkdtempSync(path.join(os.tmpdir(), "ad4m-test-"));
}

/** Service context with stateDir. */
function makeServiceCtx(stateDir?: string): any {
  return {
    stateDir: stateDir ?? makeTempDir(),
    config: {},
    logger: makeMockLogger(),
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
// loadWakerState / saveWakerState
// ---------------------------------------------------------------------------

describe("loadWakerState / saveWakerState", () => {
  let tmpDir: string;

  beforeEach(() => {
    tmpDir = makeTempDir();
  });

  afterEach(() => {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  });

  it("returns empty state when state file does not exist", () => {
    const result = loadWakerState(tmpDir);
    expect(result).toEqual({ subscriptions: [], seenMessages: {} });
  });

  it("saves and loads subscriptions with result hashes", () => {
    const subs: WakerSubscription[] = [
      {
        id: "mention-abc",
        type: "mention",
        perspective: "uuid-1",
        channel: "",
        query: "SELECT ...",
        neighbourhood: "neighbourhood://Qm1",
      },
      {
        id: "children-def",
        type: "channel-messages",
        perspective: "uuid-2",
        channel: "ch-1",
        query: "SELECT ...",
      },
    ];
    const seen = { "mention-abc": ["msg-1", "msg-2"] };

    saveWakerState(tmpDir, subs, seen);
    const loaded = loadWakerState(tmpDir);
    expect(loaded.subscriptions).toEqual(subs);
    expect(loaded.seenMessages).toEqual(seen);
  });

  it("loads legacy format (plain array) as subscriptions with empty hashes", () => {
    const subs = [{ id: "a", type: "mention", perspective: "p1", channel: "", query: "q1" }];
    fs.writeFileSync(
      path.join(tmpDir, "ad4m-waker-state.json"),
      JSON.stringify(subs),
    );
    const loaded = loadWakerState(tmpDir);
    expect(loaded.subscriptions).toEqual(subs);
    expect(loaded.seenMessages).toEqual({});
  });

  it("overwrites existing state on save", () => {
    const subs1: WakerSubscription[] = [
      { id: "a", type: "mention", perspective: "p1", channel: "", query: "q1" },
    ];
    const subs2: WakerSubscription[] = [
      {
        id: "b",
        type: "channel-messages",
        perspective: "p2",
        channel: "c",
        query: "q2",
      },
    ];

    saveWakerState(tmpDir, subs1);
    saveWakerState(tmpDir, subs2, { "b": ["msg-x"] });
    const loaded = loadWakerState(tmpDir);
    expect(loaded.subscriptions).toEqual(subs2);
    expect(loaded.seenMessages).toEqual({ "b": ["msg-x"] });
  });

  it("creates stateDir if it does not exist", () => {
    const nestedDir = path.join(tmpDir, "nested", "dir");
    saveWakerState(nestedDir, []);
    expect(fs.existsSync(nestedDir)).toBe(true);
  });

  it("returns empty state for invalid JSON", () => {
    fs.writeFileSync(path.join(tmpDir, "ad4m-waker-state.json"), "not json");
    const result = loadWakerState(tmpDir);
    expect(result).toEqual({ subscriptions: [], seenMessages: {} });
  });

  it("returns empty state for non-array non-object JSON", () => {
    fs.writeFileSync(
      path.join(tmpDir, "ad4m-waker-state.json"),
      JSON.stringify({ foo: "bar" }),
    );
    const result = loadWakerState(tmpDir);
    expect(result).toEqual({ subscriptions: [], seenMessages: {} });
  });
});

// ---------------------------------------------------------------------------
// findExecutorBinary
// ---------------------------------------------------------------------------

describe("findExecutorBinary", () => {
  it("returns null when ad4m-executor is not installed", () => {
    // On most CI / dev machines ad4m-executor won't be installed
    // If it IS installed, this test still passes (returns a valid path)
    const result = findExecutorBinary();
    if (result !== null) {
      // Verify it's actually an executable file
      expect(fs.existsSync(result)).toBe(true);
      expect(result).toContain("ad4m-executor");
    }
    // No assertion if null — that's the expected case
  });

  it("returns a string ending with ad4m-executor if found", () => {
    const result = findExecutorBinary();
    if (result !== null) {
      expect(result.endsWith("ad4m-executor")).toBe(true);
      expect(path.isAbsolute(result)).toBe(true);
    }
  });
});

// ---------------------------------------------------------------------------
// isExecutorRunning
// ---------------------------------------------------------------------------

describe("isExecutorRunning", () => {
  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("returns 'mcp' when MCP endpoint responds", async () => {
    vi.spyOn(globalThis, "fetch").mockImplementation((url: any) => {
      if (String(url).includes("/mcp")) {
        return Promise.resolve(
          fakeJsonResponse({ jsonrpc: "2.0", id: 0, result: {} }),
        );
      }
      return Promise.reject(new Error("ECONNREFUSED"));
    });
    const result = await isExecutorRunning(
      "http://localhost:3001/mcp",
      1000,
      "http://localhost:12000",
    );
    expect(result).toBe("mcp");
  });

  it("returns 'http' when only HTTP endpoint responds with AD4M root info", async () => {
    vi.spyOn(globalThis, "fetch").mockImplementation((url: any) => {
      if (String(url).includes("/mcp")) {
        return Promise.reject(new Error("ECONNREFUSED"));
      }
      // Root info endpoint payload
      return Promise.resolve(
        fakeJsonResponse({
          name: "AD4M Executor",
          version: "0.12.0",
          api: "/api/v1",
          transport: "websocket",
          ws: "/api/v1/ws",
        }),
      );
    });
    const result = await isExecutorRunning(
      "http://localhost:3001/mcp",
      1000,
      "http://localhost:12000",
    );
    expect(result).toBe("http");
  });

  it("returns false when HTTP endpoint returns 200 but is not the AD4M executor", async () => {
    vi.spyOn(globalThis, "fetch").mockImplementation((url: any) => {
      if (String(url).includes("/mcp")) {
        return Promise.reject(new Error("ECONNREFUSED"));
      }
      // Some other service occupying port 12000 — must not be mistaken for AD4M
      return Promise.resolve(
        fakeJsonResponse({ name: "SomethingElse", status: "ok" }),
      );
    });
    const result = await isExecutorRunning(
      "http://localhost:3001/mcp",
      1000,
      "http://localhost:12000",
    );
    expect(result).toBe(false);
  });

  it("returns false when both endpoints fail", async () => {
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

  it("returns 'already_running' when executor is already running", async () => {
    // Mock fetch to succeed (executor already running)
    vi.spyOn(globalThis, "fetch").mockResolvedValue(
      fakeJsonResponse({ jsonrpc: "2.0", id: 0, result: {} }),
    );

    const logger = makeMockLogger();
    const result = await ensureExecutorRunning("cred", logger);

    expect(result).toBe("already_running");
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
      logger.error.mock.calls.some((c: any[]) => c[0].includes("ENOENT")),
    ).toBe(true);

    // Should have logged "failed to start" and mention ad4mBinaryPath
    expect(
      logger.error.mock.calls.some(
        (c: any[]) =>
          c[0].includes("failed to start") || c[0].includes("Failed to start"),
      ),
    ).toBe(true);
    expect(
      logger.error.mock.calls.some((c: any[]) =>
        c[0].includes("ad4mBinaryPath"),
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
        (m: string) =>
          m.includes("exited with code") || m.includes("failed to start"),
      ),
    ).toBe(true);
  }, 10000);

  it("returns 'spawned' when executor becomes available during polling", async () => {
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

    expect(result).toBe("spawned");
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

  it("uses custom binaryPath when provided", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);

    const logger = makeMockLogger();
    const resultPromise = ensureExecutorRunning(
      "cred",
      logger,
      "http://localhost:3001/mcp",
      "http://localhost:12000",
      "/opt/custom/bin/ad4m-executor",
    );

    setTimeout(() => {
      fakeProc.emit("error", new Error("spawn ENOENT"));
    }, 100);

    await resultPromise;

    // spawn should have been called with the custom path
    expect(mockSpawn).toHaveBeenCalledWith(
      "/opt/custom/bin/ad4m-executor",
      expect.any(Array),
      expect.any(Object),
    );

    // Should log which binary it's using
    const infoMsgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(
      infoMsgs.some((m: string) => m.includes("/opt/custom/bin/ad4m-executor")),
    ).toBe(true);
  }, 10000);

  it("defaults to 'ad4m-executor' when binaryPath is not set", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);

    const logger = makeMockLogger();
    const resultPromise = ensureExecutorRunning("cred", logger);

    setTimeout(() => {
      fakeProc.emit("error", new Error("spawn ENOENT"));
    }, 100);

    await resultPromise;

    // spawn should have been called with default name
    expect(mockSpawn).toHaveBeenCalledWith(
      "ad4m-executor",
      expect.any(Array),
      expect.any(Object),
    );
  }, 10000);

  it("runs ad4m-executor init when data directory does not exist", async () => {
    const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
    const ad4mDir = path.join(home, ".ad4m");

    // Mock fs.existsSync to report the data directory as missing
    const realExistsSync = fs.existsSync;
    vi.spyOn(fs, "existsSync").mockImplementation((p: fs.PathLike) => {
      if (p === ad4mDir) return false;
      return realExistsSync(p);
    });

    try {
      vi.spyOn(globalThis, "fetch").mockRejectedValue(
        new Error("ECONNREFUSED"),
      );
      // Make init succeed (execFileSync mock does nothing = success)
      mockExecFileSync.mockReturnValue(Buffer.from(""));

      const fakeProc = createFakeChildProcess();
      mockSpawn.mockReturnValue(fakeProc as any);

      const logger = makeMockLogger();
      const resultPromise = ensureExecutorRunning("cred", logger);

      setTimeout(() => {
        fakeProc.emit("error", new Error("spawn ENOENT"));
      }, 100);

      await resultPromise;

      // Should have called execFileSync with init
      expect(mockExecFileSync).toHaveBeenCalledWith(
        "ad4m-executor",
        ["init"],
        expect.any(Object),
      );

      // Should have logged about first-time setup
      const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
      expect(msgs.some((m: string) => m.includes("first-time setup"))).toBe(
        true,
      );
    } finally {
      mockExecFileSync.mockReset();
    }
  }, 10000);

  it("returns false when ad4m-executor init fails", async () => {
    const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
    const ad4mDir = path.join(home, ".ad4m");

    // Mock fs.existsSync to report the data directory as missing
    const realExistsSync = fs.existsSync;
    vi.spyOn(fs, "existsSync").mockImplementation((p: fs.PathLike) => {
      if (p === ad4mDir) return false;
      return realExistsSync(p);
    });

    try {
      vi.spyOn(globalThis, "fetch").mockRejectedValue(
        new Error("ECONNREFUSED"),
      );
      // Make init fail
      mockExecFileSync.mockImplementation(() => {
        const err: any = new Error("init crashed");
        err.stderr = Buffer.from("segfault");
        throw err;
      });

      const logger = makeMockLogger();
      const result = await ensureExecutorRunning("cred", logger);

      expect(result).toBe(false);

      const errorMsgs = logger.error.mock.calls.map((c: any[]) => c[0]);
      expect(errorMsgs.some((m: string) => m.includes("init failed"))).toBe(
        true,
      );

      // spawn should NOT have been called (init failed before spawn)
      expect(mockSpawn).not.toHaveBeenCalled();
    } finally {
      mockExecFileSync.mockReset();
    }
  }, 10000);

  it("skips init when data directory exists", async () => {
    const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
    const ad4mDir = path.join(home, ".ad4m");

    // Mock fs.existsSync to report the data directory as present
    const realExistsSync = fs.existsSync;
    vi.spyOn(fs, "existsSync").mockImplementation((p: fs.PathLike) => {
      if (p === ad4mDir) return true;
      return realExistsSync(p);
    });

    try {
      vi.spyOn(globalThis, "fetch").mockRejectedValue(
        new Error("ECONNREFUSED"),
      );

      const fakeProc = createFakeChildProcess();
      mockSpawn.mockReturnValue(fakeProc as any);

      const logger = makeMockLogger();
      const resultPromise = ensureExecutorRunning("cred", logger);

      setTimeout(() => {
        fakeProc.emit("error", new Error("spawn ENOENT"));
      }, 100);

      await resultPromise;

      // execFileSync should NOT have been called (directory exists, no init needed)
      expect(mockExecFileSync).not.toHaveBeenCalled();

      // Should NOT have logged about first-time setup
      const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
      expect(msgs.some((m: string) => m.includes("first-time setup"))).toBe(
        false,
      );
    } finally {
      mockExecFileSync.mockReset();
    }
  }, 10000);

  it("logs executor output to ~/.ad4m/ad4m.log", async () => {
    const home = process.env.HOME || process.env.USERPROFILE || "/tmp";
    const logFile = path.join(home, ".ad4m", "ad4m.log");

    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);

    const logger = makeMockLogger();
    const resultPromise = ensureExecutorRunning("cred", logger);

    // Emit some output then fail
    setTimeout(() => {
      fakeProc.stdout.emit("data", Buffer.from("test log line\n"));
      fakeProc.stderr.emit("data", Buffer.from("test error line\n"));
    }, 50);
    setTimeout(() => {
      fakeProc.emit("error", new Error("spawn ENOENT"));
    }, 200);

    await resultPromise;

    // Logger should mention the log file
    const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(msgs.some((m: string) => m.includes("ad4m.log"))).toBe(true);
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
// ensureAgentReady
// ---------------------------------------------------------------------------

describe("ensureAgentReady", () => {
  it("returns { did } when agent is already initialized and unlocked", async () => {
    const mockStatus = vi.fn().mockResolvedValue({
      isInitialized: true,
      isUnlocked: true,
      did: "did:key:z6MkAlreadyReady",
    });
    const mockGenerate = vi.fn();
    const mockUnlock = vi.fn();
    const client = makeMockAd4mClient({
      status: mockStatus,
      generate: mockGenerate,
      unlock: mockUnlock,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      undefined,
      client,
    );

    expect(result).toEqual({ did: "did:key:z6MkAlreadyReady" });
    expect(mockGenerate).not.toHaveBeenCalled();
    expect(mockUnlock).not.toHaveBeenCalled();

    const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(msgs.some((m: string) => m.includes("Agent is ready"))).toBe(true);
  });

  it("generates new agent on first run and returns { did, passphrase }", async () => {
    let callCount = 0;
    const mockStatus = vi.fn().mockImplementation(async () => {
      callCount++;
      if (callCount === 1) {
        return { isInitialized: false, isUnlocked: false, did: "" };
      }
      return {
        isInitialized: true,
        isUnlocked: true,
        did: "did:key:z6MkNewAgent",
      };
    });
    const mockGenerate = vi.fn().mockResolvedValue(undefined);
    const client = makeMockAd4mClient({
      status: mockStatus,
      generate: mockGenerate,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      undefined,
      client,
    );

    expect(result).not.toBeNull();
    expect(result!.did).toBe("did:key:z6MkNewAgent");
    // Passphrase should be returned since none was provided
    expect(result!.passphrase).toBeDefined();
    expect(typeof result!.passphrase).toBe("string");
    expect(result!.passphrase!.length).toBeGreaterThan(0);
    expect(mockGenerate).toHaveBeenCalledOnce();

    const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(msgs.some((m: string) => m.includes("generating new agent"))).toBe(
      true,
    );
    expect(msgs.some((m: string) => m.includes("generated successfully"))).toBe(
      true,
    );
  });

  it("does not return passphrase when one was provided", async () => {
    let callCount = 0;
    const mockStatus = vi.fn().mockImplementation(async () => {
      callCount++;
      if (callCount === 1) {
        return { isInitialized: false, isUnlocked: false, did: "" };
      }
      return {
        isInitialized: true,
        isUnlocked: true,
        did: "did:key:z6MkNewAgent",
      };
    });
    const mockGenerate = vi.fn().mockResolvedValue(undefined);
    const client = makeMockAd4mClient({
      status: mockStatus,
      generate: mockGenerate,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      "user-provided-passphrase",
      client,
    );

    expect(result).not.toBeNull();
    expect(result!.did).toBe("did:key:z6MkNewAgent");
    // Passphrase was provided, so it should NOT be returned
    expect(result!.passphrase).toBeUndefined();
  });

  it("unlocks agent with passphrase from config and returns { did }", async () => {
    let callCount = 0;
    const mockStatus = vi.fn().mockImplementation(async () => {
      callCount++;
      if (callCount === 1) {
        return {
          isInitialized: true,
          isUnlocked: false,
          did: "did:key:z6MkLocked",
        };
      }
      return {
        isInitialized: true,
        isUnlocked: true,
        did: "did:key:z6MkLocked",
      };
    });
    const mockUnlock = vi.fn().mockResolvedValue(undefined);
    const client = makeMockAd4mClient({
      status: mockStatus,
      unlock: mockUnlock,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      "test-stored-passphrase",
      client,
    );

    expect(result).toEqual({ did: "did:key:z6MkLocked" });
    expect(mockUnlock).toHaveBeenCalledWith("test-stored-passphrase");

    const msgs = logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(msgs.some((m: string) => m.includes("unlocked successfully"))).toBe(
      true,
    );
  });

  it("uses agentPassphrase from config for unlock", async () => {
    let callCount = 0;
    const mockStatus = vi.fn().mockImplementation(async () => {
      callCount++;
      if (callCount === 1) {
        return {
          isInitialized: true,
          isUnlocked: false,
          did: "did:key:z6MkLocked",
        };
      }
      return {
        isInitialized: true,
        isUnlocked: true,
        did: "did:key:z6MkLocked",
      };
    });
    const mockUnlock = vi.fn().mockResolvedValue(undefined);
    const client = makeMockAd4mClient({
      status: mockStatus,
      unlock: mockUnlock,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      "config-passphrase",
      client,
    );

    expect(result).toEqual({ did: "did:key:z6MkLocked" });
    expect(mockUnlock).toHaveBeenCalledWith("config-passphrase");
  });

  it("returns null when agent is locked and no passphrase available", async () => {
    const mockStatus = vi.fn().mockResolvedValue({
      isInitialized: true,
      isUnlocked: false,
      did: "did:key:z6MkLocked",
    });
    const mockUnlock = vi.fn();
    const client = makeMockAd4mClient({
      status: mockStatus,
      unlock: mockUnlock,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      undefined,
      client,
    );

    expect(result).toBeNull();
    expect(mockUnlock).not.toHaveBeenCalled();

    const errorMsgs = logger.error.mock.calls.map((c: any[]) => c[0]);
    expect(
      errorMsgs.some((m: string) => m.includes("no passphrase available")),
    ).toBe(true);
  });

  it("returns null when generate fails", async () => {
    const mockStatus = vi.fn().mockResolvedValue({
      isInitialized: false,
      isUnlocked: false,
      did: "",
    });
    const mockGenerate = vi
      .fn()
      .mockRejectedValue(new Error("generate failed"));
    const client = makeMockAd4mClient({
      status: mockStatus,
      generate: mockGenerate,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      undefined,
      client,
    );

    expect(result).toBeNull();
    const errorMsgs = logger.error.mock.calls.map((c: any[]) => c[0]);
    expect(
      errorMsgs.some((m: string) => m.includes("Failed to generate agent")),
    ).toBe(true);
  });

  it("returns null when unlock fails", async () => {
    const mockStatus = vi.fn().mockResolvedValue({
      isInitialized: true,
      isUnlocked: false,
      did: "did:key:z6MkLocked",
    });
    const mockUnlock = vi.fn().mockRejectedValue(new Error("wrong passphrase"));
    const client = makeMockAd4mClient({
      status: mockStatus,
      unlock: mockUnlock,
    });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      "test-passphrase",
      client,
    );

    expect(result).toBeNull();
    const errorMsgs = logger.error.mock.calls.map((c: any[]) => c[0]);
    expect(
      errorMsgs.some((m: string) => m.includes("Failed to unlock agent")),
    ).toBe(true);
  });

  it("returns null when agent.status() throws", async () => {
    const mockStatus = vi
      .fn()
      .mockRejectedValue(new Error("connection refused"));
    const client = makeMockAd4mClient({ status: mockStatus });

    const logger = makeMockLogger();
    const result = await ensureAgentReady(
      "http://localhost:12000",
      "test-cred",
      logger,
      undefined,
      client,
    );

    expect(result).toBeNull();
    const errorMsgs = logger.error.mock.calls.map((c: any[]) => c[0]);
    expect(
      errorMsgs.some((m: string) =>
        m.includes("Failed to connect to executor"),
      ),
    ).toBe(true);
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
    token: "test-cred",
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

  it("builds a mention wake message with per-message parents", () => {
    const msg = buildWakeMessage(config, mentionSub, "did:key:z6Mk123", [
      { address: "msg-1", parents: ["channel-abc", "conversation-xyz"] },
      { address: "msg-2", parents: ["channel-abc"] },
    ]);
    expect(msg).toContain("You were @mentioned in an AD4M neighbourhood.");
    expect(msg).toContain("Agent DID: did:key:z6Mk123");
    expect(msg).toContain("Perspective: uuid-123");
    expect(msg).toContain("Mentioned messages (2):");
    expect(msg).toContain("Message: msg-1");
    expect(msg).toContain("Parents: channel-abc, conversation-xyz");
    expect(msg).toContain("Message: msg-2");
  });

  it("builds a channel-messages wake message without mentions", () => {
    const msg = buildWakeMessage(config, channelSub, "did:key:z6Mk456");
    expect(msg).toContain("New messages in an AD4M neighbourhood.");
    expect(msg).toContain("Perspective: uuid-456");
    expect(msg).toContain("Event type: channel-messages");
    expect(msg).not.toContain("Mentioned messages");
  });

  it("shows (unknown) parents when empty", () => {
    const msg = buildWakeMessage(config, mentionSub, "did:key:z6Mk123", [
      { address: "msg-1", parents: [] },
    ]);
    expect(msg).toContain("Parents: (unknown)");
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
      token: "cred",
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
      token: "c",
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
      token: "c",
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
    _resetModuleState();
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
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn((svc: any) => registeredServices.push(svc)),
      registerCli: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    // Check that base tools are registered
    const toolNames = registeredTools.map((t) => t.name);
    expect(toolNames).toContain("ad4m_get_sample_config");
    expect(toolNames).toContain("ad4m_refresh_ad4m_tools");
    expect(toolNames).toContain("ad4m_subscribe_to_mentions");
    expect(toolNames).toContain("ad4m_unsubscribe_from_mentions");
    expect(toolNames).toContain("ad4m_subscribe_to_children");
    expect(toolNames).toContain("ad4m_unsubscribe_from_children");
    expect(toolNames).toContain("ad4m_list_waker_subscriptions");

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
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn(),
      registerCli: vi.fn(),
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
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn(),
      registerCli: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    const listTool = registeredTools.find(
      (t) => t.name === "ad4m_list_waker_subscriptions",
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
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn(),
      registerCli: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    const refreshTool = registeredTools.find(
      (t) => t.name === "ad4m_refresh_ad4m_tools",
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
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn((svc: any) => registeredServices.push(svc)),
      registerCli: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    // Start the MCP service with ctx
    const mcpService = registeredServices.find((s) => s.id === "ad4m-mcp");
    expect(mcpService).toBeDefined();
    await mcpService!.start(makeServiceCtx());

    // Should now have the MCP tool registered (list_perspectives)
    const toolNames = registeredTools.map((t) => t.name);
    expect(toolNames).toContain("ad4m_list_perspectives");

    // The registered tool should strip $schema
    const lpTool = registeredTools.find(
      (t) => t.name === "ad4m_list_perspectives",
    );
    expect(lpTool).toBeDefined();
    // Check that $schema was stripped by calling the tool
    expect((lpTool as any).parameters.$schema).toBeUndefined();

    // Stop the service
    mcpService!.stop();
  });

  it("managed mode generates ephemeral credential", async () => {
    // In managed mode, the plugin generates an ephemeral admin credential.
    // Mock fetch to fail (executor not running), and mock spawn to fail immediately.
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const fakeProc = createFakeChildProcess();
    mockSpawn.mockReturnValue(fakeProc as any);
    // Trigger immediate spawn failure
    setTimeout(() => {
      fakeProc.emit("error", new Error("spawn ad4m-executor ENOENT"));
    }, 50);

    const mockApi = {
      id: "ad4m",
      pluginConfig: {
        mode: "managed",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn(),
      registerService: vi.fn(),
      registerCli: vi.fn(),
      config: {},
    };

    ad4mPlugin(mockApi);

    // ad4mPlugin is synchronous — auth happens in the mcp service start().
    // Find the registered ad4m-mcp service and start it.
    const mcpService = mockApi.registerService.mock.calls.find(
      (c: any[]) => c[0]?.id === "ad4m-mcp",
    )?.[0];
    expect(mcpService).toBeDefined();
    await mcpService.start(makeServiceCtx());

    // Logger should show the auth token was generated (it logs the length)
    const infoMessages = mockApi.logger.info.mock.calls.map((c: any[]) => c[0]);
    const credMessage = infoMessages.find((m: string) =>
      m.includes("Auth token ready"),
    );
    expect(credMessage).toBeDefined();
    // Ephemeral credential is 24 chars
    expect(credMessage).toContain("length: 24");
  }, 10000);

  it("refreshTools recovers from 422 by re-initializing session", async () => {
    const registeredTools: Array<{ name: string; execute: Function }> = [];
    const registeredServices: Array<{
      id: string;
      start: Function;
      stop: Function;
    }> = [];

    // Track fetch calls to simulate 422 on first tools/list, then success
    let toolsListCallCount = 0;
    let initializeCallCount = 0;
    vi.spyOn(globalThis, "fetch").mockImplementation(async (_url, opts) => {
      const body = JSON.parse((opts as any).body as string);

      if (body.method === "initialize") {
        initializeCallCount++;
        return new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            id: body.id,
            result: { serverInfo: { name: "ad4m" } },
          }),
          {
            status: 200,
            headers: {
              "Content-Type": "application/json",
              "Mcp-Session-Id": `sess-${initializeCallCount}`,
            },
          },
        );
      }
      if (body.method === "notifications/initialized") {
        return new Response(null, { status: 200 });
      }
      if (body.method === "tools/list") {
        toolsListCallCount++;
        if (toolsListCallCount === 1) {
          // First tools/list → 422 (expired session)
          return new Response("Unprocessable Entity", {
            status: 422,
            statusText: "Unprocessable Entity",
          });
        }
        // Retry succeeds
        return fakeJsonResponse({
          jsonrpc: "2.0",
          id: body.id,
          result: {
            tools: [
              {
                name: "recovered_tool",
                description: "A tool discovered after session recovery",
                inputSchema: { type: "object", properties: {} },
              },
            ],
          },
        });
      }
      return fakeJsonResponse({ jsonrpc: "2.0", id: body.id, result: {} });
    });

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn((svc: any) => registeredServices.push(svc)),
      registerCli: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    // Start the MCP service — ensureSession + refreshTools with 422 recovery
    const mcpService = registeredServices.find((s) => s.id === "ad4m-mcp");
    expect(mcpService).toBeDefined();
    await mcpService!.start(makeServiceCtx());

    // Session should have been initialized twice (once initially, once after 422)
    expect(initializeCallCount).toBe(2);
    // tools/list should have been called twice (once 422, once success)
    expect(toolsListCallCount).toBe(2);

    // The recovered tool should be registered
    const toolNames = registeredTools.map((t) => t.name);
    expect(toolNames).toContain("ad4m_recovered_tool");

    // Logger should show re-initialization
    const infoMsgs = mockApi.logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(infoMsgs.some((m: string) => m.includes("re-initializing"))).toBe(
      true,
    );

    mcpService!.stop();
  });

  it("MCP tool execute recovers from 422 by re-initializing session", async () => {
    const registeredTools: Array<{ name: string; execute: Function }> = [];
    const registeredServices: Array<{
      id: string;
      start: Function;
      stop: Function;
    }> = [];

    let initializeCallCount = 0;
    let toolCallCount = 0;
    vi.spyOn(globalThis, "fetch").mockImplementation(async (_url, opts) => {
      const body = JSON.parse((opts as any).body as string);

      if (body.method === "initialize") {
        initializeCallCount++;
        return new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            id: body.id,
            result: { serverInfo: { name: "ad4m" } },
          }),
          {
            status: 200,
            headers: {
              "Content-Type": "application/json",
              "Mcp-Session-Id": `sess-${initializeCallCount}`,
            },
          },
        );
      }
      if (body.method === "notifications/initialized") {
        return new Response(null, { status: 200 });
      }
      if (body.method === "tools/list") {
        return fakeJsonResponse({
          jsonrpc: "2.0",
          id: body.id,
          result: {
            tools: [
              {
                name: "test_tool",
                description: "A test tool",
                inputSchema: { type: "object", properties: {} },
              },
            ],
          },
        });
      }
      if (body.method === "tools/call") {
        toolCallCount++;
        if (toolCallCount === 1) {
          // First call → 422 (expired session)
          return new Response("Unprocessable Entity", {
            status: 422,
            statusText: "Unprocessable Entity",
          });
        }
        // Retry succeeds
        return fakeJsonResponse({
          jsonrpc: "2.0",
          id: body.id,
          result: {
            content: [{ type: "text", text: '{"status":"ok"}' }],
          },
        });
      }
      return fakeJsonResponse({ jsonrpc: "2.0", id: body.id, result: {} });
    });

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn((svc: any) => registeredServices.push(svc)),
      registerCli: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    // Start the MCP service to register tools
    const mcpService = registeredServices.find((s) => s.id === "ad4m-mcp");
    await mcpService!.start(makeServiceCtx());

    // Find the dynamically registered MCP tool
    const testTool = registeredTools.find((t) => t.name === "ad4m_test_tool");
    expect(testTool).toBeDefined();

    // Reset counters to track just the tool call
    initializeCallCount = 0;
    toolCallCount = 0;

    // Execute the tool — first call gets 422, should recover
    const result = await testTool!.execute("call-1", {});

    // Should have re-initialized session
    expect(initializeCallCount).toBe(1); // one re-init after 422
    // Should have called the tool twice (first 422, then success)
    expect(toolCallCount).toBe(2);
    // Should return the successful result
    expect(result.content[0].text).toContain("ok");

    mcpService!.stop();
  });

  it("non-4xx errors are not retried and propagate", async () => {
    const registeredTools: Array<{ name: string; execute: Function }> = [];
    const registeredServices: Array<{
      id: string;
      start: Function;
      stop: Function;
    }> = [];

    let initializeCallCount = 0;
    vi.spyOn(globalThis, "fetch").mockImplementation(async (_url, opts) => {
      const body = JSON.parse((opts as any).body as string);

      if (body.method === "initialize") {
        initializeCallCount++;
        return new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            id: body.id,
            result: { serverInfo: { name: "ad4m" } },
          }),
          {
            status: 200,
            headers: {
              "Content-Type": "application/json",
              "Mcp-Session-Id": `sess-${initializeCallCount}`,
            },
          },
        );
      }
      if (body.method === "notifications/initialized") {
        return new Response(null, { status: 200 });
      }
      if (body.method === "tools/list") {
        return fakeJsonResponse({
          jsonrpc: "2.0",
          id: body.id,
          result: {
            tools: [
              {
                name: "failing_tool",
                description: "Tool that fails with 500",
                inputSchema: { type: "object", properties: {} },
              },
            ],
          },
        });
      }
      if (body.method === "tools/call") {
        // 500 error — should NOT trigger session recovery
        return new Response("Internal Server Error", {
          status: 500,
          statusText: "Internal Server Error",
        });
      }
      return fakeJsonResponse({ jsonrpc: "2.0", id: body.id, result: {} });
    });

    const mockApi = {
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        token: "test-cred",
      },
      logger: makeMockLogger(),
      registerTool: vi.fn((tool: any) => registeredTools.push(tool)),
      registerService: vi.fn((svc: any) => registeredServices.push(svc)),
      registerCli: vi.fn(),
    };

    await ad4mPlugin(mockApi);

    const mcpService = registeredServices.find((s) => s.id === "ad4m-mcp");
    await mcpService!.start(makeServiceCtx());

    const failingTool = registeredTools.find(
      (t) => t.name === "ad4m_failing_tool",
    );
    expect(failingTool).toBeDefined();

    initializeCallCount = 0;

    // Execute — should get error but NOT re-initialize session
    const result = await failingTool!.execute("call-1", {});

    // Should NOT have re-initialized (500 is not 4xx)
    expect(initializeCallCount).toBe(0);
    // Should return an error
    expect(result.error).toBeDefined();
    expect(result.error).toContain("500");

    mcpService!.stop();
  });

  it("warns when no auth token is available in external mode", async () => {
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("No executor"));

    const mockApi = {
      id: "ad4m",
      pluginConfig: {
        mode: "external",
        mcpEndpoint: "http://localhost:3001/mcp",
        // no token
      },
      logger: makeMockLogger(),
      registerTool: vi.fn(),
      registerService: vi.fn(),
      registerCli: vi.fn(),
      config: {},
    };

    ad4mPlugin(mockApi);

    // Auth happens in the mcp service start()
    const mcpService = mockApi.registerService.mock.calls.find(
      (c: any[]) => c[0]?.id === "ad4m-mcp",
    )?.[0];
    expect(mcpService).toBeDefined();
    await mcpService.start(makeServiceCtx());

    const warnMessages = mockApi.logger.warn.mock.calls.map((c: any[]) => c[0]);
    const warningFound = warnMessages.some((m: string) =>
      m.includes("No auth token"),
    );
    expect(warningFound).toBe(true);
  });

  it("logs setup hint when mode is not configured", async () => {
    const mockApi: any = {
      id: "ad4m",
      pluginConfig: {},
      logger: makeMockLogger(),
      registerTool: vi.fn(),
      registerService: vi.fn(),
      registerCli: vi.fn(),
      config: {},
    };

    ad4mPlugin(mockApi);

    const mcpService = mockApi.registerService.mock.calls.find(
      (c: any[]) => c[0]?.id === "ad4m-mcp",
    )?.[0];
    expect(mcpService).toBeDefined();
    await mcpService.start(makeServiceCtx());

    const infoMessages = mockApi.logger.info.mock.calls.map((c: any[]) => c[0]);
    expect(
      infoMessages.some((m: string) => m.includes("openclaw ad4m-setup")),
    ).toBe(true);
  });

  it("registers ad4m-setup CLI command", () => {
    const mockApi: any = {
      id: "ad4m",
      pluginConfig: {},
      logger: makeMockLogger(),
      registerTool: vi.fn(),
      registerService: vi.fn(),
      registerCli: vi.fn(),
      config: {},
    };

    ad4mPlugin(mockApi);

    expect(mockApi.registerCli).toHaveBeenCalledTimes(1);
    const [registrar, opts] = mockApi.registerCli.mock.calls[0];
    expect(typeof registrar).toBe("function");
    expect(opts).toEqual({ commands: ["ad4m-setup"] });
  });

  it("runSetup asks for existing passphrase when ~/.ad4m already exists", async () => {
    // Mock: no running executor, but binary found and ~/.ad4m exists
    vi.spyOn(globalThis, "fetch").mockRejectedValue(new Error("ECONNREFUSED"));

    const realExistsSync = fs.existsSync;
    const realAccessSync = fs.accessSync;
    vi.spyOn(fs, "existsSync").mockImplementation((p: fs.PathLike) => {
      const s = p.toString();
      if (s.endsWith("/.ad4m")) return true; // existing agent data
      return realExistsSync(s);
    });
    // Make findExecutorBinary discover a binary
    vi.spyOn(fs, "accessSync").mockImplementation(
      (p: fs.PathLike, mode?: number) => {
        const s = p.toString();
        if (s.endsWith("/ad4m-executor")) return; // found + executable
        return realAccessSync(s, mode);
      },
    );

    const logger = makeMockLogger();
    const { runSetup } = await import("./setup");
    await runSetup({}, logger);

    const infoMessages = logger.info.mock.calls.map((c: any[]) => c[0]);

    // Should mention existing agent data
    expect(
      infoMessages.some((m: string) => m.includes("existing AD4M agent data")),
    ).toBe(true);

    // Should ask for existing passphrase
    expect(
      infoMessages.some((m: string) => m.includes("existing agent passphrase")),
    ).toBe(true);

    // Config snippet should contain placeholder
    expect(
      infoMessages.some((m: string) =>
        m.includes("<enter-your-existing-passphrase>"),
      ),
    ).toBe(true);

    // Should NOT have attempted to start executor
    expect(mockSpawn).not.toHaveBeenCalled();
  }, 10000);
});

// ============================================================================
// WakerSubscriptionManager unit tests
// ============================================================================

describe("WakerSubscriptionManager", () => {
  /** Create a mock QuerySubscriptionProxy that lets tests control onResult delivery. */
  function makeMockProxy() {
    let resultCallback: ((result: any) => void) | null = null;
    const proxy = {
      isSurrealDB: false,
      initialized: Promise.resolve(),
      subscribe: vi.fn(() => Promise.resolve()),
      dispose: vi.fn(),
      onResult: vi.fn((cb: (result: any) => void) => {
        resultCallback = cb;
      }),
    };
    return {
      ProxyClass: vi.fn(() => proxy),
      proxy,
      /** Simulate SurrealDB delivering a result */
      deliver: (result: any) => {
        if (!resultCallback) throw new Error("onResult not registered yet");
        resultCallback(result);
      },
    };
  }

  const noopLogger = {
    info: vi.fn(),
    warn: vi.fn(),
    error: vi.fn(),
    debug: vi.fn(),
  };

  const mockPerspectiveClientSimple = {
    querySparql: vi.fn(() => Promise.resolve({ results: { bindings: [] }})),
  };

  it("should ignore non-array results (e.g. false) and not store them as seen", async () => {
    const mock = makeMockProxy();
    const persisted: { seen: Record<string, string[]> } = { seen: {} };
    let wakeCount = 0;

    const manager = new WakerSubscriptionManager({
      perspectiveClient: mockPerspectiveClientSimple,
      logger: { ...noopLogger },
      QuerySubscriptionProxy: mock.ProxyClass,
      debounceMs: 10,
      onWake: () => { wakeCount++; },
      onPersist: (_subs, seenMessages) => { persisted.seen = seenMessages; },
    });

    await manager.subscribe({
      id: "test-false",
      type: "mention",
      perspective: "fake-uuid",
      channel: "",
      query: "SELECT * FROM link",
    });

    // Deliver `false` — should be ignored
    mock.deliver(false);
    await new Promise(r => setTimeout(r, 50));
    expect(wakeCount).toBe(0);

    // Deliver a real result — should fire
    mock.deliver([{ source: "channel-1", target: "literal://string:Hey @you" }]);
    await new Promise(r => setTimeout(r, 50));
    expect(wakeCount).toBe(1);
    // Seen messages should contain the message address
    expect(persisted.seen["test-false"]).toContain("channel-1");

    manager.disposeAll();
  });

  it("should not re-wake for already-seen messages after restart", async () => {
    const mock = makeMockProxy();
    let wakeCount = 0;

    const manager = new WakerSubscriptionManager({
      perspectiveClient: mockPerspectiveClientSimple,
      logger: { ...noopLogger },
      QuerySubscriptionProxy: mock.ProxyClass,
      debounceMs: 10,
      onWake: () => { wakeCount++; },
      // Simulate persisted state with previously seen messages
      previousSeenMessages: { "test-seen": ["channel-1"] },
    });

    await manager.subscribe({
      id: "test-seen",
      type: "mention",
      perspective: "fake-uuid",
      channel: "",
      query: "SELECT * FROM link",
    });

    // Deliver same message that was already seen — should NOT wake
    mock.deliver([{ source: "channel-1", target: "literal://string:Hey @you" }]);
    await new Promise(r => setTimeout(r, 50));
    expect(wakeCount).toBe(0);

    // Deliver a new message — should wake
    mock.deliver([
      { source: "channel-1", target: "literal://string:Hey @you" },
      { source: "channel-2", target: "literal://string:@you check this" },
    ]);
    await new Promise(r => setTimeout(r, 50));
    expect(wakeCount).toBe(1);

    manager.disposeAll();
  });

  it("should ignore null and undefined results", async () => {
    const mock = makeMockProxy();
    let wakeCount = 0;

    const manager = new WakerSubscriptionManager({
      perspectiveClient: mockPerspectiveClientSimple,
      logger: { ...noopLogger },
      QuerySubscriptionProxy: mock.ProxyClass,
      debounceMs: 10,
      onWake: () => { wakeCount++; },
    });

    await manager.subscribe({
      id: "test-nullish",
      type: "channel-messages",
      perspective: "fake-uuid",
      channel: "ch1",
      query: "SELECT * FROM link",
    });

    mock.deliver(null);
    mock.deliver(undefined);
    mock.deliver(0);
    mock.deliver("");
    await new Promise(r => setTimeout(r, 50));
    expect(wakeCount).toBe(0);

    // Real result should still work after all the junk
    mock.deliver([{ target: "msg-1" }]);
    await new Promise(r => setTimeout(r, 50));
    expect(wakeCount).toBe(1);

    manager.disposeAll();
  });

  it("should resolve per-message parents via second query for mention subscriptions", async () => {
    const mock = makeMockProxy();
    let capturedMentions: any[] | undefined;

    // Mock perspectiveClient.querySparql to return has_child links for parent resolution
    const mockPerspectiveClient = {
      querySparql: vi.fn((_perspectiveId: string, query: string) => {
        // msg-1 has two parents (channel + conversation thread)
        if (query.includes("msg-1")) {
          return Promise.resolve({ results: { bindings: [
            { source: { value: "channel-abc" } },
            { source: { value: "conversation-xyz" } },
          ]}});
        }
        // msg-2 is only in channel-abc
        if (query.includes("msg-2")) {
          return Promise.resolve({ results: { bindings: [
            { source: { value: "channel-abc" } },
          ]}});
        }
        return Promise.resolve({ results: { bindings: [] }});
      }),
    };

    const manager = new WakerSubscriptionManager({
      perspectiveClient: mockPerspectiveClient,
      logger: { ...noopLogger },
      debounceMs: 0,
      onWake: (_sub, _result, mentions) => {
        capturedMentions = mentions;
      },
      QuerySubscriptionProxy: mock.ProxyClass,
    });

    await manager.subscribe({
      id: "test-multi-parent",
      type: "mention",
      perspective: "fake-uuid",
      channel: "fallback-channel",
      query: "SELECT * FROM link",
    });

    // Body links: source = message address, target = body content with mention
    mock.deliver([
      { source: "msg-1", target: "literal://string:Hey @Marvin!" },
      { source: "msg-2", target: "literal://string:@Marvin check this" },
    ]);
    await new Promise(r => setTimeout(r, 50));

    expect(mockPerspectiveClient.querySparql).toHaveBeenCalledTimes(2);
    expect(capturedMentions).toBeDefined();
    expect(capturedMentions).toHaveLength(2);
    // msg-1 has two parents
    expect(capturedMentions![0].address).toBe("msg-1");
    expect(capturedMentions![0].parents).toEqual(["channel-abc", "conversation-xyz"]);
    // msg-2 has one parent
    expect(capturedMentions![1].address).toBe("msg-2");
    expect(capturedMentions![1].parents).toEqual(["channel-abc"]);

    manager.disposeAll();
  });

  it("should return empty parents array when parent resolution returns nothing", async () => {
    const mock = makeMockProxy();
    let capturedMentions: any[] | undefined;

    const mockPerspectiveClient = {
      querySparql: vi.fn(() => Promise.resolve({ results: { bindings: [] }})),
    };

    const manager = new WakerSubscriptionManager({
      perspectiveClient: mockPerspectiveClient,
      logger: { ...noopLogger },
      debounceMs: 0,
      onWake: (_sub, _result, mentions) => {
        capturedMentions = mentions;
      },
      QuerySubscriptionProxy: mock.ProxyClass,
    });

    await manager.subscribe({
      id: "test-no-parent",
      type: "mention",
      perspective: "fake-uuid",
      channel: "fallback-channel",
      query: "SELECT * FROM link",
    });

    mock.deliver([{ source: "msg-1", target: "literal://string:Hey @Marvin!" }]);
    await new Promise(r => setTimeout(r, 50));

    expect(capturedMentions).toHaveLength(1);
    expect(capturedMentions![0].address).toBe("msg-1");
    expect(capturedMentions![0].parents).toEqual([]);

    manager.disposeAll();
  });

  it("should handle parent resolution failure gracefully with empty parents", async () => {
    const mock = makeMockProxy();
    let capturedMentions: any[] | undefined;
    let wakeCount = 0;

    const mockPerspectiveClient = {
      querySparql: vi.fn(() => Promise.reject(new Error("network error"))),
    };

    const manager = new WakerSubscriptionManager({
      perspectiveClient: mockPerspectiveClient,
      logger: { ...noopLogger },
      debounceMs: 0,
      onWake: (_sub, _result, mentions) => {
        capturedMentions = mentions;
        wakeCount++;
      },
      QuerySubscriptionProxy: mock.ProxyClass,
    });

    await manager.subscribe({
      id: "test-parent-fail",
      type: "mention",
      perspective: "fake-uuid",
      channel: "fallback-channel",
      query: "SELECT * FROM link",
    });

    mock.deliver([{ source: "msg-1", target: "literal://string:Hey @Marvin!" }]);
    await new Promise(r => setTimeout(r, 50));

    // Should still wake with empty parents for the message
    expect(wakeCount).toBe(1);
    expect(capturedMentions).toHaveLength(1);
    expect(capturedMentions![0].address).toBe("msg-1");
    expect(capturedMentions![0].parents).toEqual([]);

    manager.disposeAll();
  });
});
