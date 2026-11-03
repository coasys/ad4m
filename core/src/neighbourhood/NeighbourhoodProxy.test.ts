import { NeighbourhoodClient } from "./NeighbourhoodClient";
import { NeighbourhoodProxy } from "./NeighbourhoodProxy";
import { Perspective, PerspectiveExpression } from "../perspectives/Perspective";
import { PerspectiveClient } from "../perspectives/PerspectiveClient";

const signal = new PerspectiveExpression("did:test:alice", "2026-01-01T00:00:00Z", new Perspective(), { key: "key", signature: "sig" });

// Mock ApiClient's on() to avoid real WebSocket connections
jest.mock('../apiClient', () => {
  return {
    ApiClient: jest.fn().mockImplementation(() => ({
      get: jest.fn(),
      post: jest.fn(),
      put: jest.fn(),
      delete: jest.fn(),
      on: jest.fn().mockReturnValue(() => {}),
      watchApplied: jest.fn().mockResolvedValue(undefined),
      onReconnect: jest.fn().mockReturnValue(() => {}),
    }))
  };
});

describe("NeighbourhoodProxy", () => {
  it("should add multiple signal handlers", async () => {
    const neighbourhoodURI = "did://123";

    const neighbourhoodClient = new NeighbourhoodClient("http://localhost:0", "test-token");
    const neighbourhoodProxy = new NeighbourhoodProxy(
      neighbourhoodClient,
      neighbourhoodURI
    );

    let callbacks = 0;

    const handler1 = () => {
      callbacks++;
    };
    const handler2 = () => {
      callbacks++;
    };

    // Add multiple signal handlers in parallel
    const promise = neighbourhoodProxy.addSignalHandler(handler1);
    neighbourhoodProxy.addSignalHandler(handler2);
    await promise;

    neighbourhoodClient.dispatchSignal(neighbourhoodURI, signal);

    expect(callbacks).toBe(2);
  });

  it("should not add multiple subscriptions when removing and adding another signal handler", async () => {
    const neighbourhoodURI = "did://123";

    // Track on() calls via the mock
    let onCallCount = 0;
    const { ApiClient } = jest.requireMock('../apiClient');
    ApiClient.mockImplementation(() => ({
      get: jest.fn(),
      post: jest.fn(),
      put: jest.fn(),
      delete: jest.fn(),
      on: jest.fn().mockImplementation(() => {
        onCallCount++;
        return () => {};
      }),
      watchApplied: jest.fn().mockResolvedValue(undefined),
      onReconnect: jest.fn().mockReturnValue(() => {}),
    }));

    const neighbourhoodClient = new NeighbourhoodClient("http://localhost:0", "test-token");
    const neighbourhoodProxy = new NeighbourhoodProxy(
      neighbourhoodClient,
      neighbourhoodURI
    );

    let callbacks1 = 0;
    let callbacks2 = 0;

    const handler1 = () => {
      callbacks1++;
    };
    const handler2 = () => {
      callbacks2++;
    };

    // Add signal handler 1
    await neighbourhoodProxy.addSignalHandler(handler1);

    // Remove signal handler 1
    neighbourhoodProxy.removeSignalHandler(handler1);

    // Add signal handler 2
    await neighbourhoodProxy.addSignalHandler(handler2);

    // Check that subscription was re-created (handler1 removed = unsub, handler2 added = new sub)
    expect(onCallCount).toBe(2);

    // Dispatch signal
    neighbourhoodClient.dispatchSignal(neighbourhoodURI, signal);

    // Check that only handler2 was called (handler1 was removed)
    expect(callbacks1).toBe(0);
    expect(callbacks2).toBe(1);
  });

  // `perspective.getNeighbourhoodProxy()` passes no DID, and that is how apps reach sessions.
  describe("createSession with no DID given", () => {
    function proxyAnswering(did: string | null) {
      const client = new NeighbourhoodClient("http://localhost:0", "test-token");
      const callerDid = jest.spyOn(client, "callerDid").mockResolvedValue(did);
      jest.spyOn(client, "sfuStatus").mockRejectedValue(new Error("no sfu"));
      return { proxy: new NeighbourhoodProxy(client, "uuid"), callerDid };
    }

    it("asks the executor for the caller's DID, once", async () => {
      const { proxy, callerDid } = proxyAnswering("did:key:me");
      const session = await proxy.createSession("room", { neighbourhoodUrl: "neighbourhood://n" });
      await proxy.createSession("room-2", { neighbourhoodUrl: "neighbourhood://n" });
      expect(session).toBeDefined();
      expect(callerDid).toHaveBeenCalledTimes(1);
    });

    it("refuses when the executor reports no DID", async () => {
      const { proxy } = proxyAnswering(null);
      await expect(proxy.createSession("room")).rejects.toThrow("no agent DID");
    });
  });

  // WE asks `getNeighbourhoodProxy()` for a new proxy on every call join.
  describe("createSession through a new proxy per join", () => {
    const NAT = { reachability: "nat", isPublic: false, bindAddress: "0.0.0.0:1", advertisedAddress: null, detail: "nat" };
    const PUBLIC = { ...NAT, reachability: "public", isPublic: true, detail: "public" };

    /** An executor socket: `agent.status` and `sfu.status` answer after `rttMs`, or when the test says so. */
    function executor(rttMs = 0) {
      const started: string[] = [];
      const pending: Record<string, Array<{ resolve: (v: unknown) => void; reject: (e: unknown) => void }>> = {};
      const answers: Record<string, () => unknown> = {
        "agent.status": () => ({ did: "did:key:me", isInitialized: true, isUnlocked: true }),
        "sfu.status": () => NAT,
      };
      let deferred = false;
      const reconnects = new Set<() => void>();
      const api = {
        call: jest.fn((method: string) => {
          started.push(method);
          if (!deferred) {
            const reply = new Promise((resolve) => (rttMs ? setTimeout(resolve, rttMs) : resolve(undefined)));
            return reply.then(() => answers[method]?.());
          }
          return new Promise((resolve, reject) => (pending[method] ??= []).push({ resolve, reject }));
        }),
        on: jest.fn().mockReturnValue(() => {}),
        onReconnect: jest.fn((callback: () => void) => {
          reconnects.add(callback);
          return () => reconnects.delete(callback);
        }),
      };
      const perspectives = new PerspectiveClient("http://localhost:0", undefined, api as never);
      perspectives.setNeighbourhoodClient(new NeighbourhoodClient("http://localhost:0", undefined, api as never));
      const join = (room: string) =>
        perspectives.getNeighbourhoodProxy("uuid").createSession(room, { neighbourhoodUrl: "neighbourhood://n" });
      const count = (method: string) => started.filter((m) => m === method).length;
      return {
        answers, started, pending, join, count,
        defer: () => { deferred = true; },
        reconnect: () => reconnects.forEach((callback) => callback()),
      };
    }

    it("reads the caller's DID and the SFU status once for two joins", async () => {
      const ex = executor();
      await ex.join("room-1");
      await ex.join("room-2");
      expect(ex.count("agent.status")).toBe(1);
      expect(ex.count("sfu.status")).toBe(1);
    });

    it("sends agent.status and sfu.status together", async () => {
      const ex = executor();
      ex.defer();
      const joined = ex.join("room");
      for (let i = 0; i < 5; i++) await Promise.resolve();
      expect(ex.started).toEqual(expect.arrayContaining(["agent.status", "sfu.status"]));
      ex.pending["agent.status"][0].resolve({ did: "did:key:me" });
      ex.pending["sfu.status"][0].resolve(NAT);
      await expect(joined).resolves.toBeDefined();
    });

    it("reads the SFU status and DID again after the client reconnects (an executor restart)", async () => {
      const ex = executor();
      await ex.join("room-1");
      ex.answers["sfu.status"] = () => PUBLIC;
      ex.reconnect();
      await ex.join("room-2");
      expect(ex.count("sfu.status")).toBe(2);
      expect(ex.count("agent.status")).toBe(2);
    });

    it("does not keep an answer that arrives after a reconnect it was sent before", async () => {
      const ex = executor();
      ex.defer();
      const first = ex.join("room-1");
      await Promise.resolve();
      // The executor restarts while both reads are out; the old process's answers land after.
      ex.reconnect();
      ex.pending["agent.status"].shift()!.resolve({ did: "did:key:me" });
      ex.pending["sfu.status"].shift()!.resolve(NAT);
      await first;
      const second = ex.join("room-2");
      await Promise.resolve();
      expect(ex.count("agent.status")).toBe(2);
      expect(ex.count("sfu.status")).toBe(2);
      ex.pending["agent.status"].shift()!.resolve({ did: "did:key:me" });
      ex.pending["sfu.status"].shift()!.resolve(PUBLIC);
      await second;
    });

    it("asks for the SFU status again while the SFU is not available", async () => {
      const ex = executor();
      ex.answers["sfu.status"] = () => { throw new Error("SFU service not yet available"); };
      await ex.join("room-1");
      ex.answers["sfu.status"] = () => PUBLIC;
      await ex.join("room-2");
      await ex.join("room-3");
      expect(ex.count("sfu.status")).toBe(2);
      expect(ex.count("agent.status")).toBe(1);
    });

    it("asks for the DID again while the executor reports none", async () => {
      const ex = executor();
      ex.answers["agent.status"] = () => ({ did: null, isInitialized: false, isUnlocked: false });
      await expect(ex.join("room-1")).rejects.toThrow("no agent DID");
      ex.answers["agent.status"] = () => ({ did: "did:key:me" });
      await expect(ex.join("room-2")).resolves.toBeDefined();
      expect(ex.count("agent.status")).toBe(2);
    });

    // Staging-like wall time: SDK_RTT_MS=336 npx jest NeighbourhoodProxy -t "wall time"
    const rtt = Number(process.env.SDK_RTT_MS || 0);
    (rtt ? it : it.skip)("wall time per join at a fixed round trip", async () => {
      const runs = Number(process.env.SDK_RTT_RUNS || 3);
      const median = (ms: number[]) => [...ms].sort((a, b) => a - b)[Math.floor(ms.length / 2)].toFixed(0);
      const first: number[] = [];
      const second: number[] = [];
      for (let i = 0; i < runs; i++) {
        const ex = executor(rtt);
        let start = performance.now();
        await ex.join("room-1");
        first.push(performance.now() - start);
        start = performance.now();
        await ex.join("room-2");
        second.push(performance.now() - start);
      }
      console.log(
        `RTT ${rtt} ms, ${runs} runs: first join median ${median(first)} ms [${first.map((m) => m.toFixed(0))}], ` +
          `next join median ${median(second)} ms [${second.map((m) => m.toFixed(0))}]`,
      );
    });
  });
});
