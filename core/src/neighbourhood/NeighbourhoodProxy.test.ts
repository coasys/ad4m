import { NeighbourhoodClient } from "./NeighbourhoodClient";
import { NeighbourhoodProxy } from "./NeighbourhoodProxy";
import { Perspective, PerspectiveExpression } from "../perspectives/Perspective";

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
});
