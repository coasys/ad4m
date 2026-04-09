import { NeighbourhoodClient } from "./NeighbourhoodClient";
import { NeighbourhoodProxy } from "./NeighbourhoodProxy";

// Mock RestClient's subscribe to avoid real SSE connections
jest.mock('../restClient', () => {
  return {
    RestClient: jest.fn().mockImplementation(() => ({
      get: jest.fn(),
      post: jest.fn(),
      put: jest.fn(),
      delete: jest.fn(),
      subscribe: jest.fn().mockReturnValue(() => {}),
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

    neighbourhoodClient.dispatchSignal(neighbourhoodURI, true);

    expect(callbacks).toBe(2);
  });

  it("should not add multiple subscriptions when removing and adding another signal handler", async () => {
    const neighbourhoodURI = "did://123";

    // Track subscribe calls via the mock
    let subscribeCallCount = 0;
    const { RestClient } = jest.requireMock('../restClient');
    RestClient.mockImplementation(() => ({
      get: jest.fn(),
      post: jest.fn(),
      put: jest.fn(),
      delete: jest.fn(),
      subscribe: jest.fn().mockImplementation(() => {
        subscribeCallCount++;
        return () => {};
      }),
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

    // Check that only one subscription was added (handlers share subscription per perspective)
    expect(subscribeCallCount).toBe(1);

    // Dispatch signal
    neighbourhoodClient.dispatchSignal(neighbourhoodURI, true);

    // Check that only handler2 was called (handler1 was removed)
    expect(callbacks1).toBe(0);
    expect(callbacks2).toBe(1);
  });
});
