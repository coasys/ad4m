import { NeighbourhoodClient } from "./NeighbourhoodClient";
import { NeighbourhoodProxy } from "./NeighbourhoodProxy";

describe("NeighbourhoodProxy", () => {
  it("should add multiple signal handlers", async () => {
    const neighbourhoodURI = "did://123";

    const mockApolloClient = {
      subscribe: () => ({
        subscribe: () =>
          new Promise((resolve) => {
            setTimeout(() => {
              resolve({});
            }, 10);
          }),
      }),
    } as any;

    const neighbourhoodClient = new NeighbourhoodClient(mockApolloClient);
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

    // Add multiple signal handlers in paralell
    const promise = neighbourhoodProxy.addSignalHandler(handler1);
    neighbourhoodProxy.addSignalHandler(handler2);
    await promise;

    neighbourhoodClient.dispatchSignal(neighbourhoodURI, true);

    expect(callbacks).toBe(2);
  });

  it("should not add multiple subscriptions when removing and adding another signal handler", async () => {
    const neighbourhoodURI = "did://123";

    const subscriptions = [];
    const subscribe = (args: { next: (result: any) => void }) => {
      subscriptions.push(args);
      new Promise((resolve) => {
        setTimeout(() => {
          resolve({});
        }, 10);
      });
    };

    const mockApolloClient = {
      subscribe: () => ({
        subscribe,
      }),
    } as any;

    const neighbourhoodClient = new NeighbourhoodClient(mockApolloClient);
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

    // Remove signal handler 2
    neighbourhoodProxy.removeSignalHandler(handler1);

    // Add signal handler 2
    await neighbourhoodProxy.addSignalHandler(handler2);

    // Check that only one subscription was added
    expect(subscriptions.length).toBe(1);

    // mock trigger signal event
    for (const subscription of subscriptions) {
      subscription.next({ data: { signal: { neighbourhoodSignal: true } } });
    }

    // Check that only our second callback was called
    expect(callbacks1).toBe(0);
    expect(callbacks2).toBe(1);
  });
});
