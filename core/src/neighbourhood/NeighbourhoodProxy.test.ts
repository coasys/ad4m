import { ApolloClient } from "@apollo/client";
import { LinkExpression } from "../links/Links";
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
});
