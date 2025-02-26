import { LinkExpression } from "../links/Links";
import { NeighbourhoodClient } from "./NeighbourhoodClient";
import { NeighbourhoodProxy } from "./NeighbourhoodProxy";

describe("NeighbourhoodProxy", () => {
  it("should add multiple signal handlers", async () => {
    const neighbourhoodURI = "did://123";
    const subscribeCB = () => {};
    const mockApolloClient = {
      subscribe: () => {
        return new Promise((resolve) => {
          setTimeout(() => {
            resolve({ subscribe: () => {} });
          }, 10);
        });
      },
    } as any;
    const neighbourhoodProxy = new NeighbourhoodProxy(
      new NeighbourhoodClient(mockApolloClient),
      neighbourhoodURI
    );
    const handler1 = (payload: LinkExpression) => {};
    const handler2 = (payload: LinkExpression) => {};

    // Add multiple signal handlers in paralell
    const promise = neighbourhoodProxy.addSignalHandler(handler1);
    neighbourhoodProxy.addSignalHandler(handler2);
    await promise;

    expect(neighbourhoodProxy["signalHandlers"].length).toBe(2);
  });
});
