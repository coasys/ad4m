import { ApiClient } from "../apiClient";
import { NeighbourhoodClient } from "./NeighbourhoodClient";

/** Minimal ApiClient stand-in that lets the test push server events. */
function fakeApiClient() {
  const callbacks = new Set<(data: any) => void>();
  const api = {
    subscribe: (cb: (data: any) => void) => {
      callbacks.add(cb);
      return () => callbacks.delete(cb);
    },
    waitForSubscription: async () => {},
  };
  const push = (event: any) => callbacks.forEach((cb) => cb(event));
  return { api: api as unknown as ApiClient, push };
}

describe("NeighbourhoodClient signal routing", () => {
  it("delivers a signal only to the handlers of the perspective it belongs to", async () => {
    const { api, push } = fakeApiClient();
    const client = new NeighbourhoodClient("http://localhost:0", undefined, api);

    const receivedA: unknown[] = [];
    const receivedB: unknown[] = [];
    await client.addSignalHandler("uuid-A", (s) => { receivedA.push(s) });
    await client.addSignalHandler("uuid-B", (s) => { receivedB.push(s) });

    const signal = { author: "did:test:alice", data: { links: [] } };
    push({ type: "signal", perspective: { uuid: "uuid-A" }, signal, recipient: null });

    expect(receivedA).toEqual([signal]);
    expect(receivedB).toEqual([]);

    // An event that names no perspective reaches no handler.
    push({ type: "signal", signal, recipient: null });
    expect(receivedA).toEqual([signal]);
    expect(receivedB).toEqual([]);
  });
});
