import { ApiClient } from "../apiClient";
import { PerspectiveExpression } from "../perspectives/Perspective";
import { NeighbourhoodClient } from "./NeighbourhoodClient";

/** A socket that opens at once and acknowledges every request, so the test can push server events. */
class FakeWebSocket {
  static last: FakeWebSocket;
  readyState = 0;
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onerror: ((e: unknown) => void) | null = null;
  onclose: (() => void) | null = null;
  constructor(public url: string) {
    FakeWebSocket.last = this;
    queueMicrotask(() => { this.readyState = 1; this.onopen?.(); });
  }
  send(data: string) {
    const { id } = JSON.parse(data);
    queueMicrotask(() => this.push({ id, result: true }));
  }
  close() { this.readyState = 3; }
  push(message: Record<string, unknown>) { this.onmessage?.({ data: JSON.stringify(message) }); }
}

const handle = (uuid: string) => ({ uuid, name: null, neighbourhood: null, sharedUrl: null, state: "SYNCED", owners: null });
const signal = {
  author: "did:test:alice",
  timestamp: "2026-01-01T00:00:00Z",
  data: { links: [] },
  proof: { key: "key", signature: "sig", valid: true, invalid: false },
};
/** A `signal` event exactly as the executor sends it. */
const signalEvent = (perspectiveUuid: string) =>
  ({ type: "signal", perspectiveUuid, perspective: handle(perspectiveUuid), signal, recipient: null });

describe("NeighbourhoodClient signal routing", () => {
  const api = new ApiClient("http://localhost:0", undefined, FakeWebSocket as unknown as new (url: string) => WebSocket);
  afterAll(() => api.closeAll());

  it("delivers a signal only to the handlers of the perspective it belongs to", async () => {
    const client = new NeighbourhoodClient("http://localhost:0", undefined, api);

    const receivedA: PerspectiveExpression[] = [];
    const receivedB: PerspectiveExpression[] = [];
    await client.addSignalHandler("uuid-A", (s) => { receivedA.push(s) });
    await client.addSignalHandler("uuid-B", (s) => { receivedB.push(s) });
    expect(api.watchedEvents()).toEqual({ signal: ["uuid-A", "uuid-B"] });

    FakeWebSocket.last.push(signalEvent("uuid-A"));

    expect(receivedA).toHaveLength(1);
    expect(receivedA[0]).toMatchObject({ author: "did:test:alice", data: { links: [] } });
    expect(receivedB).toEqual([]);

    // A signal for a perspective without handlers reaches no handler.
    FakeWebSocket.last.push(signalEvent("uuid-C"));
    expect(receivedA).toHaveLength(1);
    expect(receivedB).toEqual([]);
  });
});
