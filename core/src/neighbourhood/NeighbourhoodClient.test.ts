import { ApiClient } from "../apiClient";
import { PerspectiveExpression } from "../perspectives/Perspective";
import { NeighbourhoodClient } from "./NeighbourhoodClient";

/** A socket that opens at once and acknowledges every request, so the test can push server events. */
class FakeWebSocket {
  static last: FakeWebSocket;
  /** Every request sent, in order. */
  static sent: { type: string; params: Record<string, unknown> }[] = [];
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
    const { id, type, params } = JSON.parse(data);
    FakeWebSocket.sent.push({ type, params });
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

describe("NeighbourhoodClient SFU calls", () => {
  const api = new ApiClient("http://localhost:0", undefined, FakeWebSocket as unknown as new (url: string) => WebSocket);
  afterAll(() => api.closeAll());
  const url = "neighbourhood://n";

  it("names the device on join and the participant on every call about the session", async () => {
    const client = new NeighbourhoodClient("http://localhost:0", undefined, api);
    FakeWebSocket.sent = [];
    await client.sfuCallJoin(url, "r", "{}", "laptop");
    await client.sfuCallLeave(url, "r", "P1");
    await client.sfuCallAnswerServerOffer(url, "r", "P1", "{}");
    await client.sfuAddIceCandidate(url, "r", "P1", "candidate:1");
    await client.sfuCallSetQualityPreference(url, "r", "P1", "low");
    await client.sfuSendData(url, "r", "P1", "chat", "hi");

    const sfu = FakeWebSocket.sent.filter((m) => m.type.startsWith("sfu."));
    expect(sfu).toEqual([
      { type: "sfu.callJoin", params: { neighbourhoodUrl: url, roomName: "r", sdpOffer: "{}", deviceId: "laptop" } },
      { type: "sfu.callLeave", params: { neighbourhoodUrl: url, roomName: "r", participantId: "P1" } },
      { type: "sfu.callAnswerServerOffer", params: { neighbourhoodUrl: url, roomName: "r", participantId: "P1", sdpAnswer: "{}" } },
      { type: "sfu.addIceCandidate", params: { neighbourhoodUrl: url, roomName: "r", participantId: "P1", candidate: "candidate:1" } },
      { type: "sfu.callSetQualityPreference", params: { neighbourhoodUrl: url, roomName: "r", participantId: "P1", preference: "low" } },
      { type: "sfu.sendData", params: { neighbourhoodUrl: url, roomName: "r", participantId: "P1", channelLabel: "chat", data: "hi", binary: false } },
    ]);
  });

  it("delivers the executor's replaced notice and each offer with the participant it is for", async () => {
    const client = new NeighbourhoodClient("http://localhost:0", undefined, api);
    const replaced: unknown[] = [];
    const offers: unknown[] = [];
    client.subscribeSfuSessionReplaced((event) => { replaced.push(event) });
    client.subscribeSfuCallRenegotiationOffer("did:me", (event) => { offers.push(event) });
    // Both watched, on no perspective in particular.
    expect(api.watchedEvents()).toMatchObject({ "sfu-session-replaced": null, "sfu-call-renegotiation-offer": null });

    const notice = { targetDid: "did:me", neighbourhoodUrl: url, roomName: "r", participantId: "P1" };
    FakeWebSocket.last.push({ type: "sfu-session-replaced", ...notice });
    FakeWebSocket.last.push({ type: "sfu-call-renegotiation-offer", targetDid: "did:me", participantId: "P2", neighbourhoodUrl: url, roomName: "r", sdpOffer: "{}" });

    expect(replaced).toEqual([expect.objectContaining(notice)]);
    expect(offers).toEqual([expect.objectContaining({ participantId: "P2" })]);
  });
});
