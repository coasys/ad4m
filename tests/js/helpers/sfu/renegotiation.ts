/**
 * Wires the SFU server-pushed renegotiation pipeline into a wind tunnel peer.
 *
 * The SFU emits an `sfu-call-renegotiation-offer` event per participant
 * whenever the set of outbound tracks for that participant changes (a
 * new peer joins the room, so the relay now has more tracks to forward).
 * The event carries a fresh SDP offer the client must apply, then post
 * back an answer via `sfu.callAnswerServerOffer`.
 *
 * This module subscribes to the per-peer events_ws stream, filters for
 * the renegotiation topic (the server already targets per-DID), applies
 * the offer to the peer's `RTCPeerConnection`, and pushes the answer.
 *
 * Returns a detach function the scenario calls during teardown.
 */
import { WebRtcPeer } from "./peer.js";
import { InstrumentedClient } from "./client.js";
import { EventsClient, EventFrame } from "./events.js";

export interface RenegotiationWireConfig {
  /** Authenticated client used to post the server's answer back. */
  client: InstrumentedClient;
  /** The peer whose `RTCPeerConnection` is being renegotiated. */
  peer: WebRtcPeer;
  /** Per-user JWT — same as `client.adminToken`, used by the events WS. */
  token: string;
  /** Executor WS port. */
  port: number;
  host?: string;
  neighbourhoodUrl: string;
  roomName: string;
  /** Optional callback fired after every successful renegotiation roundtrip. */
  onRenegotiated?: () => void;
}

export interface RenegotiationWire {
  /** Disconnects the events WS and stops listening. */
  detach: () => Promise<void>;
  /** Number of renegotiations applied so far. */
  count: () => number;
  /** Number of renegotiation attempts that failed. */
  failures: () => number;
}

export async function wireRenegotiation(cfg: RenegotiationWireConfig): Promise<RenegotiationWire> {
  const events = new EventsClient({
    port: cfg.port,
    host: cfg.host,
    token: cfg.token,
  });
  await events.connect();

  let applied = 0;
  let failed = 0;
  let pending: Promise<void> = Promise.resolve();

  const off = events.on("sfu-call-renegotiation-offer", (frame: EventFrame) => {
    pending = pending.then(() => applyOffer(frame).catch((e) => {
      failed += 1;
      console.error(`[renegotiation:${cfg.peer.id}] apply failed:`, e);
    }));
  });

  async function applyOffer(frame: EventFrame): Promise<void> {
    if (frame.neighbourhoodUrl !== cfg.neighbourhoodUrl) return;
    if (frame.roomName !== cfg.roomName) return;
    const sdpOfferJson = frame.sdpOffer;
    if (typeof sdpOfferJson !== "string") return;
    const offer = JSON.parse(sdpOfferJson) as RTCSessionDescriptionInit;
    const answer = await cfg.peer.createAnswer(offer);
    await cfg.client.call("sfu.callAnswerServerOffer", {
      neighbourhoodUrl: cfg.neighbourhoodUrl,
      roomName: cfg.roomName,
      sdpAnswer: JSON.stringify(answer),
    });
    applied += 1;
    cfg.onRenegotiated?.();
  }

  return {
    async detach() {
      off();
      // Drain in-flight renegotiations before tearing the WS down so
      // the SFU doesn't see an orphaned offer.
      try {
        await pending;
      } catch {
        /* logged above */
      }
      await events.disconnect();
    },
    count() {
      return applied;
    },
    failures() {
      return failed;
    },
  };
}
