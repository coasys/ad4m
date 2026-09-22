/**
 * T19: Mesh data channel — peer-to-peer data exchange.
 *
 * Validates RTCDataChannel between mesh peers, exercising the same
 * transport path that `MeshManager.sendData()` uses when the Session
 * runs in mesh topology.  (The SFU path relays data through the
 * server; mesh sends it peer-to-peer.)
 *
 *   2 peers connect → open data channel → peer A sends text →
 *   peer B receives → peer B sends text → peer A receives →
 *   burst send → verify all arrive in order.
 *
 * Uses raw `RTCDataChannel` on the peer connection created by
 * `pairPeers`, so the scenario validates the WebRTC data channel
 * path itself — independent of the signalling overlay
 * `MeshManager.sendData` adds on top.
 *
 * Asserts:
 *   - String payloads round-trip correctly.
 *   - Burst of 20 messages arrives complete and in order.
 *   - Data channel opens within 5s of connection.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, pairPeers } from "../peer.js";

const ROOM_NAME = "t19-mesh-data-channel";

export const t19MeshDataChannel: Scenario = {
  id: "t19",
  name: "Mesh data channel",
  description: "Peer-to-peer RTCDataChannel exchange between 2 mesh peers",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    let alice: WebRtcPeer | null = null;
    let bob: WebRtcPeer | null = null;
    let passed = false;

    try {
      alice = new WebRtcPeer("t19-alice", { audioToneHz: 440 });
      bob = new WebRtcPeer("t19-bob", { audioToneHz: 660 });
      await alice.attachSyntheticStream();
      await bob.attachSyntheticStream();

      // ── Connect peers ─────────────────────────────────────────────
      const tPair = Date.now();
      await pairPeers(alice, bob);
      samples.push({
        name: "sdp_exchange",
        durationMs: Date.now() - tPair,
        timestamp: Date.now(),
      });

      await sleep(2_000);

      // ── Create data channel ───────────────────────────────────────
      //
      // Alice creates the channel (offerer side).  Bob receives it
      // via the ondatachannel event on his peer connection.
      const alicePc = alice.peerConnection();
      const bobPc = bob.peerConnection();

      const aliceDc = alicePc.createDataChannel("wind-tunnel-data");
      const bobDcPromise = waitForDataChannel(bobPc, 5_000);

      // The data channel creation fires negotiationneeded on some
      // implementations.  Exchange SDP again to stabilise.
      const renegOffer = await alicePc.createOffer();
      await alicePc.setLocalDescription(renegOffer);
      await bobPc.setRemoteDescription(alicePc.localDescription!);
      const renegAnswer = await bobPc.createAnswer();
      await bobPc.setLocalDescription(renegAnswer);
      await alicePc.setRemoteDescription(bobPc.localDescription!);

      const bobDc = await bobDcPromise;
      metrics["dataChannelLabel"] = bobDc.label;

      // Wait for both channels to open.
      await waitForOpen(aliceDc, 5_000);
      await waitForOpen(bobDc, 5_000);

      samples.push({
        name: "data_channel_open",
        durationMs: Date.now() - tPair,
        timestamp: Date.now(),
      });

      // ── Alice sends string to Bob ─────────────────────────────────
      const receivedByBob: string[] = [];
      bobDc.onmessage = (ev: MessageEvent) => {
        receivedByBob.push(typeof ev.data === "string" ? ev.data : "");
      };

      const payload0 = JSON.stringify({ type: "reaction", emoji: "👍", ts: Date.now() });
      const tSend0 = Date.now();
      aliceDc.send(payload0);
      samples.push({
        name: "send_alice_to_bob",
        durationMs: Date.now() - tSend0,
        timestamp: Date.now(),
      });

      // ── Bob sends string to Alice ─────────────────────────────────
      const receivedByAlice: string[] = [];
      aliceDc.onmessage = (ev: MessageEvent) => {
        receivedByAlice.push(typeof ev.data === "string" ? ev.data : "");
      };

      const payload1 = JSON.stringify({ type: "hand-raise", raised: true });
      const tSend1 = Date.now();
      bobDc.send(payload1);
      samples.push({
        name: "send_bob_to_alice",
        durationMs: Date.now() - tSend1,
        timestamp: Date.now(),
      });

      await sleep(1_500);

      const aliceGotPayload = receivedByAlice.includes(payload1);
      const bobGotPayload = receivedByBob.includes(payload0);
      metrics["aliceReceivedPayload"] = aliceGotPayload;
      metrics["bobReceivedPayload"] = bobGotPayload;

      // ── Burst send: 20 messages from Alice ────────────────────────
      const burstCount = 20;
      const burstReceived: string[] = [];
      // Replace Bob's handler to capture burst.
      bobDc.onmessage = (ev: MessageEvent) => {
        burstReceived.push(typeof ev.data === "string" ? ev.data : "");
      };

      const tBurst = Date.now();
      for (let i = 0; i < burstCount; i++) {
        aliceDc.send(`burst-${i}`);
      }
      samples.push({
        name: `send_burst_${burstCount}`,
        durationMs: Date.now() - tBurst,
        timestamp: Date.now(),
      });

      await sleep(3_000);

      metrics["burst_sent"] = burstCount;
      metrics["burst_received"] = burstReceived.length;
      const burstComplete = burstReceived.length === burstCount;
      metrics["burstComplete"] = burstComplete;

      let burstOrdered = true;
      for (let i = 0; i < burstReceived.length; i++) {
        if (burstReceived[i] !== `burst-${i}`) {
          burstOrdered = false;
          break;
        }
      }
      metrics["burstOrdered"] = burstOrdered;

      // ── Verdict ───────────────────────────────────────────────────
      passed =
        aliceGotPayload &&
        bobGotPayload &&
        burstComplete &&
        burstOrdered;
    } finally {
      if (alice) await alice.close();
      if (bob) await bob.close();
    }

    const endTime = Date.now();
    return {
      scenario: "t19-mesh-data-channel",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T19: Mesh data channel — ` +
        `alice→bob=${metrics["bobReceivedPayload"]} ` +
        `bob→alice=${metrics["aliceReceivedPayload"]} ` +
        `burst=${metrics["burst_received"]}/${metrics["burst_sent"]} ` +
        `ordered=${metrics["burstOrdered"]}`,
    };
  },
};

function waitForDataChannel(
  pc: any,
  timeoutMs: number,
): Promise<any> {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error(`waitForDataChannel: ${timeoutMs}ms elapsed`)),
      timeoutMs,
    );
    pc.ondatachannel = (ev: any) => {
      clearTimeout(timer);
      resolve(ev.channel);
    };
  });
}

function waitForOpen(dc: any, timeoutMs: number): Promise<void> {
  return new Promise((resolve, reject) => {
    if (dc.readyState === "open") { resolve(); return; }
    const timer = setTimeout(
      () => reject(new Error(`waitForOpen: ${timeoutMs}ms elapsed, state=${dc.readyState}`)),
      timeoutMs,
    );
    dc.onopen = () => {
      clearTimeout(timer);
      resolve();
    };
  });
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
