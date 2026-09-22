/**
 * W1: Mesh 2-peer call setup.
 *
 * Baseline scenario for everything WebRTC.  Two peers connect directly
 * (no SFU), exchange SDP via the scenario harness, and start a synthetic
 * media stream.  Measures:
 *
 * - Time from `createOffer` to first remote-track event on each peer
 *   (the "time-to-media" for a 2-peer call).
 * - 30s of getStats() at 1Hz on both peers.  Reports packet loss, RTT,
 *   bandwidth, frame decode rate.
 *
 * The result here is the floor every other WebRTC scenario reports
 * against: anything in the T-series or F-series should be no
 * *worse* than this for a 2-peer case.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, pairPeers, PeerStats } from "../peer.js";

export const w1Mesh2Peer: Scenario = {
  id: "w1",
  name: "Mesh 2-peer call setup",
  description: "Two-peer full-mesh WebRTC, baseline call setup + media flow",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    let alice: WebRtcPeer | null = null;
    let bob: WebRtcPeer | null = null;
    try {
      alice = new WebRtcPeer("alice", { audioToneHz: 440 });
      bob = new WebRtcPeer("bob", { audioToneHz: 660 });
      await alice.attachSyntheticStream();
      await bob.attachSyntheticStream();

      const pairStart = Date.now();
      await pairPeers(alice, bob);
      const pairDoneAt = Date.now();
      samples.push({
        name: "sdp_exchange",
        durationMs: pairDoneAt - pairStart,
        timestamp: pairDoneAt,
      });

      // Wait for both ICE pairs to come up, then both first-frame events.
      const firstFrame = await waitForBidirectionalMedia(alice, bob, 15000);
      metrics["timeToMediaAliceMs"] = firstFrame.alice;
      metrics["timeToMediaBobMs"] = firstFrame.bob;
      samples.push({
        name: "time_to_media_alice",
        durationMs: firstFrame.alice,
        timestamp: pairDoneAt + firstFrame.alice,
      });
      samples.push({
        name: "time_to_media_bob",
        durationMs: firstFrame.bob,
        timestamp: pairDoneAt + firstFrame.bob,
      });

      // 30s of stats sampling.
      const aliceStats: PeerStats[] = [];
      const bobStats: PeerStats[] = [];
      alice.on("stats", (s: PeerStats) => aliceStats.push(s));
      bob.on("stats", (s: PeerStats) => bobStats.push(s));
      alice.startStats();
      bob.startStats();
      await sleep(30_000);
      alice.stopStats();
      bob.stopStats();

      metrics["aliceStatsSamples"] = aliceStats.length;
      metrics["bobStatsSamples"] = bobStats.length;
      if (aliceStats.length > 0) {
        const last = aliceStats[aliceStats.length - 1];
        metrics["aliceBytesSent"] = last.bytesSent;
        metrics["aliceBytesReceived"] = last.bytesReceived;
        metrics["alicePacketsLost"] = last.packetsLost;
        metrics["aliceRoundTripMs"] = last.currentRoundTripTimeMs;
      }
      if (bobStats.length > 0) {
        const last = bobStats[bobStats.length - 1];
        metrics["bobBytesSent"] = last.bytesSent;
        metrics["bobBytesReceived"] = last.bytesReceived;
        metrics["bobPacketsLost"] = last.packetsLost;
        metrics["bobRoundTripMs"] = last.currentRoundTripTimeMs;
      }
    } finally {
      if (alice) await alice.close();
      if (bob) await bob.close();
    }

    const endTime = Date.now();
    return {
      scenario: "w1-mesh-2peer",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(metrics["aliceBytesReceived"] ?? 0) > 0 && Number(metrics["bobBytesReceived"] ?? 0) > 0,
      metrics,
      samples,
      summary:
        `W1: mesh 2-peer — alice sent=${metrics["aliceBytesSent"]}B received=${metrics["aliceBytesReceived"]}B ` +
        `bob sent=${metrics["bobBytesSent"]}B received=${metrics["bobBytesReceived"]}B ` +
        `packetsLost=${metrics["alicePacketsLost"]}/${metrics["bobPacketsLost"]} ` +
        `rtt=${metrics["aliceRoundTripMs"]}ms/${metrics["bobRoundTripMs"]}ms`,
    };
  },
};

async function waitForBidirectionalMedia(
  alice: WebRtcPeer,
  bob: WebRtcPeer,
  timeoutMs: number,
): Promise<{ alice: number; bob: number }> {
  const start = Date.now();
  const deadline = start + timeoutMs;
  while (Date.now() < deadline) {
    const a = alice.getFirstFrameAt();
    const b = bob.getFirstFrameAt();
    if (a !== null && b !== null) return { alice: a - start, bob: b - start };
    await sleep(100);
  }
  throw new Error(`waitForBidirectionalMedia: ${timeoutMs}ms deadline elapsed`);
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
