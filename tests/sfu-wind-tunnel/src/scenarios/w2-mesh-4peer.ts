/**
 * W2: Mesh 4-peer ICE gathering.
 *
 * Four hosts full-mesh.  Each host maintains 3 separate
 * `RTCPeerConnection`s (one per remote) via `MeshHost`, because a single
 * PC can't act as both offerer and answerer to multiple remotes without
 * DTLS role conflict ("Failed to set SSL role for the transport").
 *
 * Measures:
 *   - SDP+ICE wall time across all 6 pairs.
 *   - 30 s of stats — per-host upload bandwidth (sum across that
 *     host's 3 outbound peers).  Scales O(N-1) for mesh, which is the
 *     baseline T-series SFU scenarios measure savings against.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { MeshHost, connectAll } from "../mesh.js";

const HOST_COUNT = 4;

export const w2Mesh4Peer: Scenario = {
  id: "w2",
  name: "Mesh 4-peer ICE gathering",
  description: "Four hosts, full N(N-1)/2 = 6 pairwise mesh, baseline for O(N-1) cost",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const hosts: MeshHost[] = [];
    for (let i = 0; i < HOST_COUNT; i++) {
      hosts.push(new MeshHost(`peer-${i}`, { audioToneHz: 440 + i * 60 }));
    }

    try {
      const pairWall = await connectAll(hosts);
      const pairCount = (HOST_COUNT * (HOST_COUNT - 1)) / 2;
      samples.push({
        name: "all_pairs_sdp",
        durationMs: pairWall,
        timestamp: Date.now(),
      });
      metrics["pairs"] = pairCount;

      // Settle, then 30 s stats sampling.
      await sleep(2000);
      hosts.forEach((h) => h.startStats());
      await sleep(30_000);
      hosts.forEach((h) => h.stopStats());

      const uploads = hosts.map((h) => h.totalBytesSent());
      const downloads = hosts.map((h) => h.totalBytesReceived());
      const losses = hosts.map((h) => h.totalPacketsLost());
      const rtts = hosts.map((h) => h.worstRttMs());
      metrics["uploadBytesPerHost"] = uploads;
      metrics["downloadBytesPerHost"] = downloads;
      metrics["uploadMean"] = mean(uploads);
      metrics["downloadMean"] = mean(downloads);
      metrics["packetsLostPerHost"] = losses;
      metrics["worstRttMsPerHost"] = rtts;
    } finally {
      await Promise.all(hosts.map((h) => h.close().catch(() => {})));
    }

    const endTime = Date.now();
    return {
      scenario: "w2-mesh-4peer",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(metrics["downloadMean"] ?? 0) > 0,
      metrics,
      samples,
      summary:
        `W2: mesh 4-host (${metrics["pairs"]} pairs) — upload mean=${metrics["uploadMean"]}B ` +
        `download mean=${metrics["downloadMean"]}B`,
    };
  },
};

function mean(arr: number[]): number {
  return arr.length ? Math.round(arr.reduce((a, b) => a + b, 0) / arr.length) : 0;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
