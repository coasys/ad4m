/**
 * W3: Audio/video round-trip latency.
 *
 * Two peers in a mesh call.  After ICE establishment, sample
 * `currentRoundTripTimeMs` for 30 s on both peers and report min /
 * mean / p95 / max.  Baseline for everything where SFU adds a hop:
 * SFU RTT = peer→SFU + SFU→peer; mesh RTT = peer→peer.
 *
 * In single-host loopback the absolute numbers are sub-ms — the value
 * is to detect *regressions* between branches (str0m version bumps,
 * congestion control changes, codec swaps).
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { MeshHost, connectAll } from "../mesh.js";

const SAMPLE_SEC = 30;

export const w3MeshRtt: Scenario = {
  id: "w3",
  name: "Mesh 2-peer RTT distribution",
  description: "Two peers, 30 s RTT sampling — distribution baseline for SFU comparison",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const alice = new MeshHost("alice", { audioToneHz: 440 });
    const bob = new MeshHost("bob", { audioToneHz: 660 });
    const hosts = [alice, bob];

    try {
      await connectAll(hosts);
      await sleep(1500);

      // Sample RTT once per second for SAMPLE_SEC seconds.  We poll
      // worstRttMs() (max across the host's peers, which is == the
      // single remote here) rather than installing a stats listener
      // because the latter doesn't expose the timing semantics we want.
      const aliceRtts: number[] = [];
      const bobRtts: number[] = [];

      alice.startStats();
      bob.startStats();
      const pollStart = Date.now();
      while (Date.now() - pollStart < SAMPLE_SEC * 1000) {
        await sleep(1000);
        const r = alice.worstRttMs();
        const s = bob.worstRttMs();
        if (r != null) aliceRtts.push(r);
        if (s != null) bobRtts.push(s);
      }
      alice.stopStats();
      bob.stopStats();

      metrics["alice_rtt_samples"] = aliceRtts.length;
      metrics["bob_rtt_samples"] = bobRtts.length;
      metrics["alice_rtt_summary"] = summarise(aliceRtts);
      metrics["bob_rtt_summary"] = summarise(bobRtts);

      samples.push({
        name: "rtt_sample_window",
        durationMs: SAMPLE_SEC * 1000,
        timestamp: Date.now(),
      });
    } finally {
      await Promise.all(hosts.map((h) => h.close().catch(() => {})));
    }

    const endTime = Date.now();
    const a = metrics["alice_rtt_summary"] as ReturnType<typeof summarise>;
    const b = metrics["bob_rtt_summary"] as ReturnType<typeof summarise>;
    return {
      scenario: "w3-mesh-rtt",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(metrics["alice_rtt_samples"] ?? 0) > 0 && Number(metrics["bob_rtt_samples"] ?? 0) > 0,
      metrics,
      samples,
      summary:
        `W3: mesh RTT — alice mean=${a?.mean}ms p95=${a?.p95}ms ` +
        `bob mean=${b?.mean}ms p95=${b?.p95}ms`,
    };
  },
};

function summarise(arr: number[]): {
  count: number;
  min: number;
  mean: number;
  p95: number;
  max: number;
} | null {
  if (arr.length === 0) return null;
  const sorted = [...arr].sort((a, b) => a - b);
  const sum = sorted.reduce((a, b) => a + b, 0);
  return {
    count: sorted.length,
    min: +sorted[0].toFixed(3),
    mean: +(sum / sorted.length).toFixed(3),
    p95: +sorted[Math.min(Math.floor(sorted.length * 0.95), sorted.length - 1)].toFixed(3),
    max: +sorted[sorted.length - 1].toFixed(3),
  };
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
