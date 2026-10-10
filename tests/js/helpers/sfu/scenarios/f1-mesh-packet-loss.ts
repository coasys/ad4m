/**
 * F1: Mesh packet loss 5% on uplink.
 *
 * Builds a 2-host mesh, lets it settle, applies a 5% packet-loss
 * qdisc to loopback for 15 s, then removes it.  Reports per-host
 * bytes flowing under loss + packet drops.
 *
 * tc affects the WHOLE loopback interface — including the WS RPC
 * channel — so loss is applied AFTER ICE settles and pairing
 * completes, and is removed before scenario teardown.
 *
 * Requires Linux + sudo -n.  Skips on macOS or when sudo would
 * prompt.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { MeshHost, connectAll } from "../mesh.js";
import { clearNet, setNetem, netAvailable } from "../net.js";

const LOSS_PCT = 5;
const SAMPLE_SEC = 15;

export const f1MeshPacketLoss: Scenario = {
  id: "f1",
  name: "Mesh packet loss 5%",
  description: "2-host mesh with 5% packet loss on lo — measures visible quality drop",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    if (!netAvailable()) {
      metrics["skipped"] = true;
      metrics["skip_reason"] =
        "tc qdisc + sudo -n not available on this platform";
      return {
        scenario: "f1-mesh-packet-loss",
        branch,
        startTime,
        endTime: Date.now(),
        durationMs: Date.now() - startTime,
        passed: true,
        metrics,
        samples,
        summary: `F1: SKIPPED — ${metrics["skip_reason"]}`,
      };
    }

    const alice = new MeshHost("alice", { audioToneHz: 440 });
    const bob = new MeshHost("bob", { audioToneHz: 660 });
    const hosts = [alice, bob];

    try {
      await connectAll(hosts);
      await sleep(2000);

      const applied = setNetem({ lossPct: LOSS_PCT });
      metrics["lossApplied"] = applied;
      if (!applied) {
        throw new Error("F1: setNetem returned false even though netAvailable() said true");
      }

      hosts.forEach((h) => h.startStats());
      await sleep(SAMPLE_SEC * 1000);
      hosts.forEach((h) => h.stopStats());

      const uploads = hosts.map((h) => h.totalBytesSent());
      const downloads = hosts.map((h) => h.totalBytesReceived());
      const losses = hosts.map((h) => h.totalPacketsLost());
      metrics["uploadBytesPerHost"] = uploads;
      metrics["downloadBytesPerHost"] = downloads;
      metrics["packetsLostPerHost"] = losses;
      metrics["packetsLostTotal"] = losses.reduce((a, b) => a + b, 0);
      metrics["lossInjectedPct"] = LOSS_PCT;

      samples.push({
        name: `loss_${LOSS_PCT}pct_sample_window`,
        durationMs: SAMPLE_SEC * 1000,
        timestamp: Date.now(),
      });
    } finally {
      clearNet();
      await Promise.all(hosts.map((h) => h.close().catch(() => {})));
    }

    const endTime = Date.now();
    return {
      scenario: "f1-mesh-packet-loss",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(mean(metrics["downloadBytesPerHost"] as number[])) > 0,
      metrics,
      samples,
      summary:
        `F1: mesh ${LOSS_PCT}% loss — uploadMean=${mean(metrics["uploadBytesPerHost"] as number[])}B ` +
        `downloadMean=${mean(metrics["downloadBytesPerHost"] as number[])}B ` +
        `packetsLost=${metrics["packetsLostTotal"]}`,
    };
  },
};

function mean(arr: number[]): number {
  return arr.length ? Math.round(arr.reduce((a, b) => a + b, 0) / arr.length) : 0;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
