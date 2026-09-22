/**
 * W4: Mesh bandwidth scaling.
 *
 * Runs 2-, 3-, and 4-host mesh back-to-back.  For each, measures the
 * mean per-host upload bandwidth over 15 s.  In mesh, each host
 * uploads its stream to every other host, so per-host upload scales
 * linearly with N-1.
 *
 * Expected, at constant content:
 *   N=2 →  X bytes/s
 *   N=3 → 2X bytes/s
 *   N=4 → 3X bytes/s
 *
 * The actual baseline depends on codec & simulcast; this scenario
 * reports the absolute numbers and the ratios.  Compare to T1/T2
 * (SFU): per-host upload should stay roughly constant regardless of N.
 *
 * Uses `MeshHost` so each host maintains N-1 distinct
 * `RTCPeerConnection`s, sidestepping the DTLS role conflict that
 * breaks single-PC peers in mesh > 2.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { MeshHost, connectAll } from "../mesh.js";

const SAMPLE_SEC = 15;

export const w4MeshBandwidthScaling: Scenario = {
  id: "w4",
  name: "Mesh bandwidth scaling",
  description: "2-, 3-, 4-host mesh comparison — per-host upload should scale as O(N-1)",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const bandwidthByN: Record<number, number> = {};
    for (const N of [2, 3, 4]) {
      const hosts: MeshHost[] = [];
      try {
        for (let i = 0; i < N; i++) {
          hosts.push(new MeshHost(`n${N}-host-${i}`, { audioToneHz: 440 + i * 60 }));
        }
        await connectAll(hosts);
        await sleep(2000);
        hosts.forEach((h) => h.startStats());
        await sleep(SAMPLE_SEC * 1000);
        hosts.forEach((h) => h.stopStats());

        const uploads = hosts.map((h) => h.totalBytesSent());
        const meanUp = uploads.reduce((a, b) => a + b, 0) / uploads.length;
        bandwidthByN[N] = Math.round(meanUp);
        samples.push({
          name: `n${N}_upload_mean`,
          durationMs: SAMPLE_SEC * 1000,
          timestamp: Date.now(),
        });
      } finally {
        await Promise.all(hosts.map((h) => h.close().catch(() => {})));
      }
    }

    metrics["uploadBytesPerHostByN"] = bandwidthByN;
    metrics["ratio_3to2"] = bandwidthByN[2] ? +(bandwidthByN[3] / bandwidthByN[2]).toFixed(2) : null;
    metrics["ratio_4to2"] = bandwidthByN[2] ? +(bandwidthByN[4] / bandwidthByN[2]).toFixed(2) : null;

    const endTime = Date.now();
    return {
      scenario: "w4-mesh-bandwidth-scaling",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: true,
      metrics,
      samples,
      summary:
        `W4: mesh scaling — N=2:${bandwidthByN[2]}B  N=3:${bandwidthByN[3]}B  N=4:${bandwidthByN[4]}B  ` +
        `(3:2=${metrics["ratio_3to2"]}x, 4:2=${metrics["ratio_4to2"]}x — mesh O(N-1) expected 2x and 3x)`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
