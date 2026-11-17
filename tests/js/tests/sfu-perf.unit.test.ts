/**
 * Unit tests of the SFU wind tunnel's performance numbers (helpers/sfu/perf.ts):
 * which ScenarioResult numbers become history metrics, their names, units and
 * direction. No executor. `pnpm run test-perf-unit`.
 */

import { expect } from "chai";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { classify, perfMetrics, words, writePerfResults } from "../helpers/sfu/perf.js";
import type { ScenarioResult } from "../helpers/sfu/scenario.js";

function result(metrics: Record<string, any>, samples: ScenarioResult["samples"] = []): ScenarioResult {
  return { scenario: "x", branch: "test", startTime: 1, endTime: 2, durationMs: 1234, passed: true, metrics, samples, summary: "" };
}

const byName = (ms: ReturnType<typeof perfMetrics>) => Object.fromEntries(ms.map((m) => [m.metric, m]));

describe("SFU perf metrics", () => {
  it("splits camel case and snake case into words", () => {
    expect(words("preCrash_nodeA_pipeCount")).to.deep.equal(["pre", "crash", "node", "a", "pipe", "count"]);
    expect(words("alice_rtt_summary.p95")).to.deep.equal(["alice", "rtt", "summary", "p95"]);
  });

  it("classifies times, bytes, frames and losses; leaves out exact counts and instants", () => {
    const ms = { unit: "ms", better: "lower" };
    expect(classify("pipeWaitMs")).to.deep.equal(ms);
    expect(classify("colocated_roster_ms")).to.deep.equal(ms);
    expect(classify("migrateLatencyMs")).to.deep.equal(ms);
    expect(classify("alice_rtt_summary.p95")).to.deep.equal(ms);
    expect(classify("alice_rtt_samples")).to.deep.equal({ unit: "count", better: "higher" });
    expect(classify("alice_rtt_summary.count")).to.deep.equal({ unit: "count", better: "higher" });
    expect(classify("uploadBytesPerPeer.median")).to.deep.equal({ unit: "bytes", better: "higher" });
    expect(classify("uploadMean")).to.deep.equal({ unit: "bytes", better: "higher" });
    expect(classify("messages_delivered")).to.deep.equal({ unit: "count", better: "higher" });
    expect(classify("colocated_frames.alice.bob")).to.deep.equal({ unit: "count", better: "higher" });
    expect(classify("descriptions_dropped")).to.deep.equal({ unit: "count", better: "lower" });
    // A loss word wins over a frame word: fewer dropped frames is better.
    expect(classify("framesDropped")).to.deep.equal({ unit: "count", better: "lower" });
    expect(classify("frames_lost")).to.deep.equal({ unit: "count", better: "lower" });
    // ...and over a byte word, which keeps its unit.
    expect(classify("bytesDropped")).to.deep.equal({ unit: "bytes", better: "lower" });
    expect(classify("colocated_frames_delivered")).to.deep.equal({ unit: "count", better: "higher" });
    expect(classify("renegotiationsPerPeer.max")).to.deep.equal({ unit: "count", better: "lower" });
    for (const exact of [
      "nodeAKilledAt", "startTime", "preCrash_nodeA_pipeCount", "overflowCount", "landedCount",
      "nodeAParticipants", "totalLanded", "redirectFromA", "simulcastActiveWithBytes", "simulcastActiveLayers",
      "items_total",
    ]) {
      expect(classify(exact), exact).to.equal(null);
    }
  });

  it("names metrics sfu.<id>.<path>, summarises arrays and reads DID-keyed objects as arrays", () => {
    const m = byName(perfMetrics("t7", result({
      pipeWaitMs: 812,
      pipeEstablished: true,
      nodeDids: ["did:key:a", "did:key:b"],
      uploadBytesPerPeer: [100, 300, 200],
      perNodeWait: { "did:key:zA": 10, "did:key:zB": 30 },
      alice_rtt_summary: { min: 1, p95: 9, count: 40 },
      colocated_frames: { "bob->alice": 50 },
      failures: ["one"],
      binding_crashes: [{ pid: 1 }],
      error: "boom",
    })));
    expect(m["sfu.t7.duration_ms"]).to.deep.include({ value: 1234, unit: "ms", better: "lower" });
    expect(m["sfu.t7.pipeWaitMs"].value).to.equal(812);
    expect(m["sfu.t7.uploadBytesPerPeer.min"].value).to.equal(100);
    expect(m["sfu.t7.uploadBytesPerPeer.median"].value).to.equal(200);
    expect(m["sfu.t7.uploadBytesPerPeer.max"].value).to.equal(300);
    expect(m["sfu.t7.perNodeWait.median"].value).to.equal(20);
    expect(m["sfu.t7.alice_rtt_summary.p95"].value).to.equal(9);
    expect(m["sfu.t7.alice_rtt_summary.count"].value).to.equal(40);
    expect(m["sfu.t7.colocated_frames.bob_to_alice"]).to.deep.include({ value: 50, unit: "count", better: "higher" });
    expect(Object.keys(m).some((k) => k.includes("did"))).to.equal(false);
    expect(Object.keys(m).some((k) => k.includes("failures") || k.includes("crashes") || k.includes("error"))).to.equal(false);
    for (const metric of Object.keys(m)) expect(metric).to.match(/^[A-Za-z0-9_-]+(\.[A-Za-z0-9_-]+)*$/);
  });

  it("suffixes keys that slug to one name, so record never refuses the run", () => {
    const ms = perfMetrics("t21", result({
      duration_ms: 7,
      colocated_frames: { "bob->alice": 50, bob_to_alice: 60, "bob to alice": 70 },
    }));
    expect(ms.map((m) => [m.metric, m.value])).to.deep.equal([
      ["sfu.t21.duration_ms", 1234],
      ["sfu.t21.duration_ms_2", 7],
      ["sfu.t21.colocated_frames.bob_to_alice", 50],
      ["sfu.t21.colocated_frames.bob_to_alice_2", 60],
      ["sfu.t21.colocated_frames.bob_to_alice_3", 70],
    ]);
  });

  it("takes timed samples without an error, once per name", () => {
    const m = byName(perfMetrics("s2-sfu", result({}, [
      { name: "join peer 1", durationMs: 50, timestamp: 1 },
      { name: "join peer 1", durationMs: 70, timestamp: 2 },
      { name: "join peer 2", durationMs: 0, timestamp: 3 },
      { name: "join peer 3", durationMs: 90, timestamp: 4, error: "timeout" },
    ])));
    expect(m["sfu.s2-sfu.sample.join_peer_1"].value).to.equal(50);
    expect(m["sfu.s2-sfu.sample.join_peer_2"]).to.equal(undefined);
    expect(m["sfu.s2-sfu.sample.join_peer_3"]).to.equal(undefined);
  });

  it("writes sfu-<id>.json only when PERF_RESULTS_DIR is set", () => {
    expect(writePerfResults("t3", result({ pipeWaitMs: 5 }), undefined)).to.equal(null);
    const dir = mkdtempSync(join(tmpdir(), "sfu-perf-"));
    try {
      const file = writePerfResults("t3", result({ pipeWaitMs: 5 }), join(dir, "nested"));
      expect(file).to.equal(join(dir, "nested", "sfu-t3.json"));
      const doc = JSON.parse(readFileSync(file!, "utf8"));
      expect(doc.metrics.map((x: any) => x.metric)).to.deep.equal(["sfu.t3.duration_ms", "sfu.t3.pipeWaitMs"]);
    } finally {
      rmSync(dir, { recursive: true, force: true });
    }
  });
});
