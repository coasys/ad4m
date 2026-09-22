#!/usr/bin/env -S npx tsx
/**
 * Standalone runner for the WebRTC + SFU scenarios.
 *
 * Skips the full wind tunnel orchestration (which builds an ad4m
 * executor) and just runs the new W-series/T-series scenarios end-to-end.  The
 * W-series needs no executor at all; the T-series points at an
 * already-running executor via env vars.
 *
 * Usage:
 *   tsx src/run-webrtc.ts w1                      # mesh, no executor needed
 *   tsx src/run-webrtc.ts t1 \                    # SFU, executor must be running
 *     --executor-url=http://127.0.0.1:12000 \
 *     --executor-token=$AD4M_ADMIN_TOKEN
 *   tsx src/run-webrtc.ts all                     # everything
 */

import { writeFileSync, mkdirSync, existsSync } from "node:fs";
import { join } from "node:path";

import { w1Mesh2Peer } from "./scenarios/w1-mesh-2peer.js";
import { w1mMeshMultiMachine } from "./scenarios/w1m-mesh-multimachine.js";
import { w2Mesh4Peer } from "./scenarios/w2-mesh-4peer.js";
import { w3MeshRtt } from "./scenarios/w3-mesh-rtt.js";
import { w4MeshBandwidthScaling } from "./scenarios/w4-mesh-bandwidth-scaling.js";
import { w5TurnFallback } from "./scenarios/w5-turn-fallback.js";
import { t1Sfu5Peer } from "./scenarios/t1-sfu-5peer.js";
import { t2Sfu10Peer } from "./scenarios/t2-sfu-10peer.js";
import { t5TopologyTable } from "./scenarios/t5-topology-table.js";
import { t6PipeHandshake } from "./scenarios/t6-pipe-handshake.js";
import { m1MeshToSfu } from "./scenarios/m1-mesh-to-sfu.js";
import { m4SfuOfflineFallback } from "./scenarios/m4-sfu-offline-fallback.js";
import { f5RenegotiationFlood } from "./scenarios/f5-renegotiation-flood.js";
import { f6NonMemberJoin } from "./scenarios/f6-non-member-join.js";
import { f7BadCapability } from "./scenarios/f7-bad-capability.js";
import { s1Sfu20Peer } from "./scenarios/s1-sfu-20peer.js";
import { f1MeshPacketLoss } from "./scenarios/f1-mesh-packet-loss.js";
import { f2SfuPacketLoss } from "./scenarios/f2-sfu-packet-loss.js";
import { f3OneWayNat } from "./scenarios/f3-one-way-nat.js";
import { m2SfuToMesh } from "./scenarios/m2-sfu-to-mesh.js";
import { t3SfuCascade2Node } from "./scenarios/t3-sfu-cascade-2node.js";
import { t4SfuCascade3Node } from "./scenarios/t4-sfu-cascade-3node.js";
import { m3CascadeFailover } from "./scenarios/m3-cascade-failover.js";
import { f4NetworkPartition } from "./scenarios/f4-network-partition.js";
import { s2SfuCascade4Node } from "./scenarios/s2-sfu-cascade-4node.js";
import { s3MaxParticipantsEnforced } from "./scenarios/s3-max-participants.js";
import { t7SfuCascadeMedia } from "./scenarios/t7-sfu-cascade-media.js";
import { t8ConcurrentJoinRace } from "./scenarios/t8-concurrent-join-race.js";
import { t9TrackDidAttribution } from "./scenarios/t9-track-did-attribution.js";
import { t10SimulcastLayerSelection } from "./scenarios/t10-simulcast-layer-selection.js";
import { t11CascadeRebalance } from "./scenarios/t11-cascade-rebalance.js";
import { t12DeferredTracks } from "./scenarios/t12-deferred-tracks.js";
import { t15SimulcastCascade } from "./scenarios/t15-simulcast-cascade.js";
import { t13PeerDepartureMedia } from "./scenarios/t13-peer-departure-media.js";
import { t14MediaRoutingCorrectness } from "./scenarios/t14-media-routing-correctness.js";
import { InstrumentedClient } from "./client.js";
import type { Scenario, ScenarioContext, ScenarioResult } from "./scenario.js";

interface Args {
  ids: string[];
  executorUrl?: string;
  executorToken?: string;
  resultsDir: string;
  label: string;
}

function parseArgs(argv: string[]): Args {
  const ids: string[] = [];
  let executorUrl: string | undefined;
  let executorToken: string | undefined;
  let resultsDir = "results/webrtc";
  let label = `webrtc-${new Date().toISOString().replace(/[:.]/g, "-")}`;

  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a.startsWith("--executor-url=")) executorUrl = a.split("=")[1];
    else if (a === "--executor-url") executorUrl = argv[++i];
    else if (a.startsWith("--executor-token=")) executorToken = a.split("=")[1];
    else if (a === "--executor-token") executorToken = argv[++i];
    else if (a.startsWith("--results-dir=")) resultsDir = a.split("=")[1];
    else if (a === "--results-dir") resultsDir = argv[++i];
    else if (a.startsWith("--label=")) label = a.split("=")[1];
    else if (a === "--label") label = argv[++i];
    else if (!a.startsWith("--")) ids.push(a);
  }

  if (ids.length === 0) ids.push("all");
  return { ids, executorUrl, executorToken, resultsDir, label };
}

function pick(ids: string[]): Scenario[] {
  const registry: Record<string, Scenario> = {
    w1: w1Mesh2Peer,
    w1m: w1mMeshMultiMachine,
    w2: w2Mesh4Peer,
    w3: w3MeshRtt,
    w4: w4MeshBandwidthScaling,
    w5: w5TurnFallback,
    t1: t1Sfu5Peer,
    t2: t2Sfu10Peer,
    t3: t3SfuCascade2Node,
    t4: t4SfuCascade3Node,
    t5: t5TopologyTable,
    t6: t6PipeHandshake,
    t7: t7SfuCascadeMedia,
    t8: t8ConcurrentJoinRace,
    t9: t9TrackDidAttribution,
    t10: t10SimulcastLayerSelection,
    t11: t11CascadeRebalance,
    t15: t15SimulcastCascade,
    m1: m1MeshToSfu,
    m2: m2SfuToMesh,
    m3: m3CascadeFailover,
    m4: m4SfuOfflineFallback,
    f1: f1MeshPacketLoss,
    f2: f2SfuPacketLoss,
    f3: f3OneWayNat,
    f4: f4NetworkPartition,
    f5: f5RenegotiationFlood,
    f6: f6NonMemberJoin,
    f7: f7BadCapability,
    s1: s1Sfu20Peer,
    s2: s2SfuCascade4Node,
    s3: s3MaxParticipantsEnforced,
    t12: t12DeferredTracks,
    t13: t13PeerDepartureMedia,
    t14: t14MediaRoutingCorrectness,
  };
  if (ids.includes("all")) return Object.values(registry);
  const picked: Scenario[] = [];
  for (const id of ids) {
    const s = registry[id];
    if (!s) {
      console.warn(`[run-webrtc] unknown scenario: ${id}`);
      continue;
    }
    picked.push(s);
  }
  return picked;
}

/**
 * The T-series scenarios use the `InstrumentedClient.call()` method to
 * hit the SFU's WS RPC.  This stub provides just enough surface so the
 * scenarios compile and run against a pre-built executor.  The full
 * client lives in src/client.ts; we instantiate it directly here
 * (skipping the wind tunnel's spawn-from-build path).
 */
function buildClient(url: string, token: string): InstrumentedClient {
  // InstrumentedClient already wraps a `@coasys/ad4m` ApiClient under
  // the hood.  Reuse its constructor exactly as the m-series scenarios
  // do — see src/scenarios/m1-neighbourhood-sync.ts.
  const parsed = new URL(url);
  return new InstrumentedClient({
    port: Number(parsed.port || 12000),
    adminToken: token,
  });
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const scenarios = pick(args.ids);
  if (scenarios.length === 0) {
    console.error("[run-webrtc] no scenarios to run");
    process.exit(1);
  }

  const resultsDir = args.resultsDir;
  if (!existsSync(resultsDir)) mkdirSync(resultsDir, { recursive: true });

  // Scenarios that need an executor: T (except T5), M (except cascade
  // stubs), F (except packet-loss stubs), S, plus W5.  T5 is a pure
  // logic check; the cascade/packet-loss stubs short-circuit before
  // touching the client.
  const liveExecutorScenarioIds = new Set([
    // Self-spawning (startCluster): t3,t4,t6,t7,t11,t15,m3-sfu,f4,f9,s2-sfu,s3-sfu — NOT listed.
    "t1", "t2", "t8", "t9", "t10", "t12", "t13", "t14",
    // Migration scenarios that need a pre-running executor.
    "m1-sfu", "m2-sfu", "m4-sfu",
    // Fault scenarios: f1/f3 are pure WebRTC (no executor); f4 self-spawns.
    "f2", "f5", "f6", "f7",
    // Scale: s1-sfu needs pre-running; s2-sfu/s3-sfu self-spawn.
    "s1-sfu",
    // W5 is pure peer-to-peer TURN (no executor); f1/f3 are pure mesh/p2p.

  ]);
  const requiresExecutor = scenarios.some((s) => liveExecutorScenarioIds.has(s.id));
  let client: InstrumentedClient | null = null;
  if (requiresExecutor) {
    const url = args.executorUrl ?? process.env.AD4M_URL ?? "http://127.0.0.1:12000";
    const token = args.executorToken ?? process.env.AD4M_ADMIN_TOKEN;
    if (!token) {
      console.error(
        "[run-webrtc] T/M/F/S/W5 scenarios need an admin token. " +
          "Set --executor-token or AD4M_ADMIN_TOKEN.",
      );
      process.exit(1);
    }
    console.log(`[run-webrtc] connecting executor client → ${url}`);
    client = buildClient(url, token);
    await client.connect();
    const health = await client.health();
    if (health.error) {
      console.error(`[run-webrtc] executor health check failed: ${health.error}`);
      process.exit(1);
    }

    // SFU handlers require ctx.user_did, which is only populated after an
    // agent is generated.  agent.generate is heavy (key derivation +
    // language load) so we wait up to ~90s and tolerate "already exists"
    // on re-runs.
    console.log("[run-webrtc] ensuring agent is generated for SFU calls");
    const gen = await Promise.race([
      client.generateAgent("wind-tunnel-webrtc"),
      sleep(120_000).then(() => ({ error: "agent.generate timeout (120s)" })),
    ]);
    if (gen && (gen as any).error && !/already/i.test((gen as any).error)) {
      console.warn(`[run-webrtc] agent.generate returned: ${(gen as any).error}`);
    }
  }

  const results: ScenarioResult[] = [];
  for (const scenario of scenarios) {
    console.log(`\n=== Running ${scenario.id}: ${scenario.name} ===`);
    const ctx: ScenarioContext = {
      // Some scenarios (W*) don't touch the client; others (T*) do.
      // Pass a no-op stand-in when there's no executor connected.
      client: client ?? ({ call: () => Promise.reject(new Error("no executor")) } as any),
      branch: args.label,
      port: args.executorUrl
        ? Number(new URL(args.executorUrl).port || 12000)
        : Number(process.env.AD4M_PORT || 12000),
      adminToken: args.executorToken ?? "",
      adamRepoPath: "",
      tmpDirBase: "/tmp",
    };
    const start = Date.now();
    try {
      const result = await scenario.run(ctx);
      results.push(result);
      const elapsed = ((Date.now() - start) / 1000).toFixed(1);
      console.log(`✓ ${scenario.id} done in ${elapsed}s — ${result.summary}`);
    } catch (err) {
      const elapsed = ((Date.now() - start) / 1000).toFixed(1);
      console.error(`✗ ${scenario.id} failed in ${elapsed}s:`, err);
      results.push({
        scenario: scenario.id,
        branch: args.label,
        passed: false,
        startTime: start,
        endTime: Date.now(),
        durationMs: Date.now() - start,
        metrics: { error: err instanceof Error ? err.message : String(err) },
        samples: [],
        summary: `FAILED: ${err instanceof Error ? err.message : String(err)}`,
      });
    }
  }

  if (client) await client.disconnect();

  // Write per-scenario JSON + a summary table.
  for (const r of results) {
    writeFileSync(join(resultsDir, `${r.scenario}.json`), JSON.stringify(r, null, 2));
  }
  const summary = formatSummary(results, args.label);
  writeFileSync(join(resultsDir, "summary.md"), summary);
  console.log(`\n[run-webrtc] results written to ${resultsDir}/`);
  console.log("\n" + summary);
}

function formatSummary(results: ScenarioResult[], label: string): string {
  const lines: string[] = [];
  lines.push(`# WebRTC + SFU wind tunnel — ${label}`);
  lines.push("");
  lines.push("| Scenario | Duration | Summary |");
  lines.push("|---|---|---|");
  for (const r of results) {
    const dur = `${(r.durationMs / 1000).toFixed(1)}s`;
    lines.push(`| ${r.scenario} | ${dur} | ${r.summary} |`);
  }
  return lines.join("\n");
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

main().catch((err) => {
  console.error("[run-webrtc] fatal:", err);
  process.exit(1);
});
