/**
 * T18: Mesh session lifecycle — join, leave, rejoin, close.
 *
 * Exercises the mesh topology lifecycle that the SDK Session interface
 * manages when `topology: "mesh"`:
 *
 *   3 hosts connect → verify full-mesh media flow →
 *   host B leaves → verify 2 remaining flow →
 *   host B reconnects → verify 3 flow again →
 *   all close → verify clean teardown.
 *
 * Complements the W* baselines (which validate raw 2- and 4-peer
 * connection establishment) by exercising mid-session churn: the same
 * lifecycle transitions that `MeshManager.setRoster()` drives in the
 * SDK, reproduced at the peer level.
 *
 * Uses `MeshHost` / `connectAll` for connection management — no
 * executor needed.  Media flow validated via getStats() byte counts.
 *
 * Asserts:
 *   - All 3 hosts receive media after initial mesh.
 *   - Remaining 2 hosts still receive media after host B leaves.
 *   - All 3 hosts receive media after host B rejoins.
 *   - Peer count drops to 0 after all close.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { MeshHost, connectAll } from "../mesh.js";
import { pairPeers } from "../peer.js";

export const t18MeshSessionLifecycle: Scenario = {
  id: "t18",
  name: "Mesh session lifecycle",
  description: "Join/leave/rejoin lifecycle for 3-peer mesh (no SFU)",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const TONES = [440, 550, 660];
    const hostA = new MeshHost("t18-alice", { audioToneHz: TONES[0] });
    const hostB = new MeshHost("t18-bob", { audioToneHz: TONES[1] });
    const hostC = new MeshHost("t18-carol", { audioToneHz: TONES[2] });
    let hostB2: MeshHost | null = null;
    let passed = false;

    try {
      // ── Phase A: all 3 hosts connect ──────────────────────────────
      const tConnect = Date.now();
      const connectMs = await connectAll([hostA, hostB, hostC]);
      samples.push({
        name: "connect_all_3",
        durationMs: connectMs,
        timestamp: Date.now(),
      });
      metrics["connectMs"] = connectMs;

      await sleep(3_000);

      // Verify all 3 hosts receive media.
      hostA.startStats();
      hostB.startStats();
      hostC.startStats();
      await sleep(5_000);
      hostA.stopStats();
      hostB.stopStats();
      hostC.stopStats();

      const aRecvA = hostA.totalBytesReceived();
      const bRecvA = hostB.totalBytesReceived();
      const cRecvA = hostC.totalBytesReceived();
      metrics["phase_a_alice_received"] = aRecvA;
      metrics["phase_a_bob_received"] = bRecvA;
      metrics["phase_a_carol_received"] = cRecvA;
      const allReceivedA = aRecvA > 0 && bRecvA > 0 && cRecvA > 0;
      metrics["phase_a_allReceived"] = allReceivedA;
      metrics["phase_a_peerCounts"] = [
        hostA.peerCount(),
        hostB.peerCount(),
        hostC.peerCount(),
      ];

      // ── Phase B: host B leaves ────────────────────────────────────
      const tLeave = Date.now();
      await hostB.close();
      samples.push({
        name: "leave_bob",
        durationMs: Date.now() - tLeave,
        timestamp: Date.now(),
      });

      await sleep(2_000);

      // Verify remaining 2 hosts still receive media.
      hostA.startStats();
      hostC.startStats();
      await sleep(5_000);
      hostA.stopStats();
      hostC.stopStats();

      const aRecvB = hostA.totalBytesReceived();
      const cRecvB = hostC.totalBytesReceived();
      metrics["phase_b_alice_received"] = aRecvB;
      metrics["phase_b_carol_received"] = cRecvB;
      const remainingReceived = aRecvB > 0 && cRecvB > 0;
      metrics["phase_b_remainingReceived"] = remainingReceived;
      metrics["phase_b_peerCounts"] = [
        hostA.peerCount(),
        hostC.peerCount(),
      ];

      // ── Phase C: host B reconnects ────────────────────────────────
      hostB2 = new MeshHost("t18-bob-rejoin", { audioToneHz: TONES[1] });

      const tRejoin = Date.now();
      // Connect bob2 to both existing hosts.
      const peerBtoA = await hostB2.getPeer(hostA.id);
      const peerAtoB = await hostA.getPeer(hostB2.id);
      await pairPeers(peerBtoA, peerAtoB);

      const peerBtoC = await hostB2.getPeer(hostC.id);
      const peerCtoB = await hostC.getPeer(hostB2.id);
      await pairPeers(peerBtoC, peerCtoB);

      samples.push({
        name: "rejoin_bob",
        durationMs: Date.now() - tRejoin,
        timestamp: Date.now(),
      });

      await sleep(3_000);

      // Verify all 3 hosts receive media again.
      hostA.startStats();
      hostB2.startStats();
      hostC.startStats();
      await sleep(5_000);
      hostA.stopStats();
      hostB2.stopStats();
      hostC.stopStats();

      const aRecvC = hostA.totalBytesReceived();
      const bRecvC = hostB2.totalBytesReceived();
      const cRecvC = hostC.totalBytesReceived();
      metrics["phase_c_alice_received"] = aRecvC;
      metrics["phase_c_bob_received"] = bRecvC;
      metrics["phase_c_carol_received"] = cRecvC;
      const allReceivedC = aRecvC > 0 && bRecvC > 0 && cRecvC > 0;
      metrics["phase_c_allReceived"] = allReceivedC;
      metrics["phase_c_peerCounts"] = [
        hostA.peerCount(),
        hostB2.peerCount(),
        hostC.peerCount(),
      ];

      // ── Phase D: all close ────────────────────────────────────────
      const tClose = Date.now();
      await hostA.close();
      if (hostB2) await hostB2.close();
      await hostC.close();
      samples.push({
        name: "close_all",
        durationMs: Date.now() - tClose,
        timestamp: Date.now(),
      });

      metrics["phase_d_peerCounts"] = [
        hostA.peerCount(),
        hostB2?.peerCount() ?? 0,
        hostC.peerCount(),
      ];

      const cleanTeardown =
        hostA.peerCount() === 0 &&
        (hostB2?.peerCount() ?? 0) === 0 &&
        hostC.peerCount() === 0;
      metrics["phase_d_cleanTeardown"] = cleanTeardown;

      passed =
        allReceivedA &&
        remainingReceived &&
        allReceivedC &&
        cleanTeardown;
    } finally {
      // Best-effort cleanup in case of early failure.
      try { await hostA.close(); } catch { /* best-effort */ }
      try { await hostB.close(); } catch { /* best-effort */ }
      try { if (hostB2) await hostB2.close(); } catch { /* best-effort */ }
      try { await hostC.close(); } catch { /* best-effort */ }
    }

    const endTime = Date.now();
    return {
      scenario: "t18-mesh-session-lifecycle",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T18: Mesh lifecycle — ` +
        `joinAll=${metrics["phase_a_allReceived"]} ` +
        `afterLeave=${metrics["phase_b_remainingReceived"]} ` +
        `afterRejoin=${metrics["phase_c_allReceived"]} ` +
        `teardown=${metrics["phase_d_cleanTeardown"]}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
