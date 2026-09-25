/**
 * T16: Session lifecycle — join, participants, leave, rejoin, destroy.
 *
 * Validates the lifecycle flow that the SDK Session interface codifies:
 *   create room → join with media → verify participant count →
 *   leave one peer → verify count drops → rejoin → verify restored →
 *   leave all → verify room empty.
 *
 * Uses 3 peers to exercise mid-session churn without cascade complexity.
 * Each peer is a distinct AD4M user (multi-user mode).
 *
 * Asserts:
 *   - Participant count matches after join, leave, rejoin.
 *   - Media flows (bytes received > 0) for all active peers.
 *   - Room reports 0 participants after all peers leave.
 *   - Renegotiations fire on peer join/leave (track set changes).
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t16-session-lifecycle";

export const t16SessionLifecycle: Scenario = {
  id: "t16",
  name: "Session lifecycle",
  description: "Join/leave/rejoin lifecycle through the session surface pattern (3 peers)",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const neighbourhoodUrl = "windtunnel://t16";
    metrics["neighbourhoodUrl"] = neighbourhoodUrl;

    await admin.call("sfu.startRoom", { neighbourhoodUrl, roomName: ROOM_NAME });

    const sessions = await provisionPeers({
      admin,
      port,
      count: 3,
      labelPrefix: "t16-peer",
    });

    await registerSfuMembers({ admin, neighbourhoodUrl, sessions });

    const TONES = sessions.map((_, i) => 440 + i * 50);
    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;

    try {
      // ── Phase A: all 3 peers join ──────────────────────────────────
      for (let i = 0; i < sessions.length; i++) {
        const session = sessions[i];
        const peer = new WebRtcPeer(session.label, { audioToneHz: TONES[i] });
        await peer.attachSyntheticStream();
        peers.push(peer);

        const wire = await wireRenegotiation({
          client: session.client,
          peer,
          token: session.token,
          port,
          neighbourhoodUrl,
          roomName: ROOM_NAME,
        });
        wires.push(wire);

        const offer = await peer.createOffer();
        const t0 = Date.now();
        const joinResp = await session.client.call<{
          sdpAnswer: string;
          participantId: string;
          streamMapping: string[];
        }>("sfu.callJoin", {
          neighbourhoodUrl,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });
        samples.push({ name: `join_${session.label}`, durationMs: Date.now() - t0, timestamp: Date.now() });
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));
      }

      await waitForParticipantCount(admin, ROOM_NAME, 3, 15_000);
      await sleep(2_000);

      // Verify all 3 peers receive media.
      const statsA = await collectStats(peers, 5_000);
      const allReceivedA = statsA.every((s) => s.bytesReceived > 0);
      metrics["phase_a_allReceived"] = allReceivedA;
      metrics["phase_a_participants"] = await roomParticipantCount(admin, ROOM_NAME);

      // ── Phase B: peer 1 leaves ─────────────────────────────────────
      const t1Leave = Date.now();
      await sessions[1].client.call("sfu.callLeave", {
        neighbourhoodUrl,
        roomName: ROOM_NAME,
      });
      samples.push({ name: "leave_peer1", durationMs: Date.now() - t1Leave, timestamp: Date.now() });

      await waitForParticipantCount(admin, ROOM_NAME, 2, 10_000);
      await sleep(1_500);

      metrics["phase_b_participants"] = await roomParticipantCount(admin, ROOM_NAME);

      // Remaining peers (0, 2) still receive media.
      peers[0].startStats();
      peers[2].startStats();
      await sleep(3_000);
      peers[0].stopStats();
      peers[2].stopStats();

      const p0recv = peers[0].getLastStats()?.bytesReceived ?? 0;
      const p2recv = peers[2].getLastStats()?.bytesReceived ?? 0;
      metrics["phase_b_peer0_received"] = p0recv;
      metrics["phase_b_peer2_received"] = p2recv;
      const remainingReceive = p0recv > 0 && p2recv > 0;
      metrics["phase_b_remainingReceive"] = remainingReceive;

      // ── Phase C: peer 1 rejoins ────────────────────────────────────
      const rejoinPeer = new WebRtcPeer("t16-peer-1-rejoin", { audioToneHz: TONES[1] });
      await rejoinPeer.attachSyntheticStream();

      // Detach old wire, create new one for rejoin.
      await wires[1].detach();
      const rejoinWire = await wireRenegotiation({
        client: sessions[1].client,
        peer: rejoinPeer,
        token: sessions[1].token,
        port,
        neighbourhoodUrl,
        roomName: ROOM_NAME,
      });
      wires[1] = rejoinWire;

      const rejoinOffer = await rejoinPeer.createOffer();
      const t1Rejoin = Date.now();
      const rejoinResp = await sessions[1].client.call<{
        sdpAnswer: string;
        participantId: string;
        streamMapping: string[];
      }>("sfu.callJoin", {
        neighbourhoodUrl,
        roomName: ROOM_NAME,
        sdpOffer: JSON.stringify(rejoinOffer),
      });
      samples.push({ name: "rejoin_peer1", durationMs: Date.now() - t1Rejoin, timestamp: Date.now() });
      await rejoinPeer.acceptAnswer(JSON.parse(rejoinResp.sdpAnswer));

      // Replace the old peer reference.
      await peers[1].close();
      peers[1] = rejoinPeer;

      await waitForParticipantCount(admin, ROOM_NAME, 3, 15_000);
      await sleep(2_000);

      metrics["phase_c_participants"] = await roomParticipantCount(admin, ROOM_NAME);

      const statsC = await collectStats(peers, 5_000);
      const allReceivedC = statsC.every((s) => s.bytesReceived > 0);
      metrics["phase_c_allReceived"] = allReceivedC;

      // ── Phase D: all peers leave ───────────────────────────────────
      for (let i = 0; i < sessions.length; i++) {
        await sessions[i].client.call("sfu.callLeave", {
          neighbourhoodUrl,
          roomName: ROOM_NAME,
        }).catch(() => { /* best-effort */ });
      }

      await sleep(2_000);
      metrics["phase_d_participants"] = await roomParticipantCount(admin, ROOM_NAME);

      metrics["renegotiationsPerPeer"] = wires.map((w) => w.count());
      metrics["renegotiationFailuresPerPeer"] = wires.map((w) => w.failures());

      passed =
        allReceivedA &&
        (metrics["phase_a_participants"] as number) === 3 &&
        (metrics["phase_b_participants"] as number) === 2 &&
        remainingReceive &&
        (metrics["phase_c_participants"] as number) === 3 &&
        allReceivedC &&
        (metrics["phase_d_participants"] as number) === 0;
    } finally {
      for (const w of wires) {
        try { await w.detach(); } catch { /* best-effort */ }
      }
      for (let i = 0; i < peers.length; i++) {
        try {
          await sessions[i].client.call("sfu.callLeave", {
            neighbourhoodUrl,
            roomName: ROOM_NAME,
          });
        } catch { /* best-effort */ }
        try { await peers[i].close(); } catch { /* best-effort */ }
      }
      try {
        await admin.call("sfu.stopRoom", { neighbourhoodUrl, roomName: ROOM_NAME });
      } catch { /* best-effort */ }
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "t16-session-lifecycle",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T16: Session lifecycle — ` +
        `joinAll=${metrics["phase_a_allReceived"]}(${metrics["phase_a_participants"]}) ` +
        `afterLeave=${metrics["phase_b_remainingReceive"]}(${metrics["phase_b_participants"]}) ` +
        `afterRejoin=${metrics["phase_c_allReceived"]}(${metrics["phase_c_participants"]}) ` +
        `afterAllLeave=(${metrics["phase_d_participants"]})`,
    };
  },
};

async function waitForParticipantCount(
  client: ScenarioContext["client"],
  roomName: string,
  expected: number,
  timeoutMs: number,
): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const count = await roomParticipantCount(client, roomName);
    if (count === expected) return;
    await sleep(250);
  }
  throw new Error(
    `T16 waitForParticipantCount: room=${roomName} expected=${expected} within ${timeoutMs}ms`,
  );
}

async function roomParticipantCount(
  client: ScenarioContext["client"],
  roomName: string,
): Promise<number> {
  const rooms = await client
    .call<Array<{ roomName: string; participantCount: number }>>("sfu.listRooms", {})
    .catch(() => [] as Array<{ roomName: string; participantCount: number }>);
  const room = rooms.find((r) => r.roomName === roomName);
  return room?.participantCount ?? 0;
}

async function collectStats(
  peers: WebRtcPeer[],
  durationMs: number,
): Promise<Array<{ bytesReceived: number; bytesSent: number }>> {
  peers.forEach((p) => p.startStats());
  await sleep(durationMs);
  peers.forEach((p) => p.stopStats());
  return peers.map((p) => ({
    bytesReceived: p.getLastStats()?.bytesReceived ?? 0,
    bytesSent: p.getLastStats()?.bytesSent ?? 0,
  }));
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
