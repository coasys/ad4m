/**
 * F8: Stuck renegotiation recovery.
 *
 * Verifies the SFU clears stale pending renegotiation offers and
 * continues serving non-stuck peers.  A peer that never answers
 * server-pushed renegotiation offers should NOT block other peers
 * from receiving tracks.
 *
 * Flow:
 *   1. Start a single-node SFU room.
 *   2. Join peer A normally (with renegotiation wired).
 *   3. Join peer B but do NOT wire renegotiation — peer B never
 *      answers server offers.
 *   4. Wait 15 seconds (the pending offer timeout defaults to 10 s).
 *   5. Join peer C normally (with renegotiation wired).
 *   6. Verify peer A eventually receives peer C's tracks (the stuck
 *      peer B does not block A <-> C renegotiation).
 *   7. Report whether peer B's stale renegotiation was cleared.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { provisionPeers, disconnectPeers, PeerSession, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "f8-stuck-renegotiation";
const NEIGHBOURHOOD = `windtunnel://f8`;
const STUCK_WAIT_MS = 15_000;
const SETTLE_TIMEOUT_MS = 15_000;

export const f8StuckRenegotiationRecovery: Scenario = {
  id: "f8",
  name: "Stuck renegotiation recovery",
  description:
    "Peer B never answers renegotiation offers — verifies the SFU " +
    "clears stale offers and peer A still receives peer C's tracks",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    await admin.call("sfu.startRoom", {
      neighbourhoodUrl: NEIGHBOURHOOD,
      roomName: ROOM_NAME,
    });

    // Provision 3 peers up front.
    const sessions = await provisionPeers({
      admin,
      port,
      count: 3,
      labelPrefix: "f8-peer",
    });

    await registerSfuMembers({
      admin,
      neighbourhoodUrl: NEIGHBOURHOOD,
      sessions,
    });

    const [sessionA, sessionB, sessionC] = sessions;

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let wireA: RenegotiationWire | null = null;
    let wireC: RenegotiationWire | null = null;
    let passed = false;

    try {
      // ── Peer A: join normally, renegotiation wired ──
      const peerA = new WebRtcPeer(sessionA.label, { audioToneHz: 440 });
      await peerA.attachSyntheticStream();
      peers.push(peerA);

      wireA = await wireRenegotiation({
        client: sessionA.client,
        peer: peerA,
        token: sessionA.token,
        port,
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
      });
      wires.push(wireA);

      const offerA = await peerA.createOffer();
      const respA = await sessionA.client.call<{
        sdpAnswer: string;
        participantId: string;
        redirectTo?: string;
        streamMapping: string[];
      }>("sfu.callJoin", {
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
        sdpOffer: JSON.stringify(offerA),
      });
      await peerA.acceptAnswer(JSON.parse(respA.sdpAnswer));
      const renegA_afterSelf = wireA.count();
      metrics["peerA_renegotiationsAfterOwnJoin"] = renegA_afterSelf;

      // ── Peer B: join but do NOT wire renegotiation ──
      const peerB = new WebRtcPeer(sessionB.label, { audioToneHz: 520 });
      await peerB.attachSyntheticStream();
      peers.push(peerB);
      // Deliberately no wireRenegotiation — peer B never answers offers.

      const offerB = await peerB.createOffer();
      const respB = await sessionB.client.call<{
        sdpAnswer: string;
        participantId: string;
        redirectTo?: string;
        streamMapping: string[];
      }>("sfu.callJoin", {
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
        sdpOffer: JSON.stringify(offerB),
      });
      await peerB.acceptAnswer(JSON.parse(respB.sdpAnswer));

      // Record peer A's renegotiation count after B joined — A should
      // receive an offer about B's tracks.
      await sleep(2000);
      const renegA_afterB = wireA.count();
      metrics["peerA_renegotiationsAfterBJoin"] = renegA_afterB;

      // ── Wait for the pending-offer timeout to expire ──
      samples.push({
        name: "stuck_wait_start",
        durationMs: 0,
        timestamp: Date.now(),
      });
      await sleep(STUCK_WAIT_MS);
      samples.push({
        name: "stuck_wait_end",
        durationMs: STUCK_WAIT_MS,
        timestamp: Date.now(),
      });

      // ── Peer C: join normally, renegotiation wired ──
      const peerC = new WebRtcPeer(sessionC.label, { audioToneHz: 600 });
      await peerC.attachSyntheticStream();
      peers.push(peerC);

      wireC = await wireRenegotiation({
        client: sessionC.client,
        peer: peerC,
        token: sessionC.token,
        port,
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
      });
      wires.push(wireC);

      const offerC = await peerC.createOffer();
      const respC = await sessionC.client.call<{
        sdpAnswer: string;
        participantId: string;
        redirectTo?: string;
        streamMapping: string[];
      }>("sfu.callJoin", {
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
        sdpOffer: JSON.stringify(offerC),
      });
      await peerC.acceptAnswer(JSON.parse(respC.sdpAnswer));

      // ── Wait for peer A to learn about peer C ──
      const settleStart = Date.now();
      while (Date.now() - settleStart < SETTLE_TIMEOUT_MS) {
        // Peer A should receive at least one more renegotiation (for C).
        if (wireA.count() > renegA_afterB) break;
        await sleep(500);
      }
      const settleMs = Date.now() - settleStart;
      samples.push({
        name: "post_stuck_settle",
        durationMs: settleMs,
        timestamp: Date.now(),
      });

      const renegA_afterC = wireA.count();
      metrics["peerA_renegotiationsAfterCJoin"] = renegA_afterC;
      metrics["peerA_receivedCTracks"] = renegA_afterC > renegA_afterB;

      // Peer C should also learn about A (and maybe B).
      const renegC = wireC.count();
      metrics["peerC_renegotiations"] = renegC;

      // Check server participant count — all 3 should still show.
      const rooms = await admin.call<
        Array<{ roomName: string; participantCount: number }>
      >("sfu.listRooms", {});
      const room = rooms.find((r) => r.roomName === ROOM_NAME);
      metrics["serverParticipantCount"] = room?.participantCount ?? -1;

      metrics["stuckPeerBCleared"] = renegA_afterC > renegA_afterB;
      metrics["recoverySuccessful"] =
        (metrics["peerA_receivedCTracks"] as boolean) === true;
      passed = (metrics["peerA_receivedCTracks"] as boolean) === true;
    } finally {
      for (const w of wires) {
        try {
          await w.detach();
        } catch {
          /* best-effort */
        }
      }
      for (let i = 0; i < peers.length; i++) {
        try {
          await sessions[i].client.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {
          /* best-effort */
        }
        try {
          await peers[i].close();
        } catch {
          /* best-effort */
        }
      }
      try {
        await admin.call("sfu.stopRoom", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
      } catch {
        /* best-effort */
      }
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "f8-stuck-renegotiation-recovery",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `F8: stuck renegotiation — ` +
        `peerA_receivedC=${metrics["peerA_receivedCTracks"]} ` +
        `peerA_reneg=[${metrics["peerA_renegotiationsAfterOwnJoin"]},` +
        `${metrics["peerA_renegotiationsAfterBJoin"]},` +
        `${metrics["peerA_renegotiationsAfterCJoin"]}] ` +
        `recovery=${metrics["recoverySuccessful"]} ` +
        `participants=${metrics["serverParticipantCount"]}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
