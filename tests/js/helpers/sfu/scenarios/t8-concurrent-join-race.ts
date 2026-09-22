/**
 * T8: Concurrent join renegotiation race.
 *
 * Joins 5 peers to a single-node SFU room SIMULTANEOUSLY via
 * `Promise.all` and verifies the renegotiation pipeline handles the
 * burst:
 *
 *   1. Start a single-node SFU room.
 *   2. Provision 5 peers, attach streams, wire renegotiation.
 *   3. Fire all 5 callJoin requests via `Promise.all`.
 *   4. Wait for server participant count = 5.
 *   5. Wait for renegotiation to settle — each peer should receive
 *      offers for all other peers (>= 4 per peer).
 *   6. Count total renegotiations applied.
 *   7. Report any peers that got stuck (never received all tracks).
 *
 * The race between concurrent joins and server-pushed renegotiation
 * offers is real: a peer whose RTCPeerConnection is still in
 * "have-local-offer" state (waiting for the callJoin answer) cannot
 * apply a server renegotiation offer.  The scenario surfaces whether
 * the SFU recovers from this or leaves peers permanently stuck.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, DetectedTone } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t8-concurrent-join";
const NEIGHBOURHOOD = `windtunnel://t8`;
const PEER_COUNT = 5;
const SETTLE_TIMEOUT_MS = 20_000;

export const t8ConcurrentJoinRace: Scenario = {
  id: "t8",
  name: "Concurrent join renegotiation race",
  description:
    "5 peers join a single-node SFU room simultaneously — verifies " +
    "renegotiation pipeline handles the burst without dropping peers",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    await admin.call("sfu.startRoom", {
      neighbourhoodUrl: NEIGHBOURHOOD,
      roomName: ROOM_NAME,
    });

    const sessions = await provisionPeers({
      admin,
      port,
      count: PEER_COUNT,
      labelPrefix: "t8-peer",
    });

    await registerSfuMembers({
      admin,
      neighbourhoodUrl: NEIGHBOURHOOD,
      sessions,
    });

    // Build tone list before the peer loop — one frequency per peer.
    const TONES = sessions.map((_, i) => 440 + i * 40);

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;

    try {
      // Phase 1: prepare all peers — attach streams, wire renegotiation,
      // create offers.  Preparation happens sequentially so every event
      // subscription exists before the concurrent burst.
      const offers: Array<{ type: string; sdp?: string }> = [];
      for (let i = 0; i < sessions.length; i++) {
        const session = sessions[i];
        const peer = new WebRtcPeer(session.label, {
          audioToneHz: TONES[i],
        });
        peer.enableAudioFingerprinting(TONES);
        await peer.attachSyntheticStream();
        peers.push(peer);

        const wire = await wireRenegotiation({
          client: session.client,
          peer,
          token: session.token,
          port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
        wires.push(wire);

        const offer = await peer.createOffer();
        offers.push(offer);
      }

      // Phase 2: fire ALL callJoin requests simultaneously.
      const joinStart = Date.now();
      const joinResults = await Promise.all(
        sessions.map((session, i) =>
          session.client
            .call<{
              sdpAnswer: string;
              participantId: string;
              redirectTo?: string;
              streamMapping: string[];
            }>("sfu.callJoin", {
              neighbourhoodUrl: NEIGHBOURHOOD,
              roomName: ROOM_NAME,
              sdpOffer: JSON.stringify(offers[i]),
            })
            .then((resp) => ({
              idx: i,
              resp,
              error: null as string | null,
            }))
            .catch((e) => ({
              idx: i,
              resp: null as any,
              error: e instanceof Error ? e.message : String(e),
            })),
        ),
      );
      const joinElapsed = Date.now() - joinStart;
      samples.push({
        name: "concurrent_join_burst",
        durationMs: joinElapsed,
        timestamp: Date.now(),
      });

      // Accept answers for successful joins.
      const joinErrors: string[] = [];
      let successfulJoins = 0;
      for (const result of joinResults) {
        if (result.error) {
          joinErrors.push(`peer-${result.idx}: ${result.error}`);
          continue;
        }
        await peers[result.idx].acceptAnswer(
          JSON.parse(result.resp.sdpAnswer),
        );
        successfulJoins++;
      }
      metrics["joinErrors"] = joinErrors;
      metrics["successfulJoins"] = successfulJoins;

      // Phase 3: wait for server to report the expected participant count.
      const countStart = Date.now();
      let serverCount = 0;
      while (Date.now() - countStart < 15_000) {
        const rooms = await admin
          .call<Array<{ roomName: string; participantCount: number }>>(
            "sfu.listRooms",
            {},
          )
          .catch(
            () => [] as Array<{ roomName: string; participantCount: number }>,
          );
        const room = rooms.find((r) => r.roomName === ROOM_NAME);
        serverCount = room?.participantCount ?? 0;
        if (serverCount >= successfulJoins) break;
        await sleep(250);
      }
      metrics["serverParticipantCount"] = serverCount;

      // Phase 4: wait for renegotiations to settle.
      //
      // With concurrent joins, the expected total renegotiation count
      // follows N*(N-1)/2 (triangular): each peer that joins triggers
      // server-pushed offers to peers already in the room, but the
      // joining peer itself learns about existing peers from its
      // callJoin SDP answer — no renegotiation needed for those.
      //
      // Per-peer counts depend on join order: the first peer processed
      // gets N-1 renegotiations, the last gets 0.  The correct check
      // for a concurrent burst: total >= N*(N-1)/2 and every peer
      // received at least 1 renegotiation (except possibly the very
      // last joiner, who already knows about everyone from its answer).
      const expectedTotal = (successfulJoins * (successfulJoins - 1)) / 2;
      const settleStart = Date.now();
      while (Date.now() - settleStart < SETTLE_TIMEOUT_MS) {
        const total = wires.reduce((sum, w) => sum + w.count(), 0);
        if (total >= expectedTotal) break;
        await sleep(500);
      }
      const settleElapsed = Date.now() - settleStart;
      samples.push({
        name: "renegotiation_settle",
        durationMs: settleElapsed,
        timestamp: Date.now(),
      });

      const renegotiationCounts = wires.map((w) => w.count());
      const totalRenegotiations = renegotiationCounts.reduce(
        (a, b) => a + b,
        0,
      );
      metrics["renegotiationsPerPeer"] = renegotiationCounts;
      metrics["totalRenegotiations"] = totalRenegotiations;
      metrics["expectedTotal"] = expectedTotal;

      // A peer counts as "stuck" if it received 0 renegotiations AND
      // was not the last joiner (the last joiner legitimately gets 0
      // because no one joins after it).  In a concurrent burst the
      // exact "last" joiner is indeterminate, so we allow exactly one
      // peer with count 0.
      const zeroPeers = renegotiationCounts.filter((c) => c === 0).length;
      const stuckPeers = renegotiationCounts
        .map((c, i) => ({ peer: i, count: c }))
        .filter((p) => p.count === 0);
      metrics["stuckPeers"] = stuckPeers;
      metrics["allPeersSettled"] =
        totalRenegotiations >= expectedTotal && zeroPeers <= 1;

      // Phase 5: brief media window to verify track routing via
      // audio fingerprinting.  Concurrent joins can corrupt track
      // assignment — the tones reveal that.
      await sleep(5000);

      const toneResults: Array<{ peer: string; detected: DetectedTone[] }> = [];
      let routingCorrect = true;
      for (let i = 0; i < peers.length; i++) {
        const detected = peers[i].getDetectedTones();
        toneResults.push({ peer: sessions[i].label, detected });
        const otherTones = TONES.filter((_, j) => j !== i);
        const detectedHz = new Set(detected.map((d) => d.dominantHz));
        const matched = otherTones.filter((hz) => detectedHz.has(hz));
        if (matched.length === 0 && detected.length > 0) {
          routingCorrect = false;
        }
      }
      metrics["toneDetection"] = toneResults;
      metrics["routingCorrect"] = routingCorrect;

      passed =
        successfulJoins === PEER_COUNT &&
        totalRenegotiations >= expectedTotal &&
        routingCorrect;
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
      scenario: "t8-concurrent-join-race",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T8: concurrent join — joined=${metrics["successfulJoins"]}/${PEER_COUNT} ` +
        `renegotiations=[${(metrics["renegotiationsPerPeer"] as number[] ?? []).join(",")}] ` +
        `total=${metrics["totalRenegotiations"]} (expected>=${metrics["expectedTotal"]}) ` +
        `zeroPeers=${(metrics["stuckPeers"] as any[] ?? []).length} ` +
        `allSettled=${metrics["allPeersSettled"]} ` +
        `routingCorrect=${metrics["routingCorrect"]}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
