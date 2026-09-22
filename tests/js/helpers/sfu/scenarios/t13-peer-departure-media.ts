/**
 * T13: Peer departure — media continuity + stale track cleanup.
 *
 * Verifies that after a peer departs, remaining peers continue
 * receiving correct media and no stale references cause misrouting.
 * Exercises the SFU server's `clean_stale_track_refs()` path, which
 * removes tracks_out/tracks_out_rev/deferred_tracks entries for a
 * departed peer.
 *
 * Sequence:
 *   1. Join 4 peers (A, B, C, D) with distinct tone frequencies.
 *   2. Let media flow for 5s, verify all peers receive media.
 *   3. Peer B calls `sfu.callLeave` and closes.
 *   4. Wait 5s for the server to process departure and clean stale tracks.
 *   5. Remaining peers (A, C, D) must:
 *      - Continue receiving bytes (bytesReceived increases after departure)
 *      - Detect tones ONLY from living peers (not B's frequency)
 *
 * Asserts:
 *   - Remaining peers keep receiving media after departure.
 *   - No stale media from the departed peer reaches anyone.
 *
 * Pre-requisite: executor running with `--enable-multi-user`.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, PeerStats, DetectedTone } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t13-peer-departure";
const NEIGHBOURHOOD = `windtunnel://t13`;
const PEER_COUNT = 4;
/** Index of the peer that departs mid-session. */
const DEPARTING_PEER = 1;
/** Pre-departure media-flow window (ms). */
const PRE_DEPARTURE_MS = 5_000;
/** Post-departure settle + media-flow window (ms). */
const POST_DEPARTURE_MS = 10_000;

export const t13PeerDepartureMedia: Scenario = {
  id: "t13",
  name: "Peer departure media continuity",
  description:
    "4 peers join — one departs — verifies remaining peers continue " +
    "receiving correct media with no stale track misrouting",

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
      labelPrefix: "t13-peer",
    });

    await registerSfuMembers({
      admin,
      neighbourhoodUrl: NEIGHBOURHOOD,
      sessions,
    });

    // Widely-spaced tones — 400, 600, 800, 1000 Hz.
    const TONES = [400, 600, 800, 1000];

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;

    try {
      // Phase 1: join all 4 peers sequentially.
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
        const joinResponse = await session.client.call<{
          sdpAnswer: string;
          participantId: string;
          redirectTo?: string;
          streamMapping: string[];
        }>("sfu.callJoin", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });

        if (joinResponse.redirectTo) {
          throw new Error(
            `T13 expects a single-node SFU but got cascade redirect to ${joinResponse.redirectTo}`,
          );
        }
        await peer.acceptAnswer(JSON.parse(joinResponse.sdpAnswer));
      }

      // Wait for server participant count to reach 4.
      await waitForServerParticipantCount(
        admin,
        ROOM_NAME,
        PEER_COUNT,
        15_000,
      );

      // Phase 2: pre-departure — let media flow, collect baseline stats.
      peers.forEach((p) => p.startStats());
      await sleep(PRE_DEPARTURE_MS);

      const preDepartureBytes: number[] = peers.map(
        (p) => p.getLastStats()?.bytesReceived ?? 0,
      );
      metrics["preDepartureBytesReceived"] = preDepartureBytes;

      // Verify all peers received media before departure.
      const allReceivedPreDeparture = preDepartureBytes.every((b) => b > 0);
      metrics["allReceivedPreDeparture"] = allReceivedPreDeparture;

      // Phase 3: peer B departs.
      const departStart = Date.now();
      try {
        await wires[DEPARTING_PEER].detach();
      } catch {
        /* best-effort */
      }
      try {
        await sessions[DEPARTING_PEER].client.call("sfu.callLeave", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
      } catch {
        /* best-effort */
      }
      try {
        await peers[DEPARTING_PEER].close();
      } catch {
        /* best-effort */
      }
      samples.push({
        name: "peer_departure",
        durationMs: Date.now() - departStart,
        timestamp: Date.now(),
      });

      // Phase 4: post-departure — wait for stale track cleanup, then
      // let remaining peers accumulate fresh Goertzel data.
      //
      // The SFU needs time to process departure + send renegotiation
      // offers removing B's tracks. The jitter buffer also drains
      // residual frames for ~1-2s. Wait for that to settle, THEN
      // reset fingerprinting so Goertzel only accumulates clean
      // post-departure audio.
      const remainingIndices = [0, 2, 3]; // A, C, D
      await sleep(3_000); // let SFU process departure + drain jitter buffers
      for (const idx of remainingIndices) {
        peers[idx].resetFingerprinting();
      }
      await sleep(POST_DEPARTURE_MS);

      // Snapshot post-departure bytesReceived.
      const postDepartureBytes: Record<string, number> = {};
      for (const idx of remainingIndices) {
        postDepartureBytes[sessions[idx].label] =
          peers[idx].getLastStats()?.bytesReceived ?? 0;
      }
      metrics["postDepartureBytesReceived"] = postDepartureBytes;

      // Phase 5: assertions.
      // 5a. Remaining peers must have received MORE bytes after departure
      // (media still flowing).
      let remainingPeersReceiveMedia = true;
      for (const idx of remainingIndices) {
        const pre = preDepartureBytes[idx];
        const post = peers[idx].getLastStats()?.bytesReceived ?? 0;
        if (post <= pre) {
          remainingPeersReceiveMedia = false;
        }
      }
      metrics["remainingPeersReceiveMedia"] = remainingPeersReceiveMedia;

      // 5b. Tone detection — remaining peers should detect tones from
      // other LIVING peers only, not from the departed peer (B = 600 Hz).
      const departedHz = TONES[DEPARTING_PEER]; // 600 Hz
      const livingTones = TONES.filter((_, i) => i !== DEPARTING_PEER);
      let noStaleMediaFromDeparted = true;
      const toneResults: Array<{
        peer: string;
        detected: DetectedTone[];
        staleDetected: boolean;
      }> = [];

      for (const idx of remainingIndices) {
        const detected = peers[idx].getDetectedTones();
        // Check whether any detected tone matches the departed peer's frequency.
        const staleDetected = detected.some(
          (d) => d.dominantHz === departedHz,
        );
        if (staleDetected) {
          noStaleMediaFromDeparted = false;
        }
        toneResults.push({
          peer: sessions[idx].label,
          detected,
          staleDetected,
        });
      }
      metrics["toneDetection"] = toneResults;
      metrics["noStaleMediaFromDeparted"] = noStaleMediaFromDeparted;

      // Check the server reports the reduced participant count.
      const rooms = await admin
        .call<Array<{ roomName: string; participantCount: number }>>(
          "sfu.listRooms",
          {},
        )
        .catch(
          () => [] as Array<{ roomName: string; participantCount: number }>,
        );
      const room = rooms.find((r) => r.roomName === ROOM_NAME);
      const postDepartureCount = room?.participantCount ?? -1;
      metrics["postDepartureParticipantCount"] = postDepartureCount;
      const participantCountCorrect =
        postDepartureCount === PEER_COUNT - 1;
      metrics["participantCountCorrect"] = participantCountCorrect;

      passed =
        allReceivedPreDeparture &&
        remainingPeersReceiveMedia &&
        noStaleMediaFromDeparted &&
        participantCountCorrect;
    } finally {
      // Clean up remaining peers (skip the departed one at DEPARTING_PEER).
      for (let i = 0; i < wires.length; i++) {
        if (i === DEPARTING_PEER) continue;
        try {
          await wires[i].detach();
        } catch {
          /* best-effort */
        }
      }
      for (let i = 0; i < peers.length; i++) {
        if (i === DEPARTING_PEER) continue;
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
      // Stop stats on all (close() already stopped the departed peer).
      for (const p of peers) {
        try {
          p.stopStats();
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
      scenario: "t13-peer-departure-media",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T13: peer departure — ` +
        `preMedia=${metrics["allReceivedPreDeparture"]} ` +
        `postMedia=${metrics["remainingPeersReceiveMedia"]} ` +
        `noStale=${metrics["noStaleMediaFromDeparted"]} ` +
        `participants=${metrics["postDepartureParticipantCount"]}`,
    };
  },
};

async function waitForServerParticipantCount(
  client: ScenarioContext["client"],
  roomName: string,
  expected: number,
  timeoutMs: number,
): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const rooms = await client
      .call<Array<{ roomName: string; participantCount: number }>>(
        "sfu.listRooms",
        {},
      )
      .catch(
        () => [] as Array<{ roomName: string; participantCount: number }>,
      );
    const room = rooms.find((r) => r.roomName === roomName);
    if (room && room.participantCount >= expected) return;
    await sleep(250);
  }
  throw new Error(
    `T13 waitForServerParticipantCount: room=${roomName} ` +
      `expected=${expected} within ${timeoutMs}ms`,
  );
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
