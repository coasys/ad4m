/**
 * T14: Exhaustive media routing correctness.
 *
 * The most stringent media-path verification in the wind tunnel.
 * Joins 4 peers sequentially (clean renegotiation path — no deferred
 * tracks), lets media flow for a generous window, then checks that
 * EVERY peer received EXACTLY the right audio tones from every other
 * peer, with no misrouting or missing tracks.
 *
 * Unlike T1's opportunistic tone check (which allows partial matches),
 * this scenario requires a full (N-1) match per peer and explicitly
 * flags unexpected frequencies.
 *
 * Asserts:
 *   - Each peer detected exactly (N-1) tones.
 *   - Each detected tone matches one of the known other-peer frequencies.
 *   - No unexpected frequencies appear.
 *
 * Pre-requisite: executor running with `--enable-multi-user`.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, DetectedTone } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t14-routing-correctness";
const NEIGHBOURHOOD = `windtunnel://t14`;
const PEER_COUNT = 4;
/** Widely-spaced frequencies — easy for Goertzel to separate. */
const TONES = [400, 600, 800, 1000];
/** Generous media-flow window so every fingerprinter accumulates data. */
const MEDIA_FLOW_MS = 10_000;

export const t14MediaRoutingCorrectness: Scenario = {
  id: "t14",
  name: "Exhaustive media routing correctness",
  description:
    "4 peers with distinct tones — verifies each peer receives exactly " +
    "the right audio from every other peer (no misrouting, no missing tracks)",

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
      labelPrefix: "t14-peer",
    });

    await registerSfuMembers({
      admin,
      neighbourhoodUrl: NEIGHBOURHOOD,
      sessions,
    });

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;

    try {
      // Phase 1: join peers SEQUENTIALLY for a clean renegotiation path.
      // Each join settles before the next one starts, so no deferred
      // tracks — this isolates pure routing correctness.
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
        const joinStart = Date.now();
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
        const joinElapsed = Date.now() - joinStart;
        samples.push({
          name: `call_join_${session.label}`,
          durationMs: joinElapsed,
          timestamp: Date.now(),
        });

        if (joinResponse.redirectTo) {
          throw new Error(
            `T14 expects a single-node SFU but got cascade redirect to ${joinResponse.redirectTo}`,
          );
        }
        await peer.acceptAnswer(JSON.parse(joinResponse.sdpAnswer));

        // Brief settle between joins — let renegotiation complete for
        // already-joined peers before the next peer enters.
        if (i < sessions.length - 1) {
          await sleep(1_000);
        }
      }

      // Wait for server to report all participants.
      await waitForServerParticipantCount(
        admin,
        ROOM_NAME,
        PEER_COUNT,
        15_000,
      );

      // Phase 2: let media flow for a generous window.
      await sleep(MEDIA_FLOW_MS);

      // Phase 3: exhaustive tone verification.
      const toneResults: Array<{
        peer: string;
        detected: DetectedTone[];
        expectedHz: number[];
        matchedHz: number[];
        missingHz: number[];
        unexpectedHz: number[];
      }> = [];
      let allPeersDetectedAllOtherTones = true;
      let noMisrouting = true;

      for (let i = 0; i < peers.length; i++) {
        const detected = peers[i].getDetectedTones();
        const otherTones = TONES.filter((_, j) => j !== i);
        const detectedHz = detected.map((d) => d.dominantHz);
        const detectedSet = new Set(detectedHz);

        // Which expected tones appeared?
        const matchedHz = otherTones.filter((hz) => detectedSet.has(hz));
        // Which expected tones never appeared?
        const missingHz = otherTones.filter((hz) => !detectedSet.has(hz));
        // Which detected tones don't match any expected frequency?
        // Filter out 0 Hz — Goertzel returns 0 for silent/empty tracks
        // (self-track or tracks not yet producing audio). Silence does
        // not constitute misrouting.
        const expectedSet = new Set(otherTones);
        const unexpectedHz = detectedHz.filter(
          (hz) => hz !== 0 && !expectedSet.has(hz),
        );

        toneResults.push({
          peer: sessions[i].label,
          detected,
          expectedHz: otherTones,
          matchedHz,
          missingHz,
          unexpectedHz,
        });

        // Strict checks: exactly (N-1) matches, nothing missing,
        // nothing unexpected.
        if (missingHz.length > 0) {
          allPeersDetectedAllOtherTones = false;
        }
        if (unexpectedHz.length > 0) {
          noMisrouting = false;
        }
      }

      metrics["toneDetection"] = toneResults;
      metrics["allPeersDetectedAllOtherTones"] =
        allPeersDetectedAllOtherTones;
      metrics["noMisrouting"] = noMisrouting;

      // Also verify server participant count matches.
      const rooms = await admin
        .call<Array<{ roomName: string; participantCount: number }>>(
          "sfu.listRooms",
          {},
        )
        .catch(
          () => [] as Array<{ roomName: string; participantCount: number }>,
        );
      const room = rooms.find((r) => r.roomName === ROOM_NAME);
      const serverParticipants = room?.participantCount ?? -1;
      metrics["serverReportedParticipants"] = serverParticipants;
      const participantsMatch = serverParticipants === PEER_COUNT;
      metrics["participantsMatch"] = participantsMatch;

      metrics["renegotiationsPerPeer"] = wires.map((w) => w.count());

      passed =
        allPeersDetectedAllOtherTones &&
        noMisrouting &&
        participantsMatch;
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
      scenario: "t14-media-routing-correctness",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T14: routing correctness — ` +
        `allTones=${metrics["allPeersDetectedAllOtherTones"]} ` +
        `noMisrouting=${metrics["noMisrouting"]} ` +
        `participants=${metrics["serverReportedParticipants"]}`,
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
    `T14 waitForServerParticipantCount: room=${roomName} ` +
      `expected=${expected} within ${timeoutMs}ms`,
  );
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
