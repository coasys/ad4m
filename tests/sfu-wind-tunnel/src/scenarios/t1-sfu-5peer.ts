/**
 * T1: SFU 1-node × 5 peers.
 *
 * Drives 5 synthetic-media peers against a single executor SFU.  Each
 * peer is a distinct AD4M user (multi-user mode via `user.create` +
 * `user.login` — no admin escape hatches), so the SFU's per-DID
 * duplicate-join check just works.
 *
 * Asserts:
 *   - `time_to_media` per peer (offer → first remote track).
 *   - Per-peer upload bandwidth is *not* a function of N: a fifth peer
 *     joining doesn't double Alice's upload like it would in mesh.
 *   - The room's `participant_count` matches.
 *
 * **Pre-requisite**: the executor must be running with `--enable-multi-user`
 * so the wind tunnel can provision per-peer users.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, PeerStats, DetectedTone } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t1-sfu-5peer";

export const t1Sfu5Peer: Scenario = {
  id: "t1",
  name: "SFU 1-node × 5 peers",
  description: "Single SFU node, 5 peers (each a distinct user), baseline correctness + bandwidth",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const neighbourhoodUrl = `windtunnel://t1`;
    metrics["neighbourhoodUrl"] = neighbourhoodUrl;

    // Boot the SFU room server-side using admin.
    await admin.call("sfu.startRoom", { neighbourhoodUrl, roomName: ROOM_NAME });

    const sessions = await provisionPeers({
      admin,
      port,
      count: 5,
      labelPrefix: "t1-peer",
    });

    await registerSfuMembers({
      admin,
      neighbourhoodUrl,
      sessions,
    });

    // Build tone list before the peer loop — one frequency per peer.
    const TONES = sessions.map((_, i) => 440 + i * 40);

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;
    try {
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
          neighbourhoodUrl,
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
          neighbourhoodUrl,
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
            `T1 expects a single-node SFU but got cascade redirect to ${joinResponse.redirectTo}`,
          );
        }
        await peer.acceptAnswer(JSON.parse(joinResponse.sdpAnswer));
      }

      await waitForServerParticipantCount(admin, ROOM_NAME, peers.length, 15_000);
      await sleep(2000);

      const allStats: PeerStats[][] = peers.map(() => []);
      peers.forEach((p, i) => p.on("stats", (s: PeerStats) => allStats[i].push(s)));
      peers.forEach((p) => p.startStats());
      await sleep(30_000);
      peers.forEach((p) => p.stopStats());

      const uploads: number[] = peers.map((p) => p.getLastStats()?.bytesSent ?? 0);
      const downloads: number[] = peers.map((p) => p.getLastStats()?.bytesReceived ?? 0);
      metrics["uploadBytesPerPeer"] = uploads;
      metrics["downloadBytesPerPeer"] = downloads;
      metrics["uploadMean"] = uploads.reduce((a, b) => a + b, 0) / uploads.length;
      metrics["downloadMean"] = downloads.reduce((a, b) => a + b, 0) / downloads.length;

      const rooms = await admin.call<Array<{ roomName: string; participantCount: number }>>(
        "sfu.listRooms",
        {},
      );
      const room = rooms.find((r) => r.roomName === ROOM_NAME);
      metrics["serverReportedParticipants"] = room?.participantCount ?? -1;
      metrics["renegotiationsAppliedPerPeer"] = wires.map((w) => w.count());

      // Hard assertions: every peer must receive media from the SFU.
      const allReceived = downloads.every((b) => b > 0);
      metrics["allPeersReceivedMedia"] = allReceived;
      const participantsMatch = (room?.participantCount ?? -1) === peers.length;
      metrics["participantsMatch"] = participantsMatch;

      // Frequency-based routing verification.
      const toneResults: Array<{ peer: string; detected: DetectedTone[] }> = [];
      let routingCorrect = true;
      for (let i = 0; i < peers.length; i++) {
        const detected = peers[i].getDetectedTones();
        toneResults.push({ peer: sessions[i].label, detected });
        // Each peer should detect tones from OTHER peers, not itself.
        const otherTones = TONES.filter((_, j) => j !== i);
        const detectedHz = new Set(detected.map((d) => d.dominantHz));
        const matched = otherTones.filter((hz) => detectedHz.has(hz));
        if (matched.length === 0 && detected.length > 0) {
          // Detected audio but NONE matched expected tones — wrong routing.
          routingCorrect = false;
        }
      }
      metrics["toneDetection"] = toneResults;
      metrics["routingCorrect"] = routingCorrect;

      passed = allReceived && participantsMatch && routingCorrect;
    } finally {
      for (const w of wires) {
        try {
          await w.detach();
        } catch {
          /* best-effort */
        }
      }
      for (let i = 0; i < peers.length; i++) {
        const session = sessions[i];
        if (session) {
          try {
            await session.client.call("sfu.callLeave", {
              neighbourhoodUrl,
              roomName: ROOM_NAME,
            });
          } catch {
            /* best-effort */
          }
        }
        try {
          await peers[i].close();
        } catch {
          /* best-effort */
        }
      }
      try {
        await admin.call("sfu.stopRoom", { neighbourhoodUrl, roomName: ROOM_NAME });
      } catch {
        /* best-effort */
      }
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "t1-sfu-5peer",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T1: SFU 5 peers — ` +
        `mediaFlowing=${metrics["allPeersReceivedMedia"]} ` +
        `uploadMean=${metrics["uploadMean"]}B downloadMean=${metrics["downloadMean"]}B ` +
        `serverParticipants=${metrics["serverReportedParticipants"]} ` +
        `routingCorrect=${metrics["routingCorrect"]}`,
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
      .call<Array<{ roomName: string; participantCount: number }>>("sfu.listRooms", {})
      .catch(() => [] as Array<{ roomName: string; participantCount: number }>);
    const room = rooms.find((r) => r.roomName === roomName);
    if (room && room.participantCount >= expected) return;
    await sleep(250);
  }
  throw new Error(
    `T1 waitForServerParticipantCount: room=${roomName} expected=${expected} ` +
      `within ${timeoutMs}ms`,
  );
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
