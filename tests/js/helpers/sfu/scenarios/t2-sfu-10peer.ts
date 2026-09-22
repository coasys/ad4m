/**
 * T2: SFU 1-node × 10 peers.
 *
 * Same shape as T1 with N=10.  At this size:
 *   - Mesh would have 10*9/2 = 45 pairs (10×9=90 PCs) — clearly
 *     untenable.
 *   - SFU has 1 PC per peer; per-peer upload is O(1).
 *
 * Identifies the SFU's single-node bandwidth/CPU ceiling.  If T2
 * regresses heavily relative to T1, the SFU's media-relay loop is
 * the suspect.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, PeerStats, DetectedTone } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t2-sfu-10peer";
const PEER_COUNT = 10;

export const t2Sfu10Peer: Scenario = {
  id: "t2",
  name: "SFU 1-node × 10 peers",
  description: "Same as T1 with N=10 — single-node SFU bandwidth/CPU ceiling check",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client, branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const neighbourhoodUrl = `windtunnel://t2`;
    metrics["neighbourhoodUrl"] = neighbourhoodUrl;
    await client.call("sfu.startRoom", { neighbourhoodUrl, roomName: ROOM_NAME });

    const sessions = await provisionPeers({
      admin: client,
      port: ctx.port,
      count: PEER_COUNT,
      labelPrefix: "t2-peer",
    });

    await registerSfuMembers({
      admin: client,
      neighbourhoodUrl,
      sessions,
    });

    // Build tone list before the peer loop — one frequency per peer.
    const TONES = sessions.map((_, i) => 440 + i * 20);

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
          port: ctx.port,
          neighbourhoodUrl,
          roomName: ROOM_NAME,
        });
        wires.push(wire);

        const offer = await peer.createOffer();
        const joinStart = Date.now();
        const joinResp = await session.client.call<{
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

        if (joinResp.redirectTo) {
          throw new Error(
            `T2 expects a single-node SFU but got cascade redirect to ${joinResp.redirectTo}`,
          );
        }
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));
      }

      await waitForServerParticipantCount(client, ROOM_NAME, PEER_COUNT, 30_000);
      await sleep(2000);

      const allStats: PeerStats[][] = peers.map(() => []);
      peers.forEach((p, i) => p.on("stats", (s: PeerStats) => allStats[i].push(s)));
      peers.forEach((p) => p.startStats());
      await sleep(30_000);
      peers.forEach((p) => p.stopStats());

      const uploads = peers.map((p) => p.getLastStats()?.bytesSent ?? 0);
      const downloads = peers.map((p) => p.getLastStats()?.bytesReceived ?? 0);
      metrics["uploadBytesPerPeer"] = uploads;
      metrics["downloadBytesPerPeer"] = downloads;
      metrics["uploadMean"] = mean(uploads);
      metrics["downloadMean"] = mean(downloads);
      metrics["uploadStddev"] = stddev(uploads);
      metrics["downloadStddev"] = stddev(downloads);

      const rooms = await client.call<Array<{ roomName: string; participantCount: number }>>(
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
        const otherTones = TONES.filter((_, j) => j !== i);
        const detectedHz = new Set(detected.map((d) => d.dominantHz));
        const matched = otherTones.filter((hz) => detectedHz.has(hz));
        if (matched.length === 0 && detected.length > 0) {
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
        } catch {}
      }
      for (let i = 0; i < peers.length; i++) {
        try {
          await sessions[i]?.client.call("sfu.callLeave", {
            neighbourhoodUrl,
            roomName: ROOM_NAME,
          });
        } catch {}
        try {
          await peers[i].close();
        } catch {}
      }
      try {
        await client.call("sfu.stopRoom", { neighbourhoodUrl, roomName: ROOM_NAME });
      } catch {}
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "t2-sfu-10peer",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T2: SFU 10 peers — uploadMean=${metrics["uploadMean"]}B (sd=${metrics["uploadStddev"]}B) ` +
        `downloadMean=${metrics["downloadMean"]}B serverParticipants=${metrics["serverReportedParticipants"]} ` +
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
    `T2 waitForServerParticipantCount: room=${roomName} expected=${expected} within ${timeoutMs}ms`,
  );
}

function mean(arr: number[]): number {
  return arr.length ? Math.round(arr.reduce((a, b) => a + b, 0) / arr.length) : 0;
}

function stddev(arr: number[]): number {
  if (arr.length === 0) return 0;
  const m = arr.reduce((a, b) => a + b, 0) / arr.length;
  const variance = arr.reduce((a, b) => a + (b - m) ** 2, 0) / arr.length;
  return Math.round(Math.sqrt(variance));
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
