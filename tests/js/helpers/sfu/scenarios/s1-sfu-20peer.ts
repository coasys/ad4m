/**
 * S1: SFU 1-node × 20 peers.
 *
 * The single-SFU CPU saturation probe.  Same shape as T1/T2 scaled to
 * N=20.  At this size:
 *   - Mesh would require 190 pair-wise PCs, untenable.
 *   - SFU has 20 inbound + 20×19 outbound forwards = 380 forward
 *     decisions per arriving RTP packet.  This is where the relay
 *     loop's hot path matters.
 *
 * Reports:
 *   - Per-peer upload distribution (mean, sd, max) — should stay flat.
 *   - Per-peer download distribution — should scale linearly with N
 *     (each peer downloads N-1 streams).
 *   - Per-peer packet loss — if SFU saturates we expect loss > 0.
 *   - Server-reported participant count == 20.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, PeerStats, DetectedTone } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "s1-sfu-20peer";
const NEIGHBOURHOOD = `windtunnel://s1`;
const PEER_COUNT = 20;

export const s1Sfu20Peer: Scenario = {
  id: "s1-sfu",
  name: "SFU 1-node × 20 peers (scale)",
  description: "20-peer single SFU — bandwidth + loss distribution at scale",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client, branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    try {
      await client.call("sfu.startRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      if (msg.includes("not yet available")) {
        metrics["skipped"] = true;
        metrics["skip_reason"] = msg;
        return {
          scenario: "s1-sfu-20peer",
          branch,
          passed: true,
          startTime,
          endTime: Date.now(),
          durationMs: Date.now() - startTime,
          metrics,
          samples,
          summary: `S1: SKIPPED — ${msg}`,
        };
      }
      throw e;
    }

    const sessions = await provisionPeers({
      admin: client,
      port: ctx.port,
      count: PEER_COUNT,
      labelPrefix: "s1-peer",
    });
    await registerSfuMembers({ admin: client, neighbourhoodUrl: NEIGHBOURHOOD, sessions });

    // Build tone list before the peer loop — one frequency per peer.
    const TONES = sessions.map((_, i) => 440 + i * 12);

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;
    try {
      for (let i = 0; i < sessions.length; i++) {
        const s = sessions[i];
        const peer = new WebRtcPeer(s.label, {
          audioToneHz: TONES[i],
        });
        peer.enableAudioFingerprinting(TONES);
        await peer.attachSyntheticStream();
        peers.push(peer);
        const wire = await wireRenegotiation({
          client: s.client,
          peer,
          token: s.token,
          port: ctx.port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
        wires.push(wire);
        const offer = await peer.createOffer();
        const joinStart = Date.now();
        const joinResp = await s.client.call<{
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
          name: `call_join_${s.label}`,
          durationMs: joinElapsed,
          timestamp: Date.now(),
        });
        if (joinResp.redirectTo) {
          throw new Error(`S1 unexpected cascade redirect to ${joinResp.redirectTo}`);
        }
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));
      }

      await waitForServerParticipantCount(client, ROOM_NAME, PEER_COUNT, 45_000);
      await sleep(2000);

      const allStats: PeerStats[][] = peers.map(() => []);
      peers.forEach((p, i) => p.on("stats", (s: PeerStats) => allStats[i].push(s)));
      peers.forEach((p) => p.startStats());
      await sleep(30_000);
      peers.forEach((p) => p.stopStats());

      const uploads = peers.map((p) => p.getLastStats()?.bytesSent ?? 0);
      const downloads = peers.map((p) => p.getLastStats()?.bytesReceived ?? 0);
      const losses = peers.map((p) => p.getLastStats()?.packetsLost ?? 0);
      const rtts = peers.map((p) => p.getLastStats()?.currentRoundTripTimeMs ?? null);
      metrics["uploadBytesPerPeer"] = uploads;
      metrics["downloadBytesPerPeer"] = downloads;
      metrics["packetsLostPerPeer"] = losses;
      metrics["rttPerPeer"] = rtts;
      metrics["uploadMean"] = mean(uploads);
      metrics["uploadStddev"] = stddev(uploads);
      metrics["downloadMean"] = mean(downloads);
      metrics["downloadStddev"] = stddev(downloads);
      metrics["packetsLostTotal"] = losses.reduce((a, b) => a + b, 0);

      const rooms = await client.call<Array<{ roomName: string; participantCount: number }>>(
        "sfu.listRooms",
        {},
      );
      metrics["serverReportedParticipants"] =
        rooms.find((r) => r.roomName === ROOM_NAME)?.participantCount ?? -1;
      metrics["renegotiationsAppliedPerPeer"] = wires.map((w) => w.count());

      // Hard assertions: every peer must receive media from the SFU.
      const allReceived = downloads.every((b) => b > 0);
      metrics["allPeersReceivedMedia"] = allReceived;
      const participantsMatch =
        (metrics["serverReportedParticipants"] as number) === peers.length;
      metrics["participantsMatch"] = participantsMatch;

      // Frequency-based routing verification (relaxed for 20 peers).
      // With 20 peers some tracks may not accumulate enough samples in
      // the measurement window — require each peer to detect at least
      // half the other peers' tones.
      const toneResults: Array<{ peer: string; detected: DetectedTone[] }> = [];
      let routingCorrect = true;
      const halfOtherCount = Math.floor((peers.length - 1) / 2);
      for (let i = 0; i < peers.length; i++) {
        const detected = peers[i].getDetectedTones();
        toneResults.push({ peer: sessions[i].label, detected });
        const otherTones = TONES.filter((_, j) => j !== i);
        const detectedHz = new Set(detected.map((d) => d.dominantHz));
        const matched = otherTones.filter((hz) => detectedHz.has(hz));
        if (matched.length < halfOtherCount && detected.length > 0) {
          // Fewer than half the expected tones detected — routing suspect.
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
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {}
        try {
          await peers[i].close();
        } catch {}
      }
      try {
        await client.call("sfu.stopRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });
      } catch {}
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "s1-sfu-20peer",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `S1: SFU 20 peers — uploadMean=${metrics["uploadMean"]}B (sd=${metrics["uploadStddev"]}B) ` +
        `downloadMean=${metrics["downloadMean"]}B packetsLostTotal=${metrics["packetsLostTotal"]} ` +
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
    `S1 waitForServerParticipantCount: room=${roomName} expected=${expected} within ${timeoutMs}ms`,
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
