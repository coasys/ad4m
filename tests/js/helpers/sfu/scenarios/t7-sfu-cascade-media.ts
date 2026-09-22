/**
 * T7: Cross-node media flow verification.
 *
 * The cascade's central claim — media flows across pipe transports
 * between SFU nodes — demands explicit proof.  This scenario:
 *
 *   1. Starts a 2-node cascade cluster.
 *   2. Joins one peer on each node via forced assignment (peer A
 *      joins node A directly, peer B joins node B directly — no
 *      redirect following).
 *   3. Both peers attach synthetic audio + video streams.
 *   4. Polls `sfu.cascadeStatus` on both nodes until
 *      `establishedCount >= 1` (pipe transport up).
 *   5. Runs media for 10 seconds, collecting PeerStats at 1 Hz.
 *   6. Asserts BOTH peers report `bytesReceived > 0` — proving
 *      media crossed the pipe transport in both directions.
 *   7. Reports per-peer upload/download bytes and pipe establishment
 *      time.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, PeerStats, DetectedTone } from "../peer.js";
import { startCluster } from "../cascade.js";
import {
  provisionClusterPeers,
  disconnectClusterPeers,
  ClusterPeerSession,
  registerClusterSfuMembers,
} from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t7-cascade-media";
const NEIGHBOURHOOD = `windtunnel://t7`;
const WS_BASE = 16200;
const GOSSIP_BASE = 26200;
const MAX_PER_NODE = 4;
const MEDIA_DURATION_MS = 10_000;
const PIPE_TIMEOUT_MS = 15_000;

interface CascadeStatus {
  establishedCount: number;
  pipes: Array<{ roomId: string; remoteDid: string }>;
}

export const t7SfuCascadeMedia: Scenario = {
  id: "t7",
  name: "SFU cascade — cross-node media flow",
  description:
    "2 SFU nodes, 1 peer each, force-assigned — proves media crosses " +
    "the pipe transport in both directions (bytesReceived > 0 on BOTH peers)",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const cluster = await startCluster({
      executorBin: ctx.executorPath!,
      nodeCount: 2,
      maxParticipantsPerNode: MAX_PER_NODE,
      wsBasePort: WS_BASE,
      gossipBasePort: GOSSIP_BASE,
    });
    metrics["nodeDids"] = cluster.nodes.map((n) => n.did);

    // Build tone list — one frequency per peer (2 peers, step 80 Hz).
    const TONES = [440, 520];

    let sessions: ClusterPeerSession[] = [];
    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;

    try {
      // Open the room on both nodes.
      await Promise.all(
        cluster.nodes.map((n) =>
          n.client.call("sfu.startRoom", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          }),
        ),
      );

      // Provision 2 users across the cluster.
      sessions = await provisionClusterPeers({
        nodes: cluster.nodes.map((n) => ({
          nodeId: n.id,
          admin: n.client,
          port: n.port,
        })),
        count: 2,
        labelPrefix: "t7-peer",
      });

      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.id, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions,
      });

      // Join peer i on node i — forced assignment, no redirect following.
      for (let i = 0; i < 2; i++) {
        const node = cluster.nodes[i];
        const session = sessions[i];
        const entry = session.byNode.get(node.id);
        if (!entry) {
          throw new Error(`T7: missing cluster auth for node ${node.id}`);
        }

        const peer = new WebRtcPeer(session.label, {
          audioToneHz: TONES[i],
        });
        peer.enableAudioFingerprinting(TONES);
        await peer.attachSyntheticStream();
        peers.push(peer);

        const wire = await wireRenegotiation({
          client: entry.client,
          peer,
          token: entry.token,
          port: node.port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
        wires.push(wire);

        const offer = await peer.createOffer();
        const joinResp = await entry.client.call<{
          sdpAnswer: string;
          participantId: string;
          redirectTo?: string;
          streamMapping: string[];
        }>("sfu.callJoin", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });

        if (joinResp.redirectTo) {
          throw new Error(
            `T7: peer ${i} got unexpected redirect to ${joinResp.redirectTo} — ` +
              `force-assignment requires direct join on the target node`,
          );
        }
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));

        samples.push({
          name: `t7_peer_${i}_joined_node_${node.id}`,
          durationMs: 0,
          timestamp: Date.now(),
        });
      }

      // Poll cascadeStatus until pipes establish.
      const pipeWaitStart = Date.now();
      let pipeCounts: number[] = [];
      while (Date.now() - pipeWaitStart < PIPE_TIMEOUT_MS) {
        pipeCounts = await Promise.all(
          cluster.nodes.map((n) =>
            n.client
              .call<CascadeStatus>("sfu.cascadeStatus", {})
              .then((s) => s.establishedCount)
              .catch(() => 0),
          ),
        );
        if (pipeCounts.every((c) => c >= 1)) break;
        await sleep(250);
      }
      const pipeWaitMs = Date.now() - pipeWaitStart;
      metrics["pipeWaitMs"] = pipeWaitMs;
      metrics["pipeEstablished"] = pipeCounts.every((c) => c >= 1);
      samples.push({
        name: "pipe_establishment_wait",
        durationMs: pipeWaitMs,
        timestamp: Date.now(),
      });

      // Allow renegotiation to settle before measuring media.
      await sleep(2000);

      // Collect per-peer stats over the media window.
      const statTimeline: PeerStats[][] = peers.map(() => []);
      peers.forEach((p, i) =>
        p.on("stats", (s: PeerStats) => statTimeline[i].push(s)),
      );
      peers.forEach((p) => p.startStats());
      await sleep(MEDIA_DURATION_MS);
      peers.forEach((p) => p.stopStats());

      const uploads = peers.map((p) => p.getLastStats()?.bytesSent ?? 0);
      const downloads = peers.map(
        (p) => p.getLastStats()?.bytesReceived ?? 0,
      );
      metrics["uploadBytesPerPeer"] = uploads;
      metrics["downloadBytesPerPeer"] = downloads;
      metrics["renegotiationsPerPeer"] = wires.map((w) => w.count());

      // The central assertion: BOTH peers received bytes from the other
      // via the pipe transport.
      const bothReceived = downloads.every((b) => b > 0);
      metrics["bothPeersReceivedMedia"] = bothReceived;
      metrics["mediaAcrossPipeProven"] = bothReceived;
      const pipeEstablished = pipeCounts.every((c) => c >= 1);

      // Strict frequency verification: each peer MUST detect exactly
      // the other peer's tone — proving media crossed the pipe.
      const toneResults: Array<{ peer: string; detected: DetectedTone[] }> = [];
      let routingCorrect = true;
      for (let i = 0; i < peers.length; i++) {
        const detected = peers[i].getDetectedTones();
        toneResults.push({ peer: sessions[i].label, detected });
        const expectedHz = TONES[1 - i]; // the OTHER peer's frequency
        const detectedHz = new Set(detected.map((d) => d.dominantHz));
        if (!detectedHz.has(expectedHz)) {
          // Failed to detect the other peer's tone — media did not
          // cross the pipe correctly.
          routingCorrect = false;
        }
      }
      metrics["toneDetection"] = toneResults;
      metrics["routingCorrect"] = routingCorrect;

      passed = pipeEstablished && bothReceived && routingCorrect;
    } finally {
      for (const w of wires) {
        try {
          await w.detach();
        } catch {
          /* best-effort */
        }
      }
      for (let i = 0; i < peers.length; i++) {
        const node = cluster.nodes[i];
        const session = sessions[i];
        if (session && node) {
          const entry = session.byNode.get(node.id);
          try {
            await entry?.client.call("sfu.callLeave", {
              neighbourhoodUrl: NEIGHBOURHOOD,
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
      await disconnectClusterPeers(sessions);
      try {
        await cluster.shutdown();
      } catch {
        /* best-effort */
      }
    }

    const endTime = Date.now();
    return {
      scenario: "t7-sfu-cascade-media",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T7: cross-node media — pipeEstablished=${metrics["pipeEstablished"]} ` +
        `pipeWaitMs=${metrics["pipeWaitMs"]} ` +
        `uploads=[${(metrics["uploadBytesPerPeer"] as number[] ?? []).join(",")}]B ` +
        `downloads=[${(metrics["downloadBytesPerPeer"] as number[] ?? []).join(",")}]B ` +
        `mediaProven=${metrics["mediaAcrossPipeProven"]} ` +
        `routingCorrect=${metrics["routingCorrect"]}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
