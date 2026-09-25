/**
 * S3: `max_participants_per_node` enforcement.
 *
 * Single 2-node cluster with `maxParticipantsPerNode = 3`.  Drive 7
 * peers through node A.  The first 3 land on A, the next 3 redirect
 * to B (and land), and the 7th gets a hard redirect cycle (both
 * nodes are at capacity).
 *
 * Verifies:
 *  - Each node enforces its own capacity.
 *  - When ALL nodes are at capacity, the SFU still returns a
 *    `redirectTo` (the harness detects the cycle).  Without an
 *    overflow handler the call doesn't land, which surfaces as a
 *    final-peer error.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { startCluster, CascadeNode } from "../cascade.js";
import {
  provisionClusterPeers,
  disconnectClusterPeers,
  registerClusterSfuMembers,
  ClusterPeerSession,
} from "../users.js";

const ROOM_NAME = "s3-max-participants";
const NEIGHBOURHOOD = `windtunnel://s3`;
const MAX_PER_NODE = 3;
const TOTAL_PEERS = 7;

export const s3MaxParticipantsEnforced: Scenario = {
  id: "s3-sfu",
  name: "max_participants_per_node enforcement",
  description: "2-node cluster, cap=3 each, 7 peers — last peer must overflow cleanly",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    let cluster: Awaited<ReturnType<typeof startCluster>> | null = null;
    const peers: { peer: WebRtcPeer; node: CascadeNode; session: ClusterPeerSession }[] = [];
    let clusterSessions: ClusterPeerSession[] = [];
    const overflowed: { idx: number; reason: string }[] = [];
    let passed = false;

    try {
      cluster = await startCluster({
        executorBin: ctx.executorPath!,
        nodeCount: 2,
        maxParticipantsPerNode: MAX_PER_NODE,
        wsBasePort: 13300,
      });

      const didToNode = new Map<string, CascadeNode>();
      for (const n of cluster.nodes) {
        didToNode.set(n.did, n);
        await n.client.call("sfu.startRoom", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
      }
      clusterSessions = await provisionClusterPeers({
        nodes: cluster.nodes.map((n) => ({
          nodeId: n.did,
          admin: n.client,
          port: n.port,
        })),
        count: TOTAL_PEERS,
        labelPrefix: "s3-peer",
      });
      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.did, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions: clusterSessions,
      });
      const nodeA = cluster.nodes[0];
      const counts: Record<string, number> = {};
      cluster.nodes.forEach((n) => (counts[n.did] = 0));

      for (let i = 0; i < TOTAL_PEERS; i++) {
        const cs = clusterSessions[i];
        const peer = new WebRtcPeer(cs.label, { audioToneHz: 440 + i * 20 });
        await peer.attachSyntheticStream();
        const offer = await peer.createOffer();

        let landed: CascadeNode | null = null;
        const aClient = cs.byNode.get(nodeA.did)!.client;
        let session = await aClient.call<{
          sdpAnswer: string;
          participantId: string;
          redirectTo?: string;
          streamMapping: string[];
        }>("sfu.callJoin", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });

        const seenNodes = new Set<string>([nodeA.did]);
        while (session.redirectTo) {
          if (seenNodes.has(session.redirectTo)) {
            overflowed.push({
              idx: i,
              reason: `redirect cycle — all ${cluster.nodes.length} nodes at capacity`,
            });
            await peer.close().catch(() => {});
            session = null as any;
            break;
          }
          seenNodes.add(session.redirectTo);
          const target = didToNode.get(session.redirectTo);
          if (!target) {
            overflowed.push({ idx: i, reason: `unknown redirect target ${session.redirectTo}` });
            await peer.close().catch(() => {});
            session = null as any;
            break;
          }
          const targetClient = cs.byNode.get(target.did)!.client;
          session = await targetClient.call<{
            sdpAnswer: string;
            participantId: string;
            redirectTo?: string;
            streamMapping: string[];
          }>("sfu.callJoin", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
            sdpOffer: JSON.stringify(offer),
          });
          if (!session.redirectTo) {
            landed = target;
          }
        }
        if (session && !session.redirectTo) {
          await peer.acceptAnswer(JSON.parse(session.sdpAnswer));
          if (!landed) landed = nodeA;
          peers.push({ peer, node: landed, session: cs });
          counts[landed.did]++;
        }
      }

      metrics["participantsPerNode"] = counts;
      metrics["overflowedPeers"] = overflowed;
      metrics["overflowCount"] = overflowed.length;
      metrics["landedCount"] = peers.length;
      metrics["expectedOverflow"] = TOTAL_PEERS - cluster.nodes.length * MAX_PER_NODE;
      metrics["capacityEnforced"] = Object.values(counts).every((c) => c <= MAX_PER_NODE);
      passed = Object.values(counts).every((c) => c <= MAX_PER_NODE);

      samples.push({
        name: "s3_join_window",
        durationMs: Date.now() - startTime,
        timestamp: Date.now(),
      });
    } finally {
      for (const { peer, node, session } of peers) {
        try {
          await session.byNode.get(node.did)!.client.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {}
        try {
          await peer.close();
        } catch {}
      }
      await disconnectClusterPeers(clusterSessions);
      if (cluster) {
        try {
          await cluster.shutdown();
        } catch {}
      }
    }

    const endTime = Date.now();
    return {
      scenario: "s3-max-participants",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `S3: cap-enforced — landed=${metrics["landedCount"]}/${TOTAL_PEERS}, ` +
        `overflowed=${metrics["overflowCount"]} (expected=${metrics["expectedOverflow"]}), ` +
        `capacityEnforced=${metrics["capacityEnforced"]}`,
    };
  },
};
