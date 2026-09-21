/**
 * S2: Cascaded 4-node × 40 peers.
 *
 * Same shape as T4 with 4 nodes and 40 peers (10 per node @
 * max=10).  Stresses the cascade decision under load.
 *
 * Each peer issues `sfu.callJoin` against node A and follows any
 * `redirectTo`; the wind tunnel verifies the room ends up split
 * 10/10/10/10 across the cluster (give or take 1 due to redirect
 * decision timing).
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
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "s2-sfu-cascade-4node";
const NEIGHBOURHOOD = `windtunnel://s2`;
const PEER_COUNT = 40;
const MAX_PER_NODE = 10;

export const s2SfuCascade4Node: Scenario = {
  id: "s2-sfu",
  name: "Cascaded 4-node × 40 peers",
  description: "Four SFU nodes, 40 peers; cascade decision under load",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    let cluster: Awaited<ReturnType<typeof startCluster>> | null = null;
    const peers: {
      peer: WebRtcPeer;
      node: CascadeNode;
      session: ClusterPeerSession;
      wire: RenegotiationWire;
    }[] = [];
    let clusterSessions: ClusterPeerSession[] = [];

    try {
      cluster = await startCluster({
        executorBin: ctx.executorPath!,
        nodeCount: 4,
        maxParticipantsPerNode: MAX_PER_NODE,
        wsBasePort: 16400,
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
        count: PEER_COUNT,
        labelPrefix: "s2-peer",
      });
      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.did, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions: clusterSessions,
      });

      const nodeA = cluster.nodes[0];
      const counts: Record<string, number> = {};
      cluster.nodes.forEach((n) => (counts[n.did] = 0));

      for (let i = 0; i < PEER_COUNT; i++) {
        const cs = clusterSessions[i];
        const peer = new WebRtcPeer(cs.label, { audioToneHz: 440 + (i % 30) * 10 });
        await peer.attachSyntheticStream();
        const offer = await peer.createOffer();

        let landed: CascadeNode = nodeA;
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
        let hops = 1;
        while (session.redirectTo) {
          const target = didToNode.get(session.redirectTo);
          if (!target) throw new Error(`S2 peer ${i} redirectTo unknown ${session.redirectTo}`);
          hops++;
          if (hops > cluster.nodes.length + 1) {
            throw new Error(`S2 peer ${i} bounced ${hops} times — cascade cycle`);
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
          landed = target;
        }
        await peer.acceptAnswer(JSON.parse(session.sdpAnswer));
        // Subscribe to server-pushed renegotiation on whichever node
        // the peer landed on, so download bytes from co-located peers
        // are received.
        const landedEntry = cs.byNode.get(landed.did)!;
        const wire = await wireRenegotiation({
          client: landedEntry.client,
          peer,
          token: landedEntry.token,
          port: landed.port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
        peers.push({ peer, node: landed, session: cs, wire });
        counts[landed.did]++;
      }

      metrics["participantsPerNode"] = counts;
      const total = Object.values(counts).reduce((a, b) => a + b, 0);
      const maxPerNodeActual = Math.max(...Object.values(counts));
      const minPerNodeActual = Math.min(...Object.values(counts));
      metrics["totalLanded"] = total;
      metrics["maxPerNodeActual"] = maxPerNodeActual;
      metrics["minPerNodeActual"] = minPerNodeActual;
      metrics["distributionOk"] = total === PEER_COUNT && maxPerNodeActual <= MAX_PER_NODE;

      samples.push({
        name: "s2_cascade_join_window",
        durationMs: Date.now() - startTime,
        timestamp: Date.now(),
      });

      // Settle + measure intra-node download bandwidth so we can
      // assert that the renegotiation pipeline is alive on each
      // cascade node.  Cross-node media flow needs Phase F.
      await sleep(2000);
      peers.forEach(({ peer: p }) => p.startStats());
      await sleep(15_000);
      peers.forEach(({ peer: p }) => p.stopStats());
      const downloads = peers.map(({ peer: p }) => p.getLastStats()?.bytesReceived ?? 0);
      metrics["downloadBytesPerPeer"] = downloads;
      metrics["downloadMean"] = mean(downloads);
      metrics["renegotiationsAppliedPerPeer"] = peers.map((p) => p.wire.count());
    } finally {
      for (const { wire } of peers) {
        try {
          await wire.detach();
        } catch {}
      }
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
    const distributionOk = metrics["distributionOk"] === true;
    return {
      scenario: "s2-sfu-cascade-4node",
      branch,
      passed: distributionOk,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `S2: 4-node cascade — peers=${PEER_COUNT}, total=${metrics["totalLanded"]}, ` +
        `[min=${metrics["minPerNodeActual"]}, max=${metrics["maxPerNodeActual"]}] ` +
        `downloadMean=${metrics["downloadMean"]}B (ok=${distributionOk})`,
    };
  },
};

function mean(arr: number[]): number {
  return arr.length ? Math.round(arr.reduce((a, b) => a + b, 0) / arr.length) : 0;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
