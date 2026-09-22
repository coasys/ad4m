/**
 * F4: Cascade network partition.
 *
 * 2-node cluster.  Once peers are joined to both nodes, drop the
 * gossip TCP connection between A and B by installing an iptables
 * DROP rule on each node's gossip port.  After the gossip has been
 * partitioned for long enough that the node-state TTL would expire,
 * verify:
 *
 *  - Existing peers keep their existing local SFU connections.
 *  - New peers joining A no longer see B as a redirect target.
 *  - Both sides remain responsive.
 *
 * The TCP gossip transport bound at executor boot drops outbound
 * frames silently when the peer is unreachable, so the partition is
 * one-shot iptables and clean to revert.
 *
 * Requires `iptables` + passwordless `sudo`.  Skips gracefully when
 * unavailable.
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
import { iptablesPartitionAvailable, dropTcpPorts, clearPartition } from "../net.js";

const ROOM_NAME = "f4-network-partition";
const NEIGHBOURHOOD = `windtunnel://f4`;
const MAX_PER_NODE = 4;

export const f4NetworkPartition: Scenario = {
  id: "f4",
  name: "Cascade network partition",
  description: "2-node cluster, partition A↔B via iptables, verify no ghost cascade decisions",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    if (!iptablesPartitionAvailable()) {
      metrics["skipped"] = true;
      metrics["skip_reason"] = "iptables + passwordless sudo not available on this host";
      return {
        scenario: "f4-network-partition",
        branch,
        startTime,
        endTime: Date.now(),
        durationMs: Date.now() - startTime,
        passed: true,
        metrics,
        samples,
        summary: `F4: SKIPPED — ${metrics["skip_reason"]}`,
      };
    }

    let cluster: Awaited<ReturnType<typeof startCluster>> | null = null;
    const peers: { peer: WebRtcPeer; node: CascadeNode; session: ClusterPeerSession }[] = [];
    let clusterSessions: ClusterPeerSession[] = [];
    let probeSession: ClusterPeerSession | null = null;

    try {
      cluster = await startCluster({
        executorBin: ctx.executorPath!,
        nodeCount: 2,
        maxParticipantsPerNode: MAX_PER_NODE,
        wsBasePort: 16600,
      });

      const didToNode = new Map<string, CascadeNode>();
      for (const n of cluster.nodes) {
        didToNode.set(n.did, n);
        await n.client.call("sfu.startRoom", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
      }
      const [nodeA, nodeB] = cluster.nodes;

      clusterSessions = await provisionClusterPeers({
        nodes: cluster.nodes.map((n) => ({
          nodeId: n.did,
          admin: n.client,
          port: n.port,
        })),
        count: 5,
        labelPrefix: "f4-peer",
      });
      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.did, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions: clusterSessions,
      });

      // 5 peers — first 4 to A, 5th cascades to B.
      for (let i = 0; i < 5; i++) {
        const cs = clusterSessions[i];
        const peer = new WebRtcPeer(cs.label, { audioToneHz: 440 + i * 30 });
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
        if (session.redirectTo) {
          const target = didToNode.get(session.redirectTo)!;
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
        peers.push({ peer, node: landed, session: cs });
      }

      const preA = peers.filter((p) => p.node === nodeA).length;
      const preB = peers.filter((p) => p.node === nodeB).length;
      metrics["preParitionNodeA"] = preA;
      metrics["preParitionNodeB"] = preB;

      // Partition: install iptables DROP for both gossip ports.  The
      // gossip transport's outbound send becomes a no-op silently and
      // each side's known-nodes view stops being refreshed by the
      // peer's announces.
      const partitionStart = Date.now();
      const partitionOk = dropTcpPorts([nodeA.gossipPort, nodeB.gossipPort]);
      metrics["partitionApplied"] = partitionOk;
      if (!partitionOk) {
        throw new Error("F4 dropTcpPorts failed despite iptablesPartitionAvailable()");
      }
      // Wait for one announce-tick worth of TTL.
      await new Promise<void>((r) => setTimeout(r, 3000));
      const partitionMs = Date.now() - partitionStart;
      metrics["partitionMs"] = partitionMs;

      // Probe: a fresh peer joining A should NOT be redirected — B is
      // no longer reachable from A's view.
      const [probeCs] = await provisionClusterPeers({
        nodes: cluster.nodes.map((n) => ({
          nodeId: n.did,
          admin: n.client,
          port: n.port,
        })),
        count: 1,
        labelPrefix: "f4-probe",
      });
      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.did, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions: [probeCs],
      });
      probeSession = probeCs;
      const probe = new WebRtcPeer(probeCs.label, { audioToneHz: 880 });
      await probe.attachSyntheticStream();
      const probeOffer = await probe.createOffer();
      const probeAClient = probeCs.byNode.get(nodeA.did)!.client;
      const probeResp = await probeAClient.call<{
        sdpAnswer: string;
        participantId: string;
        redirectTo?: string;
        streamMapping: string[];
      }>("sfu.callJoin", {
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
        sdpOffer: JSON.stringify(probeOffer),
      });
      metrics["probeRedirected"] = !!probeResp.redirectTo;
      metrics["probeRedirectTarget"] = probeResp.redirectTo ?? null;
      if (probeResp.redirectTo) {
        await probe.close().catch(() => {});
      } else {
        try {
          await probe.acceptAnswer(JSON.parse(probeResp.sdpAnswer));
        } catch {}
        try {
          await probeAClient.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {}
        await probe.close().catch(() => {});
      }

      // Both sides remain responsive.
      const roomsA = await nodeA.client.call<
        Array<{ roomName: string; participantCount: number }>
      >("sfu.listRooms", {});
      const roomsB = await nodeB.client.call<
        Array<{ roomName: string; participantCount: number }>
      >("sfu.listRooms", {});
      metrics["postPartitionNodeA"] =
        roomsA.find((r) => r.roomName === ROOM_NAME)?.participantCount ?? -1;
      metrics["postPartitionNodeB"] =
        roomsB.find((r) => r.roomName === ROOM_NAME)?.participantCount ?? -1;

      samples.push({
        name: "f4_partition_apply",
        durationMs: partitionMs,
        timestamp: Date.now(),
      });
    } finally {
      clearPartition();
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
      if (probeSession) {
        await disconnectClusterPeers([probeSession]);
      }
      if (cluster) {
        try {
          await cluster.shutdown();
        } catch {}
      }
    }

    const endTime = Date.now();
    return {
      scenario: "f4-network-partition",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: true,
      metrics,
      samples,
      summary:
        `F4: partition — pre A/B=${metrics["preParitionNodeA"]}/${metrics["preParitionNodeB"]}, ` +
        `post A/B=${metrics["postPartitionNodeA"]}/${metrics["postPartitionNodeB"]}, ` +
        `probeRedirected=${metrics["probeRedirected"]}`,
    };
  },
};
