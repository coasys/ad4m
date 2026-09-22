/**
 * M3: Cascade node failure mid-call.
 *
 * 2-node cluster, max=5 each.  Join 6 peers — 5 land on A, 1 redirects
 * to B.  Kill node A's executor process.  Reconnect the 5 peers that
 * were on A to node B (the wind tunnel simulates the failover the way
 * flux's `handleCascadeFailover` does: when the local SFU connection
 * dies, try the next cascade node).  Measure failover time +
 * participantCount on B after.
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

const ROOM_NAME = "m3-cascade-failover";
const NEIGHBOURHOOD = `windtunnel://m3`;
const TOTAL_PEERS = 6;
// Must absorb all peers after one node dies: B's original share + A's peers.
const MAX_PER_NODE = TOTAL_PEERS;

interface TrackedPeer {
  peer: WebRtcPeer;
  node: CascadeNode;
  session: ClusterPeerSession;
}

export const m3CascadeFailover: Scenario = {
  id: "m3-sfu",
  name: "Cascade node failure mid-call",
  description: "Kill node A mid-call; verify peers reconnect to node B",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    let cluster: Awaited<ReturnType<typeof startCluster>> | null = null;
    const peers: TrackedPeer[] = [];
    let clusterSessions: ClusterPeerSession[] = [];

    try {
      cluster = await startCluster({
        executorBin: ctx.executorPath!,
        nodeCount: 2,
        maxParticipantsPerNode: MAX_PER_NODE,
        wsBasePort: 16500,
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
        count: TOTAL_PEERS,
        labelPrefix: "m3-peer",
      });
      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.did, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions: clusterSessions,
      });

      for (let i = 0; i < TOTAL_PEERS; i++) {
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
          const target = didToNode.get(session.redirectTo);
          if (!target) throw new Error(`M3 unknown redirect ${session.redirectTo}`);
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

      const initialACount = peers.filter((p) => p.node === nodeA).length;
      const initialBCount = peers.filter((p) => p.node === nodeB).length;
      metrics["initialNodeAParticipants"] = initialACount;
      metrics["initialNodeBParticipants"] = initialBCount;

      // Kill node A.
      const killStart = Date.now();
      try {
        nodeA.process.kill("SIGKILL");
      } catch {}
      // Wait until A's port is no longer responsive.
      let aDead = false;
      for (let waited = 0; waited < 5000; waited += 250) {
        try {
          const res = await fetch(`http://127.0.0.1:${nodeA.port}/health`);
          if (!res.ok) {
            aDead = true;
            break;
          }
        } catch {
          aDead = true;
          break;
        }
        await sleep(250);
      }
      metrics["nodeAKilledAt"] = killStart;
      metrics["nodeAConfirmedDead"] = aDead;

      // Failover: reconnect peers that were on A onto B.
      const failoverStart = Date.now();
      const reconnected: { idx: number; ok: boolean }[] = [];
      for (let i = 0; i < peers.length; i++) {
        const tp = peers[i];
        if (tp.node !== nodeA) continue;
        try {
          await tp.peer.close();
        } catch {}
        const fresh = new WebRtcPeer(`m3-failover-${i}`, { audioToneHz: 440 + i * 30 });
        await fresh.attachSyntheticStream();
        const offer = await fresh.createOffer();
        try {
          const bClient = tp.session.byNode.get(nodeB.did)!.client;
          const session = await bClient.call<{
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
            // B is also at capacity — no cascade target.
            reconnected.push({ idx: i, ok: false });
            await fresh.close().catch(() => {});
            continue;
          }
          await fresh.acceptAnswer(JSON.parse(session.sdpAnswer));
          peers[i] = { peer: fresh, node: nodeB, session: tp.session };
          reconnected.push({ idx: i, ok: true });
        } catch (e) {
          reconnected.push({ idx: i, ok: false });
          await fresh.close().catch(() => {});
        }
      }
      const failoverMs = Date.now() - failoverStart;
      metrics["failoverMs"] = failoverMs;
      metrics["reconnectAttempts"] = reconnected.length;
      metrics["reconnectSuccess"] = reconnected.filter((r) => r.ok).length;

      samples.push({
        name: "m3_failover_window",
        durationMs: failoverMs,
        timestamp: Date.now(),
      });

      // Final state on node B.
      await sleep(300);
      const rooms = await nodeB.client.call<
        Array<{ roomName: string; participantCount: number }>
      >("sfu.listRooms", {});
      metrics["finalNodeBParticipants"] =
        rooms.find((r) => r.roomName === ROOM_NAME)?.participantCount ?? -1;
    } finally {
      for (const tp of peers) {
        if (tp.node === cluster?.nodes[0]) continue; // A is dead, skip leave
        try {
          await tp.session.byNode.get(tp.node.did)!.client.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {}
        try {
          await tp.peer.close();
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
    // Hard gate: all peers that were on A must successfully reconnect to B.
    const reconnectCount = (metrics["reconnectSuccess"] as number) ?? 0;
    const reconnectAttempts = (metrics["reconnectAttempts"] as number) ?? 0;
    const passed = reconnectAttempts > 0 && reconnectCount === reconnectAttempts;
    return {
      scenario: "m3-cascade-failover",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `M3: failover — initial A=${metrics["initialNodeAParticipants"]}, B=${metrics["initialNodeBParticipants"]} → ` +
        `reconnected ${reconnectCount}/${reconnectAttempts} in ${metrics["failoverMs"]}ms, ` +
        `final B=${metrics["finalNodeBParticipants"]}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
