/**
 * F9: Cascade node crash cleanup (TTL eviction).
 *
 * Verifies that when a cascade node dies without a graceful Leave,
 * the surviving node evicts the stale pipe entry after the TTL
 * expires (30 s) and its own peers remain functional.
 *
 * Flow:
 *   1. Start a 2-node cascade cluster.
 *   2. Join 2 peers on node A, 2 on node B.
 *   3. Wait for pipe establishment.
 *   4. Kill node B's executor process (SIGKILL — no graceful leave).
 *   5. Wait 35 seconds (TTL default = 30 s).
 *   6. Query `sfu.cascadeStatus` on node A.
 *   7. Assert established pipe count dropped to 0 (stale entry evicted).
 *   8. Verify node A's peers remain connected and functional
 *      (`sfu.listRooms` still returns).
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { startCluster, CascadeNode } from "../cascade.js";
import {
  provisionClusterPeers,
  disconnectClusterPeers,
  ClusterPeerSession,
  registerClusterSfuMembers,
} from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "f9-crash-cleanup";
const NEIGHBOURHOOD = `windtunnel://f9`;
const WS_BASE = 13200;
const GOSSIP_BASE = 25200;
const MAX_PER_NODE = 4;
const PIPE_TIMEOUT_MS = 15_000;
const TTL_WAIT_MS = 35_000;

interface CascadeStatus {
  establishedCount: number;
  pipes: Array<{ roomId: string; remoteDid: string }>;
}

export const f9CascadeNodeCrashCleanup: Scenario = {
  id: "f9",
  name: "Cascade node crash cleanup",
  description:
    "Kill a cascade node (SIGKILL) — verify the surviving node " +
    "evicts the stale pipe after TTL and its peers remain functional",

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
    const [nodeA, nodeB] = cluster.nodes;
    metrics["nodeA_did"] = nodeA.did;
    metrics["nodeB_did"] = nodeB.did;

    let sessions: ClusterPeerSession[] = [];
    const peersA: { peer: WebRtcPeer; wire: RenegotiationWire }[] = [];
    const peersB: { peer: WebRtcPeer; wire: RenegotiationWire }[] = [];

    try {
      // Start rooms on both nodes.
      await Promise.all(
        cluster.nodes.map((n) =>
          n.client.call("sfu.startRoom", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          }),
        ),
      );

      // Provision 4 users across the cluster.
      sessions = await provisionClusterPeers({
        nodes: cluster.nodes.map((n) => ({
          nodeId: n.id,
          admin: n.client,
          port: n.port,
        })),
        count: 4,
        labelPrefix: "f9-peer",
      });

      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.id, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions,
      });

      // Join peers: 0-1 on node A, 2-3 on node B.
      for (let i = 0; i < 4; i++) {
        const targetNode = i < 2 ? nodeA : nodeB;
        const session = sessions[i];
        const entry = session.byNode.get(targetNode.id);
        if (!entry) {
          throw new Error(
            `F9: missing cluster auth for node ${targetNode.id}`,
          );
        }

        const peer = new WebRtcPeer(session.label, {
          audioToneHz: 440 + i * 40,
        });
        await peer.attachSyntheticStream();

        const wire = await wireRenegotiation({
          client: entry.client,
          peer,
          token: entry.token,
          port: targetNode.port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });

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
            `F9: peer ${i} got unexpected redirect — force-assignment required`,
          );
        }
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));

        if (i < 2) {
          peersA.push({ peer, wire });
        } else {
          peersB.push({ peer, wire });
        }
      }

      // Wait for pipe establishment.
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
        name: "pipe_establishment",
        durationMs: pipeWaitMs,
        timestamp: Date.now(),
      });

      // Record pre-crash pipe count on node A.
      const preCrashStatus = await nodeA.client
        .call<CascadeStatus>("sfu.cascadeStatus", {})
        .catch(() => ({ establishedCount: -1, pipes: [] }));
      metrics["preCrash_nodeA_pipeCount"] = preCrashStatus.establishedCount;

      // ── Kill node B (SIGKILL — no graceful leave) ──
      const killStart = Date.now();
      try {
        nodeB.process.kill("SIGKILL");
      } catch {
        /* process may have already exited */
      }
      samples.push({
        name: "node_b_killed",
        durationMs: 0,
        timestamp: Date.now(),
      });

      // Detach node B's renegotiation wires — the events WS is dead.
      for (const pb of peersB) {
        try {
          await pb.wire.detach();
        } catch {
          /* expected — node B is gone */
        }
      }

      // ── Wait for TTL eviction ──
      await sleep(TTL_WAIT_MS);
      const ttlElapsed = Date.now() - killStart;
      samples.push({
        name: "ttl_wait",
        durationMs: ttlElapsed,
        timestamp: Date.now(),
      });

      // ── Verify node A ──
      const postCrashStatus = await nodeA.client
        .call<CascadeStatus>("sfu.cascadeStatus", {})
        .catch(() => ({ establishedCount: -1, pipes: [] }));
      metrics["postCrash_nodeA_pipeCount"] =
        postCrashStatus.establishedCount;
      metrics["stalePipeEvicted"] =
        postCrashStatus.establishedCount === 0;

      // Verify node A's SFU still responds.
      let nodeAResponsive = false;
      try {
        const rooms = await nodeA.client.call<
          Array<{ roomName: string; participantCount: number }>
        >("sfu.listRooms", {});
        nodeAResponsive = true;
        const room = rooms.find((r) => r.roomName === ROOM_NAME);
        metrics["nodeA_participantsAfterCrash"] =
          room?.participantCount ?? -1;
      } catch (e) {
        metrics["nodeA_listRoomsError"] =
          e instanceof Error ? e.message : String(e);
      }
      metrics["nodeA_stillResponsive"] = nodeAResponsive;

      // Check node A peers' WebRTC state.
      metrics["nodeA_peerStates"] = peersA.map((pa) => ({
        label: pa.peer.id,
        renegotiations: pa.wire.count(),
        lastBytesSent: pa.peer.getLastStats()?.bytesSent ?? 0,
      }));
    } finally {
      // Tear down node A peers.
      for (const pa of peersA) {
        try {
          await pa.wire.detach();
        } catch {
          /* best-effort */
        }
        try {
          await pa.peer.close();
        } catch {
          /* best-effort */
        }
      }
      // Node B peers are already dead (process killed).
      for (const pb of peersB) {
        try {
          await pb.peer.close();
        } catch {
          /* best-effort */
        }
      }
      await disconnectClusterPeers(sessions);
      // Shutdown will force-kill any remaining processes.
      try {
        await cluster.shutdown();
      } catch {
        /* best-effort */
      }
    }

    const endTime = Date.now();
    const stalePipeEvicted = metrics["stalePipeEvicted"] === true;
    const nodeAResponsive = metrics["nodeA_stillResponsive"] === true;
    const passed = stalePipeEvicted && nodeAResponsive;
    return {
      scenario: "f9-cascade-node-crash-cleanup",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `F9: crash cleanup — pipeEstablished=${metrics["pipeEstablished"]} ` +
        `preCrashPipes=${metrics["preCrash_nodeA_pipeCount"]} ` +
        `postCrashPipes=${metrics["postCrash_nodeA_pipeCount"]} ` +
        `evicted=${stalePipeEvicted} ` +
        `nodeAResponsive=${nodeAResponsive}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
