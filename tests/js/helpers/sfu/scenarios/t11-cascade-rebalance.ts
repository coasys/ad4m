/**
 * T11: Cascade rebalance migration.
 *
 * Verifies the server-side rebalancing mechanism:
 *
 *   1. Starts a 2-node cascade cluster (capacity 4 per node).
 *   2. Joins 4 peers on node 0 — filling it to 100% (above the
 *      90% rebalance threshold).
 *   3. Node 1 stays empty (gap = 4, meeting the minimum-gap-of-4
 *      anti-pingpong threshold).
 *   4. Subscribes to `sfu-migrate` events on the events WS for
 *      every peer on node 0.
 *   5. Waits for the rebalance sweep (runs every 5s in the cascade
 *      cleanup loop) to fire and publish a migrate event.
 *   6. Asserts:
 *      - A migrate event arrived within 15s.
 *      - The event targets one of the participants on node 0.
 *      - The `migrateToDid` points to node 1's DID.
 *
 * This tests the full server-side pipeline: suggest_rebalance →
 * SfuMigrateEvent → pubsub → events_ws → client delivery. The
 * client-side rejoin (leave + join on target node) uses the same
 * path as cascade failover, already covered by M3/T7.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { startCluster } from "../cascade.js";
import {
  provisionClusterPeers,
  disconnectClusterPeers,
  ClusterPeerSession,
  registerClusterSfuMembers,
} from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";
import { EventsClient, EventFrame } from "../events.js";

const ROOM_NAME = "t11-rebalance";
const NEIGHBOURHOOD = `windtunnel://t11`;
const WS_BASE = 16300;
const GOSSIP_BASE = 26300;
const MAX_PER_NODE = 4;
const PEER_COUNT = 4;
const MIGRATE_TIMEOUT_MS = 15_000;

interface MigrateEvent {
  targetDid: string;
  neighbourhoodUrl: string;
  roomName: string;
  migrateToDid: string;
}

export const t11CascadeRebalance: Scenario = {
  id: "t11",
  name: "SFU cascade — rebalance migration",
  description:
    "2 SFU nodes, 4 peers on node 0, node 1 empty — rebalance " +
    "sweep emits sfu-migrate event directing a participant to node 1",

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

    let sessions: ClusterPeerSession[] = [];
    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    const eventsClients: EventsClient[] = [];

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

      // Provision peers across the cluster.
      sessions = await provisionClusterPeers({
        nodes: cluster.nodes.map((n) => ({
          nodeId: n.id,
          admin: n.client,
          port: n.port,
        })),
        count: PEER_COUNT,
        labelPrefix: "t11-peer",
      });

      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.id, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions,
      });

      // Join ALL peers on node 0 — forces an imbalance.
      const node0 = cluster.nodes[0];
      for (let i = 0; i < PEER_COUNT; i++) {
        const session = sessions[i];
        const entry = session.byNode.get(node0.id);
        if (!entry) {
          throw new Error(`T11: missing auth for peer ${i} on node ${node0.id}`);
        }

        const peer = new WebRtcPeer(session.label, {
          audioToneHz: 440 + i * 40,
        });
        await peer.attachSyntheticStream();
        peers.push(peer);

        const wire = await wireRenegotiation({
          client: entry.client,
          peer,
          token: entry.token,
          port: node0.port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
        wires.push(wire);

        const offer = await peer.createOffer();
        const joinResult = await entry.client.call<{
          participantId: string;
          sdpAnswer: string;
          redirectTo?: string;
          streamMapping?: string[];
        }>("sfu.callJoin", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });

        if (joinResult.redirectTo) {
          // Should NOT redirect — we want all peers on node 0.
          throw new Error(
            `T11: unexpected redirect for peer ${i} to ${joinResult.redirectTo}`,
          );
        }

        const answer = JSON.parse(joinResult.sdpAnswer);
        await peer.acceptAnswer(answer);
      }

      metrics["peersJoinedOnNode0"] = PEER_COUNT;

      // Subscribe to sfu-migrate events for each peer on node 0.
      // The rebalancer publishes to pubsub with targetDid filtering;
      // the events WS forwards only to the matching client.
      const migrateReceived: MigrateEvent[] = [];
      const migratePromise = new Promise<MigrateEvent>((resolve) => {
        for (let i = 0; i < PEER_COUNT; i++) {
          const session = sessions[i];
          const entry = session.byNode.get(node0.id);
          if (!entry) continue;

          const evts = new EventsClient({
            port: node0.port,
            token: entry.token,
          });
          eventsClients.push(evts);

          // Connect and subscribe asynchronously
          evts.connect().then(() => {
            evts.on("sfu-migrate", (frame: EventFrame) => {
              const evt: MigrateEvent = {
                targetDid: frame.targetDid as string,
                neighbourhoodUrl: frame.neighbourhoodUrl as string,
                roomName: frame.roomName as string,
                migrateToDid: frame.migrateToDid as string,
              };
              migrateReceived.push(evt);
              resolve(evt);
            });
          });
        }
      });

      // Wait for a migrate event or timeout.
      const migrateResult = await Promise.race([
        migratePromise,
        new Promise<null>((resolve) =>
          setTimeout(() => resolve(null), MIGRATE_TIMEOUT_MS),
        ),
      ]);

      const migrateLatencyMs = Date.now() - startTime;
      metrics["migrateLatencyMs"] = migrateLatencyMs;

      if (!migrateResult) {
        const endTime = Date.now();
        return {
          scenario: "t11",
          branch,
          passed: false,
          startTime,
          endTime,
          durationMs: endTime - startTime,
          summary: `T11: no sfu-migrate event within ${MIGRATE_TIMEOUT_MS}ms`,
          metrics,
          samples,
        };
      }

      // Validate the migrate event.
      const node1Did = cluster.nodes[1].did;
      const correctTarget = migrateResult.migrateToDid === node1Did;
      const correctRoom = migrateResult.roomName === ROOM_NAME;
      const correctNeighbourhood =
        migrateResult.neighbourhoodUrl === NEIGHBOURHOOD;

      metrics["migrateEvent"] = migrateResult;
      metrics["targetNodeCorrect"] = correctTarget;
      metrics["roomCorrect"] = correctRoom;
      metrics["neighbourhoodCorrect"] = correctNeighbourhood;

      const allCorrect = correctTarget && correctRoom && correctNeighbourhood;

      const endTime = Date.now();
      return {
        scenario: "t11",
        branch,
        passed: allCorrect,
        startTime,
        endTime,
        durationMs: endTime - startTime,
        summary: allCorrect
          ? `T11: rebalance migrate event received in ${migrateLatencyMs}ms, ` +
            `target=${migrateResult.targetDid.slice(-8)}, ` +
            `migrateTo=${node1Did.slice(-8)}`
          : `T11: migrate event incorrect — ` +
            `targetCorrect=${correctTarget}, roomCorrect=${correctRoom}`,
        metrics,
        samples,
      };
    } finally {
      // Teardown
      for (const wire of wires) {
        try {
          await wire.detach();
        } catch {
          /* swallow */
        }
      }
      for (const evts of eventsClients) {
        try {
          await evts.disconnect();
        } catch {
          /* swallow */
        }
      }
      for (const peer of peers) {
        try {
          peer.close();
        } catch {
          /* swallow */
        }
      }
      if (sessions.length > 0) {
        await disconnectClusterPeers(sessions);
      }
      await cluster.shutdown();
    }
  },
};
