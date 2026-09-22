/**
 * T6: cross-node pipe transport handshake + media (Phases E + F).
 *
 * Boots a 2-node SFU cluster with TCP gossip, joins one peer on each
 * node for the same room, and verifies:
 *
 *   1. Both nodes see each other in `sfu.cascadeStatus.pipes` — i.e.
 *      the gossip-driven `PipeOffer` / `PipeAnswer` handshake actually
 *      ran end-to-end (Phase E).
 *   2. Each peer subscribes to the server-pushed renegotiation events
 *      so its RTCPeerConnection picks up the cross-node track once the
 *      pipe carries it.
 *   3. After 12 s of steady-state media, at least one peer reports
 *      `bytesReceived > 0` — proof that bytes peer-A→SFU-1→pipe→
 *      SFU-2→peer-B reach the destination (Phase F).
 *
 * The two nodes share a single physical machine via loopback TCP
 * gossip; the executor's `--sfu-local-did` flag distinguishes them.
 * Tie-break: the lexically-higher DID is the dialer, so we explicitly
 * use `did:windtunnel:cascade:node-0` / `did:windtunnel:cascade:node-1`
 * so node-1 drives establish_pipe.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { startCluster } from "../cascade.js";
import { provisionClusterPeers, disconnectClusterPeers, registerClusterSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t6-pipe-handshake";
const NEIGHBOURHOOD = `windtunnel://t6`;
const WS_BASE = 16200;
const GOSSIP_BASE = 26200;
const MAX_PER_NODE = 4;

interface CascadeStatusPipe {
  roomId: string;
  remoteDid: string;
}
interface CascadeStatus {
  establishedCount: number;
  pipes: CascadeStatusPipe[];
}

export const t6PipeHandshake: Scenario = {
  id: "t6",
  name: "SFU cascade — pipe transport handshake",
  description: "2 SFU nodes auto-establish a TCP-gossip-driven pipe transport when both serve the same room",

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

    try {
      // Open the room on both nodes — start_room is per-node, so each
      // node needs to know about the room locally before peers arrive.
      await Promise.all(
        cluster.nodes.map((n) =>
          n.client.call("sfu.startRoom", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          }),
        ),
      );

      // Provision two distinct users — one per node — and have each
      // user join their respective node's room.  After both joins, the
      // announce broadcasts cross the gossip wire and the
      // higher-DID side calls establish_pipe.
      const sessions = await provisionClusterPeers({
        nodes: cluster.nodes.map((n) => ({
          nodeId: n.id,
          admin: n.client,
          port: n.port,
        })),
        count: 2,
        labelPrefix: "t6-peer",
      });
      await registerClusterSfuMembers({
        nodes: cluster.nodes.map((n) => ({ nodeId: n.id, admin: n.client })),
        neighbourhoodUrl: NEIGHBOURHOOD,
        sessions,
      });

      const peers: WebRtcPeer[] = [];
      const wires: RenegotiationWire[] = [];
      for (let i = 0; i < cluster.nodes.length; i++) {
        const node = cluster.nodes[i];
        const session = sessions[i];
        const entry = session.byNode.get(node.id);
        if (!entry) {
          throw new Error(`T6: missing cluster auth for node ${node.id}`);
        }
        const peer = new WebRtcPeer(session.label, { audioToneHz: 440 + i * 80 });
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
            `T6 unexpected cascade redirect to ${joinResp.redirectTo} on join ${i}`,
          );
        }
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));
      }

      // Poll cascadeStatus on each node — the gossip-driven handshake
      // happens after both nodes' first Announce hits the cluster, but
      // that round-trip is a few hundred ms.  Give it up to 10s.
      const pipeWaitStart = Date.now();
      const TIMEOUT_MS = 10_000;
      let perNodePipeCounts: number[] = [];
      while (Date.now() - pipeWaitStart < TIMEOUT_MS) {
        perNodePipeCounts = await Promise.all(
          cluster.nodes.map((n) =>
            n.client
              .call<CascadeStatus>("sfu.cascadeStatus", {})
              .then((s) => s.establishedCount)
              .catch(() => 0),
          ),
        );
        if (perNodePipeCounts.every((c) => c >= 1)) break;
        await sleep(250);
      }
      const pipeWaitMs = Date.now() - pipeWaitStart;
      samples.push({
        name: "pipe_handshake_wait_ms",
        durationMs: pipeWaitMs,
        timestamp: Date.now(),
      });

      // Collect the final detailed status from each node so we can
      // assert who-points-at-whom.
      const finalStatuses: CascadeStatus[] = await Promise.all(
        cluster.nodes.map((n) =>
          n.client.call<CascadeStatus>("sfu.cascadeStatus", {}),
        ),
      );
      metrics["perNodePipeCounts"] = perNodePipeCounts;
      metrics["pipeWaitMs"] = pipeWaitMs;
      metrics["perNodePipes"] = finalStatuses.map((s, i) => ({
        nodeId: cluster.nodes[i].id,
        nodeDid: cluster.nodes[i].did,
        establishedCount: s.establishedCount,
        pipes: s.pipes,
      }));

      const handshakeSucceeded = perNodePipeCounts.every((c) => c >= 1);
      metrics["handshakeSucceeded"] = handshakeSucceeded;
      // Cross-node correctness: each node's pipe should reference the
      // OTHER node's DID.
      const linksValid = finalStatuses.every((status, i) => {
        const otherDid = cluster.nodes[1 - i].did;
        return status.pipes.some((p) => p.remoteDid === otherDid);
      });
      metrics["linksValid"] = linksValid;

      // Phase F: now that the pipe is up and each peer is subscribed
      // to the renegotiation channel, let media flow for a window and
      // measure cross-node bandwidth.  Each peer should see bytes from
      // the OTHER peer arrive via the SFU→pipe→SFU path.
      await sleep(2000);
      peers.forEach((p) => p.startStats());
      await sleep(12_000);
      peers.forEach((p) => p.stopStats());
      const uploads = peers.map((p) => p.getLastStats()?.bytesSent ?? 0);
      const downloads = peers.map((p) => p.getLastStats()?.bytesReceived ?? 0);
      metrics["uploadBytesPerPeer"] = uploads;
      metrics["downloadBytesPerPeer"] = downloads;
      metrics["uploadMean"] = mean(uploads);
      metrics["downloadMean"] = mean(downloads);
      metrics["renegotiationsAppliedPerPeer"] = wires.map((w) => w.count());
      // Every peer must receive media — .every(), not .some().
      const mediaAcrossPipeOk = downloads.every((b) => b > 0);
      metrics["mediaAcrossPipeOk"] = mediaAcrossPipeOk;

      // Hard pass/fail: handshake established, links point at the correct
      // remote node, and media actually crossed the pipe for ALL peers.
      const passed = handshakeSucceeded && linksValid && mediaAcrossPipeOk;
      metrics["passed"] = passed;

      // Tear peers down (best-effort).
      for (const w of wires) {
        try { await w.detach(); } catch {}
      }
      for (let i = 0; i < peers.length; i++) {
        const node = cluster.nodes[i];
        const session = sessions[i];
        const entry = session.byNode.get(node.id);
        try {
          await entry?.client.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {}
        try {
          await peers[i].close();
        } catch {}
      }
      await disconnectClusterPeers(sessions);

      const endTime = Date.now();
      return {
        scenario: "t6-pipe-handshake",
        branch,
        passed,
        startTime,
        endTime,
        durationMs: endTime - startTime,
        metrics,
        samples,
        summary:
          `T6: cascade — pipes=${JSON.stringify(perNodePipeCounts)} ` +
          `linksValid=${linksValid} uploadMean=${metrics["uploadMean"]}B ` +
          `downloadMean=${metrics["downloadMean"]}B mediaAcrossPipe=${mediaAcrossPipeOk} ` +
          `waitMs=${pipeWaitMs}`,
      };
    } finally {
      try {
        await cluster.shutdown();
      } catch {}
    }
  },
};

function mean(arr: number[]): number {
  return arr.length ? Math.round(arr.reduce((a, b) => a + b, 0) / arr.length) : 0;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
