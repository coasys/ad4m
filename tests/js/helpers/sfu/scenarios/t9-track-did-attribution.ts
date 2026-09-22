/**
 * T9: Track-to-DID attribution fidelity.
 *
 * Verifies that the SFU correctly attributes received tracks to the
 * DID that sent them.  Three peers join with distinct synthetic audio
 * tones (440 Hz, 520 Hz, 600 Hz).  After renegotiation settles, the
 * scenario examines each peer's `streamMapping` from `callJoin` to
 * confirm the attributed DIDs match the expected senders.
 *
 * Because peers join sequentially, each peer's `streamMapping` at
 * join time should reference every peer that joined before it:
 *   - Peer 0 (first to join): empty mapping — no other tracks yet.
 *   - Peer 1: mapping references peer 0's DID.
 *   - Peer 2: mapping references peers 0 and 1's DIDs.
 *
 * After all joins, renegotiation pushes should deliver the remaining
 * attributions (e.g. peer 0 learns about peers 1 and 2).
 *
 * Assertion: no misattributions detected — every DID found in a
 * peer's mapping belongs to a different peer, and no peer references
 * its own DID.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t9-track-attribution";
const NEIGHBOURHOOD = `windtunnel://t9`;
const TONES = [440, 520, 600];
const PEER_COUNT = TONES.length;

interface AttributionCheck {
  peerIdx: number;
  peerDid: string;
  mapping: string[];
  referencedDids: string[];
  expectedDids: string[];
  missingDids: string[];
  selfReferenced: boolean;
  correct: boolean;
}

export const t9TrackDidAttribution: Scenario = {
  id: "t9",
  name: "Track-to-DID attribution fidelity",
  description:
    "3 peers with distinct tones — verifies each received track's " +
    "stream mapping references the correct sender DID",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    await admin.call("sfu.startRoom", {
      neighbourhoodUrl: NEIGHBOURHOOD,
      roomName: ROOM_NAME,
    });

    const sessions = await provisionPeers({
      admin,
      port,
      count: PEER_COUNT,
      labelPrefix: "t9-peer",
    });

    await registerSfuMembers({
      admin,
      neighbourhoodUrl: NEIGHBOURHOOD,
      sessions,
    });

    const knownDids = sessions.map((s) => s.did);
    metrics["knownDids"] = knownDids;

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    const streamMappings: Array<{ peerIdx: number; mapping: string[] }> = [];
    let passed = false;

    try {
      // Join each peer sequentially so the renegotiation pipeline
      // processes them one at a time — clean baseline for attribution.
      for (let i = 0; i < PEER_COUNT; i++) {
        const session = sessions[i];
        const peer = new WebRtcPeer(session.label, {
          audioToneHz: TONES[i],
        });
        await peer.attachSyntheticStream();
        peers.push(peer);

        const wire = await wireRenegotiation({
          client: session.client,
          peer,
          token: session.token,
          port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
        wires.push(wire);

        const offer = await peer.createOffer();
        const joinResp = await session.client.call<{
          sdpAnswer: string;
          participantId: string;
          redirectTo?: string;
          streamMapping: string[];
        }>("sfu.callJoin", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));
        streamMappings.push({
          peerIdx: i,
          mapping: joinResp.streamMapping,
        });

        // Small gap between joins so the renegotiation push from this
        // join reaches the already-joined peers before the next one.
        await sleep(200);
      }

      // Wait for renegotiation to settle — each peer should learn about
      // all others.
      const settleStart = Date.now();
      const expectedPerPeer = PEER_COUNT - 1;
      while (Date.now() - settleStart < 15_000) {
        if (wires.every((w) => w.count() >= expectedPerPeer)) break;
        await sleep(500);
      }
      const settleMs = Date.now() - settleStart;
      samples.push({
        name: "renegotiation_settle",
        durationMs: settleMs,
        timestamp: Date.now(),
      });
      metrics["renegotiationsPerPeer"] = wires.map((w) => w.count());

      // Examine stream mappings for DID references.
      //
      // At join time, peer i's mapping should reference the DIDs of all
      // peers 0..i-1 (peers already in the room).  We check:
      //   1. Whether expected DIDs appear in the mapping.
      //   2. Whether the peer's OWN DID appears (a self-reference bug).
      const attributionResults: AttributionCheck[] = [];

      for (let i = 0; i < PEER_COUNT; i++) {
        const peerDid = knownDids[i];
        const mapping = streamMappings[i].mapping;
        const mappingStr = JSON.stringify(mapping);

        // DIDs already in the room when peer i joined.
        const expectedDids = knownDids.slice(0, i);

        const referencedDids = knownDids.filter((did) =>
          mappingStr.includes(did),
        );
        const missingDids = expectedDids.filter(
          (did) => !mappingStr.includes(did),
        );
        const selfReferenced = mappingStr.includes(peerDid);

        attributionResults.push({
          peerIdx: i,
          peerDid,
          mapping,
          referencedDids,
          expectedDids,
          missingDids,
          selfReferenced,
          correct: missingDids.length === 0 && !selfReferenced,
        });
      }

      metrics["attributionResults"] = attributionResults;
      metrics["allAttributionsCorrect"] = attributionResults.every(
        (r) => r.correct,
      );
      const misattributions = attributionResults.filter((r) => !r.correct);
      metrics["misattributionCount"] = misattributions.length;
      metrics["misattributionDetails"] = misattributions;
      passed =
        (metrics["allAttributionsCorrect"] as boolean) === true &&
        misattributions.length === 0;
    } finally {
      for (const w of wires) {
        try {
          await w.detach();
        } catch {
          /* best-effort */
        }
      }
      for (let i = 0; i < peers.length; i++) {
        try {
          await sessions[i].client.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {
          /* best-effort */
        }
        try {
          await peers[i].close();
        } catch {
          /* best-effort */
        }
      }
      try {
        await admin.call("sfu.stopRoom", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
      } catch {
        /* best-effort */
      }
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "t9-track-did-attribution",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T9: track attribution — peers=${PEER_COUNT} ` +
        `allCorrect=${metrics["allAttributionsCorrect"]} ` +
        `misattributions=${metrics["misattributionCount"]}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
