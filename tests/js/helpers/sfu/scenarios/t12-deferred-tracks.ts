/**
 * T12: Deferred tracks — rapid concurrent join.
 *
 * Exercises the SFU server's `deferred_tracks` path: when a peer joins
 * while another peer has a `pending_offer`, track additions get queued
 * in `deferred_tracks` and drain after the pending offer clears.
 *
 * The scenario joins 5 peers in a simultaneous burst (like T8) and then
 * verifies — via audio frequency fingerprinting — that EVERY peer
 * eventually receives media from EVERY other peer.  If deferred tracks
 * got permanently stuck, at least one peer would fail to detect one or
 * more expected tones.
 *
 * Asserts:
 *   - All peers join successfully.
 *   - Every peer detects tones from every other peer (N-1 distinct
 *     frequencies per peer).
 *   - No tracks got permanently lost to the deferred queue.
 *
 * Pre-requisite: executor running with `--enable-multi-user`.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, DetectedTone } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "t12-deferred-tracks";
const NEIGHBOURHOOD = `windtunnel://t12`;
const PEER_COUNT = 5;
/** Generous media-flow window so Goertzel accumulates enough samples. */
const MEDIA_FLOW_MS = 15_000;
/** Renegotiation settle timeout after the concurrent burst. */
const SETTLE_TIMEOUT_MS = 20_000;

export const t12DeferredTracks: Scenario = {
  id: "t12",
  name: "Deferred tracks drain after concurrent join",
  description:
    "5 peers join simultaneously — verifies deferred_tracks drain " +
    "correctly via tone detection (every peer hears every other peer)",

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
      labelPrefix: "t12-peer",
    });

    await registerSfuMembers({
      admin,
      neighbourhoodUrl: NEIGHBOURHOOD,
      sessions,
    });

    // Widely-spaced tones for clean Goertzel separation.
    const TONES = sessions.map((_, i) => 400 + i * 80);

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    let passed = false;

    try {
      // Phase 1: prepare all peers — streams, renegotiation wires, offers.
      // Sequential preparation so every event subscription exists before
      // the concurrent burst.
      const offers: Array<{ type: string; sdp?: string }> = [];
      for (let i = 0; i < sessions.length; i++) {
        const session = sessions[i];
        const peer = new WebRtcPeer(session.label, {
          audioToneHz: TONES[i],
        });
        peer.enableAudioFingerprinting(TONES);
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
        offers.push(offer);
      }

      // Phase 2: fire ALL callJoin requests simultaneously — this
      // maximises the chance that the SFU must defer tracks.
      const joinStart = Date.now();
      const joinResults = await Promise.all(
        sessions.map((session, i) =>
          session.client
            .call<{
              sdpAnswer: string;
              participantId: string;
              redirectTo?: string;
              streamMapping: string[];
            }>("sfu.callJoin", {
              neighbourhoodUrl: NEIGHBOURHOOD,
              roomName: ROOM_NAME,
              sdpOffer: JSON.stringify(offers[i]),
            })
            .then((resp) => ({
              idx: i,
              resp,
              error: null as string | null,
            }))
            .catch((e) => ({
              idx: i,
              resp: null as any,
              error: e instanceof Error ? e.message : String(e),
            })),
        ),
      );
      const joinElapsed = Date.now() - joinStart;
      samples.push({
        name: "concurrent_join_burst",
        durationMs: joinElapsed,
        timestamp: Date.now(),
      });

      // Accept answers for successful joins.
      const joinErrors: string[] = [];
      let successfulJoins = 0;
      for (const result of joinResults) {
        if (result.error) {
          joinErrors.push(`peer-${result.idx}: ${result.error}`);
          continue;
        }
        await peers[result.idx].acceptAnswer(
          JSON.parse(result.resp.sdpAnswer),
        );
        successfulJoins++;
      }
      metrics["joinErrors"] = joinErrors;
      metrics["successfulJoins"] = successfulJoins;

      // Phase 3: wait for renegotiation to settle.
      const expectedRenegotiations =
        (successfulJoins * (successfulJoins - 1)) / 2;
      const settleStart = Date.now();
      while (Date.now() - settleStart < SETTLE_TIMEOUT_MS) {
        const total = wires.reduce((sum, w) => sum + w.count(), 0);
        if (total >= expectedRenegotiations) break;
        await sleep(500);
      }
      const settleElapsed = Date.now() - settleStart;
      samples.push({
        name: "renegotiation_settle",
        durationMs: settleElapsed,
        timestamp: Date.now(),
      });
      metrics["renegotiationsPerPeer"] = wires.map((w) => w.count());
      metrics["totalRenegotiations"] = wires.reduce(
        (sum, w) => sum + w.count(),
        0,
      );

      // Phase 4: let media flow long enough for Goertzel detection.
      await sleep(MEDIA_FLOW_MS);

      // Phase 5: verify tone detection — the core deferred-tracks assertion.
      // Every peer must detect tones from every OTHER peer.
      const toneResults: Array<{
        peer: string;
        detected: DetectedTone[];
        expectedHz: number[];
        matchedHz: number[];
        missingHz: number[];
      }> = [];
      let allPeersDetectedAllOthers = true;

      for (let i = 0; i < peers.length; i++) {
        const detected = peers[i].getDetectedTones();
        const otherTones = TONES.filter((_, j) => j !== i);
        // Filter out 0 Hz (silence/self-track) from detection — Goertzel
        // returns 0 for tracks with no energy at any candidate frequency.
        const detectedHz = new Set(
          detected.map((d) => d.dominantHz).filter((hz) => hz !== 0),
        );
        const matchedHz = otherTones.filter((hz) => detectedHz.has(hz));
        const missingHz = otherTones.filter((hz) => !detectedHz.has(hz));

        toneResults.push({
          peer: sessions[i].label,
          detected,
          expectedHz: otherTones,
          matchedHz,
          missingHz,
        });

        if (missingHz.length > 0) {
          allPeersDetectedAllOthers = false;
        }
      }

      metrics["toneDetection"] = toneResults;
      metrics["allPeersDetectedAllOthers"] = allPeersDetectedAllOthers;

      passed =
        successfulJoins === PEER_COUNT && allPeersDetectedAllOthers;
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
      scenario: "t12-deferred-tracks",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T12: deferred tracks — joined=${metrics["successfulJoins"]}/${PEER_COUNT} ` +
        `allDetected=${metrics["allPeersDetectedAllOthers"]} ` +
        `renegotiations=${metrics["totalRenegotiations"]}`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
