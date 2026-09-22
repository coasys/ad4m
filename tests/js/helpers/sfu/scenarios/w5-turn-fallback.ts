/**
 * W5: TURN fallback.
 *
 * Sets `iceTransportPolicy: "relay"` so peers MUST establish the call
 * via TURN relay candidates — direct host/srflx candidates are filtered
 * out at the ICE layer.  This is the negative test for "what happens
 * when symmetric NAT blocks both ends": the call should still complete,
 * but every byte goes through the TURN server.
 *
 * Requires a real TURN server.  Set:
 *
 *   TURN_URL=turn:turn.example.com:3478
 *   TURN_USERNAME=...
 *   TURN_CREDENTIAL=...
 *
 * If `TURN_URL` is missing, the scenario reports `skipped=true` with a
 * note rather than failing — the rest of the wind tunnel still runs.
 *
 * Measures:
 *   - Connection time (TURN allocation + ICE check).
 *   - Bandwidth cost (should equal direct mesh, ie. no compression
 *     penalty — but every byte is double-relayed through the TURN
 *     server, so it doubles the *server* bandwidth which is the actual
 *     TURN cost).
 *   - `selected_candidate_pair` type = "relay" / "relay" (verify both
 *     ends are using a relay candidate, not just one).
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, pairPeers, PeerOptions } from "../peer.js";

const SAMPLE_SEC = 15;

export const w5TurnFallback: Scenario = {
  id: "w5",
  name: "TURN fallback (relay-only ICE)",
  description: "Two peers with iceTransportPolicy=relay — verify TURN path works and quantify cost",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const turnUrl = process.env.TURN_URL;
    if (!turnUrl) {
      metrics["skipped"] = true;
      metrics["skip_reason"] =
        "TURN_URL env var not set — set TURN_URL=turn:host:port + TURN_USERNAME + TURN_CREDENTIAL to run.";
      return {
        scenario: "w5-turn-fallback",
        branch,
        startTime,
        endTime: Date.now(),
        durationMs: Date.now() - startTime,
        passed: true,
        metrics,
        samples,
        summary: `W5: SKIPPED — TURN_URL unset`,
      };
    }

    const opts: PeerOptions = {
      audioToneHz: 440,
      iceServers: [
        {
          urls: turnUrl,
          username: process.env.TURN_USERNAME,
          credential: process.env.TURN_CREDENTIAL,
        } as RTCIceServer,
      ],
      iceTransportPolicy: "relay",
    };

    const alice = new WebRtcPeer("alice", opts);
    const bob = new WebRtcPeer("bob", { ...opts, audioToneHz: 660 });
    await alice.attachSyntheticStream(opts);
    await bob.attachSyntheticStream(opts);

    try {
      const pairStart = Date.now();
      await pairPeers(alice, bob);
      const pairElapsed = Date.now() - pairStart;
      samples.push({
        name: "pair_via_turn",
        durationMs: pairElapsed,
        timestamp: Date.now(),
      });
      metrics["sdpExchangeMs"] = pairElapsed;

      await sleep(2000);
      alice.startStats();
      bob.startStats();
      await sleep(SAMPLE_SEC * 1000);
      alice.stopStats();
      bob.stopStats();

      const a = alice.getLastStats();
      const b = bob.getLastStats();
      metrics["aliceUploadBytes"] = a?.bytesSent ?? 0;
      metrics["aliceDownloadBytes"] = a?.bytesReceived ?? 0;
      metrics["bobUploadBytes"] = b?.bytesSent ?? 0;
      metrics["bobDownloadBytes"] = b?.bytesReceived ?? 0;
      metrics["aliceSelectedPair"] = a?.selectedCandidatePair ?? null;
      metrics["bobSelectedPair"] = b?.selectedCandidatePair ?? null;
      metrics["aliceLocalCandidateType"] = a?.selectedLocalCandidateType ?? null;
      metrics["aliceRemoteCandidateType"] = a?.selectedRemoteCandidateType ?? null;
      metrics["bobLocalCandidateType"] = b?.selectedLocalCandidateType ?? null;
      metrics["bobRemoteCandidateType"] = b?.selectedRemoteCandidateType ?? null;
      metrics["aliceRttMs"] = a?.currentRoundTripTimeMs ?? null;
      metrics["bobRttMs"] = b?.currentRoundTripTimeMs ?? null;

      // Both ends MUST use a relay candidate locally — that's the proof
      // ICE went through TURN rather than picking up a leaked host
      // candidate.  Remote-side type is whatever the other end advertised
      // (also relay if both sides set iceTransportPolicy=relay).
      metrics["allPairsRelay"] =
        a?.selectedLocalCandidateType === "relay" &&
        b?.selectedLocalCandidateType === "relay";
    } finally {
      await alice.close().catch(() => {});
      await bob.close().catch(() => {});
    }

    const endTime = Date.now();
    return {
      scenario: "w5-turn-fallback",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(metrics["aliceDownloadBytes"] ?? 0) > 0 && Number(metrics["bobDownloadBytes"] ?? 0) > 0,
      metrics,
      samples,
      summary:
        `W5: TURN ${metrics["allPairsRelay"] ? "OK" : "WARN"} — sdp=${metrics["sdpExchangeMs"]}ms ` +
        `aliceUp=${metrics["aliceUploadBytes"]}B bobUp=${metrics["bobUploadBytes"]}B ` +
        `rtt=${metrics["aliceRttMs"]}/${metrics["bobRttMs"]}ms`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
