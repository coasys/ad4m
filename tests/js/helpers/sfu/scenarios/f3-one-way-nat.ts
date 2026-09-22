/**
 * F3: One-way NAT (asymmetric reachability).
 *
 * Simulates the case where peer A is behind a symmetric NAT that
 * blocks inbound, and peer B is publicly reachable.  In WebRTC terms:
 * A only gets TURN relay candidates; B can use any candidate.  ICE
 * still pairs through the TURN relay.
 *
 * Concretely:
 *   - Alice's `RTCPeerConnection` is created with
 *     `iceTransportPolicy: "relay"` → only relay candidates.
 *   - Bob's is "all" — host/srflx/relay.
 *
 * The expected nominated pair has `selectedLocalCandidateType=relay`
 * on Alice (no other choice) and either `relay` or another type on
 * Bob (depending on what str0m picks).  The point is media flows.
 *
 * Like W5 this needs a TURN_URL.  Skips gracefully without one.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, pairPeers, PeerOptions } from "../peer.js";

const SAMPLE_SEC = 15;

export const f3OneWayNat: Scenario = {
  id: "f3",
  name: "Asymmetric ICE: one peer relay-only",
  description: "Alice relay-only, Bob unrestricted — verifies asymmetric ICE establishment",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const turnUrl = process.env.TURN_URL;
    if (!turnUrl) {
      metrics["skipped"] = true;
      metrics["skip_reason"] = "TURN_URL env var not set";
      return {
        scenario: "f3-one-way-nat",
        branch,
        startTime,
        endTime: Date.now(),
        durationMs: Date.now() - startTime,
        passed: true,
        metrics,
        samples,
        summary: `F3: SKIPPED — TURN_URL unset`,
      };
    }

    const iceServers = [
      {
        urls: turnUrl,
        username: process.env.TURN_USERNAME,
        credential: process.env.TURN_CREDENTIAL,
      } as RTCIceServer,
    ];

    const aliceOpts: PeerOptions = {
      audioToneHz: 440,
      iceServers,
      iceTransportPolicy: "relay",
    };
    const bobOpts: PeerOptions = {
      audioToneHz: 660,
      iceServers,
      iceTransportPolicy: "all",
    };

    const alice = new WebRtcPeer("alice", aliceOpts);
    const bob = new WebRtcPeer("bob", bobOpts);
    await alice.attachSyntheticStream(aliceOpts);
    await bob.attachSyntheticStream(bobOpts);

    try {
      const pairStart = Date.now();
      await pairPeers(alice, bob);
      const sdpMs = Date.now() - pairStart;
      samples.push({ name: "asymmetric_pair", durationMs: sdpMs, timestamp: Date.now() });
      metrics["sdpExchangeMs"] = sdpMs;

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
      metrics["aliceLocalCandidateType"] = a?.selectedLocalCandidateType ?? null;
      metrics["bobLocalCandidateType"] = b?.selectedLocalCandidateType ?? null;
      metrics["aliceRttMs"] = a?.currentRoundTripTimeMs ?? null;
      metrics["bobRttMs"] = b?.currentRoundTripTimeMs ?? null;

      // Alice must be relay-only (her policy forced it).  Bob may use
      // any type; the point is the call worked despite asymmetric
      // reachability.
      metrics["asymmetricEstablished"] =
        a?.selectedLocalCandidateType === "relay" &&
        (a?.bytesSent ?? 0) > 0 &&
        (b?.bytesSent ?? 0) > 0;
    } finally {
      await alice.close().catch(() => {});
      await bob.close().catch(() => {});
    }

    const endTime = Date.now();
    return {
      scenario: "f3-one-way-nat",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(metrics["aliceUploadBytes"] ?? 0) > 0 && Number(metrics["bobUploadBytes"] ?? 0) > 0,
      metrics,
      samples,
      summary:
        `F3: asymmetric ICE established=${metrics["asymmetricEstablished"]} — ` +
        `alice(${metrics["aliceLocalCandidateType"]})up=${metrics["aliceUploadBytes"]}B ` +
        `bob(${metrics["bobLocalCandidateType"]})up=${metrics["bobUploadBytes"]}B`,
    };
  },
};

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
