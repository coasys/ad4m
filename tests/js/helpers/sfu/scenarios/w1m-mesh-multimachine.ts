/**
 * W1M: Mesh 2-peer call across two physical machines.
 *
 * Like W1 but one of the peers lives on a remote box, driven via the
 * `peer-server.ts` HTTP harness.  Exercises the real network path:
 * SDP + ICE flow over HTTP between the runner and the remote, then
 * media flows directly peer-to-peer.
 *
 * The runner is the LOCAL caller (Alice).  The remote is the callee
 * (Bob).  Set the remote peer-server URL via:
 *   AD4M_REMOTE_PEER_URL=http://<remote-host>:<port>
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer, PeerStats } from "../peer.js";

const DEFAULT_REMOTE = process.env.AD4M_REMOTE_PEER_URL ?? "";

export const w1mMeshMultiMachine: Scenario = {
  id: "w1m",
  name: "Mesh 2-peer across two machines",
  description: "Mesh 2-peer call with peers on two physical machines via peer-server",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const remoteUrl = process.env.AD4M_REMOTE_PEER_URL ?? DEFAULT_REMOTE;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = { remoteUrl };

    if (!remoteUrl) {
      return {
        scenario: "w1m-mesh-multimachine",
        branch,
        startTime,
        endTime: Date.now(),
        durationMs: Date.now() - startTime,
        passed: true,
        metrics: {
          skipped: true,
          skip_reason: "AD4M_REMOTE_PEER_URL not set — point at a peer-server",
        },
        samples,
        summary: "W1M: SKIPPED — AD4M_REMOTE_PEER_URL unset",
      };
    }

    let local: WebRtcPeer | null = null;
    try {
      // Start the remote peer.
      await rpc(remoteUrl, "POST", "/peer/start", { audioToneHz: 660 });

      // Start the local peer.
      local = new WebRtcPeer("alice", { audioToneHz: 440 });
      await local.attachSyntheticStream();

      // Wire local ICE candidates → remote /peer/iceCandidate as they arrive.
      local.peerConnection().addEventListener("icecandidate", (ev: any) => {
        if (ev.candidate) {
          rpc(remoteUrl, "POST", "/peer/iceCandidate", {
            candidate: ev.candidate,
          }).catch((e) => console.warn("local→remote ICE push failed:", e));
        }
      });

      // Poll the remote's ICE candidates and feed them to the local peer.
      // 5 Hz for the first 10s, then back off.
      let iceStop = false;
      const icePoller = (async () => {
        while (!iceStop) {
          try {
            const { candidates } = await rpc<{ candidates: RTCIceCandidateInit[] }>(
              remoteUrl,
              "GET",
              "/peer/iceCandidates",
            );
            for (const c of candidates ?? []) {
              if (!local) break;
              await local.peerConnection().addIceCandidate(c).catch(() => {});
            }
          } catch (_e) {
            // ignore — keep polling
          }
          await sleep(200);
        }
      })();

      // SDP exchange.
      const sdpStart = Date.now();
      const offer = await local.createOffer();
      const { answer } = await rpc<{ answer: RTCSessionDescriptionInit }>(
        remoteUrl,
        "POST",
        "/peer/offer",
        { offer },
      );
      await local.acceptAnswer(answer);
      const sdpDone = Date.now();
      samples.push({
        name: "sdp_exchange",
        durationMs: sdpDone - sdpStart,
        timestamp: sdpDone,
      });

      // Start stats sampling on both sides.
      const localStats: PeerStats[] = [];
      local.on("stats", (s: PeerStats) => localStats.push(s));
      local.startStats();
      await rpc(remoteUrl, "POST", "/peer/startStats", {});

      // Hold the call for 30s, polling remote stats once a second.
      const remoteStatsHistory: any[] = [];
      const remoteStatsTimer = setInterval(async () => {
        try {
          const { stats } = await rpc<{ stats: any }>(remoteUrl, "GET", "/peer/stats");
          remoteStatsHistory.push({ at: Date.now(), stats });
        } catch (_e) {
          /* swallow */
        }
      }, 1000);
      await sleep(30_000);
      clearInterval(remoteStatsTimer);
      iceStop = true;
      await icePoller.catch(() => {});

      local.stopStats();

      // Roll up.
      metrics["localStatsSamples"] = localStats.length;
      metrics["remoteStatsSamples"] = remoteStatsHistory.length;
      if (localStats.length > 0) {
        const last = localStats[localStats.length - 1];
        metrics["localBytesSent"] = last.bytesSent;
        metrics["localBytesReceived"] = last.bytesReceived;
        metrics["localPacketsLost"] = last.packetsLost;
        metrics["localRoundTripMs"] = last.currentRoundTripTimeMs;
      }
      const lastRemote = remoteStatsHistory[remoteStatsHistory.length - 1]?.stats;
      if (lastRemote) {
        metrics["remoteBytesSent"] = lastRemote.bytesSent;
        metrics["remoteBytesReceived"] = lastRemote.bytesReceived;
        metrics["remotePacketsLost"] = lastRemote.packetsLost;
        metrics["remoteRoundTripMs"] = lastRemote.currentRoundTripTimeMs;
      }
    } finally {
      if (local) await local.close();
      try {
        await rpc(remoteUrl, "POST", "/peer/close", {});
      } catch (_e) {
        /* swallow */
      }
    }

    const endTime = Date.now();
    return {
      scenario: "w1m-mesh-multimachine",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(metrics["localBytesReceived"] ?? 0) > 0 && Number(metrics["remoteBytesReceived"] ?? 0) > 0,
      metrics,
      samples,
      summary:
        `W1M: cross-machine mesh — local sent=${metrics["localBytesSent"]}B received=${metrics["localBytesReceived"]}B ` +
        `remote sent=${metrics["remoteBytesSent"]}B received=${metrics["remoteBytesReceived"]}B ` +
        `rtt local=${metrics["localRoundTripMs"]}ms remote=${metrics["remoteRoundTripMs"]}ms`,
    };
  },
};

async function rpc<T = any>(
  baseUrl: string,
  method: "GET" | "POST",
  path: string,
  body?: unknown,
): Promise<T> {
  const res = await fetch(baseUrl + path, {
    method,
    headers: method === "POST" ? { "content-type": "application/json" } : undefined,
    body: method === "POST" ? JSON.stringify(body ?? {}) : undefined,
  });
  if (!res.ok) {
    const text = await res.text().catch(() => "");
    throw new Error(`peer-server ${method} ${path} → ${res.status}: ${text}`);
  }
  return (await res.json()) as T;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
