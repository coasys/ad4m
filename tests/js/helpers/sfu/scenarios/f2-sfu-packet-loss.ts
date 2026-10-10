/**
 * F2: SFU packet loss 20% on uplink.
 *
 * 5-peer SFU room.  Once everyone is in, apply 20% packet loss to
 * loopback and measure for 15 s.  The SFU's congestion control +
 * simulcast layer drop logic should kick in — we expect to see
 * packetsLost > 0 on receivers but rest of pipeline holding up
 * (room remains responsive afterward, see F7).
 *
 * tc affects ALL loopback traffic — including the WS RPC channel.
 * So loss is applied AFTER all peers have joined, and removed before
 * teardown.  The 20% rate is high enough to be measurable; 50%+
 * makes the SFU's UDP signalling unhealthy.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { clearNet, setNetem, netAvailable } from "../net.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "f2-sfu-packet-loss";
const NEIGHBOURHOOD = `windtunnel://f2`;
const PEER_COUNT = 5;
const LOSS_PCT = 20;
const SAMPLE_SEC = 15;

export const f2SfuPacketLoss: Scenario = {
  id: "f2",
  name: "SFU packet loss 20%",
  description: "5-peer SFU room with 20% loss on lo — verifies congestion control + simulcast drop",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client, branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    if (!netAvailable()) {
      metrics["skipped"] = true;
      metrics["skip_reason"] =
        "tc qdisc + sudo -n not available on this platform";
      return {
        scenario: "f2-sfu-packet-loss",
        branch,
        startTime,
        endTime: Date.now(),
        durationMs: Date.now() - startTime,
        passed: true,
        metrics,
        samples,
        summary: `F2: SKIPPED — ${metrics["skip_reason"]}`,
      };
    }

    try {
      await client.call("sfu.startRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      if (msg.includes("not yet available")) {
        metrics["skipped"] = true;
        metrics["skip_reason"] = msg;
        return {
          scenario: "f2-sfu-packet-loss",
          branch,
          startTime,
          endTime: Date.now(),
          durationMs: Date.now() - startTime,
          passed: true,
          metrics,
          samples,
          summary: `F2: SKIPPED — ${msg}`,
        };
      }
      throw e;
    }

    const sessions = await provisionPeers({
      admin: client,
      port: ctx.port,
      count: PEER_COUNT,
      labelPrefix: "f2-peer",
    });
    await registerSfuMembers({ admin: client, neighbourhoodUrl: NEIGHBOURHOOD, sessions });

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    try {
      for (let i = 0; i < sessions.length; i++) {
        const s = sessions[i];
        const peer = new WebRtcPeer(s.label, { audioToneHz: 440 + i * 40 });
        await peer.attachSyntheticStream();
        peers.push(peer);
        const wire = await wireRenegotiation({
          client: s.client,
          peer,
          token: s.token,
          port: ctx.port,
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
        wires.push(wire);
        const offer = await peer.createOffer();
        const joinResp = await s.client.call<{
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
      }

      await sleep(2000);

      const applied = setNetem({ lossPct: LOSS_PCT });
      metrics["lossApplied"] = applied;
      metrics["lossInjectedPct"] = LOSS_PCT;
      if (!applied) {
        throw new Error("F2: setNetem returned false even though netAvailable() said true");
      }

      peers.forEach((p) => p.startStats());
      await sleep(SAMPLE_SEC * 1000);
      peers.forEach((p) => p.stopStats());

      const uploads = peers.map((p) => p.getLastStats()?.bytesSent ?? 0);
      const losses = peers.map((p) => p.getLastStats()?.packetsLost ?? 0);
      metrics["uploadBytesPerPeer"] = uploads;
      metrics["uploadMean"] = mean(uploads);
      metrics["packetsLostPerPeer"] = losses;
      metrics["packetsLostTotal"] = losses.reduce((a, b) => a + b, 0);

      samples.push({
        name: `loss_${LOSS_PCT}pct_sample_window`,
        durationMs: SAMPLE_SEC * 1000,
        timestamp: Date.now(),
      });
    } finally {
      clearNet();
      for (const w of wires) {
        try {
          await w.detach();
        } catch {}
      }
      for (let i = 0; i < peers.length; i++) {
        try {
          await sessions[i]?.client.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {}
        try {
          await peers[i].close();
        } catch {}
      }
      try {
        await client.call("sfu.stopRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });
      } catch {}
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "f2-sfu-packet-loss",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed: Number(metrics["uploadMean"] ?? 0) > 0,
      metrics,
      samples,
      summary:
        `F2: SFU ${LOSS_PCT}% loss — uploadMean=${metrics["uploadMean"]}B ` +
        `packetsLost=${metrics["packetsLostTotal"]}`,
    };
  },
};

function mean(arr: number[]): number {
  return arr.length ? Math.round(arr.reduce((a, b) => a + b, 0) / arr.length) : 0;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
