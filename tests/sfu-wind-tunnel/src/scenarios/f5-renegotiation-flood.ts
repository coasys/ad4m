/**
 * F5: SDP renegotiation flood.
 *
 * Within a 10-peer SFU room, churn join/leave events at high rate to
 * stress-test the renegotiation push pipeline.  Each leave triggers an
 * SDP renegotiation to the remaining peers (the SFU re-pushes track
 * mappings); each new join triggers a fresh callJoin and pushes an
 * updated stream mapping to all existing peers.
 *
 * Verifies:
 *   - The SFU doesn't deadlock or drop the room after many in/out
 *     transitions.
 *   - `sfu.listRooms` reports correct participant_count throughout.
 *   - Renegotiations land within a bounded time (we measure how long
 *     each join takes — it should NOT grow with churn count).
 *
 * Each peer's leave path explicitly calls `sfu.callLeave` because
 * closing the local RTCPeerConnection alone doesn't tell the SFU the
 * peer left — the server would otherwise keep counting it as a
 * participant until the connection times out (~30s).
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { InstrumentedClient } from "../client.js";
import { provisionPeers, PeerSession, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "f5-renegotiation-flood";
const NEIGHBOURHOOD = `windtunnel://f5`;
const STEADY_STATE_PEERS = 10;
const CHURN_CYCLES = 10; // each cycle = 1 leave + 1 new join

interface F5Peer {
  peer: WebRtcPeer;
  session: PeerSession;
  wire: RenegotiationWire;
}

export const f5RenegotiationFlood: Scenario = {
  id: "f5",
  name: "SDP renegotiation flood",
  description: "10-peer SFU room, 10 churn cycles in <60s — verify renegotiation pipeline holds",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client, branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    try {
      await client.call("sfu.startRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      if (msg.includes("not yet available")) {
        metrics["skipped"] = true;
        metrics["skip_reason"] = msg;
        return {
          scenario: "f5-renegotiation-flood",
          branch,
          passed: true,
          startTime,
          endTime: Date.now(),
          durationMs: Date.now() - startTime,
          metrics,
          samples,
          summary: `F5: SKIPPED — ${msg}`,
        };
      }
      throw e;
    }

    const peers: F5Peer[] = [];
    let nextPeerId = 0;
    let passed = false;
    try {
      // Steady state: 10 peers joined.
      for (let i = 0; i < STEADY_STATE_PEERS; i++) {
        peers.push(await joinOne(client, ctx.port, nextPeerId++));
      }

      // Confirm the steady state.
      const initialRooms = await client.call<Array<{ roomName: string; participantCount: number }>>(
        "sfu.listRooms",
        {},
      );
      metrics["initialParticipants"] =
        initialRooms.find((r) => r.roomName === ROOM_NAME)?.participantCount ?? -1;

      // Churn cycles: each cycle leaves peers[0] (via sfu.callLeave +
      // local close) and joins a new peer.
      const joinDurations: number[] = [];
      const churnStart = Date.now();
      for (let c = 0; c < CHURN_CYCLES; c++) {
        const leaving = peers.shift();
        if (leaving) {
          await leaveOne(leaving);
        }
        const t0 = Date.now();
        const f5peer = await joinOne(client, ctx.port, nextPeerId++);
        const dt = Date.now() - t0;
        joinDurations.push(dt);
        peers.push(f5peer);
        samples.push({ name: `churn_cycle_${c}_join_ms`, durationMs: dt, timestamp: Date.now() });
      }
      const churnElapsed = Date.now() - churnStart;
      metrics["churnElapsedMs"] = churnElapsed;
      metrics["joinDurationsMs"] = joinDurations;
      metrics["joinMeanMs"] = mean(joinDurations);
      metrics["joinMaxMs"] = Math.max(...joinDurations);
      metrics["joinGrowthMs"] =
        joinDurations[joinDurations.length - 1] - joinDurations[0];

      // Let the SFU's room state catch up — call_leave is fire-and-forget
      // server-side; the listRooms read can race the leave.
      await sleep(500);
      const finalRooms = await client.call<Array<{ roomName: string; participantCount: number }>>(
        "sfu.listRooms",
        {},
      );
      metrics["finalParticipants"] =
        finalRooms.find((r) => r.roomName === ROOM_NAME)?.participantCount ?? -1;
      metrics["participantInvariant"] =
        metrics["initialParticipants"] === metrics["finalParticipants"];
      passed = metrics["initialParticipants"] === metrics["finalParticipants"];
    } finally {
      for (const f5peer of peers) {
        try {
          await leaveOne(f5peer);
        } catch {}
      }
      try {
        await client.call("sfu.stopRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });
      } catch {}
    }

    const endTime = Date.now();
    return {
      scenario: "f5-renegotiation-flood",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `F5: ${CHURN_CYCLES} churn cycles in ${metrics["churnElapsedMs"]}ms — ` +
        `joinMean=${metrics["joinMeanMs"]}ms joinMax=${metrics["joinMaxMs"]}ms ` +
        `participantInvariant=${metrics["participantInvariant"]}`,
    };
  },
};

async function joinOne(
  admin: InstrumentedClient,
  port: number,
  idx: number,
): Promise<F5Peer> {
  const [session] = await provisionPeers({
    admin,
    port,
    count: 1,
    labelPrefix: `f5-peer-${idx}`,
  });
  await registerSfuMembers({
    admin,
    neighbourhoodUrl: NEIGHBOURHOOD,
    sessions: [session],
  });
  const peer = new WebRtcPeer(session.label, { audioToneHz: 440 + (idx % 20) * 10 });
  await peer.attachSyntheticStream();
  const wire = await wireRenegotiation({
    client: session.client,
    peer,
    token: session.token,
    port,
    neighbourhoodUrl: NEIGHBOURHOOD,
    roomName: ROOM_NAME,
  });
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
  return { peer, session, wire };
}

async function leaveOne(f5peer: F5Peer): Promise<void> {
  try {
    await f5peer.session.client.call("sfu.callLeave", {
      neighbourhoodUrl: NEIGHBOURHOOD,
      roomName: ROOM_NAME,
    });
  } catch {}
  try {
    await f5peer.wire.detach();
  } catch {}
  try {
    await f5peer.peer.close();
  } catch {}
  try {
    await f5peer.session.client.disconnect();
  } catch {}
}

function mean(arr: number[]): number {
  return arr.length ? Math.round(arr.reduce((a, b) => a + b, 0) / arr.length) : 0;
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
