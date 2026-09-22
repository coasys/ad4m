/**
 * T17: Session data channel — send and receive via SFU relay.
 *
 * Validates the data channel path that the SDK Session interface exposes
 * through `session.sendData()` and `session.onData()`:
 *   join 2 peers → peer A sends data → peer B receives →
 *   peer B sends data → peer A receives → verify payloads match.
 *
 * Tests both string and binary (base64-encoded) data paths.
 * Each peer is a distinct AD4M user (multi-user mode).
 *
 * Asserts:
 *   - Data sent by peer A arrives at peer B with correct label and payload.
 *   - Data sent by peer B arrives at peer A with correct label and payload.
 *   - Binary-flagged data round-trips correctly.
 *   - Multiple messages on different labels all arrive.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";
import { EventsClient, EventFrame } from "../events.js";

const ROOM_NAME = "t17-session-data-channel";

interface ReceivedDataMessage {
  label: string;
  data: string;
  binary: boolean;
  from: string;
  timestamp: number;
}

export const t17SessionDataChannel: Scenario = {
  id: "t17",
  name: "Session data channel",
  description: "SFU-relayed data channels: send/receive string and binary payloads between 2 peers",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    const neighbourhoodUrl = "windtunnel://t17";
    metrics["neighbourhoodUrl"] = neighbourhoodUrl;

    await admin.call("sfu.startRoom", { neighbourhoodUrl, roomName: ROOM_NAME });

    const sessions = await provisionPeers({
      admin,
      port,
      count: 2,
      labelPrefix: "t17-peer",
    });

    await registerSfuMembers({ admin, neighbourhoodUrl, sessions });

    const peers: WebRtcPeer[] = [];
    const wires: RenegotiationWire[] = [];
    const eventClients: EventsClient[] = [];
    let passed = false;

    try {
      // ── Join both peers ────────────────────────────────────────────
      for (let i = 0; i < sessions.length; i++) {
        const session = sessions[i];
        const peer = new WebRtcPeer(session.label, { audioToneHz: 440 + i * 80 });
        await peer.attachSyntheticStream();
        peers.push(peer);

        const wire = await wireRenegotiation({
          client: session.client,
          peer,
          token: session.token,
          port,
          neighbourhoodUrl,
          roomName: ROOM_NAME,
        });
        wires.push(wire);

        const offer = await peer.createOffer();
        const t0 = Date.now();
        const joinResp = await session.client.call<{
          sdpAnswer: string;
          participantId: string;
          streamMapping: string[];
        }>("sfu.callJoin", {
          neighbourhoodUrl,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });
        samples.push({ name: `join_${session.label}`, durationMs: Date.now() - t0, timestamp: Date.now() });
        await peer.acceptAnswer(JSON.parse(joinResp.sdpAnswer));
      }

      await waitForParticipantCount(admin, ROOM_NAME, 2, 15_000);
      await sleep(2_000);

      // ── Subscribe data channels for both peers ─────────────────────
      const received: [ReceivedDataMessage[], ReceivedDataMessage[]] = [[], []];

      for (let i = 0; i < sessions.length; i++) {
        const evts = new EventsClient({
          port,
          token: sessions[i].token,
        });
        await evts.connect();
        eventClients.push(evts);

        evts.on("sfu-data-channel", (frame: EventFrame) => {
          received[i].push({
            label: frame.channelLabel as string,
            data: frame.data as string,
            binary: (frame.binary as boolean) ?? false,
            from: (frame.fromDid as string) ?? "",
            timestamp: Date.now(),
          });
        });
      }

      await sleep(500);

      // ── Peer 0 sends string data ───────────────────────────────────
      const payload0 = JSON.stringify({ type: "reaction", emoji: "👍", ts: Date.now() });
      const t0Send = Date.now();
      await sessions[0].client.call("sfu.sendData", {
        neighbourhoodUrl,
        roomName: ROOM_NAME,
        channelLabel: "reactions",
        data: payload0,
        binary: false,
      });
      samples.push({ name: "send_string_peer0", durationMs: Date.now() - t0Send, timestamp: Date.now() });

      // ── Peer 1 sends string data ───────────────────────────────────
      const payload1 = JSON.stringify({ type: "hand-raise", raised: true });
      const t1Send = Date.now();
      await sessions[1].client.call("sfu.sendData", {
        neighbourhoodUrl,
        roomName: ROOM_NAME,
        channelLabel: "hand-raise",
        data: payload1,
        binary: false,
      });
      samples.push({ name: "send_string_peer1", durationMs: Date.now() - t1Send, timestamp: Date.now() });

      // ── Peer 0 sends binary data ───────────────────────────────────
      const binaryPayload = Buffer.from("binary-test-payload-12345").toString("base64");
      const tBin = Date.now();
      await sessions[0].client.call("sfu.sendData", {
        neighbourhoodUrl,
        roomName: ROOM_NAME,
        channelLabel: "binary-test",
        data: binaryPayload,
        binary: true,
      });
      samples.push({ name: "send_binary_peer0", durationMs: Date.now() - tBin, timestamp: Date.now() });

      // ── Wait for delivery ──────────────────────────────────────────
      await sleep(3_000);

      // ── Verify received messages ───────────────────────────────────
      // Peer 1 should have received peer 0's "reactions" and "binary-test" messages.
      const peer1reactions = received[1].filter((m) => m.label === "reactions");
      const peer1binary = received[1].filter((m) => m.label === "binary-test");
      // Peer 0 should have received peer 1's "hand-raise" message.
      const peer0handRaise = received[0].filter((m) => m.label === "hand-raise");

      metrics["peer1_reactions_count"] = peer1reactions.length;
      metrics["peer1_binary_count"] = peer1binary.length;
      metrics["peer0_handRaise_count"] = peer0handRaise.length;

      const reactionsPayloadMatch = peer1reactions.length > 0 && peer1reactions[0].data === payload0;
      const handRaisePayloadMatch = peer0handRaise.length > 0 && peer0handRaise[0].data === payload1;
      const binaryPayloadMatch = peer1binary.length > 0 && peer1binary[0].data === binaryPayload;

      metrics["reactionsPayloadMatch"] = reactionsPayloadMatch;
      metrics["handRaisePayloadMatch"] = handRaisePayloadMatch;
      metrics["binaryPayloadMatch"] = binaryPayloadMatch;
      metrics["binaryFlagCorrect"] = peer1binary.length > 0 ? peer1binary[0].binary === true : false;

      // ── Multi-message burst on same label ──────────────────────────
      const burstCount = 10;
      const tBurst = Date.now();
      for (let n = 0; n < burstCount; n++) {
        await sessions[0].client.call("sfu.sendData", {
          neighbourhoodUrl,
          roomName: ROOM_NAME,
          channelLabel: "burst",
          data: `msg-${n}`,
          binary: false,
        });
      }
      samples.push({ name: `send_burst_${burstCount}`, durationMs: Date.now() - tBurst, timestamp: Date.now() });

      await sleep(3_000);

      const peer1burst = received[1].filter((m) => m.label === "burst");
      metrics["burst_sent"] = burstCount;
      metrics["burst_received"] = peer1burst.length;
      const burstComplete = peer1burst.length === burstCount;
      metrics["burstComplete"] = burstComplete;

      // Verify ordering.
      let burstOrdered = true;
      for (let n = 0; n < peer1burst.length; n++) {
        if (peer1burst[n].data !== `msg-${n}`) {
          burstOrdered = false;
          break;
        }
      }
      metrics["burstOrdered"] = burstOrdered;

      passed =
        reactionsPayloadMatch &&
        handRaisePayloadMatch &&
        binaryPayloadMatch &&
        (metrics["binaryFlagCorrect"] as boolean) &&
        burstComplete &&
        burstOrdered;
    } finally {
      for (const evts of eventClients) {
        try { await evts.disconnect(); } catch { /* best-effort */ }
      }
      for (const w of wires) {
        try { await w.detach(); } catch { /* best-effort */ }
      }
      for (let i = 0; i < peers.length; i++) {
        try {
          await sessions[i].client.call("sfu.callLeave", {
            neighbourhoodUrl,
            roomName: ROOM_NAME,
          });
        } catch { /* best-effort */ }
        try { await peers[i].close(); } catch { /* best-effort */ }
      }
      try {
        await admin.call("sfu.stopRoom", { neighbourhoodUrl, roomName: ROOM_NAME });
      } catch { /* best-effort */ }
      await disconnectPeers(sessions);
    }

    const endTime = Date.now();
    return {
      scenario: "t17-session-data-channel",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `T17: Data channel — ` +
        `reactions=${metrics["reactionsPayloadMatch"]} ` +
        `handRaise=${metrics["handRaisePayloadMatch"]} ` +
        `binary=${metrics["binaryPayloadMatch"]} ` +
        `burst=${metrics["burst_received"]}/${metrics["burst_sent"]} ` +
        `ordered=${metrics["burstOrdered"]}`,
    };
  },
};

async function waitForParticipantCount(
  client: ScenarioContext["client"],
  roomName: string,
  expected: number,
  timeoutMs: number,
): Promise<void> {
  const start = Date.now();
  while (Date.now() - start < timeoutMs) {
    const rooms = await client
      .call<Array<{ roomName: string; participantCount: number }>>("sfu.listRooms", {})
      .catch(() => [] as Array<{ roomName: string; participantCount: number }>);
    const room = rooms.find((r) => r.roomName === roomName);
    if (room && room.participantCount >= expected) return;
    await sleep(250);
  }
  throw new Error(
    `T17 waitForParticipantCount: room=${roomName} expected=${expected} within ${timeoutMs}ms`,
  );
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
