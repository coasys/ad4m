/**
 * F10: every SFU RPC that names a neighbourhood is scoped to its members.
 *
 * F6 covers `sfu.callJoin`. This covers the rest: on a multi-user node every
 * user token holds NEIGHBOURHOOD_READ/UPDATE on `*`, so a capability check
 * alone lets any user stop another neighbourhood's room, rewrite its config
 * or read who is in it.
 *
 * Three users: the neighbourhood's owner (its first member on this node), a
 * second member, and an outsider. The owner joins a call so there is a room,
 * a participant and a quality preference to leak.
 *
 * Verifies:
 *   - the outsider is refused by each of the 12 neighbourhood-scoped RPCs;
 *   - a member who is not the owner is refused room and config writes (the
 *     config only the neighbourhood's creator may write; a synthetic
 *     neighbourhood has none, and the Rust tests in `sfu::config_store`
 *     cover the creator's write);
 *   - `listRooms` and `qualityPreferences` show the outsider nothing of this
 *     neighbourhood, while the owner sees the room.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { provisionPeers, disconnectPeers, registerSfuMembers, PeerSession } from "../users.js";

const NEIGHBOURHOOD = `neighbourhood://f10-scoping/${Date.now()}`;
const ROOM_NAME = "f10-room";

/** Calls `method` and reports whether it was refused, with the error text. */
async function refused(
  session: PeerSession,
  method: string,
  params: Record<string, unknown>,
): Promise<{ refused: boolean; error: string | null }> {
  try {
    await session.client.call(method, params);
    return { refused: false, error: null };
  } catch (e) {
    const error = e instanceof Error ? e.message : String(e);
    // A refusal from the guard, not an unrelated failure further in.
    return { refused: /member|owner|creator|forbidden/i.test(error), error };
  }
}

export const f10NonMemberRpcs: Scenario = {
  id: "f10",
  name: "Neighbourhood-scoped SFU RPCs refuse non-members",
  description:
    "An outsider is refused every neighbourhood-scoped SFU RPC, a non-owner member " +
    "is refused room and config writes, and room listings hide other neighbourhoods.",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const metrics: Record<string, unknown> = {};
    const failures: string[] = [];

    let sessions: PeerSession[] = [];
    let peer: WebRtcPeer | null = null;

    try {
      await admin.call("sfu.startRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });

      sessions = await provisionPeers({ admin, port, count: 3, labelPrefix: "f10" });
      const [owner, member, outsider] = sessions;
      // Registration order decides ownership: the first member is the owner.
      await registerSfuMembers({ admin, neighbourhoodUrl: NEIGHBOURHOOD, sessions: [owner, member] });

      peer = new WebRtcPeer(owner.label, { audioToneHz: 440 });
      await peer.attachSyntheticStream();
      const join = await owner.client.call<{ sdpAnswer: string; participantId: string }>(
        "sfu.callJoin",
        { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME, sdpOffer: JSON.stringify(await peer.createOffer()) },
      );
      await peer.acceptAnswer(JSON.parse(join.sdpAnswer));
      await owner.client.call("sfu.callSetQualityPreference", {
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
        preference: "low",
      });

      const room = { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME };
      const nh = { neighbourhoodUrl: NEIGHBOURHOOD };
      const outsiderPeer = new WebRtcPeer(outsider.label, { audioToneHz: 520 });
      await outsiderPeer.attachSyntheticStream();
      const outsiderOffer = JSON.stringify(await outsiderPeer.createOffer());
      await outsiderPeer.close();

      const outsiderCalls: [string, Record<string, unknown>][] = [
        ["sfu.startRoom", room],
        ["sfu.stopRoom", room],
        ["sfu.callJoin", { ...room, sdpOffer: outsiderOffer }],
        ["sfu.callLeave", room],
        ["sfu.callSetQualityPreference", { ...room, preference: "high" }],
        ["sfu.callAnswerServerOffer", { ...room, sdpAnswer: "{}" }],
        ["sfu.addIceCandidate", { ...room, candidate: "candidate:1 1 udp 1 127.0.0.1 9 typ host" }],
        ["sfu.sendData", { ...room, channelLabel: "chat", data: "hello" }],
        ["sfu.getConfig", nh],
        ["sfu.setConfig", { ...nh, config: { mode: "mesh" } }],
        ["sfu.sfuPeerForNeighbourhood", nh],
        ["sfu.sfuPeersForNeighbourhood", nh],
      ];
      const outsiderResults: Record<string, unknown> = {};
      for (const [method, params] of outsiderCalls) {
        const r = await refused(outsider, method, params);
        outsiderResults[method] = r;
        if (!r.refused) failures.push(`outsider ${method}: ${r.error ?? "accepted"}`);
      }
      metrics["outsider"] = outsiderResults;

      const memberResults: Record<string, unknown> = {};
      for (const [method, params] of [
        ["sfu.startRoom", room],
        ["sfu.stopRoom", room],
        ["sfu.setConfig", { ...nh, config: { mode: "mesh" } }],
      ] as [string, Record<string, unknown>][]) {
        const r = await refused(member, method, params);
        memberResults[method] = r;
        if (!r.refused) failures.push(`member ${method}: ${r.error ?? "accepted"}`);
      }
      metrics["member"] = memberResults;

      const outsiderRooms = await outsider.client.call<{ neighbourhoodUrl: string }[]>("sfu.listRooms", {});
      if (outsiderRooms.some((r) => r.neighbourhoodUrl === NEIGHBOURHOOD)) {
        failures.push("outsider sfu.listRooms shows the neighbourhood's room");
      }
      const outsiderPrefs = await outsider.client.call<{ participantId: string }[]>("sfu.qualityPreferences", {});
      if (outsiderPrefs.some((p) => p.participantId === join.participantId)) {
        failures.push("outsider sfu.qualityPreferences shows the owner's preference");
      }
      const ownerRooms = await owner.client.call<{ neighbourhoodUrl: string }[]>("sfu.listRooms", {});
      if (!ownerRooms.some((r) => r.neighbourhoodUrl === NEIGHBOURHOOD)) {
        failures.push("owner sfu.listRooms does not show its own room");
      }
    } finally {
      if (peer) await peer.close().catch(() => {});
      await admin.call("sfu.stopRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME }).catch(() => {});
      await disconnectPeers(sessions);
    }

    metrics["failures"] = failures;
    const endTime = Date.now();
    return {
      scenario: "f10-non-member-rpcs",
      branch,
      passed: failures.length === 0,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples: [],
      summary:
        failures.length === 0
          ? "F10: every neighbourhood-scoped RPC refused the outsider; writes refused the non-owner"
          : `F10: ${failures.length} unguarded — ${failures.join("; ").slice(0, 300)}`,
    };
  },
};
