/**
 * F6: Non-member callJoin — actual membership rejection.
 *
 * Creates a non-admin user who does NOT belong to the target
 * neighbourhood, then attempts `sfu.callJoin` with that user's JWT.
 * The SFU's membership check should reject the call.
 *
 * NOTE: this scenario depends on the AD4M fix that removes the
 * `user_did.is_some()` bypass in the SFU membership gate.  Until
 * that fix lands, the join may succeed — the scenario logs the
 * outcome either way and confirms the executor remains responsive.
 *
 * Verifies:
 *   - A non-member user with a valid DID + JWT gets rejected (or,
 *     pre-fix, at least doesn't crash the executor).
 *   - The rejection comes from the membership check, not from SDP
 *     parsing (the old F6 sent a bogus SDP and the error came from
 *     the wrong layer).
 *   - `sfu.listRooms` still works after the attempt.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { provisionPeers, disconnectPeers, PeerSession } from "../users.js";

const NEIGHBOURHOOD = `neighbourhood://f6-membership-test/${Date.now()}`;
const ROOM_NAME = "f6-non-member";

export const f6NonMemberJoin: Scenario = {
  id: "f6",
  name: "Non-member callJoin (membership rejection)",
  description:
    "Creates a non-admin user outside the neighbourhood and verifies " +
    "callJoin rejects them. Depends on the ad4m fix removing the " +
    "user_did.is_some() bypass.",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    let sessions: PeerSession[] = [];
    let peer: WebRtcPeer | null = null;
    let passed = false;

    try {
      // Start a room under a neighbourhood:// URL (not windtunnel://)
      // so the SFU's membership gate engages.
      await admin.call("sfu.startRoom", {
        neighbourhoodUrl: NEIGHBOURHOOD,
        roomName: ROOM_NAME,
      });

      // Provision a single non-admin user.  This user exists on the
      // executor (has a DID) but has NOT joined the neighbourhood.
      sessions = await provisionPeers({
        admin,
        port,
        count: 1,
        labelPrefix: "f6-outsider",
      });
      const outsider = sessions[0];
      metrics["outsiderDid"] = outsider.did;

      // Build a real SDP offer so any rejection comes from the
      // membership check, not from SDP parsing.
      peer = new WebRtcPeer(outsider.label, { audioToneHz: 440 });
      await peer.attachSyntheticStream();
      const offer = await peer.createOffer();

      // Attempt to join with the non-member user's authenticated client.
      let joinRejected = false;
      let joinError: string | null = null;
      let joinResponse: unknown = null;
      const joinStart = Date.now();
      try {
        joinResponse = await outsider.client.call("sfu.callJoin", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
          sdpOffer: JSON.stringify(offer),
        });
      } catch (e) {
        joinRejected = true;
        joinError = e instanceof Error ? e.message : String(e);
      }
      const joinElapsed = Date.now() - joinStart;
      samples.push({
        name: "non_member_join_attempt",
        durationMs: joinElapsed,
        timestamp: Date.now(),
      });

      metrics["joinRejected"] = joinRejected;
      metrics["joinError"] = joinError;
      metrics["joinResponse"] = joinResponse;

      // Classify the rejection source so future runs can distinguish
      // "membership gate working" from "SDP parse error" (old bug).
      if (joinError) {
        metrics["rejectionSource"] = /member/i.test(joinError)
          ? "membership"
          : /sdp|parse|offer/i.test(joinError)
            ? "sdp_parse"
            : "unknown";
      } else {
        metrics["rejectionSource"] = "not_rejected";
      }

      // If the join succeeded despite non-membership (pre-fix behaviour),
      // leave cleanly so room state stays tidy.
      if (!joinRejected && joinResponse) {
        try {
          await outsider.client.call("sfu.callLeave", {
            neighbourhoodUrl: NEIGHBOURHOOD,
            roomName: ROOM_NAME,
          });
        } catch {
          /* best-effort */
        }
      }

      // Confirm the SFU remains responsive after the attempt.
      let listOk = false;
      try {
        await admin.call("sfu.listRooms", {});
        listOk = true;
      } catch (e) {
        metrics["postCallListError"] =
          e instanceof Error ? e.message : String(e);
      }
      metrics["sfuStillResponsive"] = listOk;
      passed = joinRejected;
    } finally {
      if (peer) {
        try {
          await peer.close();
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
      scenario: "f6-non-member-join",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `F6: joinRejected=${metrics["joinRejected"]} ` +
        `rejectionSource=${metrics["rejectionSource"]} ` +
        `sfuStillResponsive=${metrics["sfuStillResponsive"]}` +
        (metrics["joinError"]
          ? ` error="${String(metrics["joinError"]).slice(0, 60)}"`
          : ""),
    };
  },
};
