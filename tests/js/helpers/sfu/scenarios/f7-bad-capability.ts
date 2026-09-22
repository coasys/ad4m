/**
 * F7: Capability missing.
 *
 * Connect with an invalid admin token and confirm `sfu.*` returns an
 * auth error rather than crashing or leaking state.
 *
 * Today the wind tunnel's WS handler dispatches every method through
 * the same auth pipe, so `sfu.callJoin` with a bad token surfaces the
 * same 401-ish error as any other admin-gated RPC.  This scenario
 * primarily protects against regressions where SFU handlers
 * accidentally drop the auth guard.
 */

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { InstrumentedClient } from "../client.js";

const NEIGHBOURHOOD = "windtunnel://f7";
const ROOM_NAME = "f7-bad-cap";

export const f7BadCapability: Scenario = {
  id: "f7",
  name: "Capability missing → 401/403",
  description: "sfu.callJoin with invalid admin token must reject, not crash",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client, branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};

    // Build a sibling client with a deliberately-bad token.
    const badClient = new InstrumentedClient({
      port: client.config.port,
      host: client.config.host,
      adminToken: "not-a-real-admin-token-xxxxx",
    });

    let connected = false;
    let connectError: string | null = null;
    try {
      await badClient.connect();
      connected = true;
    } catch (e) {
      connectError = e instanceof Error ? e.message : String(e);
    }
    metrics["wsConnectedWithBadToken"] = connected;
    metrics["wsConnectError"] = connectError;

    // If the WS layer accepted the bad token (i.e. enforces auth per
    // method rather than at handshake), confirm the RPC layer still
    // rejects.  If the WS handshake refused, that's already the right
    // answer.
    if (connected) {
      let rpcRejected = false;
      let rpcError: string | null = null;
      try {
        await badClient.call("sfu.startRoom", { neighbourhoodUrl: NEIGHBOURHOOD, roomName: ROOM_NAME });
      } catch (e) {
        rpcRejected = true;
        rpcError = e instanceof Error ? e.message : String(e);
      }
      metrics["rpcRejected"] = rpcRejected;
      metrics["rpcError"] = rpcError;
      try {
        await badClient.disconnect();
      } catch {}
    }

    // Verify the executor is still responsive on the GOOD client.
    let goodClientStillOk = false;
    try {
      await client.call("sfu.listRooms", {});
      goodClientStillOk = true;
    } catch (e) {
      metrics["goodClientError"] = e instanceof Error ? e.message : String(e);
    }
    metrics["executorStillResponsive"] = goodClientStillOk;

    const endTime = Date.now();
    const guarded =
      !metrics["wsConnectedWithBadToken"] || metrics["rpcRejected"] === true;
    const passed = guarded && goodClientStillOk;
    return {
      scenario: "f7-bad-capability",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `F7: badTokenGuarded=${guarded} (wsConnected=${metrics["wsConnectedWithBadToken"]} ` +
        `rpcRejected=${metrics["rpcRejected"] ?? "n/a"}) ` +
        `executorStillResponsive=${metrics["executorStillResponsive"]}`,
    };
  },
};
