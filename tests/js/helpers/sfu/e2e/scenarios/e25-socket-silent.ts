/**
 * E25: one participant's app socket stays open but passes nothing. A
 * regression row: dev passes it.
 *
 * carol's executor WebSocket crosses a `TcpShaper`; her media is UDP beside
 * it and never does. `silence()` keeps that TCP connection up and holds
 * every byte on it both ways for good — the case a browser has no event
 * for at all (the socket reads open, nobody answers on it), and what a NAT
 * that drops the flow looks like to TCP. A new dial passes.
 *
 * The SDK must notice by itself (its ping deadline: pings go out on a 10 s
 * tick and one unanswered for a tick drops the socket, so 10 to 30 s), dial a new
 * socket, and get answers on it: the row sees a new connection through the
 * shaper and an RPC answered over it. The media beside the socket carries
 * on throughout and nobody may be marked left.
 *
 * Skipped while listed in `FAILS_ON_DEV`; `E2E_KNOWN_FAILURES=1` runs it.
 */

import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { RunChecks, TcpShaper, failsOnDev, reconnected, rowPassed } from "../../faults/index.js";

const EVERYONE = ["alice", "bob", "carol"];
/** The ping deadline (up to 30 s, seen at 29 s), the redial and an RPC over the new socket. */
const RECONNECT_MS = 40_000;
/** Media keeps flowing this long past the new socket, while the SDK repairs the call's signalling on it. */
const SETTLE_MS = 10_000;

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export const e25SocketSilent = e2eScenario({
  id: "e25",
  name: "Open but silent app socket",
  description: "carol's executor WebSocket stays open but forwards nothing: she notices, redials over a new connection and repairs while media flows throughout",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => failsOnDev("e25"),
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e25");
    const shaper = new TcpShaper(world.users.carol.port);
    await shaper.listen();
    world.users.carol = { ...world.users.carol, port: shaper.port };
    let checks: RunChecks | undefined;
    try {
      await step("three join and settle", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.topologyIs("sfu"), 30_000, "topology");
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(CAMERA), 60_000, "video");
      });

      checks = new RunChecks(call, { recoverMs: 20_000, maxDepartures: 0 });
      await checks.start();
      const fault = checks.fault("silent-socket", ["carol"], { uninterrupted: true });
      await step("carol's socket goes silent; she dials a new one that answers", async () => {
        const silenced = shaper.silence();
        if (silenced.length === 0) throw new Error("carol had no connection to silence");
        await reconnected(shaper, call.who("carol"), silenced, RECONNECT_MS);
        await sleep(SETTLE_MS);
        fault.end();
      });
      await step("the call kept carol, and media still flows on every pair", async () => {
        const violations = await checks!.stop();
        if (violations.length) throw new Error(violations.join("\n"));
      });
      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
      rowPassed("e25");
    } finally {
      await checks?.abort();
      shaper.close();
    }
  },
});
