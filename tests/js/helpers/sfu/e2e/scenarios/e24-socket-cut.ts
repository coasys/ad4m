/**
 * E24: one participant's app socket is cut while the network — and its
 * media — stays up. A regression row: dev passes it.
 *
 * carol's executor WebSocket crosses a `TcpShaper`; her media is UDP beside
 * it and never does. `cut()` closes the socket both ways, as the kernel does
 * when a route or a NAT entry dies — the socket reports closed, nothing on
 * the link is wrong. The SDK must dial a new socket over the same link: the
 * row sees a new connection through the shaper and an RPC answered over it.
 *
 * A peer lives while its ICE path is alive (socket evidence decides the
 * socket's health, not the peer's), so nothing may mark carol, or anyone,
 * left: no `participant-left` for a peer still joined, every pair's audio
 * and video growing through the cut and the redial, and no departure.
 *
 * Skipped while listed in `FAILS_ON_DEV`; `E2E_KNOWN_FAILURES=1` runs it.
 */

import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { RunChecks, TcpShaper, failsOnDev, reconnected } from "../../faults/index.js";

const EVERYONE = ["alice", "bob", "carol"];
/** The redial (first retry after 500 ms) and an RPC over the new socket fit inside this. */
const RECONNECT_MS = 10_000;
/** Media keeps flowing this long past the new socket, while the SDK repairs the call's signalling on it. */
const SETTLE_MS = 10_000;

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export const e24SocketCut = e2eScenario({
  id: "e24",
  name: "App socket cut while media stays up",
  description: "carol's executor WebSocket is cut mid-call with the link itself fine: she redials over a new connection, nobody leaves, media keeps flowing",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => failsOnDev("e24"),
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e24");
    // carol's app socket crosses the shaper; her media (UDP) never does.
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
      const fault = checks.fault("socket-cut", ["carol"], { uninterrupted: true });
      await step("carol's app socket is cut; she dials a new one that answers", async () => {
        const before = shaper.connections().filter((c) => c.open).map((c) => c.conn);
        if (shaper.cut() === 0) throw new Error("carol had no connection to cut");
        await reconnected(shaper, call.who("carol"), before, RECONNECT_MS);
        await sleep(SETTLE_MS);
        fault.end();
      });
      await step("the call kept carol, and media still flows on every pair", async () => {
        const violations = await checks!.stop();
        if (violations.length) throw new Error(violations.join("\n"));
      });
      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
    } finally {
      await checks?.abort();
      shaper.close();
    }
  },
});
