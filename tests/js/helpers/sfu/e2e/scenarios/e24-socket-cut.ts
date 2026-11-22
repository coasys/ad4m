/**
 * E24: one participant's app socket is cut while the network — and its
 * media — stays up.
 *
 * carol's executor WebSocket crosses a `TcpShaper`; her media is UDP beside
 * it and never does. `cut()` closes the socket both ways, as the kernel does
 * when a route or a NAT entry dies — the socket reports closed, nothing on
 * the link is wrong. The SDK must dial a new socket over the same link and
 * repair the call's signalling on it; the media never stopped.
 *
 * A peer lives while its ICE path is alive (socket evidence decides the
 * socket's health, not the peer's), so nothing may mark carol, or anyone,
 * left: no `participant-left` for a peer still joined, every pair's packets
 * growing again inside the recovery bound, and no call rebuild.
 */

import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { FAILS_ON_DEV, RunChecks, TcpShaper } from "../../faults/index.js";

const EVERYONE = ["alice", "bob", "carol"];
/** The redial, the re-watched events and any ICE repair play out inside this. */
const SETTLE_MS = 10_000;

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export const e24SocketCut = e2eScenario({
  id: "e24",
  name: "App socket cut while media stays up",
  description: "carol's executor WebSocket is cut mid-call with the link itself fine: she redials, nobody leaves, media keeps flowing",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => FAILS_ON_DEV["e24"] ?? null,
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e24");
    // carol's app socket crosses the shaper; her media (UDP) never does.
    const shaper = new TcpShaper(world.users.carol.port);
    await shaper.listen();
    world.users.carol = { ...world.users.carol, port: shaper.port };
    try {
      await step("three join and settle", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.topologyIs("sfu"), 30_000, "topology");
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(CAMERA), 60_000, "video");
      });

      const checks = new RunChecks(call, { recoverMs: 20_000, maxRebuilds: 0 });
      await checks.start();
      const fault = checks.fault("socket-cut", ["carol"]);
      await step("carol's app socket is cut; the call repairs over a new one", async () => {
        if (shaper.cut() === 0) throw new Error("carol had no connection to cut");
        fault.end();
        await sleep(SETTLE_MS);
      });
      await step("the call kept carol, and media still flows on every pair", async () => {
        const violations = await checks.stop();
        if (violations.length) throw new Error(violations.join("\n"));
      });
      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
    } finally {
      shaper.close();
    }
  },
});
