/**
 * E25: one participant's app socket stays open but passes nothing for 20 s.
 *
 * carol's executor WebSocket crosses a `TcpShaper`; her media is UDP beside
 * it and never does. `silence(20000)` keeps the TCP connection up and holds
 * every byte both ways — the case a browser has no event for at all (the
 * socket reads open, nobody answers on it), and what a NAT that drops the
 * flow looks like to TCP.
 *
 * The SDK must notice by itself (its ping deadline), dial a new socket, and
 * repair the call's signalling over it — while the media beside the socket
 * carries on and nobody may be marked left.
 */

import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { FAILS_ON_DEV, RunChecks, TcpShaper } from "../../faults/index.js";

const EVERYONE = ["alice", "bob", "carol"];
const SILENCE_MS = 20_000;
/** The silent-socket drop, the redial and the re-watched events settle inside this. */
const SETTLE_MS = 10_000;

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export const e25SocketSilent = e2eScenario({
  id: "e25",
  name: "Open but silent app socket for 20 s",
  description: "carol's executor WebSocket stays open but forwards nothing for 20 s: she notices, redials and repairs while media flows throughout",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => FAILS_ON_DEV["e25"] ?? null,
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e25");
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
      const fault = checks.fault("silent-socket", ["carol"], { uninterrupted: true });
      await step("carol's socket stays open and silent for 20 s while her media flows", async () => {
        shaper.silence(SILENCE_MS);
        await sleep(SILENCE_MS + 1_000);
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
