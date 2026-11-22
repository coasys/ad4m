/**
 * E29: carol's socket is down while the call changes; only a resume can
 * bring her what she missed.
 *
 * alice, bob and carol are in an SFU call; carol's executor WebSocket
 * crosses a `TcpShaper`. `blackout()` cuts it, and her client's redial gets
 * no answer until the link is back (the dial waits, as a SYN retransmits,
 * then connects). Her media is UDP beside it and keeps flowing. Meanwhile
 * dave joins: the room's roster and the executor's offer of his tracks go
 * to carol's socket, which is down, and so do the offer's two resends
 * (10 s and 30 s after it). The blackout outlasts them all.
 *
 * When her socket is back, nothing the executor sends by itself tells carol
 * that dave is there or carries his media to her: only `sfu.callResume`
 * (slice 3), asked over the new socket, can bring her the missed roster and
 * the pending offer. Within `RESUME_MS` of the link coming back, carol and
 * dave must list, hear and see each other, with nobody marked left and the
 * three first participants' media flowing throughout.
 *
 * Gates slice 3. Skipped while listed in `FAILS_ON_DEV`;
 * `E2E_KNOWN_FAILURES=1` runs it.
 */

import { all } from "../call.js";
import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { RunChecks, TcpShaper, failsOnDev, reconnected } from "../../faults/index.js";

const PRESENT = ["alice", "bob", "carol"];
const EVERYONE = [...PRESENT, "dave"];
/** dave's join and its offer's resends at 10 s and 30 s all fall inside this. */
const BLACKOUT_MS = 40_000;
/** carol's waiting dial connects at the blackout's end; the resume and its renegotiation fit inside this. */
const RESUME_MS = 15_000;

export const e29MissedState = e2eScenario({
  id: "e29",
  name: "Missed call state while the socket is down",
  description: "carol's socket is down while dave joins, past every resend of his offer: once it is back, only a resume brings her the roster and his media",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => failsOnDev("e29"),
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e29");
    const shaper = new TcpShaper(world.users.carol.port);
    await shaper.listen();
    world.users.carol = { ...world.users.carol, port: shaper.port };
    let checks: RunChecks | undefined;
    try {
      await step("three join and settle", async () => {
        await call.join(PRESENT);
        await call.eventually(call.topologyIs("sfu"), 30_000, "topology");
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(CAMERA), 60_000, "video");
      });

      checks = new RunChecks(call, { recoverMs: 20_000, maxDepartures: 0 });
      await checks.start();
      const fault = checks.fault("socket-blackout", ["carol"], { uninterrupted: true });
      const blackoutEnds = Date.now() + BLACKOUT_MS;
      const cut = shaper.blackout(BLACKOUT_MS);
      if (cut.length === 0) throw new Error("carol had no connection to cut");

      await step("dave joins while carol's socket is down", async () => {
        await call.join(["dave"]);
        await new Promise((resolve) => setTimeout(resolve, Math.max(0, blackoutEnds - Date.now())));
      });

      await step(`carol's socket is back, and she and dave list, hear and see each other within ${RESUME_MS / 1000} s`, async () => {
        await reconnected(shaper, call.who("carol"), cut, RESUME_MS);
        const remaining = Math.max(1, RESUME_MS - (Date.now() - blackoutEnds));
        await call.eventually(all(call.rosterIsEveryone(), call.hearsEveryone(), call.seesEveryone(CAMERA)), remaining, "resume");
        fault.end();
      });

      await step("nobody left, and media flowed on every pair throughout", async () => {
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
