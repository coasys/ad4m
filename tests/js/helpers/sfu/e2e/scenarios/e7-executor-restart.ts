/**
 * E7: the executor restarts in the middle of a call.
 *
 * An operator restarts the executor that hosts the SFU and every user. Its
 * rooms are gone with it. When it is back, each participant's session must
 * bring the call back by itself — the app does nothing — and everyone must
 * hear everyone again.
 */

import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3", "u4"];

export const e7ExecutorRestart = e2eScenario({
  id: "e7",
  name: "Executor restart during an SFU call",
  description: "The executor restarts mid-call; every session rejoins by itself and everyone hears everyone",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "sfu" } }],
  },
  async run({ world, call: inSpace, step, metric }) {
    const call = inSpace("space", "e7", { video: false });

    await step("four join", () => call.join(EVERYONE));
    await step("four settle", async () => {
      await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
      await call.eventually(call.hearsEveryone(), 60_000, "audio");
    });

    await step("the executor restarts", () => world.restartExecutor());
    metric("recovered_ms", await step("every session brings the call back by itself", async () => {
      const started = Date.now();
      await call.eventually(call.rosterIsEveryone(), 90_000, "roster after the restart");
      await call.eventually(call.hearsEveryone(), 60_000, "audio after the restart");
      return Date.now() - started;
    }));
  },
});
