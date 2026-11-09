/**
 * E5: a participant vanishes — a closed tab, a dead battery.
 *
 * u3's process is killed mid-call: no leave, no goodbye. The others must
 * see u3 go within a bound and keep hearing each other; the room must not
 * keep u3's slot, so u3 can come straight back on a fresh client and be
 * heard by everyone.
 */

import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3", "u4", "u5"];

export const e5AbruptLoss = e2eScenario({
  id: "e5",
  name: "A participant vanishes from an SFU call",
  description: "Kill a participant's process; the others see it leave and carry on; it rejoins on a fresh client",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ call: inSpace, step, metric }) {
    const call = inSpace("space", "e5", { video: false });

    await step("five join", () => call.join(EVERYONE));
    await step("five settle", async () => {
      await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
      await call.eventually(call.hearsEveryone(), 60_000, "audio");
    });

    await step("u3's process is killed", async () => call.kill("u3"));
    metric("departure_seen_ms", await step("the others see u3 go", () =>
      call.eventually(call.rosterIsEveryone(), 45_000, "roster without u3")));
    await step("the other four still hear each other", () => call.eventually(call.hearsEveryone(), 15_000, "audio among 4"));

    await step("u3 comes back on a fresh client", async () => {
      await call.join(["u3"]);
      await call.eventually(call.rosterIsEveryone(), 60_000, "roster with u3 back");
      await call.eventually(call.hearsEveryone(), 60_000, "audio with u3 back");
    });
    await step("no surviving session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
