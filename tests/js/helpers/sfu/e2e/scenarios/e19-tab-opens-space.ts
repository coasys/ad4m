/**
 * E19: a second tab shows the space but stays out of the call.
 *
 * u1 is in a call with u2 and u3 from its laptop. Another tab of the same
 * browser (`u1:tab2`, the same device) opens the space's call session and
 * never joins; later it leaves the call it is not in, and closes. A tab that
 * is not in the call must never change the call's session: everyone keeps
 * hearing everyone, and nobody's call is rebuilt.
 */

import { all } from "../call.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
const LAPTOP = "u1-laptop";

export const e19TabOpensSpace = e2eScenario({
  id: "e19",
  name: "A second tab opens the space but stays out of the call",
  description: "Another tab of a caller on the same device opens the session, leaves without joining and closes; the call is untouched",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e19", { video: false, clients: ["u1:tab2"] });
    const steady = () => all(call.rosterIsEveryone(), call.hearsEveryone(), call.noDeparturesSince());

    await step("three join, u1 from its laptop", async () => {
      await call.join(["u1"], { device: LAPTOP });
      await call.join(["u2", "u3"]);
    });
    await step("three settle", async () => {
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster");
      await call.eventually(call.hearsEveryone(), 30_000, "audio");
    });

    await step("a second tab on u1's laptop opens the space", () => call.open("u1:tab2", { device: LAPTOP }));
    await step("the call carries on for 15 s, nobody's rebuilt", () => call.holds(steady(), 15_000, "call with a second tab open"));

    await step("the second tab leaves the call it is not in, and closes", async () => {
      await call.who("u1:tab2").leave();
      await call.close("u1:tab2");
    });
    await step("the call carries on for 10 s more", () => call.holds(steady(), 10_000, "call after the second tab closed"));
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
