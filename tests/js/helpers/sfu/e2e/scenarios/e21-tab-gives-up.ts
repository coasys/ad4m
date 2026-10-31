/**
 * E21: tabs that give up never touch the call another tab is in.
 *
 * u1 is in a call with u2 and u3 from its laptop. On the same laptop, a second
 * tab opens the space, leaves the call it never joined and closes; a third
 * tab's join fails before it reaches the executor, and that tab closes too —
 * as an app destroys a session whose join failed. Neither may end u1's call:
 * it stays up, and nobody sees anyone leave.
 */

import { all } from "../call.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
const LAPTOP = "u1-laptop";

export const e21TabGivesUp = e2eScenario({
  id: "e21",
  name: "Tabs that give up never end the call another tab is in",
  description: "On a caller's device, a tab leaves without joining and one destroys its session after a failed join; the call is untouched",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e21", { video: false, clients: ["u1:tab2", "u1:tab3"] });
    const steady = () => all(call.rosterIsEveryone(), call.hearsEveryone(), call.noDeparturesSince());

    await step("three join, u1 from its laptop", async () => {
      await call.join(["u1"], { device: LAPTOP });
      await call.join(["u2", "u3"]);
    });
    await step("three settle", async () => {
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster");
      await call.eventually(call.hearsEveryone(), 30_000, "audio");
    });

    await step("a second tab opens the space, leaves without joining, and closes", async () => {
      await call.open("u1:tab2", { device: LAPTOP });
      await call.who("u1:tab2").leave();
      await call.close("u1:tab2");
    });
    await step("a third tab's join fails, and the tab closes", async () => {
      const failed = await call.join(["u1:tab3"], { device: LAPTOP, failOffer: true }).then(() => false, () => true);
      if (!failed) throw new Error("u1:tab3's join was meant to fail");
      await call.close("u1:tab3");
    });

    await step("u1's call stays up for 20 s, nobody's rebuilt", () => call.holds(steady(), 20_000, "call after the tabs gave up"));
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
