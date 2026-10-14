/**
 * E4: many joins and leaves at the same moment.
 *
 * Ten users join at once; then three leave together and rejoin together,
 * three times; then one leaves and rejoins straight away. Each time every
 * roster must settle on exactly who is present and everyone must hear
 * everyone — no participant lost to a race, no stale one kept, and no
 * renegotiation left hanging.
 */

import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10"];
const CHURN = ["u2", "u5", "u8"];

export const e4JoinStorms = e2eScenario({
  id: "e4",
  name: "Join and leave storms in an SFU call",
  description: "10 join at once; 3 leave and rejoin together, 3 times; a leave-and-rejoin at once; rosters and audio settle each time",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "sfu" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e4", { video: false });
    const settled = async (label: string) => {
      await call.eventually(call.rosterIsEveryone(), 60_000, `${label}: roster`);
      await call.eventually(call.hearsEveryone(), 60_000, `${label}: audio`);
    };

    await step("ten join at once", () => call.join(EVERYONE));
    await step("ten settle", () => settled("ten"));

    for (let cycle = 1; cycle <= 3; cycle++) {
      await step(`cycle ${cycle}: three leave together`, async () => {
        await Promise.all(CHURN.map((name) => call.leave(name)));
        await settled(`cycle ${cycle} after leaving`);
      });
      await step(`cycle ${cycle}: the three rejoin together`, async () => {
        await Promise.all(CHURN.map((name) => call.rejoin(name)));
        await settled(`cycle ${cycle} after rejoining`);
      });
    }

    await step("u7 leaves and rejoins at once", async () => {
      await call.leave("u7");
      await call.rejoin("u7");
      await settled("after u7's rejoin");
    });
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
