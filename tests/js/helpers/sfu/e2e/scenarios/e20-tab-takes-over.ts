/**
 * E20: a second tab on the same device joins the call, and takes it over.
 *
 * u1 is in a call with u2 and u3 from its laptop. Another tab of the same
 * browser (`u1:tab2`) joins the same call. The newest join wins: the first
 * tab's session is replaced — it is told so, ends its call and does not come
 * back on its own — and the others hear u1 once, from the new tab. Then the
 * call holds still: nobody leaves, nobody comes back.
 */

import { all } from "../call.js";
import type { Snapshot } from "../protocol.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
const LAPTOP = "u1-laptop";

/** Why the first tab is not out of the call as a replaced session, or null. */
function replacedProblem(s: Snapshot): string | null {
  if (!s.replaced) return "u1's first tab was not told it was replaced";
  if (s.joined) return `u1's first tab is still in the call (${s.state})`;
  if (s.selectedRemotes.length) return `u1's first tab still holds a connection to ${s.selectedRemotes}`;
  return null;
}

export const e20TabTakesOver = e2eScenario({
  id: "e20",
  name: "A second tab on the same device takes the call over",
  description: "Another tab of a caller on the same device joins; the first is replaced and stays out, the others hear the caller once",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e20", { video: false, clients: ["u1:tab2"] });

    await step("three join, u1 from its laptop", async () => {
      await call.join(["u1"], { device: LAPTOP });
      await call.join(["u2", "u3"]);
    });
    await step("three settle", async () => {
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster");
      await call.eventually(call.hearsEveryone(), 30_000, "audio");
    });

    await step("a second tab on u1's laptop joins", async () => {
      await call.join(["u1:tab2"], { device: LAPTOP });
      call.present.delete("u1");
    });
    await step("the first tab is told it was replaced, and ends its call", async () => {
      const started = Date.now();
      for (let problem = replacedProblem(await call.who("u1").snapshot()); problem; problem = replacedProblem(await call.who("u1").snapshot())) {
        if (Date.now() - started > 15_000) throw new Error(problem);
        await new Promise((r) => setTimeout(r, 500));
      }
    });
    await step("the others hear u1 once, from the new tab", async () => {
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster with the new tab");
      await call.eventually(call.hearsEveryone(), 30_000, "audio from the new tab");
    });

    await step("nobody flaps for 15 s, and the first tab stays out", async () => {
      const settled = await call.snapshots();
      await call.holds(all(call.rosterIsEveryone(), call.hearsEveryone(), call.noDeparturesSince(settled)), 15_000, "call after the takeover");
      const problem = replacedProblem(await call.who("u1").snapshot());
      if (problem) throw new Error(problem);
    });
    await step("no session in the call reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
