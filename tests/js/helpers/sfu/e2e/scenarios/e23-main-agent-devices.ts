/**
 * E23: two devices of the executor's main agent in one mesh call.
 *
 * The main agent calls from its laptop and its phone. A device hears its
 * sibling only if the executor routes the main agent's broadcasts back to
 * the main agent: before, it skipped that copy, so the siblings never
 * exchanged offers or beats, and each wrote the other out of the status as
 * stale after about 90 s. Both must stay listed past that bound and hear
 * each other.
 *
 * No managed user joins: an executor lists its co-located agents in
 * `onlineAgents` from its managed users only, so a managed user's roster
 * never shows the main agent, a gap this scenario does not cover.
 */

import { all } from "../call.js";
import { e2eScenario } from "../scenario.js";

/** Past the 90 s a reader keeps a device it has not heard, with room for a poll and a repair write. */
const PAST_STALE_MS = 120_000;

export const e23MainAgentDevices = e2eScenario({
  id: "e23",
  name: "Main agent in a mesh call from two devices",
  description: "The main agent's laptop and phone hear each other and stay listed past the stale bound",
  world: {
    users: [{ name: "op", main: true }],
    neighbourhoods: [{ name: "space", owner: "op", members: ["op"], call: { mode: "mesh" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e23", { video: false, clients: ["op:phone"] });

    await step("the main agent joins from its laptop and its phone", async () => {
      await call.join(["op"], { device: "op-laptop" });
      await call.join(["op:phone"], { device: "op-phone" });
    });
    await step("both run the mesh", () => call.eventually(call.topologyIs("mesh"), 30_000, "topology"));
    await step("each device lists the other", () => call.eventually(call.rosterIsEveryone(), 60_000, "roster"));
    await step("each device hears the other, neither itself", () =>
      call.eventually(all(call.hearsEveryone(), call.hearsNoEcho()), 90_000, "audio"));
    await step("both stay listed and heard past the stale bound", () =>
      call.holds(all(call.rosterIsEveryone(), call.hearsEveryone()), PAST_STALE_MS, "past the stale bound"));
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
