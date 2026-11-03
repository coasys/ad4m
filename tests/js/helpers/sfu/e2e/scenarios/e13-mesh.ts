/**
 * E13: a mesh call through the SDK, on one executor.
 *
 * A space whose creator chose mesh: four users call peer to peer. The
 * roster comes from their online statuses in the neighbourhood, so the
 * executor must keep each co-located user's status apart. Everyone must
 * hear and see everyone; one leaves and the others' rosters follow.
 */

import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3", "u4"];

export const e13Mesh = e2eScenario({
  id: "e13",
  name: "Mesh call on one executor",
  description: "Four users, peer to peer: roster from presence, attributed audio, video, a leave",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "mesh" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e13");

    await step("four join", () => call.join(EVERYONE));
    await step("all four run the mesh", () => call.eventually(call.topologyIs("mesh"), 30_000, "topology"));
    await step("four rosters", () => call.eventually(call.rosterIsEveryone(), 60_000, "roster"));
    await step("four hear each other", () => call.eventually(call.hearsEveryone(), 90_000, "audio"));
    await step("four see each other", () => call.eventually(call.seesEveryone(CAMERA), 90_000, "video"));

    await step("u4 leaves; the others' rosters drop u4", async () => {
      await call.leave("u4");
      await call.eventually(call.rosterIsEveryone(), 60_000, "roster after u4 left");
    });
    await step("the other three still hear each other", () => call.eventually(call.hearsEveryone(), 30_000, "audio among 3"));
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
