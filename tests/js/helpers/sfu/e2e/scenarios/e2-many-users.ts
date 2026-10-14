/**
 * E2: a call that outgrows a mesh, on one executor's SFU.
 *
 * Eight users join at once, then four more one at a time. At every point
 * each participant must list exactly the others and hear each of them —
 * attributed: the audio the SDK assigns to a participant must carry that
 * participant's own tone, so a track attached to the wrong DID fails here.
 * One participant then leaves and comes back.
 */

import { e2eScenario } from "../scenario.js";

const FIRST = ["u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8"];
const LATE = ["u9", "u10", "u11", "u12"];
const EVERYONE = [...FIRST, ...LATE];

export const e2ManyUsers = e2eScenario({
  id: "e2",
  name: "Twelve users in an SFU call",
  description: "8 join at once, 4 late; roster, attributed audio and video for every pair; a leave and a rejoin",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "sfu", maxMeshParticipants: 6 } }],
  },
  async run({ call: inSpace, step, metric }) {
    const call = inSpace("space", "e2");

    await step("eight join at once", () => call.join(FIRST));
    await step("all eight run the SFU", () => call.eventually(call.topologyIs("sfu"), 30_000, "topology"));
    metric("roster_8_ms", await step("eight rosters", () => call.eventually(call.rosterIsEveryone(), 60_000, "roster of 8")));
    metric("audio_8_ms", await step("eight hear each other", () => call.eventually(call.hearsEveryone(), 60_000, "audio among 8")));
    await step("eight see each other", () => call.eventually(call.seesEveryone(), 60_000, "video among 8"));

    await step("four join late, one at a time", () => call.join(LATE, { oneByOne: true }));
    metric("roster_12_ms", await step("twelve rosters", () => call.eventually(call.rosterIsEveryone(), 60_000, "roster of 12")));
    metric("audio_12_ms", await step("twelve hear each other", () => call.eventually(call.hearsEveryone(), 90_000, "audio among 12")));
    await step("twelve see each other", () => call.eventually(call.seesEveryone(), 90_000, "video among 12"));

    await step("u5 leaves; everyone's roster drops u5", async () => {
      await call.leave("u5");
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster after u5 left");
    });
    await step("the other eleven still hear each other", () => call.eventually(call.hearsEveryone(), 30_000, "audio among 11"));

    await step("u5 rejoins", async () => {
      await call.rejoin("u5");
      await call.eventually(call.rosterIsEveryone(), 60_000, "roster after u5 rejoined");
      await call.eventually(call.hearsEveryone(), 60_000, "audio after u5 rejoined");
    });
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
