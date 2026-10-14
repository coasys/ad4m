/**
 * E3: changing what you send, mid-call, without anyone rejoining.
 *
 * Mute and unmute the microphone, turn the camera off and on, share a screen
 * and stop: each must reach everyone else, and the call must carry on for
 * all. A participant who joins after the changes gets the current state.
 */

import { CAMERA, SCREEN } from "../media.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3", "u4", "u5"];

export const e3TrackChanges = e2eScenario({
  id: "e3",
  name: "Track changes in an SFU call",
  description: "Mute, unmute, camera off/on, screen share on/off reach everyone; a late joiner gets the current state",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "sfu" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e3");
    const u1 = () => call.who("u1");

    await step("four join", () => call.join(["u1", "u2", "u3", "u4"]));
    await step("everyone hears and sees everyone", async () => {
      await call.eventually(call.hearsEveryone(), 60_000, "audio");
      await call.eventually(call.seesFrom("u1", CAMERA), 60_000, "u1's camera");
    });

    await step("u1 mutes: nobody hears u1, u1 still hears everyone", async () => {
      await u1().mute("audio");
      await call.eventually(call.silentFrom("u1"), 15_000, "silence from u1");
    });
    await step("u1 unmutes: everyone hears u1 again", async () => {
      await u1().unmute("audio");
      await call.eventually(call.hearsEveryone(), 15_000, "audio after unmute");
    });

    await step("u1 turns the camera off and on", async () => {
      await u1().mute("video");
      await call.eventually(call.noVideoFrom("u1"), 15_000, "no video from u1");
      await u1().unmute("video");
      await call.eventually(call.seesFrom("u1", CAMERA), 15_000, "u1's camera back");
    });

    await step("u1 shares a screen; everyone sees it", async () => {
      await u1().shareScreen();
      await call.eventually(call.seesFrom("u1", SCREEN), 20_000, "u1's screen");
    });

    await step("u5 joins while u1 shares: u5 sees the screen and hears everyone", async () => {
      await call.join(["u5"]);
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster with u5");
      await call.eventually(call.hearsEveryone(), 60_000, "audio with u5");
      await call.eventually(call.seesFrom("u1", SCREEN), 30_000, "u1's screen for u5");
    });

    await step("u1 stops sharing; everyone sees the camera again", async () => {
      await u1().stopScreen();
      await call.eventually(call.seesFrom("u1", CAMERA), 20_000, "u1's camera after the share");
    });
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
