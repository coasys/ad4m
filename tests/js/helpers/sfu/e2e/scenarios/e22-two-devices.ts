/**
 * E22: one user in a call from two devices at once.
 *
 * u1 joins from its laptop and from its phone, each sending its own tone.
 * Both are participants: u2 and u3 hear both tones, each of u1's devices
 * hears the other and everyone else, and no device hears itself. When the
 * phone hangs up, the laptop stays in the call.
 */

import { all } from "../call.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];

export const e22TwoDevices = e2eScenario({
  id: "e22",
  name: "One user in a call from two devices",
  description: "A caller on a laptop and a phone is two participants; everyone hears both, no device hears itself, one hangs up alone",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e22", { video: false, clients: ["u1:phone"] });

    await step("u1 joins from its laptop and its phone, u2 and u3 join", async () => {
      await call.join(["u1"], { device: "u1-laptop" });
      await call.join(["u1:phone"], { device: "u1-phone" });
      await call.join(["u2", "u3"]);
    });
    await step("everyone lists both of u1's devices", () => call.eventually(call.rosterIsEveryone(), 30_000, "roster"));
    await step("everyone hears everyone, both of u1's devices included, nobody itself", () =>
      call.eventually(all(call.hearsEveryone(), call.hearsNoEcho()), 30_000, "audio"));

    await step("u1's phone hangs up; the laptop stays", async () => {
      await call.leave("u1:phone");
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster after the phone left");
      await call.eventually(call.hearsEveryone(), 30_000, "audio after the phone left");
    });
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
