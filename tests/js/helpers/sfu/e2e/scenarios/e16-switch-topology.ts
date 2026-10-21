/**
 * E16: a live call switches from the SFU to the mesh and back.
 *
 * As WE switches: every participant destroys its session and creates one on
 * the other topology, joining it with the same local stream. They do not
 * switch at once — each follows within about two seconds of the first. After
 * each switch everyone must hear and see everyone on the new topology.
 */

import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import type { Call } from "../call.js";

const EVERYONE = ["u1", "u2", "u3"];
/** When each participant switches, after the first: the others follow within seconds. */
const STAGGER_MS = [0, 1_200, 2_000];
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

async function switchAll(call: Call, topology: "mesh" | "sfu"): Promise<void> {
  await Promise.all(EVERYONE.map(async (name, i) => {
    await sleep(STAGGER_MS[i]);
    await call.switchTo(name, topology);
  }));
}

export const e16SwitchTopology = e2eScenario({
  id: "e16",
  name: "Switch a live call between SFU and mesh",
  description: "Three on the SFU switch to the mesh, staggered, then back: media flows both ways after each switch",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ call: inSpace, step, metric }) {
    const call = inSpace("space", "e16");
    const everyone = async (topology: string, timeoutMs: number) => {
      await call.eventually(call.topologyIs(topology), timeoutMs, `topology ${topology}`);
      await call.eventually(call.rosterIsEveryone(), timeoutMs, `roster on ${topology}`);
      await call.eventually(call.hearsEveryone("shown"), timeoutMs, `audio on ${topology}`);
      return call.eventually(call.seesEveryone(CAMERA, "shown"), timeoutMs, `video on ${topology}`);
    };

    await step("three call on the SFU", async () => {
      await call.join(EVERYONE);
      await everyone("sfu", 60_000);
    });

    metric("to_mesh_ms", await step("everyone switches to the mesh: everyone hears and sees everyone", async () => {
      const started = Date.now();
      await switchAll(call, "mesh");
      await everyone("mesh", 60_000);
      return Date.now() - started;
    }));

    metric("to_sfu_ms", await step("everyone switches back to the SFU: everyone hears and sees everyone", async () => {
      const started = Date.now();
      await switchAll(call, "sfu");
      await everyone("sfu", 60_000);
      return Date.now() - started;
    }));

    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
