/**
 * E9: choosing a video quality.
 *
 * Every participant sends its camera as three simulcast layers: full, half
 * and quarter size. A participant who asks for a lower quality must then
 * receive everyone at that layer's size, while the others keep receiving
 * full size — the SFU picks per receiver, not per sender.
 *
 * The camera sends 720p: libwebrtc drops simulcast layers below its
 * resolution thresholds, and a 640x480 camera sends two layers only.
 */

import type { Check } from "../call.js";
import type { Size } from "../protocol.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
const CAMERA: Size = { width: 1280, height: 720 };
const at = (divisor: number): Size => ({ width: CAMERA.width / divisor, height: CAMERA.height / divisor });

export const e9Simulcast = e2eScenario({
  id: "e9",
  name: "Simulcast quality in an SFU call",
  description: "A receiver's quality preference picks the simulcast layer it receives; the others are unaffected",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e9", { video: CAMERA });
    /** `name` receives everyone else at `size`. */
    const receives = (name: string, size: Size): Check => (snapshots) => {
      const snapshot = snapshots.get(name)!;
      const problems: string[] = [];
      for (const other of EVERYONE.filter((n) => n !== name)) {
        const remote = snapshot.remotes[call.who(other).did];
        if (!remote || remote.videoRecent === 0) problems.push(`${name} receives no video from ${other}`);
        else if (remote.width !== size.width || remote.height !== size.height) {
          problems.push(`${name} receives ${other} at ${remote.width}x${remote.height}, expected ${size.width}x${size.height}`);
        }
      }
      return problems;
    };

    await step("three join", () => call.join(EVERYONE));
    await step("everyone receives full size", () => call.eventually(call.seesEveryone(CAMERA), 60_000, "full size"));

    await step("u2 asks for low: u2 receives quarter size", async () => {
      await call.who("u2").quality("low");
      await call.eventually(receives("u2", at(4)), 30_000, "u2 at low");
    });
    await step("u1 and u3 still receive full size", async () => {
      await call.eventually(receives("u1", CAMERA), 10_000, "u1 at full");
      await call.eventually(receives("u3", CAMERA), 10_000, "u3 at full");
    });
    await step("u2 asks for medium: u2 receives half size", async () => {
      await call.who("u2").quality("medium");
      await call.eventually(receives("u2", at(2)), 30_000, "u2 at medium");
    });
    await step("u2 asks for high: u2 receives full size again", async () => {
      await call.who("u2").quality("high");
      await call.eventually(receives("u2", CAMERA), 30_000, "u2 at high");
    });
  },
});
