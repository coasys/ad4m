/**
 * E24: the SFU sends each receiver what its downlink can carry.
 *
 * Everyone sends its camera as three simulcast layers, and with a free
 * network every receiver gets full size. When one receiver's downlink (and
 * only its downlink) is capped at 1 Mbit/s, the SFU's estimate of that
 * receiver's bandwidth falls and it must forward the low layer (a quarter of
 * the camera's size) to it, within SHAPED_MS, while the others keep full size.
 * Its video keeps arriving and everyone keeps hearing everyone. When the cap
 * is lifted, the estimate climbs and the receiver returns to full size.
 *
 * Needs `sudo -n tc`; skipped without it. Shares E6's host lease.
 */

import { leaseNet, netAvailable, socketPorts } from "../../net.js";
import type { Check } from "../call.js";
import type { Size } from "../protocol.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
/** 720p: libwebrtc drops simulcast layers below its resolution thresholds, and a smaller camera sends fewer. */
const CAMERA: Size = { width: 1280, height: 720 };
const LOW: Size = { width: CAMERA.width / 4, height: CAMERA.height / 4 };
/** The capped receiver's downlink: the low layers of two senders fit, their medium layers do not. */
const CAP_KBIT = 1000;
/** From the cap to the capped receiver on the low layer. */
const SHAPED_MS = 45_000;
/** From the cap's end to full size again: the estimate climbs, then an upgrade holds 4 s per layer. */
const RECOVERY_MS = 90_000;

export const e24ReceiverBandwidth = e2eScenario({
  id: "e24",
  name: "A receiver's bandwidth picks its simulcast layer",
  description: "One receiver's downlink is capped at 1 Mbit/s: it gets the low layer, the others full size, audio keeps flowing; it recovers when the cap goes",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => (netAvailable() ? null : "needs passwordless `sudo -n tc` to shape the loopback interface"),
  async run({ call: inSpace, step, metric }) {
    const net = await leaseNet({ onWait: () => console.log("e24: waiting for another run's impairment on this host to end") });
    const call = inSpace("space", "e24", { video: CAMERA });
    /** `name` receives everyone else's video now, at `size`. */
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
    try {
      await step("three join; with a free network everyone receives full size", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        // A receiver starts on the low layer and climbs as its estimate allows.
        await call.eventually(call.seesEveryone(CAMERA), RECOVERY_MS, "full size");
      });

      metric("capped_to_low_ms", await step(`u2's downlink is capped at ${CAP_KBIT} kbit/s: u2 receives low within ${SHAPED_MS / 1000} s, the others full size`, async () => {
        const media = socketPorts(call.who("u2").pid).filter((s) => s.protocol === "udp");
        if (media.length === 0) throw new Error("u2 holds no media socket to cap");
        net.limitDownlink(media, CAP_KBIT);
        const capped = Date.now();
        await call.eventually(receives("u2", LOW), SHAPED_MS, "u2 at low");
        const took = Date.now() - capped;
        await call.eventually(receives("u1", CAMERA), 10_000, "u1 stays at full size");
        await call.eventually(receives("u3", CAMERA), 10_000, "u3 stays at full size");
        return took;
      }));

      await step("under the cap, video keeps arriving and everyone hears everyone", async () => {
        // The capped receiver's frames keep advancing: `receives` needs recent video from both senders.
        await call.eventually(receives("u2", LOW), 10_000, "u2's video under the cap");
        await call.eventually(call.hearsEveryone(), 30_000, "audio under the cap");
      });

      metric("recovery_to_full_ms", await step("the cap is lifted: u2 receives full size again", async () => {
        net.clear();
        const lifted = Date.now();
        await call.eventually(receives("u2", CAMERA), RECOVERY_MS, "u2 at full size");
        await call.eventually(call.hearsEveryone(), 30_000, "audio after the cap");
        return Date.now() - lifted;
      }));
    } finally {
      await net.release();
    }
  },
});
