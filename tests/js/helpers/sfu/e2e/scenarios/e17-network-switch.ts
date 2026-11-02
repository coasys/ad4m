/**
 * E17: one participant switches networks mid-call (Wi-Fi to another Wi-Fi).
 *
 * Every socket u2's process holds at that moment goes dark for good: its
 * executor WebSocket and the sockets its WebRTC stack gathered on, found from
 * /proc. Sockets it opens afterwards work, as on the new network. Nothing
 * closes the old ones: no FIN or RST ever arrives, so the old WebSocket stays
 * OPEN with nobody answering on it, as a browser keeps it after a network
 * switch. The SDK must notice that by itself, dial a new socket, and rebuild
 * the call over it; with no user action and no page refresh, everyone must
 * hear and see everyone again within RECOVERY_MS.
 *
 * Needs `sudo -n tc`; skipped without it. Shares E6's host lease.
 */

import { leaseNet, netAvailable, socketPorts } from "../../net.js";
import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
/** From the switch to everyone hearing and seeing everyone again. */
const RECOVERY_MS = 90_000;
const debug = (line: string) => {
  if (process.env.E2E_DEBUG) console.log(`[e17] ${new Date().toISOString().slice(11, 23)} ${line}`);
};

export const e17NetworkSwitch = e2eScenario({
  id: "e17",
  name: "One participant switches networks",
  description: "Every socket one participant holds goes dark for good while new ones work: its call comes back with no user action",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => (netAvailable() ? null : "needs passwordless `sudo -n tc` to impair the loopback interface"),
  async run({ call: inSpace, step, metric }) {
    const net = await leaseNet({ onWait: () => console.log("e17: waiting for another run's impairment on this host to end") });
    const call = inSpace("space", "e17");
    try {
      await step("three join and settle", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(CAMERA), 60_000, "video");
      });

      metric("network_switch_recovery_ms", await step(`u2 switches networks: everyone hears and sees everyone again within ${RECOVERY_MS / 1000} s`, async () => {
        const old = socketPorts(call.who("u2").pid);
        // Both must go dark: the executor connection (TCP) and the media path (UDP).
        if (!old.some((s) => s.protocol === "tcp") || !old.some((s) => s.protocol === "udp")) {
          throw new Error(`u2 holds no executor connection or no media socket to cut: ${old.map((s) => `${s.protocol}/${s.port}`).join(" ")}`);
        }
        net.impair(old, { lossPct: 100 });
        const switched = Date.now();
        debug(`u2's old sockets dark: ${old.map((s) => `${s.protocol}/${s.port}`).join(" ")}`);
        await call.eventually(call.noVideoFrom("u2"), 10_000, "u2's switch takes effect");
        const left = () => Math.max(1, RECOVERY_MS - (Date.now() - switched));
        await call.eventually(call.rosterIsEveryone(), left(), "roster after the switch");
        await call.eventually(call.hearsEveryone(), left(), "audio after the switch");
        await call.eventually(call.seesEveryone(CAMERA), left(), "video after the switch");
        return Date.now() - switched;
      }));

      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
    } finally {
      await net.release();
    }
  },
});
