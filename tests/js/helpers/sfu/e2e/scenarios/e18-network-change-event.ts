/**
 * E18: one participant switches networks mid-call, and its platform says so.
 *
 * E17's cut: every socket u2's process holds goes dark for good, its executor
 * WebSocket and its WebRTC sockets, while sockets it opens afterwards work.
 * Once the old network is gone, u2's process hears `online`, as a browser
 * fires it when the new network comes up. The SDK redials the executor at
 * once, without waiting for its ping deadline, and restarts ICE on the call's
 * connection instead of rebuilding the call. Everyone must hear and see
 * everyone again within RECOVERY_MS of the event.
 *
 * Needs `sudo -n tc`; skipped without it. Shares E6's host lease.
 */

import { leaseNet, netAvailable, socketPorts } from "../../net.js";
import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
/** From the network-change event to everyone hearing and seeing everyone again. */
const RECOVERY_MS = 5_000;
const debug = (line: string) => {
  if (process.env.E2E_DEBUG) console.log(`[e18] ${new Date().toISOString().slice(11, 23)} ${line}`);
};

export const e18NetworkChangeEvent = e2eScenario({
  id: "e18",
  name: "One participant switches networks and hears of it",
  description: "Every socket one participant holds goes dark, then `online` fires: its call comes back within seconds",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => (netAvailable() ? null : "needs passwordless `sudo -n tc` to impair the loopback interface"),
  async run({ call: inSpace, step, metric }) {
    const net = await leaseNet({ onWait: () => console.log("e18: waiting for another run's impairment on this host to end") });
    const call = inSpace("space", "e18");
    try {
      await step("three join and settle", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(CAMERA), 60_000, "video");
      });

      await step("u2's old network goes dark", async () => {
        const old = socketPorts(call.who("u2").pid);
        if (!old.some((s) => s.protocol === "tcp") || !old.some((s) => s.protocol === "udp")) {
          throw new Error(`u2 holds no executor connection or no media socket to cut: ${old.map((s) => `${s.protocol}/${s.port}`).join(" ")}`);
        }
        net.impair(old, { lossPct: 100 });
        debug(`u2's old sockets dark: ${old.map((s) => `${s.protocol}/${s.port}`).join(" ")}`);
        await call.eventually(call.noVideoFrom("u2"), 10_000, "u2's switch takes effect");
      });

      const departuresBefore = (await call.who("u2").snapshot()).departures;
      metric("network_change_recovery_ms", await step(`u2 hears "online": everyone hears and sees everyone again within ${RECOVERY_MS / 1000} s`, async () => {
        const changed = Date.now();
        await call.who("u2").networkChanged();
        debug("u2 told the network changed");
        const left = () => Math.max(1, RECOVERY_MS - (Date.now() - changed));
        await call.eventually(call.rosterIsEveryone(), left(), "roster after the change");
        await call.eventually(call.hearsEveryone(), left(), "audio after the change");
        await call.eventually(call.seesEveryone(CAMERA), left(), "video after the change");
        return Date.now() - changed;
      }));
      // A rebuilt call drops and re-adds every participant; an ICE restart keeps them.
      metric("u2_rebuilds", (await call.who("u2").snapshot()).departures - departuresBefore);

      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
    } finally {
      await net.release();
    }
  },
});
