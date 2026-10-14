/**
 * E6: a bad network, then no network.
 *
 * Impairs this world's traffic on the loopback — its executor's API and its
 * SFU, so signalling and media alike, as a host's network failing does. Only
 * those ports: other processes on the host (CI runners share it) are not
 * touched.
 *
 * - Loss and jitter (5%, 40 ms ± 20 ms): the call stays up and everyone
 *   keeps hearing everyone.
 * - A 6 s outage, shorter than the server's grace for a disconnected peer:
 *   nobody is dropped; audio comes back on its own.
 * - A 20 s outage, longer than that grace: the server drops everyone, and
 *   every session rejoins by itself once the network is back.
 *
 * Needs `sudo -n tc`; skipped without it. Always clears the impairment, pass or fail.
 */

import { clearNet, netAvailable, setNetemOnPorts, type NetemOptions } from "../../net.js";
import type { Check } from "../call.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3", "u4", "u5", "u6"];
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

export const e6Network = e2eScenario({
  id: "e6",
  name: "SFU call over a bad network",
  description: "Loss and jitter, a short outage within the grace, a long outage past it: the call holds or rebuilds itself",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => (netAvailable() ? null : "needs passwordless `sudo -n tc` to impair the loopback interface"),
  async run({ world, call: inSpace, step, metric }) {
    const ports = await world.ports();
    const setNetem = (opts: NetemOptions) => {
      if (!setNetemOnPorts(ports, opts)) throw new Error(`could not impair ports ${ports.join(", ")}`);
    };
    const call = inSpace("space", "e6", { video: false });
    // An outage that reached nobody would pass every step after it: check that it cut the audio.
    // Frames keep arriving (the receiver conceals loss), so listen for the tones instead.
    const silence: Check = (snapshots) =>
      [...snapshots].flatMap(([name, s]) =>
        Object.values(s.remotes).some((remote) => remote.tone !== null) ? [`${name} still hears a tone`] : []);
    try {
      await step("six join and settle", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
      });

      await step("5% loss, 40 ms ± 20 ms: everyone keeps hearing everyone for 20 s", async () => {
        setNetem({ lossPct: 5, delayMs: 40, jitterMs: 20 });
        for (let n = 0; n < 10; n++) {
          await call.eventually(call.hearsEveryone(), 5_000, `audio under loss, sample ${n + 1}`);
          await sleep(2_000);
        }
        clearNet();
      });

      await step("a 6 s outage: nobody is dropped, audio comes back", async () => {
        const departures = async () => [...(await call.snapshots())].map(([name, s]) => `${name}:${s.departures}`).join(" ");
        const before = await departures();
        setNetem({ lossPct: 100 });
        const cut = Date.now();
        await call.eventually(silence, 5_000, "the outage takes effect");
        await sleep(6_000 - (Date.now() - cut));
        clearNet();
        await call.eventually(call.hearsEveryone(), 20_000, "audio after a short outage");
        await call.eventually(call.rosterIsEveryone(), 1, "roster after a short outage");
        // The same connections carried on: no session saw anyone leave, not even for a moment.
        const after = await departures();
        if (after !== before) throw new Error(`participants left during a short outage: ${before} → ${after}`);
      });

      metric("long_outage_recovery_ms", await step("a 20 s outage: every session rejoins by itself", async () => {
        setNetem({ lossPct: 100 });
        const cut = Date.now();
        await call.eventually(silence, 5_000, "the outage takes effect");
        await sleep(20_000 - (Date.now() - cut));
        clearNet();
        const started = Date.now();
        await call.eventually(call.rosterIsEveryone(), 90_000, "roster after a long outage");
        await call.eventually(call.hearsEveryone(), 60_000, "audio after a long outage");
        return Date.now() - started;
      }));
    } finally {
      clearNet();
    }
  },
});
