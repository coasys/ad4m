/**
 * E14: callers reach an SFU behind NAT through the address it advertises.
 *
 * The executor binds its SFU on 127.0.0.1, on a fixed port P, and advertises
 * 127.0.0.2 as its public address (`AD4M_SFU_UDP_PORT`,
 * `AD4M_SFU_PUBLIC_ADDR`): its answers offer the host candidate 127.0.0.1:P
 * and the server-reflexive candidate 127.0.0.2:P. On this host, UDP to
 * 127.0.0.2:P is translated to 127.0.0.1:P, and UDP sent to 127.0.0.1:P
 * directly is dropped. A call therefore works only if the callers use the
 * advertised candidate and the SFU accepts what arrives through it — and each
 * caller's selected candidate pair names 127.0.0.2:P.
 *
 * Needs `sudo -n iptables` with the nat table; skipped without it.
 */

import { clearReachOnlyThrough, natAvailable, reachOnlyThrough } from "../../net.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["ann", "ben", "cat"];
const LOCAL = "127.0.0.1";
const PUBLISHED = "127.0.0.2";
/** Fixed for the run; a taken port stops the executor with a message that names it. */
const PORT = 42000 + Math.floor(Math.random() * 1000);
const ADVERTISED = `${PUBLISHED}:${PORT}`;

export const e14AdvertisedAddress = e2eScenario({
  id: "e14",
  name: "SFU reached through its advertised address",
  description: "With the bind address unreachable, callers connect through the advertised public address on the fixed port",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "ann", members: EVERYONE, call: { mode: "designated" } }],
    executorEnv: { AD4M_SFU_UDP_PORT: String(PORT), AD4M_SFU_PUBLIC_ADDR: PUBLISHED },
  },
  unavailable: () => (natAvailable() ? null : "needs passwordless `sudo -n iptables` with the nat table"),
  async run({ call: inSpace, step }) {
    if (!reachOnlyThrough(PUBLISHED, LOCAL, PORT)) throw new Error(`could not install the rules for UDP ${PORT}`);
    try {
      const call = inSpace("space", "e14");
      const throughTheAdvertisedAddress = (snapshots: Map<string, any>) =>
        [...snapshots].flatMap(([name, s]) =>
          s.selectedRemotes.length > 0 && s.selectedRemotes.every((a: string) => a === ADVERTISED)
            ? []
            : [`${name}'s media goes to ${s.selectedRemotes.join(", ") || "nothing"}, not ${ADVERTISED}`]);

      await step("ann and ben join", () => call.join(["ann", "ben"]));
      await step("they hear and see each other", async () => {
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(), 30_000, "video");
      });
      await step(`their media goes through ${ADVERTISED}`, () =>
        call.eventually(throughTheAdvertisedAddress, 10_000, "selected pairs"));
      await step("cat joins; all three hear each other, through the same address", async () => {
        await call.join(["cat"]);
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(throughTheAdvertisedAddress, 10_000, "selected pairs");
      });
      await step("ben leaves and rejoins", async () => {
        await call.leave("ben");
        await call.rejoin("ben");
        await call.eventually(call.hearsEveryone(), 60_000, "audio after the rejoin");
        await call.eventually(throughTheAdvertisedAddress, 10_000, "selected pairs after the rejoin");
      });
      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
    } finally {
      clearReachOnlyThrough(PUBLISHED, LOCAL, PORT);
    }
  },
});
