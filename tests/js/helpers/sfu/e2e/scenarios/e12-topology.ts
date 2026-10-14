/**
 * E12: the space's call policy decides the topology, for everyone alike.
 *
 * Four spaces on one executor, two users in each, both calling the way an
 * app does. A space whose creator chose "sfu" runs on the SFU; one that
 * chose "mesh" runs peer to peer. "auto", chosen or by default (no policy),
 * runs mesh here: every member is hosted on this executor, but an SFU bound
 * to loopback is not public. Both users of a space must always land on the
 * same topology — a split call is the failure this guards.
 */

import { e2eScenario } from "../scenario.js";

const SPACES: { name: string; mode?: "sfu" | "mesh" | "auto"; expected: "sfu" | "mesh" }[] = [
  { name: "sfu", mode: "sfu", expected: "sfu" },
  { name: "mesh", mode: "mesh", expected: "mesh" },
  { name: "auto", mode: "auto", expected: "mesh" },
  { name: "unset", expected: "mesh" },
];

export const e12Topology = e2eScenario({
  id: "e12",
  name: "Topology follows the space's policy",
  description: "sfu runs on the SFU; mesh, and auto with a loopback SFU, run mesh; both users of a space always agree",
  world: {
    users: SPACES.flatMap((s) => [{ name: `${s.name}-a` }, { name: `${s.name}-b` }]),
    neighbourhoods: SPACES.map((s) => ({
      name: s.name,
      owner: `${s.name}-a`,
      members: [`${s.name}-a`, `${s.name}-b`],
      ...(s.mode ? { call: { mode: s.mode } } : {}),
    })),
  },
  async run({ call: inSpace, step }) {
    for (const space of SPACES) {
      await step(`${space.name}: both users run ${space.expected} and hear each other`, async () => {
        const call = inSpace(space.name, `e12-${space.name}`, { video: false });
        await call.join([`${space.name}-a`, `${space.name}-b`]);
        await call.eventually(call.topologyIs(space.expected), 30_000, "topology");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
      });
    }
  },
});
