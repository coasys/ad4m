/**
 * E8: the limits of one executor's SFU, met cleanly.
 *
 * A full room, a user in too many calls, and a neighbourhood with too many
 * rooms: each joiner past a limit is refused with a reason an app can show,
 * and nobody already in a call is disturbed.
 *
 * - Room: a space's mesh limit sets its rooms' size, four times over; with
 *   a limit of 2, the ninth joiner is refused.
 * - Calls per user: one user may be in four calls on a node at once.
 * - Rooms per neighbourhood: sixteen on a node.
 */

import { e2eScenario } from "../scenario.js";
import type { Call } from "../call.js";

const ROOM = ["u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9"];
const BUSY = "busy";
const HOSTS = ["h1", "h2", "h3", "h4", "h5"];

/** `join` must fail, and say `reason`. */
async function refused(join: Promise<void>, reason: RegExp): Promise<void> {
  try {
    await join;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    if (reason.test(message)) return;
    throw new Error(`refused, but with "${message}" rather than ${reason}`);
  }
  throw new Error(`joined, but should have been refused (${reason})`);
}

export const e8Capacity = e2eScenario({
  id: "e8",
  name: "SFU limits on one executor",
  description: "A full room, a user in too many calls and a neighbourhood with too many rooms each refuse cleanly",
  world: {
    users: [...ROOM, BUSY, ...HOSTS].map((name) => ({ name })),
    neighbourhoods: [
      { name: "small", owner: "u1", members: [...ROOM, BUSY], call: { mode: "designated", maxMeshParticipants: 2 } },
      { name: "rooms", owner: "h1", members: HOSTS, call: { mode: "designated" } },
    ],
  },
  async run({ call: inSpace, step }) {
    const room = inSpace("small", "full", { video: false });

    await step("eight fill a room sized for eight", async () => {
      await room.join(ROOM.slice(0, 8));
      await room.eventually(room.rosterIsEveryone(), 60_000, "roster of 8");
    });
    await step("the ninth is refused: the room is full", () => refused(room.join(["u9"]), /room is full/i));
    await step("the eight are undisturbed", () => room.eventually(room.hearsEveryone(), 30_000, "audio among 8"));

    const calls: Call[] = [];
    await step("one user joins four calls", async () => {
      for (let n = 1; n <= 4; n++) {
        const call = inSpace("small", `busy-${n}`, { video: false });
        calls.push(call);
        await call.join([BUSY]);
      }
    });
    await step("a fifth call is refused", () =>
      refused(inSpace("small", "busy-5", { video: false }).join([BUSY]), /already in 4 calls/i));

    await step("sixteen rooms open in one neighbourhood", async () => {
      for (let n = 0; n < 16; n++) {
        const call = inSpace("rooms", `room-${n}`, { video: false });
        await call.join([HOSTS[Math.floor(n / 4)]]);
      }
    });
    await step("a seventeenth is refused", () =>
      refused(inSpace("rooms", "room-16", { video: false }).join([HOSTS[4]]), /already has 16 rooms/i));
  },
});
