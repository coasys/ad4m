/**
 * E11: a user of the same executor who is not in the space.
 *
 * Ann and Ben call in their neighbourhood. Eve shares their executor but is
 * not a member. Eve plays an attacker, not an app: she calls the executor's
 * call RPCs with her own token for a neighbourhood she has no perspective
 * of — the only way a non-member could try. Every attempt must be refused,
 * and none of the call's events (roster, data) may reach her, while Ann and
 * Ben carry on.
 */

import type { SfuDataMessage, SfuRoomRoster } from "@coasys/ad4m";
import { e2eScenario } from "../scenario.js";

export const e11Outsider = e2eScenario({
  id: "e11",
  name: "An outsider on the same executor",
  description: "A non-member cannot join, list or observe a call; members are undisturbed",
  world: {
    users: [{ name: "ann" }, { name: "ben" }, { name: "eve" }],
    neighbourhoods: [{ name: "space", owner: "ann", members: ["ann", "ben"], call: { mode: "sfu" } }],
  },
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e11");
    const url = world.url("space");
    const eve = world.users.eve.client;
    const seen: { rosters: SfuRoomRoster[]; data: SfuDataMessage[] } = { rosters: [], data: [] };
    const stopRosters = eve.neighbourhood.subscribeSfuRoomRoster((r) => seen.rosters.push(r));
    const stopData = eve.neighbourhood.subscribeSfuDataChannel((d) => seen.data.push(d));

    await step("ann and ben call", async () => {
      await call.join(["ann", "ben"]);
      await call.eventually(call.hearsEveryone(), 60_000, "audio");
    });

    const attempts: [string, () => Promise<unknown>][] = [
      ["join the room", () => eve.neighbourhood.sfuCallJoin(url, "e11", "{}")],
      ["read the call config", () => eve.neighbourhood.sfuGetConfig(url)],
      ["send data into the room", () => eve.neighbourhood.sfuSendData(url, "e11", "chat", "hi")],
    ];
    for (const [what, attempt] of attempts) {
      await step(`eve cannot ${what}`, async () => {
        const outcome = await attempt().then(() => "allowed", (e: Error) => e.message);
        if (outcome === "allowed" || !/member|forbidden|not allowed|denied|403/i.test(outcome)) {
          throw new Error(`eve tried to ${what}: ${outcome}`);
        }
      });
    }
    await step("eve's room list has none of the space's rooms", async () => {
      const rooms = await eve.neighbourhood.sfuListRooms().catch(() => []);
      const leaked = rooms.filter((r: { neighbourhoodUrl?: string }) => r.neighbourhoodUrl === url);
      if (leaked.length) throw new Error(`eve sees ${leaked.length} room(s) of the space`);
    });

    await step("the call's events never reach eve", async () => {
      await call.who("ann").sendData("chat", "members only");
      await call.leave("ben");
      await call.rejoin("ben");
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster");
      await new Promise((r) => setTimeout(r, 2000));
      if (seen.rosters.length) throw new Error(`eve received ${seen.rosters.length} roster event(s)`);
      if (seen.data.length) throw new Error(`eve received ${seen.data.length} data message(s)`);
    });
    await step("ann and ben still hear each other", () => call.eventually(call.hearsEveryone(), 30_000, "audio"));
    stopRosters();
    stopData();
  },
});
