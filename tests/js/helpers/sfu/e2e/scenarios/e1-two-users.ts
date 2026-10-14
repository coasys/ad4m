/**
 * E1: two users of one executor in an SFU call, everything an app does.
 *
 * Both are members of a real neighbourhood whose creator chose an SFU mode,
 * and both call through `createSession` as an app does. Each must see the
 * other in the roster, hear the other's own tone, see the other's video,
 * and receive the other's data — string and binary — but not its own. When
 * one leaves, the other's session must say so.
 */

import { e2eScenario } from "../scenario.js";

export const e1TwoUsers = e2eScenario({
  id: "e1",
  name: "Two users in an SFU call",
  description: "Roster, attributed audio, video, data both ways and a leave, through the SDK",
  world: {
    users: [{ name: "ann" }, { name: "ben" }],
    neighbourhoods: [{ name: "space", owner: "ann", members: ["ann", "ben"], call: { mode: "sfu" } }],
  },
  async run({ call: inSpace, step }) {
    const call = inSpace("space", "e1");

    await step("both join", () => call.join(["ann", "ben"]));
    await step("both run the SFU", () => call.eventually(call.topologyIs("sfu"), 15_000, "topology"));
    await step("each lists the other", () => call.eventually(call.rosterIsEveryone(), 30_000, "roster"));
    await step("each hears the other", () => call.eventually(call.hearsEveryone(), 30_000, "audio"));
    await step("each sees the other", () => call.eventually(call.seesEveryone(), 30_000, "video"));

    await step("data reaches the other and only the other", async () => {
      await call.who("ann").sendData("chat", "hello from ann");
      await call.who("ben").sendData("blob", Buffer.from([1, 2, 3]).toString("base64"), true);
      await call.eventually((snapshots) => {
        const problems: string[] = [];
        const ann = snapshots.get("ann")!;
        const ben = snapshots.get("ben")!;
        const annDid = call.who("ann").did;
        const benDid = call.who("ben").did;
        if (!ben.data.some((m) => m.from === annDid && m.label === "chat" && m.data === "hello from ann" && !m.binary)) {
          problems.push("ben has not received ann's string");
        }
        if (!ann.data.some((m) => m.from === benDid && m.label === "blob" && m.binary)) {
          problems.push("ann has not received ben's binary");
        }
        if (ann.data.some((m) => m.from === annDid)) problems.push("ann received her own message back");
        if (ben.data.some((m) => m.from === benDid)) problems.push("ben received his own message back");
        return problems;
      }, 15_000, "data");
    });

    await step("ben leaves; ann's session says so", async () => {
      await call.leave("ben");
      await call.eventually(call.rosterIsEveryone(), 15_000, "roster after ben left");
    });
    await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
  },
});
