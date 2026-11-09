/**
 * E10: app data through an SFU call — reactions, hand raises, chat.
 *
 * Six users each send a string and a binary message. Every other
 * participant must receive each exactly once, from the right sender, intact;
 * nobody receives their own back; a participant who left receives nothing.
 */

import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3", "u4", "u5", "u6"];

export const e10Data = e2eScenario({
  id: "e10",
  name: "Data in an SFU call",
  description: "String and binary data from six users reach every other participant once, intact, never echoed",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e10", { video: false });
    const did = (name: string) => world.users[name].did;

    await step("six join", () => call.join(EVERYONE));
    await step("rosters complete", () => call.eventually(call.rosterIsEveryone(), 60_000, "roster"));

    await step("u6 leaves before anyone sends", async () => {
      await call.leave("u6");
      await call.eventually(call.rosterIsEveryone(), 30_000, "roster without u6");
    });
    const senders = EVERYONE.filter((n) => n !== "u6");

    await step("each sends a string and a binary message", async () => {
      for (const name of senders) {
        await call.who(name).sendData("chat", `hello from ${name}`);
        await call.who(name).sendData("bytes", Buffer.from(name).toString("base64"), true);
      }
    });

    await step("everyone gets every other message once, intact; nobody gets their own", () =>
      call.eventually((snapshots) => {
        const problems: string[] = [];
        for (const [me, snapshot] of snapshots) {
          for (const from of senders) {
            const got = snapshot.data.filter((m) => m.from === did(from));
            if (from === me) {
              if (got.length) problems.push(`${me} received ${got.length} of their own messages`);
              continue;
            }
            const strings = got.filter((m) => m.label === "chat" && !m.binary);
            const bytes = got.filter((m) => m.label === "bytes" && m.binary);
            if (strings.length !== 1 || strings[0].data !== `hello from ${from}`) {
              problems.push(`${me} has ${strings.length} string(s) from ${from}`);
            }
            if (bytes.length !== 1 || Buffer.from(bytes[0].data, "base64").toString() !== from) {
              problems.push(`${me} has ${bytes.length} intact binary message(s) from ${from}`);
            }
          }
        }
        return problems;
      }, 30_000, "data"));

    await step("u6, who left, received nothing", async () => {
      const u6 = await call.who("u6").snapshot();
      if (u6.data.length) throw new Error(`u6 received ${u6.data.length} message(s) after leaving`);
    });
  },
});
