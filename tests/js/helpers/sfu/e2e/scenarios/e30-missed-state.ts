/**
 * E30: carol's socket is down while the call changes; only the reply to
 * her `sfu.callResume` may bring her what she missed.
 *
 * alice, bob and carol are in an SFU call. carol's executor WebSocket
 * crosses a `WsShaper` in front of a `TcpShaper`. `blackout()` on the
 * TcpShaper cuts the socket, and her client's redial gets no answer until
 * the link is back (the dial waits, as a SYN retransmits, then connects).
 * Her media is UDP beside it and keeps flowing. Meanwhile dave joins: the
 * room's roster and the executor's offer of his tracks go to carol's
 * socket, which is down. The row checks that dave's join came back inside
 * the blackout, so the change really happened while carol was away.
 *
 * From before the blackout until carol has recovered, the WsShaper eats
 * every `sfu-room-roster` and `sfu-call-renegotiation-offer` event to her:
 * the offer's resends, and anything a resume republishes as events. What is
 * left is the reply to `sfu.callResume` (slice 3), which the WsShaper lets
 * through and reports. So the row needs, within `RESUME_MS` of the link
 * coming back:
 *  - a new socket that answers an RPC;
 *  - a successful `sfu.callResume` reply on it;
 *  - carol and dave listing, hearing and seeing each other, which only that
 *    reply's state can have brought.
 * Nobody may be marked left, and the three first participants' media must
 * flow throughout.
 *
 * Gates slice 3. Skipped while listed in `FAILS_ON_DEV`;
 * `E2E_KNOWN_FAILURES=1` runs it.
 */

import { all } from "../call.js";
import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { RunChecks, TcpShaper, WsShaper, failsOnDev, matchEvent, reconnected, rowPassed } from "../../faults/index.js";

const PRESENT = ["alice", "bob", "carol"];
const EVERYONE = [...PRESENT, "dave"];
/** dave's join must come back inside this. */
const BLACKOUT_MS = 10_000;
/** carol's waiting dial connects at the blackout's end; the resume and its renegotiation fit inside this. */
const RESUME_MS = 15_000;
/** The events that could carry the missed state instead of the resume reply. */
const STATE_EVENTS = ["sfu-room-roster", "sfu-call-renegotiation-offer"];

async function awaitWithin<T>(promise: Promise<T>, ms: number, what: string): Promise<T> {
  let timer: ReturnType<typeof setTimeout>;
  const timed = new Promise<never>((_, reject) => {
    timer = setTimeout(() => reject(new Error(what)), Math.max(1, ms));
  });
  return Promise.race([promise, timed]).finally(() => clearTimeout(timer));
}

export const e30MissedState = e2eScenario({
  id: "e30",
  name: "Missed call state while the socket is down",
  description: "carol's socket is down while dave joins, and roster and offer events to her are eaten while she recovers: only the sfu.callResume reply brings her the roster and his media",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => failsOnDev("e30"),
  async run({ world, call: inSpace, step, metric }) {
    const call = inSpace("space", "e30");
    const link = new TcpShaper(world.users.carol.port);
    await link.listen();
    const frames = new WsShaper(link.port);
    await frames.listen();
    world.users.carol = { ...world.users.carol, port: frames.port };
    let checks: RunChecks | undefined;
    let suppression: ReturnType<WsShaper["dropWhile"]> | undefined;
    const suppressed = () => {
      const counts = new Map<string, number>();
      for (const d of suppression?.dropped ?? []) {
        const type = JSON.parse(d.text).type;
        counts.set(type, (counts.get(type) ?? 0) + 1);
      }
      return [...counts].map(([type, n]) => `${n} ${type}`).join(", ") || "none";
    };
    try {
      await step("three join and settle", async () => {
        await call.join(PRESENT);
        await call.eventually(call.topologyIs("sfu"), 30_000, "topology");
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(CAMERA), 60_000, "video");
      });

      checks = new RunChecks(call, { recoverMs: 20_000, maxDepartures: 0 });
      await checks.start();
      const fault = checks.fault("socket-blackout", ["carol"], { uninterrupted: true });
      suppression = frames.dropWhile({ direction: "down", match: (text, opcode) => STATE_EVENTS.some((name) => matchEvent(name)(text, opcode)) });
      const { reply: resumed } = frames.observeReply("sfu.callResume");
      const blackoutEnds = Date.now() + BLACKOUT_MS;
      const cut = link.blackout(BLACKOUT_MS);
      if (cut.length === 0) throw new Error("carol had no connection to cut");

      await step("dave joins while carol's socket is down", async () => {
        await call.join(["dave"]);
        const left = blackoutEnds - Date.now();
        if (left <= 0) throw new Error(`dave's join came back ${-left}ms after the blackout ended: carol missed nothing`);
        await new Promise((resolve) => setTimeout(resolve, left));
      });

      await step("carol's socket is back and answers", () => reconnected(link, call.who("carol"), cut, RESUME_MS));

      await step(`a successful sfu.callResume reply reaches carol within ${RESUME_MS / 1000} s`, async () => {
        const { reply } = await awaitWithin(
          resumed,
          blackoutEnds + RESUME_MS - Date.now(),
          `no sfu.callResume reply reached carol within ${RESUME_MS}ms of her socket coming back (state events eaten: ${suppressed()})`,
        );
        if (reply.error !== undefined || !("result" in reply)) throw new Error(`sfu.callResume was refused: ${JSON.stringify(reply.error)}`);
      });

      await step("from that reply alone, carol and dave list, hear and see each other", async () => {
        const remaining = Math.max(1, blackoutEnds + RESUME_MS - Date.now());
        try {
          await call.eventually(all(call.rosterIsEveryone(), call.hearsEveryone(), call.seesEveryone(CAMERA)), remaining, "resume");
        } catch (error) {
          throw new Error(`${error instanceof Error ? error.message : error} (state events eaten: ${suppressed()})`);
        }
        fault.end();
      });

      suppression.stop();
      metric("state_events_eaten", suppressed());
      await step("nobody left, and media flowed on every pair throughout", async () => {
        const violations = await checks!.stop();
        if (violations.length) throw new Error(violations.join("\n"));
      });
      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
      rowPassed("e30");
    } finally {
      suppression?.stop();
      await checks?.abort();
      frames.close();
      link.close();
    }
  },
});
