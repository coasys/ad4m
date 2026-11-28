/**
 * E30: carol's socket is down while the call changes; only the reply to
 * her `sfu.callResume` may bring her what she missed. Needs no `sudo`.
 *
 * alice, bob and carol are in an SFU call. carol's executor WebSocket
 * crosses one `WsShaper`. `blackout()` cuts the socket, and her client's
 * redial gets no answer until `restore()` (the dial waits, as a SYN
 * retransmits, then connects). Her media is UDP beside it and keeps
 * flowing. Meanwhile dave joins: the room's roster and the executor's offer
 * of his tracks go to carol's socket, which is down. The blackout lasts
 * until dave's join returns, so the change really happened while carol was
 * away, however slow the host.
 *
 * From before the blackout until carol has recovered, the proxy eats every
 * `sfu-room-roster` and `sfu-call-renegotiation-offer` event to her: the
 * offer's resends, and anything a resume republishes as events. What is
 * left is the reply to `sfu.callResume`, which the proxy lets through and
 * reports. So the row needs, within `RESUME_MS` of the link coming back:
 *  - a new socket that answers an RPC;
 *  - an `sfu.callResume` reply on it that lists her (`listed: true`) and
 *    carries the room's roster, crossing the proxy inside the bound;
 *  - carol and dave listing, hearing and seeing each other, which only that
 *    reply's state can have brought;
 *  - carol's and dave's audio and video frames growing in both directions
 *    over a baseline taken after that, still inside the same bound (a pair
 *    without baseline counters fails).
 * Nobody may be marked left, and the three first participants' media must
 * flow throughout.
 */

import { all, type Check } from "../call.js";
import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { within } from "../time.js";
import { RunChecks, WsShaper, matchEvent, parse, reconnected, replyTo } from "../../faults/index.js";
import { RESUME_MS } from "../../faults/timing.js";

const PRESENT = ["alice", "bob", "carol"];
const EVERYONE = [...PRESENT, "dave"];
/** The events that could carry the missed state instead of the resume reply. */
const STATE_EVENTS = ["sfu-room-roster", "sfu-call-renegotiation-offer"];

export const e30MissedState = e2eScenario({
  id: "e30",
  name: "Missed call state while the socket is down",
  description: "carol's socket is down while dave joins, and roster and offer events to her are eaten while she recovers: only the sfu.callResume reply brings her the roster and his media",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  async run({ world, call: inSpace, step, metric }) {
    const call = inSpace("space", "e30");
    const proxy = new WsShaper(world.users.carol.port);
    await proxy.listen();
    world.routeThrough("carol", proxy.port);
    let checks: RunChecks | undefined;
    let suppression: ReturnType<WsShaper["intercept"]> | undefined;
    const suppressed = () => {
      const counts = new Map<string, number>();
      for (const d of suppression?.seen ?? []) {
        const type = String(parse(d.text)?.type);
        counts.set(type, (counts.get(type) ?? 0) + 1);
      }
      return [...counts].map(([type, n]) => `${n} ${type}`).join(", ") || "none";
    };
    try {
      await step("three join and settle", () => call.settle(PRESENT));

      checks = new RunChecks(call);
      await checks.start();
      const fault = checks.fault("socket-blackout", ["carol"], { uninterrupted: true });
      suppression = proxy.intercept({
        direction: "down",
        match: (text, direction) => STATE_EVENTS.some((name) => matchEvent(name)(text, direction)),
        once: false,
        action: "drop",
      });
      const resumed = proxy.intercept({ direction: "down", match: replyTo("sfu.callResume"), once: true, action: "observe" });
      const cut = proxy.blackout();
      if (cut.length === 0) throw new Error("carol had no connection to cut");

      let linkBack = 0;
      await step("dave joins while carol's socket is down", async () => {
        await call.join(["dave"]);
        linkBack = Date.now();
        proxy.restore();
      });
      const left = () => linkBack + RESUME_MS - Date.now();

      await step("carol's socket is back and answers", () => reconnected(proxy, call.who("carol"), cut, RESUME_MS));

      await step(`a sfu.callResume reply that lists carol and carries the roster reaches her within ${RESUME_MS / 1000} s`, async () => {
        // A reply already seen is judged by its own crossing time below, even when the reconnect step used up the budget.
        const observed = resumed.seen[0] ?? await within(
          left(),
          resumed.first,
          `no sfu.callResume reply reached carol within ${RESUME_MS}ms of her socket coming back (state events eaten: ${suppressed()})`,
        );
        if (observed.at - linkBack > RESUME_MS) throw new Error(`the sfu.callResume reply crossed ${observed.at - linkBack}ms after the link came back, past the ${RESUME_MS}ms bound`);
        const reply = parse(observed.text);
        const result = reply?.result as { listed?: unknown; roster?: unknown } | undefined;
        if (reply?.error !== undefined) throw new Error(`sfu.callResume was refused: ${JSON.stringify(reply.error)}`);
        if (result?.listed !== true || result.roster === undefined) {
          throw new Error(`the sfu.callResume reply did not list carol with the room's roster: ${observed.text.slice(0, 300)}`);
        }
      });

      await step("from that reply alone, carol and dave list, hear and see each other", async () => {
        const remaining = left();
        if (remaining <= 0) throw new Error(`resume: the ${RESUME_MS}ms bound from the link coming back had passed`);
        try {
          await call.eventually(all(call.rosterIsEveryone(), call.hearsEveryone(), call.seesEveryone(CAMERA)), remaining, "resume");
        } catch (error) {
          throw new Error(`${error instanceof Error ? error.message : error} (state events eaten: ${suppressed()})`);
        }
      });

      await step(`carol's and dave's audio and video frames grow both ways within ${RESUME_MS / 1000} s of the link coming back`, async () => {
        const remaining = left();
        if (remaining <= 0) throw new Error(`frames: the ${RESUME_MS}ms bound from the link coming back had passed`);
        const baseline = await call.snapshots();
        const since = Date.now();
        const pairs = [["carol", "dave"], ["dave", "carol"]] as const;
        // A pair with no counters in the baseline is a failure here: framesGrow would skip it.
        const baselined: Check = () => pairs
          .filter(([me, other]) => !baseline.get(me)?.remotes[call.who(other).did])
          .map(([me, other]) => `${me} had no frame counters for ${other} in the baseline`);
        const grow: Check = (snaps, holding) => call.framesGrow(baseline, Date.now() - since, ["carol", "dave"])(snaps, holding);
        await call.eventually(all(baselined, grow, call.hearsEveryone(["carol", "dave"]), call.seesEveryone(CAMERA, ["carol", "dave"])), left(), "frames");
        fault.end();
      });

      suppression.stop();
      metric("state_events_eaten", suppressed());
      await step("nobody left, and media flowed on every pair throughout", async () => {
        const violations = await checks!.stop();
        if (violations.length) throw new Error(violations.join("\n"));
      });
      await step("no session reported an error", () => call.now(call.noErrors(), "errors"));
    } finally {
      suppression?.stop();
      await checks?.abort();
      proxy.close();
    }
  },
});
