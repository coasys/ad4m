/**
 * The shape of the lost-signal rows (e27, e28, e29): one message of the
 * renegotiation dave's join starts for carol goes missing, and the call must
 * carry on as if it had not.
 *
 * alice, bob and carol are in an SFU call; carol's executor WebSocket
 * crosses a `WsShaper`, which eats one whole message (dropping bytes would
 * corrupt the stream). Then dave joins: the executor offers carol his
 * tracks (`sfu-call-renegotiation-offer`) and carol answers
 * (`sfu.callAnswerServerOffer`). The renegotiation is driven by a join, not
 * by an ICE restart, and its media needs the offer applied: carol hears and
 * sees dave only through it.
 *
 * Each row asserts, for the affected pair and every other one:
 *  - carol and dave are in each other's roster, and hear and see each
 *    other, within `recoverMs` of the moment the proxy ate the message
 *    (its own timestamp, not the end of dave's join);
 *  - the three already in the call hear and see each other in every sample
 *    from the drop until `holdMs` after it (past the dropped call's 30 s RPC
 *    timeout), so a rebuild a timeout starts is caught;
 *  - nobody records `participant-left`, and dave's media is checked to grow
 *    like everyone's.
 */

import { all } from "../call.js";
import { CAMERA } from "../media.js";
import { e2eScenario, type E2eDefinition } from "../scenario.js";
import { within } from "../time.js";
import { RunChecks, WsShaper, type InterceptedMessage } from "../../faults/index.js";
import { DROP_TIMEOUT_MS } from "../../faults/timing.js";

const PRESENT = ["alice", "bob", "carol"];
const EVERYONE = [...PRESENT, "dave"];

export interface LostSignalRow {
  id: string;
  name: string;
  description: string;
  /** The fault's kind in the checks' report. */
  kind: string;
  /** Arm the drop on carol's proxy; `what` names the message for a timeout. */
  arm(proxy: WsShaper): { first: Promise<InterceptedMessage>; what: string };
  /** Throws when the eaten message is not the one the row means to lose. */
  check?(dropped: InterceptedMessage): void;
  /** carol and dave hear, see and list each other within this of the drop. */
  recoverMs: number;
  /** Watch the call this long after the drop (`timing.ts` names what it must outlast). */
  holdMs: number;
}

export function lostSignalRow(row: LostSignalRow): ReturnType<typeof e2eScenario> {
  const def: E2eDefinition = {
    id: row.id,
    name: row.name,
    description: row.description,
    world: {
      users: EVERYONE.map((name) => ({ name })),
      neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
    },
    async run({ world, call: inSpace, step }) {
      const call = inSpace("space", row.id);
      const proxy = new WsShaper(world.users.carol.port);
      await proxy.listen();
      world.routeThrough("carol", proxy.port);
      let checks: RunChecks | undefined;
      try {
        await step("three join and settle", () => call.settle(PRESENT));

        checks = new RunChecks(call);
        await checks.start();
        const fault = checks.fault(row.kind, ["carol"], { uninterrupted: true });
        const { first, what } = row.arm(proxy);
        let droppedAt = 0;
        const everyone = all(call.rosterIsEveryone(), call.hearsEveryone(), call.seesEveryone(CAMERA));

        await step(`dave joins; ${what} is lost`, async () => {
          // The wait for the drop starts now, beside the join: the drop can come before the join returns.
          const joined = call.join(["dave"]);
          joined.catch(() => {});
          const message = await within(DROP_TIMEOUT_MS, first, `no ${what} crossed carol's proxy`);
          droppedAt = message.at;
          row.check?.(message);
          await joined;
        });

        await step(`carol and dave list, hear and see each other within ${row.recoverMs / 1000} s`, async () => {
          const remaining = row.recoverMs - (Date.now() - droppedAt);
          if (remaining <= 0) throw new Error(`recovery: the ${row.recoverMs}ms bound from the drop had passed when dave's join returned`);
          await call.eventually(everyone, remaining, "recovery");
        });

        await step("the call holds past the dropped call's timeout", async () => {
          await call.holds(everyone, Math.max(0, droppedAt + row.holdMs - Date.now()), "the call after the drop");
          fault.end();
          await call.eventually(everyone, 1, "after the hold");
        });

        await step("nobody left, and media flowed on every pair throughout", async () => {
          const violations = await checks!.stop();
          if (violations.length) throw new Error(violations.join("\n"));
        });
        await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
      } finally {
        await checks?.abort();
        proxy.close();
      }
    },
  };
  return e2eScenario(def);
}
