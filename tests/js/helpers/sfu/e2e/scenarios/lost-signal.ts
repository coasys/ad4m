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
 *    from the drop until past the dropped call's 30 s RPC timeout (`holdMs`),
 *    so a rebuild a timeout starts is caught;
 *  - nobody records `participant-left`.
 */

import { all } from "../call.js";
import { CAMERA } from "../media.js";
import { e2eScenario, type E2eDefinition } from "../scenario.js";
import { RunChecks, WsShaper, failsOnDev, rowPassed, type DroppedMessage } from "../../faults/index.js";

const PRESENT = ["alice", "bob", "carol"];
const EVERYONE = [...PRESENT, "dave"];
/** How long after the drop is armed (dave's join included) the message must cross the proxy. */
const DROP_TIMEOUT_MS = 30_000;

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

async function awaitWithin<T>(promise: Promise<T>, ms: number, what: string): Promise<T> {
  let timer: ReturnType<typeof setTimeout>;
  const timed = new Promise<never>((_, reject) => {
    timer = setTimeout(() => reject(new Error(`after ${ms}ms: ${what}`)), ms);
  });
  return Promise.race([promise, timed]).finally(() => clearTimeout(timer));
}

export interface LostSignalRow {
  id: string;
  name: string;
  description: string;
  /** The fault's kind in the checks' report. */
  kind: string;
  /** Arm the drop on carol's proxy; `what` names the message for a timeout. */
  arm(proxy: WsShaper): { dropped: Promise<DroppedMessage>; what: string };
  /** Throws when the eaten message is not the one the row means to lose. */
  check?(dropped: DroppedMessage): void;
  /** carol and dave hear, see and list each other within this of the drop. */
  recoverMs: number;
  /** Watch the call this long after the drop: past a dropped call's 30 s timeout and the rebuild it starts. */
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
    unavailable: () => failsOnDev(row.id),
    async run({ world, call: inSpace, step }) {
      const call = inSpace("space", row.id);
      const proxy = new WsShaper(world.users.carol.port);
      await proxy.listen();
      world.users.carol = { ...world.users.carol, port: proxy.port };
      let checks: RunChecks | undefined;
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
        const fault = checks.fault(row.kind, ["carol"], { uninterrupted: true });
        const { dropped, what } = row.arm(proxy);
        let droppedAt = 0;

        await step(`dave joins; ${what} is lost`, async () => {
          // The wait for the drop starts now, beside the join: the drop can come before the join returns.
          const joined = call.join(["dave"]);
          joined.catch(() => {});
          const message = await awaitWithin(dropped, DROP_TIMEOUT_MS, `no ${what} crossed carol's proxy`);
          droppedAt = message.at;
          row.check?.(message);
          await joined;
        });

        await step(`carol and dave list, hear and see each other within ${row.recoverMs / 1000} s`, async () => {
          const remaining = Math.max(1, row.recoverMs - (Date.now() - droppedAt));
          await call.eventually(all(call.rosterIsEveryone(), call.hearsEveryone(), call.seesEveryone(CAMERA)), remaining, "recovery");
        });

        await step("the call holds past the dropped call's timeout", async () => {
          await sleep(Math.max(0, droppedAt + row.holdMs - Date.now()));
          fault.end();
          await call.eventually(all(call.rosterIsEveryone(), call.hearsEveryone(), call.seesEveryone(CAMERA)), 1, "after the hold");
        });

        await step("nobody left, and media flowed on every pair throughout", async () => {
          const violations = await checks!.stop();
          if (violations.length) throw new Error(violations.join("\n"));
        });
        await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
        rowPassed(row.id);
      } finally {
        await checks?.abort();
        proxy.close();
      }
    },
  };
  return e2eScenario(def);
}
