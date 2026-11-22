/**
 * E26: single WebSocket messages lost on a healthy link.
 *
 * carol's executor WebSocket crosses a `WsShaper`, which parses the framing
 * and eats whole messages — dropping bytes would corrupt the stream, so the
 * loss is message-level, exactly as a packet's loss mid-message looks to a
 * peer that never reorders:
 *
 *  - one `sfu.callAnswerServerOffer` request (client → executor): carol
 *    answers a renegotiation offer and the answer never lands. The server
 *    resends the offer and the resend is answered; the dropped call must not
 *    cost the call — a 30 s RPC timeout read as fatal rebuilds it, and the
 *    rebuild is what the checks fail.
 *  - one `sfu-call-renegotiation-offer` event (executor → client): the offer
 *    never reaches carol; its resend does.
 *
 * Each offer is produced by `networkChanged`: the socket redials and the SDK
 * restarts ICE, which the server answers with a renegotiation offer.
 */

import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { FAILS_ON_DEV, RunChecks, WsShaper, matchEvent, matchRpc } from "../../faults/index.js";

const EVERYONE = ["alice", "bob", "carol"];
/** The dropped call times out after 30 s; the rebuild and rejoin settle inside this. */
const ANSWER_SETTLE_MS = 45_000;
/** An unanswered offer is resent after 10 s, answered, and ICE comes back. */
const OFFER_SETTLE_MS = 18_000;
/** How long to wait for the proxied message to arrive at all. */
const DROP_TIMEOUT_MS = 20_000;

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

async function awaitWithin<T>(promise: Promise<T>, ms: number, what: string): Promise<T> {
  let timer: ReturnType<typeof setTimeout>;
  const timed = new Promise<never>((_, reject) => {
    timer = setTimeout(() => reject(new Error(`after ${ms}ms: ${what}`)), ms);
  });
  return Promise.race([promise, timed]).finally(() => clearTimeout(timer));
}

export const e26LostMessages = e2eScenario({
  id: "e26",
  name: "A lost answer and a lost offer event",
  description: "one sfu.callAnswerServerOffer request and one sfu-call-renegotiation-offer event eaten by the framing proxy: the resend paths carry the call without a rebuild",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => FAILS_ON_DEV["e26"] ?? null,
  async run({ world, call: inSpace, step }) {
    const call = inSpace("space", "e26");
    const proxy = new WsShaper(world.users.carol.port);
    await proxy.listen();
    world.users.carol = { ...world.users.carol, port: proxy.port };
    try {
      await step("three join and settle", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.topologyIs("sfu"), 30_000, "topology");
        await call.eventually(call.rosterIsEveryone(), 60_000, "roster");
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        await call.eventually(call.seesEveryone(CAMERA), 60_000, "video");
      });

      const checks = new RunChecks(call, { recoverMs: 45_000, maxRebuilds: 0 });
      await checks.start();
      const carol = call.who("carol");

      await step("one answer is lost: the offer's resend is answered instead", async () => {
        const fault = checks.fault("lost-answer", ["carol"]);
        const { dropped } = proxy.dropOnce({ direction: "up", match: matchRpc("sfu.callAnswerServerOffer") });
        await carol.networkChanged();
        await awaitWithin(dropped, DROP_TIMEOUT_MS, "no sfu.callAnswerServerOffer request reached the proxy");
        await sleep(ANSWER_SETTLE_MS);
        fault.end();
      });

      await step("one offer event is lost: its resend is answered", async () => {
        const fault = checks.fault("lost-offer", ["carol"]);
        // Offers go only to their target's sockets, so carol's proxy sees only hers.
        const { dropped } = proxy.dropOnce({ direction: "down", match: matchEvent("sfu-call-renegotiation-offer") });
        await carol.networkChanged();
        await awaitWithin(dropped, DROP_TIMEOUT_MS, "no sfu-call-renegotiation-offer event reached the proxy");
        await sleep(OFFER_SETTLE_MS);
        fault.end();
      });

      await step("the call kept carol, and media still flows on every pair", async () => {
        const violations = await checks.stop();
        if (violations.length) throw new Error(violations.join("\n"));
      });
      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
    } finally {
      proxy.close();
    }
  },
});
