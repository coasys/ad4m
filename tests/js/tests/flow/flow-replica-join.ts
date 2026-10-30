/**
 * Integration test: an owner that joins a neighbourhood whose flow already
 * has history is caught up by its executor without doing anything (#1424).
 *
 * Two executors. Alice (executor 1) publishes a neighbourhood, adds a flow,
 * starts an instance and settles Ready → Mid. Bob (executor 2) joins
 * afterwards and does nothing — no vote, no read that writes. His
 * executor's sync-triggered pass (about 300 ms after the flow links arrive)
 * is his first over the instance, so it is a catch-up. The test pins that
 * catch-up as Bob's clients see it:
 *
 *  - Bob's FIRST `flow-state-changed` for the instance is the catch-up:
 *    `caughtUp: true`, `from: null`, `firedProposals: []`, `to: "Mid"`.
 *    `from: null` says Bob had no row before it; no normal event comes
 *    first, so the history was not replayed to him as a settle.
 *  - `flowStates()` and `FlowInstance.findAll` then read `Mid` on Bob's side.
 *  - When Alice settles Mid → Done, Bob gets an ordinary event
 *    (`caughtUp: false`, `from: "Mid"`, the settling proposal fired).
 *
 * The moment between the links arriving and the catch-up is a debounce
 * inside the join's own sync, so "not derived yet" is not observable here
 * deterministically. On a single executor it is: see the non-owner reader
 * in `flow-replica-state.test.ts`.
 *
 * Registered in `integration-server-link.test.ts` (server-link-language)
 * and `integration.test.ts` (p-diff-sync); not a `*.test.ts` of its own, as
 * it needs their two executors.
 */
import { expect } from "chai";
import {
  FlowInstance,
  Link,
  LinkQuery,
  Perspective,
  PerspectiveProxy,
  SHACLFlow,
} from "@coasys/ad4m";
import fs from "fs";
import { v4 as uuidv4 } from "uuid";
import { TestContext } from "../test-context";
import { waitUntil } from "../../helpers/index";
import { pollUntil } from "../../utils/utils";
import { LinkLangConfig, holochainLinkLang, publishLinkLanguage } from "../../utils/linkLangConfig";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

/** Ready → Mid → Done, guard-free, each edge settled by one vote. */
function makeFlow(): SHACLFlow {
  const flow = new SHACLFlow("JoinCatchUpFlow", "replica-join-test://");
  flow.consensusRule = { n: 1 };
  flow.addState({ name: "Ready", value: 0 });
  flow.addState({ name: "Mid", value: 1 });
  flow.addState({ name: "Done", value: 2 });
  flow.addTransition({ actionName: "Begin", fromState: "Ready", toState: "Mid", actions: [] });
  flow.addTransition({ actionName: "Finish", fromState: "Mid", toState: "Done", actions: [] });
  return flow;
}

export default function flowReplicaJoinTests(
  testContext: TestContext,
  getLinkLang: () => LinkLangConfig = () => holochainLinkLang(DIFF_SYNC_OFFICIAL),
) {
  return () => {
    describe("Flow replica state on join", function () {
      // Sync warm-up on CI can take minutes (see cross-peer-shape-sync).
      this.timeout(360_000);

      it("a joining owner is caught up without acting, then follows new settles", async () => {
        const alice = testContext.alice;
        const bob = testContext.bob;

        // ── Alice: neighbourhood, flow, an instance with one settled edge ──
        const aliceHandle = await alice.perspective.add(`flow-join-${uuidv4()}`);
        const socialContext = await publishLinkLanguage(alice, getLinkLang(), "flow replica join");
        const url = await alice.neighbourhood.publishFromPerspective(
          aliceHandle.uuid,
          socialContext.address,
          new Perspective(),
        );
        const aliceP = (await alice.perspective.byUUID(aliceHandle.uuid)) as PerspectiveProxy;

        await aliceP.addFlow("JoinCatchUpFlow", makeFlow());
        const subject = `replica-join://task/${uuidv4()}`;
        const inst = await FlowInstance.start(aliceP, "JoinCatchUpFlow", subject);
        const begin = await inst.proposeTransition("Mid");
        expect(begin.outcomes.map((o) => o.toState), "Alice settles Ready → Mid").to.deep.equal([
          "Mid",
        ]);
        // Written last: once Bob holds it, he holds Alice's history before it.
        const marker = `replica-join://marker/${uuidv4()}`;
        await aliceP.add(
          new Link({ source: marker, predicate: "ns://ping", target: "literal:string:ok" }),
        );

        // ── Bob joins, listening from before the join ──────────────────────
        const bobEvents: any[] = [];
        const release = bob.perspective.on("flow-state-changed", (e: any) => bobEvents.push(e));
        // Any call flushes the socket's pending `events.watch` first.
        await bob.agent.me();
        const bobDid = (await bob.agent.me()).did;

        const bobHandle = await bob.neighbourhood.joinFromUrl(url);
        await testContext.makeAllNodesKnown([alice, aliceHandle.uuid], [bob, bobHandle.uuid]);
        const bobP = (await bob.perspective.byUUID(bobHandle.uuid)) as PerspectiveProxy;
        // The payload as the executor sent it: the client adds `type`.
        const forInstance = () =>
          bobEvents
            .filter((e) => e.perspectiveUuid === bobP.uuid && e.instance === inst.uri)
            .map(({ type, ...payload }) => payload);

        await waitUntil(
          async () => (await bobP.get(new LinkQuery({ source: marker }))).length > 0,
          180_000,
          "Alice's history to reach Bob",
        );

        // ── The catch-up, with nothing done by Bob ──────────────────────────
        await waitUntil(() => forInstance().length > 0, 60_000, "Bob's catch-up event");
        const [first] = forInstance();
        expect(first, "Bob's first event for the run is the catch-up").to.deep.equal({
          perspectiveUuid: bobP.uuid,
          agentDid: bobDid,
          instance: inst.uri,
          from: null,
          to: "Mid",
          firedProposals: [],
          caughtUp: true,
        });

        const states = await bobP.flowStates([inst.uri]);
        expect(states.map((s) => [s.instance, s.currentState])).to.deep.equal([[inst.uri, "Mid"]]);
        let bobView: FlowInstance[] = [];
        await pollUntil(
          async () => {
            try {
              bobView = await FlowInstance.findAll(bobP, { subject });
              return bobView.length === 1;
            } catch {
              return false; // the flow's SHACL may still be arriving
            }
          },
          { timeoutMs: 90_000, intervalMs: 2_000, label: "Bob's findAll sees Alice's run" },
        );
        expect(bobView[0].currentStateName, "findAll reads the caught-up state").to.equal("Mid");
        expect(forInstance(), "and nothing was replayed as a settle").to.have.lengthOf(1);

        // ── A new settle is an ordinary event for Bob ───────────────────────
        const finish = await inst.proposeTransition("Done");
        expect(finish.outcomes.map((o) => o.toState)).to.deep.equal(["Done"]);
        await waitUntil(() => forInstance().length > 1, 180_000, "Bob's event for Mid → Done");
        expect(forInstance()[1]).to.deep.equal({
          perspectiveUuid: bobP.uuid,
          agentDid: bobDid,
          instance: inst.uri,
          from: "Mid",
          to: "Done",
          firedProposals: [finish.proposalUri],
          caughtUp: false,
        });
        expect((await bobP.flowStates([inst.uri]))[0].currentState).to.equal("Done");

        release();
      });
    });
  };
}
