/**
 * Integration test: a flow's per-agent bookkeeping — `perspective.flowStates`,
 * `perspective.flowFired` and the `flow-state-changed` event (#1424).
 *
 * The executor keeps the state it derived for each flow instance, and which
 * proposals it recorded as fired, per agent and outside the perspective. This
 * pins the client surface over that, and that it is the caller's own:
 *
 *  - the agent that settles an edge gets one `flow-state-changed` event, and
 *    reads the new state and the fired proposal back;
 *  - `FlowInstance.findAll` fills `currentStateName` from it;
 *  - nothing of it is on the graph — no link query, by the owner or by another
 *    agent with access to the perspective, returns a state or a mark;
 *  - another agent on the same executor neither receives the event nor reads
 *    the entries.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 900000 --exit \
 *     tests/flow/flow-replica-state.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, FlowInstance, LinkQuery, PerspectiveProxy, SHACLFlow } from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";
import { baseUrl } from "../../utils/utils.js";

/** Ready → Done, guard-free, settled by one vote. */
function makeFlow(): SHACLFlow {
  const flow = new SHACLFlow("ReplicaStateFlow", "replica-state-test://");
  flow.consensusRule = { n: 1 };
  flow.addState({ name: "Ready", value: 0 });
  flow.addState({ name: "Done", value: 1 });
  flow.addTransition({ actionName: "Finish", fromState: "Ready", toState: "Done", actions: [] });
  return flow;
}

const LEGACY_PREDICATES = ["ad4m://flow/current_state", "ad4m://flow/resolved_as"];

describe("flow replica state — flowStates, flowFired, flow-state-changed", function () {
  this.timeout(900_000);

  let agent: AgentHandle;
  let admin: Ad4mClient;
  let alice: Ad4mClient;
  let aliceDid: string, adminDid: string;

  before(async () => {
    agent = await startAgent("flow-replica-state");
    admin = agent.client;
    await admin.runtime.setMultiUserEnabled(true);

    await admin.agent.createUser("alice@replica-state.local", "pass");
    await admin.runtime.setUserFreeAccess("alice@replica-state.local", true);
    const aliceToken = await admin.agent.loginUser("alice@replica-state.local", "pass");
    alice = new Ad4mClient(baseUrl(agent.apiPort), aliceToken);
    aliceDid = (await alice.agent.me()).did;
    // The admin credential can open Alice's perspective and acts as the main
    // agent: a second agent on the same executor with access to her data.
    adminDid = (await admin.agent.me()).did;
    expect(aliceDid).to.not.equal(adminDid);
  });

  after(async () => {
    await agent?.stop();
  });

  it("reports a settle to its agent only, and keeps nothing on the graph", async () => {
    const handle = await alice.perspective.add("replica-state");
    const aliceP = (await alice.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await aliceP.addFlow("ReplicaStateFlow", makeFlow());

    const aliceEvents: any[] = [];
    const release = aliceP.onFlowStateChanged((e) => aliceEvents.push(e));
    // The admin socket watches every perspective, unfiltered.
    const adminEvents: any[] = [];
    const releaseAdmin = admin.perspective.on("flow-state-changed", (e: any) =>
      adminEvents.push(e),
    );
    // Any call flushes the socket's pending `events.watch` first.
    await admin.agent.me();

    const inst = await FlowInstance.start(aliceP, "ReplicaStateFlow", "replica-state://task/1");
    expect(inst.currentStateName, "a run starts at the flow's first state").to.equal("Ready");

    const result = await inst.proposeTransition("Done");
    expect(result.outcomes, "n:1 settles on Alice's own vote").to.have.lengthOf(1);
    expect(
      result.outcomes[0].toState,
      "and is reported: the start seeded Alice's row, so this is not a catch-up",
    ).to.equal("Done");

    await waitUntil(() => aliceEvents.length > 0, 10_000, "Alice's flow-state-changed");
    expect(aliceEvents).to.have.lengthOf(1);
    expect(aliceEvents[0]).to.include({
      perspectiveUuid: aliceP.uuid,
      agentDid: aliceDid,
      instance: inst.uri,
      from: "Ready",
      to: "Done",
    });
    expect(aliceEvents[0].firedProposals).to.deep.equal([result.proposalUri]);

    const states = await aliceP.flowStates();
    expect(states.map((s) => [s.instance, s.currentState])).to.deep.equal([[inst.uri, "Done"]]);
    expect(Number.isNaN(Date.parse(states[0].derivedAt)), "derivedAt is a timestamp").to.equal(
      false,
    );
    expect(await aliceP.flowStates(["replica-state://not-an-instance"])).to.deep.equal([]);
    expect(await aliceP.flowFired(inst.uri)).to.deep.equal([result.proposalUri]);

    const [reloaded] = await FlowInstance.findAll(aliceP, { subject: "replica-state://task/1" });
    expect(reloaded.currentStateName, "findAll reads the recorded state").to.equal("Done");

    // Nothing on the graph: not for Alice, not for an agent that can read
    // her perspective. Before #1424 both were Local links, which every agent
    // with access to the perspective could list.
    const adminP = (await admin.perspective.byUUID(aliceP.uuid)) as PerspectiveProxy;
    for (const p of [aliceP, adminP]) {
      for (const predicate of LEGACY_PREDICATES) {
        expect(await p.get(new LinkQuery({ predicate })), predicate).to.deep.equal([]);
      }
    }

    // The other agent's bookkeeping is its own: empty, though it can read
    // the perspective.
    expect(await adminP.flowStates()).to.deep.equal([]);
    expect(await adminP.flowFired(inst.uri)).to.deep.equal([]);

    // And it never saw Alice's event. Give a misdelivery time to arrive.
    await new Promise((r) => setTimeout(r, 1500));
    expect(
      adminEvents.filter((e) => e.perspectiveUuid === aliceP.uuid),
      "Alice's event reaches Alice's sockets only",
    ).to.deep.equal([]);
    expect(aliceEvents, "and exactly once").to.have.lengthOf(1);

    release();
    releaseAdmin();
    await alice.perspective.remove(aliceP.uuid);
  });

  // An agent's first pass over an instance with history is a catch-up: it
  // reports no settles, but it does change what that agent reads, so its
  // clients are told (`caughtUp`). Until then the instance is not derived
  // for that agent, and the wrapper says so instead of guessing the first
  // state.
  it("an instance not derived here reads as unknown; the catch-up is published", async () => {
    const handle = await alice.perspective.add("replica-state-catch-up");
    const aliceP = (await alice.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    const flow = new SHACLFlow("CatchUpFlow", "replica-state-test://");
    flow.consensusRule = { n: 1 };
    flow.addState({ name: "Ready", value: 0 });
    flow.addState({ name: "Mid", value: 1 });
    flow.addState({ name: "Done", value: 2 });
    flow.addTransition({ actionName: "Begin", fromState: "Ready", toState: "Mid", actions: [] });
    flow.addTransition({ actionName: "Finish", fromState: "Mid", toState: "Done", actions: [] });
    await aliceP.addFlow("CatchUpFlow", flow);

    const subject = "replica-state://task/catch-up";
    const inst = await FlowInstance.start(aliceP, "CatchUpFlow", subject);
    const [minted] = await FlowInstance.findAll(aliceP, { subject });
    expect(minted.currentStateName, "Alice minted it: it stands at the first state").to.equal(
      "Ready",
    );
    expect((await inst.proposeTransition("Mid")).outcomes).to.have.lengthOf(1);

    const adminEvents: any[] = [];
    const releaseAdmin = admin.perspective.on("flow-state-changed", (e: any) =>
      adminEvents.push(e),
    );
    await admin.agent.me();

    // The admin agent can read Alice's perspective but is not an owner: no
    // pass of Alice's runs for it, so it has never derived this instance.
    const adminP = (await admin.perspective.byUUID(aliceP.uuid)) as PerspectiveProxy;
    const [unseen] = await FlowInstance.findAll(adminP, { subject });
    expect(unseen.currentStateName, "not derived by this agent yet: unknown, not `Ready`").to.be
      .undefined;
    expect(unseen.availableTransitions).to.deep.equal([]);

    // Its first vote catches it up first (silently for outcomes), then
    // reports exactly what the vote settled.
    const result = await unseen.proposeTransition("Done");
    expect(result.outcomes.map((o) => [o.fromState, o.toState])).to.deep.equal([["Mid", "Done"]]);

    await waitUntil(() => adminEvents.length >= 2, 10_000, "the admin agent's two events");
    const mine = adminEvents.filter((e) => e.perspectiveUuid === aliceP.uuid);
    expect(mine.map((e) => [e.from, e.to, e.caughtUp, e.firedProposals])).to.deep.equal([
      [null, "Mid", true, []],
      ["Mid", "Done", false, [result.proposalUri]],
    ]);

    releaseAdmin();
    await alice.perspective.remove(aliceP.uuid);
  });
});
