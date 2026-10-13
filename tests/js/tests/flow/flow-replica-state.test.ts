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
 * Who is who: Alice is a managed user and the owner of every perspective
 * here. "admin" is the executor's main agent, connected with the admin
 * credential. It is NOT an owner or a member of Alice's perspectives: the
 * credential just lets it open them by UUID. That matters because the
 * executor only runs flow passes for a perspective's owners. For an owner
 * the catch-up is automatic — about 300 ms after flow links sync in, and
 * once at perspective load — so an owner almost never reads an instance
 * as "not derived yet". A non-owner reader has no pass at all until it
 * votes, which makes it the one way, on a single executor, to observe that
 * state on purpose. The second test relies on exactly that; the
 * two-executor join case is in `flow-replica-join.ts`.
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
    // The admin client acts as the main agent. Its credential can open
    // Alice's perspectives by UUID, but it is not their owner or a member, so
    // no flow pass ever runs for it there (see the header).
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

    // Nothing on the graph: not for Alice, not for the admin agent reading
    // her perspective through the admin credential. Before #1424 both were
    // Local links, which every agent with access to the perspective could
    // list.
    const adminP = (await admin.perspective.byUUID(aliceP.uuid)) as PerspectiveProxy;
    for (const p of [aliceP, adminP]) {
      for (const predicate of LEGACY_PREDICATES) {
        expect(await p.get(new LinkQuery({ predicate })), predicate).to.deep.equal([]);
      }
    }

    // The admin agent's bookkeeping is its own, and empty: it can read the
    // perspective, but it is not an owner, so no pass has run for it.
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
  //
  // To SEE "not derived yet" this test reads as the admin agent, which is
  // not an owner of Alice's perspective and only opens it through the admin
  // credential. No pass runs for a non-owner until it votes. For an owner
  // the window is about 300 ms after its flow links sync in, and it is closed
  // again at every perspective load, so on one executor this non-owner setup
  // is the only deterministic way to observe the state. The owner's join
  // case, across two executors, is `flow-replica-join.ts`.
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

    // The admin agent opens Alice's perspective through the admin credential.
    // It is not an owner or a member, so `flow_pass_contexts` never names it
    // and no pass — not the sync-triggered one, not the one at load — has
    // derived this instance for it. That is the only reason it reads
    // `undefined` here; an owner would be caught up within a debounce.
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
