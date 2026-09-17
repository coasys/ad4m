/**
 * Integration test: `perspective.proposeFlowTransition` — manual proposals, no roles.
 *
 * Exercises the new `FlowInstance.proposeTransition(toState, rationale?)` API
 * across a four-state task flow with a guard-free fifth "Cancelled" state.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 900000 --exit \
 *     tests/model/flow-propose.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy, SHACLFlow } from "@coasys/ad4m";
import { FlowInstance } from "@coasys/ad4m";
import type { ConsensusRule } from "@coasys/ad4m";
import { Ad4mModel, Model, Property } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";
import { baseUrl } from "../../utils/utils.js";

// ── Domain models ──────────────────────────────────────────────────────────

@Model({ name: "ProposeTask" })
class Task extends Ad4mModel {
  @Property({ through: "propose-test://title", required: true })
  title: string = "";
}

@Model({ name: "ProposeWorkLog" })
class WorkLog extends Ad4mModel {
  @Property({ through: "propose-test://worklog_note", required: true })
  note: string = "";
}

@Model({ name: "ProposeDeliverable" })
class Deliverable extends Ad4mModel {
  @Property({ through: "propose-test://deliverable_title", required: true })
  title: string = "";
}

@Model({ name: "ProposeReviewNote" })
class ReviewNote extends Ad4mModel {
  @Property({ through: "propose-test://reviewnote_body", required: true })
  body: string = "";
}

// ── Flow factory ───────────────────────────────────────────────────────────

/**
 * Ready(0) → InProgress(1, WorkLog guard) → InReview(2, Deliverable guard)
 *   → Done(3, Deliverable+ReviewNote guard, doneRule)
 *
 * Plus: Ready → Cancelled(10, no guard, n:1) — tests the guard-free seal path
 * (commit A: `EvidenceSeal::NoGuard` lets `acceptProposal` co-sign on a
 * guard-less target state).
 */
function makeFlow(doneRule: ConsensusRule): SHACLFlow {
  const flow = new SHACLFlow("ProposeTaskFlow", "propose-test://");
  flow.inputTypes = ["ProposeTask"];
  flow.consensusRule = { n: 1 };

  flow.addState({ name: "Ready", value: 0 });
  flow.addState({
    name: "InProgress",
    value: 1,
    requires: [{ className: "ProposeWorkLog", linkedTo: "base", count: { min: 1 } }],
    consensusRule: { n: 1 },
  });
  flow.addState({
    name: "InReview",
    value: 2,
    requires: [{ className: "ProposeDeliverable", linkedTo: "base", count: { min: 1 } }],
    consensusRule: { n: 1 },
  });
  flow.addState({
    name: "Done",
    value: 3,
    requires: [
      { className: "ProposeDeliverable", linkedTo: "base", count: { min: 1 } },
      { className: "ProposeReviewNote", linkedTo: "base", count: { min: 1 } },
    ],
    consensusRule: doneRule,
  });
  // Guard-free state: tests that commit A's NoGuard seal lets accept work.
  flow.addState({ name: "Cancelled", value: 10 });

  flow.addTransition({ actionName: "Start", fromState: "Ready", toState: "InProgress", actions: [] });
  flow.addTransition({ actionName: "SubmitForReview", fromState: "InProgress", toState: "InReview", actions: [] });
  flow.addTransition({ actionName: "Accept", fromState: "InReview", toState: "Done", actions: [] });
  flow.addTransition({ actionName: "Cancel", fromState: "Ready", toState: "Cancelled", actions: [] });
  return flow;
}

const HAS_CHILD = "ad4m://has_child";

async function createUnder<T extends Ad4mModel>(
  model: any,
  p: PerspectiveProxy,
  parentUri: string,
  data: Record<string, any>,
): Promise<T> {
  return (await model.create(p, data, {
    parent: { id: parentUri, predicate: HAS_CHILD },
  })) as T;
}

async function instanceOn(p: PerspectiveProxy, subject: string): Promise<FlowInstance> {
  const found = await FlowInstance.findAll(p, { subject });
  expect(found, `one FlowInstance on ${subject}`).to.have.lengthOf(1);
  return found[0];
}

// ── Suite ──────────────────────────────────────────────────────────────────

describe("proposeFlowTransition — manual proposals, no roles", function () {
  this.timeout(900_000);

  let agent: AgentHandle;
  let admin: Ad4mClient;
  let alice: Ad4mClient, bob: Ad4mClient;
  let aliceDid: string, bobDid: string;

  before(async () => {
    agent = await startAgent("flow-propose-no-roles");
    admin = agent.client;
    await admin.runtime.setMultiUserEnabled(true);

    for (const u of [
      { email: "alice@propose.local", password: "pass" },
      { email: "bob@propose.local", password: "pass" },
    ]) {
      await admin.agent.createUser(u.email, u.password);
      await admin.runtime.setUserFreeAccess(u.email, true);
    }
    const aliceToken = await admin.agent.loginUser("alice@propose.local", "pass");
    const bobToken = await admin.agent.loginUser("bob@propose.local", "pass");
    alice = new Ad4mClient(baseUrl(agent.apiPort), aliceToken, false);
    bob = new Ad4mClient(baseUrl(agent.apiPort), bobToken, false);
    aliceDid = (await alice.agent.me()).did;
    bobDid = (await bob.agent.me()).did;

    expect(new Set([aliceDid, bobDid]).size, "two distinct DIDs").to.equal(2);
  });

  after(async () => {
    if (agent) await agent.stop();
  });

  async function sharedPerspective(name: string): Promise<{
    aliceP: PerspectiveProxy;
    bobP: PerspectiveProxy;
  }> {
    const handle = await alice.perspective.add(name);
    const aliceP = (await alice.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    const bobP = (await bob.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    for (const p of [aliceP]) {
      await (Task as any).register(p);
      await (WorkLog as any).register(p);
      await (Deliverable as any).register(p);
      await (ReviewNote as any).register(p);
    }
    return { aliceP, bobP };
  }

  // ── Test 1: full four-state lifecycle with n:1 quorum ───────────────────
  it("walks Ready → InProgress → InReview (n:1 on every step)", async () => {
    const { aliceP, bobP } = await sharedPerspective("flow-propose-lifecycle");
    const flow = makeFlow({ n: 2 });
    await aliceP.addFlow("ProposeTaskFlow", flow);

    const task = (await (Task as any).create(aliceP, { title: "Ship propose API" })) as Task;

    const started = await FlowInstance.start(aliceP, "ProposeTaskFlow", task.id);
    expect(started.currentStateName).to.equal("Ready");

    // ── Negative 1: unreachable edge ───────────────────────────────────────
    // Done is not reachable from Ready in one step — verify the handler
    // rejects it cleanly rather than silently minting a bad proposal.
    let errMsg = "";
    try {
      await started.proposeTransition("Done");
    } catch (e: any) {
      errMsg = String(e?.message ?? e);
    }
    expect(errMsg, "proposing an unreachable edge must fail").to.match(/not reachable|Done/i);

    // ── Negative 2: guard unmet ────────────────────────────────────────────
    // InProgress requires a WorkLog. No WorkLog exists yet, so the guard is
    // unmet and the handler must return an error without writing a proposal.
    let unmetErr = "";
    try {
      await started.proposeTransition("InProgress");
    } catch (e: any) {
      unmetErr = String(e?.message ?? e);
    }
    expect(unmetErr, "an unmet guard must be an error, not a silent no-op").to.match(
      /not satisfied|guard|InProgress/i,
    );

    // ── Guard-free cancel path (tests NoGuard seal — commit A) ─────────────
    // "Cancelled" carries no `requires` guard. Before commit A, the seal
    // recomputed to `None` in `recompute_evidence_hash` and every voter's
    // `accept` call refused with "cannot reproduce evidence". After commit A,
    // the canonical empty-bag hash is used and the proposal settles.
    const cancelOutcome = await started.proposeTransition("Cancelled");
    expect(cancelOutcome).to.have.lengthOf(1, "n:1 cancel fires immediately");
    expect(cancelOutcome[0].toState).to.equal("Cancelled");

    // ── Full lifecycle on a fresh task ─────────────────────────────────────
    const task2 = (await (Task as any).create(aliceP, { title: "Walk the flow" })) as Task;
    const inst = await FlowInstance.start(aliceP, "ProposeTaskFlow", task2.id);
    expect(inst.currentStateName).to.equal("Ready");

    // Start: Bob writes a WorkLog, then proposes the transition
    await createUnder<WorkLog>(WorkLog, bobP, task2.id, { note: "Picked up." });
    const bobInst = await instanceOn(bobP, task2.id);
    const startOutcome = await bobInst.proposeTransition("InProgress", "I've started — see the log");
    expect(startOutcome, "n:1 into InProgress fires on propose").to.have.lengthOf(1);
    expect(startOutcome[0].fromState).to.equal("Ready");
    expect(startOutcome[0].toState).to.equal("InProgress");

    const afterStart = await instanceOn(bobP, task2.id);
    expect(afterStart.currentStateName).to.equal("InProgress");

    // SubmitForReview: Bob writes a Deliverable, then proposes
    await createUnder<Deliverable>(Deliverable, bobP, task2.id, { title: "deliver.ts" });
    const reviewOutcome = await (await instanceOn(bobP, task2.id)).proposeTransition("InReview");
    expect(reviewOutcome, "n:1 into InReview fires on propose").to.have.lengthOf(1);
    expect(reviewOutcome[0].toState).to.equal("InReview");

    const afterReview = await instanceOn(aliceP, task2.id);
    expect(afterReview.currentStateName).to.equal("InReview");
  });

  // ── Test 2: n:2 quorum on Done requires two distinct voters ─────────────
  it("Done (n:2) requires two distinct voters — one proposal, two accepts", async () => {
    const { aliceP, bobP } = await sharedPerspective("flow-propose-quorum");
    const flow = makeFlow({ n: 2 });
    await aliceP.addFlow("ProposeTaskFlow", flow);

    const task = (await (Task as any).create(aliceP, { title: "Quorum task" })) as Task;
    await FlowInstance.start(aliceP, "ProposeTaskFlow", task.id);

    // Advance to InReview
    await createUnder<WorkLog>(WorkLog, bobP, task.id, { note: "Started." });
    await (await instanceOn(bobP, task.id)).proposeTransition("InProgress");

    await createUnder<Deliverable>(Deliverable, bobP, task.id, { title: "output.ts" });
    await (await instanceOn(bobP, task.id)).proposeTransition("InReview");

    // Alice writes her review BEFORE proposing — the seal is over evidence
    // present at mint time.
    await createUnder<ReviewNote>(ReviewNote, aliceP, task.id, { body: "LGTM" });
    const aliceInst = await instanceOn(aliceP, task.id);

    // Alice proposes Done — first vote of two; flow should NOT have moved.
    const pending = await aliceInst.proposeTransition(
      "Done",
      "Deliverable present and reviewed.",
    );
    expect(pending, "one of two signatures must not advance the flow").to.have.lengthOf(0);

    const stillInReview = await instanceOn(aliceP, task.id);
    expect(
      stillInReview.currentStateName,
      "flow must still be InReview after one vote of two",
    ).to.equal("InReview");

    // Bob co-signs via acceptProposal — his replica re-verifies the seal
    // before signing (including the NoGuard path for guard-free states).
    const proposals = await stillInReview.proposals();
    expect(proposals, "exactly one proposal pending").to.have.lengthOf(1);

    const fired = await (await instanceOn(bobP, task.id)).acceptProposal(proposals[0].id);
    expect(fired, "settling vote returns the fired outcomes").to.have.lengthOf(1);
    expect(fired[0].fromState).to.equal("InReview");
    expect(fired[0].toState).to.equal("Done");
    expect(fired[0].voters).to.have.members([aliceDid, bobDid]);

    const done = await instanceOn(bobP, task.id);
    expect(done.currentStateName).to.equal("Done");
  });

  // ── Test 3: idempotency — re-proposing (before fire) does not duplicate ──
  it("re-proposing the same edge before it fires does not mint a duplicate", async () => {
    const { aliceP, bobP } = await sharedPerspective("flow-propose-idempotency");
    // n:2 on Done so Alice can propose twice before Bob votes
    const flow = makeFlow({ n: 2 });
    await aliceP.addFlow("ProposeTaskFlow", flow);

    const task = (await (Task as any).create(aliceP, { title: "Idempotency task" })) as Task;
    await FlowInstance.start(aliceP, "ProposeTaskFlow", task.id);

    // Advance to InReview
    await createUnder<WorkLog>(WorkLog, aliceP, task.id, { note: "Started." });
    await (await instanceOn(aliceP, task.id)).proposeTransition("InProgress");
    await createUnder<Deliverable>(Deliverable, aliceP, task.id, { title: "output.ts" });
    await (await instanceOn(aliceP, task.id)).proposeTransition("InReview");
    await createUnder<ReviewNote>(ReviewNote, aliceP, task.id, { body: "LGTM" });

    // Alice proposes Done — first vote of two; flow does NOT advance.
    const first = await (await instanceOn(aliceP, task.id)).proposeTransition("Done");
    expect(first, "one of two — not fired yet").to.have.lengthOf(0);

    // Alice proposes Done a second time — idempotency: no duplicate written.
    const second = await (await instanceOn(aliceP, task.id)).proposeTransition("Done");
    expect(second, "idempotent re-propose must not advance the flow").to.have.lengthOf(0);

    const proposals = await (await instanceOn(aliceP, task.id)).proposals();
    const doneProposals = proposals.filter((p) => p.toState === "Done");
    expect(doneProposals, "exactly one Done proposal written").to.have.lengthOf(1);

    // Bob now votes to confirm the proposal settles correctly.
    const settled = await (await instanceOn(bobP, task.id)).acceptProposal(doneProposals[0].id);
    expect(settled).to.have.lengthOf(1);
    expect(settled[0].toState).to.equal("Done");
  });
});
