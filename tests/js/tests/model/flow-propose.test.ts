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

    await admin.agent.createUser("alice@propose.local", "pass");
    await admin.runtime.setUserFreeAccess("alice@propose.local", true);
    const aliceToken = await admin.agent.loginUser("alice@propose.local", "pass");
    alice = new Ad4mClient(baseUrl(agent.apiPort), aliceToken, false);
    aliceDid = (await alice.agent.me()).did;

    // Use the admin client as the second signer: admin has ALL_CAPABILITY so it
    // can read/write to any user's perspective (no 403). Admin has its own DID
    // (the main agent key), distinct from Alice's sub-user DID.
    bob = admin;
    bobDid = (await admin.agent.me()).did;

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
    // Admin (bob) has ALL_CAPABILITY so it can access any user's perspective by UUID.
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
    expect(cancelOutcome.outcomes).to.have.lengthOf(1, "n:1 cancel fires immediately");
    expect(cancelOutcome.outcomes[0].toState).to.equal("Cancelled");
    expect(cancelOutcome.minted, "this call wrote the proposal").to.be.true;
    expect(cancelOutcome.recordedVote).to.be.true;
    expect(cancelOutcome.derivedState).to.equal("Cancelled");
    expect(cancelOutcome.proposalUri, "the created proposal is returned").to.be.a("string").and.not.be.empty;

    // ── Full lifecycle on a fresh task ─────────────────────────────────────
    const task2 = (await (Task as any).create(aliceP, { title: "Walk the flow" })) as Task;
    const inst = await FlowInstance.start(aliceP, "ProposeTaskFlow", task2.id);
    expect(inst.currentStateName).to.equal("Ready");

    // Start: Bob writes a WorkLog, then proposes the transition
    await createUnder<WorkLog>(WorkLog, bobP, task2.id, { note: "Picked up." });
    const bobInst = await instanceOn(bobP, task2.id);
    const startOutcome = await bobInst.proposeTransition("InProgress", "I've started — see the log");
    expect(startOutcome.outcomes, "n:1 into InProgress fires on propose").to.have.lengthOf(1);
    expect(startOutcome.outcomes[0].fromState).to.equal("Ready");
    expect(startOutcome.outcomes[0].toState).to.equal("InProgress");
    expect(startOutcome.derivedState).to.equal("InProgress");

    const afterStart = await instanceOn(bobP, task2.id);
    expect(afterStart.currentStateName).to.equal("InProgress");

    // SubmitForReview: Bob writes a Deliverable, then proposes
    await createUnder<Deliverable>(Deliverable, bobP, task2.id, { title: "deliver.ts" });
    const reviewOutcome = await (await instanceOn(bobP, task2.id)).proposeTransition("InReview");
    expect(reviewOutcome.outcomes, "n:1 into InReview fires on propose").to.have.lengthOf(1);
    expect(reviewOutcome.outcomes[0].toState).to.equal("InReview");

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
    expect(pending.outcomes, "one of two signatures must not advance the flow").to.have.lengthOf(0);
    // The empty list alone said nothing. These are what tell a UI "your vote
    // landed, waiting for others" from "nothing happened".
    expect(pending.recordedVote, "Alice's vote landed").to.be.true;
    expect(pending.minted).to.be.true;
    expect(pending.contested).to.be.false;
    expect(pending.derivedState).to.equal("InReview");

    const stillInReview = await instanceOn(aliceP, task.id);
    expect(
      stillInReview.currentStateName,
      "flow must still be InReview after one vote of two",
    ).to.equal("InReview");

    // Bob co-signs via acceptProposal — his replica re-verifies the seal
    // before signing (including the NoGuard path for guard-free states).
    // proposals() returns all FlowTransitionProposal subjects including fired
    // ones; filter to the pending Done edge.
    const allProposals = await stillInReview.proposals();
    const proposals = allProposals.filter((p) => p.toState === "Done");
    expect(proposals, "exactly one Done proposal pending").to.have.lengthOf(1);
    expect(
      proposals[0].id,
      "proposeTransition returned the URI of the proposal it minted — a co-signer's handle",
    ).to.equal(pending.proposalUri);

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
    expect(first.outcomes, "one of two — not fired yet").to.have.lengthOf(0);
    expect(first.minted).to.be.true;
    expect(first.recordedVote).to.be.true;

    // Alice proposes Done a second time — idempotency: no duplicate written.
    const second = await (await instanceOn(aliceP, task.id)).proposeTransition("Done");
    expect(second.outcomes, "idempotent re-propose must not advance the flow").to.have.lengthOf(0);
    // Same empty list as `first`, opposite meaning. This is the distinction
    // the old bare-array return could not express.
    expect(second.minted, "nothing was written the second time").to.be.false;
    expect(second.recordedVote, "Alice had already voted").to.be.false;
    expect(second.proposalUri, "the no-op still names the live proposal").to.equal(first.proposalUri);

    const proposals = await (await instanceOn(aliceP, task.id)).proposals();
    const doneProposals = proposals.filter((p) => p.toState === "Done");
    expect(doneProposals, "exactly one Done proposal written").to.have.lengthOf(1);

    // Bob now votes to confirm the proposal settles correctly.
    const settled = await (await instanceOn(bobP, task.id)).acceptProposal(doneProposals[0].id);
    expect(settled).to.have.lengthOf(1);
    expect(settled[0].toState).to.equal("Done");
  });

  // ── Test 4: guard-free state with n:2 requires two distinct co-signers ──
  // This is the core regression test for the EvidenceSeal::NoGuard path.
  // Before the fix, `recompute_evidence_hash` returned `None` for guard-free
  // target states and `accept.rs` refused every co-sign attempt, leaving the
  // flow deadlocked at {n: 2}. After the fix the seal is NoGuard (canonical
  // empty-bag hash), and both proposer and voter produce the same hash by
  // construction, so co-signing succeeds.
  it("guard-free state with n:2 — two distinct signers advance the flow", async () => {
    const { aliceP, bobP } = await sharedPerspective("flow-propose-guard-free-n2");

    // Minimal flow: Ready → Archived (guard-free, n:2).
    const flow = new SHACLFlow("GuardFreeN2Flow", "gf-n2://");
    flow.inputTypes = ["ProposeTask"];
    flow.consensusRule = { n: 1 };
    flow.addState({ name: "Ready", value: 0 });
    flow.addState({ name: "Archived", value: 99, consensusRule: { n: 2 } });
    flow.addTransition({ actionName: "Archive", fromState: "Ready", toState: "Archived", actions: [] });
    await aliceP.addFlow("GuardFreeN2Flow", flow);

    const task = (await (Task as any).create(aliceP, { title: "Guard-free n:2 task" })) as Task;
    const inst = await FlowInstance.start(aliceP, "GuardFreeN2Flow", task.id);
    expect(inst.currentStateName).to.equal("Ready");

    // Alice proposes the guard-free transition — first of two votes.
    const pending = await inst.proposeTransition("Archived", "archive it");
    expect(pending.outcomes, "one of two — must not fire yet").to.have.lengthOf(0);
    expect(pending.recordedVote).to.be.true;
    expect((await instanceOn(aliceP, task.id)).currentStateName).to.equal("Ready");

    // Bob co-signs: his replica must reproduce the same NoGuard seal hash,
    // not reject with "cannot reproduce evidence".
    const proposals = await (await instanceOn(aliceP, task.id)).proposals();
    expect(proposals, "one pending proposal").to.have.lengthOf(1);

    const fired = await (await instanceOn(bobP, task.id)).acceptProposal(proposals[0].id);
    expect(fired, "second vote settles the guard-free transition").to.have.lengthOf(1);
    expect(fired[0].fromState).to.equal("Ready");
    expect(fired[0].toState).to.equal("Archived");
    expect(fired[0].voters, "both signers recorded").to.have.members([aliceDid, bobDid]);

    const archived = await instanceOn(bobP, task.id);
    expect(archived.currentStateName).to.equal("Archived");
  });

  // ── Test 5: two DIDs both reach for `proposeTransition` on ONE edge ──────
  // The case no test in this suite had: tests 2 and 4 route the second voter
  // through `acceptProposal`, and test 3 pins the SAME agent re-proposing.
  //
  // The dedup key `(evidence_hash, instance, toState)` carries no proposer, so
  // Bob's click matched Alice's proposal. Before the fix the mint was skipped
  // and Bob cast no vote at all — he received the same empty array the API
  // documents as "queued for other voters", and at {n: 2} the edge could never
  // settle. Now Bob's click co-signs Alice's proposal.
  it("two DIDs both calling proposeTransition on one n:2 edge reach quorum", async () => {
    const { aliceP, bobP } = await sharedPerspective("flow-propose-two-proposers");

    const flow = new SHACLFlow("TwoProposersFlow", "two-prop://");
    flow.inputTypes = ["ProposeTask"];
    flow.consensusRule = { n: 1 };
    flow.addState({ name: "Ready", value: 0 });
    flow.addState({ name: "Archived", value: 99, consensusRule: { n: 2 } });
    flow.addTransition({ actionName: "Archive", fromState: "Ready", toState: "Archived", actions: [] });
    await aliceP.addFlow("TwoProposersFlow", flow);

    const task = (await (Task as any).create(aliceP, { title: "Two proposers" })) as Task;
    const inst = await FlowInstance.start(aliceP, "TwoProposersFlow", task.id);

    const alicePress = await inst.proposeTransition("Archived");
    expect(alicePress.outcomes, "one of two — not yet").to.have.lengthOf(0);
    expect(alicePress.minted).to.be.true;

    // Bob presses the SAME button — not acceptProposal.
    const bobPress = await (await instanceOn(bobP, task.id)).proposeTransition("Archived");

    expect(
      bobPress.outcomes,
      "two distinct DIDs on one edge IS quorum at n:2 — an empty array here is the bug",
    ).to.have.lengthOf(1);
    expect(bobPress.outcomes[0].fromState).to.equal("Ready");
    expect(bobPress.outcomes[0].toState).to.equal("Archived");
    expect(bobPress.outcomes[0].voters, "both signers recorded").to.have.members([aliceDid, bobDid]);

    // Bob joined Alice's proposal rather than minting an unreachable twin.
    expect(bobPress.minted, "Bob did not write a second proposal").to.be.false;
    expect(bobPress.recordedVote, "Bob's vote landed").to.be.true;
    expect(bobPress.proposalUri).to.equal(alicePress.proposalUri);
    expect(bobPress.derivedState).to.equal("Archived");

    const settledInst = await instanceOn(bobP, task.id);
    expect(settledInst.currentStateName).to.equal("Archived");
    const archivedProposals = (await settledInst.proposals()).filter((p) => p.toState === "Archived");
    expect(archivedProposals, "one proposal, co-signed — no twin").to.have.lengthOf(1);
  });
});
