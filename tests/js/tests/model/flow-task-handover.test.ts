/**
 * ═══════════════════════════════════════════════════════════════════════════
 * FLOW ENGINE HANDOVER — the WE-facing API, with roles
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Written for James / WE. Read this file top to bottom: it is a worked
 * example of the calls a UI makes, not an engine-internals suite. The
 * executable twin of `docs-src/pages/flows.mdx`.
 *
 * ---------------------------------------------------------------------------
 * THE ONE RULE
 * ---------------------------------------------------------------------------
 * A flow's state is NEVER stored. It is derived, every time, from the signed
 * links present right now. `FlowInstance.currentStateName` is a per-replica
 * `Local` cache of the last derivation — read it for display, never decide
 * on it.
 *
 * ---------------------------------------------------------------------------
 * WHAT A UI DOES, STEP BY STEP
 * ---------------------------------------------------------------------------
 *
 *  1. DEFINE THE PROCESS (once, per community — this is Social DNA)
 *       const flow = new SHACLFlow("TaskFlow", "handover://");
 *       flow.inputTypes = ["HandoverTask"];
 *       flow.addState({ name, value, interpretationHint, requires, consensusRule });
 *       flow.addTransition({ actionName, fromState, toState, actions: [] });
 *       await perspective.addFlow("TaskFlow", flow);
 *
 *  2. START A RUN on a concrete expression
 *       const inst = await FlowInstance.start(perspective, "TaskFlow", task.id);
 *     Seeded in the LOWEST-VALUED state.
 *
 *  3. RENDER THE CURRENT STATE AND ITS BUTTONS
 *       inst.currentStateName
 *       inst.currentState.interpretationHint
 *       inst.availableTransitions   // one button each; empty = terminal
 *
 *  4. USER CLICKS A BUTTON
 *       const result = await inst.proposeTransition("InProgress", rationale?);
 *     `FlowProposeResult` is what the UI switches on:
 *
 *       | outcomes   | recordedVote | minted | meaning                         |
 *       |------------|--------------|--------|---------------------------------|
 *       | non-empty  | —            | —      | the transition fired            |
 *       | []         | true         | true   | you opened it; waiting for n    |
 *       | []         | true         | false  | you joined someone else's       |
 *       | []         | false        | false  | you had already voted           |
 *
 *     `result.proposalUri` is the handle you hand to another agent, or to
 *     `rejectProposal`. Two agents pressing the same button both call
 *     `proposeTransition` — the second co-signs rather than minting a twin.
 *
 *  5. OTHER MEMBERS VOTE (same button, or accept/reject on a listed proposal)
 *       await inst.acceptProposal(result.proposalUri);
 *       await inst.rejectProposal(result.proposalUri);
 *     `rejectProposal` withdraws only YOUR links. Retracting a settling vote
 *     moves the flow BACK. That is the one-rule, not a bug.
 *
 *  6. GATE AN EDGE ON A ROLE
 *       consensusRule: {
 *         n: 1,
 *         fromRole: {
 *           className: "HandoverReviewerRole",
 *           where: { domain: "frontend" },
 *           didProperty: "agent",
 *         },
 *       }
 *     `fromRole` is an ordinary ModelQuery. Roles are not special objects.
 *     A key the engine does not know anywhere in the rule refuses the edge
 *     (Part 4); it is not dropped. A grant starts at a link whose signature
 *     verifies, from an author the rule accepts; an earlier link from anyone
 *     else does not move it (Part 5).
 *
 *  7. GRANTING A ROLE IS ITSELF A FLOW  (the fractal stretch)
 *     A `ReviewerRoleGrant` flow runs over a `HandoverReviewerRole` instance
 *     as its base. Completing that flow is *not* what `fromRole` reads —
 *     unless the query also carries `producedByFlow`:
 *       producedByFlow: { flow: "handover://ReviewerRoleGrantFlow",
 *                         state: "Granted" }
 *     Then an instance counts only when it is a verified OUTPUT (className
 *     AND id) of a completed run of that flow, dated from the receipt's
 *     `settled_at`. `state` is required here (unlike the model-query filter
 *     of the same name): without it, a run settled into a "rejected" state
 *     would grant too. The UI closes the loop in three calls:
 *       // a) the Grant clicks name the role instance as the run's output
 *       await grant.proposeTransition("Granted", undefined,
 *         [{ className: "HandoverReviewerRole", id: role.id }]);
 *       // b) once it has settled, any member mints the receipt
 *       await perspective.mintFlowReceipt(grantRunUri);
 *       // c) optional: what the gate will read
 *       await perspective.flowValidOutputs(grantFlowUri, "Granted");
 *     No receipt, no membership: `producedByFlow` is fail-closed. A settled
 *     run nobody minted does not count, and neither does a nominated
 *     instance whose own grant run never settled.
 *
 * ---------------------------------------------------------------------------
 * GAPS THIS FILE NAMES rather than papers over
 * ---------------------------------------------------------------------------
 *  GAP 1 — CLOSED. It was "no JS mint API, so the positive half of
 *          `producedByFlow` cannot be written". #1127 added
 *          `perspective.mintFlowReceipt` / `flowValidOutputs` /
 *          `verifyFlowReceipt`, and #1076 now reads grants through #1127's
 *          per-flow receipt index. Part 3 runs both halves. What a UI still
 *          has to do by hand: call `mintFlowReceipt` after the grant settles.
 *          Nothing mints automatically.
 *  GAP 2 — no flow subscriptions (`onStateChange` / `onProposalAdded`),
 *          and no read that re-derives a run's state without voting:
 *          `currentStateName` is the engine's cache. Part 3 probes with the
 *          button itself, whose refusal names the derived state.
 *          Poll `findAll` / `proposals()` / `currentStateName`.
 *  GAP 3 — `FlowTransition.actions` exists on the type and nothing executes it.
 *  GAP 4 — two managed users on one executor cannot share a perspective, so
 *          this harness is owner + admin (two distinct DIDs, one graph).
 *          Three-party grant ceremonies still need a neighbourhood.
 *
 * Run standalone (with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 900000 --exit \
 *     tests/model/flow-task-handover.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Link, LinkQuery, PerspectiveProxy, SHACLFlow } from "@coasys/ad4m";
import { FlowInstance } from "@coasys/ad4m";
import type { ConsensusRule, FlowProposeResult } from "@coasys/ad4m";
import { Ad4mModel, Model, Property } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";
import { baseUrl } from "../../utils/utils.js";

// ── Domain models. Ordinary @Model classes — a flow points AT data. ────────

@Model({ name: "HandoverTask" })
class Task extends Ad4mModel {
  @Property({ through: "handover://task_title", required: true })
  title: string = "";
}

@Model({ name: "HandoverWorkLog" })
class WorkLog extends Ad4mModel {
  @Property({ through: "handover://worklog_note", required: true })
  note: string = "";
}

@Model({ name: "HandoverDeliverable" })
class Deliverable extends Ad4mModel {
  @Property({ through: "handover://deliverable_title", required: true })
  title: string = "";
}

@Model({ name: "HandoverReviewNote" })
class ReviewNote extends Ad4mModel {
  @Property({ through: "handover://reviewnote_body", required: true })
  body: string = "";
}

/** A role as plain data. `fromRole` is a ModelQuery over exactly this. */
@Model({ name: "HandoverReviewerRole" })
class ReviewerRole extends Ad4mModel {
  @Property({ through: "handover://role_agent", required: true })
  agent: string = "";

  @Property({ through: "handover://role_domain", required: true })
  domain: string = "";
}

/** What the grant flow's `Granted` state requires — an on-graph endorsement. */
@Model({ name: "HandoverGrantEndorsement" })
class GrantEndorsement extends Ad4mModel {
  @Property({ through: "handover://endorsement_reason", required: true })
  reason: string = "";
}

const HAS_CHILD = "ad4m://has_child";
const GRANT_FLOW_URI = "handover://ReviewerRoleGrantFlow";

function makeTaskFlow(doneRule: ConsensusRule, name = "TaskFlow"): SHACLFlow {
  const flow = new SHACLFlow(name, "handover://");
  flow.inputTypes = ["HandoverTask"];
  flow.consensusRule = { n: 1 };
  flow.interpretationHint =
    "Advance a Task: Ready → InProgress → InReview → Done.";

  flow.addState({
    name: "Ready",
    value: 0,
    interpretationHint: "Named, not yet started.",
  });
  flow.addState({
    name: "InProgress",
    value: 1,
    interpretationHint: "Someone is working on it.",
    requires: [{ className: "HandoverWorkLog", linkedTo: "base", count: { min: 1 } }],
  });
  flow.addState({
    name: "InReview",
    value: 2,
    interpretationHint: "A deliverable exists and is waiting on review.",
    requires: [{ className: "HandoverDeliverable", linkedTo: "base", count: { min: 1 } }],
  });
  flow.addState({
    name: "Done",
    value: 3,
    interpretationHint: "Accepted.",
    requires: [
      { className: "HandoverDeliverable", linkedTo: "base", count: { min: 1 } },
      { className: "HandoverReviewNote", linkedTo: "base", count: { min: 1 } },
    ],
    consensusRule: doneRule,
  });

  flow.addTransition({ actionName: "Start", fromState: "Ready", toState: "InProgress", actions: [] });
  flow.addTransition({ actionName: "SubmitForReview", fromState: "InProgress", toState: "InReview", actions: [] });
  flow.addTransition({ actionName: "Accept", fromState: "InReview", toState: "Done", actions: [] });
  return flow;
}

function makeGrantFlow(): SHACLFlow {
  const flow = new SHACLFlow("ReviewerRoleGrant", "handover://");
  flow.inputTypes = ["HandoverReviewerRole"];
  flow.consensusRule = { n: 1 };
  flow.interpretationHint =
    "Grant a ReviewerRole: Proposed → Granted. Completing this flow is what producedByFlow binds to.";

  flow.addState({
    name: "Proposed",
    value: 0,
    interpretationHint: "This agent has been nominated as a reviewer.",
  });
  flow.addState({
    name: "Granted",
    value: 1,
    interpretationHint: "The group agreed this agent holds the role.",
    requires: [{ className: "HandoverGrantEndorsement", linkedTo: "base", count: { min: 1 } }],
    consensusRule: { n: 2 },
  });
  flow.addTransition({
    actionName: "Grant",
    fromState: "Proposed",
    toState: "Granted",
    actions: [],
  });
  return flow;
}

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

async function advanceToInReview(
  aliceP: PerspectiveProxy,
  bobP: PerspectiveProxy,
  taskId: string,
): Promise<void> {
  await createUnder<WorkLog>(WorkLog, bobP, taskId, { note: "Picked up." });
  const started = await (await instanceOn(bobP, taskId)).proposeTransition(
    "InProgress",
    "I've started — see the log",
  );
  expect(started.outcomes, "n:1 Start fires on proposeTransition").to.have.lengthOf(1);
  expect(started.recordedVote).to.be.true;
  expect(started.derivedState).to.equal("InProgress");

  await createUnder<Deliverable>(Deliverable, bobP, taskId, { title: "patch.ts" });
  const submitted = await (await instanceOn(bobP, taskId)).proposeTransition("InReview");
  expect(submitted.outcomes).to.have.lengthOf(1);
  expect(submitted.derivedState).to.equal("InReview");

  const aliceView = await instanceOn(aliceP, taskId);
  expect(aliceView.currentStateName).to.equal("InReview");
}

describe("flow task handover — WE-facing API with roles", function () {
  this.timeout(900_000);

  let agent: AgentHandle;
  let admin: Ad4mClient;
  let alice: Ad4mClient, bob: Ad4mClient;
  let aliceDid: string, bobDid: string;

  before(async () => {
    agent = await startAgent("flow-task-handover");
    admin = agent.client;
    await admin.runtime.setMultiUserEnabled(true);

    await admin.agent.createUser("alice@handover.local", "pass");
    await admin.runtime.setUserFreeAccess("alice@handover.local", true);
    const aliceToken = await admin.agent.loginUser("alice@handover.local", "pass");
    alice = new Ad4mClient(baseUrl(agent.apiPort), aliceToken, false);
    aliceDid = (await alice.agent.me()).did;

    // Admin is the second signer: ALL_CAPABILITY lets it read Alice's
    // perspective, and it has a distinct DID. Two managed users cannot
    // share a perspective (GAP 4).
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
    const bobP = (await bob.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    for (const model of [Task, WorkLog, Deliverable, ReviewNote, ReviewerRole, GrantEndorsement]) {
      await (model as any).register(aliceP);
    }
    return { aliceP, bobP };
  }

  // ── Part 1: the four-state task flow, driven the way a UI would ──────────

  it("Part 1 — Ready → InProgress → InReview → Done via proposeTransition; reject moves it back", async () => {
    const { aliceP, bobP } = await sharedPerspective("handover-lifecycle");
    await aliceP.addFlow("TaskFlow", makeTaskFlow({ n: 2 }));

    const task = (await (Task as any).create(aliceP, { title: "Ship the handover" })) as Task;
    const started = await FlowInstance.start(aliceP, "TaskFlow", task.id);

    expect(started.currentStateName, "lowest-valued state is where every run begins").to.equal("Ready");
    expect(
      started.availableTransitions.map((t) => t.actionName),
      "availableTransitions is the button list",
    ).to.deep.equal(["Start"]);

    // Unreachable edge is an error, not a silent mint. Fail-on-old-code:
    // a client that wrote raw proposal links used to be able to name Done
    // from Ready; proposeTransition must refuse.
    let unreachable = "";
    try {
      await started.proposeTransition("Done");
    } catch (e: any) {
      unreachable = String(e?.message ?? e);
    }
    // Exact wording from `flow_instance/propose.rs`. A loose pattern such as
    // `/not reachable|Done/i` or `/guard/i` also passes on an unrelated error
    // that merely mentions `Done` or a guard, including one thrown before
    // these checks run. The reachability check runs before the guard, so each
    // call produces one deterministic message and exact matching cannot flake.
    expect(unreachable).to.match(/`Done` is not reachable from `Ready`/);

    // Unmet guard is an error. Fail-on-old-code: InProgress requires a WorkLog.
    let unmet = "";
    try {
      await started.proposeTransition("InProgress");
    } catch (e: any) {
      unmet = String(e?.message ?? e);
    }
    expect(unmet).to.match(/guard for `InProgress` is not satisfied on this replica/);

    await advanceToInReview(aliceP, bobP, task.id);
    await createUnder<ReviewNote>(ReviewNote, aliceP, task.id, { body: "LGTM" });

    const alicePress: FlowProposeResult = await (await instanceOn(aliceP, task.id)).proposeTransition(
      "Done",
      "Deliverable present and reviewed.",
    );
    expect(alicePress.outcomes, "one of two — must not fire yet").to.have.lengthOf(0);
    expect(alicePress.recordedVote, "Alice's click landed").to.be.true;
    expect(alicePress.minted).to.be.true;
    expect(alicePress.derivedState).to.equal("InReview");
    expect(alicePress.proposalUri).to.be.a("string").and.not.be.empty;

    // Bob presses the SAME button — not acceptProposal. Two UI clicks.
    const bobPress = await (await instanceOn(bobP, task.id)).proposeTransition("Done");
    expect(bobPress.outcomes, "two distinct DIDs at n:2 IS quorum").to.have.lengthOf(1);
    expect(bobPress.outcomes[0].fromState).to.equal("InReview");
    expect(bobPress.outcomes[0].toState).to.equal("Done");
    expect(bobPress.outcomes[0].voters).to.have.members([aliceDid, bobDid]);
    expect(bobPress.minted, "Bob joined Alice's proposal").to.be.false;
    expect(bobPress.recordedVote).to.be.true;
    expect(bobPress.proposalUri).to.equal(alicePress.proposalUri);
    expect(bobPress.derivedState).to.equal("Done");

    const done = await instanceOn(aliceP, task.id);
    expect(done.currentStateName).to.equal("Done");
    expect(done.availableTransitions, "Done is terminal").to.have.lengthOf(0);

    // Reject is not cancel: Bob withdraws his settling vote. The FOLD
    // stands in InReview again. `currentStateName` is a Local cache and
    // reject does not heal it — the next proposeTransition re-derives.
    const retracted = await (await instanceOn(bobP, task.id)).rejectProposal(bobPress.proposalUri);
    expect(retracted, "one acceptedBy link withdrawn").to.be.at.least(1);
    const afterReject = await (await instanceOn(aliceP, task.id)).proposeTransition("Done");
    expect(
      afterReject.derivedState,
      "retracting the settling vote moved the fold back to InReview",
    ).to.equal("InReview");
    expect(afterReject.outcomes, "Alice already voted; still one short of n:2").to.have.lengthOf(0);
  });

  // ── Part 2: fromRole / didProperty ───────────────────────────────────────

  it("Part 2 — Done gated by fromRole: only the reviewer DID counts", async () => {
    const { aliceP, bobP } = await sharedPerspective("handover-fromrole");
    await aliceP.addFlow(
      "TaskFlow",
      makeTaskFlow({
        n: 1,
        fromRole: {
          className: "HandoverReviewerRole",
          where: { domain: "frontend" },
          didProperty: "agent",
        },
      }),
    );

    const task = (await (Task as any).create(aliceP, { title: "Needs a reviewer" })) as Task;
    await FlowInstance.start(aliceP, "TaskFlow", task.id);
    await advanceToInReview(aliceP, bobP, task.id);
    await createUnder<ReviewNote>(ReviewNote, aliceP, task.id, { body: "Looks good" });

    // Bob holds the role. Alice does not. Creating the instance is enough
    // for fromRole without producedByFlow — that is the hole Part 3 names.
    const role = await createUnder<ReviewerRole>(ReviewerRole, aliceP, task.id, {
      agent: bobDid,
      domain: "frontend",
    });
    expect(role.agent).to.equal(bobDid);

    // Alice clicks Accept. Her vote is recorded (she is a real agent) but
    // fromRole does not count it, so n:1 does not fire.
    // Fail-on-old-code: if fromRole is ignored, this n:1 edge fires here.
    const alicePress = await (await instanceOn(aliceP, task.id)).proposeTransition("Done");
    expect(
      alicePress.outcomes,
      "a non-reviewer click must not settle Done — if this fires, fromRole is being ignored",
    ).to.have.lengthOf(0);
    expect(alicePress.recordedVote, "the click still wrote a vote; it just does not count").to.be.true;
    expect((await instanceOn(aliceP, task.id)).currentStateName).to.equal("InReview");

    const bobPress = await (await instanceOn(bobP, task.id)).proposeTransition("Done");
    expect(bobPress.outcomes, "the reviewer DID is the one fromRole counts").to.have.lengthOf(1);
    expect(bobPress.outcomes[0].toState).to.equal("Done");
    expect(bobPress.outcomes[0].voters).to.include(bobDid);
    expect(bobPress.outcomes[0].voters).to.not.include(aliceDid);
    expect(bobPress.derivedState).to.equal("Done");
  });

  // ── Part 3: fractal — a flow whose output is a role ──────────────────────

  it("Part 3 — grant flow over a ReviewerRole; fromRole matches before Granted; producedByFlow counts only a minted grant", async () => {
    const { aliceP, bobP } = await sharedPerspective("handover-grant-flow");
    await aliceP.addFlow("ReviewerRoleGrant", makeGrantFlow());

    // The role instance is the grant flow's base expression. Creating it
    // is the nomination; the grant flow is the group's agreement.
    const role = (await (ReviewerRole as any).create(aliceP, {
      agent: bobDid,
      domain: "frontend",
    })) as ReviewerRole;

    const grant = await FlowInstance.start(aliceP, "ReviewerRoleGrant", role.id);
    expect(grant.currentStateName).to.equal("Proposed");
    expect(grant.availableTransitions.map((t) => t.actionName)).to.deep.equal(["Grant"]);
    expect(grant.subject).to.equal(role.id);

    // ── The hole, while the grant is still Proposed ───────────────────────
    // Done gated by fromRole WITHOUT producedByFlow. The role instance
    // exists; the grant flow has not moved. fromRole matches the instance
    // anyway — that is the hole, asserted rather than discovered.
    await aliceP.addFlow(
      "TaskFlow",
      makeTaskFlow({
        n: 1,
        fromRole: {
          className: "HandoverReviewerRole",
          where: { domain: "frontend" },
          didProperty: "agent",
        },
      }),
    );

    const openTask = (await (Task as any).create(aliceP, { title: "Hole: role instance is enough" })) as Task;
    await FlowInstance.start(aliceP, "TaskFlow", openTask.id);
    await advanceToInReview(aliceP, bobP, openTask.id);
    await createUnder<ReviewNote>(ReviewNote, aliceP, openTask.id, { body: "ok" });

    expect((await instanceOn(aliceP, role.id)).currentStateName, "grant still Proposed").to.equal(
      "Proposed",
    );
    const openDone = await (await instanceOn(bobP, openTask.id)).proposeTransition("Done");
    expect(
      openDone.outcomes,
      "HOLE: fromRole matched the ReviewerRole instance while its grant flow was still Proposed",
    ).to.have.lengthOf(1);
    expect(openDone.derivedState).to.equal("Done");

    // Completing the grant flow is a real WE call — two clicks on Grant.
    // It does not change the hole above. Each click names the run's OUTPUT:
    // the role instance itself. `Granted` is terminal, so the proposal signs
    // a hash over that instance's content, and a receipt for this run can
    // speak for exactly that instance and nothing else. The run's subject is
    // never an output by default — name it, or the receipt grants nothing.
    // Both clicks must name the same outputs: a press naming different ones
    // is refused rather than joined (it would sign what the clicker did not
    // name).
    const grantOutputs = [{ className: "HandoverReviewerRole", id: role.id }];
    await createUnder<GrantEndorsement>(GrantEndorsement, aliceP, role.id, {
      reason: "Bob reviewed the last three frontend tasks.",
    });
    const aliceGrant = await (await instanceOn(aliceP, role.id)).proposeTransition(
      "Granted",
      undefined,
      grantOutputs,
    );
    expect(aliceGrant.outcomes, "grant is n:2 — Alice alone does not grant").to.have.lengthOf(0);
    expect(aliceGrant.recordedVote).to.be.true;
    expect(aliceGrant.derivedState).to.equal("Proposed");

    const bobGrant = await (await instanceOn(bobP, role.id)).proposeTransition(
      "Granted",
      undefined,
      grantOutputs,
    );
    expect(bobGrant.outcomes, "two DIDs settle the grant flow").to.have.lengthOf(1);
    expect(bobGrant.minted, "Bob co-signed Alice's proposal, outputs and all").to.be.false;
    expect(bobGrant.derivedState).to.equal("Granted");
    expect((await instanceOn(aliceP, role.id)).currentStateName).to.equal("Granted");
    const grantRunUri = bobGrant.outcomes[0].instanceUri;

    // ── producedByFlow: fail-closed without a receipt ─────────────────────
    // Same role instance, same Bob, a fresh task whose Done carries
    // producedByFlow. The grant run has settled, but nobody has minted its
    // receipt yet, so there is nothing for the gate to verify and Bob must
    // NOT count. "The run completed" is not evidence; a receipt is.
    // Fail-on-old-code: if producedByFlow is dropped on the floor, this
    // n:1 edge fires the same way the hole did. (A misspelt key, or the
    // pre-#1076 name `grantedByFlow`, refuses the edge instead: Part 4.)
    // Distinct flow name so this definition does not collide with TaskFlow
    // above. flowUri is handover://GatedTaskFlowFlow.
    await aliceP.addFlow(
      "GatedTaskFlow",
      makeTaskFlow(
        {
          n: 1,
          fromRole: {
            className: "HandoverReviewerRole",
            where: { domain: "frontend" },
            didProperty: "agent",
            producedByFlow: { flow: GRANT_FLOW_URI, state: "Granted" },
          },
        },
        "GatedTaskFlow",
      ),
    );

    const gatedTask = (await (Task as any).create(aliceP, { title: "Fail-closed producedByFlow" })) as Task;
    await FlowInstance.start(aliceP, "GatedTaskFlow", gatedTask.id);
    await advanceToInReview(aliceP, bobP, gatedTask.id);
    await createUnder<ReviewNote>(ReviewNote, aliceP, gatedTask.id, { body: "ok" });

    const gatedDone = await (await instanceOn(bobP, gatedTask.id)).proposeTransition("Done");
    expect(
      gatedDone.outcomes,
      "producedByFlow without a receipt must not count Bob — if this fires, the field is being ignored",
    ).to.have.lengthOf(0);
    expect(gatedDone.recordedVote, "the click still wrote; the fold did not count it").to.be.true;
    expect(gatedDone.derivedState).to.equal("InReview");
    expect((await instanceOn(aliceP, gatedTask.id)).currentStateName).to.equal("InReview");
    expect(
      await aliceP.flowValidOutputs(GRANT_FLOW_URI, "Granted"),
      "no receipt yet, so the grant flow has no valid outputs",
    ).to.be.empty;

    // ── producedByFlow: the positive half — mint the receipt ──────────────
    // Any member can mint once the run has settled. The receipt is filed
    // under the grant flow (`F --ad4m://flow/flow_receipt--> receipt`), and
    // that per-flow index is the only place the gate looks.
    const minted = await aliceP.mintFlowReceipt(grantRunUri);
    expect(minted.receiptUri).to.match(/^ad4m:\/\/flow\/receipt\//);

    // What the gate will read, asked the way a UI would: the role instance
    // is a valid output of a completed ReviewerRoleGrant run in `Granted`.
    const granted = await aliceP.flowValidOutputs(GRANT_FLOW_URI, "Granted");
    expect(granted).to.have.lengthOf(1);
    expect(granted[0].output).to.deep.equal({ className: "HandoverReviewerRole", id: role.id });
    expect(granted[0].receiptUri).to.equal(minted.receiptUri);
    const verdict = await bobP.verifyFlowReceipt(minted.receipt);
    expect(verdict.outcome, verdict.detail).to.equal("verified");
    expect(verdict.voters).to.have.members([aliceDid, bobDid]);

    // THE ONE RULE, on the gated task from the negative half: Bob's vote is
    // unchanged, but a receipt is on the graph now, so the same vote counts.
    // The grant is dated from the receipt's `settled_at` (when Bob's grant
    // reached quorum), and Bob voted after that, so the fold now derives
    // Done with nothing new written. There is no client read that re-derives
    // without voting (GAP 2; `currentStateName` is the engine's cache), so
    // the probe is the button: the refusal names the state the fold derived.
    let reDerived = "";
    try {
      await (await instanceOn(bobP, gatedTask.id)).proposeTransition("Done");
    } catch (e: any) {
      reDerived = String(e?.message ?? e);
    }
    expect(
      reDerived,
      "the vote Bob cast before the mint counts once the receipt exists — the fold is already in Done",
    ).to.match(/`Done` is not reachable from `Done`/);

    // A fresh gated task, and a sharper non-holder: Alice now has her OWN
    // ReviewerRole instance (same class, same domain), nominated through the
    // same grant flow — but her run is still `Proposed`, so it has no
    // completion to mint and no receipt names her instance.
    const aliceRole = (await (ReviewerRole as any).create(aliceP, {
      agent: aliceDid,
      domain: "frontend",
    })) as ReviewerRole;
    const aliceNomination = await FlowInstance.start(aliceP, "ReviewerRoleGrant", aliceRole.id);
    expect(aliceNomination.currentStateName).to.equal("Proposed");
    let unminted = "";
    try {
      await aliceP.mintFlowReceipt(aliceNomination.uri);
    } catch (e: any) {
      unminted = String(e?.message ?? e);
    }
    expect(unminted, "a run that has not settled has nothing to mint").to.match(/no settled edge/);

    const positiveTask = (await (Task as any).create(aliceP, {
      title: "Positive producedByFlow",
    })) as Task;
    await FlowInstance.start(aliceP, "GatedTaskFlow", positiveTask.id);
    await advanceToInReview(aliceP, bobP, positiveTask.id);
    await createUnder<ReviewNote>(ReviewNote, aliceP, positiveTask.id, { body: "ok" });

    // Alice first. She matches `fromRole`'s where-clause through her own
    // instance, so only the grant binding keeps her out.
    // Fail-on-old-code: if the gate stops binding a receipt to the instance
    // it names — reads "F completed" rather than "F produced THIS
    // (className, id)" — Bob's receipt admits Alice's instance and this n:1
    // edge fires on her click.
    const aliceDone = await (await instanceOn(aliceP, positiveTask.id)).proposeTransition("Done");
    expect(
      aliceDone.outcomes,
      "Alice's instance was never granted — if this fires, the receipt is not bound to its instance",
    ).to.have.lengthOf(0);
    expect(aliceDone.recordedVote).to.be.true;
    expect(aliceDone.derivedState).to.equal("InReview");

    // Bob, the holder. Fail-on-old-code: every earlier version of this file
    // ran against a gate that could not grant from TS (no mint API). A gate
    // that never reads F's receipt index fails here too, and fails the
    // re-derivation probe above first.
    const bobDone = await (await instanceOn(bobP, positiveTask.id)).proposeTransition("Done");
    expect(
      bobDone.outcomes,
      "Bob's role instance is a verified output of a completed grant run — he counts",
    ).to.have.lengthOf(1);
    expect(bobDone.outcomes[0].toState).to.equal("Done");
    expect(bobDone.outcomes[0].voters).to.include(bobDid);
    expect(bobDone.outcomes[0].voters, "Alice's earlier click still does not count").to.not.include(
      aliceDid,
    );
    expect(bobDone.derivedState).to.equal("Done");
  });

  // ── Part 4: a role gate with a key the engine does not know ──────────────

  it("Part 4 — a fromRole with an unknown key refuses the edge, even for the role holder", async () => {
    const { aliceP, bobP } = await sharedPerspective("handover-unknown-key");
    // Built from JSON, the way an untyped client or a stored definition
    // arrives: the TS types would reject `grantedByFlow` in an object
    // literal, but nothing checks a parsed one. `grantedByFlow` is the name
    // #1076 renamed to `producedByFlow`.
    const staleGate = JSON.parse(`{
      "n": 1,
      "fromRole": {
        "className": "HandoverReviewerRole",
        "where": { "domain": "frontend" },
        "didProperty": "agent",
        "grantedByFlow": { "flow": "${GRANT_FLOW_URI}", "state": "Granted" }
      }
    }`) as ConsensusRule;
    await aliceP.addFlow("TaskFlow", makeTaskFlow(staleGate));

    const task = (await (Task as any).create(aliceP, { title: "Stale gate" })) as Task;
    await FlowInstance.start(aliceP, "TaskFlow", task.id);
    await advanceToInReview(aliceP, bobP, task.id);
    await createUnder<ReviewNote>(ReviewNote, aliceP, task.id, { body: "Looks good" });
    await createUnder<ReviewerRole>(ReviewerRole, aliceP, task.id, {
      agent: bobDid,
      domain: "frontend",
    });

    // Bob holds the role exactly as in Part 2, where this click fires Done.
    // Fail-on-old-code: the engine dropped `grantedByFlow`, read the gate as
    // Part 2's plain fromRole, and this n:1 edge fired with no receipt.
    const bobPress = await (await instanceOn(bobP, task.id)).proposeTransition("Done");
    expect(
      bobPress.outcomes,
      "a rule with an unknown key is refused — if this fires, the key was dropped and the gate widened",
    ).to.have.lengthOf(0);
    expect(bobPress.recordedVote, "the click is recorded; the rule refuses to count it").to.be.true;
    expect(bobPress.derivedState).to.equal("InReview");
  });

  // ── Part 5: when a grant starts ──────────────────────────────────────────

  it("Part 5 — a grant starts at the granter's link, not at an earlier one the grantee wrote", async () => {
    const { aliceP, bobP } = await sharedPerspective("handover-grant-start");
    // Only Alice grants the role: the rule's author sits under every field,
    // so both the `agent` and the `domain` link must be hers.
    await aliceP.addFlow(
      "TaskFlow",
      makeTaskFlow({
        n: 1,
        fromRole: {
          className: "HandoverReviewerRole",
          where: { domain: "frontend", author: aliceDid },
          didProperty: "agent",
        },
      }),
    );

    const task = (await (Task as any).create(aliceP, { title: "Early self-assignment" })) as Task;
    await FlowInstance.start(aliceP, "TaskFlow", task.id);
    await advanceToInReview(aliceP, bobP, task.id);
    await createUnder<ReviewNote>(ReviewNote, aliceP, task.id, { body: "Looks good" });

    // Bob writes the role instance naming himself, then votes. His own links
    // are not a grant under this rule, so the vote does not count.
    const role = await createUnder<ReviewerRole>(ReviewerRole, bobP, task.id, {
      agent: bobDid,
      domain: "frontend",
    });
    const bobPress = await (await instanceOn(bobP, task.id)).proposeTransition("Done");
    expect(bobPress.outcomes, "Bob granted himself nothing").to.have.lengthOf(0);
    expect(bobPress.recordedVote).to.be.true;

    // Alice grants him the role after his vote, by writing the same two
    // links herself. The vote was cast outside the role.
    for (const predicate of ["handover://role_agent", "handover://role_domain"]) {
      const [bobs] = await aliceP.get(new LinkQuery({ source: role.id, predicate }));
      expect(bobs, `Bob's ${predicate} link`).to.exist;
      await aliceP.add(new Link({ source: role.id, predicate, target: bobs.data.target }));
    }

    // Alice's own click re-derives the run. She holds no role, so it counts
    // for nothing, and Bob's vote predates his grant.
    // Fail-on-old-code: the grant started at the earliest `agent` link naming
    // Bob from ANY author, which was his own, written before his vote, so the
    // vote counted and the run settled Done here.
    const alicePress = await (await instanceOn(aliceP, task.id)).proposeTransition("Done");
    expect(
      alicePress.outcomes,
      "Bob's own earlier link must not date the grant Alice made after his vote",
    ).to.have.lengthOf(0);
    expect(alicePress.derivedState).to.equal("InReview");
    // Part 2 is the positive half: a vote cast after a genuine grant counts.
  });
});
