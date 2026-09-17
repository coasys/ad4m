/**
 * ═══════════════════════════════════════════════════════════════════════════
 * FLOW ENGINE HANDOVER — a realistic task flow, driven the way a UI would
 * ═══════════════════════════════════════════════════════════════════════════
 *
 * Written for James, who is about to build flows in WE and has not read the
 * engine source. Read this file top to bottom: it is a worked example, and
 * every place the test has to reach past the public API carries a `GAP:`
 * comment naming the call an app would actually want.
 *
 * ---------------------------------------------------------------------------
 * THE ONE RULE
 * ---------------------------------------------------------------------------
 * A flow's state is NEVER stored. It is derived, every time, from the signed
 * links present right now. `FlowInstance.currentStateName` is a per-replica
 * `Local` cache of the last derivation — read it for display, never decide on
 * it. Everything below follows from that.
 *
 * ---------------------------------------------------------------------------
 * WHAT A UI DOES, STEP BY STEP
 * ---------------------------------------------------------------------------
 *
 *  1. DEFINE THE PROCESS (once, per community — this is Social DNA)
 *       const flow = new SHACLFlow("TaskFlow", "we://");
 *       flow.inputTypes = ["Task"];
 *       flow.addState({ name, value, interpretationHint, requires, consensusRule });
 *       flow.addTransition({ actionName, fromState, toState, actions: [] });
 *       await perspective.addFlow("TaskFlow", flow);
 *     The definition is data in the perspective. A peer who syncs it runs your
 *     process without installing your app.
 *
 *  2. START A RUN on a concrete expression
 *       const inst = await FlowInstance.start(perspective, "TaskFlow", task.id);
 *     Seeded in the LOWEST-VALUED state. `perspective.availableFlows(uri)` tells
 *     a UI which flows may be offered on a given expression.
 *
 *  3. RENDER THE CURRENT STATE AND ITS BUTTONS
 *       inst.currentStateName          // "Ready"
 *       inst.currentState.interpretationHint
 *       inst.availableTransitions      // [{ actionName: "Start", toState: "InProgress" }]
 *     Each `availableTransitions` entry is one button. An empty array is a
 *     terminal state.
 *
 *  4. USER CLICKS "Start"  ←──────────  THIS IS WHERE THE API RUNS OUT
 *     There is no `inst.proposeTransition("InProgress")`. Proposals are minted
 *     by the engine's deterministic evaluator, which today runs in exactly one
 *     place: after an LLM interpretation pass
 *     (`rust-executor/src/perspectives/interpretation/run.rs:956`).
 *     So a button-driven UI must write the proposal itself — see
 *     `proposeTransition()` below and GAP 1 / GAP 2.
 *
 *  5. OTHER MEMBERS VOTE (this part IS a real API)
 *       const pending = await inst.proposals();
 *       const fired   = await inst.acceptProposal(pending[0]);   // FlowFireOutcome[]
 *       const gone    = await inst.rejectProposal(pending[0]);   // number of links retracted
 *     `acceptProposal` re-verifies the proposal's evidence seal against THIS
 *     replica's own graph before signing. `fired` is empty when the vote landed
 *     but quorum is not yet reached — that is success, not failure.
 *     `rejectProposal` withdraws only YOUR links. It does not cancel anyone
 *     else's proposal, and retracting a settling vote moves the flow BACK.
 *
 *  6. GATE THE IRREVERSIBLE EDGE ON A ROLE
 *       consensusRule: { n: 1, fromRole: { className: "ReviewerRole",
 *                                          where: { domain: "frontend" },
 *                                          didProperty: "agent" } }
 *     `fromRole` is an ordinary ModelQuery over ordinary subject classes.
 *     Roles are not special objects — they are whatever data you point at.
 *
 * ---------------------------------------------------------------------------
 * THE SEVEN GAPS THIS FILE DEMONSTRATES  (search for `GAP:`)
 * ---------------------------------------------------------------------------
 *  GAP 1 — no `FlowInstance.proposeTransition(toState, rationale?)`.
 *  GAP 2 — a client cannot compute the evidence seal without reimplementing
 *          an engine-internal hash (this file does; ~40 lines of mirror code).
 *  GAP 3 — a state with NO `requires` guard can never be entered through
 *          `acceptProposal`: the seal recomputes to `None` and the vote is
 *          always refused. Guards are mandatory on every votable state.
 *  GAP 4 — no "derive now" read. `currentStateName` is a Local cache that only
 *          this replica's own pass heals; a reader who has not voted must poll.
 *  GAP 5 — `FlowTransition.actions` exists on the type and nothing executes it.
 *  GAP 6 — no flow subscriptions (`onStateChange` / `onProposalAdded`).
 *  GAP 7 — `fromRole` matches a role INSTANCE. It cannot ask whether the flow
 *          that was supposed to grant that role ever completed. Part 2 asserts
 *          this as executable fact: an ungranted reviewer already votes.
 *
 * ---------------------------------------------------------------------------
 * HARNESS
 * ---------------------------------------------------------------------------
 * Quorum counts DISTINCT DIDs, so this needs three real agents. One executor
 * in multi-user mode with three managed users (Alice, Bob, Charlie), each with
 * their own JWT'd `Ad4mClient` writing into one shared perspective.
 *
 * GAP 8 — THAT HARNESS CANNOT EXIST TODAY. Two managed users on one executor
 *         cannot share a perspective. All three tests below fail in setup with
 *         `bob.perspective.byUUID(...) === null`, and the cause is a product
 *         hole, not a test-harness mistake:
 *
 *           - `perspective.create` stamps exactly ONE owner, the calling
 *             session's DID:
 *             rust-executor/src/api/perspectives_ws.rs:373-379
 *             (`new_with_owner(name, ctx.user_did)`).
 *           - Every read goes through `get_perspective_with_access`, which for
 *             a non-admin-credential session requires
 *             `can_access_perspective_with_did`:
 *             rust-executor/src/api/perspectives_ws.rs:108-124
 *             -> rust-executor/src/helpers.rs:37-45
 *             -> `PerspectiveHandle::is_owned_by`
 *                rust-executor/src/types/domain.rs:540-545.
 *           - `is_owned_by` is a plain membership test on `owners`, and
 *             `PerspectiveHandle::add_owner` (domain.rs:530+) is reachable from
 *             NO RPC method. `perspective.update` only sets `name`
 *             (perspectives_ws.rs:388-410). Grepping `owners` across
 *             `rust-executor/src/api/` and `core/src/` finds no mutator.
 *           - Creating the perspective as the unauthenticated admin client does
 *             not help: that yields `owners = None`, and `is_owned_by` returns
 *             `false` for `None` (`unwrap_or(false)`), so a managed user is
 *             still denied. Only a session with `user_did == None` (the host
 *             main agent) may read an unowned perspective.
 *
 *         Net: on a single executor, managed-user sessions are mutually
 *         perspective-isolated by construction. The only sharing mechanism AD4M
 *         has is a neighbourhood — each agent joins and gets their OWN owned
 *         perspective, synced by the link language.
 *
 *         Do not "fix" this by giving all three clients the admin credential:
 *         `get_perspective_with_access` skips the ownership check for
 *         `is_admin_credential`, but such a session has `user_did == None`, so
 *         all three collapse onto the host agent's DID — and quorum, which
 *         counts DISTINCT DIDs, becomes a single-agent no-op. That would make
 *         the suite green while testing nothing.
 *
 * GAP 9 — `tests/auto-processor-multi-user.test.ts` (~line 180) asserts the
 *         opposite ("Bob (managed user on the same executor) must resolve
 *         Alice's perspective") with a comment claiming the current hosted path
 *         allows it. That assertion is false against the code above, and it has
 *         never been observed to pass: the whole suite is gated behind
 *         `describeIfLLM` (`process.env.LLM_E2E === "1"`) and is
 *         `describe.skip`ped in every default run. Treat it as an untested
 *         claim, not as precedent.
 *
 * Until GAP 8 is closed — either an `owners` param on `perspective.create` / an
 * `addOwner` RPC, or this suite rebuilt on a neighbourhood with
 * `runHcLocalServices` — the three tests below are expected to fail in setup.
 * The bodies are the deliverable: they encode the flow-engine contract and
 * GAPs 1-7, and must not be weakened to obtain a green run.
 *
 * Run standalone (with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 900000 --exit \
 *     tests/model/flow-task-handover.test.ts
 */

import { expect } from "chai";
import crypto from "node:crypto";
import { Ad4mClient, PerspectiveProxy, SHACLFlow } from "@coasys/ad4m";
import { FlowInstance, FlowTransitionProposal } from "@coasys/ad4m";
import type { ConsensusRule, ModelQuery } from "@coasys/ad4m";
import { Ad4mModel, Model, Property } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";
import { baseUrl } from "../../utils/utils.js";

// GAP 8 — `FlowFireOutcome`, the return type of the public `acceptProposal`,
// is declared in `core/src/perspectives/PerspectiveClient.ts` and that module
// is NOT re-exported from `core/src/index.ts`. A TypeScript app can call the
// method but cannot name what it gets back. One line in `index.ts` fixes it.
type FlowFireOutcome = Awaited<ReturnType<FlowInstance["acceptProposal"]>>[number];

// ═══════════════════════════════════════════════════════════════════════════
// The domain classes. Ordinary @Model subject classes — nothing flow-specific
// about them. A flow points AT data; it does not own it.
// ═══════════════════════════════════════════════════════════════════════════

@Model({ name: "HandoverTask" })
class Task extends Ad4mModel {
  @Property({ through: "we://task_title", required: true })
  title: string = "";

  @Property({ through: "we://task_description" })
  description: string = "";

  @Property({ through: "we://task_assignee" })
  assignee: string = "";
}

/** Evidence that work actually started — what `InProgress` requires. */
@Model({ name: "HandoverWorkLog" })
class WorkLog extends Ad4mModel {
  @Property({ through: "we://worklog_note", required: true })
  note: string = "";
}

/** The thing the task produced — what `InReview` requires. */
@Model({ name: "HandoverDeliverable" })
class Deliverable extends Ad4mModel {
  @Property({ through: "we://deliverable_title", required: true })
  title: string = "";

  @Property({ through: "we://deliverable_url" })
  url: string = "";
}

/** A reviewer's written verdict — half of what `Done` requires. */
@Model({ name: "HandoverReviewNote" })
class ReviewNote extends Ad4mModel {
  @Property({ through: "we://reviewnote_body", required: true })
  body: string = "";
}

/**
 * A role, as plain data. `fromRole` is a ModelQuery over exactly this —
 * there is no privileged "role" concept in the engine.
 */
@Model({ name: "HandoverReviewerRole" })
class ReviewerRole extends Ad4mModel {
  @Property({ through: "we://role_agent", required: true })
  agent: string = "";

  @Property({ through: "we://role_domain", required: true })
  domain: string = "";
}

/** One granter's written endorsement — what the grant flow's `Granted` requires. */
@Model({ name: "HandoverGrantEndorsement" })
class GrantEndorsement extends Ad4mModel {
  @Property({ through: "we://endorsement_reason", required: true })
  reason: string = "";
}

// ═══════════════════════════════════════════════════════════════════════════
// GAP 2 — THE EVIDENCE SEAL, REIMPLEMENTED CLIENT-SIDE
// ═══════════════════════════════════════════════════════════════════════════
// Every proposal carries `evidence_hashes`: a seal over the instances that
// satisfied the target state's `requires` guard, computed at mint time.
// `acceptProposal` recomputes it on the voter's own graph and refuses to
// co-sign on a mismatch (`flow_instance/accept.rs`) — that is what makes a
// vote a statement about content rather than about a URI.
//
// The engine computes it in `flow_evaluator::evidence_hash`. A client that
// wants to mint a proposal must produce a byte-identical value, and there is
// no API that returns one. So the three functions below are a hand-written
// mirror of Rust internals, kept honest only by this test failing if they
// drift.
//
// GAP: what an app wants is
//        `await instance.proposeTransition("InReview", { rationale })`
//      — the executor already has the evaluator, the guard and the hash
//      function; it should mint from the target state name and nothing else.
//      Failing that, at minimum
//        `await perspective.flowEvidenceSeal(instanceUri, toState)`
//      so the seal is computed by the code that owns its definition.

/** Mirror of `flow_evaluator::canonical_json` — recursively key-sorted JSON. */
function canonicalJson(v: any): string {
  if (v === null || v === undefined) return "null";
  if (Array.isArray(v)) return "[" + v.map(canonicalJson).join(",") + "]";
  if (typeof v === "object") {
    const keys = Object.keys(v).sort();
    return "{" + keys.map((k) => `${JSON.stringify(k)}:${canonicalJson(v[k])}`).join(",") + "}";
  }
  return JSON.stringify(v);
}

type EvidenceItem = { id: string; className: string; content: string };

/**
 * Mirror of `flow_evaluator::evidence_hash` — SHA-256 over the class names
 * followed by each `(class, id, canonical-content)` triple, sorted, with every
 * field length-prefixed (u64 LE) so no field's content can shift bytes across
 * a boundary.
 */
function evidenceHash(classNames: string[], evidence: EvidenceItem[]): string {
  const hasher = crypto.createHash("sha256");
  const frame = (field: string) => {
    const bytes = Buffer.from(field, "utf8");
    const len = Buffer.alloc(8);
    len.writeBigUInt64LE(BigInt(bytes.length));
    hasher.update(len);
    hasher.update(bytes);
  };

  const items = evidence.map((e) => {
    let canonical: string;
    try {
      canonical = canonicalJson(JSON.parse(e.content));
    } catch {
      canonical = e.content;
    }
    return [e.className, e.id, canonical] as [string, string, string];
  });
  // Rust sorts `Vec<(String, String, String)>` lexicographically by bytes.
  items.sort((a, b) => {
    for (let i = 0; i < 3; i++) {
      const c = Buffer.compare(Buffer.from(a[i], "utf8"), Buffer.from(b[i], "utf8"));
      if (c !== 0) return c;
    }
    return 0;
  });

  for (const name of classNames) frame(name);
  for (const [cls, id, content] of items) {
    frame(cls);
    frame(id);
    frame(content);
  }
  return hasher.digest("hex");
}

/**
 * Mirror of `flow_evaluator::evaluate_requires` + `requires_query_input`:
 * run each `requires` ModelQuery through `perspective.modelQuery` and collect
 * the matched instances as evidence, deduplicated by id in first-seen order.
 *
 * `linkedTo: "base"` compiles to `{ parent: { id: <base>, predicate:
 * "ad4m://has_child" } }`; `linkedTo: "flow"` anchors on the instance URI
 * instead. `didProperty` injects `{ [prop]: <acting did> }` into `where`.
 */
async function collectEvidence(
  p: PerspectiveProxy,
  requires: ModelQuery[],
  baseUri: string,
  instanceUri: string,
  actingDid: string,
): Promise<{ classNames: string[]; evidence: EvidenceItem[] }> {
  const classNames: string[] = [];
  const evidence: EvidenceItem[] = [];

  for (const q of requires) {
    const where: Record<string, any> = { ...(q.where ?? {}) };
    if (q.didProperty) where[q.didProperty] = actingDid;

    const input: Record<string, any> = {};
    if (Object.keys(where).length > 0) input.where = where;
    if (q.linkedTo) {
      const anchor =
        typeof q.linkedTo === "string"
          ? { to: q.linkedTo, via: "ad4m://has_child" }
          : { to: q.linkedTo.to, via: q.linkedTo.via };
      input.parent = {
        id: anchor.to === "base" ? baseUri : instanceUri,
        predicate: anchor.via,
      };
    }

    const result = await p.modelQuery(q.className, JSON.stringify(input));
    const matched = result.instances ?? [];

    // Cardinality: unset `count` means "at least one match".
    const min = q.count?.min ?? (q.count?.max !== undefined ? 0 : 1);
    const max = q.count?.max;
    if (matched.length < min || (max !== undefined && matched.length > max)) {
      throw new Error(
        `requires guard unmet: ${q.className} matched ${matched.length}, ` +
          `needed min=${min}${max !== undefined ? ` max=${max}` : ""}`,
      );
    }

    if (!classNames.includes(q.className)) classNames.push(q.className);
    for (const inst of matched) {
      const id = inst?.id;
      if (typeof id !== "string") continue;
      if (evidence.some((e) => e.id === id)) continue;
      // The Rust side seals `inst.to_string()` — the instance exactly as
      // `model_query` returned it. `evidence_hash` re-parses and canonicalises
      // it, so key order does not matter; content does.
      evidence.push({ id, className: q.className, content: JSON.stringify(inst) });
    }
  }

  return { classNames, evidence };
}

/**
 * GAP 1 — THE MISSING `proposeTransition`.
 *
 * This is the function a WE UI needs behind its "Start" / "Submit for review"
 * / "Mark done" buttons, and it does not exist. Everything here is the client
 * doing the engine's job:
 *
 *   - looking up the target state's `requires` guard off the definition,
 *   - re-running it through `modelQuery` to collect evidence,
 *   - sealing that evidence with a reimplemented hash (GAP 2),
 *   - writing a `FlowTransitionProposal` instance directly.
 *
 * The only part that is legitimately app-side is "which transition did the
 * user pick". The rest is engine knowledge leaking into every client that
 * wants a button.
 *
 * Note the one thing this DOES get right by construction: the proposal is
 * written by the proposer's own client, so every link is signed by them. The
 * engine reads a proposal's fields only from links the proposer signed
 * themselves (`atom.rs::from_links`), and the mint is the proposer's own first
 * vote.
 */
async function proposeTransition(
  p: PerspectiveProxy,
  flow: SHACLFlow,
  instance: FlowInstance,
  fromState: string,
  toState: string,
  proposerDid: string,
  rationale?: string,
): Promise<FlowTransitionProposal> {
  const state = flow.states.find((s) => s.name === toState);
  if (!state) throw new Error(`no state "${toState}" on flow "${flow.name}"`);

  const requires = state.requires ?? [];
  // GAP 3: a state with no `requires` cannot be proposed into at all —
  // `recompute_evidence_hash` returns `None` for a guard-less state and
  // `accept.rs` treats `None` as "cannot reproduce", refusing every vote.
  // An empty seal is separately rejected as `AtomRejection::EmptySeal`.
  // So the engine silently requires every votable state to carry a guard,
  // and nothing in the type system or `addState()` says so.
  if (requires.length === 0) {
    throw new Error(
      `state "${toState}" carries no \`requires\` guard — the engine cannot seal ` +
        `a proposal into it, so no vote on it can ever be accepted (GAP 3)`,
    );
  }

  const { classNames, evidence } = await collectEvidence(
    p,
    requires,
    instance.subject,
    instance.uri,
    proposerDid,
  );
  const seal = evidenceHash(classNames, evidence);

  // The engine mints proposals with `create_subject` on the hardwired
  // `FlowTransitionProposal` class (`flow_classes.rs::write_flow_transition_proposal`),
  // so creating the @Model instance produces byte-identical links.
  const proposal = (await (FlowTransitionProposal as any).create(p, {
    flowInstance: instance.uri,
    fromState,
    toState,
    proposer: proposerDid,
    evidenceHashes: seal,
    evidence: evidence.map((e) => e.id),
    ...(rationale ? { rationale } : {}),
  })) as FlowTransitionProposal;

  // GAP 4a — MINTING A PROPOSAL DOES NOT DERIVE ANYTHING.
  // `schedule_flow_consensus_pass` is called from exactly one place:
  // `perspective_instance.rs:1601`, inside `diff_from_link_language` — the
  // INBOUND SYNC path. A local write never queues a pass. So on a local
  // perspective (and for every write an app makes on its own replica) the
  // proposal lands and the derived state simply never materialises.
  //
  // The one client-reachable trigger is `acceptProposal`, which ends in
  // `run_flow_consensus_pass`. Re-signing a proposal this DID already signed
  // writes no link (the `already` branch of `accept.rs`) but still runs the
  // pass — so the proposer accepting their own mint is a no-op vote used
  // purely as a "derive now" call. That is what this line is.
  //
  // It has one happy side effect worth keeping even after GAP 1 is closed:
  // `acceptProposal` re-verifies the seal, so a client whose hash mirror has
  // drifted finds out HERE, at mint time, with a clear error — instead of
  // silently minting proposals nobody can ever co-sign.
  await instance.acceptProposal(proposal.id);
  return proposal;
}

// ═══════════════════════════════════════════════════════════════════════════
// Small helpers
// ═══════════════════════════════════════════════════════════════════════════

const HAS_CHILD = "ad4m://has_child";

/** Create a child instance under an anchor, the way `linkedTo` expects it. */
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

/**
 * GAP 4 — NO "DERIVE NOW", AND NO PASS AT ALL ON LOCAL WRITES.
 *
 * `FlowInstance.currentStateName` reads `ad4m://flow/current_state`, a `Local`
 * link only this replica's own consensus pass writes. Two consequences an app
 * hits immediately:
 *
 *   (a) The pass is scheduled only from `diff_from_link_language`
 *       (`perspective_instance.rs:1601`) — the inbound sync path. Nothing a
 *       client writes locally queues one. On a perspective that is not a
 *       neighbourhood, the derived state NEVER materialises on its own.
 *   (b) `rejectProposal` runs no pass either, so retracting a settling vote
 *       leaves your cache claiming the state it had before the retraction.
 *
 * There is no `instance.derive()` / `perspective.deriveFlowState(uri)` and no
 * subscription (GAP 6). The only client-reachable trigger is `acceptProposal`,
 * and re-signing a proposal you already signed writes nothing while still
 * running the pass — so this helper asks an agent to re-affirm a vote it has
 * already cast, purely to force a re-derivation. The fact that this helper has
 * to exist IS the finding.
 */
async function deriveNow(
  p: PerspectiveProxy,
  subject: string,
  alreadySignedProposalUri: string,
): Promise<FlowInstance> {
  const inst = await instanceOn(p, subject);
  await inst.acceptProposal(alreadySignedProposalUri);
  return instanceOn(p, subject);
}

/** Re-read a live instance handle for a given agent's perspective. */
async function instanceOn(p: PerspectiveProxy, subject: string): Promise<FlowInstance> {
  const found = await FlowInstance.findAll(p, { subject });
  expect(found, `expected exactly one flow instance on ${subject}`).to.have.lengthOf(1);
  return found[0];
}

// ═══════════════════════════════════════════════════════════════════════════
// FLOW DEFINITIONS
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Ready(0) → InProgress(1) → InReview(2) → Done(3).
 *
 * Guards that mean something:
 *   InProgress — the task carries a WorkLog (someone actually picked it up)
 *   InReview   — the task carries a Deliverable (there is something to review)
 *   Done       — the Deliverable is still there AND a ReviewNote exists,
 *                and two distinct agents must sign.
 *
 * Note every non-initial state carries a guard. That is not stylistic: see
 * GAP 3. `consensusRule` is per-state and gates ENTRY into that state.
 */
function makeTaskFlow(doneRule: ConsensusRule): SHACLFlow {
  const flow = new SHACLFlow("TaskFlow", "we://");
  flow.inputTypes = ["HandoverTask"];
  flow.interpretationHint =
    "Advance a Task through delivery: Ready → InProgress → InReview → Done.";
  flow.consensusRule = { n: 1 };

  flow.addState({
    name: "Ready",
    value: 0,
    interpretationHint: "The task is described and waiting for someone to pick it up.",
  });
  flow.addState({
    name: "InProgress",
    value: 1,
    interpretationHint: "Someone has started work and said so.",
    requires: [{ className: "HandoverWorkLog", linkedTo: "base", count: { min: 1 } }],
    consensusRule: { n: 1 },
  });
  flow.addState({
    name: "InReview",
    value: 2,
    interpretationHint: "The task has produced a deliverable that is ready to be reviewed.",
    requires: [{ className: "HandoverDeliverable", linkedTo: "base", count: { min: 1 } }],
    consensusRule: { n: 1 },
  });
  flow.addState({
    name: "Done",
    value: 3,
    interpretationHint: "The deliverable was reviewed and accepted.",
    requires: [
      { className: "HandoverDeliverable", linkedTo: "base", count: { min: 1 } },
      { className: "HandoverReviewNote", linkedTo: "base", count: { min: 1 } },
    ],
    consensusRule: doneRule,
  });

  // GAP 5: `actions` is a declared member of FlowTransition
  // (`actions: AD4MAction[]`) and grep shows every single construction site in
  // the repo passes `[]`. Nothing executes them. This is the natural place to
  // hang "when this fires, mint the role instance" / "notify the assignee",
  // and it is the cheap version of the fix for GAP 7 (see Part 2).
  flow.addTransition({ actionName: "Start", fromState: "Ready", toState: "InProgress", actions: [] });
  flow.addTransition({
    actionName: "SubmitForReview",
    fromState: "InProgress",
    toState: "InReview",
    actions: [],
  });
  flow.addTransition({ actionName: "Accept", fromState: "InReview", toState: "Done", actions: [] });
  return flow;
}

/**
 * Proposed(0) → Granted(1), all three agents must sign.
 *
 * The base expression of this flow is a `ReviewerRole` INSTANCE. That is the
 * "granting a role is itself a flow" shape: the role object exists from the
 * moment it is proposed, and the flow is supposed to say whether it counts.
 * Part 2 shows that nothing connects those two facts.
 */
function makeGrantFlow(): SHACLFlow {
  const flow = new SHACLFlow("ReviewerRoleGrant", "we://");
  flow.inputTypes = ["HandoverReviewerRole"];
  flow.interpretationHint =
    "Decide whether a proposed reviewer role is granted. Everyone must agree.";

  flow.addState({
    name: "Proposed",
    value: 0,
    interpretationHint: "Someone has suggested this agent as a reviewer for this domain.",
  });
  flow.addState({
    name: "Granted",
    value: 1,
    interpretationHint: "The whole group endorsed this agent as a reviewer for this domain.",
    requires: [{ className: "HandoverGrantEndorsement", linkedTo: "base", count: { min: 1 } }],
    consensusRule: { n: 3 },
  });
  flow.addTransition({ actionName: "Grant", fromState: "Proposed", toState: "Granted", actions: [] });
  return flow;
}

// ═══════════════════════════════════════════════════════════════════════════
// THE SUITE
// ═══════════════════════════════════════════════════════════════════════════

describe("Flow engine handover — a task flow driven by three agents", function () {
  this.timeout(900_000);

  let agent: AgentHandle;
  let admin: Ad4mClient;
  let alice: Ad4mClient, bob: Ad4mClient, charlie: Ad4mClient;
  let aliceDid: string, bobDid: string, charlieDid: string;

  before(async () => {
    // Own executor, not the model suite's shared one: multi-user mode is a
    // runtime-wide switch and must not leak into the other model tests.
    agent = await startAgent("flow-task-handover");
    admin = agent.client;
    // `startAgent` already flips multi-user on; repeated here because this
    // suite's whole premise is three distinct DIDs on one executor and a
    // future change to the helper must break loudly, not silently collapse
    // every vote onto one agent.
    await admin.runtime.setMultiUserEnabled(true);

    const users = [
      { email: "alice@flowhandover.local", password: "password" },
      { email: "bob@flowhandover.local", password: "password" },
      { email: "charlie@flowhandover.local", password: "password" },
    ];
    const clients: Ad4mClient[] = [];
    for (const u of users) {
      await admin.agent.createUser(u.email, u.password);
      await admin.runtime.setUserFreeAccess(u.email, true);
      const token = await admin.agent.loginUser(u.email, u.password);
      const c = new Ad4mClient(baseUrl(agent.apiPort), token, false);
      await c.agent.me(); // touches last_seen, which the supervisor filters on
      clients.push(c);
    }
    [alice, bob, charlie] = clients;
    aliceDid = (await alice.agent.me()).did;
    bobDid = (await bob.agent.me()).did;
    charlieDid = (await charlie.agent.me()).did;

    expect(new Set([aliceDid, bobDid, charlieDid]).size, "three distinct DIDs").to.equal(3);
  });

  after(async () => {
    if (agent) await agent.stop();
  });

  /** One shared perspective, one proxy per agent. */
  async function sharedPerspective(name: string): Promise<{
    aliceP: PerspectiveProxy;
    bobP: PerspectiveProxy;
    charlieP: PerspectiveProxy;
  }> {
    const handle = await alice.perspective.add(name);
    const aliceP = (await alice.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    const bobP = (await bob.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    const charlieP = (await charlie.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    expect(aliceP, "Alice must see her own perspective").to.exist;
    expect(bobP, "Bob (managed user, same executor) must resolve Alice's perspective").to.exist;
    expect(charlieP, "Charlie must resolve Alice's perspective").to.exist;

    for (const p of [aliceP]) {
      await (Task as any).register(p);
      await (WorkLog as any).register(p);
      await (Deliverable as any).register(p);
      await (ReviewNote as any).register(p);
      await (ReviewerRole as any).register(p);
      await (GrantEndorsement as any).register(p);
    }
    return { aliceP, bobP, charlieP };
  }

  // ═════════════════════════════════════════════════════════════════════════
  // PART 1 — the plain task flow
  // ═════════════════════════════════════════════════════════════════════════
  describe("Part 1 — Ready → InProgress → InReview → Done, with real guards", function () {
    it("walks the full lifecycle and regresses when a settling vote is retracted", async () => {
      const { aliceP, bobP, charlieP } = await sharedPerspective("flow-handover-part1");

      // ── 1. Define the process ──────────────────────────────────────────
      const flow = makeTaskFlow({ n: 2 });
      await aliceP.addFlow("TaskFlow", flow);

      // ── 2. Alice files a task and starts a run on it ───────────────────
      const task = (await (Task as any).create(aliceP, {
        title: "Ship the flow handover",
        description: "Prove the engine drives a realistic task flow end to end.",
        assignee: bobDid,
      })) as Task;

      // A UI asks this before offering a "start a flow" menu.
      const available = await aliceP.availableFlows(task.id);
      expect(available, "TaskFlow must be offered on a HandoverTask").to.include("TaskFlow");

      const started = await FlowInstance.start(aliceP, "TaskFlow", task.id);
      expect(started.currentStateName, "a run begins in the lowest-valued state").to.equal("Ready");
      expect(
        started.availableTransitions.map((t) => t.actionName),
        "the buttons a UI renders in Ready",
      ).to.deep.equal(["Start"]);

      // ── 3. GAP 3, demonstrated up front ────────────────────────────────
      // `Ready` is the only state in this flow with no `requires` guard, so it
      // is the only state nothing can move INTO. That sounds harmless for an
      // initial state — but the same rule applies to every other state, and it
      // is invisible: `addState()` accepts a guard-less state happily, and the
      // failure surfaces only when a voter's `acceptProposal` is refused with
      // "cites evidence this replica cannot reproduce". The cause is
      // `recompute_evidence_hash` returning `None` for a guard-less state and
      // `accept.rs` comparing that `None` against the proposal's seal.
      //
      // An app author designing a flow will absolutely write a state like
      // "Cancelled" with no guard and then wonder why nobody can vote for it.
      // What the engine wants is either a documented rule enforced at
      // `addFlow` time, or a defined seal for the empty guard.
      let guardless = "";
      try {
        await proposeTransition(aliceP, flow, started, "InProgress", "Ready", aliceDid);
      } catch (e: any) {
        guardless = String(e?.message ?? e);
      }
      expect(guardless, "a guard-less target state is unproposable").to.contain("GAP 3");

      // ── 4. Bob picks the task up: writes a WorkLog, then proposes Start ─
      await createUnder<WorkLog>(WorkLog, bobP, task.id, {
        note: "Picked this up, starting on the harness.",
      });

      const bobInst = await instanceOn(bobP, task.id);
      const startProposal = await proposeTransition(
        bobP,
        flow,
        bobInst,
        "Ready",
        "InProgress",
        bobDid,
        "I have started — see the work log.",
      );

      // `{ n: 1 }` into InProgress: the proposer's own mint IS the first vote,
      // so this edge settles on the pass `proposeTransition` had to force.
      const afterStart = await instanceOn(bobP, task.id);
      expect(afterStart.currentStateName).to.equal("InProgress");
      expect(
        afterStart.availableTransitions.map((t) => t.actionName),
      ).to.deep.equal(["SubmitForReview"]);

      // Every member can see the proposal that moved it — this is the audit
      // trail a UI shows next to the state.
      const seen = await (await instanceOn(aliceP, task.id)).proposals();
      expect(seen.map((x) => x.id)).to.include(startProposal.id);
      expect(seen.find((x) => x.id === startProposal.id)!.proposer).to.equal(bobDid);

      // ── 5. Bob produces a deliverable and submits for review ───────────
      await createUnder<Deliverable>(Deliverable, bobP, task.id, {
        title: "flow-task-handover.test.ts",
        url: "https://github.com/coasys/ad4m/pull/handover",
      });

      await proposeTransition(
        bobP,
        flow,
        await instanceOn(bobP, task.id),
        "InProgress",
        "InReview",
        bobDid,
      );
      const afterSubmit = await instanceOn(bobP, task.id);
      expect(afterSubmit.currentStateName).to.equal("InReview");

      // ── 6. The Done edge: two distinct signatures ──────────────────────
      // Alice reviews and writes her note FIRST. The seal is computed over the
      // graph as it stands at mint time, so evidence added after the mint
      // invalidates every pending vote — see the comment on GAP 2b below.
      await createUnder<ReviewNote>(ReviewNote, aliceP, task.id, {
        body: "Reviewed. The module doc is the handover; guards are real.",
      });

      const aliceInst = await instanceOn(aliceP, task.id);
      const doneProposal = await proposeTransition(
        aliceP,
        flow,
        aliceInst,
        "InReview",
        "Done",
        aliceDid,
        "Deliverable is there and I have reviewed it.",
      );

      // One vote of two. The flow has NOT moved, and that is a success.
      const stillInReview = await instanceOn(aliceP, task.id);
      expect(
        stillInReview.currentStateName,
        "one of two signatures must not advance the flow",
      ).to.equal("InReview");

      // Bob co-signs. His replica recomputes the seal on his own graph before
      // signing; a mismatch would make this throw rather than half-accept.
      const bobsInst = await instanceOn(bobP, task.id);
      const fired: FlowFireOutcome[] = await bobsInst.acceptProposal(doneProposal.id);

      expect(fired, "the settling vote reports what fired").to.have.lengthOf(1);
      expect(fired[0].fromState).to.equal("InReview");
      expect(fired[0].toState).to.equal("Done");
      expect(fired[0].voters).to.have.members([aliceDid, bobDid]);
      expect(fired[0].contributingProposalUris).to.include(doneProposal.id);

      const done = await instanceOn(bobP, task.id);
      expect(done.currentStateName).to.equal("Done");
      expect(done.availableTransitions, "Done is terminal — no buttons").to.deep.equal([]);

      // ── 7. THE REGRESSION PROPERTY ─────────────────────────────────────
      // This is the part every app author gets wrong on first reading, so it
      // is asserted rather than documented. State is a function of the links
      // present NOW. Bob withdraws the signature that settled the Done edge;
      // the edge is 1 < 2 again, the walk stops earlier, and the flow stands
      // in InReview. Nothing was "undone" — the same derivation ran over one
      // link fewer. There is deliberately no "already fired" guard.
      const retracted = await bobsInst.rejectProposal(doneProposal.id);
      expect(retracted, "exactly one link goes: Bob's acceptedBy").to.equal(1);

      // GAP 4b, visible: `rejectProposal` runs no consensus pass, so Bob's own
      // cache still claims "Done" — a UI that renders `currentStateName` right
      // after a retraction shows a state the graph no longer supports.
      expect(
        (await instanceOn(bobP, task.id)).currentStateName,
        "the Local cache is stale immediately after a retraction (GAP 4b)",
      ).to.equal("Done");

      // Alice re-affirms the vote she already cast when she minted. No link is
      // written; the pass runs; the truth comes out.
      const regressed = await deriveNow(aliceP, task.id, doneProposal.id);
      expect(
        regressed.currentStateName,
        "retracting a settling vote moves the flow BACK — this is the contract, not a bug",
      ).to.equal("InReview");

      // Alice's proposal is untouched and still standing: reject withdraws
      // your own links, it does not cancel anyone else's proposal.
      const survivors = await regressed.proposals();
      expect(
        survivors.map((x) => x.id),
        "Alice's proposal survives Bob's retraction",
      ).to.include(doneProposal.id);

      // And Bob can sign again — the same proposal re-settles the same edge.
      const refired = await (await instanceOn(bobP, task.id)).acceptProposal(doneProposal.id);
      expect(refired.map((f) => f.toState)).to.include("Done");

      // GAP 4c — CHARLIE IS THE PURE READER, AND HE IS STUCK.
      // Charlie has signed nothing on this flow. `acceptProposal` is the only
      // client-reachable way to run a pass, so the one thing that would give
      // him a derived state is casting a vote he may not want to cast. His
      // cache is therefore whatever it was — on a local perspective, nothing.
      // Every list view in WE is this case.
      const charlieSees = await instanceOn(charlieP, task.id);
      expect(
        charlieSees.currentStateName,
        "a reader who has not voted has no way to derive — this is GAP 4, asserted",
      ).to.not.equal("Done");
    });
  });

  // ═════════════════════════════════════════════════════════════════════════
  // PART 2 — domain reviewer roles, and the hole
  // ═════════════════════════════════════════════════════════════════════════
  describe("Part 2 — domain reviewer roles granted by their own flow", function () {
    it("grants a frontend reviewer role by unanimous flow, and that reviewer's vote counts", async () => {
      const { aliceP, bobP, charlieP } = await sharedPerspective("flow-handover-part2-granted");

      await aliceP.addFlow("ReviewerRoleGrant", makeGrantFlow());

      // Three domains. Only the frontend one gates the task flow below.
      const frontend = (await (ReviewerRole as any).create(aliceP, {
        agent: bobDid,
        domain: "frontend",
      })) as ReviewerRole;
      await (ReviewerRole as any).create(aliceP, { agent: charlieDid, domain: "backend" });
      await (ReviewerRole as any).create(aliceP, { agent: aliceDid, domain: "ad4m" });

      // ── The grant flow runs ON the role instance ───────────────────────
      const grantFlow = (await aliceP.getFlow("ReviewerRoleGrant")) as SHACLFlow;
      const grant = await FlowInstance.start(aliceP, "ReviewerRoleGrant", frontend.id);
      expect(grant.currentStateName).to.equal("Proposed");

      // GAP 2b — THE SEAL FREEZES THE GRAPH AT MINT TIME.
      // Every endorsement must exist BEFORE the proposal is minted. The seal
      // is computed over the guard's matches at that moment; an endorsement
      // added afterwards changes what the guard returns, so every pending
      // voter recomputes a different hash and refuses to co-sign a proposal
      // they agree with. A UI that lets people endorse and vote concurrently
      // will deadlock on this, and nothing in the API warns it.
      // What an app wants: either a re-seal call, or a proposal that seals
      // only the evidence it explicitly cites.
      await createUnder<GrantEndorsement>(GrantEndorsement, aliceP, frontend.id, {
        reason: "Bob has reviewed every frontend PR this quarter.",
      });
      await createUnder<GrantEndorsement>(GrantEndorsement, bobP, frontend.id, {
        reason: "Happy to take it on.",
      });
      await createUnder<GrantEndorsement>(GrantEndorsement, charlieP, frontend.id, {
        reason: "No objection.",
      });

      const grantProposal = await proposeTransition(
        aliceP,
        grantFlow,
        await instanceOn(aliceP, frontend.id),
        "Proposed",
        "Granted",
        aliceDid,
        "All three of us endorsed it.",
      );

      // n: 3 — Alice's mint is vote 1; Bob and Charlie must both sign.
      const afterBob = await (await instanceOn(bobP, frontend.id)).acceptProposal(grantProposal.id);
      expect(afterBob, "two of three is not quorum — an empty outcome is success").to.deep.equal([]);

      const afterCharlie = await (
        await instanceOn(charlieP, frontend.id)
      ).acceptProposal(grantProposal.id);
      expect(afterCharlie, "the third signature settles the grant").to.have.lengthOf(1);
      expect(afterCharlie[0].toState).to.equal("Granted");
      expect(afterCharlie[0].voters).to.have.members([aliceDid, bobDid, charlieDid]);

      const granted = await instanceOn(charlieP, frontend.id);
      expect(granted.currentStateName).to.equal("Granted");

      // ── Now the task flow, with Done gated on frontend reviewers ───────
      const roleGate: ConsensusRule = {
        n: 1,
        fromRole: {
          className: "HandoverReviewerRole",
          where: { domain: "frontend" },
          didProperty: "agent",
        },
      };
      const taskFlow = makeTaskFlow(roleGate);
      await aliceP.addFlow("TaskFlow", taskFlow);

      const task = (await (Task as any).create(aliceP, {
        title: "Restyle the channel list",
        description: "Frontend work — needs a frontend reviewer to close.",
        assignee: aliceDid,
      })) as Task;

      await createUnder<WorkLog>(WorkLog, aliceP, task.id, { note: "Started." });
      await createUnder<Deliverable>(Deliverable, aliceP, task.id, {
        title: "channel-list.css",
        url: "https://example.invalid/pr/1",
      });
      await createUnder<ReviewNote>(ReviewNote, bobP, task.id, { body: "Looks right to me." });

      await FlowInstance.start(aliceP, "TaskFlow", task.id);
      await proposeTransition(
        aliceP,
        taskFlow,
        await instanceOn(aliceP, task.id),
        "Ready",
        "InProgress",
        aliceDid,
      );
      expect((await instanceOn(aliceP, task.id)).currentStateName).to.equal("InProgress");
      await proposeTransition(
        aliceP,
        taskFlow,
        await instanceOn(aliceP, task.id),
        "InProgress",
        "InReview",
        aliceDid,
      );
      expect((await instanceOn(aliceP, task.id)).currentStateName).to.equal("InReview");

      // Alice is the `ad4m` reviewer, not a frontend one. Her proposal into
      // Done is a vote that the gate does not count, so the edge stays open.
      await proposeTransition(
        aliceP,
        taskFlow,
        await instanceOn(aliceP, task.id),
        "InReview",
        "Done",
        aliceDid,
        "I think this is done.",
      );
      expect(
        (await instanceOn(aliceP, task.id)).currentStateName,
        "a vote from outside the role must not settle a fromRole-gated edge",
      ).to.equal("InReview");

      // Bob holds the granted frontend role. His signature settles it.
      const bobFires = await (await instanceOn(bobP, task.id)).acceptProposal(
        (await (await instanceOn(bobP, task.id)).proposals()).find((x) => x.toState === "Done")!.id,
      );
      expect(bobFires, "the frontend reviewer's vote settles Done").to.have.lengthOf(1);
      expect(bobFires[0].toState).to.equal("Done");
      expect(bobFires[0].voters, "the granted frontend reviewer is what settled it").to.include(
        bobDid,
      );
    });

    it(
      "GAP 7 — a role whose grant flow is still Proposed is ALREADY accepted by the gate",
      async () => {
        // ═══════════════════════════════════════════════════════════════════
        // THE FINDING, AS AN EXECUTABLE STATEMENT.
        //
        // Nico's question was: can a role's validity be bound to the
        // completion of the role-granting flow with what the engine has today?
        //
        // No, and the reason is structural, not a missing feature:
        //
        //   `fromRole` is a ModelQuery over graph DATA. It matches role
        //   INSTANCES. A flow's completion is DERIVED and never stored, so
        //   there is nothing in the graph for a ModelQuery to match on.
        //
        // The two obvious fixes are both wrong:
        //
        //   (a) "filter on the instance's currentState == Granted" —
        //       `currentState` is a `Local` link (FlowModels.ts:157,
        //       flow_instance/pass.rs: "cache for readers, never authority").
        //       Local links do not sync, so the same query would return a
        //       different eligible-voter set on every replica, destroying the
        //       one property the engine rests on: same links in, same state
        //       out.
        //
        //   (b) "have the flow write granted=true when it fires" — this is
        //       precisely the anti-pattern `flow_instance/roles.rs` was
        //       written to prevent: "role instances are add-only, and
        //       membership ends only through tombstones… a query keyed on a
        //       mutable property reopens the deletion problem through the side
        //       door: flipping the property makes the instance vanish from
        //       historical verdicts too, un-settling edges its votes once
        //       settled."
        //
        // What the code itself names as the fix (roles.rs, "accepted caveat"):
        //   "Roles granted as flow outputs — the planned recursive composition
        //    — will carry a quorum-fixed time no single party can back-date."
        // Concretely, let `fromRole` name a flow terminal state as well as a
        // class:
        //
        //   fromRole: {
        //     className: "HandoverReviewerRole",
        //     where: { domain: "frontend" },
        //     didProperty: "agent",
        //     grantedByFlow: { flow: "ReviewerRoleGrant", terminalState: "Granted" },  // NEW
        //   }
        //
        // The resolver would then derive the grant flow's state over each
        // matched role instance and count it only at the terminal state — with
        // `granted_at` being the timestamp of the SETTLING VOTE, which is the
        // quorum-fixed, un-back-datable time roles.rs says it wants. It needs
        // a depth cap for the recursive case (a role flow gated on its own
        // role).
        //
        // Until then, the assertion below is true: creating the role instance
        // IS the grant. The flow around it is decoration.
        // ═══════════════════════════════════════════════════════════════════
        const { aliceP, bobP, charlieP } = await sharedPerspective("flow-handover-part2-hole");

        await aliceP.addFlow("ReviewerRoleGrant", makeGrantFlow());

        // Charlie is PROPOSED as a frontend reviewer. Nobody has endorsed him,
        // nobody has voted, the grant flow is sitting in `Proposed`.
        const ungranted = (await (ReviewerRole as any).create(aliceP, {
          agent: charlieDid,
          domain: "frontend",
        })) as ReviewerRole;
        const grant = await FlowInstance.start(aliceP, "ReviewerRoleGrant", ungranted.id);
        expect(
          grant.currentStateName,
          "the grant flow has NOT completed — nobody has signed anything",
        ).to.equal("Proposed");
        expect(
          await grant.proposals(),
          "and there is not even a proposal to grant it",
        ).to.deep.equal([]);

        // A task whose Done edge is gated on frontend reviewers.
        const taskFlow = makeTaskFlow({
          n: 1,
          fromRole: {
            className: "HandoverReviewerRole",
            where: { domain: "frontend" },
            didProperty: "agent",
          },
        });
        await aliceP.addFlow("TaskFlow", taskFlow);

        const task = (await (Task as any).create(aliceP, {
          title: "Something only a frontend reviewer may close",
          description: "The gate is the whole point of this task.",
          assignee: aliceDid,
        })) as Task;
        await createUnder<WorkLog>(WorkLog, aliceP, task.id, { note: "Started." });
        await createUnder<Deliverable>(Deliverable, aliceP, task.id, {
          title: "thing.tsx",
          url: "https://example.invalid/pr/2",
        });
        await createUnder<ReviewNote>(ReviewNote, bobP, task.id, { body: "LGTM." });

        await FlowInstance.start(aliceP, "TaskFlow", task.id);
        await proposeTransition(
          aliceP,
          taskFlow,
          await instanceOn(aliceP, task.id),
          "Ready",
          "InProgress",
          aliceDid,
        );
        expect((await instanceOn(aliceP, task.id)).currentStateName).to.equal("InProgress");
        await proposeTransition(
          aliceP,
          taskFlow,
          await instanceOn(aliceP, task.id),
          "InProgress",
          "InReview",
          aliceDid,
        );
        expect((await instanceOn(aliceP, task.id)).currentStateName).to.equal("InReview");

        // Charlie — whose grant flow never left `Proposed` — proposes Done.
        // His own mint is his own vote, and `{ n: 1, fromRole: frontend }`
        // counts it, because the gate asks only "is there a HandoverReviewerRole
        // instance with domain=frontend and agent=charlie?" and there is.
        await proposeTransition(
          charlieP,
          taskFlow,
          await instanceOn(charlieP, task.id),
          "InReview",
          "Done",
          charlieDid,
          "Closing this as the (allegedly) frontend reviewer.",
        );

        const settled = await instanceOn(charlieP, task.id);
        expect(
          settled.currentStateName,
          "THE HOLE: an ungranted reviewer settled a fromRole-gated edge, because " +
            "`fromRole` matches the role INSTANCE and cannot see that the flow " +
            "which was supposed to grant it never completed",
        ).to.equal("Done");

        // Belt and braces: the grant flow is still, demonstrably, Proposed.
        const grantNow = await instanceOn(aliceP, ungranted.id);
        expect(
          grantNow.currentStateName,
          "and the grant flow is STILL in Proposed while its holder closes tasks",
        ).to.equal("Proposed");
      },
    );
  });
});
