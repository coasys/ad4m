/**
 * FlowTransitionProposal — @Model class that mirrors the Rust-side hardwired
 * SDNA for the flow runtime (design doc §4.2).
 *
 * These tests verify that:
 *   1. The TS SHACL shape can be registered in a perspective.
 *   2. `FlowTransitionProposal.findAll()` returns nodes that carry the
 *      discriminating `ad4m://flow/instance` link — the pending-proposals
 *      query a Flux/UI panel will use to show "flow transitions awaiting
 *      review".
 *   3. The TS-generated shape matches the Rust hardwired SDNA JSON — same
 *      parity locking convention as the interpretation-models tests
 *      (2026-08-20 debug: paths matched, names diverged, both hardcoded
 *      Rust-side and TS-side parity tests happily passed, `create_subject`
 *      silently no-op'd all writes).
 *
 * Proposals are normally written by the Rust flow engine (later PR arc).
 * Here we add the raw links manually so the test does not require a running
 * consensus firing — it is a pure graph-layer check.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit \
 *     --require tests/model/hooks.ts tests/model/flow-models.test.ts
 */

import { expect } from "chai";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { Ad4mClient, Link, PerspectiveProxy, SHACLFlow, FlowState } from "@coasys/ad4m";
import { FlowInstance, FlowInstanceRecord, FlowTransitionProposal, FlowTransition } from "@coasys/ad4m";
import { Ad4mModel, Flag, Model, Property } from "@coasys/ad4m";
import { getSharedAgent } from "./hooks.js";
import { startAgent } from "../../helpers/index.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Local @Model class used by the `availableFlows` describe block below —
// declared here rather than in `models.ts` so this file remains standalone.
// Distinct class name (`FlowTestPost`) so co-running tests can't collide.
@Model({ name: "FlowTestPost" })
class TestPostForFlows extends Ad4mModel {
  @Flag({ through: "flow-test://post_type", value: "flow-test://post" })
  type = "flow-test://post";

  @Property({ through: "flow-test://title", required: true })
  title: string = "";

  @Property({ through: "flow-test://body" })
  body: string = "";
}

describe("FlowTransitionProposal — @Model", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-models");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-models-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await FlowTransitionProposal.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  it("FlowTransitionProposal.findAll() returns proposals by flow-instance link", async () => {
    // Simulate what the Rust flow engine writes when consensus firing lands.
    const proposal = "ad4m://flow/proposal/uuid-1";
    const flowInstance = "ad4m://flow/instance/inst-1";
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/instance",
      target: `literal:string:${flowInstance}`,
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/from_state",
      target: "literal:string:collectingPerspectives",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/to_state",
      target: "literal:string:tensionIdentified",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/proposer",
      target: "literal:string:did:example:alice",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/evidence_hashes",
      target: "literal:string:{}",
    }));

    const proposals = await FlowTransitionProposal.findAll(p);
    expect(proposals).to.have.lengthOf(1);
    expect(proposals[0].flowInstance).to.equal(flowInstance);
    expect(proposals[0].fromState).to.equal("collectingPerspectives");
    expect(proposals[0].toState).to.equal("tensionIdentified");
    expect(proposals[0].proposer).to.equal("did:example:alice");
    expect(proposals[0].evidenceHashes).to.equal("{}");
    // "When was this proposed?" is answered by Ad4mModel's built-in
    // `createdAt`, synthesised on hydration from the earliest link
    // timestamp — verified in the FlowInstance suite below.
  });

  it("FlowTransitionProposal round-trip: evidence + optional runUri/rationale hydrate", async () => {
    const proposal = "ad4m://flow/proposal/uuid-2";
    // Minimal required scaffold.
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/instance",
      target: "literal:string:ad4m://flow/instance/inst-2",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/from_state",
      target: "literal:string:inProgress",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/to_state",
      target: "literal:string:review",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/proposer",
      target: "literal:string:did:example:bob",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/evidence_hashes",
      target: 'literal:string:{"ad4m://task/1":"aa"}',
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/created_at",
      target: "literal:string:2026-08-26T09:05:00Z",
    }));
    // Evidence collection + optionals.
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/evidence",
      target: "literal:string:ad4m://task/1",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/evidence",
      target: "literal:string:ad4m://task/2",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/run_uri",
      target: "literal:string:ad4m://interp/run/run-abc",
    }));
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/rationale",
      target: "literal:string:PR is up and the review checklist is unchecked.",
    }));

    const proposals = await FlowTransitionProposal.findAll(p);
    expect(proposals).to.have.lengthOf(1);
    const proposalInstance = proposals[0];
    expect(proposalInstance.evidence).to.have.members([
      "ad4m://task/1",
      "ad4m://task/2",
    ]);
    expect(proposalInstance.runUri).to.equal("ad4m://interp/run/run-abc");
    expect(proposalInstance.rationale).to.equal(
      "PR is up and the review checklist is unchecked.",
    );
  });

  // ── SDNA parity — TS @Model shape agrees with Rust hardwired SDNA ──────────

  const SDNA_DIR = path.resolve(
    __dirname,
    "../../../../rust-executor/src/perspectives/hardwired_sdna",
  );

  function loadSdnaPathNamePairs(fileName: string): Map<string, string> {
    const raw = fs.readFileSync(path.join(SDNA_DIR, fileName), "utf-8");
    const parsed = JSON.parse(raw) as {
      properties: Array<{ path: string; name: string }>;
    };
    return new Map(parsed.properties.map((prop) => [prop.path, prop.name]));
  }

  function loadSdnaTargetClass(fileName: string): string {
    const raw = fs.readFileSync(path.join(SDNA_DIR, fileName), "utf-8");
    return (JSON.parse(raw) as { target_class: string }).target_class;
  }

  it("FlowTransitionProposal @Model shape matches Rust flow_transition_proposal.json", () => {
    const { shape } = (FlowTransitionProposal as any).generateSHACL();
    const actual = new Map<string, string>(
      shape.properties.map((prop: any): [string, string] => [prop.path, prop.name]),
    );

    const expectedTargetClass = loadSdnaTargetClass("flow_transition_proposal.json");
    const expected = loadSdnaPathNamePairs("flow_transition_proposal.json");

    expect(shape.targetClass, "target class must match Rust SDNA")
      .to.equal(expectedTargetClass);
    expect(actual, "TS shape must match Rust SDNA path→name pairs")
      .to.deep.equal(expected);
  });
});

describe("FlowInstanceRecord — @Model", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-models-instance");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-models-instance-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await FlowInstanceRecord.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  it("FlowInstanceRecord.findAll() returns records by flowUri discriminator", async () => {
    // Simulate what the Rust flow engine writes when startFlow lands.
    // Post-J#5: record stores the flow's canonical URI, not the bare name.
    const instance = "ad4m://flow/instance/inst-1";
    await p.add(new Link({
      source: instance,
      predicate: "ad4m://flow/flow_uri",
      target: "literal:string:delivery%3A%2F%2FDeliveryFlow",
    }));
    await p.add(new Link({
      source: instance,
      predicate: "ad4m://flow/base",
      target: "literal:string:ad4m%3A%2F%2Fsome-subject",
    }));
    await p.add(new Link({
      source: instance,
      predicate: "ad4m://flow/current_state",
      target: "literal:string:Scoped",
    }));

    const all = await FlowInstanceRecord.findAll(p);
    expect(all.length).to.equal(1);
    expect(all[0].flowUri).to.equal("delivery://DeliveryFlow");
    expect(all[0].subject).to.equal("ad4m://some-subject");
    expect(all[0].currentState).to.equal("Scoped");
    // `createdAt` is Ad4mModel's synthesised earliest-link timestamp
    // (ms since epoch after hydration); presence is enough for parity.
    expect(all[0].createdAt).to.be.a("number");
  });

  it("FlowInstanceRecord @Model shape matches Rust flow_instance.json", () => {
    const SDNA_DIR = path.resolve(
      __dirname,
      "../../../../rust-executor/src/perspectives/hardwired_sdna",
    );
    const raw = fs.readFileSync(path.join(SDNA_DIR, "flow_instance.json"), "utf-8");
    const parsed = JSON.parse(raw) as {
      target_class: string;
      properties: Array<{ path: string; name: string }>;
    };

    const { shape } = (FlowInstanceRecord as any).generateSHACL();
    const actual = new Map<string, string>(
      shape.properties.map((prop: any): [string, string] => [prop.path, prop.name]),
    );
    const expected = new Map(parsed.properties.map((prop) => [prop.path, prop.name]));

    expect(shape.targetClass, "target class must match Rust SDNA")
      .to.equal(parsed.target_class);
    expect(actual, "TS shape must match Rust SDNA path→name pairs")
      .to.deep.equal(expected);
  });
});

// ── FlowInstance.start — v5 API (design doc §4.3) ────────────
// Mints an on-graph `FlowInstance` node tied to a base expression, seeded at
// the flow's first declared state. Registration of the hardwired runtime
// classes is idempotent inside the API — callers do NOT pre-register.

describe("FlowInstance.start — v5 API", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-start-instance");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-start-instance-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  function makeDeliveryFlow(): SHACLFlow {
    const flow = new SHACLFlow("Delivery", "flow://");
    flow.inputTypes = ["ad4m://Task"];
    // v5-shape flow: no `stateCheck` on any state so
    // `FlowInstance.start`'s legacy-flow guard admits it (J#2 disjointness).
    const identified: FlowState = { name: "Identified", value: 0 };
    const inProgress: FlowState = { name: "InProgress", value: 1 };
    flow.addState(identified);
    flow.addState(inProgress);
    return flow;
  }

  it("mints a FlowInstance seeded at the first state and returns the hydrated model", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());

    const before = new Date().toISOString();
    const instance = await FlowInstance.start(p, "Delivery", "ad4m://task/1");
    const after = new Date().toISOString();

    expect(instance).to.be.instanceOf(FlowInstance);
    expect(instance.flowName).to.equal("Delivery");
    expect(instance.subject).to.equal("ad4m://task/1");
    expect(instance.currentStateName).to.equal("Identified");
    // `currentState` on the wrapper resolves to the `FlowState` object on
    // the shape whose name matches `currentStateName` — verify the linkage.
    expect(instance.currentState).to.equal(instance.shape.states[0]);
    // Start time = Ad4mModel's synthesised `createdAt` (earliest link
    // timestamp on the instance URI, epoch millis after hydration), surfaced
    // via the wrapper's `startedAtMillis` accessor.
    const beforeMs = new Date(before).getTime();
    const afterMs = new Date(after).getTime();
    const startedAt = instance.startedAtMillis;
    expect(startedAt !== undefined && startedAt >= beforeMs && startedAt <= afterMs,
      `startedAtMillis ${startedAt} must fall in [${beforeMs}, ${afterMs}]`).to.equal(true);

    // Findable via the discriminator predicate — same lookup UIs will use.
    const all = await FlowInstance.findAll(p);
    expect(all.length).to.equal(1);
    expect(all[0].flowName).to.equal("Delivery");
    expect(all[0].subject).to.equal("ad4m://task/1");
    expect(all[0].currentStateName).to.equal("Identified");
  });

  it("supports multiple concurrent instances on distinct bases", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await FlowInstance.start(p, "Delivery", "ad4m://task/a");
    await FlowInstance.start(p, "Delivery", "ad4m://task/b");

    const all = await FlowInstance.findAll(p);
    expect(all.length).to.equal(2);
    const bases = all.map((i) => i.subject).sort();
    expect(bases).to.deep.equal(["ad4m://task/a", "ad4m://task/b"]);
  });

  it("throws when the named flow is not registered on the perspective", async () => {
    let caught: unknown = null;
    try {
      await FlowInstance.start(p, "Nope", "ad4m://task/1");
    } catch (e) {
      caught = e;
    }
    expect(String(caught)).to.match(/Flow "Nope" not found/);
  });

  it("throws when the flow has zero declared states", async () => {
    const zeroState = new SHACLFlow("Like", "flow://");
    // no addState calls — zero-state action flow (§6.3), handled by
    // runFlowAction / fireAction, not FlowInstance.start.
    await p.addFlow("Like", zeroState);

    let caught: unknown = null;
    try {
      await FlowInstance.start(p, "Like", "ad4m://post/1");
    } catch (e) {
      caught = e;
    }
    expect(String(caught)).to.match(/has no states/);
  });
});

// ── FlowInstance.findAll — v5 read side (design doc §4.3 / §5) ──
// Enumerates live FlowInstance records; optionally narrowed by flow-name
// discriminator. Feeds §5 Model C prompt gathering and UI indicators. Class
// registration is idempotent so callers may query BEFORE the first
// FlowInstance.start ever fires without hitting hydration errors.

describe("FlowInstance.findAll — v5 read side", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-get-instances");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-get-instances-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  function makeDeliveryFlow(): SHACLFlow {
    const flow = new SHACLFlow("Delivery", "flow://");
    flow.inputTypes = ["ad4m://Task"];
    flow.addState({ name: "Identified", value: 0 });
    flow.addState({ name: "InProgress", value: 1 });
    return flow;
  }

  function makeDeliberationFlow(): SHACLFlow {
    const flow = new SHACLFlow("Deliberation", "flow://");
    flow.inputTypes = ["ad4m://Proposal"];
    flow.addState({ name: "Proposal", value: 0 });
    return flow;
  }

  it("returns an empty array when no flow instances exist on the perspective", async () => {
    const all = await FlowInstance.findAll(p);
    expect(all).to.deep.equal([]);
  });

  it("does not throw when called before any FlowInstance.start — idempotent registration", async () => {
    // No addFlow / FlowInstance.start beforehand; class must self-register.
    const all = await FlowInstance.findAll(p);
    expect(all).to.deep.equal([]);

    // And a filtered call must be equally safe.
    const filtered = await FlowInstance.findAll(p, "Delivery");
    expect(filtered).to.deep.equal([]);
  });

  it("returns all live instances across flows when called with no filter", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await p.addFlow("Deliberation", makeDeliberationFlow());

    await FlowInstance.start(p, "Delivery", "ad4m://task/1");
    await FlowInstance.start(p, "Delivery", "ad4m://task/2");
    await FlowInstance.start(p, "Deliberation", "ad4m://proposal/1");

    const all = await FlowInstance.findAll(p);
    expect(all.length).to.equal(3);
    expect(all.every((i) => i instanceof FlowInstance)).to.equal(true);
    const flows = all.map((i) => i.flowName).sort();
    expect(flows).to.deep.equal(["Deliberation", "Delivery", "Delivery"]);
  });

  it("narrows by flow-name discriminator when the filter is supplied", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await p.addFlow("Deliberation", makeDeliberationFlow());

    await FlowInstance.start(p, "Delivery", "ad4m://task/a");
    await FlowInstance.start(p, "Delivery", "ad4m://task/b");
    await FlowInstance.start(p, "Deliberation", "ad4m://proposal/x");

    const deliveries = await FlowInstance.findAll(p, "Delivery");
    expect(deliveries.length).to.equal(2);
    expect(deliveries.every((i) => i.flowName === "Delivery")).to.equal(true);
    const bases = deliveries.map((i) => i.subject).sort();
    expect(bases).to.deep.equal(["ad4m://task/a", "ad4m://task/b"]);

    const deliberations = await FlowInstance.findAll(p, "Deliberation");
    expect(deliberations.length).to.equal(1);
    expect(deliberations[0].subject).to.equal("ad4m://proposal/x");

    const misses = await FlowInstance.findAll(p, "Nope");
    expect(misses).to.deep.equal([]);
  });

  // Review criterion 3 (2026-08-27): "Flows can be listed on user requests.
  // Give the address to search on." — the UI or agent hands us a URI and
  // wants every flow running on that expression, regardless of which flow.
  it("narrows by subject URI when { subject } is supplied", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await p.addFlow("Deliberation", makeDeliberationFlow());

    await FlowInstance.start(p, "Delivery", "ad4m://task/target");
    await FlowInstance.start(p, "Deliberation", "ad4m://task/target");
    await FlowInstance.start(p, "Delivery", "ad4m://task/other");

    const onTarget = await FlowInstance.findAll(p, { subject: "ad4m://task/target" });
    expect(onTarget.length).to.equal(2);
    const flows = onTarget.map((i) => i.flowName).sort();
    expect(flows).to.deep.equal(["Deliberation", "Delivery"]);
    expect(onTarget.every((i) => i.subject === "ad4m://task/target")).to.equal(true);

    const missSubject = await FlowInstance.findAll(p, { subject: "ad4m://task/absent" });
    expect(missSubject).to.deep.equal([]);
  });

  it("combines flowName + subject filters into a single AND-joined query", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await p.addFlow("Deliberation", makeDeliberationFlow());

    await FlowInstance.start(p, "Delivery", "ad4m://task/shared");
    await FlowInstance.start(p, "Deliberation", "ad4m://task/shared");
    await FlowInstance.start(p, "Delivery", "ad4m://task/other");

    const deliveriesOnShared = await FlowInstance.findAll(p, {
      flowName: "Delivery",
      subject: "ad4m://task/shared",
    });
    expect(deliveriesOnShared.length).to.equal(1);
    expect(deliveriesOnShared[0].flowName).to.equal("Delivery");
    expect(deliveriesOnShared[0].subject).to.equal("ad4m://task/shared");

    // Both filters must match — Deliberation on a distinct subject returns none.
    const misses = await FlowInstance.findAll(p, {
      flowName: "Deliberation",
      subject: "ad4m://task/other",
    });
    expect(misses).to.deep.equal([]);
  });

  it("returns fully hydrated wrappers (flowName, subject, currentStateName, startedAtMillis, shape)", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    const before = new Date().toISOString();
    await FlowInstance.start(p, "Delivery", "ad4m://task/hydrate");
    const after = new Date().toISOString();

    const [hydrated] = await FlowInstance.findAll(p, "Delivery");
    expect(hydrated.flowName).to.equal("Delivery");
    expect(hydrated.subject).to.equal("ad4m://task/hydrate");
    expect(hydrated.currentStateName).to.equal("Identified");
    // Wrapper carries the parsed shape — verify it round-trips.
    expect(hydrated.shape.name).to.equal("Delivery");
    expect(hydrated.shape.states.length).to.equal(2);
    // Start time = Ad4mModel's synthesised `createdAt` on the underlying
    // record (earliest link timestamp on the instance URI, epoch millis
    // after hydration), surfaced via the wrapper's `startedAtMillis`.
    const beforeMs = new Date(before).getTime();
    const afterMs = new Date(after).getTime();
    const startedAt = hydrated.startedAtMillis;
    expect(startedAt !== undefined && startedAt >= beforeMs && startedAt <= afterMs,
      `startedAtMillis ${startedAt} must fall in [${beforeMs}, ${afterMs}]`).to.equal(true);
  });
});

// ── FlowInstance wrapper — OO API (design doc §4.3) ──────────────────────────
// The wrapper carries a `FlowInstanceRecord` + `SHACLFlow` shape and exposes
// the read + subscribe surface a UI or agent needs. Mutations + subscriptions
// land with slice 10.6 (consensus engine); today they throw with a clear
// "not yet implemented" so call-sites surface a typed error rather than a
// silent no-op.

describe("FlowInstance wrapper — read + stub API", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-instance-wrapper");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-instance-wrapper-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  function makeDeliveryFlowWithTransitions(): SHACLFlow {
    const flow = new SHACLFlow("Delivery", "flow://");
    flow.inputTypes = ["ad4m://Task"];
    flow.addState({ name: "Identified", value: 0 });
    flow.addState({ name: "InProgress", value: 1 });
    flow.addState({ name: "Done", value: 2 });
    const t1: FlowTransition = {
      fromState: "Identified",
      toState: "InProgress",
      actionName: "start",
      actions: [],
    };
    const t2: FlowTransition = {
      fromState: "InProgress",
      toState: "Done",
      actionName: "complete",
      actions: [],
    };
    flow.addTransition(t1);
    flow.addTransition(t2);
    return flow;
  }

  it("currentState resolves to the FlowState object on the shape", async () => {
    await p.addFlow("Delivery", makeDeliveryFlowWithTransitions());
    const inst = await FlowInstance.start(p, "Delivery", "ad4m://task/currentstate");

    const state = inst.currentState;
    expect(state.name).to.equal("Identified");
    // Object identity: same reference as the one on the shape.
    expect(state).to.equal(inst.shape.states[0]);
  });

  it("availableTransitions filters shape.transitions by fromState", async () => {
    await p.addFlow("Delivery", makeDeliveryFlowWithTransitions());
    const inst = await FlowInstance.start(p, "Delivery", "ad4m://task/transitions");

    // Fresh instance is in "Identified" → only the Identified→InProgress edge.
    const outgoing = inst.availableTransitions;
    expect(outgoing.length).to.equal(1);
    expect(outgoing[0].fromState).to.equal("Identified");
    expect(outgoing[0].toState).to.equal("InProgress");
  });

  it("availableTransitions is empty for a terminal state", async () => {
    await p.addFlow("Delivery", makeDeliveryFlowWithTransitions());
    const inst = await FlowInstance.start(p, "Delivery", "ad4m://task/terminal");

    // Simulate consensus firing by mutating the underlying record directly —
    // slice 10.6 wires this properly; today we exercise the wrapper's
    // filter logic in isolation.
    (inst.record as any).currentState = "Done";
    expect(inst.availableTransitions).to.deep.equal([]);
  });

  it("proposals() returns an empty array when no transitions have been proposed", async () => {
    await p.addFlow("Delivery", makeDeliveryFlowWithTransitions());
    const inst = await FlowInstance.start(p, "Delivery", "ad4m://task/no-proposals");

    const proposals = await inst.proposals();
    expect(proposals).to.deep.equal([]);
  });

  // Note: `proposeTransition` / `accept` / `reject` / `fireAction` /
  // `onStateChange` used to exist as unconditionally-throwing stubs. They
  // were removed in the same slice that moved the instance lifecycle to
  // `FlowInstance.start` / `.findAll`, since a type-level lie is worse than
  // an absent method. They come back — for real — in slice 10.6 when the
  // consensus engine lands.

  it("wrapper carries the record — callers can reach in for anything unexposed", async () => {
    const flow = makeDeliveryFlowWithTransitions();
    await p.addFlow("Delivery", flow);
    const inst = await FlowInstance.start(p, "Delivery", "ad4m://task/record-access");

    expect(inst.record).to.be.instanceOf(FlowInstanceRecord);
    // Post-J#5: record stores the flow's URI (`flowUri`), not the bare
    // `name`. The human label is available via the wrapper's `flowName`
    // getter, which reads it off the paired SHACLFlow.
    expect(inst.record.flowUri).to.equal(flow.flowUri);
    expect(inst.flowName).to.equal("Delivery");
    expect(inst.record.subject).to.equal("ad4m://task/record-access");
    expect(inst.record.currentState).to.equal("Identified");
  });
});

// ── PerspectiveProxy.availableFlows — concrete-type matching (§5 spawn engine) ─
// Post-`flowable` retirement, `availableFlows(uri)` must return a flow when
// `inputTypes` contains any registered subject-class name of `uri`, in
// addition to the "empty or `any`" always-match cases (James PR #929 J#3).
// Before this fix, typed flows were the entire point of v4/v5 yet
// `availableFlows` silently returned only untyped/wildcard flows — a live
// public API returning a confidently wrong set.

describe("PerspectiveProxy.availableFlows — concrete-type matching", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-available-flows");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-available-flows-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  function makeTypedFlow(name: string, inputTypes: string[]): SHACLFlow {
    // v5-shape flow — no `stateCheck` so `FlowInstance.start` (if ever
    // called on it) admits it; `availableFlows` cares only about
    // `inputTypes`.
    const flow = new SHACLFlow(name, "flow://");
    flow.inputTypes = inputTypes;
    flow.addState({ name: "s0", value: 0 });
    return flow;
  }

  it("returns typed flows whose inputTypes intersect the expression's registered classes", async () => {
    // TestPost is a real @Model — registering it teaches the perspective
    // to classify TestPost instances via `subjectClassesOf`.
    await (TestPostForFlows as any).register(p);

    await p.addFlow("PostFlow", makeTypedFlow("PostFlow", ["FlowTestPost"]));
    await p.addFlow("OtherFlow", makeTypedFlow("OtherFlow", ["OtherClass"]));

    const post = await (TestPostForFlows as any).create(p, { title: "T", body: "B" });
    const found = await p.availableFlows(post.id);
    expect(found).to.include("PostFlow");
    expect(found).to.not.include("OtherFlow");
  });

  it("keeps returning untyped and 'any'-wildcard flows for any expression", async () => {
    await p.addFlow("Untyped", makeTypedFlow("Untyped", []));
    await p.addFlow("Wildcard", makeTypedFlow("Wildcard", ["any"]));
    await p.addFlow("PostOnly", makeTypedFlow("PostOnly", ["FlowTestPost"]));

    // Random URI, no registered class on this perspective for it.
    const found = await p.availableFlows("test-lang://random-1234");
    expect(found).to.include("Untyped");
    expect(found).to.include("Wildcard");
    expect(found).to.not.include("PostOnly");
  });

  it("returns an empty array when no flows are registered", async () => {
    const found = await p.availableFlows("test-lang://anything");
    expect(found).to.deep.equal([]);
  });
});

describe("FlowInstance.acceptProposal / rejectProposal — consensus write API", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-accept-reject");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-accept-reject-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  function makeDeliveryFlow(): SHACLFlow {
    const flow = new SHACLFlow("Delivery", "flow://");
    flow.inputTypes = ["ad4m://Task"];
    const identified: FlowState = { name: "Identified", value: 0 };
    const inProgress: FlowState = { name: "InProgress", value: 1 };
    flow.addState(identified);
    flow.addState(inProgress);
    return flow;
  }

  /** Write a proposal the way the Rust engine does: raw URI target for the
   *  identity link, literal-wrapped scalars — but with an EMPTY evidence
   *  seal, which the consensus pass treats as unverifiable and
   *  auto-invalidates (client-side writes cannot produce a valid seal;
   *  only the engine's evidence-collection path can). */
  async function seedProposal(instanceUri: string, fromState: string, toState: string): Promise<string> {
    const proposal = `ad4m://flow/proposal/js-test-${Math.random().toString(36).slice(2)}`;
    await p.add(new Link({ source: proposal, predicate: "ad4m://flow/instance", target: instanceUri }));
    await p.add(new Link({ source: proposal, predicate: "ad4m://flow/from_state", target: `literal:string:${fromState}` }));
    await p.add(new Link({ source: proposal, predicate: "ad4m://flow/to_state", target: `literal:string:${toState}` }));
    await p.add(new Link({ source: proposal, predicate: "ad4m://flow/proposer", target: "did:example:proposer" }));
    await p.add(new Link({ source: proposal, predicate: "ad4m://flow/evidence_hashes", target: "literal:string:" }));
    return proposal;
  }

  it("acceptProposal counts the vote, and the pass auto-invalidates the unverifiable seal", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    const instance = await FlowInstance.start(p, "Delivery", "ad4m://task/1");
    expect(instance.currentStateName).to.equal("Identified");
    const proposal = await seedProposal(instance.uri, "Identified", "InProgress");

    // The wire round-trip: accept succeeds (vote recorded, pass runs), but a
    // client-seeded proposal cannot carry a valid evidence seal, so the pass
    // invalidates it rather than firing — nothing an external writer can put
    // on the graph advances a flow without engine-verifiable evidence. (The
    // fire-at-quorum path itself is covered by the Rust live-store e2es,
    // where the engine mints validly-sealed proposals.)
    const fired = await instance.acceptProposal(proposal);
    expect(fired).to.have.lengthOf(0);

    // The unverifiable proposal is gone: a second accept has nothing to act on.
    let threw = false;
    try {
      await instance.acceptProposal(proposal);
    } catch {
      threw = true;
    }
    expect(threw, "accept on an invalidated proposal must error").to.equal(true);

    // And the instance never moved.
    const all = await FlowInstance.findAll(p);
    expect(all).to.have.lengthOf(1);
    expect(all[0].currentStateName).to.equal("Identified");
  });

  it("rejectProposal hard-deletes a live proposal and errors on unknown URIs", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    const instance = await FlowInstance.start(p, "Delivery", "ad4m://task/2");
    const proposal = await seedProposal(instance.uri, "Identified", "InProgress");

    expect(await instance.rejectProposal(proposal)).to.equal(true);

    // Deleted: a second reject has nothing to act on.
    let threw = false;
    try {
      await instance.rejectProposal(proposal);
    } catch {
      threw = true;
    }
    expect(threw, "reject on a deleted proposal must error").to.equal(true);

    // And the instance never moved.
    const all = await FlowInstance.findAll(p);
    expect(all[0].currentStateName).to.equal("Identified");
  });
});
