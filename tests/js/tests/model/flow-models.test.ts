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
import { FlowInstance, FlowTransitionProposal } from "@coasys/ad4m";
import { computeFlowEvidenceHash } from "@coasys/ad4m";
import { getSharedAgent } from "./hooks.js";
import { startAgent } from "../../helpers/index.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

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
    await p.add(new Link({
      source: proposal,
      predicate: "ad4m://flow/created_at",
      target: "literal:string:2026-08-26T09:00:00Z",
    }));

    const proposals = await FlowTransitionProposal.findAll(p);
    expect(proposals).to.have.lengthOf(1);
    expect(proposals[0].flowInstance).to.equal(flowInstance);
    expect(proposals[0].fromState).to.equal("collectingPerspectives");
    expect(proposals[0].toState).to.equal("tensionIdentified");
    expect(proposals[0].proposer).to.equal("did:example:alice");
    expect(proposals[0].evidenceHashes).to.equal("{}");
    expect(proposals[0].proposedAt).to.equal("2026-08-26T09:00:00Z");
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

// ── FlowTransitionProposal.propose — v5 client factory (slice 10.8c) ─────────
// End-to-end proof that the pure `buildFlowTransitionProposalFields` +
// `Ad4mModel.create` composition mints a proposal on-graph that (a) round-trips
// through `findAll` with all seven declared predicates, (b) hydrates evidence
// as a collection (not a scalar), and (c) carries an `evidenceHashes` value
// reproducible by the browser-side `computeFlowEvidenceHash` — so a Flux UI or
// consensus verifier can independently re-derive it from the class list + the
// listed evidence URIs without trusting the proposer.
describe("FlowTransitionProposal.propose — v5 client factory", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-propose");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-propose-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await FlowTransitionProposal.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  it("mints a proposal on-graph that findAll can rehydrate with all declared fields", async () => {
    const flowInstance = "ad4m://flow/instance/inst-live-1";
    const classNames = ["ad4m://Task"];
    const evidence = ["ad4m://task/1", "ad4m://task/2"];
    const proposedAt = "2026-08-29T23:45:00Z";

    const minted = await FlowTransitionProposal.propose(p, {
      flowInstance,
      fromState: "InProgress",
      toState: "Review",
      proposer: "did:example:alice",
      evidence,
      classNames,
      rationale: "PR opened and awaiting review",
      runUri: "ad4m://interp/run/run-live-1",
      proposedAt,
    });

    expect(minted).to.be.instanceOf(FlowTransitionProposal);
    expect(minted.flowInstance).to.equal(flowInstance);
    expect(minted.toState).to.equal("Review");

    const all = await FlowTransitionProposal.findAll(p);
    expect(all).to.have.lengthOf(1);
    const hydrated = all[0];
    expect(hydrated.flowInstance).to.equal(flowInstance);
    expect(hydrated.fromState).to.equal("InProgress");
    expect(hydrated.toState).to.equal("Review");
    expect(hydrated.proposer).to.equal("did:example:alice");
    expect(hydrated.proposedAt).to.equal(proposedAt);
    expect(hydrated.rationale).to.equal("PR opened and awaiting review");
    expect(hydrated.runUri).to.equal("ad4m://interp/run/run-live-1");
    expect(hydrated.evidence).to.have.members(evidence);
    // The on-graph evidenceHashes value must be reproducible from the same
    // (classNames, evidence) inputs — the whole point of the algorithm being
    // pure and byte-parity across TS + Rust.
    const expectedHash = computeFlowEvidenceHash(classNames, evidence);
    expect(hydrated.evidenceHashes).to.equal(expectedHash);
  });

  it("omits optional fields (rationale, runUri) when the caller does not supply them", async () => {
    const minted = await FlowTransitionProposal.propose(p, {
      flowInstance: "ad4m://flow/instance/inst-live-2",
      fromState: "Identified",
      toState: "InProgress",
      proposer: "did:example:bob",
      evidence: [],
      classNames: [],
      proposedAt: "2026-08-29T23:50:00Z",
    });

    const all = await FlowTransitionProposal.findAll(p);
    expect(all).to.have.lengthOf(1);
    const hydrated = all[0];
    // Fields not written must hydrate to their @Property defaults — either
    // undefined or the empty-string / empty-array init — never a lingering
    // sibling proposal's value.
    expect(hydrated.rationale ?? "").to.equal("");
    expect(hydrated.runUri ?? "").to.equal("");
    expect(hydrated.evidence ?? []).to.deep.equal([]);
    // But the required scaffolding is still there:
    expect(hydrated.flowInstance).to.equal("ad4m://flow/instance/inst-live-2");
    expect(hydrated.fromState).to.equal("Identified");
    expect(hydrated.toState).to.equal("InProgress");
    // And the hash of empty inputs is still deterministic + reproducible.
    expect(hydrated.evidenceHashes).to.equal(computeFlowEvidenceHash([], []));
    // Keep referencing the caller-visible return so a lint pass can't strip
    // the .propose() call to a dangling await.
    expect(minted.proposer).to.equal("did:example:bob");
  });

  it("rejects empty toState / proposer at the factory boundary (no on-graph write)", async () => {
    let caughtToState: unknown = null;
    try {
      await FlowTransitionProposal.propose(p, {
        flowInstance: "ad4m://flow/instance/x",
        fromState: "A",
        toState: "",
        proposer: "did:example:c",
        evidence: [],
        classNames: [],
      });
    } catch (e) {
      caughtToState = e;
    }
    expect(String(caughtToState)).to.match(/toState is required/);

    let caughtProposer: unknown = null;
    try {
      await FlowTransitionProposal.propose(p, {
        flowInstance: "ad4m://flow/instance/x",
        fromState: "A",
        toState: "B",
        proposer: "",
        evidence: [],
        classNames: [],
      });
    } catch (e) {
      caughtProposer = e;
    }
    expect(String(caughtProposer)).to.match(/proposer is required/);

    // Neither rejected call may have leaked a partial write on-graph.
    const all = await FlowTransitionProposal.findAll(p);
    expect(all).to.deep.equal([]);
  });
});

describe("FlowInstance — @Model", function () {
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
    await FlowInstance.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  it("FlowInstance.findAll() returns instances by flow-name discriminator", async () => {
    // Simulate what the Rust flow engine writes when startFlow lands.
    const instance = "ad4m://flow/instance/inst-1";
    await p.add(new Link({
      source: instance,
      predicate: "ad4m://flow/flow_name",
      target: "literal:string:Delivery",
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
    await p.add(new Link({
      source: instance,
      predicate: "ad4m://flow/created_at",
      target: "literal:string:2026-08-26T09%3A45%3A00Z",
    }));

    const all = await FlowInstance.findAll(p);
    expect(all.length).to.equal(1);
    expect(all[0].flow).to.equal("Delivery");
    expect(all[0].subject).to.equal("ad4m://some-subject");
    expect(all[0].currentState).to.equal("Scoped");
    expect(all[0].startedAt).to.equal("2026-08-26T09:45:00Z");
  });

  it("FlowInstance @Model shape matches Rust flow_instance.json", () => {
    const SDNA_DIR = path.resolve(
      __dirname,
      "../../../../rust-executor/src/perspectives/hardwired_sdna",
    );
    const raw = fs.readFileSync(path.join(SDNA_DIR, "flow_instance.json"), "utf-8");
    const parsed = JSON.parse(raw) as {
      target_class: string;
      properties: Array<{ path: string; name: string }>;
    };

    const { shape } = (FlowInstance as any).generateSHACL();
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

// ── PerspectiveProxy.startFlowInstance — v5 API (design doc §4.3) ────────────
// Mints an on-graph `FlowInstance` node tied to a base expression, seeded at
// the flow's first declared state. Registration of the hardwired runtime
// classes is idempotent inside the API — callers do NOT pre-register.

describe("PerspectiveProxy.startFlowInstance — v5 API", function () {
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
    // v5 flow states express membership via `requires`; the legacy
    // `value` / `stateCheck` fields are still on the interface until the
    // retirement pass and are populated here with harmless placeholders
    // so the type checks — they are not exercised by startFlowInstance.
    const identified: FlowState = {
      name: "Identified",
      value: 0,
      stateCheck: { predicate: "flow://legacy", target: "flow://Identified" },
    };
    const inProgress: FlowState = {
      name: "InProgress",
      value: 1,
      stateCheck: { predicate: "flow://legacy", target: "flow://InProgress" },
    };
    flow.addState(identified);
    flow.addState(inProgress);
    return flow;
  }

  it("mints a FlowInstance seeded at the first state and returns the hydrated model", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());

    const before = new Date().toISOString();
    const instance = await p.startFlowInstance("Delivery", "ad4m://task/1");
    const after = new Date().toISOString();

    expect(instance).to.be.instanceOf(FlowInstance);
    expect(instance.flow).to.equal("Delivery");
    expect(instance.subject).to.equal("ad4m://task/1");
    expect(instance.currentState).to.equal("Identified");
    expect(instance.startedAt >= before && instance.startedAt <= after,
      `startedAt ${instance.startedAt} must fall in [${before}, ${after}]`).to.equal(true);

    // Findable via the discriminator predicate — same lookup UIs will use.
    const all = await FlowInstance.findAll(p);
    expect(all.length).to.equal(1);
    expect(all[0].flow).to.equal("Delivery");
    expect(all[0].subject).to.equal("ad4m://task/1");
    expect(all[0].currentState).to.equal("Identified");
  });

  it("supports multiple concurrent instances on distinct bases", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await p.startFlowInstance("Delivery", "ad4m://task/a");
    await p.startFlowInstance("Delivery", "ad4m://task/b");

    const all = await FlowInstance.findAll(p);
    expect(all.length).to.equal(2);
    const bases = all.map((i) => i.subject).sort();
    expect(bases).to.deep.equal(["ad4m://task/a", "ad4m://task/b"]);
  });

  it("throws when the named flow is not registered on the perspective", async () => {
    let caught: unknown = null;
    try {
      await p.startFlowInstance("Nope", "ad4m://task/1");
    } catch (e) {
      caught = e;
    }
    expect(String(caught)).to.match(/Flow "Nope" not found/);
  });

  it("throws when the flow has zero declared states", async () => {
    const zeroState = new SHACLFlow("Like", "flow://");
    // no addState calls — zero-state action flow (§6.3), handled by
    // runFlowAction / fireAction, not startFlowInstance.
    await p.addFlow("Like", zeroState);

    let caught: unknown = null;
    try {
      await p.startFlowInstance("Like", "ad4m://post/1");
    } catch (e) {
      caught = e;
    }
    expect(String(caught)).to.match(/has no states/);
  });
});

// ── PerspectiveProxy.getFlowInstances — v5 read side (design doc §4.3 / §5) ──
// Enumerates live FlowInstance records; optionally narrowed by flow-name
// discriminator. Feeds §5 Model C prompt gathering and UI indicators. Class
// registration is idempotent so callers may query BEFORE the first
// startFlowInstance ever fires without hitting hydration errors.

describe("PerspectiveProxy.getFlowInstances — v5 read side", function () {
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
    flow.addState({
      name: "Identified",
      value: 0,
      stateCheck: { predicate: "flow://legacy", target: "flow://Identified" },
    });
    flow.addState({
      name: "InProgress",
      value: 1,
      stateCheck: { predicate: "flow://legacy", target: "flow://InProgress" },
    });
    return flow;
  }

  function makeDeliberationFlow(): SHACLFlow {
    const flow = new SHACLFlow("Deliberation", "flow://");
    flow.inputTypes = ["ad4m://Proposal"];
    flow.addState({
      name: "Proposal",
      value: 0,
      stateCheck: { predicate: "flow://legacy", target: "flow://Proposal" },
    });
    return flow;
  }

  it("returns an empty array when no flow instances exist on the perspective", async () => {
    const all = await p.getFlowInstances();
    expect(all).to.deep.equal([]);
  });

  it("does not throw when called before any startFlowInstance — idempotent registration", async () => {
    // No addFlow / startFlowInstance beforehand; class must self-register.
    const all = await p.getFlowInstances();
    expect(all).to.deep.equal([]);

    // And a filtered call must be equally safe.
    const filtered = await p.getFlowInstances("Delivery");
    expect(filtered).to.deep.equal([]);
  });

  it("returns all live instances across flows when called with no filter", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await p.addFlow("Deliberation", makeDeliberationFlow());

    await p.startFlowInstance("Delivery", "ad4m://task/1");
    await p.startFlowInstance("Delivery", "ad4m://task/2");
    await p.startFlowInstance("Deliberation", "ad4m://proposal/1");

    const all = await p.getFlowInstances();
    expect(all.length).to.equal(3);
    expect(all.every((i) => i instanceof FlowInstance)).to.equal(true);
    const flows = all.map((i) => i.flow).sort();
    expect(flows).to.deep.equal(["Deliberation", "Delivery", "Delivery"]);
  });

  it("narrows by flow-name discriminator when the filter is supplied", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    await p.addFlow("Deliberation", makeDeliberationFlow());

    await p.startFlowInstance("Delivery", "ad4m://task/a");
    await p.startFlowInstance("Delivery", "ad4m://task/b");
    await p.startFlowInstance("Deliberation", "ad4m://proposal/x");

    const deliveries = await p.getFlowInstances("Delivery");
    expect(deliveries.length).to.equal(2);
    expect(deliveries.every((i) => i.flow === "Delivery")).to.equal(true);
    const bases = deliveries.map((i) => i.subject).sort();
    expect(bases).to.deep.equal(["ad4m://task/a", "ad4m://task/b"]);

    const deliberations = await p.getFlowInstances("Deliberation");
    expect(deliberations.length).to.equal(1);
    expect(deliberations[0].subject).to.equal("ad4m://proposal/x");

    const misses = await p.getFlowInstances("Nope");
    expect(misses).to.deep.equal([]);
  });

  it("returns fully hydrated instances (flow, subject, currentState, startedAt)", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    const before = new Date().toISOString();
    await p.startFlowInstance("Delivery", "ad4m://task/hydrate");
    const after = new Date().toISOString();

    const [hydrated] = await p.getFlowInstances("Delivery");
    expect(hydrated.flow).to.equal("Delivery");
    expect(hydrated.subject).to.equal("ad4m://task/hydrate");
    expect(hydrated.currentState).to.equal("Identified");
    expect(hydrated.startedAt >= before && hydrated.startedAt <= after,
      `startedAt ${hydrated.startedAt} must fall in [${before}, ${after}]`).to.equal(true);
  });
});
