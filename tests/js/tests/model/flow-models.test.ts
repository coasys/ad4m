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
import { aggregateFlowVotes, fireIfConsensus } from "@coasys/ad4m";
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

describe("FlowTransitionProposal.listForInstance — v5 query helper", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-list-for-instance");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-list-for-instance-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await FlowTransitionProposal.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  it("returns only proposals targeting the requested FlowInstance, oldest first", async () => {
    const instA = "ad4m://flow/instance/inst-A";
    const instB = "ad4m://flow/instance/inst-B";

    // Deliberately mint out of chronological order to prove the sort
    // contract is enforced by listForInstance, not accidentally satisfied
    // by insertion order.
    await FlowTransitionProposal.propose(p, {
      flowInstance: instA,
      fromState: "Identified",
      toState: "Scoped",
      proposer: "did:example:alice",
      evidence: [],
      classNames: [],
      proposedAt: "2026-08-29T10:20:00Z",
    });
    await FlowTransitionProposal.propose(p, {
      flowInstance: instA,
      fromState: "Scoped",
      toState: "InProgress",
      proposer: "did:example:bob",
      evidence: [],
      classNames: [],
      proposedAt: "2026-08-29T10:00:00Z",
    });
    await FlowTransitionProposal.propose(p, {
      flowInstance: instB,
      fromState: "Identified",
      toState: "Scoped",
      proposer: "did:example:carol",
      evidence: [],
      classNames: [],
      proposedAt: "2026-08-29T10:10:00Z",
    });

    const forA = await FlowTransitionProposal.listForInstance(p, instA);
    expect(forA).to.have.lengthOf(2);
    // Oldest first — 10:00 before 10:20 — proves the sort contract.
    expect(forA[0].proposedAt).to.equal("2026-08-29T10:00:00Z");
    expect(forA[0].toState).to.equal("InProgress");
    expect(forA[1].proposedAt).to.equal("2026-08-29T10:20:00Z");
    expect(forA[1].toState).to.equal("Scoped");
    // And the where-filter shape actually reached SPARQL — instB's
    // proposal must NOT appear even though it was minted between them.
    for (const proposal of forA) {
      expect(proposal.flowInstance).to.equal(instA);
    }

    const forB = await FlowTransitionProposal.listForInstance(p, instB);
    expect(forB).to.have.lengthOf(1);
    expect(forB[0].proposer).to.equal("did:example:carol");
  });

  it("returns an empty array when no proposals target the instance", async () => {
    // No proposals minted — empty perspective. Proves the query does
    // not throw on the empty-result case (vote aggregation over a
    // never-proposed-against instance must return 0, not error).
    const forNone = await FlowTransitionProposal.listForInstance(
      p,
      "ad4m://flow/instance/inst-unknown",
    );
    expect(forNone).to.deep.equal([]);
  });

  it("rejects empty flowInstanceUri before touching the perspective", async () => {
    let caught: unknown = null;
    try {
      await FlowTransitionProposal.listForInstance(p, "");
    } catch (e) {
      caught = e;
    }
    expect(String(caught)).to.match(/flowInstanceUri is required/);
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

// ── FlowInstance.proposeTransition — 10.8b2 OO wrapper (design doc §4.3 / §5.4) ──
// Live-perspective proof that `instance.proposeTransition({ ... })` writes the
// same on-graph shape as the static `FlowTransitionProposal.propose(...)`
// factory, with `flowInstance` and `fromState` derived from `this`. The unit
// suite in `core/src/perspectives/FlowModels.test.ts` locks the delegation
// argument shape via a poison-perspective + spy; here we prove the delegation
// actually reaches the on-graph writer and its output is hydratable through
// the same `findAll` path any UI would use.

describe("FlowInstance.proposeTransition — 10.8b2 OO wrapper", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-instance-propose");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-instance-propose-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await FlowTransitionProposal.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  function makeDeliveryFlow(): SHACLFlow {
    const flow = new SHACLFlow("Delivery", "flow://");
    flow.inputTypes = ["ad4m://Task"];
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

  it("wrapper output is byte-equivalent to a static-factory call for the same instance / current state", async () => {
    // Two paths (wrapper vs static) must write proposals that a downstream
    // consensus verifier cannot tell apart. That's the whole reason `.propose`
    // is the single field-derivation seam; the wrapper only injects the two
    // derived fields.
    await p.addFlow("Delivery", makeDeliveryFlow());
    const instance = await p.startFlowInstance("Delivery", "ad4m://task/wrapper-parity");

    const classNames = ["ad4m://Task"];
    const evidence = ["ad4m://task/wrapper-parity"];
    const proposedAt = "2026-08-30T11:15:00Z";

    // Wrapper path — flowInstance / fromState derived from `this`.
    const viaWrapper = await instance.proposeTransition({
      toState: "InProgress",
      proposer: "did:example:alice",
      evidence,
      classNames,
      rationale: "seed satisfied",
      runUri: "ad4m://interp/run/w-1",
      proposedAt,
    });

    // Static path — caller supplies identical opts explicitly.
    const viaStatic = await FlowTransitionProposal.propose(p, {
      flowInstance: instance.id,
      fromState: instance.currentState,
      toState: "InProgress",
      proposer: "did:example:alice",
      evidence,
      classNames,
      rationale: "seed satisfied",
      runUri: "ad4m://interp/run/w-1",
      proposedAt,
    });

    expect(viaWrapper).to.be.instanceOf(FlowTransitionProposal);
    expect(viaWrapper.flowInstance).to.equal(instance.id);
    expect(viaWrapper.fromState).to.equal("Identified");
    expect(viaWrapper.toState).to.equal("InProgress");

    // Two proposals on-graph, one from each path. On the hash-relevant
    // fields they must agree — that's the parity contract.
    const all = await FlowTransitionProposal.findAll(p);
    expect(all.length).to.equal(2);
    const byId: Record<string, FlowTransitionProposal> = {};
    for (const proposal of all) byId[proposal.id] = proposal;
    const hydratedWrapper = byId[viaWrapper.id];
    const hydratedStatic = byId[viaStatic.id];
    expect(hydratedWrapper, "wrapper proposal must round-trip via findAll").to.not.equal(
      undefined,
    );
    expect(hydratedStatic, "static proposal must round-trip via findAll").to.not.equal(
      undefined,
    );
    expect(hydratedWrapper.flowInstance).to.equal(hydratedStatic.flowInstance);
    expect(hydratedWrapper.fromState).to.equal(hydratedStatic.fromState);
    expect(hydratedWrapper.toState).to.equal(hydratedStatic.toState);
    expect(hydratedWrapper.proposer).to.equal(hydratedStatic.proposer);
    expect(hydratedWrapper.rationale).to.equal(hydratedStatic.rationale);
    expect(hydratedWrapper.runUri).to.equal(hydratedStatic.runUri);
    expect(hydratedWrapper.proposedAt).to.equal(hydratedStatic.proposedAt);
    expect(hydratedWrapper.evidence).to.have.members([...(hydratedStatic.evidence ?? [])]);
    // Same evidence hash — the consensus-verification contract.
    expect(hydratedWrapper.evidenceHashes).to.equal(hydratedStatic.evidenceHashes);
    expect(hydratedWrapper.evidenceHashes).to.equal(
      computeFlowEvidenceHash(classNames, evidence),
    );
  });

  it("derives fromState from currentState AT CALL TIME (post-advance re-call reflects the new state)", async () => {
    // If a caller advances the instance's currentState (e.g. after
    // consensus fires) and then calls proposeTransition again, the
    // wrapper must read `this.currentState` fresh — not cache the value
    // from the first call. Same-object re-use is the common case
    // (a UI holds an instance handle, keeps proposing, keeps advancing).
    await p.addFlow("Delivery", makeDeliveryFlow());
    const instance = await p.startFlowInstance("Delivery", "ad4m://task/re-call");

    const firstProposal = await instance.proposeTransition({
      toState: "InProgress",
      proposer: "did:example:bob",
      evidence: [],
      classNames: [],
    });
    expect(firstProposal.fromState).to.equal("Identified");

    // Simulate a post-consensus advance — write directly to the field the
    // real advance path (fireIfConsensus / advance_flow_instance_state)
    // would set on the perspective. Refetch to prove the read went through
    // hydration, not just an in-memory field on the JS object.
    instance.currentState = "InProgress";
    await instance.save();
    const rehydrated = (await FlowInstance.findAll(p)).find(
      (i) => i.id === instance.id,
    );
    expect(rehydrated, "instance must survive save + findAll").to.not.equal(undefined);
    expect(rehydrated!.currentState).to.equal("InProgress");

    const secondProposal = await rehydrated!.proposeTransition({
      toState: "Review",
      proposer: "did:example:bob",
      evidence: [],
      classNames: [],
    });
    // The wrapper read the *new* currentState — not the initial "Identified".
    expect(secondProposal.fromState).to.equal("InProgress");
    expect(secondProposal.toState).to.equal("Review");
  });

  it("boundary defence: empty toState / proposer raise before any on-graph write", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    const instance = await p.startFlowInstance("Delivery", "ad4m://task/defence");

    let caughtToState: unknown = null;
    try {
      await instance.proposeTransition({
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
      await instance.proposeTransition({
        toState: "InProgress",
        proposer: "",
        evidence: [],
        classNames: [],
      });
    } catch (e) {
      caughtProposer = e;
    }
    expect(String(caughtProposer)).to.match(/proposer is required/);

    // Both defences run *before* the on-graph write path — no ghost proposals.
    const all = await FlowTransitionProposal.findAll(p);
    expect(all.length).to.equal(0);
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

// ── Full-stack consensus loop — slice 10.8g ─────────────────────────────────
// Stitches every 10.8 primitive against a real perspective (no mocks):
//   startFlowInstance → N × FlowTransitionProposal.propose (distinct DIDs) →
//   listForInstance → aggregateFlowVotes(rule) → fireIfConsensus →
//   re-query FlowInstance.findAll → assert currentState advanced.
//
// Also proves the stale-fromState guard: a second fireIfConsensus with the
// pre-fire aggregate must return undefined, not re-advance / no-op-throw.
// (Direct integration proof that the design §5.4 concurrency hazard called
// out in FlowConsensusFire.ts is actually caught by the shipped helper.)

describe("Client-side flow consensus loop — 10.8g", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("flow-consensus-loop");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("flow-consensus-loop-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await FlowTransitionProposal.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  function makeDeliveryFlow(): SHACLFlow {
    const flow = new SHACLFlow("Delivery", "flow://");
    flow.inputTypes = ["ad4m://Task"];
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

  async function propose(
    instanceUri: string,
    proposer: string,
    proposedAt: string,
  ): Promise<FlowTransitionProposal> {
    return FlowTransitionProposal.propose(p, {
      flowInstance: instanceUri,
      fromState: "Identified",
      toState: "InProgress",
      proposer,
      evidence: [],
      classNames: ["ad4m://Task"],
      proposedAt,
    });
  }

  it("2-of-3 threshold: three proposals clear consensus, instance advances, second fire is a no-op", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    const instance = await p.startFlowInstance("Delivery", "ad4m://task/consensus-1");
    expect(instance.currentState).to.equal("Identified");

    await propose(instance.id, "did:example:alice", "2026-08-30T05:00:00Z");
    await propose(instance.id, "did:example:bob",   "2026-08-30T05:01:00Z");
    await propose(instance.id, "did:example:carol", "2026-08-30T05:02:00Z");

    const proposals = await FlowTransitionProposal.listForInstance(p, instance.id);
    expect(proposals).to.have.lengthOf(3);

    const aggregate = aggregateFlowVotes(proposals, { n: 2 });
    expect(aggregate.fires, "aggregate must surface a firing tally").to.exist;
    expect(aggregate.fires!.fromState).to.equal("Identified");
    expect(aggregate.fires!.toState).to.equal("InProgress");
    expect(aggregate.fires!.eligibleProposers).to.deep.equal([
      "did:example:alice",
      "did:example:bob",
      "did:example:carol",
    ]);

    const outcome = await fireIfConsensus(p, instance, aggregate);
    expect(outcome, "fireIfConsensus must return a FireOutcome").to.exist;
    expect(outcome!.instanceUri).to.equal(instance.id);
    expect(outcome!.fromState).to.equal("Identified");
    expect(outcome!.toState).to.equal("InProgress");
    expect(outcome!.firedByProposers).to.deep.equal([
      "did:example:alice",
      "did:example:bob",
      "did:example:carol",
    ]);
    expect(outcome!.contributingProposalUris).to.have.lengthOf(3);

    // The on-graph currentState link must have been retired + rewritten —
    // a fresh findAll (independent of the local `instance` object) proves it.
    const [rehydrated] = await FlowInstance.findAll(p);
    expect(rehydrated.currentState).to.equal("InProgress");
    expect(rehydrated.subject).to.equal("ad4m://task/consensus-1");
    expect(rehydrated.flow).to.equal("Delivery");

    // Stale-fromState guard: `aggregate` was computed before the advance,
    // so `aggregate.fires.fromState === "Identified"`, but the instance
    // is now at "InProgress". Re-firing must be a no-op returning undefined,
    // not throw and not re-advance.
    const secondFire = await fireIfConsensus(p, instance, aggregate);
    expect(secondFire, "second fire on stale aggregate must be no-op").to.equal(undefined);

    // And no double-advance leaked into the graph.
    const [afterSecondFire] = await FlowInstance.findAll(p);
    expect(afterSecondFire.currentState).to.equal("InProgress");
  });

  it("below threshold: single proposal against {n:2} does not fire, instance stays put", async () => {
    await p.addFlow("Delivery", makeDeliveryFlow());
    const instance = await p.startFlowInstance("Delivery", "ad4m://task/consensus-below");

    await propose(instance.id, "did:example:alice", "2026-08-30T05:10:00Z");

    const proposals = await FlowTransitionProposal.listForInstance(p, instance.id);
    expect(proposals).to.have.lengthOf(1);

    const aggregate = aggregateFlowVotes(proposals, { n: 2 });
    expect(aggregate.tallies).to.have.lengthOf(1);
    expect(aggregate.tallies[0].consensusReached).to.equal(false);
    expect(aggregate.fires, "no fires tally when consensus not reached").to.equal(undefined);

    const outcome = await fireIfConsensus(p, instance, aggregate);
    expect(outcome).to.equal(undefined);

    // Graph unchanged — instance still at initial state.
    const [rehydrated] = await FlowInstance.findAll(p);
    expect(rehydrated.currentState).to.equal("Identified");
  });
});
