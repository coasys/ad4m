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
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import { FlowInstance, FlowTransitionProposal } from "@coasys/ad4m";
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
    expect(proposals[0].createdAt).to.equal("2026-08-26T09:00:00Z");
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
    expect(all[0].baseExpression).to.equal("ad4m://some-subject");
    expect(all[0].currentState).to.equal("Scoped");
    expect(all[0].createdAt).to.equal("2026-08-26T09:45:00Z");
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
