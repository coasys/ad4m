/**
 * Ad4mModel — model inheritance integration tests
 *
 * Covers: WeakMap metadata registry, metadata isolation between base/derived,
 * generateSHACL() sh:node inheritance, derived findAll() discrimination,
 * instanceof check, and polymorphic base findAll().
 *
 * Ported from playground scenario 09 (Model Inheritance).
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-inheritance.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestPost, TestBaseModel, TestDerivedModel } from "./models.js";


// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — Model Inheritance", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-inheritance");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-inheritance-test");
    await TestDerivedModel.register(perspective);
    await TestPost.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestDerivedModel.register(perspective);
    await TestPost.register(perspective);
  });

  // ── Metadata (no executor) ────────────────────────────────────────────────

  it("getModelMetadata() on base returns only base fields", () => {
    const meta = TestBaseModel.getModelMetadata();
    expect("content" in meta.properties).to.be.true;
    expect("question" in meta.properties).to.be.false;
    expect("pollType" in meta.properties).to.be.false;
  });

  it("getModelMetadata() on derived returns merged base+derived fields", () => {
    const meta = TestDerivedModel.getModelMetadata();
    expect("content" in meta.properties).to.be.true;
    expect("question" in meta.properties).to.be.true;
    expect("pollType" in meta.properties).to.be.true;
  });

  it("derived class decorators do not corrupt base class metadata", () => {
    // Read derived first, then verify base is still clean
    TestDerivedModel.getModelMetadata();
    const baseMeta = TestBaseModel.getModelMetadata();
    const keys = Object.keys(baseMeta.properties);
    expect(keys).to.have.length(1);
    expect(keys[0]).to.equal("content");
  });

  // ── SHACL generation ──────────────────────────────────────────────────────

  it("generateSHACL() for derived emits sh:node reference to base shape", () => {
    const { shape } = TestDerivedModel.generateSHACL();
    expect(shape.parentShapes).to.be.an("array").with.length.greaterThan(0);
    const parentShapeUri = shape.parentShapes![0];
    expect(parentShapeUri).to.include("TestBaseModel");
  });

  it("generateSHACL() for derived does not duplicate base property shapes", () => {
    const { shape } = TestDerivedModel.generateSHACL();
    const propPaths = (shape.properties ?? []).map((p: any) => p.path);
    expect(propPaths).to.not.include("test://base_content");
  });

  // ── Live / executor-facing ────────────────────────────────────────────────

  it("TestDerivedModel.findAll() returns only derived instances (via @Flag)", async () => {
    // Save a TestPost (different @Flag) as noise — it must not appear in derived results
    await TestPost.create(perspective, { title: "noise post", body: "" });

    const derived = await TestDerivedModel.create(perspective, {
      content: "derived content",
      question: "Favourite color?",
    });

    const results = await TestDerivedModel.findAll(perspective);
    expect(results).to.have.length(1);
    expect(results[0].id).to.equal(derived.id);
  });

  it("TestDerivedModel instance passes instanceof TestBaseModel check", () => {
    const derived = new TestDerivedModel(perspective);
    expect(derived instanceof TestBaseModel).to.be.true;
  });

  it("TestDerivedModel.findOne() returns instance with both content and question", async () => {
    const derived = await TestDerivedModel.create(perspective, {
      content: "shared content",
      question: "Which option?",
    });

    const found = await TestDerivedModel.findOne(perspective, {
      where: { id: derived.id },
    });
    expect(found).to.not.be.null;
    expect(found!.content).to.equal("shared content");
    expect(found!.question).to.equal("Which option?");
  });

  it("TestBaseModel.findAll() returns instances of both base and derived types (polymorphic)", async () => {
    const derived = await TestDerivedModel.create(perspective, {
      content: "polymorphic test",
      question: "Any answer?",
    });

    // TestBaseModel.findAll() queries by test://base_content predicate —
    // derived instances also carry this link (inherited @Property).
    const allBase = await TestBaseModel.findAll(perspective);
    expect(allBase.length).to.be.at.least(1);
    expect(allBase.some((b) => b.id === derived.id)).to.be.true;
  });
});
