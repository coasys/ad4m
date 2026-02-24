/**
 * Ad4mModel — WhereCondition operator integration tests
 *
 * Covers: array-IN, { not }, { not: array }, { contains },
 * { gt }, { gte }, { lt }, { lte }, { between }.
 *
 * String operators (not, not[], array IN) are pushed into SQL WHERE
 * via fn::parse_literal comparisons.  Numeric operators (gt/gte/lt/lte/between)
 * and contains are post-filtered in JavaScript after a broad SQL fetch.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-where-operators.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";
import fetch from "node-fetch";
import { startAgent } from "../../helpers/index.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestPost } from "./models.js";

//@ts-ignore
global.fetch = fetch;

describe("Ad4mModel — WhereCondition operators", function () {
  this.timeout(120_000);

  let stop: () => Promise<void>;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  // Three seeded posts with distinct titles and numeric viewCounts
  let p1: TestPost; // title: "Alpha", viewCount: 1
  let p2: TestPost; // title: "Beta",  viewCount: 3
  let p3: TestPost; // title: "Gamma", viewCount: 5

  before(async () => {
    const agent = await startAgent("model-where-operators");
    ad4m = agent.client;
    stop = agent.stop;
    perspective = await ad4m.perspective.add("model-where-operators-test");
    await TestPost.register(perspective);
  });

  after(async () => {
    await stop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestPost.register(perspective);
    p1 = await TestPost.create(perspective, {
      title: "Alpha",
      body: "",
      viewCount: 1,
    });
    p2 = await TestPost.create(perspective, {
      title: "Beta",
      body: "",
      viewCount: 3,
    });
    p3 = await TestPost.create(perspective, {
      title: "Gamma",
      body: "",
      viewCount: 5,
    });
  });

  // ── Array IN ──────────────────────────────────────────────────────────────

  it("where: { id: [id1, id2] } returns only those two instances", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { id: [p1.id, p2.id] },
    });
    expect(results).to.have.length(2);
    const ids = results.map((r) => r.id);
    expect(ids).to.include(p1.id);
    expect(ids).to.include(p2.id);
    expect(ids).to.not.include(p3.id);
  });

  // ── not (single value) ────────────────────────────────────────────────────

  it("where: { title: { not: 'Alpha' } } excludes the matching instance", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { title: { not: "Alpha" } },
    });
    expect(results).to.have.length(2);
    expect(results.every((r) => r.title !== "Alpha")).to.be.true;
  });

  // ── not (array) ───────────────────────────────────────────────────────────

  it("where: { title: { not: ['Alpha', 'Beta'] } } excludes both from result set", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { title: { not: ["Alpha", "Beta"] } },
    });
    expect(results).to.have.length(1);
    expect(results[0].title).to.equal("Gamma");
  });

  // ── contains ──────────────────────────────────────────────────────────────

  it("where: { title: { contains: 'eta' } } matches the substring in title", async () => {
    // "Beta" contains "eta"; "Alpha" and "Gamma" do not
    const results = await TestPost.findAll(perspective, {
      where: { title: { contains: "eta" } },
    });
    expect(results).to.have.length(1);
    expect(results[0].title).to.equal("Beta");
  });

  // ── gt ────────────────────────────────────────────────────────────────────

  it("where: { viewCount: { gt: 3 } } returns only instances strictly above 3", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { gt: 3 } },
    });
    expect(results).to.have.length(1);
    expect(results[0].id).to.equal(p3.id); // viewCount: 5
  });

  // ── gte ───────────────────────────────────────────────────────────────────

  it("where: { viewCount: { gte: 3 } } includes the boundary value", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { gte: 3 } },
    });
    expect(results).to.have.length(2);
    const ids = results.map((r) => r.id);
    expect(ids).to.include(p2.id); // viewCount: 3 (at boundary)
    expect(ids).to.include(p3.id); // viewCount: 5
  });

  // ── lt ────────────────────────────────────────────────────────────────────

  it("where: { viewCount: { lt: 3 } } returns only instances strictly below 3", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { lt: 3 } },
    });
    expect(results).to.have.length(1);
    expect(results[0].id).to.equal(p1.id); // viewCount: 1
  });

  // ── lte ───────────────────────────────────────────────────────────────────

  it("where: { viewCount: { lte: 3 } } includes the boundary value", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { lte: 3 } },
    });
    expect(results).to.have.length(2);
    const ids = results.map((r) => r.id);
    expect(ids).to.include(p1.id); // viewCount: 1
    expect(ids).to.include(p2.id); // viewCount: 3 (at boundary)
  });

  // ── between ───────────────────────────────────────────────────────────────

  it("where: { viewCount: { between: [2, 4] } } returns only instances inside the range", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { between: [2, 4] } },
    });
    expect(results).to.have.length(1);
    expect(results[0].id).to.equal(p2.id); // viewCount: 3 is in [2, 4]
  });
});
