/**
 * Ad4mModel — where-clause operator integration tests
 *
 * Dedicated coverage for every WhereCondition operator supported by the model
 * query engine: exact match, IN (array), not, not[] (exclude list), contains,
 * gt, gte, lt, lte, between.
 *
 * Ported from playground scenario 08 (Where Operators Deep-Dive).
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-where-operators.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestPost } from "./models.js";

// ── Seed helper ───────────────────────────────────────────────────────────────

async function seed(perspective: PerspectiveProxy) {
  const data = [
    { title: "Alpha", body: "hello world", viewCount: 10 },
    { title: "Beta", body: "goodbye world", viewCount: 20 },
    { title: "Gamma", body: "hello again", viewCount: 30 },
    { title: "Delta", body: "something else", viewCount: 40 },
    { title: "Epsilon", body: "hello universe", viewCount: 50 },
  ];
  const posts: InstanceType<typeof TestPost>[] = [];
  for (const d of data) {
    posts.push(await TestPost.create(perspective, d));
  }
  return posts;
}

// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — Where-Operator Deep Dive", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-where-ops");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-where-ops-test");
    await TestPost.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestPost.register(perspective);
  });

  // ── 1. Exact match (primitive value) ─────────────────────────────────────

  it("where: exact string match returns only matching records", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { title: "Alpha" },
    });
    expect(results).to.have.length(1);
    expect(results[0].title).to.equal("Alpha");
  });

  // ── 2. IN — array of values ──────────────────────────────────────────────

  it("where: array value acts as IN — returns posts matching any value", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { title: ["Alpha", "Gamma", "Epsilon"] as any },
    });
    expect(results).to.have.length(3);
    const titles = results.map((r) => r.title).sort();
    expect(titles).to.deep.equal(["Alpha", "Epsilon", "Gamma"]);
  });

  // ── 3. not — single value exclusion ───────────────────────────────────────

  it("where: { not: value } excludes records with that value", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { title: { not: "Alpha" } as any },
    });
    expect(results).to.have.length(4);
    expect(results.every((r) => r.title !== "Alpha")).to.be.true;
  });

  // ── 4. not — array exclusion ──────────────────────────────────────────────

  it("where: { not: [values] } excludes multiple records", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { title: { not: ["Alpha", "Beta"] } as any },
    });
    expect(results).to.have.length(3);
    const titles = results.map((r) => r.title).sort();
    expect(titles).to.deep.equal(["Delta", "Epsilon", "Gamma"]);
  });

  // ── 5. contains — substring match ────────────────────────────────────────

  it("where: { contains: substring } returns matching records", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { body: { contains: "hello" } as any },
    });
    expect(results).to.have.length(3);
    for (const r of results) {
      expect(r.body!.toLowerCase()).to.include("hello");
    }
  });

  // ── 6. gt — greater than ─────────────────────────────────────────────────

  it("where: { gt: number } filters records with field > value", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { gt: 30 } as any },
    });
    expect(results).to.have.length(2);
    for (const r of results) {
      expect(r.viewCount).to.be.greaterThan(30);
    }
  });

  // ── 7. gte — greater than or equal ───────────────────────────────────────

  it("where: { gte: number } includes the boundary", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { gte: 30 } as any },
    });
    expect(results).to.have.length(3);
    for (const r of results) {
      expect(r.viewCount).to.be.gte(30);
    }
  });

  // ── 8. lt / lte — less than ───────────────────────────────────────────────

  it("where: { lt: number } and { lte: number } both work correctly", async () => {
    await seed(perspective);

    const lt = await TestPost.findAll(perspective, {
      where: { viewCount: { lt: 30 } as any },
    });
    expect(lt).to.have.length(2); // 10, 20
    for (const r of lt) {
      expect(r.viewCount).to.be.lessThan(30);
    }

    const lte = await TestPost.findAll(perspective, {
      where: { viewCount: { lte: 30 } as any },
    });
    expect(lte).to.have.length(3); // 10, 20, 30
    for (const r of lte) {
      expect(r.viewCount).to.be.lte(30);
    }
  });

  // ── 9. between — inclusive range ──────────────────────────────────────────

  it("where: { between: [lo, hi] } returns records in inclusive range", async () => {
    await seed(perspective);
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { between: [20, 40] } as any },
    });
    expect(results).to.have.length(3); // 20, 30, 40
    for (const r of results) {
      expect(r.viewCount).to.be.gte(20).and.to.be.lte(40);
    }
  });
});
