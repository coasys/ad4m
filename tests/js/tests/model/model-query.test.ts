/**
 * Ad4mModel — query API integration tests
 *
 * Covers: findAll() with where / order / limit / offset, findOne(), count(),
 * paginate(), findAllAndCount(), fluent ModelQueryBuilder, IncludeMap eager
 * loading, properties field projection, Query<T> composability, and
 * instance.get().
 *
 * Sections (in order):
 *   1.  where — exact match
 *   2.  where — operators (gt/gte/lt/lte/between/contains/not/IN)
 *   3.  where — combined conditions (AND semantics)
 *   4.  where — relation-based filtering (@BelongsToOne in where clause)
 *   5.  order
 *   6.  limit / offset
 *   7.  findOne
 *   8.  count
 *   9.  findAllAndCount
 *   10. paginate
 *   11. instance.get()
 *   12. fluent QueryBuilder
 *   13. IncludeMap — @HasMany / @HasOne / @BelongsToOne / @BelongsToMany
 *   14. IncludeMap — sub-queries (where / order / limit / properties)
 *   15. IncludeMap — nested (multi-level)
 *   16. IncludeMap — edge cases (non-conforming nodes)
 *   17. properties field projection
 *   18. parent query
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-query.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  HasMany,
  HasManyMethods,
  Link,
  Literal,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective, sleep } from "../../utils/utils.js";
import { TestComment, TestPost, TestTag, TestReaction, TestChannel } from "./models.js";

describe("Ad4mModel — Query API", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  // Three seeded posts for ordering / filtering tests
  let p1: TestPost, p2: TestPost, p3: TestPost;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-query");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-query-test");
    await TestPost.register(perspective);
    await TestComment.register(perspective);
    await TestTag.register(perspective);
    await TestReaction.register(perspective);
    await TestChannel.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestPost.register(perspective);
    await TestComment.register(perspective);
    await TestTag.register(perspective);
    await TestReaction.register(perspective);
    await TestChannel.register(perspective);
    // Re-seed fresh posts for every test so tests are fully independent
    p1 = await TestPost.create(perspective, {
      title: "Alpha",
      body: "first",
      viewCount: 0,
    });
    p2 = await TestPost.create(perspective, {
      title: "Beta",
      body: "second",
      viewCount: 0,
    });
    p3 = await TestPost.create(perspective, {
      title: "Gamma",
      body: "third",
      viewCount: 0,
    });
  });

  // ── 1. where — exact match ─────────────────────────────────────────────────

  it("findAll() with where.id returns only the matching instance", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { id: p1.id },
    });
    expect(results).to.have.length(1);
    expect(results[0].title).to.equal("Alpha");
  });

  it("findAll() with where.title returns only the matching title", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { title: "Beta" },
    });
    expect(results).to.have.length(1);
    expect(results[0].title).to.equal("Beta");
  });

  it("where: { id: [p1.id, p2.id] } IN-style filter returns exactly those two instances", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { id: [p1.id, p2.id] },
    });
    expect(results).to.have.length(2);
    const ids = results.map((r) => r.id);
    expect(ids).to.include(p1.id);
    expect(ids).to.include(p2.id);
    expect(ids).to.not.include(p3.id);
  });

  // ── 2. where — operators ───────────────────────────────────────────────────

  it("where: { viewCount: { gt: 5 } } returns only posts with viewCount > 5", async () => {
    await TestPost.create(perspective, {
      title: "Low",
      body: "",
      viewCount: 3,
    });
    const high = await TestPost.create(perspective, {
      title: "High",
      body: "",
      viewCount: 10,
    });
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { gt: 5 } },
    });
    expect(results.every((r) => (r.viewCount as any as number) > 5)).to.be.true;
    expect(results.some((r) => r.id === high.id)).to.be.true;
  });

  it("where: { viewCount: { gte: 10 } } includes the exact boundary", async () => {
    const exact = await TestPost.create(perspective, {
      title: "Exact",
      body: "",
      viewCount: 10,
    });
    const below = await TestPost.create(perspective, {
      title: "Below",
      body: "",
      viewCount: 9,
    });
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { gte: 10 } },
    });
    expect(results.some((r) => r.id === exact.id)).to.be.true;
    expect(results.some((r) => r.id === below.id)).to.be.false;
  });

  it("where: { viewCount: { lt: 5 } } returns only posts with viewCount < 5", async () => {
    const low = await TestPost.create(perspective, {
      title: "VeryLow",
      body: "",
      viewCount: 2,
    });
    const high = await TestPost.create(perspective, {
      title: "VeryHigh",
      body: "",
      viewCount: 20,
    });
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { lt: 5 } },
    });
    expect(results.some((r) => r.id === low.id)).to.be.true;
    expect(results.some((r) => r.id === high.id)).to.be.false;
  });

  it("where: { viewCount: { lte: 0 } } includes the zero boundary", async () => {
    // p1/p2/p3 all have viewCount 0
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { lte: 0 } },
    });
    expect(results.length).to.be.at.least(3);
    expect(results.every((r) => (r.viewCount as any as number) <= 0)).to.be
      .true;
  });

  it("where: { viewCount: { between: [5, 15] } } returns posts in the inclusive range", async () => {
    const inRange = await TestPost.create(perspective, {
      title: "InRange",
      body: "",
      viewCount: 10,
    });
    const outRange = await TestPost.create(perspective, {
      title: "OutRange",
      body: "",
      viewCount: 20,
    });
    const results = await TestPost.findAll(perspective, {
      where: { viewCount: { between: [5, 15] } },
    });
    expect(results.some((r) => r.id === inRange.id)).to.be.true;
    expect(results.some((r) => r.id === outRange.id)).to.be.false;
  });

  it("where: { title: { contains: 'lph' } } matches by substring", async () => {
    // p1.title = 'Alpha' contains 'lph'
    const results = await TestPost.findAll(perspective, {
      where: { title: { contains: "lph" } },
    });
    expect(results.some((r) => r.id === p1.id)).to.be.true;
    expect(results.some((r) => r.id === p2.id)).to.be.false;
  });

  it("where: { title: { not: 'Alpha' } } excludes the single matching instance", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { title: { not: "Alpha" } },
    });
    expect(results.some((r) => r.id === p1.id)).to.be.false;
    expect(results.some((r) => r.id === p2.id)).to.be.true;
    expect(results.some((r) => r.id === p3.id)).to.be.true;
  });

  it("where: { title: { not: ['Alpha', 'Beta'] } } excludes multiple values", async () => {
    const results = await TestPost.findAll(perspective, {
      where: { title: { not: ["Alpha", "Beta"] } },
    });
    expect(results.some((r) => r.id === p1.id)).to.be.false;
    expect(results.some((r) => r.id === p2.id)).to.be.false;
    expect(results.some((r) => r.id === p3.id)).to.be.true;
  });

  // ── 3. where — combined conditions (AND semantics) ─────────────────────────

  it("where with two fields applies AND semantics — only rows matching both are returned", async () => {
    // p1: title='Alpha', viewCount=0  → matches { title: 'Alpha', viewCount: 0 }
    // extra: title='Alpha', viewCount=99 → title matches but viewCount doesn't
    const extra = await TestPost.create(perspective, {
      title: "Alpha",
      body: "dupe",
      viewCount: 99,
    });
    const results = await TestPost.findAll(perspective, {
      where: { title: "Alpha", viewCount: 0 },
    });
    expect(results.some((r) => r.id === p1.id)).to.be.true;
    expect(results.some((r) => r.id === extra.id)).to.be.false;
    expect(results.some((r) => r.id === p2.id)).to.be.false;
  });

  // ── 4. where — relation-based filtering ───────────────────────────────────
  //
  // `where` can reference relation fields (@BelongsToOne / @HasMany / etc.)
  // in addition to @Property fields. This enables the pattern:
  //   Comment.findAll(perspective, { where: { post: postId } })
  // which replaces the deprecated `source` parameter.

  it("where on @BelongsToOne: { post: postId } returns only comments linked to that post", async () => {
    const c1 = await TestComment.create(perspective, { body: "on alpha" });
    const c2 = await TestComment.create(perspective, { body: "on beta" });
    const c3 = await TestComment.create(perspective, { body: "orphan" });
    await p1.addComments(c1.id);
    await p2.addComments(c2.id);
    // c3 is not linked to any post

    const results = await TestComment.findAll(perspective, {
      where: { post: p1.id },
    });
    expect(results).to.have.length(1);
    expect(results[0].id).to.equal(c1.id);
    expect(results[0].body).to.equal("on alpha");
  });

  it("where on @BelongsToOne with findOne: { post: postId } returns the first matching comment", async () => {
    const c1 = await TestComment.create(perspective, { body: "the one" });
    await p1.addComments(c1.id);

    const found = await TestComment.findOne(perspective, {
      where: { post: p1.id },
    });
    expect(found).to.not.be.null;
    expect(found!.id).to.equal(c1.id);
  });

  it("where on @BelongsToOne returns null/empty when no comments are linked to the given post", async () => {
    // Create a comment linked to p2, but query for p1
    const c = await TestComment.create(perspective, { body: "on p2" });
    await p2.addComments(c.id);

    const results = await TestComment.findAll(perspective, {
      where: { post: p1.id },
    });
    expect(results).to.have.length(0);

    const found = await TestComment.findOne(perspective, {
      where: { post: p1.id },
    });
    expect(found).to.be.null;
  });

  it("where on @BelongsToOne combined with @Property: { post: postId, body: 'target' }", async () => {
    const c1 = await TestComment.create(perspective, { body: "target" });
    const c2 = await TestComment.create(perspective, { body: "other" });
    await p1.addComments(c1.id);
    await p1.addComments(c2.id);

    const results = await TestComment.findAll(perspective, {
      where: { post: p1.id, body: "target" },
    });
    expect(results).to.have.length(1);
    expect(results[0].body).to.equal("target");
  });

  it("where on @BelongsToOne with IN-style array: { post: [p1.id, p2.id] }", async () => {
    const c1 = await TestComment.create(perspective, { body: "on p1" });
    const c2 = await TestComment.create(perspective, { body: "on p2" });
    const c3 = await TestComment.create(perspective, { body: "on p3" });
    await p1.addComments(c1.id);
    await p2.addComments(c2.id);
    await p3.addComments(c3.id);

    const results = await TestComment.findAll(perspective, {
      where: { post: [p1.id, p2.id] },
    });
    expect(results).to.have.length(2);
    const ids = results.map((r) => r.id);
    expect(ids).to.include(c1.id);
    expect(ids).to.include(c2.id);
    expect(ids).to.not.include(c3.id);
  });

  it("where on @BelongsToOne + include hydrates the relation", async () => {
    const c1 = await TestComment.create(perspective, { body: "hydrate me" });
    await p1.addComments(c1.id);

    const found = await TestComment.findOne(perspective, {
      where: { post: p1.id },
      include: { post: true },
    });
    expect(found).to.not.be.null;
    expect(found!.post).to.be.instanceOf(TestPost);
    expect((found!.post as TestPost).title).to.equal("Alpha");
  });

  // ── 5. order ───────────────────────────────────────────────────────────────

  it("findAll() with order: { title: 'ASC' } sorts alphabetically", async () => {
    const results = await TestPost.findAll(perspective, {
      order: { title: "ASC" },
    });
    const titles = results.map((r) => r.title);
    expect(titles).to.deep.equal([...titles].sort());
  });

  it("findAll() with order: { title: 'DESC' } reverse-sorts", async () => {
    const results = await TestPost.findAll(perspective, {
      order: { title: "DESC" },
    });
    const titles = results.map((r) => r.title);
    expect(titles).to.deep.equal([...titles].sort().reverse());
  });

  it("order: { viewCount: 'ASC' } sorts by a numeric property", async () => {
    const low = await TestPost.create(perspective, {
      title: "Low",
      body: "",
      viewCount: 1,
    });
    const mid = await TestPost.create(perspective, {
      title: "Mid",
      body: "",
      viewCount: 5,
    });
    const high = await TestPost.create(perspective, {
      title: "High",
      body: "",
      viewCount: 9,
    });
    const results = await TestPost.findAll(perspective, {
      where: { id: [low.id, mid.id, high.id] },
      order: { viewCount: "ASC" },
    });
    expect(results).to.have.length(3);
    const counts = results.map((r) => r.viewCount as any as number);
    expect(counts).to.deep.equal([...counts].sort((a, b) => a - b));
  });

  it("order by multiple fields — primary sort then secondary sort", async () => {
    const a1 = await TestPost.create(perspective, {
      title: "A",
      body: "",
      viewCount: 2,
    });
    const a2 = await TestPost.create(perspective, {
      title: "A",
      body: "",
      viewCount: 1,
    });
    const b1 = await TestPost.create(perspective, {
      title: "B",
      body: "",
      viewCount: 5,
    });
    const results = await TestPost.findAll(perspective, {
      where: { id: [a1.id, a2.id, b1.id] },
      order: { title: "ASC", viewCount: "ASC" },
    });
    expect(results).to.have.length(3);
    // All "A" entries must come before "B"
    const titles = results.map((r) => r.title);
    const bIndex = titles.indexOf("B");
    const lastAIndex = titles.lastIndexOf("A");
    expect(lastAIndex).to.be.lessThan(bIndex);
    // Within "A", ascending viewCount: a2 (1) before a1 (2)
    const aEntries = results.filter((r) => r.title === "A");
    expect(aEntries[0].viewCount as any as number).to.equal(1);
    expect(aEntries[1].viewCount as any as number).to.equal(2);
  });

  // ── 6. limit / offset ──────────────────────────────────────────────────────

  it("findAll() with limit returns at most that many results", async () => {
    const results = await TestPost.findAll(perspective, { limit: 2 });
    expect(results.length).to.be.at.most(2);
  });

  it("findAll() with offset skips the first N results", async () => {
    const all = await TestPost.findAll(perspective);
    const paged = await TestPost.findAll(perspective, { offset: 1 });
    expect(paged.length).to.equal(all.length - 1);
  });

  it("findAll() with limit + offset pages correctly without overlap", async () => {
    const page1 = await TestPost.findAll(perspective, {
      limit: 2,
      offset: 0,
      order: { title: "ASC" },
    });
    const page2 = await TestPost.findAll(perspective, {
      limit: 2,
      offset: 2,
      order: { title: "ASC" },
    });
    expect(page1.length).to.be.at.most(2);
    expect(page2.length).to.be.at.most(2);
    const page1Ids = page1.map((p) => p.id);
    const page2Ids = page2.map((p) => p.id);
    expect(page1Ids.some((id) => page2Ids.includes(id))).to.be.false;
  });

  // ── 7. findOne ─────────────────────────────────────────────────────────────

  it("findOne() returns the matching instance", async () => {
    const found = await TestPost.findOne(perspective, { where: { id: p2.id } });
    expect(found).to.not.be.null;
    expect(found!.title).to.equal("Beta");
  });

  it("findOne() returns null when no instance matches", async () => {
    const missing = await TestPost.findOne(perspective, {
      where: { id: "literal://string:no-such-id" },
    });
    expect(missing).to.be.null;
  });

  // ── 8. count ───────────────────────────────────────────────────────────────

  it("count() returns the total number of instances", async () => {
    const n = await TestPost.count(perspective, {});
    expect(n).to.equal(3);
  });

  it("count() with where clause counts only matching instances", async () => {
    const n = await TestPost.count(perspective, { where: { title: "Alpha" } });
    expect(n).to.equal(1);
  });

  it("count() with comparison operator (gt) exercises the JS slow-path", async () => {
    await TestPost.create(perspective, {
      title: "HasCount",
      body: "",
      viewCount: 5,
    });
    // p1/p2/p3 have viewCount 0
    const n = await TestPost.count(perspective, {
      where: { viewCount: { gt: 0 } },
    });
    expect(n).to.equal(1);
  });

  // ── 9. findAllAndCount ─────────────────────────────────────────────────────

  it("findAllAndCount() returns both the instances and the total", async () => {
    const { results, totalCount } = await TestPost.findAllAndCount(
      perspective,
      {},
    );
    expect(results).to.have.length(3);
    expect(totalCount).to.equal(3);
  });

  it("findAllAndCount() with limit returns a page but totalCount reflects all rows", async () => {
    const { results, totalCount } = await TestPost.findAllAndCount(
      perspective,
      { limit: 2 },
    );
    expect(results.length).to.be.at.most(2);
    expect(totalCount).to.equal(3);
  });

  it("findAllAndCount() with where returns filtered results and the matching totalCount", async () => {
    const { results, totalCount } = await TestPost.findAllAndCount(
      perspective,
      {
        where: { title: "Alpha" },
      },
    );
    expect(results).to.have.length(1);
    expect(results[0].id).to.equal(p1.id);
    expect(totalCount).to.equal(1);
  });

  // ── 10. paginate ───────────────────────────────────────────────────────────

  it("paginate() returns the correct page with metadata", async () => {
    const page = await TestPost.paginate(perspective, 2, 1);
    expect(page.results.length).to.be.at.most(2);
    expect(page.totalCount).to.equal(3);
    expect(page.pageNumber).to.equal(1);
    expect(page.pageSize).to.equal(2);
  });

  it("paginate() with where filters before paginating and totalCount reflects matching rows only", async () => {
    // Add a 4th 'Alpha' so we have 2 Alphas to paginate
    await TestPost.create(perspective, { title: "Alpha", body: "duplicate" });
    const page = await TestPost.paginate(perspective, 1, 1, {
      where: { title: "Alpha" },
    });
    expect(page.results).to.have.length(1);
    expect(page.totalCount).to.equal(2); // not all 4 rows
    expect(page.pageSize).to.equal(1);
    expect(page.pageNumber).to.equal(1);
  });

  // ── 11. instance.get() ─────────────────────────────────────────────────────

  it("instance.get() hydrates a bare instance in-place and returns it", async () => {
    // Construct a bare instance with only the id — no data yet
    const bare = new TestPost(perspective, p1.id);
    // Class field initialiser sets title = "" — the property is an empty
    // string, not undefined, before hydration.
    expect(bare.title).to.equal("");

    const hydrated = await bare.get();
    expect(hydrated).to.equal(bare); // returns same instance
    expect(bare.title).to.equal("Alpha");
    expect(bare.body).to.equal("first");
  });

  it("instance.get() with include map eagerly hydrates relations", async () => {
    const comment = await TestComment.create(perspective, {
      body: "get-include",
    });
    await p1.addComments(comment.id);

    const bare = new TestPost(perspective, p1.id);
    await bare.get({ comments: true });

    expect(bare.comments.length).to.be.at.least(1);
    expect(bare.comments[0]).to.be.instanceOf(TestComment);
    expect((bare.comments[0] as TestComment).body).to.equal("get-include");
  });

  // ── 12. fluent QueryBuilder ────────────────────────────────────────────────

  it("fluent .query().where().get() matches findAll()", async () => {
    const json = await TestPost.findAll(perspective, { where: { id: p3.id } });
    const fluent = await TestPost.query(perspective).where({ id: p3.id }).get();
    expect(json.length).to.equal(fluent.length);
    expect(json.every((j, i) => j.id === fluent[i].id)).to.be.true;
  });

  it("fluent .query().where().include().first() matches findOne() with include", async () => {
    const comment = await TestComment.create(perspective, { body: "fluent" });
    await p1.addComments(comment.id);

    const json = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: true },
    });
    const fluent = await TestPost.query(perspective)
      .where({ id: p1.id })
      .include({ comments: true })
      .first();

    expect(json).to.not.be.null;
    expect(fluent).to.not.be.null;
    expect(json!.id).to.equal(fluent!.id);
    expect(json!.comments.length).to.equal(fluent!.comments.length);
    expect(json!.comments[0]).to.be.instanceOf(TestComment);
    expect(fluent!.comments[0]).to.be.instanceOf(TestComment);
  });

  it("Query<T> objects are composable with spread", async () => {
    const base = { order: { title: "ASC" as const } };
    const withLimit = { ...base, limit: 2 };
    const results = await TestPost.findAll(perspective, withLimit);
    expect(results.length).to.be.at.most(2);
    const titles = results.map((r) => r.title);
    expect(titles).to.deep.equal([...titles].sort());
  });

  // ── 13. IncludeMap — relation types ───────────────────────────────────────

  it("include: { comments: true } hydrates @HasMany to TestComment instances", async () => {
    const comment = await TestComment.create(perspective, { body: "hydrated" });
    await p1.addComments(comment.id);
    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: true },
    });
    expect(found).to.not.be.null;
    expect(found!.comments.length).to.be.at.least(1);
    expect(found!.comments[0]).to.be.instanceOf(TestComment);
    expect(found!.comments[0].body).to.equal("hydrated");
  });

  it("without include, @HasMany relations remain as string[]", async () => {
    const comment = await TestComment.create(perspective, {
      body: "stays string",
    });
    await p1.addComments(comment.id);
    const found = await TestPost.findOne(perspective, { where: { id: p1.id } });
    expect(found).to.not.be.null;
    expect(found!.comments.length).to.be.at.least(1);
    expect(typeof found!.comments[0]).to.equal("string");
  });

  it("@HasOne — pinnedComment is a scalar ID without include", async () => {
    const comment = await TestComment.create(perspective, { body: "pinned" });
    await p1.addPinnedComment(comment.id);
    const found = await TestPost.findOne(perspective, { where: { id: p1.id } });
    expect(Array.isArray(found!.pinnedComment)).to.be.false;
    expect(found!.pinnedComment as unknown as string).to.equal(comment.id);
  });

  it("@HasOne — include: { pinnedComment: true } hydrates to a TestComment instance", async () => {
    const comment = await TestComment.create(perspective, {
      body: "Pinned body",
    });
    await p1.addPinnedComment(comment.id);
    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { pinnedComment: true },
    });
    expect(found).to.not.be.null;
    expect(found!.pinnedComment).to.be.instanceOf(TestComment);
    expect((found!.pinnedComment as TestComment).body).to.equal("Pinned body");
  });

  it("include: { post: true } hydrates @BelongsToOne to a TestPost instance", async () => {
    const comment = await TestComment.create(perspective, { body: "reverse" });
    await p1.addComments(comment.id);
    const found = await TestComment.findOne(perspective, {
      where: { id: comment.id },
      include: { post: true },
    });
    expect(found).to.not.be.null;
    expect(found!.post).to.be.instanceOf(TestPost);
    expect((found!.post as TestPost).title).to.equal("Alpha");
  });

  it("@BelongsToMany — tag.posts is string[] without include", async () => {
    const tag = await TestTag.create(perspective, { label: "many" });
    await p1.addTags(tag.id);
    await p2.addTags(tag.id);
    let found: TestTag | null = null;
    await waitUntil(
      async () => {
        found = await TestTag.findOne(perspective, { where: { id: tag.id } });
        if (!found) return false;
        const ids = found.posts as unknown as string[];
        return ids.includes(p1.id) && ids.includes(p2.id);
      },
      5000,
      "tag.posts to include both post IDs",
    );
    expect(Array.isArray(found!.posts)).to.be.true;
    const postIds = found!.posts as unknown as string[];
    expect(postIds).to.include(p1.id);
    expect(postIds).to.include(p2.id);
  });

  it("@BelongsToMany — include: { posts: true } hydrates to TestPost instances", async () => {
    const tag = await TestTag.create(perspective, { label: "hydrated-many" });
    const post1 = await TestPost.create(perspective, {
      title: "Tagged Post 1",
      body: "",
    });
    const post2 = await TestPost.create(perspective, {
      title: "Tagged Post 2",
      body: "",
    });
    await post1.addTags(tag.id);
    await post2.addTags(tag.id);
    const found = await TestTag.findOne(perspective, {
      where: { id: tag.id },
      include: { posts: true },
    });
    expect(found).to.not.be.null;
    expect(found!.posts.every((p) => p instanceof TestPost)).to.be.true;
    expect(found!.posts.some((p) => (p as TestPost).id === post1.id)).to.be
      .true;
    expect(found!.posts.some((p) => (p as TestPost).id === post2.id)).to.be
      .true;
  });

  it("findAll() with include hydrates relations across multiple instances in one pass", async () => {
    const c1 = await TestComment.create(perspective, { body: "for alpha" });
    const c2 = await TestComment.create(perspective, { body: "for beta" });
    await p1.addComments(c1.id);
    await p2.addComments(c2.id);

    const posts = await TestPost.findAll(perspective, {
      where: { id: [p1.id, p2.id] },
      include: { comments: true },
    });
    expect(posts).to.have.length(2);
    const alpha = posts.find((p) => p.id === p1.id)!;
    const beta = posts.find((p) => p.id === p2.id)!;
    expect(alpha.comments.some((c) => (c as TestComment).body === "for alpha"))
      .to.be.true;
    expect(beta.comments.some((c) => (c as TestComment).body === "for beta")).to
      .be.true;
  });

  it("include on a post with no relations returns an empty array, not null", async () => {
    // p3 has no comments attached
    const found = await TestPost.findOne(perspective, {
      where: { id: p3.id },
      include: { comments: true },
    });
    expect(found).to.not.be.null;
    expect(Array.isArray(found!.comments)).to.be.true;
    expect(found!.comments).to.have.length(0);
  });

  // ── 14. IncludeMap — sub-queries ───────────────────────────────────────────

  it("include sub-query: { limit: 2 } caps the number of hydrated relations", async () => {
    for (let i = 0; i < 3; i++) {
      const c = await TestComment.create(perspective, { body: `c${i}` });
      await p1.addComments(c.id);
    }
    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: { limit: 2 } },
    });
    expect(found).to.not.be.null;
    expect(found!.comments.length).to.be.at.most(2);
  });

  it("include sub-query: { where: { id } } narrows hydrated relations to matching ids", async () => {
    const keep = await TestComment.create(perspective, { body: "keep" });
    const drop = await TestComment.create(perspective, { body: "drop" });
    await p1.addComments(keep.id);
    await p1.addComments(drop.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: { where: { id: keep.id } } },
    });
    expect(found).to.not.be.null;
    expect(found!.comments).to.have.length(1);
    expect((found!.comments[0] as TestComment).id).to.equal(keep.id);
    expect((found!.comments[0] as TestComment).body).to.equal("keep");
  });

  it("include sub-query: { order: { body: 'ASC' } } sorts hydrated relations", async () => {
    const cZ = await TestComment.create(perspective, { body: "zzz" });
    const cA = await TestComment.create(perspective, { body: "aaa" });
    const cM = await TestComment.create(perspective, { body: "mmm" });
    await p1.addComments(cZ.id);
    await p1.addComments(cA.id);
    await p1.addComments(cM.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: { order: { body: "ASC" } } },
    });
    expect(found).to.not.be.null;
    const bodies = (found!.comments as TestComment[]).map((c) => c.body);
    expect(bodies).to.deep.equal([...bodies].sort());
  });

  it("@BelongsToMany — include: { posts: { order: { title: 'ASC' } } } orders hydrated posts", async () => {
    const tag = await TestTag.create(perspective, { label: "ordered-many" });
    const postA = await TestPost.create(perspective, {
      title: "Ant",
      body: "",
    });
    const postZ = await TestPost.create(perspective, {
      title: "Zebra",
      body: "",
    });
    await postA.addTags(tag.id);
    await postZ.addTags(tag.id);

    let found: TestTag | null = null;
    await waitUntil(
      async () => {
        found = await TestTag.findOne(perspective, {
          where: { id: tag.id },
          include: { posts: { order: { title: "ASC" } } },
        });
        return (
          found !== null &&
          found.posts.length === 2 &&
          found.posts[0] instanceof TestPost
        );
      },
      5000,
      "tag.posts to be hydrated with 2 entries",
    );
    const titles = (found!.posts as TestPost[]).map((p) => p.title);
    expect(titles).to.deep.equal([...titles].sort());
  });

  it("@BelongsToMany — include: { posts: { limit: 1 } } caps hydrated results", async () => {
    const tag = await TestTag.create(perspective, { label: "capped-many" });
    await p1.addTags(tag.id);
    await p2.addTags(tag.id);
    await p3.addTags(tag.id);

    let found: TestTag | null = null;
    await waitUntil(
      async () => {
        found = await TestTag.findOne(perspective, {
          where: { id: tag.id },
          include: { posts: { limit: 1 } },
        });
        return found !== null && found.posts.length > 0;
      },
      5000,
      "tag.posts to be non-empty",
    );
    expect(found!.posts).to.have.length(1);
  });

  // ── 15. IncludeMap — nested (multi-level) ─────────────────────────────────

  it("nested include: post → comments → reactions (2 levels, findOne)", async () => {
    const comment = await TestComment.create(perspective, { body: "nested" });
    const reaction = await TestReaction.create(perspective, { emoji: "👍" });
    await comment.addReactions(reaction.id);
    await p1.addComments(comment.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: { include: { reactions: true } } },
    });

    expect(found).to.not.be.null;
    const hydratedComment = found!.comments[0] as TestComment;
    expect(hydratedComment).to.be.instanceOf(TestComment);
    expect(hydratedComment.reactions[0]).to.be.instanceOf(TestReaction);
    expect((hydratedComment.reactions[0] as TestReaction).emoji).to.equal("👍");
  });

  it("nested include: findAll() post → comments → reactions across multiple posts", async () => {
    const c1 = await TestComment.create(perspective, { body: "on alpha" });
    const c2 = await TestComment.create(perspective, { body: "on beta" });
    const r1 = await TestReaction.create(perspective, { emoji: "❤️" });
    const r2 = await TestReaction.create(perspective, { emoji: "🚂" });
    await c1.addReactions(r1.id);
    await c2.addReactions(r2.id);
    await p1.addComments(c1.id);
    await p2.addComments(c2.id);

    const posts = await TestPost.findAll(perspective, {
      where: { id: [p1.id, p2.id] },
      include: { comments: { include: { reactions: true } } },
    });

    expect(posts).to.have.length(2);
    const alpha = posts.find((p) => p.title === "Alpha")!;
    const beta = posts.find((p) => p.title === "Beta")!;

    expect(
      ((alpha.comments[0] as TestComment).reactions[0] as TestReaction).emoji,
    ).to.equal("❤️");
    expect(
      ((beta.comments[0] as TestComment).reactions[0] as TestReaction).emoji,
    ).to.equal("🚂");
  });

  it("nested include with sub-query filter: post → comments (body filter) → reactions", async () => {
    const keep = await TestComment.create(perspective, { body: "keep" });
    const drop = await TestComment.create(perspective, { body: "drop" });
    const reaction = await TestReaction.create(perspective, { emoji: "🔥" });
    await keep.addReactions(reaction.id);
    await p1.addComments(keep.id);
    await p1.addComments(drop.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: {
        comments: {
          where: { body: "keep" },
          include: { reactions: true },
        },
      },
    });

    expect(found).to.not.be.null;
    expect(found!.comments).to.have.length(1);
    expect((found!.comments[0] as TestComment).body).to.equal("keep");
    expect(
      ((found!.comments[0] as TestComment).reactions[0] as TestReaction).emoji,
    ).to.equal("🔥");
  });

  it("nested include without the inner include leaves leaf relations as string[]", async () => {
    const comment = await TestComment.create(perspective, { body: "no-nest" });
    const reaction = await TestReaction.create(perspective, { emoji: "🌟" });
    await comment.addReactions(reaction.id);
    await p1.addComments(comment.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: true }, // reactions NOT included
    });

    expect(found).to.not.be.null;
    const hydratedComment = found!.comments[0] as TestComment;
    expect(hydratedComment).to.be.instanceOf(TestComment);
    expect(typeof hydratedComment.reactions[0]).to.equal("string");
  });

  // ── 16. IncludeMap — edge cases (non-conforming nodes) ────────────────────
  //
  // Only nodes that conform to the related model's SDNA class are hydrated;
  // bare URIs or nodes of a different type are silently dropped.

  describe("include: edge cases — non-conforming linked nodes", () => {
    @Model({ name: "EdgeComment" })
    class EdgeComment extends Ad4mModel {
      @Flag({ through: "ad4m://type", value: "ad4m://edge-comment" })
      type!: string;

      @Property({ through: "comment://text" })
      text: string = "";
    }

    @Model({ name: "EdgeArticle" })
    class EdgeArticle extends Ad4mModel {
      @Property({ through: "article://title" })
      title: string = "";

      @HasMany(() => EdgeComment, { through: "article://has_comment" })
      comments: EdgeComment[] = [];
    }
    interface EdgeArticle extends HasManyMethods<"comments"> {}

    let edgePerspective: PerspectiveProxy;

    beforeEach(async () => {
      if (edgePerspective) {
        await ad4m.perspective.remove(edgePerspective.uuid);
      }
      edgePerspective = await ad4m.perspective.add("include-edge-test");
      await edgePerspective.ensureSDNASubjectClass(EdgeComment);
      await edgePerspective.ensureSDNASubjectClass(EdgeArticle);
      await sleep(200);
    });

    afterEach(async () => {
      if (edgePerspective) {
        await ad4m.perspective.remove(edgePerspective.uuid);
        (edgePerspective as any) = null;
      }
    });

    it("include hydrates only conforming nodes — bare URIs are dropped", async () => {
      const article = await EdgeArticle.create(edgePerspective, {
        title: "Article with mixed links",
      });
      const validComment = await EdgeComment.create(edgePerspective, {
        text: "Valid",
      });
      const invalidItem = Literal.from("not-a-comment").toUrl();

      await article.addComments(validComment);
      await edgePerspective.add(
        new Link({
          source: article.id,
          predicate: "article://has_comment",
          target: invalidItem,
        }),
      );

      const retrieved = await EdgeArticle.findOne(edgePerspective, {
        where: { id: article.id },
        include: { comments: true },
      });

      expect(retrieved).to.not.be.null;
      expect(retrieved!.comments).to.have.lengthOf(1);
      expect(retrieved!.comments[0].id).to.equal(validComment.id);
    });

    it("findAll() with include drops non-conforming nodes across multiple instances", async () => {
      const article1 = await EdgeArticle.create(edgePerspective, {
        title: "Article 1",
      });
      const article2 = await EdgeArticle.create(edgePerspective, {
        title: "Article 2",
      });
      const c1 = await EdgeComment.create(edgePerspective, {
        text: "Comment on 1",
      });
      const c2 = await EdgeComment.create(edgePerspective, {
        text: "Comment on 2",
      });

      await article1.addComments(c1);
      await article2.addComments(c2);
      await edgePerspective.add(
        new Link({
          source: article1.id,
          predicate: "article://has_comment",
          target: Literal.from("not-a-comment-1").toUrl(),
        }),
      );
      await edgePerspective.add(
        new Link({
          source: article2.id,
          predicate: "article://has_comment",
          target: Literal.from("not-a-comment-2").toUrl(),
        }),
      );

      const articles = await EdgeArticle.findAll(edgePerspective, {
        include: { comments: true },
      });

      expect(articles).to.have.lengthOf(2);
      const found1 = articles.find((a) => a.title === "Article 1")!;
      const found2 = articles.find((a) => a.title === "Article 2")!;
      expect(found1.comments).to.have.lengthOf(1);
      expect(found1.comments[0].id).to.equal(c1.id);
      expect(found2.comments).to.have.lengthOf(1);
      expect(found2.comments[0].id).to.equal(c2.id);
    });
  });

  // ── 17. properties field projection ───────────────────────────────────────

  it("properties: [] throws — empty array is disallowed", async () => {
    let threw = false;
    try {
      await TestPost.findAll(perspective, { properties: [] });
    } catch (e: any) {
      threw = true;
      expect(e.message).to.include("properties[]");
    }
    expect(threw).to.be.true;
  });

  it("properties: ['title'] returns only id + title — all other schema and metadata fields absent", async () => {
    const results = await TestPost.findAll(perspective, {
      properties: ["title"],
    });
    expect(results.length).to.be.at.least(3);
    for (const r of results) {
      expect(r.id).to.be.a("string");
      expect(r.title).to.be.a("string");
      // The @Property decorator places shadow descriptors on the prototype, so
      // `delete instance.body` removes the own property but `'body' in instance`
      // still returns true.  We assert on own properties to test projection.
      expect(r).to.not.have.own.property("body");
      expect(r).to.not.have.own.property("viewCount");
      expect(r).to.not.have.own.property("comments");
      expect(r).to.not.have.own.property("tags");
      expect(r).to.not.have.own.property("author");
      expect(r).to.not.have.own.property("createdAt");
      expect(r).to.not.have.own.property("updatedAt");
    }
  });

  it("properties: ['body'] on findOne() strips all unrequested schema + metadata fields", async () => {
    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      properties: ["body"],
    });
    expect(found).to.not.be.null;
    expect(found!.id).to.equal(p1.id);
    expect(found!.body).to.equal("first");
    expect(found).to.not.have.own.property("title");
    expect(found).to.not.have.own.property("viewCount");
    expect(found).to.not.have.own.property("author");
    expect(found).to.not.have.own.property("createdAt");
    expect(found).to.not.have.own.property("updatedAt");
  });

  it("properties: ['author', 'createdAt'] returns metadata fields when explicitly requested", async () => {
    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      properties: ["author", "createdAt"],
    });
    expect(found).to.not.be.null;
    expect(found!.id).to.be.a("string");
    expect(found!.author).to.be.a("string");
    expect(found!.createdAt).to.exist;
    expect(found).to.not.have.own.property("title");
    expect(found).to.not.have.own.property("body");
    expect(found).to.not.have.own.property("updatedAt");
  });

  it("properties projection preserves internal machinery — addX methods and save() still work", async () => {
    const results = await TestPost.findAll(perspective, {
      properties: ["title"],
    });
    expect(results.length).to.be.at.least(1);
    const r = results[0];
    expect(r.id).to.be.a("string");
    expect(typeof (r as any).addComments).to.equal("function");
    // save() must not throw — dirty tracking skips everything (nothing changed)
    await r.save();
  });

  it("properties projection + dirty tracking: save() after projected fetch only writes the changed field", async () => {
    // Fetch with only 'title' — snapshot records only title
    const fetched = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      properties: ["title"],
    });
    expect(fetched).to.not.be.null;

    // Mutate title and save — body/comments/etc. are absent so dirty tracking skips them
    fetched!.title = "Updated";
    await fetched!.save();

    const refetched = await TestPost.findOne(perspective, {
      where: { id: p1.id },
    });
    expect(refetched!.title).to.equal("Updated");
    // body was never touched — must still be 'first'
    expect(refetched!.body).to.equal("first");
  });

  it("properties on nested include: { comments: { properties: ['body'] } } strips unrequested fields from comments", async () => {
    const comment = await TestComment.create(perspective, { body: "partial" });
    await p1.addComments(comment.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: { properties: ["body"] } },
    });
    expect(found).to.not.be.null;
    const c = found!.comments[0] as TestComment;
    expect(c.id).to.be.a("string");
    expect(c.body).to.equal("partial");
    expect(c).to.not.have.own.property("reactions");
  });

  it("top-level properties + include: post properties stripped, included relations still hydrated", async () => {
    const comment = await TestComment.create(perspective, { body: "full" });
    await p1.addComments(comment.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      properties: ["title"],
      include: { comments: true },
    });
    expect(found).to.not.be.null;
    expect(found!.title).to.equal("Alpha");
    expect(found).to.not.have.own.property("body");
    expect(found).to.not.have.own.property("author");
    expect(found).to.not.have.own.property("createdAt");
    // include is orthogonal to properties — relations are hydrated regardless
    expect(found!.comments.length).to.be.at.least(1);
    expect(found!.comments[0]).to.be.instanceOf(TestComment);
  });

  // ── 18. parent query ──────────────────────────────────────────────────────

  it("findAll() with parent (raw predicate form) returns only children of that node", async () => {
    const channel = await TestChannel.create(perspective, { name: "chan" });
    await channel.addPosts(p1.id);
    await channel.addPosts(p2.id);

    const results = await TestPost.findAll(perspective, {
      parent: { id: channel.id, predicate: "test://channel_post" },
    });

    const ids = results.map((r) => r.id);
    expect(ids).to.include(p1.id);
    expect(ids).to.include(p2.id);
    expect(ids).to.not.include(p3.id);
  });

  it("findAll() with parent (model-backed, field inferred) returns only children of that node", async () => {
    const channel = await TestChannel.create(perspective, { name: "chan" });
    await channel.addPosts(p1.id);
    await channel.addPosts(p2.id);

    const results = await TestPost.findAll(perspective, {
      parent: { model: TestChannel, id: channel.id },
    });

    const ids = results.map((r) => r.id);
    expect(ids).to.include(p1.id);
    expect(ids).to.include(p2.id);
    expect(ids).to.not.include(p3.id);
  });

  it("findAll() with parent (model-backed, explicit field) resolves the correct predicate", async () => {
    const channel = await TestChannel.create(perspective, { name: "chan" });
    const comment = await TestComment.create(perspective, { body: "hello" });
    await channel.addPosts(p1.id);
    await channel.addComments(comment.id);

    // Explicit field = 'posts' — must not return comments
    const results = await TestPost.findAll(perspective, {
      parent: { model: TestChannel, id: channel.id, field: "posts" },
    });

    const ids = results.map((r) => r.id);
    expect(ids).to.include(p1.id);
    expect(ids).to.not.include(comment.id);
  });

  it("findOne() with parent returns a single child of that node", async () => {
    const channel = await TestChannel.create(perspective, { name: "chan" });
    await channel.addPosts(p1.id);

    const found = await TestPost.findOne(perspective, {
      parent: { model: TestChannel, id: channel.id },
    });

    expect(found).to.not.be.null;
    expect(found!.id).to.equal(p1.id);
  });

  it("findAll() with parent returns empty array when node has no children", async () => {
    const channel = await TestChannel.create(perspective, { name: "empty" });
    // no posts linked

    const results = await TestPost.findAll(perspective, {
      parent: { model: TestChannel, id: channel.id },
    });

    expect(results).to.have.length(0);
  });

  it("parent query and where can be combined", async () => {
    const channel = await TestChannel.create(perspective, { name: "chan" });
    await channel.addPosts(p1.id); // title = "Alpha"
    await channel.addPosts(p2.id); // title = "Beta"

    const results = await TestPost.findAll(perspective, {
      parent: { model: TestChannel, id: channel.id },
      where: { title: "Alpha" },
    });

    expect(results).to.have.length(1);
    expect(results[0].id).to.equal(p1.id);
  });
});
