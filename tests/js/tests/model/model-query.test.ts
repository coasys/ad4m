/**
 * Ad4mModel — query API integration tests
 *
 * Covers: findAll() with where / order / limit / offset, findOne(), count(),
 * paginate(), findAllAndCount(), fluent ModelQueryBuilder, IncludeMap eager
 * loading, and Query<T> composability.
 *
 * Ported from playground scenarios 02 (Querying), 03 (Collections),
 * 04 (Relationships & Include). The query-heavy tests have been moved to model-query.test.ts.
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
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective, sleep } from "../../utils/utils.js";
import { TestComment, TestPost, TestTag, TestReaction } from "./models.js";

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
    // Re-seed fresh posts for every test so tests are fully independent
    p1 = await TestPost.create(perspective, { title: "Alpha", body: "first" });
    p2 = await TestPost.create(perspective, { title: "Beta", body: "second" });
    p3 = await TestPost.create(perspective, { title: "Gamma", body: "third" });
  });

  // ── where ──────────────────────────────────────────────────────────────────

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

  // ── order ──────────────────────────────────────────────────────────────────

  it("findAll() with order: title ASC sorts alphabetically", async () => {
    const results = await TestPost.findAll(perspective, {
      order: { title: "ASC" },
    });
    const titles = results.map((r) => r.title);
    expect(titles).to.deep.equal([...titles].sort());
  });

  it("findAll() with order: title DESC reverse-sorts", async () => {
    const results = await TestPost.findAll(perspective, {
      order: { title: "DESC" },
    });
    const titles = results.map((r) => r.title);
    expect(titles).to.deep.equal([...titles].sort().reverse());
  });

  // ── limit / offset ─────────────────────────────────────────────────────────

  it("findAll() with limit returns at most that many results", async () => {
    const results = await TestPost.findAll(perspective, { limit: 2 });
    expect(results.length).to.be.at.most(2);
  });

  it("findAll() with offset skips the first N results", async () => {
    const all = await TestPost.findAll(perspective);
    const paged = await TestPost.findAll(perspective, { offset: 1 });
    expect(paged.length).to.equal(all.length - 1);
  });

  it("findAll() with limit + offset pages correctly", async () => {
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
    // No overlap
    expect(page1.map((p) => p.id)).to.not.include(page2.map((p) => p.id)[0]);
  });

  // ── findOne ────────────────────────────────────────────────────────────────

  it("findOne() returns the matching instance or null", async () => {
    const found = await TestPost.findOne(perspective, { where: { id: p2.id } });
    expect(found).to.not.be.null;
    expect(found!.title).to.equal("Beta");

    const missing = await TestPost.findOne(perspective, {
      where: { id: "literal://string:no-such-id" },
    });
    expect(missing).to.be.null;
  });

  // ── count ──────────────────────────────────────────────────────────────────

  it("count() returns the number of matching instances", async () => {
    const n = await TestPost.count(perspective, {});
    expect(n).to.equal(3);
  });

  it("count() with where clause counts only matching instances", async () => {
    const n = await TestPost.count(perspective, { where: { title: "Alpha" } });
    expect(n).to.equal(1);
  });

  // ── findAllAndCount ────────────────────────────────────────────────────────

  it("findAllAndCount() returns both the instances and the total", async () => {
    const { results, totalCount } = await TestPost.findAllAndCount(
      perspective,
      {},
    );
    expect(results).to.have.length(3);
    expect(totalCount).to.equal(3);
  });

  it("findAllAndCount() with limit returns paged results with full totalCount", async () => {
    const { results, totalCount } = await TestPost.findAllAndCount(
      perspective,
      { limit: 2 },
    );
    expect(results.length).to.be.at.most(2);
    expect(totalCount).to.equal(3);
  });

  // ── paginate ───────────────────────────────────────────────────────────────

  it("paginate() returns the correct page with metadata", async () => {
    const page = await TestPost.paginate(perspective, 2, 1);
    expect(page.results.length).to.be.at.most(2);
    expect(page.totalCount).to.equal(3);
    expect(page.pageNumber).to.equal(1);
    expect(page.pageSize).to.equal(2);
  });

  // ── fluent QueryBuilder ────────────────────────────────────────────────────

  it("fluent .query().where().get() matches JSON findAll()", async () => {
    const json = await TestPost.findAll(perspective, { where: { id: p3.id } });
    const fluent = await TestPost.query(perspective).where({ id: p3.id }).get();
    expect(json.length).to.equal(fluent.length);
    expect(json.every((j, i) => j.id === fluent[i].id)).to.be.true;
  });

  it("Query<T> objects are composable with spread", async () => {
    const base = { order: { title: "ASC" as const } };
    const withLimit = { ...base, limit: 2 };
    const results = await TestPost.findAll(perspective, withLimit);
    expect(results.length).to.be.at.most(2);
    const titles = results.map((r) => r.title);
    expect(titles).to.deep.equal([...titles].sort());
  });

  // ── IncludeMap eager loading ───────────────────────────────────────────────

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

  it("include sub-query: { comments: { limit: 2 } } caps related results", async () => {
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

  // ── @HasOne — returns scalar, not array ────────────────────────────────────

  it("@HasOne — pinnedComment is a scalar ID without include", async () => {
    const comment = await TestComment.create(perspective, { body: "pinned" });
    await p1.addPinnedComment(comment.id);
    const found = await TestPost.findOne(perspective, { where: { id: p1.id } });
    expect(Array.isArray(found!.pinnedComment)).to.be.false;
    expect(found!.pinnedComment as unknown as string).to.equal(comment.id);
  });

  // ── @BelongsToMany ─────────────────────────────────────────────────────────

  it("@BelongsToMany — tag.posts is string[] without include", async () => {
    const tag = await TestTag.create(perspective, { label: "many" });
    await p1.addTags(tag.id);
    await p2.addTags(tag.id);
    const found = await TestTag.findOne(perspective, { where: { id: tag.id } });
    expect(Array.isArray(found!.posts)).to.be.true;
    const postIds = found!.posts as unknown as string[];
    expect(postIds).to.include(p1.id);
    expect(postIds).to.include(p2.id);
  });

  it("include sub-query { where: { id } } narrows hydrated relations to matching ids", async () => {
    const keep = await TestComment.create(perspective, { body: "keep" });
    const drop = await TestComment.create(perspective, { body: "drop" });
    await p1.addComments(keep.id);
    await p1.addComments(drop.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: { where: { id: keep.id } } },
    });
    expect(found).to.not.be.null;
    // Only 'keep' should survive the sub-query id filter
    expect(found!.comments).to.have.length(1);
    expect((found!.comments[0] as TestComment).id).to.equal(keep.id);
    expect((found!.comments[0] as TestComment).body).to.equal("keep");
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

  // ── Nested (multi-level) include ────────────────────────────────────────────

  it("nested include: post → comments → reactions (2 levels)", async () => {
    const comment = await TestComment.create(perspective, { body: "nested" });
    const reaction = await TestReaction.create(perspective, { emoji: "👍" });
    await comment.addReactions(reaction.id);
    await p1.addComments(comment.id);

    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: {
        comments: {
          include: { reactions: true },
        },
      },
    });

    expect(found).to.not.be.null;
    expect(found!.comments.length).to.be.at.least(1);
    const hydratedComment = found!.comments[0] as TestComment;
    expect(hydratedComment).to.be.instanceOf(TestComment);
    expect(hydratedComment.reactions.length).to.be.at.least(1);
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
      include: {
        comments: {
          include: { reactions: true },
        },
      },
    });

    expect(posts).to.have.length(2);
    const alpha = posts.find((p) => p.title === "Alpha")!;
    const beta = posts.find((p) => p.title === "Beta")!;

    expect(alpha.comments[0]).to.be.instanceOf(TestComment);
    expect((alpha.comments[0] as TestComment).reactions[0]).to.be.instanceOf(
      TestReaction,
    );
    expect(
      ((alpha.comments[0] as TestComment).reactions[0] as TestReaction).emoji,
    ).to.equal("❤️");

    expect(beta.comments[0]).to.be.instanceOf(TestComment);
    expect((beta.comments[0] as TestComment).reactions[0]).to.be.instanceOf(
      TestReaction,
    );
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
    expect((found!.comments[0] as TestComment).reactions[0]).to.be.instanceOf(
      TestReaction,
    );
    expect(
      ((found!.comments[0] as TestComment).reactions[0] as TestReaction).emoji,
    ).to.equal("🔥");
  });

  it("nested include without inner include leaves reactions as string[]", async () => {
    const comment = await TestComment.create(perspective, { body: "no-nest" });
    const reaction = await TestReaction.create(perspective, { emoji: "🌟" });
    await comment.addReactions(reaction.id);
    await p1.addComments(comment.id);

    // include comments but do NOT include reactions within them
    const found = await TestPost.findOne(perspective, {
      where: { id: p1.id },
      include: { comments: true },
    });

    expect(found).to.not.be.null;
    const hydratedComment = found!.comments[0] as TestComment;
    expect(hydratedComment).to.be.instanceOf(TestComment);
    // reactions not requested — should stay as string[]
    expect(typeof hydratedComment.reactions[0]).to.equal("string");
  });

  // ── include: edge cases — non-conforming linked nodes ─────────────────────
  //
  // These tests verify that when using include: { rel: true }, only nodes that
  // actually conform to the related model's SDNA class are hydrated; bare URIs
  // or nodes of a different type are silently dropped.

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
      // Manually add a link to a non-EdgeComment target
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
      // Add non-conforming links to both
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

      const found1 = articles.find((a) => a.title === "Article 1");
      const found2 = articles.find((a) => a.title === "Article 2");

      expect(found1!.comments).to.have.lengthOf(1);
      expect(found1!.comments[0]).to.be.instanceOf(EdgeComment);
      expect(found1!.comments[0].id).to.equal(c1.id);

      expect(found2!.comments).to.have.lengthOf(1);
      expect(found2!.comments[0]).to.be.instanceOf(EdgeComment);
      expect(found2!.comments[0].id).to.equal(c2.id);
    });
  });
});
