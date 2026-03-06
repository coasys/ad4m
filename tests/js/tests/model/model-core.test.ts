/**
 * Ad4mModel — core CRUD integration tests
 *
 * Covers: save() / create() / get() / findAll() / findOne() / delete()
 * and the @Flag / @Property decorator round-trip.
 *
 * Ported from playground scenarios 01 (Basic CRUD) and 08 (Decorator API)
 * with all six decorator types covered.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-core.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, LinkQuery, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestComment, TestPost, TestTag } from "./models.js";

describe("Ad4mModel — Core CRUD", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-core");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-core-test");
    await TestPost.register(perspective);
    await TestComment.register(perspective);
    await TestTag.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestPost.register(perspective);
    await TestComment.register(perspective);
    await TestTag.register(perspective);
  });

  // ── save() / id ────────────────────────────────────────────────────────────

  it("save() populates a non-empty id", async () => {
    const post = new TestPost(perspective);
    post.title = "CRUD Test";
    post.body = "body";
    await post.save();
    expect(post.id).to.not.equal("");
  });

  it("create() constructs, assigns and saves in one call", async () => {
    const post = await TestPost.create(perspective, {
      title: "Created",
      body: "via create",
    });
    expect(post.id).to.not.equal("");
    expect(post.title).to.equal("Created");
    expect(post.body).to.equal("via create");

    const found = await TestPost.findOne(perspective, {
      where: { id: post.id },
    });
    expect(found).to.not.be.null;
    expect(found!.title).to.equal("Created");
  });

  // ── get() ──────────────────────────────────────────────────────────────────

  it("get() re-reads persisted values from the perspective", async () => {
    const post = await TestPost.create(perspective, {
      title: "getData Target",
      body: "snapshot body",
    });
    const data = await post.get();
    expect(data).to.be.an("object");
    expect(data.title).to.equal("getData Target");
    expect(data.body).to.equal("snapshot body");
  });

  // ── findAll() ──────────────────────────────────────────────────────────────

  it("findAll() returns all saved instances", async () => {
    const post = await TestPost.create(perspective, {
      title: "Count Test",
      body: "",
    });
    const results = await TestPost.findAll(perspective, {
      where: { id: post.id },
    });
    expect(results).to.have.length(1);
    expect(results[0].title).to.equal("Count Test");
  });

  // ── @Flag ──────────────────────────────────────────────────────────────────

  it("@Flag — findAll() returns only TestPost instances", async () => {
    await TestPost.create(perspective, { title: "Flag Check", body: "" });
    await TestComment.create(perspective, { body: "not a post" });

    let posts: TestPost[] = [];
    await waitUntil(
      async () => {
        posts = await TestPost.findAll(perspective);
        return posts.length > 0;
      },
      8000,
      "first post appears in findAll()",
    );

    expect(posts.every((p) => p instanceof TestPost)).to.be.true;
    // Comments should not appear in TestPost.findAll() — different @Flag
    expect(
      posts.some(
        (p) =>
          (p as any).body !== undefined && (p as any).type === "test://comment",
      ),
    ).to.be.false;
  });

  it("@Flag — flag value survives re-save (immutable after creation)", async () => {
    const post = new TestPost(perspective);
    post.title = "Flag Immutability";
    post.body = "";
    await post.save();

    post.title = "Updated Title";
    await post.save();

    const found = await TestPost.findAll(perspective, {
      where: { id: post.id },
    });
    expect(found).to.have.length(1);
    expect(found[0].title).to.equal("Updated Title");
    expect(found[0].type).to.equal("test://post");
  });

  // ── @Property round-trip ───────────────────────────────────────────────────

  it("@Property — fields round-trip correctly through save/findOne", async () => {
    const post = await TestPost.create(perspective, {
      title: "Round Trip",
      body: "body text",
    });
    const found = await TestPost.findOne(perspective, {
      where: { id: post.id },
    });
    expect(found).to.not.be.null;
    expect(found!.title).to.equal("Round Trip");
    expect(found!.body).to.equal("body text");
  });

  // ── re-save / update ───────────────────────────────────────────────────────

  it("save() on existing instance updates without creating a duplicate", async () => {
    const post = new TestPost(perspective);
    post.title = "Original";
    post.body = "";
    await post.save();
    const id = post.id;

    post.title = "Updated";
    await post.save();

    const all = await TestPost.findAll(perspective, { where: { id } });
    expect(all).to.have.length(1);
    expect(all[0].title).to.equal("Updated");
  });

  // ── findOne ────────────────────────────────────────────────────────────────

  it("findOne() returns matching instance", async () => {
    const post = await TestPost.create(perspective, {
      title: "FindOne Target",
      body: "",
    });
    const found = await TestPost.findOne(perspective, {
      where: { id: post.id },
    });
    expect(found).to.not.be.null;
    expect(found).to.be.instanceOf(TestPost);
    expect(found!.id).to.equal(post.id);
    expect(found!.title).to.equal("FindOne Target");
  });

  it("findOne() returns null for non-existent id", async () => {
    const missing = await TestPost.findOne(perspective, {
      where: { id: "literal://string:no-such-id" },
    });
    expect(missing).to.be.null;
  });

  // ── delete ─────────────────────────────────────────────────────────────────

  it("delete() removes the instance from the perspective", async () => {
    const post = await TestPost.create(perspective, {
      title: "Delete Target",
      body: "",
    });
    const id = post.id;
    await post.delete();
    const found = await TestPost.findAll(perspective, { where: { id } });
    expect(found).to.have.length(0);
  });

  // ── static delete() ────────────────────────────────────────────────────────

  it("TestPost.delete(perspective, id) removes the instance", async () => {
    const post = await TestPost.create(perspective, { title: "Static Delete", body: "" });
    const id = post.id;
    await TestPost.delete(perspective, id);
    const found = await TestPost.findAll(perspective, { where: { id } });
    expect(found).to.have.length(0);
  });

  // ── static update() ────────────────────────────────────────────────────────

  it("TestPost.update() mutates only the specified field and leaves others intact", async () => {
    const post = await TestPost.create(perspective, { title: "Before", body: "Keep this" });

    await TestPost.update(perspective, post.id, { title: "After" });

    const found = await TestPost.findOne(perspective, { where: { id: post.id } });
    expect(found).to.not.be.null;
    expect(found!.title).to.equal("After");
    expect(found!.body).to.equal("Keep this");
  });

  it("TestPost.update() returns the updated instance", async () => {
    const post = await TestPost.create(perspective, { title: "Original", body: "" });
    const updated = await TestPost.update(perspective, post.id, { title: "Returned" });
    expect(updated).to.be.instanceOf(TestPost);
    expect(updated.id).to.equal(post.id);
    expect(updated.title).to.equal("Returned");
  });

  it("TestPost.update() with multiple fields updates all of them", async () => {
    const post = await TestPost.create(perspective, { title: "Old Title", body: "Old Body" });
    await TestPost.update(perspective, post.id, { title: "New Title", body: "New Body" });
    const found = await TestPost.findOne(perspective, { where: { id: post.id } });
    expect(found!.title).to.equal("New Title");
    expect(found!.body).to.equal("New Body");
  });

  // ── @HasMany — addComments ─────────────────────────────────────────────────

  it("@HasMany — addComments() links comment to post", async () => {
    const post = await TestPost.create(perspective, {
      title: "Post With Comment",
      body: "",
    });
    const comment = await TestComment.create(perspective, {
      body: "Nice post!",
    });
    await post.addComments(comment);
    const updated = await TestPost.findOne(perspective, {
      where: { id: post.id },
      include: { comments: true },
    });
    expect(updated).to.not.be.null;
    expect(updated!.comments.some((c) => c.id === comment.id)).to.be.true;
  });

  it("@HasMany — removeComments() unlinks a comment from a post", async () => {
    const post = await TestPost.create(perspective, {
      title: "Post For Remove",
      body: "",
    });
    const c1 = await TestComment.create(perspective, { body: "To keep" });
    const c2 = await TestComment.create(perspective, { body: "To remove" });
    await post.addComments(c1);
    await post.addComments(c2);
    await post.removeComments(c2);
    const found = await TestPost.findOne(perspective, {
      where: { id: post.id },
      include: { comments: true },
    });
    expect(found).to.not.be.null;
    expect(found!.comments.some((c) => c.id === c1.id)).to.be.true;
    expect(found!.comments.some((c) => c.id === c2.id)).to.be.false;
  });

  it("@HasMany — setComments() replaces entire relation set atomically", async () => {
    const post = await TestPost.create(perspective, {
      title: "Post For Set",
      body: "",
    });
    const c1 = await TestComment.create(perspective, { body: "Initial A" });
    const c2 = await TestComment.create(perspective, { body: "Initial B" });
    await post.addComments(c1);
    await post.addComments(c2);
    const c3 = await TestComment.create(perspective, { body: "Replacement" });
    await post.setComments([c3]);
    const found = await TestPost.findOne(perspective, {
      where: { id: post.id },
      include: { comments: true },
    });
    expect(found).to.not.be.null;
    expect(found!.comments.some((c) => c.id === c1.id)).to.be.false;
    expect(found!.comments.some((c) => c.id === c2.id)).to.be.false;
    expect(found!.comments.some((c) => c.id === c3.id)).to.be.true;
    expect(found!.comments).to.have.length(1);
  });

  // ── @HasOne ────────────────────────────────────────────────────────────────

  it("@HasOne — pinnedComment hydrates to a TestComment instance", async () => {
    const post = await TestPost.create(perspective, {
      title: "Post With Pin",
      body: "",
    });
    const comment = await TestComment.create(perspective, { body: "Pinned!" });
    await post.addPinnedComment(comment);
    const updated = await TestPost.findOne(perspective, {
      where: { id: post.id },
      include: { pinnedComment: true },
    });
    expect(updated).to.not.be.null;
    expect(updated!.pinnedComment).to.be.instanceOf(TestComment);
    expect((updated!.pinnedComment as TestComment).id).to.equal(comment.id);
  });

  // ── @BelongsToOne ──────────────────────────────────────────────────────────

  it("@BelongsToOne — comment.post resolves to a TestPost instance", async () => {
    const post = await TestPost.create(perspective, {
      title: "Parent Post",
      body: "",
    });
    const comment = await TestComment.create(perspective, {
      body: "Reverse traversal test",
    });
    await post.addComments(comment);
    const found = await TestComment.findOne(perspective, {
      where: { id: comment.id },
      include: { post: true },
    });
    expect(found).to.not.be.null;
    expect(found!.post).to.be.instanceOf(TestPost);
    expect((found!.post as TestPost).id).to.equal(post.id);
  });

  it("@BelongsToOne — comment.pinnedBy resolves to the post that pinned it", async () => {
    const post = await TestPost.create(perspective, {
      title: "Pinning Post",
      body: "",
    });
    const comment = await TestComment.create(perspective, {
      body: "I am the pinned comment",
    });
    await post.addPinnedComment(comment);
    const found = await TestComment.findOne(perspective, {
      where: { id: comment.id },
      include: { pinnedBy: true },
    });
    expect(found).to.not.be.null;
    expect(found!.pinnedBy).to.be.instanceOf(TestPost);
    expect((found!.pinnedBy as TestPost).id).to.equal(post.id);

    const unpinned = await TestComment.create(perspective, {
      body: "Not pinned",
    });
    const foundUnpinned = await TestComment.findOne(perspective, {
      where: { id: unpinned.id },
      include: { pinnedBy: true },
    });
    expect(foundUnpinned!.pinnedBy).to.be.null;
  });

  // ── @BelongsToMany ─────────────────────────────────────────────────────────

  it("@BelongsToMany — tag.posts contains all posts that use the tag", async () => {
    const tag = await TestTag.create(perspective, { label: "belongs-many" });
    const post1 = await TestPost.create(perspective, {
      title: "Tagged 1",
      body: "",
    });
    const post2 = await TestPost.create(perspective, {
      title: "Tagged 2",
      body: "",
    });
    await post1.addTags(tag.id);
    await post2.addTags(tag.id);
    const found = await TestTag.findOne(perspective, {
      where: { id: tag.id },
      include: { posts: true },
    });
    expect(found).to.not.be.null;
    expect(found!.posts.some((p) => p.id === post1.id)).to.be.true;
    expect(found!.posts.some((p) => p.id === post2.id)).to.be.true;
  });

  // ── relation links are visible in the raw perspective ─────────────────────

  it("relation links are visible via perspective.get() after add*()", async () => {
    const post = await TestPost.create(perspective, {
      title: "Link Visibility",
      body: "",
    });
    const c = await TestComment.create(perspective, { body: "visible" });
    await post.addComments(c.id);
    const links = await perspective.get(
      new LinkQuery({ predicate: "test://has_comment", source: post.id }),
    );
    expect(links.length).to.be.at.least(1);
    expect(links.some((l) => l.data.target === c.id)).to.be.true;
  });

  // ── createdAt / updatedAt / author ─────────────────────────────────────────

  it("findOne() populates createdAt, updatedAt, and author after save", async () => {
    const post = await TestPost.create(perspective, {
      title: "Meta Fields",
      body: "",
    });
    const found = await TestPost.findOne(perspective, {
      where: { id: post.id },
    });
    expect(found).to.not.be.null;
    expect(found!.createdAt).to.not.be.undefined;
    expect(found!.updatedAt).to.not.be.undefined;
    expect(found!.author).to.be.a("string").and.not.equal("");
    // createdAt ≤ updatedAt always holds (equal when nothing changed)
    expect(Number(found!.createdAt)).to.be.at.most(Number(found!.updatedAt));
  });

  it("updatedAt advances past createdAt after a re-save", async () => {
    const post = await TestPost.create(perspective, {
      title: "Timestamp Advance",
      body: "",
    });
    // Small pause — ensures the re-save link gets a strictly later timestamp
    await new Promise((r) => setTimeout(r, 100));

    post.body = "updated body";
    await post.save();

    const found = await TestPost.findOne(perspective, {
      where: { id: post.id },
    });
    expect(found).to.not.be.null;
    expect(Number(found!.updatedAt)).to.be.greaterThan(
      Number(found!.createdAt),
    );
  });

  // ── get(include) ───────────────────────────────────────────────────────────

  it("get(include) hydrates relations on a bare-id instance", async () => {
    const post = await TestPost.create(perspective, {
      title: "Get Include Post",
      body: "",
    });
    const comment = await TestComment.create(perspective, {
      body: "populated via get",
    });
    await post.addComments(comment.id);

    // Construct a fresh instance with only the id — nothing loaded yet
    const bare = new TestPost(perspective, post.id);
    await bare.get({ comments: true });

    expect(bare.comments).to.be.an("array").with.length(1);
    expect(bare.comments[0]).to.be.instanceOf(TestComment);
    expect((bare.comments[0] as TestComment).id).to.equal(comment.id);
    expect((bare.comments[0] as TestComment).body).to.equal(
      "populated via get",
    );
  });
});
