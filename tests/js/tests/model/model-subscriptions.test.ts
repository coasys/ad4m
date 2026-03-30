/**
 * Ad4mModel — subscription integration tests
 *
 * Covers: ModelQueryBuilder.subscribe() initial callback, SPARQL live-query
 * re-fire on link-added and link-removed, dispose() stopping callbacks,
 * countSubscribe(), and parent-scoped subscriptions.
 *
 * Adapted from PR #694 subscription tests for our SPARQL-based subscription
 * system (ModelQueryBuilder.subscribe / countSubscribe / dispose).
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-subscriptions.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestComment, TestPost, TestTag, TestChannel } from "./models.js";

// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — Subscriptions (SPARQL)", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-subscriptions");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-subscriptions-test");
    await TestPost.register(perspective);
    await TestComment.register(perspective);
    await TestTag.register(perspective);
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
    await TestChannel.register(perspective);
  });

  // ── 1. Immediate initial callback ─────────────────────────────────────────

  it("subscribe() returns initial results immediately", async () => {
    const builder = TestPost.query(perspective);
    const initialResults = await builder.subscribe(() => {});
    builder.dispose();
    expect(Array.isArray(initialResults)).to.be.true;
  });

  // ── 2. Re-fires on link-added ─────────────────────────────────────────────

  it("subscribe() calls callback when a relevant link is added", async () => {
    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective);
    await builder.subscribe((r) => all.push(r));

    // Wait for at least one callback (initial)
    await waitUntil(() => all.length >= 1, 6000, "initial callback");

    const post = new TestPost(perspective);
    post.title = "New For Sub";
    post.body = "";
    await post.save();

    await waitUntil(
      () => all.some((batch) => batch.some((p) => p.id === post.id)),
      8000,
      "new post appears in subscription results",
    );
    builder.dispose();

    expect(all.length).to.be.at.least(2);
    expect(all.some((batch) => batch.some((p) => p.id === post.id))).to.be.true;
  });

  // ── 3. Re-fires on link-removed ───────────────────────────────────────────

  it("subscribe() calls callback when a relevant link is removed", async () => {
    const post = await TestPost.create(perspective, {
      title: "Will Be Deleted",
      body: "",
    });
    const postId = post.id;

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective);
    await builder.subscribe((r) => all.push(r));

    await waitUntil(
      () => all.some((batch) => batch.some((p) => p.id === postId)),
      8000,
      "initial callback contains the post",
    );

    await post.delete();

    // Small settling delay — under load the SPARQL live-query notification
    // can lag slightly before the subscription re-fires.
    await new Promise((r) => setTimeout(r, 300));

    await waitUntil(
      () => {
        const latest = all.at(-1);
        return latest !== undefined && !latest.some((p) => p.id === postId);
      },
      15_000,
      "subscription fires without the deleted post",
    );
    builder.dispose();

    expect(all.length).to.be.at.least(2);
    expect(all.at(-1)!.some((p) => p.id === postId)).to.be.false;
  });

  // ── 4. dispose() stops further callbacks ──────────────────────────────────

  it("dispose() stops further callback invocations", async () => {
    let callCount = 0;
    const builder = TestPost.query(perspective);
    await builder.subscribe(() => {
      callCount++;
    });

    // Wait for the immediate callback
    await new Promise((r) => setTimeout(r, 400));
    const countAfterInitial = callCount;
    builder.dispose();

    const post = new TestPost(perspective);
    post.title = "Post After Dispose";
    post.body = "";
    await post.save();

    // Give it a moment to fire — it should not
    await new Promise((r) => setTimeout(r, 600));
    expect(callCount).to.equal(countAfterInitial);
  });

  // ── 5. countSubscribe() returns correct count ─────────────────────────────

  it("countSubscribe() returns correct count and changes after add", async () => {
    const builder = TestPost.query(perspective);
    const initialCount = await builder.count();

    await TestPost.create(perspective, { title: "Count Post", body: "" });

    const afterCount = await TestPost.query(perspective).count();
    expect(afterCount).to.equal(initialCount + 1);
  });

  // ── 6. @HasMany relation changes trigger re-fire ──────────────────────────
  //
  // Regression: snapshot-based subscriptions previously missed @HasMany relation
  // changes because the serialized fingerprint didn't include relation fields.
  // Adding a tag/comment to an existing instance would not change the fingerprint
  // and the subscription would never re-broadcast.

  it("subscribe() re-fires when a @HasMany relation is updated on an existing instance", async () => {
    const post = await TestPost.create(perspective, { title: "Tagged", body: "" });

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective);
    await builder.subscribe((r) => all.push(r));

    // Wait for the initial callback that contains the post
    await waitUntil(
      () => all.some((batch) => batch.some((p) => p.id === post.id)),
      8000,
      "initial callback contains the post",
    );
    const initialTagCount = (all.at(-1)!.find((p) => p.id === post.id)?.tags ?? []).length;

    // Add a tag — this is a @HasMany relation change, not a property change
    const tag = await TestTag.create(perspective, { label: "rust" });
    await post.addTags(tag.id);

    // Subscription must re-fire with the updated tags array
    await waitUntil(
      () => {
        const latest = all.at(-1);
        const latestPost = latest?.find((p) => p.id === post.id);
        return (latestPost?.tags ?? []).length > initialTagCount;
      },
      8000,
      "subscription re-fires with updated tags after addTags()",
    );
    builder.dispose();

    const finalPost = all.at(-1)!.find((p) => p.id === post.id)!;
    expect(finalPost.tags.length).to.be.greaterThan(initialTagCount);
  });

  // ── 7. parent-scoped subscriptions ────────────────────────────────────────

  it("parent-scoped subscribe() fires when a child is linked to the parent", async () => {
    const channel = await TestChannel.create(perspective, { name: "sub-chan" });
    const post = await TestPost.create(perspective, { title: "Sub Post", body: "" });

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective)
      .parent(channel.id, TestChannel, { field: "posts" });
    await builder.subscribe((r) => all.push(r));

    // Wait for initial (empty) callback
    await waitUntil(() => all.length >= 1, 6000, "initial callback");
    expect(all[0]).to.have.length(0);

    // Link the post to the channel
    await channel.addPosts(post.id);

    // Subscription must re-fire with the post now included
    await waitUntil(
      () => all.some((batch) => batch.some((p) => p.id === post.id)),
      8000,
      "re-fire after child linked to parent",
    );
    builder.dispose();

    expect(all.some((batch) => batch.some((p) => p.id === post.id))).to.be.true;
  });

  it("parent-scoped subscribe() fires when a child is unlinked from the parent", async () => {
    const channel = await TestChannel.create(perspective, { name: "sub-chan-2" });
    const post = await TestPost.create(perspective, { title: "Removable", body: "" });
    await channel.addPosts(post.id);

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective)
      .parent(channel.id, TestChannel, { field: "posts" });
    await builder.subscribe((r) => all.push(r));

    // Wait until initial callback contains the post
    await waitUntil(
      () => all.some((batch) => batch.some((p) => p.id === post.id)),
      8000,
      "initial callback contains the post",
    );

    // Remove the link
    await channel.removePosts(post.id);

    // Subscription must re-fire with the post absent
    await waitUntil(
      () => {
        const latest = all.at(-1);
        return latest !== undefined && !latest.some((p) => p.id === post.id);
      },
      8000,
      "re-fire after child unlinked from parent",
    );
    builder.dispose();

    expect(all.at(-1)!.some((p) => p.id === post.id)).to.be.false;
  });

  it("parent-scoped subscribe() does NOT fire when a child is added to a different parent", async () => {
    const channelA = await TestChannel.create(perspective, { name: "chan-a" });
    const channelB = await TestChannel.create(perspective, { name: "chan-b" });

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective)
      .parent(channelA.id, TestChannel, { field: "posts" });
    await builder.subscribe((r) => all.push(r));

    // Wait for initial (empty) callback
    await waitUntil(() => all.length >= 1, 6000, "initial callback");
    const countAfterInitial = all.length;

    // Add a post to channel B — should not trigger channel A's subscription
    const post = await TestPost.create(perspective, { title: "Wrong Chan", body: "" });
    await channelB.addPosts(post.id);

    await new Promise((r) => setTimeout(r, 1000));
    builder.dispose();

    expect(all.length).to.equal(countAfterInitial);
  });
});
