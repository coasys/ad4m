/**
 * Ad4mModel — subscription integration tests
 *
 * Covers: ModelQueryBuilder.subscribe() initial callback, SPARQL live-query
 * re-fire on link-added and link-removed, dispose() stopping callbacks,
 * countSubscribe(), parent-scoped subscriptions, re-opening after the
 * client's socket reconnects, and the writes on related records that re-run
 * a subscription (#1237: includes, `where` filters and quantifiers, dotted
 * order keys, `producedByFlow`).
 *
 * Adapted from PR #694 subscription tests for our SPARQL-based subscription
 * system (ModelQueryBuilder.subscribe / countSubscribe / dispose).
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-subscriptions.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  FlowInstance,
  Link,
  Model,
  PerspectiveProxy,
  Property,
  SHACLFlow,
} from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { baseUrl, wipePerspective } from "../../utils/utils.js";
import { TestComment, TestPost, TestReaction, TestTag, TestChannel } from "./models.js";

// ── producedByFlow models (section 9) ────────────────────────────────────────

@Model({ name: "SubProducedTask" })
class SubProducedTask extends Ad4mModel {
  @Flag({ through: "sub-produced://task_type", value: "sub-produced://task" })
  type = "sub-produced://task";

  @Property({ through: "sub-produced://title", required: true })
  title: string = "";
}

@Model({ name: "SubProducedDeliverable" })
class SubProducedDeliverable extends Ad4mModel {
  @Flag({ through: "sub-produced://deliverable_type", value: "sub-produced://deliverable" })
  type = "sub-produced://deliverable";

  @Property({ through: "sub-produced://deliverable_title", required: true })
  title: string = "";
}

/** Ready → Done, guard-free, n:1: the smallest flow that completes and
 *  commits outputs (as in tests/flow/flow-produced-by.test.ts). */
function producedFlow(): SHACLFlow {
  const flow = new SHACLFlow("SubProducedDelivery", "sub-produced://");
  flow.inputTypes = ["SubProducedTask"];
  flow.consensusRule = { n: 1 };
  flow.addState({ name: "Ready", value: 0 });
  flow.addState({ name: "Done", value: 1 });
  flow.addTransition({ actionName: "Finish", fromState: "Ready", toState: "Done", actions: [] });
  return flow;
}

const PRODUCED_FLOW_URI = "sub-produced://SubProducedDeliveryFlow";

/** Long enough for a write's subscription check (250 ms batch window plus
 *  the 200 ms check interval) to have run before the next step. */
const settle = () => new Promise((r) => setTimeout(r, 1000));

// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — Subscriptions (SPARQL)", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let apiPort: number;
  let adminCredential: string;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
      apiPort = shared.apiPort;
      adminCredential = shared.adminCredential;
    } else {
      const agent = await startAgent("model-subscriptions");
      ad4m = agent.client;
      apiPort = agent.apiPort;
      adminCredential = agent.adminCredential;
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
    // Initial results returned via Promise only — callback fires for subsequent updates
    await builder.subscribe((r) => all.push(r));

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

    expect(all.length).to.be.at.least(1);
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
    // Initial results returned via Promise — callback fires for subsequent updates only
    const initialResults = await builder.subscribe((r) => all.push(r));
    expect(initialResults.some((p: any) => p.id === postId)).to.be.true;

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

    expect(all.length).to.be.at.least(1);
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

  // ── 5b. Reconnect ─────────────────────────────────────────────────────────

  it("subscribe() keeps updating after the client's socket closes and reconnects", async () => {
    // A second client whose sockets the test can reach through the public
    // `webSocketImpl` option.
    const sockets: WebSocket[] = [];
    class TrackedWebSocket extends WebSocket {
      constructor(url: string) {
        super(url);
        sockets.push(this);
      }
    }
    const client = new Ad4mClient(baseUrl(apiPort), adminCredential, { webSocketImpl: TrackedWebSocket });
    const builder = TestPost.query((await client.perspective.byUUID(perspective.uuid))!);
    try {
      const all: TestPost[][] = [];
      await builder.subscribe((r) => all.push(r));
      expect(sockets).to.have.length(1);

      sockets[0].close();
      await waitUntil(
        () => sockets.length === 2 && sockets[1].readyState === WebSocket.OPEN,
        15_000,
        "client reconnects",
      );

      const post = await TestPost.create(perspective, { title: "After Reconnect", body: "" });
      await waitUntil(
        () => all.some((batch) => batch.some((p) => p.id === post.id)),
        15_000,
        "subscription re-fires after the reconnect",
      );
    } finally {
      builder.dispose();
      client.close();
    }
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
    // Initial results returned via Promise — callback fires for subsequent updates only
    const initialResults = await builder.subscribe((r) => all.push(r));
    const initialTagCount = (initialResults.find((p: any) => p.id === post.id)?.tags ?? []).length;

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
    // Initial results returned via Promise — callback fires for subsequent updates only
    const initialResults = await builder.subscribe((r) => all.push(r));
    expect(initialResults).to.have.length(0);

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

  it("parent-scoped paginateSubscribe() fires when a child is created via create(..., { parent })", async () => {
    const channel = await TestChannel.create(perspective, { name: "sub-chan-batch" });

    const pages: Array<{ results: TestPost[]; totalCount?: number }> = [];
    const builder = TestPost.query(perspective)
      .parent(channel.id, TestChannel, { field: "posts" })
      .order({ createdAt: "DESC" });

    const initialPage = await builder.paginateSubscribe(10, 1, (page) => pages.push(page));
    expect(initialPage.results).to.have.length(0);
    expect(initialPage.totalCount).to.equal(0);

    // Give the subscription a brief moment to finish wiring up before the batch commit.
    await new Promise((r) => setTimeout(r, 500));

    const post = await TestPost.create(
      perspective,
      { title: "Batch Child", body: "created via parent" },
      { parent: { model: TestChannel, id: channel.id, field: "posts" } },
    );

    await waitUntil(
      () => pages.some((page) => page.results.some((p) => p.id === post.id) && page.totalCount === 1),
      8000,
      "paginated parent subscription re-fires after child create() batch commit",
    );
    builder.dispose();

    const latest = pages.at(-1)!;
    expect(latest.results.some((p) => p.id === post.id)).to.be.true;
    expect(latest.totalCount).to.equal(1);
  });

  it("parent-scoped subscribe() fires when a child is unlinked from the parent", async () => {
    const channel = await TestChannel.create(perspective, { name: "sub-chan-2" });
    const post = await TestPost.create(perspective, { title: "Removable", body: "" });
    await channel.addPosts(post.id);

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective)
      .parent(channel.id, TestChannel, { field: "posts" });
    // Initial results returned via Promise — callback fires for subsequent updates only
    const initialResults = await builder.subscribe((r) => all.push(r));
    expect(initialResults.some((p: any) => p.id === post.id)).to.be.true;

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

    // Initial results returned via Promise — callback fires for subsequent updates only
    const countAfterInitial = all.length; // should be 0 since no initial callback

    // Add a post to channel B — should not trigger channel A's subscription
    const post = await TestPost.create(perspective, { title: "Wrong Chan", body: "" });
    await channelB.addPosts(post.id);

    await new Promise((r) => setTimeout(r, 1000));
    builder.dispose();

    expect(all.length).to.equal(countAfterInitial);
  });

  // ── 8. writes on related records (#1237) ──────────────────────────────────
  //
  // A subscription re-runs on writes to the records its query reads beyond
  // its own class: included records, and the targets a typed relation lists.

  it("subscribe() with include re-fires when an included record is edited", async () => {
    // `test://emoji` is TestReaction's alone. (`test://body` would not do:
    // TestPost declares it too, so any body edit re-ran TestPost queries.)
    await TestReaction.register(perspective);
    const comment = await TestComment.create(perspective, { body: "c" });
    const reaction = await TestReaction.create(perspective, { emoji: "👍" });
    await comment.addReactions(reaction.id);
    // Settle, so the edit is checked in a batch of its own (see the next test).
    await new Promise((r) => setTimeout(r, 1000));

    const all: TestComment[][] = [];
    const builder = TestComment.query(perspective).include({ reactions: true });
    const initial = await builder.subscribe((r) => all.push(r));
    expect(initial[0].reactions.map((r) => r.emoji)).to.deep.equal(["👍"]);
    await new Promise((r) => setTimeout(r, 1000));

    await TestReaction.update(perspective, reaction.id, { emoji: "🎉" });

    await waitUntil(
      () => all.at(-1)?.[0]?.reactions?.[0]?.emoji === "🎉",
      8000,
      "subscription re-fires with the included reaction's new emoji",
    );
    builder.dispose();
  });

  it("subscribe() without include re-fires when a related record starts to conform", async () => {
    const post = await TestPost.create(perspective, { title: "Late comment", body: "" });
    // Flagged as a TestComment, but without the `body` TestComment requires,
    // so `post.comments` leaves it out.
    const late = "test://late-comment";
    await perspective.add(
      new Link({ source: late, predicate: "test://comment_type", target: "test://comment" }),
    );
    await post.addComments(late);
    // Let the relation link's subscription check pass before subscribing, and
    // the subscription settle before the write under test. Batched with the
    // `body` write, the relation link (on the post, which is in the result)
    // would re-run the query on its own.
    await new Promise((r) => setTimeout(r, 1000));

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective);
    const initial = await builder.subscribe((r) => all.push(r));
    expect(initial.find((p) => p.id === post.id)!.comments).to.deep.equal([]);
    await new Promise((r) => setTimeout(r, 1000));

    await perspective.add(
      new Link({ source: late, predicate: "test://body", target: "literal:string:now" }),
    );

    await waitUntil(
      () => ((all.at(-1)?.find((p) => p.id === post.id)?.comments ?? []) as unknown[]).includes(late),
      8000,
      "subscription re-fires once the comment conforms",
    );
    builder.dispose();
  });

  // ── 9. the other trigger rules (#1237) ────────────────────────────────────
  //
  // One test per way a write reaches a subscription in `subscription_triggers.rs`
  // that section 8 does not exercise. Each sets up, settles, subscribes,
  // settles again, then makes the one write under test, so the write is checked
  // in a batch of its own and nothing else can re-run the query.
  //
  // These all assert a push arrives. The opposite ("this write does not re-run
  // the query") cannot be seen from a client: the executor only pushes a
  // re-run whose result changed, so an extra re-run of an unchanged result is
  // silent. The Rust tests count re-runs for that.

  it("subscribe() with where re-fires when an edit moves a record into the filter", async () => {
    // `scan`: the post is in no result, so only its own flag says the write
    // concerns this class.
    const post = await TestPost.create(perspective, { title: "draft", body: "" });
    await settle();

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective).where({ title: "published" });
    const initial = await builder.subscribe((r) => all.push(r));
    expect(initial).to.deep.equal([]);
    await settle();

    await TestPost.update(perspective, post.id, { title: "published" });

    await waitUntil(
      () => (all.at(-1) ?? []).some((p) => p.id === post.id),
      8000,
      "subscription re-fires once the post matches the filter",
    );
    builder.dispose();
  });

  it("subscribe() with a `some` relation filter re-fires when an edit to a related record makes it hold", async () => {
    // A `where` quantifier reads the related class: its records are scanned
    // like the subscribed class's. The post is not in the result, so neither
    // it nor the comment is a result node.
    const post = await TestPost.create(perspective, { title: "Quantified", body: "" });
    const comment = await TestComment.create(perspective, { body: "maybe" });
    await post.addComments(comment.id);
    await settle();

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective).where({ comments: { some: { body: "yes" } } } as any);
    const initial = await builder.subscribe((r) => all.push(r));
    expect(initial).to.deep.equal([]);
    await settle();

    await TestComment.update(perspective, comment.id, { body: "yes" });

    await waitUntil(
      () => (all.at(-1) ?? []).some((p) => p.id === post.id),
      8000,
      "subscription re-fires once a comment satisfies `some`",
    );
    builder.dispose();
  });

  it("subscribe() with a filtered include re-fires when an edit moves a related record into it", async () => {
    // `via`: the reaction is filtered out of the include, so it is in no
    // result. It is linked to the comment, which is.
    await TestReaction.register(perspective);
    const comment = await TestComment.create(perspective, { body: "c" });
    const reaction = await TestReaction.create(perspective, { emoji: "👍" });
    await comment.addReactions(reaction.id);
    await settle();

    const all: TestComment[][] = [];
    const builder = TestComment.query(perspective).include({ reactions: { where: { emoji: "🎉" } } });
    const initial = await builder.subscribe((r) => all.push(r));
    expect(initial.find((c) => c.id === comment.id)!.reactions).to.deep.equal([]);
    await settle();

    await TestReaction.update(perspective, reaction.id, { emoji: "🎉" });

    await waitUntil(
      () =>
        (all.at(-1)?.find((c) => c.id === comment.id)?.reactions ?? []).some(
          (r) => r.id === reaction.id && r.emoji === "🎉",
        ),
      8000,
      "subscription re-fires with the reaction that now passes the include's filter",
    );
    builder.dispose();
  });

  it("subscribe() with a nested include re-fires when a record two includes deep is edited", async () => {
    // Post → comments → reactions. The reaction is a result node, nested two
    // levels below the post.
    await TestReaction.register(perspective);
    const post = await TestPost.create(perspective, { title: "Deep", body: "" });
    const comment = await TestComment.create(perspective, { body: "c" });
    const reaction = await TestReaction.create(perspective, { emoji: "👍" });
    await comment.addReactions(reaction.id);
    await post.addComments(comment.id);
    await settle();

    const emojiOf = (posts: TestPost[] | undefined) =>
      (posts?.find((p) => p.id === post.id)?.comments?.[0] as TestComment | undefined)?.reactions?.[0]?.emoji;

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective).include({ comments: { include: { reactions: true } } });
    const initial = await builder.subscribe((r) => all.push(r));
    expect(emojiOf(initial)).to.equal("👍");
    await settle();

    await TestReaction.update(perspective, reaction.id, { emoji: "🎉" });

    await waitUntil(
      () => emojiOf(all.at(-1)) === "🎉",
      8000,
      "subscription re-fires with the nested reaction's new emoji",
    );
    builder.dispose();
  });

  it("subscribe() follows a has-one include to a newly linked record, and re-fires on an edit below it", async () => {
    // Re-pointing `pinnedComment` re-runs the query (a relation edit on a
    // result node). The new comment and its reaction were in no earlier
    // result: the edit to the reaction is only seen if the re-run refreshed
    // the nodes the trigger watches.
    await TestReaction.register(perspective);
    const post = await TestPost.create(perspective, { title: "Pinned", body: "" });
    const first = await TestComment.create(perspective, { body: "first" });
    const second = await TestComment.create(perspective, { body: "second" });
    const reaction = await TestReaction.create(perspective, { emoji: "👍" });
    await second.addReactions(reaction.id);
    await post.addPinnedComment(first.id);
    await settle();

    const pinnedOf = (posts: TestPost[] | undefined) =>
      posts?.find((p) => p.id === post.id)?.pinnedComment as TestComment | null | undefined;

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective).include({ pinnedComment: { include: { reactions: true } } });
    const initial = await builder.subscribe((r) => all.push(r));
    expect(pinnedOf(initial)?.id).to.equal(first.id);
    await settle();

    await post.setPinnedComment([second.id]);
    await waitUntil(
      () => pinnedOf(all.at(-1))?.id === second.id,
      8000,
      "subscription re-fires with the newly pinned comment",
    );
    expect(pinnedOf(all.at(-1))?.reactions?.[0]?.emoji).to.equal("👍");
    await settle();

    await TestReaction.update(perspective, reaction.id, { emoji: "🎉" });

    await waitUntil(
      () => pinnedOf(all.at(-1))?.reactions?.[0]?.emoji === "🎉",
      8000,
      "subscription re-fires with the new pinned comment's reaction edited",
    );
    builder.dispose();
  });

  it("subscribe() ordered by a related record's property re-fires when that property is edited", async () => {
    // A dotted order key reads the pinned comments without including them:
    // they are in no result. The `limit` is what makes the query sort by the
    // key (as in model-query.test.ts); both posts stay on the page. A record
    // off the page is #1389.
    const p1 = await TestPost.create(perspective, { title: "One", body: "" });
    const p2 = await TestPost.create(perspective, { title: "Two", body: "" });
    const alpha = await TestComment.create(perspective, { body: "Alpha" });
    const beta = await TestComment.create(perspective, { body: "Beta" });
    await p1.addPinnedComment(alpha.id);
    await p2.addPinnedComment(beta.id);
    await settle();

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective)
      .order({ "pinnedComment.body": "ASC" } as any)
      .limit(10);
    const initial = await builder.subscribe((r) => all.push(r));
    expect(initial.map((p) => p.id)).to.deep.equal([p1.id, p2.id]);
    await settle();

    await TestComment.update(perspective, alpha.id, { body: "Zulu" });

    await waitUntil(
      () => JSON.stringify(all.at(-1)?.map((p) => p.id)) === JSON.stringify([p2.id, p1.id]),
      8000,
      "subscription re-fires in the new order",
    );
    builder.dispose();
  });

  it("subscribe() with where.producedByFlow re-fires when a run's receipt is minted", async () => {
    // `producedByFlow` is decided by the flow's receipts, not by the
    // deliverable's links: the mint writes nothing on the deliverable. The
    // run has completed before subscribing, so the mint is the only write
    // that can admit the deliverable. A perspective of its own keeps the
    // flow out of the shared one.
    const handle = await ad4m.perspective.add("model-subscriptions-produced-by-flow");
    const p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    try {
      await SubProducedTask.register(p);
      await SubProducedDeliverable.register(p);
      await p.addFlow("SubProducedDelivery", producedFlow());
      const task = await SubProducedTask.create(p, { title: "Ship it" });
      const deliverable = await SubProducedDeliverable.create(p, { title: "the thing" });
      const run = await FlowInstance.start(p, "SubProducedDelivery", task.id);
      const outcome = await run.proposeTransition("Done", undefined, [
        { className: "SubProducedDeliverable", id: deliverable.id },
      ]);
      expect(outcome.derivedState).to.equal("Done");
      await settle();

      const all: SubProducedDeliverable[][] = [];
      const builder = SubProducedDeliverable.query(p).where({
        producedByFlow: { flow: PRODUCED_FLOW_URI },
      });
      const initial = await builder.subscribe((r) => all.push(r));
      expect(initial, "no receipt yet, so no valid output").to.deep.equal([]);
      await settle();

      await p.mintFlowReceipt(outcome.outcomes[0].instanceUri);

      await waitUntil(
        () => (all.at(-1) ?? []).some((d) => d.id === deliverable.id),
        8000,
        "subscription re-fires with the minted run's output",
      );
      builder.dispose();
      expect(all.at(-1)!.map((d) => d.id)).to.deep.equal([deliverable.id]);
    } finally {
      await ad4m.perspective.remove(handle.uuid);
    }
  });
});
