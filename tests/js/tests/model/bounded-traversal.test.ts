/**
 * Bounded traversal — client ↔ executor round-trip.
 *
 * The Rust suite pins the query engine against a seeded store, and
 * `query-sparql.test.ts` pins what TypeScript emits. Both sit on one side of
 * the wire, which is how `limitPerAnchor` came to be dropped in transit by a
 * missing `rename_all` and stayed green in 300-odd tests: the field was written
 * on one side and read on the other, and nothing crossed.
 *
 * So this file's job is the crossing, not the semantics. Every case here is one
 * the Rust suite already proves — asked again through `findAll`, a real
 * executor and a real store, so that a field arriving under the wrong name, a
 * refusal that turns into an empty result, or a count that means something else
 * on the way back, fails here.
 *
 * Picked up by `pnpm run test-model` via the tests/model/*.test.ts glob.
 */

import { expect } from "chai";
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { TestComment, TestPost } from "./models.js";

const PREDICATE = "test://has_comment";

describe("Ad4mModel — bounded traversal round-trip", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  /**
   * One thread, reused by every case — they are all reads.
   *
   * ```text
   * post ─┬─ c1 ─┬─ r1 ─── rr1
   *       │      └─ r2
   *       ├─ c2 ─── r3
   *       └─ c3
   * ```
   *
   * Every node below `post` is a TestComment, linked to its parent by the same
   * `test://has_comment` that `TestPost.comments` declares — which is what lets
   * a comment be an anchor, and so what makes "the anchor is excluded" a claim
   * with something to exclude. Bodies sort in tree order, so `order: { body }`
   * is a deterministic ordering without depending on timestamp resolution.
   */
  let post: TestPost;
  const comment: Record<string, TestComment> = {};

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("bounded-traversal");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("bounded-traversal-test");
    await TestPost.register(perspective);
    await TestComment.register(perspective);

    post = await TestPost.create(perspective, { title: "thread root", body: "" });
    for (const body of ["c1", "c2", "c3", "r1", "r2", "r3", "rr1"]) {
      comment[body] = await TestComment.create(perspective, { body });
    }
    await post.addComments(comment.c1.id);
    await post.addComments(comment.c2.id);
    await post.addComments(comment.c3.id);
    for (const [parent, child] of [
      ["c1", "r1"],
      ["c1", "r2"],
      ["c2", "r3"],
      ["r1", "rr1"],
    ]) {
      await perspective.add(
        new Link({
          source: comment[parent].id,
          predicate: PREDICATE,
          target: comment[child].id,
        }),
      );
    }
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  /** Bodies rather than ids, so a failure reads as a tree rather than as UUIDs. */
  const bodies = (results: TestComment[]) => results.map((c) => c.body);

  const traverse = (parent: any) =>
    TestComment.findAll(perspective, { parent, order: { body: "ASC" } });

  // ── the anchors ───────────────────────────────────────────────────────────

  it("returns every child of a single anchor", async () => {
    expect(bodies(await traverse({ ids: post.id, predicate: PREDICATE }))).to.deep.equal([
      "c1",
      "c2",
      "c3",
    ]);
  });

  it("answers for several anchors in one query", async () => {
    // The list spelling of `ids`, which is the whole reason the scope exists —
    // and a different branch of the executor's deserializer from a bare string,
    // so a query that works for one anchor proves nothing about twenty.
    expect(
      bodies(await traverse({ ids: [comment.c1.id, comment.c2.id], predicate: PREDICATE })),
    ).to.deep.equal(["r1", "r2", "r3"]);
  });

  it("names its predicate through the model that declares it", async () => {
    // The same query as the first, written the way the other two scope forms
    // are written — without the caller repeating a predicate string.
    expect(
      bodies(await traverse({ ids: post.id, model: TestPost, field: "comments" })),
    ).to.deep.equal(["c1", "c2", "c3"]);
  });

  // ── limitPerAnchor ────────────────────────────────────────────────────────

  it("applies limitPerAnchor per anchor, not to the result as a whole", async () => {
    // Two anchors and a limit of one: a global limit would answer with r1
    // alone. This is the shape the field was silently dropped in — and with a
    // single anchor the two readings are indistinguishable, so the test that
    // catches a wrong one needs two.
    expect(
      bodies(
        await traverse({
          ids: [comment.c1.id, comment.c2.id],
          predicate: PREDICATE,
          limitPerAnchor: 1,
        }),
      ),
    ).to.deep.equal(["r1", "r3"]);
  });

  // ── direction ─────────────────────────────────────────────────────────────

  it("follows the predicate inward when asked", async () => {
    // "What points at these" as a query that can be ordered and limited, rather
    // than a reverse include over rows already in hand.
    expect(
      bodies(
        await traverse({
          ids: [comment.r1.id, comment.r2.id],
          predicate: PREDICATE,
          direction: "in",
        }),
      ),
    ).to.deep.equal(["c1"]);
  });

  // ── transitive ────────────────────────────────────────────────────────────

  it("reaches the whole subtree transitively", async () => {
    expect(
      bodies(await traverse({ ids: post.id, predicate: PREDICATE, transitive: true })),
    ).to.deep.equal(["c1", "c2", "c3", "r1", "r2", "r3", "rr1"]);
  });

  it("excludes every named anchor from a transitive read", async () => {
    // c1 is reachable from post and is also an anchor. An anchor is where the
    // read started, not something it found, so it appears at neither place.
    expect(
      bodies(
        await traverse({
          ids: [post.id, comment.c1.id],
          predicate: PREDICATE,
          transitive: true,
        }),
      ),
    ).to.deep.equal(["c2", "c3", "r1", "r2", "r3", "rr1"]);
  });

  it("counts the whole subtree in a transitive projection", async () => {
    // `{ count: true, transitive: true }` is "42 replies" on a collapsed
    // branch — the conversation, not the direct replies. It did not typecheck
    // against a model with typed fields until this PR, so it is worth asking
    // for it here in the spelling the docs use.
    const [direct] = (await TestPost.findAll(perspective, {
      where: { id: post.id },
      include: { $replies: { from: "comments", count: true } },
    })) as any[];
    const [whole] = (await TestPost.findAll(perspective, {
      where: { id: post.id },
      include: { $replies: { from: "comments", count: true, transitive: true } },
    })) as any[];

    expect(direct.$replies).to.equal(3);
    expect(whole.$replies).to.equal(7);
  });

  // ── levels ────────────────────────────────────────────────────────────────

  it("walks each depth with its own per-anchor breadth", async () => {
    // One per level, two levels: the first reply to the post, then the first
    // reply to that. `[2, 1]` would take c1 and c2 and then one reply each.
    expect(
      bodies(await traverse({ ids: post.id, predicate: PREDICATE, levels: [1, 1] })),
    ).to.deep.equal(["c1", "r1"]);
    expect(
      bodies(await traverse({ ids: post.id, predicate: PREDICATE, levels: [2, 1] })),
    ).to.deep.equal(["c1", "c2", "r1", "r3"]);
  });

  it("totals a walk by the walk, whether or not rows were asked for", async () => {
    // `count()` is `limit: 0`, which takes a different path through the
    // executor than `findAllAndCount`. Both are asked the same question here,
    // because a walk that reports two rows and a total of three is a client
    // paging for a row that does not exist.
    const parent = { ids: post.id, predicate: PREDICATE, levels: [1, 1] };
    const { results, totalCount } = await TestComment.findAllAndCount(perspective, {
      parent,
      order: { body: "ASC" },
    });
    const counted = await TestComment.count(perspective, { parent, order: { body: "ASC" } });

    expect(bodies(results as TestComment[])).to.deep.equal(["c1", "r1"]);
    expect(totalCount).to.equal(2);
    expect(counted).to.equal(2);
  });

  // ── refusals ──────────────────────────────────────────────────────────────

  it("surfaces a refused combination as an error rather than an answer", async () => {
    // The executor refuses `levels` + `transitive`. What matters on this side of
    // the wire is that the refusal arrives as one: an error swallowed into an
    // empty array reads as "no replies", which is a wrong answer wearing the
    // shape of a right one.
    let message: string | null = null;
    try {
      await traverse({
        ids: post.id,
        predicate: PREDICATE,
        levels: [2],
        transitive: true,
      });
    } catch (e: any) {
      message = String(e?.message ?? e);
    }
    expect(message, "the combination is refused, not silently resolved").to.be.a("string");
    expect(message!).to.contain("mutually exclusive");
  });

  it("refuses a per-anchor slice alongside a walk", async () => {
    let message: string | null = null;
    try {
      await traverse({
        ids: post.id,
        predicate: PREDICATE,
        levels: [2],
        limitPerAnchor: 5,
      });
    } catch (e: any) {
      message = String(e?.message ?? e);
    }
    expect(message, "the combination is refused, not silently resolved").to.be.a("string");
    expect(message!).to.contain("mutually exclusive");
  });
});
