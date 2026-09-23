/**
 * Bounded traversal — client ↔ executor round-trip.
 *
 * The Rust suite pins the query engine against a seeded store. This file
 * is the missing half: TypeScript `findAll({ parent: TraverseScope })`
 * through `prepareModelQueryParams` onto the live executor, including
 * camelCase `limitPerAnchor` (the field serde used to drop).
 *
 * Picked up by `pnpm run test-model` via the tests/model/*.test.ts glob.
 */

import { expect } from "chai";
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestComment, TestPost } from "./models.js";

describe("Ad4mModel — bounded traversal round-trip", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

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
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestPost.register(perspective);
    await TestComment.register(perspective);
  });

  async function thread() {
    const post = await TestPost.create(perspective, {
      title: "thread root",
      body: "",
    });
    const c1 = await TestComment.create(perspective, { body: "c1" });
    const c2 = await TestComment.create(perspective, { body: "c2" });
    const c3 = await TestComment.create(perspective, { body: "c3" });
    await post.addComments(c1);
    await post.addComments(c2);
    await post.addComments(c3);
    const r1 = await TestComment.create(perspective, { body: "r1" });
    await perspective.add(
      new Link({
        source: c1.id,
        predicate: "test://has_comment",
        target: r1.id,
      }),
    );
    return { post, c1, c2, c3, r1 };
  }

  it("findAll with a Traverse parent returns every child of the anchor", async () => {
    const { post, c1, c2, c3 } = await thread();
    const results = await TestComment.findAll(perspective, {
      parent: { ids: post.id, predicate: "test://has_comment" },
    });
    const ids = results.map((c) => c.id);
    expect(ids).to.have.members([c1.id, c2.id, c3.id]);
    expect(ids).to.not.include(post.id);
  });

  it("limitPerAnchor survives the TS camelCase wire and is applied by the executor", async () => {
    const { post } = await thread();
    const results = await TestComment.findAll(perspective, {
      parent: {
        ids: post.id,
        predicate: "test://has_comment",
        limitPerAnchor: 2,
      },
    });
    expect(results).to.have.length(2);
  });

  it("levels walks each depth with its own per-anchor cap", async () => {
    const post = await TestPost.create(perspective, {
      title: "levels root",
      body: "",
    });
    const c1 = await TestComment.create(perspective, { body: "c1" });
    const c2 = await TestComment.create(perspective, { body: "c2" });
    await post.addComments(c1);
    await post.addComments(c2);
    const r1 = await TestComment.create(perspective, { body: "r1" });
    await perspective.add(
      new Link({
        source: c1.id,
        predicate: "test://has_comment",
        target: r1.id,
      }),
    );
    const results = await TestComment.findAll(perspective, {
      parent: {
        ids: post.id,
        predicate: "test://has_comment",
        levels: [2, 1],
      },
    });
    const ids = results.map((c) => c.id);
    expect(ids).to.have.members([c1.id, c2.id, r1.id]);
  });
});
