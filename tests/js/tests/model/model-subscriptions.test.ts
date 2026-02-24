/**
 * Ad4mModel — subscription integration tests
 *
 * Covers: subscribe() initial callback, link-added re-fire, link-removed re-fire,
 * unsubscribe() stopping callbacks, debounce batching, error handling,
 * and ModelQueryBuilder.subscribe() / countSubscribe() / paginateSubscribe().
 *
 * Ported from playground scenario 05 (Subscriptions) and the subscription
 * sections of sdna.test.ts.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-subscriptions.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestPost } from "./models.js";


// ── Helper ────────────────────────────────────────────────────────────────────

function waitForCallbacks<T>(
  targetCount: number,
  timeoutMs = 8000,
): { callback: (results: T[]) => void; done: Promise<T[][]>; all: T[][] } {
  const all: T[][] = [];
  let resolve: ((v: T[][]) => void) | null = null;
  let timer: ReturnType<typeof setTimeout> | null = null;

  const done = new Promise<T[][]>((res, rej) => {
    timer = setTimeout(
      () =>
        rej(
          new Error(
            `Timeout: expected ${targetCount} callbacks, got ${all.length}`,
          ),
        ),
      timeoutMs,
    );
    resolve = res;
  });

  const callback = (results: T[]) => {
    all.push(results);
    if (all.length >= targetCount && resolve) {
      if (timer) clearTimeout(timer);
      resolve(all);
    }
  };

  return { callback, done, all };
}

// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — Subscriptions", function () {
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
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestPost.register(perspective);
  });

  // ── 1. Immediate initial callback ─────────────────────────────────────────

  it("subscribe() calls callback immediately with initial results", async () => {
    const { callback, done } = waitForCallbacks<TestPost>(1);
    const sub = TestPost.subscribe(perspective, {}, callback);
    const [results] = await done;
    sub.unsubscribe();
    expect(Array.isArray(results)).to.be.true;
  });

  // ── 2. Re-fires on link-added ─────────────────────────────────────────────

  it("subscribe() calls callback again when a relevant link is added", async () => {
    const all: TestPost[][] = [];
    const sub = TestPost.subscribe(perspective, {}, (r) => all.push(r));

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
    sub.unsubscribe();

    expect(all.length).to.be.at.least(2);
    expect(all.some((batch) => batch.some((p) => p.id === post.id))).to.be.true;
  });

  // ── 3. Re-fires on link-removed ───────────────────────────────────────────

  it("subscribe() calls callback again when a relevant link is removed", async () => {
    const post = await TestPost.create(perspective, {
      title: "Will Be Deleted",
      body: "",
    });
    const postId = post.id;

    const all: TestPost[][] = [];
    const sub = TestPost.subscribe(perspective, {}, (r) => all.push(r));

    await waitUntil(
      () => all.some((batch) => batch.some((p) => p.id === postId)),
      8000,
      "initial callback contains the post",
    );

    await post.delete();

    await waitUntil(
      () => {
        const latest = all.at(-1);
        return latest !== undefined && !latest.some((p) => p.id === postId);
      },
      8000,
      "subscription fires without the deleted post",
    );
    sub.unsubscribe();

    expect(all.length).to.be.at.least(2);
    expect(all.at(-1)!.some((p) => p.id === postId)).to.be.false;
  });

  // ── 4. unsubscribe() stops further callbacks ──────────────────────────────

  it("unsubscribe() stops further callback invocations", async () => {
    let callCount = 0;
    const sub = TestPost.subscribe(perspective, {}, () => {
      callCount++;
    });

    // Wait for the immediate callback
    await new Promise((r) => setTimeout(r, 400));
    const countAfterInitial = callCount;
    sub.unsubscribe();

    const post = new TestPost(perspective);
    post.title = "Post After Unsub";
    post.body = "";
    await post.save();

    // Give it a moment to fire — it should not
    await new Promise((r) => setTimeout(r, 600));
    expect(callCount).to.equal(countAfterInitial);
  });

  // ── 5. debounce batches rapid changes ─────────────────────────────────────

  it("debounce option batches rapid successive link changes into fewer callbacks", async () => {
    let callCount = 0;
    const sub = TestPost.subscribe(perspective, { debounce: 400 }, () => {
      callCount++;
    });

    // Wait for the initial callback
    await new Promise((r) => setTimeout(r, 150));
    const countAfterInitial = callCount;

    // Fire 3 saves in rapid succession within the debounce window
    await Promise.all(
      Array.from({ length: 3 }, async (_, i) => {
        const p = new TestPost(perspective);
        p.title = `Rapid ${i}`;
        p.body = "";
        await p.save();
      }),
    );

    // After debounce settles, there should be fewer than 3 extra callbacks
    await new Promise((r) => setTimeout(r, 1000));
    sub.unsubscribe();

    const extraCallbacks = callCount - countAfterInitial;
    expect(extraCallbacks).to.be.lessThan(3);
  });

  // ── 6. ModelQueryBuilder.live() ──────────────────────────────────────────

  it("ModelQueryBuilder.live() delivers results and can be unsubscribed", async () => {
    const all: TestPost[][] = [];
    const sub = TestPost.query(perspective).live((r) => all.push(r));

    await waitUntil(
      () => all.length >= 1,
      6000,
      "initial callback from query builder",
    );
    expect(Array.isArray(all[0])).to.be.true;

    const post = new TestPost(perspective);
    post.title = "QB Sub Post";
    post.body = "";
    await post.save();

    await waitUntil(
      () => all.some((batch) => batch.some((p) => p.id === post.id)),
      8000,
      "new post appears via query builder live subscription",
    );

    sub.unsubscribe();

    const countAfter = all.length;
    await new Promise((r) => setTimeout(r, 400));
    expect(all.length).to.equal(countAfter);
  });

  // ── 7. ModelQueryBuilder.count() ─────────────────────────────────────────

  it("ModelQueryBuilder.count() returns correct count and changes after add", async () => {
    const initialCount = await TestPost.query(perspective).count();

    await TestPost.create(perspective, { title: "Count Post", body: "" });

    const afterCount = await TestPost.query(perspective).count();
    expect(afterCount).to.equal(initialCount + 1);
  });

  // ── 8. Error handling ─────────────────────────────────────────────────────

  it("subscribe() exposes lastError (null until failure)", async () => {
    const sub = TestPost.subscribe(perspective, {}, () => {});
    await new Promise((r) => setTimeout(r, 300));
    expect(sub.lastError).to.be.null;
    sub.unsubscribe();
  });
});
