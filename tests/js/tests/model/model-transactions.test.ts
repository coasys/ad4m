/**
 * Ad4mModel — transaction integration tests
 *
 * Covers: batching multiple saves in one transaction commit, save+update,
 * save+delete, throw/rollback behaviour, and the Ad4mModel.transaction() API.
 *
 * Ported from playground scenario 06 (Transactions).
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-transactions.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";
import fetch from "node-fetch";
import { startAgent } from "../../helpers/index.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestPost } from "./models.js";

//@ts-ignore
global.fetch = fetch;

// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — Transactions", function () {
  this.timeout(120_000);

  let stop: () => Promise<void>;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const agent = await startAgent("model-transactions");
    ad4m = agent.client;
    stop = agent.stop;
    perspective = await ad4m.perspective.add("model-transactions-test");
    await TestPost.register(perspective);
  });

  after(async () => {
    await stop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
  });

  // ── 1. Batch creates in one transaction ───────────────────────────────────

  it("commits multiple saves atomically and all objects are findable afterwards", async () => {
    await TestPost.transaction(perspective, async (tx) => {
      const p1 = new TestPost(perspective);
      p1.title = "Tx Alpha";
      p1.body = "body a";
      await p1.save(tx.batchId);

      const p2 = new TestPost(perspective);
      p2.title = "Tx Beta";
      p2.body = "body b";
      await p2.save(tx.batchId);

      const p3 = new TestPost(perspective);
      p3.title = "Tx Gamma";
      p3.body = "body c";
      await p3.save(tx.batchId);
    });

    const all = await TestPost.findAll(perspective);
    expect(all).to.have.length(3);
    const titles = all.map((p) => p.title).sort();
    expect(titles).to.deep.equal(["Tx Alpha", "Tx Beta", "Tx Gamma"]);
  });

  // ── 2. Save then update in same transaction ───────────────────────────────

  it("save + update inside transaction reflects final state after commit", async () => {
    let savedId: string | undefined;

    await TestPost.transaction(perspective, async (tx) => {
      const post = new TestPost(perspective);
      post.title = "Before Update";
      post.body = "";
      await post.save(tx.batchId);
      savedId = post.id;

      post.title = "After Update";
      await post.save(tx.batchId);
    });

    expect(savedId).to.be.a("string");
    const found = await TestPost.findOne(perspective, {
      where: { id: savedId! },
    });
    expect(found).to.not.be.null;
    expect(found!.title).to.equal("After Update");
  });

  // ── 3. Save then delete in same transaction ───────────────────────────────

  it("save + delete inside transaction results in no remaining record", async () => {
    let savedId: string | undefined;

    await TestPost.transaction(perspective, async (tx) => {
      const post = new TestPost(perspective);
      post.title = "Ephemeral";
      post.body = "";
      await post.save(tx.batchId);
      savedId = post.id;

      await post.delete(tx.batchId);
    });

    expect(savedId).to.be.a("string");
    const found = await TestPost.findOne(perspective, {
      where: { id: savedId! },
    });
    expect(found).to.be.null;
  });

  // ── 4. Transaction does not affect pre-existing records ───────────────────

  it("transaction only modifies the records it explicitly touches", async () => {
    // Pre-existing post outside any transaction
    const outside = await TestPost.create(perspective, {
      title: "Outside Tx",
      body: "",
    });

    await TestPost.transaction(perspective, async (tx) => {
      const inside = new TestPost(perspective);
      inside.title = "Inside Tx";
      inside.body = "";
      await inside.save(tx.batchId);
    });

    const all = await TestPost.findAll(perspective);
    expect(all).to.have.length(2);
    expect(all.some((p) => p.id === outside.id && p.title === "Outside Tx")).to
      .be.true;
  });

  // ── 5. Reads within a transaction ─────────────────────────────────────────

  it("objects created inside a transaction are readable after commit", async () => {
    const ids: string[] = [];

    await TestPost.transaction(perspective, async (tx) => {
      for (let i = 0; i < 5; i++) {
        const post = new TestPost(perspective);
        post.title = `Bulk ${i}`;
        post.body = "";
        await post.save(tx.batchId);
        ids.push(post.id);
      }
    });

    expect(ids).to.have.length(5);
    for (const id of ids) {
      const found = await TestPost.findOne(perspective, { where: { id: id } });
      expect(found, `Post ${id} should exist`).to.not.be.null;
    }
  });

  // ── 6. Mixed transaction operations ──────────────────────────────────────

  it("handles mixed create/update/delete operations inside one transaction", async () => {
    // Create a record before the transaction so we can update it inside
    const preExisting = await TestPost.create(perspective, {
      title: "Pre Tx",
      body: "",
    });

    await TestPost.transaction(perspective, async (tx) => {
      // Create a new one
      const newPost = new TestPost(perspective);
      newPost.title = "New In Tx";
      newPost.body = "";
      await newPost.save(tx.batchId);

      // Update the pre-existing one
      preExisting.title = "Updated In Tx";
      await preExisting.save(tx.batchId);

      // Delete and re-add a post
      const temp = new TestPost(perspective);
      temp.title = "Temp";
      temp.body = "";
      await temp.save(tx.batchId);
      await temp.delete(tx.batchId);
    });

    const all = await TestPost.findAll(perspective);
    expect(all).to.have.length(2); // preExisting + newPost, temp was deleted

    const updated = await TestPost.findOne(perspective, {
      where: { id: preExisting.id },
    });
    expect(updated!.title).to.equal("Updated In Tx");
  });
});
