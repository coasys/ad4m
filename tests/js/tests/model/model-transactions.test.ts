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
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";
import { TestPost } from "./models.js";


// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — Transactions", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-transactions");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-transactions-test");
    await TestPost.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestPost.register(perspective);
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

  it("delete inside transaction removes a pre-existing record", async () => {
    // Create the record BEFORE the transaction — then delete it inside one
    const post = await TestPost.create(perspective, {
      title: "Ephemeral",
      body: "",
    });
    const savedId = post.id;

    await TestPost.transaction(perspective, async (tx) => {
      await post.delete(tx.batchId);
    });

    const found = await TestPost.findOne(perspective, {
      where: { id: savedId },
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

  it("handles mixed create/update and pre-tx delete inside one transaction", async () => {
    // Create two records before the transaction
    const keep = await TestPost.create(perspective, {
      title: "Keep Me",
      body: "",
    });
    const remove = await TestPost.create(perspective, {
      title: "Remove Me",
      body: "",
    });

    await TestPost.transaction(perspective, async (tx) => {
      // Create a new one
      const newPost = new TestPost(perspective);
      newPost.title = "New In Tx";
      newPost.body = "";
      await newPost.save(tx.batchId);

      // Update the keep record
      keep.title = "Updated In Tx";
      await keep.save(tx.batchId);

      // Delete the remove record (pre-existing, not created in this batch)
      await remove.delete(tx.batchId);
    });

    const all = await TestPost.findAll(perspective);
    expect(all).to.have.length(2); // keep (updated) + newPost; remove was deleted

    const updated = await TestPost.findOne(perspective, {
      where: { id: keep.id },
    });
    expect(updated!.title).to.equal("Updated In Tx");

    const gone = await TestPost.findOne(perspective, {
      where: { id: remove.id },
    });
    expect(gone).to.be.null;
  });

  // ── 7. Rollback on throw ──────────────────────────────────────────────────

  it("transaction that throws is not committed — data remains unpersisted", async () => {
    let abortedId: string | undefined;

    let thrownErr: Error | undefined;
    try {
      await TestPost.transaction(perspective, async (tx) => {
        const post = new TestPost(perspective);
        post.title = "Should Not Persist";
        post.body = "";
        await post.save(tx.batchId);
        abortedId = post.id;
        throw new Error("intentional rollback");
      });
    } catch (err: any) {
      thrownErr = err;
    }

    // The transaction MUST re-throw the original error
    expect(thrownErr?.message).to.equal("intentional rollback");

    // commitBatch was never called — the node should not exist in the perspective
    expect(abortedId).to.be.a("string");
    const found = await TestPost.findOne(perspective, {
      where: { id: abortedId! },
    });
    expect(found).to.be.null;
  });
});
