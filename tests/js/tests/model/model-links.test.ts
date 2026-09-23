/**
 * Ad4mModel — the `links` query option (#1111, #1112)
 *
 * Invariant: `links: [...]` returns, under `instance.__links`, every stored
 * link behind each requested entry, each with its own author, timestamp and
 * proof, and it does so without changing anything else about the instance.
 *
 *   1. Annotation links: a predicate the model does not declare (a tombstone)
 *      is returned when asked for by IRI, and it does not move `updatedAt`.
 *   2. Per-link timestamps: collection members attached at different times are
 *      dated by their own links, not by the instance's `createdAt`. The rows
 *      are the stored `LinkExpression`s (same author, timestamp, signature).
 *   3. Fail-closed keys: a reverse relation or an unresolvable key is an
 *      error, not `[]`; "asked, none found" is `[]`, "not asked" is absent.
 *   4. The same option through the fluent builder, an `include` sub-query and
 *      the raw `perspective.modelQuery` RPC.
 *   5. A live subscription re-fires when a link it asks for lands, even on a
 *      predicate the model does not declare (a subscribed revocation check
 *      must not keep reporting `[]` after the tombstone is written).
 *   6. Per-item provenance for a collection (#1115): each member's row carries
 *      its author, timestamp and the stored signature verdict, so a member
 *      written under someone else's name with a signature that does not verify
 *      reads `valid: false` next to a genuine one.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-links.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Link,
  LinkQuery,
  PerspectiveProxy,
} from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective, sleep } from "../../utils/utils.js";
import { TestComment, TestPost, TestTag, TestReaction } from "./models.js";

/** Not declared by any test model: an annotation such as a revocation tombstone. */
const TOMBSTONE = "test://revoked";
const HAS_COMMENT = "test://has_comment";

/** The error an awaited call rejects with, or a failure if it resolves. */
async function rejection(p: Promise<unknown>): Promise<string> {
  try {
    await p;
  } catch (e: any) {
    return String(e?.message ?? e);
  }
  expect.fail("expected the query to be rejected");
}

describe("Ad4mModel — links option (per-link rows)", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;
  let me: string;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-links");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    me = (await ad4m.agent.me()).did;
    perspective = await ad4m.perspective.add("model-links-test");
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
  });

  // ── 1. annotation links on an undeclared predicate ───────────────────────

  it("returns a link on a predicate the model does not declare, asked for by IRI", async () => {
    const post = await TestPost.create(perspective, { title: "revocable" });
    const stored = await perspective.add(
      new Link({ source: post.id, predicate: TOMBSTONE, target: "literal://string:gone" }),
    );

    const [found] = await TestPost.findAll(perspective, {
      where: { id: post.id },
      links: [TOMBSTONE],
    });

    const rows = found.__links![TOMBSTONE];
    expect(rows).to.have.length(1);
    expect(rows[0].data).to.deep.equal({
      source: post.id,
      predicate: TOMBSTONE,
      target: "literal://string:gone",
    });
    expect(rows[0].author).to.equal(me);
    expect(rows[0].author).to.equal(stored.author);
    expect(new Date(rows[0].timestamp).getTime()).to.equal(
      new Date(stored.timestamp).getTime(),
    );
    expect(rows[0].proof.signature).to.equal(stored.proof.signature);
    expect(rows[0].proof.key).to.equal(stored.proof.key);
  });

  it("is additive: the tombstone does not move updatedAt or any property", async () => {
    const post = await TestPost.create(perspective, { title: "stable", body: "b" });
    await sleep(20);
    await perspective.add(
      new Link({ source: post.id, predicate: TOMBSTONE, target: "literal://string:x" }),
    );

    const [plain] = await TestPost.findAll(perspective, { where: { id: post.id } });
    const [withLinks] = await TestPost.findAll(perspective, {
      where: { id: post.id },
      links: [TOMBSTONE],
    });

    expect(plain.__links).to.be.undefined;
    expect(withLinks.__links![TOMBSTONE]).to.have.length(1);
    const tombstoneAt = new Date(withLinks.__links![TOMBSTONE][0].timestamp).getTime();
    expect(withLinks.updatedAt).to.equal(plain.updatedAt);
    expect(withLinks.updatedAt).to.be.below(tombstoneAt);
    expect(withLinks.createdAt).to.equal(plain.createdAt);
    expect(withLinks.title).to.equal("stable");
    expect(withLinks.body).to.equal("b");
  });

  // ── 2. per-link timestamps ────────────────────────────────────────────────

  it("dates each collection member by its own link, not by the instance", async () => {
    const post = await TestPost.create(perspective, { title: "dated" });
    const c1 = await TestComment.create(perspective, { body: "early" });
    const c2 = await TestComment.create(perspective, { body: "late" });
    await sleep(20);
    await post.addComments(c1.id);
    await sleep(20);
    await post.addComments(c2.id);

    const [found] = await TestPost.findAll(perspective, {
      where: { id: post.id },
      links: ["comments"],
    });

    const rows = found.__links!.comments;
    expect(rows.map((r) => r.data.target)).to.deep.equal([c1.id, c2.id]);
    const [t1, t2] = rows.map((r) => new Date(r.timestamp).getTime());
    expect(t1).to.be.above(found.createdAt);
    expect(t2).to.be.above(t1);
    for (const r of rows) {
      expect(r.author).to.equal(me);
      expect(r.data.source).to.equal(post.id);
      expect(r.data.predicate).to.equal(HAS_COMMENT);
    }

    // The rows are the stored LinkExpressions, not reconstructions of them.
    const stored = await perspective.get(
      new LinkQuery({ source: post.id, predicate: HAS_COMMENT }),
    );
    expect(stored).to.have.length(2);
    for (const s of stored) {
      const row = rows.find((r) => r.data.target === s.data.target)!;
      expect(row, `row for ${s.data.target}`).to.exist;
      expect(row.author).to.equal(s.author);
      expect(new Date(row.timestamp).getTime()).to.equal(new Date(s.timestamp).getTime());
      expect(row.proof.signature).to.equal(s.proof.signature);
    }
  });

  it("gives every instance every requested key, as [] when it has no such link", async () => {
    const withComment = await TestPost.create(perspective, { title: "a" });
    const without = await TestPost.create(perspective, { title: "b" });
    const c = await TestComment.create(perspective, { body: "c" });
    await withComment.addComments(c.id);

    const posts = await TestPost.findAll(perspective, { links: ["comments", TOMBSTONE] });
    const byId = new Map(posts.map((p) => [p.id, p]));

    expect(byId.get(withComment.id)!.__links!.comments).to.have.length(1);
    expect(byId.get(withComment.id)!.__links![TOMBSTONE]).to.deep.equal([]);
    expect(byId.get(without.id)!.__links).to.deep.equal({ comments: [], [TOMBSTONE]: [] });
  });

  // ── 3. keys that cannot be answered are errors ────────────────────────────

  it("rejects a reverse relation (@BelongsToOne) instead of answering []", async () => {
    const post = await TestPost.create(perspective, { title: "parent" });
    const c = await TestComment.create(perspective, { body: "child" });
    await post.addComments(c.id);

    // The incoming link exists and hydrates the relation...
    const [hydrated] = await TestComment.findAll(perspective, {
      where: { id: c.id },
      include: { post: true },
    });
    expect(hydrated.post).to.be.instanceOf(TestPost);
    expect(hydrated.post!.id).to.equal(post.id);

    // ...but `links` reads outgoing links only, so asking for it is an error.
    const message = await rejection(
      TestComment.findAll(perspective, { where: { id: c.id }, links: ["post"] }),
    );
    expect(message).to.contain("post");
    expect(message).to.contain("reverse relation");
  });

  it("rejects a key that is neither a declared name nor an IRI", async () => {
    await TestPost.create(perspective, { title: "x" });
    const message = await rejection(
      TestPost.findAll(perspective, { links: ["role_grant_revokd"] }),
    );
    expect(message).to.contain("role_grant_revokd");
  });

  // ── 4. other entry points ─────────────────────────────────────────────────

  it("works through the fluent builder", async () => {
    const post = await TestPost.create(perspective, { title: "fluent" });
    const tag = await TestTag.create(perspective, { label: "t" });
    await post.addTags(tag.id);

    const [found] = await TestPost.query(perspective)
      .where({ id: post.id } as any)
      .links(["tags"])
      .get();
    expect(found.__links!.tags.map((r) => r.data.target)).to.deep.equal([tag.id]);
  });

  it("works inside an include sub-query, and only there", async () => {
    const post = await TestPost.create(perspective, { title: "outer" });
    const c = await TestComment.create(perspective, { body: "inner" });
    await post.addComments(c.id);
    await perspective.add(
      new Link({ source: c.id, predicate: TOMBSTONE, target: "literal://string:hidden" }),
    );

    const [found] = await TestPost.findAll(perspective, {
      where: { id: post.id },
      include: { comments: { links: [TOMBSTONE] } },
    });

    expect(found.__links).to.be.undefined;
    expect(found.comments).to.have.length(1);
    const inner = found.comments[0];
    expect(inner.__links![TOMBSTONE].map((r) => r.data.target)).to.deep.equal([
      "literal://string:hidden",
    ]);
  });

  // ── 6. per-item provenance: author, timestamp and verdict (#1115) ───────

  it("gives each collection member its own author, timestamp and signature verdict", async () => {
    const post = await TestPost.create(perspective, { title: "provenance" });
    const mine = await TestComment.create(perspective, { body: "signed by me" });
    const theirs = await TestComment.create(perspective, { body: "claimed by someone else" });
    await post.addComments(mine.id);
    await sleep(20);

    // A member link under another agent's name whose signature is not theirs:
    // stored as given, verdict computed from the signature at insert time.
    const forgedAuthor = "did:key:z6MkForgedAuthorForgedAuthorForgedAuthorForged";
    const forgedAt = new Date().toISOString();
    await perspective.addLinkExpression({
      author: forgedAuthor,
      timestamp: forgedAt,
      data: { source: post.id, predicate: HAS_COMMENT, target: theirs.id },
      proof: { key: `${forgedAuthor}#key`, signature: "00".repeat(64) },
    } as any);

    const [found] = await TestPost.findAll(perspective, {
      where: { id: post.id },
      links: ["comments"],
    });

    // The plain array stays as it is.
    expect([...found.comments].sort()).to.deep.equal([mine.id, theirs.id].sort());

    const rows = found.__links!.comments;
    expect(rows.map((r) => r.data.target)).to.deep.equal([mine.id, theirs.id]);
    const [own, forged] = rows;

    expect(own.author).to.equal(me);
    expect(own.proof.valid).to.equal(true);
    expect(own.proof.invalid).to.equal(false);

    expect(forged.author).to.equal(forgedAuthor);
    expect(new Date(forged.timestamp).getTime()).to.equal(new Date(forgedAt).getTime());
    expect(new Date(forged.timestamp).getTime()).to.be.above(
      new Date(own.timestamp).getTime(),
    );
    expect(forged.proof.valid).to.equal(false);
    expect(forged.proof.invalid).to.equal(true);
    // A failed signature is told apart from an unsigned link by its non-empty
    // signature, since `invalid` alone covers both.
    expect(forged.proof.signature).to.equal("00".repeat(64));

    // Same verdict perspective.get() reports for the stored link.
    const stored = await perspective.get(
      new LinkQuery({ source: post.id, predicate: HAS_COMMENT }),
    );
    for (const s of stored) {
      const row = rows.find((r) => r.data.target === s.data.target)!;
      expect(!!row.proof.valid, `verdict for ${s.data.target}`).to.equal(!!s.proof.valid);
    }
  });

  it("is available on the raw perspective.modelQuery RPC", async () => {
    const post = await TestPost.create(perspective, { title: "raw" });
    await perspective.add(
      new Link({ source: post.id, predicate: TOMBSTONE, target: "literal://string:raw" }),
    );

    const result = await perspective.modelQuery(
      "TestPost",
      JSON.stringify({ links: [TOMBSTONE, "title"] }),
    );
    const inst = result.instances.find((i: any) => i.id === post.id);
    expect(inst.__links[TOMBSTONE]).to.have.length(1);
    expect(inst.__links.title).to.have.length(1);
    expect(inst.__links.title[0].data.predicate).to.equal("test://title");
  });

  // ── 5. subscriptions re-fire on a requested link ──────────────────────────

  it("subscribe() re-fires when a link on an undeclared links IRI is added", async () => {
    const post = await TestPost.create(perspective, { title: "watched" });
    // Let the executor's 250 ms subscription batch drain first. Otherwise the
    // setup links above land in the same batch as the tombstone and re-run the
    // subscription on their own, which would hide a missing trigger predicate.
    await sleep(1000);

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective)
      .where({ id: post.id } as any)
      .links([TOMBSTONE]);
    const initial = await builder.subscribe((r) => all.push(r));
    try {
      expect(initial).to.have.length(1);
      expect(initial[0].__links![TOMBSTONE]).to.deep.equal([]);

      await perspective.add(
        new Link({ source: post.id, predicate: TOMBSTONE, target: "literal://string:revoked" }),
      );

      await waitUntil(
        () => all.some((batch) => batch[0]?.__links?.[TOMBSTONE]?.length === 1),
        15_000,
        "subscription re-fires with the tombstone row",
      );
      const row = all.find((b) => b[0]?.__links?.[TOMBSTONE]?.length === 1)![0].__links![TOMBSTONE][0];
      expect(row.data).to.deep.equal({
        source: post.id,
        predicate: TOMBSTONE,
        target: "literal://string:revoked",
      });
    } finally {
      builder.dispose();
    }
  });

  it("subscribe() re-fires when a link on a links IRI inside an include lands", async () => {
    const post = await TestPost.create(perspective, { title: "outer" });
    const c = await TestComment.create(perspective, { body: "inner" });
    await post.addComments(c.id);
    // Let the executor's 250 ms subscription batch drain first. Otherwise the
    // setup links above land in the same batch as the tombstone and re-run the
    // subscription on their own, which would hide a missing trigger predicate.
    await sleep(1000);

    const all: TestPost[][] = [];
    const builder = TestPost.query(perspective)
      .where({ id: post.id } as any)
      .include({ comments: { links: [TOMBSTONE] } } as any);
    const initial = await builder.subscribe((r) => all.push(r));
    try {
      expect(initial[0].comments[0].__links![TOMBSTONE]).to.deep.equal([]);

      await perspective.add(
        new Link({ source: c.id, predicate: TOMBSTONE, target: "literal://string:hidden" }),
      );

      await waitUntil(
        () =>
          all.some(
            (batch) => (batch[0]?.comments?.[0] as any)?.__links?.[TOMBSTONE]?.length === 1,
          ),
        15_000,
        "subscription re-fires with the included tombstone row",
      );
    } finally {
      builder.dispose();
    }
  });
});
