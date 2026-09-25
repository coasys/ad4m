/**
 * Ad4mModel — `where` `author`: nested per link, side by side means both (#1114)
 *
 * The invariant: `{ agent: { eq: X, author: A } }` matches only when A wrote
 * the `agent -> X` link itself. It is not enough that A created the instance
 * (wrote its earliest link). A top-level `author` beside a property condition,
 * `{ agent: X, author: A }`, means both: A created the instance AND A wrote
 * the `agent -> X` link. Alone, `{ author: A }` is still the instance author.
 *
 * This is the role-gating case from #1046 §1. Admin creates a role instance,
 * then a peer adds `agent -> themselves`. `{ agent: peer, author: admin }` used
 * to match, because `author` was compared with the instance's hydrated
 * `author` field, which is the author of its earliest link.
 *
 * A peer's link is simulated with `addLinkExpression`, which stores a
 * LinkExpression under the author it claims, the same way a synced link
 * arrives from a neighbourhood.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit tests/model/model-where-author.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Ad4mModel, Flag, Literal, Model, PerspectiveProxy, Property } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";

@Model({ name: "TestRoleGrant" })
class TestRoleGrant extends Ad4mModel {
  @Flag({ through: "test://role_grant_type", value: "test://reviewer" })
  type = "test://reviewer";

  @Property({ through: "test://role_agent" })
  agent: string = "";

  @Property({ through: "test://role_note" })
  note: string = "";
}

const MALLORY = "did:key:zMalloryTheSyncedPeer";
const BOB = "did:key:zBobTheAppointee";

/** `linkAs` links carry a signature that does not verify, and model queries
 *  withhold those by default (#1113). This suite is about `author`, not
 *  signatures, so every query opts in. The default is tested in
 *  model-unverified-links.test.ts. */
const UNVERIFIED = { includeUnverified: true } as const;

describe("Ad4mModel — where author: nested per link, side by side both", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;
  let me: string;

  /** Store a link as if `author` had written it, `ageMs` in the past. The
   *  default is a second in the future, so a peer's link never ties with the
   *  earliest link of an instance created just before it: a tie for the
   *  earliest link makes the side-by-side form fail closed. */
  async function linkAs(author: string, source: string, predicate: string, target: string, ageMs = -1_000) {
    await perspective.addLinkExpression({
      author,
      timestamp: new Date(Date.now() - ageMs).toISOString(),
      data: { source, predicate, target },
      proof: { key: `${author}#key`, signature: "not-a-real-signature" },
    } as any);
  }

  /** A role instance created by `creator` (it writes the earliest link). */
  async function roleInstanceBy(creator: string): Promise<string> {
    const id = Literal.from(`role-instance-${Math.random()}`).toUrl();
    if (creator === me) {
      await perspective.add({ source: id, predicate: "test://role_grant_type", target: "test://reviewer" });
    } else {
      await linkAs(creator, id, "test://role_grant_type", "test://reviewer", 60_000);
    }
    return id;
  }

  /** Ids matching `where`, asserting findAll, count and the paged form agree. */
  async function idsFor(where: any): Promise<string[]> {
    const rows = await TestRoleGrant.findAll(perspective, { where, ...UNVERIFIED });
    const count = await TestRoleGrant.count(perspective, { where, ...UNVERIFIED });
    const paged = await TestRoleGrant.findAllAndCount(perspective, { where, limit: 10, ...UNVERIFIED });
    const ids = rows.map((r) => r.id).sort();
    expect(count, `count for ${JSON.stringify(where)}`).to.equal(ids.length);
    expect(paged.results.map((r) => r.id).sort(), `paged rows for ${JSON.stringify(where)}`).to.deep.equal(ids);
    expect(paged.totalCount, `paged total for ${JSON.stringify(where)}`).to.equal(ids.length);
    return ids;
  }

  async function refusal(where: any): Promise<string> {
    let error: unknown = null;
    try {
      await TestRoleGrant.findAll(perspective, { where, ...UNVERIFIED });
    } catch (e) {
      error = e;
    }
    expect(error, `${JSON.stringify(where)} must be refused, not answered`).to.not.equal(null);
    return String(error);
  }

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-where-author");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    me = (await ad4m.agent.me()).did;
    perspective = await ad4m.perspective.add("model-where-author-test");
    await TestRoleGrant.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await TestRoleGrant.register(perspective);
  });

  it("Mallory's own agent link on my instance matches neither the nested nor the side-by-side form", async () => {
    const id = await roleInstanceBy(me);
    await linkAs(MALLORY, id, "test://role_agent", Literal.from(MALLORY).toUrl());

    // Control: the link is there, and the instance JSON is unchanged.
    const all = await TestRoleGrant.findAll(perspective, { where: { agent: MALLORY }, ...UNVERIFIED });
    expect(all).to.have.length(1);
    expect(all[0].author).to.equal(me, "hydrated author is still the earliest link's");

    expect(await idsFor({ agent: { eq: MALLORY, author: me } }), "I never wrote agent -> Mallory").to.deep.equal([]);
    expect(await idsFor({ agent: MALLORY, author: me }), "side by side needs my agent link too").to.deep.equal([]);
    expect(await idsFor({ agent: { author: me } }), "I wrote no agent link at all").to.deep.equal([]);
    expect(await idsFor({ agent: { eq: MALLORY, author: MALLORY } })).to.deep.equal([id]);
  });

  it("the nested form asks who wrote the link, side by side also asks who created the instance", async () => {
    // Mallory created the instance; I wrote the agent link.
    const split = await roleInstanceBy(MALLORY);
    await perspective.add({ source: split, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });
    // I created this one and wrote its agent link.
    const single = await roleInstanceBy(me);
    await perspective.add({ source: single, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });

    expect(await idsFor({ agent: { eq: BOB, author: me } })).to.deep.equal([single, split].sort());
    expect(
      await idsFor({ agent: BOB, author: me }),
      "side by side: a single-author instance matches as before, the split one does not",
    ).to.deep.equal([single]);
    expect(await idsFor({ agent: BOB, author: MALLORY }), "Mallory wrote no agent link").to.deep.equal([]);
  });

  it("supports the array, not and eq-array author and value forms", async () => {
    const id = await roleInstanceBy(me);
    await linkAs(MALLORY, id, "test://role_agent", Literal.from(MALLORY).toUrl());

    expect(await idsFor({ agent: { eq: MALLORY, author: [me, BOB] } })).to.deep.equal([]);
    expect(await idsFor({ agent: { eq: MALLORY, author: { not: me } } })).to.deep.equal([id]);
    expect(await idsFor({ agent: { eq: [MALLORY, BOB], author: MALLORY } })).to.deep.equal([id]);
    expect(await idsFor({ agent: { eq: MALLORY } }), "`eq` alone is the bare value").to.deep.equal([id]);
  });

  it("NOT around a nested author differs from not inside it", async () => {
    // Both Mallory and I wrote agent -> Bob here; only Mallory did on `hers`.
    const both = await roleInstanceBy(me);
    await perspective.add({ source: both, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });
    await linkAs(MALLORY, both, "test://role_agent", Literal.from(BOB).toUrl());
    const hers = await roleInstanceBy(me);
    await linkAs(MALLORY, hers, "test://role_agent", Literal.from(BOB).toUrl());

    expect(
      await idsFor({ agent: BOB, NOT: { agent: { eq: BOB, author: me } } }),
      "I wrote no agent -> Bob link",
    ).to.deep.equal([hers]);
    expect(
      await idsFor({ agent: { eq: BOB, author: { not: me } } }),
      "someone other than me wrote an agent -> Bob link",
    ).to.deep.equal([both, hers].sort());
  });

  it("scopes every property beside a side-by-side author, but not inside a sub-clause", async () => {
    const id = await roleInstanceBy(me);
    await perspective.add({ source: id, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });
    await linkAs(MALLORY, id, "test://role_note", Literal.from("trusted").toUrl());

    expect(await idsFor({ agent: BOB, author: me })).to.deep.equal([id]);
    expect(await idsFor({ agent: BOB, note: "trusted", author: me }), "I did not write the note").to.deep.equal([]);
    expect(
      await idsFor({ agent: BOB, author: me, AND: [{ note: "trusted" }] }),
      "the AND's note is its own object's",
    ).to.deep.equal([id]);
  });

  it("keeps the instance-level meaning of a bare author condition", async () => {
    const id = await roleInstanceBy(me);
    await linkAs(MALLORY, id, "test://role_agent", Literal.from(MALLORY).toUrl());

    expect(await idsFor({ author: me })).to.deep.equal([id]);
    expect(await idsFor({ author: MALLORY })).to.deep.equal([]);
    expect(await idsFor({ author: { not: [MALLORY] } }), "a mute list is still instance-level").to.deep.equal([id]);
  });

  it("refuses a per-link author the store cannot answer, and an author with no link", async () => {
    const id = await roleInstanceBy(me);
    await perspective.add({ source: id, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });

    for (const where of [
      { agent: { eq: BOB, author: me }, timestamp: { gt: 0 } },
      { agent: BOB, author: me, timestamp: { gt: 0 } },
    ]) {
      expect(await refusal(where)).to.contain("per-link `author`");
    }
    expect(await refusal({ timestamp: { author: me } })).to.contain("not a property stored as a link");
  });
});
