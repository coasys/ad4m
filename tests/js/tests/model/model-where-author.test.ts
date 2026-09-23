/**
 * Ad4mModel — `where: { author }` beside a property condition is per link (#1114)
 *
 * The invariant: when a `where` names an `author` and a property, the link
 * that carries the property value must have been written by that author. It
 * is not enough that the instance's earliest link was.
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

describe("Ad4mModel — where author is a per-link condition", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;
  let me: string;

  /** Store a link as if `author` had written it, `ageMs` in the past. */
  async function linkAs(author: string, source: string, predicate: string, target: string, ageMs = 0) {
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

  it("does not accept a property link another agent wrote on my instance", async () => {
    const id = await roleInstanceBy(me);
    await linkAs(MALLORY, id, "test://role_agent", Literal.from(MALLORY).toUrl());

    // Control: the link is there, and the instance JSON is unchanged.
    const all = await TestRoleGrant.findAll(perspective, { where: { agent: MALLORY } });
    expect(all).to.have.length(1);
    expect(all[0].author).to.equal(me, "hydrated author is still the earliest link's");

    const granted = await TestRoleGrant.findAll(perspective, { where: { agent: MALLORY, author: me } });
    expect(granted, "I never appointed Mallory").to.have.length(0);
    expect(await TestRoleGrant.count(perspective, { where: { agent: MALLORY, author: me } })).to.equal(0);
    const paged = await TestRoleGrant.findAllAndCount(perspective, {
      where: { agent: MALLORY, author: me },
      limit: 10,
    });
    expect(paged.results).to.have.length(0);
    expect(paged.totalCount).to.equal(0);
  });

  it("matches the author of the property link, not the instance's creator", async () => {
    // Mallory created the instance; I wrote the agent link.
    const id = await roleInstanceBy(MALLORY);
    await perspective.add({ source: id, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });

    const mine = await TestRoleGrant.findAll(perspective, { where: { agent: BOB, author: me } });
    expect(mine.map((r) => r.id)).to.deep.equal([id]);
    expect(mine[0].author).to.equal(MALLORY, "hydrated author is still the earliest link's");

    const hers = await TestRoleGrant.findAll(perspective, { where: { agent: BOB, author: MALLORY } });
    expect(hers, "Mallory created the instance but did not write the agent link").to.have.length(0);
  });

  it("scopes the enclosing condition from OR branches, and supports the array and not forms", async () => {
    const id = await roleInstanceBy(me);
    await linkAs(MALLORY, id, "test://role_agent", Literal.from(MALLORY).toUrl());

    const viaOr = await TestRoleGrant.findAll(perspective, {
      where: { agent: MALLORY, OR: [{ author: me }, { author: BOB }] },
    });
    expect(viaOr).to.have.length(0);

    const viaIn = await TestRoleGrant.findAll(perspective, { where: { agent: MALLORY, author: [me, BOB] } });
    expect(viaIn).to.have.length(0);

    const notMine = await TestRoleGrant.findAll(perspective, { where: { agent: MALLORY, author: { not: me } } });
    expect(notMine.map((r) => r.id)).to.deep.equal([id]);
  });

  it("scopes every property condition in the clause", async () => {
    const id = await roleInstanceBy(me);
    await perspective.add({ source: id, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });
    await linkAs(MALLORY, id, "test://role_note", Literal.from("trusted").toUrl());

    expect(await TestRoleGrant.findAll(perspective, { where: { agent: BOB, author: me } })).to.have.length(1);
    expect(
      await TestRoleGrant.findAll(perspective, { where: { agent: BOB, note: "trusted", author: me } }),
      "I did not write the note",
    ).to.have.length(0);
  });

  it("keeps the instance-level meaning of a bare author condition", async () => {
    const id = await roleInstanceBy(me);
    await linkAs(MALLORY, id, "test://role_agent", Literal.from(MALLORY).toUrl());

    const byMe = await TestRoleGrant.findAll(perspective, { where: { author: me } });
    expect(byMe.map((r) => r.id)).to.deep.equal([id]);
    expect(await TestRoleGrant.findAll(perspective, { where: { author: MALLORY } })).to.have.length(0);
  });

  it("refuses a per-link author the store cannot answer", async () => {
    const id = await roleInstanceBy(me);
    await perspective.add({ source: id, predicate: "test://role_agent", target: Literal.from(BOB).toUrl() });

    let error: unknown = null;
    try {
      await TestRoleGrant.findAll(perspective, {
        where: { agent: BOB, author: me, timestamp: { gt: 0 } },
      });
    } catch (e) {
      error = e;
    }
    expect(error, "author + timestamp beside a property must be refused, not answered").to.not.equal(null);
    expect(String(error)).to.contain("author");
  });
});
