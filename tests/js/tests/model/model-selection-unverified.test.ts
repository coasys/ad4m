/**
 * Ad4mModel — an unverified link does not select an instance (issue #1120)
 *
 * #1113 made `model_query` withhold links whose signature does not verify from
 * the values it returns. #1120 applies the same rule to what decides *which*
 * instances are returned: `where` (including a per-link `author`), the class's
 * flag, `count` / `totalCount` and the page behind `limit`.
 *
 * The role check the flow engine builds on is "admin wrote an `agent -> X`
 * link": `where: { agent: { eq: X, author: admin } }`. Each forged link here
 * claims this agent (the admin) as its author and carries the agent's
 * signature over a different target, which is what a peer without the key can
 * produce. `includeUnverified: true` must still see each one, so no case can
 * pass because the forged link never mattered.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-selection-unverified.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  Link,
  LinkExpression,
  Literal,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";

@Model({ name: "SelectionGrant" })
class SelectionGrant extends Ad4mModel {
  @Flag({ through: "sgt://type", value: "sgt://grant" })
  type = "sgt://grant";

  @Property({ through: "sgt://agent" })
  agent: string = "";

  @Property({ through: "sgt://name" })
  name: string = "";
}

describe("Ad4mModel — unverified links do not select instances", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;
  let admin: string;

  /** Write the agent's own signed link, then a copy re-targeted after signing. */
  const forge = async (source: string, predicate: string, signedTarget: string, target: string) => {
    const signed = await perspective.add(new Link({ source, predicate, target: signedTarget }));
    await perspective.addLinkExpression({
      author: signed.author,
      timestamp: signed.timestamp,
      data: { source, predicate, target },
      proof: { key: signed.proof.key, signature: signed.proof.signature },
    } as LinkExpression);
    return signed;
  };

  /** A grant with only its signed flag link, so `save()` writes no defaults. */
  const grant = async (id: string) => {
    await perspective.add(new Link({ source: id, predicate: "sgt://type", target: "sgt://grant" }));
    return id;
  };

  const ids = async (query: object) =>
    (await SelectionGrant.findAll(perspective, query)).map((g) => g.id).sort();

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-selection-unverified");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    admin = (await ad4m.agent.me()).did;
  });

  beforeEach(async () => {
    perspective = await ad4m.perspective.add(`model-selection-unverified-${Date.now()}`);
    await SelectionGrant.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  it("a forged agent link claiming admin does not satisfy a nested author", async () => {
    const forgedGrant = await grant("sgt://grant/forged");
    await forge(forgedGrant, "sgt://agent", Literal.from("carol").toUrl(), Literal.from("mallory").toUrl());
    const byMallory = { where: { agent: { eq: "mallory", author: admin } } };

    expect(await ids(byMallory)).to.deep.equal([]);
    expect(await ids({ ...byMallory, limit: 10 })).to.deep.equal([]);
    expect(await SelectionGrant.count(perspective, byMallory)).to.equal(0);

    expect(await ids({ ...byMallory, includeUnverified: true })).to.deep.equal([forgedGrant]);
    expect(await SelectionGrant.count(perspective, { ...byMallory, includeUnverified: true })).to.equal(1);

    // The signed value still selects.
    expect(await ids({ where: { agent: { eq: "carol", author: admin } } })).to.deep.equal([forgedGrant]);
  });

  it("a forged flag does not make a node an instance", async () => {
    const real = await grant("sgt://grant/real");
    const node = "sgt://node/retyped";
    await forge(node, "sgt://type", "sgt://other", "sgt://grant");

    expect(await ids({})).to.deep.equal([real]);
    expect(await SelectionGrant.count(perspective)).to.equal(1);
    expect(await ids({ includeUnverified: true })).to.deep.equal([node, real].sort());
    expect(await SelectionGrant.count(perspective, { includeUnverified: true })).to.equal(2);
  });

  it("a forged value does not take a slot on a page or in totalCount", async () => {
    // `c` is created first, so it sorts first by timestamp. Its signed name is
    // `y`; a forged copy says `x`.
    const c = await grant("sgt://grant/c");
    await forge(c, "sgt://name", Literal.from("y").toUrl(), Literal.from("x").toUrl());
    const a = await grant("sgt://grant/a");
    await perspective.add(new Link({ source: a, predicate: "sgt://name", target: Literal.from("x").toUrl() }));
    const b = await grant("sgt://grant/b");
    await perspective.add(new Link({ source: b, predicate: "sgt://name", target: Literal.from("x").toUrl() }));

    const page = (includeUnverified?: boolean) =>
      SelectionGrant.findAllAndCount(perspective, {
        where: { name: "x" },
        limit: 2,
        includeUnverified,
      });

    const first = await page();
    expect(first.results.map((g) => g.id)).to.deep.equal([a, b]);
    expect(first.totalCount).to.equal(2);

    const opted = await page(true);
    expect(opted.results.map((g) => g.id), "the opt-in selects c onto page 1").to.deep.equal([c, a]);
    expect(opted.totalCount).to.equal(3);
  });
});
