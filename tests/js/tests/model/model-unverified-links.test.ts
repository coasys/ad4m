/**
 * Ad4mModel — links whose signature does not verify are withheld (issue #1113)
 *
 * The store records a signature verdict for every link, computed from the
 * signature when the link is inserted. `model_query` now honours it by
 * default: a link whose signature does not verify does not hydrate. The only
 * way to see it is the explicit `includeUnverified` opt-in.
 *
 * The forged link here is the one a peer without the author's key can produce:
 * a real signed link with its target swapped, stamped *later* than the
 * original. Under last-write-wins that later value would become the property,
 * so it is the case the default exists for. Everything is driven through the
 * public model API (`findAll`, `findOne`, the query builder) that a client
 * uses.
 *
 * The same default covers the rows under `__links` and the order behind
 * `limit`: a forged link must not reorder a page.
 *
 * Not covered here, and still open: which instances are *selected* (`where`,
 * the class's flags, `count`) still matches unverified links. See the
 * `#[ignore]`d Rust test `proof_valid_where_does_not_select_on_a_forged_value`.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-unverified-links.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  Link,
  LinkExpression,
  LinkQuery,
  Literal,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";

@Model({ name: "UnverifiedRecipe" })
class UnverifiedRecipe extends Ad4mModel {
  @Flag({ through: "uvr://type", value: "uvr://recipe" })
  type = "uvr://recipe";

  /** Gets a forged later value. */
  @Property({ through: "uvr://name" })
  name: string = "";

  /** Only ever signed: the control, present both ways. */
  @Property({ through: "uvr://cuisine" })
  cuisine: string = "";
}

describe("Ad4mModel — unverified links are withheld by default", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;
  let recipeId: string;
  let realUpdatedAt: unknown;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-unverified-links");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-unverified-links-test");
    await UnverifiedRecipe.register(perspective);

    const recipe = new UnverifiedRecipe(perspective);
    recipe.name = "real";
    recipe.cuisine = "thai";
    await recipe.save();
    recipeId = recipe.id;

    const before = await UnverifiedRecipe.findOne(perspective, { where: { id: recipeId } });
    realUpdatedAt = (before as any)?.updatedAt;

    // The agent's own signed `name` link, with its target swapped after
    // signing and its timestamp moved later. The signature no longer covers
    // the data, so the executor stores it as unverified.
    const [signed] = await perspective.get(
      new LinkQuery({ source: recipeId, predicate: "uvr://name" })
    );
    expect(signed, "the signed name link").to.exist;
    const forged = {
      author: signed.author,
      timestamp: new Date(Date.parse(signed.timestamp) + 60_000).toISOString(),
      data: {
        source: signed.data.source,
        predicate: signed.data.predicate,
        target: Literal.from("forged").toUrl(),
      },
      proof: { key: signed.proof.key, signature: signed.proof.signature },
    } as LinkExpression;
    await perspective.addLinkExpression(forged);

    // Precondition: the forged link is in the store, and marked invalid.
    const nameLinks = await perspective.get(
      new LinkQuery({ source: recipeId, predicate: "uvr://name" })
    );
    const stored = nameLinks.find((l) => l.data.target.includes("forged"));
    expect(stored, "the forged link must be stored").to.exist;
    expect(stored!.proof.valid, "the forged link must not verify").to.not.equal(true);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  it("does not hydrate a forged later value by default", async () => {
    const all = await UnverifiedRecipe.findAll(perspective);
    const recipe = all.find((r) => r.id === recipeId);
    expect(recipe, "the recipe is still returned").to.exist;
    expect(recipe!.name).to.equal("real");
    expect(recipe!.cuisine).to.equal("thai");
    expect((recipe as any).updatedAt, "the forged link must not move updatedAt").to.deep.equal(
      realUpdatedAt
    );

    const one = await UnverifiedRecipe.findOne(perspective, { where: { id: recipeId } });
    expect(one?.name).to.equal("real");
  });

  it("hydrates it with the includeUnverified opt-in", async () => {
    const all = await UnverifiedRecipe.findAll(perspective, { includeUnverified: true });
    const recipe = all.find((r) => r.id === recipeId);
    expect(recipe?.name).to.equal("forged");
    expect(recipe?.cuisine, "a signed link is present both ways").to.equal("thai");

    const viaBuilder = await UnverifiedRecipe.query(perspective)
      .where({ id: recipeId })
      .includeUnverified()
      .get();
    expect(viaBuilder[0]?.name).to.equal("forged");
  });

  it("leaves the forged link out of __links by default", async () => {
    const targets = async (includeUnverified?: boolean) => {
      const [found] = await UnverifiedRecipe.findAll(perspective, {
        where: { id: recipeId },
        links: ["name"],
        includeUnverified,
      });
      return found.__links!.name.map((row) => row.data.target);
    };
    expect(await targets()).to.deep.equal([Literal.from("real").toUrl()]);
    expect(await targets(true)).to.deep.equal([
      Literal.from("real").toUrl(),
      Literal.from("forged").toUrl(),
    ]);
  });

  it("does not let a forged link reorder a page", async () => {
    // A second recipe with no name at all: only its type link, written
    // directly (`save()` would also write the `name = ""` initializer).
    // Unnamed sorts last, so page 1 of `order: { name: "ASC" }, limit: 1` is
    // the first recipe. A forged name that sorts first would put the second
    // recipe there instead.
    const otherId = "uvr://recipe/unnamed";
    const typeLink = await perspective.add(
      new Link({ source: otherId, predicate: "uvr://type", target: "uvr://recipe" })
    );
    await perspective.addLinkExpression({
      author: typeLink.author,
      timestamp: typeLink.timestamp,
      data: {
        source: otherId,
        predicate: "uvr://name",
        target: Literal.from("aaa").toUrl(),
      },
      proof: { key: typeLink.proof.key, signature: typeLink.proof.signature },
    } as LinkExpression);

    const page = (includeUnverified?: boolean) =>
      UnverifiedRecipe.findAll(perspective, {
        order: { name: "ASC" },
        limit: 1,
        includeUnverified,
      });
    expect((await page()).map((r) => r.id)).to.deep.equal([recipeId]);
    expect(
      (await page(true)).map((r) => r.id),
      "the opt-in sorts on the forged name"
    ).to.deep.equal([otherId]);
  });

  it("an explicit includeUnverified: false is the default", async () => {
    const all = await UnverifiedRecipe.findAll(perspective, { includeUnverified: false });
    expect(all.find((r) => r.id === recipeId)?.name).to.equal("real");
  });
});
