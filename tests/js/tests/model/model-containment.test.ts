/**
 * Containment through the built-in `Container` (#1446), end to end through
 * the SDK against a real executor:
 *
 * - `Container` children queries: a node no class describes holding by
 *   `ad4m://has_child` (`ad4m://self` too), a Block holding through its own
 *   `we://child`, and a Block using both.
 * - `parent` on ordinary model queries in both directions: the children of a
 *   holder (`parent: { id }`) and what holds a record
 *   (`parent: { ids, direction: "in" }`), each through `has_child` and
 *   through a class's own relation.
 * - Each holder's own predicate only: a Board's `children` is `board://item`,
 *   so a `we://child` link from a Board is not containment, in either
 *   direction, and a node no class describes holds only by `has_child`.
 *
 * The executor's rules are tested in
 * `rust-executor/src/perspectives/{model_query/containment.rs,
 * perspective_instance/containment_tests.rs}`.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit \
 *     --require tests/model/hooks.ts tests/model/model-containment.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Container,
  Flag,
  HasMany,
  Link,
  LinkQuery,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";

const HAS_CHILD = "ad4m://has_child";
const WE_CHILD = "we://child";
const BOARD_ITEM = "board://item";

@Model({ name: "CtMessage" })
class CtMessage extends Ad4mModel {
  @Flag({ through: "test://ct/kind", value: "test://ct/message" })
  kind = "test://ct/message";

  @Property({ through: "test://ct/body" })
  body: string = "";
}

// Holds its children through its own relation, as a We block does.
@Model({ name: "CtBlock", implements: [Container] })
class CtBlock extends Ad4mModel {
  @Flag({ through: "test://ct/kind", value: "test://ct/block" })
  kind = "test://ct/block";

  @HasMany({ through: WE_CHILD, alias: { Container: "children" } })
  blocks: string[] = [];
}

// Holds its children through another relation of its own.
@Model({ name: "CtBoard", implements: [Container] })
class CtBoard extends Ad4mModel {
  @Flag({ through: "test://ct/kind", value: "test://ct/board" })
  kind = "test://ct/board";

  @HasMany({ through: BOARD_ITEM, alias: { Container: "children" } })
  items: string[] = [];
}

const sorted = (ids: string[]) => [...ids].sort();
const ids = (rows: { id: string }[]) => sorted(rows.map((r) => r.id));

describe("Containment through Container", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-containment");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-containment-test");
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await CtMessage.register(perspective);
    await CtBlock.register(perspective);
    await CtBoard.register(perspective);
  });

  const link = (source: string, predicate: string, target: string) =>
    perspective.add(new Link({ source, predicate, target }));

  const message = async (body: string) => (await CtMessage.create(perspective, { body })).id;

  /** The children of `holder`'s Container row, or `undefined` when it has no row. */
  const childrenOf = async (holder: string) => {
    const rows = await Container.findAll(perspective, { where: { id: holder } });
    expect(rows.length, `rows for ${holder}`).to.be.at.most(1);
    return rows[0] ? sorted(rows[0].children) : undefined;
  };

  /** Every holder of `child`, read through Container. */
  const holdersOf = async (child: string) =>
    ids(await Container.findAll(perspective, { parent: { ids: [child], direction: "in" } }));

  describe("Container children", () => {
    it("a node no class describes holds what it links by has_child", async () => {
      const node = "test://ct/node-" + Date.now();
      const [m1, m2] = [await message("one"), await message("two")];
      await link(node, HAS_CHILD, m1);
      await link(node, HAS_CHILD, m2);

      expect(await childrenOf(node)).to.deep.equal(sorted([m1, m2]));
    });

    it("ad4m://self holds what it links by has_child", async () => {
      const top = await message("top level");
      await link("ad4m://self", HAS_CHILD, top);

      expect(await childrenOf("ad4m://self")).to.include(top);
      expect(await holdersOf(top)).to.deep.equal(["ad4m://self"]);
    });

    it("a Block holds what it links by its own we://child", async () => {
      const block = (await CtBlock.create(perspective)).id;
      const m1 = await message("in the block");
      await link(block, WE_CHILD, m1);

      expect(await childrenOf(block)).to.deep.equal([m1]);
      // The Block's own relation reads the same link.
      const [own] = await CtBlock.findAll(perspective, { where: { id: block } });
      expect(own.blocks).to.deep.equal([m1]);
    });

    it("a Block holding by both we://child and has_child holds both", async () => {
      const block = (await CtBlock.create(perspective)).id;
      const [viaOwn, viaHasChild] = [await message("own"), await message("has_child")];
      await link(block, WE_CHILD, viaOwn);
      await link(block, HAS_CHILD, viaHasChild);

      expect(await childrenOf(block)).to.deep.equal(sorted([viaOwn, viaHasChild]));
      // A where on children reads both edges.
      for (const child of [viaOwn, viaHasChild]) {
        const rows = await Container.findAll(perspective, { where: { children: child } });
        expect(ids(rows), `holders of ${child} by where`).to.deep.equal([block]);
      }
    });
  });

  describe("parent on model queries", () => {
    it("children of P: through has_child, and through the holder's own relation", async () => {
      const node = "test://ct/node-" + Date.now();
      const block = (await CtBlock.create(perspective)).id;
      const [inNode, inBlock] = [await message("node"), await message("block")];
      await message("under nothing");
      await link(node, HAS_CHILD, inNode);
      await link(block, WE_CHILD, inBlock);

      expect(ids(await CtMessage.findAll(perspective, { parent: { id: node } }))).to.deep.equal([inNode]);
      expect(ids(await CtMessage.findAll(perspective, { parent: { id: block } }))).to.deep.equal([inBlock]);
      // A step out from both at once, as a traversal.
      expect(ids(await CtMessage.findAll(perspective, { parent: { ids: [node, block] } }))).to.deep.equal(
        sorted([inNode, inBlock]),
      );
    });

    it("who holds X: through has_child, and through the holder's own relation", async () => {
      const node = "test://ct/node-" + Date.now();
      const byHasChild = (await CtBlock.create(perspective)).id;
      const byOwn = (await CtBlock.create(perspective)).id;
      const board = (await CtBoard.create(perspective)).id;
      const x = await message("held four times");
      await link(node, HAS_CHILD, x);
      await link(byHasChild, HAS_CHILD, x);
      await link(byOwn, WE_CHILD, x);
      await link(board, BOARD_ITEM, x);

      expect(
        ids(await CtBlock.findAll(perspective, { parent: { ids: [x], direction: "in" } })),
      ).to.deep.equal(sorted([byHasChild, byOwn]));
      expect(
        ids(await CtBoard.findAll(perspective, { parent: { ids: [x], direction: "in" } })),
      ).to.deep.equal([board]);
      expect(await holdersOf(x)).to.deep.equal(sorted([node, byHasChild, byOwn, board]));
    });

    it("create under { id } writes the holder's own relation, or has_child", async () => {
      const node = "test://ct/node-" + Date.now();
      const block = (await CtBlock.create(perspective)).id;
      const inBlock = (await CtMessage.create(perspective, { body: "b" }, { parent: { id: block } })).id;
      const inNode = (await CtMessage.create(perspective, { body: "n" }, { parent: { id: node } })).id;

      const predicates = async (source: string, target: string) =>
        (await perspective.get(new LinkQuery({ source, target }))).map((l) => l.data.predicate);
      expect(await predicates(block, inBlock)).to.deep.equal([WE_CHILD]);
      expect(await predicates(node, inNode)).to.deep.equal([HAS_CHILD]);
      expect(ids(await CtMessage.findAll(perspective, { parent: { id: block } }))).to.deep.equal([inBlock]);
      expect(ids(await CtMessage.findAll(perspective, { parent: { id: node } }))).to.deep.equal([inNode]);
    });
  });

  describe("each holder's own predicate only", () => {
    /**
     * A Board holding `item` by `board://item`, with a stray `we://child` to
     * `stray`; a Block holding `inBlock` by `we://child`, with a stray
     * `board://item` to `strayInBlock`; a node no class describes holding
     * `kept` by has_child, with stray `we://child` and `board://item` links.
     * Every target is a CtMessage, so a class filter never hides a stray.
     */
    const seed = async () => {
      const board = (await CtBoard.create(perspective)).id;
      const block = (await CtBlock.create(perspective)).id;
      const node = "test://ct/node-" + Date.now();
      const m = {
        item: await message("board item"),
        stray: await message("we://child on a board"),
        inBlock: await message("block child"),
        strayInBlock: await message("board://item on a block"),
        kept: await message("has_child on a node"),
        nodeWe: await message("we://child on a node"),
        nodeBoard: await message("board://item on a node"),
      };
      await link(board, BOARD_ITEM, m.item);
      await link(board, WE_CHILD, m.stray);
      await link(block, WE_CHILD, m.inBlock);
      await link(block, BOARD_ITEM, m.strayInBlock);
      await link(node, HAS_CHILD, m.kept);
      await link(node, WE_CHILD, m.nodeWe);
      await link(node, BOARD_ITEM, m.nodeBoard);
      return { board, block, node, m };
    };

    it("children: a Board counts board://item, a Block we://child, a classless node has_child", async () => {
      const { board, block, node, m } = await seed();

      expect(await childrenOf(board), "Board's Container children").to.deep.equal([m.item]);
      expect(await childrenOf(block), "Block's Container children").to.deep.equal([m.inBlock]);
      expect(await childrenOf(node), "classless node's Container children").to.deep.equal([m.kept]);

      expect(ids(await CtMessage.findAll(perspective, { parent: { id: board } })), "parent: Board").to.deep.equal([
        m.item,
      ]);
      expect(ids(await CtMessage.findAll(perspective, { parent: { id: block } })), "parent: Block").to.deep.equal([
        m.inBlock,
      ]);
      expect(ids(await CtMessage.findAll(perspective, { parent: { id: node } })), "parent: node").to.deep.equal([
        m.kept,
      ]);
    });

    it("holders: a stray link makes nothing a holder", async () => {
      const { board, block, node, m } = await seed();

      for (const stray of [m.stray, m.strayInBlock, m.nodeWe, m.nodeBoard]) {
        expect(await holdersOf(stray), `Container holders of ${stray}`).to.deep.equal([]);
        expect(
          ids(await CtBoard.findAll(perspective, { parent: { ids: [stray], direction: "in" } })),
          `Board holders of ${stray}`,
        ).to.deep.equal([]);
        expect(
          ids(await CtBlock.findAll(perspective, { parent: { ids: [stray], direction: "in" } })),
          `Block holders of ${stray}`,
        ).to.deep.equal([]);
      }
      expect(await holdersOf(m.item)).to.deep.equal([board]);
      expect(await holdersOf(m.inBlock)).to.deep.equal([block]);
      expect(await holdersOf(m.kept)).to.deep.equal([node]);
    });
  });
});
