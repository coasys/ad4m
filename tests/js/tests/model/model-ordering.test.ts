/**
 * Ad4mModel — CRDT-ordered collections, end to end
 *
 * The ordering feature is declared in `core` (the `@HasMany({ ordering })`
 * decorator and the SHACL it generates) and acted on in the executor (entries
 * written by `collectionSetter`, order reconstructed in `hydrate_one`). Until
 * this file nothing exercised the join: the Rust tests build their shape from
 * JSON, and the `core` tests assert what the decorator emits. Both were green
 * for a period in which the feature did nothing at all, because the declaration
 * was being dropped in the serialisation between them.
 *
 * These drive the real path: `@Model` classes registered through
 * `ensureSubjectClass`, saved through a real batch, read back through the ORM.
 *
 * Why a reorder rather than a fresh create is the load-bearing case: on create,
 * the collection's links are written in the order assigned, so reading by link
 * timestamp coincides with the array and proves nothing. A *reorder* separates
 * them — `collectionSetter` now diffs, so an unchanged member keeps its original
 * timestamp, and only the ordering entries carry the new sequence.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit \
 *     --require tests/model/hooks.ts tests/model/model-ordering.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
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

/** The predicate the ordering entries are stored under, on the parent. */
const COLLECTION_ORDER = "ad4m://collection_order";

// ── Models ────────────────────────────────────────────────────────────────────

@Model({ name: "OrderTask" })
class OrderTask extends Ad4mModel {
  @Flag({ through: "test://ord/type", value: "test://ord/task" })
  type = "test://ord/task";

  @Property({ through: "test://ord/title" })
  title: string = "";
}

/**
 * One ordered collection and one plain one on the same class, so every
 * assertion about ordering has a control that shares the instance, the save and
 * the read.
 */
@Model({ name: "OrderColumn" })
class OrderColumn extends Ad4mModel {
  @Flag({ through: "test://ord/type", value: "test://ord/column" })
  type = "test://ord/column";

  @HasMany({
    through: "test://ord/tasks",
    target: () => OrderTask,
    ordering: { strategy: "linkedList" },
  })
  tasks: string[] = [];

  @HasMany({ through: "test://ord/watchers", target: () => OrderTask })
  watchers: string[] = [];
}

const ALL_MODELS = [OrderTask, OrderColumn];

describe("Ad4mModel — CRDT-ordered collections", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  const registerAll = async () => {
    for (const M of ALL_MODELS) await (M as any).register(perspective);
  };

  /** Three tasks, created in a-b-c order so their link timestamps ascend. */
  const seedTasks = async () => {
    const a = await OrderTask.create(perspective, { title: "a" });
    const b = await OrderTask.create(perspective, { title: "b" });
    const c = await OrderTask.create(perspective, { title: "c" });
    return { a, b, c };
  };

  const readTasks = async (id: string): Promise<string[]> => {
    const found = await OrderColumn.findOne(perspective, { where: { id } });
    expect(found, "column reads back").to.not.be.null;
    return found!.tasks as string[];
  };

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-ordering");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-ordering-test");
    await registerAll();
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await registerAll();
  });

  // ── 1. The declaration reaches the executor at all ──────────────────────────

  it("writes ordering entries when the collection is created", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      tasks: [a.id, b.id, c.id],
    });

    const entries = await perspective.get(
      new LinkQuery({ source: column.id, predicate: COLLECTION_ORDER }),
    );

    // A create is one batch: the flag link that classifies the column and the
    // setter that fills the collection are staged together, so the strategy
    // lookup has to see the batch's own staged links. Reading the store alone
    // answers "no class" and silently writes nothing.
    expect(
      entries.length,
      "the collection carries ordering entries after create",
    ).to.be.greaterThan(0);
  });

  it("declares no ordering entries for a relation that is not ordered", async () => {
    const { a, b } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      watchers: [a.id, b.id],
    });

    const entries = await perspective.get(
      new LinkQuery({ source: column.id, predicate: COLLECTION_ORDER }),
    );
    expect(entries.length, "an unordered relation writes no entries").to.equal(
      0,
    );
  });

  // ── 2. The headline claim ───────────────────────────────────────────────────

  it("returns a reordered collection in the assigned order", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      tasks: [a.id, b.id, c.id],
    });
    expect(await readTasks(column.id)).to.deep.equal([a.id, b.id, c.id]);

    column.tasks = [c.id, a.id, b.id];
    await column.save();

    // The members are unchanged, so the diff leaves every link timestamp where
    // it was: reading by timestamp would still answer a, b, c. Only the ordering
    // entries carry the new sequence, which makes this the assertion that fails
    // if any link in the chain — decorator, SHACL, registration, setter,
    // hydration — drops the declaration.
    expect(await readTasks(column.id)).to.deep.equal([c.id, a.id, b.id]);
  });

  it("keeps CRDT order when the collection is read through findAll", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      tasks: [a.id, b.id, c.id],
    });
    column.tasks = [c.id, b.id, a.id];
    await column.save();

    // Reconstruction lives in `hydrate_one`, which is what the ORM reads
    // through — `findAll` included. Placed in `get_links` instead it would be
    // invisible here, which was the design's largest correction.
    const all = await OrderColumn.findAll(perspective, {});
    const found = all.find((x: any) => x.id === column.id);
    expect(found, "column is in findAll").to.not.be.undefined;
    expect((found as any).tasks).to.deep.equal([c.id, b.id, a.id]);
  });

  // ── 3. Order survives membership changes ────────────────────────────────────

  it("holds a member's position when another is inserted before it", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      tasks: [a.id, b.id, c.id],
    });

    const d = await OrderTask.create(perspective, { title: "d" });
    column.tasks = [a.id, d.id, b.id, c.id];
    await column.save();

    expect(await readTasks(column.id)).to.deep.equal([
      a.id,
      d.id,
      b.id,
      c.id,
    ]);
  });

  it("keeps the surviving members in order when one is removed", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      tasks: [c.id, a.id, b.id],
    });

    column.tasks = [c.id, b.id];
    await column.save();

    // A removal writes no ordering entry at all — dropping the data link is the
    // whole deletion, and the entries left behind are position hints over a
    // membership set the data links define.
    expect(await readTasks(column.id)).to.deep.equal([c.id, b.id]);
  });

  it("survives a reorder saved twice in a row", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      tasks: [a.id, b.id, c.id],
    });

    column.tasks = [b.id, c.id, a.id];
    await column.save();
    column.tasks = [c.id, b.id, a.id];
    await column.save();

    // The second save diffs against entries the first one wrote, rather than
    // restating the chain from scratch.
    expect(await readTasks(column.id)).to.deep.equal([c.id, b.id, a.id]);
  });

  // ── 4. The unordered control ────────────────────────────────────────────────

  it("leaves an unordered relation as a set", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {
      watchers: [a.id, b.id, c.id],
    });

    column.watchers = [c.id, a.id, b.id];
    await column.save();

    const found = await OrderColumn.findOne(perspective, {
      where: { id: column.id },
    });
    // Same members, no claim about sequence: an unordered relation is a set,
    // and re-assigning the same members in another order is not a change at
    // all — `changedFields` sorts both sides before comparing unless the
    // relation declares an ordering.
    expect((found!.watchers as string[]).slice().sort()).to.deep.equal(
      [a.id, b.id, c.id].slice().sort(),
    );
  });

  // ── 5. The migration path ───────────────────────────────────────────────────

  it("still returns every member when a collection has no entries yet", async () => {
    const { a, b, c } = await seedTasks();
    const column = await OrderColumn.create(perspective, {});

    // Written as raw links, bypassing `collectionSetter` — which is how a
    // collection that predates the ordering declaration looks, and how one
    // written by a peer that does not know the strategy arrives.
    for (const t of [c, a, b]) {
      await perspective.add(
        new Link({
          source: column.id,
          predicate: "test://ord/tasks",
          target: t.id,
        }),
      );
    }

    // Membership, not sequence: with no entries there is nothing to reconstruct
    // from, and this relation names a target class, so its array comes from a
    // conformance getter whose SELECT carries no ORDER BY. The order is
    // genuinely unspecified until the first save writes a chain — asserting one
    // here would be asserting a guarantee the executor does not make.
    //
    // What the migration path does promise is that the collection still reads:
    // an ordered relation whose ordering links have not arrived degrades to an
    // ordinary unordered one rather than failing or coming back empty.
    const read = await readTasks(column.id);
    expect(read.slice().sort()).to.deep.equal(
      [a.id, b.id, c.id].slice().sort(),
    );
  });
});
