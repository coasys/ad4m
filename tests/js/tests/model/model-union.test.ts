/**
 * Ad4mModel — one query over several classes (#1238), end to end
 *
 * `Ad4mModel.findAllOf` / `queryOf` send `class_names` to the executor, which
 * answers the union in one query: `where`, `order`, `limit` and `offset` apply
 * once over the union, each row comes back as the class it was hydrated as,
 * and a live subscription re-runs on an edit to any of the classes.
 *
 * Decided semantics: https://github.com/coasys/ad4m/issues/1238#issuecomment-6081266040
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit \
 *     --require tests/model/hooks.ts tests/model/model-union.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  Link,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";

// Two classes on one flag predicate with different values, as apps write them.
// Both declare `position`; only the task declares `status`, only the note `body`.

@Model({ name: "UnionTask" })
class UnionTask extends Ad4mModel {
  @Flag({ through: "test://union/kind", value: "test://union/task" })
  kind = "test://union/task";

  @Property({ through: "test://union/position" })
  position: number = 0;

  @Property({ through: "test://union/status" })
  status: string = "";
}

@Model({ name: "UnionNote" })
class UnionNote extends Ad4mModel {
  @Flag({ through: "test://union/kind", value: "test://union/note" })
  kind = "test://union/note";

  @Property({ through: "test://union/position" })
  position: number = 0;

  @Property({ through: "test://union/body" })
  body: string = "";
}

describe("Ad4mModel — one query over several classes", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  const registerAll = async () => {
    await UnionTask.register(perspective);
    await UnionNote.register(perspective);
  };

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-union");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-union-test");
    await registerAll();
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await registerAll();
  });

  /** Tasks at odd positions, notes at even ones. */
  const interleaved = async () => {
    const t1 = await UnionTask.create(perspective, { position: 1, status: "open" });
    const n2 = await UnionNote.create(perspective, { position: 2, body: "two" });
    const t3 = await UnionTask.create(perspective, { position: 3, status: "open" });
    const n4 = await UnionNote.create(perspective, { position: 4, body: "four" });
    const t5 = await UnionTask.create(perspective, { position: 5, status: "done" });
    return { t1, n2, t3, n4, t5 };
  };

  it("answers class_names over the wire with tagged rows", async () => {
    const { t1, n2 } = await interleaved();
    const result = await perspective.modelQuery(
      ["UnionTask", "UnionNote"],
      JSON.stringify({ order: [["position", "ASC"]], limit: 2 }),
    );
    expect(result.instances.map((i: any) => i.id)).to.deep.equal([t1.id, n2.id]);
    expect(result.instances.map((i: any) => i.__subjectClass)).to.deep.equal([
      "UnionTask",
      "UnionNote",
    ]);
    expect(result.totalCount).to.equal(5);
  });

  it("pages once over the union with interleaved sort keys", async () => {
    const { n2, t3, n4 } = await interleaved();
    const page = await Ad4mModel.queryOf(perspective, [UnionTask, UnionNote])
      .order({ position: "ASC" } as any)
      .limit(3)
      .offset(1)
      .get();

    expect(page.map((r) => r.id)).to.deep.equal([n2.id, t3.id, n4.id]);
    expect(page[0]).to.be.instanceOf(UnionNote);
    expect(page[1]).to.be.instanceOf(UnionTask);
    expect((page[0] as UnionNote).body).to.equal("two");
    expect((page[1] as UnionTask).status).to.equal("open");

    const total = await Ad4mModel.queryOf(perspective, [UnionTask, UnionNote]).count();
    expect(total).to.equal(5);
  });

  it("where on a property one class declares excludes the other class", async () => {
    const { t1, t3 } = await interleaved();
    const open = await Ad4mModel.findAllOf(perspective, [UnionTask, UnionNote], {
      where: { status: "open" },
      order: { position: "ASC" },
    });
    expect(open.map((r) => r.id)).to.deep.equal([t1.id, t3.id]);
  });

  it("order on a property one class lacks sorts those rows last, in both directions", async () => {
    const { t1, n2, t3, n4, t5 } = await interleaved();
    const notesLast = [n2.id, n4.id].sort();
    for (const dir of ["ASC", "DESC"] as const) {
      const rows = await Ad4mModel.findAllOf(perspective, [UnionTask, UnionNote], {
        order: { status: dir },
      });
      const ids = rows.map((r) => r.id);
      expect(ids.slice(3), dir).to.deep.equal(notesLast);
      expect(ids.slice(0, 3).sort(), dir).to.deep.equal([t1.id, t3.id, t5.id].sort());
    }
  });

  it("returns a record of both classes once, naming both classes", async () => {
    const both = await UnionTask.create(perspective, { position: 1, status: "open" });
    // The note's flag and body on the same node: it is now a UnionNote as well.
    await perspective.add(
      new Link({ source: both.id, predicate: "test://union/kind", target: "test://union/note" }),
    );

    const rows = await Ad4mModel.findAllOf(perspective, [UnionTask, UnionNote], {
      preferClasses: ["UnionTask"],
    });
    expect(rows.length).to.equal(1);
    expect(rows[0]).to.be.instanceOf(UnionTask);
    expect([...(rows[0] as any).__subjectClasses].sort()).to.deep.equal([
      "UnionNote",
      "UnionTask",
    ]);

    const asNote = await Ad4mModel.findAllOf(perspective, [UnionTask, UnionNote], {
      preferClasses: ["UnionNote"],
    });
    expect(asNote.length).to.equal(1);
    expect(asNote[0]).to.be.instanceOf(UnionNote);
  });

  it("keeps a record of both classes that passes `where` as one of them", async () => {
    const both = await UnionTask.create(perspective, { position: 1, status: "open" });
    await perspective.add(
      new Link({ source: both.id, predicate: "test://union/kind", target: "test://union/note" }),
    );
    await UnionNote.create(perspective, { position: 2, body: "other" });

    // `status` is the task's alone, so only the task reading passes; even a
    // preference for the note reading must not drop the record.
    for (const preferClasses of [undefined, ["UnionNote"]]) {
      const rows = await Ad4mModel.findAllOf(perspective, [UnionTask, UnionNote], {
        where: { status: "open" },
        ...(preferClasses && { preferClasses }),
      });
      expect(rows.map((r) => r.id)).to.deep.equal([both.id]);
      expect(rows[0]).to.be.instanceOf(UnionTask);
    }
  });

  it("one live subscription fires on an edit to each class", async () => {
    const { t1, n2 } = await interleaved();
    const batches: Ad4mModel[][] = [];
    const builder = Ad4mModel.queryOf(perspective, [UnionTask, UnionNote]);
    const initial = await builder.subscribe((r) => batches.push(r));
    expect(initial.length).to.equal(5);

    try {
      // `status` is only the task's predicate…
      await UnionTask.update(perspective, t1.id, { status: "done" });
      await waitUntil(
        () => batches.some((b) => b.some((r) => r.id === t1.id && (r as UnionTask).status === "done")),
        10_000,
        "task edit reaches the union subscription",
      );
      // …and `body` only the note's.
      await UnionNote.update(perspective, n2.id, { body: "edited" });
      await waitUntil(
        () => batches.some((b) => b.some((r) => r.id === n2.id && (r as UnionNote).body === "edited")),
        10_000,
        "note edit reaches the union subscription",
      );
    } finally {
      await builder.dispose();
    }
  });
});
