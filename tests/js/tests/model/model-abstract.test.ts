/**
 * Abstract model classes (#1435), end to end
 *
 * `@Model({ abstract: true })` is queried through the ordinary API (`findAll`,
 * `modelQuery`, subscriptions): it reads every installed class that has its
 * property names, or aliases for them, as one union. Rows keep their class,
 * an aliased property comes back under the abstract class's name, a class
 * without the name is left out, the abstract class cannot be created and no
 * record is written through it, and a subscription picks up a class installed
 * after it was opened.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit \
 *     --require tests/model/hooks.ts tests/model/model-abstract.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  LinkQuery,
  LiveQuery,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";

@Model({ name: "AbsText", abstract: true })
class AbsText extends Ad4mModel {
  @Property({ through: "test://abs/text" })
  text: string = "";
}

// Calls it `text`, under its own predicate.
@Model({ name: "AbsBlock", implements: [AbsText] })
class AbsBlock extends Ad4mModel {
  @Flag({ through: "test://abs/kind", value: "test://abs/block" })
  kind = "test://abs/block";

  @Property({ through: "test://abs/block_text" })
  text: string = "";
}

// Flux-like: `body`, aliased `text`.
@Model({ name: "AbsMessage", implements: [AbsText] })
class AbsMessage extends Ad4mModel {
  @Flag({ through: "test://abs/kind", value: "test://abs/message" })
  kind = "test://abs/message";

  @Property({ through: "test://abs/body", alias: { AbsText: "text" } })
  body: string = "";
}

// No `text`.
@Model({ name: "AbsPoll" })
class AbsPoll extends Ad4mModel {
  @Flag({ through: "test://abs/kind", value: "test://abs/poll" })
  kind = "test://abs/poll";

  @Property({ through: "test://abs/title" })
  title: string = "";
}

// Installed only after a subscription is open.
@Model({ name: "AbsLate" })
class AbsLate extends Ad4mModel {
  @Flag({ through: "test://abs/kind", value: "test://abs/late" })
  kind = "test://abs/late";

  @Property({ through: "test://abs/late_body", alias: { AbsText: "text" } })
  content: string = "";
}

// A real `text` and an alias `text`: refused when it is registered.
@Model({ name: "AbsClash" })
class AbsClash extends Ad4mModel {
  @Flag({ through: "test://abs/kind", value: "test://abs/clash" })
  kind = "test://abs/clash";

  @Property({ through: "test://abs/clash_text" })
  text: string = "";

  @Property({ through: "test://abs/clash_body", alias: { AbsText: "text" } })
  body: string = "";
}

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

function rows(result: any): { id: string; cls: string; text: string }[] {
  const r = typeof result === "string" ? JSON.parse(result) : result;
  return (r.instances as any[])
    .map((i) => ({ id: i.id, cls: i.__subjectClass, text: i.text }))
    .sort((a, b) => a.id.localeCompare(b.id));
}

describe("Abstract model classes", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  const registerAll = async () => {
    await AbsText.register(perspective);
    await AbsBlock.register(perspective);
    await AbsMessage.register(perspective);
    await AbsPoll.register(perspective);
  };

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-abstract");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-abstract-test");
    await registerAll();
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await registerAll();
  });

  const seed = async () => {
    const block = await AbsBlock.create(perspective, { text: "hello" });
    const message = await AbsMessage.create(perspective, { body: "<p>hi @data</p>" });
    const poll = await AbsPoll.create(perspective, { title: "lunch?" });
    return { block, message, poll };
  };

  it("modelQuery reads every class with the names, tagged, aliases under the abstract name", async () => {
    const { block, message, poll } = await seed();
    const result = await perspective.modelQuery("AbsText", JSON.stringify({}));
    expect(rows(result)).to.deep.equal(
      [
        { id: block.id, cls: "AbsBlock", text: "hello" },
        { id: message.id, cls: "AbsMessage", text: "<p>hi @data</p>" },
      ].sort((a, b) => a.id.localeCompare(b.id)),
    );
    expect(result.totalCount).to.equal(2);
    expect(rows(result).map((r) => r.id)).to.not.include(poll.id);
    const raw = result.instances.find((i: any) => i.id === message.id);
    expect(raw).to.not.have.property("body");
  });

  it("findAll on the abstract class, with where on an aliased name", async () => {
    const { block, message } = await seed();
    const all = await AbsText.findAll(perspective);
    expect(all.map((i) => [i.id, i.text]).sort()).to.deep.equal(
      [[block.id, "hello"], [message.id, "<p>hi @data</p>"]].sort(),
    );
    const filtered = await AbsText.findAll(perspective, { where: { text: "<p>hi @data</p>" } });
    expect(filtered.map((i) => i.id)).to.deep.equal([message.id]);
  });

  it("the concrete class read on its own keeps its own name", async () => {
    const { message } = await seed();
    const [own] = await AbsMessage.findAll(perspective);
    expect(own.id).to.equal(message.id);
    expect(own.body).to.equal("<p>hi @data</p>");
    expect((own as any).text).to.equal(undefined);
  });

  it("an abstract class cannot be created and is never a record's class", async () => {
    const { block } = await seed();
    let error: any;
    try {
      await AbsText.create(perspective, { text: "nope" });
    } catch (e) {
      error = e;
    }
    expect(String(error?.message ?? error)).to.contain("abstract class");

    error = undefined;
    try {
      await perspective.createSubject("AbsText", "test://abs/direct", { text: "nope" });
    } catch (e) {
      error = e;
    }
    expect(String(error?.message ?? error)).to.contain("abstract class");

    expect(await perspective.subjectClassesOf([block.id])).to.deep.equal({ [block.id]: ["AbsBlock"] });
  });

  it("a record read through an abstract class is never written through it", async () => {
    const { message } = await seed();
    const refusal = async (write: () => Promise<unknown>) => {
      try {
        await write();
      } catch (e: any) {
        return String(e?.message ?? e);
      }
      return "(not refused)";
    };

    const [row] = await AbsText.findAll(perspective, { where: { text: "<p>hi @data</p>" } });
    expect(row.id).to.equal(message.id);
    row.text = "edited";
    expect(await refusal(() => row.save())).to.contain(
      "'AbsText' is an abstract class: a record cannot be updated through it. Write it through its own class, 'AbsMessage'",
    );
    expect(await refusal(() => row.delete())).to.contain("cannot be deleted through it");
    expect(await refusal(() => AbsText.update(perspective, message.id, { text: "edited" }))).to.contain(
      "cannot be updated through it",
    );
    expect(await refusal(() => perspective.removeSubject("AbsText", message.id))).to.contain(
      "cannot be deleted through it",
    );

    // Nothing was written or removed: the abstract predicate is absent, the
    // body and the record are as they were.
    expect(await perspective.get(new LinkQuery({ source: message.id, predicate: "test://abs/text" }))).to.have.length(0);
    const [own] = await AbsMessage.findAll(perspective);
    expect(own.id).to.equal(message.id);
    expect(own.body).to.equal("<p>hi @data</p>");

    // Through its own class it goes through.
    own.body = "edited";
    await own.save();
    const [again] = await AbsText.findAll(perspective, { where: { text: "edited" } });
    expect(again?.id).to.equal(message.id);
  });

  it("an alias that clashes with a name of the class is refused at registration", async () => {
    let error: any;
    try {
      await AbsClash.register(perspective);
    } catch (e) {
      error = e;
    }
    expect(String(error?.message ?? error)).to.contain("already has a property named 'text'");
  });

  it("refuses what a union refuses, and an abstract class among several", async () => {
    let error: any;
    try {
      await perspective.modelQuery(
        "AbsText",
        JSON.stringify({ parent: { ids: ["test://p"], predicate: "ad4m://has_child", limitPerAnchor: 2 } }),
      );
    } catch (e) {
      error = e;
    }
    expect(String(error?.message ?? error)).to.contain("limitPerAnchor");

    error = undefined;
    try {
      await perspective.modelQuery(["AbsText", "AbsPoll"], JSON.stringify({}));
    } catch (e) {
      error = e;
    }
    expect(String(error?.message ?? error)).to.contain("abstract class");
  });

  it("a subscription picks up a class installed after subscribe", async () => {
    // A perspective of its own: `AbsLate` must not be installed yet.
    const fresh = await ad4m.perspective.add("model-abstract-late");
    await AbsText.register(fresh);
    await AbsBlock.register(fresh);
    const block = await AbsBlock.create(fresh, { text: "before" });

    const results: any[] = [];
    const live = new LiveQuery(
      fresh.client,
      fresh.uuid,
      () => fresh.modelSubscribe("AbsText", JSON.stringify({})),
      (r) => results.push(r),
    );
    const initial = await live.start();
    try {
      expect(rows(initial).map((r) => r.id)).to.deep.equal([block.id]);

      // Settle after subscribe, install the class, settle again, so the write
      // under test is alone in its subscription-check batch.
      await sleep(1000);
      await AbsLate.register(fresh);
      await sleep(1000);

      const late = await AbsLate.create(fresh, { content: "@data after" });
      await waitUntil(
        () =>
          results.some((r) =>
            rows(r).some((x) => x.id === late.id && x.cls === "AbsLate" && x.text === "@data after"),
          ),
        10_000,
        "the new class's record reaches the subscription, under the abstract name",
      );
      const last = rows(results[results.length - 1]);
      expect(last.map((r) => r.id).sort()).to.deep.equal([block.id, late.id].sort());
    } finally {
      live.dispose();
      await ad4m.perspective.remove(fresh.uuid);
    }
  });
});
