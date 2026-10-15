/**
 * Abstract model classes (#1435), end to end through the SDK: `findAll` over
 * `@Model({ abstract: true })` reads every class with its names (aliases
 * under the abstract name), and `save()` / `delete()` on a row read through
 * it reach the record's own class. The executor's rules (conformance,
 * views, dispatch, refusals, subscriptions) are tested in
 * `rust-executor/src/perspectives/{model_query/abstract_classes_tests.rs,
 * perspective_instance_abstract_tests.rs}`.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit \
 *     --require tests/model/hooks.ts tests/model/model-abstract.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Ad4mModel, Flag, LinkQuery, Model, PerspectiveProxy, Property } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
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

const byId = (a: { id: string }, b: { id: string }) => a.id.localeCompare(b.id);

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
    await AbsPoll.create(perspective, { title: "lunch?" });
    return { block, message };
  };

  it("findAll reads every class with the names, aliases under the abstract name", async () => {
    const { block, message } = await seed();
    const all = await AbsText.findAll(perspective);
    expect(all.map((i) => ({ id: i.id, text: i.text })).sort(byId)).to.deep.equal(
      [
        { id: block.id, text: "hello" },
        { id: message.id, text: "<p>hi @data</p>" },
      ].sort(byId),
    );
    const filtered = await AbsText.findAll(perspective, { where: { text: "<p>hi @data</p>" } });
    expect(filtered.map((i) => i.id)).to.deep.equal([message.id]);
  });

  it("save() and delete() through the abstract class write the record's own class", async () => {
    const { block, message } = await seed();
    const [row] = await AbsText.findAll(perspective, { where: { text: "<p>hi @data</p>" } });
    row.text = "edited";
    await row.save();
    const [own] = await AbsMessage.findAll(perspective);
    expect(own.body).to.equal("edited");
    expect(await perspective.get(new LinkQuery({ source: message.id, predicate: "test://abs/text" }))).to.have.length(0);

    await row.delete();
    expect(await AbsMessage.findAll(perspective)).to.have.length(0);
    expect((await AbsText.findAll(perspective)).map((i) => i.id)).to.deep.equal([block.id]);
  });
});
