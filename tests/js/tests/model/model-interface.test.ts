/**
 * Model query over an interface (#1435), end to end
 *
 * `modelQuery({ implements })` / `modelQuery({ interface })` read every
 * installed class that has what the interface asks for, as one union query:
 * rows carry `__subjectClass`, a class without the property is left out, and
 * a live subscription picks up a class installed after it was opened.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit \
 *     --require tests/model/hooks.ts tests/model/model-interface.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  LiveQuery,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";

// Two apps' messages: both call it `text`, each stores it under its own predicate.

@Model({ name: "IfaceBlock" })
class IfaceBlock extends Ad4mModel {
  @Flag({ through: "test://iface/kind", value: "test://iface/block" })
  kind = "test://iface/block";

  @Property({ through: "test://iface/block_text" })
  text: string = "";
}

@Model({ name: "IfaceChat" })
class IfaceChat extends Ad4mModel {
  @Flag({ through: "test://iface/kind", value: "test://iface/chat" })
  kind = "test://iface/chat";

  @Property({ through: "test://iface/chat_body" })
  text: string = "";
}

// No `text`.
@Model({ name: "IfacePoll" })
class IfacePoll extends Ad4mModel {
  @Flag({ through: "test://iface/kind", value: "test://iface/poll" })
  kind = "test://iface/poll";

  @Property({ through: "test://iface/title" })
  title: string = "";
}

// Installed only after a subscription is open.
@Model({ name: "IfaceLate" })
class IfaceLate extends Ad4mModel {
  @Flag({ through: "test://iface/kind", value: "test://iface/late" })
  kind = "test://iface/late";

  @Property({ through: "test://iface/late_text" })
  text: string = "";
}

const HAS_TEXT = { implements: { properties: ["text"] } };

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));

function rows(result: any): { id: string; cls: string; text: string }[] {
  const r = typeof result === "string" ? JSON.parse(result) : result;
  return (r.instances as any[])
    .map((i) => ({ id: i.id, cls: i.__subjectClass, text: i.text }))
    .sort((a, b) => a.id.localeCompare(b.id));
}

describe("Model query over an interface", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  const registerAll = async () => {
    await IfaceBlock.register(perspective);
    await IfaceChat.register(perspective);
    await IfacePoll.register(perspective);
  };

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-interface");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-interface-test");
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
    const block = await IfaceBlock.create(perspective, { text: "hello" });
    const chat = await IfaceChat.create(perspective, { text: "hi @data" });
    const poll = await IfacePoll.create(perspective, { title: "lunch?" });
    return { block, chat, poll };
  };

  it("returns every class with `text` under one query, tagged, and leaves out a class without it", async () => {
    const { block, chat, poll } = await seed();
    const result = await perspective.modelQuery(HAS_TEXT, JSON.stringify({}));
    expect(rows(result)).to.deep.equal(
      [
        { id: block.id, cls: "IfaceBlock", text: "hello" },
        { id: chat.id, cls: "IfaceChat", text: "hi @data" },
      ].sort((a, b) => a.id.localeCompare(b.id)),
    );
    expect(result.totalCount).to.equal(2);
    expect(rows(result).map((r) => r.id)).to.not.include(poll.id);
  });

  it("answers 'is this base a message?' with where: { id }", async () => {
    const { chat, poll } = await seed();
    const yes = await perspective.modelQuery(HAS_TEXT, JSON.stringify({ where: { id: chat.id } }));
    expect(rows(yes)).to.deep.equal([{ id: chat.id, cls: "IfaceChat", text: "hi @data" }]);
    const no = await perspective.modelQuery(HAS_TEXT, JSON.stringify({ where: { id: poll.id } }));
    expect(no.instances).to.deep.equal([]);
  });

  it("pins the predicate with `through`", async () => {
    const { chat } = await seed();
    const result = await perspective.modelQuery(
      { implements: { properties: [{ name: "text", through: "test://iface/chat_body" }] } },
      JSON.stringify({}),
    );
    expect(rows(result).map((r) => r.id)).to.deep.equal([chat.id]);
  });

  it("reads a named interface registered with addInterface", async () => {
    const { block, chat } = await seed();
    await perspective.addInterface("IfaceText", { properties: ["text"] });
    const result = await perspective.modelQuery({ interface: "IfaceText" }, JSON.stringify({}));
    expect(rows(result).map((r) => r.id).sort()).to.deep.equal([block.id, chat.id].sort());
    // An interface is not a class.
    expect(await perspective.subjectClassesOf([block.id])).to.deep.equal({ [block.id]: ["IfaceBlock"] });
  });

  it("answers an empty result when no class implements the interface", async () => {
    await seed();
    const result = await perspective.modelQuery(
      { implements: { properties: ["nothing-has-this"] } },
      JSON.stringify({}),
    );
    expect(result).to.deep.equal({ instances: [], totalCount: 0 });
  });

  it("refuses what a union refuses", async () => {
    let error: any;
    try {
      await perspective.modelQuery(
        HAS_TEXT,
        JSON.stringify({ parent: { ids: ["test://p"], predicate: "ad4m://has_child", limitPerAnchor: 2 } }),
      );
    } catch (e) {
      error = e;
    }
    expect(String(error?.message ?? error)).to.contain("limitPerAnchor");
  });

  it("a subscription picks up a class installed after subscribe", async () => {
    // A perspective of its own: `IfaceLate` must not be installed yet.
    const fresh = await ad4m.perspective.add("model-interface-late");
    await IfaceBlock.register(fresh);
    const block = await IfaceBlock.create(fresh, { text: "before" });

    const results: any[] = [];
    const live = new LiveQuery(
      fresh.client,
      fresh.uuid,
      () => fresh.modelSubscribe(HAS_TEXT, JSON.stringify({})),
      (r) => results.push(r),
    );
    const initial = await live.start();
    try {
      expect(rows(initial).map((r) => r.id)).to.deep.equal([block.id]);

      // Settle after subscribe, install the class, settle again, so the write
      // under test is alone in its subscription-check batch.
      await sleep(1000);
      await IfaceLate.register(fresh);
      await sleep(1000);

      const late = await IfaceLate.create(fresh, { text: "@data after" });
      await waitUntil(
        () => results.some((r) => rows(r).some((x) => x.id === late.id && x.cls === "IfaceLate")),
        10_000,
        "the new class's record reaches the interface subscription",
      );
      const last = rows(results[results.length - 1]);
      expect(last.map((r) => r.id).sort()).to.deep.equal([block.id, late.id].sort());
    } finally {
      live.dispose();
      await ad4m.perspective.remove(fresh.uuid);
    }
  });
});
