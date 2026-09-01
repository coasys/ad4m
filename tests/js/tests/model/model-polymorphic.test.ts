/**
 * Ad4mModel — polymorphic relations, end to end
 *
 * Every case here already has a Rust test and a `core` unit test, and until
 * this file the two never met: the executor tests assert the shape of a
 * response, the `core` tests assert the ORM's reading of a response written by
 * hand, and both were written against the same assumption. `__subjectClass` and
 * `__subjectClasses` are declared independently on each side, so the contract
 * between them was the one thing nothing exercised.
 *
 * These drive the whole pipeline: real perspective, real SHACL registration,
 * real classification in the executor, real model classes coming out.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --serial --exit \
 *     --require tests/model/hooks.ts tests/model/model-polymorphic.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  BelongsToMany,
  Flag,
  HasMany,
  Link,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { wipePerspective } from "../../utils/utils.js";

// ── Models ────────────────────────────────────────────────────────────────────
//
// Four block classes distinguished only by their flag, so classification has to
// do real work: nothing about the link says what a target is, and the shapes
// differ in which property each one carries.

@Model({ name: "PolyTextBlock" })
class PolyTextBlock extends Ad4mModel {
  @Flag({ through: "test://poly/block_type", value: "test://poly/text_block" })
  blockType = "test://poly/text_block";

  @Property({ through: "test://poly/text" })
  text: string = "";
}

@Model({ name: "PolyImageBlock" })
class PolyImageBlock extends Ad4mModel {
  @Flag({ through: "test://poly/block_type", value: "test://poly/image_block" })
  blockType = "test://poly/image_block";

  @Property({ through: "test://poly/src" })
  src: string = "";
}

/** Deliberately absent from every `instantiateAs` below. */
@Model({ name: "PolyTaskBlock" })
class PolyTaskBlock extends Ad4mModel {
  @Flag({ through: "test://poly/block_type", value: "test://poly/task_block" })
  blockType = "test://poly/task_block";

  @Property({ through: "test://poly/label" })
  label: string = "";
}

/**
 * Unrelated to the blocks — a different flag predicate entirely. Its whole
 * purpose is to be satisfiable *at the same time* as `PolyTextBlock`, which
 * membership being structural makes possible.
 *
 * Both classes require exactly one flag triple and nothing else, so the
 * specificity ranking ties between them and the alphabetical tie-break decides.
 * `PolyBookmark` sorts before `PolyTextBlock`.
 */
@Model({ name: "PolyBookmark" })
class PolyBookmark extends Ad4mModel {
  @Flag({ through: "test://poly/bookmark_type", value: "test://poly/bookmark" })
  bookmarkType = "test://poly/bookmark";

  @Property({ through: "test://poly/url" })
  url: string = "";
}

/**
 * A base class and a subclass of it, discriminated by required properties rather
 * than flags: `PolyImagePost` requires everything `PolyPost` does and a source
 * besides, so an image post conforms to both and matches `PolyPost` by less.
 *
 * This is the pair the specificity default reads the wrong way round for a
 * caller that declared it holds posts.
 */
@Model({ name: "PolyPost" })
class PolyPost extends Ad4mModel {
  @Property({ through: "test://poly/headline", required: true })
  headline: string = "";
}

@Model({ name: "PolyImagePost" })
class PolyImagePost extends Ad4mModel {
  @Property({ through: "test://poly/headline", required: true })
  headline: string = "";

  @Property({ through: "test://poly/cover", required: true })
  cover: string = "";
}

/** Holds posts — and says so, both in its target and in what it can build. */
@Model({ name: "PolyFeed" })
class PolyFeed extends Ad4mModel {
  @Flag({ through: "test://poly/feed_type", value: "test://poly/feed" })
  feedType = "test://poly/feed";

  @HasMany(() => PolyPost, {
    through: "test://poly/entries",
    polymorphic: true,
    instantiateAs: () => [PolyPost],
  })
  entries: any[] = [];
}

@Model({ name: "PolyCollection" })
class PolyCollection extends Ad4mModel {
  @Flag({ through: "test://poly/collection_type", value: "test://poly/collection" })
  collectionType = "test://poly/collection";

  // No target class: the members are of genuinely different types, which is the
  // situation `polymorphic` exists for. `PolyTaskBlock` is missing from the list
  // on purpose — see the test that reads one back.
  @HasMany({
    through: "test://poly/children",
    polymorphic: true,
    instantiateAs: () => [PolyTextBlock, PolyImageBlock, PolyBookmark],
  })
  children: string[] = [];
}

/** Untyped *and* not polymorphic — the declaration that cannot be resolved. */
@Model({ name: "PolyLooseCollection" })
class PolyLooseCollection extends Ad4mModel {
  @Flag({ through: "test://poly/loose_type", value: "test://poly/loose" })
  looseType = "test://poly/loose";

  @HasMany({ through: "test://poly/children" })
  children: string[] = [];
}

/** Carries a typed relation to `PolyCollection`, so `children` is reached at depth. */
@Model({ name: "PolyPage" })
class PolyPage extends Ad4mModel {
  @Flag({ through: "test://poly/page_type", value: "test://poly/page" })
  pageType = "test://poly/page";

  @HasMany(() => PolyCollection, { through: "test://poly/sections" })
  sections: PolyCollection[] = [];
}

/** Reverse direction: whatever points at this marker, whatever class it is. */
@Model({ name: "PolyMarker" })
class PolyMarker extends Ad4mModel {
  @Flag({ through: "test://poly/marker_type", value: "test://poly/marker" })
  markerType = "test://poly/marker";

  @BelongsToMany({
    through: "test://poly/marks",
    polymorphic: true,
    instantiateAs: () => [PolyTextBlock, PolyImageBlock],
  })
  markedBy: any[] = [];
}

const ALL_MODELS = [
  PolyPost,
  PolyImagePost,
  PolyFeed,
  PolyTextBlock,
  PolyImageBlock,
  PolyTaskBlock,
  PolyBookmark,
  PolyCollection,
  PolyLooseCollection,
  PolyPage,
  PolyMarker,
];

// ── Tests ─────────────────────────────────────────────────────────────────────

describe("Ad4mModel — polymorphic relations", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  const registerAll = async () => {
    for (const M of ALL_MODELS) await (M as any).register(perspective);
  };

  /** Link a child into a collection. The relation is untyped, so this is a plain link. */
  const linkChild = async (collectionId: string, childId: string) =>
    perspective.add(
      new Link({
        source: collectionId,
        predicate: "test://poly/children",
        target: childId,
      }),
    );

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-polymorphic");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-polymorphic-test");
    await registerAll();
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  beforeEach(async () => {
    await wipePerspective(perspective);
    await registerAll();
  });

  // ── 1. The headline claim ───────────────────────────────────────────────────

  it("hydrates each child as the class it actually is", async () => {
    const collection = await PolyCollection.create(perspective, {});
    const text = await PolyTextBlock.create(perspective, { text: "hello" });
    const image = await PolyImageBlock.create(perspective, { src: "cat.png" });
    await linkChild(collection.id, text.id);
    await linkChild(collection.id, image.id);

    const found = await PolyCollection.findOne(perspective, {
      where: { id: collection.id },
      include: { children: true },
    });

    expect(found).to.not.be.null;
    const children = found!.children as any[];
    expect(children.length).to.equal(2);

    const gotText = children.find((c) => c.id === text.id);
    const gotImage = children.find((c) => c.id === image.id);

    expect(gotText).to.be.instanceOf(PolyTextBlock);
    expect(gotImage).to.be.instanceOf(PolyImageBlock);
    // The property that exists only on the concrete class. Hydrated against the
    // base class — or against either sibling — it would be absent, not wrong,
    // which is the failure this whole feature is about.
    expect(gotText.text).to.equal("hello");
    expect(gotImage.src).to.equal("cat.png");
  });

  it("asks for polymorphic hydration without the call site saying so", async () => {
    // `include: { children: true }` — the declaration on the relation is the
    // only thing making this a polymorphic read. If the default failed to reach
    // the executor, an untyped include would error rather than resolve.
    const collection = await PolyCollection.create(perspective, {});
    const text = await PolyTextBlock.create(perspective, { text: "no ceremony" });
    await linkChild(collection.id, text.id);

    const found = await PolyCollection.findOne(perspective, {
      where: { id: collection.id },
      include: { children: true },
    });

    expect((found!.children as any[])[0]).to.be.instanceOf(PolyTextBlock);
  });

  // ── 2. instantiateAs is advisory ────────────────────────────────────────────

  it("returns a child whose class is not listed, as data", async () => {
    // `PolyTaskBlock` is missing from `PolyCollection`'s `instantiateAs`. The
    // list says what this side can construct, not what the relation may hold, so
    // the task block must still arrive — hydrated correctly, just untyped.
    const collection = await PolyCollection.create(perspective, {});
    const text = await PolyTextBlock.create(perspective, { text: "known" });
    const task = await PolyTaskBlock.create(perspective, { label: "unknown" });
    await linkChild(collection.id, text.id);
    await linkChild(collection.id, task.id);

    const found = await PolyCollection.findOne(perspective, {
      where: { id: collection.id },
      include: { children: true },
    });

    const children = found!.children as any[];
    expect(children.length).to.equal(2, "an unlisted class is not dropped");

    const gotTask = children.find((c) => c.id === task.id);
    expect(gotTask).to.not.be.instanceOf(PolyTaskBlock);
    expect(gotTask.label).to.equal("unknown", "correct data, just untyped");
    expect(gotTask.__subjectClass).to.equal(
      "PolyTaskBlock",
      "and it names its class, so a caller can dispatch on it",
    );
  });

  // ── 3. One base expression, two classes ─────────────────────────────────────

  it("reports every class a child conforms to, and still returns one member per link", async () => {
    const collection = await PolyCollection.create(perspective, {});

    // One node satisfying two unrelated classes: created as a text block, then
    // given the bookmark's flag as well. Nothing forbids this — membership is
    // structural, so both readings are simply true.
    const both = await PolyTextBlock.create(perspective, { text: "hello" });
    await perspective.add(
      new Link({
        source: both.id,
        predicate: "test://poly/bookmark_type",
        target: "test://poly/bookmark",
      }),
    );
    await linkChild(collection.id, both.id);

    const found = await PolyCollection.findOne(perspective, {
      where: { id: collection.id },
      include: { children: true },
    });

    const children = found!.children as any[];
    // Links decide cardinality, classes decide only how a member is read. One
    // link is one child however many classes that child answers to.
    expect(children.length).to.equal(1, "two readings of one node is still one node");

    const child = children[0];
    expect(child.id).to.equal(both.id);
    // Both classes require one triple, so specificity ties and the alphabetical
    // fallback would have said PolyBookmark. It does not get that far: the
    // collection declares it builds text blocks before bookmarks, so that is
    // how a node answering to both is read. A convention losing to a statement
    // of what the read is for.
    expect(child.__subjectClass).to.equal("PolyTextBlock");
    expect(child).to.be.instanceOf(PolyTextBlock);
    expect(child.text).to.equal("hello");
    // The reading that lost, reported rather than dropped, and still ranked by
    // specificity — so the set stays a fact about the node while the choice
    // follows the request. A caller wanting the other has the id.
    expect(child.__subjectClasses).to.deep.equal(["PolyBookmark", "PolyTextBlock"]);
  });

  it("lets the call site name a different reading of the same child", async () => {
    // The same node, the same collection, one word changed in the query. What a
    // preference is for: two views over one collection can read a member as the
    // thing each of them is about, without either being wrong.
    const collection = await PolyCollection.create(perspective, {});
    const both = await PolyBookmark.create(perspective, { url: "https://ad4m.dev" });
    await perspective.add(
      new Link({
        source: both.id,
        predicate: "test://poly/block_type",
        target: "test://poly/text_block",
      }),
    );
    await linkChild(collection.id, both.id);

    const found = await PolyCollection.findOne(perspective, {
      where: { id: collection.id },
      include: { children: { preferClasses: ["PolyBookmark"] } },
    });

    const child = (found!.children as any[])[0];
    expect(child.__subjectClass).to.equal("PolyBookmark");
    expect(child).to.be.instanceOf(PolyBookmark);
    // Read through the bookmark's shape, so the bookmark's property is what
    // came back — the choice decides which predicates are fetched, not just
    // which constructor runs.
    expect(child.url).to.equal("https://ad4m.dev");
  });

  // ── 4. Nested and reverse ───────────────────────────────────────────────────

  it("applies the polymorphic default to an include at depth", async () => {
    // `children` is read off `PolyCollection`, not off the class being queried,
    // so the default has to be carried down with the class it belongs to. Left
    // at the top level it never reaches here and the include arrives at the
    // executor with no shape to resolve.
    const page = await PolyPage.create(perspective, {});
    const collection = await PolyCollection.create(perspective, {});
    const image = await PolyImageBlock.create(perspective, { src: "nested.png" });
    await linkChild(collection.id, image.id);
    await perspective.add(
      new Link({
        source: page.id,
        predicate: "test://poly/sections",
        target: collection.id,
      }),
    );

    const found = await PolyPage.findOne(perspective, {
      where: { id: page.id },
      include: { sections: { include: { children: true } } },
    });

    const sections = found!.sections as any[];
    expect(sections.length).to.equal(1);
    const nested = sections[0].children as any[];
    expect(nested.length).to.equal(1);
    expect(nested[0]).to.be.instanceOf(PolyImageBlock);
    expect(nested[0].src).to.equal("nested.png");
  });

  it("hydrates a reverse include polymorphically too", async () => {
    const marker = await PolyMarker.create(perspective, {});
    const text = await PolyTextBlock.create(perspective, { text: "marks it" });
    const image = await PolyImageBlock.create(perspective, { src: "marks.png" });
    for (const src of [text.id, image.id]) {
      await perspective.add(
        new Link({ source: src, predicate: "test://poly/marks", target: marker.id }),
      );
    }

    const found = await PolyMarker.findOne(perspective, {
      where: { id: marker.id },
      include: { markedBy: true },
    });

    const markedBy = found!.markedBy as any[];
    expect(markedBy.length).to.equal(2);
    expect(markedBy.find((m) => m.id === text.id)).to.be.instanceOf(PolyTextBlock);
    expect(markedBy.find((m) => m.id === image.id)).to.be.instanceOf(PolyImageBlock);
    expect(markedBy.find((m) => m.id === text.id).text).to.equal("marks it");
  });

  // ── 5. Reading a target as the class the caller declared ────────────────────

  it("yields the declared class for a target that is also something more specific", async () => {
    // The whole pipeline, which is where this has to be true: a child that is a
    // PolyImagePost conforms to PolyPost as well and matches it by fewer
    // required triples, so specificity alone reads it as the subclass — a class
    // the feed never declared and cannot construct. Declaring what it holds is
    // what makes the answer come back as a PolyPost.
    const feed = await PolyFeed.create(perspective, {});
    const post = await PolyImagePost.create(perspective, {
      headline: "Chocolate cake",
      cover: "cake.png",
    });
    await perspective.add(
      new Link({
        source: feed.id,
        predicate: "test://poly/entries",
        target: post.id,
      }),
    );

    const found = await PolyFeed.findOne(perspective, {
      where: { id: feed.id },
      include: { entries: true },
    });

    const entries = found!.entries as any[];
    expect(entries.length).to.equal(1);

    const entry = entries[0];
    expect(entry).to.be.instanceOf(PolyPost, "constructed as the class the feed declared");
    expect(entry.headline).to.equal("Chocolate cake");
    expect(entry.__subjectClass).to.equal("PolyPost");
    // The set stays ranked by specificity whatever was preferred, so it remains
    // a fact about the node — and the caller can see the more specific reading
    // exists, and fetch it by id if it ever wants it.
    expect(entry.__subjectClasses).to.deep.equal(["PolyImagePost", "PolyPost"]);
  });

  // ── 6. The failure that used to be unreadable ───────────────────────────────

  it("names the relation and the fix when an untyped include is not polymorphic", async () => {
    const collection = await PolyLooseCollection.create(perspective, {});
    const text = await PolyTextBlock.create(perspective, { text: "orphan" });
    await linkChild(collection.id, text.id);

    let message = "";
    try {
      await PolyLooseCollection.findOne(perspective, {
        where: { id: collection.id },
        include: { children: true },
      });
    } catch (e: any) {
      message = String(e?.message ?? e);
    }

    expect(message).to.not.equal("", "an unresolvable include must not succeed silently");
    // It used to surface as a shape lookup for the empty string, naming neither
    // what failed nor what to do about it.
    expect(message).to.contain("children");
    expect(message).to.contain("polymorphic");
  });
});
