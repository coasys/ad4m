/**
 * Ad4mModel — `local` property flag, end to end (issue #1026)
 *
 * The flag existed at three layers and only one of them did anything: write
 * status was decided from the *action's* `local`, so a property-shape
 * `local: true` was inert on its own and the model-query layer parsed the flag
 * out of the SHACL graph and threw it away.
 *
 * Every other test for this lives in Rust, against the store or the MCP
 * handlers. This one drives the real decorator API — `@Property({ local: true })`
 * through `register()` / `save()` — and asserts the persisted `LinkStatus`,
 * which is the layer the drift actually happened at and the layer no test
 * covered.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-local-property.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  Ad4mModel,
  Flag,
  HasMany,
  HasManyMethods,
  LinkQuery,
  Model,
  PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";

// A cache-like class: one ordinary networked property, plus local scalars and a
// local collection. `state` is deliberately `required` — that is the shape that
// used to be written Shared at construction and Local on every later setter,
// leaving one property with mixed link status depending on when it was written.
@Model({ name: "LocalFlagCache" })
class LocalFlagCache extends Ad4mModel {
  @Flag({ through: "lfc://type", value: "lfc://cache" })
  type = "lfc://cache";

  /** Ordinary shared property — the control. */
  @Property({ through: "lfc://title", required: true })
  title: string = "";

  /** Local scalar, required: exercises the constructor path. */
  @Property({ through: "lfc://state", required: true, local: true })
  state: string = "";

  /** Local scalar, optional: exercises the plain setter path. */
  @Property({ through: "lfc://note", local: true })
  note: string = "";

  /** Local collection: exercises the adder path. */
  @HasMany(() => LocalFlagMark, { through: "lfc://mark", local: true })
  marks: LocalFlagMark[] = [];
}
interface LocalFlagCache extends HasManyMethods<"marks"> {}

@Model({ name: "LocalFlagMark" })
class LocalFlagMark extends Ad4mModel {
  @Flag({ through: "lfc://mark_type", value: "lfc://mark" })
  type = "lfc://mark";

  @Property({ through: "lfc://label", required: true })
  label: string = "";
}

describe("Ad4mModel — local property flag", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-local-property");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-local-property-test");
    await LocalFlagMark.register(perspective);
    await LocalFlagCache.register(perspective);
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  /** Status of every link on `base` carrying `predicate`. */
  const statusesFor = async (base: string, predicate: string): Promise<string[]> => {
    const links = await perspective.get(
      new LinkQuery({ source: base, predicate })
    );
    return links.map((l) => String(l.status ?? "shared").toLowerCase());
  };

  it("writes declared-local properties as local links and shared ones as shared", async () => {
    const cache = new LocalFlagCache(perspective);
    cache.title = "a cache";
    cache.state = "warm";
    cache.note = "scratch";
    await cache.save();

    const base = cache.id;
    expect(base).to.be.a("string").and.not.empty;

    // Control: an ordinary property must stay networked.
    const titleStatuses = await statusesFor(base, "lfc://title");
    expect(titleStatuses).to.not.be.empty;
    expect(titleStatuses.every((s) => s === "shared")).to.equal(
      true,
      `title should be shared, got ${JSON.stringify(titleStatuses)}`
    );

    // Required local scalar — written through the constructor path.
    const stateStatuses = await statusesFor(base, "lfc://state");
    expect(stateStatuses).to.not.be.empty;
    expect(stateStatuses.every((s) => s === "local")).to.equal(
      true,
      `required local property must be local at creation, got ${JSON.stringify(stateStatuses)}`
    );

    // Optional local scalar — written through the setter path.
    const noteStatuses = await statusesFor(base, "lfc://note");
    expect(noteStatuses).to.not.be.empty;
    expect(noteStatuses.every((s) => s === "local")).to.equal(
      true,
      `optional local property must be local, got ${JSON.stringify(noteStatuses)}`
    );
  });

  it("keeps a local property local across an update", async () => {
    const cache = new LocalFlagCache(perspective);
    cache.title = "updatable";
    cache.state = "cold";
    await cache.save();

    await LocalFlagCache.update(perspective, cache.id, { state: "hot" });

    const statuses = await statusesFor(cache.id, "lfc://state");
    expect(statuses).to.not.be.empty;
    expect(statuses.every((s) => s === "local")).to.equal(
      true,
      `local property must stay local after update, got ${JSON.stringify(statuses)}`
    );

    // And the value still reads back — the read-side status filter must not
    // hide a genuinely local value from its own executor.
    const reloaded = await LocalFlagCache.findOne(perspective, {
      where: { id: cache.id },
    });
    expect(reloaded?.state).to.equal("hot");
  });

  it("writes a local collection's links as local", async () => {
    const cache = new LocalFlagCache(perspective);
    cache.title = "with marks";
    cache.state = "warm";
    await cache.save();

    const mark = new LocalFlagMark(perspective);
    mark.label = "one";
    await mark.save();

    await cache.addMarks(mark.id);

    const statuses = await statusesFor(cache.id, "lfc://mark");
    expect(statuses).to.not.be.empty;
    expect(statuses.every((s) => s === "local")).to.equal(
      true,
      `local collection links must be local, got ${JSON.stringify(statuses)}`
    );
  });

  it("hydrates local properties for the executor that owns them", async () => {
    const cache = new LocalFlagCache(perspective);
    cache.title = "readable";
    cache.state = "warm";
    cache.note = "visible-locally";
    await cache.save();

    const all = await LocalFlagCache.findAll(perspective);
    const found = all.find((c) => c.id === cache.id);
    expect(found, "instance should be found by findAll").to.not.be.undefined;
    expect(found!.title).to.equal("readable");
    expect(found!.state).to.equal("warm");
    expect(found!.note).to.equal("visible-locally");
  });
});
