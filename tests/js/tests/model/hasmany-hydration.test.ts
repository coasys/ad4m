/**
 * HasMany hydration — round-trip for both string-valued and URI-valued
 * collections. Guards the fix in
 * `rust-executor/src/perspectives/model_query/hydration.rs` where the
 * relation-collection branch was skipping the `literal:*` wire-form
 * decode, so a `HasMany<string>` (e.g. under PR #874 typed literals)
 * came back to the caller as `["literal:string:<hex>", ...]` instead
 * of `["<hex>", ...]`.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 60000 --exit \
 *     --require tests/model/hooks.ts tests/model/hasmany-hydration.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import { Ad4mModel, HasMany, Model, Property } from "@coasys/ad4m";
import { getSharedAgent } from "./hooks.js";
import { startAgent } from "../../helpers/index.js";

// A minimal @Model class with two HasMany collections: one where the
// targets are IRIs (the common case — Recipe→Ingredient, Channel→
// Message, etc.) and one where the targets are plain strings that
// under PR #874 land on the wire as `literal:string:<value>`.
@Model({ name: "HydrationProbe" })
class HydrationProbe extends Ad4mModel {
  // Identity so instances survive the shape's conformance check without
  // requiring the fixture to seed a rdf:type link.
  @Property({ through: "test://probe/id", required: true, identity: true })
  id_: string = "";

  // HasMany-of-URI: targets look like subject-instance URIs.
  @HasMany({ through: "test://probe/link" })
  refs: string[] = [];

  // HasMany-of-string: targets stored as `literal:string:<value>` wire
  // form. Same predicate would ordinarily point at URIs; this test
  // seeds the wire-form fixture directly so we don't depend on the
  // writer path also being fixed.
  @HasMany({ through: "test://probe/tag" })
  tags: string[] = [];
}

describe("HasMany hydration — string and URI targets round-trip cleanly", function () {
  this.timeout(60_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("hasmany-hydration");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("hasmany-hydration-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await HydrationProbe.register(p);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  it("URI targets in a HasMany round-trip byte-for-byte", async () => {
    const base = "test://probe/instance/uri";
    await p.add(new Link({ source: base, predicate: "test://probe/id", target: "literal:string:uri-probe" }));
    await p.add(new Link({ source: base, predicate: "test://probe/link", target: "flux://message/abc" }));
    await p.add(new Link({ source: base, predicate: "test://probe/link", target: "did:key:z6MkExample" }));

    const probes = await HydrationProbe.findAll(p);
    expect(probes.length, "should find the one probe").to.equal(1);
    // Sort for order-independence — the store may return links in
    // arbitrary insertion order.
    const refs = [...(probes[0].refs ?? [])].sort();
    expect(refs).to.deep.equal(["did:key:z6MkExample", "flux://message/abc"]);
  });

  it("literal:string:<value> targets in a HasMany decode to plain strings", async () => {
    // This is the bug from PR #881's two-executor auto-processor test:
    // `InterpretationRun.sources` holds turn IDs as plain hex, the
    // writer wraps them in `literal:string:<hex>` under PR #874, and
    // before the fix the reader returned the encoded form.
    const base = "test://probe/instance/lit";
    await p.add(new Link({ source: base, predicate: "test://probe/id", target: "literal:string:lit-probe" }));
    await p.add(new Link({ source: base, predicate: "test://probe/tag", target: "literal:string:turn-hex-1" }));
    await p.add(new Link({ source: base, predicate: "test://probe/tag", target: "literal:string:turn-hex-2" }));

    const probes = await HydrationProbe.findAll(p);
    expect(probes.length, "should find the one probe").to.equal(1);
    const tags = [...(probes[0].tags ?? [])].sort();
    expect(tags).to.deep.equal(
      ["turn-hex-1", "turn-hex-2"],
      "wire-form `literal:string:<value>` HasMany targets must decode to plain values",
    );
  });

  it("mixed URI + literal targets on the same predicate are each decoded on their own prefix", async () => {
    // Migration-in-flight defensive: if some instances still carry
    // plain URIs alongside newly-wire-encoded literals on the same
    // predicate, each entry must be decided by its own prefix, not by
    // the property's `is_relation` flag.
    const base = "test://probe/instance/mixed";
    await p.add(new Link({ source: base, predicate: "test://probe/id", target: "literal:string:mixed-probe" }));
    await p.add(new Link({ source: base, predicate: "test://probe/tag", target: "https://example.com/thing" }));
    await p.add(new Link({ source: base, predicate: "test://probe/tag", target: "literal:string:legacy-string" }));

    const probes = await HydrationProbe.findAll(p);
    expect(probes.length).to.equal(1);
    const tags = [...(probes[0].tags ?? [])].sort();
    expect(tags).to.deep.equal(["https://example.com/thing", "legacy-string"]);
  });
});
