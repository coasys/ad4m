/**
 * `PerspectiveProxy.interpretationOverlays()` end to end (#1017).
 *
 * Invariants:
 *   1. The executor lists every base that has an `ad4m://interp/kind` link,
 *      sorted by base, with its `run` and its `inferred/<p>` staged values,
 *      and nothing from the base's normal links. It reads all bases in one
 *      batch instead of one read per base, so this checks the batched read
 *      against a hand-written expectation.
 *   2. Concurrent callers share one RPC and each gets its own array.
 *   3. Nothing is cached on the client: a change is visible on the next call,
 *      and the TTL cache API from the first #1017 version (`{ fresh }`,
 *      `invalidateOverlaysCache()`) does not exist.
 *
 * The overlay links are written by hand, so no LLM is needed. `kind` uses the
 * plain `create` / `update` target that the class constructor writes.
 *
 * Run standalone (from tests/js, with a built executor):
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit \
 *     --require tests/model/hooks.ts tests/model/interpretation-overlays-list.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Link, LinkQuery, PerspectiveProxy } from "@coasys/ad4m";
import { getSharedAgent } from "./hooks.js";
import { startAgent } from "../../helpers/index.js";

const KIND = "ad4m://interp/kind";
const RUN = "ad4m://interp/run";
const inferred = (p: string) => `ad4m://interp/inferred/${p}`;

describe("PerspectiveProxy.interpretationOverlays()", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("interpretation-overlays-list");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("interp-overlays-list-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await p.addLinks([
      // b: an update overlay with no run, next to the base's normal data.
      new Link({ source: "test://task/b", predicate: "soa://title", target: "literal:string:Human" }),
      new Link({ source: "test://task/b", predicate: "soa://points", target: "literal:number:1" }),
      new Link({ source: "test://task/b", predicate: KIND, target: "update" }),
      new Link({ source: "test://task/b", predicate: inferred("soa://title"), target: "literal:string:LLM" }),
      // a: a create overlay with a run.
      new Link({ source: "test://task/a", predicate: KIND, target: "create" }),
      new Link({ source: "test://task/a", predicate: RUN, target: "ad4m://interp/run/r1" }),
      new Link({ source: "test://task/a", predicate: inferred("soa://points"), target: "literal:number:3" }),
      // c: an inferred link but no kind, so not an overlay.
      new Link({ source: "test://task/c", predicate: inferred("soa://title"), target: "literal:string:Orphan" }),
    ]);
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  it("lists each overlay's kind, run and staged values, sorted by base", async () => {
    const overlays = await p.interpretationOverlays();
    expect(overlays).to.deep.equal([
      { base: "test://task/a", kind: "create", run: "ad4m://interp/run/r1", inferred: [["soa://points", 3]] },
      { base: "test://task/b", kind: "update", run: null, inferred: [["soa://title", "LLM"]] },
    ]);
  });

  it("concurrent callers get the same list, each in its own array", async () => {
    const results = await Promise.all([1, 2, 3, 4, 5].map(() => p.interpretationOverlays()));
    for (const r of results) expect(r).to.deep.equal(results[0]);
    expect(results[0]).to.have.length(2);
    expect(new Set(results).size, "distinct array objects").to.equal(results.length);
    results[0].length = 0;
    expect(results[1]).to.have.length(2);
  });

  it("does not cache: a change is visible on the very next call", async () => {
    expect((await p.interpretationOverlays()).map((o) => o.base)).to.deep.equal([
      "test://task/a",
      "test://task/b",
    ]);

    await p.add(new Link({ source: "test://task/c", predicate: KIND, target: "update" }));
    expect((await p.interpretationOverlays()).map((o) => o.base)).to.deep.equal([
      "test://task/a",
      "test://task/b",
      "test://task/c",
    ]);

    // Rejecting an update overlay drops it and keeps the real value.
    expect(await p.rejectInterpretation("test://task/b")).to.equal(true);
    expect((await p.interpretationOverlays()).map((o) => o.base)).to.deep.equal([
      "test://task/a",
      "test://task/c",
    ]);
    const title = await p.get(new LinkQuery({ source: "test://task/b", predicate: "soa://title" }));
    expect(title.map((l) => l.data.target)).to.deep.equal(["literal:string:Human"]);
  });

  it("has no client-side cache API", () => {
    expect((p as any).invalidateOverlaysCache).to.equal(undefined);
    expect(p.interpretationOverlays.length, "takes no options").to.equal(0);
  });
});
