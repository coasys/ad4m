/**
 * Ad4mModel — Prolog bridge tests
 *
 * Covers: generatePrologFacts() pure-function output (no executor needed for
 * the generation itself) and perspective.infer() integration using
 * model-generated Prolog facts.
 *
 * Ported from playground scenario 07 (Prolog Bridge).
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 120000 --exit tests/model/model-prolog.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  PerspectiveProxy,
  generatePrologFacts,
} from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { getSharedAgent } from "./hooks.js";
import { TestPost, TestTag } from "./models.js";


// ── Pure function tests (no executor) ─────────────────────────────────────────
//
// generatePrologFacts() is a stateless compiler — it reads decorator metadata
// attached to the class and produces a Prolog string. No perspective or
// network connection is required.

describe("Ad4mModel — generatePrologFacts() [pure]", function () {
  // No executor — these are synchronous pure-function tests.
  const postFacts = generatePrologFacts(TestPost);
  const tagFacts = generatePrologFacts(TestTag);

  it("returns a non-empty string", () => {
    expect(typeof postFacts).to.equal("string");
    expect(postFacts.length).to.be.greaterThan(0);
  });

  it("includes the @Flag predicate clause", () => {
    // TestPost: @Flag({ through: 'test://post_type', value: 'test://post' })
    expect(postFacts).to.include(
      "triple(X, 'test://post_type', 'test://post')",
    );
  });

  it("includes clauses for @Property predicates", () => {
    // TestPost has title and body @Property decorators
    expect(postFacts).to.include("'test://title'");
    expect(postFacts).to.include("'test://body'");
  });

  it("includes clauses for @HasMany predicates", () => {
    // TestPost has tags and comments @HasMany decorators
    expect(postFacts).to.include("'test://has_tag'");
    expect(postFacts).to.include("'test://has_comment'");
  });

  it("@BelongsToMany uses reverse clause form (V → X, not X → V)", () => {
    // TestTag.posts is @BelongsToMany(() => TestPost, { through: 'test://has_tag' }).
    // The link direction is Post→test://has_tag→Tag.  To find all Posts for a Tag,
    // Prolog must traverse in reverse: triple(V, 'test://has_tag', X).
    expect(tagFacts).to.include("triple(V, 'test://has_tag', X)");

    // The forward form must NOT appear for a reverse relation.
    expect(tagFacts).to.not.include("triple(X, 'test://has_tag', V)");
  });
});

// ── perspective.infer() integration ───────────────────────────────────────────
//
// Verifies the Prolog bridge end-to-end: facts generated from decorator
// metadata can be loaded into the Prolog engine and queried via infer().

describe("Ad4mModel — Prolog Bridge (executor)", function () {
  this.timeout(120_000);

  let ownStop: (() => Promise<void>) | null = null;
  let ad4m: Ad4mClient;
  let perspective: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("model-prolog");
      ad4m = agent.client;
      ownStop = agent.stop;
    }
    perspective = await ad4m.perspective.add("model-prolog-test");
  });

  after(async () => {
    if (ownStop) await ownStop();
  });

  it("perspective.infer() succeeds using generated Prolog facts", async () => {
    const facts = generatePrologFacts(TestPost);
    // Goal: test_post(X). — returns [] (empty, no triples yet) but not null.
    // A null return means a hard error (parse / compile failure).
    const result = await perspective.infer(`${facts}\ntest_post(X).`);
    expect(result).to.not.be.null;
  });

  it("infer() with TestTag reverse-relation facts returns a non-null result", async () => {
    const facts = generatePrologFacts(TestTag);
    // Goal: test_tag(X). — same reasoning as above.
    const result = await perspective.infer(`${facts}\ntest_tag(X).`);
    expect(result).to.not.be.null;
  });
});
