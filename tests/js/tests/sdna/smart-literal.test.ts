/**
 * SmartLiteral — integration tests
 *
 * Tests for the SmartLiteral utility class: creation, instantiation,
 * and enumeration of smart literals inside a perspective.
 *
 * Run with:
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --serial --exit tests/smart-literal.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  LinkQuery,
  Literal,
  PerspectiveProxy,
  SmartLiteral,
  SMART_LITERAL_CONTENT_PREDICATE,
} from "@coasys/ad4m";
import fetch from "node-fetch";
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";

//@ts-ignore
global.fetch = fetch;

describe("Smart Literal", () => {
  let agent: AgentHandle;
  let ad4m: Ad4mClient;

  before(async () => {
    agent = await startAgent("smart-literal");
    ad4m = agent.client;
  });

  after(async () => {
    await agent.stop();
  });

  describe("SmartLiteral operations", () => {
    let perspective: PerspectiveProxy | null = null;

    before(async () => {
      perspective = await ad4m.perspective.add("smart literal test");
      // for test debugging:
      //console.log("UUID: " + perspective.uuid)
    });

    it("can create and use a new smart literal", async () => {
      let sl = await SmartLiteral.create(perspective!, "Hello World");
      let base = sl.base;

      expect(await sl.get()).to.equal("Hello World");

      let links = await perspective!.get(
        new LinkQuery({ predicate: SMART_LITERAL_CONTENT_PREDICATE }),
      );
      expect(links.length).to.equal(1);
      expect(links[0].data.source).to.equal(base);
      let literal = Literal.fromUrl(links[0].data.target);
      expect(literal.get()).to.equal("Hello World");

      await sl.set(5);
      expect(await sl.get()).to.equal(5);

      links = await perspective!.get(
        new LinkQuery({ predicate: SMART_LITERAL_CONTENT_PREDICATE }),
      );
      expect(links.length).to.equal(1);
      expect(links[0].data.source).to.equal(base);
      literal = Literal.fromUrl(links[0].data.target);
      expect(literal.get()).to.equal(5);
    });

    it("can instantiate smart literal from perspective", async () => {
      let source = Literal.from("base").toUrl();
      let target = Literal.from("Hello World 2").toUrl();
      await perspective!.add({
        source,
        predicate: SMART_LITERAL_CONTENT_PREDICATE,
        target,
      });

      let sl = new SmartLiteral(perspective!, source);
      expect(await sl.get()).to.equal("Hello World 2");
    });

    it("can get all smart literals in a perspective", async () => {
      let all = await SmartLiteral.getAllSmartLiterals(perspective!);
      expect(all.length).to.equal(2);
      expect(all[1].base).to.equal(Literal.from("base").toUrl());
      expect(await all[0].get()).to.equal(5);
      expect(await all[1].get()).to.equal("Hello World 2");
    });
  });

  // SKIPPED: Embedding cache tests - only applies to Prolog-pooled mode
  // These tests verify embedding URL post-processing with Prolog infer() queries.
  // With SHACL migration, embedding queries should use SurrealDB vector search instead.
  // Keeping as reference for future SurrealDB vector embedding implementation.
  describe.skip("Embedding cache", () => {
    let perspective: PerspectiveProxy | null = null;
    const EMBEDDING_LANG =
      "QmzSYwdbqjGGbYbWJvdKA4WnuFwmMx3AsTfgg7EwbeNUGyE555c";

    before(async () => {
      perspective = await ad4m.perspective.add("embedding-cache-test");
    });

    it("correctly post-processes nested query results containing embedding URLs", async () => {
      // Create some links with embedding URLs
      const embeddingUrl1 = `${EMBEDDING_LANG}://vector1/1.2,3.4,5.6`;
      const embeddingUrl2 = `${EMBEDDING_LANG}://vector2/7.8,9.0,1.2`;
      const embeddingUrl3 = `${EMBEDDING_LANG}://vector3/2.3,4.5,6.7`;

      // Create a link structure that will produce nested results
      await perspective!.add({
        source: "test://root",
        predicate: "test://has-vector",
        target: embeddingUrl1,
      });

      await perspective!.add({
        source: embeddingUrl1,
        predicate: "test://related-to",
        target: embeddingUrl2,
      });

      await perspective!.add({
        source: embeddingUrl2,
        predicate: "test://points-to",
        target: embeddingUrl3,
      });

      // Query that will produce nested results with embedding URLs at different levels
      const result = await perspective!.infer(`
                % Find all vectors connected to root
                findall(
                    [FirstVector, RelatedVectors],
                    (
                        % Get first vector from root
                        triple("test://root", "test://has-vector", FirstVector),
                        % Find all vectors related to the first one
                        findall(
                            [SecondVector, ThirdVector],
                            (
                                triple(FirstVector, "test://related-to", SecondVector),
                                triple(SecondVector, "test://points-to", ThirdVector)
                            ),
                            RelatedVectors
                        )
                    ),
                    Results
                ).
            `);

      // The query should return a deeply nested structure:
      // Results = [
      //   [embeddingUrl1, [
      //     [embeddingUrl2, embeddingUrl3]
      //   ]]
      // ]
      console.log("result", result);
      expect(result).to.be.an("array");
      expect(result.length).to.be.greaterThan(0);

      let binding = result[0];
      expect(binding.Results).to.be.an("array");
      expect(binding.Results).to.have.lengthOf(1);

      const [firstLevel] = binding.Results;
      expect(firstLevel).to.be.an("array");
      expect(firstLevel[0]).to.equal(embeddingUrl1);
      expect(firstLevel[1]).to.be.an("array");

      const relatedVectors = firstLevel[1];
      expect(relatedVectors).to.have.lengthOf(1);
      expect(relatedVectors[0]).to.be.an("array");
      expect(relatedVectors[0][0]).to.equal(embeddingUrl2);
      expect(relatedVectors[0][1]).to.equal(embeddingUrl3);
    });
  });
});
