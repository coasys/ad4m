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
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";

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
});
