import { Perspective, LinkQuery } from "@coasys/ad4m";
import fs from "fs";
import { TestContext } from "./integration.test";
import { sleep } from "../../utils/utils";
import { expect } from "chai";
import { v4 as uuidv4 } from "uuid";

const DIFF_SYNC_OFFICIAL = fs
  .readFileSync("./scripts/perspective-diff-sync-hash")
  .toString();

export default function tripleAgentTests(testContext: TestContext) {
  return () => {
    it("three agents can join and use a neighbourhood", async () => {
      const alice = testContext.alice;
      const bob = testContext.bob;
      const jim = testContext.jim;

      const aliceP1 = await alice.perspective.add("three-agents");
      const socialContext = await alice.languages.applyTemplateAndPublish(
        DIFF_SYNC_OFFICIAL,
        JSON.stringify({
          uid: uuidv4(),
          name: "Alice's neighbourhood with Bob",
        }),
      );
      expect(socialContext.name).to.be.equal("Alice's neighbourhood with Bob");
      const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(
        aliceP1.uuid,
        socialContext.address,
        new Perspective(),
      );

      let bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);
      let jimP1 = await jim.neighbourhood.joinFromUrl(neighbourhoodUrl);

      await testContext.makeAllThreeNodesKnown();

      expect(bobP1!.name).not.to.be.undefined;
      expect(bobP1!.sharedUrl).to.be.equal(neighbourhoodUrl);
      expect(bobP1!.neighbourhood).not.to.be.undefined;
      expect(bobP1!.neighbourhood!.data!.linkLanguage).to.be.equal(
        socialContext.address,
      );
      expect(bobP1!.neighbourhood!.data!.meta.links.length).to.be.equal(0);

      expect(jimP1!.name).not.to.be.undefined;
      expect(jimP1!.sharedUrl).to.be.equal(neighbourhoodUrl);
      expect(jimP1!.neighbourhood).not.to.be.undefined;
      expect(jimP1!.neighbourhood!.data!.linkLanguage).to.be.equal(
        socialContext.address,
      );
      expect(jimP1!.neighbourhood!.data!.meta.links.length).to.be.equal(0);

      await sleep(1000);

      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });

      await sleep(1000);

      let bobLinks = await bob.perspective.queryLinks(
        bobP1!.uuid,
        new LinkQuery({ source: "ad4m://root" }),
      );
      // Increase retries and sleep for CI robustness
      const MAX_RETRIES = process.env.CI ? 80 : 40;
      const SLEEP_MS = process.env.CI ? 4000 : 3000;

      let tries = 1;
      while (bobLinks.length < 10 && tries < MAX_RETRIES) {
        console.log(
          `Bob retrying getting links (attempt ${tries}/${MAX_RETRIES}, have ${bobLinks.length}/10)...`,
        );
        await sleep(SLEEP_MS);
        bobLinks = await bob.perspective.queryLinks(
          bobP1!.uuid,
          new LinkQuery({ source: "ad4m://root" }),
        );
        tries++;
      }
      if (bobLinks.length !== 10) {
        console.error(
          `Bob final: got ${bobLinks.length}/10 links after ${tries} tries`,
        );
      }
      expect(bobLinks.length).to.be.equal(
        10,
        `Bob saw ${bobLinks.length}/10 links after ${tries} tries`,
      );

      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });

      // Re-exchange agent infos so Jim's DHT routing table is fresh after Alice
      // and Bob have made many new Holochain commits.  Without this, Jim's node
      // can accumulate "Could not find entry" failures because it doesn't know
      // which peers hold the new entries.
      await testContext.makeAllThreeNodesKnown();

      // Give the DHT time to gossip the new entries to Jim before the first check.
      await sleep(10000);
      let jimLinks = await jim.perspective.queryLinks(
        jimP1!.uuid,
        new LinkQuery({ source: "ad4m://root" }),
      );
      let jimRetries = 1;
      while (jimLinks.length < 20 && jimRetries < MAX_RETRIES) {
        console.log(
          `Jim retrying getting links (attempt ${jimRetries}/${MAX_RETRIES}, have ${jimLinks.length}/20)...`,
        );
        await sleep(SLEEP_MS);
        jimLinks = await jim.perspective.queryLinks(
          jimP1!.uuid,
          new LinkQuery({ source: "ad4m://root" }),
        );
        jimRetries++;
      }
      if (jimLinks.length !== 20) {
        console.error(
          `Jim final: got ${jimLinks.length}/20 links after ${jimRetries} tries`,
        );
      }
      expect(jimLinks.length).to.be.equal(
        20,
        `Jim saw ${jimLinks.length}/20 links after ${jimRetries} tries`,
      );

      // Refresh routing tables again now that Jim has caught up, so that Jim's
      // phase-3 writes will propagate back to Alice and Bob (Jim was a slow peer
      // throughout phase 2 — without a fresh exchange his new entries may not
      // reach the other nodes).
      await testContext.makeAllThreeNodesKnown();
      await sleep(10000);

      // Verify Alice also sees all 20 links before phase 3 begins — she may be
      // missing Bob's phase-2 contributions if the DHT was slow while Jim was
      // catching up.
      let aliceLinks = await alice.perspective.queryLinks(
        aliceP1!.uuid,
        new LinkQuery({ source: "ad4m://root" }),
      );
      tries = 1;
      while (aliceLinks.length < 20 && tries < MAX_RETRIES) {
        console.log(
          `Alice pre-phase3 sync (attempt ${tries}/20, have ${aliceLinks.length}/20)...`,
        );
        await sleep(SLEEP_MS);
        aliceLinks = await alice.perspective.queryLinks(
          aliceP1!.uuid,
          new LinkQuery({ source: "ad4m://root" }),
        );
        tries++;
      }
      if (aliceLinks.length !== 20) {
        console.error(
          `Alice final: got ${aliceLinks.length}/20 links after ${tries} tries`,
        );
      }
      expect(aliceLinks.length).to.be.equal(
        20,
        `Alice saw ${aliceLinks.length}/20 links after ${tries} tries`,
      );

      // Verify Bob also sees all 20 links before phase 3 begins.
      bobLinks = await bob.perspective.queryLinks(
        bobP1!.uuid,
        new LinkQuery({ source: "ad4m://root" }),
      );
      tries = 1;
      while (bobLinks.length < 20 && tries < MAX_RETRIES) {
        console.log(
          `Bob pre-phase3 sync (attempt ${tries}/20, have ${bobLinks.length}/20)...`,
        );
        await sleep(SLEEP_MS);
        bobLinks = await bob.perspective.queryLinks(
          bobP1!.uuid,
          new LinkQuery({ source: "ad4m://root" }),
        );
        tries++;
      }
      if (bobLinks.length !== 20) {
        console.error(
          `Bob final: got ${bobLinks.length}/20 links after ${tries} tries`,
        );
      }
      expect(bobLinks.length).to.be.equal(
        20,
        `Bob saw ${bobLinks.length}/20 links after ${tries} tries`,
      );

      //Alice bob and jim all collectively add 10 links and then check can be received by all agents
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await jim.perspective.addLink(jimP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await jim.perspective.addLink(jimP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await alice.perspective.addLink(aliceP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await bob.perspective.addLink(bobP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await jim.perspective.addLink(jimP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });
      await jim.perspective.addLink(jimP1.uuid, {
        source: "ad4m://root",
        target: "test://test",
      });

      aliceLinks = await alice.perspective.queryLinks(
        aliceP1!.uuid,
        new LinkQuery({ source: "ad4m://root" }),
      );
      tries = 1;

      while (aliceLinks.length < 30 && tries < MAX_RETRIES) {
        console.log(
          `Alice retrying getting links (attempt ${tries}/${MAX_RETRIES}, have ${aliceLinks.length}/30)...`,
        );
        await sleep(SLEEP_MS);
        aliceLinks = await alice.perspective.queryLinks(
          aliceP1!.uuid,
          new LinkQuery({ source: "ad4m://root" }),
        );
        tries++;
      }
      if (aliceLinks.length !== 30) {
        console.error(
          `Alice final: got ${aliceLinks.length}/30 links after ${tries} tries`,
        );
      }
      expect(aliceLinks.length).to.be.equal(
        30,
        `Alice saw ${aliceLinks.length}/30 links after ${tries} tries`,
      );

      // Bob waits for 30 links
      tries = 1;
      while (bobLinks.length < 30 && tries < MAX_RETRIES) {
        console.log(
          `Bob retrying getting links (attempt ${tries}/${MAX_RETRIES}, have ${bobLinks.length}/30)...`,
        );
        await sleep(SLEEP_MS);
        bobLinks = await bob.perspective.queryLinks(
          bobP1!.uuid,
          new LinkQuery({ source: "ad4m://root" }),
        );
        tries++;
      }
      if (bobLinks.length !== 30) {
        console.error(
          `Bob final: got ${bobLinks.length}/30 links after ${tries} tries`,
        );
      }
      expect(bobLinks.length).to.be.equal(
        30,
        `Bob saw ${bobLinks.length}/30 links after ${tries} tries`,
      );

      // Jim waits for 30 links
      tries = 1;
      while (jimLinks.length < 30 && tries < MAX_RETRIES) {
        console.log(
          `Jim retrying getting links (attempt ${tries}/${MAX_RETRIES}, have ${jimLinks.length}/30)...`,
        );
        await sleep(SLEEP_MS);
        jimLinks = await jim.perspective.queryLinks(
          jimP1!.uuid,
          new LinkQuery({ source: "ad4m://root" }),
        );
        tries++;
      }
      if (jimLinks.length !== 30) {
        console.error(
          `Jim final: got ${jimLinks.length}/30 links after ${tries} tries`,
        );
      }
      expect(jimLinks.length).to.be.equal(
        30,
        `Jim saw ${jimLinks.length}/30 links after ${tries} tries`,
      );
    });
  };
}
