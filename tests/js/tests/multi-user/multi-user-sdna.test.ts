import path from "path";
import { Ad4mClient, LinkQuery, Model, Property } from "@coasys/ad4m";
import { fileURLToPath } from "url";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient } from "../../utils/utils";
import fetch from "node-fetch";
import { startAgent } from "../../helpers/executor";
import type { AgentHandle } from "../../helpers/executor";

//@ts-ignore
global.fetch = fetch;

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Multi-User SDNA Operations tests", () => {
  let agentHandle: AgentHandle | null = null;
  let adminAd4mClient: Ad4mClient | null = null;
  let gqlPort: number = 0;

  before(async function () {
    this.timeout(120_000);
    agentHandle = await startAgent("multi-user-sdna");
    adminAd4mClient = agentHandle.client;
    gqlPort = agentHandle.gqlPort;
    await adminAd4mClient.runtime.setMultiUserEnabled(true);
  });

  after(async () => {
    await agentHandle?.stop();
  });

  describe("Subject Creation and SDNA Operations", () => {
    // Define the test subject class outside the test function
    let TestSubject: any;

    before(async () => {
      // Import necessary decorators and classes
      const { Model, Property } = await import("@coasys/ad4m");

      // Define a proper subject class with decorators
      @Model({
        name: "TestSubject",
      })
      class TestSubjectClass {
        @Property({
          through: "test://name",
          writable: true,
          initial: "test://initial",
          resolveLanguage: "literal",
        })
        name: string = "";
      }

      TestSubject = TestSubjectClass;
    });

    it("should have correct authors and valid signatures for subject operations", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "subject1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "subject2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "subject1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "subject2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // User 1 creates perspective and ensures SDNA subject class
      // @ts-ignore - Suppress Apollo type mismatch
      const p1 = await client1.perspective.add(
        "User 1 Subject Test Perspective",
      );

      // User 1 ensures SDNA subject class
      await p1.ensureSDNASubjectClass(TestSubject);

      // User 1 creates a subject instance
      // @ts-ignore - Suppress Apollo type mismatch
      await p1.createSubject(new TestSubject(), "test://subject1", {
        name: "Test Subject 1",
      });

      // Get all links from the perspective to check authors
      // @ts-ignore - Suppress Apollo type mismatch
      const links1 = await p1.get(new LinkQuery({}));
      expect(links1.length).to.be.greaterThan(0);

      const user1Me = await client1.agent.me();

      // Verify all links are authored by user1
      for (const link of links1) {
        expect(link.author).to.equal(
          user1Me.did,
          `Link with predicate ${link.predicate} should be authored by user1`,
        );
        expect(link.proof.valid).to.be.true;
      }

      // User 2 creates perspective and does similar operations
      // @ts-ignore - Suppress Apollo type mismatch
      const p2 = await client2.perspective.add(
        "User 2 Subject Test Perspective",
      );

      // User 2 ensures the same SDNA subject class
      // @ts-ignore - Suppress Apollo type mismatch
      await p2.ensureSDNASubjectClass(TestSubject);

      // User 2 creates a subject instance
      // @ts-ignore - Suppress Apollo type mismatch
      await p2.createSubject(new TestSubject(), "test://subject2", {
        name: "Test Subject 2",
      });

      // Get all links from user2's perspective
      // @ts-ignore - Suppress Apollo type mismatch
      const links2 = await p2.get(new LinkQuery({}));
      expect(links2.length).to.be.greaterThan(0);

      const user2Me = await client2.agent.me();

      // Verify all links are authored by user2
      for (const link of links2) {
        expect(link.author).to.equal(
          user2Me.did,
          `Link with predicate ${link.predicate} should be authored by user2`,
        );
        expect(link.proof.valid).to.be.true;
      }

      // Ensure authors are different
      expect(user1Me.did).not.to.equal(user2Me.did);
    });
  });
});
