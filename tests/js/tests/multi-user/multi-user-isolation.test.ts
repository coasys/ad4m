import { Ad4mClient, LinkQuery } from "@coasys/ad4m";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient } from "../../utils/utils";
import { startAgent } from "../../helpers/executor";
import type { AgentHandle } from "../../helpers/executor";

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Multi-User Perspective Isolation tests", () => {
  let agentHandle: AgentHandle | null = null;
  let adminAd4mClient: Ad4mClient | null = null;
  let gqlPort: number = 0;

  before(async function () {
    this.timeout(120_000);
    agentHandle = await startAgent("multi-user-isolation");
    adminAd4mClient = agentHandle.client;
    gqlPort = agentHandle.gqlPort;
    await adminAd4mClient.runtime.setMultiUserEnabled(true);
  });

  after(async () => {
    await agentHandle?.stop();
  });

  describe("Perspective Isolation", () => {
    it("should isolate perspectives between users", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "isolation1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "isolation2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "isolation1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "isolation2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get initial perspective counts
      const user1InitialPerspectives = await client1.perspective.all();
      const user2InitialPerspectives = await client2.perspective.all();

      // User 1 creates a perspective
      const perspective1 = await client1.perspective.add("User 1 Perspective");
      expect(perspective1.name).to.equal("User 1 Perspective");
      console.log("User 1 created perspective:", perspective1.uuid);

      // User 2 creates a perspective
      const perspective2 = await client2.perspective.add("User 2 Perspective");
      expect(perspective2.name).to.equal("User 2 Perspective");
      console.log("User 2 created perspective:", perspective2.uuid);

      // User 1 should see only their own perspectives (initial + new one)
      const user1Perspectives = await client1.perspective.all();
      expect(user1Perspectives.length).to.equal(
        user1InitialPerspectives.length + 1,
      );
      const user1HasOwnPerspective = user1Perspectives.some(
        (p) => p.uuid === perspective1.uuid,
      );
      expect(user1HasOwnPerspective).to.be.true;
      const user1HasUser2Perspective = user1Perspectives.some(
        (p) => p.uuid === perspective2.uuid,
      );
      expect(user1HasUser2Perspective).to.be.false;

      // User 2 should see only their own perspectives (initial + new one)
      const user2Perspectives = await client2.perspective.all();
      expect(user2Perspectives.length).to.equal(
        user2InitialPerspectives.length + 1,
      );
      const user2HasOwnPerspective = user2Perspectives.some(
        (p) => p.uuid === perspective2.uuid,
      );
      expect(user2HasOwnPerspective).to.be.true;
      const user2HasUser1Perspective = user2Perspectives.some(
        (p) => p.uuid === perspective1.uuid,
      );
      expect(user2HasUser1Perspective).to.be.false;

      // User 1 should not be able to access User 2's perspective by UUID
      const user1AccessToUser2 = await client1.perspective.byUUID(
        perspective2.uuid,
      );
      expect(user1AccessToUser2).to.be.null;

      // User 2 should not be able to access User 1's perspective by UUID
      const user2AccessToUser1 = await client2.perspective.byUUID(
        perspective1.uuid,
      );
      expect(user2AccessToUser1).to.be.null;
    });

    it("should isolate user perspectives from main agent", async () => {
      // Create a user and their perspective
      const userResult = await adminAd4mClient!.agent.createUser(
        "mainisolation@example.com",
        "password",
      );
      const userToken = await adminAd4mClient!.agent.loginUser(
        "mainisolation@example.com",
        "password",
      );
      // @ts-ignore - Suppress Apollo type mismatch
      const userClient = new Ad4mClient(
        apolloClient(gqlPort, userToken),
        false,
      );

      const userPerspective = await userClient.perspective.add(
        "User Isolated Perspective",
      );
      expect(userPerspective.name).to.equal("User Isolated Perspective");

      // Main agent creates their own perspective
      const mainPerspective = await adminAd4mClient!.perspective.add(
        "Main Agent Perspective",
      );
      expect(mainPerspective.name).to.equal("Main Agent Perspective");

      // Main agent SHOULD see ALL perspectives (including user perspectives)
      const mainPerspectives = await adminAd4mClient!.perspective.all();
      const hasUserPerspective = mainPerspectives.some(
        (p) => p.uuid === userPerspective.uuid,
      );
      expect(hasUserPerspective).to.be.true; // Admin sees all perspectives
      const hasOwnPerspective = mainPerspectives.some(
        (p) => p.uuid === mainPerspective.uuid,
      );
      expect(hasOwnPerspective).to.be.true;

      // User should NOT see main agent perspectives
      const userPerspectives = await userClient.perspective.all();
      const hasMainPerspective = userPerspectives.some(
        (p) => p.uuid === mainPerspective.uuid,
      );
      expect(hasMainPerspective).to.be.false; // Users only see their own perspectives
    });

    it("should handle perspective access control for operations", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "accessctrl1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "accessctrl2@example.com",
        "password2",
      );

      const token1 = await adminAd4mClient!.agent.loginUser(
        "accessctrl1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "accessctrl2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // User 1 creates a perspective
      const perspective1 = await client1.perspective.add(
        "Access Test Perspective",
      );

      // User 2 should not be able to access User 1's perspective for operations
      try {
        await client2.perspective.addLink(perspective1.uuid, {
          source: "test://source",
          target: "test://target",
          predicate: "test://predicate",
        });
        expect.fail(
          "User 2 should not be able to add links to User 1's perspective",
        );
      } catch (error) {
        const errorMessage =
          error instanceof Error ? error.message : String(error);
        expect(errorMessage).to.include("Access denied");
      }
    });
  });

  describe("Link Authoring and Signatures", () => {
    it("should have correct authors and valid signatures for user links", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "linkauth1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "linkauth2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "linkauth1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "linkauth2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // User 1 creates perspective and adds a link
      // @ts-ignore - Suppress Apollo type mismatch
      const p1 = await client1.perspective.add("User 1 Test Perspective");
      // @ts-ignore - Suppress Apollo type mismatch
      const link1 = await client1.perspective.addLink(p1.uuid, {
        source: "ad4m://root",
        target: "test://target1",
        predicate: "test://predicate",
      });

      // Get the link and verify
      // @ts-ignore - Suppress Apollo type mismatch
      const links1 = await client1.perspective.queryLinks(
        p1.uuid,
        new LinkQuery({}),
      );
      expect(links1.length).to.equal(1);
      const user1Me = await client1.agent.me();
      expect(links1[0].author).to.equal(user1Me.did);
      expect(links1[0].proof.valid).to.be.true;

      // User 2 creates perspective and adds a link
      // @ts-ignore - Suppress Apollo type mismatch
      const p2 = await client2.perspective.add("User 2 Test Perspective");
      // @ts-ignore - Suppress Apollo type mismatch
      const link2 = await client2.perspective.addLink(p2.uuid, {
        source: "ad4m://root",
        target: "test://target2",
        predicate: "test://predicate",
      });

      // Get the link and verify
      // @ts-ignore - Suppress Apollo type mismatch
      const links2 = await client2.perspective.queryLinks(
        p2.uuid,
        new LinkQuery({}),
      );
      expect(links2.length).to.equal(1);
      const user2Me = await client2.agent.me();
      expect(links2[0].author).to.equal(user2Me.did);
      expect(links2[0].proof.valid).to.be.true;

      // Ensure authors are different
      expect(user1Me.did).not.to.equal(user2Me.did);
    });
  });
});
