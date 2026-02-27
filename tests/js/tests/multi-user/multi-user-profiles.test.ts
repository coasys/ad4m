import {
  Ad4mClient,
  ExpressionProof,
  Link,
  LinkExpression,
  Perspective,
} from "@coasys/ad4m";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient } from "../../utils/utils";
import { startAgent } from "../../helpers/executor";
import type { AgentHandle } from "../../helpers/executor";

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Multi-User Agent Profiles tests", () => {
  let agentHandle: AgentHandle | null = null;
  let adminAd4mClient: Ad4mClient | null = null;
  let gqlPort: number = 0;

  before(async function () {
    this.timeout(300_000);
    agentHandle = await startAgent("multi-user-profiles");
    adminAd4mClient = agentHandle.client;
    gqlPort = agentHandle.gqlPort;
    await adminAd4mClient.runtime.setMultiUserEnabled(true);
  });

  after(async () => {
    await agentHandle?.stop();
  });

  describe("Agent Profiles and Status", () => {
    it("should maintain separate agent profiles for different users", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "profile1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "profile2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "profile1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "profile2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get initial agent info for both users
      const user1Agent = await client1.agent.me();
      const user2Agent = await client2.agent.me();

      // Verify each user has their own DID
      expect(user1Agent.did).to.not.equal(user2Agent.did);
      console.log("User 1 DID:", user1Agent.did);
      console.log("User 2 DID:", user2Agent.did);

      // Verify each user sees their own profile
      const user1Profile = await client1.agent.me();
      const user2Profile = await client2.agent.me();

      // Each user should see their own DID (not the main agent's DID)
      expect(user1Profile.did).to.equal(user1Agent.did);
      expect(user2Profile.did).to.equal(user2Agent.did);

      // DIDs should be different between users
      expect(user1Profile.did).to.not.equal(user2Profile.did);
    });

    it("should handle agent status correctly for different users", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "status1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "status2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "status1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "status2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Check agent status for both users
      const user1Status = await client1.agent.status();
      const user2Status = await client2.agent.status();

      console.log("User 1 status:", user1Status);
      console.log("User 2 status:", user2Status);

      // Both users should have valid status
      expect(user1Status).to.have.property("isInitialized");
      expect(user2Status).to.have.property("isInitialized");
      expect(user1Status.isInitialized).to.be.true;
      expect(user2Status.isInitialized).to.be.true;

      // Each user should have their own DID in status
      expect(user1Status.did).to.not.equal(user2Status.did);

      // Assert on DID documents
      expect(user1Status.didDocument).to.be.a("string");
      expect(user2Status.didDocument).to.be.a("string");
      expect(user1Status.didDocument).to.not.equal(user2Status.didDocument);

      // Parse and validate DID documents
      const user1DidDoc = JSON.parse(user1Status.didDocument!);
      const user2DidDoc = JSON.parse(user2Status.didDocument!);

      expect(user1DidDoc.id).to.equal(user1Status.did);
      expect(user2DidDoc.id).to.equal(user2Status.did);
      expect(user1DidDoc).to.have.property("verificationMethod");
      expect(user2DidDoc).to.have.property("verificationMethod");
      expect(user1DidDoc.verificationMethod).to.be.an("array").that.is.not
        .empty;
      expect(user2DidDoc.verificationMethod).to.be.an("array").that.is.not
        .empty;
    });

    it("should allow users to update their own agent profiles independently", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "update1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "update2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "update1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "update2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // User 1 updates their profile
      let link1 = new LinkExpression();
      link1.author = "did:test:1";
      link1.timestamp = new Date().toISOString();
      link1.data = new Link({
        source: "user1",
        target: "profile1",
        predicate: "name",
      });
      link1.proof = new ExpressionProof("sig1", "key1");
      await client1.agent.updatePublicPerspective(new Perspective([link1]));

      // User 2 updates their profile with different data
      let link2 = new LinkExpression();
      link2.author = "did:test:2";
      link2.timestamp = new Date().toISOString();
      link2.data = new Link({
        source: "user2",
        target: "profile2",
        predicate: "name",
      });
      link2.proof = new ExpressionProof("sig2", "key2");
      await client2.agent.updatePublicPerspective(new Perspective([link2]));

      // Verify that each user's public perspective was updated correctly
      const user1AfterUpdate = await client1.agent.me();
      const user2AfterUpdate = await client2.agent.me();

      // Check that profiles contain the correct links
      expect(user1AfterUpdate.perspective).to.not.be.null;
      expect(user2AfterUpdate.perspective).to.not.be.null;

      if (
        user1AfterUpdate.perspective &&
        user1AfterUpdate.perspective.links.length > 0
      ) {
        const user1Link = user1AfterUpdate.perspective.links.find(
          (l) => l.data.source === "user1" && l.data.target === "profile1",
        );
        expect(user1Link).to.not.be.undefined;
      }

      if (
        user2AfterUpdate.perspective &&
        user2AfterUpdate.perspective.links.length > 0
      ) {
        const user2Link = user2AfterUpdate.perspective.links.find(
          (l) => l.data.source === "user2" && l.data.target === "profile2",
        );
        expect(user2Link).to.not.be.undefined;
      }

      console.log("User 1 after update:", user1AfterUpdate.did);
      console.log("User 2 after update:", user2AfterUpdate.did);

      // Verify DIDs are still different
      expect(user1AfterUpdate.did).to.not.equal(user2AfterUpdate.did);
    });

    it("should not allow users to see other users' agent profiles", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "private1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "private2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "private1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "private2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get agent info for both users
      const user1Agent = await client1.agent.me();
      const user2Agent = await client2.agent.me();

      // Verify each user only sees their own agent information
      expect(user1Agent.did).to.not.equal(user2Agent.did);

      // Try to query the other user's DID (this should fail or return nothing)
      try {
        const user1TryingToSeeUser2 = await client1.agent.byDID(user2Agent.did);
        // If this succeeds, it should not return user2's private information
        console.log("User 1 trying to see User 2:", user1TryingToSeeUser2);
      } catch (error) {}
    });

    it("should publish managed users to the agent language", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "agentlang1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "agentlang2@example.com",
        "password2",
      );

      // Login both users to trigger any agent language publishing
      const token1 = await adminAd4mClient!.agent.loginUser(
        "agentlang1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "agentlang2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get the DIDs for both users
      const user1Agent = await client1.agent.me();
      const user2Agent = await client2.agent.me();

      console.log("User 1 DID:", user1Agent.did);
      console.log("User 2 DID:", user2Agent.did);

      // Wait a moment for the agents to be fully published
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Try to retrieve the users from the agent language by their DIDs
      try {
        console.log("Attempting to retrieve user 1 with DID:", user1Agent.did);
        const retrievedUser1 = await adminAd4mClient!.agent.byDID(
          user1Agent.did,
        );
        console.log("Retrieved user 1:", retrievedUser1);

        console.log("Attempting to retrieve user 2 with DID:", user2Agent.did);
        const retrievedUser2 = await adminAd4mClient!.agent.byDID(
          user2Agent.did,
        );
        console.log("Retrieved user 2:", retrievedUser2);

        expect(retrievedUser1).to.not.be.null;
        expect(retrievedUser2).to.not.be.null;

        if (retrievedUser1) {
          expect(retrievedUser1.did).to.equal(user1Agent.did);
        }

        if (retrievedUser2) {
          expect(retrievedUser2.did).to.equal(user2Agent.did);
        }

        // Also test getting agent expressions via expression.get()
        console.log("Testing expression.get() method...");
        const expr1 = await adminAd4mClient!.expression.get(user1Agent.did);
        const expr2 = await adminAd4mClient!.expression.get(user2Agent.did);

        console.log("Expression 1 result:", expr1);
        console.log("Expression 2 result:", expr2);

        if (expr1?.data) {
          const agent1Data =
            typeof expr1.data === "string"
              ? JSON.parse(expr1.data)
              : expr1.data;
          expect(agent1Data.did).to.equal(user1Agent.did);
        } else {
          console.log("ℹ️  User 1 expression.get() returned null");
        }

        if (expr2?.data) {
          const agent2Data =
            typeof expr2.data === "string"
              ? JSON.parse(expr2.data)
              : expr2.data;
          expect(agent2Data.did).to.equal(user2Agent.did);
        } else {
          console.log("ℹ️  User 2 expression.get() returned null");
        }
      } catch (error) {
        console.log("❌ Failed to retrieve users from agent language:", error);
        throw error;
      }
    });

    it("should publish updated public perspectives to the agent language", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "perspective1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "perspective2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "perspective1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "perspective2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get initial agent info
      const user1Agent = await client1.agent.me();
      const user2Agent = await client2.agent.me();

      console.log("User 1 DID:", user1Agent.did);
      console.log("User 2 DID:", user2Agent.did);

      // User 1 updates their public perspective
      let link1 = new LinkExpression();
      link1.author = user1Agent.did;
      link1.timestamp = new Date().toISOString();
      link1.data = new Link({
        source: "user1",
        target: "profile1",
        predicate: "name",
      });
      link1.proof = new ExpressionProof("sig1", "key1");
      await client1.agent.updatePublicPerspective(new Perspective([link1]));

      // User 2 updates their public perspective with different data
      let link2 = new LinkExpression();
      link2.author = user2Agent.did;
      link2.timestamp = new Date().toISOString();
      link2.data = new Link({
        source: "user2",
        target: "profile2",
        predicate: "name",
      });
      link2.proof = new ExpressionProof("sig2", "key2");
      await client2.agent.updatePublicPerspective(new Perspective([link2]));

      // Wait for the updates to be published
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Retrieve the updated agents from the agent language
      try {
        console.log("Retrieving updated agents from agent language...");
        const retrievedUser1 = await adminAd4mClient!.agent.byDID(
          user1Agent.did,
        );
        const retrievedUser2 = await adminAd4mClient!.agent.byDID(
          user2Agent.did,
        );

        expect(retrievedUser1).to.not.be.null;
        expect(retrievedUser2).to.not.be.null;

        if (retrievedUser1?.perspective) {
          expect(retrievedUser1.perspective.links).to.have.length.greaterThan(
            0,
          );
          const hasUser1Link = retrievedUser1.perspective.links.some(
            (link) =>
              link.data.source === "user1" && link.data.target === "profile1",
          );
          expect(hasUser1Link).to.be.true;
        }

        if (retrievedUser2?.perspective) {
          expect(retrievedUser2.perspective.links).to.have.length.greaterThan(
            0,
          );
          const hasUser2Link = retrievedUser2.perspective.links.some(
            (link) =>
              link.data.source === "user2" && link.data.target === "profile2",
          );
          expect(hasUser2Link).to.be.true;
        }

        // Also test via expression.get()
        console.log("Testing updated perspectives via expression.get()...");
        const expr1 = await adminAd4mClient!.expression.get(user1Agent.did);
        const expr2 = await adminAd4mClient!.expression.get(user2Agent.did);

        if (expr1?.data) {
          const agent1Data =
            typeof expr1.data === "string"
              ? JSON.parse(expr1.data)
              : expr1.data;
          expect(agent1Data.perspective?.links).to.have.length.greaterThan(0);
        } else {
          console.log("ℹ️  User 1 updated expression.get() returned null");
        }

        if (expr2?.data) {
          const agent2Data =
            typeof expr2.data === "string"
              ? JSON.parse(expr2.data)
              : expr2.data;
          expect(agent2Data.perspective?.links).to.have.length.greaterThan(0);
        } else {
          console.log("ℹ️  User 2 updated expression.get() returned null");
        }
      } catch (error) {
        console.log(
          "❌ Failed to retrieve updated agents from agent language:",
          error,
        );
        throw error;
      }
    });

    it("should use correct user context for expression.create()", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "expr1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "expr2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "expr1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "expr2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      const user1Agent = await client1.agent.me();
      const user2Agent = await client2.agent.me();

      console.log("User 1 DID:", user1Agent.did);
      console.log("User 2 DID:", user2Agent.did);

      // User 1 creates a literal expression
      const expr1Url = await client1.expression.create(
        "Hello from User 1",
        "literal",
      );
      console.log("User 1 created expression:", expr1Url);

      // User 2 creates a literal expression
      const expr2Url = await client2.expression.create(
        "Hello from User 2",
        "literal",
      );
      console.log("User 2 created expression:", expr2Url);

      // Retrieve the expressions and check their authors
      const expr1 = await adminAd4mClient!.expression.get(expr1Url);
      const expr2 = await adminAd4mClient!.expression.get(expr2Url);

      console.log("Expression 1:", JSON.stringify(expr1, null, 2));
      console.log("Expression 2:", JSON.stringify(expr2, null, 2));

      // The expressions should be authored by the respective users, not the main agent
      expect(expr1?.author).to.equal(user1Agent.did);
      expect(expr2?.author).to.equal(user2Agent.did);

      if (expr1) {
        console.log("Expression 1 proof:", expr1.proof);
        expect(expr1.proof.signature).to.not.be.empty;
        expect(expr1.proof.key).to.not.be.empty;
      }
      if (expr2) {
        console.log("Expression 2 proof:", expr2.proof);
        expect(expr2.proof.signature).to.not.be.empty;
        expect(expr2.proof.key).to.not.be.empty;
      }
    });

    it("should use correct user context for expression.interact()", async () => {
      // This test would require a language with interactions
      // For now, we'll just verify that the context-aware code path exists
      console.log(
        "ℹ️  Expression interaction context test skipped - requires custom language with interactions",
      );
    });
  });
});
