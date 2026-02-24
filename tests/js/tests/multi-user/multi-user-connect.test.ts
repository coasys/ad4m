import { Ad4mClient } from "@coasys/ad4m";
import { getAd4mClient } from "@coasys/ad4m-connect";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { startAgent, AgentHandle } from "../../helpers/executor";

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Multi-User Ad4m-Connect integration tests", () => {
  let agent: AgentHandle;
  let adminAd4mClient: Ad4mClient;

  before(async () => {
    agent = await startAgent("multi-user-connect-agent");
    adminAd4mClient = agent.client;
  });

  after(async () => {
    await agent.stop();
  });

  describe("Multi-User Connect Flow", () => {
    it("should create user and login via ad4m-connect", async () => {
      // Create ad4m-connect instance with multi-user options
      const client = await getAd4mClient({
        appInfo: {
          name: "Multi-User Test App",
          description: "Testing multi-user functionality",
          url: "test.ad4m.org",
          iconPath: "https://example.com/icon.png",
        },
        capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
        multiUser: true,
        remoteUrl: `ws://localhost:${agent.gqlPort}/graphql`,
        userEmail: "test@example.com",
        userPassword: "password123",
      });

      // Connect should handle user creation and login automatically
      expect(client).to.be.ok;

      // Verify we have an authenticated client
      const status = await client.agent.status();
      expect(status.isUnlocked).to.be.true;
      expect(status.did).to.be.ok;
      expect(status.did).to.match(/^did:key:.+/);

      // Verify the agent.me returns the correct user DID
      const me = await client.agent.me();
      expect(me.did).to.equal(status.did);

      console.log("Successfully connected as user:", me.did);
    });

    it("should login existing user via ad4m-connect", async () => {
      // First, create a user directly via admin client
      const userResult = await adminAd4mClient.agent.createUser(
        "existing@example.com",
        "password456",
      );
      expect(userResult.success).to.be.true;

      // Now try to connect via ad4m-connect with existing user credentials
      const client = await getAd4mClient({
        appInfo: {
          name: "Multi-User Test App",
          description: "Testing multi-user functionality",
          url: "test.ad4m.org",
          iconPath: "https://example.com/icon.png",
        },
        capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
        multiUser: true,
        remoteUrl: `ws://localhost:${agent.gqlPort}/graphql`,
        userEmail: "existing@example.com",
        userPassword: "password456",
      });

      // Connect should login the existing user
      expect(client).to.be.ok;

      // Verify we're logged in as the correct user
      const ag = await client.agent.me();
      expect(ag.did).to.equal(userResult.did);

      console.log("Successfully logged in existing user:", ag.did);
    });

    it("should fail with wrong password", async () => {
      // Try to connect with wrong password
      const client = await getAd4mClient({
        appInfo: {
          name: "Multi-User Test App",
          description: "Testing multi-user functionality",
          url: "test.ad4m.org",
          iconPath: "https://example.com/icon.png",
        },
        capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
        multiUser: true,
        remoteUrl: `ws://localhost:${agent.gqlPort}/graphql`,
        userEmail: "existing@example.com",
        userPassword: "wrongpassword",
      });

      // Connect should fail
      const call = async () => {
        return client;
      };

      await expect(call()).to.be.rejected;
    });
  });
});
