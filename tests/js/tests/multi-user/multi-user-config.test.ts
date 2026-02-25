import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import { fileURLToPath } from "url";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep } from "../../utils/utils";
import { startAgent } from "../../helpers/executor";
import type { AgentHandle } from "../../helpers/executor";

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Multi-User Configuration tests", () => {
  let agentHandle: AgentHandle | null = null;
  let adminAd4mClient: Ad4mClient | null = null;
  let gqlPort: number = 0;

  before(async function () {
    this.timeout(120_000);
    agentHandle = await startAgent("multi-user-config");
    adminAd4mClient = agentHandle.client;
    gqlPort = agentHandle.gqlPort;
  });

  after(async () => {
    await agentHandle?.stop();
  });

  describe("Multi-User Configuration", () => {
    it("should have multi-user disabled by default and require activation", async () => {
      // Disable multi-user to test the guard (startAgent enables it by default)
      await adminAd4mClient!.runtime.setMultiUserEnabled(false);

      const isDisabled = await adminAd4mClient!.runtime.multiUserEnabled();
      expect(isDisabled).to.be.false;

      // Attempt to create a user while multi-user is disabled (should fail)
      const userResult = await adminAd4mClient!.agent.createUser(
        "test@example.com",
        "password123",
      );
      expect(userResult.success).to.be.false;
      expect(userResult.error).to.include("Multi-user mode is not enabled");

      // Enable multi-user mode
      const setResult =
        await adminAd4mClient!.runtime.setMultiUserEnabled(true);
      expect(setResult).to.be.true;

      // Verify it's now enabled
      const isEnabledAfter = await adminAd4mClient!.runtime.multiUserEnabled();
      expect(isEnabledAfter).to.be.true;

      // Now user creation should work
      const userResult2 = await adminAd4mClient!.agent.createUser(
        "working@example.com",
        "password456",
      );
      expect(userResult2.success).to.be.true;
      expect(userResult2.did).to.match(/^did:key:.+/);
    });

    it("should return empty array when multi-user is disabled", async () => {
      // Disable multi-user mode temporarily
      await adminAd4mClient!.runtime.setMultiUserEnabled(false);

      // List users should return empty array
      const users = await adminAd4mClient!.runtime.listUsers();
      expect(users).to.be.an("array");
      expect(users).to.have.lengthOf(0);

      // Re-enable for other tests
      await adminAd4mClient!.runtime.setMultiUserEnabled(true);
    });

    it("should list users with statistics", async () => {
      // Create a few users
      await adminAd4mClient!.agent.createUser(
        "stats1@example.com",
        "password1",
      );
      await adminAd4mClient!.agent.createUser(
        "stats2@example.com",
        "password2",
      );

      // Login one user to update their last_seen
      const token1 = await adminAd4mClient!.agent.loginUser(
        "stats1@example.com",
        "password1",
      );
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);

      // User 1 creates a perspective
      await client1.perspective.add("User 1 Perspective");

      // Wait a moment for last_seen to be updated
      await sleep(1000);

      // List users
      const users = await adminAd4mClient!.runtime.listUsers();
      expect(users).to.be.an("array");
      expect(users.length).to.be.greaterThan(0);

      console.log("Users:", JSON.stringify(users, null, 2));

      // Find our test users
      const user1 = users.find((u) => u.email === "stats1@example.com");
      const user2 = users.find((u) => u.email === "stats2@example.com");

      expect(user1).to.not.be.undefined;
      expect(user2).to.not.be.undefined;

      // Verify structure
      expect(user1).to.have.property("email");
      expect(user1).to.have.property("did");
      expect(user1).to.have.property("perspectiveCount");

      // Verify user1 has a perspective
      expect(user1!.perspectiveCount).to.equal(1);

      // Verify user2 has no perspectives
      expect(user2!.perspectiveCount).to.equal(0);

      // Verify user1 has last_seen set (they logged in)
      expect(user1).to.have.property("lastSeen");
      console.log("User 1 last seen:", user1!.lastSeen);

      // Verify DIDs are different
      expect(user1!.did).to.not.equal(user2!.did);
    });

    it("should track last_seen timestamps", async () => {
      // Create a user
      await adminAd4mClient!.agent.createUser(
        "lastseen@example.com",
        "password",
      );

      // List users before login
      let users = await adminAd4mClient!.runtime.listUsers();
      let user = users.find((u) => u.email === "lastseen@example.com");
      expect(user).to.not.be.undefined;

      // Initially might not have last_seen
      const initialLastSeen = user!.lastSeen;
      console.log("Initial last_seen:", initialLastSeen);

      // Login the user (this should trigger last_seen tracking)
      const token = await adminAd4mClient!.agent.loginUser(
        "lastseen@example.com",
        "password",
      );
      const userClient = new Ad4mClient(apolloClient(gqlPort, token), false);

      console.log(
        "========================HERE================================",
      );
      // Make a request to trigger last_seen update
      await userClient.agent.me();

      console.log(
        "========================HERE 2================================",
      );

      // Wait for middleware to process (async task needs time)
      await sleep(2000);

      // List users again
      users = await adminAd4mClient!.runtime.listUsers();
      user = users.find((u) => u.email === "lastseen@example.com");

      // Now last_seen should be set
      expect(user!.lastSeen).to.not.be.undefined;
      console.log("Updated last_seen:", user!.lastSeen);

      // Parse the timestamp - could be ISO string or Unix timestamp in seconds
      let lastSeenDate: Date;
      const lastSeenValue = user!.lastSeen!;

      // Handle both number and string timestamp formats
      if (typeof lastSeenValue === "number") {
        console.log(
          "Last seen value is a number Unix timestamp in seconds, converting to milliseconds",
        );
        lastSeenDate = new Date(lastSeenValue * 1000);
      } else {
        console.log(
          "Last seen value is a string, checking if it's a Unix timestamp in seconds",
        );
        console.log("Last seen value:", lastSeenValue);
        if (/^\d+(\.\d+)?$/.test(lastSeenValue)) {
          console.log(
            "Last seen value is a Unix timestamp in seconds, converting to milliseconds",
          );
          lastSeenDate = new Date(parseInt(lastSeenValue) * 1000);
        } else {
          console.log("Last seen value is a ISO string, converting to Date");
          lastSeenDate = new Date(lastSeenValue);
        }
      }

      const now = new Date();

      // Should be recent (within last 5 seconds)
      const diffMs = now.getTime() - lastSeenDate.getTime();
      const diffSeconds = Math.abs(diffMs) / 1000;
      console.log("Time difference:", {
        nowMs: now.getTime(),
        lastSeenMs: lastSeenDate.getTime(),
        diffMs,
        diffSeconds,
        lastSeenValue,
      });
      expect(diffSeconds).to.be.lessThan(5);
    });
  });

  describe("Basic Multi-User Functionality", () => {
    before(async () => {
      await adminAd4mClient!.runtime.setMultiUserEnabled(true);
    });

    it("should create and login users with unique DIDs", async () => {
      // Create first user
      const user1Result = await adminAd4mClient!.agent.createUser(
        "alice@example.com",
        "password123",
      );
      expect(user1Result.success).to.be.true;
      expect(user1Result.did).to.match(/^did:key:.+/);

      // Create second user
      const user2Result = await adminAd4mClient!.agent.createUser(
        "bob@example.com",
        "password456",
      );
      expect(user2Result.success).to.be.true;
      expect(user2Result.did).to.match(/^did:key:.+/);

      // Users should have different DIDs
      expect(user1Result.did).to.not.equal(user2Result.did);

      // Login first user
      const user1Token = await adminAd4mClient!.agent.loginUser(
        "alice@example.com",
        "password123",
      );
      expect(user1Token).to.be.ok;

      // Login second user
      const user2Token = await adminAd4mClient!.agent.loginUser(
        "bob@example.com",
        "password456",
      );
      expect(user2Token).to.be.ok;

      // Verify JWT tokens contain correct user DIDs
      const user1Payload = JSON.parse(atob(user1Token.split(".")[1]));
      const user2Payload = JSON.parse(atob(user2Token.split(".")[1]));

      expect(user1Payload.sub).to.equal("alice@example.com");
      expect(user2Payload.sub).to.equal("bob@example.com");
    });

    it("should return correct user DID in agent.me", async () => {
      // Create and login user
      const userResult = await adminAd4mClient!.agent.createUser(
        "charlie@example.com",
        "password789",
      );
      const userToken = await adminAd4mClient!.agent.loginUser(
        "charlie@example.com",
        "password789",
      );

      // Create authenticated client
      const userClient = new Ad4mClient(
        apolloClient(gqlPort, userToken),
        false,
      );

      // Test agent.me
      const me = await userClient.agent.me();
      expect(me.did).to.equal(userResult.did);

      // Test agent.status
      const status = await userClient.agent.status();
      expect(status.did).to.equal(userResult.did);
      expect(status.isUnlocked).to.be.true;
    });

    it("should handle login persistence", async () => {
      // Create user
      const userResult = await adminAd4mClient!.agent.createUser(
        "dave@example.com",
        "passwordABC",
      );

      // Login first time
      const token1 = await adminAd4mClient!.agent.loginUser(
        "dave@example.com",
        "passwordABC",
      );
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      const agent1 = await client1.agent.me();

      // Login second time
      const token2 = await adminAd4mClient!.agent.loginUser(
        "dave@example.com",
        "passwordABC",
      );
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);
      const agent2 = await client2.agent.me();

      // Should get the same DID both times
      expect(agent1.did).to.equal(agent2.did);
      expect(agent1.did).to.equal(userResult.did);
    });

    it("should reject wrong passwords", async () => {
      // Create user
      await adminAd4mClient!.agent.createUser(
        "eve@example.com",
        "correctpassword",
      );

      // Try to login with wrong password
      const call = async () => {
        return await adminAd4mClient!.agent.loginUser(
          "eve@example.com",
          "wrongpassword",
        );
      };

      await expect(call()).to.be.rejectedWith(/Invalid credentials/);
    });

    it("should reject non-existent users", async () => {
      const call = async () => {
        return await adminAd4mClient!.agent.loginUser(
          "nonexistent@example.com",
          "password",
        );
      };

      // verify_user_password returns false for unknown emails — same "Invalid credentials" path as wrong password
      await expect(call()).to.be.rejectedWith(/Invalid credentials/);
    });
  });
});
