import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from "url";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor } from "../../utils/utils";
import { ChildProcess } from "node:child_process";
import fetch from "node-fetch";

//@ts-ignore
global.fetch = fetch;

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Multi-User integration tests", () => {
  const TEST_DIR = path.join(`${__dirname}/../../tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "multi-user-agent");
  const bootstrapSeedPath = path.join(`${__dirname}/../../bootstrapSeed.json`);
  const gqlPort = 15500;
  const hcAdminPort = 15501;
  const hcAppPort = 15502;

  let executorProcess: ChildProcess | null = null;
  let adminAd4mClient: Ad4mClient | null = null;

  before(async () => {
    if (!fs.existsSync(appDataPath)) {
      fs.mkdirSync(appDataPath, { recursive: true });
    }

    // Start executor with multi-user mode enabled
    executorProcess = await startExecutor(
      appDataPath,
      bootstrapSeedPath,
      gqlPort,
      hcAdminPort,
      hcAppPort,
      false,
      "admin123",
    );

    adminAd4mClient = new Ad4mClient(apolloClient(gqlPort, "admin123"), false);

    // Generate initial admin agent
    await adminAd4mClient.agent.generate("passphrase");
  });

  after(async () => {
    if (executorProcess) {
      while (!executorProcess?.killed) {
        let status = executorProcess?.kill();
        console.log("killed executor with", status);
        await sleep(500);
      }
    }
  });

  describe("User Registration and Authentication", () => {
    it("should create a new user with username and password", async () => {
      const result = await adminAd4mClient!.agent.createUser(
        "alice",
        "password123",
      );
      expect(result).to.have.property("did");
      expect(result).to.have.property("success", true);
      expect(result.did).to.match(/^did:key:.+/);
    });

    it("should return existing user if already exists", async () => {
      // Create user first time
      const result1 = await adminAd4mClient!.agent.createUser(
        "bob",
        "password456",
      );
      expect(result1.success).to.be.true;

      // Try to create same user again
      const result2 = await adminAd4mClient!.agent.createUser(
        "bob",
        "password456",
      );
      expect(result2.success).to.be.true;
      expect(result2.did).to.equal(result1.did);
    });

    it("should fail to create user with wrong password for existing username", async () => {
      // Create user first
      await adminAd4mClient!.agent.createUser("charlie", "correctpassword");

      // Try with wrong password
      const result = await adminAd4mClient!.agent.createUser(
        "charlie",
        "wrongpassword",
      );
      expect(result.success).to.be.false;
      expect(result.error).to.include("Invalid credentials");
    });

    it("should generate a JWT token for a specific user via login", async () => {
      // Create user
      const userResult = await adminAd4mClient!.agent.createUser(
        "dave",
        "password789",
      );
      expect(userResult.success).to.be.true;

      // Login as the user to get a JWT token
      const token = await adminAd4mClient!.agent.loginUser(
        "dave",
        "password789",
      );
      expect(token).to.match(/.+/);

      // Create a client with the user's token and verify DID
      const userClient = new Ad4mClient(apolloClient(gqlPort, token), false);
      const me = await userClient.agent.me();
      expect(me.did).to.equal(userResult.did);
    });
  });

  describe("User-Scoped Perspectives", () => {
    let aliceClient: Ad4mClient;
    let bobClient: Ad4mClient;
    let aliceDid: string;
    let bobDid: string;

    before(async () => {
      // Create users and get their tokens via loginUser
      const aliceResult = await adminAd4mClient!.agent.createUser(
        "alice_persp",
        "password123",
      );
      const bobResult = await adminAd4mClient!.agent.createUser(
        "bob_persp",
        "password456",
      );

      aliceDid = aliceResult.did;
      bobDid = bobResult.did;

      const aliceToken = await adminAd4mClient!.agent.loginUser(
        "alice_persp",
        "password123",
      );
      const bobToken = await adminAd4mClient!.agent.loginUser(
        "bob_persp",
        "password456",
      );

      aliceClient = new Ad4mClient(apolloClient(gqlPort, aliceToken), false);
      bobClient = new Ad4mClient(apolloClient(gqlPort, bobToken), false);
    });

    it("should create perspectives scoped to specific users", async () => {
      // Alice creates a perspective
      const alicePerspective = await aliceClient.perspective.add(
        "Alice's Perspective",
      );
      expect(alicePerspective.uuid).to.be.ok;

      // Bob creates a perspective
      const bobPerspective =
        await bobClient.perspective.add("Bob's Perspective");
      expect(bobPerspective.uuid).to.be.ok;

      // Perspectives should have different UUIDs
      expect(alicePerspective.uuid).to.not.equal(bobPerspective.uuid);
    });

    it("should only show user's own perspectives", async () => {
      // Create perspectives for each user
      const alicePerspective1 = await aliceClient.perspective.add(
        "Alice Perspective 1",
      );
      const alicePerspective2 = await aliceClient.perspective.add(
        "Alice Perspective 2",
      );
      const bobPerspective1 =
        await bobClient.perspective.add("Bob Perspective 1");
      const bobPerspective2 =
        await bobClient.perspective.add("Bob Perspective 2");

      // Alice should only see her perspectives
      const alicePerspectives = await aliceClient.perspective.all();
      const aliceUuids = alicePerspectives.map((p) => p.uuid);
      expect(aliceUuids).to.include(alicePerspective1.uuid);
      expect(aliceUuids).to.include(alicePerspective2.uuid);
      expect(aliceUuids).to.not.include(bobPerspective1.uuid);
      expect(aliceUuids).to.not.include(bobPerspective2.uuid);

      // Bob should only see his perspectives
      const bobPerspectives = await bobClient.perspective.all();
      const bobUuids = bobPerspectives.map((p) => p.uuid);
      expect(bobUuids).to.include(bobPerspective1.uuid);
      expect(bobUuids).to.include(bobPerspective2.uuid);
      expect(bobUuids).to.not.include(alicePerspective1.uuid);
      expect(bobUuids).to.not.include(alicePerspective2.uuid);
    });

    it("should not allow access to other user's perspectives", async () => {
      // Alice creates a perspective
      const alicePerspective = await aliceClient.perspective.add(
        "Alice Private Perspective",
      );

      // Bob tries to access Alice's perspective
      const call = async () => {
        return await bobClient.perspective.byUUID(alicePerspective.uuid);
      };

      await expect(call()).to.be.rejectedWith(
        /not found|access denied|unauthorized/i,
      );
    });

    it("should handle perspective updates with user scoping", async () => {
      // Alice creates and updates a perspective
      const perspective = await aliceClient.perspective.add(
        "Alice Updatable Perspective",
      );
      const updatedPerspective = await aliceClient.perspective.update(
        perspective.uuid,
        "Updated Name",
      );

      expect(updatedPerspective.name).to.equal("Updated Name");

      // Bob should not be able to update Alice's perspective
      const call = async () => {
        return await bobClient.perspective.update(
          perspective.uuid,
          "Bob's Malicious Update",
        );
      };

      await expect(call()).to.be.rejectedWith(
        /not found|access denied|unauthorized/i,
      );
    });
  });

  describe("User Context in Agent Operations", () => {
    let userClient: Ad4mClient;
    let userDid: string;

    before(async () => {
      // Create user and get token via loginUser
      const userResult = await adminAd4mClient!.agent.createUser(
        "test_user_ops",
        "password123",
      );
      userDid = userResult.did;

      const token = await adminAd4mClient!.agent.loginUser(
        "test_user_ops",
        "password123",
      );

      userClient = new Ad4mClient(apolloClient(gqlPort, token), false);
    });

    it("should return correct agent status for user", async () => {
      const status = await userClient.agent.status();
      expect(status.did).to.equal(userDid);
      expect(status.isUnlocked).to.be.true;
    });

    it("should handle agent operations in user context", async () => {
      // This test will be expanded once we implement multi-user agent service
      // For now, just verify the user context is maintained
      const agent = await userClient.agent.me();
      expect(agent.did).to.equal(userDid);
    });
  });
});
