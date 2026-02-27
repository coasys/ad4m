import { Ad4mClient } from "@coasys/ad4m";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep } from "../../utils/utils";
import { startAgent } from "../../helpers/executor";
import type { AgentHandle } from "../../helpers/executor";

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Multi-User Perspective Subscriptions tests", () => {
  let agentHandle: AgentHandle | null = null;
  let adminAd4mClient: Ad4mClient | null = null;
  let gqlPort: number = 0;

  before(async function () {
    this.timeout(300_000);
    agentHandle = await startAgent("multi-user-subscriptions");
    adminAd4mClient = agentHandle.client;
    gqlPort = agentHandle.gqlPort;
    await adminAd4mClient.runtime.setMultiUserEnabled(true);
  });

  after(async () => {
    await agentHandle?.stop();
  });

  describe("Perspective Subscriptions", () => {
    it("should only notify users about their own perspectives in perspectiveAdded", async () => {
      console.log("\n=== Testing perspective subscription filtering ===");

      // Create two users
      await adminAd4mClient!.agent.createUser("sub1@example.com", "password1");
      await adminAd4mClient!.agent.createUser("sub2@example.com", "password2");

      const token1 = await adminAd4mClient!.agent.loginUser(
        "sub1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "sub2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Track perspective added events for both users
      const user1Events: any[] = [];
      const user2Events: any[] = [];

      // Subscribe both users to perspectiveAdded
      console.log("Subscribing users to perspectiveAdded...");

      // @ts-ignore
      client1.perspective.addPerspectiveAddedListener((perspective) => {
        console.log(
          `User 1 received perspectiveAdded event: ${perspective.name} (UUID: ${perspective.uuid})`,
        );
        user1Events.push(perspective);
      });
      client1.perspective.subscribePerspectiveAdded();

      // @ts-ignore
      client2.perspective.addPerspectiveAddedListener((perspective) => {
        console.log(
          `User 2 received perspectiveAdded event: ${perspective.name} (UUID: ${perspective.uuid})`,
        );
        user2Events.push(perspective);
      });
      client2.perspective.subscribePerspectiveAdded();

      // Wait for subscriptions to be active
      await sleep(1000);

      // User 1 creates a perspective
      console.log("\nUser 1 creating perspective...");
      const user1Perspective = await client1.perspective.add(
        "User 1 Only Perspective",
      );
      console.log(`User 1 created perspective: ${user1Perspective.uuid}`);

      // Wait for events to propagate
      await sleep(2000);

      // User 2 creates a perspective
      console.log("\nUser 2 creating perspective...");
      const user2Perspective = await client2.perspective.add(
        "User 2 Only Perspective",
      );
      console.log(`User 2 created perspective: ${user2Perspective.uuid}`);

      // Wait for events to propagate
      await sleep(2000);

      console.log(`\nUser 1 received ${user1Events.length} events`);
      console.log(`User 2 received ${user2Events.length} events`);

      // Each user should only see their own perspective creation event
      expect(user1Events.length).to.equal(
        1,
        "User 1 should only receive 1 event (their own perspective)",
      );
      expect(user2Events.length).to.equal(
        1,
        "User 2 should only receive 1 event (their own perspective)",
      );

      expect(user1Events[0].uuid).to.equal(
        user1Perspective.uuid,
        "User 1 should only see their own perspective",
      );
      expect(user2Events[0].uuid).to.equal(
        user2Perspective.uuid,
        "User 2 should only see their own perspective",
      );
    });

    it("should only notify users about their own perspectives in perspectiveUpdated", async () => {
      console.log(
        "\n=== Testing perspectiveUpdated subscription filtering ===",
      );

      // Create two users
      await adminAd4mClient!.agent.createUser(
        "update1@example.com",
        "password1",
      );
      await adminAd4mClient!.agent.createUser(
        "update2@example.com",
        "password2",
      );

      const token1 = await adminAd4mClient!.agent.loginUser(
        "update1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "update2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Create perspectives for both users
      const user1Perspective =
        await client1.perspective.add("User 1 Update Test");
      const user2Perspective =
        await client2.perspective.add("User 2 Update Test");

      // Track events
      const user1UpdateEvents: any[] = [];
      const user2UpdateEvents: any[] = [];

      // Subscribe to perspectiveUpdated
      console.log("Subscribing users to perspectiveUpdated...");
      // @ts-ignore
      client1.perspective.addPerspectiveUpdatedListener((perspective) => {
        console.log(
          `User 1 received perspectiveUpdated event: ${perspective.name} (UUID: ${perspective.uuid})`,
        );
        user1UpdateEvents.push(perspective);
      });
      client1.perspective.subscribePerspectiveUpdated();

      // @ts-ignore
      client2.perspective.addPerspectiveUpdatedListener((perspective) => {
        console.log(
          `User 2 received perspectiveUpdated event: ${perspective.name} (UUID: ${perspective.uuid})`,
        );
        user2UpdateEvents.push(perspective);
      });
      client2.perspective.subscribePerspectiveUpdated();

      await sleep(1000);

      // User 1 updates their perspective metadata (name)
      console.log("\nUser 1 updating their perspective name...");
      await client1.perspective.update(
        user1Perspective.uuid,
        "User 1 Updated Name",
      );

      await sleep(2000);

      // User 2 updates their perspective metadata (name)
      console.log("\nUser 2 updating their perspective name...");
      await client2.perspective.update(
        user2Perspective.uuid,
        "User 2 Updated Name",
      );

      await sleep(2000);

      console.log(
        `\nUser 1 received ${user1UpdateEvents.length} update events`,
      );
      console.log(`User 2 received ${user2UpdateEvents.length} update events`);

      // Each user should only see updates to their own perspectives
      expect(user1UpdateEvents.length).to.equal(
        1,
        "User 1 should only receive updates for their own perspective",
      );
      expect(user2UpdateEvents.length).to.equal(
        1,
        "User 2 should only receive updates for their own perspective",
      );

      expect(user1UpdateEvents[0].uuid).to.equal(user1Perspective.uuid);
      expect(user2UpdateEvents[0].uuid).to.equal(user2Perspective.uuid);

      expect(user1UpdateEvents[0].name).to.equal("User 1 Updated Name");
      expect(user2UpdateEvents[0].name).to.equal("User 2 Updated Name");
    });

    it("should only notify users about links in their own perspectives", async () => {
      console.log(
        "\n=== Testing perspective_link_added subscription filtering ===",
      );

      // Create two users
      await adminAd4mClient!.agent.createUser(
        "linkuser1@example.com",
        "password1",
      );
      await adminAd4mClient!.agent.createUser(
        "linkuser2@example.com",
        "password2",
      );

      const token1 = await adminAd4mClient!.agent.loginUser(
        "linkuser1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "linkuser2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Create perspectives for both users
      const user1Perspective =
        await client1.perspective.add("User 1 Link Test");
      const user2Perspective =
        await client2.perspective.add("User 2 Link Test");

      // Track events
      const user1LinkEvents: any[] = [];
      const user2LinkEvents: any[] = [];

      // Subscribe to perspective_link_added for each user's perspective
      console.log("Subscribing users to perspective_link_added...");
      // @ts-ignore
      client1.perspective.addPerspectiveLinkAddedListener(
        user1Perspective.uuid,
        [
          // @ts-ignore
          (link) => {
            console.log(
              `User 1 received link added event in perspective ${user1Perspective.uuid}`,
            );
            user1LinkEvents.push(link);
          },
        ],
      );

      // @ts-ignore
      client2.perspective.addPerspectiveLinkAddedListener(
        user2Perspective.uuid,
        [
          // @ts-ignore
          (link) => {
            console.log(
              `User 2 received link added event in perspective ${user2Perspective.uuid}`,
            );
            user2LinkEvents.push(link);
          },
        ],
      );

      await sleep(1000);

      // User 1 adds a link to their perspective
      console.log("\nUser 1 adding link to their perspective...");
      await client1.perspective.addLink(user1Perspective.uuid, {
        source: "test://user1",
        target: "test://data1",
        predicate: "test://has",
      });

      await sleep(2000);

      // User 2 adds a link to their perspective
      console.log("\nUser 2 adding link to their perspective...");
      await client2.perspective.addLink(user2Perspective.uuid, {
        source: "test://user2",
        target: "test://data2",
        predicate: "test://has",
      });

      await sleep(2000);

      console.log(`\nUser 1 received ${user1LinkEvents.length} link events`);
      console.log(`User 2 received ${user2LinkEvents.length} link events`);

      console.log(user1LinkEvents);
      console.log(user2LinkEvents);

      // Each user should ONLY see link events for links they added; they should NOT see the other user's
      const user1HasCorrectLink = user1LinkEvents.some(
        (event) =>
          event.data.source === "test://user1" &&
          event.data.target === "test://data1",
      );
      const user1HasOtherUserLink = user1LinkEvents.some(
        (event) =>
          event.data.source === "test://user2" &&
          event.data.target === "test://data2",
      );
      const user2HasCorrectLink = user2LinkEvents.some(
        (event) =>
          event.data.source === "test://user2" &&
          event.data.target === "test://data2",
      );
      const user2HasOtherUserLink = user2LinkEvents.some(
        (event) =>
          event.data.source === "test://user1" &&
          event.data.target === "test://data1",
      );

      expect(
        user1HasCorrectLink,
        "User 1 should receive events for their own link",
      ).to.be.true;
      expect(
        user1HasOtherUserLink,
        "User 1 should NOT receive events for User 2's link",
      ).to.be.false;
      expect(
        user2HasCorrectLink,
        "User 2 should receive events for their own link",
      ).to.be.true;
      expect(
        user2HasOtherUserLink,
        "User 2 should NOT receive events for User 1's link",
      ).to.be.false;

      expect(user1LinkEvents[0].data.source).to.equal("test://user1");
      expect(user2LinkEvents[0].data.source).to.equal("test://user2");
    });

    it("should only notify users about removal of their own perspectives", async () => {
      console.log(
        "\n=== Testing perspectiveRemoved subscription filtering ===",
      );

      // Create two users
      await adminAd4mClient!.agent.createUser(
        "remove1@example.com",
        "password1",
      );
      await adminAd4mClient!.agent.createUser(
        "remove2@example.com",
        "password2",
      );

      const token1 = await adminAd4mClient!.agent.loginUser(
        "remove1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "remove2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Create perspectives for both users
      const user1Perspective =
        await client1.perspective.add("User 1 Remove Test");
      const user2Perspective =
        await client2.perspective.add("User 2 Remove Test");

      // Track events
      const user1RemoveEvents: string[] = [];
      const user2RemoveEvents: string[] = [];

      // Subscribe to perspectiveRemoved
      console.log("Subscribing users to perspectiveRemoved...");
      // @ts-ignore
      client1.perspective.addPerspectiveRemovedListener((uuid) => {
        console.log(`User 1 received perspectiveRemoved event: ${uuid}`);
        user1RemoveEvents.push(uuid);
      });
      client1.perspective.subscribePerspectiveRemoved();

      // @ts-ignore
      client2.perspective.addPerspectiveRemovedListener((uuid) => {
        console.log(`User 2 received perspectiveRemoved event: ${uuid}`);
        user2RemoveEvents.push(uuid);
      });
      client2.perspective.subscribePerspectiveRemoved();

      await sleep(1000);

      // User 1 removes their perspective
      console.log("\nUser 1 removing their perspective...");
      await client1.perspective.remove(user1Perspective.uuid);

      await sleep(2000);

      // User 2 removes their perspective
      console.log("\nUser 2 removing their perspective...");
      await client2.perspective.remove(user2Perspective.uuid);

      await sleep(2000);

      console.log(
        `\nUser 1 received ${user1RemoveEvents.length} removal events`,
      );
      console.log(`User 2 received ${user2RemoveEvents.length} removal events`);

      // Each user should only see removal of their own perspectives
      expect(user1RemoveEvents.length).to.equal(
        1,
        "User 1 should only be notified about their own perspective removal",
      );
      expect(user2RemoveEvents.length).to.equal(
        1,
        "User 2 should only be notified about their own perspective removal",
      );

      expect(user1RemoveEvents[0]).to.equal(user1Perspective.uuid);
      expect(user2RemoveEvents[0]).to.equal(user2Perspective.uuid);
    });
  });
});
