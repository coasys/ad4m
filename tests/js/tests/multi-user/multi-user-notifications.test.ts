import { Ad4mClient } from "@coasys/ad4m";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep } from "../../utils/utils";
import { startAgent } from "../../helpers/executor";
import type { AgentHandle } from "../../helpers/executor";
import { NotificationInput } from "@coasys/ad4m/lib/src/runtime/RuntimeResolver";

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Multi-User Notifications tests", () => {
  let agentHandle: AgentHandle | null = null;
  let adminAd4mClient: Ad4mClient | null = null;
  let gqlPort: number = 0;

  before(async function () {
    this.timeout(300_000);
    agentHandle = await startAgent("multi-user-notifications");
    adminAd4mClient = agentHandle.client;
    gqlPort = agentHandle.gqlPort;
    await adminAd4mClient.runtime.setMultiUserEnabled(true);
  });

  after(async () => {
    await agentHandle?.stop();
  });

  describe("Multi-User Notifications", () => {
    it("should isolate notifications between users", async () => {
      console.log("\n=== Testing notification isolation between users ===");

      // Create two users
      await adminAd4mClient!.agent.createUser(
        "notify1@example.com",
        "password1",
      );
      await adminAd4mClient!.agent.createUser(
        "notify2@example.com",
        "password2",
      );

      const token1 = await adminAd4mClient!.agent.loginUser(
        "notify1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "notify2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // User 1 creates a perspective and notification
      const user1Perspective = await client1.perspective.add(
        "User 1 Notification Test",
      );
      const user1Notification: NotificationInput = {
        description: "User 1's notification",
        appName: "User 1 App",
        appUrl: "https://user1.app",
        appIconPath: "/user1.png",
        trigger: `SELECT source, target FROM link WHERE predicate = 'user1://test'`,
        perspectiveIds: [user1Perspective.uuid],
        webhookUrl: "https://user1.webhook",
        webhookAuth: "user1-auth",
      };

      // User 2 creates a perspective and notification
      const user2Perspective = await client2.perspective.add(
        "User 2 Notification Test",
      );
      const user2Notification: NotificationInput = {
        description: "User 2's notification",
        appName: "User 2 App",
        appUrl: "https://user2.app",
        appIconPath: "/user2.png",
        trigger: `SELECT source, target FROM link WHERE predicate = 'user2://test'`,
        perspectiveIds: [user2Perspective.uuid],
        webhookUrl: "https://user2.webhook",
        webhookAuth: "user2-auth",
      };

      // Install notifications - managed users get auto-granted
      const notif1Id =
        await client1.runtime.requestInstallNotification(user1Notification);
      const notif2Id =
        await client2.runtime.requestInstallNotification(user2Notification);

      await sleep(500);

      // User 1 retrieves notifications - should only see their own and it should be auto-granted
      const user1Notifications = await client1.runtime.notifications();
      console.log(`User 1 sees ${user1Notifications.length} notification(s)`);
      expect(user1Notifications.length).to.equal(1);
      expect(user1Notifications[0].description).to.equal(
        "User 1's notification",
      );
      expect(user1Notifications[0].id).to.equal(notif1Id);
      expect(user1Notifications[0].granted).to.be.true;

      // User 2 retrieves notifications - should only see their own and it should be auto-granted
      const user2Notifications = await client2.runtime.notifications();
      console.log(`User 2 sees ${user2Notifications.length} notification(s)`);
      expect(user2Notifications.length).to.equal(1);
      expect(user2Notifications[0].description).to.equal(
        "User 2's notification",
      );
      expect(user2Notifications[0].id).to.equal(notif2Id);
      expect(user2Notifications[0].granted).to.be.true;
    });

    it("should use correct agent DID for each user's notification queries", async () => {
      console.log(
        "\n=== Testing per-user agent DID in notification queries ===",
      );

      // Create two users
      await adminAd4mClient!.agent.createUser("did1@example.com", "password1");
      await adminAd4mClient!.agent.createUser("did2@example.com", "password2");

      const token1 = await adminAd4mClient!.agent.loginUser(
        "did1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "did2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get each user's agent DID
      const user1Status = await client1.agent.status();
      const user2Status = await client2.agent.status();
      const user1Did = user1Status.did;
      const user2Did = user2Status.did;

      console.log(`User 1 DID: ${user1Did}`);
      console.log(`User 2 DID: ${user2Did}`);

      expect(user1Did).to.not.equal(
        user2Did,
        "Users should have different DIDs",
      );

      // Create perspectives for both users
      const user1Perspective = await client1.perspective.add(
        "User 1 Mention Test",
      );
      const user2Perspective = await client2.perspective.add(
        "User 2 Mention Test",
      );

      // User 1 creates a mention notification (using $agentDid variable)
      const user1Notification: NotificationInput = {
        description: "User 1 was mentioned",
        appName: "Mentions for User 1",
        appUrl: "https://mentions.app",
        appIconPath: "/mentions.png",
        trigger: `SELECT
                    source as message_id,
                    fn::parse_literal(target) as content,
                    $agentDid as mentioned_user
                FROM link
                WHERE predicate = 'rdf://content'
                    AND fn::contains(fn::parse_literal(target), $agentDid)`,
        perspectiveIds: [user1Perspective.uuid],
        webhookUrl: "https://user1.webhook",
        webhookAuth: "user1-auth",
      };

      // User 2 creates a mention notification (using $agentDid variable)
      const user2Notification: NotificationInput = {
        description: "User 2 was mentioned",
        appName: "Mentions for User 2",
        appUrl: "https://mentions.app",
        appIconPath: "/mentions.png",
        trigger: `SELECT
                    source as message_id,
                    fn::parse_literal(target) as content,
                    $agentDid as mentioned_user
                FROM link
                WHERE predicate = 'rdf://content'
                    AND fn::contains(fn::parse_literal(target), $agentDid)`,
        perspectiveIds: [user2Perspective.uuid],
        webhookUrl: "https://user2.webhook",
        webhookAuth: "user2-auth",
      };

      // Install notifications - managed users get auto-granted
      const notif1Id =
        await client1.runtime.requestInstallNotification(user1Notification);
      const notif2Id =
        await client2.runtime.requestInstallNotification(user2Notification);

      await sleep(500);

      // Verify that both notifications contain the $agentDid variable in their triggers
      // and are auto-granted for managed users
      const user1Notifs = await client1.runtime.notifications();
      const user2Notifs = await client2.runtime.notifications();

      const user1SavedNotif = user1Notifs.find((n) => n.id === notif1Id);
      const user2SavedNotif = user2Notifs.find((n) => n.id === notif2Id);

      expect(user1SavedNotif).to.not.be.undefined;
      expect(user2SavedNotif).to.not.be.undefined;

      // Verify the triggers contain the $agentDid placeholder
      expect(user1SavedNotif!.trigger).to.include(
        "$agentDid",
        "User 1's notification should contain $agentDid variable",
      );
      expect(user2SavedNotif!.trigger).to.include(
        "$agentDid",
        "User 2's notification should contain $agentDid variable",
      );

      // The actual DID injection and query execution is tested in the runtime.ts tests
      // This test verifies that multi-user contexts preserve the query correctly
    });

    it("should prevent users from seeing or modifying other users' notifications", async () => {
      console.log("\n=== Testing notification access control ===");

      // Create two users
      await adminAd4mClient!.agent.createUser(
        "access1@example.com",
        "password1",
      );
      await adminAd4mClient!.agent.createUser(
        "access2@example.com",
        "password2",
      );

      const token1 = await adminAd4mClient!.agent.loginUser(
        "access1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "access2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      const perspective = await client1.perspective.add("Access Test");

      // User 1 creates a notification
      const notification: NotificationInput = {
        description: "Private to User 1",
        appName: "Private App",
        appUrl: "https://private.app",
        appIconPath: "/private.png",
        trigger: `SELECT * FROM link WHERE predicate = 'test://private'`,
        perspectiveIds: [perspective.uuid],
        webhookUrl: "https://webhook.test",
        webhookAuth: "secret-auth",
      };

      // Install notification - managed users get auto-granted
      const notificationId =
        await client1.runtime.requestInstallNotification(notification);
      await sleep(500);

      // User 1 can see their notification and it should be auto-granted
      const user1Notifs = await client1.runtime.notifications();
      const user1Notif = user1Notifs.find((n) => n.id === notificationId);
      expect(user1Notif).to.not.be.undefined;
      expect(user1Notif!.granted).to.be.true;

      // User 2 cannot see User 1's notification
      const user2Notifs = await client2.runtime.notifications();
      expect(user2Notifs.some((n) => n.id === notificationId)).to.be.false;
    });

    it("should prevent managed users from calling grantNotification", async () => {
      console.log(
        "\n=== Testing that managed users cannot call grantNotification ===",
      );

      // Create a managed user
      await adminAd4mClient!.agent.createUser(
        "grant-test@example.com",
        "password1",
      );
      const token = await adminAd4mClient!.agent.loginUser(
        "grant-test@example.com",
        "password1",
      );

      // @ts-ignore
      const managedClient = new Ad4mClient(apolloClient(gqlPort, token), false);

      const perspective = await managedClient.perspective.add("Grant Test");

      // Create a notification (which will be auto-granted)
      const notification: NotificationInput = {
        description: "Test notification",
        appName: "Test App",
        appUrl: "https://test.app",
        appIconPath: "/test.png",
        trigger: `SELECT * FROM link WHERE predicate = 'test://grant'`,
        perspectiveIds: [perspective.uuid],
        webhookUrl: "https://webhook.test",
        webhookAuth: "test-auth",
      };

      const notificationId =
        await managedClient.runtime.requestInstallNotification(notification);
      await sleep(500);

      // Verify the notification is auto-granted
      const notifs = await managedClient.runtime.notifications();
      const notif = notifs.find((n) => n.id === notificationId);
      expect(notif).to.not.be.undefined;
      expect(notif!.granted).to.be.true;

      // Managed user should NOT be able to call grantNotification
      try {
        await managedClient.runtime.grantNotification(notificationId);
        expect.fail(
          "Managed user should not be able to call grantNotification",
        );
      } catch (error: any) {
        expect(error.message).to.include("Permission denied");
      }
    });
  });
});
