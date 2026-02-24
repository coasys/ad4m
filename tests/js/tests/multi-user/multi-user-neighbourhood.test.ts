import path from "path";
import {
  Ad4mClient,
  Ad4mModel,
  Link,
  LinkQuery,
  Model,
  Perspective,
  PerspectiveUnsignedInput,
  Property,
} from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from "url";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import {
  apolloClient,
  sleep,
  startExecutor,
  runHcLocalServices,
} from "../../utils/utils";
import { getFreePorts } from "../../helpers/ports";
import { ChildProcess } from "node:child_process";
import fetch from "node-fetch";
import { v4 as uuidv4 } from "uuid";

//@ts-ignore
global.fetch = fetch;

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const DIFF_SYNC_OFFICIAL = fs
  .readFileSync("./scripts/perspective-diff-sync-hash")
  .toString();

describe("Multi-User Neighbourhood Sharing tests", () => {
  const TEST_DIR = path.join(`${__dirname}/../../tst-tmp`);
  const bootstrapSeedPath = path.join(`${__dirname}/../../bootstrapSeed.json`);

  let gqlPort: number = 0;
  let hcAdminPort: number = 0;
  let hcAppPort: number = 0;
  let executorProcess: ChildProcess | null = null;
  let adminAd4mClient: Ad4mClient | null = null;
  let proxyUrl: string | null = null;
  let bootstrapUrl: string | null = null;
  let localServicesProcess: ChildProcess | null = null;

  before(async function () {
    this.timeout(120_000);
    [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);

    const appDataPath = path.join(
      TEST_DIR,
      "agents",
      "multi-user-neighbourhood",
    );
    if (!fs.existsSync(appDataPath)) {
      fs.mkdirSync(appDataPath, { recursive: true });
    }

    const localServices = await runHcLocalServices();
    proxyUrl = localServices.proxyUrl;
    bootstrapUrl = localServices.bootstrapUrl;
    localServicesProcess = localServices.process;

    executorProcess = await startExecutor(
      appDataPath,
      bootstrapSeedPath,
      gqlPort,
      hcAdminPort,
      hcAppPort,
      false,
      undefined,
      proxyUrl!,
      bootstrapUrl!,
    );

    // @ts-ignore
    adminAd4mClient = new Ad4mClient(apolloClient(gqlPort), false);
    await adminAd4mClient.agent.generate("passphrase");
    await adminAd4mClient.runtime.setMultiUserEnabled(true);
  });

  after(async () => {
    if (executorProcess) {
      while (!executorProcess?.killed) {
        executorProcess.kill();
        await sleep(500);
      }
    }
    if (localServicesProcess) {
      while (!localServicesProcess?.killed) {
        localServicesProcess.kill();
        await sleep(500);
      }
    }
  });

  describe("Multi-User Neighbourhood Sharing", () => {
    it("should allow multiple local users to share the same neighbourhood", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "nh1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "nh2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "nh1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "nh2@example.com",
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

      // User 1 creates a perspective and shares it as a neighbourhood
      const perspective1 = await client1.perspective.add(
        "Shared Neighbourhood",
      );
      console.log("User 1 created perspective:", perspective1.uuid);

      // Add some initial links to the perspective
      const link1 = new Link({
        source: "user1",
        target: "data1",
        predicate: "test://created",
      });
      await client1.perspective.addLink(perspective1.uuid, link1);

      console.log("Cloning link language...");
      const linkLanguage = await client1.languages.applyTemplateAndPublish(
        DIFF_SYNC_OFFICIAL,
        JSON.stringify({
          uid: uuidv4(),
          name: "Multi-User Neighbourhood Sharing",
        }),
      );
      console.log("Link language cloned:", linkLanguage.address);

      // Publish the neighbourhood using the centralized link language
      console.log("Publishing neighbourhood...");
      const neighbourhoodUrl =
        await client1.neighbourhood.publishFromPerspective(
          perspective1.uuid,
          linkLanguage.address,
          new Perspective([]),
        );
      console.log("User 1 published neighbourhood:", neighbourhoodUrl);

      // Wait for neighbourhood to be fully set up
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // User 2 joins the same neighbourhood
      const joinResult =
        await client2.neighbourhood.joinFromUrl(neighbourhoodUrl);
      console.log("User 2 joined neighbourhood:", joinResult);

      // Wait for neighbourhood sync
      await new Promise((resolve) => setTimeout(resolve, 2000));

      // Verify both users can see the shared perspective
      const user1Perspectives = await client1.perspective.all();
      const user2Perspectives = await client2.perspective.all();

      const user1SharedPerspective = user1Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      const user2SharedPerspective = user2Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );

      console.log("User 1 perspectives:", user1Perspectives);
      console.log("User 2 perspectives:", user2Perspectives);

      expect(user1SharedPerspective).to.not.be.null;
      expect(user2SharedPerspective).to.not.be.null;

      // User 2 adds a link to the shared perspective
      const link2 = new Link({
        source: "user2",
        target: "data2",
        predicate: "test://added",
      });
      await client2.perspective.addLink(user2SharedPerspective!.uuid, link2);

      // Wait for sync
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // User 1 should see User 2's link
      const user1Links = await client1.perspective.queryLinks(
        user1SharedPerspective!.uuid,
        new LinkQuery({}),
      );
      const user2Links = await client2.perspective.queryLinks(
        user2SharedPerspective!.uuid,
        new LinkQuery({}),
      );

      console.log("User 1 sees links:", user1Links.length);
      console.log("User 2 sees links:", user2Links.length);

      // Both users should see both links
      expect(user1Links.length).to.be.greaterThan(1);
      expect(user2Links.length).to.be.greaterThan(1);

      // Verify specific links exist
      const user1SeesUser2Link = user1Links.some(
        (l) => l.data.source === "user2" && l.data.target === "data2",
      );
      const user2SeesUser1Link = user2Links.some(
        (l) => l.data.source === "user1" && l.data.target === "data1",
      );

      expect(user1SeesUser2Link).to.be.true;
      expect(user2SeesUser1Link).to.be.true;
    });

    it("should use separate prolog pools for different users in shared neighbourhood", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "prolog1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "prolog2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "prolog1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "prolog2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      console.log("User 1 creates neighbourhood and adds initial SDNA...");

      // User 1 creates a perspective and shares it as a neighbourhood
      const perspective1 = await client1.perspective.add("Prolog Pool Test");

      @Model({
        name: "User1Model",
      })
      class User1Model extends Ad4mModel {
        @Property({
          through: "test://user1-property",
          writable: true,
          initial: "test://user1-initial",
          resolveLanguage: "literal",
        })
        user1Property: string = "";
      }

      console.log("Ensuring User 1 model...");
      await perspective1.ensureSDNASubjectClass(User1Model);
      console.log("User 1 model ensured");

      // Wait for SDNA to be processed
      await new Promise((resolve) => setTimeout(resolve, 1000));

      let user1Model = new User1Model(perspective1);
      user1Model.user1Property = "User1 created this";
      console.log("Saving User 1 model...");
      await user1Model.save();
      console.log("User 1 model saved");

      console.log("User 1 neighbourhood setup complete, User 2 joining...");

      // Clone link language and publish neighbourhood
      const linkLanguage = await client1.languages.applyTemplateAndPublish(
        DIFF_SYNC_OFFICIAL,
        JSON.stringify({ uid: uuidv4(), name: "Prolog Pool Test" }),
      );
      const neighbourhoodUrl =
        await client1.neighbourhood.publishFromPerspective(
          perspective1.uuid,
          linkLanguage.address,
          new Perspective([]),
        );

      // User 2 joins the neighbourhood
      const joinResult =
        await client2.neighbourhood.joinFromUrl(neighbourhoodUrl);
      const user2Perspectives = await client2.perspective.all();
      const user2SharedPerspective = user2Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(user2SharedPerspective).to.not.be.null;

      console.log("User 2 joined, adding their own SDNA...");

      @Model({
        name: "User2Model",
      })
      class User2Model extends Ad4mModel {
        @Property({
          through: "test://user2-property",
          writable: true,
          initial: "test://user2-initial",
          resolveLanguage: "literal",
        })
        user2Property: string = "";
      }

      console.log("Ensuring User 2 model...");
      await user2SharedPerspective!.ensureSDNASubjectClass(User2Model);
      console.log("User 2 model ensured");

      // Wait for SDNA to be processed
      await new Promise((resolve) => setTimeout(resolve, 1000));

      let user2Model = new User2Model(user2SharedPerspective!);
      user2Model.user2Property = "User2 created this";
      console.log("Saving User 2 model...");
      await user2Model.save();
      console.log("User 2 model saved");

      console.log("Testing prolog pool isolation...");

      let classesSeenByUser1 = await perspective1.subjectClasses();
      console.log("User 1 sees classes:", classesSeenByUser1);
      expect(classesSeenByUser1.length).to.equal(1);

      let classesSeenByUser2 = await user2SharedPerspective!.subjectClasses();
      console.log("User 2 sees classes:", classesSeenByUser2);
      expect(classesSeenByUser2.length).to.equal(2);
    });

    it("should route neighbourhood signals locally between users on the same node", async () => {
      // Create two users
      const user1Result = await adminAd4mClient!.agent.createUser(
        "signal1@example.com",
        "password1",
      );
      const user2Result = await adminAd4mClient!.agent.createUser(
        "signal2@example.com",
        "password2",
      );

      // Login both users
      const token1 = await adminAd4mClient!.agent.loginUser(
        "signal1@example.com",
        "password1",
      );
      const token2 = await adminAd4mClient!.agent.loginUser(
        "signal2@example.com",
        "password2",
      );

      // @ts-ignore - Suppress Apollo type mismatch
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore - Suppress Apollo type mismatch
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get user DIDs
      const user1Status = await client1.agent.status();
      const user2Status = await client2.agent.status();
      const user1Did = user1Status.did!;
      const user2Did = user2Status.did!;

      console.log("User 1 DID:", user1Did);
      console.log("User 2 DID:", user2Did);

      // User 1 creates a perspective and shares it as a neighbourhood
      const perspective1 = await client1.perspective.add(
        "Signal Test Neighbourhood",
      );

      // Clone link language and publish neighbourhood
      const linkLanguage = await client1.languages.applyTemplateAndPublish(
        DIFF_SYNC_OFFICIAL,
        JSON.stringify({ uid: uuidv4(), name: "Signal Test" }),
      );
      const neighbourhoodUrl =
        await client1.neighbourhood.publishFromPerspective(
          perspective1.uuid,
          linkLanguage.address,
          new Perspective([]),
        );

      console.log("User 1 created neighbourhood:", neighbourhoodUrl);

      // User 2 joins the neighbourhood
      const joinResult =
        await client2.neighbourhood.joinFromUrl(neighbourhoodUrl);
      const user2Perspectives = await client2.perspective.all();
      const user2SharedPerspective = user2Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(user2SharedPerspective).to.not.be.null;

      console.log("User 2 joined neighbourhood");

      // Wait a bit for neighbourhood to be fully set up
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Get neighbourhood proxy for User 2
      const user2Neighbourhood =
        await user2SharedPerspective!.getNeighbourhoodProxy();
      expect(user2Neighbourhood).to.not.be.null;

      // Set up signal listener for User 2
      const user2ReceivedSignals: any[] = [];
      const user2SignalSubscription = user2Neighbourhood!.addSignalHandler(
        (signal) => {
          user2ReceivedSignals.push(signal);
        },
      );

      console.log("User 2 signal listener set up");

      // Get neighbourhood proxy for User 1
      const user1Neighbourhood = await perspective1.getNeighbourhoodProxy();
      expect(user1Neighbourhood).to.not.be.null;

      // Set up signal listener for User 1 to verify they DON'T receive User 2's signals
      const user1ReceivedSignals: any[] = [];
      const user1SignalSubscription = user1Neighbourhood!.addSignalHandler(
        (signal) => {
          user1ReceivedSignals.push(signal);
        },
      );

      console.log("User 1 signal listener set up");

      // Wait a bit to ensure subscriptions are active
      await new Promise((resolve) => setTimeout(resolve, 500));

      // User 1 sends a signal to User 2
      const testSignalPayload = new PerspectiveUnsignedInput([
        {
          source: "test://signal",
          predicate: "test://from",
          target: user1Did,
        },
      ]);

      console.log("User 1 sending signal to User 2...");
      await user1Neighbourhood!.sendSignalU(user2Did, testSignalPayload);

      console.log("Signal sent, waiting for delivery...");

      // Wait for signal to be received (with timeout)
      const maxWaitTime = 5000; // 5 seconds
      let startTime = Date.now();
      while (
        user2ReceivedSignals.length === 0 &&
        Date.now() - startTime < maxWaitTime
      ) {
        await new Promise((resolve) => setTimeout(resolve, 100));
      }

      // Verify User 2 received the signal
      expect(user2ReceivedSignals.length).to.be.greaterThan(
        0,
        "User 2 should have received at least one signal",
      );

      console.log("User 2 received signals:", user2ReceivedSignals);

      const user2ReceivedSignal = user2ReceivedSignals[0];
      expect(user2ReceivedSignal.data.links).to.have.lengthOf(1);
      expect(user2ReceivedSignal.data.links[0].data.source).to.equal(
        "test://signal",
      );
      expect(user2ReceivedSignal.data.links[0].data.predicate).to.equal(
        "test://from",
      );
      expect(user2ReceivedSignal.data.links[0].data.target).to.equal(user1Did);

      // Verify User 1 did NOT receive the signal (it was meant for User 2)
      expect(user1ReceivedSignals.length).to.equal(
        0,
        "User 1 should NOT have received the signal meant for User 2",
      );

      // Now test the reverse: User 2 sends a signal to User 1
      const reverseSignalPayload = new PerspectiveUnsignedInput([
        {
          source: "test://reverse-signal",
          predicate: "test://from",
          target: user2Did,
        },
      ]);

      console.log("User 2 sending signal to User 1...");
      await user2Neighbourhood!.sendSignalU(user1Did, reverseSignalPayload);

      // Wait for signal to be received
      startTime = Date.now();
      while (
        user1ReceivedSignals.length === 0 &&
        Date.now() - startTime < maxWaitTime
      ) {
        await new Promise((resolve) => setTimeout(resolve, 100));
      }

      // Verify User 1 received the signal
      expect(user1ReceivedSignals.length).to.be.greaterThan(
        0,
        "User 1 should have received at least one signal",
      );

      const user1ReceivedSignal = user1ReceivedSignals[0];
      expect(user1ReceivedSignal.data.links).to.have.lengthOf(1);
      expect(user1ReceivedSignal.data.links[0].data.source).to.equal(
        "test://reverse-signal",
      );
      expect(user1ReceivedSignal.data.links[0].data.predicate).to.equal(
        "test://from",
      );
      expect(user1ReceivedSignal.data.links[0].data.target).to.equal(user2Did);

      // Verify User 2 did NOT receive their own signal back (User 1 should have only 1 signal from first send)
      expect(user2ReceivedSignals.length).to.equal(
        1,
        "User 2 should still only have 1 signal (not their own reverse signal)",
      );
    });

    it("should receive neighbourhood signals between two managed users (Flux scenario)", async () => {
      console.log(
        "\n=== Replicating Flux Scenario: Fresh Agent with Managed Users ===",
      );

      // Create two managed users (simulating Flux signup flow)
      console.log("Creating first managed user...");
      await adminAd4mClient!.agent.createUser("flux1@example.com", "password1");
      const token1 = await adminAd4mClient!.agent.loginUser(
        "flux1@example.com",
        "password1",
      );

      console.log("Creating second managed user...");
      await adminAd4mClient!.agent.createUser("flux2@example.com", "password2");
      const token2 = await adminAd4mClient!.agent.loginUser(
        "flux2@example.com",
        "password2",
      );

      // @ts-ignore
      const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
      // @ts-ignore
      const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

      // Get user DIDs
      const user1Status = await client1.agent.me();
      const user2Status = await client2.agent.me();
      const user1Did = user1Status.did!;
      const user2Did = user2Status.did!;

      console.log("User 1 DID:", user1Did);
      console.log("User 2 DID:", user2Did);

      // FIRST managed user creates a perspective and neighbourhood
      console.log("\nUser 1 (first managed user) creating neighbourhood...");
      const perspective1 = await client1.perspective.add(
        "Flux Test Neighbourhood",
      );

      // Add a test link
      await client1.perspective.addLink(
        perspective1.uuid,
        new Link({
          source: "test://initial",
          target: "test://data",
          predicate: "test://created_by_user1",
        }),
      );

      // Clone link language and publish neighbourhood (using Holochain p-diff-sync)
      const linkLanguage = await client1.languages.applyTemplateAndPublish(
        DIFF_SYNC_OFFICIAL,
        JSON.stringify({ uid: uuidv4(), name: "Flux Test Neighbourhood" }),
      );

      console.log("Link language cloned:", linkLanguage.address);

      const neighbourhoodUrl =
        await client1.neighbourhood.publishFromPerspective(
          perspective1.uuid,
          linkLanguage.address,
          new Perspective([]),
        );

      console.log("User 1 published neighbourhood:", neighbourhoodUrl);

      // Wait for neighbourhood to be published
      await new Promise((resolve) => setTimeout(resolve, 2000));

      // SECOND managed user joins the neighbourhood
      console.log("\nUser 2 (second managed user) joining neighbourhood...");
      const joinResult =
        await client2.neighbourhood.joinFromUrl(neighbourhoodUrl);
      console.log("User 2 join result:", joinResult.uuid);

      const user2Perspectives = await client2.perspective.all();
      const user2SharedPerspective = user2Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(user2SharedPerspective).to.not.be.null;

      console.log("User 2 joined neighbourhood");

      // Wait for neighbourhood to sync
      await new Promise((resolve) => setTimeout(resolve, 3000));

      // Get neighbourhood proxies
      const user1Neighbourhood = await perspective1.getNeighbourhoodProxy();
      const user2Neighbourhood =
        await user2SharedPerspective!.getNeighbourhoodProxy();

      expect(user1Neighbourhood).to.not.be.null;
      expect(user2Neighbourhood).to.not.be.null;

      console.log("\n=== Testing Signal Delivery ===");

      // Set up signal listeners
      const user1ReceivedSignals: any[] = [];
      const user2ReceivedSignals: any[] = [];

      const user1SignalHandler = user1Neighbourhood!.addSignalHandler(
        (signal) => {
          console.log(
            "✉️ User 1 received signal:",
            JSON.stringify(signal, null, 2),
          );
          user1ReceivedSignals.push(signal);
        },
      );

      const user2SignalHandler = user2Neighbourhood!.addSignalHandler(
        (signal) => {
          console.log(
            "✉️ User 2 received signal:",
            JSON.stringify(signal, null, 2),
          );
          user2ReceivedSignals.push(signal);
        },
      );

      console.log("Signal handlers set up for both users");

      // Wait for subscriptions to be active
      await new Promise((resolve) => setTimeout(resolve, 1000));

      // Check if users can see each other in otherAgents
      console.log("\n=== Checking otherAgents() ===");
      const user1Others = await user1Neighbourhood!.otherAgents();
      const user2Others = await user2Neighbourhood!.otherAgents();

      console.log("User 1 sees others:", user1Others);
      console.log("User 2 sees others:", user2Others);

      // User 1 sends a signal to User 2
      console.log("\n=== User 1 sending signal to User 2 ===");
      const signal1to2 = new PerspectiveUnsignedInput([
        new Link({
          source: "test://signal",
          predicate: "test://user1_to_user2",
          target: user1Did,
        }),
      ]);

      await user1Neighbourhood!.sendSignalU(user2Did, signal1to2);
      console.log("Signal sent from User 1 to User 2");

      // Wait for signal delivery
      await new Promise((resolve) => setTimeout(resolve, 2000));

      // User 2 sends a signal to User 1
      console.log("\n=== User 2 sending signal to User 1 ===");
      const signal2to1 = new PerspectiveUnsignedInput([
        new Link({
          source: "test://signal",
          predicate: "test://user2_to_user1",
          target: user2Did,
        }),
      ]);

      await user2Neighbourhood!.sendSignalU(user1Did, signal2to1);
      console.log("Signal sent from User 2 to User 1");

      // Wait for signal delivery
      await new Promise((resolve) => setTimeout(resolve, 2000));

      // Verify signals were received
      console.log("\n=== Verification ===");
      console.log(
        "User 1 received signals count:",
        user1ReceivedSignals.length,
      );
      console.log(
        "User 2 received signals count:",
        user2ReceivedSignals.length,
      );

      if (user2ReceivedSignals.length > 0) {
        console.log(
          "User 2 received signals:",
          JSON.stringify(user2ReceivedSignals, null, 2),
        );
      }
      if (user1ReceivedSignals.length > 0) {
        console.log(
          "User 1 received signals:",
          JSON.stringify(user1ReceivedSignals, null, 2),
        );
      }

      // Assertions
      expect(user2ReceivedSignals.length).to.be.greaterThan(
        0,
        "User 2 should receive signal from User 1",
      );
      expect(user1ReceivedSignals.length).to.be.greaterThan(
        0,
        "User 1 should receive signal from User 2",
      );

      console.log(
        "User 2 received signals:",
        JSON.stringify(user2ReceivedSignals, null, 2),
      );
      console.log(
        "User 1 received signals:",
        JSON.stringify(user1ReceivedSignals, null, 2),
      );
      // Verify signal content
      const user2Signal = user2ReceivedSignals[0];
      expect(user2Signal.data.links[0].data.predicate).to.equal(
        "test://user1_to_user2",
      );

      const user1Signal = user1ReceivedSignals[0];
      expect(user1Signal.data.links[0].data.predicate).to.equal(
        "test://user2_to_user1",
      );
    });

    it("should exchange neighbourhood signals between main agent and managed user", async () => {
      console.log(
        "\n=== Testing signals between main agent and managed user ===",
      );

      // The admin client (empty/admin-credential token) IS the main agent.
      // A managed user joins the same neighbourhood.  Signals must work both ways.
      const mainAgentStatus = await adminAd4mClient!.agent.status();
      const mainAgentDid = mainAgentStatus.did!;
      console.log("Main agent DID:", mainAgentDid);

      // Create and login a managed user
      await adminAd4mClient!.agent.createUser(
        "main_agent_signal@example.com",
        "password",
      );
      const userToken = await adminAd4mClient!.agent.loginUser(
        "main_agent_signal@example.com",
        "password",
      );
      // @ts-ignore
      const userClient = new Ad4mClient(
        apolloClient(gqlPort, userToken),
        false,
      );

      const userStatus = await userClient.agent.me();
      const userDid = userStatus.did!;
      console.log("Managed user DID:", userDid);

      // Main agent creates the neighbourhood
      const mainPerspective = await adminAd4mClient!.perspective.add(
        "Main-Agent Neighbourhood",
      );
      const linkLanguage =
        await adminAd4mClient!.languages.applyTemplateAndPublish(
          DIFF_SYNC_OFFICIAL,
          JSON.stringify({ uid: uuidv4(), name: "Main-Agent Signal Test" }),
        );
      const neighbourhoodUrl =
        await adminAd4mClient!.neighbourhood.publishFromPerspective(
          mainPerspective.uuid,
          linkLanguage.address,
          new Perspective([]),
        );
      console.log("Main agent created neighbourhood:", neighbourhoodUrl);
      await new Promise((resolve) => setTimeout(resolve, 2000));

      // Managed user joins the neighbourhood
      await userClient.neighbourhood.joinFromUrl(neighbourhoodUrl);
      const userPerspectives = await userClient.perspective.all();
      const userSharedPerspective = userPerspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(userSharedPerspective).to.not.be.null;
      console.log("Managed user joined neighbourhood");
      await new Promise((resolve) => setTimeout(resolve, 2000));

      // Get neighbourhood proxies for both sides
      const mainAgentNH = await mainPerspective.getNeighbourhoodProxy();
      const userNH = await userSharedPerspective!.getNeighbourhoodProxy();
      expect(mainAgentNH).to.not.be.null;
      expect(userNH).to.not.be.null;

      // Register signal listeners on both sides
      const mainAgentReceivedSignals: any[] = [];
      const userReceivedSignals: any[] = [];

      mainAgentNH!.addSignalHandler((signal) => {
        console.log("✉️ Main agent received signal:", JSON.stringify(signal));
        mainAgentReceivedSignals.push(signal);
      });
      userNH!.addSignalHandler((signal) => {
        console.log("✉️ Managed user received signal:", JSON.stringify(signal));
        userReceivedSignals.push(signal);
      });

      await new Promise((resolve) => setTimeout(resolve, 1000));

      // --- Test 1: main agent sends signal to managed user ---
      console.log("\n--- Main agent sending signal to managed user ---");
      await mainAgentNH!.sendSignalU(
        userDid,
        new PerspectiveUnsignedInput([
          new Link({
            source: "test://signal",
            predicate: "test://main_to_user",
            target: mainAgentDid,
          }),
        ]),
      );

      const maxWait = 5000;
      let start = Date.now();
      while (userReceivedSignals.length === 0 && Date.now() - start < maxWait) {
        await new Promise((r) => setTimeout(r, 100));
      }
      expect(userReceivedSignals.length).to.be.greaterThan(
        0,
        "Managed user should receive signal from main agent",
      );
      expect(userReceivedSignals[0].data.links[0].data.predicate).to.equal(
        "test://main_to_user",
      );

      // --- Test 2: managed user sends signal to main agent ---
      console.log("\n--- Managed user sending signal to main agent ---");
      await userNH!.sendSignalU(
        mainAgentDid,
        new PerspectiveUnsignedInput([
          new Link({
            source: "test://signal",
            predicate: "test://user_to_main",
            target: userDid,
          }),
        ]),
      );

      start = Date.now();
      while (
        mainAgentReceivedSignals.length === 0 &&
        Date.now() - start < maxWait
      ) {
        await new Promise((r) => setTimeout(r, 100));
      }
      expect(mainAgentReceivedSignals.length).to.be.greaterThan(
        0,
        "Main agent should receive signal from managed user",
      );
      expect(mainAgentReceivedSignals[0].data.links[0].data.predicate).to.equal(
        "test://user_to_main",
      );

      // --- Test 3: managed user broadcasts, main agent receives ---
      console.log(
        "\n--- Managed user broadcasting, main agent should receive ---",
      );
      const mainAgentCountBefore = mainAgentReceivedSignals.length;
      await userNH!.sendBroadcastU(
        new PerspectiveUnsignedInput([
          new Link({
            source: "test://broadcast",
            predicate: "test://user_broadcast",
            target: userDid,
          }),
        ]),
      );

      start = Date.now();
      while (
        mainAgentReceivedSignals.length === mainAgentCountBefore &&
        Date.now() - start < maxWait
      ) {
        await new Promise((r) => setTimeout(r, 100));
      }
      expect(mainAgentReceivedSignals.length).to.be.greaterThan(
        mainAgentCountBefore,
        "Main agent should receive broadcast from managed user",
      );
      const broadcastSignal =
        mainAgentReceivedSignals[mainAgentReceivedSignals.length - 1];
      expect(broadcastSignal.data.links[0].data.predicate).to.equal(
        "test://user_broadcast",
      );
    });
  });
});
