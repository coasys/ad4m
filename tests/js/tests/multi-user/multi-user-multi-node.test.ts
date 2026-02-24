import path from "path";
import {
  Ad4mClient,
  Link,
  LinkQuery,
  Perspective,
  PerspectiveUnsignedInput,
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

describe("Multi-Node Multi-User Integration tests", () => {
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

    const appDataPath = path.join(TEST_DIR, "agents", "multi-user-node1");
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

  describe("Multi-Node Multi-User Integration", () => {
    // Test with 2 nodes, each with 2 users (4 users total)
    const node2AppDataPath = path.join(TEST_DIR, "agents", "multi-user-node2");
    let node2GqlPort: number = 0;
    let node2HcAdminPort: number = 0;
    let node2HcAppPort: number = 0;

    let node2ExecutorProcess: ChildProcess | null = null;
    let node2AdminClient: Ad4mClient | null = null;

    // User clients for node 1
    let node1User1Client: Ad4mClient | null = null;
    let node1User2Client: Ad4mClient | null = null;
    let node1User1Did: string = "";
    let node1User2Did: string = "";

    // User clients for node 2
    let node2User1Client: Ad4mClient | null = null;
    let node2User2Client: Ad4mClient | null = null;
    let node2User1Did: string = "";
    let node2User2Did: string = "";

    before(async function () {
      this.timeout(300000); // Increase timeout for setup with Holochain 0.7.0

      [node2GqlPort, node2HcAdminPort, node2HcAppPort] = await getFreePorts(3);

      console.log("\n=== Setting up Node 2 ===");
      if (!fs.existsSync(node2AppDataPath)) {
        fs.mkdirSync(node2AppDataPath, { recursive: true });
      }

      // Start node 2 executor with local services
      node2ExecutorProcess = await startExecutor(
        node2AppDataPath,
        bootstrapSeedPath,
        node2GqlPort,
        node2HcAdminPort,
        node2HcAppPort,
        false,
        undefined,
        proxyUrl!,
        bootstrapUrl!,
      );

      // @ts-ignore
      node2AdminClient = new Ad4mClient(apolloClient(node2GqlPort), false);
      await node2AdminClient.agent.generate("passphrase");
      await node2AdminClient.runtime.setMultiUserEnabled(true);

      console.log("\n=== Creating users on Node 1 ===");
      // Create and login 2 users on node 1
      await adminAd4mClient!.agent.createUser(
        "node1user1@example.com",
        "password1",
      );
      const node1User1Token = await adminAd4mClient!.agent.loginUser(
        "node1user1@example.com",
        "password1",
      );
      // @ts-ignore
      node1User1Client = new Ad4mClient(
        apolloClient(gqlPort, node1User1Token),
        false,
      );
      const node1User1Agent = await node1User1Client.agent.me();
      node1User1Did = node1User1Agent.did;
      console.log("Node 1 User 1 DID:", node1User1Did);

      await adminAd4mClient!.agent.createUser(
        "node1user2@example.com",
        "password2",
      );
      const node1User2Token = await adminAd4mClient!.agent.loginUser(
        "node1user2@example.com",
        "password2",
      );
      // @ts-ignore
      node1User2Client = new Ad4mClient(
        apolloClient(gqlPort, node1User2Token),
        false,
      );
      const node1User2Agent = await node1User2Client.agent.me();
      node1User2Did = node1User2Agent.did;
      console.log("Node 1 User 2 DID:", node1User2Did);

      console.log("\n=== Creating users on Node 2 ===");
      // Create and login 2 users on node 2
      await node2AdminClient.agent.createUser(
        "node2user1@example.com",
        "password3",
      );
      const node2User1Token = await node2AdminClient.agent.loginUser(
        "node2user1@example.com",
        "password3",
      );
      // @ts-ignore
      node2User1Client = new Ad4mClient(
        apolloClient(node2GqlPort, node2User1Token),
        false,
      );
      const node2User1Agent = await node2User1Client.agent.me();
      node2User1Did = node2User1Agent.did;
      console.log("Node 2 User 1 DID:", node2User1Did);

      await node2AdminClient.agent.createUser(
        "node2user2@example.com",
        "password4",
      );
      const node2User2Token = await node2AdminClient.agent.loginUser(
        "node2user2@example.com",
        "password4",
      );
      // @ts-ignore
      node2User2Client = new Ad4mClient(
        apolloClient(node2GqlPort, node2User2Token),
        false,
      );
      const node2User2Agent = await node2User2Client.agent.me();
      node2User2Did = node2User2Agent.did;
      console.log("Node 2 User 2 DID:", node2User2Did);

      // Make nodes known to each other (for Holochain peer discovery)
      console.log("\n=== Making nodes known to each other ===");
      const node1AgentInfos = await adminAd4mClient!.runtime.hcAgentInfos();
      const node2AgentInfos = await node2AdminClient.runtime.hcAgentInfos();
      await adminAd4mClient!.runtime.hcAddAgentInfos(node2AgentInfos);
      await node2AdminClient.runtime.hcAddAgentInfos(node1AgentInfos);

      console.log("\n=== Setup complete ===\n");
    });

    after(async function () {
      this.timeout(20000);
      if (node2ExecutorProcess) {
        while (!node2ExecutorProcess?.killed) {
          let status = node2ExecutorProcess?.kill();
          console.log("killed node 2 executor with", status);
          await sleep(500);
        }
      }
    });

    it("should return all DIDs in 'others()' for each user", async function () {
      this.timeout(240000); // Increased for Holochain 0.7.0 - allow initial wait + polling time

      console.log("\n=== Testing 'others()' functionality ===");

      // Node 1 User 1 creates and publishes a neighbourhood
      const perspective = await node1User1Client!.perspective.add(
        "Multi-Node Test Neighbourhood",
      );

      // Add initial link
      await node1User1Client!.perspective.addLink(perspective.uuid, {
        source: "test://root",
        target: "test://data",
        predicate: "test://contains",
      });

      // Clone and publish neighbourhood
      const linkLanguage =
        await node1User1Client!.languages.applyTemplateAndPublish(
          DIFF_SYNC_OFFICIAL,
          JSON.stringify({ uid: uuidv4(), name: "Multi-Node Test" }),
        );

      const neighbourhoodUrl =
        await node1User1Client!.neighbourhood.publishFromPerspective(
          perspective.uuid,
          linkLanguage.address,
          new Perspective([]),
        );

      console.log("Published neighbourhood:", neighbourhoodUrl);

      // All other users join the neighbourhood
      console.log("Node 1 User 2 joining...");
      await node1User2Client!.neighbourhood.joinFromUrl(neighbourhoodUrl);
      await sleep(2000); // Wait for join to complete

      console.log("Node 2 User 1 joining...");
      await node2User1Client!.neighbourhood.joinFromUrl(neighbourhoodUrl);
      await sleep(2000); // Wait for join to complete

      console.log("Node 2 User 2 joining...");
      await node2User2Client!.neighbourhood.joinFromUrl(neighbourhoodUrl);
      await sleep(2000); // Wait for join to complete

      // Re-exchange agent infos with retry to handle K2 space initialization delays (Holochain 0.7.0)
      console.log(
        "Re-exchanging agent infos with retry for K2 space readiness...",
      );
      for (let attempt = 1; attempt <= 5; attempt++) {
        try {
          console.log(`Agent info exchange attempt ${attempt}/5`);
          const node1AgentInfos = await adminAd4mClient!.runtime.hcAgentInfos();
          const node2AgentInfos =
            await node2AdminClient!.runtime.hcAgentInfos();
          await adminAd4mClient!.runtime.hcAddAgentInfos(node2AgentInfos);
          await node2AdminClient!.runtime.hcAddAgentInfos(node1AgentInfos);
          console.log(`✅ Agent info exchange attempt ${attempt} successful`);
        } catch (error) {
          console.log(
            `⚠️  Agent info exchange attempt ${attempt} failed:`,
            error,
          );
        }
        if (attempt < 5) {
          await sleep(3000); // Wait before retry
        }
      }

      // Wait for neighbourhood to sync and owners lists to be updated
      console.log("Waiting for neighbourhood sync and owners list updates...");
      await sleep(15000);

      // Get neighbourhood proxies for each user
      const node1User1Perspectives = await node1User1Client!.perspective.all();
      const node1User1Neighbourhood = node1User1Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(node1User1Neighbourhood).to.not.be.undefined;
      const node1User1Proxy =
        await node1User1Neighbourhood!.getNeighbourhoodProxy();

      const node1User2Perspectives = await node1User2Client!.perspective.all();
      const node1User2Neighbourhood = node1User2Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(node1User2Neighbourhood).to.not.be.undefined;
      const node1User2Proxy =
        await node1User2Neighbourhood!.getNeighbourhoodProxy();

      const node2User1Perspectives = await node2User1Client!.perspective.all();
      const node2User1Neighbourhood = node2User1Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(node2User1Neighbourhood).to.not.be.undefined;
      const node2User1Proxy =
        await node2User1Neighbourhood!.getNeighbourhoodProxy();

      const node2User2Perspectives = await node2User2Client!.perspective.all();
      const node2User2Neighbourhood = node2User2Perspectives.find(
        (p) => p.sharedUrl === neighbourhoodUrl,
      );
      expect(node2User2Neighbourhood).to.not.be.undefined;
      const node2User2Proxy =
        await node2User2Neighbourhood!.getNeighbourhoodProxy();

      // Check 'others()' for each user - should see all other users' DIDs
      // Poll with retries to account for DHT gossip delays
      console.log("\nChecking 'others()' for each user:");

      // Helper function to poll until all expected DIDs are seen
      const pollUntilAllSeen = async (
        proxy: any,
        expectedDids: string[],
        userLabel: string,
        maxAttempts = 50,
      ) => {
        console.log(`Polling ${userLabel} for others...`);
        console.log(`Expected DIDs:`, expectedDids);
        for (let attempt = 1; attempt <= maxAttempts; attempt++) {
          const others = await proxy.otherAgents();
          console.log(`${userLabel} sees others (attempt ${attempt}):`, others);

          const allFound = expectedDids.every((did) => {
            console.log(`Checking if ${did} is in ${others}`);
            let result = others.includes(did);
            console.log(`Result: ${result}`);
            return result;
          });
          if (allFound) {
            console.log(`✅ ${userLabel} sees all expected users!`);
            return others;
          }

          if (attempt < maxAttempts) {
            console.log(
              `${userLabel} waiting for DHT gossip... (${attempt}/${maxAttempts})`,
            );
            await sleep(2000);
          }
        }

        // Return the last result even if not complete
        const finalOthers = await proxy.otherAgents();
        console.log(
          `${userLabel} final result after ${maxAttempts} attempts:`,
          finalOthers,
        );
        return finalOthers;
      };

      const node1User1Others = await pollUntilAllSeen(
        node1User1Proxy!,
        [node1User2Did, node2User1Did, node2User2Did],
        "Node 1 User 1",
      );
      expect(node1User1Others).to.include(
        node1User2Did,
        "Node 1 User 1 should see Node 1 User 2",
      );
      expect(node1User1Others).to.include(
        node2User1Did,
        "Node 1 User 1 should see Node 2 User 1",
      );
      expect(node1User1Others).to.include(
        node2User2Did,
        "Node 1 User 1 should see Node 2 User 2",
      );
      expect(node1User1Others).to.have.lengthOf(
        3,
        "Node 1 User 1 should see exactly 3 other users",
      );

      const node1User2Others = await pollUntilAllSeen(
        node1User2Proxy!,
        [node1User1Did, node2User1Did, node2User2Did],
        "Node 1 User 2",
      );
      console.log("Node 1 User 2 sees others:", node1User2Others);
      expect(node1User2Others).to.include(
        node1User1Did,
        "Node 1 User 2 should see Node 1 User 1",
      );
      expect(node1User2Others).to.include(
        node2User1Did,
        "Node 1 User 2 should see Node 2 User 1",
      );
      expect(node1User2Others).to.include(
        node2User2Did,
        "Node 1 User 2 should see Node 2 User 2",
      );
      expect(node1User2Others).to.have.lengthOf(
        3,
        "Node 1 User 2 should see exactly 3 other users",
      );

      const node2User1Others = await pollUntilAllSeen(
        node2User1Proxy!,
        [node1User1Did, node1User2Did, node2User2Did],
        "Node 2 User 1",
      );
      console.log("Node 2 User 1 sees others:", node2User1Others);
      expect(node2User1Others).to.include(
        node1User1Did,
        "Node 2 User 1 should see Node 1 User 1",
      );
      expect(node2User1Others).to.include(
        node1User2Did,
        "Node 2 User 1 should see Node 1 User 2",
      );
      expect(node2User1Others).to.include(
        node2User2Did,
        "Node 2 User 1 should see Node 2 User 2",
      );
      expect(node2User1Others).to.have.lengthOf(
        3,
        "Node 2 User 1 should see exactly 3 other users",
      );

      const node2User2Others = await pollUntilAllSeen(
        node2User2Proxy!,
        [node1User1Did, node1User2Did, node2User1Did],
        "Node 2 User 2",
      );
      console.log("Node 2 User 2 sees others:", node2User2Others);
      expect(node2User2Others).to.include(
        node1User1Did,
        "Node 2 User 2 should see Node 1 User 1",
      );
      expect(node2User2Others).to.include(
        node1User2Did,
        "Node 2 User 2 should see Node 1 User 2",
      );
      expect(node2User2Others).to.include(
        node2User1Did,
        "Node 2 User 2 should see Node 2 User 1",
      );
      expect(node2User2Others).to.have.lengthOf(
        3,
        "Node 2 User 2 should see exactly 3 other users",
      );

      console.log("\n✅ All users correctly see all other users via others()");
    });

    it("should route p2p signals correctly between users across nodes", async function () {
      this.timeout(120000); // Increased for Holochain 0.7.0

      console.log("\n=== Testing cross-node p2p signal routing ===");

      // Get neighbourhood URL from previous test
      const node1User1Perspectives = await node1User1Client!.perspective.all();
      const node1User1Neighbourhood = node1User1Perspectives.find(
        (p) => p.sharedUrl,
      );
      expect(node1User1Neighbourhood).to.not.be.undefined;
      const node1User1Proxy =
        await node1User1Neighbourhood!.getNeighbourhoodProxy();

      const node2User1Perspectives = await node2User1Client!.perspective.all();
      const node2User1Neighbourhood = node2User1Perspectives.find(
        (p) => p.sharedUrl,
      );
      expect(node2User1Neighbourhood).to.not.be.undefined;
      const node2User1Proxy =
        await node2User1Neighbourhood!.getNeighbourhoodProxy();

      const node2User2Perspectives = await node2User2Client!.perspective.all();
      const node2User2Neighbourhood = node2User2Perspectives.find(
        (p) => p.sharedUrl,
      );
      expect(node2User2Neighbourhood).to.not.be.undefined;
      const node2User2Proxy =
        await node2User2Neighbourhood!.getNeighbourhoodProxy();

      // Set up signal handlers
      const node2User1ReceivedSignals: any[] = [];
      node2User1Proxy!.addSignalHandler((signal) => {
        console.log("Node 2 User 1 received signal from:", signal.author);
        node2User1ReceivedSignals.push(signal);
      });

      const node2User2ReceivedSignals: any[] = [];
      node2User2Proxy!.addSignalHandler((signal) => {
        console.log("Node 2 User 2 received signal from:", signal.author);
        node2User2ReceivedSignals.push(signal);
      });

      await sleep(3000); // Let handlers initialize and subscriptions become active

      // Node 2 User 1 sends a signal to Node 2 User 2 (both on same node - local routing)
      console.log(
        `\nNode 2 User 1 (${node2User1Did.substring(0, 20)}...) sending signal to Node 2 User 2 (${node2User2Did.substring(0, 20)}...)`,
      );
      await node2User1Proxy!.sendSignalU(
        node2User2Did,
        new PerspectiveUnsignedInput([
          {
            source: "test://signal0",
            predicate: "test://from",
            target: node2User1Did,
          },
        ]),
      );

      // Wait for signal delivery
      console.log("Waiting for signal delivery...");
      const maxWaitTime = 20000;
      let startTime = Date.now();
      while (
        node2User2ReceivedSignals.length === 0 &&
        Date.now() - startTime < maxWaitTime
      ) {
        await sleep(100);
        console.log(".");
      }

      console.log("Signal delivery complete");

      // Verify Node 2 User 2 received the signal
      console.log("Node 2 User 2 received signals:", node2User2ReceivedSignals);
      expect(node2User2ReceivedSignals.length).to.be.greaterThan(
        0,
        "Node 2 User 2 should have received signal",
      );
      expect(node2User2ReceivedSignals[0].author).to.equal(node2User1Did);

      let node2user2signalCount = node2User2ReceivedSignals.length;

      // Node 1 User 1 sends a signal to Node 2 User 1
      console.log(
        `\nNode 1 User 1 (${node1User1Did.substring(0, 20)}...) sending signal to Node 2 User 1 (${node2User1Did.substring(0, 20)}...)`,
      );
      await node1User1Proxy!.sendSignalU(
        node2User1Did,
        new PerspectiveUnsignedInput([
          {
            source: "test://signal1",
            predicate: "test://from",
            target: node1User1Did,
          },
        ]),
      );

      // Wait for signal delivery
      startTime = Date.now();
      console.log("Waiting for signal delivery...");
      while (
        node2User1ReceivedSignals.length === 0 &&
        Date.now() - startTime < maxWaitTime
      ) {
        await sleep(100);
        console.log(".");
      }
      console.log("Signal delivery complete");

      // Verify Node 2 User 1 received the signal
      console.log("Node 2 User 1 received signals:", node2User1ReceivedSignals);
      expect(node2User1ReceivedSignals.length).to.be.greaterThan(
        0,
        "Node 2 User 1 should have received signal",
      );
      expect(node2User1ReceivedSignals[0].author).to.equal(node1User1Did);

      // Verify Node 2 User 2 did NOT receive the signal (it was meant for Node 2 User 1)
      expect(node2User2ReceivedSignals.length).to.equal(
        node2user2signalCount,
        "Node 2 User 2 should NOT have received signal meant for Node 2 User 1",
      );

      // Now test the reverse: Node 2 User 1 sends to Node 1 User 1
      const node1User1ReceivedSignals: any[] = [];
      node1User1Proxy!.addSignalHandler((signal) => {
        console.log("Node 1 User 1 received signal from:", signal.author);
        node1User1ReceivedSignals.push(signal);
      });

      await sleep(1500);

      console.log(
        `\nNode 2 User 1 (${node2User1Did.substring(0, 20)}...) sending signal to Node 1 User 1 (${node1User1Did.substring(0, 20)}...)`,
      );
      await node2User1Proxy!.sendSignalU(
        node1User1Did,
        new PerspectiveUnsignedInput([
          {
            source: "test://signal2",
            predicate: "test://from",
            target: node2User1Did,
          },
        ]),
      );

      startTime = Date.now();
      while (
        node1User1ReceivedSignals.length === 0 &&
        Date.now() - startTime < maxWaitTime
      ) {
        await sleep(100);
      }

      expect(node1User1ReceivedSignals.length).to.be.greaterThan(
        0,
        "Node 1 User 1 should have received signal",
      );
      expect(node1User1ReceivedSignals[0].author).to.equal(node2User1Did);

      console.log("\n✅ Cross-node p2p signal routing works correctly");
    });

    it("should sync links correctly between all users across nodes", async function () {
      this.timeout(180000); // Increased for Holochain 0.7.0 - link sync takes longer

      console.log("\n=== Testing cross-node link synchronization ===");

      // Get neighbourhood perspectives for all users
      const node1User1Perspectives = await node1User1Client!.perspective.all();
      const node1User1Neighbourhood = node1User1Perspectives.find(
        (p) => p.sharedUrl,
      );
      expect(node1User1Neighbourhood).to.not.be.undefined;

      const node1User2Perspectives = await node1User2Client!.perspective.all();
      const node1User2Neighbourhood = node1User2Perspectives.find(
        (p) => p.sharedUrl,
      );
      expect(node1User2Neighbourhood).to.not.be.undefined;

      const node2User1Perspectives = await node2User1Client!.perspective.all();
      const node2User1Neighbourhood = node2User1Perspectives.find(
        (p) => p.sharedUrl,
      );
      expect(node2User1Neighbourhood).to.not.be.undefined;

      const node2User2Perspectives = await node2User2Client!.perspective.all();
      const node2User2Neighbourhood = node2User2Perspectives.find(
        (p) => p.sharedUrl,
      );
      expect(node2User2Neighbourhood).to.not.be.undefined;

      // Each user adds a unique link
      console.log("\nEach user adding their own unique link...");

      await node1User1Client!.perspective.addLink(
        node1User1Neighbourhood!.uuid,
        {
          source: "test://node1user1",
          target: "test://link1",
          predicate: "test://created",
        },
      );
      console.log("Node 1 User 1 added link");

      await node1User2Client!.perspective.addLink(
        node1User2Neighbourhood!.uuid,
        {
          source: "test://node1user2",
          target: "test://link2",
          predicate: "test://created",
        },
      );
      console.log("Node 1 User 2 added link");

      await node2User1Client!.perspective.addLink(
        node2User1Neighbourhood!.uuid,
        {
          source: "test://node2user1",
          target: "test://link3",
          predicate: "test://created",
        },
      );
      console.log("Node 2 User 1 added link");

      await node2User2Client!.perspective.addLink(
        node2User2Neighbourhood!.uuid,
        {
          source: "test://node2user2",
          target: "test://link4",
          predicate: "test://created",
        },
      );
      console.log("Node 2 User 2 added link");

      // Wait for synchronization
      console.log("\nWaiting for sync...");
      await sleep(10000);

      // Query links from each user's perspective
      console.log("\nQuerying links from each user's perspective...");

      const node1User1Links = await node1User1Client!.perspective.queryLinks(
        node1User1Neighbourhood!.uuid,
        new LinkQuery({}),
      );
      console.log(`Node 1 User 1 sees ${node1User1Links.length} links`);

      const node1User2Links = await node1User2Client!.perspective.queryLinks(
        node1User2Neighbourhood!.uuid,
        new LinkQuery({}),
      );
      console.log(`Node 1 User 2 sees ${node1User2Links.length} links`);

      const node2User1Links = await node2User1Client!.perspective.queryLinks(
        node2User1Neighbourhood!.uuid,
        new LinkQuery({}),
      );
      console.log(`Node 2 User 1 sees ${node2User1Links.length} links`);

      const node2User2Links = await node2User2Client!.perspective.queryLinks(
        node2User2Neighbourhood!.uuid,
        new LinkQuery({}),
      );
      console.log(`Node 2 User 2 sees ${node2User2Links.length} links`);

      // All users should see at least 5 links (1 from setup + 4 from each user)
      expect(node1User1Links.length).to.be.greaterThanOrEqual(
        5,
        "Node 1 User 1 should see all links",
      );
      expect(node1User2Links.length).to.be.greaterThanOrEqual(
        5,
        "Node 1 User 2 should see all links",
      );
      expect(node2User1Links.length).to.be.greaterThanOrEqual(
        5,
        "Node 2 User 1 should see all links",
      );
      expect(node2User2Links.length).to.be.greaterThanOrEqual(
        5,
        "Node 2 User 2 should see all links",
      );

      // Verify each user sees all the specific links
      const checkLinkExists = (
        links: any[],
        source: string,
        target: string,
      ) => {
        return links.some(
          (l) => l.data.source === source && l.data.target === target,
        );
      };

      // Check Node 1 User 1 sees all links
      expect(
        checkLinkExists(node1User1Links, "test://node1user1", "test://link1"),
      ).to.be.true;
      expect(
        checkLinkExists(node1User1Links, "test://node1user2", "test://link2"),
      ).to.be.true;
      expect(
        checkLinkExists(node1User1Links, "test://node2user1", "test://link3"),
      ).to.be.true;
      expect(
        checkLinkExists(node1User1Links, "test://node2user2", "test://link4"),
      ).to.be.true;

      // Check Node 1 User 2 sees all links
      expect(
        checkLinkExists(node1User2Links, "test://node1user1", "test://link1"),
      ).to.be.true;
      expect(
        checkLinkExists(node1User2Links, "test://node1user2", "test://link2"),
      ).to.be.true;
      expect(
        checkLinkExists(node1User2Links, "test://node2user1", "test://link3"),
      ).to.be.true;
      expect(
        checkLinkExists(node1User2Links, "test://node2user2", "test://link4"),
      ).to.be.true;

      // Check Node 2 User 1 sees all links
      expect(
        checkLinkExists(node2User1Links, "test://node1user1", "test://link1"),
      ).to.be.true;
      expect(
        checkLinkExists(node2User1Links, "test://node1user2", "test://link2"),
      ).to.be.true;
      expect(
        checkLinkExists(node2User1Links, "test://node2user1", "test://link3"),
      ).to.be.true;
      expect(
        checkLinkExists(node2User1Links, "test://node2user2", "test://link4"),
      ).to.be.true;

      // Check Node 2 User 2 sees all links
      expect(
        checkLinkExists(node2User2Links, "test://node1user1", "test://link1"),
      ).to.be.true;
      expect(
        checkLinkExists(node2User2Links, "test://node1user2", "test://link2"),
      ).to.be.true;
      expect(
        checkLinkExists(node2User2Links, "test://node2user1", "test://link3"),
      ).to.be.true;
      expect(
        checkLinkExists(node2User2Links, "test://node2user2", "test://link4"),
      ).to.be.true;

      // Verify link authors are correct
      const node1User1Link = node1User1Links.find(
        (l) => l.data.source === "test://node1user1",
      );
      expect(node1User1Link?.author).to.equal(node1User1Did);

      const node1User2Link = node1User1Links.find(
        (l) => l.data.source === "test://node1user2",
      );
      expect(node1User2Link?.author).to.equal(node1User2Did);

      const node2User1Link = node1User1Links.find(
        (l) => l.data.source === "test://node2user1",
      );
      expect(node2User1Link?.author).to.equal(node2User1Did);

      const node2User2Link = node1User1Links.find(
        (l) => l.data.source === "test://node2user2",
      );
      expect(node2User2Link?.author).to.equal(node2User2Did);

      console.log("\n✅ Cross-node link synchronization works correctly");
    });
  });
});
