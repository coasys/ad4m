import fs from 'fs-extra'
import path from 'path'
import { isProcessRunning, sleep } from "../utils/utils";
import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective } from "@coasys/ad4m";
import { fileURLToPath } from 'url';
import { expect } from "chai";
import { startExecutor, baseUrl, runHcLocalServices, quitExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { startLinkServer, LinkServerHandle } from "../utils/linkServer";
import { LinkLangConfig, holochainLinkLang, serverLinkLang } from "../utils/linkLangConfig";
import { ChildProcess } from 'child_process';
import perspectiveTests from "./perspective";
import agentTests from "./agent";
import aiTests from "./ai";
import languageTests from "./language";
import expressionTests from "./expression";
import neighbourhoodTests from "./neighbourhood";
import autoProcessorNeighbourhoodTests from "./auto-processor-neighbourhood";
import crossPeerShapeSyncTests from "./cross-peer-shape-sync";
import runtimeTests from "./runtime";
import flatLanguageTests from "./flat-language.test";
//import { Crypto } from "@peculiar/webcrypto"
import agentLanguageTests from "./agent-language";
import socialDNATests from "./social-dna-flow";

// Both link-language hashes are required — prepare-test must publish both.
// The absence check happens in the outer before(), so a missing file fails
// loudly rather than silently dropping half the matrix.
const SERVER_LINK_HASH_PATH = "./scripts/server-link-language-hash";
const SERVER_LINK_HASH = fs.existsSync(SERVER_LINK_HASH_PATH)
    ? fs.readFileSync(SERVER_LINK_HASH_PATH).toString().trim()
    : "";
const DIFF_SYNC_HASH = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString().trim();

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

//@ts-ignore
//global.crypto = new Crypto();

const TEST_DIR = `${__dirname}/../tst-tmp`

export class TestContext {
    //#ad4mClient: Ad4mClient | undefined
    #alice: Ad4mClient | undefined
    #bob: Ad4mClient | undefined

    #aliceCore: ChildProcess | undefined
    #bobCore: ChildProcess | undefined

    get ad4mClient(): Ad4mClient {
      return this.#alice!
    }

    get alice(): Ad4mClient {
      return this.#alice!
    }

    get bob(): Ad4mClient {
      return this.#bob!
    }

    set alice(client: Ad4mClient) {
      this.#alice = client
    }

    set bob(client: Ad4mClient) {
      this.#bob = client
    }

    set aliceCore(aliceCore: ChildProcess) {
      this.#aliceCore = aliceCore
    }

    set bobCore(bobCore: ChildProcess) {
      this.#bobCore = bobCore
    }

    async makeAllNodesKnown() {
      for (let attempt = 1; attempt <= 5; attempt++) {
        try {
          const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
          const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();

          await this.#alice!.runtime.hcAddAgentInfos(bobAgentInfo);
          await this.#bob!.runtime.hcAddAgentInfos(aliceAgentInfo);
          console.log(`Agent info exchange attempt ${attempt} successful`);
          break;
        } catch (error) {
          console.log(`Agent info exchange attempt ${attempt} failed:`, error);
          if (attempt < 5) {
            await sleep(3000);
          }
        }
      }
    }
}
let testContext: TestContext = new TestContext()

describe("Integration tests", function () {
    //@ts-ignore
    this.timeout(200000)
    const appDataPath = path.join(TEST_DIR, 'agents', 'alice')
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    let apiPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;

    let executorProcess: ChildProcess | null = null

    let proxyUrl: string | null = null;
    let bootstrapUrl: string | null = null;
    let localServicesProcess: ChildProcess | null = null;
    let relayUrl: string | null = null;

    before(async () => {
        [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
        registerPorts([apiPort, hcAdminPort, hcAppPort]);
        if(!fs.existsSync(TEST_DIR)) {
          throw Error("Please ensure that prepare-test is run before running tests!");
        }
        if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
          fs.mkdirSync(path.join(TEST_DIR, 'agents'))
        if(!fs.existsSync(appDataPath))
            fs.mkdirSync(appDataPath)

        let localServices = await runHcLocalServices();
        proxyUrl = localServices.proxyUrl;
        bootstrapUrl = localServices.bootstrapUrl;
        localServicesProcess = localServices.process;
        relayUrl = localServices.relayUrl;

        executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
          apiPort, hcAdminPort, hcAppPort, false, undefined, proxyUrl!, bootstrapUrl!, relayUrl!);

        testContext.alice = new Ad4mClient(baseUrl(apiPort))
        testContext.aliceCore = executorProcess
    })

    after(async () => {
      if (executorProcess) {
        await quitExecutor(executorProcess, apiPort);
      }
      if (localServicesProcess) {
        localServicesProcess.kill('SIGKILL');
      }
      deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
    })

    describe('Agent / Agent-Setup', agentTests(testContext))
    describe('Artificial Intelligence', aiTests(testContext))
    describe('Runtime', runtimeTests(testContext))
    describe('Expression', expressionTests(testContext))
    describe('Perspective', perspectiveTests(testContext))
    describe('Social DNA', socialDNATests(testContext))
        describe('Flat Language (new flat export pattern)', flatLanguageTests(testContext))

        describe('with Alice and Bob', () => {
        let bobExecutorProcess: ChildProcess | null = null
        let bobApiPort: number;
        let bobHcAdminPort: number;
        let bobHcAppPort: number;
        // Link-server for the server-link-language matrix leg. Lives here (not
        // per-neighbourhood test) so multiple describe blocks share a single
        // server instance; ROOM_ID uniqueness per neighbourhood comes from
        // linkLangConfig.buildTemplateParams.
        let linkServer: LinkServerHandle | null = null;
        let serverLinkConfig: LinkLangConfig | null = null;
        const holochainConfig: LinkLangConfig = holochainLinkLang(DIFF_SYNC_HASH);
        before(async () => {
          const bobAppDataPath = path.join(TEST_DIR, 'agents', 'bob')
          const bobBootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
          [bobApiPort, bobHcAdminPort, bobHcAppPort] = await getFreePorts(3);
          registerPorts([bobApiPort, bobHcAdminPort, bobHcAppPort]);

          if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
            fs.mkdirSync(path.join(TEST_DIR, 'agents'))
          if(!fs.existsSync(bobAppDataPath))
            fs.mkdirSync(bobAppDataPath)

          bobExecutorProcess = await startExecutor(bobAppDataPath, bobBootstrapSeedPath,
            bobApiPort, bobHcAdminPort, bobHcAppPort, false, undefined, proxyUrl!, bootstrapUrl!, relayUrl!);

          testContext.bob = new Ad4mClient(baseUrl(bobApiPort))
          testContext.bobCore = bobExecutorProcess
          await testContext.bob.agent.generate("passphrase")

          const status = await testContext.bob.agent.status()

          expect(status.isInitialized).to.be.true;
          expect(status.isUnlocked).to.be.true;

          let link = new LinkExpression();
          link.author = "did:test";
          link.timestamp = new Date().toISOString();
          link.data = new Link({source: "ad4m://src", target: "test://target", predicate: "ad4m://pred"});
          link.proof = new ExpressionProof("sig", "key")

          await testContext.bob.agent.updatePublicPerspective(new Perspective([link]))

          await testContext.makeAllNodesKnown()

          // Boot a link-server for the server-link-language matrix leg.
          // Fail hard if the hash file is missing — a broken prepare-test
          // must not silently drop half the matrix; that's the exact drift
          // this suite exists to catch.
          if (!SERVER_LINK_HASH) {
              throw new Error(
                  `[integration] ${SERVER_LINK_HASH_PATH} is missing or empty. ` +
                  `Server-link-language did not publish during prepare-test — ` +
                  `fix that before running the integration suite (this test must not run without it).`,
              );
          }
          linkServer = await startLinkServer();
          serverLinkConfig = serverLinkLang(SERVER_LINK_HASH, linkServer.url);
        })

        after(async () => {
          if (bobExecutorProcess) {
            await quitExecutor(bobExecutorProcess, bobApiPort);
          }
          if (linkServer) {
              await linkServer.kill();
              linkServer = null;
          }
          deregisterPorts([bobApiPort, bobHcAdminPort, bobHcAppPort]);
        })

        describe('Agent Language', agentLanguageTests(testContext))
        describe('Language', languageTests(testContext))
        // Same neighbourhood suite, run once per link-language flavour. The
        // getter closes over the outer `serverLinkConfig` so it picks up the
        // real value after the outer before() runs.
        describe('Neighbourhood [holochain]', neighbourhoodTests(testContext, () => holochainConfig))
        describe('Neighbourhood [server-link]', neighbourhoodTests(testContext, () => {
            if (!serverLinkConfig) throw new Error("server-link config not initialised — before() didn't run?");
            return serverLinkConfig;
        }))
        describe('Auto-processor (two executors)', autoProcessorNeighbourhoodTests(testContext))
        describe('Cross-peer SHACL shape sync', crossPeerShapeSyncTests(testContext))
    })
})