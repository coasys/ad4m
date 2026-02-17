import fs from 'fs-extra'
import path from 'path'
import { isProcessRunning, sleep } from "../utils/utils";
import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective } from "@coasys/ad4m";
import { fileURLToPath } from 'url';
import { expect } from "chai";
import { startExecutor, apolloClient, runHcLocalServices } from "../utils/utils";
import { ChildProcess } from 'child_process';
import perspectiveTests from "./perspective";
import agentTests from "./agent";
import aiTests from "./ai";
import languageTests from "./language";
import expressionTests from "./expression";
import neighbourhoodTests from "./neighbourhood";
import runtimeTests from "./runtime";
//import { Crypto } from "@peculiar/webcrypto"
import directMessageTests from "./direct-messages";
import agentLanguageTests from "./agent-language";
import socialDNATests from "./social-dna-flow";
import fetch from "node-fetch";

//@ts-ignore
global.fetch = fetch

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
      const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
      const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();
    
      await this.#alice!.runtime.hcAddAgentInfos(bobAgentInfo);
      await this.#bob!.runtime.hcAddAgentInfos(aliceAgentInfo);
    }
}
let testContext: TestContext = new TestContext()

describe("Integration tests", function () {
    //@ts-ignore
    this.timeout(200000)
    const appDataPath = path.join(TEST_DIR, 'agents', 'alice')
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 15300
    const hcAdminPort = 15301
    const hcAppPort = 15302

    let executorProcess: ChildProcess | null = null

    let proxyUrl: string | null = null;
    let bootstrapUrl: string | null = null;
    let localServicesProcess: ChildProcess | null = null;
    let relayUrl: string | null = null;

    before(async () => {    
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
          gqlPort, hcAdminPort, hcAppPort, false, undefined, proxyUrl!, bootstrapUrl!, relayUrl!);

        testContext.alice = new Ad4mClient(apolloClient(gqlPort))
        testContext.aliceCore = executorProcess
    })

    after(async () => {
      if (executorProcess) {
        while (!executorProcess?.killed) {
          let status  = executorProcess?.kill();
          console.log("killed executor with", status);
          await sleep(500);
        }
      }
      if (localServicesProcess) {
        while (!localServicesProcess?.killed) {
          let status  = localServicesProcess?.kill();
          console.log("killed local services with", status);
          await sleep(500);
        }
      }
    })

    describe('Agent / Agent-Setup', agentTests(testContext))
    describe('Artificial Intelligence', aiTests(testContext))
    describe('Runtime', runtimeTests(testContext))
    describe('Expression', expressionTests(testContext))
    describe('Perspective', perspectiveTests(testContext))
    describe('Social DNA', socialDNATests(testContext))

    describe('with Alice and Bob', () => {
        let bobExecutorProcess: ChildProcess | null = null
        before(async () => {
          const bobAppDataPath = path.join(TEST_DIR, 'agents', 'bob')
          const bobBootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
          const bobGqlPort = 15400
          const bobHcAdminPort = 15401
          const bobHcAppPort = 15402

          if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
            fs.mkdirSync(path.join(TEST_DIR, 'agents'))
          if(!fs.existsSync(bobAppDataPath))
            fs.mkdirSync(bobAppDataPath)

          bobExecutorProcess = await startExecutor(bobAppDataPath, bobBootstrapSeedPath,
            bobGqlPort, bobHcAdminPort, bobHcAppPort, false, undefined, proxyUrl!, bootstrapUrl!, relayUrl!);

          testContext.bob = new Ad4mClient(apolloClient(bobGqlPort))
          testContext.bobCore = bobExecutorProcess
          await testContext.bob.agent.generate("passphrase")

          const status = await testContext.bob.agent.status()

          expect(status.isInitialized).to.be.true;
          expect(status.isUnlocked).to.be.true;

          let link = new LinkExpression();
          link.author = "did:test";
          link.timestamp = new Date().toISOString();
          link.data = new Link({source: "src", target: "target", predicate: "pred"});
          link.proof = new ExpressionProof("sig", "key")

          await testContext.bob.agent.updatePublicPerspective(new Perspective([link]))

          await testContext.makeAllNodesKnown()
        })

        after(async () => {
          if (executorProcess) {
            while (!bobExecutorProcess?.killed) {
              let status  = bobExecutorProcess?.kill();
              console.log("killed bobs executor with", status);
              await sleep(500);
            }
          }
        })

        describe('Agent Language', agentLanguageTests(testContext))
        describe('Language', languageTests(testContext))
        describe('Neighbourhood', neighbourhoodTests(testContext))
        //describe('Direct Messages', directMessageTests(testContext))
    })
})