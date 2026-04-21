import fs from 'fs-extra'
import path from 'path'
import { isProcessRunning, sleep } from "../utils/utils";
import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective } from "@coasys/ad4m";
import { fileURLToPath } from 'url';
import { expect } from "chai";
import { startExecutor, baseUrl, runHcLocalServices, quitExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { ChildProcess } from 'child_process';
import perspectiveTests from "./perspective";
import agentTests from "./agent";
import aiTests from "./ai";
import languageTests from "./language";
import expressionTests from "./expression";
import neighbourhoodTests from "./neighbourhood";
import runtimeTests from "./runtime";
import flatLanguageTests from "./flat-language.test";
//import { Crypto } from "@peculiar/webcrypto"
import agentLanguageTests from "./agent-language";
import socialDNATests from "./social-dna-flow";

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
    let gqlPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;

    let executorProcess: ChildProcess | null = null

    let proxyUrl: string | null = null;
    let bootstrapUrl: string | null = null;
    let localServicesProcess: ChildProcess | null = null;
    let relayUrl: string | null = null;

    before(async () => {
        [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
        registerPorts([gqlPort, hcAdminPort, hcAppPort]);
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

        testContext.alice = new Ad4mClient(baseUrl(gqlPort))
        testContext.aliceCore = executorProcess
    })

    after(async () => {
      if (executorProcess) {
        await quitExecutor(executorProcess, gqlPort);
      }
      if (localServicesProcess) {
        localServicesProcess.kill('SIGKILL');
      }
      deregisterPorts([gqlPort, hcAdminPort, hcAppPort]);
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
        let bobGqlPort: number;
        let bobHcAdminPort: number;
        let bobHcAppPort: number;
        before(async () => {
          const bobAppDataPath = path.join(TEST_DIR, 'agents', 'bob')
          const bobBootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
          [bobGqlPort, bobHcAdminPort, bobHcAppPort] = await getFreePorts(3);
          registerPorts([bobGqlPort, bobHcAdminPort, bobHcAppPort]);

          if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
            fs.mkdirSync(path.join(TEST_DIR, 'agents'))
          if(!fs.existsSync(bobAppDataPath))
            fs.mkdirSync(bobAppDataPath)

          bobExecutorProcess = await startExecutor(bobAppDataPath, bobBootstrapSeedPath,
            bobGqlPort, bobHcAdminPort, bobHcAppPort, false, undefined, proxyUrl!, bootstrapUrl!, relayUrl!);

          testContext.bob = new Ad4mClient(baseUrl(bobGqlPort))
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
        })

        after(async () => {
          if (bobExecutorProcess) {
            await quitExecutor(bobExecutorProcess, bobGqlPort);
          }
          deregisterPorts([bobGqlPort, bobHcAdminPort, bobHcAppPort]);
        })

        describe('Agent Language', agentLanguageTests(testContext))
        describe('Language', languageTests(testContext))
        describe('Neighbourhood', neighbourhoodTests(testContext))
    })
})