import fs from 'fs-extra'
import path from 'path'
import { isProcessRunning } from "../utils/utils";
import { Ad4mClient } from "@perspect3vism/ad4m";
import { fileURLToPath } from 'url';
import { expect } from "chai";
import { startExecutor, apolloClient } from "../utils/utils";
import { ChildProcess } from 'child_process';
import perspectiveTests from "./perspective";
import agentTests from "./agent";
import languageTests from "./language";
import expressionTests from "./expression";
import neighbourhoodTests from "./neighbourhood";
import runtimeTests from "./runtime";
import { Crypto } from "@peculiar/webcrypto"
import directMessageTests from "./direct-messages";
import agentLanguageTests from "./agent-language";
import socialDNATests from "./social-dna-flow";
import fetch from "node-fetch";

//@ts-ignore
global.fetch = fetch

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

//@ts-ignore
global.crypto = new Crypto();

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
    const ipfsRepoPath = path.join(appDataPath)
    const gqlPort = 15000
    const hcAdminPort = 15001
    const hcAppPort = 15002
    const ipfsSwarmPort = 15005

    let executorProcess: ChildProcess | null = null

    before(async () => {    
        if(!fs.existsSync(TEST_DIR)) {
          throw Error("Please ensure that prepare-test is run before running tests!");
        }
        if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
          fs.mkdirSync(path.join(TEST_DIR, 'agents'))
        if(!fs.existsSync(appDataPath))
            fs.mkdirSync(appDataPath)

        executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
          gqlPort, hcAdminPort, hcAppPort, ipfsSwarmPort);

        testContext.alice = new Ad4mClient(apolloClient(gqlPort))
        testContext.aliceCore = executorProcess
    })

    after(async () => {
      expect(await isProcessRunning("holochain")).to.be.true;
      expect(fs.existsSync(path.join(ipfsRepoPath, "repo.lock"))).to.be.true;

      if (executorProcess) {
          executorProcess.kill()
      }

      expect(await isProcessRunning("holochain")).to.be.false;
      expect(fs.existsSync(path.join(ipfsRepoPath, "repo.lock"))).to.be.false;
    })

    describe('Agent / Agent-Setup', agentTests(testContext))
    describe('Runtime', runtimeTests(testContext))
    describe('Expression', expressionTests(testContext))
    //describe('Perspective', perspectiveTests(testContext))
    //describe('Social DNA', socialDNATests(testContext))

    // describe('with Alice and Bob', () => {
    //     let bobExecutorProcess: ChildProcess | null = null
    //     before(async () => {
    //       const bobAppDataPath = path.join(TEST_DIR, 'agents', 'bob')
    //       const bobBootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    //       const bobGqlPort = 14000
    //       const bobHcAdminPort = 12000
    //       const bobHcAppPort = 11337
    //       const bobIpfsSwarmPort = 14002

    //       if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
    //         fs.mkdirSync(path.join(TEST_DIR, 'agents'))
    //       if(!fs.existsSync(bobAppDataPath))
    //         fs.mkdirSync(bobAppDataPath)

    //       bobExecutorProcess = await startExecutor(bobAppDataPath, bobBootstrapSeedPath,
    //         bobGqlPort, bobHcAdminPort, bobHcAppPort, bobIpfsSwarmPort);

    //       testContext.bob = new Ad4mClient(apolloClient(bobGqlPort))
    //       testContext.bobCore = bobExecutorProcess
    //       await testContext.bob.agent.generate("passphrase")

    //       const status = await testContext.bob.agent.status()

    //       expect(status.isInitialized).to.be.true;
    //       expect(status.isUnlocked).to.be.true;
    //       //await testContext.makeAllNodesKnown()
    //     })

    //     after(async () => {
    //       if (bobExecutorProcess) {
    //           bobExecutorProcess.kill()
    //       }
    //       await new Promise((resolve)=>setTimeout(resolve, 500))
    //     })

    //     describe('Agent Language', agentLanguageTests(testContext))
    //     // describe('Direct Messages', directMessageTests(testContext))
    //     // describe('Language', languageTests(testContext))
    //     // describe('Neighbourhood', neighbourhoodTests(testContext))
    // })
})