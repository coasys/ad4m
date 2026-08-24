// Re-export TestContext for backwards compat (any file still importing from here)
export { TestContext } from './test-context'
import { TestContext } from './test-context'

import fs from 'fs-extra'
import path from 'path'
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
import autoProcessorNeighbourhoodTests from "./auto-processor-neighbourhood";
import crossPeerShapeSyncTests from "./cross-peer-shape-sync";
import runtimeTests from "./runtime";
import flatLanguageTests from "./flat-language.test";
import agentLanguageTests from "./agent-language";
import socialDNATests from "./social-dna-flow";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = `${__dirname}/../tst-tmp`

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
        })

        after(async () => {
          if (bobExecutorProcess) {
            await quitExecutor(bobExecutorProcess, bobApiPort);
          }
          deregisterPorts([bobApiPort, bobHcAdminPort, bobHcAppPort]);
        })

        describe('Agent Language', agentLanguageTests(testContext))
        describe('Language', languageTests(testContext))
        describe('Neighbourhood', neighbourhoodTests(testContext))
        describe('Auto-processor (two executors)', autoProcessorNeighbourhoodTests(testContext))
        describe('Cross-peer SHACL shape sync', crossPeerShapeSyncTests(testContext))
    })
})