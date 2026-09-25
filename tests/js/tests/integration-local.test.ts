/**
 * Local-only integration tests — no Holochain.
 *
 * Runs the same single-node test suites as integration.test.ts but with
 * local bootstrap languages and --run-holochain false. No kitsune bootstrap
 * server, no HC conductor — much faster and zero network overhead.
 *
 * Alice and Bob share languages and neighbourhoods through the shared mode of
 * the local language-language and neighbourhood store (utils/sharedStores.ts),
 * so the suites that only need that sharing run here: Shared language store
 * and Language. The suites that need links to sync between nodes
 * (Neighbourhood, Auto-processor, Cross-peer SHACL shape sync) stay in
 * integration.test.ts, the multi-node Holochain suite.
 */
import fs from 'fs-extra'
import path from 'path'
import { Ad4mClient } from "@coasys/ad4m";
import { fileURLToPath } from 'url';
import { startExecutor, baseUrl, quitExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { ChildProcess } from 'child_process';
import { TestContext } from './test-context';
import perspectiveTests from "./perspective";
import agentTests from "./agent";
import aiTests from "./ai";
import expressionTests from "./expression";
import runtimeTests from "./runtime";
import shaclRpcTests from "./shacl-rpc";
import flatLanguageTests from "./flat-language.test";
import languageTests from "./language";
import sharedLanguageStoreTests from "./shared-language-store";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = `${__dirname}/../tst-tmp`

let testContext: TestContext = new TestContext()
testContext.holochain = false

describe("Local integration tests (no Holochain)", function () {
    //@ts-ignore
    this.timeout(200000)
    const appDataPath = path.join(TEST_DIR, 'agents', 'alice-local')
    let apiPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;

    let executorProcess: ChildProcess | null = null

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

        // No HC local services — executor runs with --run-holochain false.
        executorProcess = await startLocalExecutor(appDataPath, apiPort, hcAdminPort, hcAppPort);

        testContext.alice = new Ad4mClient(baseUrl(apiPort))
        testContext.aliceCore = executorProcess
    })

    after(async () => {
      if (executorProcess) {
        await quitExecutor(executorProcess, apiPort);
      }
      deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
    })

    describe('Agent / Agent-Setup', agentTests(testContext))
    describe('Artificial Intelligence', aiTests(testContext))
    describe('Runtime', runtimeTests(testContext, { hasHolochain: false }))
    describe('Expression', expressionTests(testContext))
    describe('Perspective', perspectiveTests(testContext))
    describe('SHACL RPC', shaclRpcTests(testContext))
    describe('Flat Language (new flat export pattern)', flatLanguageTests(testContext))

    describe('with Alice and Bob', () => {
        const bobAppDataPath = path.join(TEST_DIR, 'agents', 'bob-local')
        let bobExecutorProcess: ChildProcess | null = null
        let bobPorts: number[] = []

        before(async () => {
            bobPorts = await getFreePorts(3);
            registerPorts(bobPorts);
            const [bobApiPort, bobHcAdminPort, bobHcAppPort] = bobPorts;
            bobExecutorProcess = await startLocalExecutor(bobAppDataPath, bobApiPort, bobHcAdminPort, bobHcAppPort);
            testContext.bob = new Ad4mClient(baseUrl(bobApiPort))
            testContext.bobCore = bobExecutorProcess
            await testContext.bob.agent.generate("passphrase")
        })

        after(async () => {
            if (bobExecutorProcess) {
                await quitExecutor(bobExecutorProcess, bobPorts[0]);
            }
            deregisterPorts(bobPorts);
        })

        describe('Shared language store', sharedLanguageStoreTests(testContext))
        describe('Language', languageTests(testContext))
    })
})

function startLocalExecutor(appDataPath: string, apiPort: number, hcAdminPort: number, hcAppPort: number) {
    return startExecutor(
        appDataPath, path.join(`${__dirname}/../bootstrapSeed.json`),
        apiPort, hcAdminPort, hcAppPort,
        false,          // languageLanguageOnly
        undefined,      // adminCredential
        undefined,      // proxyUrl
        undefined,      // bootstrapUrl
        undefined,      // relayUrl
        false,          // enableMcp
        undefined,      // mcpPort
        undefined,      // dynamicClassTools
        false,          // runHolochain
    );
}
