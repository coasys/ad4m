/**
 * Local-only integration tests — no Holochain.
 *
 * Runs the same single-node test suites as integration.test.ts but with
 * local bootstrap languages and --run-holochain false. No kitsune bootstrap
 * server, no HC conductor — much faster and zero network overhead.
 *
 * The Alice+Bob tests (Agent Language, Language, Neighbourhood) that exercise
 * Holochain cross-peer sync remain in integration.test.ts.
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
import flatLanguageTests from "./flat-language.test";
import socialDNATests from "./social-dna-flow";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = `${__dirname}/../tst-tmp`

let testContext: TestContext = new TestContext()

describe("Local integration tests (no Holochain)", function () {
    //@ts-ignore
    this.timeout(200000)
    const appDataPath = path.join(TEST_DIR, 'agents', 'alice-local')
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
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

        // Pre-populate language bundles from the publish step.
        // In HC mode the HC DHT distributes published languages across agents;
        // in local mode there is no shared network, so we copy them on disk.
        const publishedLangsDir = path.join(TEST_DIR, 'published-languages');
        const testLangsDir = path.join(appDataPath, 'ad4m', 'languages');
        if (fs.existsSync(publishedLangsDir)) {
            fs.ensureDirSync(testLangsDir);
            fs.copySync(publishedLangsDir, testLangsDir, { overwrite: true });
        }

        // No HC local services — executor runs with --run-holochain false.
        // HC proxy/bootstrap URLs are ignored but we pass defaults since the
        // params are positional.
        executorProcess = await startExecutor(
            appDataPath, bootstrapSeedPath,
            apiPort, hcAdminPort, hcAppPort,
            false,                                              // languageLanguageOnly
            undefined,                                          // adminCredential
            "wss://dev-test-bootstrap2.holochain.org",          // proxyUrl (ignored)
            "https://dev-test-bootstrap2.holochain.org",        // bootstrapUrl (ignored)
            undefined,                                          // relayUrl
            false,                                              // enableMcp
            undefined,                                          // mcpPort
            false,                                              // runHolochain
        );

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
    describe('Social DNA', socialDNATests(testContext))
    describe('Flat Language (new flat export pattern)', flatLanguageTests(testContext))
})
