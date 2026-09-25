/**
 * Local-only integration tests — no Holochain.
 *
 * Runs the same single-node test suites as integration.test.ts but with
 * local bootstrap languages and --run-holochain false. No kitsune bootstrap
 * server, no HC conductor — much faster and zero network overhead.
 *
 * Alice and Bob share languages, neighbourhoods and agent profiles through the
 * shared mode of the local stores (utils/sharedStores.ts). Links sync between
 * them through the server-link-language and a link-server started here, so
 * this suite is also the multi-node suite for that link language: the
 * Neighbourhood, Auto-processor and Cross-peer SHACL shape sync suites run on
 * the [server-link] config. Their p-diff-sync legs stay in integration.test.ts,
 * the multi-node Holochain suite.
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
import neighbourhoodTests from "./neighbourhood";
import agentLanguageTests from "./agent-language";
import autoProcessorNeighbourhoodTests from "./auto-processor-neighbourhood";
import crossPeerShapeSyncTests from "./cross-peer-shape-sync";
import { startLinkServer, LinkServerHandle } from "../utils/linkServer";
import { LinkLangConfig, serverLinkLang } from "../utils/linkLangConfig";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = `${__dirname}/../tst-tmp`

// Published by prepare-test (publishTestLangs.ts). The link-server leg
// below needs it; fail loudly rather than silently dropping the leg.
const SERVER_LINK_HASH_PATH = "./scripts/server-link-language-hash";
const SERVER_LINK_HASH = fs.existsSync(SERVER_LINK_HASH_PATH) ? fs.readFileSync(SERVER_LINK_HASH_PATH).toString().trim() : "";

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
        // One link-server for every server-link neighbourhood in this block;
        // per-neighbourhood isolation comes from the UID template param.
        let linkServer: LinkServerHandle | null = null
        let serverLinkConfig: LinkLangConfig | null = null
        const getServerLinkConfig = () => {
            if (!serverLinkConfig) throw new Error("server-link config not initialised — before() didn't run?");
            return serverLinkConfig;
        }

        before(async () => {
            bobPorts = await getFreePorts(3);
            registerPorts(bobPorts);
            const [bobApiPort, bobHcAdminPort, bobHcAppPort] = bobPorts;
            bobExecutorProcess = await startLocalExecutor(bobAppDataPath, bobApiPort, bobHcAdminPort, bobHcAppPort);
            testContext.bob = new Ad4mClient(baseUrl(bobApiPort))
            testContext.bobCore = bobExecutorProcess
            await testContext.bob.agent.generate("passphrase")

            if (!SERVER_LINK_HASH) {
                throw new Error(
                    `[integration-local] ${SERVER_LINK_HASH_PATH} is missing or empty. ` +
                    `Server-link-language did not publish during prepare-test — ` +
                    `fix that before running this suite (the link-server leg must not be dropped silently).`,
                );
            }
            linkServer = await startLinkServer();
            serverLinkConfig = serverLinkLang(SERVER_LINK_HASH, linkServer.url);
        })

        after(async () => {
            if (bobExecutorProcess) {
                await quitExecutor(bobExecutorProcess, bobPorts[0]);
            }
            if (linkServer) {
                await linkServer.kill();
                linkServer = null;
            }
            deregisterPorts(bobPorts);
        })

        describe('Shared stores', sharedLanguageStoreTests(testContext))
        describe('Agent Language', agentLanguageTests(testContext, true))
        describe('Language', languageTests(testContext))
        describe('Neighbourhood [server-link]', neighbourhoodTests(testContext, getServerLinkConfig))
        describe('Auto-processor (two executors) [server-link]', autoProcessorNeighbourhoodTests(testContext, getServerLinkConfig))
        describe('Cross-peer SHACL shape sync [server-link]', crossPeerShapeSyncTests(testContext, getServerLinkConfig))
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
