/**
 * Multi-node integration tests over the server-link-language — no Holochain.
 *
 * Alice and Bob run with --run-holochain false on the local bootstrap
 * languages. They share languages, neighbourhoods and agent profiles
 * through the shared mode of the local stores (utils/sharedStores.ts), and
 * links sync between them through the server-link-language and the
 * link-server this file starts (utils/linkServer.ts). The suites that
 * need link sync run here on the [server-link] config; their p-diff-sync legs
 * run in integration.test.ts, the multi-node Holochain suite.
 *
 * The link server speaks protocol 2 and admits no one automatically: a room
 * is made by its creator's person (`neighbourhood.create`) and a key gets in
 * only as a member. So the link server starts with an identity store, both
 * executors run against that store with the centralized agent language (it
 * resolves people), and each gets a person from the test holder: Alice makes
 * the spaces, Bob joins them (`join: open`).
 *
 * CI job: integration-tests-multi-node-server-link (pnpm run test-main-server-link).
 */
import fs from 'fs-extra'
import path from 'path'
import { Ad4mClient } from "@coasys/ad4m";
import { fileURLToPath } from 'url';
import { startExecutor, baseUrl, quitExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { ChildProcess } from 'child_process';
import { TestContext } from './test-context';
import neighbourhoodTests from "./neighbourhood";
import autoProcessorNeighbourhoodTests from "./auto-processor-neighbourhood";
import crossPeerShapeSyncTests from "./cross-peer-shape-sync";
import flowReplicaJoinTests from "./flow/flow-replica-join";
import { startLinkServer, LinkServerHandle } from "../utils/linkServer";
import { LinkLangConfig, serverLinkLang } from "../utils/linkLangConfig";
import { givePerson, Person, writePersonSeed } from "../helpers/identity";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = `${__dirname}/../tst-tmp`

// Published by prepare-test (publishTestLangs.ts). Fail loudly rather than
// silently running nothing.
const SERVER_LINK_HASH_PATH = "./scripts/server-link-language-hash";
const SERVER_LINK_HASH = fs.existsSync(SERVER_LINK_HASH_PATH) ? fs.readFileSync(SERVER_LINK_HASH_PATH).toString().trim() : "";

let testContext: TestContext = new TestContext()
testContext.holochain = false

describe("Multi-node integration tests [server-link] (no Holochain)", function () {
    //@ts-ignore
    this.timeout(200000)
    const aliceAppDataPath = path.join(TEST_DIR, 'agents', 'alice-server-link')
    const bobAppDataPath = path.join(TEST_DIR, 'agents', 'bob-server-link')
    let alicePorts: number[] = []
    let bobPorts: number[] = []
    let aliceExecutorProcess: ChildProcess | null = null
    let bobExecutorProcess: ChildProcess | null = null
    let alicePerson: Person | null = null
    let bobPerson: Person | null = null

    // One link-server for every neighbourhood in this file; per-neighbourhood
    // isolation comes from the UID template param.
    let linkServer: LinkServerHandle | null = null
    let serverLinkConfig: LinkLangConfig | null = null
    const getServerLinkConfig = () => {
        if (!serverLinkConfig) throw new Error("server-link config not initialised — before() didn't run?");
        return serverLinkConfig;
    }

    before(async () => {
        if(!fs.existsSync(TEST_DIR)) {
          throw Error("Please ensure that prepare-test is run before running tests!");
        }
        if (!SERVER_LINK_HASH) {
            throw new Error(
                `[integration-server-link] ${SERVER_LINK_HASH_PATH} is missing or empty. ` +
                `Server-link-language did not publish during prepare-test — fix that before running this suite.`,
            );
        }
        fs.mkdirSync(path.join(TEST_DIR, 'agents'), { recursive: true })

        // The link server and its identity store first: the executors resolve
        // people in that store.
        linkServer = await startLinkServer();
        const seedPath = writePersonSeed(path.join(TEST_DIR, 'serverLinkSeed.json'));
        const storeEnv = linkServer.store.executorEnv();

        alicePorts = await getFreePorts(3);
        registerPorts(alicePorts);
        aliceExecutorProcess = await startLocalExecutor(aliceAppDataPath, seedPath, storeEnv, alicePorts[0], alicePorts[1], alicePorts[2]);
        testContext.alice = new Ad4mClient(baseUrl(alicePorts[0]))
        testContext.aliceCore = aliceExecutorProcess
        await testContext.alice.agent.generate("passphrase")

        bobPorts = await getFreePorts(3);
        registerPorts(bobPorts);
        bobExecutorProcess = await startLocalExecutor(bobAppDataPath, seedPath, storeEnv, bobPorts[0], bobPorts[1], bobPorts[2]);
        testContext.bob = new Ad4mClient(baseUrl(bobPorts[0]))
        testContext.bobCore = bobExecutorProcess
        await testContext.bob.agent.generate("passphrase")

        // Protocol 2: a room is made by its creator, a person, and admits only
        // members, people too. Alice makes the spaces and adds Bob's person as a
        // member (serverLinkLang's `members`); Bob joins them.
        alicePerson = await givePerson(alicePorts[0]);
        bobPerson = await givePerson(bobPorts[0]);
        const aliceIdentity = (await testContext.alice.agent.me()).identity;
        if (aliceIdentity !== alicePerson.did) {
            throw new Error(
                `[integration-server-link] Alice's executor does not speak for her person: ` +
                `agent.me().identity is ${aliceIdentity}, the holder made ${alicePerson.did}.`,
            );
        }
        serverLinkConfig = serverLinkLang(
            SERVER_LINK_HASH,
            linkServer.url,
            () => alicePerson!.did,
            () => [alicePerson!.did, bobPerson!.did],
        );
    })

    after(async () => {
        await alicePerson?.holder.stop();
        await bobPerson?.holder.stop();
        if (bobExecutorProcess) {
            await quitExecutor(bobExecutorProcess, bobPorts[0]);
        }
        if (aliceExecutorProcess) {
            await quitExecutor(aliceExecutorProcess, alicePorts[0]);
        }
        if (linkServer) {
            await linkServer.kill();
            linkServer = null;
        }
        deregisterPorts([...alicePorts, ...bobPorts]);
    })

    describe('Neighbourhood [server-link]', neighbourhoodTests(testContext, getServerLinkConfig))
    describe('Auto-processor (two executors) [server-link]', autoProcessorNeighbourhoodTests(testContext, getServerLinkConfig))
    describe('Cross-peer SHACL shape sync [server-link]', crossPeerShapeSyncTests(testContext, getServerLinkConfig))
    describe('Flow replica state on join [server-link]', flowReplicaJoinTests(testContext, getServerLinkConfig))
})

function startLocalExecutor(
    appDataPath: string, seedPath: string, env: Record<string, string>,
    apiPort: number, hcAdminPort: number, hcAppPort: number,
) {
    return startExecutor(
        appDataPath, seedPath,
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
        true,           // sharedStores
        env,            // the identity store
    );
}
