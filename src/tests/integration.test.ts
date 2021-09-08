import {
    ApolloClient,
    InMemoryCache,
} from "@apollo/client/core";
import { WebSocketLink } from '@apollo/client/link/ws';
import PerspectivismCore from '../core/PerspectivismCore'
import main from "../main";
import fs from 'fs-extra'
import path from 'path'
import ws from "ws"
import { isProcessRunning } from "./utils";
import { Ad4mClient } from "@perspect3vism/ad4m";

// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from '../shims/getOwnPropertyDescriptor'
import perspectiveTests from "./perspective";
import agentTests from "./agent";
import languageTests from "./language";
import expressionTests from "./expression";
import neighbourhoodTests from "./neighbourhood";
import runtimeTests from "./runtime";
import { Crypto } from "@peculiar/webcrypto"
//@ts-ignore
global.crypto = new Crypto();
Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

const TEST_DIR = `${__dirname}/../test-temp`

jest.setTimeout(205000)
let core: PerspectivismCore | null = null

function apolloClient(port: number): ApolloClient<any> {
  return new ApolloClient({
    link: new WebSocketLink({
        uri: `http://localhost:${port}/graphql`,
        options: { reconnect: true },
        webSocketImpl: ws,
    }),
    cache: new InMemoryCache({resultCaching: false, addTypename: false}),
    defaultOptions: {
        watchQuery: {
            fetchPolicy: "no-cache",
        },
        query: {
            fetchPolicy: "no-cache",
        }
    },
});
}


export class TestContext {
    //#ad4mClient: Ad4mClient | undefined
    #alice: Ad4mClient | undefined
    #bob: Ad4mClient | undefined

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
}
let testContext: TestContext = new TestContext()

describe("Integration tests", () => {
    const appDataPath = path.join(TEST_DIR, 'agents', 'alice')
    const ipfsRepoPath = path.join(appDataPath, 'agents', 'alice', '.jsipfs')

    beforeAll(async () => {    
        if(!fs.existsSync(TEST_DIR)) {
          throw Error("Please ensure that prepare-test is run before running tests!");
        }
        if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
          fs.mkdirSync(path.join(TEST_DIR, 'agents'))
        if(!fs.existsSync(appDataPath))
            fs.mkdirSync(appDataPath)
        core = await main.init({
            appDataPath,
            resourcePath: TEST_DIR,
            appDefaultLangPath: path.join(TEST_DIR, 'languages'),
            ad4mBootstrapLanguages: {
              agents: "agent-expression-store",
              languages: "languages",
              neighbourhoods: "neighbourhood-store",
            },
            ad4mBootstrapFixtures: {
              languages: [],
              perspectives: [],
            },
            appBuiltInLangs: ['note-ipfs'],
            appLangAliases: null,
            mocks: false,
            ipfsRepoPath,
            useLocalHolochainProxy: false,
            useMdnsHolochain: false
        })

        core.initControllers()
        await core.initLanguages()

        testContext.alice = new Ad4mClient(apolloClient(4000))
    })

    afterAll(async () => {
        expect(await isProcessRunning("holochain")).toBe(true);
        expect(await isProcessRunning("lair-keystore")).toBe(true);
        expect(fs.existsSync(path.join(ipfsRepoPath, "repo.lock"))).toBe(true);

        await core!.exit();
        await new Promise((resolve)=>setTimeout(resolve, 1000))
        
        expect(await isProcessRunning("holochain")).toBe(false);
        expect(await isProcessRunning("lair-keystore")).toBe(false);
        expect(fs.existsSync(path.join(ipfsRepoPath, "repo.lock"))).toBe(false);
    })

    describe('Agent / Agent-Setup', agentTests(testContext))
    describe('Runtime', runtimeTests(testContext))
    describe('Expression', expressionTests(testContext))
    describe('Perspective', perspectiveTests(testContext))

    describe('with Alice and Bob', () => {
        let bob: PerspectivismCore | null = null
        beforeAll(async () => {
            const appDataPath = path.join(TEST_DIR, 'agents', 'bob')
            const ipfsRepoPath = path.join(appDataPath, '.jsipfs')
            if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
              fs.mkdirSync(path.join(TEST_DIR, 'agents'))
            if(!fs.existsSync(appDataPath))
              fs.mkdirSync(appDataPath)

            bob = await main.init({
                appDataPath,
                resourcePath: TEST_DIR,
                appDefaultLangPath: path.join(TEST_DIR, 'languages'),
                ad4mBootstrapLanguages: {
                  agents: "agent-expression-store",
                  languages: "languages",
                  neighbourhoods: "neighbourhood-store",
                },
                ad4mBootstrapFixtures: {
                  languages: [],
                  perspectives: [],
                },
                appBuiltInLangs: ['note-ipfs'],
                appLangAliases: null,
                mocks: false,
                portGraphQL: 14000,
                portHCAdmin: 12000,
                portHCApp: 11337,
                ipfsSwarmPort: 14002,
                ipfsRepoPath,
                useLocalHolochainProxy: false,
                useMdnsHolochain: false
          })

          bob.initControllers()
          await bob.initLanguages()

          const aliceAgentInfo = await core!.holochainRequestAgentInfos()
          const bobAgentInfo = await bob.holochainRequestAgentInfos()
          await core?.holochainAddAgentInfos(bobAgentInfo)
          bob.holochainAddAgentInfos(aliceAgentInfo)

          testContext.bob = new Ad4mClient(apolloClient(14000))
          const generate = await testContext.bob.agent.generate("passphrase")
          expect(generate.isInitialized).toBe(true);
          expect(generate.isUnlocked).toBe(true);
        })

        afterAll(async () => {
          await bob!.exit();
          await new Promise((resolve)=>setTimeout(resolve, 1000))
        })

        describe('Language', languageTests(testContext))
        describe('Neighbourhood', neighbourhoodTests(testContext))
    })
})