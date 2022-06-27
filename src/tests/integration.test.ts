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
import directMessageTests from "./direct-messages";
import agentLanguageTests from "./agent-language";
import socialDNATests from "./social-dna-flow";
//@ts-ignore
global.crypto = new Crypto();
Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

const TEST_DIR = `${__dirname}/../test-temp`

jest.setTimeout(200000)
let core: PerspectivismCore | null = null

function apolloClient(port: number): ApolloClient<any> {
  return new ApolloClient({
    link: new WebSocketLink({
        uri: `ws://localhost:${port}/graphql`,
        options: {
          reconnect: true,
          connectionParams: () => {
            return {
              headers: {
              }
            }
          }
        },
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

    #aliceCore: PerspectivismCore | undefined
    #bobCore: PerspectivismCore | undefined

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

    set aliceCore(aliceCore: PerspectivismCore) {
      this.#aliceCore = aliceCore
    }

    set bobCore(bobCore: PerspectivismCore) {
      this.#bobCore = bobCore
    }

    async makeAllNodesKnown() {
      const aliceAgentInfo = await this.#aliceCore!.holochainRequestAgentInfos()
      const bobAgentInfo = await this.#bobCore!.holochainRequestAgentInfos()
      await this.#aliceCore!.holochainAddAgentInfos(bobAgentInfo)
      await this.#bobCore!.holochainAddAgentInfos(aliceAgentInfo)
    }
}
let testContext: TestContext = new TestContext()

describe("Integration tests", () => {
    const appDataPath = path.join(TEST_DIR, 'agents', 'alice')
    const ipfsRepoPath = path.join(appDataPath)

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
            networkBootstrapSeed: "./src/tests/bootstrapSeed.json",
            bootstrapFixtures: {
              languages: [],
              perspectives: [],
            },
            mocks: false,
            ipfsRepoPath,
            hcUseBootstrap: false,
            hcUseProxy: false,
            hcUseLocalProxy: false,
            hcUseMdns: true
        })

        testContext.alice = new Ad4mClient(apolloClient(4000))
        testContext.aliceCore = core
    })

    afterAll(async () => {
      expect(await isProcessRunning("holochain")).toBe(true);
      expect(await isProcessRunning("lair-keystore")).toBe(true);
      expect(fs.existsSync(path.join(ipfsRepoPath, "repo.lock"))).toBe(true);

      await core!.exit();
      await new Promise((resolve)=>setTimeout(resolve, 500))

      expect(await isProcessRunning("holochain")).toBe(false);
      expect(await isProcessRunning("lair-keystore")).toBe(false);
      expect(fs.existsSync(path.join(ipfsRepoPath, "repo.lock"))).toBe(false);
    })

    describe('Agent / Agent-Setup', agentTests(testContext))
    describe('Runtime', runtimeTests(testContext))
    describe('Expression', expressionTests(testContext))
    describe('Perspective', perspectiveTests(testContext))
    describe('Social DNA', socialDNATests(testContext))

    describe('with Alice and Bob', () => {
        let bob: PerspectivismCore | null = null
        beforeAll(async () => {
            const appDataPath = path.join(TEST_DIR, 'agents', 'bob')
            const ipfsRepoPath = path.join(appDataPath)
            if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
              fs.mkdirSync(path.join(TEST_DIR, 'agents'))
            if(!fs.existsSync(appDataPath))
              fs.mkdirSync(appDataPath)

            bob = await main.init({
                appDataPath,
                resourcePath: TEST_DIR,
                networkBootstrapSeed: "./src/tests/bootstrapSeed.json",
                bootstrapFixtures: {
                  languages: [],
                  perspectives: [],
                },
                mocks: false,
                gqlPort: 14000,
                hcPortAdmin: 12000,
                hcPortApp: 11337,
                ipfsSwarmPort: 14002,
                ipfsRepoPath,
                hcUseBootstrap: false,
                hcUseProxy: false,
                hcUseLocalProxy: false,
                hcUseMdns: true
          })

          testContext.bob = new Ad4mClient(apolloClient(14000))
          testContext.bobCore = bob
          await testContext.bob.agent.generate("passphrase")

          const status = await testContext.bob.agent.status()

          expect(status.isInitialized).toBe(true);
          expect(status.isUnlocked).toBe(true);
          await testContext.makeAllNodesKnown()
        })

        afterAll(async () => {
          await bob!.exit();
          await new Promise((resolve)=>setTimeout(resolve, 500))
        })

        describe('Agent Language', agentLanguageTests(testContext))
        describe('Direct Messages', directMessageTests(testContext))
        describe('Language', languageTests(testContext))
        describe('Neighbourhood', neighbourhoodTests(testContext))
    })
})