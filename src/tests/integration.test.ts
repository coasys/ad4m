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
              languages: [{
                address: 'QmYGSJUQib1H6rHumdRVDhoHgkHm3U8KzSEQFfTPgziAYe',
                meta:  {"author":"did:key:zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n","timestamp":"2021-10-06T17:58:05.141Z","data":{"name":"Direct Message Language","address":"QmYGSJUQib1H6rHumdRVDhoHgkHm3U8KzSEQFfTPgziAYe","description":"Template source for personal, per-agent DM languages. Holochain based.","possibleTemplateParams":["recipient_did","recipient_hc_agent_pubkey"],"sourceCodeLink":"https://github.com/perspect3vism/direct-message-language"},"proof":{"signature":"8a495d3a1f59479109d78800362870158fdc4d8833299bd46d5c4aca3616754d3b3e8dbe74a7b2601b8b81862ffbcbeb6f73f31be255e57e3054026318ac9e38","key":"#zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n","valid":true}},
                bundle: fs.readFileSync(path.join(TEST_DIR, 'languages', 'direct-message-language', 'build', 'bundle.js')).toString()
              }],
              perspectives: [],
            },
            appBuiltInLangs: ['note-ipfs', 'direct-message-language'],
            appLangAliases: null,
            mocks: false,
            ipfsRepoPath,
            hcUseBootstrap: false,
            hcUseProxy: false,
            hcUseLocalProxy: false,
            hcUseMdns: true
        })

        core.initControllers()
        await core.initLanguages()

        testContext.alice = new Ad4mClient(apolloClient(4000))
        testContext.aliceCore = core
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
                  languages: [{
                    address: 'QmYGSJUQib1H6rHumdRVDhoHgkHm3U8KzSEQFfTPgziAYe',
                meta:  {"author":"did:key:zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n","timestamp":"2021-10-06T17:58:05.141Z","data":{"name":"Direct Message Language","address":"QmYGSJUQib1H6rHumdRVDhoHgkHm3U8KzSEQFfTPgziAYe","description":"Template source for personal, per-agent DM languages. Holochain based.","possibleTemplateParams":["recipient_did","recipient_hc_agent_pubkey"],"sourceCodeLink":"https://github.com/perspect3vism/direct-message-language"},"proof":{"signature":"8a495d3a1f59479109d78800362870158fdc4d8833299bd46d5c4aca3616754d3b3e8dbe74a7b2601b8b81862ffbcbeb6f73f31be255e57e3054026318ac9e38","key":"#zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n","valid":true}},
                    bundle: fs.readFileSync(path.join(TEST_DIR, 'languages', 'direct-message-language', 'build', 'bundle.js')).toString()
                  }],
                  perspectives: [],
                },
                appBuiltInLangs: ['note-ipfs', 'direct-message-language'],
                appLangAliases: null,
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

          bob.initControllers()
          await bob.initLanguages()

          testContext.bob = new Ad4mClient(apolloClient(14000))
          testContext.bobCore = bob
          const generate = await testContext.bob.agent.generate("passphrase")
          expect(generate.isInitialized).toBe(true);
          expect(generate.isUnlocked).toBe(true);
          await testContext.makeAllNodesKnown()
        })

        afterAll(async () => {
          await bob!.exit();
          await new Promise((resolve)=>setTimeout(resolve, 1000))
        })

        describe('Agent Language', agentLanguageTests(testContext))
        describe('Direct Messages', directMessageTests(testContext))
        describe('Language', languageTests(testContext))
        describe('Neighbourhood', neighbourhoodTests(testContext))
    })
})