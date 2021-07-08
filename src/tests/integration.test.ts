import {
    ApolloClient,
    InMemoryCache,
    HttpLink
} from "@apollo/client/core";
import { WebSocketLink } from '@apollo/client/link/ws';
import PerspectivismCore from '../core/PerspectivismCore'
import main from "../main";
import fs from 'fs-extra'
import path from 'path'
import ws from "ws"
import os from "os";
import { Ad4mClient } from "@perspect3vism/ad4m";

// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from '../shims/getOwnPropertyDescriptor'
import perspectiveTests from "./perspective";
import agentTests from "./agent";
import languageTests from "./language";
import expressionTests from "./expression";
Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

const DATA_RESOURCE_PATH = `${__dirname}/../test-temp`

fs.removeSync(path.join(DATA_RESOURCE_PATH, 'ad4m'))

jest.setTimeout(15000)
let core: PerspectivismCore = null

const apolloClient = new ApolloClient({
    link: new WebSocketLink({
        uri: 'http://localhost:4000/graphql',
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

export class TestContext {
    ad4mClient: Ad4mClient | undefined
}
let testContext: TestContext = new TestContext()

export async function isProcessRunning(processName: string): Promise<boolean> {
    const cmd = (() => {
      switch (process.platform) {
        case 'win32': return `tasklist`
        case 'darwin': return `ps -ax | grep ${processName}`
        case 'linux': return `ps -A`
        default: return false
      }
    })()
  
    return new Promise((resolve, reject) => {
      require('child_process').exec(cmd, (err: Error, stdout: string, stderr: string) => {
        if (err) reject(err)
  
        resolve(stdout.toLowerCase().indexOf(processName.toLowerCase()) > -1)
      })
    })
  }

describe("Integration tests", () => {

    beforeAll(async () => {
        core = await main.init({
            appDataPath: DATA_RESOURCE_PATH,
            resourcePath: DATA_RESOURCE_PATH,
            appDefaultLangPath: DATA_RESOURCE_PATH,
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
        })

        core.initControllers()
        await core.initLanguages(false)

        testContext.ad4mClient = new Ad4mClient(apolloClient)
    })

    afterAll(async () => {
        expect(await isProcessRunning("holochain")).toBe(true);
        expect(await isProcessRunning("lair-keystore")).toBe(true);
        expect(fs.existsSync(path.join(os.homedir(), ".jsipfs/repo.lock"))).toBe(true);

        await core.exit();
        await new Promise((resolve)=>setTimeout(resolve, 1000))
        
        expect(await isProcessRunning("holochain")).toBe(false);
        expect(await isProcessRunning("lair-keystore")).toBe(false);
        expect(fs.existsSync(path.join(os.homedir(), ".jsipfs/repo.lock"))).toBe(false);

        //Delete all languages created during test
        fs.readdir(path.join(DATA_RESOURCE_PATH, "ad4m/languages"), (err, files) => {
            if (err) throw err;
          
            for (const file of files) {
              fs.rmdir(path.join(path.join(DATA_RESOURCE_PATH, "ad4m/languages"), file), {recursive: true});
            }
          });
    })

    describe('Agent / Agent-Setup', agentTests(testContext))
    describe('Expression', expressionTests(testContext))
    describe('Language', languageTests(testContext))
    describe('Perspective', perspectiveTests(testContext))
})