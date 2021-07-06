import {
    ApolloClient,
    InMemoryCache,
    HttpLink
} from "@apollo/client/core";
import PerspectivismCore from '../core/PerspectivismCore'
import main from "../main";
import fs from 'fs-extra'
import path from 'path'
import fetch from 'node-fetch';
import { Ad4mClient } from "@perspect3vism/ad4m";

// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from '../shims/getOwnPropertyDescriptor'
Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

const DATA_RESOURCE_PATH = `${__dirname}/../test-temp`

fs.removeSync(path.join(DATA_RESOURCE_PATH, 'ad4m'))

jest.setTimeout(15000)
let core: PerspectivismCore = null

const apolloClient = new ApolloClient({
    link: new HttpLink({
        uri: 'http://localhost:4000/graphql',
        fetch: fetch,
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
let ad4mClient: Ad4mClient | undefined = undefined;

describe("Perspective-CRUD-tests", () => {

    beforeAll(async () => {
        let init = await main.init({
            appDataPath: DATA_RESOURCE_PATH,
            resourcePath: DATA_RESOURCE_PATH,
            appDefaultLangPath: DATA_RESOURCE_PATH,
            ad4mBootstrapLanguages: {
              agents: "profiles",
              languages: "languages",
              neighbourhoods: "neighbourhoods",
            },
            ad4mBootstrapFixtures: {
              languages: [],
              perspectives: [],
            },
            appBuiltInLangs: [],
            appLangAliases: null,
            mocks: false,
        })
        core = init;

        init.waitForAgent().then(async () => {
            core.initControllers()
            await core.initLanguages(true)
        })

        ad4mClient = new Ad4mClient(apolloClient)
    })

    afterAll(async () => {
        await core.exit()
        await new Promise((resolve)=>setTimeout(resolve, 1000))
    })

    describe('basic agent operations', () => {
        it('can get and create agent store', async () => {
            const did = "did:test:test"
            const didDocument = "did document test"
            const keystore = "test"
            const passphrase = "secret"

            const agentStatus = await ad4mClient.agent.initialize({
                did, didDocument, keystore, passphrase
            })
            console.log(agentStatus);

            expect(agentStatus.did).toBe(did)
            expect(agentStatus.didDocument).toBe(didDocument)
            expect(agentStatus.isInitialized).toBe(true)
            expect(agentStatus.isUnlocked).toBe(false)
        })
    })
})