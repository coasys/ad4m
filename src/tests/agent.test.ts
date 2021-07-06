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
            const generate = await ad4mClient.agent.generate("passphrase")
            expect(generate.isInitialized).toBe(true);
            expect(generate.isUnlocked).toBe(true);

            const lockAgent = await ad4mClient.agent.lock("passphrase");
            expect(lockAgent.isInitialized).toBe(true);
            expect(lockAgent.isUnlocked).toBe(false);

            const unlockAgent = await ad4mClient.agent.unlock("passphrase");
            expect(unlockAgent.isInitialized).toBe(true);
            expect(unlockAgent.isUnlocked).toBe(true);

            const agentDump = await ad4mClient.agent.status();
            expect(agentDump.isInitialized).toBe(true);
            expect(agentDump.isUnlocked).toBe(true);
        })
    })
})