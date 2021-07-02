import {
    ApolloClient,
    InMemoryCache,
    gql,
    HttpLink
} from "@apollo/client/core";
import PerspectivismCore from '../core/PerspectivismCore'
import main from "../main";
import fs from 'fs-extra'
import path from 'path'
import fetch from 'node-fetch';

// Patch Reflect to have missing getOwnPropertyDescriptor()
// which should be there in any ES6 runtime but for some reason
// is missing on some machines...
import getOwnPropertyDescriptor from '../shims/getOwnPropertyDescriptor'
Reflect.getOwnPropertyDescriptor = getOwnPropertyDescriptor

const DATA_RESOURCE_PATH = `${__dirname}/../test-temp`

export const ADD_PERSPECTIVE = gql`
mutation perspectiveAdd($name: String) {
    perspectiveAdd(input: { name: $name }) {
        name: String
        sharedURL: String
        uuid: String
    }
}
`;

fs.removeSync(path.join(DATA_RESOURCE_PATH, 'ad4m'))

jest.setTimeout(15000)
let core: PerspectivismCore = null

const apolloClient = new ApolloClient({
    link: new HttpLink({
        uri: 'http://localhost:4000/graphql',
        fetch: fetch,
    }),
    cache: new InMemoryCache(),
});

describe("Perspective-CRUD-tests", () => {

    beforeAll(async () => {
        let init = await main.init({
            appDataPath: DATA_RESOURCE_PATH,
            resourcePath: DATA_RESOURCE_PATH,
            appDefaultLangPath: DATA_RESOURCE_PATH,
            ad4mBootstrapLanguages: {
              agents: "profiles",
              languages: "languages",
              perspectives: "shared-perspectives",
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

        await core.agentService.createNewKeys()
        core.agentService.save('')
        core.initControllers()
        await core.initLanguages(true)
    })

    afterAll(async () => {
        await core.exit()
        await new Promise((resolve)=>setTimeout(resolve, 1000))
    })

    describe('create, update, get, delete perspective', () => {
        it('can create perspective', async () => {
            let create = await apolloClient.mutate({ mutation: ADD_PERSPECTIVE, variables: { name: "test" } });

            console.log(create);
        })
    })
})