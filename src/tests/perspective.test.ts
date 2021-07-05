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
import { Ad4mClient, Link, LinkQuery } from "@perspect3vism/ad4m";

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
    cache: new InMemoryCache({resultCaching: false}),
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

        await core.agentService.createNewKeys()
        core.agentService.save('')
        core.initControllers()
        await core.initLanguages(true)

        ad4mClient = new Ad4mClient(apolloClient)
    })

    afterAll(async () => {
        await core.exit()
        await new Promise((resolve)=>setTimeout(resolve, 1000))
    })

    describe('create, update, get, delete perspective', () => {
        // it('can create perspective', async () => {
        //     const create = await ad4mClient!.perspective.add("test");
        //     expect(create.name).toEqual("test");

        //     const get = await ad4mClient!.perspective.byUUID(create.uuid);
        //     expect(get.name).toEqual("test");

        //     const update = await ad4mClient!.perspective.update(create.uuid, "updated-test");
        //     expect(update.name).toEqual("updated-test");

        //     const getUpdated = await ad4mClient!.perspective.byUUID(update.uuid);
        //     expect(getUpdated.name).toEqual("updated-test");

        //     const deletePerspective = await ad4mClient!.perspective.remove(update.uuid);
        //     console.log(deletePerspective);

        //     const getDeleted = await ad4mClient!.perspective.byUUID(update.uuid);
        //     expect(getDeleted).toEqual(null);
        // })

        it('test local perspective links', async () => {
            const create = await ad4mClient!.perspective.add("test-links");
            expect(create.name).toEqual("test-links");

            let addLinks = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target", predicate: "lang://predicate"}));
            expect(addLinks.data.target).toEqual("lang://test-target");
            expect(addLinks.data.source).toEqual("lang://test");

            let queryLinks = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test"}));
            expect(queryLinks.length).toEqual(1);
            expect(queryLinks[0].data.target).toEqual("lang://test-target");
            expect(queryLinks[0].data.source).toEqual("lang://test");

            let queryLinksTarget = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({target: "lang://test-target"}));
            expect(queryLinksTarget.length).toEqual(1);
            expect(queryLinksTarget[0].data.target).toEqual("lang://test-target");
            expect(queryLinksTarget[0].data.source).toEqual("lang://test");

            let queryLinksPredicate = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({predicate: "lang://predicate"}));
            expect(queryLinksPredicate.length).toEqual(1);
            expect(queryLinksPredicate[0].data.target).toEqual("lang://test-target");
            expect(queryLinksPredicate[0].data.source).toEqual("lang://test");
        })
    })
})