import { buildSchema } from "type-graphql"

import { createServer } from 'http';
import { ApolloServer } from '@apollo/server';
import { expressMiddleware } from '@apollo/server/express4';
import { ApolloServerPluginDrainHttpServer } from '@apollo/server/plugin/drainHttpServer'

import { WebSocketServer } from 'ws';
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { useServer } from 'graphql-ws/lib/use/ws';

import { ApolloClient, InMemoryCache } from "@apollo/client/core";

import { createClient } from 'graphql-ws';
import Websocket from "ws";
import express from 'express';

import AgentResolver from "./agent/AgentResolver"
import { Ad4mClient } from "./Ad4mClient";
import { Perspective, PerspectiveUnsignedInput } from "./perspectives/Perspective";
import { Link, LinkExpression, LinkExpressionInput, LinkInput, LinkMutations } from "./links/Links";
import LanguageResolver from "./language/LanguageResolver";
import NeighbourhoodResolver from "./neighbourhood/NeighbourhoodResolver";
import PerspectiveResolver from "./perspectives/PerspectiveResolver";
import RuntimeResolver from "./runtime/RuntimeResolver";
import ExpressionResolver from "./expression/ExpressionResolver";
import AIResolver from './ai/AIResolver'
import { AuthInfoInput, EntanglementProofInput, CapabilityInput, ResourceInput } from "./agent/Agent";
import { LanguageMetaInput } from "./language/LanguageMeta";
import { InteractionCall } from "./language/Language";
import { PerspectiveState } from "./perspectives/PerspectiveHandle";

jest.setTimeout(15000)

async function createGqlServer(port: number) {
    const schema = await buildSchema({
        resolvers: [
            AgentResolver,
            ExpressionResolver,
            LanguageResolver,
            NeighbourhoodResolver,
            PerspectiveResolver,
            RuntimeResolver,
            AIResolver
        ]
    })

    const app = express();
    const httpServer = createServer(app);

    let serverCleanup: any;
    const server = new ApolloServer({
        schema,
        plugins: [
            ApolloServerPluginDrainHttpServer({ httpServer }),
            {
                async serverWillStart() {
                    return {
                        async drainServer() {
                            await serverCleanup.dispose();
                        },
                    };
                },
            },
        ],
    });


    // Creating the WebSocket server
    const wsServer = new WebSocketServer({
        // This is the `httpServer` we created in a previous step.
        server: httpServer,
        // Pass a different path here if your ApolloServer serves at
        // a different path.
        path: '/subscriptions',
    });

    // Hand in the schema we just created and have the
    // WebSocketServer start listening.
    serverCleanup = useServer({ schema }, wsServer);

    await server.start()
    app.use('/graphql', expressMiddleware(server));
    // server.applyMiddleware({ app });
    httpServer.listen({ port })
    return port
}

describe('Ad4mClient', () => {
    let ad4mClient
    let apolloClient

    beforeAll(async () => {
        let port = await createGqlServer(4000);

        console.log(`GraphQL server listening at: http://localhost:${port}/graphql`)

        const wsLink = new GraphQLWsLink(createClient({
            url: `ws://localhost:${port}/subscriptions`,
            webSocketImpl: Websocket
        }));



        apolloClient = new ApolloClient({
            link: wsLink,
            cache: new InMemoryCache(),
            defaultOptions: {
                watchQuery: {
                    fetchPolicy: 'network-only',
                    nextFetchPolicy: 'network-only'
                },
            }
        });

        console.log("GraphQL client connected")

        ad4mClient = new Ad4mClient(apolloClient)

        console.log("GraphQL client connected")
    })

    describe('.agent', () => {
        it('me() smoke test', async () => {
            const agent = await ad4mClient.agent.me()
            expect(agent.did).toBe('did:ad4m:test')
        })

        it('status() smoke test', async () => {
            const agentStatus = await ad4mClient.agent.status()
            expect(agentStatus.did).toBe('did:ad4m:test')
            expect(agentStatus.isUnlocked).toBe(false)
        })

        it('import() smoke test', async () => {
            const did = "did:test:test"
            const didDocument = "did document test"
            const keystore = "test"
            const passphrase = "secret"

            const agentStatus = await ad4mClient.agent.import({
                did, didDocument, keystore, passphrase
            })

            expect(agentStatus.did).toBe(did)
            expect(agentStatus.didDocument).toBe(didDocument)
            expect(agentStatus.isInitialized).toBe(true)
            expect(agentStatus.isUnlocked).toBe(true)
        })

        it('generate() smoke test', async () => {
            const agentStatus = await ad4mClient.agent.generate("passphrase")
            expect(agentStatus.did).toBeDefined()
            expect(agentStatus.isInitialized).toBeTruthy()
        })

        it('lock() smoke test', async () => {
            const agentStatus = await ad4mClient.agent.lock('secret')
            expect(agentStatus.did).toBe("did:ad4m:test")
            expect(agentStatus.isUnlocked).toBe(false)
        })

        it('unlock() smoke test', async () => {
            const agentStatus = await ad4mClient.agent.unlock('secret', false)
            expect(agentStatus.did).toBe("did:ad4m:test")
            expect(agentStatus.isUnlocked).toBe(true)
        })

        it('byDID() smoke test', async () => {
            const agent = await ad4mClient.agent.byDID('did:method:12345')
            expect(agent.did).toBe('did:method:12345')
        })

        it('updatePublicPerspective() smoke test', async () => {
            const perspective = new Perspective()
            const link = new LinkExpression()
            link.author = 'did:method:12345'
            link.timestamp = new Date().toString()
            link.data = new Link({source: 'root', target: 'perspective://Qm34589a3ccc0'})
            link.proof = { signature: 'asdfasdf', key: 'asdfasdf' }
            perspective.links.push(link)

            const agent = await ad4mClient.agent.updatePublicPerspective(perspective)
            expect(agent.did).toBe('did:ad4m:test')
            expect(agent.perspective.links.length).toBe(1)
            expect(agent.perspective.links[0].data.source).toBe('root')
            expect(agent.perspective.links[0].data.target).toBe('perspective://Qm34589a3ccc0')
        })

        it('mutatePublicPerspective() smoke test', async () => {
            let additionLink = new Link({source: 'root', target: 'perspective://Qm34589a3ccc0'})
            const removalLink = new LinkExpression()
            removalLink.author = "did:ad4m:test"
            removalLink.timestamp = Date.now().toString()
            removalLink.data = {
                source: 'root2',
                target: 'perspective://Qm34589a3ccc0'
            }
            removalLink.proof = {
                signature: '',
                key: '',
                valid: true
            }


            //Note; here we dont get the links above since mutatePublicPerspective relies on a snapshot which returns the default test link for perspectives
            const agent = await ad4mClient.agent.mutatePublicPerspective({additions: [additionLink], removals: [removalLink]} as LinkMutations)
            expect(agent.did).toBe('did:ad4m:test')
            expect(agent.perspective.links.length).toBe(1)
            expect(agent.perspective.links[0].data.source).toBe('root')
            expect(agent.perspective.links[0].data.target).toBe('neighbourhood://Qm12345')
        })

        it('updateDirectMessageLanguage() smoke test', async () => {
            const agent = await ad4mClient.agent.updateDirectMessageLanguage("abcd")
            expect(agent.directMessageLanguage).toBe('abcd')
        })

        it('entanglementProof() smoke tests', async () => {
            const addProof = await ad4mClient.agent.addEntanglementProofs([new EntanglementProofInput("did:key:hash", "did-key-id", "ethereum", "ethAddr", "sig", "sig2")]);
            expect(addProof[0].did).toBe("did:key:hash")
            expect(addProof[0].didSigningKeyId).toBe("did-key-id")
            expect(addProof[0].deviceKeyType).toBe("ethereum")
            expect(addProof[0].deviceKey).toBe("ethAddr")
            expect(addProof[0].deviceKeySignedByDid).toBe("sig")
            expect(addProof[0].didSignedByDeviceKey).toBe("sig2")

            const getProofs = await ad4mClient.agent.getEntanglementProofs();
            expect(getProofs[0].did).toBe("did:key:hash")
            expect(getProofs[0].didSigningKeyId).toBe("did-key-id")
            expect(getProofs[0].deviceKeyType).toBe("ethereum")
            expect(getProofs[0].deviceKey).toBe("ethAddr")
            expect(getProofs[0].deviceKeySignedByDid).toBe("sig")
            expect(getProofs[0].didSignedByDeviceKey).toBe("sig2")

            const deleteProofs = await ad4mClient.agent.deleteEntanglementProofs([new EntanglementProofInput("did:key:hash", "did-key-id", "ethereum", "ethAddr", "sig", "sig2")]);
            expect(deleteProofs[0].did).toBe("did:key:hash")
            expect(deleteProofs[0].didSigningKeyId).toBe("did-key-id")
            expect(deleteProofs[0].deviceKeyType).toBe("ethereum")
            expect(deleteProofs[0].deviceKey).toBe("ethAddr")
            expect(deleteProofs[0].deviceKeySignedByDid).toBe("sig")
            expect(deleteProofs[0].didSignedByDeviceKey).toBe("sig2")

            const preflight = await ad4mClient.agent.entanglementProofPreFlight("ethAddr", "ethereum");
            expect(preflight.did).toBe("did:key:hash")
            expect(preflight.didSigningKeyId).toBe("did-key-id")
            expect(preflight.deviceKeyType).toBe("ethereum")
            expect(preflight.deviceKey).toBe("ethAddr")
            expect(preflight.deviceKeySignedByDid).toBe("sig")
            expect(preflight.didSignedByDeviceKey).toBe(null)
        })

        it('requestCapability() smoke tests', async () => {
            const requestId = await ad4mClient.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "demo.test.org",
                appUrl: "https://demo-link",
                appIconPath: "/some/image/path",
                capabilities: [
                    {
                        with: {
                            "domain":"agent",
                            "pointers":["*"]
                        } as ResourceInput,
                        can: ["QUERY"]
                }] as CapabilityInput[]
            } as AuthInfoInput)
            expect(requestId).toBe("test-request-id")
        })


        it('agentGetApps() smoke tests', async () => {
            const apps = await ad4mClient.agent.getApps()
            expect(apps.length).toBe(0)
        })

        it('agentPermitCapability() smoke tests', async () => {
            const rand = await ad4mClient.agent.permitCapability('{"requestId":"4f30e2e2-d307-4f2b-b0a0-6dac4ca4af26","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]}}')
            expect(rand).toBe("123")
        })

        it('agentGenerateJwt() smoke tests', async () => {
            const jwt = await ad4mClient.agent.generateJwt("test-request-id", "123")
            expect(jwt).toBe("test-jwt")
        })

        it('agentRevokeToken() smoke tests', async () => {
            const newApps = await ad4mClient.agent.revokeToken('test-request-id')
            expect(newApps.length).toBe(1)
            expect(newApps[0].revoked).toBe(true)
        })

        it('agentRemoveToken() smoke tests', async () => {
            const newApps = await ad4mClient.agent.removeApp('test-request-id')
            expect(newApps.length).toBe(0)
        })

        it('agentIsLocked() smoke tests', async () => {
            const status = await ad4mClient.agent.isLocked()
            expect(status).toBe(false)
        })

        it('agentSignMessage() smoke tests', async () => {
            const sig = await ad4mClient.agent.signMessage("test-message")
            expect(sig.signature).toBe("test-message-signature")
            expect(sig.publicKey).toBe("test-public-key")
        })
    })

    describe('.expression', () => {
        it('get() smoke test', async () => {
            const nonExisting = await ad4mClient.expression.get("wrong address")
            expect(nonExisting).toBeNull()

            const expression = await ad4mClient.expression.get("neighbourhood://Qm123")
            expect(expression).toBeDefined()
            expect(expression.author).toBe('did:ad4m:test')
            expect(expression.data).toBe("{\"type\":\"test expression\",\"content\":\"test\"}")
        })

        it('getMany() smoke test', async () => {
            const getMany = await ad4mClient.expression.getMany(["hash1", "hash2"]);
            expect(getMany.length).toBe(2);
            expect(getMany[0].author).toBe('did:ad4m:test');
            expect(getMany[0].data).toBe("{\"type\":\"test expression\",\"content\":\"test\"}");
            expect(getMany[1]).toBeNull();
        })

        it('getRaw() smoke test', async () => {
            const nonExisting = await ad4mClient.expression.getRaw("wrong address")
            expect(nonExisting).toBeNull()

            const expressionRaw = await ad4mClient.expression.getRaw("neighbourhood://Qm123")
            expect(expressionRaw).toBeDefined()
            const expression = JSON.parse(expressionRaw)
            expect(expression.author).toBe('did:ad4m:test')
            expect(expression.data).toBe("{\"type\":\"test expression\",\"content\":\"test\"}")
        })

        it('create() smoke test', async () => {
            const address = await ad4mClient.expression.create('content', 'Qmabcdf')
            expect(address.toString()).toBe("Qm1234")

            const address2 = await ad4mClient.expression.create({content: 'json'}, 'Qmabcdf')
            expect(address2.toString()).toBe("Qm1234")
        })

        it('interactions() smoke test', async () => {
            const interactions = await ad4mClient.expression.interactions('Qmabcdf')
            expect(interactions.length).toBe(1)
            const i = interactions[0]
            expect(i.label).toBe("Add a comment")
            expect(i.name).toBe("add_comment")
            expect(i.parameters.length).toBe(1)
            const param = i.parameters[0]
            expect(param.name).toBe('comment')
            expect(param.type).toBe('string')
        })

        it('interact() smoke test', async () => {
            const call = new InteractionCall('add_comment', { content: 'test'})
            const result = await ad4mClient.expression.interact('Qmabcdf', call)
            expect(result.toString()).toBe("test result")
        })
    })

    describe('.langauges', () => {
        it('byAddress() smoke test', async () => {
            const language = await ad4mClient.languages.byAddress('test-language-address')
            expect(language.address).toBe('test-language-address')
        })

        it('byFilter() smoke test', async () => {
            const languages = await ad4mClient.languages.byFilter('linksAdapter')
            expect(languages.length).toBe(1)
            expect(languages[0].name).toBe('test-links-language')
        })

        it('all() smoke test', async () => {
            const languages = await ad4mClient.languages.all()
            expect(languages.length).toBe(1)
            expect(languages[0].name).toBe('test-links-language')
        })

        it('writeSettings() smoke test', async () => {
            const result = await ad4mClient.languages.writeSettings(
                'test-language-address',
                JSON.stringify({testSetting: true})
            )
            expect(result).toBe(true)
        })

        it('applyTemplateAndPublish() smoke test', async () => {
            const language = await ad4mClient.languages.applyTemplateAndPublish(
                'languageHash',
                '{"name": "test-templating"}',
            )
            expect(language.name).toBe('languageHash-clone')
        })

        it('publish() smoke test', async () => {
            let input = new LanguageMetaInput()
            input.name = "test language 1"
            input.description = "Language for smoke testing"
            input.possibleTemplateParams = ['uuid', 'name', 'membrane']
            input.sourceCodeLink = "https://github.com/perspect3vism/test-language"

            const languageMeta = await ad4mClient.languages.publish(
                '/some/language/path/',
                input,
            )
            expect(languageMeta.name).toBe(input.name)
            expect(languageMeta.description).toBe(input.description)
            expect(languageMeta.possibleTemplateParams).toStrictEqual(input.possibleTemplateParams)
            expect(languageMeta.sourceCodeLink).toBe(input.sourceCodeLink)
            expect(languageMeta.address).toBe("Qm12345")
            expect(languageMeta.author).toBe("did:test:me")
            expect(languageMeta.templateSourceLanguageAddress).toBe("Qm12345")
            expect(languageMeta.templateAppliedParams).toBe(JSON.stringify({uuid: 'asdfsdaf', name: 'test template'}))
        })

        it('meta() smoke test', async () => {
            let input = new LanguageMetaInput()
            input.name = "test language 1"
            input.description = "Language for smoke testing"
            input.possibleTemplateParams = ['uuid', 'name', 'membrane']
            input.sourceCodeLink = "https://github.com/perspect3vism/test-language"

            const languageMeta = await ad4mClient.languages.meta("Qm12345")

            expect(languageMeta.name).toBe("test-language")
            expect(languageMeta.address).toBe("Qm12345")
            expect(languageMeta.description).toBe("Language meta for testing")
            expect(languageMeta.author).toBe("did:test:me")
            expect(languageMeta.templated).toBe(true)
            expect(languageMeta.templateSourceLanguageAddress).toBe("Qm12345")
            expect(languageMeta.templateAppliedParams).toBe(JSON.stringify({uuid: 'asdfsdaf', name: 'test template'}))
            expect(languageMeta.possibleTemplateParams).toStrictEqual(['uuid', 'name'])
            expect(languageMeta.sourceCodeLink).toBe("https://github.com/perspect3vism/ad4m")
        })

        it('source() smoke test', async () => {
            const source = await ad4mClient.languages.source("Qm12345")
            expect(source).toBe("var test = 'language source code'")
        })

        it('remove() smoke test', async () => {
            const result = await ad4mClient.languages.remove("Qm12345");
            expect(result).toBe(true);
        })
    })

    describe('.neighbourhood', () => {
        const testPerspective = new Perspective()
        const linkExpr = new LinkExpression()
        linkExpr.author = 'did:method:12345'
        linkExpr.timestamp = new Date().toString()
        linkExpr.data = new Link({source: 'root', target: 'perspective://Qm34589a3ccc0'})
        linkExpr.proof = { signature: 'asdfasdf', key: 'asdfasdf' }
        testPerspective.links.push(linkExpr)

        const testUnsignedPerspective = new PerspectiveUnsignedInput()
        const link = new Link({
            source: 'root', target: 'perspective://Qm34589a3ccc0'
        })
        testUnsignedPerspective.links.push(link)

        it('publishFromPerspective() smoke test', async () => {
            const expressionRef = await ad4mClient.neighbourhood.publishFromPerspective('UUID', 'test-link-lang', new Perspective())
            expect(expressionRef).toBe('neighbourhood://neighbourhoodAddress')
        })

        it('joinFromUrl() smoke test', async () => {
            const perspective = await ad4mClient.neighbourhood.joinFromUrl('neighbourhood://Qm3sdf3dfwhsafd')
            expect(perspective.sharedUrl).toBe('neighbourhood://Qm3sdf3dfwhsafd')
            expect(perspective.uuid).toBeTruthy()
            expect(perspective.name).toBeTruthy()
        })

        it('hasTelepresenceAdapter() smoke test', async () => {
            const result = await ad4mClient.neighbourhood.hasTelepresenceAdapter('01234')
            expect(result).toBe(true)
        })

        it('otherAgents() smoke test', async () => {
            const agents = await ad4mClient.neighbourhood.otherAgents('01234')
            expect(agents.length).toBe(1)
            expect(agents[0]).toBe('did:test:other')
        })

        it('onlineAgents() smoke test', async () => {
            const agents = await ad4mClient.neighbourhood.onlineAgents('01234')
            expect(agents.length).toBe(1)
            expect(agents[0].did).toBe('did:test:online')
            const status = agents[0].status
            expect(status.author).toBe('did:ad4m:test')
            expect(status.data.links.length).toBe(1)
            const link = status.data.links[0]
            expect(link.author).toBe('did:ad4m:test')
            expect(link.data.source).toBe('root')
        })

        it('setOnlineStatus() smoke test', async () => {
            const result = await ad4mClient.neighbourhood.setOnlineStatus('01234', testPerspective)
            expect(result).toBe(true)
        })

        it('setOnlineStatusU() smoke test', async () => {
            const result = await ad4mClient.neighbourhood.setOnlineStatusU('01234', testUnsignedPerspective)
            expect(result).toBe(true)
        })

        it('sendSignal() smoke test', async () => {
            const result = await ad4mClient.neighbourhood.sendSignal('01234', "did:test:recipient", testPerspective)
            expect(result).toBe(true)
        })

        it('sendSignaU() smoke test', async () => {
            const result = await ad4mClient.neighbourhood.sendSignalU('01234', "did:test:recipient", testUnsignedPerspective)
            expect(result).toBe(true)
        })

        it('sendBroadcast() smoke test', async () => {
            const result = await ad4mClient.neighbourhood.sendBroadcast('01234', testPerspective)
            expect(result).toBe(true)
        })

        it('sendBroadcastU() smoke test', async () => {
            const result = await ad4mClient.neighbourhood.sendBroadcastU('01234', testUnsignedPerspective)
            expect(result).toBe(true)
        })

        it('can be accessed via NeighbourhoodProxy', async () => {
            const perspective = await ad4mClient.perspective.byUUID('00001')
            const nh = await perspective.getNeighbourhoodProxy()

            expect(await nh.hasTelepresenceAdapter()).toBe(true)
            expect(await nh.otherAgents()).toStrictEqual(['did:test:other'])
            expect((await nh.onlineAgents())[0].did).toStrictEqual('did:test:online')
            expect(await nh.setOnlineStatus(testPerspective)).toBe(true)
            expect(await nh.sendSignal('did:test:recipient', testPerspective)).toBe(true)
            expect(await nh.sendBroadcast(testPerspective)).toBe(true)

            nh.addSignalHandler((perspective) => {
                //..
            })
        })
    })

    describe('.perspective', () => {
        it('all() smoke test',async () => {
            const perspectives = await ad4mClient.perspective.all()
            expect(perspectives.length).toBe(2)
            const p1 = perspectives[0]
            const p2 = perspectives[1]
            expect(p1.name).toBe('test-perspective-1')
            expect(p2.name).toBe('test-perspective-2')
            expect(p1.uuid).toBe('00001')
            expect(p2.uuid).toBe('00002')
            expect(p2.sharedUrl).toBe('neighbourhood://Qm12345')
            expect(p2.neighbourhood.data.linkLanguage).toBe("language://Qm12345")
        })

        it('byUUID() smoke test', async () => {
            const p = await ad4mClient.perspective.byUUID('00004')
            expect(p.uuid).toBe('00004')
            expect(p.name).toBe('test-perspective-1')
        })

        it('snapshotByUUID() smoke test', async () => {
            const ps = await ad4mClient.perspective.snapshotByUUID('00004')
            expect(ps.links.length).toBe(1)
            expect(ps.links[0].author).toBe('did:ad4m:test')
            expect(ps.links[0].data.source).toBe('root')
            expect(ps.links[0].data.target).toBe('neighbourhood://Qm12345')
        })

        it('publishSnapshotByUUID() smoke test', async () => {
            const snapshotUrl = await ad4mClient.perspective.publishSnapshotByUUID('00004')
            expect(snapshotUrl).toBe('perspective://Qm12345')

        })

        it('queryLinks() smoke test', async () => {
            const links = await ad4mClient.perspective.queryLinks('000001', {source: 'root'})
            expect(links.length).toBe(1)
            expect(links[0].data.source).toBe('root')
            expect(links[0].data.target).toBe('neighbourhood://Qm12345')
        })

        it('queryProlog() smoke test', async () => {
            let result = await ad4mClient.perspective.queryProlog('000001', "link(X, 2).")
            expect(result.length).toBe(1)
            expect(result[0].X).toBe(1)

            const proxy = await ad4mClient.perspective.byUUID('000001')
            result = await proxy.infer("link(X, 2).")
            expect(result.length).toBe(1)
            expect(result[0].X).toBe(1)
        })

        it('add() smoke test', async () => {
            const p = await ad4mClient.perspective.add('p-name')
            expect(p.uuid).toBe('00006')
            expect(p.name).toBe('p-name')
        })

        it('update() smoke test', async () => {
            const p = await ad4mClient.perspective.update('00001', 'new-name')
            expect(p.uuid).toBe('00001')
            expect(p.name).toBe('new-name')
        })

        it('remove() smoke test', async () => {
            const r = await ad4mClient.perspective.remove('000001')
            expect(r).toBeTruthy()
        })

        it('addLink() smoke test', async () => {
            const link = await ad4mClient.perspective.addLink('00001', {source: 'root', target: 'lang://Qm123', predicate: 'p'})
            expect(link.author).toBe('did:ad4m:test')
            expect(link.data.source).toBe('root')
            expect(link.data.predicate).toBe('p')
            expect(link.data.target).toBe('lang://Qm123')
            expect(link.status).toBe('shared')
        })

        it('addLocalLink() smoke test', async () => {
            const link = await ad4mClient.perspective.addLink('00001', {source: 'root', target: 'lang://Qm123', predicate: 'p'}, 'local')
            expect(link.author).toBe('did:ad4m:test')
            expect(link.data.source).toBe('root')
            expect(link.data.predicate).toBe('p')
            expect(link.data.target).toBe('lang://Qm123')
            expect(link.status).toBe('local')
        })

        it('addLinks() smoke test', async () => {
            const links = await ad4mClient.perspective.addLinks('00001', [
                {source: 'root', target: 'lang://Qm123', predicate: 'p'},
                {source: 'root', target: 'lang://Qm123', predicate: 'p'}
            ])
            expect(links.length).toBe(2)
            expect(links[0].author).toBe('did:ad4m:test')
            expect(links[0].data.source).toBe('root')
            expect(links[0].data.predicate).toBe('p')
            expect(links[0].data.target)
        })

        it('removeLinks() smoke test', async () => {
            const links = await ad4mClient.perspective.removeLinks('00001', [
                {author: '', timestamp: '', proof: {signature: '', key: ''}, data: {source: 'root', target: 'lang://Qm123', predicate: 'p'}},
                {author: '', timestamp: '', proof: {signature: '', key: ''}, data: {source: 'root', target: 'lang://Qm123', predicate: 'p'}}
            ])
            expect(links.length).toBe(2)
            expect(links[0].author).toBe('did:ad4m:test')
            expect(links[0].data.source).toBe('root')
            expect(links[0].data.predicate).toBe('p')
            expect(links[0].data.target)
        })

        it('linkMutations() smoke test', async () => {
            const mutations = await ad4mClient.perspective.linkMutations('00001', {
                additions: [
                    {source: 'root', target: 'lang://Qm123', predicate: 'p'},
                    {source: 'root', target: 'lang://Qm123', predicate: 'p'}
                ],
                removals: [
                    {author: '', timestamp: '', proof: {signature: '', key: ''}, data: {source: 'root', target: 'lang://Qm123', predicate: 'p'}},
                    {author: '', timestamp: '', proof: {signature: '', key: ''}, data: {source: 'root', target: 'lang://Qm123', predicate: 'p'}}
                ]
            });
            expect(mutations.additions.length).toBe(2)
            expect(mutations.removals.length).toBe(2)

            expect(mutations.additions[0].author).toBe('did:ad4m:test')
            expect(mutations.additions[0].data.source).toBe('root')
            expect(mutations.additions[0].data.predicate).toBe('p')
            expect(mutations.additions[0].data.target).toBe('lang://Qm123')

            expect(mutations.removals[0].author).toBe('did:ad4m:test')
            expect(mutations.removals[0].data.source).toBe('root')
            expect(mutations.removals[0].data.predicate).toBe('p')
            expect(mutations.removals[0].data.target).toBe('lang://Qm123')
        })

        it('addLinkExpression() smoke test', async () => {
            const testLink = new LinkExpression()
            testLink.author = "did:ad4m:test"
            testLink.timestamp = Date.now().toString()
            testLink.data = {
                source: 'root',
                target: 'lang://Qm123',
                predicate: 'p'
            }
            testLink.proof = {
                signature: '',
                key: '',
                valid: true
            }
            const link = await ad4mClient.perspective.addLinkExpression('00001', testLink);
            expect(link.author).toBe('did:ad4m:test')
            expect(link.data.source).toBe('root')
            expect(link.data.predicate).toBe('p')
            expect(link.data.target).toBe('lang://Qm123')
        })

        it('addListener() smoke test', async () => {
            let perspective = await ad4mClient.perspective.byUUID('00004')

            const testLink = new LinkExpression()
            testLink.author = "did:ad4m:test"
            testLink.timestamp = Date.now().toString()
            testLink.data = {
                source: 'root',
                target: 'neighbourhood://Qm12345'
            }
            testLink.proof = {
                signature: '',
                key: '',
                valid: true
            }

            const linkAdded = jest.fn()
            const linkRemoved = jest.fn()

            await perspective.addListener('link-added', linkAdded)
            const link = new LinkExpressionInput()
            link.source = 'root'
            link.target = 'perspective://Qm34589a3ccc0'
            await perspective.add(link)

            expect(linkAdded).toBeCalledTimes(1)
            expect(linkRemoved).toBeCalledTimes(0)

            perspective = await ad4mClient.perspective.byUUID('00004')

            await perspective.addListener('link-removed', linkRemoved)
            await perspective.remove(testLink)

            expect(linkAdded).toBeCalledTimes(1)
            expect(linkRemoved).toBeCalledTimes(1)
        })

        it('removeListener() smoke test', async () => {
            let perspective = await ad4mClient.perspective.byUUID('00004')

            const linkAdded = jest.fn()

            await perspective.addListener('link-added', linkAdded)
            await perspective.add({source: 'root', target: 'neighbourhood://Qm12345'})

            expect(linkAdded).toBeCalledTimes(1)

            linkAdded.mockClear();

            perspective = await ad4mClient.perspective.byUUID('00004')

            await perspective.removeListener('link-added', linkAdded)
            await perspective.add({source: 'root', target: 'neighbourhood://Qm123456'})

            expect(linkAdded).toBeCalledTimes(1)
        })

        it('addSyncStateChangeListener() smoke test', async () => {
            let perspective = await ad4mClient.perspective.byUUID('00004')

            const syncState = jest.fn()

            await perspective.addSyncStateChangeListener(syncState)
            await perspective.add({source: 'root', target: 'neighbourhood://Qm12345'})

            expect(syncState).toBeCalledTimes(1)
            expect(syncState).toBeCalledWith(PerspectiveState.Synced)
        })

        it('updateLink() smoke test', async () => {
            const link = await ad4mClient.perspective.updateLink(
                '00001',
                {author: '', timestamp: '', proof: {signature: '', key: ''}, data:{source: 'root', target: 'none'}},
                {source: 'root', target: 'lang://Qm123', predicate: 'p'})
            expect(link.author).toBe('did:ad4m:test')
            expect(link.data.source).toBe('root')
            expect(link.data.predicate).toBe('p')
            expect(link.data.target).toBe('lang://Qm123')
        })

        it('removeLink() smoke test', async () => {
            const r = await ad4mClient.perspective.removeLink('00001', {author: '', timestamp: '', proof: {signature: '', key: ''}, data:{source: 'root', target: 'none'}})
            expect(r).toBeTruthy()
        })

        it('addSdna() smoke test', async () => {
            const r = await ad4mClient.perspective.addSdna('00001', "Test", 'subject_class("Test", test)', 'subject_class');
            expect(r).toBeTruthy()
        })

        it('executeCommands() smoke test', async () => {
            const result = await ad4mClient.perspective.executeCommands(
                '00001',
                'command1; command2',
                'expression1',
                'param1, param2'
            );
            expect(result).toBeTruthy();
        })

        it('getSubjectData() smoke test', async () => {
            const result = await ad4mClient.perspective.getSubjectData('00001', 'Test', 'test');
            expect(result).toBe("");
        });

        it('createSubject() smoke test', async () => {
            const result = await ad4mClient.perspective.createSubject(
                '00001',
                'command1; command2',
                'expression1',
            );
            expect(result).toBeTruthy();
        })
    })

    describe('.runtime', () => {
        it('quit() smoke test', async () => {
            const r = await ad4mClient.runtime.quit()
            expect(r).toBeTruthy()
        })

        it('openLink() smoke test', async () => {
            const r = await ad4mClient.runtime.openLink('https://ad4m.dev')
            expect(r).toBeTruthy()
        })

        it('addTrustedAgents() smoke test', async () => {
            const r = await ad4mClient.runtime.addTrustedAgents(["agentPubKey"]);
            expect(r).toStrictEqual([ 'agentPubKey' ])
        })

        it('deleteTrustedAgents() smoke test', async () => {
            const r = await ad4mClient.runtime.deleteTrustedAgents(["agentPubKey"]);
            expect(r).toStrictEqual([])
        })

        it('getTrustedAgents() smoke test', async () => {
            const r = await ad4mClient.runtime.getTrustedAgents();
            expect(r).toStrictEqual([ 'agentPubKey' ])
        })

        it('addKnownLinkLanguageTemplates() smoke test', async () => {
            const r = await ad4mClient.runtime.addKnownLinkLanguageTemplates(["Qm1337"]);
            expect(r).toStrictEqual([ 'Qm1337' ])
        })

        it('removeKnownLinkLanguageTemplates() smoke test', async () => {
            const r = await ad4mClient.runtime.removeKnownLinkLanguageTemplates(["Qm12345abcdef"]);
            expect(r).toStrictEqual([])
        })

        it('knownLinkLanguageTemplates() smoke test', async () => {
            const r = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(r).toStrictEqual([ 'Qm12345abcdef' ])
        })

        it('addFriends() smoke test', async () => {
            const r = await ad4mClient.runtime.addFriends(["did:test:another_friend"]);
            expect(r).toStrictEqual([ 'did:test:another_friend' ])
        })

        it('removeFriends() smoke test', async () => {
            const r = await ad4mClient.runtime.removeFriends(["did:test:friend"]);
            expect(r).toStrictEqual([])
        })

        it('friends() smoke test', async () => {
            const r = await ad4mClient.runtime.friends();
            expect(r).toStrictEqual([ 'did:test:friend' ])
        })

        it('hcAgentInfos smoke test', async () => {
            const agentInfos = JSON.parse(await ad4mClient.runtime.hcAgentInfos())
            expect(agentInfos.length).toBe(4)
            expect(agentInfos[0].agent).toBeDefined()
            expect(agentInfos[0].signature).toBeDefined()
            expect(agentInfos[0].agent_info).toBeDefined()
        })

        it('hcAddAgentInfos smoke test', async () => {
            await ad4mClient.runtime.hcAddAgentInfos("agent infos string")
        })

        it('ververifyStringSignedByDid() smoke test', async () => {
            const verify = await ad4mClient.runtime.verifyStringSignedByDid("did", "didSigningKeyId", "data", "signedData")
            expect(verify).toBe(true)
        })

        it('setStatus smoke test', async () => {
            const link = new LinkExpression()
            link.author = 'did:method:12345'
            link.timestamp = new Date().toString()
            link.data = new Link({source: 'root', target: 'perspective://Qm34589a3ccc0'})
            link.proof = { signature: 'asdfasdf', key: 'asdfasdf' }
            await ad4mClient.runtime.setStatus(new Perspective([link]))
        })

        it('friendStatus smoke test', async () => {
            const statusExpr = await ad4mClient.runtime.friendStatus("did:ad4m:test")
            expect(statusExpr.author).toBe("did:ad4m:test")
            const statusPersp = statusExpr.data
            expect(statusPersp.links.length).toBe(1)
            expect(statusPersp.links[0].data.source).toBe('root')
            expect(statusPersp.links[0].data.target).toBe('neighbourhood://Qm12345')
        })

        it('friendSendMessage smoke test', async () => {
            const link = new LinkExpression()
            link.author = 'did:method:12345'
            link.timestamp = new Date().toString()
            link.data = new Link({source: 'root', target: 'perspective://Qm34589a3ccc0'})
            link.proof = { signature: 'asdfasdf', key: 'asdfasdf' }
            await ad4mClient.runtime.friendSendMessage('did:ad4m:test', new Perspective([link]))
        })

        it('messageInbox smoke test', async () => {
            const messages = await ad4mClient.runtime.messageInbox()
            expect(messages.length).toBe(1)
            const message = messages[0]
            expect(message.author).toBe("did:ad4m:test")
            const messagePersp = message.data
            expect(messagePersp.links.length).toBe(1)
            expect(messagePersp.links[0].data.source).toBe('root')
            expect(messagePersp.links[0].data.target).toBe('neighbourhood://Qm12345')
        })

        it('messageOutbox smoke test', async () => {
            const sentMessages = await ad4mClient.runtime.messageOutbox("did:ad4m:test")
            expect(sentMessages.length).toBe(1)
            const sentMessage = sentMessages[0]
            expect(sentMessage.recipient).toBe("did:test:recipient")
            const message = sentMessage.message
            expect(message.author).toBe("did:ad4m:test")
            const messagePersp = message.data
            expect(messagePersp.links.length).toBe(1)
            expect(messagePersp.links[0].data.source).toBe('root')
            expect(messagePersp.links[0].data.target).toBe('neighbourhood://Qm12345')
        })

        it('runtimeInfo smoke test', async () => {
            const runtimeInfo = await ad4mClient.runtime.info();
            expect(runtimeInfo.ad4mExecutorVersion).toBe("x.x.x");
            expect(runtimeInfo.isInitialized).toBe(true);
            expect(runtimeInfo.isUnlocked).toBe(true);
        })

        it('requestInstallNotification smoke test', async () => {
            await ad4mClient.runtime.requestInstallNotification({
                description: "Test description",
                appName: "Test app name",
                appUrl: "https://example.com",
                appIconPath: "https://example.com/icon",
                trigger: "triple(X, ad4m://has_type, flux://message)",
                perspectiveIds: ["u983ud-jdhh38d"],
                webhookUrl: "https://example.com/webhook",
                webhookAuth: "test-auth",
            });
        })

        it('grantNotification smoke test', async () => {
            await ad4mClient.runtime.grantNotification("test-notification");
        })

        it('notifications smoke test', async () => {
            const notifications = await ad4mClient.runtime.notifications();
            expect(notifications.length).toBe(1);
        })

        it('updateNotification smoke test', async () => {
            await ad4mClient.runtime.updateNotification("test-notification", {
                description: "Test description",
                appName: "Test app name",
                appUrl: "https://example.com",
                appIconPath: "https://example.com/icon",
                trigger: "triple(X, ad4m://has_type, flux://message)",
                perspectiveIds: ["u983ud-jdhh38d"],
                webhookUrl: "https://example.com/webhook",
                webhookAuth: "test-auth",
            });
        })

        it('removeNotification smoke test', async () => {
            await ad4mClient.runtime.removeNotification("test-notification");
        })
    })

    describe('Ad4mClient subscriptions', () => {
        describe('ad4mClient without subscription', () => {
            let ad4mClientWithoutSubscription: Ad4mClient

            beforeEach(() => {
                ad4mClientWithoutSubscription = new Ad4mClient(apolloClient, false)
            })

            it('agent subscribeAgentUpdated smoke test', async () => {
                const agentUpdatedCallback = jest.fn()
                ad4mClientWithoutSubscription.agent.addUpdatedListener(agentUpdatedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(agentUpdatedCallback).toBeCalledTimes(0)

                ad4mClientWithoutSubscription.agent.subscribeAgentUpdated()
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithoutSubscription.agent.updateDirectMessageLanguage("lang://test");
                expect(agentUpdatedCallback).toBeCalledTimes(1)
            })

            it('agent subscribeAgentStatusChanged smoke test', async () => {
                const agentStatusChangedCallback = jest.fn()
                ad4mClientWithoutSubscription.agent.addAgentStatusChangedListener(agentStatusChangedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(agentStatusChangedCallback).toBeCalledTimes(0)

                ad4mClientWithoutSubscription.agent.subscribeAgentStatusChanged()
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithoutSubscription.agent.unlock("test", false);
                expect(agentStatusChangedCallback).toBeCalledTimes(1)
            })

            it('agent subscribeAppsChanged smoke test', async () => {
                const appsChangedCallback = jest.fn()
                ad4mClientWithoutSubscription.agent.addAppChangedListener(appsChangedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(appsChangedCallback).toBeCalledTimes(0)

                ad4mClientWithoutSubscription.agent.subscribeAppsChanged()
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithoutSubscription.agent.removeApp("test");
                expect(appsChangedCallback).toBeCalledTimes(1)
            })

            it('perspective subscribePerspectiveAdded smoke test', async () => {
                const perspectiveAddedCallback = jest.fn()
                ad4mClientWithoutSubscription.perspective.addPerspectiveAddedListener(perspectiveAddedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveAddedCallback).toBeCalledTimes(0)

                ad4mClientWithoutSubscription.perspective.subscribePerspectiveAdded()
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithoutSubscription.perspective.add('p-name-1');
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveAddedCallback).toBeCalledTimes(1)
            })

            it('perspective subscribePerspectiveUpdated smoke test', async () => {
                const perspectiveUpdatedCallback = jest.fn()
                ad4mClientWithoutSubscription.perspective.addPerspectiveUpdatedListener(perspectiveUpdatedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveUpdatedCallback).toBeCalledTimes(0)

                ad4mClientWithoutSubscription.perspective.subscribePerspectiveUpdated()
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithoutSubscription.perspective.update('00006', 'p-test2');
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveUpdatedCallback).toBeCalledTimes(1)
            })

            it('perspective subscribePerspectiveRemoved smoke test', async () => {
                const perspectiveRemovedCallback = jest.fn()
                ad4mClientWithoutSubscription.perspective.addPerspectiveRemovedListener(perspectiveRemovedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveRemovedCallback).toBeCalledTimes(0)

                ad4mClientWithoutSubscription.perspective.subscribePerspectiveRemoved()
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithoutSubscription.perspective.remove('00006');
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveRemovedCallback).toBeCalledTimes(1)
            })
        })

        describe('ad4mClient with subscription', () => {
            let ad4mClientWithSubscription

            beforeEach(() => {
                ad4mClientWithSubscription = new Ad4mClient(apolloClient, true)
            })

            it('agent subscribeAgentUpdated smoke test', async () => {
                const agentUpdatedCallback = jest.fn()
                ad4mClientWithSubscription.agent.addUpdatedListener(agentUpdatedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(agentUpdatedCallback).toBeCalledTimes(0)

                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithSubscription.agent.updateDirectMessageLanguage("lang://test");
                expect(agentUpdatedCallback).toBeCalledTimes(1)
            })

            it('agent subscribeAgentStatusChanged smoke test', async () => {
                const agentStatusChangedCallback = jest.fn()
                ad4mClientWithSubscription.agent.addAgentStatusChangedListener(agentStatusChangedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(agentStatusChangedCallback).toBeCalledTimes(0)

                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithSubscription.agent.unlock("test", false);
                expect(agentStatusChangedCallback).toBeCalledTimes(1)
            })

            it('agent subscribeAppsChanged smoke test', async () => {
                const appsChangedCallback = jest.fn()
                ad4mClientWithSubscription.agent.addAppChangedListener(appsChangedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(appsChangedCallback).toBeCalledTimes(0)

                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithSubscription.agent.removeApp("test");
                expect(appsChangedCallback).toBeCalledTimes(1)
            })

            it('perspective subscribePerspectiveAdded smoke test', async () => {
                const perspectiveAddedCallback = jest.fn()
                ad4mClientWithSubscription.perspective.addPerspectiveAddedListener(perspectiveAddedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveAddedCallback).toBeCalledTimes(0)

                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithSubscription.perspective.add('p-name-1');
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveAddedCallback).toBeCalledTimes(1)
            })

            it('perspective subscribePerspectiveUpdated smoke test', async () => {
                const perspectiveUpdatedCallback = jest.fn()
                ad4mClientWithSubscription.perspective.addPerspectiveUpdatedListener(perspectiveUpdatedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveUpdatedCallback).toBeCalledTimes(0)

                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithSubscription.perspective.update('00006', 'p-test2');
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveUpdatedCallback).toBeCalledTimes(1)
            })

            it('perspective subscribePerspectiveRemoved smoke test', async () => {
                const perspectiveRemovedCallback = jest.fn()
                ad4mClientWithSubscription.perspective.addPerspectiveRemovedListener(perspectiveRemovedCallback)
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveRemovedCallback).toBeCalledTimes(0)

                await new Promise<void>(resolve => setTimeout(resolve, 100))
                await ad4mClientWithSubscription.perspective.remove('00006');
                await new Promise<void>(resolve => setTimeout(resolve, 100))
                expect(perspectiveRemovedCallback).toBeCalledTimes(1)
            })
        })
    })
    describe('.ai', () => {
        it('embed()', async () => {
            const vector = await ad4mClient.ai.embed("model", "test ets")
            expect(vector[0]).toEqual(0)
            expect(vector[1]).toEqual(10)
            expect(vector[2]).toEqual(20)
            expect(vector[3]).toEqual(30)
        })

        it('tasks()', async () => {
            const tasks = await ad4mClient.ai.tasks()
            expect(tasks.length).toBe(2)
            expect(tasks[0].taskId).toBe("task_id")
            expect(tasks[0].modelId).toBe("modelId")
        })

        it('addTask()', async () => {
            const task = await ad4mClient.ai.addTask("task_name", "model_id", "system prompt", []);
            expect(task.name).toBe("task_name")
            expect(task.taskId).toBe("task_id")
            expect(task.modelId).toBe("model_id")
            expect(task.systemPrompt).toBe("system prompt")
        });

        it('removeTask()', async () => {
            const task = await ad4mClient.ai.removeTask("task_id", "system prompt", []);
            expect(task.taskId).toBe("task_id")
            expect(task.modelId).toBe("model_id")
            expect(task.systemPrompt).toBe("system prompt")
        });

        it('updateTask()', async () => {
            const task = await ad4mClient.ai.updateTask("task_id", {
                name: "task_name",
                modelId: "model_id",
                systemPrompt: "system prompt",
                promptExamples: []
            });
            expect(task.name).toBe("task_name")
            expect(task.taskId).toBe("task_id")
            expect(task.modelId).toBe("model_id")
            expect(task.systemPrompt).toBe("system prompt")
        });

        it('prompt()', async () => {
            const prompt = await ad4mClient.ai.prompt("task_id", "Do something");
            console.log(prompt)
            expect(prompt).toBe("output")
        })

        it('openTranscriptionStream(), closeTranscriptionStream(), feedTranscriptionStream() & aiTranscriptionText subscription', async () => {
            const streamCallback = jest.fn()
            const streamId = await ad4mClient.ai.openTranscriptionStream("model_id", streamCallback);
            expect(streamId).toBeTruthy()
            expect(streamId).toBe("streamId")
            expect(streamCallback).toBeCalledTimes(0)

            await new Promise<void>(resolve => setTimeout(resolve, 100))

            await ad4mClient.ai.feedTranscriptionStream(streamId, [0, 10, 20, 30]);

            await new Promise<void>(resolve => setTimeout(resolve, 100))

            expect(streamCallback).toBeCalledTimes(1)

            const stream = await ad4mClient.ai.closeTranscriptionStream(streamId)
            expect(stream).toBeTruthy()
        })
    })
})