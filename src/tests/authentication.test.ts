import {
    ApolloClient,
    InMemoryCache,
} from "@apollo/client/core";
import { WebSocketLink } from '@apollo/client/link/ws';
import ws from "ws"
import main from "../main";
import path from "path";
import { OuterConfig } from "../types";
import { Ad4mClient } from "@perspect3vism/ad4m";
import fs from "fs-extra";
import PerspectivismCore from "../core/PerspectivismCore";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from "graphql-ws";

function apolloClient(port: number, token?: string): ApolloClient<any> {
    return new ApolloClient({
        link: new GraphQLWsLink(
            createClient({
                url: `ws://localhost:${port}/graphql`,
                connectionParams: () => {
                    return {
                        headers: {
                            authorization: token
                        }
                    }
                },
                webSocketImpl: ws
            }),
          ),
        cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
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

describe("Authentication integration tests", () => {
    describe("admin credential is not set", () => {
        const TEST_DIR = path.join(`${__dirname}/../../src/test-temp`);
        const appDataPath = path.join(TEST_DIR, "agents", "unauth-agent");
        const bootstrapSeedPath = path.join(`${__dirname}/../../src/tests/bootstrapSeed.json`);
        const ipfsRepoPath = path.join(appDataPath);
        const gqlPort = 16000
        const hcPortAdmin = 16001
        const hcPortApp = 16002
        const ipfsSwarmPort = 16003

        let core: PerspectivismCore | null = null
        let ad4mClient: Ad4mClient | null = null

        beforeAll(async () => {
            if (!fs.existsSync(appDataPath)) {
                fs.mkdirSync(appDataPath, { recursive: true });
            }

            core = await main.init({
                appDataPath,
                resourcePath: TEST_DIR,
                networkBootstrapSeed: bootstrapSeedPath,
                bootstrapFixtures: {
                    languages: [],
                    perspectives: [],
                },
                mocks: false,
                gqlPort,
                hcPortAdmin,
                hcPortApp,
                ipfsSwarmPort,
                ipfsRepoPath,
                hcUseBootstrap: false,
                hcUseProxy: false,
                hcUseLocalProxy: false,
                hcUseMdns: true
            } as OuterConfig)

            // @ts-ignore
            ad4mClient = new Ad4mClient(apolloClient(gqlPort))
            await ad4mClient.agent.generate("passphrase")
        })

        afterAll(async () => {
            await core!.exit();
        })

        it("unauthenticated user has all the capabilities", async () => {
            let status = await ad4mClient!.agent.status()
            expect(status.isUnlocked).toBeTruthy

            let requestId = await ad4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]')
            expect(requestId).toMatch(/.+/)

            let rand = await ad4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            expect(rand).toMatch(/\d+/)

            let jwt = await ad4mClient!.agent.generateJwt(requestId, rand)
            expect(jwt).toMatch(/.+/)
        })
    })

    describe("admin credential is set", () => {
        const TEST_DIR = path.join(`${__dirname}/../../src/test-temp`);
        const appDataPath = path.join(TEST_DIR, "agents", "auth-agent");
        const bootstrapSeedPath = path.join(`${__dirname}/../../src/tests/bootstrapSeed.json`);
        const ipfsRepoPath = path.join(appDataPath);
        const gqlPort = 15000
        const hcPortAdmin = 15001
        const hcPortApp = 15002
        const ipfsSwarmPort = 15003

        let agentCore: PerspectivismCore | null = null
        let adminAd4mClient: Ad4mClient | null = null
        let unAuthenticatedAppAd4mClient: Ad4mClient | null = null

        beforeAll(async () => {
            if (!fs.existsSync(appDataPath)) {
                fs.mkdirSync(appDataPath, { recursive: true });
            }

            agentCore = await main.init({
                appDataPath,
                resourcePath: TEST_DIR,
                networkBootstrapSeed: bootstrapSeedPath,
                bootstrapFixtures: {
                    languages: [],
                    perspectives: [],
                },
                mocks: false,
                gqlPort,
                hcPortAdmin,
                hcPortApp,
                ipfsSwarmPort,
                ipfsRepoPath,
                hcUseBootstrap: false,
                hcUseProxy: false,
                hcUseLocalProxy: false,
                hcUseMdns: true,
                reqCredential: "123"
            } as OuterConfig)

            // @ts-ignore            
            adminAd4mClient = new Ad4mClient(apolloClient(gqlPort, "123"))
            await adminAd4mClient.agent.generate("passphrase")
            // await agentCore.waitForAgent();
            // agentCore.initControllers()
            // await agentCore.initLanguages()
            

            // @ts-ignore
            unAuthenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort))
        })

        afterAll(async () => {
            console.warn("After all called");
            await agentCore!.exit();
        })

        // it("unauthenticated user can not query agent status", async () => {
        //     const call = async () => {
        //         console.warn("blocked 1");
        //         return await unAuthenticatedAppAd4mClient!.agent.status()
        //     }

        //     await expect(call())
        //         .rejects
        //         .toThrowError("Capability is not matched");

        //     console.warn("blocked 2");
        // })

        it("unauthenticated user can request capability", async () => {
            const call = async () => {
                return await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]')
            }

            await expect(call())
                .resolves
                .toMatch(/.+/)
        })

        it("admin user can permit capability", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]')
            const call = async () => {
                return await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            }

            await expect(call())
                .resolves
                .toMatch(/\d+/)
        })

        it("unauthenticated user can generate jwt with a secret", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]')
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)

            const call = async () => {
                return await adminAd4mClient!.agent.generateJwt(requestId, rand)
            }

            await expect(call())
                .resolves
                .toMatch(/.+/)
        })

        it("authenticated user can query agent status if capability matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]')
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            console.warn("Init ad4m client1");
            // @ts-ignore
            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt))
            console.warn("Finish init 1");
            expect((await authenticatedAppAd4mClient!.agent.status()).isUnlocked).toBeTruthy
        })

        it("user with invalid jwt can not query agent status", async () => {
            // @ts-ignore
            let ad4mClient = new Ad4mClient(apolloClient(gqlPort, "invalid-jwt"))

            const call = async () => {
                return await ad4mClient!.agent.status()
            }

            await expect(call())
                .rejects
                .toThrow("Invalid Compact JWS")
        })

        it("authenticated user can not query agent status if capability is not matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["CREATE"]}]')
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["CREATE"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            console.warn("Init client 2");
            // @ts-ignore
            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt))
            console.warn("Finish init2");

            const call = async () => {
                return await authenticatedAppAd4mClient!.agent.status()
            }

            await expect(call())
                .rejects
                .toThrowError("Capability is not matched")
        })
    })
})
