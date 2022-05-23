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
import sleep from "./sleep"

const TEST_DIR = path.join(`${__dirname}/../../src/test-temp`);
const appDataPath = path.join(TEST_DIR, "agents", "auth-agent");
const bootstrapSeedPath = path.join(`${__dirname}/../../src/tests/bootstrapSeed.json`);
const ipfsRepoPath = path.join(appDataPath);

function apolloClient(port: number, token?: string): ApolloClient<any> {
    return new ApolloClient({
        link: new WebSocketLink({
            uri: `http://localhost:${port}/graphql`,
            options: {
                reconnect: true,
                connectionParams: () => {
                    return {
                        headers: {
                            authorization: token
                        }
                    }
                }
            },
            webSocketImpl: ws,
        }),
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
        let core: PerspectivismCore | null = null
        let unAuthenticatedAppAd4mClient: Ad4mClient | null = null

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
                gqlPort: 14000,
                hcPortAdmin: 12000,
                hcPortApp: 11337,
                ipfsSwarmPort: 14002,
                ipfsRepoPath,
                hcUseBootstrap: false,
                hcUseProxy: false,
                hcUseLocalProxy: false,
                hcUseMdns: true
            } as OuterConfig)

            core.initControllers()
            await core.initLanguages()

            unAuthenticatedAppAd4mClient = new Ad4mClient(apolloClient(14000))
            await unAuthenticatedAppAd4mClient.agent.generate("passphrase")
        })

        afterAll(async () => {
            await core!.exit();
            await sleep(500)
        })

        it("unauthenticated user has all the capabilities", async () => {
            let status = await unAuthenticatedAppAd4mClient!.agent.status()
            expect(status.isUnlocked).toBeTruthy

            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]')
            expect(requestId).toMatch(/.+/)

            let rand = await unAuthenticatedAppAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]}}`)
            expect(rand).toMatch(/\d+/)

            let jwt = await unAuthenticatedAppAd4mClient!.agent.generateJwt(requestId, rand)
            expect(jwt).toMatch(/.+/)
        })
    })

    describe("admin credential is set", () => {
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
                ipfsRepoPath,
                hcUseBootstrap: false,
                hcUseProxy: false,
                hcUseLocalProxy: false,
                hcUseMdns: true,
                reqCredential: "123"
            } as OuterConfig)

            agentCore.initControllers()
            await agentCore.initLanguages()

            adminAd4mClient = new Ad4mClient(apolloClient(4000, "123"))
            await adminAd4mClient.agent.generate("passphrase")

            unAuthenticatedAppAd4mClient = new Ad4mClient(apolloClient(4000))
        })

        afterAll(async () => {
            await agentCore!.exit();
            await sleep(500)
        })

        it("unauthenticated user can not query agent status", async () => {
            const call = async () => {
                return await unAuthenticatedAppAd4mClient!.agent.status()
            }

            await expect(call())
                .rejects
                .toThrow("Capability is not matched");
        })

        it("unauthenticated user can request capability", async () => {
            const call = async () => {
                return await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]')
            }

            await expect(call())
                .resolves
                .toMatch(/.+/)
        })

        it("admin user can permit capability", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]')
            const call = async () => {
                return await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]}}`)
            }

            await expect(call())
                .resolves
                .toMatch(/\d+/)
        })

        it("unauthenticated user can generate jwt with a secret", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]')
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]}}`)

            const call = async () => {
                return await adminAd4mClient!.agent.generateJwt(requestId, rand)
            }

            await expect(call())
                .resolves
                .toMatch(/.+/)
        })

        it("authenticated user can query agent status if capability matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]')
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["QUERY"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(4000, jwt))

            expect((await authenticatedAppAd4mClient!.agent.status()).isUnlocked).toBeTruthy
        })

        it("user with invalid jwt can not query agent status", async () => {
            let ad4mClient = new Ad4mClient(apolloClient(4000, "invalid-jwt"))

            const call = async () => {
                return await ad4mClient!.agent.status()
            }

            await expect(call())
                .rejects
                .toThrow("Invalid Compact JWS")
        })

        it("authenticated user can not query agent status if capability is matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["MUTATION"]}]')
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["MUTATION"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(4000, jwt))

            const call = async () => {
                return await authenticatedAppAd4mClient!.agent.status()
            }

            await expect(call())
                .rejects
                .toThrow("Capability is not matched")
        })
    })
})
