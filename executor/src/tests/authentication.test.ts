import { ApolloClient, InMemoryCache } from "@apollo/client/core";
import Websocket from "ws";
import main from "../main";
import path from "path";
import { OuterConfig } from "../main";
import { Ad4mClient, AuthInfoInput, CapabilityInput } from "@perspect3vism/ad4m";
import fs from "fs-extra";
import PerspectivismCore from "../core/PerspectivismCore";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from "graphql-ws";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

function apolloClient(port: number, token?: string): ApolloClient<any> {
    const wsLink = new GraphQLWsLink(createClient({
        url: `ws://localhost:${port}/graphql`,
        webSocketImpl: Websocket,
        connectionParams: () => {
            return {
                headers: {
                    authorization: token
                }
            }
        },
    }));

    return new ApolloClient({
        link: wsLink,
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
        const TEST_DIR = path.join(`${__dirname}/../../src/tst-tmp`);
        const appDataPath = path.join(TEST_DIR, "agents", "unauth-agent");
        const bootstrapSeedPath = path.join(`${__dirname}/../../src/tests/bootstrapSeed.json`);
        const ipfsRepoPath = path.join(appDataPath);
        const gqlPort = 16000
        const hcPortAdmin = 16001
        const hcPortApp = 16002
        const ipfsSwarmPort = 16003

        let core: PerspectivismCore | null = null
        let ad4mClient: Ad4mClient | null = null

        before(async () => {
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
                hcUseMdns: true,
                runDappServer: false
            } as OuterConfig)

            // @ts-ignore
            ad4mClient = new Ad4mClient(apolloClient(gqlPort))
            await ad4mClient.agent.generate("passphrase")
        })

        after(async () => {
            await core!.exit();
        })

        it("unauthenticated user has all the capabilities", async () => {
            let status = await ad4mClient!.agent.status()
            expect(status.isUnlocked).to.be.true;

            let requestId = await ad4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            expect(requestId).match(/.+/);

            let rand = await ad4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            expect(rand).match(/\d+/);

            let jwt = await ad4mClient!.agent.generateJwt(requestId, rand)
            expect(jwt).match(/.+/);
        })
    })

    describe("admin credential is set", () => {
        const TEST_DIR = path.join(`${__dirname}/../../src/tst-tmp`);
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

        before(async () => {
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
                reqCredential: "123",
                runDappServer: false
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

        after(async () => {
            await agentCore!.exit();
        })

        it("unauthenticated user can not query agent status", async () => {
            const call = async () => {
                return await unAuthenticatedAppAd4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Capability is not matched");
        })

        it("unauthenticated user can request capability", async () => {
            const call = async () => {
                return await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                    appName: "demo-app",
                    appDesc: "demo-desc",
                    appDomain: "test.ad4m.org",
                    appUrl: "https://demo-link",
                    capabilities: [
                        {
                            with: {
                                domain:"agent",
                                pointers:["*"]
                            },
                            can: ["READ"]
                        }
                    ] as CapabilityInput[]
                } as AuthInfoInput)
            }

            expect(await call()).to.be.ok.match(/.+/);
        })

        it("admin user can permit capability", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            const call = async () => {
                return await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            }

            expect(await call()).to.be.ok.match(/\d+/);
        })

        it("unauthenticated user can generate jwt with a secret", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)

            const call = async () => {
                return await adminAd4mClient!.agent.generateJwt(requestId, rand)
            }

            expect(await call()).to.be.ok.match(/.+/);
        })

        it("authenticated user can query agent status if capability matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            // @ts-ignore
            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt))
            expect((await authenticatedAppAd4mClient!.agent.status()).isUnlocked).to.be.true;
        })

        it("user with invalid jwt can not query agent status", async () => {
            // @ts-ignore
            let ad4mClient = new Ad4mClient(apolloClient(gqlPort, "invalid-jwt"))

            const call = async () => {
                return await ad4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Invalid Compact JWS");
        })

        it("authenticated user can not query agent status if capability is not matched", async () => {
            let requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
                appName: "demo-app",
                appDesc: "demo-desc",
                appDomain: "test.ad4m.org",
                appUrl: "https://demo-link",
                capabilities: [
                    {
                        with: {
                            domain:"agent",
                            pointers:["*"]
                        },
                        can: ["CREATE"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput)
            let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["CREATE"]}]}}`)
            let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

            // @ts-ignore
            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt))

            const call = async () => {
                return await authenticatedAppAd4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Capability is not matched");
        })
    })
})
