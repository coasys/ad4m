import path from "path";
import { Ad4mClient, AuthInfoInput, CapabilityInput } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch'
import { ExceptionInfo } from "@coasys/ad4m/lib/src/runtime/RuntimeResolver";

//@ts-ignore
global.fetch = fetch

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Authentication integration tests", () => {
    describe("admin credential is not set", () => {
        const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
        const appDataPath = path.join(TEST_DIR, "agents", "unauth-agent");
        const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
        const gqlPort = 15100
        const hcAdminPort = 15101
        const hcAppPort = 15102

        let executorProcess: ChildProcess | null = null
        let ad4mClient: Ad4mClient | null = null

        before(async () => {
            if (!fs.existsSync(appDataPath)) {
                fs.mkdirSync(appDataPath, { recursive: true });
            }

            executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
                gqlPort, hcAdminPort, hcAppPort);

            ad4mClient = new Ad4mClient(apolloClient(gqlPort), false)
            await ad4mClient.agent.generate("passphrase")
        })

        after(async () => {
            if (executorProcess) {
                while (!executorProcess?.killed) {
                    let status  = executorProcess?.kill();
                    console.log("killed executor with", status);
                    await sleep(500);
                }
            }
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
        const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
        const appDataPath = path.join(TEST_DIR, "agents", "auth-agent");
        const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
        const gqlPort = 15200
        const hcAdminPort = 15202
        const hcAppPort = 15203

        let executorProcess: ChildProcess | null = null
        let adminAd4mClient: Ad4mClient | null = null
        let unAuthenticatedAppAd4mClient: Ad4mClient | null = null

        before(async () => {
            if (!fs.existsSync(appDataPath)) {
                fs.mkdirSync(appDataPath, { recursive: true });
            }

            executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
                gqlPort, hcAdminPort, hcAppPort, false, "123");
       
            adminAd4mClient = new Ad4mClient(apolloClient(gqlPort, "123"), false)
            await adminAd4mClient.agent.generate("passphrase")
            
            unAuthenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort), false)
        })

        after(async () => {
            if (executorProcess) {
                executorProcess.kill()
            }
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
            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt), false)
            expect((await authenticatedAppAd4mClient!.agent.status()).isUnlocked).to.be.true;
        })

        it("user with invalid jwt can not query agent status", async () => {
            // @ts-ignore
            let ad4mClient = new Ad4mClient(apolloClient(gqlPort, "invalid-jwt"), false)

            const call = async () => {
                return await ad4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("InvalidToken");
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
            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt), false)

            const call = async () => {
                return await authenticatedAppAd4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Capability is not matched");
        })

        it("user with revoked token can not query agent status", async () => {
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
            let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt), false)
            expect((await authenticatedAppAd4mClient!.agent.status()).isUnlocked).to.be.true;

            let oldApps = await adminAd4mClient!.agent.getApps();
            let newApps = await adminAd4mClient!.agent.revokeToken(requestId);
            // revoking token should not change the number of apps
            expect(newApps.length).to.be.equal(oldApps.length);
            newApps.forEach((app, i) => {
                if(app.requestId === requestId) {
                    expect(app.revoked).to.be.true;
                }
            })

            const call = async () => {
                return await authenticatedAppAd4mClient!.agent.status()
            }

            await expect(call()).to.be.rejectedWith("Unauthorized access");
        })

        it("requesting a capability toke should trigger a CapabilityRequested exception", async () => {
            let excpetions: ExceptionInfo[] = [];
            adminAd4mClient!.runtime.addExceptionCallback((e) => { excpetions.push(e); return null; })
            adminAd4mClient!.runtime.subscribeExceptionOccurred();
            
            await sleep(1000);

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
            
            await sleep(1000);

            expect(excpetions.length).to.be.equal(1);
            expect(excpetions[0].type).to.be.equal("CAPABILITY_REQUESTED");
            let auth_info = JSON.parse(excpetions[0].addon!);
            expect(auth_info.requestId).to.be.equal(requestId);
        })
    })
})
