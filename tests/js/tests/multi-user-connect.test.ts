import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import Ad4mConnect from "@coasys/ad4m-connect";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch'

//@ts-ignore
global.fetch = fetch

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Multi-User Ad4m-Connect integration tests", () => {
    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "multi-user-connect-agent");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 15800
    const hcAdminPort = 15801
    const hcAppPort = 15802

    let executorProcess: ChildProcess | null = null
    let adminAd4mClient: Ad4mClient | null = null

    before(async () => {
        if (!fs.existsSync(appDataPath)) {
            fs.mkdirSync(appDataPath, { recursive: true });
        }

        // Start executor with multi-user mode enabled (no admin credential for this test)
        executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
            gqlPort, hcAdminPort, hcAppPort, false);

        adminAd4mClient = new Ad4mClient(apolloClient(gqlPort), false)
        
        // Generate initial admin agent (needed for JWT signing)
        await adminAd4mClient.agent.generate("passphrase")
    })

    after(async () => {
        if (executorProcess) {
            while (!executorProcess?.killed) {
                let status = executorProcess?.kill();
                console.log("killed executor with", status);
                await sleep(500);
            }
        }
    })

    describe("Multi-User Connect Flow", () => {
        it("should create user and login via ad4m-connect", async () => {
            // Create ad4m-connect instance with multi-user options
            const ui = new Ad4mConnect({
                appName: "Multi-User Test App",
                appDesc: "Testing multi-user functionality",
                appDomain: "test.ad4m.org",
                appIconPath: "https://example.com/icon.png",
                capabilities: [{ 
                    with: { domain: "*", pointers: ["*"] }, 
                    can: ["*"] 
                }],
                multiUser: true,
                backendUrl: `ws://localhost:${gqlPort}/graphql`,
                userEmail: "test@example.com",
                userPassword: "password123"
            });

            // Connect should handle user creation and login automatically
            const client = await ui.connect();
            expect(client).to.be.ok;

            // Verify we have an authenticated client
            const status = await client.agent.status();
            expect(status.isUnlocked).to.be.true;
            expect(status.did).to.be.ok;
            expect(status.did).to.match(/^did:key:.+/);

            // Verify the agent.me returns the correct user DID
            const agent = await client.agent.me();
            expect(agent.did).to.equal(status.did);
            
            console.log("Successfully connected as user:", agent.did);
        })

        it("should login existing user via ad4m-connect", async () => {
            // First, create a user directly via admin client
            const userResult = await adminAd4mClient!.agent.createUser("existing@example.com", "password456");
            expect(userResult.success).to.be.true;

            // Now try to connect via ad4m-connect with existing user credentials
            const ui = new Ad4mConnect({
                appName: "Multi-User Test App",
                appDesc: "Testing multi-user functionality",
                appDomain: "test.ad4m.org",
                capabilities: [{ 
                    with: { domain: "*", pointers: ["*"] }, 
                    can: ["*"] 
                }],
                multiUser: true,
                backendUrl: `ws://localhost:${gqlPort}/graphql`,
                userEmail: "existing@example.com",
                userPassword: "password456"
            });

            // Connect should login the existing user
            const client = await ui.connect();
            expect(client).to.be.ok;

            // Verify we're logged in as the correct user
            const agent = await client.agent.me();
            expect(agent.did).to.equal(userResult.did);
            
            console.log("Successfully logged in existing user:", agent.did);
        })

        it("should fail with wrong password", async () => {
            // Try to connect with wrong password
            const ui = new Ad4mConnect({
                appName: "Multi-User Test App",
                appDesc: "Testing multi-user functionality", 
                appDomain: "test.ad4m.org",
                capabilities: [{ 
                    with: { domain: "*", pointers: ["*"] }, 
                    can: ["*"] 
                }],
                multiUser: true,
                backendUrl: `ws://localhost:${gqlPort}/graphql`,
                userEmail: "existing@example.com",
                userPassword: "wrongpassword"
            });

            // Connect should fail
            const call = async () => {
                return await ui.connect();
            };

            await expect(call()).to.be.rejected;
        })
    })
})
