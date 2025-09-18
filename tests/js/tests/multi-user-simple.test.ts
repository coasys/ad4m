import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
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

describe("Multi-User Simple integration tests", () => {
    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "multi-user-simple");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 15900
    const hcAdminPort = 15901
    const hcAppPort = 15902

    let executorProcess: ChildProcess | null = null
    let adminAd4mClient: Ad4mClient | null = null

    before(async () => {
        if (!fs.existsSync(appDataPath)) {
            fs.mkdirSync(appDataPath, { recursive: true });
        }

        // Start executor (no admin credential for this test - all tokens have admin capabilities)
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

    describe("Basic Multi-User Functionality", () => {
        it("should create and login users with unique DIDs", async () => {
            // Create first user
            const user1Result = await adminAd4mClient!.agent.createUser("alice@example.com", "password123");
            expect(user1Result.success).to.be.true;
            expect(user1Result.did).to.match(/^did:key:.+/);

            // Create second user
            const user2Result = await adminAd4mClient!.agent.createUser("bob@example.com", "password456");
            expect(user2Result.success).to.be.true;
            expect(user2Result.did).to.match(/^did:key:.+/);

            // Users should have different DIDs
            expect(user1Result.did).to.not.equal(user2Result.did);

            // Login first user
            const user1Token = await adminAd4mClient!.agent.loginUser("alice@example.com", "password123");
            expect(user1Token).to.be.ok;

            // Login second user
            const user2Token = await adminAd4mClient!.agent.loginUser("bob@example.com", "password456");
            expect(user2Token).to.be.ok;

            // Verify JWT tokens contain correct user DIDs
            const user1Payload = JSON.parse(atob(user1Token.split('.')[1]));
            const user2Payload = JSON.parse(atob(user2Token.split('.')[1]));

            expect(user1Payload.sub).to.equal(user1Result.did);
            expect(user2Payload.sub).to.equal(user2Result.did);
            expect(user1Payload.capabilities.userDid).to.equal(user1Result.did);
            expect(user2Payload.capabilities.userDid).to.equal(user2Result.did);
        })

        it("should return correct user DID in agent.me", async () => {
            // Create and login user
            const userResult = await adminAd4mClient!.agent.createUser("charlie@example.com", "password789");
            const userToken = await adminAd4mClient!.agent.loginUser("charlie@example.com", "password789");

            // Create authenticated client
            const userClient = new Ad4mClient(apolloClient(gqlPort, userToken), false);
            
            // Test agent.me
            const agent = await userClient.agent.me();
            expect(agent.did).to.equal(userResult.did);

            // Test agent.status
            const status = await userClient.agent.status();
            expect(status.did).to.equal(userResult.did);
            expect(status.isUnlocked).to.be.true;
        })

        it("should handle login persistence", async () => {
            // Create user
            const userResult = await adminAd4mClient!.agent.createUser("dave@example.com", "passwordABC");
            
            // Login first time
            const token1 = await adminAd4mClient!.agent.loginUser("dave@example.com", "passwordABC");
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            const agent1 = await client1.agent.me();

            // Login second time
            const token2 = await adminAd4mClient!.agent.loginUser("dave@example.com", "passwordABC");
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);
            const agent2 = await client2.agent.me();

            // Should get the same DID both times
            expect(agent1.did).to.equal(agent2.did);
            expect(agent1.did).to.equal(userResult.did);
        })

        it("should reject wrong passwords", async () => {
            // Create user
            await adminAd4mClient!.agent.createUser("eve@example.com", "correctpassword");
            
            // Try to login with wrong password
            const call = async () => {
                return await adminAd4mClient!.agent.loginUser("eve@example.com", "wrongpassword");
            };

            await expect(call()).to.be.rejectedWith(/Invalid credentials/);
        })

        it("should reject non-existent users", async () => {
            const call = async () => {
                return await adminAd4mClient!.agent.loginUser("nonexistent@example.com", "password");
            };

            await expect(call()).to.be.rejectedWith(/User not found/);
        })
    })
})

