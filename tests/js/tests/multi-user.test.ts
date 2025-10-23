import path from "path";
import { Ad4mClient, AuthInfoInput, CapabilityInput } from "@coasys/ad4m";
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

describe("Multi-User integration tests", () => {
    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "multi-user-agent");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 15500
    const hcAdminPort = 15501
    const hcAppPort = 15502

    let executorProcess: ChildProcess | null = null
    let adminAd4mClient: Ad4mClient | null = null

    before(async () => {
        if (!fs.existsSync(appDataPath)) {
            fs.mkdirSync(appDataPath, { recursive: true });
        }

        // Start executor with multi-user mode enabled
        executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
            gqlPort, hcAdminPort, hcAppPort, false, "admin123");

        adminAd4mClient = new Ad4mClient(apolloClient(gqlPort, "admin123"), false)
        
        // Generate initial admin agent
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

    describe("User Registration and Authentication", () => {
        it("should create a new user with username and password", async () => {
            const result = await adminAd4mClient!.agent.createUser("alice", "password123");
            expect(result).to.have.property('did');
            expect(result).to.have.property('success', true);
            expect(result.did).to.match(/^did:key:.+/);
        })

        it("should return existing user if already exists", async () => {
            // Create user first time
            const result1 = await adminAd4mClient!.agent.createUser("bob", "password456");
            expect(result1.success).to.be.true;
            
            // Try to create same user again
            const result2 = await adminAd4mClient!.agent.createUser("bob", "password456");
            expect(result2.success).to.be.true;
            expect(result2.did).to.equal(result1.did);
        })

        it("should fail to create user with wrong password for existing username", async () => {
            // Create user first
            await adminAd4mClient!.agent.createUser("charlie", "correctpassword");
            
            // Try with wrong password
            const result = await adminAd4mClient!.agent.createUser("charlie", "wrongpassword");
            expect(result.success).to.be.false;
            expect(result.error).to.include("Invalid credentials");
        })

        it("should generate capability token for specific user", async () => {
            // Create user
            const userResult = await adminAd4mClient!.agent.createUser("dave", "password789");
            expect(userResult.success).to.be.true;

            // Request capability for this user
            const requestId = await adminAd4mClient!.agent.requestCapabilityForUser("dave", {
                appName: "test-app",
                appDesc: "test-desc", 
                appDomain: "test.ad4m.org",
                appUrl: "https://test-link",
                capabilities: [
                    {
                        with: {
                            domain: "agent",
                            pointers: ["*"]
                        },
                        can: ["READ"]
                    }
                ] as CapabilityInput[]
            } as AuthInfoInput);

            expect(requestId).to.match(/.+/);

            // Permit capability
            const rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"test-app","appDesc":"test-desc","appUrl":"test-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["READ"]}]}}`);
            expect(rand).to.match(/\d+/);

            // Generate JWT for user
            const jwt = await adminAd4mClient!.agent.generateJwtForUser("dave", requestId, rand);
            expect(jwt).to.match(/.+/);

            // Verify JWT contains user DID
            const payload = JSON.parse(atob(jwt.split('.')[1]));
            expect(payload.sub).to.equal(userResult.did);
        })
    })

    describe("User-Scoped Perspectives", () => {
        let aliceClient: Ad4mClient;
        let bobClient: Ad4mClient;
        let aliceDid: string;
        let bobDid: string;

        before(async () => {
            // Create users and get their capability tokens
            const aliceResult = await adminAd4mClient!.agent.createUser("alice_persp", "password123");
            const bobResult = await adminAd4mClient!.agent.createUser("bob_persp", "password456");
            
            aliceDid = aliceResult.did;
            bobDid = bobResult.did;

            // Get capability tokens for both users
            const aliceRequestId = await adminAd4mClient!.agent.requestCapabilityForUser("alice_persp", {
                appName: "perspective-app",
                appDesc: "test perspectives",
                appDomain: "test.ad4m.org", 
                appUrl: "https://test-link",
                capabilities: [
                    {
                        with: { domain: "*", pointers: ["*"] },
                        can: ["*"]
                    }
                ]
            });

            const bobRequestId = await adminAd4mClient!.agent.requestCapabilityForUser("bob_persp", {
                appName: "perspective-app",
                appDesc: "test perspectives",
                appDomain: "test.ad4m.org",
                appUrl: "https://test-link", 
                capabilities: [
                    {
                        with: { domain: "*", pointers: ["*"] },
                        can: ["*"]
                    }
                ]
            });

            const aliceRand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${aliceRequestId}","auth":{"appName":"perspective-app","appDesc":"test perspectives","appUrl":"test-url","capabilities":[{"with":{"domain":"*","pointers":["*"]},"can":["*"]}]}}`);
            const bobRand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${bobRequestId}","auth":{"appName":"perspective-app","appDesc":"test perspectives","appUrl":"test-url","capabilities":[{"with":{"domain":"*","pointers":["*"]},"can":["*"]}]}}`);

            const aliceJwt = await adminAd4mClient!.agent.generateJwtForUser("alice_persp", aliceRequestId, aliceRand);
            const bobJwt = await adminAd4mClient!.agent.generateJwtForUser("bob_persp", bobRequestId, bobRand);

            aliceClient = new Ad4mClient(apolloClient(gqlPort, aliceJwt), false);
            bobClient = new Ad4mClient(apolloClient(gqlPort, bobJwt), false);
        })

        it("should create perspectives scoped to specific users", async () => {
            // Alice creates a perspective
            const alicePerspective = await aliceClient.perspective.add("Alice's Perspective");
            expect(alicePerspective.uuid).to.be.ok;

            // Bob creates a perspective  
            const bobPerspective = await bobClient.perspective.add("Bob's Perspective");
            expect(bobPerspective.uuid).to.be.ok;

            // Perspectives should have different UUIDs
            expect(alicePerspective.uuid).to.not.equal(bobPerspective.uuid);
        })

        it("should only show user's own perspectives", async () => {
            // Create perspectives for each user
            const alicePerspective1 = await aliceClient.perspective.add("Alice Perspective 1");
            const alicePerspective2 = await aliceClient.perspective.add("Alice Perspective 2");
            const bobPerspective1 = await bobClient.perspective.add("Bob Perspective 1");
            const bobPerspective2 = await bobClient.perspective.add("Bob Perspective 2");

            // Alice should only see her perspectives
            const alicePerspectives = await aliceClient.perspective.all();
            const aliceUuids = alicePerspectives.map(p => p.uuid);
            expect(aliceUuids).to.include(alicePerspective1.uuid);
            expect(aliceUuids).to.include(alicePerspective2.uuid);
            expect(aliceUuids).to.not.include(bobPerspective1.uuid);
            expect(aliceUuids).to.not.include(bobPerspective2.uuid);

            // Bob should only see his perspectives
            const bobPerspectives = await bobClient.perspective.all();
            const bobUuids = bobPerspectives.map(p => p.uuid);
            expect(bobUuids).to.include(bobPerspective1.uuid);
            expect(bobUuids).to.include(bobPerspective2.uuid);
            expect(bobUuids).to.not.include(alicePerspective1.uuid);
            expect(bobUuids).to.not.include(alicePerspective2.uuid);
        })

        it("should not allow access to other user's perspectives", async () => {
            // Alice creates a perspective
            const alicePerspective = await aliceClient.perspective.add("Alice Private Perspective");
            
            // Bob tries to access Alice's perspective
            const call = async () => {
                return await bobClient.perspective.byUUID(alicePerspective.uuid);
            };

            await expect(call()).to.be.rejectedWith(/not found|access denied|unauthorized/i);
        })

        it("should handle perspective updates with user scoping", async () => {
            // Alice creates and updates a perspective
            const perspective = await aliceClient.perspective.add("Alice Updatable Perspective");
            const updatedPerspective = await aliceClient.perspective.update(perspective.uuid, "Updated Name");
            
            expect(updatedPerspective.name).to.equal("Updated Name");

            // Bob should not be able to update Alice's perspective
            const call = async () => {
                return await bobClient.perspective.update(perspective.uuid, "Bob's Malicious Update");
            };

            await expect(call()).to.be.rejectedWith(/not found|access denied|unauthorized/i);
        })
    })

    describe("User Context in Agent Operations", () => {
        let userClient: Ad4mClient;
        let userDid: string;

        before(async () => {
            // Create user and get capability token
            const userResult = await adminAd4mClient!.agent.createUser("test_user_ops", "password123");
            userDid = userResult.did;

            const requestId = await adminAd4mClient!.agent.requestCapabilityForUser("test_user_ops", {
                appName: "user-ops-app",
                appDesc: "test user operations",
                appDomain: "test.ad4m.org",
                appUrl: "https://test-link",
                capabilities: [
                    {
                        with: { domain: "*", pointers: ["*"] },
                        can: ["*"]
                    }
                ]
            });

            const rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"user-ops-app","appDesc":"test user operations","appUrl":"test-url","capabilities":[{"with":{"domain":"*","pointers":["*"]},"can":["*"]}]}}`);
            const jwt = await adminAd4mClient!.agent.generateJwtForUser("test_user_ops", requestId, rand);

            userClient = new Ad4mClient(apolloClient(gqlPort, jwt), false);
        })

        it("should return correct agent status for user", async () => {
            const status = await userClient.agent.status();
            expect(status.did).to.equal(userDid);
            expect(status.isUnlocked).to.be.true;
        })

        it("should handle agent operations in user context", async () => {
            // This test will be expanded once we implement multi-user agent service
            // For now, just verify the user context is maintained
            const agent = await userClient.agent.me();
            expect(agent.did).to.equal(userDid);
        })
    })
})
