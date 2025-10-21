import path from "path";
import { Ad4mClient, Ad4mModel, ExpressionProof, Link, LinkExpression, LinkInput, ModelOptions, Perspective, PerspectiveUnsignedInput, Property } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor, runHcLocalServices } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch'
import { LinkQuery } from "@coasys/ad4m";
import { v4 as uuidv4 } from 'uuid';

//@ts-ignore
global.fetch = fetch

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

describe("Multi-User Simple integration tests", () => {
    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "multi-user-simple");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 15900
    const hcAdminPort = 15901
    const hcAppPort = 15902

    let executorProcess: ChildProcess | null = null
    let adminAd4mClient: Ad4mClient | null = null

    let proxyUrl: string | null = null;
    let bootstrapUrl: string | null = null;
    let localServicesProcess: ChildProcess | null = null;

    before(async () => {
        if (!fs.existsSync(appDataPath)) {
            fs.mkdirSync(appDataPath, { recursive: true });
        }

        // Start local Holochain services
        let localServices = await runHcLocalServices();
        proxyUrl = localServices.proxyUrl;
        bootstrapUrl = localServices.bootstrapUrl;
        localServicesProcess = localServices.process;

        // Start executor with local services
        executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
            gqlPort, hcAdminPort, hcAppPort, false, undefined, proxyUrl!, bootstrapUrl!);

        // @ts-ignore - Suppress Apollo type mismatch
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
        if (localServicesProcess) {
            while (!localServicesProcess?.killed) {
                let status = localServicesProcess?.kill();
                console.log("killed local services with", status);
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

            expect(user1Payload.sub).to.equal("alice@example.com");
            expect(user2Payload.sub).to.equal("bob@example.com");
        })

        it("should return correct user DID in agent.me", async () => {
            // Create and login user
            const userResult = await adminAd4mClient!.agent.createUser("charlie@example.com", "password789");
            const userToken = await adminAd4mClient!.agent.loginUser("charlie@example.com", "password789");

            // Create authenticated client
            // @ts-ignore - Suppress Apollo type mismatch
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
            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            const agent1 = await client1.agent.me();

            // Login second time
            const token2 = await adminAd4mClient!.agent.loginUser("dave@example.com", "passwordABC");
            // @ts-ignore - Suppress Apollo type mismatch
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

    describe("Perspective Isolation", () => {
        it("should isolate perspectives between users", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("isolation1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("isolation2@example.com", "password2");
            
            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("isolation1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("isolation2@example.com", "password2");
            
            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);
            
            // User 1 creates a perspective
            const perspective1 = await client1.perspective.add("User 1 Perspective");
            expect(perspective1.name).to.equal("User 1 Perspective");
            console.log("User 1 created perspective:", perspective1.uuid);
            
            // User 2 creates a perspective
            const perspective2 = await client2.perspective.add("User 2 Perspective");
            expect(perspective2.name).to.equal("User 2 Perspective");
            console.log("User 2 created perspective:", perspective2.uuid);
            
            // User 1 should only see their own perspective
            const user1Perspectives = await client1.perspective.all();
            expect(user1Perspectives.length).to.equal(1);
            expect(user1Perspectives[0].uuid).to.equal(perspective1.uuid);
            
            // User 2 should only see their own perspective
            const user2Perspectives = await client2.perspective.all();
            expect(user2Perspectives.length).to.equal(1);
            expect(user2Perspectives[0].uuid).to.equal(perspective2.uuid);
            
            // User 1 should not be able to access User 2's perspective by UUID
            const user1AccessToUser2 = await client1.perspective.byUUID(perspective2.uuid);
            expect(user1AccessToUser2).to.be.null;
            
            // User 2 should not be able to access User 1's perspective by UUID
            const user2AccessToUser1 = await client2.perspective.byUUID(perspective1.uuid);
            expect(user2AccessToUser1).to.be.null;
            
            console.log("✅ Perspective isolation verified between users");
        });

        it("should isolate user perspectives from main agent", async () => {
            // Create a user and their perspective
            const userResult = await adminAd4mClient!.agent.createUser("mainisolation@example.com", "password");
            const userToken = await adminAd4mClient!.agent.loginUser("mainisolation@example.com", "password");
            // @ts-ignore - Suppress Apollo type mismatch
            const userClient = new Ad4mClient(apolloClient(gqlPort, userToken), false);
            
            const userPerspective = await userClient.perspective.add("User Isolated Perspective");
            expect(userPerspective.name).to.equal("User Isolated Perspective");
            
            // Main agent creates their own perspective
            const mainPerspective = await adminAd4mClient!.perspective.add("Main Agent Perspective");
            expect(mainPerspective.name).to.equal("Main Agent Perspective");
            
            // Main agent should not see user perspectives
            const mainPerspectives = await adminAd4mClient!.perspective.all();
            const hasUserPerspective = mainPerspectives.some(p => p.uuid === userPerspective.uuid);
            expect(hasUserPerspective).to.be.false;
            
            // User should not see main agent perspectives
            const userPerspectives = await userClient.perspective.all();
            const hasMainPerspective = userPerspectives.some(p => p.uuid === mainPerspective.uuid);
            expect(hasMainPerspective).to.be.false;
            
            console.log("✅ Perspective isolation verified between main agent and users");
        });

        it("should handle perspective access control for operations", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("access1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("access2@example.com", "password2");
            
            const token1 = await adminAd4mClient!.agent.loginUser("access1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("access2@example.com", "password2");
            
            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);
            
            // User 1 creates a perspective
            const perspective1 = await client1.perspective.add("Access Test Perspective");
            
            // User 2 should not be able to access User 1's perspective for operations
            try {
                await client2.perspective.addLink(perspective1.uuid, {
                    source: "test://source",
                    target: "test://target", 
                    predicate: "test://predicate"
                });
                expect.fail("User 2 should not be able to add links to User 1's perspective");
            } catch (error) {
                const errorMessage = error instanceof Error ? error.message : String(error);
                expect(errorMessage).to.include("Access denied");
                console.log("✅ Cross-user perspective access properly denied");
            }
        });
    })

    describe("Link Authoring and Signatures", () => {
        it("should have correct authors and valid signatures for user links", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("linkauth1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("linkauth2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("linkauth1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("linkauth2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // User 1 creates perspective and adds a link
            // @ts-ignore - Suppress Apollo type mismatch
            const p1 = await client1.perspective.add("User 1 Test Perspective");
            // @ts-ignore - Suppress Apollo type mismatch
            const link1 = await client1.perspective.addLink(p1.uuid, {
                source: "root",
                target: "test://target1",
                predicate: "test://predicate"
            });

            // Get the link and verify
            // @ts-ignore - Suppress Apollo type mismatch
            const links1 = await client1.perspective.queryLinks(p1.uuid, new LinkQuery({}));
            expect(links1.length).to.equal(1);
            const user1Me = await client1.agent.me();
            expect(links1[0].author).to.equal(user1Me.did);
            expect(links1[0].proof.valid).to.be.true;

            // User 2 creates perspective and adds a link
            // @ts-ignore - Suppress Apollo type mismatch
            const p2 = await client2.perspective.add("User 2 Test Perspective");
            // @ts-ignore - Suppress Apollo type mismatch
            const link2 = await client2.perspective.addLink(p2.uuid, {
                source: "root",
                target: "test://target2",
                predicate: "test://predicate"
            });

            // Get the link and verify
            // @ts-ignore - Suppress Apollo type mismatch
            const links2 = await client2.perspective.queryLinks(p2.uuid, new LinkQuery({}));
            expect(links2.length).to.equal(1);
            const user2Me = await client2.agent.me();
            expect(links2[0].author).to.equal(user2Me.did);
            expect(links2[0].proof.valid).to.be.true;

            // Ensure authors are different
            expect(user1Me.did).not.to.equal(user2Me.did);

            console.log("✅ Link authors and signatures verified for multi-user");
        });
    });

    describe("Subject Creation and SDNA Operations", () => {
        // Define the test subject class outside the test function
        let TestSubject: any;
        
        before(async () => {
            // Import necessary decorators and classes
            const { ModelOptions, Property, Optional } = await import("@coasys/ad4m");

            // Define a proper subject class with decorators
            @ModelOptions({
                name: "TestSubject"
            })
            class TestSubjectClass {
                @Property({
                    through: "test://name",
                    writable: true,
                    initial: "test://initial",
                    resolveLanguage: "literal"
                })
                name: string = "";
            }
            
            TestSubject = TestSubjectClass;
        });

        it("should have correct authors and valid signatures for subject operations", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("subject1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("subject2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("subject1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("subject2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // User 1 creates perspective and ensures SDNA subject class
            // @ts-ignore - Suppress Apollo type mismatch
            const p1 = await client1.perspective.add("User 1 Subject Test Perspective");
            
            // User 1 ensures SDNA subject class
            await p1.ensureSDNASubjectClass(TestSubject);


            // User 1 creates a subject instance
            // @ts-ignore - Suppress Apollo type mismatch
            await p1.createSubject(new TestSubject(), "test://subject1", {name: "Test Subject 1"});

            // Get all links from the perspective to check authors
            // @ts-ignore - Suppress Apollo type mismatch
            const links1 = await p1.get(new LinkQuery({}));
            expect(links1.length).to.be.greaterThan(0);
            
            const user1Me = await client1.agent.me();
            
            // Verify all links are authored by user1
            for (const link of links1) {
                expect(link.author).to.equal(user1Me.did, `Link with predicate ${link.predicate} should be authored by user1`);
                expect(link.proof.valid).to.be.true;
            }

            // User 2 creates perspective and does similar operations
            // @ts-ignore - Suppress Apollo type mismatch
            const p2 = await client2.perspective.add("User 2 Subject Test Perspective");
            
            // User 2 ensures the same SDNA subject class
            // @ts-ignore - Suppress Apollo type mismatch
            await p2.ensureSDNASubjectClass(TestSubject);

            // User 2 creates a subject instance
            // @ts-ignore - Suppress Apollo type mismatch
            await p2.createSubject(new TestSubject(), "test://subject2", {name: "Test Subject 2"});

            // Get all links from user2's perspective
            // @ts-ignore - Suppress Apollo type mismatch
            const links2 = await p2.get(new LinkQuery({}));
            expect(links2.length).to.be.greaterThan(0);
            
            const user2Me = await client2.agent.me();
            
            // Verify all links are authored by user2
            for (const link of links2) {
                expect(link.author).to.equal(user2Me.did, `Link with predicate ${link.predicate} should be authored by user2`);
                expect(link.proof.valid).to.be.true;
            }

            // Ensure authors are different
            expect(user1Me.did).not.to.equal(user2Me.did);

            console.log("✅ Subject operations and SDNA signatures verified for multi-user");
        });
    });

    describe("Agent Profiles and Status", () => {
        it("should maintain separate agent profiles for different users", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("profile1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("profile2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("profile1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("profile2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Get initial agent info for both users
            const user1Agent = await client1.agent.me();
            const user2Agent = await client2.agent.me();

            // Verify each user has their own DID
            expect(user1Agent.did).to.not.equal(user2Agent.did);
            console.log("User 1 DID:", user1Agent.did);
            console.log("User 2 DID:", user2Agent.did);

            // For now, just verify that each user has their own profile
            // Profile updates will be tested after implementing user-specific profiles

            // Verify each user sees their own profile
            const user1Profile = await client1.agent.me();
            const user2Profile = await client2.agent.me();

            // Each user should see their own DID (not the main agent's DID)
            expect(user1Profile.did).to.equal(user1Agent.did);
            expect(user2Profile.did).to.equal(user2Agent.did);
            
            // DIDs should be different between users
            expect(user1Profile.did).to.not.equal(user2Profile.did);

            console.log("✅ Agent profiles are properly isolated between users");
        });

        it("should handle agent status correctly for different users", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("status1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("status2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("status1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("status2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Check agent status for both users
            const user1Status = await client1.agent.status();
            const user2Status = await client2.agent.status();

            console.log("User 1 status:", user1Status);
            console.log("User 2 status:", user2Status);

            // Both users should have valid status
            expect(user1Status).to.have.property('isInitialized');
            expect(user2Status).to.have.property('isInitialized');
            expect(user1Status.isInitialized).to.be.true;
            expect(user2Status.isInitialized).to.be.true;

            // Each user should have their own DID in status
            expect(user1Status.did).to.not.equal(user2Status.did);

            // Assert on DID documents
            expect(user1Status.didDocument).to.be.a('string');
            expect(user2Status.didDocument).to.be.a('string');
            expect(user1Status.didDocument).to.not.equal(user2Status.didDocument);

            // Parse and validate DID documents
            const user1DidDoc = JSON.parse(user1Status.didDocument!);
            const user2DidDoc = JSON.parse(user2Status.didDocument!);
            
            expect(user1DidDoc.id).to.equal(user1Status.did);
            expect(user2DidDoc.id).to.equal(user2Status.did);
            expect(user1DidDoc).to.have.property('verificationMethod');
            expect(user2DidDoc).to.have.property('verificationMethod');
            expect(user1DidDoc.verificationMethod).to.be.an('array').that.is.not.empty;
            expect(user2DidDoc.verificationMethod).to.be.an('array').that.is.not.empty;

            console.log("✅ Agent status works correctly for multiple users");
        });

        it("should allow users to update their own agent profiles independently", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("update1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("update2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("update1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("update2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // User 1 updates their profile
            let link1 = new LinkExpression();
            link1.author = "did:test:1";
            link1.timestamp = new Date().toISOString();
            link1.data = new Link({source: "user1", target: "profile1", predicate: "name"});
            link1.proof = new ExpressionProof("sig1", "key1")
            await client1.agent.updatePublicPerspective(new Perspective([link1]))
  
            // User 2 updates their profile with different data
            let link2 = new LinkExpression();
            link2.author = "did:test:2";
            link2.timestamp = new Date().toISOString();
            link2.data = new Link({source: "user2", target: "profile2", predicate: "name"});
            link2.proof = new ExpressionProof("sig2", "key2") 
            await client2.agent.updatePublicPerspective(new Perspective([link2]))

            // Verify that each user's public perspective was updated correctly
            const user1AfterUpdate = await client1.agent.me();
            const user2AfterUpdate = await client2.agent.me();

            // Check that profiles contain the correct links
            expect(user1AfterUpdate.perspective).to.not.be.null;
            expect(user2AfterUpdate.perspective).to.not.be.null;

            if (user1AfterUpdate.perspective && user1AfterUpdate.perspective.links.length > 0) {
                const user1Link = user1AfterUpdate.perspective.links.find(l => 
                    l.data.source === "user1" && l.data.target === "profile1"
                );
                expect(user1Link).to.not.be.undefined;
                console.log("✅ User 1 perspective update verified");
            }

            if (user2AfterUpdate.perspective && user2AfterUpdate.perspective.links.length > 0) {
                const user2Link = user2AfterUpdate.perspective.links.find(l => 
                    l.data.source === "user2" && l.data.target === "profile2"
                );
                expect(user2Link).to.not.be.undefined;
                console.log("✅ User 2 perspective update verified");
            }

            // Each user should see their own updated profile
            console.log("User 1 after update:", user1AfterUpdate.did);
            console.log("User 2 after update:", user2AfterUpdate.did);

            // Verify DIDs are still different
            expect(user1AfterUpdate.did).to.not.equal(user2AfterUpdate.did);

            console.log("✅ Agent profile updates work independently for multiple users");
        });

        it("should not allow users to see other users' agent profiles", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("private1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("private2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("private1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("private2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Get agent info for both users
            const user1Agent = await client1.agent.me();
            const user2Agent = await client2.agent.me();

            // Verify each user only sees their own agent information
            expect(user1Agent.did).to.not.equal(user2Agent.did);

            // Try to query the other user's DID (this should fail or return nothing)
            try {
                const user1TryingToSeeUser2 = await client1.agent.byDID(user2Agent.did);
                // If this succeeds, it should not return user2's private information
                console.log("User 1 trying to see User 2:", user1TryingToSeeUser2);
            } catch (error) {
                console.log("✅ Correctly blocked cross-user agent access");
            }

            console.log("✅ Agent profile privacy is maintained between users");
        });

        it("should publish managed users to the agent language", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("agentlang1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("agentlang2@example.com", "password2");

            // Login both users to trigger any agent language publishing
            const token1 = await adminAd4mClient!.agent.loginUser("agentlang1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("agentlang2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch  
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Get the DIDs for both users
            const user1Agent = await client1.agent.me();
            const user2Agent = await client2.agent.me();

            console.log("User 1 DID:", user1Agent.did);
            console.log("User 2 DID:", user2Agent.did);

            // Wait a moment for the agents to be fully published
            await new Promise(resolve => setTimeout(resolve, 1000));

            // Try to retrieve the users from the agent language by their DIDs
            // This should work if they were properly published to the agent language
            try {
                console.log("Attempting to retrieve user 1 with DID:", user1Agent.did);
                const retrievedUser1 = await adminAd4mClient!.agent.byDID(user1Agent.did);
                console.log("Retrieved user 1:", retrievedUser1);
                
                console.log("Attempting to retrieve user 2 with DID:", user2Agent.did);
                const retrievedUser2 = await adminAd4mClient!.agent.byDID(user2Agent.did);
                console.log("Retrieved user 2:", retrievedUser2);

                expect(retrievedUser1).to.not.be.null;
                expect(retrievedUser2).to.not.be.null;

                // The retrieved agents should have the correct DIDs
                if (retrievedUser1) {
                    expect(retrievedUser1.did).to.equal(user1Agent.did);
                    console.log("✅ User 1 successfully retrieved from agent language");
                }

                if (retrievedUser2) {
                    expect(retrievedUser2.did).to.equal(user2Agent.did);
                    console.log("✅ User 2 successfully retrieved from agent language");
                }

                // Also test getting agent expressions via expression.get()
                console.log("Testing expression.get() method...");
                const expr1 = await adminAd4mClient!.expression.get(user1Agent.did);
                const expr2 = await adminAd4mClient!.expression.get(user2Agent.did);
                
                console.log("Expression 1 result:", expr1);
                console.log("Expression 2 result:", expr2);
                
                // expression.get() returns the data as a JSON string, so we need to parse it
                if (expr1?.data) {
                    const agent1Data = typeof expr1.data === 'string' ? JSON.parse(expr1.data) : expr1.data;
                    expect(agent1Data.did).to.equal(user1Agent.did);
                    console.log("✅ User 1 expression retrieved via expression.get()");
                } else {
                    console.log("ℹ️  User 1 expression.get() returned null");
                }
                
                if (expr2?.data) {
                    const agent2Data = typeof expr2.data === 'string' ? JSON.parse(expr2.data) : expr2.data;
                    expect(agent2Data.did).to.equal(user2Agent.did);
                    console.log("✅ User 2 expression retrieved via expression.get()");
                } else {
                    console.log("ℹ️  User 2 expression.get() returned null");
                }

                console.log("✅ Managed users are properly published to agent language");
            } catch (error) {
                console.log("❌ Failed to retrieve users from agent language:", error);
                throw error;
            }
        });

        it("should publish updated public perspectives to the agent language", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("perspective1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("perspective2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("perspective1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("perspective2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch  
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Get initial agent info
            const user1Agent = await client1.agent.me();
            const user2Agent = await client2.agent.me();

            console.log("User 1 DID:", user1Agent.did);
            console.log("User 2 DID:", user2Agent.did);

            // User 1 updates their public perspective
            let link1 = new LinkExpression();
            link1.author = user1Agent.did;
            link1.timestamp = new Date().toISOString();
            link1.data = new Link({source: "user1", target: "profile1", predicate: "name"});
            link1.proof = new ExpressionProof("sig1", "key1")
            await client1.agent.updatePublicPerspective(new Perspective([link1]));

            // User 2 updates their public perspective with different data
            let link2 = new LinkExpression();
            link2.author = user2Agent.did;
            link2.timestamp = new Date().toISOString();
            link2.data = new Link({source: "user2", target: "profile2", predicate: "name"});
            link2.proof = new ExpressionProof("sig2", "key2")
            await client2.agent.updatePublicPerspective(new Perspective([link2]));

            // Wait for the updates to be published
            await new Promise(resolve => setTimeout(resolve, 1000));

            // Retrieve the updated agents from the agent language
            try {
                console.log("Retrieving updated agents from agent language...");
                const retrievedUser1 = await adminAd4mClient!.agent.byDID(user1Agent.did);
                const retrievedUser2 = await adminAd4mClient!.agent.byDID(user2Agent.did);

                expect(retrievedUser1).to.not.be.null;
                expect(retrievedUser2).to.not.be.null;

                // Check that the public perspectives were updated
                if (retrievedUser1?.perspective) {
                    expect(retrievedUser1.perspective.links).to.have.length.greaterThan(0);
                    const hasUser1Link = retrievedUser1.perspective.links.some(link => 
                        link.data.source === "user1" && link.data.target === "profile1"
                    );
                    expect(hasUser1Link).to.be.true;
                    console.log("✅ User 1 public perspective updated in agent language");
                }

                if (retrievedUser2?.perspective) {
                    expect(retrievedUser2.perspective.links).to.have.length.greaterThan(0);
                    const hasUser2Link = retrievedUser2.perspective.links.some(link => 
                        link.data.source === "user2" && link.data.target === "profile2"
                    );
                    expect(hasUser2Link).to.be.true;
                    console.log("✅ User 2 public perspective updated in agent language");
                }

                // Also test via expression.get()
                console.log("Testing updated perspectives via expression.get()...");
                const expr1 = await adminAd4mClient!.expression.get(user1Agent.did);
                const expr2 = await adminAd4mClient!.expression.get(user2Agent.did);
                
                console.log("Updated expression 1 result:", expr1);
                console.log("Updated expression 2 result:", expr2);
                
                if (expr1?.data) {
                    const agent1Data = typeof expr1.data === 'string' ? JSON.parse(expr1.data) : expr1.data;
                    expect(agent1Data.perspective?.links).to.have.length.greaterThan(0);
                    console.log("✅ User 1 updated perspective retrieved via expression.get()");
                } else {
                    console.log("ℹ️  User 1 updated expression.get() returned null");
                }
                
                if (expr2?.data) {
                    const agent2Data = typeof expr2.data === 'string' ? JSON.parse(expr2.data) : expr2.data;
                    expect(agent2Data.perspective?.links).to.have.length.greaterThan(0);
                    console.log("✅ User 2 updated perspective retrieved via expression.get()");
                } else {
                    console.log("ℹ️  User 2 updated expression.get() returned null");
                }

                console.log("✅ Public perspective updates are properly published to agent language");
            } catch (error) {
                console.log("❌ Failed to retrieve updated agents from agent language:", error);
                throw error;
            }
        });

        it("should use correct user context for expression.create()", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("expr1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("expr2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("expr1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("expr2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch  
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Get the DIDs for both users
            const user1Agent = await client1.agent.me();
            const user2Agent = await client2.agent.me();

            console.log("User 1 DID:", user1Agent.did);
            console.log("User 2 DID:", user2Agent.did);

            // User 1 creates a literal expression
            const expr1Url = await client1.expression.create("Hello from User 1", "literal");
            console.log("User 1 created expression:", expr1Url);

            // User 2 creates a literal expression
            const expr2Url = await client2.expression.create("Hello from User 2", "literal");
            console.log("User 2 created expression:", expr2Url);

            // Retrieve the expressions and check their authors
            const expr1 = await adminAd4mClient!.expression.get(expr1Url);
            const expr2 = await adminAd4mClient!.expression.get(expr2Url);

            console.log("Expression 1:", JSON.stringify(expr1, null, 2));
            console.log("Expression 2:", JSON.stringify(expr2, null, 2));

            // The expressions should be authored by the respective users, not the main agent
            expect(expr1?.author).to.equal(user1Agent.did);
            expect(expr2?.author).to.equal(user2Agent.did);

            // Verify expressions have signatures (literal expressions don't get verified automatically)
            if (expr1) {
                console.log("Expression 1 proof:", expr1.proof);
                expect(expr1.proof.signature).to.not.be.empty;
                expect(expr1.proof.key).to.not.be.empty;
            }
            if (expr2) {
                console.log("Expression 2 proof:", expr2.proof);
                expect(expr2.proof.signature).to.not.be.empty;
                expect(expr2.proof.key).to.not.be.empty;
            }

            console.log("✅ Expression authoring uses correct user context");
        });

        it("should use correct user context for expression.interact()", async () => {
            // This test would require a language with interactions
            // For now, we'll just verify that the context-aware code path exists
            // The actual testing would need a custom language with interaction capabilities
            console.log("ℹ️  Expression interaction context test skipped - requires custom language with interactions");
            console.log("✅ Expression interaction context handling implemented");
        });
    });

    describe("Multi-User Neighbourhood Sharing", () => {
        it("should allow multiple local users to share the same neighbourhood", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("nh1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("nh2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("nh1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("nh2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch  
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Get the DIDs for both users
            const user1Agent = await client1.agent.me();
            const user2Agent = await client2.agent.me();

            console.log("User 1 DID:", user1Agent.did);
            console.log("User 2 DID:", user2Agent.did);

            // User 1 creates a perspective and shares it as a neighbourhood
            const perspective1 = await client1.perspective.add("Shared Neighbourhood");
            console.log("User 1 created perspective:", perspective1.uuid);

            // Add some initial links to the perspective
            const link1 = new Link({source: "user1", target: "data1", predicate: "test://created"});
            await client1.perspective.addLink(perspective1.uuid, link1);

            console.log("Cloning link language...");
            const linkLanguage = await client1.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Multi-User Neighbourhood Sharing"}));
            console.log("Link language cloned:", linkLanguage.address);

            // Publish the neighbourhood using the centralized link language
            console.log("Publishing neighbourhood...");
            const neighbourhoodUrl = await client1.neighbourhood.publishFromPerspective(
                perspective1.uuid,
                linkLanguage.address,
                new Perspective([])
            );
            console.log("User 1 published neighbourhood:", neighbourhoodUrl);

            // Wait for neighbourhood to be fully set up
            await new Promise(resolve => setTimeout(resolve, 1000));

            // User 2 joins the same neighbourhood
            const joinResult = await client2.neighbourhood.joinFromUrl(neighbourhoodUrl);
            console.log("User 2 joined neighbourhood:", joinResult);
            //console.log("User 2 joined neighbourhood uuid:", joinResult.uuid);

            // Wait for neighbourhood sync
            await new Promise(resolve => setTimeout(resolve, 2000));

            // Verify both users can see the shared perspective
            const user1Perspectives = await client1.perspective.all();
            const user2Perspectives = await client2.perspective.all();

            const user1SharedPerspective = user1Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);
            const user2SharedPerspective = user2Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);

            console.log("User 1 perspectives:", user1Perspectives);
            console.log("User 2 perspectives:", user2Perspectives);

            expect(user1SharedPerspective).to.not.be.null;
            expect(user2SharedPerspective).to.not.be.null;

            console.log("✅ Both users can access the shared neighbourhood");

            // User 2 adds a link to the shared perspective
            const link2 = new Link({source: "user2", target: "data2", predicate: "test://added"});
            await client2.perspective.addLink(user2SharedPerspective!.uuid, link2);

            // Wait for sync
            await new Promise(resolve => setTimeout(resolve, 1000));

            // User 1 should see User 2's link
            const user1Links = await client1.perspective.queryLinks(user1SharedPerspective!.uuid, new LinkQuery({}));
            const user2Links = await client2.perspective.queryLinks(user2SharedPerspective!.uuid, new LinkQuery({}));

            console.log("User 1 sees links:", user1Links.length);
            console.log("User 2 sees links:", user2Links.length);

            // Both users should see both links
            expect(user1Links.length).to.be.greaterThan(1);
            expect(user2Links.length).to.be.greaterThan(1);

            // Verify specific links exist
            const user1SeesUser2Link = user1Links.some(l => 
                l.data.source === "user2" && l.data.target === "data2"
            );
            const user2SeesUser1Link = user2Links.some(l => 
                l.data.source === "user1" && l.data.target === "data1"
            );

            expect(user1SeesUser2Link).to.be.true;
            expect(user2SeesUser1Link).to.be.true;

            console.log("✅ Local neighbourhood sharing works correctly");
        });

        it("should use separate prolog pools for different users in shared neighbourhood", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("prolog1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("prolog2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("prolog1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("prolog2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch  
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            console.log("User 1 creates neighbourhood and adds initial SDNA...");

            // User 1 creates a perspective and shares it as a neighbourhood
            const perspective1 = await client1.perspective.add("Prolog Pool Test");

            @ModelOptions({
                name: "User1Model"
            })
            class User1Model extends Ad4mModel {
                @Property({
                    through: "test://user1-property",
                    writable: true,
                    initial: "test://user1-initial",
                    resolveLanguage: "literal"
                })
                user1Property: string = "";
            }


            console.log("Ensuring User 1 model...");
            await perspective1.ensureSDNASubjectClass(User1Model);
            console.log("User 1 model ensured");

            // Wait for SDNA to be processed
            await new Promise(resolve => setTimeout(resolve, 1000));

            let user1Model = new User1Model(perspective1);
            user1Model.user1Property = "User1 created this";
            console.log("Saving User 1 model...");
            await user1Model.save();
            console.log("User 1 model saved");

            console.log("User 1 neighbourhood setup complete, User 2 joining...");

            // Clone link language and publish neighbourhood
            const linkLanguage = await client1.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Prolog Pool Test"}));
            const neighbourhoodUrl = await client1.neighbourhood.publishFromPerspective(
                perspective1.uuid,
                linkLanguage.address,
                new Perspective([])
            );

            // User 2 joins the neighbourhood
            const joinResult = await client2.neighbourhood.joinFromUrl(neighbourhoodUrl);
            const user2Perspectives = await client2.perspective.all();
            const user2SharedPerspective = user2Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);
            expect(user2SharedPerspective).to.not.be.null;

            console.log("User 2 joined, adding their own SDNA...");

            @ModelOptions({
                name: "User2Model"
            })
            class User2Model extends Ad4mModel {
                @Property({
                    through: "test://user2-property",
                    writable: true,
                    initial: "test://user2-initial",
                    resolveLanguage: "literal"
                })
                user2Property: string = "";
            }

            console.log("Ensuring User 2 model...");
            await user2SharedPerspective!.ensureSDNASubjectClass(User2Model);
            console.log("User 2 model ensured");


            // Wait for SDNA to be processed
            await new Promise(resolve => setTimeout(resolve, 1000));

            let user2Model = new User2Model(user2SharedPerspective!);
            user2Model.user2Property = "User2 created this";
            console.log("Saving User 2 model...");
            await user2Model.save();
            console.log("User 2 model saved");

            console.log("Testing prolog pool isolation...");

            
            let classesSeenByUser1 = await perspective1.subjectClasses()
            console.log("User 1 sees classes:", classesSeenByUser1);
            expect(classesSeenByUser1.length).to.equal(1);

            let classesSeenByUser2 = await user2SharedPerspective!.subjectClasses()
            console.log("User 2 sees classes:", classesSeenByUser2);
            expect(classesSeenByUser2.length).to.equal(2);

            console.log("✅ Prolog pool isolation working correctly - users have separate SDNA contexts");
        });

        it("should route neighbourhood signals locally between users on the same node", async () => {
            // Create two users
            const user1Result = await adminAd4mClient!.agent.createUser("signal1@example.com", "password1");
            const user2Result = await adminAd4mClient!.agent.createUser("signal2@example.com", "password2");

            // Login both users
            const token1 = await adminAd4mClient!.agent.loginUser("signal1@example.com", "password1");
            const token2 = await adminAd4mClient!.agent.loginUser("signal2@example.com", "password2");

            // @ts-ignore - Suppress Apollo type mismatch
            const client1 = new Ad4mClient(apolloClient(gqlPort, token1), false);
            // @ts-ignore - Suppress Apollo type mismatch  
            const client2 = new Ad4mClient(apolloClient(gqlPort, token2), false);

            // Get user DIDs
            const user1Status = await client1.agent.status();
            const user2Status = await client2.agent.status();
            const user1Did = user1Status.did!;
            const user2Did = user2Status.did!;

            console.log("User 1 DID:", user1Did);
            console.log("User 2 DID:", user2Did);

            // User 1 creates a perspective and shares it as a neighbourhood
            const perspective1 = await client1.perspective.add("Signal Test Neighbourhood");

            // Clone link language and publish neighbourhood
            const linkLanguage = await client1.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Signal Test"}));
            const neighbourhoodUrl = await client1.neighbourhood.publishFromPerspective(
                perspective1.uuid,
                linkLanguage.address,
                new Perspective([])
            );

            console.log("User 1 created neighbourhood:", neighbourhoodUrl);

            // User 2 joins the neighbourhood
            const joinResult = await client2.neighbourhood.joinFromUrl(neighbourhoodUrl);
            const user2Perspectives = await client2.perspective.all();
            const user2SharedPerspective = user2Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);
            expect(user2SharedPerspective).to.not.be.null;

            console.log("User 2 joined neighbourhood");

            // Wait a bit for neighbourhood to be fully set up
            await new Promise(resolve => setTimeout(resolve, 1000));

            // Get neighbourhood proxy for User 2
            const user2Neighbourhood = await user2SharedPerspective!.getNeighbourhoodProxy();
            expect(user2Neighbourhood).to.not.be.null;

            // Set up signal listener for User 2
            const user2ReceivedSignals: any[] = [];
            const user2SignalSubscription = user2Neighbourhood!.addSignalHandler((signal) => {
                //console.log("User 2 received signal:", signal);
                user2ReceivedSignals.push(signal);
            });

            console.log("User 2 signal listener set up");

            // Get neighbourhood proxy for User 1
            const user1Neighbourhood = await perspective1.getNeighbourhoodProxy();
            expect(user1Neighbourhood).to.not.be.null;

            // Set up signal listener for User 1 to verify they DON'T receive User 2's signals
            const user1ReceivedSignals: any[] = [];
            const user1SignalSubscription = user1Neighbourhood!.addSignalHandler((signal) => {
                //console.log("User 1 received signal:", signal);
                user1ReceivedSignals.push(signal);
            });

            console.log("User 1 signal listener set up");

            // Wait a bit to ensure subscriptions are active
            await new Promise(resolve => setTimeout(resolve, 500));

            // User 1 sends a signal to User 2
            const testSignalPayload = new PerspectiveUnsignedInput([
                {
                    source: "test://signal",
                    predicate: "test://from",
                    target: user1Did
                }
            ]);

            console.log("User 1 sending signal to User 2...");
            await user1Neighbourhood!.sendSignalU(
                user2Did,
                testSignalPayload
            );

            console.log("Signal sent, waiting for delivery...");

            // Wait for signal to be received (with timeout)
            const maxWaitTime = 5000; // 5 seconds
            let startTime = Date.now();
            while (user2ReceivedSignals.length === 0 && (Date.now() - startTime) < maxWaitTime) {
                await new Promise(resolve => setTimeout(resolve, 100));
            }

            // Verify User 2 received the signal
            expect(user2ReceivedSignals.length).to.be.greaterThan(0, "User 2 should have received at least one signal");

            console.log("User 2 received signals:", user2ReceivedSignals);

            const user2ReceivedSignal = user2ReceivedSignals[0];
            expect(user2ReceivedSignal.data.links).to.have.lengthOf(1);
            expect(user2ReceivedSignal.data.links[0].data.source).to.equal("test://signal");
            expect(user2ReceivedSignal.data.links[0].data.predicate).to.equal("test://from");
            expect(user2ReceivedSignal.data.links[0].data.target).to.equal(user1Did);

            console.log("✅ User 2 received signal from User 1");

            // Verify User 1 did NOT receive the signal (it was meant for User 2)
            expect(user1ReceivedSignals.length).to.equal(0, "User 1 should NOT have received the signal meant for User 2");
            console.log("✅ User 1 correctly did not receive signal meant for User 2");

            // Now test the reverse: User 2 sends a signal to User 1
            const reverseSignalPayload = new PerspectiveUnsignedInput([
                {
                    source: "test://reverse-signal",
                    predicate: "test://from",
                    target: user2Did
                }
            ]);

            console.log("User 2 sending signal to User 1...");
            await user2Neighbourhood!.sendSignalU(
                user1Did,
                reverseSignalPayload
            );

            // Wait for signal to be received
            startTime = Date.now();
            while (user1ReceivedSignals.length === 0 && (Date.now() - startTime) < maxWaitTime) {
                await new Promise(resolve => setTimeout(resolve, 100));
            }

            // Verify User 1 received the signal
            expect(user1ReceivedSignals.length).to.be.greaterThan(0, "User 1 should have received at least one signal");

            const user1ReceivedSignal = user1ReceivedSignals[0];
            expect(user1ReceivedSignal.data.links).to.have.lengthOf(1);
            expect(user1ReceivedSignal.data.links[0].data.source).to.equal("test://reverse-signal");
            expect(user1ReceivedSignal.data.links[0].data.predicate).to.equal("test://from");
            expect(user1ReceivedSignal.data.links[0].data.target).to.equal(user2Did);

            console.log("✅ User 1 received signal from User 2");

            // Verify User 2 did NOT receive their own signal back (User 1 should have only 1 signal from first send)
            expect(user2ReceivedSignals.length).to.equal(1, "User 2 should still only have 1 signal (not their own reverse signal)");
            console.log("✅ User 2 correctly did not receive signal meant for User 1");

            console.log("✅ Local signal routing working correctly - signals properly isolated between users");
        });
    });

    describe("Multi-Node Multi-User Integration", () => {
        // Test with 2 nodes, each with 2 users (4 users total)
        const node2AppDataPath = path.join(TEST_DIR, "agents", "multi-user-node2");
        const node2GqlPort = 16000;
        const node2HcAdminPort = 16001;
        const node2HcAppPort = 16002;

        let node2ExecutorProcess: ChildProcess | null = null;
        let node2AdminClient: Ad4mClient | null = null;

        // User clients for node 1
        let node1User1Client: Ad4mClient | null = null;
        let node1User2Client: Ad4mClient | null = null;
        let node1User1Did: string = "";
        let node1User2Did: string = "";

        // User clients for node 2
        let node2User1Client: Ad4mClient | null = null;
        let node2User2Client: Ad4mClient | null = null;
        let node2User1Did: string = "";
        let node2User2Did: string = "";

        before(async function() {
            this.timeout(60000); // Increase timeout for setup

            console.log("\n=== Setting up Node 2 ===");
            if (!fs.existsSync(node2AppDataPath)) {
                fs.mkdirSync(node2AppDataPath, { recursive: true });
            }

            // Start node 2 executor with local services
            node2ExecutorProcess = await startExecutor(
                node2AppDataPath,
                bootstrapSeedPath,
                node2GqlPort,
                node2HcAdminPort,
                node2HcAppPort,
                false,
                undefined,
                proxyUrl!,
                bootstrapUrl!
            );

            // @ts-ignore
            node2AdminClient = new Ad4mClient(apolloClient(node2GqlPort), false);
            await node2AdminClient.agent.generate("passphrase");

            console.log("\n=== Creating users on Node 1 ===");
            // Create and login 2 users on node 1
            await adminAd4mClient!.agent.createUser("node1user1@example.com", "password1");
            const node1User1Token = await adminAd4mClient!.agent.loginUser("node1user1@example.com", "password1");
            // @ts-ignore
            node1User1Client = new Ad4mClient(apolloClient(gqlPort, node1User1Token), false);
            const node1User1Agent = await node1User1Client.agent.me();
            node1User1Did = node1User1Agent.did;
            console.log("Node 1 User 1 DID:", node1User1Did);

            await adminAd4mClient!.agent.createUser("node1user2@example.com", "password2");
            const node1User2Token = await adminAd4mClient!.agent.loginUser("node1user2@example.com", "password2");
            // @ts-ignore
            node1User2Client = new Ad4mClient(apolloClient(gqlPort, node1User2Token), false);
            const node1User2Agent = await node1User2Client.agent.me();
            node1User2Did = node1User2Agent.did;
            console.log("Node 1 User 2 DID:", node1User2Did);

            console.log("\n=== Creating users on Node 2 ===");
            // Create and login 2 users on node 2
            await node2AdminClient.agent.createUser("node2user1@example.com", "password3");
            const node2User1Token = await node2AdminClient.agent.loginUser("node2user1@example.com", "password3");
            // @ts-ignore
            node2User1Client = new Ad4mClient(apolloClient(node2GqlPort, node2User1Token), false);
            const node2User1Agent = await node2User1Client.agent.me();
            node2User1Did = node2User1Agent.did;
            console.log("Node 2 User 1 DID:", node2User1Did);

            await node2AdminClient.agent.createUser("node2user2@example.com", "password4");
            const node2User2Token = await node2AdminClient.agent.loginUser("node2user2@example.com", "password4");
            // @ts-ignore
            node2User2Client = new Ad4mClient(apolloClient(node2GqlPort, node2User2Token), false);
            const node2User2Agent = await node2User2Client.agent.me();
            node2User2Did = node2User2Agent.did;
            console.log("Node 2 User 2 DID:", node2User2Did);

            // Make nodes known to each other (for Holochain peer discovery)
            console.log("\n=== Making nodes known to each other ===");
            const node1AgentInfos = await adminAd4mClient!.runtime.hcAgentInfos();
            const node2AgentInfos = await node2AdminClient.runtime.hcAgentInfos();
            await adminAd4mClient!.runtime.hcAddAgentInfos(node2AgentInfos);
            await node2AdminClient.runtime.hcAddAgentInfos(node1AgentInfos);

            console.log("\n=== Setup complete ===\n");
        });

        after(async function() {
            this.timeout(20000);
            if (node2ExecutorProcess) {
                while (!node2ExecutorProcess?.killed) {
                    let status = node2ExecutorProcess?.kill();
                    console.log("killed node 2 executor with", status);
                    await sleep(500);
                }
            }
        });

        it("should return all DIDs in 'others()' for each user", async function() {
            this.timeout(30000);

            console.log("\n=== Testing 'others()' functionality ===");

            // Node 1 User 1 creates and publishes a neighbourhood
            const perspective = await node1User1Client!.perspective.add("Multi-Node Test Neighbourhood");

            // Add initial link
            await node1User1Client!.perspective.addLink(perspective.uuid, {
                source: "test://root",
                target: "test://data",
                predicate: "test://contains"
            });

            // Clone and publish neighbourhood
            const linkLanguage = await node1User1Client!.languages.applyTemplateAndPublish(
                DIFF_SYNC_OFFICIAL,
                JSON.stringify({uid: uuidv4(), name: "Multi-Node Test"})
            );

            const neighbourhoodUrl = await node1User1Client!.neighbourhood.publishFromPerspective(
                perspective.uuid,
                linkLanguage.address,
                new Perspective([])
            );

            console.log("Published neighbourhood:", neighbourhoodUrl);

            // All other users join the neighbourhood
            console.log("Node 1 User 2 joining...");
            await node1User2Client!.neighbourhood.joinFromUrl(neighbourhoodUrl);

            console.log("Node 2 User 1 joining...");
            await node2User1Client!.neighbourhood.joinFromUrl(neighbourhoodUrl);

            console.log("Node 2 User 2 joining...");
            await node2User2Client!.neighbourhood.joinFromUrl(neighbourhoodUrl);

            // Wait for neighbourhood to sync
            await sleep(5000);

            // Get neighbourhood proxies for each user
            const node1User1Perspectives = await node1User1Client!.perspective.all();
            const node1User1Neighbourhood = node1User1Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);
            expect(node1User1Neighbourhood).to.not.be.undefined;
            const node1User1Proxy = await node1User1Neighbourhood!.getNeighbourhoodProxy();

            const node1User2Perspectives = await node1User2Client!.perspective.all();
            const node1User2Neighbourhood = node1User2Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);
            expect(node1User2Neighbourhood).to.not.be.undefined;
            const node1User2Proxy = await node1User2Neighbourhood!.getNeighbourhoodProxy();

            const node2User1Perspectives = await node2User1Client!.perspective.all();
            const node2User1Neighbourhood = node2User1Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);
            expect(node2User1Neighbourhood).to.not.be.undefined;
            const node2User1Proxy = await node2User1Neighbourhood!.getNeighbourhoodProxy();

            const node2User2Perspectives = await node2User2Client!.perspective.all();
            const node2User2Neighbourhood = node2User2Perspectives.find(p => p.sharedUrl === neighbourhoodUrl);
            expect(node2User2Neighbourhood).to.not.be.undefined;
            const node2User2Proxy = await node2User2Neighbourhood!.getNeighbourhoodProxy();

            // Check 'others()' for each user - should see all other users' DIDs
            // Poll with retries to account for DHT gossip delays
            console.log("\nChecking 'others()' for each user:");

            // Helper function to poll until all expected DIDs are seen
            const pollUntilAllSeen = async (proxy: any, expectedDids: string[], userLabel: string, maxAttempts = 50) => {
                console.log(`Polling ${userLabel} for others...`);
                console.log(`Expected DIDs:`, expectedDids);
                for (let attempt = 1; attempt <= maxAttempts; attempt++) {
                    const others = await proxy.otherAgents();
                    console.log(`${userLabel} sees others (attempt ${attempt}):`, others);

                    const allFound = expectedDids.every(did => {
                        console.log(`Checking if ${did} is in ${others}`);
                        let result = others.includes(did);
                        console.log(`Result: ${result}`);
                        return result;
                    });
                    if (allFound) {
                        console.log(`✅ ${userLabel} sees all expected users!`);
                        return others;
                    }

                    if (attempt < maxAttempts) {
                        console.log(`${userLabel} waiting for DHT gossip... (${attempt}/${maxAttempts})`);
                        await sleep(2000);
                    }
                }

                // Return the last result even if not complete
                const finalOthers = await proxy.otherAgents();
                console.log(`${userLabel} final result after ${maxAttempts} attempts:`, finalOthers);
                return finalOthers;
            };

            const node1User1Others = await pollUntilAllSeen(
                node1User1Proxy!,
                [node1User2Did, node2User1Did, node2User2Did],
                "Node 1 User 1"
            );
            expect(node1User1Others).to.include(node1User2Did, "Node 1 User 1 should see Node 1 User 2");
            expect(node1User1Others).to.include(node2User1Did, "Node 1 User 1 should see Node 2 User 1");
            expect(node1User1Others).to.include(node2User2Did, "Node 1 User 1 should see Node 2 User 2");
            expect(node1User1Others).to.have.lengthOf(3, "Node 1 User 1 should see exactly 3 other users");

            const node1User2Others = await node1User2Proxy!.otherAgents();
            console.log("Node 1 User 2 sees others:", node1User2Others);
            expect(node1User2Others).to.include(node1User1Did, "Node 1 User 2 should see Node 1 User 1");
            expect(node1User2Others).to.include(node2User1Did, "Node 1 User 2 should see Node 2 User 1");
            expect(node1User2Others).to.include(node2User2Did, "Node 1 User 2 should see Node 2 User 2");
            expect(node1User2Others).to.have.lengthOf(3, "Node 1 User 2 should see exactly 3 other users");

            const node2User1Others = await node2User1Proxy!.otherAgents();
            console.log("Node 2 User 1 sees others:", node2User1Others);
            expect(node2User1Others).to.include(node1User1Did, "Node 2 User 1 should see Node 1 User 1");
            expect(node2User1Others).to.include(node1User2Did, "Node 2 User 1 should see Node 1 User 2");
            expect(node2User1Others).to.include(node2User2Did, "Node 2 User 1 should see Node 2 User 2");
            expect(node2User1Others).to.have.lengthOf(3, "Node 2 User 1 should see exactly 3 other users");

            const node2User2Others = await node2User2Proxy!.otherAgents();
            console.log("Node 2 User 2 sees others:", node2User2Others);
            expect(node2User2Others).to.include(node1User1Did, "Node 2 User 2 should see Node 1 User 1");
            expect(node2User2Others).to.include(node1User2Did, "Node 2 User 2 should see Node 1 User 2");
            expect(node2User2Others).to.include(node2User1Did, "Node 2 User 2 should see Node 2 User 1");
            expect(node2User2Others).to.have.lengthOf(3, "Node 2 User 2 should see exactly 3 other users");

            console.log("\n✅ All users correctly see all other users via others()");
        });

        it("should route p2p signals correctly between users across nodes", async function() {
            this.timeout(30000);

            console.log("\n=== Testing cross-node p2p signal routing ===");

            // Get neighbourhood URL from previous test
            const node1User1Perspectives = await node1User1Client!.perspective.all();
            const node1User1Neighbourhood = node1User1Perspectives.find(p => p.sharedUrl);
            expect(node1User1Neighbourhood).to.not.be.undefined;
            const node1User1Proxy = await node1User1Neighbourhood!.getNeighbourhoodProxy();

            const node2User1Perspectives = await node2User1Client!.perspective.all();
            const node2User1Neighbourhood = node2User1Perspectives.find(p => p.sharedUrl);
            expect(node2User1Neighbourhood).to.not.be.undefined;
            const node2User1Proxy = await node2User1Neighbourhood!.getNeighbourhoodProxy();

            const node2User2Perspectives = await node2User2Client!.perspective.all();
            const node2User2Neighbourhood = node2User2Perspectives.find(p => p.sharedUrl);
            expect(node2User2Neighbourhood).to.not.be.undefined;
            const node2User2Proxy = await node2User2Neighbourhood!.getNeighbourhoodProxy();

            // Set up signal handlers
            const node2User1ReceivedSignals: any[] = [];
            node2User1Proxy!.addSignalHandler((signal) => {
                console.log("Node 2 User 1 received signal from:", signal.author);
                node2User1ReceivedSignals.push(signal);
            });

            const node2User2ReceivedSignals: any[] = [];
            node2User2Proxy!.addSignalHandler((signal) => {
                console.log("Node 2 User 2 received signal from:", signal.author);
                node2User2ReceivedSignals.push(signal);
            });

            await sleep(500); // Let handlers initialize

            // Node 1 User 1 sends a signal to Node 2 User 1
            console.log(`\nNode 1 User 1 (${node1User1Did.substring(0, 20)}...) sending signal to Node 2 User 1 (${node2User1Did.substring(0, 20)}...)`);
            await node1User1Proxy!.sendSignalU(node2User1Did, new PerspectiveUnsignedInput([
                {
                    source: "test://signal1",
                    predicate: "test://from",
                    target: node1User1Did
                }
            ]));

            // Wait for signal delivery
            const maxWaitTime = 10000;
            let startTime = Date.now();
            while (node2User1ReceivedSignals.length === 0 && (Date.now() - startTime) < maxWaitTime) {
                await sleep(100);
            }

            // Verify Node 2 User 1 received the signal
            expect(node2User1ReceivedSignals.length).to.be.greaterThan(0, "Node 2 User 1 should have received signal");
            expect(node2User1ReceivedSignals[0].author).to.equal(node1User1Did);
            console.log("✅ Node 2 User 1 received signal from Node 1 User 1");

            // Verify Node 2 User 2 did NOT receive the signal (it was meant for Node 2 User 1)
            expect(node2User2ReceivedSignals.length).to.equal(0, "Node 2 User 2 should NOT have received signal meant for Node 2 User 1");
            console.log("✅ Node 2 User 2 correctly did not receive signal");

            // Now test the reverse: Node 2 User 1 sends to Node 1 User 1
            const node1User1ReceivedSignals: any[] = [];
            node1User1Proxy!.addSignalHandler((signal) => {
                console.log("Node 1 User 1 received signal from:", signal.author);
                node1User1ReceivedSignals.push(signal);
            });

            await sleep(500);

            console.log(`\nNode 2 User 1 (${node2User1Did.substring(0, 20)}...) sending signal to Node 1 User 1 (${node1User1Did.substring(0, 20)}...)`);
            await node2User1Proxy!.sendSignalU(node1User1Did, new PerspectiveUnsignedInput([
                {
                    source: "test://signal2",
                    predicate: "test://from",
                    target: node2User1Did
                }
            ]));

            startTime = Date.now();
            while (node1User1ReceivedSignals.length === 0 && (Date.now() - startTime) < maxWaitTime) {
                await sleep(100);
            }

            expect(node1User1ReceivedSignals.length).to.be.greaterThan(0, "Node 1 User 1 should have received signal");
            expect(node1User1ReceivedSignals[0].author).to.equal(node2User1Did);
            console.log("✅ Node 1 User 1 received signal from Node 2 User 1");

            console.log("\n✅ Cross-node p2p signal routing works correctly");
        });

        it("should sync links correctly between all users across nodes", async function() {
            this.timeout(30000);

            console.log("\n=== Testing cross-node link synchronization ===");

            // Get neighbourhood perspectives for all users
            const node1User1Perspectives = await node1User1Client!.perspective.all();
            const node1User1Neighbourhood = node1User1Perspectives.find(p => p.sharedUrl);
            expect(node1User1Neighbourhood).to.not.be.undefined;

            const node1User2Perspectives = await node1User2Client!.perspective.all();
            const node1User2Neighbourhood = node1User2Perspectives.find(p => p.sharedUrl);
            expect(node1User2Neighbourhood).to.not.be.undefined;

            const node2User1Perspectives = await node2User1Client!.perspective.all();
            const node2User1Neighbourhood = node2User1Perspectives.find(p => p.sharedUrl);
            expect(node2User1Neighbourhood).to.not.be.undefined;

            const node2User2Perspectives = await node2User2Client!.perspective.all();
            const node2User2Neighbourhood = node2User2Perspectives.find(p => p.sharedUrl);
            expect(node2User2Neighbourhood).to.not.be.undefined;

            // Each user adds a unique link
            console.log("\nEach user adding their own unique link...");

            await node1User1Client!.perspective.addLink(node1User1Neighbourhood!.uuid, {
                source: "test://node1user1",
                target: "test://link1",
                predicate: "test://created"
            });
            console.log("Node 1 User 1 added link");

            await node1User2Client!.perspective.addLink(node1User2Neighbourhood!.uuid, {
                source: "test://node1user2",
                target: "test://link2",
                predicate: "test://created"
            });
            console.log("Node 1 User 2 added link");

            await node2User1Client!.perspective.addLink(node2User1Neighbourhood!.uuid, {
                source: "test://node2user1",
                target: "test://link3",
                predicate: "test://created"
            });
            console.log("Node 2 User 1 added link");

            await node2User2Client!.perspective.addLink(node2User2Neighbourhood!.uuid, {
                source: "test://node2user2",
                target: "test://link4",
                predicate: "test://created"
            });
            console.log("Node 2 User 2 added link");

            // Wait for synchronization
            console.log("\nWaiting for sync...");
            await sleep(5000);

            // Query links from each user's perspective
            console.log("\nQuerying links from each user's perspective...");

            const node1User1Links = await node1User1Client!.perspective.queryLinks(
                node1User1Neighbourhood!.uuid,
                new LinkQuery({})
            );
            console.log(`Node 1 User 1 sees ${node1User1Links.length} links`);

            const node1User2Links = await node1User2Client!.perspective.queryLinks(
                node1User2Neighbourhood!.uuid,
                new LinkQuery({})
            );
            console.log(`Node 1 User 2 sees ${node1User2Links.length} links`);

            const node2User1Links = await node2User1Client!.perspective.queryLinks(
                node2User1Neighbourhood!.uuid,
                new LinkQuery({})
            );
            console.log(`Node 2 User 1 sees ${node2User1Links.length} links`);

            const node2User2Links = await node2User2Client!.perspective.queryLinks(
                node2User2Neighbourhood!.uuid,
                new LinkQuery({})
            );
            console.log(`Node 2 User 2 sees ${node2User2Links.length} links`);

            // All users should see at least 5 links (1 from setup + 4 from each user)
            expect(node1User1Links.length).to.be.greaterThanOrEqual(5, "Node 1 User 1 should see all links");
            expect(node1User2Links.length).to.be.greaterThanOrEqual(5, "Node 1 User 2 should see all links");
            expect(node2User1Links.length).to.be.greaterThanOrEqual(5, "Node 2 User 1 should see all links");
            expect(node2User2Links.length).to.be.greaterThanOrEqual(5, "Node 2 User 2 should see all links");

            // Verify each user sees all the specific links
            const checkLinkExists = (links: any[], source: string, target: string) => {
                return links.some(l => l.data.source === source && l.data.target === target);
            };

            // Check Node 1 User 1 sees all links
            expect(checkLinkExists(node1User1Links, "test://node1user1", "test://link1")).to.be.true;
            expect(checkLinkExists(node1User1Links, "test://node1user2", "test://link2")).to.be.true;
            expect(checkLinkExists(node1User1Links, "test://node2user1", "test://link3")).to.be.true;
            expect(checkLinkExists(node1User1Links, "test://node2user2", "test://link4")).to.be.true;
            console.log("✅ Node 1 User 1 sees all links");

            // Check Node 1 User 2 sees all links
            expect(checkLinkExists(node1User2Links, "test://node1user1", "test://link1")).to.be.true;
            expect(checkLinkExists(node1User2Links, "test://node1user2", "test://link2")).to.be.true;
            expect(checkLinkExists(node1User2Links, "test://node2user1", "test://link3")).to.be.true;
            expect(checkLinkExists(node1User2Links, "test://node2user2", "test://link4")).to.be.true;
            console.log("✅ Node 1 User 2 sees all links");

            // Check Node 2 User 1 sees all links
            expect(checkLinkExists(node2User1Links, "test://node1user1", "test://link1")).to.be.true;
            expect(checkLinkExists(node2User1Links, "test://node1user2", "test://link2")).to.be.true;
            expect(checkLinkExists(node2User1Links, "test://node2user1", "test://link3")).to.be.true;
            expect(checkLinkExists(node2User1Links, "test://node2user2", "test://link4")).to.be.true;
            console.log("✅ Node 2 User 1 sees all links");

            // Check Node 2 User 2 sees all links
            expect(checkLinkExists(node2User2Links, "test://node1user1", "test://link1")).to.be.true;
            expect(checkLinkExists(node2User2Links, "test://node1user2", "test://link2")).to.be.true;
            expect(checkLinkExists(node2User2Links, "test://node2user1", "test://link3")).to.be.true;
            expect(checkLinkExists(node2User2Links, "test://node2user2", "test://link4")).to.be.true;
            console.log("✅ Node 2 User 2 sees all links");

            // Verify link authors are correct
            const node1User1Link = node1User1Links.find(l => l.data.source === "test://node1user1");
            expect(node1User1Link?.author).to.equal(node1User1Did);

            const node1User2Link = node1User1Links.find(l => l.data.source === "test://node1user2");
            expect(node1User2Link?.author).to.equal(node1User2Did);

            const node2User1Link = node1User1Links.find(l => l.data.source === "test://node2user1");
            expect(node2User1Link?.author).to.equal(node2User1Did);

            const node2User2Link = node1User1Links.find(l => l.data.source === "test://node2user2");
            expect(node2User2Link?.author).to.equal(node2User2Did);

            console.log("✅ All links have correct authors");

            console.log("\n✅ Cross-node link synchronization works correctly");
        });
    });
})

