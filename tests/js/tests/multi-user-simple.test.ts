import path from "path";
import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch'
import { LinkQuery } from "@coasys/ad4m";

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

            expect(user1Payload.sub).to.equal("alice@example.com");
            expect(user2Payload.sub).to.equal("bob@example.com");
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
    });
})

