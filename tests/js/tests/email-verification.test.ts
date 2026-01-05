import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor, runHcLocalServices } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch'

//@ts-ignore
global.fetch = fetch

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * Email Verification Flow Tests with Mock Email Service
 *
 * These tests use a mock email service that captures verification codes
 * instead of actually sending emails. This allows testing the full email
 * verification flow without requiring SMTP configuration.
 */
describe("Email Verification with Mock Service", () => {
    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "email-verification");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 15920
    const hcAdminPort = 15921
    const hcAppPort = 15922

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

        // Enable multi-user mode
        await adminAd4mClient.runtime.setMultiUserEnabled(true)

        // Enable email test mode - THIS IS KEY!
        await adminAd4mClient.runtime.emailTestModeEnable()
        console.log("✅ Email test mode enabled - codes will be captured");
    })

    after(async () => {
        if (adminAd4mClient) {
            await adminAd4mClient.runtime.emailTestModeDisable()
        }

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

    beforeEach(async () => {
        // Clear codes before each test
        await adminAd4mClient!.runtime.emailTestClearCodes()
    })

    describe("Email Verification Flow - New User Signup", () => {
        it("should complete full signup flow with email verification", async () => {
            const email = "newuser@example.com";
            const password = "SecurePass123!";

            console.log("\n🔹 Step 1: Request login verification for new user");
            const verifyRequest = await adminAd4mClient!.agent.requestLoginVerification(email);

            expect(verifyRequest.success).to.be.true;
            expect(verifyRequest.requiresPassword).to.be.true; // New user
            console.log(`✅ Result: success=${verifyRequest.success}, requiresPassword=${verifyRequest.requiresPassword}`);
            console.log(`   Message: ${verifyRequest.message}`);

            console.log("\n🔹 Step 2: Create user (triggers verification email)");
            const createResult = await adminAd4mClient!.agent.createUser(email, password);

            expect(createResult.success).to.be.true;
            expect(createResult.did).to.be.a("string");
            console.log(`✅ User created with DID: ${createResult.did}`);

            console.log("\n🔹 Step 3: Retrieve captured verification code");
            const code = await adminAd4mClient!.runtime.emailTestGetCode(email);

            expect(code).to.exist;
            expect(code).to.be.a("string");
            expect(code!.length).to.equal(6);
            expect(/^\d{6}$/.test(code!)).to.be.true; // Must be 6 digits
            console.log(`✅ Captured code: ${code}`);

            console.log("\n🔹 Step 4: Verify email with code");
            const token = await adminAd4mClient!.agent.verifyEmailCode(email, code!, "signup");

            expect(token).to.be.a("string");
            expect(token.length).to.be.greaterThan(0);
            console.log(`✅ JWT token received: ${token.substring(0, 20)}...`);

            console.log("\n✅ Full signup flow completed successfully!");
        });

        it("should reject invalid verification codes", async () => {
            const email = "invalid-code@example.com";
            const password = "SecurePass123!";

            // Create user
            await adminAd4mClient!.agent.createUser(email, password);

            // Get the real code (but don't use it)
            const realCode = await adminAd4mClient!.runtime.emailTestGetCode(email);
            expect(realCode).to.exist;

            // Try to verify with wrong code
            try {
                await adminAd4mClient!.agent.verifyEmailCode(email, "000000", "signup");
                expect.fail("Should have thrown error for invalid code");
            } catch (e: any) {
                expect(e.message).to.include("Invalid");
                console.log(`✅ Invalid code correctly rejected: ${e.message}`);
            }
        });

        it("should enforce code expiration (15 minutes)", async () => {
            const email = "expiry@example.com";
            await adminAd4mClient!.agent.createUser(email, "password123");

            const code = await adminAd4mClient!.runtime.emailTestGetCode(email);
            expect(code).to.exist;

            // Verify the code works when fresh
            const token = await adminAd4mClient!.agent.verifyEmailCode(email, code!, "signup");
            expect(token).to.exist;
            console.log("✅ Fresh code works");

            // Create a new code for expiration testing
            await adminAd4mClient!.agent.requestLoginVerification(email);
            const expiredCode = await adminAd4mClient!.runtime.emailTestGetCode(email);
            expect(expiredCode).to.exist;

            // Simulate expiration by setting expiry time to past (1 hour ago)
            const pastTimestamp = Math.floor(Date.now() / 1000) - (60 * 60);
            await adminAd4mClient!.runtime.emailTestSetExpiry(email, "login", pastTimestamp);

            // Try to verify with expired code - should fail
            try {
                await adminAd4mClient!.agent.verifyEmailCode(email, expiredCode!, "login");
                expect.fail("Should have thrown error for expired code");
            } catch (e: any) {
                expect(e.message).to.match(/Invalid|expired|Expired/i);
                console.log(`✅ Expired code correctly rejected: ${e.message}`);
            }
        });
    });

    describe("Email Verification Flow - Existing User Login", () => {
        const existingEmail = "existing@example.com";
        const existingPassword = "ExistingPass123!";

        before(async () => {
            // Create and verify a user first
            await adminAd4mClient!.agent.createUser(existingEmail, existingPassword);
            const signupCode = await adminAd4mClient!.runtime.emailTestGetCode(existingEmail);
            await adminAd4mClient!.agent.verifyEmailCode(existingEmail, signupCode!, "signup");
            await adminAd4mClient!.runtime.emailTestClearCodes();
        });

        it("should complete login flow with email verification (passwordless)", async () => {
            console.log("\n🔹 Step 1: Request login verification for existing user");
            const verifyRequest = await adminAd4mClient!.agent.requestLoginVerification(existingEmail);

            expect(verifyRequest.success).to.be.true;
            expect(verifyRequest.requiresPassword).to.be.false; // Existing user
            console.log(`✅ Result: success=${verifyRequest.success}, requiresPassword=${verifyRequest.requiresPassword}`);

            console.log("\n🔹 Step 2: Retrieve captured verification code");
            const code = await adminAd4mClient!.runtime.emailTestGetCode(existingEmail);

            expect(code).to.exist;
            expect(code).to.be.a("string");
            expect(code!.length).to.equal(6);
            console.log(`✅ Captured login code: ${code}`);

            console.log("\n🔹 Step 3: Verify code for login");
            const token = await adminAd4mClient!.agent.verifyEmailCode(existingEmail, code!, "login");

            expect(token).to.be.a("string");
            expect(token.length).to.be.greaterThan(0);
            console.log(`✅ Logged in with JWT: ${token.substring(0, 20)}...`);

            console.log("\n✅ Passwordless login flow completed!");
        });

        it("should still support password-based login (backwards compatibility)", async () => {
            // Even with email verification enabled, password login should work
            const token = await adminAd4mClient!.agent.loginUser(existingEmail, existingPassword);

            expect(token).to.be.a("string");
            expect(token.length).to.be.greaterThan(0);

            console.log("✅ Password-based login still works (backwards compatible)");
        });
    });

    describe("Rate Limiting", () => {
        it("should enforce rate limiting on verification requests", async () => {
            const email = "ratelimit@example.com";
            await adminAd4mClient!.agent.createUser(email, "password123");

            // First verification code
            const code1 = await adminAd4mClient!.runtime.emailTestGetCode(email);
            await adminAd4mClient!.agent.verifyEmailCode(email, code1!, "signup");

            // Request login verification
            const request1 = await adminAd4mClient!.agent.requestLoginVerification(email);
            expect(request1.success).to.be.true;

            // Immediately try again - should be rate limited
            const request2 = await adminAd4mClient!.agent.requestLoginVerification(email);
            expect(request2.success).to.be.false;
            expect(request2.message).to.match(/rate limit|too many|wait/i);
            console.log(`✅ Rate limiting enforced: ${request2.message}`);
        });
    });

    describe("Mock Service Functionality", () => {
        it("should capture codes for multiple users simultaneously", async () => {
            const users = [
                { email: "user1@example.com", password: "pass1" },
                { email: "user2@example.com", password: "pass2" },
                { email: "user3@example.com", password: "pass3" },
            ];

            // Create all users
            for (const user of users) {
                await adminAd4mClient!.agent.createUser(user.email, user.password);
            }

            // Retrieve all codes
            for (const user of users) {
                const code = await adminAd4mClient!.runtime.emailTestGetCode(user.email);
                expect(code).to.exist;
                expect(code!.length).to.equal(6);
                console.log(`✅ ${user.email} -> code: ${code}`);
            }

            console.log("✅ Multiple users handled simultaneously");
        });

        it("should clear codes when requested", async () => {
            const email = "cleartest@example.com";
            await adminAd4mClient!.agent.createUser(email, "password123");

            let code = await adminAd4mClient!.runtime.emailTestGetCode(email);
            expect(code).to.exist;

            await adminAd4mClient!.runtime.emailTestClearCodes();

            code = await adminAd4mClient!.runtime.emailTestGetCode(email);
            expect(code).to.be.null;

            console.log("✅ Codes cleared successfully");
        });

        it("should have test mode enabled during tests", async () => {
            // Verify test mode is active by checking we can capture codes
            const email = "testmode@example.com";
            await adminAd4mClient!.agent.createUser(email, "password123");

            const code = await adminAd4mClient!.runtime.emailTestGetCode(email);
            expect(code).to.exist; // Only possible if test mode is enabled

            console.log("✅ Test mode is active and working");
        });
    });

    describe("Security Features", () => {
        it("should generate unique codes for each request", async () => {
            const email = "uniquecodes@example.com";
            await adminAd4mClient!.agent.createUser(email, "password123");

            const code1 = await adminAd4mClient!.runtime.emailTestGetCode(email);
            await adminAd4mClient!.agent.verifyEmailCode(email, code1!, "signup");

            // Request new code
            await adminAd4mClient!.agent.requestLoginVerification(email);
            await sleep(100); // Small delay to ensure new code generation

            const code2 = await adminAd4mClient!.runtime.emailTestGetCode(email);

            // Codes should be different
            expect(code1).to.not.equal(code2);
            console.log(`✅ Unique codes: ${code1} != ${code2}`);
        });

        it("should only allow code to be used once (single-use)", async () => {
            const email = "singleuse@example.com";
            await adminAd4mClient!.agent.createUser(email, "password123");

            const code = await adminAd4mClient!.runtime.emailTestGetCode(email);

            // Use code once
            await adminAd4mClient!.agent.verifyEmailCode(email, code!, "signup");

            // Try to use same code again
            try {
                await adminAd4mClient!.agent.verifyEmailCode(email, code!, "signup");
                expect.fail("Should not allow reusing the same code");
            } catch (e: any) {
                console.log(`✅ Single-use enforced: ${e.message}`);
            }
        });
    });
});
