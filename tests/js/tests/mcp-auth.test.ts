import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor, killByPorts } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch';
import { callMcpTool, initializeMcp } from './mcp-utils';

//@ts-ignore
global.fetch = fetch;

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * MCP Authentication Integration Tests (HTTP-only)
 *
 * Tests MCP auth tools via raw HTTP requests to the MCP Streamable HTTP server.
 * Verifies: login_email, request_capability + generate_jwt, auth_status, and unauthenticated rejection.
 */

const MCP_PORT = 16013;
const MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}/mcp`;

// ============================================================================
// Test Suite
// ============================================================================

describe("MCP Authentication HTTP Tests", function() {
    this.timeout(180000);

    const TEST_DIR = path.join(__dirname + "/../tst-tmp");
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-auth-test");
    const bootstrapSeedPath = path.join(__dirname + "/../bootstrapSeed.json");
    // Unique ports for mcp-auth tests — must not collide with other concurrent
    // CI jobs (integration-tests-js uses 15700-15702, mcp-http uses 16000-16002)
    const gqlPort = 16010;
    const hcAdminPort = 16011;
    const hcAppPort = 16012;
    const adminCredential = "mcp-auth-test-admin";

    let executorProcess: ChildProcess | null = null;
    let mcpSessionId: string = "";
    // Perspective UUID created while authenticated — used by the auth-bypass regression tests
    let authedPerspectiveUuid: string = "";

    before(async () => {
        // Clean up and create test directory
        if (fs.existsSync(appDataPath)) {
            fs.rmSync(appDataPath, { recursive: true });
        }
        fs.mkdirSync(appDataPath, { recursive: true });

        // Start executor with MCP enabled
        executorProcess = await startExecutor(
            appDataPath,
            bootstrapSeedPath,
            gqlPort,
            hcAdminPort,
            hcAppPort,
            true,               // languageLanguageOnly
            adminCredential,
            undefined,
            undefined,
            undefined,
            true,               // enableMcp
            MCP_PORT,           // mcpPort
        );

        await sleep(3000);

        // Generate agent via GraphQL (no MCP equivalent)
        const adminClient = new Ad4mClient(apolloClient(gqlPort, adminCredential), false);
        await adminClient.agent.generate("test-passphrase");
        console.log("Agent generated via GraphQL");
    });

    after(async () => {
        if (executorProcess) {
            executorProcess.kill('SIGTERM');
            await sleep(1000);
            if (!executorProcess.killed) {
                executorProcess.kill('SIGKILL');
            }
        }
        // Port-based kill as safety net — catches the executor even if the
        // ChildProcess handle is stale or kill() missed a grandchild process.
        killByPorts([gqlPort, hcAdminPort, hcAppPort, MCP_PORT]);
    });

    // ========================================================================
    // 1. MCP Session Initialization
    // ========================================================================

    describe("1. Session Init", function() {
        it("should initialize MCP connection", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            mcpSessionId = init.sessionId;
            expect(init.serverInfo).to.exist;
            console.log("MCP initialized, session:", mcpSessionId);
        });
    });

    // ========================================================================
    // 2. Unauthenticated Rejection
    // ========================================================================

    describe("2. Unauthenticated Rejection", function() {
        it("should report unauthenticated status before login", async function() {
            const status = await callMcpTool(MCP_BASE_URL, 'auth_status', {}, mcpSessionId);
            expect(status.authenticated).to.be.false;
            expect(status.message).to.include("Not authenticated");
            console.log("Auth status (before login):", JSON.stringify(status));
        });

        it("should reject list_perspectives without auth", async function() {
            try {
                const result = await callMcpTool(MCP_BASE_URL, 'list_perspectives', {}, mcpSessionId);
                // If it doesn't throw, the result must be an auth error string/object
                const msg = typeof result === 'string' ? result : JSON.stringify(result);
                expect(msg).to.include("Authentication required");
                console.log("Unauthenticated list_perspectives:", msg);
            } catch (e: any) {
                // callMcpTool throws on JSON-RPC error responses
                expect(e.message).to.include("Authentication required");
                console.log("Unauthenticated list_perspectives (thrown):", e.message);
            }
        });
    });

    // ========================================================================
    // 3. request_capability + generate_jwt Authentication
    // ========================================================================

    describe("3. request_capability + generate_jwt", function() {
        it("should get request_id and code from request_capability", async function() {
            const result = await callMcpTool(MCP_BASE_URL, 'request_capability', {
                app_name: "auth-test",
                app_desc: "MCP Auth Test"
            }, mcpSessionId);
            expect(result.request_id).to.be.a('string');
            expect(result.code).to.be.a('string');
            console.log("request_capability result:", JSON.stringify(result));
        });

        it("should authenticate via request_capability + generate_jwt", async function() {
            const capResult = await callMcpTool(MCP_BASE_URL, 'request_capability', {
                app_name: "auth-test",
                app_desc: "MCP Auth Test"
            }, mcpSessionId);
            expect(capResult.request_id).to.be.a('string');
            expect(capResult.code).to.be.a('string');

            const jwtResult = await callMcpTool(MCP_BASE_URL, 'generate_jwt', {
                request_id: capResult.request_id,
                code: capResult.code,
            }, mcpSessionId);
            expect(jwtResult.success).to.be.true;
            expect(jwtResult.token).to.be.a('string');
            console.log("generate_jwt result:", JSON.stringify(jwtResult));
        });

        it("should confirm authenticated status after generate_jwt", async function() {
            const status = await callMcpTool(MCP_BASE_URL, 'auth_status', {}, mcpSessionId);
            expect(status.authenticated).to.be.true;
            console.log("Auth status (after generate_jwt):", JSON.stringify(status));
        });

        it("should allow list_perspectives after authentication", async function() {
            const result = await callMcpTool(MCP_BASE_URL, 'list_perspectives', {}, mcpSessionId);
            expect(result).to.be.an('array');
            console.log("Authenticated list_perspectives:", JSON.stringify(result));
        });

        it("should create a perspective while authenticated (used by auth bypass regression tests)", async function() {
            const result = await callMcpTool(MCP_BASE_URL, 'add_perspective', {
                name: "auth-test-perspective"
            }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.uuid).to.be.a('string');
            authedPerspectiveUuid = result.uuid;
            console.log("Created perspective:", authedPerspectiveUuid);
        });

        it("should reject generate_jwt with invalid request_id/code", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            const freshSession = init.sessionId;
            const result = await callMcpTool(MCP_BASE_URL, 'generate_jwt', {
                request_id: "invalid-request-id",
                code: "000000",
            }, freshSession);
            expect(result.success).to.be.false;
        });
    });

    // ========================================================================
    // 4. login_email Tool (multi-user mode not enabled — should fail gracefully)
    // ========================================================================

    describe("4. login_email Tool", function() {
        it("should reject login_email when multi-user mode is not enabled", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            const freshSession = init.sessionId;
            const result = await callMcpTool(MCP_BASE_URL, 'login_email', {
                email: "test@ad4m.dev",
                password: "password123"
            }, freshSession);
            expect(result.success).to.be.false;
            expect(result.error).to.include("Multi-user mode");
            console.log("login_email (no multi-user):", JSON.stringify(result));
        });
    });

    // ========================================================================
    // 5. Email Signup + Login Flow (multi-user mode enabled, no SMTP)
    // ========================================================================

    describe("5. Email Signup + Login", function() {
        let adminClient: Ad4mClient;

        before(async function() {
            // Enable multi-user mode via GraphQL so email tools work
            adminClient = new Ad4mClient(apolloClient(gqlPort, adminCredential), false);
            await adminClient.runtime.setMultiUserEnabled(true);
            console.log("Multi-user mode enabled");
        });

        it("should signup a new user via MCP", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            const session = init.sessionId;

            const result = await callMcpTool(MCP_BASE_URL, 'signup', {
                email: "mcpuser@test.dev",
                password: "TestPass123!"
            }, session);

            expect(result.success).to.be.true;
            expect(result.did).to.be.a('string');
            console.log("Signup result:", JSON.stringify(result));
        });

        it("should login with email+password and get JWT", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            const session = init.sessionId;

            const result = await callMcpTool(MCP_BASE_URL, 'login_email', {
                email: "mcpuser@test.dev",
                password: "TestPass123!"
            }, session);

            expect(result.success).to.be.true;
            expect(result.token).to.be.a('string');
            expect(result.token.length).to.be.greaterThan(0);
            console.log("Login JWT received:", result.token.substring(0, 30) + "...");
        });

        it("should use email JWT for authenticated operations", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            const session = init.sessionId;

            // Login to get JWT stored in session
            const loginResult = await callMcpTool(MCP_BASE_URL, 'login_email', {
                email: "mcpuser@test.dev",
                password: "TestPass123!"
            }, session);
            expect(loginResult.success).to.be.true;

            // Verify auth status shows authenticated
            const status = await callMcpTool(MCP_BASE_URL, 'auth_status', {}, session);
            expect(status.authenticated).to.be.true;
            console.log("Auth status after email login:", JSON.stringify(status));

            // Use authenticated session for a real operation
            const perspectives = await callMcpTool(MCP_BASE_URL, 'list_perspectives', {}, session);
            expect(perspectives).to.be.an('array');
            console.log("Authenticated list_perspectives:", JSON.stringify(perspectives));
        });
    });

    // ========================================================================
    // 6. Auth Bypass Regression Tests
    //
    // Verifies that the security fix in call_tool() + get_readable_perspective()
    // holds: unauthenticated clients must not be able to access perspective data
    // even when they know a valid perspective UUID.
    //
    // Before the fix, in multi-user/admin mode:
    //   - capabilities_from_token("", Some(admin_cred)) returned Ok(minimal_caps)
    //   - The call_tool() check only tested capabilities.is_err() — so it passed
    //   - get_readable_perspective() treated no-JWT as main-agent context → data exposed
    // ========================================================================

    describe("6. Auth Bypass Regression — unauthenticated sessions cannot read perspective data", function() {
        it("query_links must reject unauthenticated access even with a known perspective UUID", async function() {
            expect(authedPerspectiveUuid).to.be.a('string').with.length.greaterThan(0);

            // Open a fresh session with no JWT
            const unauthSession = (await initializeMcp(MCP_BASE_URL)).sessionId;

            try {
                const result = await callMcpTool(MCP_BASE_URL, 'query_links', {
                    perspective_id: authedPerspectiveUuid,
                }, unauthSession);

                // If it doesn't throw, the result must be an auth error
                const msg = typeof result === 'string' ? result : JSON.stringify(result);
                expect(msg).to.include("Authentication required");
                console.log("Unauthenticated query_links response:", msg.substring(0, 120));
            } catch (e: any) {
                expect(e.message).to.include("Authentication required");
                console.log("Unauthenticated query_links (thrown):", e.message.substring(0, 120));
            }
        });

        it("get_models must reject unauthenticated access even with a known perspective UUID", async function() {
            expect(authedPerspectiveUuid).to.be.a('string').with.length.greaterThan(0);

            const unauthSession = (await initializeMcp(MCP_BASE_URL)).sessionId;

            try {
                const result = await callMcpTool(MCP_BASE_URL, 'get_models', {
                    perspective_id: authedPerspectiveUuid,
                }, unauthSession);

                const msg = typeof result === 'string' ? result : JSON.stringify(result);
                expect(msg).to.include("Authentication required");
                console.log("Unauthenticated get_models response:", msg.substring(0, 120));
            } catch (e: any) {
                expect(e.message).to.include("Authentication required");
                console.log("Unauthenticated get_models (thrown):", e.message.substring(0, 120));
            }
        });

        it("query_subjects must reject unauthenticated access even with a known perspective UUID", async function() {
            expect(authedPerspectiveUuid).to.be.a('string').with.length.greaterThan(0);

            const unauthSession = (await initializeMcp(MCP_BASE_URL)).sessionId;

            try {
                const result = await callMcpTool(MCP_BASE_URL, 'query_subjects', {
                    perspective_id: authedPerspectiveUuid,
                    class_name: "Message",
                }, unauthSession);

                const msg = typeof result === 'string' ? result : JSON.stringify(result);
                expect(msg).to.include("Authentication required");
                console.log("Unauthenticated query_subjects response:", msg.substring(0, 120));
            } catch (e: any) {
                expect(e.message).to.include("Authentication required");
                console.log("Unauthenticated query_subjects (thrown):", e.message.substring(0, 120));
            }
        });
    });

    // ========================================================================
    // 7. Admin Credential via HTTP Authorization Header
    //
    // Tests that a fresh (unauthenticated) MCP session can call tools by
    // passing the admin credential in the HTTP Authorization header.
    // This is the auth path used when agents set "headers" in .mcp.json.
    // ========================================================================

    describe("7. Admin Credential via HTTP Authorization Header", function() {
        it("should authenticate with Authorization: <admin-credential> (bare token)", async function() {
            const freshSession = (await initializeMcp(MCP_BASE_URL)).sessionId;

            // Call list_perspectives with admin credential in Authorization header
            const result = await callMcpTool(
                MCP_BASE_URL,
                'list_perspectives',
                {},
                freshSession,
                { 'Authorization': adminCredential }
            );
            expect(result).to.be.an('array');
            console.log("Admin header auth (bare) list_perspectives:", JSON.stringify(result));
        });

        it("should authenticate with Authorization: Bearer <admin-credential>", async function() {
            const freshSession = (await initializeMcp(MCP_BASE_URL)).sessionId;

            const result = await callMcpTool(
                MCP_BASE_URL,
                'list_perspectives',
                {},
                freshSession,
                { 'Authorization': `Bearer ${adminCredential}` }
            );
            expect(result).to.be.an('array');
            console.log("Admin header auth (Bearer) list_perspectives:", JSON.stringify(result));
        });

        it("should reject wrong credential in Authorization header", async function() {
            const freshSession = (await initializeMcp(MCP_BASE_URL)).sessionId;

            try {
                const result = await callMcpTool(
                    MCP_BASE_URL,
                    'list_perspectives',
                    {},
                    freshSession,
                    { 'Authorization': 'wrong-credential' }
                );
                const msg = typeof result === 'string' ? result : JSON.stringify(result);
                expect(msg).to.include("Authentication required");
                console.log("Wrong admin header response:", msg.substring(0, 120));
            } catch (e: any) {
                expect(e.message).to.include("Authentication required");
                console.log("Wrong admin header (thrown):", e.message.substring(0, 120));
            }
        });

        it("should allow add_perspective via admin header auth", async function() {
            const freshSession = (await initializeMcp(MCP_BASE_URL)).sessionId;

            const result = await callMcpTool(
                MCP_BASE_URL,
                'add_perspective',
                { name: "admin-header-test" },
                freshSession,
                { 'Authorization': adminCredential }
            );
            expect(result.success).to.be.true;
            expect(result.uuid).to.be.a('string');
            console.log("Admin header add_perspective:", result.uuid);
        });
    });
});
