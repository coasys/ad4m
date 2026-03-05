/**
 * MCP Neighbourhood Integration Tests
 *
 * Tests the neighbourhood_publish_from_perspective and neighbourhood_join_from_url
 * MCP tools. Uses languageLanguageOnly mode (no HC network needed) to test:
 * 1. Tool availability and parameter validation
 * 2. Error handling for missing perspectives
 * 3. Error handling for invalid URLs
 */

import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor, killByPorts } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch';
import { mcpHttpRequest, callMcpTool, initializeMcp } from './mcp-utils';

//@ts-ignore
global.fetch = fetch;

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ============================================================================
// Test configuration
// ============================================================================

const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
const MCP_PORT = 3003;
const MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}/mcp`;
const GQL_PORT = 15800;
const HC_ADMIN_PORT = 15801;
const HC_APP_PORT = 15802;
const ADMIN_CREDENTIAL = "mcp-neighbourhood-test-secret";

// ============================================================================
// Tests
// ============================================================================

describe("MCP Neighbourhood Integration Tests", function () {
    //@ts-ignore
    this.timeout(120000);

    let executorProcess: ChildProcess | null = null;
    let mcpSessionId: string = "";
    let perspectiveUuid: string = "";

    const appDataPath = path.join(TEST_DIR, "agents", "mcp-neighbourhood-test");
    const bootstrapSeedPath = path.join(__dirname, "../bootstrapSeed.json");

    before(async () => {
        if (!fs.existsSync(path.join(TEST_DIR, 'agents'))) {
            fs.mkdirSync(path.join(TEST_DIR, 'agents'), { recursive: true });
        }

        executorProcess = await startExecutor(
            appDataPath, bootstrapSeedPath,
            GQL_PORT, HC_ADMIN_PORT, HC_APP_PORT,
            true,               // languageLanguageOnly
            ADMIN_CREDENTIAL,
            undefined, undefined, undefined,
            true,               // enableMcp
            MCP_PORT,
        );

        await sleep(3000);

        const adminClient = new Ad4mClient(apolloClient(GQL_PORT, ADMIN_CREDENTIAL), false);
        await adminClient.agent.generate("test-passphrase");
        console.log("Agent generated");
    });

    after(async () => {
        if (executorProcess) {
            executorProcess.kill('SIGTERM');
            await sleep(1000);
            if (!executorProcess.killed) executorProcess.kill('SIGKILL');
        }
        killByPorts([GQL_PORT, HC_ADMIN_PORT, HC_APP_PORT, MCP_PORT]);
    });

    // ========================================================================
    // 1. MCP Connection & Auth
    // ========================================================================

    describe("1. MCP Connection & Auth", function() {
        it("should initialize MCP connection", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            mcpSessionId = init.sessionId;
            expect(mcpSessionId).to.be.a('string');
            console.log("MCP initialized, session:", mcpSessionId);
        });

        it("should have neighbourhood tools available", async function() {
            const response = await mcpHttpRequest(MCP_BASE_URL, "tools/list", {}, mcpSessionId);
            const toolNames = (response.result?.tools || []).map((t: any) => t.name);
            expect(toolNames).to.include("neighbourhood_publish_from_perspective");
            expect(toolNames).to.include("neighbourhood_join_from_url");
            console.log("Neighbourhood tools found");
        });

        it("should authenticate via request_capability + generate_jwt", async function() {
            const capResult = await callMcpTool(MCP_BASE_URL, "request_capability", {
                app_name: "Neighbourhood Test",
                app_desc: "MCP neighbourhood integration test"
            }, mcpSessionId);
            expect(capResult.request_id).to.be.a('string');
            expect(capResult.code).to.be.a('string');
            console.log("Got capability request_id:", capResult.request_id);

            const jwtResult = await callMcpTool(MCP_BASE_URL, "generate_jwt", {
                request_id: capResult.request_id,
                code: capResult.code,
            }, mcpSessionId);
            expect(jwtResult.success).to.be.true;
            expect(jwtResult.token).to.be.a('string');
            console.log("Authenticated with JWT");
        });

        it("should confirm auth status", async function() {
            const status = await callMcpTool(MCP_BASE_URL, "auth_status", {}, mcpSessionId);
            expect(status.authenticated).to.be.true;
            console.log("Auth status confirmed");
        });
    });

    // ========================================================================
    // 2. Neighbourhood Publish — Error Cases
    // ========================================================================

    describe("2. Neighbourhood publish error handling", function() {
        it("should fail gracefully with non-existent perspective", async function() {
            const result = await callMcpTool(MCP_BASE_URL, "neighbourhood_publish_from_perspective", {
                perspective_uuid: "non-existent-uuid-12345",
                link_language: "QmFakeLinkLanguage"
            }, mcpSessionId);
            expect(result.error).to.be.a('string');
            console.log("Non-existent perspective error:", result.error);
        });

        it("should create a perspective for neighbourhood operations", async function() {
            // Create a perspective via MCP — verifies the tool works for neighbourhood setup
            const perspective = await callMcpTool(MCP_BASE_URL, "add_perspective", {
                name: "Neighbourhood Publish Test"
            }, mcpSessionId);
            expect(perspective.success).to.be.true;
            expect(perspective.uuid).to.be.a('string');
            perspectiveUuid = perspective.uuid;
            console.log("Created perspective:", perspectiveUuid);
            // Note: actual publish requires a real link language (e.g. perspective-diff-sync),
            // which isn't available in languageLanguageOnly mode. The tool schema and
            // error-handling paths are verified by other tests.
        });
    });

    // ========================================================================
    // 3. Neighbourhood Join — Error Cases
    // ========================================================================

    describe("3. Neighbourhood join error handling", function() {
        it("should fail gracefully with invalid neighbourhood URL", async function() {
            const result = await callMcpTool(MCP_BASE_URL, "neighbourhood_join_from_url", {
                url: "neighbourhood://QmInvalidNeighbourhoodUrl12345"
            }, mcpSessionId);
            expect(result.error).to.be.a('string');
            console.log("Invalid URL join error:", result.error);
        });

        it("should fail gracefully with empty URL", async function() {
            const result = await callMcpTool(MCP_BASE_URL, "neighbourhood_join_from_url", {
                url: ""
            }, mcpSessionId);
            expect(result.error).to.be.a('string');
            console.log("Empty URL join error:", result.error);
        });
    });

    // ========================================================================
    // 4. Tool Parameters Validation
    // ========================================================================

    describe("4. Tool parameter validation", function() {
        it("neighbourhood_publish_from_perspective has correct schema", async function() {
            const response = await mcpHttpRequest(MCP_BASE_URL, "tools/list", {}, mcpSessionId);
            const tools = response.result?.tools || [];
            const publishTool = tools.find((t: any) => t.name === "neighbourhood_publish_from_perspective");
            expect(publishTool).to.exist;
            expect(publishTool.description).to.include("Publish");
            expect(publishTool.inputSchema.properties).to.have.property("perspective_uuid");
            expect(publishTool.inputSchema.properties).to.have.property("link_language");
            console.log("Publish tool schema validated");
        });

        it("neighbourhood_join_from_url has correct schema", async function() {
            const response = await mcpHttpRequest(MCP_BASE_URL, "tools/list", {}, mcpSessionId);
            const tools = response.result?.tools || [];
            const joinTool = tools.find((t: any) => t.name === "neighbourhood_join_from_url");
            expect(joinTool).to.exist;
            expect(joinTool.description).to.include("Join");
            expect(joinTool.inputSchema.properties).to.have.property("url");
            console.log("Join tool schema validated");
        });
    });
});
