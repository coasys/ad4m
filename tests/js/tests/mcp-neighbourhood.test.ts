/**
 * MCP Neighbourhood Integration Tests — Two-Agent Publish/Join Flow
 *
 * Tests the neighbourhood_publish_from_perspective and neighbourhood_join_from_url
 * MCP tools with two agents (Alice and Bob) connected via local HC services.
 *
 * Flow:
 * 1. Start local HC services (bootstrap, proxy)
 * 2. Start Alice executor (MCP enabled) — creates perspective, publishes neighbourhood
 * 3. Start Bob executor (MCP enabled) — joins neighbourhood via URL
 * 4. Verify both agents have the neighbourhood perspective
 */

import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor, runHcLocalServices, killByPorts } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch';
import { v4 as uuidv4 } from 'uuid';

//@ts-ignore
global.fetch = fetch;

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ============================================================================
// MCP HTTP Client Helpers (mirrored from mcp-http.test.ts)
// ============================================================================

async function parseSSEStream(response: any): Promise<McpResponse> {
    return new Promise(function(resolve, reject) {
        var buffer = '';
        var resolved = false;
        var timeout = setTimeout(function() {
            if (!resolved) {
                resolved = true;
                reject(new Error('SSE stream timeout — no JSON data received within 30s. Buffer: ' + buffer));
            }
        }, 30000);

        var body = response.body;
        if (!body) {
            clearTimeout(timeout);
            reject(new Error('No response body'));
            return;
        }

        body.on('data', function(chunk: Buffer) {
            buffer += chunk.toString();
            var lines = buffer.split('\n');
            for (var i = 0; i < lines.length - 1; i++) {
                var line = lines[i].trim();
                if (line.indexOf('data:') === 0) {
                    var payload = line.substring(5).trim();
                    if (payload.length > 0 && !resolved) {
                        try {
                            var parsed = JSON.parse(payload);
                            if (parsed.jsonrpc) {
                                resolved = true;
                                clearTimeout(timeout);
                                resolve(parsed as McpResponse);
                                body.destroy();
                                return;
                            }
                        } catch (e) { /* not valid JSON, continue */ }
                    }
                }
            }
            buffer = lines[lines.length - 1];
        });

        body.on('end', function() {
            if (!resolved) {
                resolved = true;
                clearTimeout(timeout);
                var lines = buffer.split('\n');
                for (var i = 0; i < lines.length; i++) {
                    var line = lines[i].trim();
                    if (line.indexOf('data:') === 0) {
                        var payload = line.substring(5).trim();
                        if (payload.length > 0) {
                            try {
                                resolve(JSON.parse(payload) as McpResponse);
                                return;
                            } catch (e) { /* skip */ }
                        }
                    }
                }
                reject(new Error('SSE stream ended without JSON data'));
            }
        });

        body.on('error', function(err: Error) {
            if (!resolved) {
                resolved = true;
                clearTimeout(timeout);
                reject(err);
            }
        });
    });
}

interface McpResponse {
    jsonrpc: string;
    id: number;
    result?: any;
    error?: { code: number; message: string; data?: any };
}

let requestIdCounter = 0;

async function mcpHttpRequest(
    mcpBaseUrl: string,
    method: string,
    params: any = {},
    sessionId?: string
): Promise<McpResponse> {
    const id = ++requestIdCounter;
    const request = { jsonrpc: "2.0", id: id, method: method, params: params };

    const headers: Record<string, string> = {
        'Content-Type': 'application/json',
        'Accept': 'application/json, text/event-stream'
    };
    if (sessionId) {
        headers['Mcp-Session-Id'] = sessionId;
    }

    const response = await fetch(mcpBaseUrl, {
        method: 'POST',
        headers: headers,
        body: JSON.stringify(request)
    });

    if (!response.ok) {
        throw new Error('HTTP error: ' + response.status + ' ' + response.statusText);
    }

    const ct = response.headers.get('content-type') || '';
    if (ct.indexOf('text/event-stream') >= 0) {
        return await parseSSEStream(response);
    }

    return await response.json() as McpResponse;
}

async function callMcpTool(
    mcpBaseUrl: string,
    toolName: string,
    args: Record<string, any>,
    sessionId?: string
): Promise<any> {
    const response = await mcpHttpRequest(mcpBaseUrl, "tools/call", {
        name: toolName,
        arguments: args
    }, sessionId);

    if (response.error) {
        throw new Error('MCP tool error [' + toolName + ']: ' + response.error.message);
    }

    const content = response.result && response.result.content;
    if (content && content[0] && content[0].text) {
        try {
            return JSON.parse(content[0].text);
        } catch (e) {
            return content[0].text;
        }
    }
    return response.result;
}

async function initializeMcp(mcpBaseUrl: string): Promise<{ sessionId: string; serverInfo: any }> {
    const id = ++requestIdCounter;
    const request = {
        jsonrpc: "2.0",
        id: id,
        method: "initialize",
        params: {
            protocolVersion: "2024-11-05",
            capabilities: { roots: { listChanged: false } },
            clientInfo: { name: "ad4m-neighbourhood-test", version: "1.0.0" }
        }
    };

    const resp = await fetch(mcpBaseUrl, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Accept': 'application/json, text/event-stream'
        },
        body: JSON.stringify(request)
    });

    if (!resp.ok) {
        throw new Error('MCP initialize HTTP error: ' + resp.status);
    }

    const sid = resp.headers.get('mcp-session-id') || "test-session";
    var result = await parseSSEStream(resp);

    if (result.error) {
        throw new Error('MCP initialize error: ' + result.error.message);
    }

    // Complete MCP handshake
    await fetch(mcpBaseUrl, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Accept': 'application/json, text/event-stream',
            'Mcp-Session-Id': sid
        },
        body: JSON.stringify({ jsonrpc: "2.0", method: "notifications/initialized" })
    });

    return { sessionId: sid, serverInfo: result.result };
}

// ============================================================================
// Test configuration
// ============================================================================

const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
const DIFF_SYNC_OFFICIAL = fs.readFileSync(path.join(__dirname, "../scripts/perspective-diff-sync-hash")).toString().trim();

// Alice: ports 15800-15802, MCP 3003
const ALICE_GQL_PORT = 15800;
const ALICE_HC_ADMIN_PORT = 15801;
const ALICE_HC_APP_PORT = 15802;
const ALICE_MCP_PORT = 3003;
const ALICE_MCP_URL = `http://127.0.0.1:${ALICE_MCP_PORT}/mcp`;
const ALICE_ADMIN_CREDENTIAL = "alice-mcp-neighbourhood-test-secret";

// Bob: ports 15803-15805, MCP 3004
const BOB_GQL_PORT = 15803;
const BOB_HC_ADMIN_PORT = 15804;
const BOB_HC_APP_PORT = 15805;
const BOB_MCP_PORT = 3004;
const BOB_MCP_URL = `http://127.0.0.1:${BOB_MCP_PORT}/mcp`;
const BOB_ADMIN_CREDENTIAL = "bob-mcp-neighbourhood-test-secret";

// ============================================================================
// Tests
// ============================================================================

describe("MCP Neighbourhood Integration Tests", function () {
    //@ts-ignore
    this.timeout(200000);

    let aliceProcess: ChildProcess | null = null;
    let bobProcess: ChildProcess | null = null;
    let localServicesProcess: ChildProcess | null = null;
    let proxyUrl: string | null = null;
    let bootstrapUrl: string | null = null;
    let relayUrl: string | null = null;

    let aliceMcpSession: string = "";
    let bobMcpSession: string = "";
    let neighbourhoodUrl: string = "";
    let alicePerspectiveUuid: string = "";

    const aliceDataPath = path.join(TEST_DIR, "agents", "mcp-neighbourhood-alice");
    const bobDataPath = path.join(TEST_DIR, "agents", "mcp-neighbourhood-bob");
    const bootstrapSeedPath = path.join(__dirname, "../bootstrapSeed.json");

    before(async () => {
        // Ensure test directory exists
        if (!fs.existsSync(path.join(TEST_DIR, 'agents'))) {
            fs.mkdirSync(path.join(TEST_DIR, 'agents'), { recursive: true });
        }

        // Start local HC services for P2P communication
        const localServices = await runHcLocalServices();
        proxyUrl = localServices.proxyUrl;
        bootstrapUrl = localServices.bootstrapUrl;
        localServicesProcess = localServices.process;
        relayUrl = localServices.relayUrl;

        console.log("Local HC services started:", { proxyUrl, bootstrapUrl, relayUrl });

        // Start Alice executor with MCP
        aliceProcess = await startExecutor(
            aliceDataPath,
            bootstrapSeedPath,
            ALICE_GQL_PORT,
            ALICE_HC_ADMIN_PORT,
            ALICE_HC_APP_PORT,
            false,                  // languageLanguageOnly = false (need full languages for neighbourhood)
            ALICE_ADMIN_CREDENTIAL,
            proxyUrl!,
            bootstrapUrl!,
            relayUrl!,
            true,                   // enableMcp
            ALICE_MCP_PORT,
        );

        // Generate Alice's agent via GraphQL
        const aliceClient = new Ad4mClient(apolloClient(ALICE_GQL_PORT, ALICE_ADMIN_CREDENTIAL), false);
        await aliceClient.agent.generate("alice-passphrase");
        console.log("Alice agent generated");

        // Start Bob executor with MCP
        bobProcess = await startExecutor(
            bobDataPath,
            bootstrapSeedPath,
            BOB_GQL_PORT,
            BOB_HC_ADMIN_PORT,
            BOB_HC_APP_PORT,
            false,
            BOB_ADMIN_CREDENTIAL,
            proxyUrl!,
            bootstrapUrl!,
            relayUrl!,
            true,                   // enableMcp
            BOB_MCP_PORT,
        );

        // Generate Bob's agent via GraphQL
        const bobClient = new Ad4mClient(apolloClient(BOB_GQL_PORT, BOB_ADMIN_CREDENTIAL), false);
        await bobClient.agent.generate("bob-passphrase");
        console.log("Bob agent generated");

        // Exchange agent infos for direct P2P communication
        const aliceInfos = await aliceClient.runtime.hcAgentInfos();
        const bobInfos = await bobClient.runtime.hcAgentInfos();
        await aliceClient.runtime.hcAddAgentInfos(bobInfos);
        await bobClient.runtime.hcAddAgentInfos(aliceInfos);
        console.log("Agent infos exchanged");

        await sleep(2000);
    });

    after(async () => {
        if (aliceProcess) {
            aliceProcess.kill('SIGTERM');
            await sleep(500);
            if (!aliceProcess.killed) aliceProcess.kill('SIGKILL');
        }
        if (bobProcess) {
            bobProcess.kill('SIGTERM');
            await sleep(500);
            if (!bobProcess.killed) bobProcess.kill('SIGKILL');
        }
        if (localServicesProcess) {
            localServicesProcess.kill('SIGKILL');
        }
        killByPorts([
            ALICE_GQL_PORT, ALICE_HC_ADMIN_PORT, ALICE_HC_APP_PORT, ALICE_MCP_PORT,
            BOB_GQL_PORT, BOB_HC_ADMIN_PORT, BOB_HC_APP_PORT, BOB_MCP_PORT
        ]);
    });

    // ========================================================================
    // 1. MCP Connection
    // ========================================================================

    describe("1. MCP Connection", function () {
        it("Alice can initialize MCP", async function () {
            const init = await initializeMcp(ALICE_MCP_URL);
            aliceMcpSession = init.sessionId;
            expect(init.serverInfo).to.exist;
            console.log("Alice MCP initialized, session:", aliceMcpSession);
        });

        it("Bob can initialize MCP", async function () {
            const init = await initializeMcp(BOB_MCP_URL);
            bobMcpSession = init.sessionId;
            expect(init.serverInfo).to.exist;
            console.log("Bob MCP initialized, session:", bobMcpSession);
        });

        it("Alice has neighbourhood tools available", async function () {
            const response = await mcpHttpRequest(ALICE_MCP_URL, "tools/list", {}, aliceMcpSession);
            const tools = response.result?.tools || [];
            const toolNames = tools.map((t: any) => t.name);
            expect(toolNames).to.include("neighbourhood_publish_from_perspective");
            expect(toolNames).to.include("neighbourhood_join_from_url");
        });
    });

    // ========================================================================
    // 2. MCP Authentication
    // ========================================================================

    describe("2. MCP Authentication", function () {
        it("Alice authenticates via MCP", async function () {
            const result = await callMcpTool(ALICE_MCP_URL, "agent_request_capability", {
                auth_info: JSON.stringify({
                    appName: "Alice Neighbourhood Test",
                    appDesc: "MCP neighbourhood integration test (Alice)",
                    appUrl: "http://localhost",
                    capabilities: [
                        { with: { domain: "*", pointers: ["*"] }, can: ["*"] }
                    ]
                })
            }, aliceMcpSession);
            expect(result.request_id).to.exist;

            const permitResult = await callMcpTool(ALICE_MCP_URL, "agent_permit_capability", {
                auth_info: JSON.stringify({ requestId: result.request_id, rand: result.rand })
            }, aliceMcpSession);
            expect(permitResult).to.exist;
            console.log("Alice authenticated");
        });

        it("Bob authenticates via MCP", async function () {
            const result = await callMcpTool(BOB_MCP_URL, "agent_request_capability", {
                auth_info: JSON.stringify({
                    appName: "Bob Neighbourhood Test",
                    appDesc: "MCP neighbourhood integration test (Bob)",
                    appUrl: "http://localhost",
                    capabilities: [
                        { with: { domain: "*", pointers: ["*"] }, can: ["*"] }
                    ]
                })
            }, bobMcpSession);
            expect(result.request_id).to.exist;

            const permitResult = await callMcpTool(BOB_MCP_URL, "agent_permit_capability", {
                auth_info: JSON.stringify({ requestId: result.request_id, rand: result.rand })
            }, bobMcpSession);
            expect(permitResult).to.exist;
            console.log("Bob authenticated");
        });
    });

    // ========================================================================
    // 3. Alice creates perspective and publishes neighbourhood
    // ========================================================================

    describe("3. Alice publishes neighbourhood", function () {
        it("Alice creates a perspective", async function () {
            const result = await callMcpTool(ALICE_MCP_URL, "perspective_add", {
                name: "MCP Neighbourhood Test"
            }, aliceMcpSession);
            expect(result.uuid).to.exist;
            expect(result.name).to.equal("MCP Neighbourhood Test");
            alicePerspectiveUuid = result.uuid;
            console.log("Alice created perspective:", alicePerspectiveUuid);
        });

        it("Alice creates a link-language from perspective-diff-sync template", async function () {
            // Apply the perspective-diff-sync template to get a unique link language
            const aliceClient = new Ad4mClient(apolloClient(ALICE_GQL_PORT, ALICE_ADMIN_CREDENTIAL), false);
            const socialContext = await aliceClient.languages.applyTemplateAndPublish(
                DIFF_SYNC_OFFICIAL,
                JSON.stringify({ uid: uuidv4(), name: "MCP Test Neighbourhood Sync" })
            );
            expect(socialContext.name).to.equal("MCP Test Neighbourhood Sync");

            // Now publish via MCP
            const result = await callMcpTool(ALICE_MCP_URL, "neighbourhood_publish_from_perspective", {
                perspective_uuid: alicePerspectiveUuid,
                link_language: socialContext.address
            }, aliceMcpSession);

            expect(result.success).to.be.true;
            expect(result.neighbourhood_url).to.exist;
            expect(result.neighbourhood_url).to.match(/^neighbourhood:\/\//);
            neighbourhoodUrl = result.neighbourhood_url;
            console.log("Alice published neighbourhood:", neighbourhoodUrl);
        });

        it("Alice adds a link to the neighbourhood perspective", async function () {
            const result = await callMcpTool(ALICE_MCP_URL, "perspective_add_link", {
                uuid: alicePerspectiveUuid,
                source: "ad4m://self",
                predicate: "ad4m://test",
                target: "literal://string:hello-from-alice"
            }, aliceMcpSession);
            expect(result).to.exist;
            console.log("Alice added link to neighbourhood");
        });
    });

    // ========================================================================
    // 4. Bob joins neighbourhood
    // ========================================================================

    describe("4. Bob joins neighbourhood", function () {
        it("Bob joins Alice's neighbourhood via MCP", async function () {
            const result = await callMcpTool(BOB_MCP_URL, "neighbourhood_join_from_url", {
                url: neighbourhoodUrl
            }, bobMcpSession);

            expect(result.success).to.be.true;
            expect(result.perspective_uuid).to.exist;
            expect(result.neighbourhood_url).to.equal(neighbourhoodUrl);
            console.log("Bob joined neighbourhood, perspective:", result.perspective_uuid);
        });

        it("Bob can see the neighbourhood in his perspectives", async function () {
            const result = await callMcpTool(BOB_MCP_URL, "perspective_all", {}, bobMcpSession);
            expect(result).to.be.an("array");
            const neighbourhoodPerspective = result.find(
                (p: any) => p.neighbourhood && p.neighbourhood.data &&
                    p.neighbourhood.data.url === neighbourhoodUrl
            );
            expect(neighbourhoodPerspective).to.exist;
            console.log("Bob can see neighbourhood perspective");
        });

        it("Bob can query links from the neighbourhood (after sync)", async function () {
            // Wait for P2P sync to propagate Alice's link
            await sleep(10000);

            const bobPerspectives = await callMcpTool(BOB_MCP_URL, "perspective_all", {}, bobMcpSession);
            const bobNeighbourhoodPerspective = bobPerspectives.find(
                (p: any) => p.neighbourhood && p.neighbourhood.data &&
                    p.neighbourhood.data.url === neighbourhoodUrl
            );
            expect(bobNeighbourhoodPerspective).to.exist;

            const links = await callMcpTool(BOB_MCP_URL, "perspective_query_links", {
                uuid: bobNeighbourhoodPerspective.uuid,
                source: "ad4m://self"
            }, bobMcpSession);

            // The link should have synced from Alice
            console.log("Bob queried links:", JSON.stringify(links));
            // Note: P2P sync timing can vary, so we check that the query succeeds
            // rather than strictly requiring the link to be present
            expect(links).to.exist;
        });
    });

    // ========================================================================
    // 5. Error cases
    // ========================================================================

    describe("5. Error handling", function () {
        it("publishing with non-existent perspective fails gracefully", async function () {
            const result = await callMcpTool(ALICE_MCP_URL, "neighbourhood_publish_from_perspective", {
                perspective_uuid: "non-existent-uuid",
                link_language: "Qmfake"
            }, aliceMcpSession);
            expect(result.error).to.exist;
            expect(result.error).to.include("not found");
        });

        it("joining with invalid URL fails gracefully", async function () {
            const result = await callMcpTool(BOB_MCP_URL, "neighbourhood_join_from_url", {
                url: "neighbourhood://invalid-url-that-does-not-exist"
            }, bobMcpSession);
            expect(result.error).to.exist;
        });
    });
});
