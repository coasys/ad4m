import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch';

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

// ============================================================================
// MCP HTTP Client Helpers (same pattern as mcp-http.test.ts)
// ============================================================================

const MCP_PORT = 3001;
const MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}/mcp`;

interface McpResponse {
    jsonrpc: string;
    id: number;
    result?: any;
    error?: { code: number; message: string; data?: any };
}

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
                        } catch (e) {
                            // Not valid JSON, continue
                        }
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

let requestIdCounter = 0;

async function mcpHttpRequest(
    method: string,
    params: any = {},
    sessionId?: string
): Promise<McpResponse> {
    const id = ++requestIdCounter;
    const request = { jsonrpc: "2.0", id, method, params };

    const headers: Record<string, string> = {
        'Content-Type': 'application/json',
        'Accept': 'application/json, text/event-stream'
    };
    if (sessionId) {
        headers['Mcp-Session-Id'] = sessionId;
    }

    const response = await fetch(MCP_BASE_URL, {
        method: 'POST',
        headers,
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
    toolName: string,
    args: Record<string, any>,
    sessionId?: string
): Promise<any> {
    const response = await mcpHttpRequest("tools/call", {
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

async function initializeMcp(): Promise<{ sessionId: string; serverInfo: any }> {
    const id = ++requestIdCounter;
    const request = {
        jsonrpc: "2.0",
        id,
        method: "initialize",
        params: {
            protocolVersion: "2024-11-05",
            capabilities: { roots: { listChanged: false } },
            clientInfo: { name: "ad4m-auth-test-client", version: "1.0.0" }
        }
    };

    const resp = await fetch(MCP_BASE_URL, {
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

    // Send notifications/initialized to complete the handshake
    await fetch(MCP_BASE_URL, {
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
// Test Suite
// ============================================================================

describe("MCP Authentication HTTP Tests", function() {
    this.timeout(180000);

    const TEST_DIR = path.join(__dirname + "/../tst-tmp");
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-auth-test");
    const bootstrapSeedPath = path.join(__dirname + "/../bootstrapSeed.json");
    const gqlPort = 15700;
    const hcAdminPort = 15701;
    const hcAppPort = 15702;
    const adminCredential = "mcp-auth-test-admin";

    let executorProcess: ChildProcess | null = null;
    let mcpSessionId: string = "";

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
        );

        await sleep(3000);

        // Generate agent via GraphQL (no MCP equivalent)
        const adminClient = new Ad4mClient(apolloClient(gqlPort, adminCredential), false);
        await adminClient.agent.generate("test-passphrase");
        console.log("Agent generated via GraphQL");
    });

    after(async () => {
        if (executorProcess) {
            var attempts = 0;
            while (!executorProcess.killed && attempts < 10) {
                executorProcess.kill();
                await sleep(500);
                attempts++;
            }
            if (!executorProcess.killed) {
                executorProcess.kill('SIGKILL');
            }
        }
    });

    // ========================================================================
    // 1. MCP Session Initialization
    // ========================================================================

    describe("1. Session Init", function() {
        it("should initialize MCP connection", async function() {
            const init = await initializeMcp();
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
            const status = await callMcpTool('auth_status', {}, mcpSessionId);
            expect(status.authenticated).to.be.false;
            expect(status.message).to.include("Not authenticated");
            console.log("Auth status (before login):", JSON.stringify(status));
        });

        it("should reject list_perspectives without auth", async function() {
            const result = await callMcpTool('list_perspectives', {}, mcpSessionId);
            // The tool returns an error string when not authenticated
            expect(typeof result).to.equal('string');
            expect(result).to.include("Authentication");
            console.log("Unauthenticated list_perspectives:", result);
        });
    });

    // ========================================================================
    // 3. request_capability + generate_jwt Authentication
    // ========================================================================

    describe("3. request_capability + generate_jwt", function() {
        it("should get request_id and code from request_capability", async function() {
            const result = await callMcpTool('request_capability', {
                app_name: "auth-test",
                app_desc: "MCP Auth Test"
            }, mcpSessionId);
            expect(result.request_id).to.be.a('string');
            expect(result.code).to.be.a('string');
            console.log("request_capability result:", JSON.stringify(result));
        });

        it("should authenticate via request_capability + generate_jwt", async function() {
            const capResult = await callMcpTool('request_capability', {
                app_name: "auth-test",
                app_desc: "MCP Auth Test"
            }, mcpSessionId);
            expect(capResult.request_id).to.be.a('string');
            expect(capResult.code).to.be.a('string');

            const jwtResult = await callMcpTool('generate_jwt', {
                request_id: capResult.request_id,
                code: capResult.code,
            }, mcpSessionId);
            expect(jwtResult.success).to.be.true;
            expect(jwtResult.token).to.be.a('string');
            console.log("generate_jwt result:", JSON.stringify(jwtResult));
        });

        it("should confirm authenticated status after generate_jwt", async function() {
            const status = await callMcpTool('auth_status', {}, mcpSessionId);
            expect(status.authenticated).to.be.true;
            console.log("Auth status (after generate_jwt):", JSON.stringify(status));
        });

        it("should allow list_perspectives after authentication", async function() {
            const result = await callMcpTool('list_perspectives', {}, mcpSessionId);
            expect(result).to.be.an('array');
            console.log("Authenticated list_perspectives:", JSON.stringify(result));
        });

        it("should reject generate_jwt with invalid request_id/code", async function() {
            const init = await initializeMcp();
            const freshSession = init.sessionId;
            const result = await callMcpTool('generate_jwt', {
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
            const init = await initializeMcp();
            const freshSession = init.sessionId;
            const result = await callMcpTool('login_email', {
                email: "test@ad4m.dev",
                password: "password123"
            }, freshSession);
            expect(result.success).to.be.false;
            expect(result.error).to.include("Multi-user mode");
            console.log("login_email (no multi-user):", JSON.stringify(result));
        });
    });
});
