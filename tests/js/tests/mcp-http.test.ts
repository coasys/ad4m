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
 * MCP HTTP Integration Tests
 * 
 * These tests verify the MCP server works correctly over HTTP transport.
 * They make actual HTTP requests to the MCP endpoint, testing the real MCP protocol.
 */

const MCP_PORT = 3001;
const MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}`;

// MCP JSON-RPC request helper over HTTP
let requestIdCounter = 0;

interface McpResponse {
    jsonrpc: string;
    id: number;
    result?: any;
    error?: { code: number; message: string; data?: any };
}

async function mcpHttpRequest(
    method: string, 
    params: any = {},
    sessionId?: string
): Promise<McpResponse> {
    const id = ++requestIdCounter;
    const request = {
        jsonrpc: "2.0",
        id,
        method,
        params
    };

    const headers: Record<string, string> = {
        'Content-Type': 'application/json',
        'Accept': 'application/json'
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
        throw new Error(`HTTP error: ${response.status} ${response.statusText}`);
    }

    return await response.json() as McpResponse;
}

// Helper to call MCP tool
async function callMcpTool(
    toolName: string,
    args: Record<string, any> = {},
    sessionId?: string
): Promise<any> {
    const response = await mcpHttpRequest("tools/call", {
        name: toolName,
        arguments: args
    }, sessionId);
    
    if (response.error) {
        throw new Error(`MCP tool error: ${response.error.message}`);
    }
    
    // MCP tool results come back as content array
    const content = response.result?.content;
    if (content && content[0]?.text) {
        try {
            return JSON.parse(content[0].text);
        } catch {
            return content[0].text;
        }
    }
    return response.result;
}

// Helper to list available tools
async function listMcpTools(sessionId?: string): Promise<any[]> {
    const response = await mcpHttpRequest("tools/list", {}, sessionId);
    if (response.error) {
        throw new Error(`MCP error: ${response.error.message}`);
    }
    return response.result?.tools || [];
}

// Helper to initialize MCP connection
async function initializeMcp(): Promise<{ sessionId: string; serverInfo: any }> {
    const response = await mcpHttpRequest("initialize", {
        protocolVersion: "2024-11-05",
        capabilities: {
            roots: { listChanged: false }
        },
        clientInfo: {
            name: "ad4m-test-client",
            version: "1.0.0"
        }
    });
    
    if (response.error) {
        throw new Error(`MCP initialize error: ${response.error.message}`);
    }
    
    // Session ID should come from response headers in stateful mode
    return {
        sessionId: response.result?.sessionId || "test-session",
        serverInfo: response.result
    };
}

describe("MCP HTTP Integration Tests", function() {
    this.timeout(120000); // 2 minute timeout

    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-http-test");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 16000;
    const hcAdminPort = 16001;
    const hcAppPort = 16002;
    const adminCredential = "mcp-http-test-admin";

    let executorProcess: ChildProcess | null = null;
    let adminClient: Ad4mClient | null = null;
    let testPerspectiveUuid: string | null = null;

    before(async () => {
        // Clean up and create test directory
        if (fs.existsSync(appDataPath)) {
            fs.rmSync(appDataPath, { recursive: true });
        }
        fs.mkdirSync(appDataPath, { recursive: true });

        // Start executor with MCP enabled
        // Note: startExecutor needs to be updated to accept enable_mcp flag
        executorProcess = await startExecutor(
            appDataPath,
            bootstrapSeedPath,
            gqlPort,
            hcAdminPort,
            hcAppPort,
            false,          // not languageLanguageOnly
            adminCredential,
            true            // enableMcp = true
        );

        // Wait for both GraphQL and MCP servers to be ready
        await sleep(5000);

        // Initialize admin client for setup
        adminClient = new Ad4mClient(apolloClient(gqlPort, adminCredential), false);
        await adminClient.agent.generate("test-passphrase");

        // Create a test perspective via GraphQL
        const perspective = await adminClient.perspective.add("MCP HTTP Test");
        testPerspectiveUuid = perspective.uuid;
        console.log(`Created test perspective: ${testPerspectiveUuid}`);
    });

    after(async () => {
        // Cleanup executor
        if (executorProcess) {
            let attempts = 0;
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

    describe("MCP Server Connection", () => {
        it("should respond to initialize request", async () => {
            const { serverInfo } = await initializeMcp();
            
            expect(serverInfo).to.exist;
            expect(serverInfo.protocolVersion).to.be.a('string');
            expect(serverInfo.serverInfo).to.exist;
            expect(serverInfo.serverInfo.name).to.equal("ad4m-mcp-server");
        });

        it("should list available tools", async () => {
            const tools = await listMcpTools();
            
            expect(tools).to.be.an('array');
            expect(tools.length).to.be.greaterThan(0);
            
            // Check for expected tools
            const toolNames = tools.map((t: any) => t.name);
            expect(toolNames).to.include('list_perspectives');
            expect(toolNames).to.include('list_subject_classes');
            expect(toolNames).to.include('query_subjects');
        });
    });

    describe("MCP Auth Tools", () => {
        it("should check auth status (unauthenticated)", async () => {
            const status = await callMcpTool('auth_status');
            
            expect(status).to.exist;
            expect(status.authenticated).to.be.a('boolean');
        });

        it("should login with email", async () => {
            const result = await callMcpTool('login_email', {
                email: 'test@example.com'
            });
            
            // This will send a verification email in production
            // For testing, we just verify the call succeeds
            expect(result).to.exist;
        });

        it("should set token for authentication", async () => {
            const result = await callMcpTool('set_token', {
                token: adminCredential
            });
            
            expect(result).to.exist;
            expect(result.success).to.be.true;
        });
    });

    describe("MCP Perspective Tools", () => {
        it("should list all perspectives", async () => {
            // First authenticate with admin credential
            await callMcpTool('set_token', { token: adminCredential });
            
            const perspectives = await callMcpTool('list_perspectives');
            
            expect(perspectives).to.be.an('array');
            expect(perspectives.length).to.be.greaterThan(0);
            
            const found = perspectives.find((p: any) => p.uuid === testPerspectiveUuid);
            expect(found).to.exist;
        });

        it("should get perspective details", async () => {
            const perspective = await callMcpTool('get_perspective', {
                uuid: testPerspectiveUuid
            });
            
            expect(perspective).to.exist;
            expect(perspective.uuid).to.equal(testPerspectiveUuid);
            expect(perspective.name).to.equal("MCP HTTP Test");
        });
    });

    describe("MCP Subject Class Tools", () => {
        it("should list subject classes in perspective", async () => {
            // Add a simple subject class first via GraphQL
            const sdna = `
                subject_class("TestClass", c).
                constructor(c, '[{action: "addLink", source: "this", predicate: "rdf://type", target: "test://class"}]').
                property(c, "name").
                property_getter(c, Base, "name", Value) :- triple(Base, "test://name", Value).
            `;
            await adminClient!.perspective.addSdna(testPerspectiveUuid!, sdna, "subject_class");
            
            // Now query via MCP
            const classes = await callMcpTool('list_subject_classes', {
                perspective_uuid: testPerspectiveUuid
            });
            
            expect(classes).to.be.an('array');
            expect(classes).to.include('TestClass');
        });

        it("should query subjects of a class", async () => {
            // Create a subject instance via GraphQL
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            await proxy!.createSubject({ name: "Test Instance" }, "TestClass");
            
            // Query via MCP
            const subjects = await callMcpTool('query_subjects', {
                perspective_uuid: testPerspectiveUuid,
                class_name: "TestClass"
            });
            
            expect(subjects).to.be.an('array');
            expect(subjects.length).to.be.greaterThan(0);
        });
    });

    describe("MCP Link Operations", () => {
        it("should query links in perspective", async () => {
            const links = await callMcpTool('query_links', {
                perspective_uuid: testPerspectiveUuid,
                query: {}  // Empty query returns all links
            });
            
            expect(links).to.be.an('array');
        });

        it("should add a link to perspective", async () => {
            const result = await callMcpTool('add_link', {
                perspective_uuid: testPerspectiveUuid,
                link: {
                    source: "test://source",
                    predicate: "test://predicate",
                    target: "test://target"
                }
            });
            
            expect(result).to.exist;
            expect(result.success).to.be.true;
            
            // Verify link was added
            const links = await callMcpTool('query_links', {
                perspective_uuid: testPerspectiveUuid,
                query: { predicate: "test://predicate" }
            });
            
            expect(links.length).to.be.greaterThan(0);
        });
    });
});
