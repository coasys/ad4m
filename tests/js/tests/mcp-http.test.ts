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
 * MCP HTTP Integration Tests — Flux Chat Flow
 *
 * Proves an AI agent can participate in a Flux chat room entirely via MCP.
 * NO AD4M client fallback — everything through MCP tools (except agent.generate).
 */

const MCP_PORT = 3001;
const MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}`;

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
    const request = { jsonrpc: "2.0", id, method, params };

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

async function listMcpTools(sessionId?: string): Promise<any[]> {
    const response = await mcpHttpRequest("tools/list", {}, sessionId);
    if (response.error) {
        throw new Error(`MCP error: ${response.error.message}`);
    }
    return response.result?.tools || [];
}

async function initializeMcp(): Promise<{ sessionId: string; serverInfo: any }> {
    const response = await mcpHttpRequest("initialize", {
        protocolVersion: "2024-11-05",
        capabilities: { roots: { listChanged: false } },
        clientInfo: { name: "ad4m-test-client", version: "1.0.0" }
    });

    if (response.error) {
        throw new Error(`MCP initialize error: ${response.error.message}`);
    }

    return {
        sessionId: response.result?.sessionId || "test-session",
        serverInfo: response.result
    };
}

// ============================================================================
// SHACL definitions for Flux models
// ============================================================================

const CHANNEL_SHACL = JSON.stringify({
    target_class: "flux://Channel",
    properties: [
        {
            path: "flux://channel_name",
            name: "name",
            datatype: "string",
            min_count: 1,
            max_count: 1,
            writable: true,
            resolve_language: "literal",
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_name", target: "value", local: false }
            ]
        }
    ],
    constructor_actions: [
        { action: "addLink", source: "this", predicate: "flux://entry_type", target: "flux://has_channel", local: false },
        { action: "addLink", source: "this", predicate: "rdf://type", target: "flux://Channel", local: false }
    ],
    destructor_actions: []
});

const MESSAGE_SHACL = JSON.stringify({
    target_class: "flux://Message",
    properties: [
        {
            path: "flux://body",
            name: "body",
            datatype: "string",
            min_count: 1,
            max_count: 1,
            writable: true,
            resolve_language: "literal",
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://body", target: "value", local: false }
            ]
        }
    ],
    constructor_actions: [
        { action: "addLink", source: "this", predicate: "flux://entry_type", target: "flux://has_message", local: false },
        { action: "addLink", source: "this", predicate: "rdf://type", target: "flux://Message", local: false }
    ],
    destructor_actions: []
});

// ============================================================================
// Test Suite
// ============================================================================

describe("MCP HTTP Flux Chat Integration Test", function() {
    this.timeout(180000); // 3 minute timeout

    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-http-test");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 16000;
    const hcAdminPort = 16001;
    const hcAppPort = 16002;
    const adminCredential = "mcp-http-test-admin";

    let executorProcess: ChildProcess | null = null;
    let perspectiveUuid: string = "";

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
            false,              // not languageLanguageOnly
            adminCredential,
            undefined,          // proxyUrl (default)
            undefined,          // bootstrapUrl (default)
            undefined,          // relayUrl
            true,               // enableMcp = true
        );

        // Wait for servers to settle
        await sleep(3000);

        // Generate agent via GraphQL (no MCP equivalent yet)
        const adminClient = new Ad4mClient(apolloClient(gqlPort, adminCredential), false);
        await adminClient.agent.generate("test-passphrase");
        console.log("Agent generated via GraphQL");
    });

    after(async () => {
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

    describe("1. MCP Connection & Auth", () => {
        it("should initialize MCP connection", async () => {
            const { serverInfo } = await initializeMcp();
            expect(serverInfo).to.exist;
            expect(serverInfo.protocolVersion).to.be.a('string');
            expect(serverInfo.serverInfo).to.exist;
        });

        it("should list all available tools including new ones", async () => {
            const tools = await listMcpTools();
            const toolNames = tools.map((t: any) => t.name);
            expect(toolNames).to.include('list_perspectives');
            expect(toolNames).to.include('add_link');
            expect(toolNames).to.include('query_links');
            expect(toolNames).to.include('add_sdna');
            expect(toolNames).to.include('add_perspective');
            expect(toolNames).to.include('create_subject');
            expect(toolNames).to.include('query_subjects');
            expect(toolNames).to.include('get_subject_data');
        });

        it("should authenticate with admin credential via set_token", async () => {
            const result = await callMcpTool('set_token', { token: adminCredential });
            expect(result.success).to.be.true;
        });

        it("should confirm auth status", async () => {
            const status = await callMcpTool('auth_status');
            expect(status.authenticated).to.be.true;
        });
    });

    describe("2. Flux Chat Flow — All via MCP", () => {
        it("step 1: create a perspective via MCP", async () => {
            const result = await callMcpTool('add_perspective', { name: "Flux Test Room" });
            expect(result.success).to.be.true;
            expect(result.uuid).to.be.a('string');
            perspectiveUuid = result.uuid;
            console.log(`Created perspective via MCP: ${perspectiveUuid}`);
        });

        it("step 2: verify perspective appears in list", async () => {
            const perspectives = await callMcpTool('list_perspectives');
            expect(perspectives).to.be.an('array');
            const found = perspectives.find((p: any) => p.uuid === perspectiveUuid);
            expect(found).to.exist;
            expect(found.name).to.equal("Flux Test Room");
        });

        it("step 3: register Channel subject class via add_sdna", async () => {
            const result = await callMcpTool('add_sdna', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                shacl_json: CHANNEL_SHACL,
            });
            expect(result.success).to.be.true;
        });

        it("step 4: register Message subject class via add_sdna", async () => {
            const result = await callMcpTool('add_sdna', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                shacl_json: MESSAGE_SHACL,
            });
            expect(result.success).to.be.true;
        });

        it("step 5: verify subject classes are registered", async () => {
            const classes = await callMcpTool('list_subject_classes', {
                perspective_id: perspectiveUuid,
            });
            // Result comes as prolog resolution string, should contain Channel and Message
            const classStr = typeof classes === 'string' ? classes : JSON.stringify(classes);
            expect(classStr).to.include('Channel');
            expect(classStr).to.include('Message');
        });

        let channelAddr: string;

        it("step 6: create a Channel instance via create_subject", async () => {
            channelAddr = `flux://channel-${Date.now()}`;
            const result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channelAddr,
                initial_values: JSON.stringify({ name: "general" }),
            });
            expect(result.created).to.be.true;
            console.log(`Created Channel: ${channelAddr}`);
        });

        it("step 7: set channel name property via add_link", async () => {
            // The constructor should have set some links, but let's also explicitly
            // set the channel name via a direct link
            const result = await callMcpTool('add_link', {
                perspective_id: perspectiveUuid,
                source: channelAddr,
                predicate: "flux://channel_name",
                target: "literal://string:general",
            });
            expect(result.success).to.be.true;
        });

        let messageAddr: string;

        it("step 8: create a Message in the channel via create_subject", async () => {
            messageAddr = `flux://message-${Date.now()}`;
            const result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: messageAddr,
                initial_values: JSON.stringify({ body: "Hello from MCP!" }),
            });
            expect(result.created).to.be.true;
            console.log(`Created Message: ${messageAddr}`);
        });

        it("step 9: set message body via add_link", async () => {
            const result = await callMcpTool('add_link', {
                perspective_id: perspectiveUuid,
                source: messageAddr,
                predicate: "flux://body",
                target: "literal://string:Hello from MCP!",
            });
            expect(result.success).to.be.true;
        });

        it("step 10: link message as child of channel via add_link", async () => {
            const result = await callMcpTool('add_link', {
                perspective_id: perspectiveUuid,
                source: channelAddr,
                predicate: "ad4m://has_child",
                target: messageAddr,
            });
            expect(result.success).to.be.true;
        });

        it("step 11: query all channels via query_subjects", async () => {
            const result = await callMcpTool('query_subjects', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
            });
            const resultStr = typeof result === 'string' ? result : JSON.stringify(result);
            expect(resultStr).to.include(channelAddr);
        });

        it("step 12: get channel data via get_subject_data", async () => {
            const data = await callMcpTool('get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channelAddr,
            });
            const dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("general");
        });

        it("step 13: query links to find messages in channel", async () => {
            const links = await callMcpTool('query_links', {
                perspective_id: perspectiveUuid,
                source: channelAddr,
                predicate: "ad4m://has_child",
            });
            expect(links).to.be.an('array');
            expect(links.length).to.be.greaterThan(0);
            const msgLink = links.find((l: any) => l.target === messageAddr);
            expect(msgLink).to.exist;
        });

        it("step 14: get message data via get_subject_data", async () => {
            const data = await callMcpTool('get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: messageAddr,
            });
            const dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("Hello from MCP!");
        });

        it("step 15: query all links in perspective", async () => {
            const links = await callMcpTool('query_links', {
                perspective_id: perspectiveUuid,
            });
            expect(links).to.be.an('array');
            expect(links.length).to.be.greaterThan(5); // channel + message + type links + child link
        });
    });
});
