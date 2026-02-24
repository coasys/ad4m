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
 * Simulates an OpenClaw bot discovering and participating in a Flux chat room
 * entirely via MCP over HTTP. NO AD4M client fallback — everything through
 * MCP tools (except agent.generate which has no MCP equivalent).
 *
 * Test scenario:
 * 1. Start executor with MCP enabled
 * 2. Authenticate via MCP
 * 3. Create perspective and register SHACL subject classes (Channel, Message)
 * 4. Populate with 2 channels, each containing messages
 * 5. Simulate a bot discovering the perspective for the first time:
 *    - List subject classes to understand the data model
 *    - Query all channels
 *    - Navigate into a channel and list its messages
 *    - Add a new message to the channel
 */

// ============================================================================
// MCP HTTP Client Helpers
// ============================================================================

const MCP_PORT = 3001;
const MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}/mcp`;

/**
 * Parse an SSE stream response, extracting the first JSON-RPC message.
 * SSE streams from MCP Streamable HTTP start with a priming event (empty data),
 * followed by the actual response data event.
 */
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
            // Process complete lines
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
            // Keep the last incomplete line in the buffer
            buffer = lines[lines.length - 1];
        });

        body.on('end', function() {
            if (!resolved) {
                resolved = true;
                clearTimeout(timeout);
                // Try to parse any remaining buffer
                var lines = buffer.split('\n');
                for (var i = 0; i < lines.length; i++) {
                    var line = lines[i].trim();
                    if (line.indexOf('data:') === 0) {
                        var payload = line.substring(5).trim();
                        if (payload.length > 0) {
                            try {
                                resolve(JSON.parse(payload) as McpResponse);
                                return;
                            } catch (e) {
                                // skip
                            }
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

/**
 * Send an MCP JSON-RPC request via HTTP.
 * Handles SSE responses from Streamable HTTP transport.
 */
async function mcpHttpRequest(
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

    const response = await fetch(MCP_BASE_URL, {
        method: 'POST',
        headers: headers,
        body: JSON.stringify(request)
    });

    if (!response.ok) {
        throw new Error('HTTP error: ' + response.status + ' ' + response.statusText);
    }

    // Handle SSE responses from Streamable HTTP transport
    const ct = response.headers.get('content-type') || '';
    if (ct.indexOf('text/event-stream') >= 0) {
        // Read SSE stream chunk by chunk, looking for JSON data events
        return await parseSSEStream(response);
    }

    return await response.json() as McpResponse;
}

/**
 * Call an MCP tool and return the parsed result.
 */
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

/**
 * List all available MCP tools.
 */
async function listMcpTools(sessionId?: string): Promise<any[]> {
    const response = await mcpHttpRequest("tools/list", {}, sessionId);
    return response.result && response.result.tools ? response.result.tools : [];
}

/**
 * Initialize MCP session. Returns session ID from response header.
 */
async function initializeMcp(): Promise<{ sessionId: string; serverInfo: any }> {
    const id = ++requestIdCounter;
    const request = {
        jsonrpc: "2.0",
        id: id,
        method: "initialize",
        params: {
            protocolVersion: "2024-11-05",
            capabilities: { roots: { listChanged: false } },
            clientInfo: { name: "ad4m-test-client", version: "1.0.0" }
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

    // Extract session ID from response header
    const sid = resp.headers.get('mcp-session-id') || "test-session";

    // Parse SSE response using streaming parser
    var result = await parseSSEStream(resp);

    if (result.error) {
        throw new Error('MCP initialize error: ' + result.error.message);
    }

    // Send notifications/initialized to complete the MCP handshake.
    // The Streamable HTTP protocol requires this before the session will accept other requests.
    await fetch(MCP_BASE_URL, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Accept': 'application/json, text/event-stream',
            'Mcp-Session-Id': sid
        },
        body: JSON.stringify({ jsonrpc: "2.0", method: "notifications/initialized" })
    });

    return {
        sessionId: sid,
        serverInfo: result.result
    };
}

// ============================================================================
// SHACL definitions for Flux models (Channel and Message)
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
        },
        {
            path: "ad4m://has_child",
            name: "messages",
            collection: true,
            writable: true,
            adder: [
                { action: "addLink", source: "this", predicate: "ad4m://has_child", target: "value", local: false }
            ],
            remover: [
                { action: "removeLink", source: "this", predicate: "ad4m://has_child", target: "value", local: false }
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

    const TEST_DIR = path.join(__dirname + "/../tst-tmp");
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-http-test");
    const bootstrapSeedPath = path.join(__dirname + "/../bootstrapSeed.json");
    const gqlPort = 16000;
    const hcAdminPort = 16001;
    const hcAppPort = 16002;
    const adminCredential = "mcp-http-test-admin";

    let executorProcess: ChildProcess | null = null;
    let perspectiveUuid: string = "";
    let mcpSessionId: string = "";

    // Addresses for the populated perspective
    let channel1Addr: string = "";
    let channel2Addr: string = "";
    let msg1Addr: string = "";
    let msg2Addr: string = "";
    let msg3Addr: string = "";

    before(async () => {
        console.log(bootstrapSeedPath);
        console.log(appDataPath);

        // Clean up and create test directory
        if (fs.existsSync(appDataPath)) {
            fs.rmSync(appDataPath, { recursive: true });
        }
        fs.mkdirSync(appDataPath, { recursive: true });

        // Start executor with MCP enabled, languageLanguageOnly to skip network bootstrap
        executorProcess = await startExecutor(
            appDataPath,
            bootstrapSeedPath,
            gqlPort,
            hcAdminPort,
            hcAppPort,
            true,               // languageLanguageOnly (skip network bootstrap)
            adminCredential,
            undefined,          // proxyUrl
            undefined,          // bootstrapUrl
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
    // 1. MCP Connection & Authentication
    // ========================================================================

    describe("1. MCP Connection & Auth", function() {
        it("should initialize MCP connection", async function() {
            const init = await initializeMcp();
            mcpSessionId = init.sessionId;
            expect(init.serverInfo).to.exist;
            console.log("MCP initialized, session:", mcpSessionId);
        });

        it("should list all available tools", async function() {
            const tools = await listMcpTools(mcpSessionId);
            const toolNames = tools.map(function(t: any) { return t.name; });
            console.log("Available tools:", toolNames);

            // Core tools
            expect(toolNames).to.include('list_perspectives');
            expect(toolNames).to.include('add_perspective');
            expect(toolNames).to.include('add_link');
            expect(toolNames).to.include('query_links');

            // Subject class tools (higher-level)
            expect(toolNames).to.include('add_sdna');
            expect(toolNames).to.include('list_subject_classes');
            expect(toolNames).to.include('create_subject');
            expect(toolNames).to.include('query_subjects');
            expect(toolNames).to.include('get_subject_data');
            expect(toolNames).to.include('execute_commands');
            expect(toolNames).to.include('set_subject_property');
            expect(toolNames).to.include('get_subject_collection');
            expect(toolNames).to.include('add_to_collection');
            expect(toolNames).to.include('remove_from_collection');

            // Auth tools
            expect(toolNames).to.include('set_token');
            expect(toolNames).to.include('auth_status');
        });

        it("should authenticate with admin credential", async function() {
            const result = await callMcpTool('set_token', { token: adminCredential }, mcpSessionId);
            expect(result.success).to.be.true;
        });

        it("should confirm auth status", async function() {
            const status = await callMcpTool('auth_status', {}, mcpSessionId);
            expect(status.authenticated).to.be.true;
        });
    });

    // ========================================================================
    // 2. Set up a Flux-like perspective with channels and messages
    // ========================================================================

    describe("2. Populate Perspective (Setup)", function() {
        it("should create a perspective", async function() {
            const result = await callMcpTool('add_perspective', { name: "Flux Test Room" }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.uuid).to.be.a('string');
            perspectiveUuid = result.uuid;
            console.log("Created perspective:", perspectiveUuid);
        });

        it("should register Channel SHACL class", async function() {
            const result = await callMcpTool('add_sdna', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                shacl_json: CHANNEL_SHACL,
            }, mcpSessionId);
            console.log("add_sdna Channel result:", JSON.stringify(result));
            expect(result.success).to.be.true;
        });

        it("should register Message SHACL class", async function() {
            const result = await callMcpTool('add_sdna', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                shacl_json: MESSAGE_SHACL,
            }, mcpSessionId);
            expect(result.success).to.be.true;
        });

        it("should create channel #general with messages", async function() {
            channel1Addr = "flux://channel-general-" + Date.now();
            var result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                initial_values: JSON.stringify({ name: "general" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add two messages to #general
            msg1Addr = "flux://msg-" + Date.now() + "-1";
            result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: msg1Addr,
                initial_values: JSON.stringify({ body: "Welcome to the channel!" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add message to channel's messages collection (high-level, no manual links)
            await callMcpTool('add_to_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
                item_address: msg1Addr,
            }, mcpSessionId);

            msg2Addr = "flux://msg-" + Date.now() + "-2";
            result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: msg2Addr,
                initial_values: JSON.stringify({ body: "Let's discuss the roadmap" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            await callMcpTool('add_to_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
                item_address: msg2Addr,
            }, mcpSessionId);

            console.log("Created #general with 2 messages");
        });

        it("should create channel #random with a message", async function() {
            channel2Addr = "flux://channel-random-" + Date.now();
            var result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel2Addr,
                initial_values: JSON.stringify({ name: "random" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            msg3Addr = "flux://msg-" + Date.now() + "-3";
            result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: msg3Addr,
                initial_values: JSON.stringify({ body: "Random thought of the day" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            await callMcpTool('add_to_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel2Addr,
                collection_name: "messages",
                item_address: msg3Addr,
            }, mcpSessionId);

            console.log("Created #random with 1 message");
        });
    });

    // ========================================================================
    // 3. Bot Discovery — Simulate an OpenClaw bot joining for the first time
    // ========================================================================

    describe("3. Bot Discovery (OpenClaw bot explores perspective)", function() {

        it("should discover the perspective", async function() {
            const perspectives = await callMcpTool('list_perspectives', {}, mcpSessionId);
            expect(perspectives).to.be.an('array');
            var found = perspectives.find(function(p: any) { return p.uuid === perspectiveUuid; });
            expect(found).to.exist;
            expect(found.name).to.equal("Flux Test Room");
            console.log("Bot found perspective:", found.name, found.uuid);
        });

        it("should discover subject classes (understand the data model)", async function() {
            const classes = await callMcpTool('list_subject_classes', {
                perspective_id: perspectiveUuid,
            }, mcpSessionId);
            var classStr = typeof classes === 'string' ? classes : JSON.stringify(classes);
            expect(classStr).to.include('Channel');
            expect(classStr).to.include('Message');
            console.log("Bot discovered subject classes:", classStr);
        });

        it("should list all channels", async function() {
            const channels = await callMcpTool('query_subjects', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
            }, mcpSessionId);
            var channelStr = typeof channels === 'string' ? channels : JSON.stringify(channels);
            expect(channelStr).to.include(channel1Addr);
            expect(channelStr).to.include(channel2Addr);
            console.log("Bot found channels:", channelStr);
        });

        it("should read #general channel data", async function() {
            const data = await callMcpTool('get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
            }, mcpSessionId);
            var dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("general");
            console.log("Bot read #general:", dataStr);
        });

        it("should get messages in #general via collection", async function() {
            const collection = await callMcpTool('get_subject_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
            }, mcpSessionId);
            expect(collection.items).to.be.an('array');
            expect(collection.count).to.equal(2);
            expect(collection.items).to.include(msg1Addr);
            expect(collection.items).to.include(msg2Addr);
            console.log("Bot found", collection.count, "messages in #general");
        });

        it("should read message content", async function() {
            const data = await callMcpTool('get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: msg1Addr,
            }, mcpSessionId);
            var dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("Welcome to the channel!");
            console.log("Bot read message:", dataStr);
        });

        it("should add a new message to #general (high-level)", async function() {
            var botMsgAddr = "flux://msg-bot-" + Date.now();

            // Create message subject
            var result = await callMcpTool('create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: botMsgAddr,
                initial_values: JSON.stringify({ body: "Hello! I'm an OpenClaw bot. How can I help?" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add message to channel's collection (no manual links!)
            result = await callMcpTool('add_to_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
                item_address: botMsgAddr,
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // Verify via collection query
            var collection = await callMcpTool('get_subject_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
            }, mcpSessionId);
            expect(collection.count).to.equal(3); // 2 original + 1 bot message

            // Verify message content
            var msgData = await callMcpTool('get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: botMsgAddr,
            }, mcpSessionId);
            var dataStr = typeof msgData === 'string' ? msgData : JSON.stringify(msgData);
            expect(dataStr).to.include("OpenClaw bot");
            console.log("Bot successfully posted message to #general");
        });

        it("should update channel name via set_subject_property", async function() {
            var result = await callMcpTool('set_subject_property', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                property_name: "name",
                value: "general-renamed",
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // Verify the change
            var data = await callMcpTool('get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
            }, mcpSessionId);
            var dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("general-renamed");
            console.log("Bot renamed channel:", dataStr);
        });

        it("should verify #random channel is separate", async function() {
            var collection = await callMcpTool('get_subject_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel2Addr,
                collection_name: "messages",
            }, mcpSessionId);
            expect(collection.items).to.be.an('array');
            expect(collection.count).to.equal(1); // Only the original message
            expect(collection.items[0]).to.equal(msg3Addr);
            console.log("Bot verified #random has", collection.count, "message (separate from #general)");
        });
    });
});
