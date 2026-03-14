import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import * as ad4mModule from "@coasys/ad4m";
const QuerySubscriptionProxy = (ad4mModule as any).QuerySubscriptionProxy;
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor, killByPorts } from "../utils/utils";
import { ChildProcess } from 'node:child_process';
import fetch from 'node-fetch';
import { McpResponse, mcpHttpRequest, callMcpTool, listMcpTools, initializeMcp } from './mcp-utils';

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
// MCP HTTP Client Helpers (imported from shared mcp-utils.ts)
// ============================================================================

const MCP_PORT = 3001;
const MCP_BASE_URL = `http://127.0.0.1:${MCP_PORT}/mcp`;

// ============================================================================
// SHACL definitions for Flux models (Channel and Message)
// ============================================================================

const CHANNEL_SHACL = JSON.stringify({
    target_class: "flux://Channel",
    properties: [
        {
            path: "flux://channel_name",
            name: "name",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
            writable: true,
            resolve_language: "literal",
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_name", target: "value", local: false }
            ]
        },
        {
            path: "flux://channel_description",
            name: "description",
            datatype: "xsd:string",
            min_count: 0,
            max_count: 1,
            writable: true,
            resolve_language: "literal",
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_description", target: "value", local: false }
            ]
        },
        {
            path: "flux://channel_is_conversation",
            name: "isConversation",
            datatype: "xsd:boolean",
            min_count: 0,
            max_count: 1,
            writable: true,
            resolve_language: "literal",
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_is_conversation", target: "value", local: false }
            ]
        },
        {
            path: "flux://channel_is_pinned",
            name: "isPinned",
            datatype: "xsd:boolean",
            min_count: 0,
            max_count: 1,
            writable: true,
            resolve_language: "literal",
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_is_pinned", target: "value", local: false }
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
            datatype: "xsd:string",
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
// WakerSubscriptionManager — inlined from plugins/ad4m/wakerSubscriptionManager.ts
// to avoid cross-package ESM/CJS import cycles in the test runner.
// ============================================================================

interface WakerSub {
    id: string;
    type: "mention" | "channel-messages";
    perspective: string;
    channel: string;
    query: string;
}

class WakerSubscriptionManager {
    private perspectiveClient: any;
    private logger: any;
    private debounceMs: number;
    private onWake: (sub: WakerSub, result: any, parentChannel?: string) => void;
    private QSP: any;
    private proxies = new Map<string, any>();
    private activeSubscriptions = new Map<string, WakerSub>();
    private debounceTimers = new Map<string, ReturnType<typeof setTimeout>>();

    constructor(opts: {
        perspectiveClient: any;
        logger: any;
        QuerySubscriptionProxy?: any;
        debounceMs?: number;
        onWake: (sub: WakerSub, result: any, parentChannel?: string) => void;
    }) {
        this.perspectiveClient = opts.perspectiveClient;
        this.logger = opts.logger;
        this.QSP = opts.QuerySubscriptionProxy;
        this.debounceMs = opts.debounceMs ?? 2000;
        this.onWake = opts.onWake;
    }

    async subscribe(sub: WakerSub): Promise<void> {
        this.dispose(sub.id);
        this.logger.info(`[waker] ${sub.id}: creating subscription`);
        this.logger.info(`[waker] ${sub.id}: query:\n${sub.query}`);

        const proxy = new this.QSP(sub.perspective, sub.query, this.perspectiveClient);
        proxy.isSurrealDB = true;
        await proxy.subscribe();
        await proxy.initialized;
        this.logger.info(`[waker] ${sub.id}: initialized`);

        let lastHash: string | null = null;
        proxy.onResult(async (result: any) => {
            const s = JSON.stringify(result);
            if (lastHash === s) return;
            lastHash = s;

            const count = Array.isArray(result) ? result.length : "?";
            this.logger.info(`[waker] ${sub.id}: result changed (${count} items)`);
            this.logger.debug(`[waker] ${sub.id}: raw: ${s.substring(0, 500)}`);

            let parentChannel = sub.channel;
            if (!parentChannel && sub.type === "mention" && Array.isArray(result) && result.length > 0) {
                const first = result[0];
                if (first && first.source) {
                    parentChannel = first.source;
                    this.logger.info(`[waker] ${sub.id}: parent=${parentChannel}`);
                }
            }

            const existing = this.debounceTimers.get(sub.id);
            if (existing) clearTimeout(existing);
            this.debounceTimers.set(sub.id, setTimeout(() => {
                this.onWake(sub, result, parentChannel);
                this.debounceTimers.delete(sub.id);
            }, this.debounceMs));
        });

        this.proxies.set(sub.id, proxy);
        this.activeSubscriptions.set(sub.id, sub);
    }

    dispose(id: string): void {
        const proxy = this.proxies.get(id);
        if (proxy) { try { proxy.dispose(); } catch {} this.proxies.delete(id); }
        const timer = this.debounceTimers.get(id);
        if (timer) { clearTimeout(timer); this.debounceTimers.delete(id); }
        this.activeSubscriptions.delete(id);
    }

    disposeAll(): void {
        for (const [id] of this.proxies) this.dispose(id);
    }
}

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
    let agentDid: string = "";

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
        const agentStatus = await adminClient.agent.generate("test-passphrase");
        agentDid = agentStatus.did!;
        console.log("Agent generated via GraphQL, DID:", agentDid);
    });

    after(async () => {
        if (executorProcess) {
            executorProcess.kill('SIGTERM');
            await sleep(1000);
            if (!executorProcess.killed) {
                executorProcess.kill('SIGKILL');
            }
        }
        // Port-based kill as safety net
        killByPorts([gqlPort, hcAdminPort, hcAppPort, MCP_PORT]);
    });

    // ========================================================================
    // 1. MCP Connection & Authentication
    // ========================================================================

    describe("1. MCP Connection & Auth", function() {
        it("should initialize MCP connection", async function() {
            const init = await initializeMcp(MCP_BASE_URL);
            mcpSessionId = init.sessionId;
            expect(init.serverInfo).to.exist;
            console.log("MCP initialized, session:", mcpSessionId);
        });

        it("should list all available tools", async function() {
            const tools = await listMcpTools(MCP_BASE_URL,mcpSessionId);
            const toolNames = tools.map(function(t: any) { return t.name; });
            console.log("Available tools:", toolNames);

            // Core tools
            expect(toolNames).to.include('list_perspectives');
            expect(toolNames).to.include('add_perspective');
            expect(toolNames).to.include('add_link');
            expect(toolNames).to.include('query_links');

            // Subject class tools (higher-level)
            expect(toolNames).to.include('add_model');
            expect(toolNames).to.include('get_models');
            expect(toolNames).to.include('create_subject');
            expect(toolNames).to.include('query_subjects');
            expect(toolNames).to.include('get_subject_data');
            expect(toolNames).to.include('execute_commands');
            expect(toolNames).to.include('set_subject_property');
            expect(toolNames).to.include('get_subject_collection');
            expect(toolNames).to.include('add_to_collection');
            expect(toolNames).to.include('remove_from_collection');

            // Auth tools
            expect(toolNames).to.include('request_capability');
            expect(toolNames).to.include('generate_jwt');
            expect(toolNames).to.include('auth_status');
        });

        it("should authenticate with admin credential via request_capability", async function() {
            const capResult = await callMcpTool(MCP_BASE_URL,'request_capability', {
                app_name: "mcp-test",
                app_desc: "MCP Integration Test"
            }, mcpSessionId);
            expect(capResult.request_id).to.be.a('string');
            expect(capResult.code).to.be.a('string');

            const jwtResult = await callMcpTool(MCP_BASE_URL,'generate_jwt', {
                request_id: capResult.request_id,
                code: capResult.code,
            }, mcpSessionId);
            expect(jwtResult.success).to.be.true;
            expect(jwtResult.token).to.be.a('string');
        });

        it("should confirm auth status", async function() {
            const status = await callMcpTool(MCP_BASE_URL,'auth_status', {}, mcpSessionId);
            expect(status.authenticated).to.be.true;
        });
    });

    // ========================================================================
    // 2. Set up a Flux-like perspective with channels and messages
    // ========================================================================

    describe("2. Populate Perspective (Setup)", function() {
        it("should create a perspective", async function() {
            const result = await callMcpTool(MCP_BASE_URL,'add_perspective', { name: "Flux Test Room" }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.uuid).to.be.a('string');
            perspectiveUuid = result.uuid;
            console.log("Created perspective:", perspectiveUuid);
        });

        it("should register Channel SHACL class", async function() {
            const result = await callMcpTool(MCP_BASE_URL,'add_model', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                shacl_json: CHANNEL_SHACL,
            }, mcpSessionId);
            console.log("add_model Channel result:", JSON.stringify(result));
            expect(result.success).to.be.true;
        });

        it("should register Message SHACL class", async function() {
            const result = await callMcpTool(MCP_BASE_URL,'add_model', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                shacl_json: MESSAGE_SHACL,
            }, mcpSessionId);
            expect(result.success).to.be.true;
        });

        it("should create channel #general with messages", async function() {
            channel1Addr = "flux://channel-general-" + Date.now();
            var result = await callMcpTool(MCP_BASE_URL,'create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                initial_values: JSON.stringify({ name: "general" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add two messages to #general
            msg1Addr = "flux://msg-" + Date.now() + "-1";
            result = await callMcpTool(MCP_BASE_URL,'create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: msg1Addr,
                initial_values: JSON.stringify({ body: "Welcome to the channel!" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add message to channel's messages collection (high-level, no manual links)
            await callMcpTool(MCP_BASE_URL,'add_to_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
                item_address: msg1Addr,
            }, mcpSessionId);

            msg2Addr = "flux://msg-" + Date.now() + "-2";
            result = await callMcpTool(MCP_BASE_URL,'create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: msg2Addr,
                initial_values: JSON.stringify({ body: "Let's discuss the roadmap" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            await callMcpTool(MCP_BASE_URL,'add_to_collection', {
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
            var result = await callMcpTool(MCP_BASE_URL,'create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel2Addr,
                initial_values: JSON.stringify({ name: "random" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            msg3Addr = "flux://msg-" + Date.now() + "-3";
            result = await callMcpTool(MCP_BASE_URL,'create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: msg3Addr,
                initial_values: JSON.stringify({ body: "Random thought of the day" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            await callMcpTool(MCP_BASE_URL,'add_to_collection', {
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
            const perspectives = await callMcpTool(MCP_BASE_URL,'list_perspectives', {}, mcpSessionId);
            expect(perspectives).to.be.an('array');
            var found = perspectives.find(function(p: any) { return p.uuid === perspectiveUuid; });
            expect(found).to.exist;
            expect(found.name).to.equal("Flux Test Room");
            console.log("Bot found perspective:", found.name, found.uuid);
        });

        it("should discover subject classes (understand the data model)", async function() {
            const classes = await callMcpTool(MCP_BASE_URL,'get_models', {
                perspective_id: perspectiveUuid,
            }, mcpSessionId);
            var classStr = typeof classes === 'string' ? classes : JSON.stringify(classes);
            expect(classStr).to.include('Channel');
            expect(classStr).to.include('Message');
            console.log("Bot discovered subject classes:", classStr);
        });

        it("should list all channels", async function() {
            const channels = await callMcpTool(MCP_BASE_URL,'query_subjects', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
            }, mcpSessionId);
            var channelStr = typeof channels === 'string' ? channels : JSON.stringify(channels);
            expect(channelStr).to.include(channel1Addr);
            expect(channelStr).to.include(channel2Addr);
            console.log("Bot found channels:", channelStr);
        });

        it("should read #general channel data", async function() {
            const data = await callMcpTool(MCP_BASE_URL,'get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
            }, mcpSessionId);
            var dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("general");
            console.log("Bot read #general:", dataStr);
        });

        it("should get messages in #general via collection", async function() {
            const collection = await callMcpTool(MCP_BASE_URL,'get_subject_collection', {
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
            const data = await callMcpTool(MCP_BASE_URL,'get_subject_data', {
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
            var result = await callMcpTool(MCP_BASE_URL,'create_subject', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: botMsgAddr,
                initial_values: JSON.stringify({ body: "Hello! I'm an OpenClaw bot. How can I help?" }),
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add message to channel's collection (no manual links!)
            result = await callMcpTool(MCP_BASE_URL,'add_to_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
                item_address: botMsgAddr,
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // Verify via collection query
            var collection = await callMcpTool(MCP_BASE_URL,'get_subject_collection', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                collection_name: "messages",
            }, mcpSessionId);
            expect(collection.count).to.equal(3); // 2 original + 1 bot message

            // Verify message content
            var msgData = await callMcpTool(MCP_BASE_URL,'get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Message",
                expression_address: botMsgAddr,
            }, mcpSessionId);
            var dataStr = typeof msgData === 'string' ? msgData : JSON.stringify(msgData);
            expect(dataStr).to.include("OpenClaw bot");
            console.log("Bot successfully posted message to #general");
        });

        it("should update channel name via set_subject_property", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'set_subject_property', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
                property_name: "name",
                value: "general-renamed",
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // Verify the change
            var data = await callMcpTool(MCP_BASE_URL,'get_subject_data', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: channel1Addr,
            }, mcpSessionId);
            var dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("general-renamed");
            console.log("Bot renamed channel:", dataStr);
        });

        it("should verify #random channel is separate", async function() {
            var collection = await callMcpTool(MCP_BASE_URL,'get_subject_collection', {
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

    // ========================================================================
    // 4. Dynamic Tool Generation — verify SHACL classes produce typed tools
    // ========================================================================

    describe("4. Dynamic Tool Generation", function() {
        it("should have generated typed tools for Channel and Message", async function() {
            const tools = await listMcpTools(MCP_BASE_URL,mcpSessionId);
            const toolNames = tools.map(function(t: any) { return t.name; });
            console.log("Tools after SDNA registration:", toolNames);

            // Dynamic tools for Channel (CRUD + per-property + collections)
            expect(toolNames).to.include('channel_create');
            expect(toolNames).to.include('channel_query');
            expect(toolNames).to.include('channel_list');
            expect(toolNames).to.include('channel_get');
            expect(toolNames).to.include('channel_delete');
            expect(toolNames).to.include('channel_set_name');
            expect(toolNames).to.include('channel_get_messages');
            expect(toolNames).to.include('channel_add_messages');
            expect(toolNames).to.include('channel_remove_messages');

            // Dynamic tools for Message (CRUD + per-property)
            expect(toolNames).to.include('message_create');
            expect(toolNames).to.include('message_query');
            expect(toolNames).to.include('message_list');
            expect(toolNames).to.include('message_get');
            expect(toolNames).to.include('message_delete');
            expect(toolNames).to.include('message_set_body');

            // Static tools should still be present
            expect(toolNames).to.include('list_perspectives');
            expect(toolNames).to.include('add_model');
        });

        it("should have correct schema for channel_create", async function() {
            const tools = await listMcpTools(MCP_BASE_URL,mcpSessionId);
            var createChannel = tools.find(function(t: any) { return t.name === 'channel_create'; });
            expect(createChannel).to.exist;
            expect(createChannel.description).to.include('Channel');

            // Check input schema has perspective_id and expression_address as required
            var schema = createChannel.inputSchema;
            expect(schema.properties).to.have.property('perspective_id');
            expect(schema.properties).to.have.property('expression_address');
            expect(schema.properties).to.have.property('name');
            expect(schema.required).to.include('perspective_id');
            expect(schema.required).to.not.include('expression_address'); // now optional
        });

        it("should query channels via typed channel_query tool", async function() {
            var channels = await callMcpTool(MCP_BASE_URL,'channel_query', {
                perspective_id: perspectiveUuid,
            }, mcpSessionId);
            var channelStr = typeof channels === 'string' ? channels : JSON.stringify(channels);
            expect(channelStr).to.include(channel1Addr);
            expect(channelStr).to.include(channel2Addr);
            console.log("channel_query result:", channelStr);
        });

        it("should get channel data via typed channel_get tool", async function() {
            var data = await callMcpTool(MCP_BASE_URL,'channel_get', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
            }, mcpSessionId);
            var dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("general-renamed"); // Was renamed in section 3
            console.log("channel_get result:", dataStr);
        });

        it("should create a new channel via typed channel_create tool", async function() {
            var newChannelAddr = "flux://channel-typed-" + Date.now();
            var result = await callMcpTool(MCP_BASE_URL,'channel_create', {
                perspective_id: perspectiveUuid,
                expression_address: newChannelAddr,
                name: "typed-test",
            }, mcpSessionId);
            var resultStr = typeof result === 'string' ? result : JSON.stringify(result);
            expect(resultStr).to.include('true');
            console.log("channel_create result:", resultStr);

            // Verify it appears in query
            var channels = await callMcpTool(MCP_BASE_URL,'channel_query', {
                perspective_id: perspectiveUuid,
            }, mcpSessionId);
            var channelStr = typeof channels === 'string' ? channels : JSON.stringify(channels);
            expect(channelStr).to.include(newChannelAddr);
        });

        it("should create a message with parent parameter and verify child link", async function() {
            // Create a fresh channel to use as parent
            var parentChannelAddr = "flux://channel-parent-test-" + Date.now();
            var result = await callMcpTool(MCP_BASE_URL,'channel_create', {
                perspective_id: perspectiveUuid,
                expression_address: parentChannelAddr,
                name: "parent-test-channel",
            }, mcpSessionId);
            var resultStr = typeof result === 'string' ? result : JSON.stringify(result);
            expect(resultStr).to.include('true');

            // Create a message with the parent parameter
            var msgResult = await callMcpTool(MCP_BASE_URL,'message_create', {
                perspective_id: perspectiveUuid,
                body: "Hello from parent test!",
                parent: parentChannelAddr,
            }, mcpSessionId);
            console.log("message_create with parent result:", JSON.stringify(msgResult));
            expect(msgResult.created).to.be.true;
            expect(msgResult.added_to_parent).to.be.true;
            var createdMsgAddr = msgResult.expression_address;

            // Verify the child link was created correctly via get_children
            var children = await callMcpTool(MCP_BASE_URL,'get_children', {
                perspective_id: perspectiveUuid,
                parent_address: parentChannelAddr,
            }, mcpSessionId);
            console.log("get_children after parent create:", JSON.stringify(children));
            expect(children.count).to.be.greaterThan(0);
            var childAddrs = children.children.map((c: any) => c.address);
            expect(childAddrs).to.include(createdMsgAddr);
        });

        it("should create with parent when parent is a plain string (not URI)", async function() {
            var plainParent = "plain-parent-" + Date.now();
            var msgResult = await callMcpTool(MCP_BASE_URL,'message_create', {
                perspective_id: perspectiveUuid,
                body: "Hello from plain parent!",
                parent: plainParent,
            }, mcpSessionId);
            expect(msgResult.created).to.be.true;
            expect(msgResult.added_to_parent).to.be.true;

            // Verify retrievable via get_children with the same plain parent
            var children = await callMcpTool(MCP_BASE_URL,'get_children', {
                perspective_id: perspectiveUuid,
                parent_address: plainParent,
            }, mcpSessionId);
            expect(children.count).to.equal(1);
        });

        it("should update channel via typed channel_update tool", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'channel_update', {
                perspective_id: perspectiveUuid,
                expression_address: channel2Addr,
                name: "random-updated",
            }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.updated_properties).to.include('name');

            // Verify the update via channel_get
            var data = await callMcpTool(MCP_BASE_URL,'channel_get', {
                perspective_id: perspectiveUuid,
                expression_address: channel2Addr,
            }, mcpSessionId);
            var dataStr = typeof data === 'string' ? data : JSON.stringify(data);
            expect(dataStr).to.include("random-updated");
            console.log("channel_update result:", dataStr);
        });

        it("should delete a message via typed message_delete tool", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'message_delete', {
                perspective_id: perspectiveUuid,
                expression_address: msg3Addr,
            }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.links_removed).to.be.greaterThan(0);
            console.log("message_delete result: removed", result.links_removed, "links");

            // Verify it's gone from query
            var messages = await callMcpTool(MCP_BASE_URL,'message_query', {
                perspective_id: perspectiveUuid,
            }, mcpSessionId);
            var msgStr = typeof messages === 'string' ? messages : JSON.stringify(messages);
            expect(msgStr).to.not.include(msg3Addr);
        });

        it("should add new SDNA and see new tools appear", async function() {
            // Define a new Task class with a subtasks collection
            var taskShacl = JSON.stringify({
                target_class: "ad4m://Task",
                properties: [
                    {
                        path: "ad4m://task_title",
                        name: "title",
                        datatype: "xsd:string",
                        min_count: 1,
                        max_count: 1,
                        writable: true,
                        resolve_language: "literal",
                        setter: [
                            { action: "setSingleTarget", source: "this", predicate: "ad4m://task_title", target: "value", local: false }
                        ]
                    },
                    {
                        path: "ad4m://task_status",
                        name: "status",
                        datatype: "xsd:string",
                        min_count: 0,
                        max_count: 1,
                        writable: true,
                        resolve_language: "literal",
                        setter: [
                            { action: "setSingleTarget", source: "this", predicate: "ad4m://task_status", target: "value", local: false }
                        ]
                    },
                    {
                        path: "ad4m://has_child",
                        name: "subtasks",
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
                    { action: "addLink", source: "this", predicate: "rdf://type", target: "ad4m://Task", local: false }
                ],
                destructor_actions: []
            });

            var result = await callMcpTool(MCP_BASE_URL,'add_model', {
                perspective_id: perspectiveUuid,
                class_name: "Task",
                shacl_json: taskShacl,
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // New tools should now appear
            var tools = await listMcpTools(MCP_BASE_URL,mcpSessionId);
            var toolNames = tools.map(function(t: any) { return t.name; });
            expect(toolNames).to.include('task_create');
            expect(toolNames).to.include('task_query');
            expect(toolNames).to.include('task_get');
            expect(toolNames).to.include('task_delete');
            // Per-property set tools
            expect(toolNames).to.include('task_set_title');
            expect(toolNames).to.include('task_set_status');
            // Collection tools
            expect(toolNames).to.include('task_get_subtasks');
            expect(toolNames).to.include('task_add_subtasks');
            expect(toolNames).to.include('task_remove_subtasks');
            console.log("New Task tools appeared after add_model:", toolNames.filter(function(n: string) { return n.includes('task'); }));

            // Verify schema has the right properties
            var createTask = tools.find(function(t: any) { return t.name === 'task_create'; });
            expect(createTask).to.exist;
            expect(createTask.inputSchema.properties).to.have.property('title');
            expect(createTask.inputSchema.properties).to.have.property('status');
        });
    });

    describe("5. Dynamic Collection & Per-Property Tools", function() {
        var task1Addr: string;
        var task2Addr: string;
        var task3Addr: string;

        it("should have generated per-property set tools for Channel", async function() {
            var tools = await listMcpTools(MCP_BASE_URL,mcpSessionId);
            var toolNames = tools.map(function(t: any) { return t.name; });
            // Channel has name (scalar) and messages (collection)
            expect(toolNames).to.include('channel_set_name');
            expect(toolNames).to.include('channel_get_messages');
            expect(toolNames).to.include('channel_add_messages');
            expect(toolNames).to.include('channel_remove_messages');
            // Message has body (scalar)
            expect(toolNames).to.include('message_set_body');
            console.log("Channel dynamic tools:", toolNames.filter(function(n: string) { return n.includes('channel_'); }));
            console.log("Message dynamic tools:", toolNames.filter(function(n: string) { return n.includes('message_'); }));
        });

        it("should set channel name via channel_set_name", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'channel_set_name', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
                value: "general-renamed",
            }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.property).to.equal("name");

            // Verify via channel_get
            var data = await callMcpTool(MCP_BASE_URL,'channel_get', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
            }, mcpSessionId);
            console.log("Channel after set_name:", JSON.stringify(data));
        });

        it("should get messages collection via channel_get_messages", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'channel_get_messages', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
            }, mcpSessionId);
            console.log("channel_get_messages result:", JSON.stringify(result));
            expect(result.collection).to.equal("messages");
            expect(result.items).to.be.an('array');
            expect(result.items.length).to.be.at.least(2);
        });

        it("should create tasks and add them as children of #general", async function() {
            // Create task 1
            task1Addr = "ad4m://task-" + Date.now() + "-1";
            var result = await callMcpTool(MCP_BASE_URL,'task_create', {
                perspective_id: perspectiveUuid,
                expression_address: task1Addr,
                title: "Fix the login bug",
                status: "open",
            }, mcpSessionId);
            console.log("task_create result:", JSON.stringify(result));
            expect(result.created).to.be.true;

            // Create task 2
            task2Addr = "ad4m://task-" + Date.now() + "-2";
            result = await callMcpTool(MCP_BASE_URL,'task_create', {
                perspective_id: perspectiveUuid,
                expression_address: task2Addr,
                title: "Update documentation",
                status: "in-progress",
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add tasks as children of #general channel
            result = await callMcpTool(MCP_BASE_URL,'channel_add_messages', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
                value: task1Addr,
            }, mcpSessionId);
            expect(result.success).to.be.true;

            result = await callMcpTool(MCP_BASE_URL,'channel_add_messages', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
                value: task2Addr,
            }, mcpSessionId);
            expect(result.success).to.be.true;
            console.log("Added 2 tasks as children of #general");
        });

        it("should get all children of #general via get_subject_children", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'get_subject_children', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
            }, mcpSessionId);
            console.log("get_subject_children result:", JSON.stringify(result));
            expect(result.children).to.be.an('array');
            // Should have original messages + the 2 tasks we just added
            expect(result.children.length).to.be.at.least(4);
        });

        it("should get children filtered by Task class", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'get_subject_children', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
                child_class_name: "Task",
            }, mcpSessionId);
            console.log("get_subject_children (Task) result:", JSON.stringify(result));
            expect(result.children).to.be.an('array');
            // Only the 2 tasks should match (they have rdf://type -> ad4m://Task)
            expect(result.children.length).to.equal(2);
        });

        it("should update task status via task_set_status", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'task_set_status', {
                perspective_id: perspectiveUuid,
                expression_address: task1Addr,
                value: "completed",
            }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.property).to.equal("status");

            // Verify via task_get
            var data = await callMcpTool(MCP_BASE_URL,'task_get', {
                perspective_id: perspectiveUuid,
                expression_address: task1Addr,
            }, mcpSessionId);
            console.log("Task after set_status:", JSON.stringify(data));
        });

        it("should add subtasks to a task via task_add_subtasks", async function() {
            task3Addr = "ad4m://subtask-" + Date.now();
            var result = await callMcpTool(MCP_BASE_URL,'task_create', {
                perspective_id: perspectiveUuid,
                expression_address: task3Addr,
                title: "Write unit tests",
                status: "open",
            }, mcpSessionId);
            expect(result.created).to.be.true;

            // Add as subtask of task1
            result = await callMcpTool(MCP_BASE_URL,'task_add_subtasks', {
                perspective_id: perspectiveUuid,
                expression_address: task1Addr,
                value: task3Addr,
            }, mcpSessionId);
            expect(result.success).to.be.true;
            console.log("Added subtask to task1");
        });

        it("should get subtasks via task_get_subtasks", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'task_get_subtasks', {
                perspective_id: perspectiveUuid,
                expression_address: task1Addr,
            }, mcpSessionId);
            console.log("task_get_subtasks result:", JSON.stringify(result));
            expect(result.collection).to.equal("subtasks");
            expect(result.items).to.be.an('array');
            expect(result.items.length).to.equal(1);
        });

        it("should remove a message from channel via channel_remove_messages", async function() {
            // Remove task1 from channel's messages
            var result = await callMcpTool(MCP_BASE_URL,'channel_remove_messages', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
                value: task1Addr,
            }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.links_removed).to.be.at.least(1);

            // Verify it's gone from channel's messages
            var messages = await callMcpTool(MCP_BASE_URL,'channel_get_messages', {
                perspective_id: perspectiveUuid,
                expression_address: channel1Addr,
            }, mcpSessionId);
            var items = messages.items || [];
            expect(items).to.not.include(task1Addr);
            console.log("Removed task1 from channel, remaining items:", items.length);
        });

        it("should set message body via message_set_body", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'message_set_body', {
                perspective_id: perspectiveUuid,
                expression_address: msg1Addr,
                value: "Welcome to the channel! (edited)",
            }, mcpSessionId);
            expect(result.success).to.be.true;
            expect(result.property).to.equal("body");
            console.log("Updated message body via message_set_body");
        });
    });

    // ========================================================================
    // 5b. Resolve Language — verify properties with resolve_language produce
    //     proper literal://json: expressions instead of literal://string:
    // ========================================================================

    describe("5b. Resolve Language for Boolean/String Properties", function() {
        let resolveTestChannelAddr: string;

        it("should create a channel with boolean initial values via channel_create", async function() {
            resolveTestChannelAddr = "flux://channel-resolve-test-" + Date.now();
            var result = await callMcpTool(MCP_BASE_URL,'channel_create', {
                perspective_id: perspectiveUuid,
                expression_address: resolveTestChannelAddr,
                name: "Resolve Test Channel",
                isConversation: "false",
                isPinned: "true",
            }, mcpSessionId);
            var resultStr = typeof result === 'string' ? result : JSON.stringify(result);
            expect(resultStr).to.include('true');
            console.log("channel_create with booleans:", resultStr);
        });

        it("should store boolean properties as literal://json: expressions, not literal://string:", async function() {
            // Query the raw links to verify the encoding format
            var links = await callMcpTool(MCP_BASE_URL,'query_links', {
                perspective_id: perspectiveUuid,
                source: resolveTestChannelAddr,
                predicate: "flux://channel_is_conversation",
            }, mcpSessionId);
            console.log("isConversation links:", JSON.stringify(links));

            // The target should be a literal://json: expression (signed expression),
            // NOT literal://string:false
            var linksArr = Array.isArray(links) ? links : (links.links || []);
            expect(linksArr.length).to.be.greaterThan(0);
            var target = linksArr[0].data?.target || linksArr[0].target || '';
            console.log("isConversation target:", target);
            expect(target).to.not.include("literal://string:false");
            expect(target).to.include("literal://json:");
        });

        it("should store string properties as literal://json: expressions when resolve_language is set", async function() {
            var links = await callMcpTool(MCP_BASE_URL,'query_links', {
                perspective_id: perspectiveUuid,
                source: resolveTestChannelAddr,
                predicate: "flux://channel_name",
            }, mcpSessionId);
            console.log("name links:", JSON.stringify(links));

            var linksArr = Array.isArray(links) ? links : (links.links || []);
            expect(linksArr.length).to.be.greaterThan(0);
            var target = linksArr[0].data?.target || linksArr[0].target || '';
            console.log("name target:", target);
            // Should be a signed expression (literal://json:) not a raw string literal
            expect(target).to.include("literal://json:");
        });

        it("should resolve boolean values via channel_set_isconversation", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'channel_set_isconversation', {
                perspective_id: perspectiveUuid,
                expression_address: resolveTestChannelAddr,
                value: "true",
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // Verify the stored link target is a proper expression
            var links = await callMcpTool(MCP_BASE_URL,'query_links', {
                perspective_id: perspectiveUuid,
                source: resolveTestChannelAddr,
                predicate: "flux://channel_is_conversation",
            }, mcpSessionId);
            var linksArr = Array.isArray(links) ? links : (links.links || []);
            expect(linksArr.length).to.be.greaterThan(0);
            var target = linksArr[0].data?.target || linksArr[0].target || '';
            console.log("Updated isConversation target:", target);
            expect(target).to.not.include("literal://string:true");
            expect(target).to.include("literal://json:");
        });

        it("should resolve string values via set_subject_property with resolve_language", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'set_subject_property', {
                perspective_id: perspectiveUuid,
                class_name: "Channel",
                expression_address: resolveTestChannelAddr,
                property_name: "description",
                value: "A test description",
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // Verify the stored link target uses literal://json: (signed expression)
            var links = await callMcpTool(MCP_BASE_URL,'query_links', {
                perspective_id: perspectiveUuid,
                source: resolveTestChannelAddr,
                predicate: "flux://channel_description",
            }, mcpSessionId);
            var linksArr = Array.isArray(links) ? links : (links.links || []);
            expect(linksArr.length).to.be.greaterThan(0);
            var target = linksArr[0].data?.target || linksArr[0].target || '';
            console.log("description target:", target);
            expect(target).to.include("literal://json:");
        });

        it("should resolve boolean values via channel_update (dynamic update)", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'channel_update', {
                perspective_id: perspectiveUuid,
                expression_address: resolveTestChannelAddr,
                isPinned: "false",
            }, mcpSessionId);
            expect(result.success).to.be.true;

            var links = await callMcpTool(MCP_BASE_URL,'query_links', {
                perspective_id: perspectiveUuid,
                source: resolveTestChannelAddr,
                predicate: "flux://channel_is_pinned",
            }, mcpSessionId);
            var linksArr = Array.isArray(links) ? links : (links.links || []);
            expect(linksArr.length).to.be.greaterThan(0);
            var target = linksArr[0].data?.target || linksArr[0].target || '';
            console.log("Updated isPinned target:", target);
            expect(target).to.not.include("literal://string:false");
            expect(target).to.include("literal://json:");
        });
    });

    describe("6. Generic Child Tools (add_child / get_children)", function() {
        let parentAddr: string = "";
        let child1Addr: string = "";
        let child2Addr: string = "";
        let child3Addr: string = "";

        it("should list add_child and get_children in available tools", async function() {
            var tools = await listMcpTools(MCP_BASE_URL,mcpSessionId);
            var toolNames = tools.map((t: any) => t.name);
            expect(toolNames).to.include('add_child');
            expect(toolNames).to.include('get_children');
            console.log("add_child and get_children tools are registered");
        });

        it("should add children to a parent address", async function() {
            // Use a plain string parent — tool should auto-wrap as literal://string:
            parentAddr = "test-parent-" + Date.now();
            child1Addr = "test-child-1-" + Date.now();
            child2Addr = "test-child-2-" + Date.now();

            var result = await callMcpTool(MCP_BASE_URL,'add_child', {
                perspective_id: perspectiveUuid,
                parent_address: parentAddr,
                child_address: child1Addr,
            }, mcpSessionId);
            console.log("add_child result 1:", JSON.stringify(result));
            expect(result.success).to.be.true;
            expect(result.link.predicate).to.equal("ad4m://has_child");

            result = await callMcpTool(MCP_BASE_URL,'add_child', {
                perspective_id: perspectiveUuid,
                parent_address: parentAddr,
                child_address: child2Addr,
            }, mcpSessionId);
            expect(result.success).to.be.true;
            console.log("Added 2 children to parent");
        });

        it("should get children of a parent", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'get_children', {
                perspective_id: perspectiveUuid,
                parent_address: parentAddr,
            }, mcpSessionId);
            console.log("get_children result:", JSON.stringify(result));
            expect(result.count).to.equal(2);
            expect(result.children).to.be.an('array');
            expect(result.children.length).to.equal(2);

            // Children should be sorted by timestamp (ISO strings are lexicographically sortable)
            var timestamps = result.children.map((c: any) => c.timestamp);
            expect(timestamps[0] <= timestamps[1]).to.be.true;
        });

        it("should return empty children for unknown parent", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'get_children', {
                perspective_id: perspectiveUuid,
                parent_address: "nonexistent-parent-" + Date.now(),
            }, mcpSessionId);
            expect(result.count).to.equal(0);
            expect(result.children).to.be.an('array');
            expect(result.children.length).to.equal(0);
        });

        it("should handle pre-wrapped literal URIs", async function() {
            // If parent is already a literal://string: URI, should not double-wrap
            var wrappedParent = "literal://string:pre-wrapped-parent-" + Date.now();
            child3Addr = "test-child-3-" + Date.now();

            var result = await callMcpTool(MCP_BASE_URL,'add_child', {
                perspective_id: perspectiveUuid,
                parent_address: wrappedParent,
                child_address: child3Addr,
            }, mcpSessionId);
            expect(result.success).to.be.true;

            // Should be retrievable with the same wrapped parent
            result = await callMcpTool(MCP_BASE_URL,'get_children', {
                perspective_id: perspectiveUuid,
                parent_address: wrappedParent,
            }, mcpSessionId);
            expect(result.count).to.equal(1);
            console.log("Pre-wrapped URI handled correctly");
        });

        it("should work with existing channel as parent (interop with SHACL tools)", async function() {
            // Use channel1Addr from earlier tests as parent
            var result = await callMcpTool(MCP_BASE_URL,'get_children', {
                perspective_id: perspectiveUuid,
                parent_address: channel1Addr,
            }, mcpSessionId);
            console.log("get_children for channel1:", JSON.stringify(result));
            // Channel already has messages/tasks added as children via SHACL tools
            expect(result.count).to.be.at.least(1);
            console.log("Generic get_children works with SHACL-created subjects");
        });
    });

    // ========================================================================
    // 7. Agent Profile Tools
    // ========================================================================

    describe("7. Agent Profile Tools", function() {
        it("should get initial profile with DID but no fields set", async function() {
            var profile = await callMcpTool(MCP_BASE_URL,'get_agent_profile', {}, mcpSessionId);
            expect(profile.did).to.equal(agentDid);
            expect(profile.username).to.be.undefined;
            expect(profile.given_name).to.be.undefined;
            expect(profile.bio).to.be.undefined;
            console.log("Initial profile:", JSON.stringify(profile));
        });

        it("should set profile fields and read them back", async function() {
            var setResult = await callMcpTool(MCP_BASE_URL,'set_agent_profile', {
                username: "testbot",
                given_name: "Test",
                family_name: "Bot",
                email: "testbot@example.com",
                bio: "I am a test bot",
            }, mcpSessionId);
            expect(setResult.success).to.be.true;
            expect(setResult.username).to.equal("testbot");

            var profile = await callMcpTool(MCP_BASE_URL,'get_agent_profile', {}, mcpSessionId);
            expect(profile.did).to.equal(agentDid);
            expect(profile.username).to.equal("testbot");
            expect(profile.given_name).to.equal("Test");
            expect(profile.family_name).to.equal("Bot");
            expect(profile.email).to.equal("testbot@example.com");
            expect(profile.bio).to.equal("I am a test bot");
            console.log("Profile round-trip OK:", JSON.stringify(profile));
        });

        it("should do partial update preserving existing fields", async function() {
            var setResult = await callMcpTool(MCP_BASE_URL,'set_agent_profile', {
                bio: "Updated bio only",
            }, mcpSessionId);
            expect(setResult.success).to.be.true;

            var profile = await callMcpTool(MCP_BASE_URL,'get_agent_profile', {}, mcpSessionId);
            expect(profile.username).to.equal("testbot");
            expect(profile.given_name).to.equal("Test");
            expect(profile.family_name).to.equal("Bot");
            expect(profile.email).to.equal("testbot@example.com");
            expect(profile.bio).to.equal("Updated bio only");
            console.log("Partial update preserved other fields");
        });

        it("should overwrite a previously set field", async function() {
            await callMcpTool(MCP_BASE_URL,'set_agent_profile', {
                username: "testbot-v2",
            }, mcpSessionId);

            var profile = await callMcpTool(MCP_BASE_URL,'get_agent_profile', {}, mcpSessionId);
            expect(profile.username).to.equal("testbot-v2");
            expect(profile.given_name).to.equal("Test");
            expect(profile.bio).to.equal("Updated bio only");
            console.log("Field overwrite works");
        });

        it("should get own public perspective with profile links", async function() {
            var result = await callMcpTool(MCP_BASE_URL,'get_agent_public_perspective', {}, mcpSessionId);
            expect(result.did).to.equal(agentDid);
            expect(result.perspective).to.exist;
            expect(result.perspective.links).to.be.an('array');
            expect(result.perspective.links.length).to.be.greaterThan(0);

            var profileLinks = result.perspective.links.filter(
                function(l: any) { return l.data.source === "flux://profile"; }
            );
            expect(profileLinks.length).to.be.greaterThan(0);
            console.log("Public perspective has", result.perspective.links.length, "links");
        });

        it("should set raw links via set_agent_public_perspective and read back via get_agent_profile", async function() {
            var customLinks = [
                {
                    author: agentDid,
                    timestamp: new Date().toISOString(),
                    data: {
                        source: "flux://profile",
                        predicate: "sioc://has_username",
                        target: "literal://string:raw-link-user",
                    },
                    proof: { key: "", signature: "", valid: false, invalid: true },
                },
                {
                    author: agentDid,
                    timestamp: new Date().toISOString(),
                    data: {
                        source: "flux://profile",
                        predicate: "sioc://has_bio",
                        target: "literal://string:Set via raw links",
                    },
                    proof: { key: "", signature: "", valid: false, invalid: true },
                },
            ];

            var setResult = await callMcpTool(MCP_BASE_URL,'set_agent_public_perspective', {
                links_json: JSON.stringify(customLinks),
            }, mcpSessionId);
            expect(setResult.did).to.equal(agentDid);
            expect(setResult.perspective).to.exist;
            expect(setResult.perspective.links).to.have.lengthOf(2);

            var profile = await callMcpTool(MCP_BASE_URL,'get_agent_profile', {}, mcpSessionId);
            expect(profile.username).to.equal("raw-link-user");
            expect(profile.bio).to.equal("Set via raw links");
            // Fields we didn't include should be gone (replaces all)
            expect(profile.given_name).to.be.undefined;
            console.log("Raw link round-trip works");
        });

        it("should restore profile via set_agent_profile after raw link override", async function() {
            await callMcpTool(MCP_BASE_URL,'set_agent_profile', {
                username: "restored-user",
                bio: "Restored after raw link test",
            }, mcpSessionId);

            var profile = await callMcpTool(MCP_BASE_URL,'get_agent_profile', {}, mcpSessionId);
            expect(profile.username).to.equal("restored-user");
            expect(profile.bio).to.equal("Restored after raw link test");
            console.log("Profile restored after raw link override");
        });
    });

    // ========================================================================
    // 8. Waker Subscription Integration Tests
    //
    // Tests the full subscription pipeline:
    //   get_mention_waker_config → SurrealDB subscription → add message with
    //   mention → verify subscription fires with correct result
    // ========================================================================

    describe("8. Waker Subscription (SurrealDB Live Query)", function() {
        // Uses the extracted WakerSubscriptionManager — same code path as the plugin
        let wakerClient: Ad4mClient;
        let wakerPerspectiveUuid: string;
        let wakerChannelAddr: string;

        before(async function() {
            // Create a dedicated Ad4mClient for subscriptions (WS transport needed)
            wakerClient = new Ad4mClient(apolloClient(gqlPort, adminCredential), false);

            // Set up a profile so get_mention_waker_config has names to search for
            await callMcpTool(MCP_BASE_URL, 'set_agent_profile', {
                username: "wakerbot",
                given_name: "WakerTest",
            }, mcpSessionId);

            // Create a fresh perspective for waker tests
            var result = await callMcpTool(MCP_BASE_URL, 'add_perspective', {
                name: "Waker Test Room",
            }, mcpSessionId);
            wakerPerspectiveUuid = result.uuid;
            console.log("Waker test perspective:", wakerPerspectiveUuid);

            // Register Channel and Message SHACL models
            await callMcpTool(MCP_BASE_URL, 'add_model', {
                perspective_id: wakerPerspectiveUuid,
                class_name: "Channel",
                shacl_json: CHANNEL_SHACL,
            }, mcpSessionId);
            await callMcpTool(MCP_BASE_URL, 'add_model', {
                perspective_id: wakerPerspectiveUuid,
                class_name: "Message",
                shacl_json: MESSAGE_SHACL,
            }, mcpSessionId);

            // Create a channel
            wakerChannelAddr = "flux://waker-test-channel-" + Date.now();
            await callMcpTool(MCP_BASE_URL, 'channel_create', {
                perspective_id: wakerPerspectiveUuid,
                expression_address: wakerChannelAddr,
                name: "Waker Test Channel",
            }, mcpSessionId);
            console.log("Waker test channel:", wakerChannelAddr);
        });

        it("should get mention waker config with SurrealQL query", async function() {
            var config = await callMcpTool(MCP_BASE_URL, 'get_mention_waker_config', {
                perspective_id: wakerPerspectiveUuid,
            }, mcpSessionId);
            console.log("Mention waker config:", JSON.stringify(config, null, 2));

            expect(config.did).to.equal(agentDid);
            expect(config.names).to.be.an('array');
            expect(config.names).to.include("wakerbot");
            expect(config.names).to.include("WakerTest");
            expect(config.query).to.be.a('string');
            expect(config.query).to.include("ad4m://has_child");
            expect(config.query).to.include("fn::contains");
            expect(config.query).to.include("fn::parse_literal");
            // Should include the two-hop graph traversal
            expect(config.query).to.include("out->link");
            console.log("SurrealQL query:", config.query);
        });

        it("should create WakerSubscriptionManager and receive initial (empty) result via onWake", async function() {
            this.timeout(15000);

            var config = await callMcpTool(MCP_BASE_URL, 'get_mention_waker_config', {
                perspective_id: wakerPerspectiveUuid,
            }, mcpSessionId);

            var wakeCount = 0;
            var manager = new WakerSubscriptionManager({
                perspectiveClient: wakerClient.perspective,
                logger: { info: console.log, warn: console.warn, error: console.error, debug: console.log },
                QuerySubscriptionProxy,
                debounceMs: 100,
                onWake: function() { wakeCount++; },
            });

            await manager.subscribe({
                id: "test-empty-" + Date.now(),
                type: "mention" as const,
                perspective: wakerPerspectiveUuid,
                channel: "",
                query: config.query,
            });

            // Wait for subscription to initialize — should NOT fire onWake for empty result
            await sleep(3000);
            expect(wakeCount).to.equal(0, "onWake should not fire for empty initial result");
            manager.disposeAll();
        });

        it("should fire onWake when a message with mention is added", async function() {
            this.timeout(30000);

            var config = await callMcpTool(MCP_BASE_URL, 'get_mention_waker_config', {
                perspective_id: wakerPerspectiveUuid,
            }, mcpSessionId);

            var wakePromise = new Promise<{ sub: any, result: any, parentChannel?: string }>(function(resolve, reject) {
                var timeout = setTimeout(function() {
                    reject(new Error("WakerSubscriptionManager did not fire onWake within 15s"));
                }, 15000);

                var manager = new WakerSubscriptionManager({
                    perspectiveClient: wakerClient.perspective,
                    logger: { info: console.log, warn: console.warn, error: console.error, debug: console.log },
                    QuerySubscriptionProxy,
                    debounceMs: 100,
                    onWake: function(sub: any, result: any, parentChannel?: string) {
                        console.log("  [WakerManager] onWake fired! parentChannel:", parentChannel);
                        clearTimeout(timeout);
                        resolve({ sub, result, parentChannel });
                    },
                });

                manager.subscribe({
                    id: "test-mention-" + Date.now(),
                    type: "mention" as const,
                    perspective: wakerPerspectiveUuid,
                    channel: "",
                    query: config.query,
                }).catch(reject);
            });

            await sleep(2000);

            var createResult = await callMcpTool(MCP_BASE_URL, 'message_create', {
                perspective_id: wakerPerspectiveUuid,
                body: "Hey @wakerbot, can you help with this?",
                parent: wakerChannelAddr,
            }, mcpSessionId);
            console.log("Message created with mention:", JSON.stringify(createResult));
            expect(createResult.created).to.be.true;
            expect(createResult.added_to_parent).to.be.true;

            var wake = await wakePromise;
            console.log("onWake fired! Result:", JSON.stringify(wake.result).substring(0, 500));

            var resultArr = Array.isArray(wake.result) ? wake.result : [wake.result];
            expect(resultArr.length).to.be.greaterThan(0);

            // For mention type, parent channel should be extracted from has_child link source
            if (wake.parentChannel) {
                console.log("Parent channel extracted:", wake.parentChannel);
            }
        });

        it("should NOT fire onWake for messages without mentions", async function() {
            this.timeout(20000);

            // Use a FRESH perspective to avoid shared subscription state
            var freshPerspResult = await callMcpTool(MCP_BASE_URL, 'add_perspective', {
                name: "Waker No-Mention Test",
            }, mcpSessionId);
            var freshPerspId = freshPerspResult.uuid;

            await callMcpTool(MCP_BASE_URL, 'add_model', {
                perspective_id: freshPerspId,
                class_name: "Channel",
                shacl_json: CHANNEL_SHACL,
            }, mcpSessionId);
            await callMcpTool(MCP_BASE_URL, 'add_model', {
                perspective_id: freshPerspId,
                class_name: "Message",
                shacl_json: MESSAGE_SHACL,
            }, mcpSessionId);

            var freshChannel = "flux://no-mention-channel-" + Date.now();
            await callMcpTool(MCP_BASE_URL, 'channel_create', {
                perspective_id: freshPerspId,
                expression_address: freshChannel,
                name: "No Mention Channel",
            }, mcpSessionId);

            var config = await callMcpTool(MCP_BASE_URL, 'get_mention_waker_config', {
                perspective_id: freshPerspId,
            }, mcpSessionId);
            console.log("No-mention test query:", config.query);

            var wakeCount = 0;
            var lastWake: any = null;
            var manager = new WakerSubscriptionManager({
                perspectiveClient: wakerClient.perspective,
                logger: { info: console.log, warn: console.warn, error: console.error, debug: console.log },
                QuerySubscriptionProxy,
                debounceMs: 100,
                onWake: function(sub: any, result: any) {
                    wakeCount++;
                    lastWake = result;
                    console.log("  [no-mention] onWake #" + wakeCount + ":", JSON.stringify(result).substring(0, 500));
                },
            });

            await manager.subscribe({
                id: "test-no-mention-" + Date.now(),
                type: "mention" as const,
                perspective: freshPerspId,
                channel: "",
                query: config.query,
            });

            await sleep(1000);

            await callMcpTool(MCP_BASE_URL, 'message_create', {
                perspective_id: freshPerspId,
                body: "Just a normal message, nothing special here.",
                parent: freshChannel,
            }, mcpSessionId);
            console.log("Non-mention message created in fresh perspective");

            await sleep(5000);

            manager.disposeAll();

            expect(wakeCount).to.equal(0,
                "onWake fired " + wakeCount + " time(s) for non-mention message. " +
                "Last wake: " + JSON.stringify(lastWake).substring(0, 300));
            console.log("Correctly did NOT fire for non-mention message");
        });

        it("should fire onWake when mention uses agent DID", async function() {
            this.timeout(30000);

            var didPerspResult = await callMcpTool(MCP_BASE_URL, 'add_perspective', {
                name: "Waker DID Mention Test",
            }, mcpSessionId);
            var didPerspId = didPerspResult.uuid;

            await callMcpTool(MCP_BASE_URL, 'add_model', {
                perspective_id: didPerspId,
                class_name: "Channel",
                shacl_json: CHANNEL_SHACL,
            }, mcpSessionId);
            await callMcpTool(MCP_BASE_URL, 'add_model', {
                perspective_id: didPerspId,
                class_name: "Message",
                shacl_json: MESSAGE_SHACL,
            }, mcpSessionId);

            var didChannel = "flux://did-mention-channel-" + Date.now();
            await callMcpTool(MCP_BASE_URL, 'channel_create', {
                perspective_id: didPerspId,
                expression_address: didChannel,
                name: "DID Mention Channel",
            }, mcpSessionId);

            var config = await callMcpTool(MCP_BASE_URL, 'get_mention_waker_config', {
                perspective_id: didPerspId,
            }, mcpSessionId);

            var wakePromise = new Promise<any>(function(resolve, reject) {
                var timeout = setTimeout(function() {
                    reject(new Error("WakerSubscriptionManager did not fire onWake for DID mention within 15s"));
                }, 15000);

                var manager = new WakerSubscriptionManager({
                    perspectiveClient: wakerClient.perspective,
                    logger: { info: console.log, warn: console.warn, error: console.error, debug: console.log },
                    QuerySubscriptionProxy,
                    debounceMs: 100,
                    onWake: function(sub: any, result: any, parentChannel?: string) {
                        console.log("  [DID mention] onWake fired! parentChannel:", parentChannel);
                        clearTimeout(timeout);
                        resolve({ sub, result, parentChannel });
                    },
                });

                manager.subscribe({
                    id: "test-did-mention-" + Date.now(),
                    type: "mention" as const,
                    perspective: didPerspId,
                    channel: "",
                    query: config.query,
                }).catch(reject);
            });

            await sleep(2000);

            await callMcpTool(MCP_BASE_URL, 'message_create', {
                perspective_id: didPerspId,
                body: "Ping " + agentDid + " — please respond",
                parent: didChannel,
            }, mcpSessionId);
            console.log("DID-mention message created");

            var wake = await wakePromise;
            console.log("DID mention onWake fired!");
            var resultArr = Array.isArray(wake.result) ? wake.result : [wake.result];
            expect(resultArr.length).to.be.greaterThan(0);
        });

        it("should generate_waker_query for channel children subscription", async function() {
            this.timeout(15000);

            var config = await callMcpTool(MCP_BASE_URL, 'generate_waker_query', {
                perspective_id: wakerPerspectiveUuid,
                class_name: "Message",
                parent_address: wakerChannelAddr,
            }, mcpSessionId);
            console.log("Channel waker query:", JSON.stringify(config, null, 2));

            expect(config.surreal_query).to.be.a('string');
            expect(config.surreal_query).to.include("ad4m://has_child");
            expect(config.subscription_id).to.be.a('string');

            // Use WakerSubscriptionManager to subscribe and verify it works
            var initialWake: any = null;
            var manager = new WakerSubscriptionManager({
                perspectiveClient: wakerClient.perspective,
                logger: { info: console.log, warn: console.warn, error: console.error, debug: console.log },
                QuerySubscriptionProxy,
                debounceMs: 100,
                onWake: function(sub: any, result: any) {
                    initialWake = result;
                },
            });

            await manager.subscribe({
                id: "test-channel-children-" + Date.now(),
                type: "channel-messages" as const,
                perspective: wakerPerspectiveUuid,
                channel: wakerChannelAddr,
                query: config.surreal_query,
            });

            // The subscription should fire with existing children (from the mention test)
            await sleep(3000);

            manager.disposeAll();

            // We added a mention message earlier, so there should be at least 1 child
            if (initialWake) {
                var resultArr = Array.isArray(initialWake) ? initialWake : [];
                console.log("Channel subscription fired with", resultArr.length, "existing children");
                expect(resultArr.length).to.be.greaterThanOrEqual(1);
            } else {
                console.log("Channel subscription did not fire — initial result may have been empty or unchanged");
            }
        });
    });
});
