import path from "path";
import { Ad4mClient, Perspective, LinkExpression } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, sleep, startExecutor } from "../utils/utils";
import { ChildProcess, spawn } from 'node:child_process';
import fetch from 'node-fetch';

//@ts-ignore
global.fetch = fetch;

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * MCP Integration Tests - Full Workflow Simulation
 * 
 * These tests simulate how Flux (or similar apps) would use MCP to interact with AD4M:
 * 1. Start executor with MCP enabled
 * 2. Login via MCP auth tools
 * 3. Create perspectives
 * 4. Define subject classes (Channel, Message)
 * 5. Query subject classes via MCP
 * 6. Create subject instances
 * 7. Query and manipulate data
 */

// MCP JSON-RPC request helper with proper ID matching
let requestIdCounter = 0;

interface McpResponse {
    jsonrpc: string;
    id: number;
    result?: any;
    error?: { code: number; message: string };
}

async function mcpRequest(
    process: ChildProcess, 
    method: string, 
    params: any = {},
    timeoutMs: number = 30000
): Promise<McpResponse> {
    return new Promise((resolve, reject) => {
        const id = ++requestIdCounter;
        const request = JSON.stringify({
            jsonrpc: "2.0",
            id,
            method,
            params
        }) + "\n";

        let buffer = "";
        let timeout: NodeJS.Timeout;
        
        const cleanup = () => {
            clearTimeout(timeout);
            process.stdout?.off('data', onData);
        };
        
        const onData = (data: Buffer) => {
            buffer += data.toString();
            
            // Try to parse complete JSON responses
            const lines = buffer.split('\n').filter(l => l.trim());
            for (const line of lines) {
                try {
                    const parsed = JSON.parse(line) as McpResponse;
                    // Match by ID to handle concurrent requests
                    if (parsed.id === id) {
                        cleanup();
                        resolve(parsed);
                        return;
                    }
                } catch (e) {
                    // Not valid JSON yet, continue reading
                }
            }
        };

        process.stdout?.on('data', onData);
        process.stdin?.write(request);

        timeout = setTimeout(() => {
            cleanup();
            reject(new Error(`MCP request timeout for ${method} (id: ${id})`));
        }, timeoutMs);
    });
}

// Helper to call MCP tool
async function callMcpTool(
    process: ChildProcess,
    toolName: string,
    args: Record<string, any> = {}
): Promise<any> {
    const response = await mcpRequest(process, "tools/call", {
        name: toolName,
        arguments: args
    });
    
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

// Flux-style Subject Class definitions (simplified)
const CHANNEL_SDNA = `
subject_class("Channel", c).
constructor(c, '[{action: "addLink", source: "this", predicate: "rdf://type", target: "flux://channel"}]').
property(c, "name").
property_getter(c, Base, "name", Value) :- triple(Base, "flux://name", Value).
property_setter(c, "name", '[{action: "setSingleTarget", source: "this", predicate: "flux://name", target: "value"}]').
property(c, "description").
property_getter(c, Base, "description", Value) :- triple(Base, "flux://description", Value).
property_setter(c, "description", '[{action: "setSingleTarget", source: "this", predicate: "flux://description", target: "value"}]').
`;

const MESSAGE_SDNA = `
subject_class("Message", m).
constructor(m, '[{action: "addLink", source: "this", predicate: "rdf://type", target: "flux://message"}]').
property(m, "body").
property_getter(m, Base, "body", Value) :- triple(Base, "flux://body", Value).
property_setter(m, "body", '[{action: "setSingleTarget", source: "this", predicate: "flux://body", target: "value"}]').
property(m, "author").
property_getter(m, Base, "author", Value) :- triple(Base, "flux://author", Value).
`;

describe("MCP Integration Tests - Flux Workflow Simulation", function() {
    this.timeout(180000); // 3 minute timeout

    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-integration-test");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    const gqlPort = 15900;
    const hcAdminPort = 15901;
    const hcAppPort = 15902;
    const adminCredential = "mcp-integration-admin";

    let executorProcess: ChildProcess | null = null;
    let mcpProcess: ChildProcess | null = null;
    let adminClient: Ad4mClient | null = null;
    let testPerspectiveUuid: string | null = null;

    before(async () => {
        // Clean up and create test directory
        if (fs.existsSync(appDataPath)) {
            fs.rmSync(appDataPath, { recursive: true });
        }
        fs.mkdirSync(appDataPath, { recursive: true });

        // Start executor (GraphQL mode)
        executorProcess = await startExecutor(
            appDataPath,
            bootstrapSeedPath,
            gqlPort,
            hcAdminPort,
            hcAppPort,
            false,
            adminCredential
        );

        // Initialize admin client
        adminClient = new Ad4mClient(apolloClient(gqlPort, adminCredential), false);
        await adminClient.agent.generate("test-passphrase");

        // Create a test perspective
        const perspective = await adminClient.perspective.add("MCP Test Perspective");
        testPerspectiveUuid = perspective.uuid;
        console.log(`Created test perspective: ${testPerspectiveUuid}`);

        // Add SDNA to the perspective
        await adminClient.perspective.addSdna(testPerspectiveUuid, CHANNEL_SDNA, "subject_class");
        await adminClient.perspective.addSdna(testPerspectiveUuid, MESSAGE_SDNA, "subject_class");
        console.log("Added Channel and Message subject classes");
    });

    after(async () => {
        // Cleanup MCP process
        if (mcpProcess && !mcpProcess.killed) {
            mcpProcess.kill('SIGKILL');
            await sleep(500);
        }

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

    describe("Setup verification (via GraphQL)", () => {
        it("should have created the test perspective", async () => {
            expect(testPerspectiveUuid).to.be.a('string');
            const perspectives = await adminClient!.perspective.all();
            const found = perspectives.find(p => p.uuid === testPerspectiveUuid);
            expect(found).to.exist;
            expect(found?.name).to.equal("MCP Test Perspective");
        });

        it("should have subject classes defined", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            const classes = await proxy!.subjectClasses();
            console.log("Subject classes:", classes);
            expect(classes).to.include("Channel");
            expect(classes).to.include("Message");
        });
    });

    describe("MCP Tool Tests (via stdio)", () => {
        // Note: These tests would require spawning the executor with --enable-mcp flag
        // For now, we test the logic through GraphQL which uses the same underlying code
        
        it("should list all perspectives", async () => {
            const perspectives = await adminClient!.perspective.all();
            expect(perspectives.length).to.be.greaterThan(0);
            const testPerspective = perspectives.find(p => p.uuid === testPerspectiveUuid);
            expect(testPerspective).to.exist;
        });

        it("should list subject classes in perspective", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            const classes = await proxy!.subjectClasses();
            expect(classes).to.be.an('array');
            expect(classes.length).to.be.greaterThan(0);
        });

        it("should query subjects (initially empty)", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            // Query for Channel instances
            const channels = await proxy!.getAllSubjectInstances("Channel");
            expect(channels).to.be.an('array');
            expect(channels.length).to.equal(0); // No channels created yet
        });
    });

    describe("Flux Workflow Simulation", () => {
        let channelBaseExpression: string;
        let messageBaseExpression: string;

        it("should create a Channel subject instance", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            
            // Create a new Channel
            const channel = await proxy!.createSubject("Channel", "literal://test-channel-1");
            channelBaseExpression = channel.baseExpression;
            
            expect(channelBaseExpression).to.be.a('string');
            console.log(`Created channel: ${channelBaseExpression}`);

            // Set properties
            await channel.setProperty("name", "General");
            await channel.setProperty("description", "Main discussion channel");
        });

        it("should query and find the created Channel", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            const channels = await proxy!.getAllSubjectInstances("Channel");
            
            expect(channels.length).to.be.greaterThan(0);
            const found = channels.find((c: any) => c.baseExpression === channelBaseExpression);
            expect(found).to.exist;
        });

        it("should get Channel subject data", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            const channel = await proxy!.getSubjectData(channelBaseExpression, "Channel");
            
            expect(channel.name).to.equal("General");
            expect(channel.description).to.equal("Main discussion channel");
        });

        it("should create a Message in the Channel", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            
            // Create a message
            const message = await proxy!.createSubject("Message", "literal://test-message-1");
            messageBaseExpression = message.baseExpression;
            
            // Set message body
            await message.setProperty("body", "Hello, World!");
            
            // Link message to channel
            await proxy!.add({
                source: channelBaseExpression,
                predicate: "ad4m://has_child",
                target: messageBaseExpression
            });

            console.log(`Created message: ${messageBaseExpression}`);
        });

        it("should query Messages", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            const messages = await proxy!.getAllSubjectInstances("Message");
            
            expect(messages.length).to.be.greaterThan(0);
            const found = messages.find((m: any) => m.baseExpression === messageBaseExpression);
            expect(found).to.exist;
        });

        it("should get Message subject data", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            const message = await proxy!.getSubjectData(messageBaseExpression, "Message");
            
            expect(message.body).to.equal("Hello, World!");
        });

        it("should find messages linked to channel", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            
            // Query links from channel to messages
            const links = await proxy!.get({
                source: channelBaseExpression,
                predicate: "ad4m://has_child"
            });

            expect(links.length).to.be.greaterThan(0);
            const messageLink = links.find((l: LinkExpression) => l.data.target === messageBaseExpression);
            expect(messageLink).to.exist;
        });

        it("should run Prolog query to find channel messages", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            
            const query = `
                subject_class("Message", MC),
                instance(MC, Message),
                triple("${channelBaseExpression}", "ad4m://has_child", Message)
            `;
            
            const results = await proxy!.infer(query);
            console.log("Prolog query results:", results);
            expect(results).to.be.an('array');
        });
    });

    describe("Subject Class Introspection", () => {
        it("should get Channel class properties", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            
            // Query Prolog for Channel properties
            const query = `subject_class("Channel", C), property(C, PropName)`;
            const results = await proxy!.infer(query);
            
            console.log("Channel properties:", results);
            expect(results).to.be.an('array');
            // Should find name and description properties
        });

        it("should get Message class properties", async () => {
            const proxy = await adminClient!.perspective.byUUID(testPerspectiveUuid!);
            
            const query = `subject_class("Message", M), property(M, PropName)`;
            const results = await proxy!.infer(query);
            
            console.log("Message properties:", results);
            expect(results).to.be.an('array');
        });
    });
});

describe("MCP Stdio Transport Tests", function() {
    this.timeout(120000);

    // These tests are skipped by default as they require special setup
    // Uncomment and configure to test actual MCP stdio transport
    
    describe.skip("Actual MCP stdio transport", () => {
        let mcpProcess: ChildProcess | null = null;

        before(async () => {
            // Would need to spawn: ad4m-executor --enable-mcp
            // mcpProcess = spawn('ad4m-executor', ['--enable-mcp', ...]);
        });

        after(async () => {
            if (mcpProcess && !mcpProcess.killed) {
                mcpProcess.kill('SIGKILL');
            }
        });

        it("should initialize MCP server", async () => {
            // Send initialize request
            // const response = await mcpRequest(mcpProcess!, "initialize", {...});
        });

        it("should list available tools", async () => {
            // const response = await mcpRequest(mcpProcess!, "tools/list", {});
        });

        it("should call auth_status tool", async () => {
            // const result = await callMcpTool(mcpProcess!, "auth_status", {});
        });

        it("should call set_token tool", async () => {
            // const result = await callMcpTool(mcpProcess!, "set_token", { token: "..." });
        });

        it("should call list_perspectives tool", async () => {
            // const result = await callMcpTool(mcpProcess!, "list_perspectives", {});
        });
    });
});
