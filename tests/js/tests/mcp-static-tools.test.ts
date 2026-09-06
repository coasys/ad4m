/**
 * MCP static instance tools — the default (static-only) tool surface.
 *
 * Executor started with MCP enabled and `dynamicClassTools` left at its
 * default (false). Everything goes through MCP over HTTP, the way an external
 * agent uses it:
 *
 * 1. tools/list is DNA-independent: the seven static tools are present and
 *    no per-class tool appears even after models are registered; calling a
 *    hidden per-class tool is refused with a pointer to instance_*.
 * 2. describe_perspective returns the SHACL schema as data (properties with
 *    type / required / cardinality / hints, collections).
 * 3. instance_create / instance_get / instance_query / instance_update /
 *    instance_add_to_collection / instance_remove round-trip typed data.
 * 4. Validation errors name the property, the expected type and the
 *    cardinality (design doc risk #2).
 *
 * The hybrid mode (flag on: per-class tools + instance_* side by side) is
 * covered by mcp-http.test.ts.
 */

import path from "path";
import { Ad4mClient } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { sleep, startExecutor, killByPorts } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { ChildProcess } from 'node:child_process';
import { callMcpTool, listMcpTools, initializeMcp } from './mcp-utils';

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ============================================================================
// SHACL fixtures — Flux-shaped Channel / Message with interpretation hints
// ============================================================================

const CHANNEL_SHACL = JSON.stringify({
    target_class: "flux://Channel",
    interpretation_hint: "A chat room that groups messages by topic.",
    properties: [
        {
            path: "flux://channel_name",
            name: "name",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
            writable: true,
            interpretation_hint: "Short room name, e.g. general.",
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
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_description", target: "value", local: false }
            ]
        },
        {
            path: "flux://channel_is_pinned",
            name: "isPinned",
            datatype: "xsd:boolean",
            min_count: 0,
            max_count: 1,
            writable: true,
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_is_pinned", target: "value", local: false }
            ]
        },
        {
            path: "flux://channel_rank",
            name: "rank",
            datatype: "xsd:integer",
            min_count: 0,
            max_count: 1,
            writable: true,
            setter: [
                { action: "setSingleTarget", source: "this", predicate: "flux://channel_rank", target: "value", local: false }
            ]
        },
        {
            path: "ad4m://has_child",
            name: "messages",
            collection: true,
            writable: true,
            interpretation_hint: "The messages posted in this room.",
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
    interpretation_hint: "One chat message.",
    properties: [
        {
            path: "flux://body",
            name: "body",
            datatype: "xsd:string",
            min_count: 1,
            max_count: 1,
            writable: true,
            // Signed envelope storage — instance_get/query must unwrap it.
            resolve_language: "literal",
            interpretation_hint: "The message text.",
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

const STATIC_INSTANCE_TOOLS = [
    'describe_perspective',
    'instance_create',
    'instance_query',
    'instance_get',
    'instance_update',
    'instance_add_to_collection',
    'instance_remove',
];

function asText(result: any): string {
    return typeof result === 'string' ? result : JSON.stringify(result);
}

// ============================================================================
// Test Suite
// ============================================================================

describe("MCP static instance tools (dynamicClassTools off)", function() {
    this.timeout(180000);

    const TEST_DIR = path.join(__dirname + "/../tst-tmp");
    const appDataPath = path.join(TEST_DIR, "agents", "mcp-static-tools-test");
    const bootstrapSeedPath = path.join(__dirname + "/../bootstrapSeed.json");
    const adminCredential = "mcp-static-tools-admin";

    let apiPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;
    let mcpPort: number;
    let mcpBaseUrl: string;

    let executorProcess: ChildProcess | null = null;
    let mcpSessionId = "";
    let perspectiveUuid = "";
    let channelUri = "";
    let msg1Uri = "";
    let msg2Uri = "";

    before(async () => {
        [apiPort, hcAdminPort, hcAppPort, mcpPort] = await getFreePorts(4);
        mcpBaseUrl = `http://127.0.0.1:${mcpPort}/mcp`;
        registerPorts([apiPort, hcAdminPort, hcAppPort, mcpPort]);

        if (fs.existsSync(appDataPath)) {
            fs.rmSync(appDataPath, { recursive: true });
        }
        fs.mkdirSync(appDataPath, { recursive: true });

        executorProcess = await startExecutor(
            appDataPath,
            bootstrapSeedPath,
            apiPort,
            hcAdminPort,
            hcAppPort,
            true,               // languageLanguageOnly
            adminCredential,
            undefined, undefined, undefined,
            true,               // enableMcp
            mcpPort,
            // dynamicClassTools intentionally omitted → executor default (false)
        );

        await sleep(3000);

        const adminClient = new Ad4mClient(`http://127.0.0.1:${apiPort}`, adminCredential, false);
        await adminClient.agent.generate("test-passphrase");
    });

    after(async () => {
        if (executorProcess) {
            executorProcess.kill('SIGTERM');
            await sleep(1000);
            if (!executorProcess.killed) {
                executorProcess.kill('SIGKILL');
            }
        }
        killByPorts([apiPort, hcAdminPort, hcAppPort, mcpPort]);
        deregisterPorts([apiPort, hcAdminPort, hcAppPort, mcpPort]);
    });

    // ========================================================================
    // 1. Surface: static tools present, per-class tools hidden
    // ========================================================================

    describe("1. Tool surface", function() {
        it("initializes and authenticates", async function() {
            const init = await initializeMcp(mcpBaseUrl);
            mcpSessionId = init.sessionId;

            const cap = await callMcpTool(mcpBaseUrl, 'request_capability', {
                app_name: "static-tools-test",
                app_desc: "MCP static instance tools test"
            }, mcpSessionId);
            const jwt = await callMcpTool(mcpBaseUrl, 'generate_jwt', {
                request_id: cap.request_id,
                code: cap.code,
            }, mcpSessionId);
            expect(jwt.success).to.be.true;
        });

        it("lists the seven static instance tools", async function() {
            const tools = await listMcpTools(mcpBaseUrl, mcpSessionId);
            const names = tools.map((t: any) => t.name);
            for (const name of STATIC_INSTANCE_TOOLS) {
                expect(names, `missing ${name}`).to.include(name);
            }

            // Schemas: class_name is a parameter, properties is an object.
            const create = tools.find((t: any) => t.name === 'instance_create');
            expect(create.inputSchema.properties).to.have.property('class_name');
            expect(create.inputSchema.properties).to.have.property('properties');
            expect(create.inputSchema.required).to.include('perspective_id');
            expect(create.inputSchema.required).to.include('class_name');
            expect(create.inputSchema.required).to.not.include('base_uri');
        });

        it("creates a perspective and registers Channel + Message models", async function() {
            const p = await callMcpTool(mcpBaseUrl, 'add_perspective', { name: "Static Tools Room" }, mcpSessionId);
            expect(p.success).to.be.true;
            perspectiveUuid = p.uuid;

            const c = await callMcpTool(mcpBaseUrl, 'add_model', {
                perspective_id: perspectiveUuid, class_name: "Channel", shacl_json: CHANNEL_SHACL,
            }, mcpSessionId);
            expect(c.success).to.be.true;
            const m = await callMcpTool(mcpBaseUrl, 'add_model', {
                perspective_id: perspectiveUuid, class_name: "Message", shacl_json: MESSAGE_SHACL,
            }, mcpSessionId);
            expect(m.success).to.be.true;
        });

        it("does not grow the tool list when models are registered", async function() {
            const tools = await listMcpTools(mcpBaseUrl, mcpSessionId);
            const names: string[] = tools.map((t: any) => t.name);
            const perClass = names.filter(n => n.startsWith('channel_') || n.startsWith('message_'));
            expect(perClass, `per-class tools leaked: ${perClass}`).to.be.empty;
            for (const name of STATIC_INSTANCE_TOOLS) {
                expect(names).to.include(name);
            }
        });

        it("refuses a hidden per-class tool call and points at instance_*", async function() {
            const result = await callMcpTool(mcpBaseUrl, 'channel_create', {
                perspective_id: perspectiveUuid, name: "nope",
            }, mcpSessionId);
            const text = asText(result);
            expect(text).to.include('channel_create');
            expect(text).to.include('instance_create');
            expect(text).to.include('dynamicClassTools');
        });
    });

    // ========================================================================
    // 2. describe_perspective — schema as data
    // ========================================================================

    describe("2. describe_perspective", function() {
        it("returns classes with typed properties, cardinality, collections and hints", async function() {
            const desc = await callMcpTool(mcpBaseUrl, 'describe_perspective', {
                perspective_id: perspectiveUuid,
            }, mcpSessionId);
            expect(desc.perspective_id).to.equal(perspectiveUuid);
            expect(desc.classes).to.be.an('array');

            const channel = desc.classes.find((c: any) => c.name === 'Channel');
            expect(channel, "Channel class").to.exist;
            expect(channel.class_uri).to.equal('flux://Channel');
            expect(channel.interpretation_hint).to.include('chat room');

            const byName = (list: any[], n: string) => list.find((p: any) => p.name === n);
            const name = byName(channel.properties, 'name');
            expect(name.type).to.equal('string');
            expect(name.required).to.be.true;
            expect(name.cardinality).to.deep.equal({ min: 1, max: 1 });
            expect(name.predicate).to.equal('flux://channel_name');
            expect(name.interpretation_hint).to.include('room name');

            expect(byName(channel.properties, 'isPinned').type).to.equal('boolean');
            expect(byName(channel.properties, 'rank').type).to.equal('integer');
            expect(byName(channel.properties, 'description').required).to.be.false;

            const messages = byName(channel.collections, 'messages');
            expect(messages, "messages collection").to.exist;
            expect(messages.cardinality.max).to.be.null;
            expect(messages.predicate).to.equal('ad4m://has_child');
            expect(messages.interpretation_hint).to.include('messages posted');

            const message = desc.classes.find((c: any) => c.name === 'Message');
            expect(message).to.exist;
            const body = byName(message.properties, 'body');
            expect(body.required).to.be.true;
            expect(body.resolve_language).to.equal('literal');

            expect(desc.flows).to.be.an('array');
            expect(desc.usage).to.include('instance_create');
        });
    });

    // ========================================================================
    // 3. Validation — property, expected type, cardinality
    // ========================================================================

    describe("3. Validation errors", function() {
        it("names a missing required property with its type and cardinality", async function() {
            const r = await callMcpTool(mcpBaseUrl, 'instance_create', {
                perspective_id: perspectiveUuid, class_name: "Channel", properties: { description: "no name" },
            }, mcpSessionId);
            expect(r.error).to.include('name');
            expect(r.error).to.include('string');
            expect(r.error).to.include('minCount 1');
            const ve = r.validation_errors.find((e: any) => e.property === 'name');
            expect(ve.problem).to.include('missing required');
            expect(ve.expected_type).to.equal('string');
            expect(ve.cardinality).to.include('maxCount 1');
        });

        it("rejects an unknown property and lists the available ones", async function() {
            const r = await callMcpTool(mcpBaseUrl, 'instance_create', {
                perspective_id: perspectiveUuid, class_name: "Channel", properties: { name: "x", nmae: "typo" },
            }, mcpSessionId);
            expect(r.error).to.include('nmae');
            expect(r.error).to.include('unknown property');
            expect(r.error).to.include('description');
        });

        it("rejects a wrongly typed value and an array on a single-valued property", async function() {
            const r = await callMcpTool(mcpBaseUrl, 'instance_create', {
                perspective_id: perspectiveUuid, class_name: "Channel",
                properties: { name: ["a", "b"], isPinned: "maybe" },
            }, mcpSessionId);
            expect(r.validation_errors).to.have.length(2);
            const pinned = r.validation_errors.find((e: any) => e.property === 'isPinned');
            expect(pinned.expected_type).to.equal('boolean');
            expect(pinned.received).to.include('maybe');
            const name = r.validation_errors.find((e: any) => e.property === 'name');
            expect(name.problem).to.include('array of 2');
            expect(name.cardinality).to.include('maxCount 1');
        });

        it("rejects an unknown class and lists registered classes", async function() {
            const r = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Widget",
            }, mcpSessionId);
            expect(r.error).to.include('Widget');
            expect(r.error).to.include('Channel');
            expect(r.error).to.include('Message');
        });
    });

    // ========================================================================
    // 4. Round trip through the generic tools
    // ========================================================================

    describe("4. Create / get / query / update / collections / remove", function() {
        it("creates a Channel with typed properties", async function() {
            const r = await callMcpTool(mcpBaseUrl, 'instance_create', {
                perspective_id: perspectiveUuid, class_name: "Channel",
                properties: { name: "general", isPinned: true, rank: 3 },
            }, mcpSessionId);
            expect(r.created, asText(r)).to.be.true;
            expect(r.class_name).to.equal('Channel');
            expect(r.base_uri).to.be.a('string');
            channelUri = r.base_uri;
        });

        it("reads it back with native JSON types", async function() {
            const c = await callMcpTool(mcpBaseUrl, 'instance_get', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
            }, mcpSessionId);
            expect(c.id).to.equal(channelUri);
            expect(c.name).to.equal('general');
            expect(c.isPinned).to.equal(true);
            expect(c.rank).to.equal(3);
        });

        it("creates Messages: one via parent, one via instance_add_to_collection", async function() {
            const m1 = await callMcpTool(mcpBaseUrl, 'instance_create', {
                perspective_id: perspectiveUuid, class_name: "Message",
                properties: { body: "Hello from a static tool" }, parent: channelUri,
            }, mcpSessionId);
            expect(m1.created, asText(m1)).to.be.true;
            expect(m1.added_to_parent).to.be.true;
            msg1Uri = m1.base_uri;

            const m2 = await callMcpTool(mcpBaseUrl, 'instance_create', {
                perspective_id: perspectiveUuid, class_name: "Message",
                properties: { body: "Second message" },
            }, mcpSessionId);
            expect(m2.created, asText(m2)).to.be.true;
            msg2Uri = m2.base_uri;

            const add = await callMcpTool(mcpBaseUrl, 'instance_add_to_collection', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
                collection: "messages", item_uri: msg2Uri,
            }, mcpSessionId);
            expect(add.success, asText(add)).to.be.true;

            const c = await callMcpTool(mcpBaseUrl, 'instance_get', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
            }, mcpSessionId);
            expect(c.messages).to.include(msg1Uri);
            expect(c.messages).to.include(msg2Uri);
        });

        it("queries Messages with unwrapped bodies, parent scope and filters", async function() {
            const all = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Message",
            }, mcpSessionId);
            expect(all.count).to.equal(2);
            expect(all.total_count).to.equal(2);
            const bodies = all.instances.map((m: any) => m.body);
            expect(bodies).to.include('Hello from a static tool');
            expect(bodies).to.include('Second message');

            const inChannel = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Message", parent: channelUri,
            }, mcpSessionId);
            expect(inChannel.count).to.equal(2);

            const filtered = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Message",
                filter: { body: { contains: "Second" } },
            }, mcpSessionId);
            expect(filtered.count).to.equal(1);
            expect(filtered.instances[0].id).to.equal(msg2Uri);

            const byName = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Channel", filter: { name: "general" },
            }, mcpSessionId);
            expect(byName.count).to.equal(1);

            const paged = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Message", limit: 1,
            }, mcpSessionId);
            expect(paged.count).to.equal(1);
            expect(paged.total_count).to.equal(2);

            const bad = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Message", filter: { subject: "x" },
            }, mcpSessionId);
            expect(bad.error).to.include('subject');
            expect(bad.error).to.include('body');
        });

        it("updates single-valued properties and validates on update", async function() {
            const ok = await callMcpTool(mcpBaseUrl, 'instance_update', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
                properties: { name: "general-renamed", isPinned: false },
            }, mcpSessionId);
            expect(ok.success, asText(ok)).to.be.true;
            expect(ok.updated_properties).to.have.members(['name', 'isPinned']);

            const c = await callMcpTool(mcpBaseUrl, 'instance_get', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
            }, mcpSessionId);
            expect(c.name).to.equal('general-renamed');
            expect(c.isPinned).to.equal(false);
            expect(c.rank).to.equal(3); // untouched

            const badType = await callMcpTool(mcpBaseUrl, 'instance_update', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
                properties: { isPinned: "nope" },
            }, mcpSessionId);
            expect(badType.error).to.include('isPinned');
            expect(badType.error).to.include('boolean');

            const collection = await callMcpTool(mcpBaseUrl, 'instance_update', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
                properties: { messages: [msg1Uri] },
            }, mcpSessionId);
            expect(collection.error).to.include('messages');
            expect(collection.error).to.include('instance_add_to_collection');

            const missing = await callMcpTool(mcpBaseUrl, 'instance_update', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: "flux://does-not-exist",
                properties: { name: "x" },
            }, mcpSessionId);
            expect(missing.error).to.include('No Channel instance');
        });

        it("rejects a non-collection in instance_add_to_collection", async function() {
            const r = await callMcpTool(mcpBaseUrl, 'instance_add_to_collection', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
                collection: "name", item_uri: msg1Uri,
            }, mcpSessionId);
            expect(r.error).to.include('name');
            expect(r.error).to.include('instance_update');
        });

        it("removes an instance only when class and URI match", async function() {
            const wrongClass = await callMcpTool(mcpBaseUrl, 'instance_remove', {
                perspective_id: perspectiveUuid, class_name: "Message", base_uri: channelUri,
            }, mcpSessionId);
            expect(wrongClass.error).to.include('No Message instance');

            const stillThere = await callMcpTool(mcpBaseUrl, 'instance_get', {
                perspective_id: perspectiveUuid, class_name: "Channel", base_uri: channelUri,
            }, mcpSessionId);
            expect(stillThere.name).to.equal('general-renamed');

            const removed = await callMcpTool(mcpBaseUrl, 'instance_remove', {
                perspective_id: perspectiveUuid, class_name: "Message", base_uri: msg2Uri,
            }, mcpSessionId);
            expect(removed.success, asText(removed)).to.be.true;
            expect(removed.links_removed).to.be.greaterThan(0);

            const left = await callMcpTool(mcpBaseUrl, 'instance_query', {
                perspective_id: perspectiveUuid, class_name: "Message",
            }, mcpSessionId);
            expect(left.count).to.equal(1);
            expect(left.instances[0].id).to.equal(msg1Uri);

            const gone = await callMcpTool(mcpBaseUrl, 'instance_get', {
                perspective_id: perspectiveUuid, class_name: "Message", base_uri: msg2Uri,
            }, mcpSessionId);
            expect(gone.error).to.include('No Message instance');
        });
    });
});
