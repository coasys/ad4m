import { Ad4mClient } from './Ad4mClient';
import { Perspective } from './perspectives/Perspective';
import { LinkQuery } from './perspectives/LinkQuery';

// Save original WebSocket so we can restore it after the suite
const originalWebSocket = (global as any).WebSocket;

/**
 * Test architecture:
 *
 * The AD4M server exposes NO REST endpoints for authenticated operations.
 * Everything goes through WebSocket RPC at /api/v1/ws.
 *
 * MockWebSocket intercepts all WS RPC messages and resolves them directly
 * from a response map — no HTTP server, no Express, no REST translation.
 *
 * Server-side unauthenticated WS RPC types (empty token allowed):
 *   user.create, user.login, user.verifyEmail, user.requestVerification,
 *   user.multiUserEnabled, runtime.info, runtime.tlsDomain
 *
 * The only real HTTP endpoints on the server are:
 *   GET  /           — server info
 *   GET  /health     — health check
 *   POST /api/v1/ai/transcription/feed — binary PCM audio (not JSON)
 */

// ===================== WS RPC MOCK RESPONSES =====================
// Each key is a WS RPC message type. The value is either a static response
// or a function (params) => response for dynamic behaviour.

type RpcHandler = unknown | ((params: Record<string, unknown>) => unknown);

const MOCK_RESPONSES: Record<string, RpcHandler> = {
    // ── Agent ──
    'agent.get': { did: 'did:test:123', perspective: new Perspective(), directMessageLanguage: 'lang://dm' },
    'agent.status': { did: 'did:test:123', didDocument: 'doc', isInitialized: true, isUnlocked: true },
    'agent.generate': { did: 'did:test:generated', didDocument: 'doc', isInitialized: true, isUnlocked: true },
    'agent.lock': { did: 'did:test:123', isInitialized: true, isUnlocked: false },
    'agent.unlock': { did: 'did:test:123', isInitialized: true, isUnlocked: true },
    'agent.import': { did: 'did:test:imported', isInitialized: true, isUnlocked: true },
    'agent.byDid': (p: Record<string, unknown>) => ({ did: p.did, perspective: new Perspective() }),
    'agent.updateProfile': { did: 'did:test:123', perspective: new Perspective(), directMessageLanguage: 'lang://dm' },
    'agent.sign': 'signed-message-data',
    'agent.isLocked': false,
    'agent.requestCapability': 'request-id-123',
    'agent.permitCapability': 'permitted-token',
    'agent.generateJwt': 'jwt-token-abc',
    'agent.getApps': [{ requestId: 'app1', auth: {}, token: 'tok', revoked: false }],
    'agent.removeApp': [],
    'agent.revokeToken': [],
    'agent.getTrustedAgents': ['did:trusted:1'],
    'agent.addTrustedAgents': ['did:trusted:1', 'did:trusted:2'],
    'agent.deleteTrustedAgents': ['did:trusted:1'],
    'agent.getEntanglementProofs': ['proof1', 'proof2'],
    'agent.addEntanglementProofs': [{ did: 'did:test:123', deviceKey: 'key1', deviceKeyType: 'type1' }],
    'agent.deleteEntanglementProofs': [],
    'agent.entanglementProofPreflight': { did: 'did:test:123', deviceKey: 'key1', deviceKeyType: 'type1' },

    // ── Perspectives ──
    'perspective.all': [{ uuid: 'uuid-1', name: 'test-perspective', sharedUrl: null, neighbourhood: null, state: 'Synced' }],
    'perspective.get': (p: Record<string, unknown>) => ({
        uuid: p.uuid, name: 'test-perspective', sharedUrl: null, neighbourhood: null, state: 'Synced',
    }),
    'perspective.create': (p: Record<string, unknown>) => ({
        uuid: 'uuid-new', name: p.name, sharedUrl: null, neighbourhood: null, state: 'Synced',
    }),
    'perspective.update': (p: Record<string, unknown>) => ({
        uuid: p.uuid, name: p.name, sharedUrl: null, neighbourhood: null, state: 'Synced',
    }),
    'perspective.remove': true,
    'perspective.snapshot': {
        links: [{ author: 'did:test:123', timestamp: '2024-01-01', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } }],
    },
    'perspective.publishSnapshot': 'Qm123',
    'perspective.queryLinks': [
        { author: 'did:test:123', timestamp: '2024-01-01', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } },
    ],
    'perspective.addLink': {
        author: 'did:test:123', timestamp: '2024-01-01',
        data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true },
    },
    'perspective.addLinkExpression': {
        author: 'did:test:123', timestamp: '2024-01-01',
        data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true },
    },
    'perspective.addLinks': [
        { author: 'did:test:123', timestamp: '2024-01-01', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } },
    ],
    'perspective.removeLinks': [],
    'perspective.linkMutations': {
        additions: [{ author: 'did:test:123', timestamp: '2024-01-01', data: { source: 'a', predicate: 'p', target: 't' }, proof: { valid: true } }],
        removals: [],
    },
    'perspective.updateLink': {
        author: 'did:test:123', timestamp: '2024-01-01',
        data: { source: 'new-s', predicate: 'p', target: 't' }, proof: { valid: true },
    },
    'perspective.removeLink': true,
    'perspective.queryProlog': JSON.stringify([{ X: 'test' }]),
    'perspective.querySparql': JSON.stringify([{ id: '1' }]),
    'perspective.addSdna': true,
    'perspective.executeCommands': true,
    'perspective.createSubject': true,
    'perspective.getSubjectData': '{"name":"test"}',
    'perspective.createBatch': 'batch-id-1',
    'perspective.commitBatch': { additions: [], removals: [] },
    'perspective.subscribeQuery': true,
    'perspective.keepAliveQuery': true,
    'perspective.disposeQuery': true,

    // ── Languages ──
    'language.all': [{ name: 'test-lang', address: 'lang://test', settings: '{}', icon: null, constructorIcon: null }],
    'language.get': (p: Record<string, unknown>) => ({
        name: 'test-lang', address: p.address, settings: '{}', icon: null, constructorIcon: null,
    }),
    'language.meta': {
        name: 'test-lang', address: 'lang://test', description: 'A test language',
        author: 'did:test:123', templated: false, templateSourceLanguageAddress: null,
        templateAppliedParams: null, possibleTemplateParams: null, sourceCodeLink: null,
    },
    'language.source': 'source-code-here',
    'language.writeSettings': true,
    'language.applyTemplate': { name: 'applied-lang', address: 'lang://applied' },
    'language.publish': {
        name: 'published-lang', address: 'lang://published', description: 'Published',
        author: 'did:test:123', templated: false,
    },
    'language.remove': true,

    // ── Neighbourhoods ──
    'neighbourhood.publish': 'neighbourhood://published',
    'neighbourhood.join': {
        uuid: 'uuid-joined', name: 'joined-neighbourhood', sharedUrl: 'neighbourhood://url',
        neighbourhood: {}, state: 'Synced',
    },
    'neighbourhood.otherAgents': ['did:other:1', 'did:other:2'],
    'neighbourhood.hasTelepresence': true,
    'neighbourhood.onlineAgents': [{ did: 'did:test:1', status: new Perspective() }],
    'neighbourhood.setOnlineStatus': true,
    'neighbourhood.sendSignal': true,
    'neighbourhood.sendBroadcast': true,

    // ── Expressions ──
    'expression.get': (p: Record<string, unknown>) => {
        if (p.raw === true) return 'raw-expression-data';
        return {
            author: 'did:test:123', timestamp: '2024-01-01', data: '{"content":"hello"}',
            language: { address: 'lang://test' }, proof: { valid: true },
        };
    },
    'expression.getMany': [
        { author: 'did:test:1', timestamp: '2023-01-01', data: { type: 'test' }, language: { address: 'lang://test' }, proof: { valid: true } },
    ],
    'expression.create': 'Qm-expression-hash',
    'expression.interactions': [{ label: 'interact1', name: 'doSomething', parameters: [] }],
    'expression.interact': 'interaction-result',

    // ── Runtime ──
    'runtime.info': { ad4mExecutorVersion: '0.1.0', isUnlocked: true, isInitialized: true },
    'runtime.tlsDomain': 'test.domain.com',
    'runtime.quit': true,
    'runtime.openLink': true,
    'runtime.friends': ['did:friend:1', 'did:friend:2'],
    'runtime.addFriends': ['did:friend:1', 'did:friend:2', 'did:friend:3'],
    'runtime.removeFriends': ['did:friend:1'],
    'runtime.friendStatus': (p: Record<string, unknown>) => ({
        author: p.did, timestamp: '2023-01-01', data: JSON.stringify({ recipe_name: 'test' }), proof: { valid: true },
    }),
    'runtime.sendFriendMessage': true,
    'runtime.inbox': [{ author: 'did:friend:1', timestamp: '2024-01-01', data: new Perspective() }],
    'runtime.outbox': [],
    'runtime.notifications': [],
    'runtime.createNotification': true,
    'runtime.updateNotification': true,
    'runtime.grantNotification': true,
    'runtime.deleteNotification': true,
    'runtime.setStatus': true,
    'runtime.linkLanguageTemplates': ['lang://template1'],
    'runtime.addLinkLanguageTemplates': ['lang://template1', 'lang://template2'],
    'runtime.removeLinkLanguageTemplates': ['lang://template1'],
    'runtime.hcAgentInfos': ['hc-agent-info-1', 'hc-agent-info-2'],
    'runtime.addHcAgentInfos': true,
    'runtime.networkMetrics': 'metrics-data',
    'runtime.restartHolochain': true,
    'runtime.verifySignature': true,
    'runtime.exportData': true,
    'runtime.importData': { success: true, count: 5 },
    'runtime.freeHostingEnabled': false,
    'runtime.setFreeHostingEnabled': true,
    'runtime.computeLog': [],

    // ── AI ──
    'ai.models': [{ id: 'model-1', name: 'GPT-Test', modelType: 'LLM', api: 'openai' }],
    'ai.addModel': 'model-new-id',
    'ai.updateModel': true,
    'ai.removeModel': true,
    'ai.setDefaultModel': true,
    'ai.getDefaultModel': { id: 'model-1', name: 'GPT-Test', modelType: 'LLM' },
    'ai.tasks': [{ taskId: 'task-1', name: 'summarize', modelId: 'model-1', systemPrompt: 'Summarize', promptExamples: [] }],
    'ai.addTask': { taskId: 'task-new', name: 'new-task', modelId: 'model-1', systemPrompt: 'Do stuff', promptExamples: [] },
    'ai.updateTask': { taskId: 'task-1', name: 'updated', modelId: 'model-1', systemPrompt: 'Updated', promptExamples: [] },
    'ai.removeTask': { taskId: 'task-1', name: 'summarize', modelId: 'model-1', systemPrompt: 'Summarize', promptExamples: [] },
    'ai.prompt': 'This is the AI response',
    'ai.modelLoadingStatus': { model: 'model-1', progress: 100, status: 'loaded' },

    // ── Users (unauthenticated on server — empty token allowed) ──
    'user.create': { success: true, did: 'did:test:new-user' },
    'user.login': 'login-jwt-token',
    'user.verifyEmail': 'verified-token',
    'user.requestVerification': { success: true, message: 'Verification email sent', requiresPassword: false, isExistingUser: true },
    'user.list': [{ email: 'user@test.com', credits: 100, freeAccess: false }],
    'user.multiUserEnabled': false,
    'user.setMultiUserEnabled': true,
    'user.credits': true,
    'user.freeAccess': true,
    'user.wallet': '0x1234567890abcdef',
    'user.emailTest': (p: Record<string, unknown>) => {
        if (p.action === 'get-code') return '123456';
        return true;
    },

    // ── Hosting ──
    'hosting.info': { email: 'test@test.com' },
    'hosting.setHotWallet': true,
    'hosting.requestPayment': { paymentUrl: 'https://pay.test' },

    // ── Runtime: host rates & Unyt ──
    'runtime.getHostRates': JSON.stringify([{ description: 'Link write', priceInHOT: 0.001 }]),
    'runtime.setHostRates': true,
    'runtime.unytAgentKey': 'unyt-agent-key-123',
    'runtime.unytHotAgentPubkey': 'unyt-hot-pubkey-456',
    'runtime.unytWalletBalance': '1000.50',
    'runtime.unytWalletHistory': '[]',
    'runtime.unytVersionInfo': '{"version":"0.1.0"}',
    'runtime.unytSetMembraneProof': { success: true, message: 'ok' },
    'runtime.unytReinstallDna': { success: true, message: 'reinstalled' },
    'runtime.unytSendHot': { success: true, message: 'sent' },
};

// ===================== MOCK WEBSOCKET =====================

/** Tracks the last RPC call for test assertions. */
let lastRpcCall: { type: string; params: Record<string, unknown> } | null = null;

class MockWebSocket {
    static instances: MockWebSocket[] = [];
    static CONNECTING = 0;
    static OPEN = 1;
    static CLOSING = 2;
    static CLOSED = 3;

    onopen: ((event: any) => void) | null = null;
    onmessage: ((event: { data: string }) => void) | null = null;
    onerror: ((event: any) => void) | null = null;
    onclose: ((event: any) => void) | null = null;
    readyState: number = MockWebSocket.CONNECTING;
    closed = false;
    url: string;

    close() {
        this.closed = true;
        this.readyState = MockWebSocket.CLOSED;
        this.onclose?.({});
    }

    send(data: string) {
        try {
            const parsed = JSON.parse(data);

            // Ping/pong keepalive
            if (parsed.type === 'ping') {
                setTimeout(() => {
                    this.onmessage?.({ data: JSON.stringify({ type: 'pong' }) });
                }, 0);
                return;
            }

            // RPC call — resolve directly from MOCK_RESPONSES
            const { id, type: msgType, params: rpcParams } = parsed;
            const params = (rpcParams && typeof rpcParams === 'object')
                ? rpcParams as Record<string, unknown>
                : {} as Record<string, unknown>;

            // Track for assertions
            lastRpcCall = { type: msgType, params };

            const handler = MOCK_RESPONSES[msgType];
            if (handler === undefined) {
                setTimeout(() => {
                    this.onmessage?.({ data: JSON.stringify({ id, error: { code: 404, message: `Unknown RPC type: ${msgType}` } }) });
                }, 0);
                return;
            }

            const result = typeof handler === 'function'
                ? (handler as (p: Record<string, unknown>) => unknown)(params)
                : handler;

            setTimeout(() => {
                this.onmessage?.({ data: JSON.stringify({ id, result }) });
            }, 0);
        } catch { /* malformed JSON — ignore */ }
    }

    /** Simulate server pushing an event to the client. */
    emit(payload: unknown) {
        this.onmessage?.({ data: JSON.stringify(payload) });
    }

    constructor(url: string) {
        this.url = url;
        MockWebSocket.instances.push(this);
        // Simulate async connection open
        setTimeout(() => {
            if (this.readyState === MockWebSocket.CONNECTING) {
                this.readyState = MockWebSocket.OPEN;
                this.onopen?.({});
            }
        }, 0);
    }
}

// Copy static constants to prototype (matches real WebSocket)
Object.defineProperty(MockWebSocket.prototype, 'CONNECTING', { value: 0 });
Object.defineProperty(MockWebSocket.prototype, 'OPEN', { value: 1 });
Object.defineProperty(MockWebSocket.prototype, 'CLOSING', { value: 2 });
Object.defineProperty(MockWebSocket.prototype, 'CLOSED', { value: 3 });

/** Return the last element of an array (avoids Array.prototype.at which needs ES2022 lib). */
function lastOf<T>(arr: T[]): T {
    return arr[arr.length - 1];
}

// ===================== TEST SETUP =====================

let ad4m: Ad4mClient;

beforeAll(() => {
    (global as any).WebSocket = MockWebSocket as any;
    // Ad4mClient takes an HTTP base URL; restClient converts to ws:// internally
    ad4m = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
});

afterAll(() => {
    (global as any).WebSocket = originalWebSocket;
});

beforeEach(() => {
    lastRpcCall = null;
    MockWebSocket.instances = [];
});

// ===================== AGENT TESTS =====================
describe('AgentClient', () => {
    test('me() returns agent with DID', async () => {
        const agent = await ad4m.agent.me();
        expect(agent.did).toBe('did:test:123');
        expect(agent.directMessageLanguage).toBe('lang://dm');
    });

    test('status() returns agent status', async () => {
        const status = await ad4m.agent.status();
        expect(status.isInitialized).toBe(true);
        expect(status.isUnlocked).toBe(true);
    });

    test('generate() sends passphrase and returns status', async () => {
        const status = await ad4m.agent.generate('secret123');
        expect(lastRpcCall!.params.passphrase).toBe('secret123');
        expect(status.did).toBe('did:test:generated');
    });

    test('lock() locks the agent', async () => {
        const status = await ad4m.agent.lock('secret123');
        expect(lastRpcCall!.params.passphrase).toBe('secret123');
        expect(status.isUnlocked).toBe(false);
    });

    test('unlock() unlocks the agent', async () => {
        const status = await ad4m.agent.unlock('secret123');
        expect(lastRpcCall!.params.passphrase).toBe('secret123');
        expect(lastRpcCall!.params.holochain).toBe(true);
        expect(status.isUnlocked).toBe(true);
    });

    test('import() imports a DID keystore', async () => {
        const status = await ad4m.agent.import({
            did: 'did:test:import', didDocument: 'doc', keystore: 'ks', passphrase: 'pass',
        });
        expect(lastRpcCall!.params.did).toBe('did:test:import');
        expect(status.did).toBe('did:test:imported');
    });

    test('byDID() fetches agent by DID', async () => {
        const agent = await ad4m.agent.byDID('did:test:other');
        expect(agent.did).toBe('did:test:other');
    });

    test('signMessage() signs a message', async () => {
        const signed = await ad4m.agent.signMessage('hello');
        expect(lastRpcCall!.params.message).toBe('hello');
        expect(signed).toBe('signed-message-data');
    });

    test('isLocked() returns lock status', async () => {
        const locked = await ad4m.agent.isLocked();
        expect(locked).toBe(false);
    });

    test('getEntanglementProofs() returns proofs', async () => {
        const proofs = await ad4m.agent.getEntanglementProofs();
        expect(proofs).toEqual(['proof1', 'proof2']);
    });

    test('addEntanglementProofs() sends proofs', async () => {
        const result = await ad4m.agent.addEntanglementProofs([{ did: 'd', deviceKey: 'k', deviceKeyType: 't', didSigningKeyId: 'sk', deviceKeySignedByDid: 'sig1', didSignedByDeviceKey: 'sig2' }]);
        expect(result).toHaveLength(1);
    });

    test('deleteEntanglementProofs() deletes proofs', async () => {
        const result = await ad4m.agent.deleteEntanglementProofs([{ did: 'd', deviceKey: 'k', deviceKeyType: 't', didSigningKeyId: 'sk', deviceKeySignedByDid: 'sig1', didSignedByDeviceKey: 'sig2' }]);
        expect(result).toEqual([]);
    });

    test('requestCapability() returns request ID', async () => {
        const id = await ad4m.agent.requestCapability({ appName: 'test', appDesc: 'test', appDomain: 'test', appUrl: 'test', capabilities: [] } as any);
        expect(id).toBe('request-id-123');
    });

    test('permitCapability() returns token', async () => {
        const token = await ad4m.agent.permitCapability('auth-string');
        expect(token).toBe('permitted-token');
    });

    test('generateJwt() returns JWT', async () => {
        const jwt = await ad4m.agent.generateJwt('req-1', 'rand-1');
        expect(lastRpcCall!.params.requestId).toBe('req-1');
        expect(jwt).toBe('jwt-token-abc');
    });

    test('getApps() returns apps list', async () => {
        const apps = await ad4m.agent.getApps();
        expect(apps).toHaveLength(1);
    });

    test('removeApp() removes an app', async () => {
        const result = await ad4m.agent.removeApp('app1');
        expect(result).toEqual([]);
    });

    test('revokeToken() revokes a token', async () => {
        const result = await ad4m.agent.revokeToken('app1');
        expect(result).toEqual([]);
    });

    test('agent-updated event unwraps nested agent payload', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
        const callback = jest.fn();
        freshClient.agent.addUpdatedListener(callback);
        freshClient.agent.subscribeAgentUpdated();

        const ws = lastOf(MockWebSocket.instances);

        ws.emit({
            type: 'agent-updated',
            agent: { did: 'did:test:updated', directMessageLanguage: 'lang://dm2', perspective: null, isInitialized: true, isUnlocked: true },
        });

        expect(callback).toHaveBeenCalledTimes(1);
        const received = callback.mock.calls[0][0];
        expect(received.did).toBe('did:test:updated');
        expect(received.directMessageLanguage).toBe('lang://dm2');
        expect(received).not.toHaveProperty('type');
    });

    test('agent-status-changed event unwraps nested agent payload', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
        const callback = jest.fn();
        freshClient.agent.addAgentStatusChangedListener(callback);
        freshClient.agent.subscribeAgentStatusChanged();

        const ws = lastOf(MockWebSocket.instances);

        ws.emit({
            type: 'agent-status-changed',
            agent: { did: 'did:test:status', isInitialized: true, isUnlocked: false },
        });

        expect(callback).toHaveBeenCalledTimes(1);
        const received = callback.mock.calls[0][0];
        expect(received.did).toBe('did:test:status');
        expect(received.isUnlocked).toBe(false);
        expect(received).not.toHaveProperty('type');
    });
});

// ===================== PERSPECTIVE TESTS =====================
describe('PerspectiveClient', () => {
    test('all() returns perspective proxies', async () => {
        const perspectives = await ad4m.perspective.all();
        expect(perspectives).toHaveLength(1);
        expect(perspectives[0].uuid).toBe('uuid-1');
    });

    test('byUUID() returns a perspective proxy', async () => {
        const p = await ad4m.perspective.byUUID('uuid-1');
        expect(p).not.toBeNull();
        expect(p!.uuid).toBe('uuid-1');
    });

    test('snapshotByUUID() returns snapshot', async () => {
        const snapshot = await ad4m.perspective.snapshotByUUID('uuid-1');
        expect(snapshot).not.toBeNull();
        expect(snapshot!.links).toHaveLength(1);
    });

    test('add() creates a new perspective', async () => {
        const p = await ad4m.perspective.add('new-perspective');
        expect(lastRpcCall!.params.name).toBe('new-perspective');
        expect(p.uuid).toBe('uuid-new');
    });

    test('update() updates a perspective name', async () => {
        const p = await ad4m.perspective.update('uuid-1', 'renamed');
        expect(lastRpcCall!.params.name).toBe('renamed');
        expect(p.name).toBe('renamed');
    });

    test('remove() deletes a perspective', async () => {
        const result = await ad4m.perspective.remove('uuid-1');
        expect(result.perspectiveRemove).toBe(true);
    });

    test('queryLinks() queries links with parameters', async () => {
        const links = await ad4m.perspective.queryLinks('uuid-1', new LinkQuery({ source: 'src', predicate: 'pred' }));
        expect(links).toHaveLength(1);
        expect(lastRpcCall!.params.source).toBe('src');
        expect(lastRpcCall!.params.predicate).toBe('pred');
    });

    test('addLink() adds a link', async () => {
        const link = await ad4m.perspective.addLink('uuid-1', { source: 's', predicate: 'p', target: 't' });
        expect(link.data.source).toBe('s');
        expect(lastRpcCall!.params.link).toEqual({ source: 's', predicate: 'p', target: 't' });
    });

    test('addLinks() adds multiple links', async () => {
        const links = await ad4m.perspective.addLinks('uuid-1', [
            { source: 's1', predicate: 'p', target: 't1' },
            { source: 's2', predicate: 'p', target: 't2' },
        ]);
        expect(links).toHaveLength(1);
        expect(lastRpcCall!.params.links).toHaveLength(2);
    });

    test('removeLinks() forwards batchId in the bulk remove request', async () => {
        const removed = await ad4m.perspective.removeLinks('uuid-1', [{
            author: 'a', timestamp: 't',
            data: { source: 's', predicate: 'p', target: 't' },
            proof: { valid: true },
        }] as any, 'batch-id-1');
        expect(removed).toHaveLength(0);
        expect(lastRpcCall!.type).toBe('perspective.removeLinks');
        expect(lastRpcCall!.params.links).toHaveLength(1);
        expect(lastRpcCall!.params.batchId).toBe('batch-id-1');
    });

    test('updateLink() updates a link', async () => {
        const link = await ad4m.perspective.updateLink('uuid-1',
            { author: 'a', timestamp: 't', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } } as any,
            { source: 'new-s', predicate: 'p', target: 't' },
        );
        expect(link.data.source).toBe('new-s');
    });

    test('removeLink() removes a link', async () => {
        const result = await ad4m.perspective.removeLink('uuid-1', {
            author: 'a', timestamp: 't',
            data: { source: 's', predicate: 'p', target: 't' },
            proof: { valid: true },
        } as any);
        expect(result).toBe(true);
    });

    test('linkMutations() applies mutations', async () => {
        const result = await ad4m.perspective.linkMutations('uuid-1', {
            additions: [{ source: 'a', predicate: 'p', target: 't' }],
            removals: [],
        });
        expect(result.additions).toHaveLength(1);
    });

    test('addSdna() adds SDNA', async () => {
        const result = await ad4m.perspective.addSdna('uuid-1', 'TestClass', 'code', 'subject_class');
        expect(result).toBe(true);
    });

    test('queryProlog() runs prolog query', async () => {
        const result = await ad4m.perspective.queryProlog('uuid-1', 'test(X)');
        expect(result).toEqual([{ X: 'test' }]);
    });

    test('subscribeToQueryUpdates() routes through the WebSocket endpoint', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
        const callback = jest.fn();
        const unsubscribe = freshClient.perspective.subscribeToQueryUpdates('sub-1', callback);
        const ws = lastOf(MockWebSocket.instances);

        expect(ws.url).toBe('ws://127.0.0.1:12000/api/v1/ws?token=test-token');

        unsubscribe();
        expect(ws.closed).toBe(true);
    });

    test('perspective lifecycle subscriptions use the WebSocket endpoint', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
        freshClient.perspective.addPerspectiveAddedListener(jest.fn());
        freshClient.perspective.subscribePerspectiveAdded();

        const ws = lastOf(MockWebSocket.instances);
        expect(ws.url).toBe('ws://127.0.0.1:12000/api/v1/ws?token=test-token');
    });

    test('perspective-scoped link subscriptions ignore events for other perspectives', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
        const linkAddedCallback = jest.fn();
        const linkRemovedCallback = jest.fn();
        const linkUpdatedCallback = jest.fn();

        await freshClient.perspective.addPerspectiveLinkAddedListener('uuid-1', [linkAddedCallback]);
        await freshClient.perspective.addPerspectiveLinkRemovedListener('uuid-1', [linkRemovedCallback]);
        await freshClient.perspective.addPerspectiveLinkUpdatedListener('uuid-1', [linkUpdatedCallback]);

        const ws = lastOf(MockWebSocket.instances);
        expect(ws.url).toBe('ws://127.0.0.1:12000/api/v1/ws?token=test-token');

        // Events for a different perspective should be ignored
        ws.emit({
            type: 'link-added',
            perspectiveUuid: 'uuid-2',
            link: { author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z', data: { source: 'test://other-added', predicate: 'test://has', target: 'test://value' }, proof: { valid: true } },
        });
        ws.emit({
            type: 'link-removed',
            perspectiveUuid: 'uuid-2',
            link: { author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z', data: { source: 'test://other-removed', predicate: 'test://has', target: 'test://value' }, proof: { valid: true } },
        });
        ws.emit({
            type: 'link-updated',
            perspectiveUuid: 'uuid-2',
            oldLink: { author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z', data: { source: 'test://other-old', predicate: 'test://has', target: 'test://value' }, proof: { valid: true } },
            newLink: { author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z', data: { source: 'test://other-new', predicate: 'test://has', target: 'test://value' }, proof: { valid: true } },
        });

        expect(linkAddedCallback).not.toHaveBeenCalled();
        expect(linkRemovedCallback).not.toHaveBeenCalled();
        expect(linkUpdatedCallback).not.toHaveBeenCalled();

        // Events for the correct perspective should fire
        const addedLink = {
            author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z',
            data: { source: 'test://added', predicate: 'test://has', target: 'test://value' }, proof: { valid: true },
        };
        const removedLink = {
            author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z',
            data: { source: 'test://removed', predicate: 'test://has', target: 'test://value' }, proof: { valid: true },
        };

        ws.emit({ type: 'link-added', perspectiveUuid: 'uuid-1', link: addedLink });
        ws.emit({ type: 'link-removed', perspectiveUuid: 'uuid-1', link: removedLink });
        ws.emit({
            type: 'link-updated',
            perspectiveUuid: 'uuid-1',
            oldLink: { author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z', data: { source: 'test://updated-old', predicate: 'test://has', target: 'test://value' }, proof: { valid: true } },
            newLink: { author: 'did:test:123', timestamp: '2024-01-01T00:00:00.000Z', data: { source: 'test://updated-new', predicate: 'test://has', target: 'test://value' }, proof: { valid: true } },
        });

        expect(linkAddedCallback).toHaveBeenCalledWith(addedLink);
        expect(linkRemovedCallback).toHaveBeenCalledWith(removedLink);
        expect(linkUpdatedCallback).toHaveBeenCalledTimes(1);
    });

    test('runtime exception subscriptions normalize PascalCase exception types', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
        const callback = jest.fn(() => null);
        freshClient.runtime.addExceptionCallback(callback);
        freshClient.runtime.subscribeExceptionOccurred();

        const ws = lastOf(MockWebSocket.instances);
        ws.emit({
            type: 'exception-occurred',
            exception: {
                title: 'Request to authenticate application',
                message: 'demo-app is waiting for authentication',
                type: 'CapabilityRequested',
                addon: '{}',
            },
        });

        expect(callback).toHaveBeenCalledWith({
            title: 'Request to authenticate application',
            message: 'demo-app is waiting for authentication',
            type: 'CAPABILITY_REQUESTED',
            addon: '{}',
        });
    });

    test('subscribeToQueryUpdates() ignores unrelated events and accepts object results', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'test-token', false);
        const callback = jest.fn();
        const unsubscribe = freshClient.perspective.subscribeToQueryUpdates('sub-1', callback);
        const ws = lastOf(MockWebSocket.instances);

        ws.emit({ type: 'perspective-added', perspective: { uuid: 'uuid-ignored' } });
        ws.emit({ type: 'query-subscription-update', subscriptionId: 'sub-2', result: { ignored: true } });
        expect(callback).not.toHaveBeenCalled();

        ws.emit({
            type: 'query-subscription-update',
            subscriptionId: 'sub-1',
            result: [{ id: 'community://1', name: 'REST Smoke Community' }],
        });
        expect(callback).toHaveBeenCalledWith([{ id: 'community://1', name: 'REST Smoke Community' }]);

        unsubscribe();
        ws.emit({
            type: 'query-subscription-update',
            subscriptionId: 'sub-1',
            result: [{ id: 'community://2', name: 'Should not arrive' }],
        });
        expect(callback).toHaveBeenCalledTimes(1);
    });

    test('publishSnapshotByUUID() publishes a snapshot', async () => {
        const hash = await ad4m.perspective.publishSnapshotByUUID('uuid-1');
        expect(hash).toBe('Qm123');
    });

    test('createBatch() creates a batch', async () => {
        const batchId = await ad4m.perspective.createBatch('uuid-1');
        expect(batchId).toBe('batch-id-1');
    });

    test('commitBatch() commits a batch', async () => {
        const result = await ad4m.perspective.commitBatch('uuid-1', 'batch-id-1');
        expect(result.additions).toEqual([]);
    });
});

// ===================== LANGUAGE TESTS =====================
describe('LanguageClient', () => {
    test('all() returns all languages', async () => {
        const langs = await ad4m.languages.all();
        expect(langs).toHaveLength(1);
        expect(langs[0].name).toBe('test-lang');
    });

    test('byFilter() filters languages', async () => {
        await ad4m.languages.byFilter('test');
        expect(lastRpcCall!.params.filter).toBe('test');
    });

    test('byAddress() returns a language handle', async () => {
        const lang = await ad4m.languages.byAddress('lang://test');
        expect(lang.name).toBe('test-lang');
    });

    test('meta() returns language meta', async () => {
        const meta = await ad4m.languages.meta('lang://test');
        expect(meta.name).toBe('test-lang');
        expect(meta.author).toBe('did:test:123');
    });

    test('source() returns source code', async () => {
        const source = await ad4m.languages.source('lang://test');
        expect(source).toBe('source-code-here');
    });

    test('writeSettings() writes language settings', async () => {
        const result = await ad4m.languages.writeSettings('lang://test', '{"key":"value"}');
        expect(result).toBe(true);
        expect(lastRpcCall!.params.settings).toBe('{"key":"value"}');
    });

    test('applyTemplateAndPublish() applies a template', async () => {
        const ref = await ad4m.languages.applyTemplateAndPublish('lang://source', '{"param":"value"}');
        expect(ref.name).toBe('applied-lang');
        expect(ref.address).toBe('lang://applied');
    });

    test('publish() publishes a language', async () => {
        const meta = await ad4m.languages.publish('/path/to/lang', { name: 'my-lang', description: 'desc' } as any);
        expect(meta.name).toBe('published-lang');
    });

    test('remove() removes a language', async () => {
        const result = await ad4m.languages.remove('lang://test');
        expect(result).toBe(true);
    });
});

// ===================== NEIGHBOURHOOD TESTS =====================
describe('NeighbourhoodClient', () => {
    test('publishFromPerspective() publishes a neighbourhood', async () => {
        const url = await ad4m.neighbourhood.publishFromPerspective(
            'uuid-1', 'lang://link', new Perspective(),
        );
        expect(url).toBe('neighbourhood://published');
        expect(lastRpcCall!.params.perspectiveUUID).toBe('uuid-1');
    });

    test('joinFromUrl() joins a neighbourhood', async () => {
        const handle = await ad4m.neighbourhood.joinFromUrl('neighbourhood://test');
        expect(handle.uuid).toBe('uuid-joined');
        expect(lastRpcCall!.params.url).toBe('neighbourhood://test');
    });

    test('otherAgents() returns other agents', async () => {
        const agents = await ad4m.neighbourhood.otherAgents('uuid-1');
        expect(agents).toEqual(['did:other:1', 'did:other:2']);
    });

    test('hasTelepresenceAdapter() checks adapter', async () => {
        const has = await ad4m.neighbourhood.hasTelepresenceAdapter('uuid-1');
        expect(has).toBe(true);
    });

    test('onlineAgents() returns online agents', async () => {
        const agents = await ad4m.neighbourhood.onlineAgents('uuid-1');
        expect(agents).toHaveLength(1);
    });

    test('setOnlineStatus() sets status', async () => {
        const result = await ad4m.neighbourhood.setOnlineStatus('uuid-1', new Perspective());
        expect(result).toBe(true);
    });

    test('sendSignal() sends a signal', async () => {
        const result = await ad4m.neighbourhood.sendSignal('uuid-1', 'did:other:1', new Perspective());
        expect(result).toBe(true);
        expect(lastRpcCall!.params.remoteAgentDid).toBe('did:other:1');
    });

    test('sendBroadcast() sends a broadcast', async () => {
        const result = await ad4m.neighbourhood.sendBroadcast('uuid-1', new Perspective(), true);
        expect(result).toBe(true);
        expect(lastRpcCall!.params.loopback).toBe(true);
    });
});

// ===================== EXPRESSION TESTS =====================
describe('ExpressionClient', () => {
    test('get() returns an expression', async () => {
        const expr = await ad4m.expression.get('lang://test/Qm123');
        expect(expr.author).toBe('did:test:123');
    });

    test('getMany() returns multiple expressions', async () => {
        const exprs = await ad4m.expression.getMany(['url1', 'url2']);
        expect(exprs).toHaveLength(1);
    });

    test('getRaw() returns raw expression', async () => {
        const raw = await ad4m.expression.getRaw('lang://test/Qm123');
        expect(raw).toBe('raw-expression-data');
    });

    test('create() creates an expression', async () => {
        const hash = await ad4m.expression.create({ content: 'hello' }, 'lang://test');
        expect(hash).toBe('Qm-expression-hash');
        expect(lastRpcCall!.params.languageAddress).toBe('lang://test');
    });

    test('interactions() returns interaction meta', async () => {
        const interactions = await ad4m.expression.interactions('lang://test/Qm123');
        expect(interactions).toHaveLength(1);
        expect(interactions[0].label).toBe('interact1');
    });

    test('interact() calls an interaction', async () => {
        const result = await ad4m.expression.interact('lang://test/Qm123', { name: 'doSomething', parameters: {} } as any);
        expect(result).toBe('interaction-result');
    });
});

// ===================== RUNTIME TESTS =====================
describe('RuntimeClient', () => {
    test('info() returns runtime info', async () => {
        const info = await ad4m.runtime.info();
        expect(info.ad4mExecutorVersion).toBe('0.1.0');
    });

    test('tlsDomain() returns TLS domain', async () => {
        const domain = await ad4m.runtime.tlsDomain();
        expect(domain).toBe('test.domain.com');
    });

    test('quit() quits the runtime', async () => {
        const result = await ad4m.runtime.quit();
        expect(result).toBe(true);
    });

    test('friends() returns friends list', async () => {
        const friends = await ad4m.runtime.friends();
        expect(friends).toEqual(['did:friend:1', 'did:friend:2']);
    });

    test('addFriends() adds friends', async () => {
        const result = await ad4m.runtime.addFriends(['did:friend:3']);
        expect(result).toHaveLength(3);
        expect(lastRpcCall!.params.dids).toEqual(['did:friend:3']);
    });

    test('removeFriends() removes friends', async () => {
        const result = await ad4m.runtime.removeFriends(['did:friend:2']);
        expect(result).toEqual(['did:friend:1']);
    });

    test('getTrustedAgents() returns trusted agents', async () => {
        const agents = await ad4m.runtime.getTrustedAgents();
        expect(agents).toEqual(['did:trusted:1']);
    });

    test('addTrustedAgents() adds trusted agents', async () => {
        const result = await ad4m.runtime.addTrustedAgents(['did:trusted:2']);
        expect(result).toHaveLength(2);
    });

    test('deleteTrustedAgents() removes trusted agents', async () => {
        const result = await ad4m.runtime.deleteTrustedAgents(['did:trusted:2']);
        expect(result).toEqual(['did:trusted:1']);
    });

    test('knownLinkLanguageTemplates() returns templates', async () => {
        const templates = await ad4m.runtime.knownLinkLanguageTemplates();
        expect(templates).toEqual(['lang://template1']);
    });

    test('hcAgentInfos() returns agent infos list', async () => {
        const infos = await ad4m.runtime.hcAgentInfos();
        expect(infos).toEqual(['hc-agent-info-1', 'hc-agent-info-2']);
    });

    test('hcAddAgentInfos() sends array payload and returns boolean', async () => {
        const result = await ad4m.runtime.hcAddAgentInfos(['hc-agent-info-1', 'hc-agent-info-2']);
        expect(result).toBe(true);
        expect(lastRpcCall!.params.agentInfos).toEqual(['hc-agent-info-1', 'hc-agent-info-2']);
    });

    test('verifyStringSignedByDid() verifies signature', async () => {
        const result = await ad4m.runtime.verifyStringSignedByDid('did:test:1', 'key-1', 'data', 'signed');
        expect(result).toBe(true);
        expect(lastRpcCall!.params.did).toBe('did:test:1');
    });

    test('friendStatus() gets friend status', async () => {
        const status = await ad4m.runtime.friendStatus('did:friend:1');
        expect(status.author).toBe('did:friend:1');
    });

    test('friendSendMessage() sends a message', async () => {
        const result = await ad4m.runtime.friendSendMessage('did:friend:1', new Perspective());
        expect(result).toBe(true);
    });

    test('messageInbox() returns inbox messages', async () => {
        const msgs = await ad4m.runtime.messageInbox();
        expect(msgs).toHaveLength(1);
    });

    test('openLink() opens a link', async () => {
        const result = await ad4m.runtime.openLink('https://example.com');
        expect(result).toBe(true);
        expect(lastRpcCall!.params.url).toBe('https://example.com');
    });

    test('notifications() returns notifications', async () => {
        const notifications = await ad4m.runtime.notifications();
        expect(notifications).toEqual([]);
    });

    test('grantNotification() grants a notification', async () => {
        const result = await ad4m.runtime.grantNotification('notif-1');
        expect(result).toBe(true);
    });

    test('exportDb() exports the database', async () => {
        const result = await ad4m.runtime.exportDb('/tmp/export');
        expect(result).toBe(true);
    });

    test('multiUserEnabled() checks multi-user status', async () => {
        const enabled = await ad4m.runtime.multiUserEnabled();
        expect(enabled).toBe(false);
    });

    test('setMultiUserEnabled() enables multi-user', async () => {
        const result = await ad4m.runtime.setMultiUserEnabled(true);
        expect(result).toBe(true);
    });

    test('restartHolochain() restarts holochain', async () => {
        const result = await ad4m.runtime.restartHolochain();
        expect(result).toBe(true);
    });
});

// ===================== AI TESTS =====================
describe('AIClient', () => {
    test('getModels() returns models', async () => {
        const models = await ad4m.ai.getModels();
        expect(models).toHaveLength(1);
        expect(models[0].name).toBe('GPT-Test');
    });

    test('addModel() adds a model', async () => {
        const id = await ad4m.ai.addModel({ name: 'New Model', modelType: 'LLM' } as any);
        expect(id).toBe('model-new-id');
        expect(lastRpcCall!.params.model).toBeDefined();
        expect((lastRpcCall!.params.model as any).type).toBe('LLM');
        expect((lastRpcCall!.params.model as any).modelType).toBeUndefined();
    });

    test('updateModel() updates a model', async () => {
        const result = await ad4m.ai.updateModel('model-1', { name: 'Updated', modelType: 'EMBEDDING' } as any);
        expect(result).toBe(true);
        expect((lastRpcCall!.params.model as any).type).toBe('EMBEDDING');
        expect((lastRpcCall!.params.model as any).modelType).toBeUndefined();
    });

    test('removeModel() removes a model', async () => {
        const result = await ad4m.ai.removeModel('model-1');
        expect(result).toBe(true);
    });

    test('tasks() returns tasks', async () => {
        const tasks = await ad4m.ai.tasks();
        expect(tasks).toHaveLength(1);
    });

    test('addTask() adds a task', async () => {
        const task = await ad4m.ai.addTask('new-task', 'model-1', 'Do stuff', []);
        expect(task.name).toBe('new-task');
    });

    test('removeTask() removes a task', async () => {
        const task = await ad4m.ai.removeTask('task-1');
        expect(task.name).toBe('summarize');
    });

    test('prompt() sends a prompt', async () => {
        const response = await ad4m.ai.prompt('task-1', 'Hello AI');
        expect(response).toBe('This is the AI response');
        expect(lastRpcCall!.params.taskId).toBe('task-1');
        expect(lastRpcCall!.params.prompt).toBe('Hello AI');
    });

    test('modelLoadingStatus() returns status', async () => {
        const status = await ad4m.ai.modelLoadingStatus('model-1');
        expect(status.progress).toBe(100);
    });

    test('setDefaultModel() sets default', async () => {
        const result = await ad4m.ai.setDefaultModel('LLM' as any, 'model-1');
        expect(result).toBe(true);
    });
});

// ===================== USER/AUTH TESTS =====================
describe('User and Auth', () => {
    test('createUser() creates a user', async () => {
        const result = await ad4m.agent.createUser('test@test.com', 'password');
        expect(result.success).toBe(true);
    });

    test('loginUser() returns JWT', async () => {
        const jwt = await ad4m.agent.loginUser('test@test.com', 'password');
        expect(jwt).toBe('login-jwt-token');
    });

    test('verifyEmailCode() returns token', async () => {
        const token = await ad4m.agent.verifyEmailCode('test@test.com', '123456', 'login');
        expect(token).toBe('verified-token');
    });

    test('hostingUserInfo() returns user info', async () => {
        const info = await ad4m.agent.hostingUserInfo();
        expect(info.email).toBe('test@test.com');
    });

    test('requestLoginVerification() sends email and returns result', async () => {
        const result = await ad4m.agent.requestLoginVerification('test@test.com');
        expect(result.success).toBe(true);
        expect(result.isExistingUser).toBe(true);
        expect(lastRpcCall!.params.email).toBe('test@test.com');
    });

    test('setHotWalletAddress() sends address', async () => {
        const result = await ad4m.agent.setHotWalletAddress('0xABC');
        expect(result).toBe(true);
        expect(lastRpcCall!.params.address).toBe('0xABC');
    });

    test('requestPayment() sends amount', async () => {
        const result = await ad4m.agent.requestPayment('100');
        expect(lastRpcCall!.params.amountHOT).toBe('100');
    });
});

// ===================== MULTI-USER EXTENDED TESTS =====================
describe('Multi-user and Hosting', () => {
    test('listUsers() returns user statistics', async () => {
        const users = await ad4m.runtime.listUsers();
        expect(users).toHaveLength(1);
        expect(users[0].email).toBe('user@test.com');
    });

    test('setUserCredits() sets credits for a user', async () => {
        const result = await ad4m.runtime.setUserCredits('user@test.com', 500);
        expect(result).toBe(true);
        expect(lastRpcCall!.params.email).toBe('user@test.com');
        expect(lastRpcCall!.params.amount).toBe(500);
    });

    test('setUserFreeAccess() toggles free access', async () => {
        const result = await ad4m.runtime.setUserFreeAccess('user@test.com', true);
        expect(result).toBe(true);
        expect(lastRpcCall!.params.email).toBe('user@test.com');
        expect(lastRpcCall!.params.enabled).toBe(true);
    });

    test('userWalletAddress() returns wallet address', async () => {
        const addr = await ad4m.runtime.userWalletAddress('user@test.com');
        expect(addr).toBe('0x1234567890abcdef');
    });

    test('emailTestModeEnable() enables email test mode', async () => {
        const result = await ad4m.runtime.emailTestModeEnable();
        expect(result).toBe(true);
        expect(lastRpcCall!.params.action).toBe('enable');
    });

    test('emailTestModeDisable() disables email test mode', async () => {
        const result = await ad4m.runtime.emailTestModeDisable();
        expect(result).toBe(true);
        expect(lastRpcCall!.params.action).toBe('disable');
    });

    test('emailTestClearCodes() clears test codes', async () => {
        const result = await ad4m.runtime.emailTestClearCodes();
        expect(result).toBe(true);
        expect(lastRpcCall!.params.action).toBe('clear-codes');
    });

    test('getHostRates() returns parsed rates', async () => {
        const rates = await ad4m.runtime.getHostRates();
        expect(rates).toHaveLength(1);
        expect(rates[0].description).toBe('Link write');
        expect(rates[0].priceInHOT).toBe(0.001);
    });

    test('setHostRates() sends rates JSON', async () => {
        const result = await ad4m.runtime.setHostRates(JSON.stringify([{ description: 'test', priceInHOT: 1 }]));
        expect(result).toBe(true);
    });
});

// ===================== UNYT / mHOT TESTS =====================
describe('Unyt Integration', () => {
    test('unytAgentKey() returns agent key', async () => {
        const key = await ad4m.runtime.unytAgentKey();
        expect(key).toBe('unyt-agent-key-123');
    });

    test('unytHotAgentPubkey() returns pubkey', async () => {
        const key = await ad4m.runtime.unytHotAgentPubkey();
        expect(key).toBe('unyt-hot-pubkey-456');
    });

    test('unytWalletBalance() returns balance', async () => {
        const balance = await ad4m.runtime.unytWalletBalance();
        expect(balance).toBe('1000.50');
    });

    test('unytWalletHistory() returns history', async () => {
        const history = await ad4m.runtime.unytWalletHistory();
        expect(history).toBe('[]');
    });

    test('unytVersionInfo() returns version info', async () => {
        const info = await ad4m.runtime.unytVersionInfo();
        expect(info).toContain('version');
    });

    test('unytSetMembraneProof() sets proof', async () => {
        const result = await ad4m.runtime.unytSetMembraneProof('proof-data');
        expect(result.success).toBe(true);
        expect(lastRpcCall!.params.proof).toBe('proof-data');
    });

    test('unytReinstallDna() reinstalls DNA', async () => {
        const result = await ad4m.runtime.unytReinstallDna();
        expect(result.success).toBe(true);
    });

    test('unytSendHot() sends HOT tokens', async () => {
        const result = await ad4m.runtime.unytSendHot('recipient-123', '50');
        expect(result.success).toBe(true);
        expect(lastRpcCall!.params.recipient).toBe('recipient-123');
        expect(lastRpcCall!.params.amount).toBe('50');
    });
});

// ===================== AD4M CLIENT INTEGRATION =====================
describe('Ad4mClient', () => {
    test('all sub-clients are accessible', () => {
        expect(ad4m.agent).toBeDefined();
        expect(ad4m.expression).toBeDefined();
        expect(ad4m.languages).toBeDefined();
        expect(ad4m.neighbourhood).toBeDefined();
        expect(ad4m.perspective).toBeDefined();
        expect(ad4m.runtime).toBeDefined();
        expect(ad4m.ai).toBeDefined();
    });

    test('token is passed via WebSocket URL query param', async () => {
        const freshClient = new Ad4mClient('http://127.0.0.1:12000', 'my-secret-token', false);
        await freshClient.agent.me();
        const ws = lastOf(MockWebSocket.instances);
        expect(ws.url).toBe('ws://127.0.0.1:12000/api/v1/ws?token=my-secret-token');
    });

    test('WS RPC message contains type and params', async () => {
        await ad4m.agent.generate('pass');
        expect(lastRpcCall!.type).toBe('agent.generate');
        expect(lastRpcCall!.params).toBeDefined();
    });
});
