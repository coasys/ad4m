import express from 'express';
import { Ad4mClient } from './Ad4mClient';
import { Perspective } from './perspectives/Perspective';
import { LinkQuery } from './perspectives/LinkQuery';

// Save original EventSource so we can restore it after the suite
const originalEventSource = (global as any).EventSource;

class MockEventSource {
    static instances: MockEventSource[] = [];
    onmessage: any = null;
    onerror: any = null;
    closed = false;
    url: string;
    init: any;

    close() {
        this.closed = true;
    }

    emit(payload: unknown) {
        this.onmessage?.({ data: JSON.stringify(payload) });
    }

    constructor(url: string, init?: any) {
        this.url = url;
        this.init = init;
        MockEventSource.instances.push(this);
    }
}

let app: ReturnType<typeof express>;
let httpServer: any;
let baseUrl: string;
let ad4m: Ad4mClient;

// Track requests for assertion
let lastRequest: { method: string; path: string; body: any; query: any; headers: any } | null = null;

function trackRequest(req: express.Request) {
    lastRequest = {
        method: req.method,
        path: req.path,
        body: req.body,
        query: req.query,
        headers: req.headers,
    };
}

beforeAll(async () => {
    (global as any).EventSource = MockEventSource as any;

    app = express();
    app.use(express.json());

    // Middleware to track all requests
    app.use((req, _res, next) => {
        trackRequest(req);
        next();
    });

    // ===================== AGENT ENDPOINTS =====================
    app.get('/api/v1/agent', (_req, res) => res.json({
        did: 'did:test:123',
        perspective: new Perspective(),
        directMessageLanguage: 'lang://dm'
    }));

    app.get('/api/v1/agent/status', (_req, res) => res.json({
        did: 'did:test:123',
        didDocument: 'doc',
        isInitialized: true,
        isUnlocked: true
    }));

    app.post('/api/v1/agent', (_req, res) => res.json({
        did: 'did:test:generated',
        didDocument: 'doc',
        isInitialized: true,
        isUnlocked: true
    }));

    app.post('/api/v1/agent/lock', (_req, res) => res.json({
        did: 'did:test:123',
        isInitialized: true,
        isUnlocked: false
    }));

    app.post('/api/v1/agent/unlock', (_req, res) => res.json({
        did: 'did:test:123',
        isInitialized: true,
        isUnlocked: true
    }));

    app.post('/api/v1/agent/import', (_req, res) => res.json({
        did: 'did:test:imported',
        isInitialized: true,
        isUnlocked: true
    }));

    app.get('/api/v1/agent/by-did/:did', (req, res) => res.json({
        did: req.params.did,
        perspective: new Perspective()
    }));

    app.patch('/api/v1/agent/profile', (_req, res) => res.json({
        did: 'did:test:123',
        perspective: new Perspective(),
        directMessageLanguage: 'lang://dm'
    }));

    app.post('/api/v1/agent/entanglement-proofs', (_req, res) => res.json([
        { did: 'did:test:123', deviceKey: 'key1', deviceKeyType: 'type1' }
    ]));

    app.delete('/api/v1/agent/entanglement-proofs', (_req, res) => res.json([]));

    app.get('/api/v1/agent/entanglement-proofs', (_req, res) => res.json(['proof1', 'proof2']));

    app.post('/api/v1/agent/entanglement-proof-preflight', (_req, res) => res.json({
        did: 'did:test:123', deviceKey: 'key1', deviceKeyType: 'type1'
    }));

    app.post('/api/v1/agent/sign', (_req, res) => res.json('signed-message-data'));

    app.get('/api/v1/agent/is-locked', (_req, res) => res.json(false));

    app.post('/api/v1/agent/auth/request', (_req, res) => res.json('request-id-123'));
    app.post('/api/v1/agent/auth/permit', (_req, res) => res.json('permitted-token'));
    app.post('/api/v1/agent/auth/jwt', (_req, res) => res.json('jwt-token-abc'));

    app.get('/api/v1/agent/apps', (_req, res) => res.json([
        { requestId: 'app1', auth: {}, token: 'tok', revoked: false }
    ]));
    app.delete('/api/v1/agent/apps/:requestId', (_req, res) => res.json([]));
    app.delete('/api/v1/agent/auth/token/:requestId', (_req, res) => res.json([]));

    // Trusted agents (used by RuntimeClient but routed under /agent)
    app.get('/api/v1/agent/trusted', (_req, res) => res.json(['did:trusted:1']));
    app.post('/api/v1/agent/trusted', (_req, res) => res.json(['did:trusted:1', 'did:trusted:2']));
    app.delete('/api/v1/agent/trusted', (_req, res) => res.json(['did:trusted:1']));

    // ===================== PERSPECTIVE ENDPOINTS =====================
    app.get('/api/v1/perspectives', (_req, res) => res.json([
        { uuid: 'uuid-1', name: 'test-perspective', sharedUrl: null, neighbourhood: null, state: 'Synced' }
    ]));

    app.get('/api/v1/perspectives/:uuid', (req, res) => res.json({
        uuid: req.params.uuid, name: 'test-perspective', sharedUrl: null, neighbourhood: null, state: 'Synced'
    }));

    app.get('/api/v1/perspectives/:uuid/snapshot', (_req, res) => res.json({
        links: [{ author: 'did:test:123', timestamp: '2024-01-01', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } }]
    }));

    app.post('/api/v1/perspectives', (req, res) => res.json({
        uuid: 'uuid-new', name: req.body.name, sharedUrl: null, neighbourhood: null, state: 'Synced'
    }));

    app.put('/api/v1/perspectives/:uuid', (req, res) => res.json({
        uuid: req.params.uuid, name: req.body.name, sharedUrl: null, neighbourhood: null, state: 'Synced'
    }));

    app.delete('/api/v1/perspectives/:uuid', (_req, res) => res.json(true));

    app.get('/api/v1/perspectives/:uuid/links', (_req, res) => res.json([
        { author: 'did:test:123', timestamp: '2024-01-01', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } }
    ]));

    app.post('/api/v1/perspectives/:uuid/links', (_req, res) => res.json({
        author: 'did:test:123', timestamp: '2024-01-01',
        data: { source: 's', predicate: 'p', target: 't' },
        proof: { valid: true }
    }));

    app.post('/api/v1/perspectives/:uuid/links/bulk', (_req, res) => res.json([
        { author: 'did:test:123', timestamp: '2024-01-01', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } }
    ]));

    app.post('/api/v1/perspectives/:uuid/links/remove-bulk', (_req, res) => res.json([]));

    app.post('/api/v1/perspectives/:uuid/links/mutations', (_req, res) => res.json({
        additions: [{ author: 'did:test:123', timestamp: '2024-01-01', data: { source: 'a', predicate: 'p', target: 't' }, proof: { valid: true } }],
        removals: []
    }));

    app.post('/api/v1/perspectives/:uuid/links/expression', (_req, res) => res.json({
        author: 'did:test:123', timestamp: '2024-01-01', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true }
    }));

    app.put('/api/v1/perspectives/:uuid/links', (_req, res) => res.json({
        author: 'did:test:123', timestamp: '2024-01-01', data: { source: 'new-s', predicate: 'p', target: 't' }, proof: { valid: true }
    }));

    app.delete('/api/v1/perspectives/:uuid/links', (_req, res) => res.json(true));

    app.post('/api/v1/perspectives/:uuid/sdna', (_req, res) => res.json(true));
    app.post('/api/v1/perspectives/:uuid/execute-commands', (_req, res) => res.json(true));
    app.post('/api/v1/perspectives/:uuid/create-subject', (_req, res) => res.json(true));
    app.post('/api/v1/perspectives/:uuid/get-subject-data', (_req, res) => res.json('{"name":"test"}'));
    app.post('/api/v1/perspectives/:uuid/publish-snapshot', (_req, res) => res.json('Qm123'));

    app.post('/api/v1/perspectives/:uuid/query/prolog', (_req, res) => res.json(JSON.stringify([{X: 'test'}])));
    app.post('/api/v1/perspectives/:uuid/query/surreal', (_req, res) => res.json(JSON.stringify([{id: '1'}])));

    app.post('/api/v1/perspectives/:uuid/batch', (_req, res) => res.json('batch-id-1'));
    app.post('/api/v1/perspectives/:uuid/batch/commit', (_req, res) => res.json({ additions: [], removals: [] }));

    // ===================== LANGUAGE ENDPOINTS =====================
    app.get('/api/v1/languages', (_req, res) => res.json([
        { name: 'test-lang', address: 'lang://test', settings: '{}', icon: null, constructorIcon: null }
    ]));

    app.get('/api/v1/languages/:address', (req, res) => {
        if (req.path.endsWith('/meta')) return; // handled below
        if (req.path.endsWith('/source')) return;
        res.json({ name: 'test-lang', address: req.params.address, settings: '{}', icon: null, constructorIcon: null });
    });

    app.get('/api/v1/languages/:address/meta', (_req, res) => res.json({
        name: 'test-lang', address: 'lang://test', description: 'A test language',
        author: 'did:test:123', templated: false, templateSourceLanguageAddress: null,
        templateAppliedParams: null, possibleTemplateParams: null, sourceCodeLink: null
    }));

    app.get('/api/v1/languages/:address/source', (_req, res) => res.json('source-code-here'));

    app.put('/api/v1/languages/:address/settings', (_req, res) => res.json(true));

    app.post('/api/v1/languages/apply-template', (_req, res) => res.json({
        name: 'applied-lang', address: 'lang://applied'
    }));

    app.post('/api/v1/languages/publish', (_req, res) => res.json({
        name: 'published-lang', address: 'lang://published', description: 'Published',
        author: 'did:test:123', templated: false
    }));

    app.delete('/api/v1/languages/:address', (_req, res) => res.json(true));

    // ===================== NEIGHBOURHOOD ENDPOINTS =====================
    app.post('/api/v1/neighbourhoods/publish', (_req, res) => res.json('neighbourhood://published'));

    app.post('/api/v1/neighbourhoods/join', (_req, res) => res.json({
        uuid: 'uuid-joined', name: 'joined-neighbourhood', sharedUrl: 'neighbourhood://url',
        neighbourhood: {}, state: 'Synced'
    }));

    app.get('/api/v1/neighbourhoods/:uuid/other-agents', (_req, res) => res.json(['did:other:1', 'did:other:2']));
    app.get('/api/v1/neighbourhoods/:uuid/has-telepresence-adapter', (_req, res) => res.json(true));
    app.get('/api/v1/neighbourhoods/:uuid/online-agents', (_req, res) => res.json([
        { did: 'did:test:1', status: new Perspective() }
    ]));
    app.put('/api/v1/neighbourhoods/:uuid/online-status', (_req, res) => res.json(true));
    app.put('/api/v1/neighbourhoods/:uuid/online-status-unsigned', (_req, res) => res.json(true));
    app.post('/api/v1/neighbourhoods/:uuid/signal', (_req, res) => res.json(true));
    app.post('/api/v1/neighbourhoods/:uuid/signal-unsigned', (_req, res) => res.json(true));
    app.post('/api/v1/neighbourhoods/:uuid/broadcast', (_req, res) => res.json(true));
    app.post('/api/v1/neighbourhoods/:uuid/broadcast-unsigned', (_req, res) => res.json(true));

    // ===================== EXPRESSION ENDPOINTS =====================
    app.get('/api/v1/expressions', (req, res) => res.json({
        author: 'did:test:123', timestamp: '2024-01-01', data: '{"content":"hello"}',
        language: { address: 'lang://test' }, proof: { valid: true }
    }));

    app.get('/api/v1/expressions/many', (_req, res) => res.json([
        { author: 'did:test:123', timestamp: '2024-01-01', data: '{"content":"hello"}', language: { address: 'lang://test' }, proof: { valid: true } }
    ]));

    app.get('/api/v1/expressions/raw', (_req, res) => res.json('raw-expression-data'));

    app.post('/api/v1/expressions', (_req, res) => res.json('Qm-expression-hash'));

    app.get('/api/v1/expressions/interactions', (_req, res) => res.json([
        { label: 'interact1', name: 'doSomething', parameters: [] }
    ]));

    app.post('/api/v1/expressions/interact', (_req, res) => res.json('interaction-result'));

    // ===================== RUNTIME ENDPOINTS =====================
    app.get('/api/v1/runtime/info', (_req, res) => res.json({
        ad4mExecutorVersion: '0.1.0', isUnlocked: true, isInitialized: true
    }));

    app.get('/api/v1/runtime/tls-domain', (_req, res) => res.json('test.domain.com'));

    app.post('/api/v1/runtime/quit', (_req, res) => res.json(true));
    app.post('/api/v1/runtime/open-link', (_req, res) => res.json(true));

    app.get('/api/v1/runtime/link-language-templates', (_req, res) => res.json(['lang://template1']));
    app.post('/api/v1/runtime/link-language-templates', (_req, res) => res.json(['lang://template1', 'lang://template2']));
    app.delete('/api/v1/runtime/link-language-templates', (_req, res) => res.json(['lang://template1']));

    app.get('/api/v1/runtime/friends', (_req, res) => res.json(['did:friend:1', 'did:friend:2']));
    app.post('/api/v1/runtime/friends', (_req, res) => res.json(['did:friend:1', 'did:friend:2', 'did:friend:3']));
    app.delete('/api/v1/runtime/friends', (_req, res) => res.json(['did:friend:1']));

    app.get('/api/v1/runtime/hc/agent-infos', (_req, res) => res.json(['hc-agent-info-1', 'hc-agent-info-2']));
    app.post('/api/v1/runtime/hc/agent-infos', (req, res) => {
        if (!Array.isArray(req.body.agentInfos)) {
            return res.status(400).json({ error: 'agentInfos must be an array' });
        }

        return res.json(true);
    });
    app.get('/api/v1/runtime/network-metrics', (_req, res) => res.json('metrics-data'));
    app.post('/api/v1/runtime/holochain/restart', (_req, res) => res.json(true));

    app.post('/api/v1/runtime/verify-signature', (_req, res) => res.json(true));

    app.put('/api/v1/runtime/status', (_req, res) => res.json(true));
    app.get('/api/v1/runtime/friends/:did/status', (_req, res) => res.json({
        author: 'did:friend:1', timestamp: '2024-01-01', data: new Perspective()
    }));
    app.post('/api/v1/runtime/friends/:did/message', (_req, res) => res.json(true));

    app.get('/api/v1/runtime/messages/inbox', (_req, res) => res.json([
        { author: 'did:friend:1', timestamp: '2024-01-01', data: new Perspective() }
    ]));
    app.get('/api/v1/runtime/messages/outbox', (_req, res) => res.json([]));

    app.post('/api/v1/runtime/notifications', (_req, res) => res.json(true));
    app.get('/api/v1/runtime/notifications', (_req, res) => res.json([]));
    app.patch('/api/v1/runtime/notifications/:id', (_req, res) => res.json(true));
    app.patch('/api/v1/runtime/notifications/:id/grant', (_req, res) => res.json(true));
    app.put('/api/v1/runtime/notifications/:id', (_req, res) => res.json(true));
    app.delete('/api/v1/runtime/notifications/:id', (_req, res) => res.json(true));

    app.post('/api/v1/runtime/export', (_req, res) => res.json(true));
    app.post('/api/v1/runtime/import', (_req, res) => res.json({ success: true, count: 5 }));
    app.post('/api/v1/runtime/export-perspective', (_req, res) => res.json(true));
    app.post('/api/v1/runtime/import-perspective', (_req, res) => res.json(true));

    app.get('/api/v1/runtime/multi-user-enabled', (_req, res) => res.json(false));
    app.put('/api/v1/runtime/multi-user-enabled', (_req, res) => res.json(true));
    app.get('/api/v1/runtime/free-hosting-enabled', (_req, res) => res.json(false));
    app.put('/api/v1/runtime/free-hosting-enabled', (_req, res) => res.json(true));

    app.get('/api/v1/runtime/users', (_req, res) => res.json([]));

    // ===================== AI ENDPOINTS =====================
    app.get('/api/v1/ai/models', (_req, res) => res.json([
        { id: 'model-1', name: 'GPT-Test', modelType: 'LLM', api: 'openai' }
    ]));

    app.post('/api/v1/ai/models', (_req, res) => res.json('model-new-id'));
    app.put('/api/v1/ai/models/:modelId', (_req, res) => res.json(true));
    app.delete('/api/v1/ai/models/:modelId', (_req, res) => res.json(true));
    app.put('/api/v1/ai/models/default', (_req, res) => res.json(true));
    app.get('/api/v1/ai/models/default', (_req, res) => res.json({ id: 'model-1', name: 'GPT-Test', modelType: 'LLM' }));

    app.get('/api/v1/ai/tasks', (_req, res) => res.json([
        { taskId: 'task-1', name: 'summarize', modelId: 'model-1', systemPrompt: 'Summarize', promptExamples: [] }
    ]));
    app.post('/api/v1/ai/tasks', (_req, res) => res.json({
        taskId: 'task-new', name: 'new-task', modelId: 'model-1', systemPrompt: 'Do stuff', promptExamples: []
    }));
    app.put('/api/v1/ai/tasks/:taskId', (_req, res) => res.json({
        taskId: 'task-1', name: 'updated', modelId: 'model-1', systemPrompt: 'Updated', promptExamples: []
    }));
    app.delete('/api/v1/ai/tasks/:taskId', (_req, res) => res.json({
        taskId: 'task-1', name: 'summarize', modelId: 'model-1', systemPrompt: 'Summarize', promptExamples: []
    }));

    app.post('/api/v1/ai/prompt', (_req, res) => res.json('This is the AI response'));

    app.get('/api/v1/ai/model-loading-status', (_req, res) => res.json({
        model: 'model-1', progress: 100, status: 'loaded'
    }));

    // ===================== USER/HOSTING ENDPOINTS =====================
    app.post('/api/v1/users', (_req, res) => res.json({ success: true, did: 'did:test:new-user' }));
    app.post('/api/v1/users/login', (_req, res) => res.json('login-jwt-token'));
    app.post('/api/v1/users/verify-email', (_req, res) => res.json('verified-token'));

    app.get('/api/v1/runtime/hosting/user-info', (_req, res) => res.json({ email: 'test@test.com' }));
    app.get('/api/v1/runtime/compute-log', (_req, res) => res.json([]));
    app.put('/api/v1/runtime/hosting/hot-wallet-address', (_req, res) => res.json(true));
    app.post('/api/v1/runtime/hosting/request-payment', (_req, res) => res.json({ paymentUrl: 'https://pay.test' }));

    // ── Missing routes (client paths that differ from original stubs) ──
    app.post('/api/v1/agent/generate', (_req, res) => res.json({
        did: 'did:test:generated',
        didDocument: 'doc',
        isInitialized: true,
        isUnlocked: true
    }));
    app.put('/api/v1/agent/trusted', (_req, res) => res.json(['did:trusted:1', 'did:trusted:2']));
    app.post('/api/v1/perspectives/:uuid/query', (_req, res) => res.json(JSON.stringify([{X: 'test'}])));
    app.get('/api/v1/neighbourhoods/:uuid/has-telepresence', (_req, res) => res.json(true));
    app.post('/api/v1/expressions/many', (_req, res) => res.json([
        { author: 'did:test:1', timestamp: '2023-01-01', data: { type: 'test' }, language: { address: 'lang://test' }, proof: { valid: true } }
    ]));
    app.get('/api/v1/expressions/:url', (req, res) => {
        if (req.query.raw === 'true') return res.json('raw-expression-data');
        res.json({ author: 'did:test:123', timestamp: '2024-01-01', data: '{"content":"hello"}', language: { address: 'lang://test' }, proof: { valid: true } });
    });
    app.get('/api/v1/expressions/:url/interactions', (_req, res) => res.json([
        { label: 'interact1', name: 'doSomething', parameters: [] }
    ]));
    app.post('/api/v1/expressions/:url/interact', (_req, res) => res.json('interaction-result'));
    app.put('/api/v1/runtime/friends', (_req, res) => res.json(['did:friend:1', 'did:friend:2', 'did:friend:3']));
    app.get('/api/v1/runtime/friends/:did', (req, res) => res.json({
        author: req.params.did, timestamp: '2023-01-01', data: JSON.stringify({ recipe_name: 'test' }), proof: { valid: true }
    }));
    app.get('/api/v1/users/multi-user-enabled', (_req, res) => res.json(false));
    app.put('/api/v1/users/multi-user-enabled', (_req, res) => res.json(true));
    app.put('/api/v1/ai/models/:modelId/default', (_req, res) => res.json(true));
    app.get('/api/v1/hosting', (_req, res) => res.json({ email: 'test@test.com' }));

    // ===================== SSE EVENTS (stub - not testing SSE) =====================
    // SSE endpoints return 404 to avoid hanging; subscribe=false prevents client from connecting
    app.get('/api/v1/events/*', (_req, res) => res.status(404).end());

    httpServer = app.listen(0);
    const addr = httpServer.address() as { port: number };
    baseUrl = `http://127.0.0.1:${addr.port}`;
    ad4m = new Ad4mClient(baseUrl, 'test-token', false);
});

afterAll(() => {
    httpServer?.close();
    (global as any).EventSource = originalEventSource;
});

beforeEach(() => {
    lastRequest = null;
    MockEventSource.instances = [];
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
        expect(lastRequest!.body.passphrase).toBe('secret123');
        expect(status.did).toBe('did:test:generated');
    });

    test('lock() locks the agent', async () => {
        const status = await ad4m.agent.lock('secret123');
        expect(lastRequest!.body.passphrase).toBe('secret123');
        expect(status.isUnlocked).toBe(false);
    });

    test('unlock() unlocks the agent', async () => {
        const status = await ad4m.agent.unlock('secret123');
        expect(lastRequest!.body.passphrase).toBe('secret123');
        expect(lastRequest!.body.holochain).toBe(true);
        expect(status.isUnlocked).toBe(true);
    });

    test('import() imports a DID keystore', async () => {
        const status = await ad4m.agent.import({
            did: 'did:test:import', didDocument: 'doc', keystore: 'ks', passphrase: 'pass'
        });
        expect(lastRequest!.body.did).toBe('did:test:import');
        expect(status.did).toBe('did:test:imported');
    });

    test('byDID() fetches agent by DID', async () => {
        const agent = await ad4m.agent.byDID('did:test:other');
        expect(agent.did).toBe('did:test:other');
    });

    test('signMessage() signs a message', async () => {
        const signed = await ad4m.agent.signMessage('hello');
        expect(lastRequest!.body.message).toBe('hello');
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
        expect(lastRequest!.body.requestId).toBe('req-1');
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

    test('agent-updated SSE event unwraps nested agent payload', async () => {
        const freshClient = new Ad4mClient(baseUrl, 'test-token', false);
        const callback = jest.fn();
        freshClient.agent.addUpdatedListener(callback);
        freshClient.agent.subscribeAgentUpdated();

        const eventSource = MockEventSource.instances.at(-1)!;

        // Server now sends { type: "agent-updated", agent: { did, ... } }
        eventSource.emit({
            type: 'agent-updated',
            agent: { did: 'did:test:updated', directMessageLanguage: 'lang://dm2', perspective: null, isInitialized: true, isUnlocked: true },
        });

        expect(callback).toHaveBeenCalledTimes(1);
        const received = callback.mock.calls[0][0];
        expect(received.did).toBe('did:test:updated');
        expect(received.directMessageLanguage).toBe('lang://dm2');
        // The type field from the SSE envelope must NOT leak into the agent object
        expect(received).not.toHaveProperty('type');
    });

    test('agent-status-changed SSE event unwraps nested agent payload', async () => {
        const freshClient = new Ad4mClient(baseUrl, 'test-token', false);
        const callback = jest.fn();
        freshClient.agent.addAgentStatusChangedListener(callback);
        freshClient.agent.subscribeAgentStatusChanged();

        const eventSource = MockEventSource.instances.at(-1)!;

        eventSource.emit({
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
        expect(lastRequest!.body.name).toBe('new-perspective');
        expect(p.uuid).toBe('uuid-new');
    });

    test('update() updates a perspective name', async () => {
        const p = await ad4m.perspective.update('uuid-1', 'renamed');
        expect(lastRequest!.body.name).toBe('renamed');
        expect(p.name).toBe('renamed');
    });

    test('remove() deletes a perspective', async () => {
        const result = await ad4m.perspective.remove('uuid-1');
        expect(result.perspectiveRemove).toBe(true);
    });

    test('queryLinks() queries links with parameters', async () => {
        const links = await ad4m.perspective.queryLinks('uuid-1', new LinkQuery({ source: 'src', predicate: 'pred' }));
        expect(links).toHaveLength(1);
        expect(lastRequest!.query.source).toBe('src');
        expect(lastRequest!.query.predicate).toBe('pred');
    });

    test('addLink() adds a link', async () => {
        const link = await ad4m.perspective.addLink('uuid-1', { source: 's', predicate: 'p', target: 't' });
        expect(link.data.source).toBe('s');
        expect(lastRequest!.body.link.source).toBe('s');
    });

    test('addLinks() adds multiple links', async () => {
        const links = await ad4m.perspective.addLinks('uuid-1', [
            { source: 's1', predicate: 'p', target: 't1' },
            { source: 's2', predicate: 'p', target: 't2' }
        ]);
        expect(links).toHaveLength(1);
        expect(lastRequest!.body.links).toHaveLength(2);
    });

    test('removeLinks() forwards batchId in the bulk remove request', async () => {
        const removed = await ad4m.perspective.removeLinks('uuid-1', [{
            author: 'a',
            timestamp: 't',
            data: { source: 's', predicate: 'p', target: 't' },
            proof: { valid: true }
        }] as any, 'batch-id-1');
        expect(removed).toHaveLength(0);
        expect(lastRequest!.path).toBe('/api/v1/perspectives/uuid-1/links/remove-bulk');
        expect(lastRequest!.body.links).toHaveLength(1);
        expect(lastRequest!.body.batchId).toBe('batch-id-1');
    });

    test('updateLink() updates a link', async () => {
        const link = await ad4m.perspective.updateLink('uuid-1',
            { author: 'a', timestamp: 't', data: { source: 's', predicate: 'p', target: 't' }, proof: { valid: true } } as any,
            { source: 'new-s', predicate: 'p', target: 't' }
        );
        expect(link.data.source).toBe('new-s');
    });

    test('removeLink() removes a link', async () => {
        const result = await ad4m.perspective.removeLink('uuid-1', {
            author: 'a', timestamp: 't',
            data: { source: 's', predicate: 'p', target: 't' },
            proof: { valid: true }
        } as any);
        expect(result).toBe(true);
    });

    test('linkMutations() applies mutations', async () => {
        const result = await ad4m.perspective.linkMutations('uuid-1', {
            additions: [{ source: 'a', predicate: 'p', target: 't' }],
            removals: []
        });
        expect(result.additions).toHaveLength(1);
    });

    test('addSdna() adds SDNA', async () => {
        const result = await ad4m.perspective.addSdna('uuid-1', 'TestClass', 'code', 'subject_class');
        expect(result).toBe(true);
    });

    test('queryProlog() runs prolog query', async () => {
        const result = await ad4m.perspective.queryProlog('uuid-1', 'test(X)');
        expect(result).toEqual([{X: 'test'}]);
    });

    test('subscribeToQueryUpdates() uses the dedicated query-subscription SSE endpoint', async () => {
        const callback = jest.fn();
        const unsubscribe = ad4m.perspective.subscribeToQueryUpdates('sub-1', callback);
        const eventSource = MockEventSource.instances.at(-1)!;

        expect(eventSource.url).toBe(`${baseUrl}/api/v1/events/query-subscription/sub-1?token=test-token`);

        unsubscribe();
        expect(eventSource.closed).toBe(true);
    });

    test('perspective lifecycle subscriptions still use the unified SSE endpoint', async () => {
        const freshClient = new Ad4mClient(baseUrl, 'test-token', false);
        freshClient.perspective.addPerspectiveAddedListener(jest.fn());
        freshClient.perspective.subscribePerspectiveAdded();

        const eventSource = MockEventSource.instances.at(-1)!;
        expect(eventSource.url).toBe(`${baseUrl}/api/v1/events/unified?token=test-token`);
    });

    test('perspective-scoped link subscriptions ignore unified SSE events for other perspectives', async () => {
        const freshClient = new Ad4mClient(baseUrl, 'test-token', false);
        const linkAddedCallback = jest.fn();
        const linkRemovedCallback = jest.fn();
        const linkUpdatedCallback = jest.fn();

        await freshClient.perspective.addPerspectiveLinkAddedListener('uuid-1', [linkAddedCallback]);
        await freshClient.perspective.addPerspectiveLinkRemovedListener('uuid-1', [linkRemovedCallback]);
        await freshClient.perspective.addPerspectiveLinkUpdatedListener('uuid-1', [linkUpdatedCallback]);

        const eventSource = MockEventSource.instances.at(-1)!;
        expect(eventSource.url).toBe(`${baseUrl}/api/v1/events/unified?token=test-token`);

        eventSource.emit({
            type: 'link-added',
            perspectiveUuid: 'uuid-2',
            link: {
                author: 'did:test:123',
                timestamp: '2024-01-01T00:00:00.000Z',
                data: { source: 'test://other-added', predicate: 'test://has', target: 'test://value' },
                proof: { valid: true }
            }
        });
        eventSource.emit({
            type: 'link-removed',
            perspectiveUuid: 'uuid-2',
            link: {
                author: 'did:test:123',
                timestamp: '2024-01-01T00:00:00.000Z',
                data: { source: 'test://other-removed', predicate: 'test://has', target: 'test://value' },
                proof: { valid: true }
            }
        });
        eventSource.emit({
            type: 'link-updated',
            perspectiveUuid: 'uuid-2',
            oldLink: {
                author: 'did:test:123',
                timestamp: '2024-01-01T00:00:00.000Z',
                data: { source: 'test://other-old', predicate: 'test://has', target: 'test://value' },
                proof: { valid: true }
            },
            newLink: {
                author: 'did:test:123',
                timestamp: '2024-01-01T00:00:00.000Z',
                data: { source: 'test://other-new', predicate: 'test://has', target: 'test://value' },
                proof: { valid: true }
            }
        });

        expect(linkAddedCallback).not.toHaveBeenCalled();
        expect(linkRemovedCallback).not.toHaveBeenCalled();
        expect(linkUpdatedCallback).not.toHaveBeenCalled();

        const addedLink = {
            author: 'did:test:123',
            timestamp: '2024-01-01T00:00:00.000Z',
            data: { source: 'test://added', predicate: 'test://has', target: 'test://value' },
            proof: { valid: true }
        };
        const removedLink = {
            author: 'did:test:123',
            timestamp: '2024-01-01T00:00:00.000Z',
            data: { source: 'test://removed', predicate: 'test://has', target: 'test://value' },
            proof: { valid: true }
        };

        eventSource.emit({
            type: 'link-added',
            perspectiveUuid: 'uuid-1',
            link: addedLink
        });
        eventSource.emit({
            type: 'link-removed',
            perspectiveUuid: 'uuid-1',
            link: removedLink
        });
        eventSource.emit({
            type: 'link-updated',
            perspectiveUuid: 'uuid-1',
            oldLink: {
                author: 'did:test:123',
                timestamp: '2024-01-01T00:00:00.000Z',
                data: { source: 'test://updated-old', predicate: 'test://has', target: 'test://value' },
                proof: { valid: true }
            },
            newLink: {
                author: 'did:test:123',
                timestamp: '2024-01-01T00:00:00.000Z',
                data: { source: 'test://updated-new', predicate: 'test://has', target: 'test://value' },
                proof: { valid: true }
            }
        });

        expect(linkAddedCallback).toHaveBeenCalledWith(addedLink);
        expect(linkRemovedCallback).toHaveBeenCalledWith(removedLink);
        expect(linkUpdatedCallback).toHaveBeenCalledTimes(1);
    });

    test('subscriptions keep using the module-native fetch even if global.fetch changes later', async () => {
        const originalFetch = global.fetch;
        const fakeFetch = jest.fn(async () => {
            throw new Error('should not be used by EventSource');
        }) as any;

        global.fetch = fakeFetch;
        try {
            const freshClient = new Ad4mClient(baseUrl, 'test-token', false);
            freshClient.runtime.addExceptionCallback(jest.fn(() => null));
            freshClient.runtime.subscribeExceptionOccurred();

            const eventSource = MockEventSource.instances.at(-1)!;
            expect(eventSource.url).toBe(`${baseUrl}/api/v1/events/unified?token=test-token`);
            expect(eventSource.init?.fetch).toBeDefined();
            expect(eventSource.init?.fetch).not.toBe(fakeFetch);
        } finally {
            global.fetch = originalFetch;
        }
    });

    test('runtime exception subscriptions normalize PascalCase exception types from REST', async () => {
        const freshClient = new Ad4mClient(baseUrl, 'test-token', false);
        const callback = jest.fn(() => null);
        freshClient.runtime.addExceptionCallback(callback);
        freshClient.runtime.subscribeExceptionOccurred();

        const eventSource = MockEventSource.instances.at(-1)!;
        eventSource.emit({
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

    test('subscribeToQueryUpdates() ignores unrelated SSE events and accepts object results', async () => {
        const callback = jest.fn();
        const unsubscribe = ad4m.perspective.subscribeToQueryUpdates('sub-1', callback);
        const eventSource = MockEventSource.instances.at(-1)!;

        eventSource.emit({ type: 'perspective-added', perspective: { uuid: 'uuid-ignored' } });
        eventSource.emit({ type: 'query-subscription-update', subscriptionId: 'sub-2', result: { ignored: true } });
        expect(callback).not.toHaveBeenCalled();

        eventSource.emit({
            type: 'query-subscription-update',
            subscriptionId: 'sub-1',
            result: [{ id: 'community://1', name: 'REST Smoke Community' }],
        });
        expect(callback).toHaveBeenCalledWith([{ id: 'community://1', name: 'REST Smoke Community' }]);

        unsubscribe();
        eventSource.emit({
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
        const langs = await ad4m.languages.byFilter('test');
        expect(lastRequest!.query.filter).toBe('test');
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
        expect(lastRequest!.body.settings).toBe('{"key":"value"}');
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
            'uuid-1', 'lang://link', new Perspective()
        );
        expect(url).toBe('neighbourhood://published');
        expect(lastRequest!.body.perspectiveUUID).toBe('uuid-1');
    });

    test('joinFromUrl() joins a neighbourhood', async () => {
        const handle = await ad4m.neighbourhood.joinFromUrl('neighbourhood://test');
        expect(handle.uuid).toBe('uuid-joined');
        expect(lastRequest!.body.url).toBe('neighbourhood://test');
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
        expect(lastRequest!.body.remoteAgentDid).toBe('did:other:1');
    });

    test('sendBroadcast() sends a broadcast', async () => {
        const result = await ad4m.neighbourhood.sendBroadcast('uuid-1', new Perspective(), true);
        expect(result).toBe(true);
        expect(lastRequest!.body.loopback).toBe(true);
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
        expect(lastRequest!.body.languageAddress).toBe('lang://test');
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
        expect(lastRequest!.body.dids).toEqual(['did:friend:3']);
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
        expect(lastRequest!.body.agentInfos).toEqual(['hc-agent-info-1', 'hc-agent-info-2']);
    });

    test('verifyStringSignedByDid() verifies signature', async () => {
        const result = await ad4m.runtime.verifyStringSignedByDid('did:test:1', 'key-1', 'data', 'signed');
        expect(result).toBe(true);
        expect(lastRequest!.body.did).toBe('did:test:1');
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
        expect(lastRequest!.body.url).toBe('https://example.com');
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
        expect(lastRequest!.body.model.type).toBe('LLM');
        expect(lastRequest!.body.model.modelType).toBeUndefined();
    });

    test('updateModel() updates a model', async () => {
        const result = await ad4m.ai.updateModel('model-1', { name: 'Updated', modelType: 'EMBEDDING' } as any);
        expect(result).toBe(true);
        expect(lastRequest!.body.model.type).toBe('EMBEDDING');
        expect(lastRequest!.body.model.modelType).toBeUndefined();
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
        expect(lastRequest!.body.taskId).toBe('task-1');
        expect(lastRequest!.body.prompt).toBe('Hello AI');
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

    test('auth header is sent with requests', async () => {
        await ad4m.agent.me();
        expect(lastRequest!.headers.authorization).toBe('Bearer test-token');
    });

    test('content-type header is JSON', async () => {
        await ad4m.agent.generate('pass');
        expect(lastRequest!.headers['content-type']).toContain('application/json');
    });
});
