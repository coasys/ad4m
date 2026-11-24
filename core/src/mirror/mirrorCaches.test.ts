/**
 * The mirror's keyed caches (profiles, expressions): one set per client, the expressions
 * kept through a reconnect, a persistent tier keyed by origin + acting DID + key whose
 * values are shown stale and read once per session.
 */
import { Ad4mClient } from '../Ad4mClient';
import { AgentClient } from '../agent/AgentClient';
import { ExpressionClient } from '../expression/ExpressionClient';
import { KeyedCache } from '../cache/KeyedCache';
import { Mirror } from './Mirror';

// An in-memory IndexedDB: one map per database and store name, shared by every client.
const databases = new Map<string, Map<string, unknown>>();
jest.mock('../cache/PersistentCache', () => ({
    ...jest.requireActual('../cache/PersistentCache'),
    createPersistentCache: (db: string, store: string) => {
        const name = `${db}/${store}`;
        if (!databases.has(name)) databases.set(name, new Map());
        return {
            get: async (key: string) => databases.get(name)!.get(key),
            put: async (key: string, value: unknown) => { databases.get(name)!.set(key, value); },
            delete: async (key: string) => { databases.get(name)!.delete(key); },
            clear: async (prefix?: string) => {
                const store = databases.get(name)!;
                for (const key of [...store.keys()]) if (prefix === undefined || key.startsWith(prefix)) store.delete(key);
            },
        };
    },
}));

const ORIGIN = 'http://127.0.0.1:12000';
const agentStore = () => databases.get('ad4m-agent-cache/agents')!;

async function until(condition: () => boolean): Promise<void> {
    while (!condition()) await new Promise(r => setTimeout(r, 1));
}

// ===================== a socket that answers as two managed users =====================

let rpcLog: { type: string; token: string; params: any }[] = [];

class FakeWebSocket {
    static instances: FakeWebSocket[] = [];
    static CONNECTING = 0; static OPEN = 1; static CLOSING = 2; static CLOSED = 3;
    readonly CONNECTING = 0; readonly OPEN = 1; readonly CLOSING = 2; readonly CLOSED = 3;
    onopen: ((e: any) => void) | null = null;
    onmessage: ((e: { data: string }) => void) | null = null;
    onerror: ((e: any) => void) | null = null;
    onclose: ((e: any) => void) | null = null;
    readyState = 0;
    token: string;

    constructor(url: string) {
        this.token = new URL(url).searchParams.get('token') ?? '';
        FakeWebSocket.instances.push(this);
        setTimeout(() => { this.readyState = 1; this.onopen?.({}); }, 0);
    }

    close() { this.readyState = 3; this.onclose?.({}); }

    send(data: string) {
        const { id, type, params } = JSON.parse(data);
        if (type === 'ping') { setTimeout(() => this.onmessage?.({ data: JSON.stringify({ type: 'pong' }) }), 0); return; }
        rpcLog.push({ type, token: this.token, params });
        const result =
            type === 'agent.status' ? { did: `did:test:${this.token}`, isInitialized: true, isUnlocked: true }
            : type === 'agent.byDIDs' ? params.dids.map((did: string) => ({ did, perspective: null, directMessageLanguage: 'lang://net' }))
            : type === 'expression.get' ? { author: 'did:x', timestamp: 't', data: params.url, language: { address: 'l' }, proof: { valid: true } }
            : true;
        setTimeout(() => this.onmessage?.({ data: JSON.stringify({ id, result }) }), 0);
    }
}

const originalWebSocket = (global as any).WebSocket;
beforeAll(() => { (global as any).WebSocket = FakeWebSocket; });
afterAll(() => { (global as any).WebSocket = originalWebSocket; });
beforeEach(() => {
    rpcLog = [];
    FakeWebSocket.instances = [];
    databases.clear();
});

const calls = (type: string) => rpcLog.filter(c => c.type === type);

describe('two clients share nothing', () => {
    test('two tokens: each client reads its own profiles, in memory and in the persistent tier', async () => {
        const alice = new Ad4mClient(ORIGIN, 'alice');
        const bob = new Ad4mClient(ORIGIN, 'bob');
        // Alice's earlier session left a stored profile of the DID both will read.
        databases.set('ad4m-agent-cache/agents', new Map([
            [`${ORIGIN}|did:test:alice|did:test:friend`, { ts: Date.now(), value: { did: 'did:test:friend', directMessageLanguage: 'lang://alice-stored' } }],
        ]));
        // The persistent tier is keyed by the DID the client already knows.
        await alice.agent.status();
        await bob.agent.status();

        // Bob sees nothing of Alice's stored value: he reads from the executor.
        expect((await bob.agent.byDID('did:test:friend')).directMessageLanguage).toBe('lang://net');
        expect(calls('agent.byDIDs').map(c => c.token)).toEqual(['bob']);

        // Alice's first read is her stored value, shown stale; one read of the executor replaces it.
        const first = await alice.agent.byDID('did:test:friend');
        expect(first.directMessageLanguage).toBe('lang://alice-stored');
        await until(() => alice.agent.live.byDID('did:test:friend').getSnapshot().state === 'fresh');
        expect(alice.agent.live.byDID('did:test:friend').getSnapshot().value?.directMessageLanguage).toBe('lang://net');
        expect(calls('agent.byDIDs').map(c => c.token)).toEqual(['bob', 'alice']);

        // What each client wrote is under its own origin + DID.
        await until(() => agentStore().has(`${ORIGIN}|did:test:bob|did:test:friend`));
        expect([...agentStore().keys()].sort()).toEqual([
            `${ORIGIN}|did:test:alice|did:test:friend`,
            `${ORIGIN}|did:test:bob|did:test:friend`,
        ]);

        // And in memory: a read by one client is no read for the other.
        const reads = () => calls('agent.byDIDs').filter(c => c.params.dids.includes('did:test:other')).map(c => c.token);
        await bob.agent.byDID('did:test:other');
        expect(reads()).toEqual(['bob']);
        await alice.agent.byDID('did:test:other');
        expect(reads()).toEqual(['bob', 'alice']);
        alice.close();
        bob.close();
    });

    test('another executor origin is another key', async () => {
        const here = new Ad4mClient(ORIGIN, 'alice');
        const there = new Ad4mClient('http://10.0.0.5:12000', 'alice');
        await here.agent.status();
        await there.agent.status();
        await here.agent.byDID('did:test:friend');
        await there.agent.byDID('did:test:friend');
        await until(() => agentStore().size === 2);
        expect([...agentStore().keys()].sort()).toEqual([
            'http://10.0.0.5:12000|did:test:alice|did:test:friend',
            `${ORIGIN}|did:test:alice|did:test:friend`,
        ]);
        here.close();
        there.close();
    });
});

describe('the persistent tier never waits on a request', () => {
    test('a cold byDID() sends no agent.status before agent.byDIDs, and stores nothing while the DID is unknown', async () => {
        const client = new Ad4mClient(ORIGIN, 'alice');
        databases.set('ad4m-agent-cache/agents', new Map([
            [`${ORIGIN}|did:test:alice|did:test:friend`, { ts: Date.now(), value: { did: 'did:test:friend', directMessageLanguage: 'lang://stored' } }],
        ]));

        const agent = await client.agent.byDID('did:test:friend');

        expect(rpcLog.map(c => c.type).filter(t => t === 'agent.status' || t === 'agent.byDIDs')).toEqual(['agent.byDIDs']);
        expect(agent.directMessageLanguage).toBe('lang://net');
        expect(agentStore().size).toBe(1);

        // Once the status entry holds the DID, the tier is used.
        await client.agent.status();
        await client.agent.byDID('did:test:other');
        await until(() => agentStore().has(`${ORIGIN}|did:test:alice|did:test:other`));
        client.close();
    });
});

describe('profile memory limits', () => {
    test('byDID() keeps at most MAX_MEM_ENTRIES profiles; the oldest are read again', async () => {
        const original = AgentClient.MAX_MEM_ENTRIES;
        AgentClient.MAX_MEM_ENTRIES = 3;
        const client = new Ad4mClient(ORIGIN, 'alice');
        try {
            for (const n of [1, 2, 3, 4]) await client.agent.byDID(`did:test:p${n}`);
            const reads = () => calls('agent.byDIDs').length;
            expect(reads()).toBe(4);
            await client.agent.byDID('did:test:p4');
            expect(reads()).toBe(4);
            await client.agent.byDID('did:test:p1');
            expect(reads()).toBe(5);
        } finally {
            AgentClient.MAX_MEM_ENTRIES = original;
            client.close();
        }
    });

    test('byDID() keeps at most MAX_MEM_BYTES of profiles', async () => {
        const original = AgentClient.MAX_MEM_BYTES;
        AgentClient.MAX_MEM_BYTES = 1;
        const client = new Ad4mClient(ORIGIN, 'alice');
        try {
            await client.agent.byDID('did:test:big');
            await client.agent.byDID('did:test:big2');
            expect(calls('agent.byDIDs')).toHaveLength(2);
            // The newest stays, the older one went out to make room.
            await client.agent.byDID('did:test:big2');
            expect(calls('agent.byDIDs')).toHaveLength(2);
            await client.agent.byDID('did:test:big');
            expect(calls('agent.byDIDs')).toHaveLength(3);
        } finally {
            AgentClient.MAX_MEM_BYTES = original;
            client.close();
        }
    });
});

describe('expressions are fresh for EXPRESSION_TTL_MS', () => {
    const url = 'lang://x/Qm1';

    test('in memory: after the TTL a read asks again; a lag marker alone does not', async () => {
        const original = ExpressionClient.EXPRESSION_TTL_MS;
        const client = new Ad4mClient(ORIGIN, 'alice');
        try {
            await client.expression.get(url);
            await client.expression.get(url);
            expect(calls('expression.get')).toHaveLength(1);

            FakeWebSocket.instances[0].onmessage?.({ data: JSON.stringify({ type: 'query-subscription-update', lagged: true }) });
            await client.expression.get(url);
            expect(calls('expression.get')).toHaveLength(1);

            ExpressionClient.EXPRESSION_TTL_MS = 0;
            await client.expression.get(url);
            expect(calls('expression.get')).toHaveLength(2);
        } finally {
            ExpressionClient.EXPRESSION_TTL_MS = original;
            client.close();
        }
    });

    test('in the persistent tier: a stored expression older than the TTL is not used', async () => {
        const client = new Ad4mClient(ORIGIN, 'alice');
        await client.agent.status();
        const stored = { author: 'did:x', timestamp: 't', data: 'stored', language: { address: 'l' }, proof: { valid: true } };
        const store = new Map<string, unknown>([
            [`${ORIGIN}|did:test:alice|${url}`, { ts: Date.now() - ExpressionClient.EXPRESSION_TTL_MS - 1000, value: stored }],
        ]);
        databases.set('ad4m-expression-cache/expressions', store);
        try {
            expect((await client.expression.get(url)).data).toBe(url);
            expect(calls('expression.get')).toHaveLength(1);

            // Within the TTL a stored expression is used, with no request.
            const other = new Ad4mClient(ORIGIN, 'alice');
            await other.agent.status();
            store.set(`${ORIGIN}|did:test:alice|${url}`, { ts: Date.now(), value: stored });
            expect((await other.expression.get(url)).data).toBe('stored');
            expect(calls('expression.get')).toHaveLength(1);
            other.close();
        } finally {
            client.close();
        }
    });
});

describe('Mirror', () => {
    test('persistent() keeps nothing while the acting DID is unknown', async () => {
        const mirror = new Mirror(() => ORIGIN, () => {}, () => null);
        const store = mirror.persistent<string>('db', 'store');
        await store.put('k', 'v');
        expect(await store.get('k')).toBeUndefined();
        expect(databases.get('db/store')!.size).toBe(0);
    });

    test('markAllStale() leaves a staleOnReconnect:false cache alone and makes the others stale', async () => {
        const mirror = new Mirror(() => ORIGIN, () => {}, () => null);
        const loads = { kept: 0, stale: 0 };
        const kept = mirror.cache<string>({ method: 'a', staleOnReconnect: false, options: () => ({
            loadMany: async keys => keys.map(k => { loads.kept++; return k; }) }) });
        const stale = mirror.cache<string>({ method: 'b', options: () => ({
            loadMany: async keys => keys.map(k => { loads.stale++; return k; }) }) });
        await kept.get('x');
        await stale.get('x');

        mirror.markAllStale();
        await kept.get('x');
        await stale.get('x');

        expect(loads).toEqual({ kept: 1, stale: 2 });
    });

    test('a cache spec registers its on handlers once, with the cache', async () => {
        const handlers: Record<string, (event: any) => void> = {};
        const mirror = new Mirror(() => ORIGIN, ((name: string, handler: (event: any) => void) => { handlers[name] = handler; }) as any, () => null);
        const spec = {
            method: 'm',
            options: () => ({ loadMany: async (keys: string[]) => keys.map(() => 'loaded') }),
            on: { 'agent-updated': (event: any, cache: KeyedCache<string>) => cache.set(event.agent.did, 'pushed') },
        };
        const cache = mirror.cache<string>(spec as any);
        mirror.cache<string>(spec as any);
        handlers['agent-updated']({ agent: { did: 'k' } });

        expect(await cache.get('k')).toBe('pushed');
        expect(Object.keys(handlers)).toEqual(['agent-updated']);
    });

    test('persistent().clear() removes only the keys of its origin and acting DID', async () => {
        let did: string | null = 'did:a';
        const a = new Mirror(() => ORIGIN, () => {}, () => did);
        const b = new Mirror(() => ORIGIN, () => {}, () => 'did:b');
        const storeA = a.persistent<string>('db', 'store');
        const storeB = b.persistent<string>('db', 'store');
        await storeA.put('k', 'a');
        await storeB.put('k', 'b');

        await storeA.clear();

        expect([...databases.get('db/store')!.keys()]).toEqual([`${ORIGIN}|did:b|k`]);
        did = null;
        await storeA.clear();
        expect(databases.get('db/store')!.size).toBe(1);
    });

    test('a cache is one per RPC kind, and a mirror has its own', () => {
        const options = () => ({ loadMany: async (keys: string[]) => keys });
        const one = new Mirror(() => '', () => {}, () => null);
        const two = new Mirror(() => '', () => {}, () => null);
        expect(one.cache({ method: 'm', options })).toBe(one.cache({ method: 'm', options }));
        expect(one.cache({ method: 'm', options })).not.toBe(two.cache({ method: 'm', options }));
    });
});

describe('KeyedCache views', () => {
    test('a stored value is stale on load: read once, then refetched once per session', async () => {
        const stored = new Map<string, any>([['k', { value: 'old', ts: Date.now() }]]);
        const loadMany = jest.fn(async (keys: string[]) => keys.map(() => 'new'));
        const cache = new KeyedCache<string>({
            loadMany,
            persistentStale: true,
            persistent: {
                get: async key => stored.get(key),
                put: async (key, value) => { stored.set(key, value); },
                delete: async key => { stored.delete(key); },
                clear: async () => stored.clear(),
            },
        });
        const live = cache.live('k');

        expect(await cache.get('k')).toBe('old');
        expect(live.getSnapshot()).toMatchObject({ state: 'loading', value: 'old' });
        await until(() => live.getSnapshot().state === 'fresh');
        expect(live.getSnapshot().value).toBe('new');
        expect(await cache.get('k')).toBe('new');
        expect(loadMany).toHaveBeenCalledTimes(1);
        expect(stored.get('k').value).toBe('new');
    });

    test('a failed read is an error snapshot that keeps nothing; the next read retries', async () => {
        let fail = true;
        const cache = new KeyedCache<string>({
            loadMany: async keys => keys.map(k => (fail ? new Error('boom') : k)),
        });
        const live = cache.live('k');
        await expect(live.read()).rejects.toThrow('boom');
        expect(live.getSnapshot()).toMatchObject({ state: 'error', value: undefined });

        fail = false;
        expect(await live.read()).toBe('k');
        expect(live.getSnapshot()).toMatchObject({ state: 'fresh', value: 'k', error: undefined });
    });

    test('a value of a read in flight at markStale() reaches its readers once and is not kept', async () => {
        let release!: () => void;
        const gate = new Promise<void>(r => { release = r; });
        let n = 0;
        const cache = new KeyedCache<string>({ loadMany: async keys => { await gate; return keys.map(() => `v${++n}`); } });

        const first = cache.get('k');
        cache.markStale();
        release();

        expect(await first).toBe('v1');
        expect(await cache.get('k')).toBe('v2');
    });
});
