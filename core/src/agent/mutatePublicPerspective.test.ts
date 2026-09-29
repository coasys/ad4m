import { Ad4mClient } from '../Ad4mClient';
import { Link, LinkExpression } from '../links/Links';
import { ApiClient } from '../apiClient';

/**
 * `AgentClient.mutatePublicPerspective` against an in-memory executor
 * reached through fake WebSockets. The executor state is shared by every
 * socket, so the test counts sockets without changing what the call sees.
 */

const DID = 'did:test:me';

function signed(data: { source: string; predicate?: string; target: string }, n: number) {
  return { author: DID, timestamp: `2026-01-01T00:00:0${n}Z`, data, proof: { key: 'k', signature: `sig-${n}` } };
}

const L1 = signed({ source: DID, predicate: 'name', target: 'literal://old' }, 1);
const L2 = signed({ source: DID, predicate: 'bio', target: 'literal://bio' }, 2);

class Executor {
  perspectives = new Map<string, any[]>();
  profile: any = { links: [L1, L2] };
  calls: string[] = [];
  #created = 0;
  #signed = 3;

  #sameLink = (a: any, b: any) =>
    a.author === b.author && a.timestamp === b.timestamp &&
    a.data.source === b.data.source && (a.data.predicate ?? null) === (b.data.predicate ?? null) &&
    a.data.target === b.data.target;

  handle(type: string, p: any): any {
    this.calls.push(type);
    const links = this.perspectives.get(p.uuid);
    switch (type) {
      case 'agent.get':
        return { did: DID, perspective: this.profile, directMessageLanguage: 'lang://dm' };
      case 'agent.updateProfile':
        this.profile = p.publicPerspective;
        return { did: DID, perspective: this.profile, directMessageLanguage: 'lang://dm' };
      case 'perspective.create': {
        const uuid = `tmp-${++this.#created}`;
        this.perspectives.set(uuid, []);
        return { uuid, name: p.name, owners: [], sharedUrl: null, neighbourhood: null, state: 'PRIVATE' };
      }
      case 'perspective.remove':
        return this.perspectives.delete(p.uuid);
      case 'perspective.addLinkExpression':
        links!.push(p.link);
        return p.link;
      case 'perspective.addLink': {
        const link = signed(p.link, this.#signed++);
        links!.push(link);
        return link;
      }
      case 'perspective.removeLink': {
        const i = links!.findIndex(l => this.#sameLink(l, p.link));
        if (i < 0) throw new Error('Link not found');
        links!.splice(i, 1);
        return true;
      }
      case 'perspective.linkMutations': {
        const removed = links!.filter(l => p.mutations.removals.some((r: any) => this.#sameLink(l, r)));
        this.perspectives.set(p.uuid, links!.filter(l => !removed.includes(l)));
        const added = p.mutations.additions.map((a: any) => signed(a, this.#signed++));
        this.perspectives.get(p.uuid)!.push(...added);
        return { additions: added, removals: removed };
      }
      case 'perspective.snapshot':
        return { links: links!.map(l => JSON.parse(JSON.stringify(l))) };
      default:
        throw new Error(`Unknown RPC type: ${type}`);
    }
  }
}

function makeFakeWebSocket(executor: Executor) {
  return class FakeWebSocket {
    static instances: FakeWebSocket[] = [];
    readyState = 0;
    onopen: (() => void) | null = null;
    onmessage: ((event: any) => void) | null = null;
    onerror: ((e: any) => void) | null = null;
    onclose: (() => void) | null = null;
    constructor(public url: string) {
      FakeWebSocket.instances.push(this);
      setTimeout(() => { this.readyState = 1; this.onopen?.(); }, 0);
    }
    send(raw: string) {
      const { id, type, params } = JSON.parse(raw);
      if (type === 'ping') return;
      let reply: any;
      try { reply = { id, result: executor.handle(type, params) }; }
      catch (e: any) { reply = { id, error: { code: 500, message: e.message } }; }
      setTimeout(() => this.onmessage?.({ data: JSON.stringify(reply) }), 0);
    }
    close() { this.readyState = 3; }
  };
}

const mutations = () => ({
  additions: [new Link({ source: DID, predicate: 'name', target: 'literal://new' })],
  removals: [L1 as any],
});

const originalWebSocket = (globalThis as any).WebSocket;
afterEach(() => { (globalThis as any).WebSocket = originalWebSocket; });

describe('AgentClient.mutatePublicPerspective (L4)', () => {
  it('opens no second socket and removes the temporary perspective', async () => {
    const executor = new Executor();
    const FakeWs = makeFakeWebSocket(executor);
    (globalThis as any).WebSocket = FakeWs;
    const client = new Ad4mClient('http://localhost:12000', 'token', false);

    await client.agent.me();
    expect(FakeWs.instances.length).toBe(1);

    // Net socket callbacks registered on any ApiClient during the call.
    let live = 0;
    const subscribe = ApiClient.prototype.subscribe;
    const spy = jest.spyOn(ApiClient.prototype, 'subscribe').mockImplementation(function (this: ApiClient, cb: any) {
      live++;
      const unsub = subscribe.call(this, cb);
      return () => { live--; unsub(); };
    });
    try {
      await client.agent.mutatePublicPerspective(mutations());
    } finally {
      spy.mockRestore();
    }

    expect(FakeWs.instances.length).toBe(1);
    expect(live).toBe(0);
    expect(executor.perspectives.size).toBe(0);
    client.close();
  });

  it('returns the same Agent as the per-link implementation', async () => {
    const executor = new Executor();
    (globalThis as any).WebSocket = makeFakeWebSocket(executor);
    const client = new Ad4mClient('http://localhost:12000', 'token', false);

    const agent = await client.agent.mutatePublicPerspective(mutations());

    expect(agent.did).toBe(DID);
    expect(agent.directMessageLanguage).toBe('lang://dm');
    expect(agent.perspective!.links).toEqual([
      L2,
      signed({ source: DID, predicate: 'name', target: 'literal://new' }, 3),
    ] as unknown as LinkExpression[]);
    client.close();
  });

  it('applies all mutations with one linkMutations call', async () => {
    const executor = new Executor();
    (globalThis as any).WebSocket = makeFakeWebSocket(executor);
    const client = new Ad4mClient('http://localhost:12000', 'token', false);

    await client.agent.mutatePublicPerspective({
      additions: [
        new Link({ source: DID, predicate: 'a', target: 'literal://a' }),
        new Link({ source: DID, predicate: 'b', target: 'literal://b' }),
      ],
      removals: [L1 as any, L2 as any],
    });

    expect(executor.calls.filter(c => c === 'perspective.linkMutations')).toHaveLength(1);
    expect(executor.calls.filter(c => c === 'perspective.addLink' || c === 'perspective.removeLink')).toHaveLength(0);
    client.close();
  });

  it('works in embedded mode (injected WebSocket, no global one)', async () => {
    const executor = new Executor();
    const FakeWs = makeFakeWebSocket(executor);
    (globalThis as any).WebSocket = class { constructor() { throw new Error('global WebSocket must not be used'); } };
    const client = new Ad4mClient('http://proxy', 'token', false, { webSocketImpl: FakeWs as any });

    const agent = await client.agent.mutatePublicPerspective(mutations());

    expect(agent.perspective!.links).toHaveLength(2);
    expect(FakeWs.instances.length).toBe(1);
    client.close();
  });

  it('removes the temporary perspective when a step fails', async () => {
    const executor = new Executor();
    (globalThis as any).WebSocket = makeFakeWebSocket(executor);
    const client = new Ad4mClient('http://localhost:12000', 'token', false);
    const original = executor.handle.bind(executor);
    executor.handle = (type, p) => {
      if (type === 'agent.updateProfile') throw new Error('update failed');
      return original(type, p);
    };

    await expect(client.agent.mutatePublicPerspective(mutations())).rejects.toThrow('update failed');
    expect(executor.perspectives.size).toBe(0);
    client.close();
  });
});
