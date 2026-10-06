import { Ad4mClient } from '../Ad4mClient';
import { Link, LinkExpression } from '../links/Links';

/**
 * `AgentClient.mutatePublicPerspective` against an in-memory executor
 * reached through fake WebSockets.
 */

const DID = 'did:test:me';

function signed(data: { source: string; predicate?: string; target: string }, n: number) {
  return { author: DID, timestamp: `2026-01-01T00:00:0${n}Z`, data, proof: { key: 'k', signature: `sig-${n}` } };
}

const L1 = signed({ source: DID, predicate: 'name', target: 'literal://old' }, 1);
const L2 = signed({ source: DID, predicate: 'bio', target: 'literal://bio' }, 2);

class Executor {
  /** Temporary perspectives that still exist. */
  perspectives = new Set<string>();
  profile: any = { links: [L1, L2] };
  #created = 0;
  #signed = 3;

  handle(type: string, p: any): any {
    switch (type) {
      case 'agent.get':
        return { did: DID, perspective: this.profile, directMessageLanguage: 'lang://dm' };
      case 'agent.updateProfile':
        this.profile = p.publicPerspective;
        return { did: DID, perspective: this.profile, directMessageLanguage: 'lang://dm' };
      case 'perspective.create': {
        const uuid = `tmp-${++this.#created}`;
        this.perspectives.add(uuid);
        return { uuid };
      }
      case 'perspective.remove':
        return this.perspectives.delete(p.uuid);
      case 'perspective.addLinks':
        return p.links.map((l: any) => signed(l, this.#signed++));
      default:
        throw new Error(`Unknown RPC type: ${type}`);
    }
  }
}

function makeFakeWebSocket(executor: Executor) {
  return class FakeWebSocket {
    readyState = 0;
    onopen: (() => void) | null = null;
    onmessage: ((event: any) => void) | null = null;
    onerror: ((e: any) => void) | null = null;
    onclose: (() => void) | null = null;
    constructor(public url: string) {
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
  it('returns the profile without the removals, plus the signed additions', async () => {
    const executor = new Executor();
    (globalThis as any).WebSocket = makeFakeWebSocket(executor);
    const client = new Ad4mClient('http://localhost:12000', 'token');

    const agent = await client.agent.mutatePublicPerspective(mutations());

    expect(executor.perspectives.size).toBe(0);

    expect(agent.did).toBe(DID);
    expect(agent.directMessageLanguage).toBe('lang://dm');
    expect(agent.perspective!.links).toEqual([
      L2,
      signed({ source: DID, predicate: 'name', target: 'literal://new' }, 3),
    ] as unknown as LinkExpression[]);
    client.close();
  });

  it('removes the temporary perspective when signing fails', async () => {
    const executor = new Executor();
    (globalThis as any).WebSocket = makeFakeWebSocket(executor);
    const client = new Ad4mClient('http://localhost:12000', 'token');
    const original = executor.handle.bind(executor);
    executor.handle = (type, p) => {
      if (type === 'perspective.addLinks') throw new Error('sign failed');
      return original(type, p);
    };

    await expect(client.agent.mutatePublicPerspective(mutations())).rejects.toThrow('sign failed');
    expect(executor.perspectives.size).toBe(0);
    client.close();
  });
});
