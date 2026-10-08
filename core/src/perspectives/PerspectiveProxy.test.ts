import { PerspectiveProxy, QuerySubscriptionProxy } from './PerspectiveProxy';
import { Link, LinkExpression } from '../links/Links';
import { LinkQuery } from './LinkQuery';
import { Literal } from '../Literal';
import { SHACLFlow } from '../shacl/SHACLFlow';

function createProxy(client: any): PerspectiveProxy {
  return new PerspectiveProxy(
    { uuid: 'test-uuid', name: 'test', owners: [], sharedUrl: null, neighbourhood: null, state: 'Synced' } as any,
    client,
  );
}

describe('QuerySubscriptionProxy', () => {
  function liveClient(subscribeQuery: jest.Mock) {
    const client = {
      subscribeQuery,
      onQueryUpdate: () => () => {},
      onReconnect: () => () => {},
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    } as any;
    return { client };
  }

  it('resolves initialized with the first result', async () => {
    const { client } = liveClient(jest.fn().mockResolvedValue({ subscriptionId: 'sub-1', result: [{ s: 'a' }], revision: 0 }));
    const subscription = new QuerySubscriptionProxy('perspective-1', 'SELECT * WHERE { ?s ?p ?o }', client);
    await subscription.subscribe();
    await expect(subscription.initialized).resolves.toBe(true);
    expect(subscription.result).toEqual([{ s: 'a' }]);
    expect(subscription.id).toBe('sub-1');
    expect(client.subscribeQuery).toHaveBeenCalledWith('perspective-1', 'SELECT * WHERE { ?s ?p ?o }');
  });

  it('rejects initialized when the subscribe call fails', async () => {
    const { client } = liveClient(jest.fn().mockRejectedValue(new Error('locked')));
    const subscription = new QuerySubscriptionProxy('p', 'SELECT ?x WHERE { ?x ?p ?o }', client);
    const initialized = subscription.initialized;
    await expect(subscription.subscribe()).rejects.toThrow('locked');
    await expect(initialized).rejects.toThrow('locked');
  });
});

describe('PerspectiveProxy.subjectClassTargetClasses', () => {
  function mockLink(source: string) {
    return { data: { source, predicate: 'rdf://type', target: 'ad4m://SubjectClass' } };
  }

  function proxyWithLinks(links: any[]): PerspectiveProxy {
    const mockClient: any = {
      queryLinks: jest.fn().mockResolvedValue(links),
    };
    return createProxy(mockClient);
  }

  it('returns full URIs, not stripped names', async () => {
    const proxy = proxyWithLinks([
      mockLink('we://Space'),
      mockLink('flux://Channel'),
      mockLink('recipe://Recipe'),
    ]);

    const result = await proxy.subjectClassTargetClasses();

    expect(result).toContain('we://Space');
    expect(result).toContain('flux://Channel');
    expect(result).toContain('recipe://Recipe');
    expect(result).toHaveLength(3);
  });

  it('deduplicates URIs', async () => {
    const proxy = proxyWithLinks([
      mockLink('we://Space'),
      mockLink('we://Space'),
    ]);

    expect(await proxy.subjectClassTargetClasses()).toEqual(['we://Space']);
  });

  it('filters out empty sources', async () => {
    const proxy = proxyWithLinks([
      mockLink('we://Space'),
      mockLink(''),
    ]);

    expect(await proxy.subjectClassTargetClasses()).toEqual(['we://Space']);
  });

  it('returns an empty array when no classes are registered', async () => {
    const proxy = proxyWithLinks([]);
    expect(await proxy.subjectClassTargetClasses()).toEqual([]);
  });

  it('rejects when the lookup fails, rather than answering that nothing is registered', async () => {
    const mockClient: any = {
      queryLinks: jest.fn().mockRejectedValue(new Error('network error')),
    };
    const proxy = createProxy(mockClient);

    await expect(proxy.subjectClassTargetClasses()).rejects.toThrow('network error');
    await expect(proxy.subjectClasses()).rejects.toThrow('network error');
  });

  it('still lets subjectClassesByTemplate fall back to property matching when the lookup fails', async () => {
    const mockClient: any = {
      queryLinks: jest.fn().mockRejectedValue(new Error('network error')),
    };
    const proxy = createProxy(mockClient);
    const byProperties = jest.spyOn(proxy as any, 'findClassByProperties').mockResolvedValue('Recipe' as never);

    // A className sends it to the subjectClasses() lookup first, which now rejects.
    await expect(proxy.subjectClassesByTemplate({ className: 'Recipe' })).resolves.toEqual(['Recipe']);
    expect(byProperties).toHaveBeenCalled();
  });
});

describe('PerspectiveProxy.interpretationOverlays coalescing', () => {
  const overlayA = [{ base: 'a', kind: 'create', inferred: [] }];
  const overlayB = [{ base: 'b', kind: 'update', inferred: [] }];

  it('coalesces concurrent reads into one RPC', async () => {
    let resolveFetch: (v: any) => void;
    let fetchCount = 0;
    const mockClient: any = {
      interpretationOverlays: jest.fn(() => {
        fetchCount++;
        return new Promise(r => { resolveFetch = r; });
      }),
    };
    const proxy = createProxy(mockClient);

    const a = proxy.interpretationOverlays();
    const b = proxy.interpretationOverlays();
    resolveFetch!(overlayA);
    expect(await a).toEqual(overlayA);
    expect(await b).toEqual(overlayA);
    expect(fetchCount).toBe(1);
  });

  it('gives each caller its own copy of the array', async () => {
    let resolveFetch: (v: any) => void;
    const mockClient: any = {
      interpretationOverlays: jest.fn(() => new Promise(r => { resolveFetch = r; })),
    };
    const proxy = createProxy(mockClient);

    const a = proxy.interpretationOverlays();
    const b = proxy.interpretationOverlays();
    resolveFetch!(overlayA);
    const first = await a;
    const second = await b;
    expect(second).not.toBe(first);
    first.length = 0;
    expect(second).toEqual(overlayA);
    expect(overlayA).toHaveLength(1);
  });

  it('does not cache: a call after the previous one resolved sends a new RPC', async () => {
    let calls = 0;
    const mockClient: any = {
      interpretationOverlays: jest.fn(async () => { calls++; return calls === 1 ? overlayA : overlayB; }),
    };
    const proxy = createProxy(mockClient);

    expect(await proxy.interpretationOverlays()).toEqual(overlayA);
    expect(await proxy.interpretationOverlays()).toEqual(overlayB);
    expect(calls).toBe(2);
  });

  it('shares a failed RPC with concurrent callers and does not keep it', async () => {
    let calls = 0;
    const mockClient: any = {
      interpretationOverlays: jest.fn(async () => {
        calls++;
        if (calls === 1) throw new Error('boom');
        return overlayA;
      }),
    };
    const proxy = createProxy(mockClient);

    const a = proxy.interpretationOverlays();
    const b = proxy.interpretationOverlays();
    await expect(a).rejects.toThrow('boom');
    await expect(b).rejects.toThrow('boom');
    expect(await proxy.interpretationOverlays()).toEqual(overlayA);
    expect(calls).toBe(2);
  });

  it('a read after acceptInterpretation resolves does not join an older in-flight RPC', async () => {
    const resolvers: Array<(v: any) => void> = [];
    const mockClient: any = {
      interpretationOverlays: jest.fn(() => new Promise(r => { resolvers.push(r); })),
      acceptInterpretation: jest.fn(async () => true),
    };
    const proxy = createProxy(mockClient);
    const before = proxy.interpretationOverlays();
    await proxy.acceptInterpretation('a');
    const after = proxy.interpretationOverlays();
    expect(resolvers.length).toBe(2);
    resolvers[0](overlayA);
    resolvers[1]([]);
    expect(await before).toEqual(overlayA);
    expect(await after).toEqual([]);
  });

  it('a read after rejectInterpretation resolves does not join an older in-flight RPC', async () => {
    const resolvers: Array<(v: any) => void> = [];
    const mockClient: any = {
      interpretationOverlays: jest.fn(() => new Promise(r => { resolvers.push(r); })),
      rejectInterpretation: jest.fn(async () => { throw new Error('reject failed'); }),
    };
    const proxy = createProxy(mockClient);
    const before = proxy.interpretationOverlays();
    // Detaches even when the write throws: it may have landed before the error.
    await expect(proxy.rejectInterpretation('a')).rejects.toThrow('reject failed');
    const after = proxy.interpretationOverlays();
    expect(resolvers.length).toBe(2);
    // The detached RPC settling first must not clear the newer one.
    resolvers[0](overlayA);
    expect(await before).toEqual(overlayA);
    const joiner = proxy.interpretationOverlays();
    expect(resolvers.length).toBe(2);
    resolvers[1]([]);
    expect(await after).toEqual([]);
    expect(await joiner).toEqual([]);
  });
});

// ── fix #1008: PerspectiveProxy.remove accepts bare Link ────────────────────
//
// PerspectiveClient.removeLink does `delete link.data.__typename` which throws
// when a bare Link (no .data) is passed. The fix resolves the Link to its stored
// LinkExpression first, so remove(new Link({...})) must work without error.
describe('PerspectiveProxy.remove with bare Link', () => {
  function makeStoredExpression(source: string, predicate: string, target: string): LinkExpression {
    const expr = new LinkExpression();
    expr.author = 'did:test:agent';
    expr.timestamp = '2026-01-01T00:00:00Z';
    expr.data = new Link({ source, predicate, target });
    expr.proof = { valid: true, invalid: false, signature: 'sig', key: 'key' } as any;
    return expr;
  }

  it('resolves a bare Link to the stored expression and removes it', async () => {
    const storedExpr = makeStoredExpression('s://a', 'p://b', 't://c');
    const removeLink = jest.fn().mockResolvedValue(true);
    const mockClient: any = {
      // queryLinks is what PerspectiveProxy.get calls
      queryLinks: jest.fn().mockResolvedValue([storedExpr]),
      removeLink,
    };
    const proxy = createProxy(mockClient);

    const result = await proxy.remove(new Link({ source: 's://a', predicate: 'p://b', target: 't://c' }));

    expect(result).toBe(true);
    // removeLink must have been called with the resolved expression, not the bare Link
    expect(removeLink).toHaveBeenCalledWith('test-uuid', storedExpr, undefined);
  });

  it('throws a descriptive error when no stored expression matches the bare Link', async () => {
    const mockClient: any = {
      queryLinks: jest.fn().mockResolvedValue([]),
    };
    const proxy = createProxy(mockClient);

    await expect(
      proxy.remove(new Link({ source: 'missing://src', predicate: 'p://pred', target: 'missing://tgt' }))
    ).rejects.toThrow('PerspectiveProxy.remove: no stored LinkExpression matches');
  });

  // Lal's #1011 review: the bare-Link resolution used `bare.predicate ||
  // undefined`, and the Link constructor coerces a missing predicate to "" —
  // so the predicate filter was silently dropped and matches[0] removed an
  // arbitrary source→target link under a different predicate.
  it('matches only predicate-less stored links when the bare Link has no predicate', async () => {
    const withPredicate = makeStoredExpression('s://a', 'p://b', 't://c');
    const withoutPredicate = makeStoredExpression('s://a', '', 't://c');
    // the store reports a missing predicate as null, not ''
    (withoutPredicate.data as any).predicate = null;
    const removeLink = jest.fn().mockResolvedValue(true);
    // the wrong candidate first: matches[0] of the unfiltered result
    const queryLinks = jest.fn().mockResolvedValue([withPredicate, withoutPredicate]);
    const mockClient: any = { queryLinks, removeLink };
    const proxy = createProxy(mockClient);

    await proxy.remove(new Link({ source: 's://a', target: 't://c' }));

    expect(removeLink).toHaveBeenCalledWith('test-uuid', withoutPredicate, undefined);
  });

  it('throws instead of removing a predicated link when the bare Link has no predicate', async () => {
    const withPredicate = makeStoredExpression('s://a', 'p://b', 't://c');
    const queryLinks = jest.fn().mockResolvedValue([withPredicate]);
    const mockClient: any = { queryLinks };
    const proxy = createProxy(mockClient);

    await expect(
      proxy.remove(new Link({ source: 's://a', target: 't://c' }))
    ).rejects.toThrow('no stored LinkExpression matches');
  });

  it('passes a full LinkExpressionInput through unchanged', async () => {
    const storedExpr = makeStoredExpression('s://x', 'p://y', 't://z');
    const removeLink = jest.fn().mockResolvedValue(true);
    const mockClient: any = {
      removeLink,
    };
    const proxy = createProxy(mockClient);

    await proxy.remove(storedExpr as any);
    // Should NOT have called queryLinks — no bare Link resolution needed
    expect(removeLink).toHaveBeenCalledWith('test-uuid', storedExpr, undefined);
  });
});

// #1291 review: addFlow writes only the difference between the stored and the
// wanted definition. These drive the real addFlow against an in-memory store,
// so each assertion is about what lands in the perspective.
describe('PerspectiveProxy.addFlow replace', () => {
  type Stored = { author: string; timestamp: string; data: { source: string; predicate?: string; target: string } };

  function memoryStore() {
    let links: Stored[] = [];
    let clock = 0;
    const stamp = (data: any): Stored => ({ author: 'did:test:agent', timestamp: String(++clock), data: { ...data } });
    const same = (a: Stored, b: Stored) => JSON.stringify(a) === JSON.stringify(b);
    const client: any = {
      queryLinks: jest.fn(async (_uuid: string, q: any) =>
        links.filter(l =>
          (!q.source || l.data.source === q.source) &&
          (!q.predicate || l.data.predicate === q.predicate) &&
          (!q.target || l.data.target === q.target))),
      addLink: jest.fn(async (_uuid: string, link: any) => {
        const expr = stamp(link);
        links.push(expr);
        return expr;
      }),
      linkMutations: jest.fn(async (_uuid: string, m: any) => {
        links = links.filter(l => !m.removals.some((r: Stored) => same(r, l)));
        links.push(...m.additions.map(stamp));
        return {};
      }),
    };
    return { proxy: createProxy(client), client, links: () => links };
  }

  const todoFlow = (n: number, namespace = 'todo://') => {
    const flow = new SHACLFlow('Todo', namespace);
    flow.addState({ name: 'ready', value: 0 });
    flow.addState({ name: 'done', value: 1, consensusRule: { n } } as any);
    flow.addTransition({ actionName: 'Complete', fromState: 'ready', toState: 'done', actions: [] });
    return flow;
  };
  const stateUri = todoFlow(1).stateUri('done');
  const transitionUri = todoFlow(1).transitionUri('ready', 'done', 'Complete');

  async function rules(proxy: PerspectiveProxy): Promise<number[]> {
    const found = await proxy.get(new LinkQuery({ source: stateUri, predicate: 'ad4m://consensusRule' }));
    return found.map(l => JSON.parse(Literal.fromUrl(l.data.target).get() as string).n);
  }

  it('replaces a changed rule and keeps links the definition does not own', async () => {
    const { proxy } = memoryStore();
    await proxy.addFlow('Todo', todoFlow(2));
    const flowUri = todoFlow(1).flowUri;
    await proxy.add(new Link({ source: flowUri, predicate: 'todo://receipt', target: 'todo://r1' }));
    await proxy.add(new Link({ source: stateUri, predicate: 'app://label', target: 'literal://string:Done' }));
    await proxy.add(new Link({ source: transitionUri, predicate: 'app://icon', target: 'app://check' }));

    await proxy.addFlow('Todo', todoFlow(3));

    expect(await rules(proxy)).toEqual([3]);
    expect(await proxy.get(new LinkQuery({ source: flowUri, predicate: 'todo://receipt' }))).toHaveLength(1);
    expect(await proxy.get(new LinkQuery({ source: stateUri, predicate: 'app://label' }))).toHaveLength(1);
    expect(await proxy.get(new LinkQuery({ source: transitionUri, predicate: 'app://icon' }))).toHaveLength(1);
  });

  it('writes nothing when the definition is unchanged', async () => {
    const { proxy, client, links } = memoryStore();
    await proxy.addFlow('Todo', todoFlow(3));
    const before = JSON.stringify(links());
    const calls = client.linkMutations.mock.calls.length;

    await proxy.addFlow('Todo', todoFlow(3));

    // Removing every link and adding it back would keep the count; it would
    // not keep each link's author and timestamp.
    expect(client.linkMutations.mock.calls.length).toBe(calls);
    expect(JSON.stringify(links())).toBe(before);
  });

  it('removes a retired stateCheck from an older definition', async () => {
    const { proxy } = memoryStore();
    await proxy.addFlow('Todo', todoFlow(3));
    await proxy.add(new Link({ source: stateUri, predicate: 'ad4m://stateCheck', target: 'literal://string:old' }));

    await proxy.addFlow('Todo', todoFlow(3));

    expect(await proxy.get(new LinkQuery({ source: stateUri, predicate: 'ad4m://stateCheck' }))).toHaveLength(0);
  });

  it('leaves no definition links on a state the new definition drops', async () => {
    const { proxy, links } = memoryStore();
    await proxy.addFlow('Todo', todoFlow(2));
    const flow = new SHACLFlow('Todo', 'todo://');
    flow.addState({ name: 'ready', value: 0 });

    await proxy.addFlow('Todo', flow);

    expect(links().filter(l => l.data.source === stateUri || l.data.target === stateUri)).toEqual([]);
  });

  it('moves the name to the new flow URI when the namespace changes', async () => {
    const { proxy } = memoryStore();
    await proxy.addFlow('Todo', todoFlow(2, 'old://'));

    await proxy.addFlow('Todo', todoFlow(2, 'new://'));

    const registered = await proxy.get(new LinkQuery({ source: Literal.from('Todo').toUrl(), predicate: 'ad4m://flow_uri' }));
    expect(registered.map(l => l.data.target)).toEqual([todoFlow(2, 'new://').flowUri]);
    expect((await proxy.getFlow('Todo'))?.flowUri).toBe(todoFlow(2, 'new://').flowUri);
  });
});
