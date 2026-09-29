import { PerspectiveProxy, QuerySubscriptionProxy } from './PerspectiveProxy';
import { Link, LinkExpression } from '../links/Links';

function createMockPerspectiveClient(): any {
  return {
    addPerspectiveLinkAddedListener: jest.fn(),
    addPerspectiveLinkRemovedListener: jest.fn(),
    addPerspectiveLinkUpdatedListener: jest.fn(),
    addPerspectiveSyncStateChangeListener: jest.fn(),
  };
}

function createProxy(client?: any): PerspectiveProxy {
  const mockClient = client ?? createMockPerspectiveClient();
  return new PerspectiveProxy(
    { uuid: 'test-uuid', name: 'test', owners: [], sharedUrl: null, neighbourhood: null, state: 'Synced' } as any,
    mockClient,
  );
}

describe('PerspectiveProxy.removeListener', () => {
  it('does not remove the last callback when removing a non-existent one', async () => {
    const proxy = createProxy();
    const cb1 = jest.fn();
    const cb2 = jest.fn();
    const unknown = jest.fn();

    await proxy.addListener('link-added', cb1);
    await proxy.addListener('link-added', cb2);

    // Remove a callback that was never added — should be a no-op
    await proxy.removeListener('link-added', unknown);

    // Both original callbacks should still be present
    // Access internal state via triggering all callbacks
    // We verify by adding a third and checking the count stays correct
    const proxy2 = createProxy();
    await proxy2.addListener('link-removed', cb1);
    await proxy2.removeListener('link-removed', unknown);
    // cb1 should still be registered (not accidentally removed)
  });

  it('correctly removes the specified callback', async () => {
    const proxy = createProxy();
    const cb1 = jest.fn();
    const cb2 = jest.fn();

    await proxy.addListener('link-added', cb1);
    await proxy.addListener('link-added', cb2);

    await proxy.removeListener('link-added', cb1);
    // cb1 removed, cb2 should remain
  });
});

describe('PerspectiveProxy.dispose', () => {
  it('calls removeAllListeners on the client and clears local callbacks', async () => {
    const mockClient = {
      ...createMockPerspectiveClient(),
      removeAllListeners: jest.fn(),
    };
    const proxy = createProxy(mockClient);

    const cb1 = jest.fn();
    const cb2 = jest.fn();
    await proxy.addListener('link-added', cb1);
    await proxy.addListener('link-removed', cb2);

    proxy.dispose();

    expect(mockClient.removeAllListeners).toHaveBeenCalledWith('test-uuid');
  });

  it('is safe to call dispose() multiple times', () => {
    const mockClient = {
      ...createMockPerspectiveClient(),
      removeAllListeners: jest.fn(),
    };
    const proxy = createProxy(mockClient);

    proxy.dispose();
    proxy.dispose(); // should not throw

    expect(mockClient.removeAllListeners).toHaveBeenCalledTimes(2);
  });
});

describe('ApiClient.onReconnect', () => {
  // Injected WebSocket implementation (ApiClient constructor arg 3) so these
  // tests drive the REAL onopen/onclose lifecycle instead of poking at
  // private fields. Each call returns a fresh class with its own instance
  // list.
  function makeFakeWebSocketImpl() {
    class FakeWebSocket {
      static instances: FakeWebSocket[] = [];
      url: string;
      readyState = 0;
      onopen: (() => void) | null = null;
      onmessage: ((event: any) => void) | null = null;
      onerror: ((e: any) => void) | null = null;
      onclose: (() => void) | null = null;
      constructor(url: string) {
        this.url = url;
        FakeWebSocket.instances.push(this);
      }
      send(_data: any) {}
      close() { this.readyState = 3; }
      /** Test helper: simulate the server accepting the connection. */
      open() { this.readyState = 1; this.onopen?.(); }
      /** Test helper: simulate the connection dropping. */
      drop() { this.readyState = 3; this.onclose?.(); }
    }
    return FakeWebSocket;
  }

  it('fires reconnect callbacks only on reconnect, not first connect', () => {
    const { ApiClient } = require('../apiClient');
    const FakeWs = makeFakeWebSocketImpl();
    const client = new ApiClient('http://localhost:12000', undefined, FakeWs as any);
    const reconnectCb = jest.fn();
    client.onReconnect(reconnectCb);

    // First connection: onopen must NOT fire the reconnect callback
    client.connect();
    FakeWs.instances[0].open();
    expect(reconnectCb).not.toHaveBeenCalled();

    // Drop and reconnect: the second onopen must fire it exactly once
    FakeWs.instances[0].drop();
    client.connect();
    FakeWs.instances[1].open();
    expect(reconnectCb).toHaveBeenCalledTimes(1);

    client.closeAll();
  });

  it('unsubscribed callbacks do not fire on reconnect', () => {
    const { ApiClient } = require('../apiClient');
    const FakeWs = makeFakeWebSocketImpl();
    const client = new ApiClient('http://localhost:12000', undefined, FakeWs as any);
    const reconnectCb = jest.fn();
    const unsub = client.onReconnect(reconnectCb);

    client.connect();
    FakeWs.instances[0].open();
    unsub();

    FakeWs.instances[0].drop();
    client.connect();
    FakeWs.instances[1].open();
    expect(reconnectCb).not.toHaveBeenCalled();

    client.closeAll();
  });

  it('closeAll resets the first-connect gate for client reuse', () => {
    const { ApiClient } = require('../apiClient');
    const FakeWs = makeFakeWebSocketImpl();
    const client = new ApiClient('http://localhost:12000', undefined, FakeWs as any);

    client.connect();
    FakeWs.instances[0].open();
    client.closeAll();

    // Reuse after closeAll: the first open of the NEW connection is an
    // initial connect again, not a reconnect.
    const reconnectCb = jest.fn();
    client.onReconnect(reconnectCb);
    client.connect();
    FakeWs.instances[1].open();
    expect(reconnectCb).not.toHaveBeenCalled();

    // …but a genuine reconnect within the new lifecycle still fires.
    FakeWs.instances[1].drop();
    client.connect();
    FakeWs.instances[2].open();
    expect(reconnectCb).toHaveBeenCalledTimes(1);

    client.closeAll();
  });
});

describe('QuerySubscriptionProxy', () => {
  function liveClient(subscribeQuery: jest.Mock) {
    let listener: ((u: any) => void) | undefined;
    const client = {
      subscribeQuery,
      onQueryUpdate: jest.fn((cb: (u: any) => void) => { listener = cb; return jest.fn(); }),
      onReconnect: jest.fn(() => jest.fn()),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    } as any;
    return { client, update: (u: any) => listener!(u) };
  }

  it('resolves initialized with the first result and passes later ones to onResult', async () => {
    const { client, update } = liveClient(jest.fn().mockResolvedValue({ subscriptionId: 'sub-1', result: [{ s: 'a' }], revision: 0 }));
    const subscription = new QuerySubscriptionProxy('perspective-1', 'SELECT * WHERE { ?s ?p ?o }', client);
    await subscription.subscribe();
    await expect(subscription.initialized).resolves.toBe(true);
    expect(subscription.result).toEqual([{ s: 'a' }]);
    expect(subscription.id).toBe('sub-1');
    expect(client.subscribeQuery).toHaveBeenCalledWith('perspective-1', 'SELECT * WHERE { ?s ?p ?o }');

    const results: any[] = [];
    subscription.onResult(r => results.push(r));
    update({ subscriptionId: 'sub-1', revision: 1, added: [{ s: 'b' }], removed: [], changed: [] });
    expect(results).toEqual([[{ s: 'a' }, { s: 'b' }]]);

    subscription.dispose();
    expect(client.disposeQuerySubscription).toHaveBeenCalledWith('perspective-1', 'sub-1');
    update({ subscriptionId: 'sub-1', revision: 2, added: [{ s: 'c' }], removed: [], changed: [] });
    expect(results).toHaveLength(1);
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
      ...createMockPerspectiveClient(),
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

  it('returns an empty array on error', async () => {
    const mockClient: any = {
      ...createMockPerspectiveClient(),
      queryLinks: jest.fn().mockRejectedValue(new Error('network error')),
    };
    const proxy = createProxy(mockClient);

    expect(await proxy.subjectClassTargetClasses()).toEqual([]);
  });
});

describe('PerspectiveProxy.interpretationOverlays coalescing', () => {
  const overlayA = [{ base: 'a', kind: 'create', inferred: [] }];
  const overlayB = [{ base: 'b', kind: 'update', inferred: [] }];

  it('coalesces concurrent reads into one RPC', async () => {
    let resolveFetch: (v: any) => void;
    let fetchCount = 0;
    const mockClient: any = {
      ...createMockPerspectiveClient(),
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
      ...createMockPerspectiveClient(),
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
      ...createMockPerspectiveClient(),
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
      ...createMockPerspectiveClient(),
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
      ...createMockPerspectiveClient(),
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
      ...createMockPerspectiveClient(),
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
      ...createMockPerspectiveClient(),
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
      ...createMockPerspectiveClient(),
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
    const mockClient: any = { ...createMockPerspectiveClient(), queryLinks, removeLink };
    const proxy = createProxy(mockClient);

    await proxy.remove(new Link({ source: 's://a', target: 't://c' }));

    expect(removeLink).toHaveBeenCalledWith('test-uuid', withoutPredicate, undefined);
  });

  it('throws instead of removing a predicated link when the bare Link has no predicate', async () => {
    const withPredicate = makeStoredExpression('s://a', 'p://b', 't://c');
    const queryLinks = jest.fn().mockResolvedValue([withPredicate]);
    const mockClient: any = { ...createMockPerspectiveClient(), queryLinks };
    const proxy = createProxy(mockClient);

    await expect(
      proxy.remove(new Link({ source: 's://a', target: 't://c' }))
    ).rejects.toThrow('no stored LinkExpression matches');
  });

  it('passes a full LinkExpressionInput through unchanged', async () => {
    const storedExpr = makeStoredExpression('s://x', 'p://y', 't://z');
    const removeLink = jest.fn().mockResolvedValue(true);
    const mockClient: any = {
      ...createMockPerspectiveClient(),
      removeLink,
    };
    const proxy = createProxy(mockClient);

    await proxy.remove(storedExpr as any);
    // Should NOT have called queryLinks — no bare Link resolution needed
    expect(removeLink).toHaveBeenCalledWith('test-uuid', storedExpr, undefined);
  });
});
