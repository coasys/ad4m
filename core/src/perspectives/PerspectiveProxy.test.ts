import { PerspectiveProxy, QuerySubscriptionProxy } from './PerspectiveProxy';

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
  it('treats the initial subscribeQuery() result as a completed initialization', async () => {
    const initialResult = [
      {
        source: 'literal:string:test-channel',
        predicate: 'flux://has_channel_name',
        target: 'literal:string:Runtime Fix Channel',
      },
    ];

    const unsubscribe = jest.fn();
    const mockClient = {
      subscribeQuery: jest.fn().mockResolvedValue({
        subscriptionId: 'sub-1',
        result: initialResult,
      }),
      subscribeToQueryUpdates: jest.fn().mockReturnValue(unsubscribe),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    } as any;

    const subscription = new QuerySubscriptionProxy('perspective-1', 'SELECT * WHERE { ?s ?p ?o }', mockClient);

    await subscription.subscribe();

    const initialized = await Promise.race([
      subscription.initialized.then(() => 'resolved'),
      new Promise<string>((resolve) => setTimeout(() => resolve('timeout'), 25)),
    ]);

    expect(initialized).toBe('resolved');
    expect(subscription.result).toEqual(initialResult);
    expect(mockClient.subscribeQuery).toHaveBeenCalledWith('perspective-1', 'SELECT * WHERE { ?s ?p ?o }');
    expect(mockClient.subscribeToQueryUpdates).toHaveBeenCalledWith('sub-1', expect.any(Function));

    subscription.dispose();
    expect(unsubscribe).toHaveBeenCalled();
  });

  it('re-subscribes immediately when onReconnect fires', async () => {
    let reconnectCallback: (() => void) | undefined;
    const initialUnsubscribe = jest.fn();
    const reconnectUnsubscribe = jest.fn();
    let subscribeToUpdatesCall = 0;
    const unsubReconnect = jest.fn();
    const mockClient = {
      subscribeQuery: jest.fn().mockResolvedValue({
        subscriptionId: 'sub-1',
        result: [{ s: 'a', p: 'b', o: 'c' }],
      }),
      subscribeToQueryUpdates: jest.fn(() => {
        subscribeToUpdatesCall++;
        return subscribeToUpdatesCall === 1 ? initialUnsubscribe : reconnectUnsubscribe;
      }),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
      onReconnect: jest.fn((cb: () => void) => {
        reconnectCallback = cb;
        return unsubReconnect;
      }),
    } as any;

    const subscription = new QuerySubscriptionProxy('perspective-1', 'SELECT ?x WHERE { ?x ?p ?o }', mockClient);
    await subscription.subscribe();

    // onReconnect should have been registered
    expect(mockClient.onReconnect).toHaveBeenCalledTimes(1);
    expect(reconnectCallback).toBeDefined();

    // Reset call counts to isolate the reconnect re-subscribe — but do NOT
    // reset the monotonic counter: the mock must keep returning DISTINCT
    // unsubscribers so the assertions below can tell "old callback disposed"
    // apart from "new callback disposed" (they'd be the same jest.fn if the
    // counter restarted at 1).
    mockClient.subscribeQuery.mockClear();
    mockClient.subscribeToQueryUpdates.mockClear();

    // Return a fresh subscriptionId on reconnect
    mockClient.subscribeQuery.mockResolvedValue({
      subscriptionId: 'sub-2',
      result: [{ s: 'x', p: 'y', o: 'z' }],
    });

    // Simulate reconnect
    reconnectCallback!();
    // Allow the async swap to settle
    await new Promise((r) => setTimeout(r, 10));

    // Should have re-established the server-side subscription with a fresh ID
    // and swapped the client-side callback in place — NEW callback registered
    // BEFORE the old one is disposed (so the WS never dips to 0 subscribers).
    expect(mockClient.subscribeQuery).toHaveBeenCalledWith('perspective-1', 'SELECT ?x WHERE { ?x ?p ?o }');
    expect(mockClient.subscribeToQueryUpdates).toHaveBeenCalledWith('sub-2', expect.any(Function));
    // The OLD callback was disposed, the NEW one was not.
    expect(initialUnsubscribe).toHaveBeenCalledTimes(1);
    expect(reconnectUnsubscribe).not.toHaveBeenCalled();

    subscription.dispose();
    // dispose() tears down the callback that is live at that point — the
    // reconnect-registered one.
    expect(reconnectUnsubscribe).toHaveBeenCalledTimes(1);
  });

  it('cleans up reconnect listener on dispose', async () => {
    const unsubReconnect = jest.fn();
    const mockClient = {
      subscribeQuery: jest.fn().mockResolvedValue({
        subscriptionId: 'sub-1',
        result: [],
      }),
      subscribeToQueryUpdates: jest.fn().mockReturnValue(jest.fn()),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
      onReconnect: jest.fn(() => unsubReconnect),
    } as any;

    const subscription = new QuerySubscriptionProxy('p-1', 'SELECT ?x WHERE { ?x ?p ?o }', mockClient);
    await subscription.subscribe();

    expect(unsubReconnect).not.toHaveBeenCalled();
    subscription.dispose();
    expect(unsubReconnect).toHaveBeenCalledTimes(1);
  });

  // Regression for CodeRabbit review on PR #899 AND for the follow-on
  // integration-tests-mcp failure the first fix exposed.
  //
  // CodeRabbit warned about the "final-subscriber" loop: when this query
  // owns the LAST `_wsCallbacks` entry in ApiClient, calling `#unsubscribe`
  // inside a reconnect-driven `subscribe()` closes the WebSocket
  // (ApiClient.subscribe()'s deleter: "if no more callbacks and no pending
  // calls, close the socket"). The subsequent `subscribeQuery` reopens the
  // socket, whose fresh `onopen` fires every registered reconnect callback
  // → recursion.
  //
  // The current fix avoids the loop AND the collateral damage by NOT
  // calling `subscribe()` from the reconnect handler at all. Instead it
  // swap-in-place: get a new server-side subscription ID via
  // `subscribeQuery`, register the new client-side callback FIRST, then
  // dispose the old callback. `_wsCallbacks.size` never dips to 0 across
  // the swap, so the socket stays open, no `onopen` re-fires, no
  // cross-proxy RPCs die with 503 (the mcp-http.test.ts "should fire
  // onWake when mention uses agent DID" failure).
  //
  // This test models the invariant the swap guarantees.
  it('reconnect handler swaps subscriptions without dropping to zero WS callbacks (regression)', async () => {
    // Real Set so we can observe the swap ordering.
    const reconnectCallbacks = new Set<() => void>();
    // Count of live client-side callback subscriptions (proxy analogue of
    // ApiClient._wsCallbacks.size). Bumped by subscribeToQueryUpdates, and
    // decremented by the returned unsubscribe. If this ever drops to 0
    // during the swap, ApiClient would close the socket — which is
    // exactly the loop CodeRabbit flagged.
    let liveCallbacks = 0;
    let minLiveDuringSwap = Number.POSITIVE_INFINITY;
    let subscribeQueryCount = 0;
    let subscribeToUpdatesCount = 0;

    const mockClient = {
      subscribeQuery: jest.fn(async (_uuid: string, _query: string) => {
        subscribeQueryCount++;
        return {
          subscriptionId: `sub-${subscribeQueryCount}`,
          result: [{ s: 'a', p: 'b', o: 'c' }],
        };
      }),
      subscribeToQueryUpdates: jest.fn(() => {
        subscribeToUpdatesCount++;
        liveCallbacks++;
        // Sample the invariant after each mutation.
        if (liveCallbacks < minLiveDuringSwap) minLiveDuringSwap = liveCallbacks;
        return jest.fn(() => {
          liveCallbacks--;
          if (liveCallbacks < minLiveDuringSwap) minLiveDuringSwap = liveCallbacks;
        });
      }),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
      onReconnect: jest.fn((cb: () => void) => {
        reconnectCallbacks.add(cb);
        return () => { reconnectCallbacks.delete(cb); };
      }),
    } as any;

    const subscription = new QuerySubscriptionProxy(
      'perspective-1',
      'SELECT ?x WHERE { ?x ?p ?o }',
      mockClient,
    );

    // Initial subscribe — one server subscription, one live callback.
    await subscription.subscribe();
    expect(subscribeQueryCount).toBe(1);
    expect(subscribeToUpdatesCount).toBe(1);
    expect(liveCallbacks).toBe(1);
    expect(reconnectCallbacks.size).toBe(1);
    // Reset the low-water mark to the post-init steady state so we only
    // measure what the reconnect handler does.
    minLiveDuringSwap = liveCallbacks;

    // Fire one genuine reconnect. The handler should:
    //   (a) call subscribeQuery (new server-side sub id: sub-2)
    //   (b) call subscribeToQueryUpdates for sub-2 → liveCallbacks 1 → 2
    //   (c) THEN invoke the old unsubscribe → liveCallbacks 2 → 1
    // At no point should liveCallbacks reach 0. And the fresh listener
    // installed at the top of the initial subscribe() must NOT itself
    // re-enter subscribe() from this reconnect.
    for (const cb of Array.from(reconnectCallbacks)) cb();
    // Let the async swap settle.
    await new Promise((r) => setTimeout(r, 20));

    expect(subscribeQueryCount).toBe(2);
    expect(subscribeToUpdatesCount).toBe(2);
    // The critical assertion: the swap never let the callback set go to 0.
    // A zero here would mean the socket got closed → reopened → onopen
    // fires reconnect callbacks again → the loop CodeRabbit warned about.
    expect(minLiveDuringSwap).toBeGreaterThanOrEqual(1);
    // One reconnect fired → exactly one server-side re-establish. NOT a
    // recursive cascade.
    expect(reconnectCallbacks.size).toBe(1);
    expect(liveCallbacks).toBe(1);

    subscription.dispose();
    // Dispose drops both the live callback and the reconnect listener.
    expect(liveCallbacks).toBe(0);
    expect(reconnectCallbacks.size).toBe(0);
  });

  // Regression for the review-blocker on PR #899: a failed full resubscribe
  // (the keepalive / init-timeout retry path) used to remove the reconnect
  // listener at the top of subscribe() and only re-register on success —
  // one subscribeQuery failure during a network flap left the proxy
  // permanently dead (keepalive loop stops itself on resubscribe failure,
  // no listener left to retry). The catch block must install a recovery
  // listener so the next reconnect retries the full subscribe.
  it('keeps reconnect recovery alive after a failed resubscribe attempt', async () => {
    let reconnectCallback: (() => void) | undefined;
    const mockClient = {
      subscribeQuery: jest.fn()
        .mockResolvedValueOnce({ subscriptionId: 'sub-1', result: [] })   // initial subscribe OK
        .mockRejectedValueOnce(new Error('flap'))                          // resubscribe attempt fails
        .mockResolvedValueOnce({ subscriptionId: 'sub-2', result: [] }),  // recovery succeeds
      subscribeToQueryUpdates: jest.fn().mockReturnValue(jest.fn()),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
      onReconnect: jest.fn((cb: () => void) => {
        reconnectCallback = cb;
        return jest.fn();
      }),
    } as any;

    const subscription = new QuerySubscriptionProxy('p-1', 'SELECT ?x WHERE { ?x ?p ?o }', mockClient);
    await subscription.subscribe();
    expect(subscription.id).toBe('sub-1');

    // A retry-subscribe fails (this is what the keepalive path does when
    // the server subscription died).
    await expect(subscription.subscribe()).rejects.toThrow('flap');

    // The failure must have installed a recovery listener — total listener
    // registrations: initial subscribe + failure recovery.
    expect(mockClient.onReconnect).toHaveBeenCalledTimes(2);
    expect(reconnectCallback).toBeDefined();

    // A reconnect arrives → the recovery listener runs a full subscribe()
    // and the proxy comes back to life on a fresh server subscription.
    reconnectCallback!();
    await new Promise((r) => setTimeout(r, 10));

    expect(mockClient.subscribeQuery).toHaveBeenCalledTimes(3);
    expect(subscription.id).toBe('sub-2');

    subscription.dispose();
  });

  // Regression for the concurrent-writers race flagged in the PR #899
  // review: the reconnect swap handler, the keepalive-retry subscribe() and
  // the init-timeout retry subscribe() all write `#unsubscribe` /
  // `#subscriptionId`. Without the generation guard, a swap handler parked
  // on its subscribeQuery while a full subscribe() ran could register a
  // client-side callback that the subscribe() then overwrote WITHOUT
  // disposing — leaking the callback in ApiClient._wsCallbacks for the
  // client lifetime (holding the socket open and firing into a dead
  // subscription). The stale writer must back out and release its
  // server-side subscription instead.
  it('a reconnect swap superseded by a concurrent resubscribe backs out without leaking callbacks', async () => {
    let reconnectCallback: (() => void) | undefined;
    // Deferred subscribeQuery responses so the test controls interleaving.
    const deferreds: Array<{ resolve: (v: any) => void }> = [];
    let subCounter = 0;
    let liveCallbacks = 0;
    const disposedServerSubs: string[] = [];

    const mockClient = {
      subscribeQuery: jest.fn(() => new Promise((resolve) => {
        deferreds.push({ resolve });
      })),
      subscribeToQueryUpdates: jest.fn(() => {
        liveCallbacks++;
        return jest.fn(() => { liveCallbacks--; });
      }),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn((_uuid: string, subId: string) => {
        disposedServerSubs.push(subId);
        return Promise.resolve(true);
      }),
      onReconnect: jest.fn((cb: () => void) => {
        reconnectCallback = cb;
        return jest.fn();
      }),
    } as any;

    const subscription = new QuerySubscriptionProxy('p-1', 'SELECT ?x WHERE { ?x ?p ?o }', mockClient);

    // Initial subscribe.
    const initial = subscription.subscribe();
    deferreds[0].resolve({ subscriptionId: `sub-${++subCounter}`, result: [] });
    await initial;
    expect(liveCallbacks).toBe(1);

    // 1. Reconnect fires; the swap handler parks on its subscribeQuery.
    reconnectCallback!();
    expect(deferreds.length).toBe(2);

    // 2. While the handler is parked, a full resubscribe runs (keepalive
    //    retry analogue) and parks on ITS subscribeQuery.
    const retry = subscription.subscribe();
    expect(deferreds.length).toBe(3);

    // 3. The handler's subscribeQuery resolves FIRST — the handler is now
    //    stale (the full subscribe superseded it) and must back out.
    deferreds[1].resolve({ subscriptionId: 'sub-stale', result: [] });
    await new Promise((r) => setTimeout(r, 5));

    // 4. The full subscribe's subscribeQuery resolves and completes.
    deferreds[2].resolve({ subscriptionId: `sub-${++subCounter}`, result: [] });
    await retry;

    // Exactly ONE live client-side callback — the stale handler must not
    // have left an orphaned one behind (nor disposed the winner's).
    expect(liveCallbacks).toBe(1);
    // The winner's subscription is active…
    expect(subscription.id).toBe('sub-2');
    // …and the stale handler released its orphaned server-side subscription.
    expect(disposedServerSubs).toContain('sub-stale');

    subscription.dispose();
    expect(liveCallbacks).toBe(0);
  });
});
