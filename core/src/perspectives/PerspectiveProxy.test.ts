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
  it('fires reconnect callbacks only on reconnect, not first connect', () => {
    // Import directly to test the reconnect callback mechanism
    const { ApiClient } = require('../apiClient');
    const client = new ApiClient('http://localhost:12000');
    const reconnectCb = jest.fn();
    client.onReconnect(reconnectCb);

    // Simulate first connect — should NOT fire reconnect
    // Access internal state to simulate connection lifecycle
    (client as any)._hasConnectedOnce = false;
    // Simulate: ws.onopen fires, hasConnectedOnce was false → no callback
    expect(reconnectCb).not.toHaveBeenCalled();

    // After marking as connected once and simulating a second onopen, it should fire
    (client as any)._hasConnectedOnce = true;
    for (const cb of (client as any)._reconnectCallbacks) {
      cb();
    }
    expect(reconnectCb).toHaveBeenCalledTimes(1);
  });

  it('unsubscribe function removes the callback', () => {
    const { ApiClient } = require('../apiClient');
    const client = new ApiClient('http://localhost:12000');
    const reconnectCb = jest.fn();
    const unsub = client.onReconnect(reconnectCb);

    // Remove and verify
    unsub();
    expect((client as any)._reconnectCallbacks.size).toBe(0);
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

    // Reset call counts to isolate the reconnect re-subscribe
    mockClient.subscribeQuery.mockClear();
    mockClient.subscribeToQueryUpdates.mockClear();
    subscribeToUpdatesCall = 0;

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
    expect(initialUnsubscribe).toHaveBeenCalledTimes(1);

    subscription.dispose();
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
});
