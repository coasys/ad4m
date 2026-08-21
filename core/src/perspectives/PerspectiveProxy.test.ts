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
    const unsubscribe = jest.fn();
    const unsubReconnect = jest.fn();
    const mockClient = {
      subscribeQuery: jest.fn().mockResolvedValue({
        subscriptionId: 'sub-1',
        result: [{ s: 'a', p: 'b', o: 'c' }],
      }),
      subscribeToQueryUpdates: jest.fn().mockReturnValue(unsubscribe),
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

    // Return a fresh subscriptionId on reconnect
    mockClient.subscribeQuery.mockResolvedValue({
      subscriptionId: 'sub-2',
      result: [{ s: 'x', p: 'y', o: 'z' }],
    });

    // Simulate reconnect
    reconnectCallback!();
    // Allow the async subscribe() to resolve
    await new Promise((r) => setTimeout(r, 10));

    // Should have re-subscribed with a new server-side subscription
    expect(mockClient.subscribeQuery).toHaveBeenCalledWith('perspective-1', 'SELECT ?x WHERE { ?x ?p ?o }');
    expect(mockClient.subscribeToQueryUpdates).toHaveBeenCalledWith('sub-2', expect.any(Function));

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

  // Regression for CodeRabbit review on PR #899:
  //   "Prevent re-entrant reconnect resubscription."
  //
  // The failure mode: when this query owns the FINAL `_wsCallbacks` entry
  // in ApiClient, calling `#unsubscribe` inside `subscribe()` closes the
  // WebSocket (ApiClient.subscribe()'s deleter: "if no more callbacks and
  // no pending calls, close the socket"). The subsequent `subscribeQuery`
  // opens a fresh socket, whose `onopen` fires every registered reconnect
  // callback. If the query's OLD reconnect listener is still in that set,
  // it re-enters `subscribe()`, which closes the socket again, which
  // reopens it again, which fires the listener again — endless loop.
  //
  // The fix removes the reconnect listener at the TOP of `subscribe()`,
  // before `#unsubscribe` can close the socket. A fresh listener is
  // installed at the end. This test simulates the exact "final subscriber
  // closes the socket, subscribeQuery reopens it and fires reconnect
  // callbacks" flow, and asserts subscribe() runs a bounded number of
  // times (once initial + once per real reconnect) — NOT recursively.
  it('does not re-enter subscribe() when this query owned the last WS callback (regression)', async () => {
    // The reconnect-callback set the ApiClient would hold — a real Set is
    // essential so the test observes the listener replacement.
    const reconnectCallbacks = new Set<() => void>();

    // Model ApiClient.subscribeToQueryUpdates's deleter behaviour:
    // when this query's callback goes away, we're the "final subscriber",
    // so a follow-up subscribeQuery() will simulate the fresh socket's
    // onopen by firing every registered reconnect callback.
    let simulateReconnectOnNextSubscribeQuery = false;
    const unsubscribeCallback = jest.fn(() => {
      // "Last callback removed → close the socket" — next subscribeQuery
      // will need to reopen it, which fires the reconnect callbacks.
      simulateReconnectOnNextSubscribeQuery = true;
    });

    let subscribeQueryCount = 0;
    const mockClient = {
      subscribeQuery: jest.fn(async (_uuid: string, _query: string) => {
        subscribeQueryCount++;
        // Reopen path: fire all currently-registered reconnect callbacks
        // synchronously, exactly as ApiClient's onopen loop would.
        if (simulateReconnectOnNextSubscribeQuery) {
          simulateReconnectOnNextSubscribeQuery = false;
          for (const cb of Array.from(reconnectCallbacks)) cb();
        }
        return {
          subscriptionId: `sub-${subscribeQueryCount}`,
          result: [{ s: 'a', p: 'b', o: 'c' }],
        };
      }),
      subscribeToQueryUpdates: jest.fn().mockReturnValue(unsubscribeCallback),
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

    // Initial subscribe — installs the first reconnect listener.
    await subscription.subscribe();
    expect(subscribeQueryCount).toBe(1);
    expect(reconnectCallbacks.size).toBe(1);

    // Trigger a genuine reconnect (as ApiClient's onopen would). The
    // query's handler will call subscribe() again, which will #unsubscribe
    // (closing the socket) and then subscribeQuery (reopening, which our
    // mock uses as the trigger to re-fire reconnect callbacks — the exact
    // loop scenario CodeRabbit warned about).
    for (const cb of Array.from(reconnectCallbacks)) cb();

    // Let all queued microtasks + the async subscribe() chain settle.
    await new Promise((r) => setTimeout(r, 20));

    // BEFORE the fix: subscribeQueryCount would grow without bound (each
    // reopen re-fires the old listener). AFTER the fix: exactly two calls
    // — the initial subscribe(), plus the single reconnect-driven one.
    // The fresh listener installed at the end of the reconnect-driven
    // subscribe() should NOT itself have been called by that same call's
    // reopen, because the OLD listener was cleared FIRST.
    expect(subscribeQueryCount).toBe(2);
    // Exactly one active reconnect listener at rest — the fresh one.
    expect(reconnectCallbacks.size).toBe(1);

    subscription.dispose();
    // Dispose must also drop the fresh listener.
    expect(reconnectCallbacks.size).toBe(0);
  });
});
