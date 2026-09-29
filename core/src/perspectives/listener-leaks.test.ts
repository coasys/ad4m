import { ApiClient } from '../apiClient';
import { PerspectiveClient } from './PerspectiveClient';
import { PerspectiveProxy } from './PerspectiveProxy';
import { PerspectiveState } from './PerspectiveHandle';

/**
 * Resource-count tests for the perspective listener plumbing. They drive a
 * real ApiClient through an injected fake WebSocket and count the socket
 * callbacks it holds (`_wsCallbacks`), which is where leaked listeners live.
 */

class FakeWebSocket {
  static instances: FakeWebSocket[] = [];
  readyState = 0;
  sent: any[] = [];
  onopen: (() => void) | null = null;
  onmessage: ((event: any) => void) | null = null;
  onerror: ((e: any) => void) | null = null;
  onclose: (() => void) | null = null;
  constructor(public url: string) {
    FakeWebSocket.instances.push(this);
  }
  send(data: string) { this.sent.push(JSON.parse(data)); }
  close() { this.readyState = 3; }
  open() { this.readyState = 1; this.onopen?.(); }
  push(event: Record<string, unknown>) { this.onmessage?.({ data: JSON.stringify(event) }); }
}

const flush = () => new Promise<void>(resolve => setTimeout(resolve, 0));

function setup() {
  FakeWebSocket.instances = [];
  const api = new ApiClient('http://localhost:12000', undefined, FakeWebSocket as any);
  const client = new PerspectiveClient('http://localhost:12000', undefined, false, api);
  const callbackCount = () => (api as any)._wsCallbacks.size as number;
  const ws = () => FakeWebSocket.instances[FakeWebSocket.instances.length - 1];
  const proxy = (uuid = 'uuid-1') => new PerspectiveProxy(
    { uuid, name: 'test', owners: [], sharedUrl: null, neighbourhood: null, state: PerspectiveState.Synced } as any,
    client,
  );
  return { api, client, callbackCount, ws, proxy };
}

const link = (source: string) => ({
  author: 'did:test', timestamp: '2026-01-01T00:00:00Z',
  data: { source, predicate: 'p', target: 't' },
  proof: { signature: 's', key: 'k', valid: true },
});

describe('PerspectiveProxy lazy listener registration (L1)', () => {
  it('building 100 proxies without listeners adds no socket callbacks', () => {
    const { callbackCount, proxy, api } = setup();
    const before = callbackCount();
    for (let i = 0; i < 100; i++) proxy(`uuid-${i}`);
    expect(callbackCount()).toBe(before);
    api.closeAll();
  });

  it('registers one socket callback per listener type, on first use', async () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    const pending = p.addListener('link-added', jest.fn());
    expect(callbackCount()).toBe(1);
    ws().open();
    await pending;
    await p.addListener('link-added', jest.fn());
    expect(callbackCount()).toBe(1);
    await p.addListener('link-removed', jest.fn());
    await p.addListener('link-updated', jest.fn());
    expect(callbackCount()).toBe(3);
    p.addSyncStateChangeListener(jest.fn());
    expect(callbackCount()).toBe(4);
    api.closeAll();
  });

  it('addListener resolves only after the socket is open', async () => {
    const { proxy, ws, api } = setup();
    const p = proxy();
    let resolved = false;
    const pending = p.addListener('link-added', jest.fn()).then(() => { resolved = true; });
    await flush();
    expect(resolved).toBe(false);
    ws().open();
    await pending;
    expect(resolved).toBe(true);
    api.closeAll();
  });

  it('delivers events to listeners exactly as before', async () => {
    const { proxy, ws, api } = setup();
    const p = proxy();
    const added = jest.fn();
    const removed = jest.fn();
    const updated = jest.fn();
    const sync = jest.fn();
    const pending = Promise.all([
      p.addListener('link-added', added),
      p.addListener('link-removed', removed),
      p.addListener('link-updated', updated),
      p.addSyncStateChangeListener(sync),
    ]);
    ws().open();
    await pending;

    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    ws().push({ type: 'link-added', perspectiveUuid: 'other', link: link('x') });
    ws().push({ type: 'link-removed', perspectiveUuid: 'uuid-1', link: link('b') });
    ws().push({ type: 'link-updated', perspectiveUuid: 'uuid-1', oldLink: link('c'), newLink: link('d') });
    ws().push({ type: 'sync-state-change', uuid: 'uuid-1', state: PerspectiveState.LinkLanguageInstalledButNotSynced });

    expect(added).toHaveBeenCalledTimes(1);
    expect(added.mock.calls[0][0].data.source).toBe('a');
    expect(removed).toHaveBeenCalledTimes(1);
    expect(removed.mock.calls[0][0].data.source).toBe('b');
    expect(updated).toHaveBeenCalledTimes(1);
    expect(updated.mock.calls[0][0].newLink.data.source).toBe('d');
    expect(sync).toHaveBeenCalledWith(PerspectiveState.LinkLanguageInstalledButNotSynced);

    await p.removeListener('link-added', added);
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('e') });
    expect(added).toHaveBeenCalledTimes(1);
    api.closeAll();
  });

  it('a disposed proxy registers nothing and receives no events', async () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    p.dispose();
    const cb = jest.fn();
    await p.addListener('link-added', cb);
    expect(callbackCount()).toBe(0);
    ws()?.push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    expect(cb).not.toHaveBeenCalled();
    api.closeAll();
  });
});

describe('Sync-state listener release (L2)', () => {
  it('dispose() leaves no sync-state callback registered', async () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    const pending = p.addSyncStateChangeListener(jest.fn());
    ws().open();
    await pending;
    expect(callbackCount()).toBe(1);
    p.dispose();
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });

  it('removeAllListeners(uuid) also removes the sync-state callback', async () => {
    const { callbackCount, client, ws, api } = setup();
    const pending = client.addPerspectiveSyncStateChangeListener('uuid-1', []);
    ws().open();
    await pending;
    expect(callbackCount()).toBe(1);
    client.removeAllListeners('uuid-1');
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });

  it('addPerspectiveSyncStateChangeListener resolves once the socket is open, with no fixed delay', async () => {
    const { client, ws, api } = setup();
    const pending = client.addPerspectiveSyncStateChangeListener('uuid-1', []);
    ws().open();
    const winner = await Promise.race([
      pending.then(() => 'registered'),
      new Promise(resolve => setTimeout(() => resolve('timeout'), 100)),
    ]);
    expect(winner).toBe('registered');
    api.closeAll();
  });
});

describe('PerspectiveProxy.dispose scope (L3)', () => {
  it('disposing one proxy leaves another proxy for the same uuid working', async () => {
    const { callbackCount, proxy, ws, api } = setup();
    const a = proxy();
    const b = proxy();
    const cbA = jest.fn();
    const cbB = jest.fn();
    const pending = Promise.all([a.addListener('link-added', cbA), b.addListener('link-added', cbB)]);
    ws().open();
    await pending;
    expect(callbackCount()).toBe(2);

    a.dispose();
    expect(callbackCount()).toBe(1);

    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    expect(cbA).not.toHaveBeenCalled();
    expect(cbB).toHaveBeenCalledTimes(1);
    api.closeAll();
  });

  it('dispose releases the auto-processor listeners of this proxy only', async () => {
    const { callbackCount, proxy, ws, api } = setup();
    const a = proxy();
    const b = proxy();
    const cbA = jest.fn();
    const cbB = jest.fn();
    const pending = Promise.all([
      a.addAutoProcessorEventListener(cbA),
      a.addAutoProcessorNeighbourhoodStateListener(jest.fn()),
      b.addAutoProcessorEventListener(cbB),
    ]);
    ws().open();
    await pending;
    expect(callbackCount()).toBe(3);

    a.dispose();
    expect(callbackCount()).toBe(1);
    ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
    expect(cbA).not.toHaveBeenCalled();
    expect(cbB).toHaveBeenCalledTimes(1);
    api.closeAll();
  });

  it('dispose before the socket opens still releases the registration', async () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    const pending = p.addListener('link-added', jest.fn());
    expect(callbackCount()).toBe(1);
    p.dispose();
    ws().open();
    await pending;
    await flush();
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });

  it('removeAllListeners(uuid) still removes every proxy\'s listeners, and a later dispose is a no-op', async () => {
    const { callbackCount, client, proxy, ws, api } = setup();
    const a = proxy();
    const b = proxy();
    const other = proxy('uuid-2');
    const pending = Promise.all([
      a.addListener('link-added', jest.fn()),
      b.addSyncStateChangeListener(jest.fn()),
      other.addListener('link-added', jest.fn()),
    ]);
    ws().open();
    await pending;
    expect(callbackCount()).toBe(3);

    client.removeAllListeners('uuid-1');
    expect(callbackCount()).toBe(1);
    a.dispose();
    b.dispose();
    expect(callbackCount()).toBe(1);
    api.closeAll();
  });
});

describe('Query-subscription unsubscribe map (L5)', () => {
  /** Captures PerspectiveClient's private `#querySubscriptionUnsubscribers` map the first time it stores a key. */
  function captureMap(keyPrefix: string) {
    const originalSet = Map.prototype.set;
    const captured: { map?: Map<unknown, unknown> } = {};
    const spy = jest.spyOn(Map.prototype, 'set').mockImplementation(function (this: Map<unknown, unknown>, key: unknown, value: unknown) {
      if (!captured.map && typeof key === 'string' && key.startsWith(keyPrefix)) captured.map = this;
      return originalSet.call(this, key, value);
    });
    return { captured, restore: () => spy.mockRestore() };
  }

  it('after 50 simulated reconnect swaps the map holds only the live subscriptions', () => {
    const { client, callbackCount, api } = setup();
    const { captured, restore } = captureMap('qsub-');
    try {
      // Two live subscriptions, each swapped 50 times the way QuerySubscriptionProxy does on reconnect:
      // subscribe the new id first, then call the old unsubscribe.
      const unsubs = [client.subscribeToQueryUpdates('qsub-a-0', jest.fn()), client.subscribeToQueryUpdates('qsub-b-0', jest.fn())];
      for (let i = 1; i <= 50; i++) {
        for (const [slot, name] of ['a', 'b'].entries()) {
          const next = client.subscribeToQueryUpdates(`qsub-${name}-${i}`, jest.fn());
          unsubs[slot]();
          unsubs[slot] = next;
        }
      }
      expect(captured.map!.size).toBe(2);
      expect(callbackCount()).toBe(2);
    } finally {
      restore();
      api.closeAll();
    }
  });

  it('disposeQuerySubscription after the returned unsubscribe ran does not unsubscribe twice', async () => {
    const { client, api } = setup();
    const unsubscribe = jest.fn();
    const subscribe = jest.spyOn(api, 'subscribe').mockReturnValue(unsubscribe);
    const call = jest.spyOn(api, 'call').mockResolvedValue(true as any);
    const unsub = client.subscribeToQueryUpdates('qsub-x', jest.fn());
    unsub();
    await client.disposeQuerySubscription('uuid-1', 'qsub-x');
    expect(unsubscribe).toHaveBeenCalledTimes(1);
    expect(call).toHaveBeenCalledWith('perspective.disposeQuery', { uuid: 'uuid-1', subscriptionId: 'qsub-x' });
    subscribe.mockRestore();
    call.mockRestore();
    api.closeAll();
  });

  it('disposeQuerySubscription still releases a live subscription', async () => {
    const { client, callbackCount, api } = setup();
    jest.spyOn(api, 'call').mockResolvedValue(true as any);
    client.subscribeToQueryUpdates('qsub-y', jest.fn());
    expect(callbackCount()).toBe(1);
    await client.disposeQuerySubscription('uuid-1', 'qsub-y');
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });
});

describe('listener registration when the socket fails to connect', () => {
  it('resolves, stays releasable by dispose(), and lets a later listener register', async () => {
    const { api, callbackCount, proxy } = setup();
    // A connect that fails before it opens (the transport rejects readiness with 503).
    (api as any).waitForSubscription = () => Promise.reject(new Error('WebSocket connection closed'));
    const before = callbackCount();

    const p = proxy();
    await expect(p.addListener('link-added', () => null)).resolves.toBeUndefined();
    expect(callbackCount()).toBe(before + 1);
    p.dispose();
    expect(callbackCount()).toBe(before);

    const q = proxy();
    await expect(q.addListener('link-added', () => null)).resolves.toBeUndefined();
    expect(callbackCount()).toBe(before + 1);
    q.dispose();
    api.closeAll();
  });
});
