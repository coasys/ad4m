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

  it('registers one socket callback per listener type, on first use', () => {
    const { callbackCount, proxy, api } = setup();
    const p = proxy();
    p.addListener('link-added', jest.fn());
    expect(callbackCount()).toBe(1);
    p.addListener('link-added', jest.fn());
    expect(callbackCount()).toBe(1);
    p.addListener('link-removed', jest.fn());
    p.addListener('link-updated', jest.fn());
    expect(callbackCount()).toBe(3);
    p.addSyncStateChangeListener(jest.fn());
    expect(callbackCount()).toBe(4);
    api.closeAll();
  });

  it('registers at once, without waiting for the socket to open', () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    expect(p.addListener('link-added', jest.fn())).toBeUndefined();
    expect(p.addSyncStateChangeListener(jest.fn())).toBeUndefined();
    expect(ws().readyState).toBe(0);
    expect(callbackCount()).toBe(2);
    api.closeAll();
  });

  it('delivers events to listeners', () => {
    const { proxy, ws, api } = setup();
    const p = proxy();
    const added = jest.fn();
    const removed = jest.fn();
    const updated = jest.fn();
    const sync = jest.fn();
    p.addListener('link-added', added);
    p.addListener('link-removed', removed);
    p.addListener('link-updated', updated);
    p.addSyncStateChangeListener(sync);
    ws().open();

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

    p.removeListener('link-added', added);
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('e') });
    expect(added).toHaveBeenCalledTimes(1);
    api.closeAll();
  });

  it('a disposed proxy used again registers again, and dispose releases that too', () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    p.addListener('link-added', jest.fn());
    p.dispose();
    const cb = jest.fn();
    p.addListener('link-added', cb);
    expect(callbackCount()).toBe(1);
    ws().open();
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    expect(cb).toHaveBeenCalledTimes(1);
    p.dispose();
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });
});

describe('PerspectiveClient listener release functions', () => {
  it('each add…Listener returns a function that removes exactly its own socket callback', () => {
    const { callbackCount, client, api } = setup();
    const releases = [
      client.addPerspectiveLinkAddedListener('uuid-1', []),
      client.addPerspectiveLinkRemovedListener('uuid-1', []),
      client.addPerspectiveLinkUpdatedListener('uuid-1', []),
      client.addPerspectiveSyncStateChangeListener('uuid-1', []),
      client.addAutoProcessorEventListener('uuid-1', jest.fn()),
      client.addAutoProcessorNeighbourhoodStateListener('uuid-1', jest.fn()),
    ];
    expect(callbackCount()).toBe(6);
    releases.forEach((release, i) => {
      expect(typeof release).toBe('function');
      release();
      expect(callbackCount()).toBe(5 - i);
    });
    api.closeAll();
  });
});

describe('Sync-state listener release (L2)', () => {
  it('dispose() leaves no sync-state callback registered', () => {
    const { callbackCount, proxy, api } = setup();
    const p = proxy();
    p.addSyncStateChangeListener(jest.fn());
    expect(callbackCount()).toBe(1);
    p.dispose();
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });
});

describe('PerspectiveProxy.dispose scope (L3)', () => {
  it('disposing one proxy leaves another proxy for the same uuid working', () => {
    const { callbackCount, proxy, ws, api } = setup();
    const a = proxy();
    const b = proxy();
    const cbA = jest.fn();
    const cbB = jest.fn();
    a.addListener('link-added', cbA);
    b.addListener('link-added', cbB);
    ws().open();
    expect(callbackCount()).toBe(2);

    a.dispose();
    expect(callbackCount()).toBe(1);

    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    expect(cbA).not.toHaveBeenCalled();
    expect(cbB).toHaveBeenCalledTimes(1);
    api.closeAll();
  });

  it('dispose releases the auto-processor listeners of this proxy only', () => {
    const { callbackCount, proxy, ws, api } = setup();
    const a = proxy();
    const b = proxy();
    const cbA = jest.fn();
    const cbB = jest.fn();
    a.addAutoProcessorEventListener(cbA);
    a.addAutoProcessorNeighbourhoodStateListener(jest.fn());
    b.addAutoProcessorEventListener(cbB);
    ws().open();
    expect(callbackCount()).toBe(3);

    a.dispose();
    expect(callbackCount()).toBe(1);
    ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
    expect(cbA).not.toHaveBeenCalled();
    expect(cbB).toHaveBeenCalledTimes(1);
    api.closeAll();
  });

  it('dispose before the socket opens releases the registration', () => {
    const { callbackCount, proxy, api } = setup();
    const p = proxy();
    p.addListener('link-added', jest.fn());
    expect(callbackCount()).toBe(1);
    p.dispose();
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });
});

describe('Query-subscription listeners (L5)', () => {
  it('after 50 simulated reconnect swaps only the live subscriptions hold socket callbacks', () => {
    const { client, callbackCount, api } = setup();
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
    expect(callbackCount()).toBe(2);
    api.closeAll();
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
});
