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

describe('PerspectiveProxy listener registration (L1–L3)', () => {
  it('building 100 proxies without listeners adds no socket callbacks', () => {
    const { callbackCount, proxy, api } = setup();
    for (let i = 0; i < 100; i++) proxy(`uuid-${i}`);
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });

  it('registers one socket callback per listener type at once, before the socket opens', () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    expect(p.addListener('link-added', jest.fn())).toBeUndefined();
    p.addListener('link-added', jest.fn());
    expect(callbackCount()).toBe(1);
    p.addListener('link-removed', jest.fn());
    p.addListener('link-updated', jest.fn());
    p.addSyncStateChangeListener(jest.fn());
    expect(callbackCount()).toBe(4);
    expect(ws().readyState).toBe(0);
    p.dispose();
    expect(callbackCount()).toBe(0);
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

  it('disposing one proxy leaves another proxy for the same uuid working', () => {
    const { callbackCount, proxy, ws, api } = setup();
    const a = proxy();
    const b = proxy();
    const cbA = jest.fn();
    const cbB = jest.fn();
    a.addListener('link-added', cbA);
    a.addAutoProcessorEventListener(cbA);
    a.addAutoProcessorNeighbourhoodStateListener(cbA);
    b.addListener('link-added', cbB);
    b.addAutoProcessorEventListener(cbB);
    ws().open();
    expect(callbackCount()).toBe(5);

    a.dispose();
    expect(callbackCount()).toBe(2);
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
    expect(cbA).not.toHaveBeenCalled();
    expect(cbB).toHaveBeenCalledTimes(2);
    api.closeAll();
  });

  it('every auto-processor listener registers its own socket callback; dispose releases them all', () => {
    const { callbackCount, proxy, ws, api } = setup();
    const p = proxy();
    const first = jest.fn();
    const second = jest.fn();
    p.addListener('link-added', jest.fn());
    p.addListener('link-added', jest.fn());
    p.addAutoProcessorEventListener(first);
    p.addAutoProcessorEventListener(second);
    ws().open();
    expect(callbackCount()).toBe(3);
    ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
    expect(first).toHaveBeenCalledTimes(1);
    expect(second).toHaveBeenCalledTimes(1);

    p.dispose();
    expect(callbackCount()).toBe(0);
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
    releases.forEach((release, i) => {
      release();
      expect(callbackCount()).toBe(5 - i);
    });
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
