import { ApiClient } from '../apiClient';
import { PerspectiveClient } from './PerspectiveClient';
import { PerspectiveProxy } from './PerspectiveProxy';
import { PerspectiveState } from './PerspectiveHandle';

/**
 * Listener lifecycle tests for the perspective listener plumbing. They drive a
 * real ApiClient through an injected fake WebSocket. ApiClient opens its socket
 * on the first subscription and closes it when the last one is released, so an
 * unopened or closed socket plus silent callbacks show that nothing leaked.
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
  const client = new PerspectiveClient('http://localhost:12000', undefined, api);
  const ws = () => FakeWebSocket.instances[FakeWebSocket.instances.length - 1];
  const proxy = (uuid = 'uuid-1') => new PerspectiveProxy(
    { uuid, name: 'test', owners: [], sharedUrl: null, neighbourhood: null, state: PerspectiveState.Synced } as any,
    client,
  );
  return { api, client, ws, proxy };
}

const CLOSED = 3;

const link = (source: string) => ({
  author: 'did:test', timestamp: '2026-01-01T00:00:00Z',
  data: { source, predicate: 'p', target: 't' },
  proof: { signature: 's', key: 'k', valid: true },
});

describe('PerspectiveProxy listener registration (L1–L3)', () => {
  it('building 100 proxies without listeners opens no socket', () => {
    const { proxy, api } = setup();
    for (let i = 0; i < 100; i++) proxy(`uuid-${i}`);
    expect(FakeWebSocket.instances).toHaveLength(0);
    api.closeAll();
  });

  it('listeners of every type added before the socket opens all receive events; dispose closes the socket', () => {
    const { proxy, ws, api } = setup();
    const p = proxy();
    const added = [jest.fn(), jest.fn()];
    const removed = jest.fn();
    const updated = jest.fn();
    const sync = jest.fn();
    added.forEach(cb => p.addListener('link-added', cb));
    p.addListener('link-removed', removed);
    p.addListener('link-updated', updated);
    p.addSyncStateChangeListener(sync);
    expect(FakeWebSocket.instances).toHaveLength(1);
    ws().open();

    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    ws().push({ type: 'link-removed', perspectiveUuid: 'uuid-1', link: link('b') });
    ws().push({ type: 'link-updated', perspectiveUuid: 'uuid-1', oldLink: link('c'), newLink: link('d') });
    ws().push({ type: 'sync-state-change', uuid: 'uuid-1', state: PerspectiveState.Synced });
    added.forEach(cb => expect(cb).toHaveBeenCalledTimes(1));
    expect(removed).toHaveBeenCalledTimes(1);
    expect(updated).toHaveBeenCalledTimes(1);
    expect(sync).toHaveBeenCalledTimes(1);

    p.dispose();
    expect(ws().readyState).toBe(CLOSED);
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
    const { proxy, ws, api } = setup();
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

    a.dispose();
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
    expect(cbA).not.toHaveBeenCalled();
    expect(cbB).toHaveBeenCalledTimes(2);
    api.closeAll();
  });

  it('every auto-processor listener receives events; dispose releases them all', () => {
    const { proxy, ws, api } = setup();
    const p = proxy();
    const first = jest.fn();
    const second = jest.fn();
    p.addListener('link-added', jest.fn());
    p.addListener('link-added', jest.fn());
    p.addAutoProcessorEventListener(first);
    p.addAutoProcessorEventListener(second);
    ws().open();
    ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
    expect(first).toHaveBeenCalledTimes(1);
    expect(second).toHaveBeenCalledTimes(1);

    p.dispose();
    ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
    expect(first).toHaveBeenCalledTimes(1);
    expect(second).toHaveBeenCalledTimes(1);
    expect(ws().readyState).toBe(CLOSED);
    api.closeAll();
  });

  it('a disposed proxy used again delivers again, and dispose releases that too', () => {
    const { proxy, ws, api } = setup();
    const p = proxy();
    p.addListener('link-added', jest.fn());
    p.dispose();
    const cb = jest.fn();
    p.addListener('link-added', cb);
    ws().open();
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    expect(cb).toHaveBeenCalledTimes(1);

    p.dispose();
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('b') });
    expect(cb).toHaveBeenCalledTimes(1);
    expect(ws().readyState).toBe(CLOSED);
    api.closeAll();
  });

  it('dispose() twice is safe, and a later addListener still delivers', () => {
    const { proxy, ws, api } = setup();
    const p = proxy();
    p.addListener('link-added', jest.fn());
    p.addAutoProcessorEventListener(jest.fn());
    p.dispose();
    expect(() => p.dispose()).not.toThrow();

    const cb = jest.fn();
    p.addListener('link-added', cb);
    ws().open();
    ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
    expect(cb).toHaveBeenCalledTimes(1);
    api.closeAll();
  });
});

describe('PerspectiveClient listener release functions', () => {
  it('each add…Listener returns a function that removes exactly its own listener', () => {
    const { client, ws, api } = setup();
    const cbs = {
      added: jest.fn(), removed: jest.fn(), updated: jest.fn(),
      sync: jest.fn(), autoEvent: jest.fn(), autoState: jest.fn(),
    };
    const releases = {
      added: client.addPerspectiveLinkAddedListener('uuid-1', [cbs.added]),
      removed: client.addPerspectiveLinkRemovedListener('uuid-1', [cbs.removed]),
      updated: client.addPerspectiveLinkUpdatedListener('uuid-1', [cbs.updated]),
      sync: client.addPerspectiveSyncStateChangeListener('uuid-1', [cbs.sync]),
      autoEvent: client.addAutoProcessorEventListener('uuid-1', cbs.autoEvent),
      autoState: client.addAutoProcessorNeighbourhoodStateListener('uuid-1', cbs.autoState),
    };
    ws().open();
    const pushAll = () => {
      ws().push({ type: 'link-added', perspectiveUuid: 'uuid-1', link: link('a') });
      ws().push({ type: 'link-removed', perspectiveUuid: 'uuid-1', link: link('b') });
      ws().push({ type: 'link-updated', perspectiveUuid: 'uuid-1', oldLink: link('c'), newLink: link('d') });
      ws().push({ type: 'sync-state-change', uuid: 'uuid-1', state: PerspectiveState.Synced });
      ws().push({ type: 'auto-processor-event', perspectiveUuid: 'uuid-1' });
      ws().push({ type: 'auto-processor-neighbourhood-state', perspectiveUuid: 'uuid-1' });
    };

    // Release one listener at a time; after each release only the released ones stay silent.
    const released = new Set<string>();
    for (const [name, release] of Object.entries(releases)) {
      release();
      released.add(name);
      Object.values(cbs).forEach(cb => cb.mockClear());
      pushAll();
      for (const [other, cb] of Object.entries(cbs)) {
        expect(cb).toHaveBeenCalledTimes(released.has(other) ? 0 : 1);
      }
    }
    expect(ws().readyState).toBe(CLOSED);
    api.closeAll();
  });
});
