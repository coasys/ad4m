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
