import { ApiClient } from '../apiClient';
import { PerspectiveClient } from './PerspectiveClient';
import { PerspectiveProxy } from './PerspectiveProxy';
import { PerspectiveHandle, PerspectiveState } from './PerspectiveHandle';

/**
 * Handler lifecycle tests for `PerspectiveProxy.on()`. They drive a real
 * ApiClient through an injected fake WebSocket. ApiClient opens its socket on
 * the first handler and closes it when the last one is released, so an
 * unopened or closed socket plus silent handlers show that nothing leaked.
 */

class FakeWebSocket {
  static instances: FakeWebSocket[] = [];
  readyState = 0;
  sent: unknown[] = [];
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onerror: ((e: unknown) => void) | null = null;
  onclose: (() => void) | null = null;
  constructor(public url: string) {
    FakeWebSocket.instances.push(this);
  }
  send(data: string) { this.sent.push(JSON.parse(data)); }
  close() { this.readyState = 3; }
  open() { this.readyState = 1; this.onopen?.(); }
  push(event: Record<string, unknown>) { this.onmessage?.({ data: JSON.stringify(event) }); }
}

// Closed after each test, so a failed assertion cannot leave a ping timer that hangs jest.
const apis: ApiClient[] = [];
afterEach(() => apis.splice(0).forEach(api => api.closeAll()));

function setup() {
  FakeWebSocket.instances = [];
  const api = new ApiClient('http://localhost:12000', undefined, FakeWebSocket as unknown as new (url: string) => WebSocket);
  apis.push(api);
  const client = new PerspectiveClient('http://localhost:12000', undefined, api);
  const ws = () => FakeWebSocket.instances[FakeWebSocket.instances.length - 1];
  const proxy = (uuid = 'uuid-1') => new PerspectiveProxy(new PerspectiveHandle(uuid, 'test', PerspectiveState.Synced), client);
  return { ws, proxy };
}

const CLOSED = 3;

// Events exactly as the executor sends them.
const link = (source: string) => ({
  author: 'did:test', timestamp: '2026-01-01T00:00:00Z',
  data: { source, predicate: 'p', target: 't' },
  proof: { signature: 's', key: 'k', valid: true, invalid: false },
  status: 'SHARED',
});
const handle = (uuid: string) => ({ uuid, name: 'test', neighbourhood: null, sharedUrl: null, state: PerspectiveState.Synced, owners: null });
const linkAdded = (perspectiveUuid: string, source: string) =>
  ({ type: 'link-added', perspectiveUuid, owner: 'did:test', link: link(source) });
const linkRemoved = (perspectiveUuid: string, source: string) =>
  ({ type: 'link-removed', perspectiveUuid, owner: 'did:test', link: link(source) });
const linkUpdated = (perspectiveUuid: string, oldSource: string, newSource: string) =>
  ({ type: 'link-updated', perspectiveUuid, owner: 'did:test', oldLink: link(oldSource), newLink: link(newSource) });
const syncStateChange = (perspectiveUuid: string, state: PerspectiveState) =>
  ({ type: 'sync-state-change', perspectiveUuid, state, perspective: handle(perspectiveUuid) });
const autoProcessorEvent = (perspectiveUuid: string) => ({
  type: 'auto-processor-event', perspectiveUuid, processorId: 'proc', agentDid: null, step: 'claimed',
  itemIds: [], batchKey: null, bases: [], detail: null, llmInput: null, llmOutput: null,
  toolName: null, toolArgsJson: null, toolResult: null,
});
const autoProcessorState = (perspectiveUuid: string) => ({
  type: 'auto-processor-neighbourhood-state', perspectiveUuid, processorId: 'proc',
  claimantDid: 'did:test', batchKey: 'key', phase: 'claimed',
});

describe('PerspectiveProxy.on handler lifecycle', () => {
  it('building 100 proxies without handlers opens no socket', () => {
    const { proxy } = setup();
    for (let i = 0; i < 100; i++) proxy(`uuid-${i}`);
    expect(FakeWebSocket.instances).toHaveLength(0);
  });

  it('handlers of every type added before the socket opens all receive events; dispose closes the socket', () => {
    const { proxy, ws } = setup();
    const p = proxy();
    const added = [jest.fn(), jest.fn()];
    const removed = jest.fn();
    const updated = jest.fn();
    const sync = jest.fn();
    const autoEvent = jest.fn();
    const autoState = jest.fn();
    added.forEach(cb => p.on('link-added', cb));
    p.on('link-removed', removed);
    p.on('link-updated', updated);
    p.on('sync-state-change', sync);
    p.on('auto-processor-event', autoEvent);
    p.on('auto-processor-neighbourhood-state', autoState);
    expect(FakeWebSocket.instances).toHaveLength(1);
    ws().open();

    ws().push(linkAdded('uuid-1', 'a'));
    ws().push(linkRemoved('uuid-1', 'b'));
    ws().push(linkUpdated('uuid-1', 'c', 'd'));
    ws().push(syncStateChange('uuid-1', PerspectiveState.Synced));
    ws().push(autoProcessorEvent('uuid-1'));
    ws().push(autoProcessorState('uuid-1'));
    [...added, removed, updated, sync, autoEvent, autoState].forEach(cb => expect(cb).toHaveBeenCalledTimes(1));

    p.dispose();
    expect(ws().readyState).toBe(CLOSED);
  });

  it("delivers this perspective's event payloads and nothing from other perspectives", () => {
    const { proxy, ws } = setup();
    const p = proxy();
    const added = jest.fn();
    const removed = jest.fn();
    const updated = jest.fn();
    const sync = jest.fn();
    const offAdded = p.on('link-added', ({ link }) => added(link.data.source));
    p.on('link-removed', ({ link }) => removed(link.data.source));
    p.on('link-updated', ({ oldLink, newLink }) => updated(oldLink.data.source, newLink.data.source));
    p.on('sync-state-change', ({ state }) => sync(state));
    ws().open();

    ws().push(linkAdded('uuid-1', 'a'));
    ws().push(linkAdded('other', 'x'));
    ws().push(linkRemoved('uuid-1', 'b'));
    ws().push(linkUpdated('uuid-1', 'c', 'd'));
    ws().push(syncStateChange('other', PerspectiveState.Synced));
    ws().push(syncStateChange('uuid-1', PerspectiveState.LinkLanguageInstalledButNotSynced));

    expect(added.mock.calls).toEqual([['a']]);
    expect(removed.mock.calls).toEqual([['b']]);
    expect(updated.mock.calls).toEqual([['c', 'd']]);
    expect(sync.mock.calls).toEqual([[PerspectiveState.LinkLanguageInstalledButNotSynced]]);

    offAdded();
    ws().push(linkAdded('uuid-1', 'e'));
    expect(added).toHaveBeenCalledTimes(1);
  });

  it('calling a release function twice leaves the other handlers', () => {
    const { proxy, ws } = setup();
    const p = proxy();
    const kept = jest.fn();
    p.on('link-added', kept);
    const off = p.on('link-added', jest.fn());
    ws().open();
    off();
    off();
    ws().push(linkAdded('uuid-1', 'a'));
    expect(kept).toHaveBeenCalledTimes(1);
  });

  it('disposing one proxy leaves another proxy for the same uuid working', () => {
    const { proxy, ws } = setup();
    const a = proxy();
    const b = proxy();
    const cbA = jest.fn();
    const cbB = jest.fn();
    a.on('link-added', cbA);
    a.on('auto-processor-event', cbA);
    a.on('auto-processor-neighbourhood-state', cbA);
    b.on('link-added', cbB);
    b.on('auto-processor-event', cbB);
    ws().open();

    a.dispose();
    ws().push(linkAdded('uuid-1', 'a'));
    ws().push(autoProcessorEvent('uuid-1'));
    ws().push(autoProcessorState('uuid-1'));
    expect(cbA).not.toHaveBeenCalled();
    expect(cbB).toHaveBeenCalledTimes(2);
  });

  it('a disposed proxy used again delivers again, and dispose releases that too', () => {
    const { proxy, ws } = setup();
    const p = proxy();
    p.on('link-added', jest.fn());
    p.dispose();
    const cb = jest.fn();
    p.on('link-added', cb);
    ws().open();
    ws().push(linkAdded('uuid-1', 'a'));
    expect(cb).toHaveBeenCalledTimes(1);

    p.dispose();
    ws().push(linkAdded('uuid-1', 'b'));
    expect(cb).toHaveBeenCalledTimes(1);
    expect(ws().readyState).toBe(CLOSED);
  });

  it('dispose() twice is safe, and a later on() still delivers', () => {
    const { proxy, ws } = setup();
    const p = proxy();
    p.on('link-added', jest.fn());
    p.on('auto-processor-event', jest.fn());
    p.dispose();
    expect(() => p.dispose()).not.toThrow();

    const cb = jest.fn();
    p.on('link-added', cb);
    ws().open();
    ws().push(linkAdded('uuid-1', 'a'));
    expect(cb).toHaveBeenCalledTimes(1);
  });
});
