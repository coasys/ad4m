/**
 * Type test: `link-updated` handlers receive `{ oldLink, newLink }`, not a
 * single link. ts-jest type-checks this file, so a wrong handler type fails
 * the suite at compile time.
 */
import { ApiClient } from '../apiClient';
import { PerspectiveClient } from './PerspectiveClient';
import { PerspectiveHandle } from './PerspectiveHandle';
import { PerspectiveProxy } from './PerspectiveProxy';
import type { DecoratedLinkExpression } from '../generated/api/DecoratedLinkExpression';
import type { PerspectiveLinkUpdatedWithOwner } from '../generated/api/PerspectiveLinkUpdatedWithOwner';

/** Captures the socket ApiClient opens, so the test can push an event. */
class FakeWebSocket {
  static last: FakeWebSocket;
  readyState = 0;
  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string }) => void) | null = null;
  onerror: ((e: unknown) => void) | null = null;
  onclose: (() => void) | null = null;
  constructor(public url: string) { FakeWebSocket.last = this; }
  send() {}
  close() { this.readyState = 3; }
}

function link(target: string): DecoratedLinkExpression {
  return {
    author: 'did:test:1', timestamp: '2024-01-01T00:00:00.000Z',
    data: { source: 's', predicate: 'p', target },
    proof: { key: 'k', signature: 's', valid: true, invalid: false },
    status: 'SHARED',
  };
}

describe('link-updated handler type', () => {
  it('types the handler argument as { oldLink, newLink }', () => {
    const api = new ApiClient('http://localhost:0', undefined, FakeWebSocket as unknown as new (url: string) => WebSocket);
    const proxy = new PerspectiveProxy(new PerspectiveHandle('test-uuid', 'test'), new PerspectiveClient('http://localhost:0', undefined, api));

    const seen: string[] = [];
    // Compiles only if the event carries `oldLink` and `newLink`.
    proxy.on('link-updated', ({ oldLink, newLink }) => {
      seen.push(`${oldLink.data.target}->${newLink.data.target}`);
    });
    // @ts-expect-error a link-updated handler does not receive a single link
    proxy.on('link-updated', (event: { link: DecoratedLinkExpression }) => {});
    // link-added keeps its `{ link }` payload
    proxy.on('link-added', ({ link }: { link: DecoratedLinkExpression }) => {});

    const event: PerspectiveLinkUpdatedWithOwner = { perspectiveUuid: 'test-uuid', owner: 'did:test:1', oldLink: link('old'), newLink: link('new') };
    FakeWebSocket.last.onmessage?.({ data: JSON.stringify({ type: 'link-updated', ...event }) });
    expect(seen).toEqual(['old->new']);
    api.closeAll();
  });
});
