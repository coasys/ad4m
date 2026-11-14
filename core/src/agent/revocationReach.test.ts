import { Ad4mClient } from '../Ad4mClient';

/** `AgentClient.revocationReach` through a fake WebSocket that records each request. */

const ROWS = [
  { didKey: 'did:key:z6MkGone', perspectiveUuid: 'p1', name: 'Garden', droppedAt: '2026-11-01T00:00:01.000Z', rotatedAt: '2026-11-01T00:00:02.000Z' },
  { didKey: 'did:key:z6MkGone', perspectiveUuid: 'p2', name: 'Holochain space' },
];

const sent: { type: string; params: unknown }[] = [];

class FakeWebSocket {
  readyState = 0;
  onopen: (() => void) | null = null;
  onmessage: ((event: any) => void) | null = null;
  onerror: ((e: any) => void) | null = null;
  onclose: (() => void) | null = null;
  constructor(public url: string) {
    setTimeout(() => { this.readyState = 1; this.onopen?.(); }, 0);
  }
  send(raw: string) {
    const { id, type, params } = JSON.parse(raw);
    if (type === 'ping') return;
    sent.push({ type, params });
    const result = type === 'agent.revocationReach' ? ROWS : null;
    setTimeout(() => this.onmessage?.({ data: JSON.stringify({ id, result }) }), 0);
  }
  close() { this.readyState = 3; }
}

const originalWebSocket = (globalThis as any).WebSocket;
afterEach(() => { (globalThis as any).WebSocket = originalWebSocket; });

describe('AgentClient.revocationReach', () => {
  it('asks for the key and returns its rows, a space without times as it is', async () => {
    (globalThis as any).WebSocket = FakeWebSocket;
    const client = new Ad4mClient('http://localhost:12000', 'token');

    const rows = await client.agent.revocationReach('did:key:z6MkGone');

    expect(sent.filter((m) => m.type === 'agent.revocationReach')).toEqual([
      { type: 'agent.revocationReach', params: { didKey: 'did:key:z6MkGone' } },
    ]);
    expect(rows).toEqual(ROWS);
    expect(rows[1].droppedAt).toBeUndefined();
    client.close();
  });
});
