import { ApiClient } from '../apiClient';
import { AIClient } from './AIClient';

/**
 * Transcription-stream listener lifecycle, driven through a real ApiClient
 * and a fake WebSocket that plays the executor.
 */

type Reply = { error?: string; result?: unknown; before?: Record<string, unknown>[] };

class FakeWebSocket {
  static last: FakeWebSocket;
  static replies: Record<string, Reply> = {};
  readyState = 0;
  onopen: (() => void) | null = null;
  onmessage: ((event: any) => void) | null = null;
  onerror: ((e: any) => void) | null = null;
  onclose: (() => void) | null = null;
  constructor(public url: string) {
    FakeWebSocket.last = this;
    setTimeout(() => { this.readyState = 1; this.onopen?.(); }, 0);
  }
  send(raw: string) {
    const { id, type } = JSON.parse(raw);
    const reply = FakeWebSocket.replies[type];
    if (!reply) return;
    setTimeout(() => {
      // Events the executor pushes before its RPC reply reaches the client.
      for (const event of reply.before ?? []) this.push(event);
      this.push(reply.error ? { id, error: { code: 500, message: reply.error } } : { id, result: reply.result });
    }, 0);
  }
  close() { this.readyState = 3; }
  push(event: Record<string, unknown>) { this.onmessage?.({ data: JSON.stringify(event) }); }
}

function setup(replies: Record<string, Reply>) {
  FakeWebSocket.replies = replies;
  const api = new ApiClient('http://localhost:12000', undefined, FakeWebSocket as any);
  const ai = new AIClient('http://localhost:12000', undefined, false, api);
  const callbackCount = () => (api as any)._wsCallbacks.size as number;
  return { api, ai, callbackCount };
}

describe('AIClient transcription streams (L7)', () => {
  it('receives text the executor sends immediately after start', async () => {
    const { api, ai } = setup({
      'ai.transcriptionOpen': {
        result: 'stream-1',
        before: [
          { type: 'transcription-text', streamId: 'stream-1', text: 'early' },
          { type: 'transcription-text', streamId: 'stream-other', text: 'not mine' },
        ],
      },
    });
    const received: string[] = [];

    const streamId = await ai.openTranscriptionStream('model', text => received.push(text));
    FakeWebSocket.last.push({ type: 'transcription-text', streamId: 'stream-1', text: 'later' });
    FakeWebSocket.last.push({ type: 'transcription-text', streamId: 'stream-other', text: 'also not mine' });

    expect(streamId).toBe('stream-1');
    expect(received).toEqual(['early', 'later']);
    api.closeAll();
  });

  it('a stream that fails to open leaves no listener', async () => {
    const { api, ai, callbackCount } = setup({ 'ai.transcriptionOpen': { error: 'no model' } });

    await expect(ai.openTranscriptionStream('model', () => {})).rejects.toThrow('no model');
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });

  it('a stream whose close call fails leaves no listener', async () => {
    const { api, ai, callbackCount } = setup({
      'ai.transcriptionOpen': { result: 'stream-1' },
      'ai.transcriptionClose': { error: 'close failed' },
    });
    const received: string[] = [];
    await ai.openTranscriptionStream('model', text => received.push(text));
    expect(callbackCount()).toBe(1);

    await expect(ai.closeTranscriptionStream('stream-1')).rejects.toThrow('close failed');
    expect(callbackCount()).toBe(0);
    FakeWebSocket.last?.push({ type: 'transcription-text', streamId: 'stream-1', text: 'after close' });
    expect(received).toEqual([]);
    api.closeAll();
  });

  it('closing a stream releases its listener', async () => {
    const { api, ai, callbackCount } = setup({
      'ai.transcriptionOpen': { result: 'stream-1' },
      'ai.transcriptionClose': { result: null },
    });
    await ai.openTranscriptionStream('model', () => {});
    await ai.closeTranscriptionStream('stream-1');
    expect(callbackCount()).toBe(0);
    api.closeAll();
  });
});
