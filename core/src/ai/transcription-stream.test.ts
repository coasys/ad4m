import { ApiClient } from '../apiClient';
import { AIClient } from './AIClient';

/**
 * Transcription-stream listener lifecycle, driven through a real ApiClient
 * and a fake WebSocket that plays the executor.
 */

type Reply = { error?: string; result?: unknown };

class FakeWebSocket {
  static last: FakeWebSocket;
  static replies: Record<string, Reply> = {};
  static sent: Array<{ type: string; params: unknown }> = [];
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
    const { id, type, params } = JSON.parse(raw);
    FakeWebSocket.sent.push({ type, params });
    const reply = FakeWebSocket.replies[type];
    if (!reply) return;
    setTimeout(() => this.push(reply.error ? { id, error: { code: 500, message: reply.error } } : { id, result: reply.result }), 0);
  }
  close() { this.readyState = 3; }
  push(event: Record<string, unknown>) { this.onmessage?.({ data: JSON.stringify(event) }); }
}

function setup(replies: Record<string, Reply>) {
  FakeWebSocket.replies = replies;
  FakeWebSocket.sent = [];
  const api = new ApiClient('http://localhost:12000', undefined, FakeWebSocket as any);
  const ai = new AIClient('http://localhost:12000', undefined, api);
  return { api, ai };
}

describe('AIClient transcription streams (L7)', () => {
  it('opens a stream with the model id only: the executor takes no voice-activity parameters', async () => {
    const { api, ai } = setup({ 'ai.transcriptionOpen': { result: 'stream-1' } });
    await ai.openTranscriptionStream('model', () => {});

    const open = FakeWebSocket.sent.find(message => message.type === 'ai.transcriptionOpen');
    expect(open?.params).toEqual({ modelId: 'model' });
    api.closeAll();
  });

  it('a stream whose close call fails leaves no listener', async () => {
    const { api, ai } = setup({
      'ai.transcriptionOpen': { result: 'stream-1' },
      'ai.transcriptionClose': { error: 'close failed' },
    });
    const received: string[] = [];
    await ai.openTranscriptionStream('model', text => received.push(text));

    await expect(ai.closeTranscriptionStream('stream-1')).rejects.toThrow('close failed');
    FakeWebSocket.last?.push({ type: 'transcription-text', streamId: 'stream-1', text: 'after close', userDid: null });
    expect(received).toEqual([]);
    api.closeAll();
  });
});
