import { ApiClient } from '../apiClient';
import { AIClient } from './AIClient';
import { AiInference_1_0_0 } from '../generated/services/ai.inference';

const method = (name: string) => `${AiInference_1_0_0.hash}.${name}`;

/**
 * Transcription-stream listener lifecycle, driven through a real ApiClient
 * and a fake WebSocket that plays the executor.
 */

type Reply = { error?: string; result?: unknown };

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
    setTimeout(() => this.push(reply.error ? { id, error: { code: 500, message: reply.error } } : { id, result: reply.result }), 0);
  }
  close() { this.readyState = 3; }
  push(event: Record<string, unknown>) { this.onmessage?.({ data: JSON.stringify(event) }); }
}

function setup(replies: Record<string, Reply>) {
  FakeWebSocket.replies = replies;
  const api = new ApiClient('http://localhost:12000', undefined, FakeWebSocket as any);
  const ai = new AIClient('http://localhost:12000', undefined, api);
  return { api, ai };
}

describe('AIClient transcription streams (L7)', () => {
  it('a stream whose close call fails leaves no listener', async () => {
    const { api, ai } = setup({
      [method('transcriptionOpen')]: { result: 'stream-1' },
      [method('transcriptionClose')]: { error: 'close failed' },
    });
    const received: string[] = [];
    await ai.openTranscriptionStream('model', text => received.push(text));

    await expect(ai.closeTranscriptionStream('stream-1')).rejects.toThrow('close failed');
    FakeWebSocket.last?.push({ type: method('transcription-text'), streamId: 'stream-1', text: 'after close' });
    expect(received).toEqual([]);
    api.closeAll();
  });
});
