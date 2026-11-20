import { describe, expect, it } from 'vitest';
import { isFrameMessage, originOf } from './messages';

const frame = {} as Window;
const event = (init: { origin: string; source: unknown; data: unknown }) => init as unknown as MessageEvent;

describe('messages', () => {
  it('takes a message from the frame window on the auth origin only', () => {
    const data = { type: 'connected', state: 's' };
    expect(isFrameMessage(event({ origin: 'https://coasys.id', source: frame, data }), 'https://coasys.id', frame)).toBe(true);
    expect(isFrameMessage(event({ origin: 'https://evil.example', source: frame, data }), 'https://coasys.id', frame)).toBe(false);
    expect(isFrameMessage(event({ origin: 'https://coasys.id', source: {}, data }), 'https://coasys.id', frame)).toBe(false);
    expect(isFrameMessage(event({ origin: 'https://coasys.id', source: frame, data: 'text' }), 'https://coasys.id', frame)).toBe(false);
    expect(isFrameMessage(event({ origin: 'https://coasys.id', source: frame, data: null }), 'https://coasys.id', frame)).toBe(false);
  });

  it('reduces a URL to its origin', () => {
    expect(originOf('https://staging.coasys.id/connect?x=1')).toBe('https://staging.coasys.id');
    expect(originOf('http://localhost:5173')).toBe('http://localhost:5173');
  });
});
