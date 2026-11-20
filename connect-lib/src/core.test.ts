import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

const AUTH = 'https://coasys.id';
const EXECUTOR = 'https://executor.example:12000';

// The executor side: every client the library builds is recorded here.
const clients: FakeClient[] = [];
let redeem: (code: string, verifier: string) => Promise<unknown>;
let renew: (proof: unknown) => Promise<unknown>;

class FakeClient {
  token: string | undefined;
  closed = false;
  logouts = 0;
  constructor(public url: string, token?: string) {
    this.token = token;
    clients.push(this);
  }
  agent = {
    redeemCode: (code: string, verifier: string) => redeem(code, verifier),
    renewSession: (proof: unknown) => renew(proof),
    sessionChallenge: async () => ({ nonce: 'n' }),
    logout: async () => {
      this.logouts++;
      return true;
    },
    createUser: async () => ({ success: true }),
    loginUser: async () => 'guest-token',
  };
  setToken(token: string) {
    this.token = token;
  }
  close() {
    this.closed = true;
  }
  onAuthFailed() {
    return () => {};
  }
}

// Built when a client is made, after this file has run: the factory itself runs before it.
vi.mock('@coasys/ad4m', () => ({
  Ad4mClient: function (url: string, token?: string) {
    return new FakeClient(url, token);
  },
}));

import Ad4mConnect from './core';
import { challengeOf } from './pkce';

type Posted = { message: any; targetOrigin: string };

/** The Connect frame: a window object the library posts to, and replies from a handler. */
class FakeFrame {
  posted: Posted[] = [];
  handler: (message: any) => void = () => {};
  window = {
    postMessage: (message: any, targetOrigin: string) => {
      this.posted.push({ message, targetOrigin });
      queueMicrotask(() => this.handler(message));
    },
  } as unknown as Window;

  /** Sends `data` to the page as the frame, with the origin and window given. */
  send(data: unknown, origin = AUTH, source: unknown = this.window) {
    const event = new MessageEvent('message', { data, origin });
    Object.defineProperty(event, 'source', { value: source });
    window.dispatchEvent(event);
  }
}

let frame: FakeFrame;
let opened: HTMLIFrameElement[];

function options(extra = {}) {
  return {
    appId: 'coasys-app',
    endpoints: { authDomain: AUTH },
    capabilities: [{ with: { domain: '*', pointers: ['*'] }, can: ['*'] }],
    appInfo: { name: 'Test app', description: 'd' },
    ...extra,
  } as any;
}

beforeEach(() => {
  vi.useFakeTimers({ toFake: ['setTimeout', 'clearTimeout', 'setInterval', 'clearInterval', 'Date'] });
  localStorage.clear();
  clients.length = 0;
  frame = new FakeFrame();
  opened = [];
  redeem = async () => ({ token: 't1', grantId: 'g1', expiresAt: Date.now() / 1000 + 3600, did: 'did:key:a', scid: 'did:scid:s' });
  renew = async () => ({ token: 't2', expiresAt: Date.now() / 1000 + 3600 });
  const create = document.createElement.bind(document);
  vi.spyOn(document, 'createElement').mockImplementation(((tag: string, o?: any) => {
    const element = create(tag, o);
    if (tag === 'iframe') {
      Object.defineProperty(element, 'contentWindow', { value: frame.window });
      // The frame is a fake: the element must not load the auth domain.
      Object.defineProperty(element, 'src', { set() {}, get: () => '' });
      opened.push(element as HTMLIFrameElement);
      // A real frame loads, then says it is ready.
      setTimeout(() => frame.send({ type: 'ready' }), 0);
    }
    return element;
  }) as any);
});

afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
  document.body.innerHTML = '';
});

/**
 * Runs the fake clock until `promise` settles. The library's WebCrypto calls finish on real
 * time, so the clock moves in small steps between real ticks.
 */
async function pump<T>(promise: Promise<T>): Promise<T> {
  let done = false;
  promise.then(() => (done = true), () => (done = true));
  for (let i = 0; i < 1000 && !done; i++) {
    await vi.advanceTimersByTimeAsync(10);
    await new Promise((resolve) => setImmediate(resolve));
  }
  return promise;
}

/** A frame that answers a `connect` request as the auth domain would. */
function answerConnect(overrides: Record<string, unknown> = {}) {
  frame.handler = (message) => {
    if (message.type === 'connect') {
      frame.send({ type: 'connected', state: message.state, executor: EXECUTOR, code: 'code-1', did: 'did:key:a', ...overrides });
    }
  };
}

describe('Ad4mConnect.connect', () => {
  it('sends the request to the auth origin only, with a challenge for a verifier it keeps', async () => {
    answerConnect();
    let verifier = '';
    redeem = async (_code, v) => {
      verifier = v;
      return { token: 't1', grantId: 'g1', expiresAt: Date.now() / 1000 + 3600, did: 'did:key:a', scid: 'did:scid:s' };
    };
    const connect = new Ad4mConnect(options());
    const promise = connect.connect();
    await pump(promise);

    const request = frame.posted.find((p) => p.message.type === 'connect')!;
    expect(request.targetOrigin).toBe(AUTH);
    expect(request.message.appId ?? request.message.app.appId).toBe('coasys-app');
    expect(request.message.origin).toBe(window.location.origin);
    expect(request.message.codeChallenge).toBe(await challengeOf(verifier));
    // The verifier is in no message to the frame.
    expect(JSON.stringify(frame.posted)).not.toContain(verifier);
    expect(connect.authState).toBe('authenticated');
    expect(connect.token).toBe('t1');
    expect(connect.baseUrl).toBe(EXECUTOR);
    expect(opened[0].style.display).toBe('none');
  });

  it('keeps the token in memory and in no storage', async () => {
    answerConnect();
    const connect = new Ad4mConnect(options());
    const promise = connect.connect();
    await pump(promise);
    expect(connect.token).toBe('t1');
    for (let i = 0; i < localStorage.length; i++) {
      expect(localStorage.getItem(localStorage.key(i)!)).not.toContain('t1');
    }
    expect(JSON.stringify(sessionStorage)).not.toContain('t1');
  });

  it('removes the tokens an old version left, on start', () => {
    localStorage.setItem('0.13.0/ad4m-token', 'old');
    new Ad4mConnect(options());
    expect(localStorage.getItem('0.13.0/ad4m-token')).toBeNull();
  });

  it('ignores an answer from another origin, another window or for another state', async () => {
    frame.handler = (message) => {
      if (message.type !== 'connect') return;
      const good = { type: 'connected', state: message.state, executor: EXECUTOR, code: 'code-1', did: 'd' };
      frame.send({ ...good, code: 'from-evil-origin' }, 'https://evil.example');
      frame.send({ ...good, code: 'from-other-window' }, AUTH, {});
      frame.send({ ...good, code: 'wrong-state', state: 'other' });
      frame.send(good);
    };
    const seen: string[] = [];
    redeem = async (code) => {
      seen.push(code);
      return { token: 't1', grantId: 'g1', expiresAt: Date.now() / 1000 + 3600, did: 'd', scid: 's' };
    };
    const promise = new Ad4mConnect(options()).connect();
    await pump(promise);
    expect(seen).toEqual(['code-1']);
  });

  it('refuses when the frame denies, and shows no session', async () => {
    frame.handler = (message) => {
      if (message.type === 'connect') frame.send({ type: 'denied', state: message.state, reason: 'cancelled' });
    };
    const connect = new Ad4mConnect(options());
    const promise = connect.connect();
    const failed = expect(promise).rejects.toThrow('cancelled');
    await pump(failed);
    expect(connect.authState).toBe('unauthenticated');
    expect(connect.token).toBe('');
    expect(document.querySelector('iframe')).toBeNull();
  });

  it('refuses an executor URL that is not http(s) or ws(s)', async () => {
    answerConnect({ executor: 'javascript:alert(1)' });
    const promise = new Ad4mConnect(options()).connect();
    const failed = expect(promise).rejects.toThrow();
    await pump(failed);
    expect(clients).toHaveLength(0);
  });

  it('does not keep a token when the executor refuses the code', async () => {
    answerConnect();
    redeem = async () => {
      throw new Error('code');
    };
    const connect = new Ad4mConnect(options());
    const promise = connect.connect();
    const failed = expect(promise).rejects.toThrow('code');
    await pump(failed);
    expect(connect.token).toBe('');
    expect(connect.ad4mClient).toBeUndefined();
    expect(clients.every((c) => c.closed)).toBe(true);
  });

  it('opens a popup when asked, and closes it once connected', async () => {
    const popup = { closed: false, close: vi.fn(function (this: any) { this.closed = true; }) };
    Object.assign(popup, frame.window);
    frame.window = popup as unknown as Window;
    (popup as any).postMessage = (message: any, targetOrigin: string) => {
      frame.posted.push({ message, targetOrigin });
      queueMicrotask(() => frame.handler(message));
    };
    const open = vi.spyOn(window, 'open').mockImplementation(() => {
      setTimeout(() => frame.send({ type: 'ready' }), 0);
      return popup as unknown as Window;
    });
    answerConnect();
    const connect = new Ad4mConnect(options({ mode: 'popup' }));
    const promise = connect.connect();
    await pump(promise);
    expect(open).toHaveBeenCalledWith(`${AUTH}/connect`, expect.any(String), expect.any(String));
    expect(popup.close).toHaveBeenCalled();
    expect(opened).toHaveLength(0);
  });
});

describe('Ad4mConnect session', () => {
  async function connected() {
    answerConnect();
    const connect = new Ad4mConnect(options());
    const promise = connect.connect();
    await pump(promise);
    return connect;
  }

  it('renews through the frame before the session ends and keeps the client', async () => {
    const connect = await connected();
    const client = connect.ad4mClient as unknown as FakeClient;
    frame.handler = (message) => {
      if (message.type === 'renew') frame.send({ type: 'renewed', state: message.state, proof: { signed: true } });
    };
    await vi.advanceTimersByTimeAsync((3600 - 300) * 1000 + 100);
    const request = frame.posted.find((p) => p.message.type === 'renew')!;
    expect(request.message).toMatchObject({ executor: EXECUTOR, grantId: 'g1', challenge: { nonce: 'n' } });
    expect(connect.token).toBe('t2');
    expect(connect.ad4mClient).toBe(client);
    expect(client.token).toBe('t2');
  });

  it('signs out: the executor ends the grant, the frame is told, the token goes', async () => {
    const connect = await connected();
    const client = connect.ad4mClient as unknown as FakeClient;
    await connect.disconnect();
    expect(client.logouts).toBe(1);
    expect(frame.posted.some((p) => p.message.type === 'sign-out' && p.targetOrigin === AUTH)).toBe(true);
    expect(connect.token).toBe('');
    expect(connect.authState).toBe('unauthenticated');
  });
});

describe('Ad4mConnect.connectAsGuest', () => {
  it('connects with a stored guest account and keeps the token out of storage', async () => {
    const connect = new Ad4mConnect(options());
    await connect.connectAsGuest('https://host.example');
    expect(connect.token).toBe('guest-token');
    expect(connect.authState).toBe('authenticated');
    const keys = Object.keys(localStorage);
    expect(keys.some((k) => k.endsWith('ad4m-token'))).toBe(false);
    expect(keys.some((k) => k.includes('ad4m-guest-email-https://host.example'))).toBe(true);
  });
});
