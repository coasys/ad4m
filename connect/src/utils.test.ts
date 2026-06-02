import { describe, it, expect, beforeEach, vi } from 'vitest';
import { wsUrlToHttpBase, getInitials, getHue, checkConnection, isEmbedded, setLocal, getLocal, removeLocal } from './utils';

describe('wsUrlToHttpBase', () => {
  it('converts ws:// to http://', () => {
    expect(wsUrlToHttpBase('ws://localhost:12000')).toBe('http://localhost:12000');
  });

  it('converts wss:// to https://', () => {
    expect(wsUrlToHttpBase('wss://remote.host:443')).toBe('https://remote.host:443');
  });

  it('passes through http:// URLs unchanged', () => {
    expect(wsUrlToHttpBase('http://localhost:12000')).toBe('http://localhost:12000');
  });

  it('passes through https:// URLs unchanged', () => {
    expect(wsUrlToHttpBase('https://remote.host:443')).toBe('https://remote.host:443');
  });

  it('handles URLs with paths', () => {
    expect(wsUrlToHttpBase('ws://localhost:12000/api/v1')).toBe('http://localhost:12000/api/v1');
  });
});

describe('getInitials', () => {
  it('returns first two letters of single word', () => {
    expect(getInitials('Alice')).toBe('AL');
  });

  it('returns initials of two words', () => {
    expect(getInitials('Alice Bob')).toBe('AB');
  });

  it('returns initials of first two words for 3+ words', () => {
    expect(getInitials('Alice Bob Carol')).toBe('AB');
  });

  it('handles leading/trailing spaces', () => {
    expect(getInitials('  Alice Bob  ')).toBe('AB');
  });

  it('returns empty string for empty input', () => {
    expect(getInitials('')).toBe('');
  });

  it('returns empty string for whitespace-only input', () => {
    expect(getInitials('   ')).toBe('');
  });

  it('uppercases results', () => {
    expect(getInitials('alice bob')).toBe('AB');
  });

  it('handles single character name', () => {
    expect(getInitials('A')).toBe('A');
  });
});

describe('getHue', () => {
  it('returns a number between 0 and 359', () => {
    const hue = getHue('test');
    expect(hue).toBeGreaterThanOrEqual(0);
    expect(hue).toBeLessThan(360);
  });

  it('is deterministic (same input → same output)', () => {
    expect(getHue('hello')).toBe(getHue('hello'));
  });

  it('returns different values for different inputs', () => {
    const h1 = getHue('alice');
    const h2 = getHue('bob');
    expect(h1).not.toBe(h2);
  });

  it('handles empty string', () => {
    expect(getHue('')).toBe(0);
  });
});

describe('isEmbedded', () => {
  it('returns false when window.self === window.top', () => {
    // happy-dom: window.self === window.top by default
    expect(isEmbedded()).toBe(false);
  });
});

describe('localStorage helpers', () => {
  beforeEach(() => {
    localStorage.clear();
  });

  it('setLocal stores value with version prefix', () => {
    setLocal('ad4m-token', 'mytoken');
    // Check that something was stored (exact prefix depends on package.json version)
    const keys = Object.keys(localStorage);
    expect(keys.length).toBe(1);
    expect(keys[0]).toContain('ad4m-token');
    expect(localStorage.getItem(keys[0])).toBe('mytoken');
  });

  it('getLocal retrieves value with version prefix', () => {
    setLocal('ad4m-url', 'http://localhost:12000');
    expect(getLocal('ad4m-url')).toBe('http://localhost:12000');
  });

  it('getLocal returns null for missing key', () => {
    expect(getLocal('nonexistent')).toBeNull();
  });

  it('removeLocal deletes the key', () => {
    setLocal('ad4m-token', 'x');
    removeLocal('ad4m-token');
    expect(getLocal('ad4m-token')).toBeNull();
  });
});

describe('checkConnection', () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  it('resolves when /health returns ok', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve({ status: 'ok' }),
    }));

    await expect(checkConnection('http://localhost:12000')).resolves.toBeUndefined();
    expect(fetch).toHaveBeenCalledWith(
      'http://localhost:12000/health',
      expect.objectContaining({ signal: expect.any(AbortSignal) })
    );
  });

  it('throws on non-ok response', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: false,
      status: 503,
      json: () => Promise.resolve({}),
    }));

    await expect(checkConnection('http://localhost:12000')).rejects.toThrow('503');
  });

  it('throws when status is not ok', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve({ status: 'starting' }),
    }));

    await expect(checkConnection('http://localhost:12000')).rejects.toThrow('Not an AD4M executor');
  });

  it('throws on network error', async () => {
    vi.stubGlobal('fetch', vi.fn().mockRejectedValue(new Error('ECONNREFUSED')));

    await expect(checkConnection('http://localhost:12000')).rejects.toThrow('ECONNREFUSED');
  });

  it('throws on timeout (abort)', async () => {
    vi.stubGlobal('fetch', vi.fn().mockImplementation((_url, opts) => {
      return new Promise((_, reject) => {
        opts.signal.addEventListener('abort', () => {
          const err = new Error('aborted');
          err.name = 'AbortError';
          reject(err);
        });
      });
    }));

    await expect(checkConnection('http://localhost:12000', 10)).rejects.toThrow('timed out');
  });
});
