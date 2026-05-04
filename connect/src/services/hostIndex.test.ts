import { describe, it, expect, vi, beforeEach } from 'vitest';
import { fetchHosts, fetchUserInfo, requestPayment } from './hostIndex';

describe('fetchHosts', () => {
  beforeEach(() => {
    vi.restoreAllMocks();
  });

  it('fetches hosts from default URL', async () => {
    const mockHosts = [
      { id: '1', name: 'Host A', url: 'http://a.com', profilePicUrl: '', location: 'US', rates: [], aiModels: [] },
    ];
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve(mockHosts),
    }));

    const result = await fetchHosts();
    expect(fetch).toHaveBeenCalledWith('https://hosting.ad4m.dev/hosts');
    expect(result).toEqual(mockHosts);
  });

  it('fetches hosts from custom URL', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve([]),
    }));

    await fetchHosts('https://custom.index');
    expect(fetch).toHaveBeenCalledWith('https://custom.index/hosts');
  });

  it('throws on non-ok response', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: false,
      status: 500,
    }));

    await expect(fetchHosts()).rejects.toThrow('Failed to fetch hosts: 500');
  });

  it('resolves relative profilePicUrl against index base URL', async () => {
    const mockHosts = [
      { id: '1', name: 'Host B', url: 'http://b.com', profilePicUrl: '/images/pic.png', location: 'EU', rates: [], aiModels: [] },
    ];
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve(mockHosts),
    }));

    const result = await fetchHosts('https://hosting.ad4m.dev');
    expect(result[0].profilePicUrl).toBe('https://hosting.ad4m.dev/images/pic.png');
  });

  it('does not modify absolute profilePicUrl', async () => {
    const mockHosts = [
      { id: '1', name: 'Host C', url: 'http://c.com', profilePicUrl: 'https://cdn.example.com/pic.png', location: 'US', rates: [], aiModels: [] },
    ];
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue({
      ok: true,
      json: () => Promise.resolve(mockHosts),
    }));

    const result = await fetchHosts();
    expect(result[0].profilePicUrl).toBe('https://cdn.example.com/pic.png');
  });
});

describe('fetchUserInfo', () => {
  it('parses numeric credits', async () => {
    const mockClient = {
      agent: {
        hostingUserInfo: vi.fn().mockResolvedValue({
          email: 'user@test.com',
          remainingCredits: '42.5',
          hotWalletAddress: '0xabc',
          freeAccess: false,
        }),
      },
    } as any;

    const info = await fetchUserInfo(mockClient);
    expect(info).toEqual({
      email: 'user@test.com',
      remainingCredits: 42.5,
      hotWalletAddress: '0xabc',
      freeAccess: false,
    });
  });

  it('treats "unlimited" as Infinity', async () => {
    const mockClient = {
      agent: {
        hostingUserInfo: vi.fn().mockResolvedValue({
          email: 'admin@test.com',
          remainingCredits: 'unlimited',
          hotWalletAddress: null,
          freeAccess: true,
        }),
      },
    } as any;

    const info = await fetchUserInfo(mockClient);
    expect(info.remainingCredits).toBe(Infinity);
    expect(info.freeAccess).toBe(true);
  });

  it('treats non-numeric strings as 0', async () => {
    const mockClient = {
      agent: {
        hostingUserInfo: vi.fn().mockResolvedValue({
          email: 'user@test.com',
          remainingCredits: 'invalid',
          hotWalletAddress: '',
          freeAccess: false,
        }),
      },
    } as any;

    const info = await fetchUserInfo(mockClient);
    expect(info.remainingCredits).toBe(0);
    expect(info.hotWalletAddress).toBeNull();
  });
});

describe('requestPayment', () => {
  it('calls agent.requestPayment with string amount', async () => {
    const mockClient = {
      agent: {
        requestPayment: vi.fn().mockResolvedValue({ success: true, message: 'OK' }),
      },
    } as any;

    const result = await requestPayment(mockClient, 100);
    expect(mockClient.agent.requestPayment).toHaveBeenCalledWith('100');
    expect(result).toEqual({ success: true, message: 'OK' });
  });
});
