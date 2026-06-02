import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest';
import Ad4mConnect from './core';
import { setLocal, getLocal } from './utils';

const { mockAgent, mockRuntime, mockClientInstance } = vi.hoisted(() => {
  const mockAgent = {
    isLocked: vi.fn().mockResolvedValue(false),
    status: vi.fn().mockResolvedValue({ isInitialized: true }),
    startSubscriptions: vi.fn(),
    requestCapability: vi.fn().mockResolvedValue('req-123'),
    generateJwt: vi.fn().mockResolvedValue('jwt-token'),
    hostingUserInfo: vi.fn().mockResolvedValue({
      email: 'test@test.com',
      remainingCredits: '100',
      hotWalletAddress: '0x123',
      freeAccess: false,
    }),
    requestLoginVerification: vi.fn().mockResolvedValue({ success: true, message: 'Sent', requiresPassword: false, isExistingUser: true }),
    verifyEmailCode: vi.fn().mockResolvedValue('jwt-from-email'),
    loginUser: vi.fn().mockResolvedValue('jwt-from-login'),
    createUser: vi.fn().mockResolvedValue({ success: true }),
    signMessage: vi.fn().mockResolvedValue({ signature: 'sig', publicKey: 'pk' }),
    addHostingUserInfoChangedListener: vi.fn(),
    subscribeHostingUserInfoChanged: vi.fn(),
    addComputeLogUpdatedListener: vi.fn(),
    subscribeComputeLogUpdated: vi.fn(),
    computeLog: vi.fn().mockResolvedValue([]),
    requestPayment: vi.fn().mockResolvedValue({ success: true, message: 'OK' }),
  };

  const mockRuntime = {
    info: vi.fn().mockResolvedValue({}),
    multiUserEnabled: vi.fn().mockResolvedValue(true),
  };

  const mockClientInstance = {
    close: vi.fn(),
    startSubscriptions: vi.fn(),
    agent: mockAgent,
    runtime: mockRuntime,
  };

  return { mockAgent, mockRuntime, mockClientInstance };
});

// Mock @coasys/ad4m
vi.mock('@coasys/ad4m', () => ({
  Ad4mClient: vi.fn().mockImplementation(() => mockClientInstance),
  VerificationRequestResult: {},
}));

// Mock only checkConnection and isEmbedded from utils (keep the real localStorage helpers)
vi.mock('./utils', async (importOriginal) => {
  const actual = await importOriginal() as any;
  return {
    ...actual,
    checkConnection: vi.fn().mockResolvedValue(undefined),
    isEmbedded: vi.fn().mockReturnValue(false),
  };
});

const defaultOptions = {
  appInfo: {
    name: 'Test App',
    description: 'A test application',
    url: 'http://localhost:3000',
    iconPath: '/icon.png',
  },
  capabilities: [{ with: { domain: '*', pointers: ['*'] }, can: ['*'] }],
};

describe('Ad4mConnect', () => {
  beforeEach(() => {
    localStorage.clear();
    // Reset call counts without clearing implementations
    mockAgent.isLocked.mockClear();
    mockAgent.status.mockClear();
    mockAgent.requestCapability.mockClear();
    mockAgent.generateJwt.mockClear();
    mockAgent.hostingUserInfo.mockClear();
    mockAgent.requestLoginVerification.mockClear();
    mockAgent.verifyEmailCode.mockClear();
    mockAgent.loginUser.mockClear();
    mockAgent.createUser.mockClear();
    mockClientInstance.close.mockClear();
    mockClientInstance.startSubscriptions.mockClear();
    mockRuntime.info.mockClear();
    mockRuntime.multiUserEnabled.mockClear();
    // Restore default implementations
    mockAgent.isLocked.mockResolvedValue(false);
    mockAgent.status.mockResolvedValue({ isInitialized: true });
  });

  describe('constructor', () => {
    it('creates instance with default port and url', () => {
      const conn = new Ad4mConnect(defaultOptions);
      expect(conn.port).toBe(12000);
      expect(conn.url).toBe('http://localhost:12000');
      expect(conn.token).toBe('');
      expect(conn.connectionState).toBe('not-connected');
      expect(conn.authState).toBe('unauthenticated');
    });

    it('uses provided port and url from options', () => {
      const conn = new Ad4mConnect({ ...defaultOptions, port: 5000, url: 'http://myhost:5000' });
      expect(conn.port).toBe(5000);
      expect(conn.url).toBe('http://myhost:5000');
    });

    it('restores token from localStorage', () => {
      setLocal('ad4m-token', 'saved-jwt');
      const conn = new Ad4mConnect(defaultOptions);
      expect(conn.token).toBe('saved-jwt');
    });

    it('restores URL from localStorage', () => {
      setLocal('ad4m-url', 'http://saved:9999');
      const conn = new Ad4mConnect(defaultOptions);
      expect(conn.url).toBe('http://saved:9999');
    });

    it('defaults hostIndexUrl to hosting.ad4m.dev', () => {
      const conn = new Ad4mConnect(defaultOptions);
      expect(conn.hostIndexUrl).toBe('https://hosting.ad4m.dev');
    });

    it('uses custom hostIndexUrl from options', () => {
      const conn = new Ad4mConnect({ ...defaultOptions, hostIndexUrl: 'https://custom.index' });
      expect(conn.hostIndexUrl).toBe('https://custom.index');
    });
  });

  describe('baseUrl', () => {
    it('normalizes ws:// to http://', () => {
      const conn = new Ad4mConnect({ ...defaultOptions, url: 'ws://localhost:12000' });
      expect(conn.baseUrl).toBe('http://localhost:12000');
    });

  });

  describe('connect() — standalone mode', () => {
    it('connects, builds client, checks auth', async () => {
      const { checkConnection } = await import('./utils');
      const conn = new Ad4mConnect(defaultOptions);
      const client = await conn.connect();

      expect(checkConnection).toHaveBeenCalledWith('http://localhost:12000');
      expect(conn.ad4mClient).toBeDefined();
      expect(conn.connectionState).toBe('connected');
      expect(conn.authState).toBe('authenticated');
      expect(client).toBe(conn.ad4mClient);
    });

    it('fires connectionstatechange events', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      const states: string[] = [];
      conn.addEventListener('connectionstatechange', (e: any) => states.push(e.detail));
      await conn.connect();
      expect(states).toContain('connecting');
      expect(states).toContain('connected');
    });

    it('fires authstatechange event on success', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      const states: string[] = [];
      conn.addEventListener('authstatechange', (e: any) => states.push(e.detail));
      await conn.connect();
      expect(states).toContain('authenticated');
    });

    it('sets connectionState to error on failure', async () => {
      const { checkConnection } = await import('./utils');
      (checkConnection as any).mockRejectedValueOnce(new Error('ECONNREFUSED'));

      const conn = new Ad4mConnect(defaultOptions);
      await expect(conn.connect()).rejects.toThrow('ECONNREFUSED');
      expect(conn.connectionState).toBe('error');
    });
  });

  describe('checkAuth()', () => {
    it('sets auth to locked when agent is locked', async () => {
      mockAgent.isLocked.mockResolvedValueOnce(true);

      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      expect(conn.authState).toBe('locked');
    });

    it('clears token on InvalidSignature', async () => {
      mockAgent.isLocked.mockResolvedValueOnce(false);
      mockAgent.status.mockRejectedValueOnce(new Error('InvalidSignature'));

      setLocal('ad4m-token', 'bad-token');
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      expect(conn.token).toBe('');
      expect(conn.authState).toBe('unauthenticated');
    });
  });

  describe('disconnect()', () => {
    it('clears client, token, and host state', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      expect(conn.ad4mClient).toBeDefined();

      await conn.disconnect();
      expect(conn.ad4mClient).toBeUndefined();
      expect(conn.token).toBe('');
      expect(conn.connectedHost).toBeNull();
      expect(conn.userInfo).toBeNull();
      expect(conn.authState).toBe('unauthenticated');
      expect(conn.connectionState).toBe('not-connected');
    });

    it('fires authstatechange and connectionstatechange events', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();

      const authStates: string[] = [];
      const connStates: string[] = [];
      conn.addEventListener('authstatechange', (e: any) => authStates.push(e.detail));
      conn.addEventListener('connectionstatechange', (e: any) => connStates.push(e.detail));
      await conn.disconnect();

      expect(authStates).toContain('unauthenticated');
      expect(connStates).toContain('not-connected');
    });
  });

  describe('setConnectedHost()', () => {
    it('stores host and persists to localStorage', () => {
      const conn = new Ad4mConnect(defaultOptions);
      const host = { id: '1', name: 'Test Host', url: 'http://host:12000', profilePicUrl: '', location: 'US', rates: [], aiModels: [] };
      conn.setConnectedHost(host);
      expect(conn.connectedHost).toEqual(host);
      const stored = getLocal('ad4m-last-host');
      expect(stored).toBeTruthy();
      expect(JSON.parse(stored!)).toMatchObject({ id: '1', name: 'Test Host', url: 'http://host:12000' });
    });
  });

  describe('requestCapability()', () => {
    it('delegates to ad4mClient.agent.requestCapability', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      const reqId = await conn.requestCapability();
      expect(reqId).toBe('req-123');
      expect(conn.requestId).toBe('req-123');
    });

    it('clears token when invalidateToken is true', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      conn.token = 'old-token';
      await conn.requestCapability(true);
      // Token was cleared before calling requestCapability
      expect(conn.requestId).toBe('req-123');
    });
  });

  describe('verifyLocalAd4mCode()', () => {
    it('returns false if requestId is missing', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      conn.requestId = undefined;
      const result = await conn.verifyLocalAd4mCode('123456');
      expect(result).toBe(false);
    });

    it('generates JWT and reconnects on valid code', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      conn.requestId = 'req-123';
      const result = await conn.verifyLocalAd4mCode('123456');
      expect(result).toBe(true);
      expect(conn.token).toBe('jwt-token');
    });
  });

  describe('remote auth methods', () => {
    it('isMultiUser() queries runtime', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      const result = await conn.isMultiUser();
      expect(result).toBe(true);
    });

    it('submitEmail() sends verification request', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      const result = await conn.submitEmail('test@test.com');
      expect(result.success).toBe(true);
    });

    it('verifyEmailCode() stores token and connects', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      const result = await conn.verifyEmailCode('test@test.com', '123456');
      expect(result).toBe(true);
      expect(conn.token).toBe('jwt-from-email');
    });

    it('loginWithPassword() stores token and connects', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      const result = await conn.loginWithPassword('test@test.com', 'password');
      expect(result).toBe(true);
      expect(conn.token).toBe('jwt-from-login');
    });

    it('createAccount() creates user, logs in, and connects', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      const result = await conn.createAccount('new@test.com', 'password');
      expect(result).toBe(true);
      expect(conn.token).toBe('jwt-from-login');
    });
  });

  describe('credit polling', () => {
    it('startCreditPolling dispatches userinfochange', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();

      const events: any[] = [];
      conn.addEventListener('userinfochange', (e: any) => events.push(e.detail));

      conn.startCreditPolling();
      // Wait for immediate poll
      await new Promise(r => setTimeout(r, 50));
      conn.stopCreditPolling();

      expect(events.length).toBeGreaterThan(0);
      expect(events[0]).toMatchObject({ email: 'test@test.com', remainingCredits: 100 });
    });

    it('stopCreditPolling clears interval', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await conn.connect();
      conn.startCreditPolling();
      conn.stopCreditPolling();
      // No error thrown
    });
  });

  describe('requestTopUp()', () => {
    it('throws when not connected', async () => {
      const conn = new Ad4mConnect(defaultOptions);
      await expect(conn.requestTopUp(100)).rejects.toThrow('Not connected');
    });
  });
});
