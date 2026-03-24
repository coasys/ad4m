import { describe, it, expect, vi, beforeEach } from 'vitest';
import { ExecutorAPI } from '../lib/api.js';

// Mock fetch globally
const mockFetch = vi.fn();
vi.stubGlobal('fetch', mockFetch);

describe('ExecutorAPI', () => {
  let api: ExecutorAPI;

  beforeEach(() => {
    api = new ExecutorAPI('http://localhost:12000/graphql', 'test-admin');
    mockFetch.mockReset();
  });

  it('should send GraphQL queries with auth header', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({ data: { agentStatus: { isInitialized: true, isUnlocked: false, did: null } } }),
    });

    const result = await api.query('{ agentStatus { isInitialized isUnlocked did } }');
    expect(result.data).toBeDefined();

    expect(mockFetch).toHaveBeenCalledWith(
      'http://localhost:12000/graphql',
      expect.objectContaining({
        method: 'POST',
        headers: expect.objectContaining({
          'Content-Type': 'application/json',
          Authorization: 'test-admin',
        }),
      })
    );
  });

  it('should throw on HTTP errors', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: false,
      status: 500,
      statusText: 'Internal Server Error',
    });

    await expect(api.query('{ test }')).rejects.toThrow('API request failed: 500');
  });

  it('should handle agentGenerate mutation', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({ data: { agentGenerate: { did: 'did:key:z6MkTest123' } } }),
    });

    const result = await api.agentGenerate('test-pass');
    expect(result.did).toBe('did:key:z6MkTest123');
  });

  it('should handle agentUnlock mutation', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({ data: { agentUnlock: { isUnlocked: true, did: 'did:key:z6MkTest' } } }),
    });

    const result = await api.agentUnlock('test-pass');
    expect(result.isUnlocked).toBe(true);
    expect(result.did).toBe('did:key:z6MkTest');
  });

  it('should throw on GraphQL errors', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({ errors: [{ message: 'Agent not initialized' }] }),
    });

    await expect(api.agentGenerate('test')).rejects.toThrow('Agent not initialized');
  });

  it('should handle perspectiveAdd', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({ data: { perspectiveAdd: { uuid: 'test-uuid-123' } } }),
    });

    const result = await api.perspectiveAdd('test-perspective');
    expect(result.uuid).toBe('test-uuid-123');
  });

  it('should set auth token', async () => {
    api.setAuth('Bearer new-jwt');

    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({ data: { agentStatus: { isInitialized: true } } }),
    });

    await api.agentStatus();

    expect(mockFetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        headers: expect.objectContaining({
          Authorization: 'Bearer new-jwt',
        }),
      })
    );
  });

  it('should send mutations with variables', async () => {
    mockFetch.mockResolvedValueOnce({
      ok: true,
      json: async () => ({ data: { neighbourhoodJoinFromUrl: { uuid: 'joined-uuid' } } }),
    });

    const result = await api.neighbourhoodJoin('neighbourhood://test');
    expect(result.uuid).toBe('joined-uuid');

    const body = JSON.parse(mockFetch.mock.calls[0][1].body);
    expect(body.variables.url).toBe('neighbourhood://test');
  });
});
