import { RestClient } from './restClient';

const originalFetch = global.fetch;
const originalEventSource = (global as any).EventSource;

function mockHeaders(values: Record<string, string>) {
  return {
    forEach(callback: (value: string, key: string) => void) {
      Object.entries(values).forEach(([key, value]) => callback(value, key));
    },
  } as any;
}

class MockEventSource {
  static instances: MockEventSource[] = [];
  onmessage: ((event: { data: string }) => void) | null = null;
  onerror: ((event: Event) => void) | null = null;
  closed = false;
  url: string;

  constructor(url: string) {
    this.url = url;
    MockEventSource.instances.push(this);
  }

  emit(payload: unknown) {
    this.onmessage?.({ data: JSON.stringify(payload) });
  }

  close() {
    this.closed = true;
  }
}

describe('RestClient devtools instrumentation', () => {
  beforeEach(() => {
    MockEventSource.instances = [];
    delete (globalThis as any).__AD4M_DEVTOOLS__;
    global.fetch = originalFetch;
    (global as any).EventSource = MockEventSource as any;
  });

  afterEach(() => {
    delete (globalThis as any).__AD4M_DEVTOOLS__;
    global.fetch = originalFetch;
    (global as any).EventSource = originalEventSource;
    jest.restoreAllMocks();
  });

  test('logs REST request metadata and completion details', async () => {
    const logOperation = jest.fn().mockReturnValue(7);
    const completeOperation = jest.fn();
    (globalThis as any).__AD4M_DEVTOOLS__ = {
      logOperation,
      completeOperation,
    };

    global.fetch = jest.fn().mockResolvedValue({
      ok: true,
      status: 200,
      headers: mockHeaders({ 'content-type': 'application/json' }),
      text: async () => JSON.stringify({ did: 'did:test:123' }),
    } as any);

    const client = new RestClient('http://executor.test', 'jwt-token');
    const result = await client.get('/api/v1/agent');

    expect(result).toEqual({ did: 'did:test:123' });
    expect(logOperation).toHaveBeenCalledWith(expect.objectContaining({
      type: 'request',
      transport: 'rest',
      method: 'GET',
      path: '/api/v1/agent',
      operationName: 'GET /api/v1/agent',
      url: 'http://executor.test/api/v1/agent',
      requestHeaders: expect.objectContaining({
        Authorization: 'Bearer jwt-token',
      }),
    }));
    expect(completeOperation).toHaveBeenCalledWith(
      7,
      { did: 'did:test:123' },
      undefined,
      expect.objectContaining({
        statusCode: 200,
        responseHeaders: expect.objectContaining({
          'content-type': 'application/json',
        }),
      })
    );
  });

  test('logs failed REST requests with status codes and error details', async () => {
    const logOperation = jest.fn().mockReturnValue(9);
    const completeOperation = jest.fn();
    (globalThis as any).__AD4M_DEVTOOLS__ = {
      logOperation,
      completeOperation,
    };

    global.fetch = jest.fn().mockResolvedValue({
      ok: false,
      status: 401,
      statusText: 'Unauthorized',
      headers: mockHeaders({ 'content-type': 'text/plain' }),
      text: async () => 'invalid token',
    } as any);

    const client = new RestClient('http://executor.test', 'bad-token');

    await expect(client.get('/api/v1/runtime/info')).rejects.toThrow('invalid token');

    expect(logOperation).toHaveBeenCalledWith(expect.objectContaining({
      method: 'GET',
      path: '/api/v1/runtime/info',
    }));

    const [id, result, errors, options] = completeOperation.mock.calls[0];
    expect(id).toBe(9);
    expect(result).toBe('invalid token');
    expect(errors).toHaveLength(1);
    expect(errors[0]).toBeInstanceOf(Error);
    expect(errors[0].status).toBe(401);
    expect(options.statusCode).toBe(401);
  });

  test('records unified event-stream messages for subscriptions', () => {
    const recordEventStreamMessage = jest.fn();
    (globalThis as any).__AD4M_DEVTOOLS__ = {
      recordEventStreamMessage,
    };

    const callback = jest.fn();
    const client = new RestClient('http://executor.test', 'jwt-token');
    const unsubscribe = client.subscribe('/api/v1/events/agent', callback);

    expect(client.getActiveEventStreams()).toBe(1);
    expect(MockEventSource.instances).toHaveLength(1);
    expect(MockEventSource.instances[0].url).toBe('http://executor.test/api/v1/events/unified?token=jwt-token');

    MockEventSource.instances[0].emit({ type: 'agent-updated', did: 'did:test:123' });

    expect(recordEventStreamMessage).toHaveBeenCalledTimes(1);
    expect(callback).toHaveBeenCalledWith({ type: 'agent-updated', did: 'did:test:123' });

    unsubscribe();

    expect(client.getActiveEventStreams()).toBe(0);
    expect(MockEventSource.instances[0].closed).toBe(true);
  });
});
