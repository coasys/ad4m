import { PerspectiveClient } from "./PerspectiveClient";

// ──────────────────────────────────────────────────────────
//  WS-7: PerspectiveClient proxy cache
// ──────────────────────────────────────────────────────────

function createMockApolloClient(perspectiveResponse: any) {
  return {
    query: jest.fn().mockResolvedValue({
      data: { perspective: perspectiveResponse },
    }),
    mutate: jest.fn(),
    subscribe: jest.fn().mockReturnValue({
      subscribe: jest.fn().mockReturnValue({ unsubscribe: jest.fn() }),
    }),
  } as any;
}

describe("PerspectiveClient proxy cache", () => {
  it("returns the same proxy reference for the same UUID", async () => {
    const mockApollo = createMockApolloClient({
      uuid: "test-uuid-123",
      name: "Test Perspective",
      sharedUrl: null,
      neighbourhood: null,
      state: "Synced",
    });

    const client = new PerspectiveClient(mockApollo, false);

    const proxy1 = await client.byUUID("test-uuid-123");
    const proxy2 = await client.byUUID("test-uuid-123");

    expect(proxy1).not.toBeNull();
    expect(proxy2).not.toBeNull();
    // Should be the SAME reference (from cache)
    expect(proxy1).toBe(proxy2);
    // The Apollo query should only happen once (second call uses cache)
    expect(mockApollo.query).toHaveBeenCalledTimes(1);
  });

  it("returns different proxies for different UUIDs", async () => {
    const mockApollo = {
      query: jest.fn(async ({ variables }: any) => ({
        data: {
          perspective: {
            uuid: variables.uuid,
            name: `Perspective ${variables.uuid}`,
            sharedUrl: null,
            neighbourhood: null,
            state: "Synced",
          },
        },
      })),
      mutate: jest.fn(),
      subscribe: jest.fn().mockReturnValue({
        subscribe: jest.fn().mockReturnValue({ unsubscribe: jest.fn() }),
      }),
    } as any;

    const client = new PerspectiveClient(mockApollo, false);

    const proxyA = await client.byUUID("uuid-a");
    const proxyB = await client.byUUID("uuid-b");

    expect(proxyA).not.toBeNull();
    expect(proxyB).not.toBeNull();
    expect(proxyA).not.toBe(proxyB);
    expect(mockApollo.query).toHaveBeenCalledTimes(2);
  });

  it("re-uses cached proxy without re-fetching", async () => {
    let callCount = 0;
    const mockApollo = {
      query: jest.fn(async () => {
        callCount++;
        return {
          data: {
            perspective: {
              uuid: "gc-test-uuid",
              name: `GC Test ${callCount}`,
              sharedUrl: null,
              neighbourhood: null,
              state: "Synced",
            },
          },
        };
      }),
      mutate: jest.fn(),
      subscribe: jest.fn().mockReturnValue({
        subscribe: jest.fn().mockReturnValue({ unsubscribe: jest.fn() }),
      }),
    } as any;

    const client = new PerspectiveClient(mockApollo, false);

    const proxy1 = await client.byUUID("gc-test-uuid");
    expect(proxy1).not.toBeNull();
    expect(callCount).toBe(1);

    const proxy2 = await client.byUUID("gc-test-uuid");
    expect(proxy2).toBe(proxy1);
    expect(callCount).toBe(1);
  });
});
