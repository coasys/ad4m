import { PerspectiveClient } from "./PerspectiveClient";

// ──────────────────────────────────────────────────────────
// PerspectiveClient proxy cache
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
    // Apollo query should only happen once (second call uses cache)
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

  it("returns null when perspective does not exist", async () => {
    const mockApollo = createMockApolloClient(null);
    const client = new PerspectiveClient(mockApollo, false);

    const proxy = await client.byUUID("non-existent-uuid");
    expect(proxy).toBeNull();
  });

  it("updates cache on perspective.update()", async () => {
    const mockApollo = {
      query: jest.fn().mockResolvedValue({
        data: { perspective: { uuid: "upd-uuid", name: "Original", sharedUrl: null, neighbourhood: null, state: "Synced" } },
      }),
      mutate: jest.fn().mockResolvedValue({
        data: { perspectiveUpdate: { uuid: "upd-uuid", name: "Updated", sharedUrl: null, neighbourhood: null, state: "Synced" } },
      }),
      subscribe: jest.fn().mockReturnValue({
        subscribe: jest.fn().mockReturnValue({ unsubscribe: jest.fn() }),
      }),
    } as any;

    const client = new PerspectiveClient(mockApollo, false);
    const proxy1 = await client.byUUID("upd-uuid");
    expect(proxy1!.name).toBe("Original");

    await client.update("upd-uuid", "Updated");
    const proxy2 = await client.byUUID("upd-uuid");
    expect(proxy2!.name).toBe("Updated");
    // byUUID should return the updated cached proxy without querying again
    expect(mockApollo.query).toHaveBeenCalledTimes(1);
  });

  it("evicts cache on perspective.remove()", async () => {
    const mockApollo = {
      query: jest.fn().mockResolvedValue({
        data: { perspective: { uuid: "rm-uuid", name: "ToRemove", sharedUrl: null, neighbourhood: null, state: "Synced" } },
      }),
      mutate: jest.fn().mockResolvedValue({
        data: { perspectiveRemove: true },
      }),
      subscribe: jest.fn().mockReturnValue({
        subscribe: jest.fn().mockReturnValue({ unsubscribe: jest.fn() }),
      }),
    } as any;

    const client = new PerspectiveClient(mockApollo, false);
    await client.byUUID("rm-uuid");
    expect(mockApollo.query).toHaveBeenCalledTimes(1);

    await client.remove("rm-uuid");
    // After remove, byUUID should query again (cache evicted)
    mockApollo.query.mockResolvedValueOnce({ data: { perspective: null } });
    const proxy = await client.byUUID("rm-uuid");
    expect(proxy).toBeNull();
    expect(mockApollo.query).toHaveBeenCalledTimes(2);
  });
});
