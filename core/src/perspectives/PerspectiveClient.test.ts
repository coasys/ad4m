import { ApolloClient, InMemoryCache, gql } from "@apollo/client/core"
import { PerspectiveClient } from "./PerspectiveClient"
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle"

/**
 * Focused unit tests for PerspectiveClient proxy cache behaviour.
 * Uses a mock Apollo client to isolate cache logic from network.
 */

// Minimal mock Apollo client that returns controllable results
function createMockApolloClient(overrides: {
    query?: (opts: any) => any,
    mutate?: (opts: any) => any,
    subscribe?: (opts: any) => any,
} = {}) {
    return {
        query: overrides.query || jest.fn().mockResolvedValue({ data: {} }),
        mutate: overrides.mutate || jest.fn().mockResolvedValue({ data: {} }),
        subscribe: overrides.subscribe || jest.fn().mockReturnValue({
            subscribe: jest.fn()
        }),
    } as unknown as ApolloClient<any>
}

function makeHandle(uuid: string, name: string, state = PerspectiveState.Private): PerspectiveHandle {
    const h = new PerspectiveHandle(uuid, name, state)
    return h
}

describe('PerspectiveClient proxy cache', () => {
    it('byUUID returns the same proxy reference on cache hit', async () => {
        const handle = makeHandle('uuid-1', 'Test')
        const mockClient = createMockApolloClient({
            query: jest.fn().mockResolvedValue({
                data: { perspective: handle }
            }),
        })
        const client = new PerspectiveClient(mockClient, false)

        const p1 = await client.byUUID('uuid-1')
        const p2 = await client.byUUID('uuid-1')
        expect(p1).toBe(p2)
        // Only one query should have been made (second is cache hit)
        expect(mockClient.query).toHaveBeenCalledTimes(1)
    })

    it('byUUID returns null for non-existent perspectives', async () => {
        const mockClient = createMockApolloClient({
            query: jest.fn().mockResolvedValue({
                data: { perspective: null }
            }),
        })
        const client = new PerspectiveClient(mockClient, false)
        const p = await client.byUUID('non-existent')
        expect(p).toBeNull()
    })

    it('all() populates cache so subsequent byUUID is a hit', async () => {
        const handles = [
            makeHandle('uuid-a', 'A'),
            makeHandle('uuid-b', 'B'),
        ]
        const queryFn = jest.fn()
            .mockResolvedValueOnce({ data: { perspectives: handles } })
        const mockClient = createMockApolloClient({ query: queryFn })
        const client = new PerspectiveClient(mockClient, false)

        const all = await client.all()
        expect(all.length).toBe(2)

        // byUUID should be a cache hit — no additional query
        const pa = await client.byUUID('uuid-a')
        expect(pa).toBe(all[0])
        expect(queryFn).toHaveBeenCalledTimes(1) // only the all() query
    })

    it('add() populates cache', async () => {
        const handle = makeHandle('uuid-new', 'New')
        const mockClient = createMockApolloClient({
            mutate: jest.fn().mockResolvedValue({
                data: { perspectiveAdd: handle }
            }),
        })
        const client = new PerspectiveClient(mockClient, false)

        const added = await client.add('New')
        expect(added.uuid).toBe('uuid-new')

        // byUUID should hit cache
        const fetched = await client.byUUID('uuid-new')
        expect(fetched).toBe(added)
    })

    it('update() evicts and recreates cached proxy', async () => {
        const originalHandle = makeHandle('uuid-u', 'Original')
        const updatedHandle = makeHandle('uuid-u', 'Updated')
        const queryFn = jest.fn().mockResolvedValue({
            data: { perspective: originalHandle }
        })
        const mutateFn = jest.fn().mockResolvedValue({
            data: { perspectiveUpdate: updatedHandle }
        })
        const mockClient = createMockApolloClient({
            query: queryFn,
            mutate: mutateFn,
        })
        const client = new PerspectiveClient(mockClient, false)

        const original = await client.byUUID('uuid-u')
        expect(original.name).toBe('Original')

        const updated = await client.update('uuid-u', 'Updated')
        expect(updated).not.toBe(original) // new reference after eviction
        expect(updated.name).toBe('Updated')
                expect(original.name).toBe('Original') // old reference retains stale data
    })

    it('remove() evicts from cache', async () => {
        const handle = makeHandle('uuid-r', 'ToRemove')
        const queryFn = jest.fn().mockResolvedValue({
            data: { perspective: handle }
        })
        const mutateFn = jest.fn().mockResolvedValue({
            data: { perspectiveRemove: true }
        })
        const mockClient = createMockApolloClient({
            query: queryFn,
            mutate: mutateFn,
        })
        const client = new PerspectiveClient(mockClient, false)

        const p = await client.byUUID('uuid-r')
        expect(p).toBeTruthy()
        expect(queryFn).toHaveBeenCalledTimes(1)

        await client.remove('uuid-r')

        // byUUID should miss cache and query again
        const handle2 = makeHandle('uuid-r', 'Recreated')
        queryFn.mockResolvedValueOnce({ data: { perspective: handle2 } })

        const p2 = await client.byUUID('uuid-r')
        expect(p2).not.toBe(p)
        expect(p2.name).toBe('Recreated')
        expect(queryFn).toHaveBeenCalledTimes(2)
    })

    it('getOrCreateProxy does not re-register listeners on cache hit', async () => {
        // Verify that getting the same proxy twice does not create duplicate subscriptions
        const handle = makeHandle('uuid-dup', 'NoDuplicates')
        const subscribeCalls: string[] = []
        const subscribeFn = jest.fn().mockImplementation((opts) => {
            // Track subscription query names
            const queryStr = opts.query?.loc?.source?.body || ''
            subscribeCalls.push(queryStr)
            return {
                subscribe: jest.fn()
            }
        })
        const queryFn = jest.fn()
            .mockResolvedValueOnce({ data: { perspectives: [handle] } })
        const mockClient = createMockApolloClient({
            query: queryFn,
            subscribe: subscribeFn,
        })

        const client = new PerspectiveClient(mockClient, false)
        const all = await client.all()
        const firstProxy = all[0]

        // Record how many subscribe calls happened from PerspectiveProxy constructor
        const callsAfterFirst = subscribeFn.mock.calls.length

        // Get same proxy again via all() — should be cache hit, no new constructor call
        queryFn.mockResolvedValueOnce({ data: { perspectives: [handle] } })
        const all2 = await client.all()
        const secondProxy = all2[0]

        expect(secondProxy).toBe(firstProxy)
        // No additional subscribe calls should have been made
        expect(subscribeFn.mock.calls.length).toBe(callsAfterFirst)
    })

    it('all() updates existing cached proxies with fresh data', async () => {
        const handle1 = makeHandle('uuid-f', 'OldName')
        const handle2 = makeHandle('uuid-f', 'FreshName')

        const queryFn = jest.fn()
            .mockResolvedValueOnce({ data: { perspective: handle1 } })
            .mockResolvedValueOnce({ data: { perspectives: [handle2] } })
        const mockClient = createMockApolloClient({ query: queryFn })
        const client = new PerspectiveClient(mockClient, false)

        const original = await client.byUUID('uuid-f')
        expect(original.name).toBe('OldName')

        const all = await client.all()
        const refreshed = all.find(p => p.uuid === 'uuid-f')

        expect(refreshed).toBe(original) // same reference
        expect(refreshed.name).toBe('FreshName') // updated in-place
        expect(original.name).toBe('FreshName') // original ref also updated
    })
})
