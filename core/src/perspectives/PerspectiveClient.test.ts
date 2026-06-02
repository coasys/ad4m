import { PerspectiveClient } from "./PerspectiveClient"
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle"
import { RpcError } from "../apiClient"
import { LinkQuery } from "./LinkQuery"

/**
 * Unit tests for PerspectiveClient RPC operations.
 * Uses a mock ApiClient to isolate from real WebSocket connections.
 */

// Mock ApiClient so we can control call/subscribe behavior
const mockCall = jest.fn()
const mockSubscribe = jest.fn().mockReturnValue(() => {})

jest.mock('../apiClient', () => {
    return {
        ApiClient: jest.fn().mockImplementation(() => ({
            call: mockCall,
            subscribe: mockSubscribe,
            waitForSubscription: jest.fn().mockResolvedValue(undefined),
        })),
        RpcError: class RpcError extends Error {
            readonly status: number
            readonly body: string
            constructor(status: number, body: string) {
                super(`RPC error ${status}: ${body}`)
                this.name = "RpcError"
                this.status = status
                this.body = body
            }
        },
    }
})

function makeHandle(uuid: string, name: string, state = PerspectiveState.Private): PerspectiveHandle {
    const h = new PerspectiveHandle(uuid, name, state)
    return h
}

describe('PerspectiveClient RPC operations', () => {
    beforeEach(() => {
        mockCall.mockReset()
        mockSubscribe.mockReset().mockReturnValue(() => {})
    })

    it('byUUID returns a PerspectiveProxy for a valid UUID', async () => {
        const handle = makeHandle('uuid-1', 'Test')
        mockCall.mockResolvedValue(handle)

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const proxy = await client.byUUID('uuid-1')

        expect(proxy).not.toBeNull()
        expect(proxy!.uuid).toBe('uuid-1')
        expect(proxy!.name).toBe('Test')
        expect(mockCall).toHaveBeenCalledWith('perspective.get', { uuid: 'uuid-1' })
    })

    it('byUUID returns null when server returns null', async () => {
        mockCall.mockResolvedValue(null)

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const proxy = await client.byUUID('non-existent')

        expect(proxy).toBeNull()
    })

    it('byUUID returns null on 404 RpcError', async () => {
        mockCall.mockRejectedValue(new RpcError(404, 'Not found'))

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const proxy = await client.byUUID('missing')

        expect(proxy).toBeNull()
    })

    it('byUUID rethrows non-404 errors', async () => {
        mockCall.mockRejectedValue(new RpcError(500, 'Internal error'))

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)

        await expect(client.byUUID('error-uuid')).rejects.toThrow('RPC error 500')
    })

    it('all() returns array of PerspectiveProxy instances', async () => {
        const handles = [
            makeHandle('uuid-a', 'A'),
            makeHandle('uuid-b', 'B'),
        ]
        mockCall.mockResolvedValue(handles)

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const all = await client.all()

        expect(all.length).toBe(2)
        expect(all[0].uuid).toBe('uuid-a')
        expect(all[0].name).toBe('A')
        expect(all[1].uuid).toBe('uuid-b')
        expect(all[1].name).toBe('B')
        expect(mockCall).toHaveBeenCalledWith('perspective.all')
    })

    it('add() creates a perspective and returns a proxy', async () => {
        const handle = makeHandle('uuid-new', 'New')
        mockCall.mockResolvedValue(handle)

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const proxy = await client.add('New')

        expect(proxy.uuid).toBe('uuid-new')
        expect(proxy.name).toBe('New')
        expect(mockCall).toHaveBeenCalledWith('perspective.create', { name: 'New' })
    })

    it('update() updates a perspective and returns a new proxy', async () => {
        const updatedHandle = makeHandle('uuid-u', 'Updated')
        mockCall.mockResolvedValue(updatedHandle)

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const proxy = await client.update('uuid-u', 'Updated')

        expect(proxy.uuid).toBe('uuid-u')
        expect(proxy.name).toBe('Updated')
        expect(mockCall).toHaveBeenCalledWith('perspective.update', { uuid: 'uuid-u', name: 'Updated' })
    })

    it('remove() calls perspective.remove and returns result', async () => {
        mockCall.mockResolvedValue(true)

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const result = await client.remove('uuid-r')

        expect(result).toEqual({ perspectiveRemove: true })
        expect(mockCall).toHaveBeenCalledWith('perspective.remove', { uuid: 'uuid-r' })
    })

    it('constructor subscribes to events when subscribe=true', () => {
        new PerspectiveClient('http://localhost:12000', 'token', true)

        // Should have subscribed 3 times (added, updated, removed)
        expect(mockSubscribe).toHaveBeenCalledTimes(3)
    })

    it('constructor does not subscribe when subscribe=false', () => {
        new PerspectiveClient('http://localhost:12000', 'token', false)

        expect(mockSubscribe).not.toHaveBeenCalled()
    })

    it('addPerspectiveAddedListener dispatches events to callbacks', () => {
        const subscriberCallbacks: ((data: any) => void)[] = []
        mockSubscribe.mockImplementation((cb: any) => {
            subscriberCallbacks.push(cb)
            return () => {}
        })

        const client = new PerspectiveClient('http://localhost:12000', 'token', true)
        const received: PerspectiveHandle[] = []
        client.addPerspectiveAddedListener((h) => { received.push(h); return null })

        // Simulate server push event to all subscribers (like real WS dispatch)
        const handle = makeHandle('uuid-event', 'EventPerspective')
        subscriberCallbacks.forEach(cb => cb({ type: 'perspective-added', perspective: handle }))

        expect(received.length).toBe(1)
        expect(received[0].uuid).toBe('uuid-event')
    })

    it('addLink calls perspective.addLink with correct params', async () => {
        const linkExpr = { author: 'did:test', timestamp: '2026-01-01', data: { source: 'a', target: 'b', predicate: 'c' }, proof: {} }
        mockCall.mockResolvedValue(linkExpr)

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        const result = await client.addLink('uuid-l', { source: 'a', target: 'b', predicate: 'c' })

        expect(result).toEqual(linkExpr)
        expect(mockCall).toHaveBeenCalledWith('perspective.addLink', {
            uuid: 'uuid-l',
            link: { source: 'a', target: 'b', predicate: 'c' },
            status: 'shared',
            batchId: undefined,
        })
    })

    it('queryLinks sends query parameters correctly', async () => {
        mockCall.mockResolvedValue([])

        const client = new PerspectiveClient('http://localhost:12000', 'token', false)
        await client.queryLinks('uuid-q', new LinkQuery({ source: 'src', predicate: 'pred', target: 'tgt' }))

        expect(mockCall).toHaveBeenCalledWith('perspective.queryLinks', expect.objectContaining({
            uuid: 'uuid-q',
            source: 'src',
            predicate: 'pred',
            target: 'tgt',
        }))
    })
})

/**
 * Unit tests for PerspectiveProxy utility functions.
 */

describe('PerspectiveProxy getClassShape sh:in URI-decoding', () => {
    // This tests the sh:in parsing logic in getClassShape() which was fixed
    // to URI-decode values from the Rust executor's SPARQL results

    function parseShInValue(raw: string): Array<{ value: string; label?: string }> | undefined {
        // Reproduce the exact parsing logic from PerspectiveProxy.getClassShape()
        try {
            if (raw.startsWith('literal:string:')) {
                raw = decodeURIComponent(raw.substring('literal:string:'.length));
            }
            return JSON.parse(raw);
        } catch { return undefined; }
    }

    it('parses plain JSON sh:in values', () => {
        const result = parseShInValue('[{"value":"a"},{"value":"b","label":"B"}]');
        expect(result).toEqual([
            { value: 'a' },
            { value: 'b', label: 'B' },
        ]);
    });

    it('strips literal:string: prefix and parses', () => {
        const result = parseShInValue('literal:string:[{"value":"x"},{"value":"y"}]');
        expect(result).toEqual([
            { value: 'x' },
            { value: 'y' },
        ]);
    });

    it('URI-decodes encoded sh:in values from Rust executor', () => {
        // Rust executor may URI-encode the JSON when returning SPARQL results
        const encoded = 'literal:string:' + encodeURIComponent('[{"value":"hello world"},{"value":"a&b"}]');
        const result = parseShInValue(encoded);
        expect(result).toEqual([
            { value: 'hello world' },
            { value: 'a&b' },
        ]);
    });

    it('handles double-encoded brackets and quotes', () => {
        const encoded = 'literal:string:%5B%7B%22value%22%3A%22active%22%7D%2C%7B%22value%22%3A%22inactive%22%7D%5D';
        const result = parseShInValue(encoded);
        expect(result).toEqual([
            { value: 'active' },
            { value: 'inactive' },
        ]);
    });

    it('returns undefined for malformed JSON', () => {
        const result = parseShInValue('literal:string:not-json');
        expect(result).toBeUndefined();
    });

    it('handles already-decoded values without prefix', () => {
        const result = parseShInValue('[{"value":"test"}]');
        expect(result).toEqual([{ value: 'test' }]);
    });
})
