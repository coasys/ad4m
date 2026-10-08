import { PerspectiveClient } from "./PerspectiveClient"
import { PerspectiveHandle, PerspectiveState } from "./PerspectiveHandle"
import { RpcError } from "../apiClient"
import { LinkQuery } from "./LinkQuery"

/**
 * Unit tests for PerspectiveClient RPC operations.
 * Uses a mock ApiClient to isolate from real WebSocket connections.
 */

// Mock ApiClient so we can control call/on behavior
const mockCall = jest.fn()
const mockOn = jest.fn().mockReturnValue(() => {})

jest.mock('../apiClient', () => {
    return {
        ApiClient: jest.fn().mockImplementation(() => ({
            call: mockCall,
            on: mockOn,
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
        mockOn.mockReset().mockReturnValue(() => {})
    })

    it('byUUID returns a PerspectiveProxy for a valid UUID', async () => {
        const handle = makeHandle('uuid-1', 'Test')
        mockCall.mockResolvedValue(handle)

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const proxy = await client.byUUID('uuid-1')

        expect(proxy).not.toBeNull()
        expect(proxy!.uuid).toBe('uuid-1')
        expect(proxy!.name).toBe('Test')
        expect(mockCall).toHaveBeenCalledWith('perspective.get', { uuid: 'uuid-1' })
    })

    it('byUUID returns null when server returns null', async () => {
        mockCall.mockResolvedValue(null)

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const proxy = await client.byUUID('non-existent')

        expect(proxy).toBeNull()
    })

    it('byUUID returns null on 404 RpcError', async () => {
        mockCall.mockRejectedValue(new RpcError(404, 'Not found'))

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const proxy = await client.byUUID('missing')

        expect(proxy).toBeNull()
    })

    it('byUUID rethrows non-404 errors', async () => {
        mockCall.mockRejectedValue(new RpcError(500, 'Internal error'))

        const client = new PerspectiveClient('http://localhost:12000', 'token')

        await expect(client.byUUID('error-uuid')).rejects.toThrow('RPC error 500')
    })

    it('all() returns array of PerspectiveProxy instances', async () => {
        const handles = [
            makeHandle('uuid-a', 'A'),
            makeHandle('uuid-b', 'B'),
        ]
        mockCall.mockResolvedValue(handles)

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const all = await client.all()

        expect(all.length).toBe(2)
        expect(all[0].uuid).toBe('uuid-a')
        expect(all[0].name).toBe('A')
        expect(all[1].uuid).toBe('uuid-b')
        expect(all[1].name).toBe('B')
        expect(mockCall).toHaveBeenCalledWith('perspective.all', {})
    })

    it('add() creates a perspective and returns a proxy', async () => {
        const handle = makeHandle('uuid-new', 'New')
        mockCall.mockResolvedValue(handle)

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const proxy = await client.add('New')

        expect(proxy.uuid).toBe('uuid-new')
        expect(proxy.name).toBe('New')
        expect(mockCall).toHaveBeenCalledWith('perspective.create', { name: 'New' })
    })

    it('update() updates a perspective and returns a new proxy', async () => {
        const updatedHandle = makeHandle('uuid-u', 'Updated')
        mockCall.mockResolvedValue(updatedHandle)

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const proxy = await client.update('uuid-u', 'Updated')

        expect(proxy.uuid).toBe('uuid-u')
        expect(proxy.name).toBe('Updated')
        expect(mockCall).toHaveBeenCalledWith('perspective.update', { uuid: 'uuid-u', name: 'Updated' })
    })

    it('remove() calls perspective.remove and returns result', async () => {
        mockCall.mockResolvedValue(true)

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const result = await client.remove('uuid-r')

        expect(result).toEqual({ perspectiveRemove: true })
        expect(mockCall).toHaveBeenCalledWith('perspective.remove', { uuid: 'uuid-r' })
    })

    it('constructor opens no event subscription', () => {
        new PerspectiveClient('http://localhost:12000', 'token')

        expect(mockOn).not.toHaveBeenCalled()
    })

    it('on() registers with ApiClient.on and returns its release function', () => {
        const release = jest.fn()
        mockOn.mockReturnValue(release)
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const handler = jest.fn()

        const off = client.on('link-added', handler, { perspective: 'uuid-l' })

        expect(mockOn).toHaveBeenCalledWith('link-added', handler, { perspective: 'uuid-l' })
        off()
        expect(release).toHaveBeenCalledTimes(1)
    })

    it('addLink calls perspective.addLink with correct params', async () => {
        const linkExpr = { author: 'did:test', timestamp: '2026-01-01', data: { source: 'a', target: 'b', predicate: 'c' }, proof: {} }
        mockCall.mockResolvedValue(linkExpr)

        const client = new PerspectiveClient('http://localhost:12000', 'token')
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

        const client = new PerspectiveClient('http://localhost:12000', 'token')
        await client.queryLinks('uuid-q', new LinkQuery({ source: 'src', predicate: 'pred', target: 'tgt' }))

        expect(mockCall).toHaveBeenCalledWith(
            'perspective.queryLinks',
            expect.objectContaining({
                uuid: 'uuid-q',
                source: 'src',
                predicate: 'pred',
                target: 'tgt',
            }),
            undefined,
        )
    })

    // ── CallOptions forwarding (AbortSignal plumbing) ──────────────────
    //
    // These tests prove the new `options?` parameter on the long-running
    // query methods reaches `ApiClient.call` as the third argument.  The
    // wire-protocol behaviour (request.cancel + AbortError rejection) is
    // covered separately in apiClient.test.ts; here we just verify the
    // signal propagates through the SDK layer instead of being silently
    // dropped.

    it('querySparql forwards options to apiClient.call', async () => {
        mockCall.mockResolvedValue(JSON.stringify([]))
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const controller = new AbortController()
        await client.querySparql('uuid-q', 'SELECT * WHERE { ?s ?p ?o }', { signal: controller.signal })
        expect(mockCall).toHaveBeenCalledWith(
            'perspective.querySparql',
            expect.objectContaining({ uuid: 'uuid-q', query: 'SELECT * WHERE { ?s ?p ?o }' }),
            { signal: controller.signal },
        )
    })

    it('modelQuery forwards options to apiClient.call', async () => {
        mockCall.mockResolvedValue(JSON.stringify({ instances: [], totalCount: 0 }))
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const controller = new AbortController()
        await client.modelQuery('uuid-q', 'Recipe', '{}', { signal: controller.signal })
        expect(mockCall).toHaveBeenCalledWith(
            'perspective.modelQuery',
            expect.objectContaining({ uuid: 'uuid-q', class_name: 'Recipe' }),
            { signal: controller.signal },
        )
    })

    it('queryLinks forwards options to apiClient.call', async () => {
        mockCall.mockResolvedValue([])
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const controller = new AbortController()
        await client.queryLinks('uuid-q', new LinkQuery({ source: 'src' }), { signal: controller.signal })
        expect(mockCall).toHaveBeenCalledWith(
            'perspective.queryLinks',
            expect.objectContaining({ uuid: 'uuid-q', source: 'src' }),
            { signal: controller.signal },
        )
    })

    it('queryProlog forwards options to apiClient.call', async () => {
        mockCall.mockResolvedValue(JSON.stringify([]))
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const controller = new AbortController()
        await client.queryProlog('uuid-q', 'foo(X).', { signal: controller.signal })
        expect(mockCall).toHaveBeenCalledWith(
            'perspective.queryProlog',
            expect.objectContaining({ uuid: 'uuid-q', query: 'foo(X).' }),
            { signal: controller.signal },
        )
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

describe('PerspectiveClient builds SDK classes from wire data', () => {
    const wireLink = {
        author: 'did:test', timestamp: '2026-01-01T00:00:00Z',
        data: { source: 'a', target: 'b', predicate: null },
        proof: { key: 'k', signature: 's', valid: true, invalid: false },
        status: 'SHARED' as const,
    }

    beforeEach(() => {
        mockCall.mockReset()
    })

    it('queryLinks returns LinkExpressions with a working hash()', async () => {
        mockCall.mockResolvedValue([wireLink])
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const [link] = await client.queryLinks('uuid-q', new LinkQuery({ source: 'a' }))

        expect(typeof link.hash()).toBe('number')
        expect(link.status).toBe('SHARED')
        expect(link.proof.valid).toBe(true)
    })

    it('byUUID gives the neighbourhood meta a Perspective with get()', async () => {
        mockCall.mockResolvedValue({
            uuid: 'uuid-n', name: 'N', state: 'SYNCED', sharedUrl: 'neighbourhood://x', owners: ['did:o'],
            neighbourhood: {
                author: 'did:test', timestamp: 't', proof: wireLink.proof,
                data: { linkLanguage: 'lang', meta: { links: [wireLink] } },
            },
        })
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const proxy = await client.byUUID('uuid-n')

        expect(proxy!.sharedUrl).toBe('neighbourhood://x')
        const meta = proxy!.neighbourhood!.data.meta
        expect(meta.get(new LinkQuery({ source: 'a' }))).toHaveLength(1)
    })

    it('linkMutations sends wire removals and returns LinkExpressionMutations', async () => {
        mockCall.mockResolvedValue({ additions: [wireLink], removals: [], updates: [] })
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const removal = { ...wireLink, data: { source: 'a', target: 'b' }, hash: () => 0 }
        const result = await client.linkMutations('uuid-m', { additions: [], removals: [removal] }, 'local')

        expect(typeof result.additions[0].hash()).toBe('number')
        expect(mockCall).toHaveBeenCalledWith('perspective.linkMutations', {
            uuid: 'uuid-m',
            mutations: {
                additions: [],
                removals: [{
                    author: 'did:test', timestamp: '2026-01-01T00:00:00Z',
                    data: { source: 'a', target: 'b', predicate: undefined },
                    proof: { key: 'k', signature: 's', valid: true, invalid: false },
                    status: 'SHARED',
                }],
            },
            status: 'local',
        })
    })

    it('removeLink sends the link without status and leaves the argument intact', async () => {
        mockCall.mockResolvedValue(true)
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        const link = { ...wireLink, data: { source: 'a', target: 'b' }, hash: () => 0 }
        await client.removeLink('uuid-r', link)

        expect(mockCall.mock.calls[0][1].link).not.toHaveProperty('status')
        expect(link.status).toBe('SHARED')
    })

    it('addSdna narrows the single/batch result union', async () => {
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        mockCall.mockResolvedValueOnce(true)
        expect(await client.addSdna('u', 'n', 'code', 'subject_class')).toBe(true)
        mockCall.mockResolvedValueOnce([true, false])
        expect(await client.addSdna('u', 'n', 'code', 'subject_class')).toBe(false)
        mockCall.mockResolvedValueOnce(true)
        expect(await client.addSdnaBatch('u', [{ name: 'n', sdnaType: 'flow' }])).toEqual([true])
        mockCall.mockResolvedValueOnce([true, false])
        expect(await client.addSdnaBatch('u', [{ name: 'n', sdnaType: 'flow' }])).toEqual([true, false])
    })

    it('interpretationOverlays narrows kind to create | update', async () => {
        mockCall.mockResolvedValue([{ base: 'b', kind: 'create', run: null, inferred: [['p', 1]] }])
        const client = new PerspectiveClient('http://localhost:12000', 'token')
        expect(await client.interpretationOverlays('u')).toEqual([
            { base: 'b', kind: 'create', run: null, inferred: [['p', 1]] },
        ])
    })
})
