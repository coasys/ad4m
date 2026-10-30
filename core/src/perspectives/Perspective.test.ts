import { Perspective } from "./Perspective"
import { Link, LinkExpression } from "../links/Links"
import { LinkQuery } from "./LinkQuery"

function makeLink(source: string, predicate: string | undefined, target: string, timestamp: string): LinkExpression {
    const l = new LinkExpression()
    l.author = 'did:method:12345'
    l.timestamp = timestamp
    l.data = new Link({ source, predicate, target })
    l.proof = { signature: 'asdfasdf', key: 'asdfasdf' }
    return l
}

const link = makeLink('root', undefined, 'perspective://Qm34589a3ccc0', new Date().toString())

describe('Perspective', () => {
    it('implements get()', () => {
        let p = new Perspective([link])
        let result = p.get(new LinkQuery({source: 'root'}))
        expect(result).toEqual([link])
    })

    it('implements getSingleTarget()', () => {
        let p = new Perspective([link])
        let result = p.getSingleTarget(new LinkQuery({source: 'root'}))
        expect(result).toEqual(link.data.target)
    })

    describe('get()', () => {
        const early = makeLink('a', 'p://name', 'literal://one', '2026-01-01T00:00:00Z')
        const middle = makeLink('b', 'p://name', 'literal://two', '2026-02-01T00:00:00Z')
        const late = makeLink('a', 'p://other', 'literal://three', '2026-03-01T00:00:00Z')
        const snapshot = () => new Perspective([early, middle, late])

        it('returns only the links of the given source', () => {
            expect(snapshot().get(new LinkQuery({ source: 'a' }))).toEqual([early, late])
            expect(snapshot().get(new LinkQuery({ source: 'a', predicate: 'p://name' }))).toEqual([early])
            expect(snapshot().get(new LinkQuery({ source: 'b', target: 'literal://one' }))).toEqual([])
        })

        it('applies the date filters to a predicate-only query', () => {
            const query = new LinkQuery({
                predicate: 'p://name',
                fromDate: new Date('2026-01-15T00:00:00Z'),
                untilDate: new Date('2026-02-15T00:00:00Z'),
            })
            expect(snapshot().get(query)).toEqual([middle])
        })

        it('applies the date filters and limit without source, predicate or target', () => {
            const p = snapshot()
            expect(p.get(new LinkQuery({ fromDate: new Date('2026-01-15T00:00:00Z') }))).toEqual([middle, late])
            expect(p.get(new LinkQuery({ limit: 1 }))).toEqual([early])
        })

        it('returns copies: changing a result does not change the snapshot', () => {
            const p = snapshot()
            for (const query of [new LinkQuery({}), new LinkQuery({ source: 'a' }), new LinkQuery({ predicate: 'p://name' })]) {
                const [first] = p.get(query)
                first.data.target = 'literal://changed'
                first.timestamp = 'changed'
            }
            expect(p.links.map(l => l.data.target)).toEqual(['literal://one', 'literal://two', 'literal://three'])
            expect(p.links[0].timestamp).toBe('2026-01-01T00:00:00Z')
        })

        it('returns LinkExpressions', () => {
            const [first] = snapshot().get(new LinkQuery({ source: 'a' }))
            expect(first).toBeInstanceOf(LinkExpression)
            expect(first.hash()).toBe(early.hash())
        })

        it('works on a snapshot held in a reactive Proxy', () => {
            // What Vue reactive() / Svelte 5 $state do: every nested read returns a Proxy.
            const deepProxy = <T extends object>(o: T): T => new Proxy(o, {
                get(t, k, r) {
                    const v = Reflect.get(t, k, r)
                    return v && typeof v === 'object' ? deepProxy(v) : v
                },
            })
            const p = deepProxy(snapshot())
            const result = p.get(new LinkQuery({ source: 'a' }))
            expect(result).toEqual([early, late])
            expect(result[0]).toBeInstanceOf(LinkExpression)
        })

        it('accepts dates given as strings', () => {
            const query = { predicate: 'p://name', fromDate: '2026-01-15T00:00:00Z' } as unknown as LinkQuery
            expect(snapshot().get(query)).toEqual([middle])
        })
    })

    it('getSingleTarget() leaves its argument unchanged', () => {
        const query = new LinkQuery({ source: 'root', target: 'literal://ignored' })
        expect(new Perspective([link]).getSingleTarget(query)).toBe(link.data.target)
        expect(query.target).toBe('literal://ignored')
    })
})
